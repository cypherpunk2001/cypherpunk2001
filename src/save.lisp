(in-package :mmorpg)

;;; save.lisp - Glue: game state serialization/deserialization orchestration
;;;
;;; This file is loaded LAST among save-* files and contains:
;;; - Top-level serialize/deserialize game state functions
;;; - Save/load game to/from file
;;; - apply-game-state (zone switching + deserialization)
;;;
;;; Split files (loaded before this, in order):
;;; - save-serialize.lisp: serialize-* functions, compact serialization, shared constants
;;; - save-deserialize.lisp: deserialize-* functions, apply-* functions
;;; - save-delta.lisp: delta encoding/decoding, dirty flags
;;; - save-edge-strips.lisp: edge strip serialization/deserialization
;;; - save-validate.lisp: schema checks, bounds validation

(defun serialize-game-state (game &key (include-visuals nil) (network-only nil))
  ;; Serialize authoritative game state to plist (server snapshot).
  ;; When network-only is true, uses minimal player serialization (no inventory/stats)
  ;; to keep snapshot size under UDP limits (~65KB).
  (let* ((players (game-players game))
         (npcs (game-npcs game))
         (world (game-world game))
         (zone (world-zone world))
         (id-source (game-id-source game))
         (player-list nil)
         (npc-list nil)
         (object-list nil))
    ;; Serialize players (use network-only mode for smaller snapshots)
    (when players
      (loop :for player :across players
            :do (push (serialize-player player
                                        :include-visuals include-visuals
                                        :network-only network-only)
                      player-list)))
    ;; Serialize NPCs
    (when npcs
      (loop :for npc :across npcs
            :do (push (serialize-npc npc :include-visuals include-visuals) npc-list)))
    ;; Serialize zone objects
    (when zone
      (let ((objects (zone-objects zone)))
        (dolist (object objects)
          (push (serialize-object object) object-list))))
    ;; Build save state
    (list :version *save-format-version*
          :zone-id (and zone (zone-id zone))
          :id-next (and id-source (id-source-next-id id-source))
          :players (nreverse player-list)
          :npcs (nreverse npc-list)
          :objects (nreverse object-list))))

(defun deserialize-game-state (plist game)
  ;; Restore authoritative game state from plist into existing GAME.
  ;; Supports legacy plist, compact-v1/v2/v3/v4/v5, and delta-v1/v2/v3/v4/v5 formats.
  ;; Returns: (values zone-id delta-positions-or-nil)
  ;; delta-positions is non-nil only for delta-v1 format, for interpolation fix.
  (let ((format (getf plist :format)))
    (cond
      ;; Delta format (incremental updates, see docs/net.md Prong 2)
      ((or (eq format :delta-v1)
           (eq format :delta-v2)
           (eq format :delta-v3)
           (eq format :delta-v4)
           (eq format :delta-v5))
       (let ((delta-positions (deserialize-game-state-delta plist game)))
         (values (getf plist :zone-id) delta-positions)))
      ;; Compact format (network-optimized, see docs/net.md 4-Prong)
      ((or (eq format :compact-v1)
           (eq format :compact-v2)
           (eq format :compact-v3)
           (eq format :compact-v4)
           (eq format :compact-v5))
       (deserialize-game-state-compact plist game)
       (values (getf plist :zone-id) nil))
      ;; Legacy plist format (DB saves, compatibility)
      (t
       (let* ((version (getf plist :version 0))
              (zone-id (getf plist :zone-id))
              (id-next (getf plist :id-next 1))
              (player-plists (or (getf plist :players)
                                 (let ((player-plist (getf plist :player)))
                                   (and player-plist (list player-plist)))))
              (npc-plists (getf plist :npcs))
              (object-plists (getf plist :objects))
              (npcs (game-npcs game))
              (world (game-world game))
              (zone (world-zone world))
              (id-source (game-id-source game)))
         ;; Version check (for future migrations)
         (when (> version *save-format-version*)
           (warn "Save file version ~d is newer than current version ~d"
                 version *save-format-version*))
         ;; Restore ID source
         (when id-source
           (setf (id-source-next-id id-source) id-next))
         ;; Restore players
         (apply-player-plists game player-plists)
         ;; Restore NPCs
         (loop :for npc-plist :in npc-plists
               :for index :from 0
               :do (deserialize-npc npc-plist npcs index))
         ;; Restore zone objects
         (when zone
           (setf (zone-objects zone)
                 (mapcar #'deserialize-object object-plists)))
         ;; Return zone ID for zone switching if needed
         (values zone-id nil))))))

(defun apply-game-state (game state &key (apply-zone t))
  ;; Apply serialized STATE into GAME, loading zones when needed.
  ;; Returns: (values zone-id zone-loaded delta-positions-or-nil)
  ;; When a zone change is needed but fails (client cache miss), the entire
  ;; snapshot is dropped to prevent desync (player in wrong zone with new-zone data).
  (when (and game state)
    (let* ((zone-id (getf state :zone-id))
           (world (game-world game))
           (current-zone (and world (world-zone world)))
           (current-zone-id (and current-zone (zone-id current-zone)))
           (zone-loaded nil)
           (zone-change-failed nil))
      (when (and apply-zone zone-id (not (eql zone-id current-zone-id)))
        (let* ((graph (and world (world-world-graph world)))
               (path (and graph (world-graph-zone-path graph zone-id)))
               ;; Step 4: Check LRU cache before loading from disk
               (zone-lru (and game (game-zone-cache game)))
               (cached-zone (and zone-lru (zone-cache-lookup zone-lru zone-id))))
          ;; ADDENDUM 3: No sync load on client transition path.
          ;; Preloading (Step 5) + urgent preload should guarantee cache hit.
          ;; Network clients skip the sync fallback entirely to prevent hitches.
          ;; Server/local modes still have the disk fallback (they don't preload).
          (let* ((is-client (eq (game-net-role game) :client))
                 (zone (or cached-zone
                           (cond
                             ;; Client: warn and drop entire snapshot — applying new-zone
                             ;; positions into old zone would cause desync
                             ((and is-client (not cached-zone) path)
                              (warn "Zone ~a: cache MISS on client transition — snapshot dropped (preload gap)"
                                    zone-id)
                              (setf zone-change-failed t)
                              nil)
                             ;; Server/local: sync load fallback with retry
                             ((and path (probe-file path))
                              (let ((loaded (load-zone path)))
                                ;; Insert into LRU cache for future transitions
                                (when (and loaded zone-lru)
                                  (zone-cache-insert zone-lru zone-id loaded))
                                loaded))))))
            (when zone
              (when path (setf *zone-path* path))
              (apply-zone-to-world world zone)
              ;; Call client hook to clear render caches (set by rendering.lisp)
              (when *client-zone-change-hook*
                (funcall *client-zone-change-hook* (zone-id zone)))
              (setf zone-loaded t)))))
      ;; If zone change was needed but failed on client, drop the entire snapshot.
      ;; Next server tick will resend; by then preload should have the zone cached.
      (when zone-change-failed
        (return-from apply-game-state (values zone-id nil nil)))
      (when zone-loaded
        (let* ((player (game-player game))
               (players (game-players game))
               (npcs (make-npcs player world
                                :id-source (game-npc-id-source game))))
          (ensure-npcs-open-spawn npcs world)
          (setf (game-npcs game) npcs
                (game-entities game) (make-entities players npcs))))
      (multiple-value-bind (result-zone-id delta-positions)
          (deserialize-game-state state game)
        (values result-zone-id zone-loaded delta-positions)))))

(defun save-game (game filepath)
  ;; Save game state to FILEPATH. Returns T on success, NIL on failure (non-fatal).
  (handler-case
      (progn
        (log-verbose "Serializing game state for save")
        (let ((state (serialize-game-state game)))
          (with-open-file (out filepath
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
            (prin1 state out))
          (format t "~&Saved game to ~a~%" filepath)
          (log-verbose "Save completed successfully")
          t))
    (error (e)
      (warn "Failed to save game to ~a: ~a" filepath e)
      (log-verbose "Save error: ~a" e)
      nil)))

(defun load-game (game filepath &key (apply-zone t))
  ;; Load game state from FILEPATH into existing GAME.
  ;; Returns zone-id on success, NIL on failure (non-fatal).
  (if (probe-file filepath)
      (handler-case
          (progn
            (log-verbose "Loading game from ~a" filepath)
            (let* ((state (with-open-file (in filepath :direction :input)
                            (let ((*read-eval* nil)) ; Security: disable eval in read
                              (read in)))))
              (multiple-value-bind (loaded-zone-id _zone-loaded)
                  (apply-game-state game state :apply-zone apply-zone)
                (declare (ignore _zone-loaded))
                (format t "~&Loaded game from ~a (zone: ~a)~%" filepath loaded-zone-id)
                (log-verbose "Load completed, zone transitioned to ~a" loaded-zone-id)
                loaded-zone-id)))
        (error (e)
          (warn "Failed to load game from ~a: ~a" filepath e)
          (log-verbose "Load error (possibly corrupt save): ~a" e)
          nil))
      (progn
        (warn "Save file not found: ~a" filepath)
        nil)))
