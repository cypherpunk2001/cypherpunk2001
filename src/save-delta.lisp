(in-package :mmorpg)

;;; save-delta.lisp - Delta encoding/decoding, compact serialization format
;;;
;;; Depends on save-serialize.lisp for: serialize-player-compact, serialize-npc-compact,
;;;   serialize-object, reset-player-vector-pool, encode-zone-id, dequantize-coord,
;;;   player-compact-zone-hash, *starting-zone-id*
;;; Depends on save-deserialize.lisp for: deserialize-player-compact, deserialize-player,
;;;   apply-player-compact-direct, deserialize-npc-compact, apply-npc-compact-direct,
;;;   deserialize-npc
;;; Depends on save-edge-strips.lisp for: serialize-edge-strips-for-zone,
;;;   deserialize-edge-strips

;;;; ========================================================================
;;;; DELTA COMPRESSION - See docs/net.md Prong 2
;;;; ========================================================================

(defun serialize-game-state-delta (game seq baseline-seq)
  "Serialize only dirty entities as delta snapshot.
   SEQ is current snapshot sequence number.
   BASELINE-SEQ is client's last acknowledged sequence.
   Returns compact delta snapshot with only changed entities."
  (let* ((players (game-players game))
         (npcs (game-npcs game))
         (world (game-world game))
         (zone (world-zone world))
         (objects (and zone (zone-objects zone)))
         (changed-players nil)
         (changed-npcs nil)
         (object-list nil))
    ;; Collect dirty players
    (when players
      (loop :for player :across players
            :when (and player (player-snapshot-dirty player))
            :do (push (serialize-player-compact player) changed-players)))
    ;; Collect dirty NPCs (with zone-id for client filtering)
    (let ((npc-zone-id (and zone (zone-id zone))))
      (when npcs
        (loop :for npc :across npcs
              :when (and npc (npc-snapshot-dirty npc))
              :do (push (serialize-npc-compact npc npc-zone-id) changed-npcs))))
    ;; Collect all zone objects (dropped items + respawning pickups)
    (when objects
      (dolist (object objects)
        (push (serialize-object object) object-list)))
    ;; Build delta snapshot
    (list :format :delta-v5
          :seq seq
          :baseline-seq baseline-seq
          :zone-id (and zone (zone-id zone))
          :changed-players (coerce (nreverse changed-players) 'vector)
          :changed-npcs (coerce (nreverse changed-npcs) 'vector)
          :objects (nreverse object-list))))

(defun serialize-game-state-delta-for-zone (game zone-id zone-state seq &key use-pool)
  "Serialize delta snapshot filtered to ZONE-ID.
   Only includes dirty players in that zone and NPCs from zone-state.
   Uses cached zone-players array when available (Task 4.1).
   When USE-POOL is T, uses pre-allocated vectors (Task 4.2).
   SEQ is current snapshot sequence number."
  ;; Reset vector pool for this serialization pass (Task 4.2)
  (when use-pool
    (reset-player-vector-pool))
  (let* ((zone-players (when zone-state (zone-state-zone-players zone-state)))
         (npcs (if zone-state (zone-state-npcs zone-state) nil))
         (zone (if zone-state (zone-state-zone zone-state) nil))
         (objects (and zone (zone-objects zone)))
         (changed-players nil)
         (changed-npcs nil)
         (object-list nil))
    ;; Filter dirty players using cached zone-players if available (Task 4.1)
    (if (and zone-players (> (length zone-players) 0))
        ;; Fast path: iterate only zone players
        (loop :for player :across zone-players
              :when (and player (player-snapshot-dirty player))
              :do (push (serialize-player-compact player :use-pool use-pool) changed-players))
        ;; Fallback: filter all players by zone
        (let ((players (game-players game)))
          (when players
            (loop :for player :across players
                  :for player-zone = (or (player-zone-id player) *starting-zone-id*)
                  :when (and player
                             (player-snapshot-dirty player)
                             (eq player-zone zone-id))
                  :do (push (serialize-player-compact player :use-pool use-pool) changed-players)))))
    ;; Collect dirty NPCs from zone-state only (with zone-id for client filtering)
    (when npcs
      (loop :for npc :across npcs
            :when (and npc (npc-snapshot-dirty npc))
            :do (push (serialize-npc-compact npc zone-id) changed-npcs)))
    ;; Zone objects
    (when objects
      (dolist (object objects)
        (push (serialize-object object) object-list)))
    ;; Build delta with EXPLICIT zone-id (matches existing delta-v5 format)
    (let ((snapshot (list :format :delta-v5
                          :seq seq
                          :baseline-seq nil  ; Keep shape stable with existing delta format
                          :zone-id zone-id
                          :changed-players (coerce (nreverse changed-players) 'vector)
                          :changed-npcs (coerce (nreverse changed-npcs) 'vector)
                          :objects (nreverse object-list))))
      ;; Step 7: Edge strips always sent in full (not as deltas) per plan.
      ;; Small (~1.6KB worst case), ephemeral, no cross-zone dirty tracking needed.
      (let ((edge-strips (serialize-edge-strips-for-zone game zone-id)))
        (when edge-strips
          (setf (getf snapshot :edge-strips) edge-strips)))
      snapshot)))

(defun clear-snapshot-dirty-flags (game)
  "Reset dirty flags on all entities after snapshot broadcast.
   Called once per frame after sending to all clients."
  (let* ((players (game-players game))
         (npcs (game-npcs game))
         (world (game-world game))
         (zone (and world (world-zone world)))
         (objects (and zone (zone-objects zone))))
    (when players
      (loop :for player :across players
            :when player
            :do (setf (player-snapshot-dirty player) nil)))
    (when npcs
      (loop :for npc :across npcs
            :when npc
            :do (setf (npc-snapshot-dirty npc) nil)))
    ;; Clear object dirty flags
    ;; Task 5.5: Use zone-object struct accessor
    (when objects
      (dolist (object objects)
        (setf (zone-object-snapshot-dirty object) nil)))))

(defun deserialize-game-state-delta (delta game)
  "Apply delta snapshot to GAME, updating changed entities and adding new ones.
   Preserves existing entities not included in the delta.
   For players: updates existing by ID, adds new players not seen before.
   For NPCs: updates existing by index (NPCs use fixed array positions).
   Returns hash table of updated entity positions for interpolation buffer.
   Client-side zone filtering: only processes entities matching snapshot's zone."
  (with-timing (:deserialize-delta)
    (let* ((changed-players (getf delta :changed-players))
         (changed-npcs (getf delta :changed-npcs))
         (players (game-players game))
         ;; Use snapshot's :zone-id for filtering (more reliable than player-zone-id
         ;; which may not be set from compact format). Defense-in-depth filter.
         (snapshot-zone-id (getf delta :zone-id))
         (local-zone-hash (when snapshot-zone-id
                            (encode-zone-id snapshot-zone-id)))
         (updated-positions (make-hash-table :test 'eql)))  ; Track what we updated
    ;; Apply changed players - update existing OR add new
    (when (and changed-players (> (length changed-players) 0))
      (let ((new-players nil))  ; Collect players we need to add
        ;; First pass: update existing, collect new
        ;; Optimization: extract values directly from vector, skip plist for existing
        (loop :for vec :across changed-players
              :for id = (aref vec 0)
              :for x = (dequantize-coord (aref vec 1))
              :for y = (dequantize-coord (aref vec 2))
              :for entity-zone-hash = (player-compact-zone-hash vec)
              :for existing = (and players (find-player-by-id-fast game id))
              ;; Client-side zone filter: skip if zone-hash mismatch (0 = unset/legacy, allow)
              :when (or (null local-zone-hash)
                        (zerop entity-zone-hash)
                        (= entity-zone-hash local-zone-hash))
              :do (progn
                    ;; Track position from snapshot (not interpolated)
                    (setf (gethash id updated-positions) (list x y))
                    (if existing
                        ;; Update existing player directly from compact vector (no plist)
                        (apply-player-compact-direct existing vec)
                        ;; New player - create plist for full deserialization
                        (let ((plist (deserialize-player-compact vec)))
                          (push (deserialize-player plist
                                                    *inventory-size*
                                                    (length *equipment-slot-ids*))
                                new-players)))))
        ;; Second pass: if we have new players, expand the array
        (when new-players
          (let* ((old-count (if players (length players) 0))
                 (new-count (+ old-count (length new-players)))
                 (expanded (make-array new-count)))
            ;; Copy existing players
            (when players
              (loop :for i :from 0 :below old-count
                    :do (setf (aref expanded i) (aref players i))))
            ;; Add new players
            (loop :for player :in new-players
                  :for i :from old-count
                  :do (setf (aref expanded i) player))
            ;; Update both players and entities arrays
            (setf (game-players game) expanded
                  (game-entities game)
                  (make-entities expanded (game-npcs game)))
            ;; Rebuild player index map after expanding array
            (rebuild-player-index-map game)))))
    ;; Apply changed NPCs - find by ID and update
    ;; NOTE: NPC IDs are entity IDs, not array indices. Must search for matching NPC.
    ;; Client-side zone filter: skip NPCs not in local player's zone (defense-in-depth).
    ;; Optimization: extract values directly from vector, skip plist creation
    (when changed-npcs
      (let ((npcs (game-npcs game)))
        (when npcs
          (loop :for vec :across changed-npcs
                :for id = (aref vec 0)
                :for x = (dequantize-coord (aref vec 1))
                :for y = (dequantize-coord (aref vec 2))
                :for npc-zone-hash = (if (>= (length vec) 15) (aref vec 14) 0)
                :for index = (position id npcs :key #'npc-id)
                ;; Client-side zone filter: skip if zone-hash mismatch (0 = unset/legacy, allow)
                :when (and index
                           (or (null local-zone-hash)
                               (zerop npc-zone-hash)
                               (= npc-zone-hash local-zone-hash)))
                :do (progn
                      ;; Track NPC position (use negative ID like capture-entity-positions)
                      (setf (gethash (- id) updated-positions) (list x y))
                      (apply-npc-compact-direct (aref npcs index) vec))))))
    ;; Apply object states from server (authoritative - replaces client objects)
    ;; This ensures dropped items appear and picked up items disappear
    (let ((object-plists (getf delta :objects)))
      (when object-plists
        (log-verbose "DELTA-DESER: received ~a objects" (length object-plists))
        (let* ((world (game-world game))
               (zone (and world (world-zone world))))
          (when zone
            ;; Replace client zone objects with server state
            ;; Task 5.5: Convert server plists to zone-object structs
            (setf (zone-objects zone)
                  (loop :for server-obj :in object-plists
                        :collect (make-zone-object-from-plist server-obj)))))))
    ;; Step 8: Process edge strips in delta snapshots (always full, not deltas)
    (deserialize-edge-strips delta game)
    ;; Return updated positions for interpolation buffer
    updated-positions)))
