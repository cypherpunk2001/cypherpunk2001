(in-package :mmorpg)

;;; save.lisp - Save/load game state serialization
;;;
;;; Provides deterministic snapshot serialization for:
;;; - Player state (position, HP, stats, inventory, equipment, timers)
;;; - NPC state (position, HP, respawn timers, behavior)
;;; - Object state (counts, respawn timers)
;;; - World context (zone ID, entity ID source)
;;;
;;; Format is versioned plist for future-proof migrations.
;;; This becomes the snapshot sync format for future networking.

(defparameter *save-format-version* 2
  "Current save file format version for migration support.")

;;; Serialization helpers

(defun serialize-skill (skill)
  ;; Convert skill to plist.
  (when skill
    (list :level (skill-level skill)
          :xp (skill-xp skill))))

(defun deserialize-skill (plist)
  ;; Restore skill from plist.
  (when plist
    (make-skill :level (getf plist :level 1)
                :xp (getf plist :xp 0))))

(defun serialize-stat-block (stats)
  ;; Convert stat-block to plist.
  (when stats
    (list :attack (serialize-skill (stat-block-attack stats))
          :strength (serialize-skill (stat-block-strength stats))
          :defense (serialize-skill (stat-block-defense stats))
          :hitpoints (serialize-skill (stat-block-hitpoints stats)))))

(defun deserialize-stat-block (plist)
  ;; Restore stat-block from plist.
  (when plist
    (make-stat-block
     :attack (deserialize-skill (getf plist :attack))
     :strength (deserialize-skill (getf plist :strength))
     :defense (deserialize-skill (getf plist :defense))
     :hitpoints (deserialize-skill (getf plist :hitpoints)))))

(defun serialize-inventory-slot (slot)
  ;; Convert inventory slot to plist.
  (when slot
    (list :item-id (inventory-slot-item-id slot)
          :count (inventory-slot-count slot))))

(defun deserialize-inventory-slot (plist)
  ;; Restore inventory slot from plist.
  (when plist
    (make-inventory-slot
     :item-id (getf plist :item-id)
     :count (getf plist :count 0))))

(defun serialize-inventory (inventory)
  ;; Convert inventory to plist.
  (when inventory
    (let* ((slots (inventory-slots inventory))
           (slot-count (and slots (length slots)))
           (slot-list nil))
      (when slots
        (loop :for i :below slot-count
              :for slot = (aref slots i)
              :when slot
                :do (push (serialize-inventory-slot slot) slot-list)))
      (list :slots (nreverse slot-list)))))

(defun deserialize-inventory (plist size)
  ;; Restore inventory from plist with SIZE slots.
  (let ((slots (make-array size :initial-element nil)))
    (when plist
      (let ((slot-list (getf plist :slots)))
        (loop :for slot-plist :in slot-list
              :for i :from 0
              :when (< i size)
                :do (setf (aref slots i) (deserialize-inventory-slot slot-plist)))))
    (%make-inventory :slots slots)))

(defun serialize-equipment (equipment)
  ;; Convert equipment to plist (array of item IDs).
  (when equipment
    (let* ((items (equipment-items equipment))
           (item-list (and items (coerce items 'list))))
      (list :items item-list))))

(defun deserialize-equipment (plist size)
  ;; Restore equipment from plist with SIZE slots.
  (let ((items (make-array size :initial-element nil)))
    (when plist
      (let ((item-list (getf plist :items)))
        (loop :for item-id :in item-list
              :for i :from 0
              :when (< i size)
                :do (setf (aref items i) item-id))))
    (%make-equipment :items items)))

(defun serialize-player (player &key (include-visuals nil) (zone-id nil))
  ;; Convert player state to plist (server-authoritative state only).
  ;; Ephemeral fields (attack/hit timers, targets, run stamina) are excluded from DB saves.
  ;; zone-id should be provided for DB persistence to support multi-zone login.
  (let ((payload (list :id (player-id player)
                       :x (player-x player)
                       :y (player-y player)
                       :hp (player-hp player)
                       :stats (serialize-stat-block (player-stats player))
                       :inventory (serialize-inventory (player-inventory player))
                       :equipment (serialize-equipment (player-equipment player)))))
    ;; Add zone-id to durable state (for DB saves)
    (when zone-id
      (setf payload (append payload (list :zone-id zone-id))))
    (when include-visuals
      (setf payload (append payload
                            (list :dx (player-dx player)
                                  :dy (player-dy player)
                                  :anim-state (player-anim-state player)
                                  :facing (player-facing player)
                                  :facing-sign (player-facing-sign player)
                                  :frame-index (player-frame-index player)
                                  :frame-timer (player-frame-timer player)
                                  :attacking (player-attacking player)
                                  :attack-hit (player-attack-hit player)
                                  :hit-active (player-hit-active player)
                                  :hit-frame (player-hit-frame player)
                                  :hit-facing (player-hit-facing player)
                                  :hit-facing-sign (player-hit-facing-sign player)
                                  :running (player-running player)
                                  ;; Ephemeral fields (for network snapshots only, not DB)
                                  :attack-timer (player-attack-timer player)
                                  :hit-timer (player-hit-timer player)
                                  :run-stamina (player-run-stamina player)
                                  :attack-target-id (player-attack-target-id player)
                                  :follow-target-id (player-follow-target-id player)))))
    payload))

(defun deserialize-player (plist inventory-size equipment-size)
  ;; Restore player from plist.
  (let ((player (make-player (getf plist :x 0.0)
                             (getf plist :y 0.0)
                             :id (getf plist :id 1))))
    (setf (player-hp player) (getf plist :hp 10)
          (player-stats player) (deserialize-stat-block (getf plist :stats))
          (player-inventory player) (deserialize-inventory (getf plist :inventory) inventory-size)
          (player-equipment player) (deserialize-equipment (getf plist :equipment) equipment-size)
          (player-attack-timer player) (getf plist :attack-timer 0.0)
          (player-hit-timer player) (getf plist :hit-timer 0.0)
          (player-run-stamina player) (getf plist :run-stamina 1.0)
          (player-attack-target-id player) (getf plist :attack-target-id 0)
          (player-follow-target-id player) (getf plist :follow-target-id 0)
          (player-dx player) (getf plist :dx 0.0)
          (player-dy player) (getf plist :dy 0.0)
          (player-anim-state player) (getf plist :anim-state :idle)
          (player-facing player) (getf plist :facing :down)
          (player-facing-sign player) (getf plist :facing-sign 1.0)
          (player-frame-index player) (getf plist :frame-index 0)
          (player-frame-timer player) (getf plist :frame-timer 0.0)
          (player-attacking player) (getf plist :attacking nil)
          (player-attack-hit player) (getf plist :attack-hit nil)
          (player-hit-active player) (getf plist :hit-active nil)
          (player-hit-frame player) (getf plist :hit-frame 0)
          (player-hit-facing player) (getf plist :hit-facing :down)
          (player-hit-facing-sign player) (getf plist :hit-facing-sign 1.0)
          (player-running player) (getf plist :running nil))
    player))

(defun apply-player-plist (player plist)
  ;; Apply plist fields onto an existing PLAYER, preserving client-only state.
  (when (and player plist)
    ;; Save client-only state before applying server snapshot
    (let ((saved-click-x (player-click-marker-x player))
          (saved-click-y (player-click-marker-y player))
          (saved-click-kind (player-click-marker-kind player))
          (saved-click-timer (player-click-marker-timer player))
          (saved-mouse-timer (player-mouse-hold-timer player))
          (saved-auto-right (player-auto-right player))
          (saved-auto-left (player-auto-left player))
          (saved-auto-down (player-auto-down player))
          (saved-auto-up (player-auto-up player)))
      (setf (player-id player) (getf plist :id (player-id player))
            (player-x player) (getf plist :x (player-x player))
            (player-y player) (getf plist :y (player-y player))
            (player-dx player) (getf plist :dx 0.0)
            (player-dy player) (getf plist :dy 0.0)
            (player-hp player) (getf plist :hp (player-hp player))
            (player-stats player) (deserialize-stat-block (getf plist :stats))
            (player-inventory player)
            (deserialize-inventory (getf plist :inventory) *inventory-size*)
            (player-equipment player)
            (deserialize-equipment (getf plist :equipment) (length *equipment-slot-ids*))
            (player-attack-timer player) (getf plist :attack-timer 0.0)
            (player-hit-timer player) (getf plist :hit-timer 0.0)
            (player-run-stamina player) (getf plist :run-stamina 1.0)
            (player-attack-target-id player) (getf plist :attack-target-id 0)
            (player-follow-target-id player) (getf plist :follow-target-id 0)
            (player-anim-state player) (getf plist :anim-state :idle)
            (player-facing player) (getf plist :facing :down)
            (player-facing-sign player) (getf plist :facing-sign 1.0)
            (player-frame-index player) (getf plist :frame-index 0)
            (player-frame-timer player) (getf plist :frame-timer 0.0)
            (player-attacking player) (getf plist :attacking nil)
            (player-attack-hit player) (getf plist :attack-hit nil)
            (player-hit-active player) (getf plist :hit-active nil)
            (player-hit-frame player) (getf plist :hit-frame 0)
            (player-hit-facing player) (getf plist :hit-facing :down)
            (player-hit-facing-sign player) (getf plist :hit-facing-sign 1.0)
            (player-running player) (getf plist :running nil))
      ;; Restore client-only state after applying server snapshot
      (setf (player-click-marker-x player) saved-click-x
            (player-click-marker-y player) saved-click-y
            (player-click-marker-kind player) saved-click-kind
            (player-click-marker-timer player) saved-click-timer
            (player-mouse-hold-timer player) saved-mouse-timer
            (player-auto-right player) saved-auto-right
            (player-auto-left player) saved-auto-left
            (player-auto-down player) saved-auto-down
            (player-auto-up player) saved-auto-up)))
  player)

(defun serialize-npc (npc &key (include-visuals nil))
  ;; Convert NPC state to plist (server-authoritative state only).
  (let ((payload (list :id (npc-id npc)
                       :x (npc-x npc)
                       :y (npc-y npc)
                       :home-x (npc-home-x npc)
                       :home-y (npc-home-y npc)
                       :hits-left (npc-hits-left npc)
                       :alive (npc-alive npc)
                       :respawn-timer (npc-respawn-timer npc)
                       :provoked (npc-provoked npc)
                       :behavior-state (npc-behavior-state npc)
                       :attack-timer (npc-attack-timer npc))))
    (when include-visuals
      (setf payload (append payload
                            (list :anim-state (npc-anim-state npc)
                                  :facing (npc-facing npc)
                                  :frame-index (npc-frame-index npc)
                                  :frame-timer (npc-frame-timer npc)
                                  :hit-active (npc-hit-active npc)
                                  :hit-timer (npc-hit-timer npc)
                                  :hit-frame (npc-hit-frame npc)
                                  :hit-facing (npc-hit-facing npc)
                                  :hit-facing-sign (npc-hit-facing-sign npc)))))
    payload))

(defun deserialize-npc (plist npcs index)
  ;; Restore NPC from plist into existing NPC array at INDEX.
  (when (and plist npcs (< index (length npcs)))
    (let ((npc (aref npcs index)))
      (when npc
        (setf (npc-id npc) (getf plist :id 0)
              (npc-x npc) (getf plist :x 0.0)
              (npc-y npc) (getf plist :y 0.0)
              (npc-home-x npc) (getf plist :home-x 0.0)
              (npc-home-y npc) (getf plist :home-y 0.0)
              (npc-hits-left npc) (getf plist :hits-left 1)
              (npc-alive npc) (getf plist :alive t)
              (npc-respawn-timer npc) (getf plist :respawn-timer 0.0)
              (npc-provoked npc) (getf plist :provoked nil)
              (npc-behavior-state npc) (getf plist :behavior-state :idle)
              (npc-attack-timer npc) (getf plist :attack-timer 0.0)
              (npc-anim-state npc) (getf plist :anim-state :idle)
              (npc-facing npc) (getf plist :facing :down)
              (npc-frame-index npc) (getf plist :frame-index 0)
              (npc-frame-timer npc) (getf plist :frame-timer 0.0)
              (npc-hit-active npc) (getf plist :hit-active nil)
              (npc-hit-timer npc) (getf plist :hit-timer 0.0)
              (npc-hit-frame npc) (getf plist :hit-frame 0)
              (npc-hit-facing npc) (getf plist :hit-facing :down)
              (npc-hit-facing-sign npc) (getf plist :hit-facing-sign 1.0))))))

(defun players-match-order-p (players plists)
  ;; Return true when PLAYERS and PLISTS share the same ID ordering.
  (and players
       (= (length players) (length plists))
       (loop :for i :from 0 :below (length players)
             :for plist :in plists
             :for id = (getf plist :id 0)
             :always (and (> id 0)
                          (= (player-id (aref players i)) id)))))

(defun apply-player-plists (game player-plists)
  ;; Apply PLAYER-PLISTS to GAME players, preserving local player state.
  (when player-plists
    (let* ((players (game-players game))
           (reuse-order (players-match-order-p players player-plists))
           (players-changed nil))
      (if reuse-order
          (loop :for i :from 0 :below (length players)
                :for plist :in player-plists
                :do (apply-player-plist (aref players i) plist))
          (let ((new-players (make-array (length player-plists)))
                (old-local-player (game-player game)))
            (loop :for plist :in player-plists
                  :for index :from 0
                  :for id = (getf plist :id 0)
                  :for existing = (and players (find-player-by-id players id))
                  :do (if existing
                          (apply-player-plist existing plist)
                          (progn
                            (setf existing
                                  (deserialize-player plist
                                                      *inventory-size*
                                                      (length *equipment-slot-ids*)))
                            ;; Preserve client-only state from old local player when creating new player
                            (when old-local-player
                              (setf (player-click-marker-x existing) (player-click-marker-x old-local-player)
                                    (player-click-marker-y existing) (player-click-marker-y old-local-player)
                                    (player-click-marker-kind existing) (player-click-marker-kind old-local-player)
                                    (player-click-marker-timer existing) (player-click-marker-timer old-local-player)
                                    (player-mouse-hold-timer existing) (player-mouse-hold-timer old-local-player)
                                    (player-auto-right existing) (player-auto-right old-local-player)
                                    (player-auto-left existing) (player-auto-left old-local-player)
                                    (player-auto-down existing) (player-auto-down old-local-player)
                                    (player-auto-up existing) (player-auto-up old-local-player)))))
                      (setf (aref new-players index) existing))
            (setf (game-players game) new-players
                  players new-players
                  players-changed t)))
      (let* ((local-id (or (game-net-player-id game)
                           (and (game-player game)
                                (player-id (game-player game)))))
             (local-player (and local-id (find-player-by-id players local-id))))
        (when (and local-player (not (eq (game-player game) local-player)))
          (setf (game-player game) local-player))
        (when (and (null local-player) (> (length players) 0))
          (setf (game-player game) (aref players 0))))
      (when players-changed
        (setf (game-entities game)
              (make-entities (game-players game) (game-npcs game)))))))

(defun serialize-object (object)
  ;; Convert zone object to plist (ID, position, count, respawn timer).
  (list :id (getf object :id)
        :x (getf object :x)
        :y (getf object :y)
        :count (getf object :count 1)
        :respawn (getf object :respawn 0.0)
        :respawnable (getf object :respawnable t)))

(defun deserialize-object (plist)
  ;; Restore zone object from plist.
  (list :id (getf plist :id)
        :x (getf plist :x)
        :y (getf plist :y)
        :count (getf plist :count 1)
        :respawn (getf plist :respawn 0.0)
        :respawnable (getf plist :respawnable t)))

(defun serialize-game-state (game &key (include-visuals nil))
  ;; Serialize authoritative game state to plist (server snapshot).
  (let* ((players (game-players game))
         (npcs (game-npcs game))
         (world (game-world game))
         (zone (world-zone world))
         (id-source (game-id-source game))
         (player-list nil)
         (npc-list nil)
         (object-list nil))
    ;; Serialize players
    (when players
      (loop :for player :across players
            :do (push (serialize-player player :include-visuals include-visuals) player-list)))
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
    zone-id))

(defun apply-game-state (game state &key (apply-zone t))
  ;; Apply serialized STATE into GAME, loading zones when needed.
  (when (and game state)
    (let* ((zone-id (getf state :zone-id))
           (world (game-world game))
           (current-zone (and world (world-zone world)))
           (current-zone-id (and current-zone (zone-id current-zone)))
           (zone-loaded nil))
      (when (and apply-zone zone-id (not (eql zone-id current-zone-id)))
        (let* ((graph (and world (world-world-graph world)))
               (path (and graph (world-graph-zone-path graph zone-id))))
          (when (and path (probe-file path))
            (let ((zone (load-zone path)))
              (when zone
                (setf *zone-path* path)
                (apply-zone-to-world world zone)
                (setf zone-loaded t))))))
      (when zone-loaded
        (let* ((player (game-player game))
               (players (game-players game))
               (npcs (make-npcs player world
                                :id-source (game-npc-id-source game))))
          (ensure-npcs-open-spawn npcs world)
          (setf (game-npcs game) npcs
                (game-entities game) (make-entities players npcs))))
      (values (deserialize-game-state state game) zone-loaded))))

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
