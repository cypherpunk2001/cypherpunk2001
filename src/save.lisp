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

(defparameter *save-format-version* 1
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

(defun serialize-player (player)
  ;; Convert player state to plist (server-authoritative state only).
  (list :id (player-id player)
        :x (player-x player)
        :y (player-y player)
        :hp (player-hp player)
        :stats (serialize-stat-block (player-stats player))
        :inventory (serialize-inventory (player-inventory player))
        :equipment (serialize-equipment (player-equipment player))
        :attack-timer (player-attack-timer player)
        :hit-timer (player-hit-timer player)
        :run-stamina (player-run-stamina player)
        :attack-target-id (player-attack-target-id player)
        :follow-target-id (player-follow-target-id player)))

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
          (player-follow-target-id player) (getf plist :follow-target-id 0))
    player))

(defun serialize-npc (npc)
  ;; Convert NPC state to plist (server-authoritative state only).
  (list :id (npc-id npc)
        :x (npc-x npc)
        :y (npc-y npc)
        :home-x (npc-home-x npc)
        :home-y (npc-home-y npc)
        :hits-left (npc-hits-left npc)
        :alive (npc-alive npc)
        :respawn-timer (npc-respawn-timer npc)
        :provoked (npc-provoked npc)
        :behavior-state (npc-behavior-state npc)
        :attack-timer (npc-attack-timer npc)))

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
              (npc-attack-timer npc) (getf plist :attack-timer 0.0))))))

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

(defun serialize-game-state (game)
  ;; Serialize authoritative game state to plist (server snapshot).
  (let* ((player (game-player game))
         (npcs (game-npcs game))
         (world (game-world game))
         (zone (world-zone world))
         (id-source (game-id-source game))
         (npc-list nil)
         (object-list nil))
    ;; Serialize NPCs
    (when npcs
      (loop :for npc :across npcs
            :do (push (serialize-npc npc) npc-list)))
    ;; Serialize zone objects
    (when zone
      (let ((objects (zone-objects zone)))
        (dolist (object objects)
          (push (serialize-object object) object-list))))
    ;; Build save state
    (list :version *save-format-version*
          :zone-id (and zone (zone-id zone))
          :id-next (and id-source (id-source-next-id id-source))
          :player (serialize-player player)
          :npcs (nreverse npc-list)
          :objects (nreverse object-list))))

(defun deserialize-game-state (plist game)
  ;; Restore authoritative game state from plist into existing GAME.
  (let* ((version (getf plist :version 0))
         (zone-id (getf plist :zone-id))
         (id-next (getf plist :id-next 1))
         (player-plist (getf plist :player))
         (npc-plists (getf plist :npcs))
         (object-plists (getf plist :objects))
         (player (game-player game))
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
    ;; Restore player
    (when player-plist
      (let ((restored-player (deserialize-player player-plist
                                                 *player-inventory-size*
                                                 *equipment-slot-count*)))
        (setf (player-x player) (player-x restored-player)
              (player-y player) (player-y restored-player)
              (player-hp player) (player-hp restored-player)
              (player-stats player) (player-stats restored-player)
              (player-inventory player) (player-inventory restored-player)
              (player-equipment player) (player-equipment restored-player)
              (player-attack-timer player) (player-attack-timer restored-player)
              (player-hit-timer player) (player-hit-timer restored-player)
              (player-run-stamina player) (player-run-stamina restored-player)
              (player-attack-target-id player) (player-attack-target-id restored-player)
              (player-follow-target-id player) (player-follow-target-id restored-player))))
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

(defun save-game (game filepath)
  ;; Save game state to FILEPATH.
  (let ((state (serialize-game-state game)))
    (with-open-file (out filepath
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (prin1 state out))
    (format t "~&Saved game to ~a~%" filepath)
    t))

(defun load-game (game filepath)
  ;; Load game state from FILEPATH into existing GAME.
  (if (probe-file filepath)
      (let ((state (with-open-file (in filepath :direction :input)
                     (read in))))
        (let ((zone-id (deserialize-game-state state game)))
          (format t "~&Loaded game from ~a (zone: ~a)~%" filepath zone-id)
          zone-id))
      (progn
        (warn "Save file not found: ~a" filepath)
        nil)))
