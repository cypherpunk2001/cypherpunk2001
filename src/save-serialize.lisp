(in-package :mmorpg)

;;; save-serialize.lisp - Serialization functions and shared constants
;;;
;;; Loaded FIRST among save-* files. Contains:
;;; - Shared constants/parameters used across save-* files
;;; - All serialize-* functions (player, NPC, object, compact, etc.)
;;; - Quantization/encoding helpers for compact serialization
;;; - Vector pool management

(defparameter *save-format-version* 2
  "Current save file format version for migration support.")

;;; Vector Pool for Compact Serialization (Task 4.2)
;;; Reduces allocation during hot snapshot path.

(defparameter *player-compact-vector-size* 20
  "Size of compact player serialization vector (indices 0-19).")

(defparameter *npc-compact-vector-size* 15
  "Size of compact NPC serialization vector (indices 0-14).")

(defparameter *player-vector-pool-capacity* 600
  "Initial capacity for player vector pool. Sized for ~500 players per active zone snapshot + headroom.")

(defvar *player-vector-pool* nil
  "Global vector pool for player compact serialization. Created lazily on first use.")

;;; Serialization helpers

(defun serialize-skill (skill)
  ;; Convert skill to plist.
  (when skill
    (list :level (skill-level skill)
          :xp (skill-xp skill))))

(defun serialize-stat-block (stats)
  ;; Convert stat-block to plist.
  (when stats
    (list :attack (serialize-skill (stat-block-attack stats))
          :strength (serialize-skill (stat-block-strength stats))
          :defense (serialize-skill (stat-block-defense stats))
          :hitpoints (serialize-skill (stat-block-hitpoints stats)))))

(defun serialize-inventory-slot (slot)
  ;; Convert inventory slot to plist.
  (when slot
    (list :item-id (inventory-slot-item-id slot)
          :count (inventory-slot-count slot))))

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

(defun serialize-equipment (equipment)
  ;; Convert equipment to plist (array of item IDs).
  (when equipment
    (let* ((items (equipment-items equipment))
           (item-list (and items (coerce items 'list))))
      (list :items item-list))))

(defun serialize-player (player &key (include-visuals nil) (zone-id nil) (network-only nil))
  ;; Convert player state to plist.
  ;; Modes:
  ;;   - Default: Full durable state for DB saves (inventory, equipment, stats, etc.)
  ;;   - :include-visuals t: Add visual/ephemeral fields for rendering
  ;;   - :network-only t: Minimal state for network snapshots (position + visuals only)
  ;;                      Excludes inventory/equipment/stats to reduce snapshot size.
  ;;                      Critical for 60+ player scalability (72KB -> ~15KB).
  (let ((payload
          (if network-only
              ;; NETWORK-ONLY: Minimal state for rendering other players
              ;; Excludes: inventory, equipment, stats, playtime, created-at, lifetime-xp
              (list :id (player-id player)
                    :x (player-x player)
                    :y (player-y player)
                    :hp (player-hp player)
                    :dx (player-dx player)
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
                    :attack-timer (player-attack-timer player)
                    :hit-timer (player-hit-timer player)
                    :run-stamina (player-run-stamina player)
                    :last-sequence (player-last-sequence player))
              ;; FULL: Complete durable state for DB saves
              (list :id (player-id player)
                    :x (player-x player)
                    :y (player-y player)
                    :hp (player-hp player)
                    :lifetime-xp (player-lifetime-xp player)
                    :playtime (player-playtime player)
                    :created-at (player-created-at player)
                    :deaths (player-deaths player)  ; Phase 4 - leaderboards
                    :stats (serialize-stat-block (player-stats player))
                    :inventory (serialize-inventory (player-inventory player))
                    :equipment (serialize-equipment (player-equipment player))))))
    ;; Add zone-id to durable state (for DB saves)
    ;; Use player's zone-id if available, otherwise use keyword argument
    (let ((effective-zone-id (or (player-zone-id player) zone-id)))
      (when (and effective-zone-id (not network-only))
        (setf payload (append payload (list :zone-id effective-zone-id)))))
    ;; Add visual fields if requested (for full snapshots, not network-only which already has them)
    (when (and include-visuals (not network-only))
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
                                  :run-stamina (player-run-stamina player)))))
    payload))

(defun serialize-player-private (player)
  ;; Serialize private player state (inventory/equipment/stats) for owner-only updates.
  (when player
    (list :stats (serialize-stat-block (player-stats player))
          :inventory (serialize-inventory (player-inventory player))
          :equipment (serialize-equipment (player-equipment player)))))

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

;;; Zone Object Serialization

(defun serialize-object (object)
  ;; Convert zone object struct to plist (ID, position, count, respawn timer).
  ;; Task 5.5: Use zone-object struct accessors for O(1) field access.
  (list :id (zone-object-id object)
        :x (zone-object-x object)
        :y (zone-object-y object)
        :count (zone-object-count object)
        :respawn (zone-object-respawn object)
        :respawnable (zone-object-respawnable object)))

;;;; ========================================================================
;;;; COMPACT SERIALIZATION (Network Optimization - see docs/net.md 4-Prong)
;;;; Uses positional vectors instead of plists to minimize snapshot size.
;;;; Target: <100 bytes per entity (vs ~232 bytes with plist format)
;;;; ========================================================================

;;; Helper functions for quantization and enum encoding

(defun quantize-coord (f)
  "Quantize coordinate to 0.1 pixel precision for network transmission."
  (round (* (or f 0.0) *coord-scale*)))

(defun dequantize-coord (i)
  "Restore coordinate from quantized integer."
  (/ (or i 0) (float *coord-scale*)))

(defun quantize-timer (f)
  "Quantize timer to 0.01 second precision for network transmission."
  (round (* (or f 0.0) *timer-scale*)))

(defun dequantize-timer (i)
  "Restore timer from quantized integer."
  (/ (or i 0) (float *timer-scale*)))

(defun encode-anim-state (state)
  "Encode animation state keyword to integer code."
  (or (cdr (assoc state *anim-state-to-code*)) 0))

(defun decode-anim-state (code)
  "Decode integer code to animation state keyword."
  (or (cdr (assoc code *code-to-anim-state*)) :idle))

(defun encode-facing (facing)
  "Encode facing direction keyword to integer code."
  (or (cdr (assoc facing *facing-to-code*)) 1))  ; default :down

(defun decode-facing (code)
  "Decode integer code to facing direction keyword."
  (or (cdr (assoc code *code-to-facing*)) :down))

(defun pack-player-flags (player)
  "Pack player boolean flags into a single integer.
   Bit 0: attacking, Bit 1: attack-hit, Bit 2: hit-active, Bit 3: running"
  (logior (if (player-attacking player) 1 0)
          (if (player-attack-hit player) 2 0)
          (if (player-hit-active player) 4 0)
          (if (player-running player) 8 0)))

(defun unpack-player-flags (flags)
  "Unpack player boolean flags from integer.
   Returns (values attacking attack-hit hit-active running)"
  (values (logbitp 0 flags)
          (logbitp 1 flags)
          (logbitp 2 flags)
          (logbitp 3 flags)))

(defun pack-npc-flags (npc)
  "Pack NPC boolean flags into a single integer.
   Bit 0: alive, Bit 1: provoked, Bit 2: hit-active"
  (logior (if (npc-alive npc) 1 0)
          (if (npc-provoked npc) 2 0)
          (if (npc-hit-active npc) 4 0)))

(defun unpack-npc-flags (flags)
  "Unpack NPC boolean flags from integer.
   Returns (values alive provoked hit-active)"
  (values (logbitp 0 flags)
          (logbitp 1 flags)
          (logbitp 2 flags)))

;;; Compact Player Serialization (v5)
;;; Vector format (20 elements):
;;; [0] id           [1] x*10        [2] y*10        [3] hp
;;; [4] dx*10        [5] dy*10       [6] anim-code   [7] facing-code
;;; [8] facing-sign  [9] frame-idx   [10] frame-timer*100
;;; [11] flags       [12] attack-timer*100  [13] hit-timer*100
;;; [14] hit-frame   [15] hit-facing-code   [16] hit-facing-sign
;;; [17] run-stamina*100  [18] last-sequence  [19] zone-id-hash
;;; v4 compatibility: vectors with length >= 22 include attack/follow targets
;;; at indices 17/18 and zone-id-hash at index 21.

(defun encode-zone-id (zone-id)
  "Encode zone-id symbol to integer for compact serialization.
   Returns 0 for nil, otherwise a deterministic hash of the symbol name.
   Uses djb2 hash algorithm for cross-process stability (sxhash is not stable)."
  (if zone-id
      (let* ((name (symbol-name zone-id))
             (hash 5381))  ; djb2 initial value
        (loop :for char :across name
              :do (setf hash (logand (+ (ash hash 5) hash (char-code char))
                                     #xFFFFFFFF)))  ; Keep 32-bit
        (logand hash #xFFFF))  ; Return 16-bit
      0))

(defun player-compact-zone-hash (vec)
  "Return zone-id-hash from compact player vector (v4/v5)."
  (let ((len (length vec)))
    (cond
      ((>= len 22) (aref vec 21))  ; v4
      ((>= len 20) (aref vec 19))  ; v5
      (t 0))))

(defun get-player-vector-pool ()
  "Get or lazily create the global player vector pool."
  (or *player-vector-pool*
      (setf *player-vector-pool*
            (make-vector-pool *player-vector-pool-capacity*
                              *player-compact-vector-size*))))

(defun reset-player-vector-pool ()
  "Reset the player vector pool for a new serialization pass.
   Call this before serializing players for a zone."
  (when *player-vector-pool*
    (reset-vector-pool *player-vector-pool*)))

(defun serialize-player-compact (player &key use-pool)
  "Serialize player to compact vector for network transmission.
   Excludes inventory (private to owning client).
   Format v5: Removed attack/follow target IDs from public snapshots.
   When USE-POOL is T, uses pre-allocated vector from pool (Task 4.2)."
  (let ((vec (if use-pool
                 (acquire-pooled-vector (get-player-vector-pool)
                                        *player-compact-vector-size*)
                 (make-array *player-compact-vector-size* :initial-element 0))))
    (setf (aref vec 0) (player-id player)
          (aref vec 1) (quantize-coord (player-x player))
          (aref vec 2) (quantize-coord (player-y player))
          (aref vec 3) (or (player-hp player) 0)
          (aref vec 4) (quantize-coord (player-dx player))
          (aref vec 5) (quantize-coord (player-dy player))
          (aref vec 6) (encode-anim-state (player-anim-state player))
          (aref vec 7) (encode-facing (player-facing player))
          (aref vec 8) (round (or (player-facing-sign player) 1.0))
          (aref vec 9) (or (player-frame-index player) 0)
          (aref vec 10) (quantize-timer (player-frame-timer player))
          (aref vec 11) (pack-player-flags player)
          (aref vec 12) (quantize-timer (player-attack-timer player))
          (aref vec 13) (quantize-timer (player-hit-timer player))
          (aref vec 14) (or (player-hit-frame player) 0)
          (aref vec 15) (encode-facing (player-hit-facing player))
          (aref vec 16) (round (or (player-hit-facing-sign player) 1.0))
          (aref vec 17) (quantize-timer (player-run-stamina player))
          (aref vec 18) (or (player-last-sequence player) 0)
          (aref vec 19) (encode-zone-id (player-zone-id player)))
    vec))

;;; Compact NPC Serialization
;;; Vector format (14 elements):
;;; [0] id           [1] x*10        [2] y*10        [3] hits-left
;;; [4] flags        [5] behavior-state-code  [6] attack-timer*100
;;; [7] anim-code    [8] facing-code [9] frame-idx   [10] frame-timer*100
;;; [11] hit-timer*100  [12] hit-frame  [13] hit-facing-code

(defun encode-behavior-state (state)
  "Encode NPC behavior state to integer code."
  (case state
    (:idle 0) (:wandering 1) (:chasing 2) (:attacking 3)
    (:fleeing 4) (:returning 5) (:dead 6) (otherwise 0)))

(defun decode-behavior-state (code)
  "Decode integer code to NPC behavior state."
  (case code
    (0 :idle) (1 :wandering) (2 :chasing) (3 :attacking)
    (4 :fleeing) (5 :returning) (6 :dead) (otherwise :idle)))

;;; Compact NPC Serialization (v4)
;;; Vector format (15 elements):
;;; [0] id        [1] x*10       [2] y*10       [3] hits-left  [4] flags
;;; [5] behavior  [6] attack-timer*100  [7] anim-code  [8] facing-code
;;; [9] frame-idx [10] frame-timer*100  [11] hit-timer*100  [12] hit-frame
;;; [13] hit-facing-code  [14] zone-id-hash

(defun serialize-npc-compact (npc &optional zone-id)
  "Serialize NPC to compact vector for network transmission.
   ZONE-ID is the zone containing this NPC (for client-side filtering)."
  (vector (npc-id npc)
          (quantize-coord (npc-x npc))
          (quantize-coord (npc-y npc))
          (or (npc-hits-left npc) 0)
          (pack-npc-flags npc)
          (encode-behavior-state (npc-behavior-state npc))
          (quantize-timer (npc-attack-timer npc))
          (encode-anim-state (npc-anim-state npc))
          (encode-facing (npc-facing npc))
          (or (npc-frame-index npc) 0)
          (quantize-timer (npc-frame-timer npc))
          (quantize-timer (npc-hit-timer npc))
          (or (npc-hit-frame npc) 0)
          (encode-facing (npc-hit-facing npc))
          (encode-zone-id zone-id)))

;;; Compact Game State Serialization

(defun serialize-game-state-compact (game)
  "Serialize game state using compact format for network transmission.
   Returns plist with :format :compact-v5 and vector-based entity data.
   v5 removes attack/follow target IDs from public player vectors and keeps
   zone-id-hash for client-side zone filtering as defense-in-depth."
  (let* ((players (game-players game))
         (npcs (game-npcs game))
         (world (game-world game))
         (zone (world-zone world))
         (objects (and zone (zone-objects zone)))
         (player-vectors nil)
         (npc-vectors nil)
         (object-list nil))
    ;; Serialize players to compact vectors
    (when players
      (loop :for player :across players
            :when player
            :do (push (serialize-player-compact player) player-vectors)))
    ;; Serialize NPCs to compact vectors (with zone-id for client filtering)
    (let ((npc-zone-id (and zone (zone-id zone))))
      (when npcs
        (loop :for npc :across npcs
              :when npc
              :do (push (serialize-npc-compact npc npc-zone-id) npc-vectors))))
    ;; Serialize zone objects (all objects - dropped items and respawning pickups)
    (when objects
      (dolist (object objects)
        (push (serialize-object object) object-list)))
    ;; Build compact snapshot
    (list :format :compact-v5
          :zone-id (and zone (zone-id zone))
          :players (coerce (nreverse player-vectors) 'vector)
          :npcs (coerce (nreverse npc-vectors) 'vector)
          :objects (nreverse object-list))))
