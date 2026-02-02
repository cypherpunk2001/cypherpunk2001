(in-package :mmorpg)

;;; save-deserialize.lisp - Deserialization functions, apply-state, plist->struct conversion
;;;
;;; Depends on save-serialize.lisp for: dequantize-coord, dequantize-timer,
;;; decode-anim-state, decode-facing, decode-behavior-state, unpack-player-flags,
;;; unpack-npc-flags, player-compact-zone-hash, encode-zone-id, serialize-object,
;;; *save-format-version*

;;; Deserialization helpers

(defun deserialize-skill (plist)
  ;; Restore skill from plist.
  (when plist
    (make-skill :level (getf plist :level 1)
                :xp (getf plist :xp 0))))

(defun deserialize-stat-block (plist)
  ;; Restore stat-block from plist.
  (when plist
    (make-stat-block
     :attack (deserialize-skill (getf plist :attack))
     :strength (deserialize-skill (getf plist :strength))
     :defense (deserialize-skill (getf plist :defense))
     :hitpoints (deserialize-skill (getf plist :hitpoints)))))

(defun deserialize-inventory-slot (plist)
  ;; Restore inventory slot from plist.
  (when plist
    (make-inventory-slot
     :item-id (getf plist :item-id)
     :count (getf plist :count 0))))

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

(defun deserialize-player (plist inventory-size equipment-size)
  ;; Restore player from plist.
  (let ((player (make-player (getf plist :x 0.0)
                             (getf plist :y 0.0)
                             :id (getf plist :id 1)
                             :zone-id (getf plist :zone-id))))
    (setf (player-hp player) (getf plist :hp 10)
          (player-zone-id player) (getf plist :zone-id)  ; Also set explicitly for clarity
          (player-lifetime-xp player) (getf plist :lifetime-xp 0)
          (player-playtime player) (float (getf plist :playtime 0) 1.0)
          (player-created-at player) (getf plist :created-at (get-universal-time))
          (player-deaths player) (getf plist :deaths 0)  ; Phase 4 - leaderboards
          (player-stats player) (deserialize-stat-block (getf plist :stats))
          (player-inventory player) (deserialize-inventory (getf plist :inventory) inventory-size)
          (player-equipment player) (deserialize-equipment (getf plist :equipment) equipment-size)
          (player-attack-timer player) (getf plist :attack-timer 0.0)
          (player-hit-timer player) (getf plist :hit-timer 0.0)
          (player-run-stamina player) (getf plist :run-stamina 1.0)
          (player-attack-target-id player) (getf plist :attack-target-id 0)
          (player-follow-target-id player) (getf plist :follow-target-id 0)
          (player-last-sequence player) (getf plist :last-sequence 0)
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
  ;; For network-only snapshots, inventory/equipment/stats may be nil - preserve existing.
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
          (saved-auto-up (player-auto-up player))
          ;; Check which heavy fields are present in plist
          (has-stats (getf plist :stats))
          (has-inventory (getf plist :inventory))
          (has-equipment (getf plist :equipment)))
      ;; NOTE: player-id is NOT set here - IDs are immutable after creation
      ;; (avoids index map desync; plist :id is used for lookup, not to change identity)
      (setf (player-x player) (getf plist :x (player-x player))
            (player-y player) (getf plist :y (player-y player))
            (player-dx player) (getf plist :dx 0.0)
            (player-dy player) (getf plist :dy 0.0)
            (player-hp player) (getf plist :hp (player-hp player))
            (player-lifetime-xp player) (getf plist :lifetime-xp (player-lifetime-xp player))
            (player-playtime player) (getf plist :playtime (player-playtime player))
            (player-created-at player) (getf plist :created-at (player-created-at player))
            ;; Phase 5: Apply deaths from snapshot for client-side display
            (player-deaths player) (getf plist :deaths (player-deaths player))
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
            (player-running player) (getf plist :running nil)
            (player-last-sequence player) (getf plist :last-sequence (player-last-sequence player)))
      ;; Only update heavy fields if present (network-only snapshots exclude these)
      (when has-stats
        (setf (player-stats player) (deserialize-stat-block has-stats)))
      (when has-inventory
        (setf (player-inventory player) (deserialize-inventory has-inventory *inventory-size*)))
      (when has-equipment
        (setf (player-equipment player) (deserialize-equipment has-equipment (length *equipment-slot-ids*))))
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

(defun apply-player-private-plist (player plist)
  ;; Apply private player state from PLIST onto PLAYER (owner-only data).
  (when (and player plist)
    (let ((stats (getf plist :stats))
          (inventory (getf plist :inventory))
          (equipment (getf plist :equipment)))
      (when stats
        (setf (player-stats player) (deserialize-stat-block stats))
        (mark-player-hud-stats-dirty player))
      (when inventory
        (setf (player-inventory player) (deserialize-inventory inventory *inventory-size*))
        (mark-player-inventory-dirty player))
      (when equipment
        (setf (player-equipment player)
              (deserialize-equipment equipment (length *equipment-slot-ids*)))
        (mark-player-inventory-dirty player))))
  player)

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

(defun deserialize-object (plist)
  ;; Restore zone object from plist to struct.
  ;; Task 5.5: Use make-zone-object-from-plist for proper struct creation.
  (make-zone-object-from-plist plist))

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
                  ;; Use O(1) fast lookup via game's index map
                  :for existing = (and players (find-player-by-id-fast game id))
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
                  players-changed t)
            ;; Rebuild index map for the new players array
            (rebuild-player-index-map game)))
      (let* ((local-id (or (game-net-player-id game)
                           (and (game-player game)
                                (player-id (game-player game)))))
             ;; Use O(1) fast lookup for local player
             (local-player (and local-id (plusp local-id) (find-player-by-id-fast game local-id))))
        (cond
          ;; Found local player - update game-player if changed
          ((and local-player (not (eq (game-player game) local-player)))
           (setf (game-player game) local-player))
          ;; CRITICAL: Valid player ID but not found in snapshot
          ;; DO NOT fall back to first player - it could be another client!
          ;; Log warning (verbose only to avoid spam) and keep current player
          ((and (null local-player) local-id (plusp local-id) (> (length players) 0))
           (log-verbose "Client player ID ~d not found in snapshot (~d players). Keeping current."
                        local-id (length players)))
          ;; No valid local ID set yet - keep current game-player
          ;; The auth-ok message sets game-net-player-id, so this case
          ;; should only happen before authentication completes
          ((and (or (null local-id) (zerop local-id)) (> (length players) 0))
           (log-verbose "No local player ID set, keeping current game-player"))))
      (when players-changed
        (setf (game-entities game)
              (make-entities (game-players game) (game-npcs game)))))))

;;; Compact Deserialization

(defun deserialize-player-compact (vec)
  "Deserialize compact vector to player plist for apply-player-plist.
   Converts back to plist format for compatibility with existing code."
  (when (and vec (>= (length vec) 17))
    (let* ((len (length vec))
           (v4-p (>= len 22)))
      (multiple-value-bind (attacking attack-hit hit-active running)
          (unpack-player-flags (aref vec 11))
        (let* ((run-stamina (cond
                              (v4-p (if (>= len 20)
                                        (dequantize-timer (aref vec 19))
                                        1.0))
                              ((>= len 18) (dequantize-timer (aref vec 17)))
                              (t 1.0)))
               (last-sequence (cond
                                (v4-p (if (>= len 21) (aref vec 20) 0))
                                ((>= len 19) (aref vec 18))
                                (t 0)))
               (plist (list :id (aref vec 0)
                            :x (dequantize-coord (aref vec 1))
                            :y (dequantize-coord (aref vec 2))
                            :hp (aref vec 3)
                            :dx (dequantize-coord (aref vec 4))
                            :dy (dequantize-coord (aref vec 5))
                            :anim-state (decode-anim-state (aref vec 6))
                            :facing (decode-facing (aref vec 7))
                            :facing-sign (float (aref vec 8))
                            :frame-index (aref vec 9)
                            :frame-timer (dequantize-timer (aref vec 10))
                            :attacking attacking
                            :attack-hit attack-hit
                            :hit-active hit-active
                            :running running
                            :attack-timer (dequantize-timer (aref vec 12))
                            :hit-timer (dequantize-timer (aref vec 13))
                            :hit-frame (aref vec 14)
                            :hit-facing (decode-facing (aref vec 15))
                            :hit-facing-sign (float (aref vec 16))
                            :run-stamina run-stamina
                            :last-sequence last-sequence
                            :zone-id-hash (player-compact-zone-hash vec))))
          ;; v4 vectors included attack/follow target IDs at indices 17/18
          (when v4-p
            (setf plist (plist-put plist :attack-target-id (aref vec 17))
                  plist (plist-put plist :follow-target-id (aref vec 18))))
          plist)))))

(defun apply-player-compact-direct (player vec)
  "Apply compact vector directly to player struct, bypassing intermediate plist.
   Preserves client-only state (click markers, mouse timer, auto-walk).
   NOTE: Does NOT set player-id - IDs are immutable after creation. The vec[0] ID
   is used for lookup, not to change the player's identity.
   Returns the zone-id-hash for filtering, or 0 if not present."
  (declare (optimize (speed 3) (safety 1)))
  (when (and player vec (>= (length vec) 17))
    (let* ((len (length vec))
           (v4-p (>= len 22)))
      ;; Unpack flags once
      (multiple-value-bind (attacking attack-hit hit-active running)
          (unpack-player-flags (aref vec 11))
        ;; Apply network fields directly - client-only state untouched
        ;; NOTE: player-id is NOT set here - IDs are immutable (avoids index map desync)
        (setf (player-x player) (dequantize-coord (aref vec 1))
              (player-y player) (dequantize-coord (aref vec 2))
              (player-hp player) (aref vec 3)
              (player-dx player) (dequantize-coord (aref vec 4))
              (player-dy player) (dequantize-coord (aref vec 5))
              (player-anim-state player) (decode-anim-state (aref vec 6))
              (player-facing player) (decode-facing (aref vec 7))
              (player-facing-sign player) (float (aref vec 8))
              (player-frame-index player) (aref vec 9)
              (player-frame-timer player) (dequantize-timer (aref vec 10))
              (player-attacking player) attacking
              (player-attack-hit player) attack-hit
              (player-hit-active player) hit-active
              (player-running player) running
              (player-attack-timer player) (dequantize-timer (aref vec 12))
              (player-hit-timer player) (dequantize-timer (aref vec 13))
              (player-hit-frame player) (aref vec 14)
              (player-hit-facing player) (decode-facing (aref vec 15))
              (player-hit-facing-sign player) (float (aref vec 16)))
        (if v4-p
            (progn
              ;; v4 vectors included attack/follow target IDs
              (setf (player-attack-target-id player) (aref vec 17)
                    (player-follow-target-id player) (aref vec 18))
              (when (>= len 20)
                (setf (player-run-stamina player) (dequantize-timer (aref vec 19))))
              (when (>= len 21)
                (setf (player-last-sequence player) (aref vec 20))))
            (progn
              ;; v5 vectors omit attack/follow target IDs from public snapshots
              (when (>= len 18)
                (setf (player-run-stamina player) (dequantize-timer (aref vec 17))))
              (when (>= len 19)
                (setf (player-last-sequence player) (aref vec 18))))))
      ;; Return zone-id-hash for filtering (0 if not present)
      (player-compact-zone-hash vec))))

(defun deserialize-npc-compact (vec)
  "Deserialize compact vector to NPC plist for compatibility.
   Supports v3 (14 elements) and v4 (15 elements with zone-id-hash)."
  (when (and vec (>= (length vec) 14))
    (multiple-value-bind (alive provoked hit-active)
        (unpack-npc-flags (aref vec 4))
      (list :id (aref vec 0)
            :x (dequantize-coord (aref vec 1))
            :y (dequantize-coord (aref vec 2))
            :hits-left (aref vec 3)
            :alive alive
            :provoked provoked
            :behavior-state (decode-behavior-state (aref vec 5))
            :attack-timer (dequantize-timer (aref vec 6))
            :anim-state (decode-anim-state (aref vec 7))
            :facing (decode-facing (aref vec 8))
            :frame-index (aref vec 9)
            :frame-timer (dequantize-timer (aref vec 10))
            :hit-active hit-active
            :hit-timer (dequantize-timer (aref vec 11))
            :hit-frame (aref vec 12)
            :hit-facing (decode-facing (aref vec 13))
            :hit-facing-sign 1.0
            ;; v4 adds zone-id-hash at index 14 (0 if v3 format)
            :zone-id-hash (if (>= (length vec) 15) (aref vec 14) 0)))))  ; Default, not stored compactly

(defun apply-npc-compact-direct (npc vec)
  "Apply compact vector directly to NPC struct, bypassing intermediate plist.
   NOTE: Does NOT set npc-id - IDs are immutable after creation. The vec[0] ID
   is used for lookup, not to change the NPC's identity.
   Returns the zone-id-hash for filtering, or 0 if not present."
  (declare (optimize (speed 3) (safety 1)))
  (when (and npc vec (>= (length vec) 14))
    (multiple-value-bind (alive provoked hit-active)
        (unpack-npc-flags (aref vec 4))
      ;; Apply network fields directly
      ;; NOTE: npc-id is NOT set here - IDs are immutable
      (setf (npc-x npc) (dequantize-coord (aref vec 1))
            (npc-y npc) (dequantize-coord (aref vec 2))
            (npc-hits-left npc) (aref vec 3)
            (npc-alive npc) alive
            (npc-provoked npc) provoked
            (npc-behavior-state npc) (decode-behavior-state (aref vec 5))
            (npc-attack-timer npc) (dequantize-timer (aref vec 6))
            (npc-anim-state npc) (decode-anim-state (aref vec 7))
            (npc-facing npc) (decode-facing (aref vec 8))
            (npc-frame-index npc) (aref vec 9)
            (npc-frame-timer npc) (dequantize-timer (aref vec 10))
            (npc-hit-active npc) hit-active
            (npc-hit-timer npc) (dequantize-timer (aref vec 11))
            (npc-hit-frame npc) (aref vec 12)
            (npc-hit-facing npc) (decode-facing (aref vec 13))
            (npc-hit-facing-sign npc) 1.0))
    ;; Return zone-id-hash for filtering (0 if not present)
    (if (>= (length vec) 15) (aref vec 14) 0)))

(defun deserialize-game-state-compact (state game)
  "Apply compact game state to GAME, converting vectors back to entity updates."
  (let ((player-vectors (getf state :players))
        (npc-vectors (getf state :npcs))
        (object-plists (getf state :objects)))
    ;; Convert compact player vectors to plists and apply
    (when player-vectors
      (let ((player-plists (loop :for vec :across player-vectors
                                 :collect (deserialize-player-compact vec))))
        (apply-player-plists game player-plists)))
    ;; Convert compact NPC vectors to plists and apply
    (when npc-vectors
      (let ((npcs (game-npcs game)))
        (loop :for vec :across npc-vectors
              :for index :from 0
              :for plist = (deserialize-npc-compact vec)
              :do (deserialize-npc plist npcs index))))
    ;; Apply object states from server (authoritative - replaces client objects)
    (when object-plists
      (let* ((world (game-world game))
             (zone (and world (world-zone world))))
        (when zone
          ;; Replace client zone objects with server state
          ;; This ensures dropped items appear and picked up items disappear
          ;; Task 5.5: Convert server plists to zone-object structs
          (setf (zone-objects zone)
                (loop :for server-obj :in object-plists
                      :collect (make-zone-object-from-plist server-obj))))))
    ;; Step 8: Process edge strips (separate deserialization path, no zone-hash check)
    (deserialize-edge-strips state game)))
