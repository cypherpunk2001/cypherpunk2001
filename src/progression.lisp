;; NOTE: If you change behavior here, update docs/progression.md :)
(in-package #:mmorpg)

;;; Thread-safe zone object access
#+sbcl
(defvar *zone-objects-lock* (sb-thread:make-mutex :name "zone-objects-lock")
  "Mutex protecting zone object modifications for thread-safe access.")

(defmacro with-zone-objects-lock (&body body)
  "Execute BODY with *zone-objects-lock* held for thread-safe zone object operations."
  #+sbcl
  `(sb-thread:with-mutex (*zone-objects-lock*)
     ,@body)
  #-sbcl
  `(progn ,@body))

(defparameter *training-modes*
  #(:attack :strength :defense :balanced))

(defun valid-training-mode-p (mode)
  ;; Return true when MODE is a supported training option.
  (and mode (find mode *training-modes*)))

(defun normalize-training-mode (mode)
  ;; Normalize MODE to a supported training mode.
  (if (valid-training-mode-p mode)
      mode
      :balanced))

(defun training-mode-label (mode)
  ;; Return a short label for MODE.
  (case (normalize-training-mode mode)
    (:attack "ATT")
    (:strength "STR")
    (:defense "DEF")
    (:balanced "BAL")
    (t "BAL")))

(defun set-training-mode (stats mode)
  ;; Update training mode when MODE is valid.
  (when (and stats (valid-training-mode-p mode))
    (setf (stat-block-training-mode stats) mode)
    mode))

(defun xp->level (xp)
  ;; Convert XP into a level using a simple quadratic curve.
  (let* ((xp (max 0 xp))
         (per-level (max 1 *stat-xp-per-level*))
         (raw (sqrt (/ xp per-level)))
         (level (1+ (floor raw))))
    (min *stat-max-level* (max 1 level))))

(defun level->xp (level)
  ;; Convert LEVEL into the minimum XP for that level.
  (skill-xp-for-level level))

(defun update-skill-level (skill)
  ;; Sync SKILL's level from its XP. Returns old/new levels.
  (let* ((old (skill-level skill))
         (new (xp->level (skill-xp skill))))
    (setf (skill-level skill) new)
    (values old new)))

(defun award-skill-xp (skill amount)
  ;; Add XP to SKILL and update its level.
  (let ((xp (max 0 (truncate amount))))
    (incf (skill-xp skill) xp)
    (update-skill-level skill)))

(defun stat-block-effective-level (stats stat-id &optional (fallback 1))
  ;; Return the effective level for STAT-ID, including modifiers.
  (if (null stats)
      fallback
      (let* ((base (stat-block-base-level stats stat-id))
             (mods (stat-block-modifiers stats))
             (bonus (if mods
                        (ecase stat-id
                          (:attack (stat-modifiers-attack mods))
                          (:strength (stat-modifiers-strength mods))
                          (:defense (stat-modifiers-defense mods))
                          (:hitpoints (stat-modifiers-hitpoints mods)))
                        0)))
        (+ base bonus))))

(defun combatant-attack-level (combatant)
  (stat-block-effective-level (combatant-stats combatant) :attack))

(defun combatant-strength-level (combatant)
  (stat-block-effective-level (combatant-stats combatant) :strength))

(defun combatant-defense-level (combatant)
  (stat-block-effective-level (combatant-stats combatant) :defense))

(defun combatant-max-hp (combatant)
  (max 1 (stat-block-effective-level (combatant-stats combatant) :hitpoints)))

(defun combat-level (stats)
  ;; Compute a simple melee combat level approximation.
  (let* ((att (stat-block-effective-level stats :attack))
         (str (stat-block-effective-level stats :strength))
         (def (stat-block-effective-level stats :defense))
         (hp (stat-block-effective-level stats :hitpoints))
         (melee (floor (/ (+ att str) 2))))
    (max 1 (floor (/ (+ def hp melee) 4)))))

(defun melee-hit-chance (attacker defender)
  ;; Return a clamped hit chance from attacker/defender stats.
  (let* ((attack (combatant-attack-level attacker))
         (defense (combatant-defense-level defender))
         (raw (/ (+ attack 1.0) (+ attack defense 2.0))))
    (clamp raw 0.05 0.95)))

(defun roll-melee-hit (attacker defender)
  ;; Return hit result along with chance and roll.
  (let* ((chance (melee-hit-chance attacker defender))
         (roll (random 1.0)))
    (values (< roll chance) chance roll)))

(defun melee-hit-p (attacker defender)
  ;; Roll for a melee hit using attacker/defender stats.
  (multiple-value-bind (hit)
      (roll-melee-hit attacker defender)
    hit))

(defun melee-max-hit (combatant &optional fallback)
  ;; Return a max hit based on strength.
  (let* ((strength (combatant-strength-level combatant))
         (base (max 1 (floor (/ (+ strength 1) 2)))))
    (if fallback
        (max base fallback)
        base)))

(defun roll-melee-damage (combatant &optional fallback)
  ;; Roll damage from strength-based max hit.
  (let ((max-hit (melee-max-hit combatant fallback)))
    (1+ (random (max 1 max-hit)))))

(defun clamp-player-hp (player)
  ;; Clamp the player's current HP to the effective max.
  (let ((max-hp (combatant-max-hp player)))
    (when (> (player-hp player) max-hp)
      (setf (player-hp player) max-hp))
    (when (< (player-hp player) 0)
      (setf (player-hp player) 0))
    max-hp))

(defun apply-hitpoints-level-up (player old new)
  ;; Increase current HP when hitpoints levels increase.
  (when (> new old)
    (incf (player-hp player) (- new old)))
  (clamp-player-hp player))

(defun award-hitpoints-xp (player amount)
  ;; Award XP to hitpoints and sync current HP.
  (let* ((stats (player-stats player))
         (skill (and stats (stat-block-hitpoints stats))))
    (when skill
      (multiple-value-bind (old new)
          (award-skill-xp skill amount)
        (apply-hitpoints-level-up player old new)
        (mark-player-hud-stats-dirty player)
        (values old new)))))

(defun combat-hitpoints-xp (amount)
  ;; Return hitpoints XP for combat XP awards.
  (let* ((xp (max 0 (truncate amount)))
         (ratio (max 0.0 *combat-hitpoints-xp-multiplier*)))
    (truncate (* xp ratio))))

(defun split-combat-xp (player amount)
  ;; Split AMOUNT into (attack strength defense hitpoints) XP awards.
  (let* ((stats (player-stats player))
         (mode (normalize-training-mode (and stats (stat-block-training-mode stats))))
         (xp (max 0 (truncate amount)))
         (hp-xp (min xp (combat-hitpoints-xp xp)))
         (remaining (max 0 (- xp hp-xp))))
    (case mode
      (:attack (values remaining 0 0 hp-xp))
      (:strength (values 0 remaining 0 hp-xp))
      (:defense (values 0 0 remaining hp-xp))
      (:balanced
       (multiple-value-bind (base remainder)
           (split-xp remaining 3)
         (values (+ base (if (> remainder 0) 1 0))
                 (+ base (if (> remainder 1) 1 0))
                 (+ base (if (> remainder 2) 1 0))
                 hp-xp)))
      (t (values 0 0 0 hp-xp)))))

(defun split-xp (amount parts)
  ;; Split AMOUNT into PARTS, returning base and remainder.
  (let* ((parts (max 1 parts))
         (base (floor amount parts))
         (remainder (- amount (* base parts))))
    (values base remainder)))

(defun award-combat-xp (player amount)
  ;; Award XP based on the player's current training mode.
  (let* ((stats (player-stats player))
         (xp (max 0 (truncate amount)))
         (attack-xp 0)
         (strength-xp 0)
         (defense-xp 0)
         (hitpoints-xp 0)
         (level-ups nil))
    (when (and stats (> xp 0))
      ;; Track lifetime XP for progression display
      (incf (player-lifetime-xp player) xp)
      (multiple-value-setq (attack-xp strength-xp defense-xp hitpoints-xp)
        (split-combat-xp player xp))
      (when (> attack-xp 0)
        (multiple-value-bind (old new)
            (award-skill-xp (stat-block-attack stats) attack-xp)
          (when (> new old)
            (push (cons :attack new) level-ups))))
      (when (> strength-xp 0)
        (multiple-value-bind (old new)
            (award-skill-xp (stat-block-strength stats) strength-xp)
          (when (> new old)
            (push (cons :strength new) level-ups))))
      (when (> defense-xp 0)
        (multiple-value-bind (old new)
            (award-skill-xp (stat-block-defense stats) defense-xp)
          (when (> new old)
            (push (cons :defense new) level-ups))))
      (when (> hitpoints-xp 0)
        (multiple-value-bind (old new)
            (award-hitpoints-xp player hitpoints-xp)
          (when (and old new (> new old))
            (push (cons :hitpoints new) level-ups))))
      (mark-player-hud-stats-dirty player)
      ;; Tier-1 write: level-ups must be saved immediately to prevent
      ;; XP rollback past level boundary on crash/logout
      ;; Use aggressive retry with exponential backoff (10 retries over ~10s)
      (when level-ups
        (with-retry-exponential (saved (lambda () (db-save-player-immediate player))
                                  :max-retries 10
                                  :initial-delay 100
                                  :max-delay 2000
                                  :on-final-fail (lambda (e)
                                                   (warn "CRITICAL: Level-up save FAILED for player ~d after 10 retries: ~a - using dirty flag fallback"
                                                         (player-id player) e)
                                                   ;; Fallback to dirty flag - will save within 30s if server survives
                                                   (mark-player-dirty (player-id player))))))
      ;; Tier-2 write: XP gains without level-ups should be marked dirty for batched saves
      (when (and (> xp 0) (null level-ups))
        (mark-player-dirty (player-id player))))
    (values attack-xp strength-xp defense-xp hitpoints-xp
            (nreverse level-ups))))

(defun mark-player-hud-stats-dirty (player)
  ;; Flag the player's HUD stats cache for refresh.
  (when player
    (setf (player-hud-stats-dirty player) t)))

(defun mark-player-inventory-dirty (player)
  ;; Flag the player's inventory cache for refresh.
  (when player
    (setf (player-inventory-dirty player) t)))

(defun format-xp-awards (attack-xp strength-xp defense-xp hitpoints-xp)
  ;; Format XP awards for combat logging.
  (let ((parts nil))
    (when (> attack-xp 0)
      (push (format nil "A+~d" attack-xp) parts))
    (when (> strength-xp 0)
      (push (format nil "S+~d" strength-xp) parts))
    (when (> defense-xp 0)
      (push (format nil "D+~d" defense-xp) parts))
    (when (> hitpoints-xp 0)
      (push (format nil "HP+~d" hitpoints-xp) parts))
    (when parts
      (format nil "XP ~{~a~^ ~}" (nreverse parts)))))

(defun format-skill-hud-line (label skill)
  ;; Build a HUD line for LABEL + SKILL.
  (if (null skill)
      (format nil "~a --" label)
      (let* ((level (skill-level skill))
             (xp (skill-xp skill)))
        (if (>= level *stat-max-level*)
            (format nil "~a ~d MAX" label level)
            (let* ((next (level->xp (1+ level)))
                   (need (- next xp)))
              (format nil "~a ~d ~d/~d (~d)"
                      label level xp next need))))))

(defun refresh-player-hud-stats (player)
  ;; Refresh cached HUD stats lines for the player.
  (let* ((lines (player-hud-stats-lines player))
         (stats (player-stats player)))
    (when lines
      (let* ((cap (length lines))
             (count 0)
             (mode (and stats (stat-block-training-mode stats))))
        (labels ((set-line (text)
                   (when (< count cap)
                     (setf (aref lines count) text)
                     (incf count))))
          (set-line (format nil "Train: ~a (1/2/3/Z)" (training-mode-label mode)))
          (when stats
            (set-line (format nil "Combat Lvl: ~d" (combat-level stats)))
            (set-line (format-skill-hud-line "ATT" (stat-block-attack stats)))
            (set-line (format-skill-hud-line "STR" (stat-block-strength stats)))
            (set-line (format-skill-hud-line "DEF" (stat-block-defense stats)))
            (set-line (format-skill-hud-line "HP" (stat-block-hitpoints stats))))
          (loop :for i :from count :below cap
                :do (setf (aref lines i) "")))
        (setf (player-hud-stats-count player) count
              (player-hud-stats-dirty player) nil)))))

(defun ensure-player-hud-stats (player)
  ;; Refresh HUD stats when flagged dirty.
  (when (and player (player-hud-stats-dirty player))
    (refresh-player-hud-stats player)))

(defun item-display-name (item-id)
  ;; Return a display-friendly name for ITEM-ID.
  (let ((item (and item-id (find-item-archetype item-id))))
    (or (and item (item-archetype-name item))
        (and item-id (string-capitalize (string item-id)))
        "Unknown")))

(defun inventory-slot-label (item-id count)
  ;; Build a label for ITEM-ID with COUNT.
  (let ((name (item-display-name item-id)))
    (if (> count 1)
        (format nil "~a x~d" name count)
        name)))

(defun refresh-player-inventory (player)
  ;; Refresh cached inventory lines for the player.
  (let* ((inventory (and player (player-inventory player)))
         (lines (and player (player-inventory-lines player)))
         (slots (and inventory (inventory-slots inventory))))
    (when (and inventory lines slots)
      (let* ((cap (length lines))
             (count 0))
        (labels ((set-line (text)
                   (when (< count cap)
                     (setf (aref lines count) text)
                     (incf count))))
          (loop :for slot :across slots
                :for item-id = (inventory-slot-item-id slot)
                :for item-count = (inventory-slot-count slot)
                :when (and item-id (> item-count 0))
                  :do (set-line (inventory-slot-label item-id item-count)))
          (loop :for i :from count :below cap
                :do (setf (aref lines i) "")))
        (setf (player-inventory-count player) count
              (player-inventory-dirty player) nil)))))

(defun ensure-player-inventory (player)
  ;; Refresh inventory lines when flagged dirty.
  (when (and player (player-inventory-dirty player))
    (refresh-player-inventory player)))

(defun npc-kill-xp (npc)
  ;; Return optional bonus XP for defeating NPC.
  (let ((archetype (npc-archetype npc)))
    (if archetype
        (npc-archetype-combat-xp archetype)
        0)))

(defun npc-loot-table-id (npc)
  ;; Return the loot table id for an NPC.
  (let ((archetype (npc-archetype npc)))
    (or (and archetype (npc-archetype-loot-table-id archetype))
        *npc-default-loot-table-id*)))

(defun item-stack-size (item-id)
  ;; Return the stack size for ITEM-ID (defaults to 1).
  (let ((item (find-item-archetype item-id)))
    (if item
        (item-archetype-stack-size item)
        1)))

(defun inventory-slot-empty-p (slot)
  ;; Return true when SLOT is empty.
  (or (null (inventory-slot-item-id slot))
      (<= (inventory-slot-count slot) 0)))

(defun inventory-add (inventory item-id count)
  ;; Add COUNT of ITEM-ID into INVENTORY, returning leftover count.
  (let* ((remaining (max 0 (truncate count)))
         (slots (and inventory (inventory-slots inventory)))
         (stack-size (item-stack-size item-id)))
    (when (and slots (> remaining 0))
      (when (> stack-size 1)
        (loop :for slot :across slots
              :while (> remaining 0)
              :when (and (eq (inventory-slot-item-id slot) item-id)
                         (< (inventory-slot-count slot) stack-size))
                :do (let* ((space (- stack-size (inventory-slot-count slot)))
                           (add (min space remaining)))
                      (incf (inventory-slot-count slot) add)
                      (decf remaining add))))
      (loop :for slot :across slots
            :while (> remaining 0)
            :when (inventory-slot-empty-p slot)
              :do (let ((add (if (> stack-size 1)
                                 (min stack-size remaining)
                                 1)))
                    (setf (inventory-slot-item-id slot) item-id
                          (inventory-slot-count slot) add)
                    (decf remaining add))))
    remaining))

(defun inventory-remove (inventory item-id count &optional slot-index)
  ;; Remove COUNT of ITEM-ID from INVENTORY, returning leftover count.
  ;; If SLOT-INDEX is provided, only remove from that specific slot.
  (let* ((remaining (max 0 (truncate count)))
         (slots (and inventory (inventory-slots inventory))))
    (log-verbose "INV-REMOVE: item=~a count=~a slot-index=~a inv=~a slots=~a"
                 item-id count slot-index inventory (and slots (length slots)))
    (when (and slots (> remaining 0))
      (if slot-index
          ;; Remove from specific slot only
          (when (< slot-index (length slots))
            (let ((slot (aref slots slot-index)))
              (when (eq (inventory-slot-item-id slot) item-id)
                (let ((before (inventory-slot-count slot))
                      (take (min remaining (inventory-slot-count slot))))
                  (log-verbose "INV-REMOVE: slot[~d] item=~a before=~d take=~d"
                               slot-index (inventory-slot-item-id slot) before take)
                  (decf (inventory-slot-count slot) take)
                  (log-verbose "INV-REMOVE: slot[~d] after=~d" slot-index (inventory-slot-count slot))
                  (decf remaining take)
                  (when (inventory-slot-empty-p slot)
                    (setf (inventory-slot-item-id slot) nil
                          (inventory-slot-count slot) 0))))))
          ;; Remove from any matching slot (legacy behavior)
          (loop :for slot :across slots
                :for idx :from 0
                :while (> remaining 0)
                :when (eq (inventory-slot-item-id slot) item-id)
                  :do (let ((before (inventory-slot-count slot))
                            (take (min remaining (inventory-slot-count slot))))
                        (log-verbose "INV-REMOVE: slot[~d] item=~a before=~d take=~d"
                                     idx (inventory-slot-item-id slot) before take)
                        (decf (inventory-slot-count slot) take)
                        (log-verbose "INV-REMOVE: slot[~d] after=~d" idx (inventory-slot-count slot))
                        (decf remaining take)
                        (when (inventory-slot-empty-p slot)
                          (setf (inventory-slot-item-id slot) nil
                                (inventory-slot-count slot) 0))))))
    (log-verbose "INV-REMOVE: leftover=~d" remaining)
    remaining))

(defun grant-inventory-item (player item-id count)
  ;; Authoritatively add items to PLAYER's inventory.
  (let* ((inventory (and player (player-inventory player)))
         (amount (max 0 (truncate count))))
    (if inventory
        (let ((leftover (inventory-add inventory item-id amount)))
          (when (< leftover amount)
            (mark-player-inventory-dirty player)
            ;; Tier-2 write: inventory changes should be marked dirty for batched saves
            (mark-player-dirty (player-id player)))
          leftover)
        amount)))

(defun consume-inventory-item (player item-id count &optional slot-index)
  ;; Authoritatively remove items from PLAYER's inventory.
  ;; If SLOT-INDEX is provided, only remove from that specific slot.
  (let* ((inventory (and player (player-inventory player)))
         (amount (max 0 (truncate count))))
    (if inventory
        (let ((leftover (inventory-remove inventory item-id amount slot-index)))
          (when (< leftover amount)
            (mark-player-inventory-dirty player)
            ;; Tier-2 write: inventory changes should be marked dirty for batched saves
            (mark-player-dirty (player-id player)))
          leftover)
        amount)))

(defun item-equipment-slot (item-id)
  ;; Return the equipment slot for ITEM-ID, if any.
  (let ((item (and item-id (find-item-archetype item-id))))
    (and item (item-archetype-equip-slot item))))

(defun equipment-slot-index (slot-id &optional (slot-ids *equipment-slot-ids*))
  ;; Return the index for SLOT-ID in SLOT-IDS.
  (position slot-id slot-ids))

(defun equipment-slot-item (equipment slot-id)
  ;; Return the equipped item in SLOT-ID.
  (let* ((items (and equipment (equipment-items equipment)))
         (index (and items (equipment-slot-index slot-id))))
    (and index (< index (length items)) (aref items index))))

(defun set-equipment-slot-item (equipment slot-id item-id)
  ;; Set the equipped ITEM-ID for SLOT-ID.
  (let* ((items (and equipment (equipment-items equipment)))
         (index (and items (equipment-slot-index slot-id))))
    (when (and index (< index (length items)))
      (setf (aref items index) item-id)
      t)))

(defun apply-item-modifiers (stats item-id direction)
  ;; Apply item modifiers to STATS (DIRECTION is 1 or -1).
  (let ((item (and item-id (find-item-archetype item-id)))
        (mods (and stats (stat-block-modifiers stats))))
    (when (and item mods)
      (let ((attack (or (item-archetype-attack item) 0))
            (strength (or (item-archetype-strength item) 0))
            (defense (or (item-archetype-defense item) 0))
            (hitpoints (or (item-archetype-hitpoints item) 0)))
        (incf (stat-modifiers-attack mods) (* direction attack))
        (incf (stat-modifiers-strength mods) (* direction strength))
        (incf (stat-modifiers-defense mods) (* direction defense))
        (incf (stat-modifiers-hitpoints mods) (* direction hitpoints)))
      t)))

(defun equip-item (player item-id)
  ;; Equip ITEM-ID from the player's inventory, returning true when equipped.
  (let* ((slot-id (item-equipment-slot item-id))
         (equipment (and player (player-equipment player)))
         (inventory (and player (player-inventory player)))
         (stats (and player (player-stats player))))
    (when (and slot-id equipment inventory stats)
      (let* ((items (equipment-items equipment))
             (index (and items (equipment-slot-index slot-id))))
        (when (and index (< index (length items)))
          (let ((current (aref items index)))
            (when (zerop (consume-inventory-item player item-id 1))
              (when current
                (let ((leftover (grant-inventory-item player current 1)))
                  (when (> leftover 0)
                    (grant-inventory-item player item-id 1)
                    (return-from equip-item nil))
                  (apply-item-modifiers stats current -1)))
              (setf (aref items index) item-id)
              (apply-item-modifiers stats item-id 1)
              (clamp-player-hp player)
              (mark-player-hud-stats-dirty player)
              ;; Tier-1 write: equipment changes saved immediately (prevents item loss on crash)
              (with-retry-exponential (saved (lambda () (db-save-player-immediate player))
                                        :max-retries 10
                                        :initial-delay 100
                                        :max-delay 500
                                        :on-final-fail (lambda (e)
                                                         (warn "Equip save failed for player ~d: ~a - using dirty flag"
                                                               (player-id player) e)
                                                         (mark-player-dirty (player-id player))))
                saved)
              t)))))))

(defun unequip-item (player slot-id)
  ;; Unequip SLOT-ID into the player's inventory, returning true on success.
  (let* ((equipment (and player (player-equipment player)))
         (inventory (and player (player-inventory player)))
         (stats (and player (player-stats player)))
         (items (and equipment (equipment-items equipment)))
         (index (and items (equipment-slot-index slot-id))))
    (when (and items index (< index (length items)))
      (let ((current (aref items index)))
        (when (and current inventory stats)
          (let ((leftover (grant-inventory-item player current 1)))
            (when (zerop leftover)
              (setf (aref items index) nil)
              (apply-item-modifiers stats current -1)
              (clamp-player-hp player)
              (mark-player-hud-stats-dirty player)
              ;; Tier-1 write: equipment changes saved immediately (prevents item loss on crash)
              (with-retry-exponential (saved (lambda () (db-save-player-immediate player))
                                        :max-retries 10
                                        :initial-delay 100
                                        :max-delay 500
                                        :on-final-fail (lambda (e)
                                                         (warn "Unequip save failed for player ~d: ~a - using dirty flag"
                                                               (player-id player) e)
                                                         (mark-player-dirty (player-id player))))
                saved)
              t)))))))

(defun player-tile-coords (player world)
  ;; Return the player's current tile coordinates.
  (let ((tile-size (world-tile-dest-size world)))
    (values (floor (player-x player) tile-size)
            (floor (player-y player) tile-size))))

(defun object-entry-count (object archetype)
  ;; Return the pickup count for OBJECT and ARCHETYPE.
  (let ((raw (getf object :count nil)))
    (cond
      ((and raw (numberp raw) (>= raw 0)) (truncate raw))
      (archetype (max 1 (object-archetype-count archetype)))
      (t 1))))

(defun object-respawn-seconds (archetype)
  ;; Return the respawn cooldown for ARCHETYPE.
  (let ((seconds (and archetype (object-archetype-respawn-seconds archetype))))
    (if (and seconds (> seconds 0.0))
        (max 0.0 seconds)
        0.0)))

(defun object-respawnable-p (object)
  ;; Return true when OBJECT should use respawn cooldowns.
  (not (eq (getf object :respawnable t) nil)))

(defun object-entry-respawn-seconds (object archetype)
  ;; Return the respawn cooldown for a specific OBJECT entry.
  (if (and object (not (object-respawnable-p object)))
      0.0
      (let ((override (getf object :respawn-seconds nil)))
        (if (and override (numberp override))
            (max 0.0 (float override 1.0))
            (object-respawn-seconds archetype)))))

(defun object-respawn-timer (object)
  ;; Return the active respawn timer for OBJECT, if any.
  (let ((timer (getf object :respawn nil)))
    (if (and timer (numberp timer))
        (max 0.0 (float timer 1.0))
        0.0)))

(defun object-respawning-p (object)
  ;; Return true when OBJECT is waiting to respawn.
  (> (object-respawn-timer object) 0.0))

(defun update-object-respawns (world dt)
  ;; Tick down respawn timers and restore object counts when ready.
  ;; Thread-safe: protects zone-objects modification from concurrent pickup.
  (with-zone-objects-lock
    (let* ((zone (world-zone world))
           (objects (and zone (zone-objects zone))))
      (when objects
        (dolist (object objects)
          (when (object-respawnable-p object)
            (let* ((timer (object-respawn-timer object)))
              (when (> timer 0.0)
                (setf timer (max 0.0 (- timer dt))
                      (getf object :respawn) timer)
                (when (<= timer 0.0)
                  (let* ((object-id (getf object :id))
                         (archetype (and object-id (find-object-archetype object-id))))
                    (when archetype
                      (setf (getf object :count) (object-archetype-count archetype)
                            (getf object :respawn) 0.0
                            ;; Mark dirty so delta snapshot includes respawn
                            (getf object :snapshot-dirty) t))))))))))))

(defun pickup-object-at-tile (player world tx ty object-id)
  ;; Attempt to pick up OBJECT-ID at TX/TY; returns true on success.
  ;; Thread-safe: protects zone-objects modification from concurrent respawn updates.
  (with-zone-objects-lock
    (let* ((zone (world-zone world))
           (objects (and zone (zone-objects zone))))
      (log-verbose "PICKUP-TILE: zone=~a objects=~a tx=~d ty=~d id=~a"
                   (and zone (zone-id zone)) (length objects) tx ty object-id)
      (when (and player zone objects)
        (let ((remaining nil)
              (picked nil))
          (dolist (object objects)
            (let ((ox (getf object :x))
                  (oy (getf object :y))
                  (id (getf object :id)))
              (log-verbose "PICKUP-TILE: checking obj ox=~a oy=~a id=~a" ox oy id)
              (if (and (eql ox tx)
                       (eql oy ty)
                       (or (null object-id) (eq id object-id)))
                  ;; Check for object archetype first, then check if id is a valid item directly
                  (let* ((archetype (and id (find-object-archetype id)))
                         (item-id (cond
                                    ;; Object archetype maps to item
                                    (archetype (object-archetype-item-id archetype))
                                    ;; No archetype - check if id is a valid item directly
                                    ((and id (find-item-archetype id)) id)
                                    (t nil)))
                         (count (object-entry-count object archetype))
                         (respawn (object-entry-respawn-seconds object archetype)))
                    (log-verbose "PICKUP-TILE: MATCH! archetype=~a item-id=~a count=~d respawn=~a respawning=~a"
                                 archetype item-id count respawn (object-respawning-p object))
                    (if (and item-id (> count 0) (not (object-respawning-p object)))
                        (let ((leftover (grant-inventory-item player item-id count)))
                          (log-verbose "PICKUP-TILE: granted! leftover=~d respawn=~a" leftover respawn)
                          (setf picked t)
                          ;; Mark player snapshot-dirty so inventory syncs via delta
                          (setf (player-snapshot-dirty player) t)
                          (cond
                            ((zerop leftover)
                             (if (> respawn 0.0)
                                 (progn
                                   (setf (getf object :count) 0
                                         (getf object :respawn) respawn)
                                   (log-verbose "PICKUP-TILE: set respawn timer to ~a on object ~a" respawn (getf object :id))
                                   (push object remaining))
                                 nil))
                            (t
                             (setf (getf object :count) leftover)
                             (push object remaining))))
                        (push object remaining)))
                  (push object remaining))))
          (when picked
            (setf (zone-objects zone) (nreverse remaining))
            (log-verbose "PICKUP-TILE: zone now has ~a objects, first respawn=~a"
                         (length (zone-objects zone))
                         (and (zone-objects zone) (getf (first (zone-objects zone)) :respawn))))
          picked)))))

(defun update-player-pickup-target (player world)
  ;; Resolve explicit pickup targets once the player reaches the tile.
  (when (and player (player-pickup-target-active player))
    (multiple-value-bind (tx ty)
        (player-tile-coords player world)
      (let ((target-x (player-pickup-target-tx player))
            (target-y (player-pickup-target-ty player))
            (target-id (player-pickup-target-id player)))
        (log-verbose "UPDATE-PICKUP: player at (~d,~d) target at (~d,~d) id=~a"
                     tx ty target-x target-y target-id)
        (when (and (eql tx target-x) (eql ty target-y))
          (log-verbose "UPDATE-PICKUP: Executing pickup!")
          (pickup-object-at-tile player world target-x target-y target-id)
          (setf (player-pickup-target-active player) nil
                (player-pickup-target-id player) nil))))))

(defun process-player-drop-request (player intent world)
  ;; Process a drop request from the client intent (server authority).
  (let ((item-id (intent-requested-drop-item-id intent))
        (count (intent-requested-drop-count intent))
        (slot-index (intent-requested-drop-slot-index intent)))
    (log-verbose "PROCESS-DROP: player=~a item=~a count=~a slot=~a"
                 (and player (player-id player)) item-id count slot-index)
    (when (and item-id (> count 0))
      (let ((dropped (drop-inventory-item player world item-id count slot-index)))
        (log-verbose "PROCESS-DROP: result=~a" dropped)
        dropped))))

(defun drop-inventory-item (player world item-id count &optional slot-index)
  ;; Drop COUNT of ITEM-ID onto the player's current tile.
  ;; If SLOT-INDEX is provided, only remove from that specific slot.
  ;; Works for ANY valid item - uses object archetype if available, otherwise uses item-id directly.
  (log-verbose "DROP-INV: item=~a count=~a slot=~a" item-id count slot-index)
  (let* ((zone (world-zone world))
         (amount (max 0 (truncate count))))
    (when (and player zone item-id (> amount 0))
      ;; Validate this is a real item
      (let ((item-arch (find-item-archetype item-id)))
        (when item-arch
          ;; Use object archetype's id if available, otherwise use item-id directly
          (let* ((obj-archetype (find-object-archetype-by-item item-id))
                 (object-id (if obj-archetype
                                (object-archetype-id obj-archetype)
                                item-id)))
            (log-verbose "DROP-INV: obj-archetype=~a object-id=~a" (not (null obj-archetype)) object-id)
            (multiple-value-bind (tx ty)
                (player-tile-coords player world)
              (let* ((objects (zone-objects zone))
                     (existing (and objects
                                    (find-if (lambda (obj)
                                               (and (eql (getf obj :x) tx)
                                                    (eql (getf obj :y) ty)))
                                             objects)))
                     (existing-id (and existing (getf existing :id)))
                     (existing-respawnable (and existing (object-respawnable-p existing))))
                (log-verbose "DROP-INV: tile=~d,~d existing=~a existing-id=~a object-id=~a"
                             tx ty (not (null existing)) existing-id object-id)
                (when (or (null existing)
                          (eq existing-id object-id))
                  (let ((leftover (consume-inventory-item player item-id amount slot-index)))
                    (let ((dropped (- amount leftover)))
                      (log-verbose "DROP-INV: consumed, leftover=~d dropped=~d" leftover dropped)
                      (when (> dropped 0)
                        ;; Mark player snapshot-dirty so inventory syncs via delta
                        (setf (player-snapshot-dirty player) t)
                        (cond
                          ((and existing (not existing-respawnable))
                           (log-verbose "DROP-INV: adding to existing non-respawnable")
                           (let ((base-count (object-entry-count existing obj-archetype)))
                             (setf (getf existing :count) (+ base-count dropped))))
                          ((and existing existing-respawnable)
                           (log-verbose "DROP-INV: creating new object (existing is respawnable)")
                           (zone-add-object zone (list :id object-id
                                                       :x tx
                                                       :y ty
                                                       :count dropped
                                                       :respawn 0.0
                                                       :respawnable nil
                                                       :snapshot-dirty nil)))
                          (t
                           (log-verbose "DROP-INV: creating new object at ~d,~d" tx ty)
                           (zone-add-object zone (list :id object-id
                                                       :x tx
                                                       :y ty
                                                       :count dropped
                                                       :respawn 0.0
                                                       :respawnable nil
                                                       :snapshot-dirty nil))))
                        dropped))))))))))))

(defun swap-inventory-slots (player slot-a slot-b)
  ;; Swap the contents of two inventory slots.
  ;; Returns T if swap succeeded, NIL otherwise.
  (log-verbose "SWAP-INV: player=~a slot-a=~a slot-b=~a"
               (and player (player-id player)) slot-a slot-b)
  (when player
    (let* ((inventory (player-inventory player))
           (slots (and inventory (inventory-slots inventory)))
           (size (and slots (length slots))))
      (when (and slots
                 (integerp slot-a) (integerp slot-b)
                 (>= slot-a 0) (< slot-a size)
                 (>= slot-b 0) (< slot-b size)
                 (/= slot-a slot-b))
        ;; Get both slots
        (let ((a (aref slots slot-a))
              (b (aref slots slot-b)))
          ;; Swap the slot contents
          (setf (aref slots slot-a) b
                (aref slots slot-b) a)
          ;; Mark player as dirty so inventory order persists
          (setf (player-snapshot-dirty player) t)
          (mark-player-dirty (player-id player))
          (log-verbose "SWAP-INV: swapped slot ~a <-> slot ~a" slot-a slot-b)
          t)))))

(defun process-player-inventory-swap (player intent)
  ;; Process an inventory swap request from the client intent.
  (let ((slot-a (intent-requested-swap-slot-a intent))
        (slot-b (intent-requested-swap-slot-b intent)))
    (when (and slot-a slot-b)
      (log-verbose "PROCESS-SWAP: player=~a slot-a=~a slot-b=~a"
                   (and player (player-id player)) slot-a slot-b)
      (swap-inventory-slots player slot-a slot-b)
      ;; Clear the swap request
      (clear-requested-inventory-swap intent))))

(defun roll-loot-entry (entries)
  ;; Roll a single loot entry from ENTRIES.
  (let ((total (loop :for entry :in entries
                     :sum (loot-entry-weight entry))))
    (when (> total 0)
      (let ((roll (random total)))
        (loop :for entry :in entries
              :for weight = (loot-entry-weight entry)
              :do (decf roll weight)
              :when (< roll 0)
                :do (return entry))))))

(defun roll-loot-count (entry)
  ;; Roll a count between ENTRY's min and max.
  (let* ((min-count (loot-entry-min-count entry))
         (max-count (loot-entry-max-count entry))
         (span (max 0 (- max-count min-count))))
    (+ min-count (if (> span 0) (random (1+ span)) 0))))

(defun award-npc-loot (player npc)
  ;; Roll NPC loot and add it to the player's inventory.
  (let* ((table-id (npc-loot-table-id npc))
         (table (and table-id (find-loot-table table-id)))
         (inventory (player-inventory player)))
    (when (and table inventory)
      (let ((entries (loot-table-entries table))
            (rolls (loot-table-rolls table)))
        (dotimes (_index (max 0 rolls))
          (let ((entry (roll-loot-entry entries)))
            (when entry
              (let* ((count (roll-loot-count entry))
                     (item-id (loot-entry-item-id entry)))
                (grant-inventory-item player item-id count)))))))))
