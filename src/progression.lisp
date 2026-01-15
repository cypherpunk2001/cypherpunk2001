;; NOTE: If you change behavior here, update docs/progression.md :)
(in-package #:mmorpg)

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

(defun apply-hitpoints-level-up (player old new)
  ;; Increase current HP when hitpoints levels increase.
  (when (> new old)
    (incf (player-hp player) (- new old)))
  (let ((max-hp (combatant-max-hp player)))
    (when (> (player-hp player) max-hp)
      (setf (player-hp player) max-hp))))

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
         (hitpoints-xp 0))
    (when (and stats (> xp 0))
      (multiple-value-setq (attack-xp strength-xp defense-xp hitpoints-xp)
        (split-combat-xp player xp))
      (when (> attack-xp 0)
        (award-skill-xp (stat-block-attack stats) attack-xp))
      (when (> strength-xp 0)
        (award-skill-xp (stat-block-strength stats) strength-xp))
      (when (> defense-xp 0)
        (award-skill-xp (stat-block-defense stats) defense-xp))
      (when (> hitpoints-xp 0)
        (award-hitpoints-xp player hitpoints-xp))
      (mark-player-hud-stats-dirty player))
    (values attack-xp strength-xp defense-xp hitpoints-xp)))

(defun mark-player-hud-stats-dirty (player)
  ;; Flag the player's HUD stats cache for refresh.
  (when player
    (setf (player-hud-stats-dirty player) t)))

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
                (inventory-add inventory item-id count)))))))))
