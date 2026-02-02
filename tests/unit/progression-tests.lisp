(in-package #:mmorpg)

;;; PROGRESSION.LISP TESTS
;;; ============================================================

(defun test-xp-to-level ()
  "Test XP to level conversion."
  (assert (= (xp->level 0) 1) () "xp->level: 0 XP = level 1")
  (assert (>= (xp->level 100) 1) () "xp->level: 100 XP >= level 1")
  (assert (> (xp->level 10000) (xp->level 100)) () "xp->level: more XP = higher level"))

(defun test-level-to-xp ()
  "Test level to XP conversion."
  (assert (= (level->xp 1) 0) () "level->xp: level 1 = 0 XP")
  (assert (> (level->xp 2) 0) () "level->xp: level 2 > 0 XP")
  (assert (> (level->xp 10) (level->xp 5)) () "level->xp: higher level = more XP"))

(defun test-xp-level-roundtrip ()
  "Test XP/level conversion consistency."
  ;; Level -> XP -> Level should return same level
  (dolist (level '(1 5 10 25 50 99))
    (let* ((xp (level->xp level))
           (back (xp->level xp)))
      (assert (= back level) ()
              (format nil "xp-level roundtrip: level ~d -> xp ~d -> level ~d" level xp back)))))

(defun test-valid-training-mode-p ()
  "Test training mode validation."
  (assert (valid-training-mode-p :balanced) () "training: balanced valid")
  (assert (valid-training-mode-p :attack) () "training: attack valid")
  (assert (valid-training-mode-p :strength) () "training: strength valid")
  (assert (valid-training-mode-p :defense) () "training: defense valid")
  (assert (not (valid-training-mode-p :invalid)) () "training: invalid rejected")
  (assert (not (valid-training-mode-p nil)) () "training: nil rejected"))

(defun test-normalize-training-mode ()
  "Test training mode normalization."
  (assert (eq (normalize-training-mode :balanced) :balanced) () "normalize: balanced")
  (assert (eq (normalize-training-mode :invalid) :balanced) () "normalize: invalid -> balanced")
  (assert (eq (normalize-training-mode nil) :balanced) () "normalize: nil -> balanced"))

(defun test-training-mode-label ()
  "Test training mode labels."
  (assert (stringp (training-mode-label :balanced)) () "label: balanced is string")
  (assert (stringp (training-mode-label :attack)) () "label: attack is string")
  (assert (not (string= (training-mode-label :attack) (training-mode-label :defense)))
          () "label: attack != defense"))

(defun test-combat-level ()
  "Test combat level calculation via stat-block-effective-level."
  (ensure-test-game-data)
  ;; Test that stat-block-effective-level returns integer for various skills
  (let ((player (make-player 0.0 0.0 :id 999)))
    (let ((stats (player-stats player)))
      (assert (integerp (stat-block-effective-level stats :attack)) () "combat-level: attack is int")
      (assert (integerp (stat-block-effective-level stats :strength)) () "combat-level: strength is int")
      (assert (integerp (stat-block-effective-level stats :defense)) () "combat-level: defense is int")
      (assert (>= (stat-block-effective-level stats :attack) 1) () "combat-level: attack >= 1"))))

(defun test-split-combat-xp ()
  "Test combat XP splitting."
  (ensure-test-game-data)
  (let ((player (make-player 0.0 0.0 :id 1)))
    ;; split-combat-xp takes (player amount), uses player's training mode
    (multiple-value-bind (atk str def hp) (split-combat-xp player 100)
      (assert (>= atk 0) () "split-xp: attack non-negative")
      (assert (>= str 0) () "split-xp: strength non-negative")
      (assert (>= def 0) () "split-xp: defense non-negative")
      (assert (>= hp 0) () "split-xp: hp non-negative")
      ;; Total should roughly equal input (may have rounding)
      (assert (<= (abs (- (+ atk str def hp) 100)) 5) () "split-xp: total approximately 100"))))

(defun test-inventory-slot-empty-p ()
  "Test inventory slot empty check."
  (let ((empty-slot (make-inventory-slot :item-id nil :count 0))
        (full-slot (make-inventory-slot :item-id :health-potion :count 5)))
    (assert (inventory-slot-empty-p empty-slot) () "slot-empty: nil item is empty")
    (assert (not (inventory-slot-empty-p full-slot)) () "slot-empty: has item not empty")))

(defun test-item-stack-size ()
  "Test item stack size lookup."
  (ensure-test-game-data)
  (let ((potion-stack (item-stack-size :health-potion))
        (sword-stack (item-stack-size :rusty-sword)))
    (assert (integerp potion-stack) () "stack-size: potion is integer")
    (assert (integerp sword-stack) () "stack-size: sword is integer")
    (assert (>= potion-stack 1) () "stack-size: at least 1")))

(defun test-equipment-slot-index ()
  "Test equipment slot index lookup."
  ;; Valid slots: :head :body :legs :weapon :offhand :accessory
  (assert (integerp (equipment-slot-index :weapon)) () "slot-index: weapon is integer")
  (assert (integerp (equipment-slot-index :body)) () "slot-index: body is integer")
  (assert (/= (equipment-slot-index :weapon) (equipment-slot-index :body))
          () "slot-index: weapon != body"))

(defun test-player-adjacent-to-tile-p ()
  "Test adjacent tile check."
  ;; Same tile
  (assert (player-adjacent-to-tile-p 5 5 5 5) () "adjacent: same tile")
  ;; Cardinal directions
  (assert (player-adjacent-to-tile-p 5 5 6 5) () "adjacent: right")
  (assert (player-adjacent-to-tile-p 5 5 4 5) () "adjacent: left")
  (assert (player-adjacent-to-tile-p 5 5 5 6) () "adjacent: down")
  (assert (player-adjacent-to-tile-p 5 5 5 4) () "adjacent: up")
  ;; Diagonals
  (assert (player-adjacent-to-tile-p 5 5 6 6) () "adjacent: down-right")
  (assert (player-adjacent-to-tile-p 5 5 4 4) () "adjacent: up-left")
  ;; Too far
  (assert (not (player-adjacent-to-tile-p 5 5 7 5)) () "adjacent: 2 tiles right")
  (assert (not (player-adjacent-to-tile-p 5 5 5 7)) () "adjacent: 2 tiles down"))

(defun test-swap-inventory-slots ()
  "Test inventory slot swapping."
  (ensure-test-game-data)
  (let ((player (make-player 0.0 0.0 :id 1)))
    ;; Set up two slots
    (let ((slots (inventory-slots (player-inventory player))))
      (setf (aref slots 0) (make-inventory-slot :item-id :health-potion :count 5))
      (setf (aref slots 1) (make-inventory-slot :item-id :rusty-sword :count 1)))
    ;; Swap via player
    (swap-inventory-slots player 0 1)
    ;; Verify swap
    (let ((slots (inventory-slots (player-inventory player))))
      (assert (eq (inventory-slot-item-id (aref slots 0)) :rusty-sword) () "swap: slot 0 has sword")
      (assert (eq (inventory-slot-item-id (aref slots 1)) :health-potion) () "swap: slot 1 has potion")
      (assert (= (inventory-slot-count (aref slots 0)) 1) () "swap: slot 0 count")
      (assert (= (inventory-slot-count (aref slots 1)) 5) () "swap: slot 1 count"))))

;;; ============================================================

;;; NEW PROGRESSION TESTS (Priority 1)
;;; ============================================================

(defun test-update-skill-level ()
  "Test skill level updates from XP."
  (let ((skill (make-skill :level 1 :xp 0)))
    ;; No change at 0 XP
    (multiple-value-bind (old new) (update-skill-level skill)
      (assert (= old 1) () "update-skill: old level 1")
      (assert (= new 1) () "update-skill: new level 1"))
    ;; Add enough XP for level 2
    (setf (skill-xp skill) (level->xp 2))
    (multiple-value-bind (old new) (update-skill-level skill)
      (assert (= old 1) () "update-skill: old was 1")
      (assert (= new 2) () "update-skill: new is 2"))))

(defun test-clamp-player-hp ()
  "Test player HP clamping."
  (ensure-test-game-data)
  (let ((player (make-player 0.0 0.0 :id 1)))
    ;; Set HP above max
    (let ((max-hp (combatant-max-hp player)))
      (setf (player-hp player) (+ max-hp 100))
      (clamp-player-hp player)
      (assert (<= (player-hp player) max-hp) () "clamp-hp: not above max"))
    ;; Set HP negative
    (setf (player-hp player) -10)
    (clamp-player-hp player)
    (assert (>= (player-hp player) 0) () "clamp-hp: not negative")))

(defun test-format-xp-awards ()
  "Test XP awards formatting."
  ;; All zeros - no format
  (let ((result (format-xp-awards 0 0 0 0)))
    (assert (null result) () "format-xp: all zeros -> nil"))
  ;; Single stat
  (let ((result (format-xp-awards 10 0 0 0)))
    (assert (stringp result) () "format-xp: attack only")
    (assert (search "A+" result) () "format-xp: contains A+"))
  ;; Multiple stats
  (let ((result (format-xp-awards 10 20 0 5)))
    (assert (stringp result) () "format-xp: multiple stats")
    (assert (search "A+" result) () "format-xp: contains A+")
    (assert (search "S+" result) () "format-xp: contains S+")
    (assert (search "HP+" result) () "format-xp: contains HP+")))

(defun test-item-display-name ()
  "Test item display name lookup."
  (ensure-test-game-data)
  ;; Known item
  (let ((name (item-display-name :health-potion)))
    (assert (stringp name) () "item-name: returns string")
    (assert (> (length name) 0) () "item-name: not empty"))
  ;; Unknown item returns fallback
  (let ((name (item-display-name :nonexistent-item-12345)))
    (assert (stringp name) () "item-name: unknown returns string"))
  ;; Nil item
  (let ((name (item-display-name nil)))
    (assert (string= name "Unknown") () "item-name: nil -> Unknown")))

(defun test-inventory-slot-label ()
  "Test inventory slot label formatting."
  (ensure-test-game-data)
  ;; Single item
  (let ((label (inventory-slot-label :health-potion 1)))
    (assert (stringp label) () "slot-label: returns string")
    (assert (not (search "x" label)) () "slot-label: single no count"))
  ;; Stacked items
  (let ((label (inventory-slot-label :health-potion 5)))
    (assert (stringp label) () "slot-label: stacked returns string")
    (assert (search "x5" label) () "slot-label: shows count")))

(defun test-inventory-add ()
  "Test adding items to inventory."
  (ensure-test-game-data)
  ;; Create a player which has a properly initialized inventory
  (let* ((player (make-player 0.0 0.0 :id 999))
         (inventory (player-inventory player))
         (slots (inventory-slots inventory)))
    ;; Clear all slots first
    (dotimes (i (length slots))
      (setf (aref slots i) (make-inventory-slot :item-id nil :count 0)))
    ;; Add items - returns leftover (coins always stack)
    (let ((leftover (inventory-add inventory :coins 100)))
      (assert (= leftover 0) () "inv-add: no leftover")
      ;; Check that coins were added somewhere
      (let ((total 0))
        (loop :for slot :across slots
              :when (eq (inventory-slot-item-id slot) :coins)
                :do (incf total (inventory-slot-count slot)))
        (assert (= total 100) () "inv-add: total coins = 100")))))

(defun test-inventory-remove ()
  "Test removing items from inventory."
  (ensure-test-game-data)
  ;; Create a player which has a properly initialized inventory
  (let* ((player (make-player 0.0 0.0 :id 999))
         (inventory (player-inventory player))
         (slots (inventory-slots inventory)))
    ;; Clear all slots first
    (dotimes (i (length slots))
      (setf (aref slots i) (make-inventory-slot :item-id nil :count 0)))
    ;; Add then remove
    (setf (aref slots 0) (make-inventory-slot :item-id :health-potion :count 5))
    (let ((leftover (inventory-remove inventory :health-potion 3)))
      (assert (= leftover 0) () "inv-remove: removed 3")
      (assert (= (inventory-slot-count (aref slots 0)) 2) () "inv-remove: 2 left"))
    ;; Remove more than exists
    (let ((leftover (inventory-remove inventory :health-potion 10)))
      (assert (= leftover 8) () "inv-remove: 8 couldn't be removed"))))

(defun test-roll-loot-entry ()
  "Test loot entry rolling."
  (let ((entries (list (%make-loot-entry :item-id :coins :weight 100 :min-count 1 :max-count 1)
                       (%make-loot-entry :item-id :health-potion :weight 50 :min-count 1 :max-count 1))))
    ;; Roll multiple times - should get entries
    (let ((results nil))
      (dotimes (_ 20)
        (let ((entry (roll-loot-entry entries)))
          (when entry
            (pushnew (loot-entry-item-id entry) results))))
      (assert (member :coins results) () "loot-roll: got coins")
      ;; Potion might not appear every 20 rolls but that's probabilistic
      )))

(defun test-roll-loot-count ()
  "Test loot count rolling."
  (let ((entry (%make-loot-entry :item-id :coins :weight 100 :min-count 5 :max-count 10)))
    (dotimes (_ 20)
      (let ((count (roll-loot-count entry)))
        (assert (>= count 5) () "loot-count: at least min")
        (assert (<= count 10) () "loot-count: at most max"))))
  ;; Single value (min = max)
  (let ((entry (%make-loot-entry :item-id :coins :weight 100 :min-count 3 :max-count 3)))
    (let ((count (roll-loot-count entry)))
      (assert (= count 3) () "loot-count: exact value"))))

(defun test-object-respawn-seconds ()
  "Test object respawn time lookup."
  (ensure-test-game-data)
  ;; Get archetype with respawn
  (let ((archetype (find-object-archetype :health-potion-drop)))
    ;; If archetype exists, test it
    (when archetype
      (let ((seconds (object-respawn-seconds archetype)))
        (assert (numberp seconds) () "respawn-seconds: returns number")
        (assert (>= seconds 0.0) () "respawn-seconds: non-negative"))))
  ;; Nil archetype
  (let ((seconds (object-respawn-seconds nil)))
    (assert (= seconds 0.0) () "respawn-seconds: nil -> 0")))

(defun test-object-respawnable-p ()
  "Test object respawnable check.
   Task 5.5: Updated to use zone-object structs."
  ;; Default (respawnable t)
  (let ((obj (%make-zone-object :id :test :x 0 :y 0 :count 1 :base-count 1
                                :respawn 0.0 :respawnable t :snapshot-dirty nil)))
    (assert (object-respawnable-p obj) () "respawnable: explicit true"))
  ;; Not respawnable
  (let ((obj (%make-zone-object :id :test :x 0 :y 0 :count 1 :base-count 1
                                :respawn 0.0 :respawnable nil :snapshot-dirty nil)))
    (assert (not (object-respawnable-p obj)) () "respawnable: explicit false")))

(defun test-object-respawn-timer ()
  "Test object respawn timer extraction.
   Task 5.5: Updated to use zone-object structs."
  ;; No timer (0.0)
  (let ((obj (%make-zone-object :id :test :x 0 :y 0 :count 1 :base-count 1
                                :respawn 0.0 :respawnable t :snapshot-dirty nil)))
    (assert (= (object-respawn-timer obj) 0.0) () "respawn-timer: no timer -> 0"))
  ;; With timer
  (let ((obj (%make-zone-object :id :test :x 0 :y 0 :count 1 :base-count 1
                                :respawn 5.5 :respawnable t :snapshot-dirty nil)))
    (assert (= (object-respawn-timer obj) 5.5) () "respawn-timer: has timer"))
  ;; Timer at 0
  (let ((obj (%make-zone-object :id :test :x 0 :y 0 :count 1 :base-count 1
                                :respawn 0.0 :respawnable t :snapshot-dirty nil)))
    (assert (= (object-respawn-timer obj) 0.0) () "respawn-timer: zero")))

;;; ============================================================

;;; ADDITIONAL PROGRESSION TESTS
;;; ============================================================

(defun test-melee-hit-p ()
  "Test melee hit roll wrapper."
  (let* ((attacker (make-player 0.0 0.0 :id 1))
         (defender (make-player 0.0 0.0 :id 2)))
    ;; Run multiple times - should return boolean
    (dotimes (_ 10)
      (let ((result (melee-hit-p attacker defender)))
        (assert (or (eq result t) (eq result nil)) ()
                "melee-hit-p: returns boolean")))))

(defun test-format-skill-hud-line ()
  "Test skill HUD line formatting."
  ;; Nil skill
  (let ((line (format-skill-hud-line "ATT" nil)))
    (assert (stringp line) () "skill-hud: returns string for nil")
    (assert (search "--" line) () "skill-hud: nil shows --"))
  ;; Valid skill
  (let* ((skill (make-skill :level 5 :xp 500))
         (line (format-skill-hud-line "ATT" skill)))
    (assert (stringp line) () "skill-hud: returns string")
    (assert (search "ATT" line) () "skill-hud: contains label")
    (assert (search "5" line) () "skill-hud: contains level")))

(defun test-object-entry-count ()
  "Test object entry count extraction.
   Task 5.5: Updated to use zone-object structs."
  ;; Object with explicit count
  (let ((obj (%make-zone-object :id :coins :x 0 :y 0 :count 50 :base-count 50
                                :respawn 0.0 :respawnable t :snapshot-dirty nil)))
    (assert (= (object-entry-count obj nil) 50) () "entry-count: explicit count"))
  ;; Object with count 1 - falls back to archetype or 1
  (let ((obj (%make-zone-object :id :coins :x 0 :y 0 :count 1 :base-count 1
                                :respawn 0.0 :respawnable t :snapshot-dirty nil)))
    (let ((count (object-entry-count obj nil)))
      (assert (= count 1) () "entry-count: count 1, no arch -> 1")))
  ;; With archetype
  (ensure-test-game-data)
  (let* ((archetype (find-object-archetype :health-potion-drop))
         (obj (%make-zone-object :id :health-potion-drop :x 0 :y 0 :count 1 :base-count 1
                                 :respawn 0.0 :respawnable t :snapshot-dirty nil)))
    (when archetype
      (let ((count (object-entry-count obj archetype)))
        (assert (>= count 1) () "entry-count: archetype count")))))

;;; ============================================================

;;; FINAL PROGRESSION TESTS
;;; ============================================================

(defun test-award-skill-xp ()
  "Test awarding XP to a skill."
  (let ((skill (make-skill :level 1 :xp 0)))
    ;; Award some XP
    (award-skill-xp skill 100)
    (assert (= (skill-xp skill) 100) () "award-xp: xp increased")
    ;; Level should update if XP threshold crossed
    (award-skill-xp skill 1000)
    (assert (>= (skill-level skill) 1) () "award-xp: level at least 1")
    ;; Negative amounts treated as 0
    (let ((before-xp (skill-xp skill)))
      (award-skill-xp skill -50)
      (assert (= (skill-xp skill) before-xp) () "award-xp: negative -> no change"))))

(defun test-apply-item-modifiers ()
  "Test applying item stat modifiers."
  (ensure-test-game-data)
  (let* ((player (make-player 0.0 0.0 :id 1))
         (stats (player-stats player))
         (mods (stat-block-modifiers stats)))
    ;; Find an item with attack bonus
    (let ((item (find-item-archetype :iron-sword)))
      (when (and item mods (> (or (item-archetype-attack item) 0) 0))
        (let ((before-attack (stat-modifiers-attack mods)))
          ;; Apply item (+1 direction)
          (apply-item-modifiers stats :iron-sword 1)
          (assert (> (stat-modifiers-attack mods) before-attack) ()
                  "apply-mods: attack increased")
          ;; Remove item (-1 direction)
          (apply-item-modifiers stats :iron-sword -1)
          (assert (= (stat-modifiers-attack mods) before-attack) ()
                  "apply-mods: attack restored")))))
  ;; Nil stats should not crash
  (assert (null (apply-item-modifiers nil :iron-sword 1)) ()
          "apply-mods: nil stats -> nil"))

;;; ============================================================


(defvar *tests-progression*
  '(test-xp-to-level
    test-level-to-xp
    test-xp-level-roundtrip
    test-valid-training-mode-p
    test-normalize-training-mode
    test-training-mode-label
    test-combat-level
    test-split-combat-xp
    test-inventory-slot-empty-p
    test-item-stack-size
    test-equipment-slot-index
    test-player-adjacent-to-tile-p
    test-swap-inventory-slots
    test-update-skill-level
    test-clamp-player-hp
    test-format-xp-awards
    test-item-display-name
    test-inventory-slot-label
    test-inventory-add
    test-inventory-remove
    test-roll-loot-entry
    test-roll-loot-count
    test-object-respawn-seconds
    test-object-respawnable-p
    test-object-respawn-timer
    ;; Additional Progression Tests
    test-melee-hit-p
    test-format-skill-hud-line
    test-object-entry-count
    ;; Final Progression Tests
    test-award-skill-xp
    test-apply-item-modifiers)
  "Progression domain test functions.")
