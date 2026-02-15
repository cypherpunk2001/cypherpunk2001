(in-package #:mmorpg)

;;; ============================================================
;;; COMBAT.LISP TESTS
;;; ============================================================

(defun test-aabb-overlap-p ()
  "Test AABB collision detection (center + half-sizes)."
  ;; Overlapping boxes: center(0,0) half(5,5) and center(3,3) half(5,5)
  ;; Distance: |3-0|=3, sum of halves: 5+5=10, 3 < 10 = overlap
  (assert (aabb-overlap-p 0 0 5 5 3 3 5 5) () "aabb-overlap-p: overlapping")
  ;; Clearly separate boxes: center(0,0) half(2,2) and center(100,100) half(2,2)
  ;; Distance: 100, sum of halves: 4, 100 > 4 = no overlap
  (assert (not (aabb-overlap-p 0 0 2 2 100 100 2 2)) () "aabb-overlap-p: far apart")
  ;; Just touching (edge case): center(0,0) half(5,5) and center(10,0) half(5,5)
  ;; Distance x: 10, sum of halves: 10, 10 <= 10 = overlap (touching counts)
  (assert (aabb-overlap-p 0 0 5 5 10 0 5 5) () "aabb-overlap-p: just touching"))

(defun test-aabb-overlap-p-edge-cases ()
  "Test AABB with edge cases."
  ;; Same position
  (assert (aabb-overlap-p 5 5 2 2 5 5 2 2) () "aabb-overlap-p: same position")
  ;; One inside the other
  (assert (aabb-overlap-p 5 5 10 10 5 5 2 2) () "aabb-overlap-p: contained")
  ;; Zero size same pos
  (assert (aabb-overlap-p 5 5 0 0 5 5 0 0) () "aabb-overlap-p: zero size same pos"))

(defun test-melee-hit-chance ()
  "Test melee hit chance calculation with player combatants."
  (ensure-test-game-data)
  (let* ((attacker (make-player 0.0 0.0 :id 1))
         (defender (make-player 0.0 0.0 :id 2)))
    ;; With equal stats, should get reasonable chance
    (let ((chance (melee-hit-chance attacker defender)))
      (assert (>= chance 0.0) () "hit-chance: non-negative")
      (assert (<= chance 1.0) () "hit-chance: max 1.0")
      (assert (> chance 0.3) () "hit-chance: equal stats > 30%")
      (assert (< chance 0.7) () "hit-chance: equal stats < 70%"))))

(defun test-melee-hit-chance-clamped ()
  "Test hit chance is clamped between min and max."
  (ensure-test-game-data)
  (let* ((attacker (make-player 0.0 0.0 :id 1))
         (defender (make-player 0.0 0.0 :id 2)))
    ;; Boost attacker's attack stat
    (setf (skill-level (stat-block-attack (player-stats attacker))) 99)
    (let ((high-chance (melee-hit-chance attacker defender)))
      (assert (<= high-chance 0.95) () "hit-chance: capped at 95%"))
    ;; Now make defender very strong
    (setf (skill-level (stat-block-attack (player-stats attacker))) 1)
    (setf (skill-level (stat-block-defense (player-stats defender))) 99)
    (let ((low-chance (melee-hit-chance attacker defender)))
      (assert (>= low-chance 0.05) () "hit-chance: floored at 5%"))))

(defun test-melee-max-hit ()
  "Test max hit calculation from strength."
  (ensure-test-game-data)
  (let* ((weak (make-player 0.0 0.0 :id 1))
         (strong (make-player 0.0 0.0 :id 2)))
    (setf (skill-level (stat-block-strength (player-stats strong))) 50)
    (let ((weak-hit (melee-max-hit weak))
          (strong-hit (melee-max-hit strong)))
      (assert (> weak-hit 0) () "max-hit: positive damage")
      (assert (> strong-hit weak-hit) () "max-hit: scales with strength"))))

;;; ============================================================

;;; NEW COMBAT TESTS (Priority 1)
;;; ============================================================

(defun test-combatant-display-name ()
  "Test combatant display name for players and NPCs."
  (ensure-test-game-data)
  ;; Player display name - returns "Player" for all players
  (let ((player (make-player 0.0 0.0 :id 1)))
    (let ((name (combatant-display-name player)))
      (assert (stringp name) () "display-name: player returns string")
      (assert (string= name "Player") () "display-name: player = Player")))
  ;; NPC display name - use default-npc-archetype for reliability
  (let* ((archetype (or (gethash :street-punk *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 0.0 0.0 :archetype archetype :id 1)))
    (let ((name (combatant-display-name npc)))
      (assert (stringp name) () "display-name: npc returns string"))))

(defun test-find-npc-by-id ()
  "Test finding NPC by ID in array."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :street-punk *npc-archetypes*) (default-npc-archetype)))
         (npc1 (make-npc 0.0 0.0 :archetype archetype :id 100))
         (npc2 (make-npc 10.0 10.0 :archetype archetype :id 200))
         (npcs (vector npc1 npc2)))
    ;; Find existing
    (assert (eq (find-npc-by-id npcs 100) npc1) () "find-npc: id 100")
    (assert (eq (find-npc-by-id npcs 200) npc2) () "find-npc: id 200")
    ;; Not found
    (assert (null (find-npc-by-id npcs 999)) () "find-npc: not found")
    ;; Empty vector - uses loop :across which requires vector
    (assert (null (find-npc-by-id (vector) 100)) () "find-npc: empty")))

(defun test-roll-melee-damage ()
  "Test melee damage roll is within expected range."
  (ensure-test-game-data)
  (let ((player (make-player 0.0 0.0 :id 1)))
    (setf (skill-level (stat-block-strength (player-stats player))) 10)
    ;; Roll multiple times to test range
    (let ((max-hit (melee-max-hit player)))
      (dotimes (_ 10)
        (let ((damage (roll-melee-damage player)))
          (assert (>= damage 1) () "roll-damage: at least 1")
          (assert (<= damage max-hit) () "roll-damage: at most max-hit"))))))

(defun test-format-combat-log ()
  "Test combat log formatting."
  ;; Hit case
  (let ((log (format-combat-log "Player" "Goblin" t 0.75 0.50 10 5 :damage 8)))
    (assert (stringp log) () "combat-log: hit returns string")
    (assert (search "hit" log) () "combat-log: hit contains 'hit'")
    (assert (search "8" log) () "combat-log: hit contains damage"))
  ;; Miss case
  (let ((log (format-combat-log "Player" "Goblin" nil 0.25 0.50 10 5)))
    (assert (stringp log) () "combat-log: miss returns string")
    (assert (search "miss" log) () "combat-log: miss contains 'miss'"))
  ;; Kill case
  (let ((log (format-combat-log "Player" "Goblin" t 0.75 0.50 10 5 :damage 10 :killed t)))
    (assert (search "KILL" log) () "combat-log: kill contains 'KILL'"))
  ;; XP text
  (let ((log (format-combat-log "Player" "Goblin" t 0.75 0.50 10 5 :damage 5 :xp-text "+50 XP")))
    (assert (search "+50 XP" log) () "combat-log: contains xp text")))

(defun test-npc-respawn-seconds ()
  "Test NPC respawn time from archetype."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :street-punk *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 0.0 0.0 :archetype archetype :id 1)))
    (let ((seconds (npc-respawn-seconds npc)))
      (assert (numberp seconds) () "npc-respawn: returns number")
      (assert (>= seconds 0) () "npc-respawn: non-negative"))))

(defun test-npc-attack-cooldown ()
  "Test NPC attack cooldown from archetype."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :street-punk *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 0.0 0.0 :archetype archetype :id 1)))
    (let ((cooldown (npc-attack-cooldown npc)))
      (assert (numberp cooldown) () "npc-cooldown: returns number")
      (assert (> cooldown 0) () "npc-cooldown: positive"))))

(defun test-npc-attack-damage ()
  "Test NPC attack damage from archetype."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :street-punk *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 0.0 0.0 :archetype archetype :id 1)))
    (let ((damage (npc-attack-damage npc)))
      (assert (numberp damage) () "npc-damage: returns number")
      (assert (>= damage 0) () "npc-damage: non-negative"))))

(defun test-intent-attack-direction ()
  "Test attack direction from intent."
  (let ((player (make-player 100.0 100.0 :id 1))
        (intent (make-intent)))
    ;; Direction from movement input - returns :side for horizontal
    (set-intent-move intent 1.0 0.0)
    (multiple-value-bind (dir sign) (intent-attack-direction player intent)
      (assert (eq dir :side) () "attack-dir: side from dx")
      (assert (numberp sign) () "attack-dir: sign is number"))
    ;; Direction up/down from dy
    (set-intent-move intent 0.0 -1.0)
    (multiple-value-bind (dir sign) (intent-attack-direction player intent)
      (assert (eq dir :up) () "attack-dir: up from negative dy"))
    ;; No input and no target - returns nil
    (set-intent-move intent 0.0 0.0)
    (multiple-value-bind (dir sign) (intent-attack-direction player intent)
      (assert (null dir) () "attack-dir: nil when no input or target")
      (assert (zerop sign) () "attack-dir: zero sign when no input"))))

(defun test-target-in-range-p ()
  "Test if NPC is within targeting range."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :street-punk *npc-archetypes*) (default-npc-archetype)))
         (player (make-player 100.0 100.0 :id 1))
         (npc-near (make-npc 120.0 100.0 :archetype archetype :id 1))
         (npc-far (make-npc 500.0 500.0 :archetype archetype :id 2))
         (world (make-test-world :tile-size 32.0 :collision-half 12.0)))
    ;; Near NPC should be in range
    (assert (target-in-range-p player npc-near world) () "target-range: near in range")
    ;; Far NPC should be out of range
    (assert (not (target-in-range-p player npc-far world)) () "target-range: far out of range")))

(defun test-attack-hitbox ()
  "Test attack hitbox calculation."
  (let ((player (make-player 100.0 100.0 :id 1))
        (world (make-test-world :tile-size 32.0 :collision-half 12.0)))
    ;; Test each facing direction
    (dolist (facing '(:up :down :left :right))
      (setf (player-facing player) facing)
      (multiple-value-bind (cx cy half-w half-h) (attack-hitbox player world)
        (assert (numberp cx) () (format nil "hitbox ~a: cx is number" facing))
        (assert (numberp cy) () (format nil "hitbox ~a: cy is number" facing))
        (assert (> half-w 0) () (format nil "hitbox ~a: half-w positive" facing))
        (assert (> half-h 0) () (format nil "hitbox ~a: half-h positive" facing))))))

(defun test-npc-attack-range ()
  "Test NPC attack range calculation."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :street-punk *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 0.0 0.0 :archetype archetype :id 1))
         (world (make-test-world :tile-size 32.0 :collision-half 12.0)))
    (let ((range (npc-attack-range npc world)))
      (assert (numberp range) () "npc-range: returns number")
      (assert (> range 0) () "npc-range: positive"))))

;;; ============================================================

;;; ADDITIONAL COMBAT TESTS
;;; ============================================================

(defun test-player-attack-target-in-range-p ()
  "Test player attack target range check."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :street-punk *npc-archetypes*) (default-npc-archetype)))
         (world (make-test-world :tile-size 32.0 :collision-half 12.0))
         (player (make-player 100.0 100.0 :id 1))
         (npc-close (make-npc 110.0 100.0 :archetype archetype :id 1))
         (npc-far (make-npc 500.0 500.0 :archetype archetype :id 2)))
    ;; Set player facing to ensure hitbox is calculated
    (setf (player-facing player) :side
          (player-facing-sign player) 1.0)
    ;; Close NPC should be in attack range
    (assert (player-attack-target-in-range-p player npc-close world) ()
            "attack-in-range: close NPC in range")
    ;; Far NPC should not be in range
    (assert (not (player-attack-target-in-range-p player npc-far world)) ()
            "attack-in-range: far NPC not in range")))

;;; ============================================================


(defvar *tests-combat*
  '(test-aabb-overlap-p
    test-aabb-overlap-p-edge-cases
    test-melee-hit-chance
    test-melee-hit-chance-clamped
    test-melee-max-hit
    test-combatant-display-name
    test-find-npc-by-id
    test-roll-melee-damage
    test-format-combat-log
    test-npc-respawn-seconds
    test-npc-attack-cooldown
    test-npc-attack-damage
    test-intent-attack-direction
    test-target-in-range-p
    test-attack-hitbox
    test-npc-attack-range
    ;; Additional Combat Tests
    test-player-attack-target-in-range-p)
  "Combat domain test functions.")
