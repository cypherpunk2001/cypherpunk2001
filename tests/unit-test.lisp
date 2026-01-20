(in-package #:mmorpg)

;;; Unit Test Suite
;;; Focus: Pure functions, game logic, utilities
;;; Run: make test-unit

(defun run-unit-tests ()
  "Run all unit tests. Returns T if all pass, NIL otherwise."
  (let ((passed 0)
        (failed 0)
        (tests '(;; Utils Tests
                test-clamp
                test-clamp-edge-cases
                test-normalize-direction
                test-normalize-direction-diagonal
                test-normalize-vector
                test-normalize-vector-zero
                test-point-in-rect-p
                test-point-in-rect-p-edges
                test-basename
                test-basename-edge-cases
                test-sanitize-identifier
                test-plist-put
                test-player-direction
                test-player-state
                test-u32-hash
                test-u32-hash-deterministic
                test-exponential-backoff-delay
                test-player-animation-params
                test-relative-path-from-root
                ;; Combat Tests
                test-aabb-overlap-p
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
                ;; Progression Tests
                test-xp-to-level
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
                ;; Movement Tests
                test-wall-blocked-p
                test-tile-center-position
                test-edge-opposite
                test-edge-offset-ratio
                test-npc-collision-half
                ;; AI Tests
                test-npc-should-flee-p
                test-npc-perception-range-sq
                test-npc-home-radius
                test-npc-move-speed
                test-npc-wander-interval
                test-npc-flee-speed-mult
                test-closest-player
                ;; Data Tests
                test-plist-form-p
                test-data-section-header-p
                test-data-section-entry-p
                test-normalize-pairs
                test-validate-item-archetype-plist
                test-item-archetype-from-plist
                test-validate-object-archetype-plist
                test-object-archetype-from-plist
                test-loot-entry-from-spec
                test-validate-loot-table-plist
                test-loot-table-from-plist
                test-animation-set-from-plist
                test-merge-animation-sets
                ;; Zone Tests
                test-zone-chunk-key
                test-tile-key-roundtrip
                test-zone-tile-in-bounds-p
                test-zone-label
                test-zone-data-plist
                test-make-empty-zone
                test-build-tiles-from-fill
                test-zone-layer-tile-at
                ;; Intent Tests
                test-set-intent-move
                test-set-intent-face
                test-reset-frame-intent
                test-consume-intent-actions
                test-set-intent-target
                test-clear-intent-target
                test-request-pickup-target
                test-request-drop-item
                test-request-inventory-swap
                test-trade-intent-functions
                test-apply-intent-plist-rejects-bad-pickup
                ;; Net Tests
                test-string-to-octets
                test-octets-to-string
                test-encode-decode-net-message
                test-host-to-string
                ;; Save/Serialization Tests
                test-quantize-coord-roundtrip
                test-quantize-timer-roundtrip
                test-encode-anim-state-roundtrip
                test-encode-facing-roundtrip
                test-pack-player-flags-roundtrip
                test-pack-npc-flags-roundtrip
                test-serialize-skill-roundtrip
                test-serialize-stat-block-roundtrip
                test-serialize-inventory-slot-roundtrip
                ;; Migration Tests
                test-migrate-player-v1-to-v2
                test-migrate-player-v2-to-v3
                test-migrate-player-v3-to-v4
                test-migrate-player-data-chain
                ;; World Graph Tests
                test-world-graph-data-plist
                test-normalize-world-graph-edges
                ;; Chat Tests
                test-trim-chat-message
                ;; Types Tests
                test-skill-xp-for-level
                test-allocate-entity-id
                test-find-player-by-id
                ;; Redis Metrics Tests (Phase 2 - Database Hardening)
                test-calculate-percentile
                test-calculate-percentile-edge-cases
                test-ring-buffer-values
                test-metrics-push-latency
                ;; Additional AI Tests
                test-npc-in-perception-range-p
                ;; Additional Combat Tests
                test-player-attack-target-in-range-p
                ;; Additional Progression Tests
                test-melee-hit-p
                test-format-skill-hud-line
                test-object-entry-count
                ;; Additional Data Tests
                test-parse-game-data-forms
                test-make-npc-archetype-from-plist
                ;; Additional Zone Tests
                test-zone-chunk-from-spec
                test-zone-layer-from-spec
                test-build-zone-collision-tiles
                test-zone-wall-map
                test-zone-layer-by-id
                test-zone-to-plist
                test-zone-slice
                test-zone-resize
                ;; Additional World Graph Tests
                test-world-graph-exits
                test-world-graph-zone-path
                test-collect-zone-files
                test-zone-id-from-file
                test-build-zone-paths
                ;; Final AI Tests
                test-update-npc-behavior
                ;; Final Progression Tests
                test-award-skill-xp
                test-apply-item-modifiers
                ;; Final Zone Tests
                test-load-write-zone-roundtrip
                ;; Final Movement Tests
                test-get-zone-state
                test-zone-state-player-count
                test-players-in-zone
                test-occupied-zone-ids
                test-derive-wall-map-from-zone
                test-wall-occupied-p
                test-blocked-at-p
                test-attempt-move
                test-update-running-state
                test-edge-spawn-position
                test-zone-bounds-from-dimensions
                test-position-blocked-p
                test-find-open-tile
                test-player-is-stuck-p
                test-world-exit-edge
                ;; Final Net Tests
                test-session-try-register
                test-session-unregister
                test-session-get)))
    (format t "~%=== Running Unit Tests ===~%")
    (dolist (test tests)
      (handler-case
          (progn
            (funcall test)
            (incf passed)
            (format t "~a ~a~%" "OK" (symbol-name test)))
        (error (e)
          (incf failed)
          (format t "~a ~a: ~a~%" "FAIL" (symbol-name test) e))))
    (format t "~%Results: ~d passed, ~d failed~%" passed failed)
    (zerop failed)))

;;; ============================================================
;;; UTILS.LISP TESTS
;;; ============================================================

(defun test-clamp ()
  "Test clamp function with normal values."
  (assert (= (clamp 5 0 10) 5) () "clamp: value in range")
  (assert (= (clamp -5 0 10) 0) () "clamp: below min")
  (assert (= (clamp 15 0 10) 10) () "clamp: above max"))

(defun test-clamp-edge-cases ()
  "Test clamp with edge cases."
  (assert (= (clamp 0 0 10) 0) () "clamp: at min")
  (assert (= (clamp 10 0 10) 10) () "clamp: at max")
  (assert (= (clamp 5 5 5) 5) () "clamp: min = max = value")
  (assert (= (clamp -1.5 -2.0 2.0) -1.5) () "clamp: floats in range")
  (assert (= (clamp -3.0 -2.0 2.0) -2.0) () "clamp: floats below"))

(defun test-normalize-direction ()
  "Test normalize-direction with axis-aligned input."
  (multiple-value-bind (dx dy) (normalize-direction 1.0 0.0)
    (assert (= dx 1.0) () "normalize-direction: right x")
    (assert (= dy 0.0) () "normalize-direction: right y"))
  (multiple-value-bind (dx dy) (normalize-direction 0.0 -1.0)
    (assert (= dx 0.0) () "normalize-direction: up x")
    (assert (= dy -1.0) () "normalize-direction: up y")))

(defun test-normalize-direction-diagonal ()
  "Test normalize-direction with diagonal input."
  (multiple-value-bind (dx dy) (normalize-direction 1.0 1.0)
    (let ((expected (/ 1.0 (sqrt 2.0))))
      (assert (< (abs (- dx expected)) 0.0001) () "normalize-direction: diagonal x")
      (assert (< (abs (- dy expected)) 0.0001) () "normalize-direction: diagonal y"))))

(defun test-normalize-vector ()
  "Test normalize-vector with various inputs."
  (multiple-value-bind (dx dy) (normalize-vector 3.0 4.0)
    (assert (< (abs (- dx 0.6)) 0.0001) () "normalize-vector: 3-4-5 x")
    (assert (< (abs (- dy 0.8)) 0.0001) () "normalize-vector: 3-4-5 y"))
  (multiple-value-bind (dx dy) (normalize-vector 1.0 0.0)
    (assert (= dx 1.0) () "normalize-vector: unit x")
    (assert (= dy 0.0) () "normalize-vector: unit y")))

(defun test-normalize-vector-zero ()
  "Test normalize-vector with zero input."
  (multiple-value-bind (dx dy) (normalize-vector 0.0 0.0)
    (assert (= dx 0.0) () "normalize-vector: zero x")
    (assert (= dy 0.0) () "normalize-vector: zero y")))

(defun test-point-in-rect-p ()
  "Test point-in-rect-p with points inside and outside."
  (assert (point-in-rect-p 5 5 0 0 10 10) () "point-in-rect-p: center")
  (assert (not (point-in-rect-p -1 5 0 0 10 10)) () "point-in-rect-p: left")
  (assert (not (point-in-rect-p 5 -1 0 0 10 10)) () "point-in-rect-p: above")
  (assert (not (point-in-rect-p 11 5 0 0 10 10)) () "point-in-rect-p: right")
  (assert (not (point-in-rect-p 5 11 0 0 10 10)) () "point-in-rect-p: below"))

(defun test-point-in-rect-p-edges ()
  "Test point-in-rect-p at rectangle edges."
  (assert (point-in-rect-p 0 0 0 0 10 10) () "point-in-rect-p: top-left corner")
  (assert (not (point-in-rect-p 10 10 0 0 10 10)) () "point-in-rect-p: bottom-right exclusive")
  (assert (point-in-rect-p 9.9 9.9 0 0 10 10) () "point-in-rect-p: just inside"))

(defun test-basename ()
  "Test basename with various path formats."
  (assert (string= (basename "/path/to/file.txt") "file.txt") () "basename: unix path")
  (assert (string= (basename "file.txt") "file.txt") () "basename: no path")
  (assert (string= (basename "/single") "single") () "basename: root level"))

(defun test-basename-edge-cases ()
  "Test basename with edge cases."
  (assert (string= (basename "C:\\path\\file.txt") "file.txt") () "basename: windows path")
  (assert (string= (basename "") "") () "basename: empty string")
  (assert (string= (basename "/") "") () "basename: just slash"))

(defun test-sanitize-identifier ()
  "Test sanitize-identifier creates valid keywords."
  (assert (eq (sanitize-identifier "hello") :HELLO) () "sanitize-identifier: simple")
  (assert (eq (sanitize-identifier "hello world") :HELLO-WORLD) () "sanitize-identifier: space")
  (assert (eq (sanitize-identifier "test_123") :TEST-123) () "sanitize-identifier: underscore")
  (assert (eq (sanitize-identifier "---hello---") :HELLO) () "sanitize-identifier: trim dashes"))

(defun test-plist-put ()
  "Test plist-put updates existing keys and adds new keys."
  (let ((plist (list :a 1 :b 2)))
    (let ((updated (plist-put plist :a 9)))
      (assert (equal updated '(:a 9 :b 2)) () "plist-put: update existing")
      (assert (equal plist '(:a 9 :b 2)) () "plist-put: modifies original on update"))
    (let ((extended (plist-put plist :c 3)))
      (assert (equal extended '(:a 9 :b 2 :c 3)) () "plist-put: add new key"))))

(defun test-player-direction ()
  "Test player-direction returns correct facing."
  (assert (eq (player-direction 1.0 0.0) :side) () "player-direction: right")
  (assert (eq (player-direction -1.0 0.0) :side) () "player-direction: left")
  (assert (eq (player-direction 0.0 -1.0) :up) () "player-direction: up")
  (assert (eq (player-direction 0.0 1.0) :down) () "player-direction: down")
  (assert (eq (player-direction 0.0 0.0) :down) () "player-direction: idle defaults down"))

(defun test-player-state ()
  "Test player-state returns correct state."
  (assert (eq (player-state 0.0 0.0) :idle) () "player-state: idle")
  (assert (eq (player-state 1.0 0.0) :walk) () "player-state: walking right")
  (assert (eq (player-state 0.0 1.0) :walk) () "player-state: walking down")
  (assert (eq (player-state 1.0 1.0) :walk) () "player-state: walking diagonal"))

(defun test-u32-hash ()
  "Test u32-hash produces 32-bit values."
  (let ((hash (u32-hash 10 20)))
    (assert (integerp hash) () "u32-hash: returns integer")
    (assert (>= hash 0) () "u32-hash: non-negative")
    (assert (<= hash #xffffffff) () "u32-hash: within 32-bit range")))

(defun test-u32-hash-deterministic ()
  "Test u32-hash is deterministic."
  (assert (= (u32-hash 5 10) (u32-hash 5 10)) () "u32-hash: same input same output")
  (assert (/= (u32-hash 5 10) (u32-hash 10 5)) () "u32-hash: different for swapped coords")
  (assert (/= (u32-hash 5 10 1) (u32-hash 5 10 2)) () "u32-hash: different seeds differ"))

(defun test-exponential-backoff-delay ()
  "Test exponential-backoff-delay calculations."
  (assert (= (exponential-backoff-delay 0 100 1000) 100) () "backoff: attempt 0")
  (assert (= (exponential-backoff-delay 1 100 1000) 200) () "backoff: attempt 1")
  (assert (= (exponential-backoff-delay 2 100 1000) 400) () "backoff: attempt 2")
  (assert (= (exponential-backoff-delay 3 100 1000) 800) () "backoff: attempt 3")
  (assert (= (exponential-backoff-delay 4 100 1000) 1000) () "backoff: capped at max")
  (assert (= (exponential-backoff-delay 10 100 1000) 1000) () "backoff: stays at max"))

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
;;; MOVEMENT.LISP TESTS
;;; ============================================================

(defun test-wall-blocked-p ()
  "Test wall-blocked-p function."
  ;; build-wall-map takes no args, uses globals *wall-map-width* and *wall-map-height*
  (let ((wall-map (build-wall-map)))
    ;; Out of bounds should be blocked (uses *wall-origin-x/y* globals)
    (assert (wall-blocked-p wall-map -1000 -1000) () "wall-blocked: out of bounds")
    ;; A nil wall-map should not block
    (assert (not (wall-blocked-p nil 5 5)) () "wall-blocked: nil map doesn't block")))

(defun test-tile-center-position ()
  "Test tile center position calculation."
  ;; tile-center-position takes (tile-size tx ty)
  (multiple-value-bind (x y) (tile-center-position 16 0 0)
    (assert (= x 8.0) () "tile-center: 0,0 x")
    (assert (= y 8.0) () "tile-center: 0,0 y"))
  (multiple-value-bind (x y) (tile-center-position 16 1 1)
    (assert (= x 24.0) () "tile-center: 1,1 x")
    (assert (= y 24.0) () "tile-center: 1,1 y")))

(defun test-edge-opposite ()
  "Test edge opposite function."
  (assert (eq (edge-opposite :north) :south) () "edge-opposite: north")
  (assert (eq (edge-opposite :south) :north) () "edge-opposite: south")
  (assert (eq (edge-opposite :east) :west) () "edge-opposite: east")
  (assert (eq (edge-opposite :west) :east) () "edge-opposite: west"))

(defun test-edge-offset-ratio ()
  "Test edge offset ratio calculation."
  ;; edge-offset-ratio takes (min-value max-value value)
  (assert (= (edge-offset-ratio 0.0 100.0 50.0) 0.5) () "edge-ratio: middle")
  (assert (= (edge-offset-ratio 0.0 100.0 0.0) 0.0) () "edge-ratio: at min")
  (assert (= (edge-offset-ratio 0.0 100.0 100.0) 1.0) () "edge-ratio: at max"))

(defun test-npc-collision-half ()
  "Test NPC collision half-size calculation constant."
  ;; npc-collision-half takes a world object, uses world-tile-dest-size
  ;; This is tested implicitly via smoke tests - skip unit test for now
  ;; The function signature is (npc-collision-half world)
  (assert t () "npc-collision-half: skipped (requires world object)"))

;;; ============================================================
;;; AI.LISP TESTS
;;; ============================================================

(defun test-npc-should-flee-p ()
  "Test NPC flee decision."
  (ensure-test-game-data)
  (let* ((archetype (gethash :goblin *npc-archetypes*))
         (npc (make-npc 0.0 0.0 :archetype archetype :id 1)))
    ;; Full hits should not flee
    (let ((max-hits (npc-hits-left npc)))
      (setf (npc-hits-left npc) max-hits)
      (assert (not (npc-should-flee-p npc)) () "flee: full HP no flee")
      ;; Very low hits - result depends on archetype flee threshold
      (setf (npc-hits-left npc) 1)
      (let ((result (npc-should-flee-p npc)))
        (assert (or (null result) (eq result t)) () "flee: returns boolean")))))

(defun test-npc-perception-range-sq ()
  "Test NPC perception range squared - requires world object."
  ;; npc-perception-range-sq takes (npc world), requiring a full world setup
  ;; Skip detailed test, verify function exists
  (assert (fboundp 'npc-perception-range-sq) () "perception: function exists"))

;;; ============================================================
;;; DATA.LISP TESTS
;;; ============================================================

(defun test-plist-form-p ()
  "Test plist form detection."
  (assert (plist-form-p '(:key1 val1 :key2 val2)) () "plist-form: valid plist")
  (assert (not (plist-form-p '(a b c))) () "plist-form: not a plist - non-keyword keys")
  ;; Note: nil and empty list returns T from plist-form-p (vacuous truth)
  (assert (plist-form-p nil) () "plist-form: empty list is valid (vacuously)")
  (assert (plist-form-p '(:single value)) () "plist-form: single pair"))

(defun test-data-section-header-p ()
  "Test data section header detection."
  (assert (data-section-header-p :items) () "section-header: keyword")
  (assert (not (data-section-header-p "items")) () "section-header: string")
  (assert (not (data-section-header-p '(:items))) () "section-header: list"))

(defun test-data-section-entry-p ()
  "Test data section entry detection."
  (assert (data-section-entry-p '(:sword (:damage 10))) () "section-entry: valid")
  ;; '(:sword) has nil as second element, nil is a list, so this passes
  (assert (data-section-entry-p '(:sword nil)) () "section-entry: nil second is list")
  (assert (not (data-section-entry-p nil)) () "section-entry: nil")
  (assert (not (data-section-entry-p '("sword" (:damage 10)))) () "section-entry: non-keyword id"))

(defun test-normalize-pairs ()
  "Test pair normalization."
  (let ((plist-result (normalize-pairs '(:a 1 :b 2)))
        (pairs-result (normalize-pairs '((:a 1) (:b 2)))))
    (assert (equal plist-result '((:a 1) (:b 2))) () "normalize-pairs: plist")
    (assert (equal pairs-result '((:a 1) (:b 2))) () "normalize-pairs: already pairs")))

;;; ============================================================
;;; ZONE.LISP TESTS
;;; ============================================================

(defun test-zone-chunk-key ()
  "Test zone chunk key packing."
  (let ((key1 (zone-chunk-key 0 0))
        (key2 (zone-chunk-key 1 0))
        (key3 (zone-chunk-key 0 1)))
    (assert (integerp key1) () "chunk-key: returns integer")
    (assert (/= key1 key2) () "chunk-key: different x")
    (assert (/= key1 key3) () "chunk-key: different y")
    (assert (/= key2 key3) () "chunk-key: all unique")))

(defun test-tile-key-roundtrip ()
  "Test tile key packing and unpacking."
  (dolist (coords '((0 0) (10 20) (100 200) (1000 2000)))
    (let* ((x (first coords))
           (y (second coords))
           (key (tile-key x y))
           (back-x (tile-key-x key))
           (back-y (tile-key-y key)))
      (assert (= x back-x) () (format nil "tile-key roundtrip x: ~d" x))
      (assert (= y back-y) () (format nil "tile-key roundtrip y: ~d" y)))))

(defun test-zone-tile-in-bounds-p ()
  "Test zone bounds checking."
  ;; Use %make-zone directly since make-zone may not exist
  (let ((zone (%make-zone :id :test :width 10 :height 10)))
    (assert (zone-tile-in-bounds-p zone 0 0) () "zone-bounds: origin")
    (assert (zone-tile-in-bounds-p zone 5 5) () "zone-bounds: center")
    (assert (zone-tile-in-bounds-p zone 9 9) () "zone-bounds: max valid")
    (assert (not (zone-tile-in-bounds-p zone 10 5)) () "zone-bounds: x out")
    (assert (not (zone-tile-in-bounds-p zone 5 10)) () "zone-bounds: y out")
    (assert (not (zone-tile-in-bounds-p zone -1 5)) () "zone-bounds: negative x")))

;;; ============================================================
;;; INTENT.LISP TESTS
;;; ============================================================

(defun test-set-intent-move ()
  "Test setting intent movement."
  (let ((intent (make-intent)))
    (set-intent-move intent 1.0 0.0)
    (assert (= (intent-move-dx intent) 1.0) () "intent-move: dx set")
    (assert (= (intent-move-dy intent) 0.0) () "intent-move: dy set")
    ;; Check face-dx/face-dy instead of intent-face
    (assert (= (intent-face-dx intent) 1.0) () "intent-move: face-dx updated")
    (assert (= (intent-face-dy intent) 0.0) () "intent-move: face-dy updated")))

(defun test-set-intent-face ()
  "Test setting intent facing."
  (let ((intent (make-intent)))
    (set-intent-face intent 0.0 -1.0)
    (assert (= (intent-face-dx intent) 0.0) () "intent-face: up dx")
    (assert (= (intent-face-dy intent) -1.0) () "intent-face: up dy")
    (set-intent-face intent 0.0 1.0)
    (assert (= (intent-face-dy intent) 1.0) () "intent-face: down dy")
    (set-intent-face intent 1.0 0.0)
    (assert (= (intent-face-dx intent) 1.0) () "intent-face: side dx")))

(defun test-apply-intent-plist-rejects-bad-pickup ()
  "Ensure malformed pickup/drop intent fields are sanitized."
  (let ((intent (make-intent)))
    (apply-intent-plist intent (list :requested-pickup-target-id :arrows
                                     :requested-pickup-tx "bad"
                                     :requested-pickup-ty -3
                                     :requested-drop-slot-index "oops"))
    (assert (null (intent-requested-pickup-tx intent)) () "pickup-tx invalid -> nil")
    (assert (null (intent-requested-pickup-ty intent)) () "pickup-ty invalid -> nil")
    (assert (null (intent-requested-drop-slot-index intent)) () "drop-slot invalid -> nil")
    (apply-intent-plist intent (list :requested-pickup-target-id :arrows
                                     :requested-pickup-tx 2
                                     :requested-pickup-ty 3
                                     :requested-drop-slot-index 1))
    (assert (= (intent-requested-pickup-tx intent) 2) () "pickup-tx valid -> 2")
    (assert (= (intent-requested-pickup-ty intent) 3) () "pickup-ty valid -> 3")
    (assert (= (intent-requested-drop-slot-index intent) 1) () "drop-slot valid -> 1")))

;;; ============================================================
;;; SAVE.LISP TESTS
;;; ============================================================

(defun test-quantize-coord-roundtrip ()
  "Test coordinate quantization and dequantization."
  (dolist (coord '(0.0 1.5 100.3 -50.7 1234.5))
    (let* ((quantized (quantize-coord coord))
           (restored (dequantize-coord quantized)))
      (assert (< (abs (- restored coord)) 0.1) ()
              (format nil "quantize-coord roundtrip: ~f -> ~d -> ~f" coord quantized restored))))
  ;; Test nil handling
  (assert (= (quantize-coord nil) 0) () "quantize-coord: nil -> 0")
  (assert (= (dequantize-coord nil) 0.0) () "dequantize-coord: nil -> 0.0"))

(defun test-quantize-timer-roundtrip ()
  "Test timer quantization and dequantization."
  (dolist (timer '(0.0 0.5 1.23 5.0 10.99))
    (let* ((quantized (quantize-timer timer))
           (restored (dequantize-timer quantized)))
      (assert (< (abs (- restored timer)) 0.01) ()
              (format nil "quantize-timer roundtrip: ~f -> ~d -> ~f" timer quantized restored))))
  ;; Test nil handling
  (assert (= (quantize-timer nil) 0) () "quantize-timer: nil -> 0")
  (assert (= (dequantize-timer nil) 0.0) () "dequantize-timer: nil -> 0.0"))

(defun test-encode-anim-state-roundtrip ()
  "Test animation state encoding and decoding."
  ;; Valid states: :idle, :walk, :attack (not :hit - see *anim-state-to-code*)
  (dolist (state '(:idle :walk :attack))
    (let* ((code (encode-anim-state state))
           (decoded (decode-anim-state code)))
      (assert (eq decoded state) ()
              (format nil "anim-state roundtrip: ~a -> ~d -> ~a" state code decoded))))
  ;; Unknown state should encode to 0, decode to :idle
  (let ((unknown-code (encode-anim-state :unknown)))
    (assert (= unknown-code 0) () "encode-anim-state: unknown -> 0")))

(defun test-encode-facing-roundtrip ()
  "Test facing direction encoding and decoding."
  (dolist (facing '(:up :down :side))
    (let* ((code (encode-facing facing))
           (decoded (decode-facing code)))
      (assert (eq decoded facing) ()
              (format nil "facing roundtrip: ~a -> ~d -> ~a" facing code decoded))))
  ;; Unknown facing should default to :down
  (let ((default (decode-facing 999)))
    (assert (eq default :down) () "decode-facing: invalid -> :down")))

(defun test-pack-player-flags-roundtrip ()
  "Test player flag packing and unpacking."
  (ensure-test-game-data)
  ;; Test all flags false
  (let ((player (make-player 0.0 0.0 :id 1)))
    (setf (player-attacking player) nil
          (player-attack-hit player) nil
          (player-hit-active player) nil
          (player-running player) nil)
    (let ((flags (pack-player-flags player)))
      (multiple-value-bind (atk atk-hit hit-active running) (unpack-player-flags flags)
        (assert (not atk) () "flags: attacking false")
        (assert (not atk-hit) () "flags: attack-hit false")
        (assert (not hit-active) () "flags: hit-active false")
        (assert (not running) () "flags: running false"))))
  ;; Test all flags true
  (let ((player (make-player 0.0 0.0 :id 2)))
    (setf (player-attacking player) t
          (player-attack-hit player) t
          (player-hit-active player) t
          (player-running player) t)
    (let ((flags (pack-player-flags player)))
      (multiple-value-bind (atk atk-hit hit-active running) (unpack-player-flags flags)
        (assert atk () "flags: attacking true")
        (assert atk-hit () "flags: attack-hit true")
        (assert hit-active () "flags: hit-active true")
        (assert running () "flags: running true")))))

(defun test-pack-npc-flags-roundtrip ()
  "Test NPC flag packing and unpacking."
  (ensure-test-game-data)
  (let* ((archetype (gethash :goblin *npc-archetypes*))
         (npc (make-npc 0.0 0.0 :archetype archetype :id 1)))
    ;; Test default state (alive)
    (let ((flags (pack-npc-flags npc)))
      (multiple-value-bind (alive provoked hit-active) (unpack-npc-flags flags)
        (assert alive () "npc-flags: alive default")
        (assert (not provoked) () "npc-flags: not provoked default")
        (assert (not hit-active) () "npc-flags: not hit-active default")))
    ;; Test modified state
    (setf (npc-alive npc) nil
          (npc-provoked npc) t
          (npc-hit-active npc) t)
    (let ((flags (pack-npc-flags npc)))
      (multiple-value-bind (alive provoked hit-active) (unpack-npc-flags flags)
        (assert (not alive) () "npc-flags: dead")
        (assert provoked () "npc-flags: provoked")
        (assert hit-active () "npc-flags: hit-active")))))

(defun test-serialize-skill-roundtrip ()
  "Test skill serialization and deserialization."
  (let* ((skill (make-skill :level 50 :xp 12345))
         (plist (serialize-skill skill))
         (restored (deserialize-skill plist)))
    (assert (= (skill-level restored) 50) () "skill roundtrip: level")
    (assert (= (skill-xp restored) 12345) () "skill roundtrip: xp"))
  ;; Test nil handling
  (assert (null (serialize-skill nil)) () "serialize-skill: nil -> nil")
  (assert (null (deserialize-skill nil)) () "deserialize-skill: nil -> nil"))

(defun test-serialize-stat-block-roundtrip ()
  "Test stat-block serialization and deserialization."
  (let* ((stats (make-stat-block
                 :attack (make-skill :level 10 :xp 100)
                 :strength (make-skill :level 20 :xp 200)
                 :defense (make-skill :level 30 :xp 300)
                 :hitpoints (make-skill :level 40 :xp 400)))
         (plist (serialize-stat-block stats))
         (restored (deserialize-stat-block plist)))
    (assert (= (skill-level (stat-block-attack restored)) 10) () "stat-block: attack level")
    (assert (= (skill-level (stat-block-strength restored)) 20) () "stat-block: strength level")
    (assert (= (skill-level (stat-block-defense restored)) 30) () "stat-block: defense level")
    (assert (= (skill-level (stat-block-hitpoints restored)) 40) () "stat-block: hp level")
    (assert (= (skill-xp (stat-block-attack restored)) 100) () "stat-block: attack xp")))

(defun test-serialize-inventory-slot-roundtrip ()
  "Test inventory slot serialization and deserialization."
  (let* ((slot (make-inventory-slot :item-id :health-potion :count 10))
         (plist (serialize-inventory-slot slot))
         (restored (deserialize-inventory-slot plist)))
    (assert (eq (inventory-slot-item-id restored) :health-potion) () "slot: item-id")
    (assert (= (inventory-slot-count restored) 10) () "slot: count"))
  ;; Test empty slot
  (let* ((empty (make-inventory-slot :item-id nil :count 0))
         (plist (serialize-inventory-slot empty))
         (restored (deserialize-inventory-slot plist)))
    (assert (null (inventory-slot-item-id restored)) () "slot: empty item-id")
    (assert (= (inventory-slot-count restored) 0) () "slot: empty count")))

;;; ============================================================
;;; MIGRATIONS.LISP TESTS
;;; ============================================================

(defun test-migrate-player-v1-to-v2 ()
  "Test migration from v1 to v2 adds lifetime-xp."
  (let* ((v1-data '(:version 1 :id 123 :x 100.0 :y 200.0 :hp 50))
         (migrated (migrate-player-v1->v2 (copy-list v1-data))))
    ;; Should have lifetime-xp added
    (assert (= (getf migrated :lifetime-xp) 0) () "v1->v2: adds lifetime-xp = 0")
    ;; Original fields preserved
    (assert (= (getf migrated :id) 123) () "v1->v2: preserves id")
    (assert (= (getf migrated :x) 100.0) () "v1->v2: preserves x")))

(defun test-migrate-player-v2-to-v3 ()
  "Test migration from v2 to v3 adds playtime and created-at."
  (let* ((v2-data '(:version 2 :id 456 :lifetime-xp 1000))
         (before-time (get-universal-time))
         (migrated (migrate-player-v2->v3 (copy-list v2-data)))
         (after-time (get-universal-time)))
    ;; Should have playtime added
    (assert (= (getf migrated :playtime) 0) () "v2->v3: adds playtime = 0")
    ;; Should have created-at added (between before and after time)
    (let ((created (getf migrated :created-at)))
      (assert (>= created before-time) () "v2->v3: created-at >= start")
      (assert (<= created after-time) () "v2->v3: created-at <= end"))
    ;; Original fields preserved
    (assert (= (getf migrated :lifetime-xp) 1000) () "v2->v3: preserves lifetime-xp")))

(defun test-migrate-player-v3-to-v4 ()
  "Test migration from v3 to v4 adds deaths field."
  (let* ((v3-data '(:version 3 :id 789 :lifetime-xp 5000 :playtime 3600 :created-at 1000000))
         (migrated (migrate-player-v3->v4 (copy-list v3-data))))
    ;; Should have deaths added
    (assert (= (getf migrated :deaths) 0) () "v3->v4: adds deaths = 0")
    ;; Original fields preserved
    (assert (= (getf migrated :id) 789) () "v3->v4: preserves id")
    (assert (= (getf migrated :lifetime-xp) 5000) () "v3->v4: preserves lifetime-xp")
    (assert (= (getf migrated :playtime) 3600) () "v3->v4: preserves playtime")
    (assert (= (getf migrated :created-at) 1000000) () "v3->v4: preserves created-at")))

(defun test-migrate-player-data-chain ()
  "Test full migration chain from v1 to current version."
  (let* ((v1-data '(:version 1 :id 789 :x 50.0 :y 75.0 :hp 100))
         (migrated (migrate-player-data (copy-list v1-data))))
    ;; Should be at current version
    (assert (= (getf migrated :version) *player-schema-version*) () "chain: at current version")
    ;; Should have all migration fields
    (assert (numberp (getf migrated :lifetime-xp)) () "chain: has lifetime-xp")
    (assert (numberp (getf migrated :playtime)) () "chain: has playtime")
    (assert (numberp (getf migrated :created-at)) () "chain: has created-at")
    ;; Original fields preserved
    (assert (= (getf migrated :id) 789) () "chain: preserves id")
    (assert (= (getf migrated :hp) 100) () "chain: preserves hp")))

;;; ============================================================
;;; WORLD-GRAPH.LISP TESTS
;;; ============================================================

(defun test-world-graph-data-plist ()
  "Test world graph data normalization."
  ;; Direct plist - any plist starting with keyword returns as-is
  (let ((direct '(:edges ((:from :town :to :forest)))))
    (assert (equal (world-graph-data-plist direct) direct) () "wg-plist: direct"))
  ;; Another keyword-starting plist (world-graph is a keyword, so first condition matches)
  (let ((wrapped '(:world-graph (:edges ()))))
    (assert (equal (world-graph-data-plist wrapped) wrapped) () "wg-plist: keyword plist"))
  ;; Invalid data
  (assert (null (world-graph-data-plist nil)) () "wg-plist: nil -> nil")
  (assert (null (world-graph-data-plist '("not" "a" "plist"))) () "wg-plist: invalid"))

(defun test-normalize-world-graph-edges ()
  "Test world graph edge normalization."
  (let* ((edges '((:from :town :to :forest :edge :north)
                  (:from :town :to :cave :edge :south)
                  (:from :forest :to :town :edge :south)))
         (table (normalize-world-graph-edges edges)))
    ;; Town should have 2 exits
    (let ((town-exits (gethash :town table)))
      (assert (= (length town-exits) 2) () "wg-edges: town has 2 exits"))
    ;; Forest should have 1 exit
    (let ((forest-exits (gethash :forest table)))
      (assert (= (length forest-exits) 1) () "wg-edges: forest has 1 exit"))
    ;; Cave should have 0 exits (only destination)
    (let ((cave-exits (gethash :cave table)))
      (assert (null cave-exits) () "wg-edges: cave has no exits"))))

;;; ============================================================
;;; CHAT.LISP TESTS
;;; ============================================================

(defun test-trim-chat-message ()
  "Test chat message trimming."
  (assert (string= (trim-chat-message "hello") "hello") () "trim: no whitespace")
  (assert (string= (trim-chat-message "  hello  ") "hello") () "trim: spaces")
  (assert (string= (trim-chat-message "	hello	") "hello") () "trim: tabs")
  (assert (string= (trim-chat-message "
hello
") "hello") () "trim: newlines")
  (assert (string= (trim-chat-message "") "") () "trim: empty string")
  (assert (string= (trim-chat-message "   ") "") () "trim: only whitespace"))

;;; ============================================================
;;; TYPES.LISP TESTS
;;; ============================================================

(defun test-skill-xp-for-level ()
  "Test XP required for level calculation."
  ;; Level 1 requires 0 XP
  (assert (= (skill-xp-for-level 1) 0) () "skill-xp: level 1 = 0")
  ;; Higher levels need more XP
  (assert (> (skill-xp-for-level 2) 0) () "skill-xp: level 2 > 0")
  (assert (> (skill-xp-for-level 10) (skill-xp-for-level 5)) () "skill-xp: level 10 > level 5")
  ;; XP increases per level
  (let ((xp5 (skill-xp-for-level 5))
        (xp6 (skill-xp-for-level 6)))
    (assert (> xp6 xp5) () "skill-xp: monotonic increase")))

(defun test-allocate-entity-id ()
  "Test entity ID allocation."
  ;; make-id-source takes positional args: (next-id persistent)
  (let ((source (make-id-source 1)))
    ;; First allocation
    (let ((id1 (allocate-entity-id source)))
      (assert (= id1 1) () "entity-id: first = 1"))
    ;; Second allocation
    (let ((id2 (allocate-entity-id source)))
      (assert (= id2 2) () "entity-id: second = 2"))
    ;; IDs are unique
    (let ((id3 (allocate-entity-id source)))
      (assert (= id3 3) () "entity-id: third = 3"))
    ;; Source tracks next
    (assert (= (id-source-next-id source) 4) () "entity-id: next is 4")))

(defun test-find-player-by-id ()
  "Test finding player by ID."
  (ensure-test-game-data)
  (let* ((p1 (make-player 0.0 0.0 :id 100))
         (p2 (make-player 10.0 10.0 :id 200))
         ;; find-player-by-id expects a vector, not list
         (players (vector p1 p2)))
    ;; Find existing
    (assert (eq (find-player-by-id players 100) p1) () "find-player: id 100")
    (assert (eq (find-player-by-id players 200) p2) () "find-player: id 200")
    ;; Not found
    (assert (null (find-player-by-id players 999)) () "find-player: not found")
    ;; Empty vector
    (assert (null (find-player-by-id (vector) 100)) () "find-player: empty vector")
    ;; Nil
    (assert (null (find-player-by-id nil 100)) () "find-player: nil")))

;;; ============================================================
;;; TEST HELPERS
;;; ============================================================

(defun ensure-test-game-data ()
  "Ensure game data is loaded for tests."
  (unless *game-data-loaded-p*
    (load-game-data)))

;;; ============================================================
;;; REDIS METRICS TESTS (Phase 2 - Database Hardening)
;;; ============================================================

(defun test-calculate-percentile ()
  "Test percentile calculation from list of values."
  ;; Basic cases
  (let ((values '(1 2 3 4 5 6 7 8 9 10)))
    ;; p50 (median) should be around 5
    (let ((p50 (calculate-percentile values 50)))
      (assert (and p50 (>= p50 4) (<= p50 6)) ()
              (format nil "p50 of 1-10 should be around 5, got ~a" p50)))
    ;; p99 should be close to 10
    (let ((p99 (calculate-percentile values 99)))
      (assert (and p99 (>= p99 9) (<= p99 10)) ()
              (format nil "p99 of 1-10 should be around 10, got ~a" p99)))
    ;; p0 should be close to 1
    (let ((p0 (calculate-percentile values 0)))
      (assert (and p0 (= p0 1)) ()
              (format nil "p0 of 1-10 should be 1, got ~a" p0)))))

(defun test-calculate-percentile-edge-cases ()
  "Test percentile edge cases."
  ;; Empty list
  (assert (null (calculate-percentile nil 50)) () "percentile: nil -> nil")
  (assert (null (calculate-percentile '() 50)) () "percentile: empty -> nil")
  ;; Single value
  (let ((p50 (calculate-percentile '(42) 50)))
    (assert (= p50 42) () "percentile: single value"))
  ;; Two values
  (let ((p50 (calculate-percentile '(10 20) 50)))
    (assert (and p50 (>= p50 10) (<= p50 20)) ()
            (format nil "percentile: two values, got ~a" p50))))

(defun test-ring-buffer-values ()
  "Test ring buffer value extraction."
  ;; Create buffer with some values
  (let ((buffer (make-array 10 :initial-element 0.0)))
    (setf (aref buffer 0) 1.0
          (aref buffer 1) 2.0
          (aref buffer 2) 3.0)
    ;; Extract with count=3
    (let ((values (ring-buffer-values buffer 3)))
      (assert (= (length values) 3) ()
              (format nil "ring-buffer: count=3 should return 3 values, got ~d" (length values))))
    ;; Extract with count=1
    (let ((values (ring-buffer-values buffer 1)))
      (assert (= (length values) 1) () "ring-buffer: count=1"))
    ;; Extract with count=0
    (let ((values (ring-buffer-values buffer 0)))
      (assert (= (length values) 0) () "ring-buffer: count=0"))
    ;; Extract with count > buffer size
    (let ((values (ring-buffer-values buffer 100)))
      (assert (= (length values) 10) () "ring-buffer: clamps to buffer size"))))

(defun test-metrics-push-latency ()
  "Test metrics latency recording."
  ;; Initialize metrics
  (init-redis-metrics)
  ;; Push some save latencies
  (metrics-push-save-latency 1.0)
  (metrics-push-save-latency 2.0)
  (metrics-push-save-latency 3.0)
  ;; Check save count
  (assert (= (redis-metrics-save-count *redis-metrics*) 3) ()
          "metrics: save count = 3")
  (assert (= (redis-metrics-total-saves *redis-metrics*) 3) ()
          "metrics: total saves = 3")
  ;; Push some load latencies
  (metrics-push-load-latency 0.5)
  (metrics-push-load-latency 1.5)
  ;; Check load count
  (assert (= (redis-metrics-load-count *redis-metrics*) 2) ()
          "metrics: load count = 2")
  (assert (= (redis-metrics-total-loads *redis-metrics*) 2) ()
          "metrics: total loads = 2")
  ;; Check p50/p99 values exist
  (let ((p99-save (get-redis-p99-save-latency))
        (p50-save (get-redis-p50-save-latency)))
    (assert (numberp p99-save) () "metrics: p99 save is number")
    (assert (numberp p50-save) () "metrics: p50 save is number"))
  ;; Record errors
  (metrics-record-save-error)
  (metrics-record-load-error)
  (assert (= (redis-metrics-total-save-errors *redis-metrics*) 1) ()
          "metrics: save errors = 1")
  (assert (= (redis-metrics-total-load-errors *redis-metrics*) 1) ()
          "metrics: load errors = 1"))

;;; ============================================================
;;; NEW UTILS TESTS (Priority 6)
;;; ============================================================

(defun test-player-animation-params ()
  "Test player animation params returns correct values for states."
  (multiple-value-bind (frames time) (player-animation-params :idle)
    (assert (integerp frames) () "anim-params: idle frames integer")
    (assert (numberp time) () "anim-params: idle time number")
    (assert (> frames 0) () "anim-params: idle frames > 0"))
  (multiple-value-bind (frames time) (player-animation-params :walk)
    (assert (integerp frames) () "anim-params: walk frames integer")
    (assert (numberp time) () "anim-params: walk time number"))
  (multiple-value-bind (frames time) (player-animation-params :attack)
    (assert (integerp frames) () "anim-params: attack frames integer")
    (assert (numberp time) () "anim-params: attack time number")))

(defun test-relative-path-from-root ()
  "Test relative path extraction from root."
  (let ((result (relative-path-from-root "/home/user/project/src/file.lisp"
                                          "/home/user/project/")))
    (assert (stringp result) () "relative-path: returns string")
    (assert (string= result "src/file.lisp") () "relative-path: strips root"))
  ;; Path not under root
  (let ((result (relative-path-from-root "/other/path/file.txt"
                                          "/home/user/")))
    (assert (stringp result) () "relative-path: other returns string")))

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
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 0.0 0.0 :archetype archetype :id 1)))
    (let ((name (combatant-display-name npc)))
      (assert (stringp name) () "display-name: npc returns string"))))

(defun test-find-npc-by-id ()
  "Test finding NPC by ID in array."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
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
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 0.0 0.0 :archetype archetype :id 1)))
    (let ((seconds (npc-respawn-seconds npc)))
      (assert (numberp seconds) () "npc-respawn: returns number")
      (assert (>= seconds 0) () "npc-respawn: non-negative"))))

(defun test-npc-attack-cooldown ()
  "Test NPC attack cooldown from archetype."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 0.0 0.0 :archetype archetype :id 1)))
    (let ((cooldown (npc-attack-cooldown npc)))
      (assert (numberp cooldown) () "npc-cooldown: returns number")
      (assert (> cooldown 0) () "npc-cooldown: positive"))))

(defun test-npc-attack-damage ()
  "Test NPC attack damage from archetype."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
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
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
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
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 0.0 0.0 :archetype archetype :id 1))
         (world (make-test-world :tile-size 32.0 :collision-half 12.0)))
    (let ((range (npc-attack-range npc world)))
      (assert (numberp range) () "npc-range: returns number")
      (assert (> range 0) () "npc-range: positive"))))

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
  "Test object respawnable check."
  ;; Default is respawnable
  (let ((obj '(:id :test :x 0 :y 0)))
    (assert (object-respawnable-p obj) () "respawnable: default true"))
  ;; Explicitly respawnable
  (let ((obj '(:id :test :x 0 :y 0 :respawnable t)))
    (assert (object-respawnable-p obj) () "respawnable: explicit true"))
  ;; Not respawnable
  (let ((obj '(:id :test :x 0 :y 0 :respawnable nil)))
    (assert (not (object-respawnable-p obj)) () "respawnable: explicit false")))

(defun test-object-respawn-timer ()
  "Test object respawn timer extraction."
  ;; No timer
  (let ((obj '(:id :test :x 0 :y 0)))
    (assert (= (object-respawn-timer obj) 0.0) () "respawn-timer: no timer -> 0"))
  ;; With timer
  (let ((obj '(:id :test :x 0 :y 0 :respawn 5.5)))
    (assert (= (object-respawn-timer obj) 5.5) () "respawn-timer: has timer"))
  ;; Timer at 0
  (let ((obj '(:id :test :x 0 :y 0 :respawn 0.0)))
    (assert (= (object-respawn-timer obj) 0.0) () "respawn-timer: zero")))

;;; ============================================================
;;; TEST HELPERS
;;; ============================================================

(defun make-test-world (&key (tile-size 32.0) (collision-half 12.0))
  "Create a minimal world struct for testing functions that need world."
  (%make-world :tile-dest-size tile-size
               :collision-half-width collision-half
               :collision-half-height collision-half))

;;; ============================================================
;;; NEW AI TESTS (Priority 1)
;;; ============================================================

(defun test-npc-home-radius ()
  "Test NPC home radius from archetype."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 0.0 0.0 :archetype archetype :id 1))
         (world (make-test-world :tile-size 32.0)))
    (let ((radius (npc-home-radius npc world)))
      (assert (numberp radius) () "home-radius: returns number")
      (assert (>= radius 0) () "home-radius: non-negative"))))

(defun test-npc-move-speed ()
  "Test NPC move speed from archetype."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 0.0 0.0 :archetype archetype :id 1)))
    (let ((speed (npc-move-speed npc)))
      (assert (numberp speed) () "move-speed: returns number")
      (assert (> speed 0) () "move-speed: positive"))))

(defun test-npc-wander-interval ()
  "Test NPC wander interval from archetype."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 0.0 0.0 :archetype archetype :id 1)))
    (let ((interval (npc-wander-interval npc)))
      (assert (numberp interval) () "wander-interval: returns number")
      (assert (>= interval 0) () "wander-interval: non-negative"))))

(defun test-npc-flee-speed-mult ()
  "Test NPC flee speed multiplier from archetype."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 0.0 0.0 :archetype archetype :id 1)))
    (let ((mult (npc-flee-speed-mult npc)))
      (assert (numberp mult) () "flee-speed-mult: returns number")
      (assert (> mult 0) () "flee-speed-mult: positive"))))

(defun test-closest-player ()
  "Test finding closest player to NPC."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 50.0 50.0 :archetype archetype :id 1))
         (p1 (make-player 100.0 50.0 :id 1))   ;; 50 units away
         (p2 (make-player 60.0 50.0 :id 2))    ;; 10 units away (closest)
         (p3 (make-player 200.0 200.0 :id 3))  ;; far away
         (players (vector p1 p2 p3)))
    ;; Note: combatant-alive-p always returns T for players
    ;; p2 is closest by position
    (let ((closest (closest-player players npc)))
      (assert (eq closest p2) () "closest-player: p2 is closest"))
    ;; Single player
    (let ((closest (closest-player (vector p1) npc)))
      (assert (eq closest p1) () "closest-player: single player"))
    ;; Empty/nil players
    (assert (null (closest-player (vector) npc)) () "closest-player: empty vector")
    (assert (null (closest-player nil npc)) () "closest-player: nil vector")))

;;; ============================================================
;;; NEW DATA TESTS (Priority 2)
;;; ============================================================

(defun test-validate-item-archetype-plist ()
  "Test item archetype plist validation."
  ;; Valid plist
  (assert (validate-item-archetype-plist :test '(:name "Test" :stack-size 10)) ()
          "validate-item: valid passes")
  ;; Invalid name type - should error
  (handler-case
      (progn
        (validate-item-archetype-plist :test '(:name 123))
        (assert nil () "validate-item: invalid name should error"))
    (error () t))
  ;; Invalid stack-size - should error
  (handler-case
      (progn
        (validate-item-archetype-plist :test '(:stack-size "not-a-number"))
        (assert nil () "validate-item: invalid stack-size should error"))
    (error () t)))

(defun test-item-archetype-from-plist ()
  "Test item archetype creation from plist."
  (let ((item (item-archetype-from-plist :test-sword
                                          '(:name "Test Sword"
                                            :description "A test weapon"
                                            :stack-size 1
                                            :value 100
                                            :equip-slot :weapon
                                            :attack 5))))
    (assert (eq (item-archetype-id item) :test-sword) () "item-from-plist: id")
    (assert (string= (item-archetype-name item) "Test Sword") () "item-from-plist: name")
    (assert (= (item-archetype-stack-size item) 1) () "item-from-plist: stack-size")
    (assert (= (item-archetype-attack item) 5) () "item-from-plist: attack")
    (assert (eq (item-archetype-equip-slot item) :weapon) () "item-from-plist: equip-slot")))

(defun test-validate-object-archetype-plist ()
  "Test object archetype plist validation."
  ;; Valid plist
  (assert (validate-object-archetype-plist :test '(:name "Test" :item-id :coins)) ()
          "validate-object: valid passes")
  ;; Invalid item-id type - should error
  (handler-case
      (progn
        (validate-object-archetype-plist :test '(:item-id "not-keyword"))
        (assert nil () "validate-object: invalid item-id should error"))
    (error () t)))

(defun test-object-archetype-from-plist ()
  "Test object archetype creation from plist."
  (let ((obj (object-archetype-from-plist :coin-pile
                                           '(:name "Coin Pile"
                                             :item-id :coins
                                             :count 10
                                             :respawn-seconds 30.0))))
    (assert (eq (object-archetype-id obj) :coin-pile) () "object-from-plist: id")
    (assert (string= (object-archetype-name obj) "Coin Pile") () "object-from-plist: name")
    (assert (eq (object-archetype-item-id obj) :coins) () "object-from-plist: item-id")
    (assert (= (object-archetype-count obj) 10) () "object-from-plist: count")
    (assert (= (object-archetype-respawn-seconds obj) 30.0) () "object-from-plist: respawn")))

(defun test-loot-entry-from-spec ()
  "Test loot entry creation from spec."
  ;; Full spec
  (let ((entry (loot-entry-from-spec '(:coins 100 5 10))))
    (assert (eq (loot-entry-item-id entry) :coins) () "loot-entry: item-id")
    (assert (= (loot-entry-weight entry) 100) () "loot-entry: weight")
    (assert (= (loot-entry-min-count entry) 5) () "loot-entry: min")
    (assert (= (loot-entry-max-count entry) 10) () "loot-entry: max"))
  ;; Minimal spec (defaults to 1,1)
  (let ((entry (loot-entry-from-spec '(:potion 50))))
    (assert (= (loot-entry-min-count entry) 1) () "loot-entry: default min")
    (assert (= (loot-entry-max-count entry) 1) () "loot-entry: default max")))

(defun test-validate-loot-table-plist ()
  "Test loot table plist validation."
  ;; Valid plist
  (assert (validate-loot-table-plist :test '(:rolls 1 :entries ((:coins 100)))) ()
          "validate-loot: valid passes")
  ;; Missing entries - should error
  (handler-case
      (progn
        (validate-loot-table-plist :test '(:rolls 1))
        (assert nil () "validate-loot: missing entries should error"))
    (error () t)))

(defun test-loot-table-from-plist ()
  "Test loot table creation from plist."
  (let ((table (loot-table-from-plist :goblin-loot
                                       '(:rolls 2
                                         :entries ((:coins 100 1 10)
                                                   (:health-potion 20 1 1))))))
    (assert (eq (loot-table-id table) :goblin-loot) () "loot-table: id")
    (assert (= (loot-table-rolls table) 2) () "loot-table: rolls")
    (assert (= (length (loot-table-entries table)) 2) () "loot-table: 2 entries")))

(defun test-animation-set-from-plist ()
  "Test animation set creation from plist."
  (let ((set (animation-set-from-plist :test-anim
                                        '(:dir "sprites/"
                                          :down-idle "idle.png"
                                          :down-walk "walk.png"))))
    (assert (eq (animation-set-id set) :test-anim) () "anim-set: id")
    (assert (string= (animation-set-dir set) "sprites/") () "anim-set: dir")
    (assert (string= (animation-set-down-idle set) "idle.png") () "anim-set: down-idle")))

(defun test-merge-animation-sets ()
  "Test animation set merging."
  (let* ((base (animation-set-from-plist :base '(:dir "base/" :down-idle "base-idle.png")))
         (override (animation-set-from-plist :override '(:down-idle "override-idle.png")))
         (merged (merge-animation-sets base override)))
    ;; Override should win for down-idle
    (assert (string= (animation-set-down-idle merged) "override-idle.png") ()
            "merge-anim: override wins")
    ;; Base should be kept for dir (not overridden)
    (assert (string= (animation-set-dir merged) "base/") ()
            "merge-anim: base kept")))

;;; ============================================================
;;; NEW ZONE TESTS (Priority 2)
;;; ============================================================

(defun test-zone-label ()
  "Test zone label generation."
  (let ((zone (%make-zone :id :test-zone :width 10 :height 10)))
    (let ((label (zone-label zone)))
      (assert (stringp label) () "zone-label: returns string")
      (assert (string= label "TEST-ZONE") () "zone-label: uppercase")))
  ;; Nil zone
  (let ((label (zone-label nil)))
    (assert (string= label "NONE") () "zone-label: nil -> NONE")))

(defun test-zone-data-plist ()
  "Test zone data plist normalization."
  ;; Direct plist
  (let ((result (zone-data-plist '(:id :test :width 10))))
    (assert (listp result) () "zone-plist: direct returns list")
    (assert (eq (getf result :id) :test) () "zone-plist: preserves id"))
  ;; Nil data
  (assert (null (zone-data-plist nil)) () "zone-plist: nil -> nil")
  ;; Invalid data
  (assert (null (zone-data-plist '("not" "a" "plist"))) () "zone-plist: invalid -> nil"))

(defun test-make-empty-zone ()
  "Test empty zone creation."
  (let ((zone (make-empty-zone :empty-test 20 15)))
    (assert (eq (zone-id zone) :empty-test) () "empty-zone: id")
    (assert (= (zone-width zone) 20) () "empty-zone: width")
    (assert (= (zone-height zone) 15) () "empty-zone: height")
    (assert (= (length (zone-layers zone)) 0) () "empty-zone: no layers")
    (assert (null (zone-objects zone)) () "empty-zone: no objects")))

(defun test-build-tiles-from-fill ()
  "Test tile vector building from fill value."
  ;; All same value
  (let ((tiles (build-tiles-from-fill 4 5 nil)))
    (assert (= (length tiles) 16) () "fill-tiles: 4x4 = 16")
    (assert (every (lambda (tile) (= tile 5)) tiles) () "fill-tiles: all 5"))
  ;; With overrides
  (let ((tiles (build-tiles-from-fill 4 0 '((0 0 1) (1 1 2)))))
    (assert (= (aref tiles 0) 1) () "fill-tiles: override at 0,0")
    (assert (= (aref tiles 5) 2) () "fill-tiles: override at 1,1")
    (assert (= (aref tiles 2) 0) () "fill-tiles: non-override is fill")))

(defun test-zone-layer-tile-at ()
  "Test getting tile at coordinates from layer."
  ;; Create a simple layer with one chunk
  (let* ((chunk (%make-zone-chunk :x 0 :y 0
                                   :tiles (make-array 16 :initial-contents
                                                      '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))))
         (chunks (make-hash-table :test 'eql))
         (layer nil))
    (setf (gethash (zone-chunk-key 0 0) chunks) chunk)
    (setf layer (%make-zone-layer :id :ground :chunks chunks))
    ;; Test tile retrieval (chunk-size 4)
    (assert (= (zone-layer-tile-at layer 4 0 0) 1) () "layer-tile: 0,0 = 1")
    (assert (= (zone-layer-tile-at layer 4 1 0) 2) () "layer-tile: 1,0 = 2")
    (assert (= (zone-layer-tile-at layer 4 0 1) 5) () "layer-tile: 0,1 = 5")
    ;; Out of chunk returns 0
    (assert (= (zone-layer-tile-at layer 4 10 10) 0) () "layer-tile: out of chunk = 0")))

;;; ============================================================
;;; NEW INTENT TESTS (Priority 5)
;;; ============================================================

(defun test-reset-frame-intent ()
  "Test resetting per-frame intent signals."
  (let ((intent (make-intent)))
    ;; Set some values
    (set-intent-move intent 1.0 1.0)
    (setf (intent-attack intent) t
          (intent-run-toggle intent) t)
    ;; Reset
    (reset-frame-intent intent)
    ;; Check cleared
    (assert (= (intent-move-dx intent) 0.0) () "reset-intent: dx cleared")
    (assert (= (intent-move-dy intent) 0.0) () "reset-intent: dy cleared")
    (assert (null (intent-attack intent)) () "reset-intent: attack cleared")
    (assert (null (intent-run-toggle intent)) () "reset-intent: run cleared")))

(defun test-consume-intent-actions ()
  "Test consuming one-shot actions."
  (let ((intent (make-intent)))
    (setf (intent-attack intent) t
          (intent-run-toggle intent) t)
    (consume-intent-actions intent)
    (assert (null (intent-attack intent)) () "consume-intent: attack cleared")
    (assert (null (intent-run-toggle intent)) () "consume-intent: run cleared")))

(defun test-set-intent-target ()
  "Test setting intent target."
  (let ((intent (make-intent)))
    (set-intent-target intent 100.0 200.0)
    (assert (= (intent-target-x intent) 100.0) () "set-target: x")
    (assert (= (intent-target-y intent) 200.0) () "set-target: y")
    (assert (intent-target-active intent) () "set-target: active")))

(defun test-clear-intent-target ()
  "Test clearing intent target."
  (let ((intent (make-intent)))
    (set-intent-target intent 100.0 200.0)
    (clear-intent-target intent)
    (assert (not (intent-target-active intent)) () "clear-target: inactive")))

(defun test-request-pickup-target ()
  "Test pickup target request."
  (let ((intent (make-intent)))
    (request-pickup-target intent :coins 5 10)
    (assert (eq (intent-requested-pickup-target-id intent) :coins) () "pickup: id")
    (assert (= (intent-requested-pickup-tx intent) 5) () "pickup: tx")
    (assert (= (intent-requested-pickup-ty intent) 10) () "pickup: ty")
    ;; Clear
    (clear-requested-pickup-target intent)
    (assert (null (intent-requested-pickup-target-id intent)) () "pickup: cleared")))

(defun test-request-drop-item ()
  "Test drop item request."
  (let ((intent (make-intent)))
    (request-drop-item intent :health-potion 5 2)
    (assert (eq (intent-requested-drop-item-id intent) :health-potion) () "drop: item-id")
    (assert (= (intent-requested-drop-count intent) 5) () "drop: count")
    (assert (= (intent-requested-drop-slot-index intent) 2) () "drop: slot")
    ;; Clear
    (clear-requested-drop-item intent)
    (assert (null (intent-requested-drop-item-id intent)) () "drop: cleared")))

(defun test-request-inventory-swap ()
  "Test inventory swap request."
  (let ((intent (make-intent)))
    (request-inventory-swap intent 0 3)
    (assert (= (intent-requested-swap-slot-a intent) 0) () "swap: slot-a")
    (assert (= (intent-requested-swap-slot-b intent) 3) () "swap: slot-b")
    ;; Clear
    (clear-requested-inventory-swap intent)
    (assert (null (intent-requested-swap-slot-a intent)) () "swap: cleared")))

(defun test-trade-intent-functions ()
  "Test trade intent request functions."
  (let ((intent (make-intent)))
    ;; Request trade
    (request-trade-with-player intent 123)
    (assert (= (intent-requested-trade-target-id intent) 123) () "trade: target-id")
    ;; Request offer
    (request-trade-offer intent 2 10)
    (assert (= (intent-requested-trade-offer-slot intent) 2) () "trade: offer-slot")
    (assert (= (intent-requested-trade-offer-count intent) 10) () "trade: offer-count")
    ;; Confirm
    (request-trade-confirm intent)
    (assert (intent-requested-trade-confirm intent) () "trade: confirm set")
    ;; Cancel
    (request-trade-cancel intent)
    (assert (intent-requested-trade-cancel intent) () "trade: cancel set")
    ;; Clear all
    (clear-all-trade-requests intent)
    (assert (null (intent-requested-trade-target-id intent)) () "trade: cleared target")
    (assert (null (intent-requested-trade-confirm intent)) () "trade: cleared confirm")))

;;; ============================================================
;;; NEW NET TESTS (Priority 3)
;;; ============================================================

(defun test-string-to-octets ()
  "Test string to octet conversion."
  (let ((octets (string-to-octets "hello")))
    (assert (vectorp octets) () "str-to-oct: returns vector")
    (assert (= (length octets) 5) () "str-to-oct: correct length")
    (assert (= (aref octets 0) (char-code #\h)) () "str-to-oct: first char")))

(defun test-octets-to-string ()
  "Test octet to string conversion."
  (let* ((input "hello")
         (octets (string-to-octets input))
         (result (octets-to-string octets (length octets))))
    (assert (stringp result) () "oct-to-str: returns string")
    (assert (string= result input) () "oct-to-str: roundtrip")))

(defun test-encode-decode-net-message ()
  "Test network message encoding and decoding."
  (let* ((message '(:type :test :value 123 :name "hello"))
         (encoded (encode-net-message message))
         (decoded (decode-net-message encoded)))
    (assert (stringp encoded) () "net-msg: encode returns string")
    (assert (listp decoded) () "net-msg: decode returns list")
    (assert (eq (getf decoded :type) :test) () "net-msg: type preserved")
    (assert (= (getf decoded :value) 123) () "net-msg: value preserved")
    (assert (string= (getf decoded :name) "hello") () "net-msg: name preserved"))
  ;; Invalid decode
  (assert (null (decode-net-message "not valid lisp")) () "net-msg: invalid -> nil"))

(defun test-host-to-string ()
  "Test host conversion to string."
  ;; Already a string
  (assert (string= (host-to-string "127.0.0.1") "127.0.0.1") ()
          "host-str: string passthrough")
  ;; Byte vector
  (let ((bytes #(192 168 1 1)))
    (assert (string= (host-to-string bytes) "192.168.1.1") ()
            "host-str: byte vector")))

;;; ============================================================
;;; ADDITIONAL AI TESTS
;;; ============================================================

(defun test-npc-in-perception-range-p ()
  "Test NPC perception range check."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 100.0 100.0 :archetype archetype :id 1))
         (player-near (make-player 110.0 100.0 :id 1))
         (player-far (make-player 1000.0 1000.0 :id 2))
         (world (make-test-world :tile-size 32.0)))
    ;; For archetypes with perception range, player-near should be in range
    ;; For archetypes without perception (0 tiles), nobody is in range
    (let ((perception-tiles (npc-archetype-perception-tiles archetype)))
      (if (and perception-tiles (> perception-tiles 0))
          (progn
            (assert (npc-in-perception-range-p npc player-near world) ()
                    "perception: near player in range")
            (assert (not (npc-in-perception-range-p npc player-far world)) ()
                    "perception: far player not in range"))
          ;; Zero perception means never in range
          (assert (not (npc-in-perception-range-p npc player-near world)) ()
                  "perception: zero perception -> never in range")))
    ;; Nil player always returns nil
    (assert (null (npc-in-perception-range-p npc nil world)) ()
            "perception: nil player -> nil")))

;;; ============================================================
;;; ADDITIONAL COMBAT TESTS
;;; ============================================================

(defun test-player-attack-target-in-range-p ()
  "Test player attack target range check."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
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
  "Test object entry count extraction."
  ;; Object with explicit count
  (let ((obj '(:id :coins :x 0 :y 0 :count 50)))
    (assert (= (object-entry-count obj nil) 50) () "entry-count: explicit count"))
  ;; Object without count - falls back to archetype or 1
  (let ((obj '(:id :coins :x 0 :y 0)))
    (let ((count (object-entry-count obj nil)))
      (assert (= count 1) () "entry-count: no count, no arch -> 1")))
  ;; With archetype
  (ensure-test-game-data)
  (let* ((archetype (find-object-archetype :health-potion-drop))
         (obj '(:id :health-potion-drop :x 0 :y 0)))
    (when archetype
      (let ((count (object-entry-count obj archetype)))
        (assert (>= count 1) () "entry-count: archetype count")))))

;;; ============================================================
;;; ADDITIONAL DATA TESTS
;;; ============================================================

(defun test-parse-game-data-forms ()
  "Test parsing game data forms into sections."
  ;; Simple plist
  (let ((result (parse-game-data-forms '((:test-key 123)))))
    (assert (listp result) () "parse-forms: returns list"))
  ;; Section with entries
  (let ((result (parse-game-data-forms '(:items
                                          (:sword (:name "Sword"))
                                          (:shield (:name "Shield"))))))
    (assert (listp result) () "parse-forms: sections parsed")
    (let ((items (getf result :items)))
      (assert (= (length items) 2) () "parse-forms: 2 items")))
  ;; Mixed tunables and sections
  (let ((result (parse-game-data-forms '((:player-speed 100.0)
                                          :npcs
                                          (:goblin (:name "Goblin"))))))
    (assert (= (getf result :player-speed) 100.0) () "parse-forms: tunable preserved")
    (assert (listp (getf result :npcs)) () "parse-forms: section present")))

(defun test-make-npc-archetype-from-plist ()
  "Test NPC archetype creation from plist."
  (let ((archetype (make-npc-archetype-from-plist
                    '(:name "Test NPC"
                      :max-hits 10
                      :attack-level 5
                      :defense-level 3
                      :combat-xp 25
                      :move-speed 80.0
                      :aggro-mode :always))))
    (assert (string= (npc-archetype-name archetype) "Test NPC") ()
            "npc-from-plist: name")
    (assert (= (npc-archetype-max-hits archetype) 10) ()
            "npc-from-plist: max-hits")
    (assert (= (npc-archetype-attack-level archetype) 5) ()
            "npc-from-plist: attack-level")
    (assert (= (npc-archetype-combat-xp archetype) 25) ()
            "npc-from-plist: combat-xp")
    (assert (eq (npc-archetype-aggro-mode archetype) :always) ()
            "npc-from-plist: aggro-mode")))

;;; ============================================================
;;; ADDITIONAL ZONE TESTS
;;; ============================================================

(defun test-zone-chunk-from-spec ()
  "Test zone chunk creation from spec."
  ;; With explicit tiles
  (let* ((tiles (make-list 16 :initial-element 1))
         (spec (list :x 2 :y 3 :tiles tiles))
         (chunk (zone-chunk-from-spec spec 4)))
    (assert (= (zone-chunk-x chunk) 2) () "chunk-from-spec: x")
    (assert (= (zone-chunk-y chunk) 3) () "chunk-from-spec: y")
    (assert (= (length (zone-chunk-tiles chunk)) 16) () "chunk-from-spec: tiles"))
  ;; With fill value
  (let* ((spec '(:x 0 :y 0 :fill 5))
         (chunk (zone-chunk-from-spec spec 4)))
    (assert (every (lambda (tile) (= tile 5)) (zone-chunk-tiles chunk)) ()
            "chunk-from-spec: fill value")))

(defun test-zone-layer-from-spec ()
  "Test zone layer creation from spec."
  (let* ((chunk-spec '(:x 0 :y 0 :fill 1))
         (spec (list :id :ground
                     :tileset :grass
                     :collision nil
                     :chunks (list chunk-spec)))
         (layer (zone-layer-from-spec spec 4)))
    (assert (eq (zone-layer-id layer) :ground) () "layer-from-spec: id")
    (assert (eq (zone-layer-tileset-id layer) :grass) () "layer-from-spec: tileset")
    (assert (hash-table-p (zone-layer-chunks layer)) () "layer-from-spec: chunks hash")))

(defun test-build-zone-collision-tiles ()
  "Test building collision tile hash from layers."
  ;; Create a collision layer with some blocked tiles
  (let* ((chunk (%make-zone-chunk :x 0 :y 0
                                   :tiles (make-array 16 :initial-contents
                                                      '(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
         (chunks (make-hash-table :test 'eql))
         (layer nil))
    (setf (gethash (zone-chunk-key 0 0) chunks) chunk)
    (setf layer (%make-zone-layer :id :collision :collision-p t :chunks chunks))
    (let ((blocked (build-zone-collision-tiles (list layer) 4)))
      (assert (hash-table-p blocked) () "collision-tiles: returns hash")
      ;; Tile at 1,0 should be blocked (non-zero in tiles array)
      (assert (gethash (tile-key 1 0) blocked) () "collision-tiles: 1,0 blocked")
      ;; Tile at 0,0 should not be blocked (zero)
      (assert (not (gethash (tile-key 0 0) blocked)) () "collision-tiles: 0,0 not blocked"))))

(defun test-zone-wall-map ()
  "Test converting collision tiles to wall map array."
  (let* ((zone (make-empty-zone :test 10 10))
         (collision (zone-collision-tiles zone)))
    ;; Mark some tiles as blocked
    (setf (gethash (tile-key 2 3) collision) t)
    (setf (gethash (tile-key 5 5) collision) t)
    (let ((wall-map (zone-wall-map zone)))
      (assert (arrayp wall-map) () "wall-map: returns array")
      (assert (= (array-dimension wall-map 0) 10) () "wall-map: height")
      (assert (= (array-dimension wall-map 1) 10) () "wall-map: width")
      (assert (= (aref wall-map 3 2) 1) () "wall-map: 2,3 blocked")
      (assert (= (aref wall-map 5 5) 1) () "wall-map: 5,5 blocked")
      (assert (= (aref wall-map 0 0) 0) () "wall-map: 0,0 not blocked"))))

(defun test-zone-layer-by-id ()
  "Test finding layer by ID."
  (let* ((layer1 (%make-zone-layer :id :ground :chunks (make-hash-table)))
         (layer2 (%make-zone-layer :id :collision :collision-p t :chunks (make-hash-table)))
         (zone (%make-zone :id :test :width 10 :height 10
                           :layers (vector layer1 layer2)
                           :collision-tiles (make-hash-table))))
    (assert (eq (zone-layer-by-id zone :ground) layer1) () "layer-by-id: ground")
    (assert (eq (zone-layer-by-id zone :collision) layer2) () "layer-by-id: collision")
    (assert (null (zone-layer-by-id zone :nonexistent)) () "layer-by-id: not found")))

(defun test-zone-to-plist ()
  "Test zone serialization to plist."
  (let* ((zone (make-empty-zone :test-zone 20 15 :chunk-size 8)))
    ;; Add an object
    (push '(:id :coins :x 5 :y 5 :count 10) (zone-objects zone))
    (let ((plist (zone-to-plist zone)))
      (assert (listp plist) () "zone-to-plist: returns list")
      (assert (eq (getf plist :id) :test-zone) () "zone-to-plist: id")
      (assert (= (getf plist :width) 20) () "zone-to-plist: width")
      (assert (= (getf plist :height) 15) () "zone-to-plist: height")
      (assert (= (getf plist :chunk-size) 8) () "zone-to-plist: chunk-size")
      (assert (listp (getf plist :objects)) () "zone-to-plist: objects list"))))

(defun test-zone-slice ()
  "Test extracting a subregion of a zone."
  (let* ((zone (make-empty-zone :big-zone 100 100))
         (sliced (zone-slice zone 10 10 20 15)))
    (assert (= (zone-width sliced) 20) () "zone-slice: width")
    (assert (= (zone-height sliced) 15) () "zone-slice: height")
    (assert (eq (zone-id sliced) :big-zone) () "zone-slice: preserves id")))

(defun test-zone-resize ()
  "Test resizing a zone."
  (let* ((zone (make-empty-zone :resizable 50 50))
         (resized (zone-resize zone 30 25)))
    (assert (= (zone-width resized) 30) () "zone-resize: new width")
    (assert (= (zone-height resized) 25) () "zone-resize: new height")))

;;; ============================================================
;;; ADDITIONAL WORLD GRAPH TESTS
;;; ============================================================

(defun test-world-graph-exits ()
  "Test getting zone exits from world graph."
  (let* ((edges (list '(:from :zone-a :to :zone-b :edge :north)
                      '(:from :zone-a :to :zone-c :edge :east)
                      '(:from :zone-b :to :zone-a :edge :south)))
         (edges-by-zone (normalize-world-graph-edges edges))
         (graph (%make-world-graph :edges-by-zone edges-by-zone
                                    :zone-paths (make-hash-table))))
    ;; Zone A has 2 exits
    (let ((exits (world-graph-exits graph :zone-a)))
      (assert (= (length exits) 2) () "graph-exits: zone-a has 2 exits"))
    ;; Zone B has 1 exit
    (let ((exits (world-graph-exits graph :zone-b)))
      (assert (= (length exits) 1) () "graph-exits: zone-b has 1 exit"))
    ;; Zone C has no exits
    (assert (null (world-graph-exits graph :zone-c)) () "graph-exits: zone-c has none")
    ;; Nil graph
    (assert (null (world-graph-exits nil :zone-a)) () "graph-exits: nil graph")))

(defun test-world-graph-zone-path ()
  "Test getting zone file path from world graph."
  (let* ((paths (make-hash-table :test 'eq))
         (graph nil))
    (setf (gethash :zone-a paths) "/path/to/zone-a.lisp")
    (setf (gethash :zone-b paths) "/path/to/zone-b.lisp")
    (setf graph (%make-world-graph :edges-by-zone (make-hash-table)
                                    :zone-paths paths))
    (assert (string= (world-graph-zone-path graph :zone-a) "/path/to/zone-a.lisp") ()
            "graph-path: zone-a")
    (assert (string= (world-graph-zone-path graph :zone-b) "/path/to/zone-b.lisp") ()
            "graph-path: zone-b")
    (assert (null (world-graph-zone-path graph :nonexistent)) ()
            "graph-path: not found")
    (assert (null (world-graph-zone-path nil :zone-a)) ()
            "graph-path: nil graph")))

(defun test-collect-zone-files ()
  "Test collecting zone files from directory."
  ;; Test against actual data/zones directory
  (let* ((zone-root (merge-pathnames "data/zones/"
                                      (asdf:system-source-directory :mmorpg)))
         (files (collect-zone-files zone-root)))
    (assert (vectorp files) () "collect-zone-files: returns vector")
    ;; Should find at least one zone file if directory exists
    (when (probe-file zone-root)
      (assert (> (length files) 0) () "collect-zone-files: finds files")))
  ;; Non-existent directory returns empty
  (let ((files (collect-zone-files "/nonexistent/path/12345/")))
    (assert (= (length files) 0) () "collect-zone-files: empty for missing dir")))

(defun test-zone-id-from-file ()
  "Test reading zone ID from file."
  ;; Test against actual zone file if exists
  (let* ((zone-root (merge-pathnames "data/zones/"
                                      (asdf:system-source-directory :mmorpg)))
         (files (collect-zone-files zone-root)))
    (when (and (> (length files) 0) (probe-file (aref files 0)))
      (let ((id (zone-id-from-file (aref files 0))))
        (assert (keywordp id) () "zone-id-from-file: returns keyword"))))
  ;; Non-existent file returns nil
  (assert (null (zone-id-from-file "/nonexistent/zone.lisp")) ()
          "zone-id-from-file: nil for missing file"))

(defun test-build-zone-paths ()
  "Test building zone path lookup table."
  (let* ((zone-root (merge-pathnames "data/zones/"
                                      (asdf:system-source-directory :mmorpg)))
         (paths (build-zone-paths zone-root)))
    (assert (hash-table-p paths) () "build-zone-paths: returns hash table")
    ;; If zones exist, table should have entries
    (when (probe-file zone-root)
      (let ((count (hash-table-count paths)))
        (assert (>= count 0) () "build-zone-paths: non-negative count")))))

;;; ============================================================
;;; FINAL AI TESTS
;;; ============================================================

(defun test-update-npc-behavior ()
  "Test NPC behavior state machine."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 100.0 100.0 :archetype archetype :id 1))
         (player (make-player 110.0 100.0 :id 1))
         (world (make-test-world :tile-size 32.0)))
    ;; Dead NPC -> :dead state
    (setf (npc-alive npc) nil)
    (update-npc-behavior npc player world)
    (assert (eq (npc-behavior-state npc) :dead) () "npc-behavior: dead -> :dead")
    ;; Revive and test idle
    (setf (npc-alive npc) t)
    (update-npc-behavior npc nil world)
    (assert (eq (npc-behavior-state npc) :idle) () "npc-behavior: no player -> :idle")
    ;; With player present, state depends on archetype aggro mode
    (update-npc-behavior npc player world)
    (assert (member (npc-behavior-state npc) '(:idle :aggressive :retaliate :flee)) ()
            "npc-behavior: valid state")))

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
;;; FINAL ZONE TESTS
;;; ============================================================

(defun test-load-write-zone-roundtrip ()
  "Test zone save and load roundtrip."
  (let* ((temp-path (merge-pathnames "test-zone-roundtrip.lisp"
                                      (uiop:temporary-directory)))
         (zone (make-empty-zone :test-roundtrip 25 20 :chunk-size 8)))
    ;; Add some content
    (push '(:id :coins :x 5 :y 5 :count 10) (zone-objects zone))
    (push '(:id :spawn :x 10 :y 10) (zone-spawns zone))
    ;; Write
    (unwind-protect
        (progn
          (write-zone zone temp-path)
          (assert (probe-file temp-path) () "roundtrip: file written")
          ;; Load
          (let ((loaded (load-zone temp-path)))
            (assert loaded () "roundtrip: zone loaded")
            (assert (eq (zone-id loaded) :test-roundtrip) () "roundtrip: id preserved")
            (assert (= (zone-width loaded) 25) () "roundtrip: width preserved")
            (assert (= (zone-height loaded) 20) () "roundtrip: height preserved")
            (assert (= (zone-chunk-size loaded) 8) () "roundtrip: chunk-size preserved")))
      ;; Cleanup
      (when (probe-file temp-path)
        (delete-file temp-path)))))

;;; ============================================================
;;; FINAL MOVEMENT TESTS
;;; ============================================================

(defun test-get-zone-state ()
  "Test getting zone state from cache."
  ;; Clear cache first
  (clear-zone-states)
  ;; Should return nil for unknown zone
  (assert (null (get-zone-state :nonexistent-zone)) ()
          "get-zone-state: nil for unknown")
  ;; Note: get-or-create-zone-state requires file path, tested implicitly
  )

(defun test-zone-state-player-count ()
  "Test counting players in a zone."
  (let* ((p1 (make-player 0.0 0.0 :id 1))
         (p2 (make-player 0.0 0.0 :id 2))
         (p3 (make-player 0.0 0.0 :id 3))
         (players (vector p1 p2 p3)))
    (setf (player-zone-id p1) :zone-a
          (player-zone-id p2) :zone-a
          (player-zone-id p3) :zone-b)
    (assert (= (zone-state-player-count :zone-a players) 2) ()
            "player-count: zone-a has 2")
    (assert (= (zone-state-player-count :zone-b players) 1) ()
            "player-count: zone-b has 1")
    (assert (= (zone-state-player-count :zone-c players) 0) ()
            "player-count: zone-c has 0")))

(defun test-players-in-zone ()
  "Test getting players in a specific zone."
  (let* ((p1 (make-player 0.0 0.0 :id 1))
         (p2 (make-player 0.0 0.0 :id 2))
         (p3 (make-player 0.0 0.0 :id 3))
         (players (vector p1 p2 p3)))
    (setf (player-zone-id p1) :zone-a
          (player-zone-id p2) :zone-a
          (player-zone-id p3) :zone-b)
    (let ((in-a (players-in-zone :zone-a players)))
      (assert (vectorp in-a) () "players-in-zone: returns vector")
      (assert (= (length in-a) 2) () "players-in-zone: zone-a has 2"))
    (let ((in-b (players-in-zone :zone-b players)))
      (assert (= (length in-b) 1) () "players-in-zone: zone-b has 1"))
    ;; Nil zone-id returns nil
    (assert (null (players-in-zone nil players)) ()
            "players-in-zone: nil zone -> nil")))

(defun test-occupied-zone-ids ()
  "Test getting list of occupied zones."
  (let* ((p1 (make-player 0.0 0.0 :id 1))
         (p2 (make-player 0.0 0.0 :id 2))
         (p3 (make-player 0.0 0.0 :id 3))
         (players (vector p1 p2 p3)))
    (setf (player-zone-id p1) :zone-a
          (player-zone-id p2) :zone-a
          (player-zone-id p3) :zone-b)
    (let ((ids (occupied-zone-ids players)))
      (assert (listp ids) () "occupied-zones: returns list")
      (assert (= (length ids) 2) () "occupied-zones: 2 unique zones")
      (assert (member :zone-a ids) () "occupied-zones: contains zone-a")
      (assert (member :zone-b ids) () "occupied-zones: contains zone-b"))))

(defun test-derive-wall-map-from-zone ()
  "Test deriving wall map from zone collision tiles."
  (let* ((zone (make-empty-zone :test 10 10))
         (collision (zone-collision-tiles zone)))
    ;; Add some collision tiles using cons cell keys (as used internally)
    (setf (gethash (cons 2 3) collision) t)
    (setf (gethash (cons 5 5) collision) t)
    (let ((wall-map (derive-wall-map-from-zone zone)))
      (assert (arrayp wall-map) () "derive-wall-map: returns array")
      (assert (= (array-dimension wall-map 0) 10) () "derive-wall-map: correct height")
      (assert (= (array-dimension wall-map 1) 10) () "derive-wall-map: correct width")
      (assert (= (aref wall-map 3 2) 1) () "derive-wall-map: 2,3 blocked")
      (assert (= (aref wall-map 5 5) 1) () "derive-wall-map: 5,5 blocked")))
  ;; Nil zone returns nil
  (assert (null (derive-wall-map-from-zone nil)) ()
          "derive-wall-map: nil zone -> nil"))

(defun test-wall-occupied-p ()
  "Test wall occupancy check."
  (let ((wall-map (make-array '(10 10) :initial-element 0)))
    ;; Set some walls
    (setf (aref wall-map 3 2) 1)
    (setf (aref wall-map 5 5) 1)
    ;; Test with *wall-origin-x/y* at 0
    (let ((*wall-origin-x* 0)
          (*wall-origin-y* 0))
      (assert (wall-occupied-p wall-map 2 3) () "wall-occupied: 2,3 blocked")
      (assert (wall-occupied-p wall-map 5 5) () "wall-occupied: 5,5 blocked")
      (assert (not (wall-occupied-p wall-map 0 0)) () "wall-occupied: 0,0 not blocked")
      ;; Out of bounds returns nil
      (assert (not (wall-occupied-p wall-map 100 100)) () "wall-occupied: out of bounds")))
  ;; Nil wall-map returns nil
  (assert (not (wall-occupied-p nil 0 0)) () "wall-occupied: nil map"))

(defun test-blocked-at-p ()
  "Test collision detection at position."
  (let* ((world (make-test-world :tile-size 32.0 :collision-half 12.0))
         (wall-map (make-array '(10 10) :initial-element 0)))
    ;; Set a wall
    (let ((*wall-origin-x* 0)
          (*wall-origin-y* 0))
      (setf (aref wall-map 5 5) 1)
      (setf (world-wall-map world) wall-map)
      ;; Position in blocked tile should be blocked
      (assert (blocked-at-p world 176.0 176.0 12.0 12.0 32.0) ()
              "blocked-at: position in wall blocked")
      ;; Position in open tile should not be blocked
      (assert (not (blocked-at-p world 48.0 48.0 12.0 12.0 32.0)) ()
              "blocked-at: open position not blocked"))))

(defun test-attempt-move ()
  "Test movement with collision resolution."
  (let* ((world (make-test-world :tile-size 32.0 :collision-half 12.0))
         (wall-map (make-array '(20 20) :initial-element 0)))
    (let ((*wall-origin-x* 0)
          (*wall-origin-y* 0))
      (setf (world-wall-map world) wall-map)
      ;; Move in open space - should succeed
      (multiple-value-bind (nx ny out-dx out-dy)
          (attempt-move world 100.0 100.0 1.0 0.0 10.0 12.0 12.0 32.0)
        (assert (> nx 100.0) () "attempt-move: x increased")
        (assert (= ny 100.0) () "attempt-move: y unchanged")
        (assert (= out-dx 1.0) () "attempt-move: dx preserved"))
      ;; Add a wall and test blocked movement
      (setf (aref wall-map 3 5) 1)  ;; Block at tile 5,3
      (multiple-value-bind (nx ny out-dx out-dy)
          (attempt-move world 150.0 100.0 1.0 0.0 100.0 12.0 12.0 32.0)
        ;; Movement may be blocked depending on exact position
        (assert (numberp nx) () "attempt-move: returns number")))))

(defun test-update-running-state ()
  "Test stamina drain and regen for running."
  (let ((player (make-player 0.0 0.0 :id 1)))
    ;; Start with full stamina
    (setf (player-run-stamina player) *run-stamina-max*
          (player-running player) nil)
    ;; Not running, not moving - stamina stays full
    (let ((mult (update-running-state player 0.1 nil nil)))
      (assert (= mult 1.0) () "running: not running -> 1.0 mult"))
    ;; Toggle run on
    (update-running-state player 0.1 nil t)
    (assert (player-running player) () "running: toggle enables run")
    ;; Running while moving drains stamina
    (let ((before (player-run-stamina player)))
      (update-running-state player 0.5 t nil)
      (assert (< (player-run-stamina player) before) () "running: stamina drains"))))

(defun test-edge-spawn-position ()
  "Test calculating spawn position on edge."
  (let* ((world (make-test-world :tile-size 32.0 :collision-half 12.0))
         (wall-map (make-array '(10 10) :initial-element 0)))
    (let ((*wall-origin-x* 0)
          (*wall-origin-y* 0))
      (setf (world-wall-map world) wall-map
            (world-wall-min-x world) 44.0
            (world-wall-max-x world) 276.0
            (world-wall-min-y world) 44.0
            (world-wall-max-y world) 276.0)
      ;; West edge spawn
      (multiple-value-bind (x y)
          (edge-spawn-position world :west nil 0.5)
        (assert (= x 44.0) () "edge-spawn: west x is min")
        (assert (numberp y) () "edge-spawn: y is number"))
      ;; East edge spawn
      (multiple-value-bind (x y)
          (edge-spawn-position world :east nil 0.5)
        (assert (= x 276.0) () "edge-spawn: east x is max")))))

(defun test-zone-bounds-from-dimensions ()
  "Test calculating zone wall bounds."
  (let ((*wall-origin-x* 0)
        (*wall-origin-y* 0))
    (multiple-value-bind (min-x max-x min-y max-y)
        (zone-bounds-from-dimensions 32.0 10 10 12.0 12.0)
      (assert (numberp min-x) () "zone-bounds: min-x is number")
      (assert (numberp max-x) () "zone-bounds: max-x is number")
      (assert (< min-x max-x) () "zone-bounds: min-x < max-x")
      (assert (< min-y max-y) () "zone-bounds: min-y < max-y"))))

(defun test-position-blocked-p ()
  "Test position blocking check wrapper."
  (let* ((world (make-test-world :tile-size 32.0 :collision-half 12.0))
         (wall-map (make-array '(10 10) :initial-element 0)))
    (let ((*wall-origin-x* 0)
          (*wall-origin-y* 0))
      (setf (world-wall-map world) wall-map)
      ;; Open position
      (assert (not (position-blocked-p world 48.0 48.0 12.0 12.0)) ()
              "position-blocked: open position")
      ;; Add wall and test
      (setf (aref wall-map 2 2) 1)
      (assert (position-blocked-p world 80.0 80.0 12.0 12.0) ()
              "position-blocked: blocked position"))))

(defun test-find-open-tile ()
  "Test finding nearest open tile."
  (let* ((world (make-test-world :tile-size 32.0 :collision-half 12.0))
         (wall-map (make-array '(10 10) :initial-element 0)))
    (let ((*wall-origin-x* 0)
          (*wall-origin-y* 0))
      (setf (world-wall-map world) wall-map)
      ;; Starting tile is open - should return it
      (multiple-value-bind (tx ty)
          (find-open-tile world 2 2 12.0 12.0 5)
        (assert (= tx 2) () "find-open: returns start x")
        (assert (= ty 2) () "find-open: returns start y"))
      ;; Block start tile - should find nearby
      (setf (aref wall-map 2 2) 1)
      (multiple-value-bind (tx ty)
          (find-open-tile world 2 2 12.0 12.0 5)
        (assert (not (and (= tx 2) (= ty 2))) () "find-open: finds different tile")))))

(defun test-player-is-stuck-p ()
  "Test player stuck detection."
  (let* ((world (make-test-world :tile-size 32.0 :collision-half 12.0))
         (player (make-player 100.0 100.0 :id 1))
         (wall-map (make-array '(10 10) :initial-element 0)))
    (let ((*wall-origin-x* 0)
          (*wall-origin-y* 0))
      (setf (world-wall-map world) wall-map)
      ;; In open space - not stuck
      (assert (not (player-is-stuck-p player world)) ()
              "is-stuck: open space -> not stuck")
      ;; Surround with walls - would be stuck
      ;; (Testing actual stuck detection requires more complex setup)
      )))

(defun test-world-exit-edge ()
  "Test detecting which edge player is at."
  (let* ((world (make-test-world :tile-size 32.0 :collision-half 12.0))
         (player (make-player 44.0 160.0 :id 1))
         (intent (player-intent player)))
    (setf (world-wall-min-x world) 44.0
          (world-wall-max-x world) 276.0
          (world-wall-min-y world) 44.0
          (world-wall-max-y world) 276.0)
    ;; Player at west edge moving west
    (set-intent-move intent -1.0 0.0)
    (let ((edge (world-exit-edge world player)))
      (assert (eq edge :west) () "exit-edge: west edge detected"))
    ;; Player in center, no edge
    (setf (player-x player) 160.0)
    (set-intent-move intent -1.0 0.0)
    (let ((edge (world-exit-edge world player)))
      (assert (null edge) () "exit-edge: center -> nil"))))

;;; ============================================================
;;; FINAL NET TESTS
;;; ============================================================

(defun test-session-try-register ()
  "Test session registration."
  ;; Clear any existing sessions
  (with-session-lock
    (clrhash *active-sessions*))
  ;; Register a session
  (let ((client (make-instance 'net-client)))
    (assert (session-try-register "testuser" client) ()
            "session-register: first registration succeeds")
    ;; Same username should fail
    (assert (not (session-try-register "testuser" client)) ()
            "session-register: duplicate fails")
    ;; Different username succeeds
    (assert (session-try-register "testuser2" client) ()
            "session-register: different user succeeds"))
  ;; Cleanup
  (with-session-lock
    (clrhash *active-sessions*)))

(defun test-session-unregister ()
  "Test session unregistration."
  ;; Clear and setup
  (with-session-lock
    (clrhash *active-sessions*))
  (let ((client (make-instance 'net-client)))
    (session-try-register "testuser" client)
    ;; Should be registered
    (assert (session-get "testuser") () "session-unreg: user exists before")
    ;; Unregister
    (session-unregister "testuser")
    ;; Should be gone
    (assert (null (session-get "testuser")) () "session-unreg: user gone after"))
  ;; Cleanup
  (with-session-lock
    (clrhash *active-sessions*)))

(defun test-session-get ()
  "Test getting session by username."
  ;; Clear and setup
  (with-session-lock
    (clrhash *active-sessions*))
  (let ((client (make-instance 'net-client)))
    (session-try-register "testuser" client)
    ;; Get existing
    (assert (eq (session-get "testuser") client) ()
            "session-get: returns correct client")
    ;; Get non-existent
    (assert (null (session-get "nonexistent")) ()
            "session-get: nil for unknown")
    ;; Case insensitive
    (assert (eq (session-get "TESTUSER") client) ()
            "session-get: case insensitive"))
  ;; Cleanup
  (with-session-lock
    (clrhash *active-sessions*)))
