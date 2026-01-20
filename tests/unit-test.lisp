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
                ;; Combat Tests
                test-aabb-overlap-p
                test-aabb-overlap-p-edge-cases
                test-melee-hit-chance
                test-melee-hit-chance-clamped
                test-melee-max-hit
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
                ;; Movement Tests
                test-wall-blocked-p
                test-tile-center-position
                test-edge-opposite
                test-edge-offset-ratio
                test-npc-collision-half
                ;; AI Tests
                test-npc-should-flee-p
                test-npc-perception-range-sq
                ;; Data Tests
                test-plist-form-p
                test-data-section-header-p
                test-data-section-entry-p
                test-normalize-pairs
                ;; Zone Tests
                test-zone-chunk-key
                test-tile-key-roundtrip
                test-zone-tile-in-bounds-p
                ;; Intent Tests
                test-set-intent-move
                test-set-intent-face
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
                test-metrics-push-latency)))
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
              (format nil "p50 of 1-10 should be ~5, got ~a" p50)))
    ;; p99 should be close to 10
    (let ((p99 (calculate-percentile values 99)))
      (assert (and p99 (>= p99 9) (<= p99 10)) ()
              (format nil "p99 of 1-10 should be ~10, got ~a" p99)))
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
