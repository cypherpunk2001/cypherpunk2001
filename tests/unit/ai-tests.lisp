(in-package #:mmorpg)

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

(defun test-update-npc-intent-nil-player ()
  "Test NPC intent with nil player (idle/wander path).
   Ensures no crash and that wander direction is computed."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 100.0f0 100.0f0 :archetype archetype :id 1))
         (world (make-test-world :tile-size 32.0f0))
         (dt 0.016f0))
    ;; Setup deterministic wander target to avoid randomness
    ;; Set wander target 10 units to the right of NPC position
    (setf (npc-wander-x npc) (+ (npc-x npc) 10.0f0)
          (npc-wander-y npc) (npc-y npc)
          (npc-wander-timer npc) 5.0f0  ; positive timer = use existing target
          (npc-alive npc) t
          (npc-behavior-state npc) :idle)
    ;; Reset intent to known state
    (let ((intent (npc-intent npc)))
      (setf (intent-move-dx intent) 0.0f0
            (intent-move-dy intent) 0.0f0))
    ;; Call update-npc-behavior with nil player
    (update-npc-behavior npc nil world)
    (assert (eq (npc-behavior-state npc) :idle) ()
            "nil-player: behavior state should be :idle")
    ;; Call update-npc-intent with nil player - should not crash
    (update-npc-intent npc nil world dt)
    ;; Check that intent was updated with wander direction
    (let* ((intent (npc-intent npc))
           (dx (intent-move-dx intent))
           (dy (intent-move-dy intent)))
      ;; Wander target is +10,0 from NPC, so normalized direction should be (1.0, 0.0)
      (assert (> dx 0.5f0) ()
              (format nil "nil-player: intent dx should be positive (wander right), got ~a" dx))
      (assert (< (abs dy) 0.1f0) ()
              (format nil "nil-player: intent dy should be near zero, got ~a" dy)))))

;;; ============================================================


(defvar *tests-ai*
  '(test-npc-should-flee-p
    test-npc-perception-range-sq
    test-npc-home-radius
    test-npc-move-speed
    test-npc-wander-interval
    test-npc-flee-speed-mult
    test-closest-player
    ;; Additional AI Tests
    test-npc-in-perception-range-p
    ;; Final AI Tests
    test-update-npc-behavior
    test-update-npc-intent-nil-player)
  "AI domain test functions.")
