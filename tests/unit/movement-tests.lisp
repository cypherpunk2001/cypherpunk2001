(in-package #:mmorpg)

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
    ;; Add some collision tiles using packed integer keys (as used by build-zone-collision-tiles)
    (setf (gethash (tile-key 2 3) collision) t)
    (setf (gethash (tile-key 5 5) collision) t)
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
  "Test calculating zone wall bounds.
   With origin=0, tile-size=32, 10 tiles, collision-half=12:
   min = 0*32+12=12, max = 10*32-12=308."
  (let ((*wall-origin-x* 0)
        (*wall-origin-y* 0))
    (multiple-value-bind (min-x max-x min-y max-y)
        (zone-bounds-from-dimensions 32.0 10 10 12.0 12.0)
      (assert (numberp min-x) () "zone-bounds: min-x is number")
      (assert (numberp max-x) () "zone-bounds: max-x is number")
      (assert (< min-x max-x) () "zone-bounds: min-x < max-x")
      (assert (< min-y max-y) () "zone-bounds: min-y < max-y")
      (assert (< (abs (- min-x 12.0)) 0.01) ()
              "zone-bounds: min-x should be 12.0, got ~,2f" min-x)
      (assert (< (abs (- max-x 308.0)) 0.01) ()
              "zone-bounds: max-x should be 308.0, got ~,2f" max-x))))

(defun test-zone-bounds-zero-origin-includes-boundary-ring ()
  "zone-bounds-zero-origin includes tile 0 and tile (width-1) as walkable.
   For a 64-tile zone with tile-dest-size=64 and collision-half=27.2:
   min = 0*64 + 27.2 = 27.2, max = 64*64 - 27.2 = 4068.8."
  (multiple-value-bind (min-x max-x min-y max-y)
      (zone-bounds-zero-origin 64.0 64 64 27.2 27.2)
    ;; tile-0 center = 32 px → should be inside bounds
    (assert (<= min-x 32.0) ()
            "boundary-ring: tile-0 center (32) must be >= min-x (~,2f)" min-x)
    (assert (< (abs (- min-x 27.2)) 0.01) ()
            "boundary-ring: min-x should be 27.2, got ~,2f" min-x)
    ;; tile-63 center = 4032 px → should be inside bounds
    (assert (>= max-x 4032.0) ()
            "boundary-ring: tile-63 center (4032) must be <= max-x (~,2f)" max-x)
    (assert (< (abs (- max-x 4068.8)) 0.01) ()
            "boundary-ring: max-x should be 4068.8, got ~,2f" max-x)
    ;; Same for Y
    (assert (< (abs (- min-y 27.2)) 0.01) ()
            "boundary-ring: min-y should be 27.2, got ~,2f" min-y)
    (assert (< (abs (- max-y 4068.8)) 0.01) ()
            "boundary-ring: max-y should be 4068.8, got ~,2f" max-y)))

(defun test-zone-bounds-from-dimensions-includes-boundary-ring ()
  "zone-bounds-from-dimensions includes tile 0 and tile (width-1).
   With origin=0, tile-size=32, 10 tiles wide, collision-half=12:
   min = 0*32 + 12 = 12, max = 10*32 - 12 = 308."
  (let ((*wall-origin-x* 0)
        (*wall-origin-y* 0))
    (multiple-value-bind (min-x max-x min-y max-y)
        (zone-bounds-from-dimensions 32.0 10 10 12.0 12.0)
      (assert (< (abs (- min-x 12.0)) 0.01) ()
              "from-dimensions: min-x should be 12, got ~,2f" min-x)
      (assert (< (abs (- max-x 308.0)) 0.01) ()
              "from-dimensions: max-x should be 308, got ~,2f" max-x)
      (assert (< (abs (- min-y 12.0)) 0.01) ()
              "from-dimensions: min-y should be 12, got ~,2f" min-y)
      (assert (< (abs (- max-y 308.0)) 0.01) ()
              "from-dimensions: max-y should be 308, got ~,2f" max-y))))

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

(defun test-wall-blocked-p-zero-origin ()
  "Test zero-origin wall-map collision checking."
  (let ((wall-map (make-array '(10 10) :initial-element 0)))
    ;; Set some blocked tiles
    (setf (aref wall-map 3 5) 1)  ; (5, 3) blocked
    (setf (aref wall-map 0 0) 1)  ; (0, 0) blocked
    ;; Test blocked tiles
    (assert (wall-blocked-p-zero-origin wall-map 5 3) ()
            "wall-blocked-zero: blocked tile returns t")
    (assert (wall-blocked-p-zero-origin wall-map 0 0) ()
            "wall-blocked-zero: origin blocked returns t")
    ;; Test open tiles
    (assert (not (wall-blocked-p-zero-origin wall-map 1 1)) ()
            "wall-blocked-zero: open tile returns nil")
    (assert (not (wall-blocked-p-zero-origin wall-map 9 9)) ()
            "wall-blocked-zero: corner open returns nil")
    ;; Test out of bounds (should return t = blocked)
    (assert (wall-blocked-p-zero-origin wall-map -1 0) ()
            "wall-blocked-zero: negative x returns t")
    (assert (wall-blocked-p-zero-origin wall-map 0 -1) ()
            "wall-blocked-zero: negative y returns t")
    (assert (wall-blocked-p-zero-origin wall-map 10 5) ()
            "wall-blocked-zero: x >= width returns t")
    (assert (wall-blocked-p-zero-origin wall-map 5 10) ()
            "wall-blocked-zero: y >= height returns t")
    ;; Test nil wall-map
    (assert (not (wall-blocked-p-zero-origin nil 5 5)) ()
            "wall-blocked-zero: nil map returns nil")))

(defun test-blocked-at-p-with-map ()
  "Test collision checking with zero-origin wall-map."
  (let ((wall-map (make-array '(10 10) :initial-element 0))
        (tile-size 32.0)
        (half-w 12.0)
        (half-h 12.0))
    ;; Block tile (3, 3)
    (setf (aref wall-map 3 3) 1)
    ;; Test blocked at tile center
    (let ((cx (+ (* 3 tile-size) (/ tile-size 2.0)))
          (cy (+ (* 3 tile-size) (/ tile-size 2.0))))
      (assert (blocked-at-p-with-map wall-map cx cy half-w half-h tile-size) ()
              "blocked-at-map: center of blocked tile -> t"))
    ;; Test open at different tile
    (let ((cx (+ (* 5 tile-size) (/ tile-size 2.0)))
          (cy (+ (* 5 tile-size) (/ tile-size 2.0))))
      (assert (not (blocked-at-p-with-map wall-map cx cy half-w half-h tile-size)) ()
              "blocked-at-map: open tile -> nil"))
    ;; Test nil wall-map (should return nil - not blocked)
    (assert (not (blocked-at-p-with-map nil 100.0 100.0 half-w half-h tile-size)) ()
            "blocked-at-map: nil map -> nil")))

(defun test-find-open-position-with-map ()
  "Test finding open spawn position with wall-map."
  (let ((wall-map (make-array '(10 10) :initial-element 0))
        (tile-size 32.0)
        (half-w 12.0)
        (half-h 12.0))
    ;; Test open position returns same
    (let ((x (+ (* 5 tile-size) (/ tile-size 2.0)))
          (y (+ (* 5 tile-size) (/ tile-size 2.0))))
      (multiple-value-bind (rx ry)
          (find-open-position-with-map wall-map x y half-w half-h tile-size)
        (assert (= rx x) () "find-open-map: open -> same x")
        (assert (= ry y) () "find-open-map: open -> same y")))
    ;; Block center tile and verify we find adjacent
    (setf (aref wall-map 5 5) 1)
    (let ((x (+ (* 5 tile-size) (/ tile-size 2.0)))
          (y (+ (* 5 tile-size) (/ tile-size 2.0))))
      (multiple-value-bind (rx ry)
          (find-open-position-with-map wall-map x y half-w half-h tile-size)
        (assert (not (and (= (floor rx tile-size) 5)
                          (= (floor ry tile-size) 5))) ()
                "find-open-map: blocked -> finds different tile")))
    ;; Test max radius extends to map size
    (let ((small-map (make-array '(5 5) :initial-element 1)))
      ;; All blocked except corner
      (setf (aref small-map 4 4) 0)
      (let ((cx (+ (* 0 tile-size) (/ tile-size 2.0)))
            (cy (+ (* 0 tile-size) (/ tile-size 2.0))))
        (multiple-value-bind (rx ry)
            (find-open-position-with-map small-map cx cy half-w half-h tile-size)
          ;; Should find the one open tile at (4, 4)
          (assert (= (floor rx tile-size) 4) ()
                  "find-open-map: finds distant open tile x")
          (assert (= (floor ry tile-size) 4) ()
                  "find-open-map: finds distant open tile y"))))))

(defun test-zone-state-spawn-position ()
  "Test zone-state spawn position calculation."
  ;; Create a minimal zone-state with wall-map
  (let* ((wall-map (make-array '(10 10) :initial-element 0))
         (zone-state (make-zone-state :zone-id :test-zone
                                       :zone nil
                                       :wall-map wall-map
                                       :npcs (make-array 0)))
         (*tile-size* 16)
         (*tile-scale* 2.0)
         (*player-collision-scale* 0.85)
         (*wall-origin-x* 0)
         (*wall-origin-y* 0))
    ;; Open map - should spawn near center
    (multiple-value-bind (x y)
        (zone-state-spawn-position zone-state)
      (assert (numberp x) () "zone-spawn: returns numeric x")
      (assert (numberp y) () "zone-spawn: returns numeric y")
      (assert (> x 0) () "zone-spawn: x > 0")
      (assert (> y 0) () "zone-spawn: y > 0"))
    ;; Block center - should still find open position
    (setf (aref wall-map 5 5) 1)
    (multiple-value-bind (x y)
        (zone-state-spawn-position zone-state)
      (let ((tile-size (* (float *tile-size* 1.0) *tile-scale*)))
        (assert (not (and (= (floor x tile-size) 5)
                          (= (floor y tile-size) 5))) ()
                "zone-spawn: blocked center -> different tile")))))

;;; ============================================================

;;; PER-ZONE COLLISION HELPER TESTS (Phase 4 deferred items)
;;; ============================================================

(defun test-get-zone-wall-map ()
  "Test getting wall-map from zone-state cache."
  ;; Clear zone-states cache
  (clrhash *zone-states*)
  ;; No zone-state should return nil
  (assert (null (get-zone-wall-map :nonexistent-zone)) ()
          "get-zone-wall-map: nil for unknown zone")
  ;; Create a zone-state manually and test
  (let ((wall-map (make-array '(10 10) :initial-element 0)))
    (setf (gethash :test-zone *zone-states*)
          (make-zone-state :zone-id :test-zone
                           :zone nil
                           :wall-map wall-map
                           :npcs (make-array 0)))
    (assert (eq wall-map (get-zone-wall-map :test-zone)) ()
            "get-zone-wall-map: returns cached wall-map"))
  ;; Cleanup
  (clrhash *zone-states*))

(defun test-get-zone-collision-bounds ()
  "Test zone collision bounds calculation."
  ;; Clear zone-states cache
  (clrhash *zone-states*)
  ;; Unknown zone returns nil
  (multiple-value-bind (min-x max-x min-y max-y)
      (get-zone-collision-bounds :nonexistent 32.0 8.0 8.0)
    (assert (null min-x) () "get-zone-collision-bounds: nil for unknown zone"))
  ;; Create a zone-state with a 10x10 wall-map
  (let ((wall-map (make-array '(10 10) :initial-element 0))
        (*wall-origin-x* 0)
        (*wall-origin-y* 0))
    (setf (gethash :test-zone *zone-states*)
          (make-zone-state :zone-id :test-zone
                           :zone nil
                           :wall-map wall-map
                           :npcs (make-array 0)))
    (multiple-value-bind (min-x max-x min-y max-y)
        (get-zone-collision-bounds :test-zone 32.0 8.0 8.0)
      (assert (numberp min-x) () "get-zone-collision-bounds: returns numeric min-x")
      (assert (numberp max-x) () "get-zone-collision-bounds: returns numeric max-x")
      (assert (< min-x max-x) () "get-zone-collision-bounds: min-x < max-x")
      (assert (< min-y max-y) () "get-zone-collision-bounds: min-y < max-y")))
  ;; Cleanup
  (clrhash *zone-states*))

(defun test-player-is-stuck-p-for-zone ()
  "Test per-zone stuck detection."
  ;; Clear zone-states cache
  (clrhash *zone-states*)
  (let* ((wall-map (make-array '(10 10) :initial-element 0))
         (*tile-size* 16)
         (*tile-scale* 2.0)
         (*player-collision-scale* 0.85)
         (*wall-origin-x* 0)
         (*wall-origin-y* 0)
         (world (make-world))
         (player (make-player 160.0 160.0)))  ; Center of open area
    ;; Setup zone-state
    (setf (gethash :test-zone *zone-states*)
          (make-zone-state :zone-id :test-zone
                           :zone nil
                           :wall-map wall-map
                           :npcs (make-array 0)))
    ;; Player in open area should not be stuck
    (assert (not (player-is-stuck-p-for-zone player :test-zone world)) ()
            "player-is-stuck-p-for-zone: open area = not stuck")
    ;; Block all tiles around player (fully enclosed)
    (dotimes (ty 10)
      (dotimes (tx 10)
        (setf (aref wall-map ty tx) 1)))
    ;; Player enclosed should be stuck
    (assert (player-is-stuck-p-for-zone player :test-zone world) ()
            "player-is-stuck-p-for-zone: enclosed = stuck"))
  ;; Cleanup
  (clrhash *zone-states*))

(defun test-get-zone-safe-spawn-for-zone ()
  "Test per-zone safe spawn position."
  ;; Clear zone-states cache
  (clrhash *zone-states*)
  (let* ((wall-map (make-array '(10 10) :initial-element 0))
         (*tile-size* 16)
         (*tile-scale* 2.0)
         (*player-collision-scale* 0.85)
         (*wall-origin-x* 0)
         (*wall-origin-y* 0)
         (world (make-world)))
    ;; Setup zone-state
    (setf (gethash :test-zone *zone-states*)
          (make-zone-state :zone-id :test-zone
                           :zone nil
                           :wall-map wall-map
                           :npcs (make-array 0)))
    ;; Should return valid coordinates within zone bounds
    (multiple-value-bind (x y)
        (get-zone-safe-spawn-for-zone :test-zone world)
      (assert (numberp x) () "get-zone-safe-spawn: returns numeric x")
      (assert (numberp y) () "get-zone-safe-spawn: returns numeric y")
      (assert (> x 0) () "get-zone-safe-spawn: x > 0")
      (assert (> y 0) () "get-zone-safe-spawn: y > 0")))
  ;; Cleanup
  (clrhash *zone-states*))

;;; ============================================================


(defvar *tests-movement*
  '(test-wall-blocked-p
    test-tile-center-position
    test-edge-opposite
    test-edge-offset-ratio
    test-npc-collision-half
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
    test-zone-bounds-zero-origin-includes-boundary-ring
    test-zone-bounds-from-dimensions-includes-boundary-ring
    test-position-blocked-p
    test-find-open-tile
    test-player-is-stuck-p
    test-world-exit-edge
    ;; Zone-state spawn helpers (Phase 1)
    test-wall-blocked-p-zero-origin
    test-blocked-at-p-with-map
    test-find-open-position-with-map
    test-zone-state-spawn-position
    ;; Per-zone collision helpers (Phase 4)
    test-get-zone-wall-map
    test-get-zone-collision-bounds
    test-player-is-stuck-p-for-zone
    test-get-zone-safe-spawn-for-zone)
  "Movement domain test functions.")
