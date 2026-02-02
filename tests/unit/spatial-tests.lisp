(in-package #:mmorpg)

;;;; ======================================================================
;;;; Spatial Grid Tests
;;;; ======================================================================

(defun test-spatial-grid-insert-remove ()
  "Test basic spatial grid insert and remove operations."
  (let ((grid (mmorpg::make-spatial-grid 128.0)))
    ;; Insert entity at position
    (mmorpg::spatial-grid-insert grid 1 100.0 100.0)
    ;; Verify entity is in the grid
    (multiple-value-bind (cx cy) (mmorpg::position-to-cell 100.0 100.0 128.0)
      (let ((ids (mmorpg::spatial-grid-get-cell grid cx cy)))
        (assert (member 1 ids) () "Entity 1 should be in cell after insert")))
    ;; Remove entity
    (multiple-value-bind (cx cy) (mmorpg::position-to-cell 100.0 100.0 128.0)
      (mmorpg::spatial-grid-remove grid 1 cx cy)
      (let ((ids (mmorpg::spatial-grid-get-cell grid cx cy)))
        (assert (not (member 1 ids)) () "Entity 1 should not be in cell after remove")))))

(defun test-spatial-grid-move ()
  "Test spatial grid move operation."
  (let ((grid (mmorpg::make-spatial-grid 128.0)))
    ;; Insert at initial position
    (mmorpg::spatial-grid-insert grid 1 50.0 50.0)
    ;; Move within same cell (should not change)
    (multiple-value-bind (new-cx new-cy changed)
        (mmorpg::spatial-grid-move grid 1 0 0 60.0 60.0)
      (declare (ignore new-cx new-cy))
      (assert (null changed) () "Moving within same cell should not report change"))
    ;; Move to different cell
    (multiple-value-bind (new-cx new-cy changed)
        (mmorpg::spatial-grid-move grid 1 0 0 200.0 200.0)
      (assert changed () "Moving to different cell should report change")
      ;; Verify new position
      (let ((ids (mmorpg::spatial-grid-get-cell grid new-cx new-cy)))
        (assert (member 1 ids) () "Entity 1 should be in new cell")))))

(defun test-spatial-grid-query-neighbors ()
  "Test spatial grid neighbor query (3x3 area)."
  (let ((grid (mmorpg::make-spatial-grid 128.0)))
    ;; Insert entities at different cells
    (mmorpg::spatial-grid-insert grid 1 64.0 64.0)     ; cell (0,0)
    (mmorpg::spatial-grid-insert grid 2 192.0 64.0)    ; cell (1,0)
    (mmorpg::spatial-grid-insert grid 3 320.0 64.0)    ; cell (2,0) - outside 3x3
    (mmorpg::spatial-grid-insert grid 4 64.0 192.0)    ; cell (0,1)
    ;; Query neighbors around (0,0)
    (let ((nearby (mmorpg::spatial-grid-query-neighbors grid 0 0)))
      (assert (member 1 nearby) () "Entity 1 should be in neighbors")
      (assert (member 2 nearby) () "Entity 2 should be in neighbors")
      (assert (not (member 3 nearby)) () "Entity 3 should not be in neighbors (too far)")
      (assert (member 4 nearby) () "Entity 4 should be in neighbors"))))

(defun test-spatial-grid-query-radius ()
  "Test spatial grid radius query."
  (let ((grid (mmorpg::make-spatial-grid 128.0)))
    ;; Insert entities at different cells
    (mmorpg::spatial-grid-insert grid 1 64.0 64.0)     ; cell (0,0)
    (mmorpg::spatial-grid-insert grid 2 320.0 64.0)    ; cell (2,0)
    (mmorpg::spatial-grid-insert grid 3 576.0 64.0)    ; cell (4,0)
    ;; Radius 1 (3x3) should not include entity 3
    (let ((nearby (mmorpg::spatial-grid-query-radius grid 0 0 1)))
      (assert (member 1 nearby) () "Radius 1: Entity 1 should be included")
      (assert (not (member 3 nearby)) () "Radius 1: Entity 3 should not be included"))
    ;; Radius 2 (5x5) should include entities 1 and 2 but not 3
    (let ((nearby (mmorpg::spatial-grid-query-radius grid 0 0 2)))
      (assert (member 1 nearby) () "Radius 2: Entity 1 should be included")
      (assert (member 2 nearby) () "Radius 2: Entity 2 should be included")
      (assert (not (member 3 nearby)) () "Radius 2: Entity 3 should not be included"))))

(defun test-position-to-cell ()
  "Test position to cell coordinate conversion."
  (let ((cell-size 128.0))
    ;; Test basic conversion
    (multiple-value-bind (cx cy) (mmorpg::position-to-cell 0.0 0.0 cell-size)
      (assert (= cx 0) () "Position (0,0) should be in cell 0")
      (assert (= cy 0) () "Position (0,0) should be in cell 0"))
    ;; Test position in second cell
    (multiple-value-bind (cx cy) (mmorpg::position-to-cell 130.0 260.0 cell-size)
      (assert (= cx 1) () "Position (130,260) should be in cell x=1")
      (assert (= cy 2) () "Position (130,260) should be in cell y=2"))
    ;; Test negative coordinates
    (multiple-value-bind (cx cy) (mmorpg::position-to-cell -50.0 -200.0 cell-size)
      (assert (= cx -1) () "Position (-50,-200) should be in cell x=-1")
      (assert (= cy -2) () "Position (-50,-200) should be in cell y=-2"))))

(defun test-entity-cell-changed-p ()
  "Test entity cell change detection."
  (let ((cell-size 128.0))
    ;; Same cell - no change
    (assert (not (mmorpg::entity-cell-changed-p 0 0 50.0 50.0 cell-size))
            () "Position in same cell should not report change")
    ;; Different cell - change
    (assert (mmorpg::entity-cell-changed-p 0 0 200.0 50.0 cell-size)
            () "Position in different cell should report change")
    ;; Nil old cell - always change
    (assert (mmorpg::entity-cell-changed-p nil nil 50.0 50.0 cell-size)
            () "Nil old cell should report change")))

(defun test-spatial-grid-stats ()
  "Test spatial grid statistics."
  (let ((grid (mmorpg::make-spatial-grid 128.0)))
    ;; Empty grid
    (multiple-value-bind (cell-count entity-count)
        (mmorpg::spatial-grid-stats grid)
      (assert (= cell-count 0) () "Empty grid should have 0 cells")
      (assert (= entity-count 0) () "Empty grid should have 0 entities"))
    ;; Add some entities
    (mmorpg::spatial-grid-insert grid 1 64.0 64.0)
    (mmorpg::spatial-grid-insert grid 2 64.0 64.0)  ; Same cell as 1
    (mmorpg::spatial-grid-insert grid 3 200.0 200.0)  ; Different cell
    (multiple-value-bind (cell-count entity-count)
        (mmorpg::spatial-grid-stats grid)
      (assert (= cell-count 2) () "Grid should have 2 occupied cells")
      (assert (= entity-count 3) () "Grid should have 3 entities total"))))

(defun test-spatial-grid-clear ()
  "Test spatial grid clear operation."
  (let ((grid (mmorpg::make-spatial-grid 128.0)))
    ;; Add some entities
    (mmorpg::spatial-grid-insert grid 1 64.0 64.0)
    (mmorpg::spatial-grid-insert grid 2 200.0 200.0)
    ;; Clear grid
    (mmorpg::spatial-grid-clear grid)
    ;; Verify empty
    (multiple-value-bind (cell-count entity-count)
        (mmorpg::spatial-grid-stats grid)
      (assert (= cell-count 0) () "Cleared grid should have 0 cells")
      (assert (= entity-count 0) () "Cleared grid should have 0 entities"))))

(defun test-zone-transition-grid-update ()
  "Test that zone transitions correctly update spatial grids.
   Player should be removed from old zone grid and inserted into new zone grid."
  (let* (;; Create two zone-states with spatial grids
         (old-zone-state (mmorpg::make-zone-state
                          :zone-id :old-zone
                          :player-grid (mmorpg::make-spatial-grid 128.0)
                          :npc-grid (mmorpg::make-spatial-grid 128.0)))
         (new-zone-state (mmorpg::make-zone-state
                          :zone-id :new-zone
                          :player-grid (mmorpg::make-spatial-grid 128.0)
                          :npc-grid (mmorpg::make-spatial-grid 128.0)))
         ;; Create a player in old zone
         (player (mmorpg::make-player 100.0 100.0 :id 42 :zone-id :old-zone))
         (old-grid (mmorpg::zone-state-player-grid old-zone-state))
         (new-grid (mmorpg::zone-state-player-grid new-zone-state)))
    ;; Insert player into old zone's grid (simulating initial state)
    (multiple-value-bind (cx cy)
        (mmorpg::position-to-cell 100.0 100.0 (mmorpg::spatial-grid-cell-size old-grid))
      (mmorpg::spatial-grid-insert old-grid 42 100.0 100.0)
      (setf (mmorpg::player-grid-cell-x player) cx
            (mmorpg::player-grid-cell-y player) cy))
    ;; Verify player is in old grid
    (multiple-value-bind (cell-count entity-count)
        (mmorpg::spatial-grid-stats old-grid)
      (declare (ignore cell-count))
      (assert (= entity-count 1) () "Player should be in old zone grid"))
    ;; Simulate zone transition: remove from old grid
    (mmorpg::spatial-grid-remove old-grid 42
                                  (mmorpg::player-grid-cell-x player)
                                  (mmorpg::player-grid-cell-y player))
    ;; Update player position and zone
    (setf (mmorpg::player-x player) 200.0
          (mmorpg::player-y player) 200.0
          (mmorpg::player-zone-id player) :new-zone)
    ;; Insert into new grid
    (multiple-value-bind (cx cy)
        (mmorpg::position-to-cell 200.0 200.0 (mmorpg::spatial-grid-cell-size new-grid))
      (mmorpg::spatial-grid-insert new-grid 42 200.0 200.0)
      (setf (mmorpg::player-grid-cell-x player) cx
            (mmorpg::player-grid-cell-y player) cy))
    ;; Verify player is NOT in old grid
    (multiple-value-bind (cell-count entity-count)
        (mmorpg::spatial-grid-stats old-grid)
      (declare (ignore cell-count))
      (assert (= entity-count 0) () "Player should NOT be in old zone grid after transition"))
    ;; Verify player IS in new grid
    (multiple-value-bind (cell-count entity-count)
        (mmorpg::spatial-grid-stats new-grid)
      (declare (ignore cell-count))
      (assert (= entity-count 1) () "Player should be in new zone grid after transition"))
    ;; Verify player's grid cell is updated
    (multiple-value-bind (expected-cx expected-cy)
        (mmorpg::position-to-cell 200.0 200.0 128.0)
      (assert (= (mmorpg::player-grid-cell-x player) expected-cx)
              () "Player grid-cell-x should match new position")
      (assert (= (mmorpg::player-grid-cell-y player) expected-cy)
              () "Player grid-cell-y should match new position"))
    ;; Verify player can be found via spatial query in new zone
    (let ((nearby (mmorpg::spatial-grid-query-neighbors
                   new-grid
                   (mmorpg::player-grid-cell-x player)
                   (mmorpg::player-grid-cell-y player))))
      (assert (member 42 nearby) () "Player should be findable via spatial query in new zone"))))


;;; Zone-Players Cache Tests (Task 4.1)

(defun test-zone-players-cache-add-remove ()
  "Test adding and removing players from zone-players cache."
  (let* ((zone-state (mmorpg::make-zone-state
                      :zone-id :test-zone
                      :player-grid (mmorpg::make-spatial-grid 128.0)
                      :npc-grid (mmorpg::make-spatial-grid 128.0)))
         (player1 (mmorpg::make-player 100.0 100.0 :id 1 :zone-id :test-zone))
         (player2 (mmorpg::make-player 200.0 200.0 :id 2 :zone-id :test-zone))
         (cache (mmorpg::zone-state-zone-players zone-state)))
    ;; Initial cache should be empty
    (assert (= (length cache) 0) () "Cache should start empty")
    ;; Add first player
    (mmorpg::add-player-to-zone-cache player1 zone-state)
    (assert (= (length (mmorpg::zone-state-zone-players zone-state)) 1)
            () "Cache should have 1 player after add")
    ;; Add second player
    (mmorpg::add-player-to-zone-cache player2 zone-state)
    (assert (= (length (mmorpg::zone-state-zone-players zone-state)) 2)
            () "Cache should have 2 players after second add")
    ;; Adding same player should not duplicate (idempotent)
    (mmorpg::add-player-to-zone-cache player1 zone-state)
    (assert (= (length (mmorpg::zone-state-zone-players zone-state)) 2)
            () "Cache should still have 2 players (no duplicates)")
    ;; Remove first player
    (mmorpg::remove-player-from-zone-cache player1 zone-state)
    (assert (= (length (mmorpg::zone-state-zone-players zone-state)) 1)
            () "Cache should have 1 player after remove")
    ;; Verify correct player remains
    (let ((remaining (aref (mmorpg::zone-state-zone-players zone-state) 0)))
      (assert (eq remaining player2) () "Player2 should be the remaining player"))
    ;; Remove second player
    (mmorpg::remove-player-from-zone-cache player2 zone-state)
    (assert (= (length (mmorpg::zone-state-zone-players zone-state)) 0)
            () "Cache should be empty after removing all players")))

(defun test-zone-players-cache-rebuild ()
  "Test rebuilding zone-players cache from game state."
  (let* ((zone-state (mmorpg::make-zone-state
                      :zone-id :test-zone
                      :player-grid (mmorpg::make-spatial-grid 128.0)
                      :npc-grid (mmorpg::make-spatial-grid 128.0)))
         (player1 (mmorpg::make-player 100.0 100.0 :id 1 :zone-id :test-zone))
         (player2 (mmorpg::make-player 200.0 200.0 :id 2 :zone-id :other-zone))
         (player3 (mmorpg::make-player 300.0 300.0 :id 3 :zone-id :test-zone))
         ;; Create minimal game struct with players
         (game (mmorpg::%make-game :players (vector player1 player2 player3))))
    ;; Rebuild the cache
    (mmorpg::rebuild-zone-players-cache zone-state game)
    ;; Should only have players in :test-zone (player1 and player3)
    (let ((cache (mmorpg::zone-state-zone-players zone-state)))
      (assert (= (length cache) 2) () "Cache should have 2 players in test-zone")
      ;; Verify correct players
      (let ((ids (loop :for p :across cache :collect (mmorpg::player-id p))))
        (assert (member 1 ids) () "Player1 should be in cache")
        (assert (not (member 2 ids)) () "Player2 should NOT be in cache (wrong zone)")
        (assert (member 3 ids) () "Player3 should be in cache")))))

(defun test-zone-players-cache-transition ()
  "Test that zone transitions correctly update zone-players caches."
  (let* ((old-zone-state (mmorpg::make-zone-state
                          :zone-id :old-zone
                          :player-grid (mmorpg::make-spatial-grid 128.0)
                          :npc-grid (mmorpg::make-spatial-grid 128.0)))
         (new-zone-state (mmorpg::make-zone-state
                          :zone-id :new-zone
                          :player-grid (mmorpg::make-spatial-grid 128.0)
                          :npc-grid (mmorpg::make-spatial-grid 128.0)))
         (player (mmorpg::make-player 100.0 100.0 :id 42 :zone-id :old-zone)))
    ;; Add player to old zone's cache
    (mmorpg::add-player-to-zone-cache player old-zone-state)
    (assert (= (length (mmorpg::zone-state-zone-players old-zone-state)) 1)
            () "Player should be in old zone cache")
    (assert (= (length (mmorpg::zone-state-zone-players new-zone-state)) 0)
            () "New zone cache should be empty initially")
    ;; Simulate zone transition
    (mmorpg::remove-player-from-zone-cache player old-zone-state)
    (setf (mmorpg::player-zone-id player) :new-zone)
    (mmorpg::add-player-to-zone-cache player new-zone-state)
    ;; Verify caches are correct
    (assert (= (length (mmorpg::zone-state-zone-players old-zone-state)) 0)
            () "Old zone cache should be empty after transition")
    (assert (= (length (mmorpg::zone-state-zone-players new-zone-state)) 1)
            () "New zone cache should have player after transition")
    ;; Verify correct player is in new cache
    (let ((cached (aref (mmorpg::zone-state-zone-players new-zone-state) 0)))
      (assert (eq cached player) () "Cached player should be same object"))))


;;; Vector Pool Tests (Task 4.2)

(defun test-vector-pool-create ()
  "Test vector pool creation."
  (let ((pool (mmorpg::make-vector-pool 10 22)))
    (assert pool () "Pool should be created")
    (assert (= (mmorpg::vector-pool-capacity pool) 10)
            () "Pool capacity should be 10")
    (assert (= (mmorpg::vector-pool-element-size pool) 22)
            () "Element size should be 22")
    (assert (= (mmorpg::vector-pool-index pool) 0)
            () "Initial index should be 0")))

(defun test-vector-pool-acquire ()
  "Test acquiring vectors from pool."
  (let ((pool (mmorpg::make-vector-pool 5 22)))
    ;; Acquire first vector
    (let ((v1 (mmorpg::acquire-pooled-vector pool)))
      (assert (= (length v1) 22) () "Acquired vector should have 22 elements")
      (assert (= (mmorpg::vector-pool-index pool) 1) () "Index should be 1 after first acquire"))
    ;; Acquire second vector
    (let ((v2 (mmorpg::acquire-pooled-vector pool)))
      (assert (= (length v2) 22) () "Second vector should have 22 elements")
      (assert (= (mmorpg::vector-pool-index pool) 2) () "Index should be 2 after second acquire"))
    ;; Vectors should be different objects
    (mmorpg::reset-vector-pool pool)
    (let ((v1 (mmorpg::acquire-pooled-vector pool))
          (v2 (mmorpg::acquire-pooled-vector pool)))
      (assert (not (eq v1 v2)) () "Acquired vectors should be different objects"))))

(defun test-vector-pool-reset ()
  "Test resetting vector pool for reuse."
  (let ((pool (mmorpg::make-vector-pool 5 22)))
    ;; Acquire some vectors
    (mmorpg::acquire-pooled-vector pool)
    (mmorpg::acquire-pooled-vector pool)
    (mmorpg::acquire-pooled-vector pool)
    (assert (= (mmorpg::vector-pool-index pool) 3) () "Index should be 3")
    ;; Reset
    (mmorpg::reset-vector-pool pool)
    (assert (= (mmorpg::vector-pool-index pool) 0) () "Index should be 0 after reset")
    ;; Can acquire again from beginning
    (let ((v (mmorpg::acquire-pooled-vector pool)))
      (assert (= (length v) 22) () "Can acquire after reset")
      (assert (= (mmorpg::vector-pool-index pool) 1) () "Index should be 1"))))

(defun test-vector-pool-overflow ()
  "Test vector pool behavior when exhausted."
  (let ((pool (mmorpg::make-vector-pool 3 22)))
    ;; Exhaust the pool
    (mmorpg::acquire-pooled-vector pool)
    (mmorpg::acquire-pooled-vector pool)
    (mmorpg::acquire-pooled-vector pool)
    (assert (= (mmorpg::vector-pool-index pool) 3) () "Index should be 3 (capacity)")
    ;; Acquiring beyond capacity should still work (creates fresh vector)
    (let ((v (mmorpg::acquire-pooled-vector pool)))
      (assert (= (length v) 22) () "Overflow vector should have 22 elements"))
    ;; Stats should show overflow
    (multiple-value-bind (used total overflow)
        (mmorpg::vector-pool-stats pool)
      (assert (= used 4) () "Used should be 4")
      (assert (= total 3) () "Total should be 3")
      (assert (= overflow 1) () "Overflow should be 1"))))



(defvar *tests-spatial*
  '(;; Spatial Grid Tests
    test-spatial-grid-insert-remove
    test-spatial-grid-move
    test-spatial-grid-query-neighbors
    test-spatial-grid-query-radius
    test-position-to-cell
    test-entity-cell-changed-p
    test-spatial-grid-stats
    test-spatial-grid-clear
    test-zone-transition-grid-update
    ;; Zone-Players Cache Tests (Task 4.1)
    test-zone-players-cache-add-remove
    test-zone-players-cache-rebuild
    test-zone-players-cache-transition
    ;; Vector Pool Tests (Task 4.2)
    test-vector-pool-create
    test-vector-pool-acquire
    test-vector-pool-reset
    test-vector-pool-overflow)
  "Spatial/pool domain test functions.")
