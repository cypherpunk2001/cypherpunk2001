(in-package #:mmorpg)

(defun test-compute-transition-overstep-north ()
  "Overstep for :north edge: distance from player Y to source min-y."
  (let ((player (%make-player)))
    (setf (player-x player) 300.0 (player-y player) 20.0)
    ;; Source zone: min-y=10, max-y=640
    (let ((overstep (compute-transition-overstep :north player 10.0 630.0 10.0 640.0)))
      ;; Player at y=20, north edge at y=10 → overstep = 20 - 10 = 10
      (assert (< (abs (- overstep 10.0)) 0.01) ()
              "overstep-north: should be ~10, got ~,2f" overstep))))

(defun test-compute-transition-overstep-all-edges ()
  "Overstep computation for all 4 cardinal edges."
  (let ((player (%make-player)))
    ;; South edge: player at y=630, south boundary at y=640
    (setf (player-x player) 300.0 (player-y player) 630.0)
    (let ((os (compute-transition-overstep :south player 10.0 630.0 10.0 640.0)))
      (assert (< (abs (- os 10.0)) 0.01) ()
              "overstep-south: should be ~10, got ~,2f" os))
    ;; West edge: player at x=15, west boundary at x=10
    (setf (player-x player) 15.0 (player-y player) 300.0)
    (let ((os (compute-transition-overstep :west player 10.0 630.0 10.0 640.0)))
      (assert (< (abs (- os 5.0)) 0.01) ()
              "overstep-west: should be ~5, got ~,2f" os))
    ;; East edge: player at x=625, east boundary at x=630
    (setf (player-x player) 625.0)
    (let ((os (compute-transition-overstep :east player 10.0 630.0 10.0 640.0)))
      (assert (< (abs (- os 5.0)) 0.01) ()
              "overstep-east: should be ~5, got ~,2f" os))))

(defun test-apply-overstep-to-spawn-directions ()
  "apply-overstep-to-spawn should shift spawn position inward from spawn edge."
  ;; Spawn at south edge (y=640), overstep=10 → should push up to y=630
  (multiple-value-bind (x y) (apply-overstep-to-spawn :south 10.0 300.0 640.0)
    (assert (< (abs (- y 630.0)) 0.01) ()
            "overstep-spawn-south: y should be ~630, got ~,2f" y)
    (assert (< (abs (- x 300.0)) 0.01) ()
            "overstep-spawn-south: x should be unchanged"))
  ;; Spawn at north edge (y=10), overstep=5 → should push down to y=15
  (multiple-value-bind (x y) (apply-overstep-to-spawn :north 5.0 300.0 10.0)
    (assert (< (abs (- y 15.0)) 0.01) ()
            "overstep-spawn-north: y should be ~15, got ~,2f" y)
    (assert (< (abs (- x 300.0)) 0.01) ()
            "overstep-spawn-north: x should be unchanged"))
  ;; Spawn at east edge (x=630), overstep=8 → should push left to x=622
  (multiple-value-bind (x y) (apply-overstep-to-spawn :east 8.0 630.0 300.0)
    (assert (< (abs (- x 622.0)) 0.01) ()
            "overstep-spawn-east: x should be ~622, got ~,2f" x))
  ;; Spawn at west edge (x=10), overstep=3 → should push right to x=13
  (multiple-value-bind (x y) (apply-overstep-to-spawn :west 3.0 10.0 300.0)
    (assert (< (abs (- x 13.0)) 0.01) ()
            "overstep-spawn-west: x should be ~13, got ~,2f" x)))

;;; ============================================================
;;; ADDENDUM 2: Commit margin tied to collision size
;;; ============================================================

(defun test-commit-margin-at-least-collision-half ()
  "Commit margin must be at least max(collision-half-width, collision-half-height).
   ADDENDUM 2: Ensures the player can trigger commit without pushing into wall."
  ;; Scenario 1: Large collision (e.g. mount) exceeds tile-based margin
  (let* ((tile-size 32.0)
         (half-w 20.0)   ; wider than 0.5 * tile-size = 16
         (half-h 24.0)   ; taller still
         (margin (max (* *zone-commit-margin-tiles* tile-size)
                      (max half-w half-h))))
    (assert (>= margin half-w) ()
            "commit-margin ~,1f must be >= collision-half-width ~,1f" margin half-w)
    (assert (>= margin half-h) ()
            "commit-margin ~,1f must be >= collision-half-height ~,1f" margin half-h)
    ;; For this case, collision-based margin should win
    (assert (= margin 24.0) ()
            "commit-margin should be 24.0 (collision wins), got ~,1f" margin))
  ;; Scenario 2: Small collision — tile-based margin wins
  (let* ((tile-size 32.0)
         (half-w 8.0)    ; smaller than 0.5 * 32 = 16
         (half-h 8.0)
         (margin (max (* *zone-commit-margin-tiles* tile-size)
                      (max half-w half-h))))
    (assert (>= margin half-w) ()
            "commit-margin ~,1f must be >= collision-half-width ~,1f" margin half-w)
    (assert (= margin (* *zone-commit-margin-tiles* tile-size)) ()
            "commit-margin should be tile-based ~,1f, got ~,1f"
            (* *zone-commit-margin-tiles* tile-size) margin)))

;;; ============================================================
;;; ADDENDUM 3: Urgent preload near commit
;;; ============================================================

(defun test-process-preload-queue-urgent-pops-multiple ()
  "process-preload-queue with :count > 1 should pop multiple entries."
  (let* ((cache (make-zone-lru-cache))
         (zone-dir (namestring
                    (merge-pathnames "data/zones/"
                                     (asdf:system-source-directory :mmorpg))))
         (path-5 (concatenate 'string zone-dir "zone-5.lisp"))
         (path-6 (concatenate 'string zone-dir "zone-6.lisp"))
         (game (%make-game :zone-cache cache
                           :preload-queue (list (cons :zone-5 path-5)
                                                (cons :zone-6 path-6)))))
    ;; Queue has 2 entries
    (assert (= (length (game-preload-queue game)) 2) ()
            "urgent-preload: queue should start with 2 entries")
    ;; Urgent mode: pop all at once
    (process-preload-queue game :count 10)
    ;; Queue should be fully drained
    (assert (null (game-preload-queue game)) ()
            "urgent-preload: queue should be empty after urgent pop")
    ;; Both zones should be cached
    (assert (zone-cache-contains-p cache :zone-5) ()
            "urgent-preload: :zone-5 should be in cache")
    (assert (zone-cache-contains-p cache :zone-6) ()
            "urgent-preload: :zone-6 should be in cache")))

(defun test-urgent-preload-no-pending-required ()
  "Urgent preload detection should use proximity only, not player-zone-transition-pending.
   Network clients never have pending set (server-only state), so the urgent path
   must trigger based solely on distance to zone edge."
  (let* ((player (%make-player))
         ;; Zone with known bounds: 0-640 in both axes (20 tiles at 32px)
         (zone (%make-zone :id :test-zone :width 20 :height 20))
         (world (%make-world :zone zone
                             :tile-dest-size 32.0
                             :collision-half-width 12.0
                             :collision-half-height 12.0
                             ;; Fallback bounds (no zone wall map for :test-zone)
                             :wall-min-x 12.0
                             :wall-max-x 628.0
                             :wall-min-y 12.0
                             :wall-max-y 628.0)))
    ;; Player near the north edge (y=20, which is 8px from wall-min-y=12)
    (setf (player-x player) 300.0 (player-y player) 20.0)
    ;; pending is nil (simulating network client)
    (assert (null (player-zone-transition-pending player)) ()
            "urgent-no-pending: pending should be nil (network client)")
    ;; urgent-px = 2 tiles * 32 = 64px. Player is 8px from edge < 64px → should detect
    (let ((urgent-px (* *zone-urgent-preload-tiles* 32.0)))
      (assert (player-within-urgent-preload-distance-p player world urgent-px) ()
              "urgent-no-pending: should detect proximity without pending flag"))
    ;; Player far from any edge (center of zone)
    (setf (player-x player) 320.0 (player-y player) 320.0)
    (let ((urgent-px (* *zone-urgent-preload-tiles* 32.0)))
      (assert (not (player-within-urgent-preload-distance-p player world urgent-px)) ()
              "urgent-no-pending: should NOT detect when player is in center"))))

(defun test-apply-game-state-cache-miss-warns ()
  "apply-game-state should emit a warning and drop the entire snapshot when zone
   cache misses on a client transition. Dropping prevents desync — applying new-zone
   positions into the old zone would leave the client in an inconsistent state."
  (let* ((player (%make-player))
         (zone-dir (namestring
                    (merge-pathnames "data/zones/"
                                     (asdf:system-source-directory :mmorpg))))
         (zone-5-path (concatenate 'string zone-dir "zone-5.lisp"))
         ;; Start in zone-5 so we can transition to zone-6
         (zone-5 (load-zone zone-5-path))
         (world (%make-world :zone zone-5
                             :tile-dest-size 32.0
                             :collision-half-width 12.0
                             :collision-half-height 12.0
                             :wall-min-x 12.0 :wall-max-x 628.0
                             :wall-min-y 12.0 :wall-max-y 628.0
                             :world-graph (load-world-graph)))
         ;; Empty LRU cache — will cause a cache miss
         (cache (make-zone-lru-cache))
         (game (%make-game :player player
                           :players (make-array 1 :initial-contents (list player))
                           :world world
                           :zone-cache cache
                           :net-role :client)))
    (setf (player-zone-id player) :zone-5)
    (setf (player-x player) 100.0 (player-y player) 200.0)
    ;; Apply state with zone-id :zone-6 — cache miss should warn and drop snapshot
    (let ((warned nil))
      (handler-bind ((warning (lambda (w)
                                (when (search "cache MISS" (format nil "~a" w))
                                  (setf warned t)
                                  (muffle-warning w)))))
        (multiple-value-bind (zone-id zone-loaded)
            (apply-game-state game (list :zone-id :zone-6 :x 999.0 :y 999.0) :apply-zone t)
          (declare (ignore zone-id))
          (assert warned ()
                  "cache-miss-warn: should have emitted a warning on zone cache miss")
          (assert (not zone-loaded) ()
                  "cache-miss-warn: zone-loaded should be nil on cache miss")
          ;; Snapshot must be dropped — player position must NOT be updated to new-zone coords
          (assert (< (abs (- (player-x player) 100.0)) 0.01) ()
                  "cache-miss-warn: player-x should be unchanged (snapshot dropped)")
          (assert (< (abs (- (player-y player) 200.0)) 0.01) ()
                  "cache-miss-warn: player-y should be unchanged (snapshot dropped)")
          ;; World should still have the old zone loaded
          (assert (eq (zone-id (world-zone world)) :zone-5) ()
                  "cache-miss-warn: world zone should still be zone-5"))))))

;;; ============================================================
;;; ADDENDUM 4: Soft interpolation/prediction reset
;;; ============================================================

(defun test-soft-reset-preserves-buffers-small-delta ()
  "handle-zone-transition with small position delta should preserve buffers.
   ADDENDUM 4: Seamless walk-through transitions keep interpolation continuity."
  (let* ((player (%make-player))
         (buffer (make-interpolation-buffer))
         (pred (%make-prediction-state
                :predicted-x 100.0 :predicted-y 100.0
                :inputs (make-array 32) :input-capacity 32
                :input-count 5 :input-head 3
                :input-sequence 0 :last-acked-sequence 0
                :misprediction-count 0))
         (game (%make-game :player player
                           :interpolation-buffer buffer
                           :prediction-state pred)))
    ;; Player starts at 100,100 — zone transition moves to 105,100 (small delta)
    (setf (player-x player) 105.0 (player-y player) 100.0)
    ;; Manually push a snapshot so buffer has data
    (push-interpolation-snapshot buffer
                                 (%make-interpolation-snapshot
                                  :timestamp 0.0
                                  :entity-positions (make-hash-table)))
    (assert (= (interpolation-buffer-count buffer) 1) ()
            "soft-reset: buffer should have 1 snapshot before transition")
    ;; Small delta: 100,100 → 105,100 = 25 sq < 1024 threshold
    (handle-zone-transition game :old-x 100.0f0 :old-y 100.0f0)
    ;; Buffer should be preserved (not cleared)
    (assert (= (interpolation-buffer-count buffer) 1) ()
            "soft-reset: buffer should still have 1 snapshot (small delta)")
    ;; Prediction input state should be preserved
    (assert (= (prediction-state-input-count pred) 5) ()
            "soft-reset: prediction input-count should be preserved")
    ;; But prediction position should still be updated to new coords
    (assert (< (abs (- (prediction-state-predicted-x pred) 105.0)) 0.01) ()
            "soft-reset: predicted-x should be updated to new position")))

(defun test-soft-reset-clears-buffers-large-delta ()
  "handle-zone-transition with large position delta should clear buffers.
   ADDENDUM 4: Teleport-like transitions (login, /unstuck) need full reset."
  (let* ((player (%make-player))
         (buffer (make-interpolation-buffer))
         (pred (%make-prediction-state
                :predicted-x 100.0 :predicted-y 100.0
                :inputs (make-array 32) :input-capacity 32
                :input-count 5 :input-head 3
                :input-sequence 0 :last-acked-sequence 0
                :misprediction-count 0))
         (game (%make-game :player player
                           :interpolation-buffer buffer
                           :prediction-state pred)))
    ;; Player jumps from 100,100 to 500,100 (large delta = 160000 sq >> 1024)
    (setf (player-x player) 500.0 (player-y player) 100.0)
    ;; Push a snapshot so buffer has data
    (push-interpolation-snapshot buffer
                                 (%make-interpolation-snapshot
                                  :timestamp 0.0
                                  :entity-positions (make-hash-table)))
    (assert (= (interpolation-buffer-count buffer) 1) ()
            "hard-reset: buffer should have 1 snapshot before transition")
    ;; Large delta: 100,100 → 500,100 = 160000 sq >> 1024 threshold
    (handle-zone-transition game :old-x 100.0f0 :old-y 100.0f0)
    ;; Buffer should be cleared
    (assert (= (interpolation-buffer-count buffer) 0) ()
            "hard-reset: buffer should be cleared (large delta)")
    ;; Prediction input state should be reset
    (assert (= (prediction-state-input-count pred) 0) ()
            "hard-reset: prediction input-count should be 0")
    ;; Prediction position should be updated to new coords
    (assert (< (abs (- (prediction-state-predicted-x pred) 500.0)) 0.01) ()
            "hard-reset: predicted-x should be updated to new position")))

;;; ============================================================
;;; Zone Transition Continuity — Seam Translation Tests
;;; ============================================================

(defun test-seam-translate-position-east ()
  "Crossing east: new-x = dst-min-x + (px - src-max-x), new-y unchanged.
   Uses collision bounds, not pixel spans."
  ;; Zone: 64 tiles * 64px, half-w=16. Collision bounds: min=80, max=4016.
  ;; Player at x=4020 (4px past src-max-x=4016). Dest same bounds.
  ;; new-x = 80 + (4020 - 4016) = 84. Y unchanged.
  (multiple-value-bind (nx ny)
      (seam-translate-position :east 4020.0 900.0
                               80.0 4016.0 80.0 4016.0   ; src bounds
                               80.0 4016.0 80.0 4016.0)  ; dst bounds
    (assert (< (abs (- nx 84.0)) 0.01) ()
            "seam-east: new-x should be 84, got ~,2f" nx)
    (assert (< (abs (- ny 900.0)) 0.01) ()
            "seam-east: new-y should be unchanged at 900, got ~,2f" ny)))

(defun test-seam-translate-position-west ()
  "Crossing west: new-x = dst-max-x + (px - src-min-x), new-y unchanged."
  ;; Player at x=76 (4px past src-min-x=80 toward west).
  ;; new-x = 4016 + (76 - 80) = 4012. Y unchanged.
  (multiple-value-bind (nx ny)
      (seam-translate-position :west 76.0 500.0
                               80.0 4016.0 80.0 4016.0
                               80.0 4016.0 80.0 4016.0)
    (assert (< (abs (- nx 4012.0)) 0.01) ()
            "seam-west: new-x should be 4012, got ~,2f" nx)
    (assert (< (abs (- ny 500.0)) 0.01) ()
            "seam-west: new-y should be unchanged at 500, got ~,2f" ny)))

(defun test-seam-translate-position-north ()
  "Crossing north: new-y = dst-max-y + (py - src-min-y), new-x unchanged."
  ;; Player at y=75 (5px past src-min-y=80 toward north).
  ;; new-y = 4016 + (75 - 80) = 4011. X unchanged.
  (multiple-value-bind (nx ny)
      (seam-translate-position :north 300.0 75.0
                               80.0 4016.0 80.0 4016.0
                               80.0 4016.0 80.0 4016.0)
    (assert (< (abs (- nx 300.0)) 0.01) ()
            "seam-north: new-x should be unchanged at 300, got ~,2f" nx)
    (assert (< (abs (- ny 4011.0)) 0.01) ()
            "seam-north: new-y should be 4011, got ~,2f" ny)))

(defun test-seam-translate-position-south ()
  "Crossing south: new-y = dst-min-y + (py - src-max-y), new-x unchanged."
  ;; Player at y=4020 (4px past src-max-y=4016 toward south).
  ;; new-y = 80 + (4020 - 4016) = 84. X unchanged.
  (multiple-value-bind (nx ny)
      (seam-translate-position :south 300.0 4020.0
                               80.0 4016.0 80.0 4016.0
                               80.0 4016.0 80.0 4016.0)
    (assert (< (abs (- nx 300.0)) 0.01) ()
            "seam-south: new-x should be unchanged at 300, got ~,2f" nx)
    (assert (< (abs (- ny 84.0)) 0.01) ()
            "seam-south: new-y should be 84, got ~,2f" ny)))

(defun test-seam-translate-position-corner ()
  "Corner crossing: translation uses only the crossing edge axis."
  ;; Crossing east at a corner: only x changes, y stays near south edge.
  ;; Player at x=4020 (4px past src-max-x), y=4010 (near south edge).
  ;; new-x = 80 + (4020 - 4016) = 84. Y unchanged at 4010.
  (multiple-value-bind (nx ny)
      (seam-translate-position :east 4020.0 4010.0
                               80.0 4016.0 80.0 4016.0
                               80.0 4016.0 80.0 4016.0)
    (assert (< (abs (- nx 84.0)) 0.01) ()
            "seam-corner-east: new-x should be 84, got ~,2f" nx)
    (assert (< (abs (- ny 4010.0)) 0.01) ()
            "seam-corner-east: new-y should be unchanged at 4010, got ~,2f" ny)))

(defun test-seam-position-valid-p ()
  "seam-position-valid-p should correctly check destination bounds."
  ;; Inside bounds
  (assert (seam-position-valid-p 100.0 200.0 10.0 500.0 10.0 500.0) ()
          "seam-valid: (100,200) should be inside (10-500, 10-500)")
  ;; On boundary (exact)
  (assert (seam-position-valid-p 10.0 10.0 10.0 500.0 10.0 500.0) ()
          "seam-valid: exact min boundary should be valid")
  (assert (seam-position-valid-p 500.0 500.0 10.0 500.0 10.0 500.0) ()
          "seam-valid: exact max boundary should be valid")
  ;; Outside bounds
  (assert (not (seam-position-valid-p 5.0 200.0 10.0 500.0 10.0 500.0)) ()
          "seam-valid: x=5 below min-x=10 should be invalid")
  (assert (not (seam-position-valid-p 100.0 505.0 10.0 500.0 10.0 500.0)) ()
          "seam-valid: y=505 above max-y=500 should be invalid"))

(defun test-seam-translation-used-in-transition-zone ()
  "Integration: transition-zone with seam translation produces position near seam.
   Verifies that after crossing east, the player appears near x=0 of destination zone
   (not teleported to an edge-spawn position)."
  (let* ((world (make-test-world :tile-size 64.0 :collision-half 16.0))
         (zone-a-id :zone-a)
         (zone-b-id :zone-b)
         ;; Create a simple 10x10 zone with no walls
         (wall-map (make-array '(10 10) :initial-element 0))
         ;; Zone B struct
         (zone-b (%make-zone :id zone-b-id :width 10 :height 10
                             :collision-tiles nil :objects nil))
         (player (%make-player))
         (game (%make-game :world world :players (vector player)
                           :npcs (vector) :entities (vector player)
                           :net-role :server
                           :npc-id-source (make-id-source 1000000 nil)))
         (exit (list :to zone-b-id :spawn-edge :west)))
    ;; Setup source zone (zone-a) state
    (setf (gethash zone-a-id *zone-states*)
          (make-zone-state :zone-id zone-a-id
                           :wall-map wall-map
                           :zone (%make-zone :id zone-a-id :width 10 :height 10
                                             :collision-tiles nil :objects nil)))
    ;; Setup destination zone (zone-b) state
    (setf (gethash zone-b-id *zone-states*)
          (make-zone-state :zone-id zone-b-id
                           :wall-map wall-map
                           :zone zone-b
                           :player-grid (make-spatial-grid-for-zone 10 10 64.0)
                           :npc-grid (make-spatial-grid-for-zone 10 10 64.0)))
    ;; Place player just past east collision bound of zone-a
    ;; 10-tile zone with tile-size=64, half-w=16:
    ;;   collision min-x=80 (1*64+16), max-x=560 (9*64-16)
    ;; Player at x=562 (2px past src-max-x=560)
    (setf (player-x player) 562.0
          (player-y player) 300.0
          (player-zone-id player) zone-a-id
          (player-intent player) (make-intent))
    ;; Set up world bounds matching 10-tile zone collision
    (setf (world-wall-min-x world) 80.0
          (world-wall-max-x world) 560.0
          (world-wall-min-y world) 80.0
          (world-wall-max-y world) 560.0
          (world-zone-label world) "Zone A")
    ;; Write zone-b to a temp file so transition-zone can load it
    (let ((tmp-path (format nil "/tmp/test-zone-b-~a.lisp" (get-universal-time))))
      (with-open-file (out tmp-path :direction :output :if-exists :supersede)
        (write (list :id zone-b-id :width 10 :height 10
                     :tile-layers nil :collision-tiles nil :objects nil)
               :stream out))
      (let* ((paths (make-hash-table :test 'eq))
             (graph (%make-world-graph :edges-by-zone (make-hash-table :test 'eq)
                                       :zone-paths paths)))
        (setf (gethash zone-b-id paths) tmp-path)
        (setf (world-world-graph world) graph))
      (unwind-protect
           (progn
             (transition-zone game player exit :east)
             ;; After east crossing: new-x = dst-min-x + (562 - src-max-x)
             ;;   = 80 + (562 - 560) = 82. Y unchanged at 300.
             ;; This is seamless: only 2px from the destination edge.
             (assert (< (abs (- (player-x player) 82.0)) 1.0) ()
                     "seam-integration: player-x should be near 82, got ~,2f"
                     (player-x player))
             ;; Y-axis must be preserved exactly (not ratio-mapped to 0.5)
             (assert (< (abs (- (player-y player) 300.0)) 1.0) ()
                     "seam-integration: player-y should be unchanged at 300, got ~,2f"
                     (player-y player))
             (assert (eq (player-zone-id player) zone-b-id) ()
                     "seam-integration: player should be in zone-b"))
        (delete-file tmp-path)))))

(defun test-seam-translation-blocked-uses-fallback ()
  "When seam-translated+clamped position is blocked by a wall, fallback ratio-spawn is used."
  (let* ((world (make-test-world :tile-size 64.0 :collision-half 16.0))
         (zone-a-id :zone-blk-a)
         (zone-b-id :zone-blk-b)
         ;; Zone B has a wall across the entire west edge (column 1)
         ;; so clamped seam position (80, y) will be blocked
         (wall-map-b (make-array '(10 10) :initial-element 0))
         (zone-b (%make-zone :id zone-b-id :width 10 :height 10
                              :collision-tiles nil :objects nil))
         (player (%make-player))
         (game (%make-game :world world :players (vector player)
                           :npcs (vector) :entities (vector player)
                           :net-role :server
                           :npc-id-source (make-id-source 1000000 nil)))
         (exit (list :to zone-b-id :spawn-edge :west)))
    ;; Block column 1 (the west spawn edge area) in zone-b
    (loop :for row :from 0 :below 10
          :do (setf (aref wall-map-b row 1) 1))
    ;; Source zone wall-map (no walls)
    (let ((wall-map-a (make-array '(10 10) :initial-element 0)))
      (setf (gethash zone-a-id *zone-states*)
            (make-zone-state :zone-id zone-a-id
                             :wall-map wall-map-a
                             :zone (%make-zone :id zone-a-id :width 10 :height 10
                                               :collision-tiles nil :objects nil)))
      (setf (gethash zone-b-id *zone-states*)
            (make-zone-state :zone-id zone-b-id
                             :wall-map wall-map-b
                             :zone zone-b
                             :player-grid (make-spatial-grid-for-zone 10 10 64.0)
                             :npc-grid (make-spatial-grid-for-zone 10 10 64.0)))
      ;; Player just past east collision bound
      (setf (player-x player) 562.0
            (player-y player) 300.0
            (player-zone-id player) zone-a-id
            (player-intent player) (make-intent))
      (setf (world-wall-min-x world) 80.0
            (world-wall-max-x world) 560.0
            (world-wall-min-y world) 80.0
            (world-wall-max-y world) 560.0
            (world-zone-label world) "Zone A")
      (let ((tmp-path (format nil "/tmp/test-zone-blk-~a.lisp" (get-universal-time))))
        (with-open-file (out tmp-path :direction :output :if-exists :supersede)
          (write (list :id zone-b-id :width 10 :height 10
                       :tile-layers nil :collision-tiles nil :objects nil)
                 :stream out))
        (let* ((paths (make-hash-table :test 'eq))
               (graph (%make-world-graph :edges-by-zone (make-hash-table :test 'eq)
                                         :zone-paths paths)))
          (setf (gethash zone-b-id paths) tmp-path)
          (setf (world-world-graph world) graph))
        (unwind-protect
             (progn
               (transition-zone game player exit :east)
               ;; Seam translation gives clamped (80, 300) which is blocked → fallback
               ;; Fallback uses find-open-position-with-map spiral search
               ;; Player should end up in zone-b at a non-blocked position
               (assert (eq (player-zone-id player) zone-b-id) ()
                       "seam-blocked: player should be in zone-b")
               ;; Position should be positive (within zone pixel space)
               (assert (> (player-x player) 0.0) ()
                       "seam-blocked: player-x should be positive, got ~,2f"
                       (player-x player))
               ;; Should NOT be at the blocked column 1 (tile center = 96px for 64px tiles)
               ;; Column 1 tile center = 1*64 + 32 = 96
               (assert (not (and (>= (player-x player) 64.0)
                                 (<= (player-x player) 128.0)
                                 (= (aref wall-map-b
                                          (floor (player-y player) 64.0)
                                          (floor (player-x player) 64.0))
                                    1)))
                       () "seam-blocked: player should not be on a blocked tile"))
          (delete-file tmp-path))))))

(defun test-seam-translate-mixed-bounds ()
  "Seam translation with different-sized source and destination zones.
   Crossing east from a small zone (10 tiles) into a large zone (20 tiles).
   Overstep distance should be preserved regardless of zone size mismatch."
  ;; Source 10-tile zone: collision bounds min=80, max=560 (tile-size=64, half-w=16)
  ;; Dest 20-tile zone: collision bounds min=80, max=1200 (19*64-16=1200)
  ;; Player at x=563 (3px past src-max-x=560)
  ;; new-x = dst-min-x + (px - src-max-x) = 80 + (563 - 560) = 83
  (multiple-value-bind (nx ny)
      (seam-translate-position :east 563.0 400.0
                               80.0 560.0 80.0 560.0    ; src (10-tile)
                               80.0 1200.0 80.0 1200.0)  ; dst (20-tile)
    (assert (< (abs (- nx 83.0)) 0.01) ()
            "seam-mixed-east: new-x should be 83, got ~,2f" nx)
    (assert (< (abs (- ny 400.0)) 0.01) ()
            "seam-mixed-east: new-y should be unchanged at 400, got ~,2f" ny))
  ;; Crossing west from large zone (20 tiles) into small zone (10 tiles)
  ;; Player at x=78 (2px past src-min-x=80 toward west)
  ;; new-x = dst-max-x + (px - src-min-x) = 560 + (78 - 80) = 558
  (multiple-value-bind (nx ny)
      (seam-translate-position :west 78.0 400.0
                               80.0 1200.0 80.0 1200.0   ; src (20-tile)
                               80.0 560.0 80.0 560.0)    ; dst (10-tile)
    (assert (< (abs (- nx 558.0)) 0.01) ()
            "seam-mixed-west: new-x should be 558, got ~,2f" nx)
    (assert (< (abs (- ny 400.0)) 0.01) ()
            "seam-mixed-west: new-y should be unchanged at 400, got ~,2f" ny)))


(defparameter *tests-zone-continuity* (list
    ;; ADDENDUM 1: Overstep preservation tests
    'test-compute-transition-overstep-north
    'test-compute-transition-overstep-all-edges
    'test-apply-overstep-to-spawn-directions
    ;; ADDENDUM 2: Commit margin tied to collision size
    'test-commit-margin-at-least-collision-half
    ;; ADDENDUM 3: Urgent preload near commit
    'test-process-preload-queue-urgent-pops-multiple
    'test-urgent-preload-no-pending-required
    ;; Sync load fallback warning
    'test-apply-game-state-cache-miss-warns
    ;; ADDENDUM 4: Soft interpolation/prediction reset
    'test-soft-reset-preserves-buffers-small-delta
    'test-soft-reset-clears-buffers-large-delta
    ;; Zone transition continuity (seam translation)
    'test-seam-translate-position-east
    'test-seam-translate-position-west
    'test-seam-translate-position-north
    'test-seam-translate-position-south
    'test-seam-translate-position-corner
    'test-seam-position-valid-p
    'test-seam-translation-used-in-transition-zone
    'test-seam-translation-blocked-uses-fallback
    'test-seam-translate-mixed-bounds))
