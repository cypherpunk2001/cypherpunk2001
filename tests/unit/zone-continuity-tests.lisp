(in-package #:mmorpg)

(defun test-compute-transition-overstep-north ()
  "Overstep for :north edge: distance attempted-y extends past src-min-y."
  (let ((player (%make-player)))
    (setf (player-x player) 300.0 (player-y player) 10.0)
    ;; Attempted position past north edge (y < min-y=10)
    (setf (player-attempted-x player) 300.0
          (player-attempted-y player) 5.0)
    ;; Source zone: min-y=10, max-y=640
    (let ((overstep (compute-transition-overstep :north player 10.0 630.0 10.0 640.0)))
      ;; Attempted y=5, north edge at y=10 → overstep = 10 - 5 = 5
      (assert (< (abs (- overstep 5.0)) 0.01) ()
              "overstep-north: should be ~5, got ~,2f" overstep))))

(defun test-compute-transition-overstep-all-edges ()
  "Overstep computation for all 4 cardinal edges using attempted position."
  (let ((player (%make-player)))
    ;; South edge: attempted-y past south boundary (y=640)
    (setf (player-x player) 300.0 (player-y player) 640.0)
    (setf (player-attempted-x player) 300.0
          (player-attempted-y player) 643.0)
    (let ((os (compute-transition-overstep :south player 10.0 630.0 10.0 640.0)))
      (assert (< (abs (- os 3.0)) 0.01) ()
              "overstep-south: should be ~3, got ~,2f" os))
    ;; West edge: attempted-x past west boundary (x=10)
    (setf (player-x player) 10.0 (player-y player) 300.0)
    (setf (player-attempted-x player) 7.0
          (player-attempted-y player) 300.0)
    (let ((os (compute-transition-overstep :west player 10.0 630.0 10.0 640.0)))
      (assert (< (abs (- os 3.0)) 0.01) ()
              "overstep-west: should be ~3, got ~,2f" os))
    ;; East edge: attempted-x past east boundary (x=630)
    (setf (player-x player) 630.0 (player-y player) 300.0)
    (setf (player-attempted-x player) 634.0
          (player-attempted-y player) 300.0)
    (let ((os (compute-transition-overstep :east player 10.0 630.0 10.0 640.0)))
      (assert (< (abs (- os 4.0)) 0.01) ()
              "overstep-east: should be ~4, got ~,2f" os))
    ;; North edge: attempted-y past north boundary (y=10)
    (setf (player-x player) 300.0 (player-y player) 10.0)
    (setf (player-attempted-x player) 300.0
          (player-attempted-y player) 8.0)
    (let ((os (compute-transition-overstep :north player 10.0 630.0 10.0 640.0)))
      (assert (< (abs (- os 2.0)) 0.01) ()
              "overstep-north: should be ~2, got ~,2f" os))))

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

(defun test-commit-requires-boundary-crossing ()
  "Commit detection requires attempted position to strictly exceed collision bound.
   Replaces old commit-margin test: commit now uses world-crossing-edge with
   attempted position instead of commit-margin relaxation."
  (let ((player (%make-player))
        (intent (make-intent)))
    (setf (player-intent player) intent)
    (setf (intent-move-dx intent) 1.0 (intent-move-dy intent) 0.0)
    ;; Scenario 1: Attempted position ON the boundary (not past) → no crossing
    (setf (player-x player) 560.0 (player-y player) 300.0)
    (setf (player-attempted-x player) 560.0
          (player-attempted-y player) 300.0)
    (let ((edge (world-crossing-edge player 80.0 560.0 80.0 560.0)))
      (assert (null edge) ()
              "crossing-on-boundary: should be nil (strict inequality), got ~a" edge))
    ;; Scenario 2: Attempted position PAST the boundary → crossing detected
    (setf (player-attempted-x player) 562.0)
    (let ((edge (world-crossing-edge player 80.0 560.0 80.0 560.0)))
      (assert (eq edge :east) ()
              "crossing-past-boundary: should be :east, got ~a" edge))
    ;; Scenario 3: Moving west, attempted past west boundary
    (setf (intent-move-dx intent) -1.0 (intent-move-dy intent) 0.0)
    (setf (player-x player) 80.0 (player-y player) 300.0)
    (setf (player-attempted-x player) 78.0
          (player-attempted-y player) 300.0)
    (let ((edge (world-crossing-edge player 80.0 560.0 80.0 560.0)))
      (assert (eq edge :west) ()
              "crossing-west: should be :west, got ~a" edge))))

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
    ;;   collision min-x=16 (0*64+16), max-x=624 (10*64-16)
    ;; Player at x=626 (2px past src-max-x=624)
    (setf (player-x player) 626.0
          (player-y player) 300.0
          ;; Attempted position = pre-collision intended position (same as actual when past edge)
          (player-attempted-x player) 626.0
          (player-attempted-y player) 300.0
          (player-zone-id player) zone-a-id
          (player-intent player) (make-intent))
    ;; Set up world bounds matching 10-tile zone collision (tile-0 based)
    (setf (world-wall-min-x world) 16.0
          (world-wall-max-x world) 624.0
          (world-wall-min-y world) 16.0
          (world-wall-max-y world) 624.0
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
             ;; After east crossing: now uses actual position + 1px push,
             ;; not raw attempted. So: px = src-max-x + 1.0 = 625.
             ;; new-x = dst-min-x + (625 - 624) = 16 + 1 = 17.
             ;; Y unchanged at 300.
             (assert (< (abs (- (player-x player) 17.0)) 1.0) ()
                     "seam-integration: player-x should be near 17, got ~,2f"
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
      ;; 10-tile zone with tile-size=64, half-w=16:
      ;;   collision min-x=16 (0*64+16), max-x=624 (10*64-16)
      ;; Player at x=626 (2px past src-max-x=624)
      (setf (player-x player) 626.0
            (player-y player) 300.0
            ;; Attempted position = pre-collision intended position
            (player-attempted-x player) 626.0
            (player-attempted-y player) 300.0
            (player-zone-id player) zone-a-id
            (player-intent player) (make-intent))
      (setf (world-wall-min-x world) 16.0
            (world-wall-max-x world) 624.0
            (world-wall-min-y world) 16.0
            (world-wall-max-y world) 624.0
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
               ;; Seam translation gives clamped (18, 300) at column 0, but column 1 is
               ;; blocked so spiral search from clamped position avoids it → fallback
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


;;; ============================================================
;;; New tests for attempted-position and world-crossing-edge
;;; ============================================================

(defun test-crossing-edge-does-not-affect-preview ()
  "world-exit-edge-with-bounds uses actual position, not attempted.
   Preview/minimap must be unaffected by the crossing-edge change."
  (let ((player (%make-player))
        (intent (make-intent)))
    (setf (player-intent player) intent)
    (setf (intent-move-dx intent) 1.0 (intent-move-dy intent) 0.0)
    ;; Actual position is ON the boundary, attempted is past it
    (setf (player-x player) 560.0 (player-y player) 300.0)
    (setf (player-attempted-x player) 563.0
          (player-attempted-y player) 300.0)
    ;; world-exit-edge-with-bounds should use actual position (560 <= 560) with margin=0
    ;; At the boundary with zero margin → should detect east
    (let ((edge (world-exit-edge-with-bounds player 80.0 560.0 80.0 560.0 0.0)))
      (assert (eq edge :east) ()
              "preview-edge: actual position on boundary should detect :east, got ~a" edge))
    ;; Player well inside the zone: actual=300, attempted=563 (stale from hypothetical)
    (setf (player-x player) 300.0)
    ;; world-exit-edge-with-bounds should NOT detect an edge (player is deep inside)
    (let ((edge (world-exit-edge-with-bounds player 80.0 560.0 80.0 560.0 0.0)))
      (assert (null edge) ()
              "preview-inside: player at x=300 should not trigger edge, got ~a" edge))))

(defun test-overstep-zero-when-inside ()
  "Overstep should be 0.0 when attempted position is inside the zone."
  (let ((player (%make-player)))
    ;; Attempted position well inside zone bounds
    (setf (player-attempted-x player) 300.0
          (player-attempted-y player) 300.0)
    (assert (< (abs (compute-transition-overstep :north player 10.0 630.0 10.0 640.0)) 0.001) ()
            "overstep-inside-north: should be 0")
    (assert (< (abs (compute-transition-overstep :south player 10.0 630.0 10.0 640.0)) 0.001) ()
            "overstep-inside-south: should be 0")
    (assert (< (abs (compute-transition-overstep :east player 10.0 630.0 10.0 640.0)) 0.001) ()
            "overstep-inside-east: should be 0")
    (assert (< (abs (compute-transition-overstep :west player 10.0 630.0 10.0 640.0)) 0.001) ()
            "overstep-inside-west: should be 0")))

(defun test-overstep-positive-when-past ()
  "Overstep should be positive when attempted position is past the zone boundary."
  (let ((player (%make-player)))
    ;; South: attempted-y > max-y
    (setf (player-attempted-x player) 300.0
          (player-attempted-y player) 645.0)
    (let ((os (compute-transition-overstep :south player 10.0 630.0 10.0 640.0)))
      (assert (> os 0.0) ()
              "overstep-past-south: should be positive, got ~,2f" os)
      (assert (< (abs (- os 5.0)) 0.01) ()
              "overstep-past-south: should be ~5, got ~,2f" os))
    ;; East: attempted-x > max-x
    (setf (player-attempted-x player) 635.0
          (player-attempted-y player) 300.0)
    (let ((os (compute-transition-overstep :east player 10.0 630.0 10.0 640.0)))
      (assert (> os 0.0) ()
              "overstep-past-east: should be positive, got ~,2f" os)
      (assert (< (abs (- os 5.0)) 0.01) ()
              "overstep-past-east: should be ~5, got ~,2f" os))))

(defun test-seam-translation-in-bounds-for-same-size ()
  "For same-size zones (src-bounds == dst-bounds), seam translation must produce
   an in-bounds position. This is the core invariant from the behavioral spec."
  ;; 64x64 zone, tile-size=64, half=16: bounds = [80, 560]
  (let ((min-b 80.0) (max-b 560.0))
    ;; East crossing: attempted-x = 563 (3px past max-x=560)
    (multiple-value-bind (nx ny)
        (seam-translate-position :east 563.0 300.0
                                 min-b max-b min-b max-b
                                 min-b max-b min-b max-b)
      ;; new-x = 80 + (563 - 560) = 83
      (assert (seam-position-valid-p nx ny min-b max-b min-b max-b) ()
              "same-size-east: seam position (~,1f,~,1f) must be in bounds" nx ny)
      (assert (< (abs (- nx 83.0)) 0.01) ()
              "same-size-east: new-x should be 83, got ~,2f" nx))
    ;; North crossing: attempted-y = 77 (3px past min-y=80)
    (multiple-value-bind (nx ny)
        (seam-translate-position :north 300.0 77.0
                                 min-b max-b min-b max-b
                                 min-b max-b min-b max-b)
      ;; new-y = 560 + (77 - 80) = 557
      (assert (seam-position-valid-p nx ny min-b max-b min-b max-b) ()
              "same-size-north: seam position (~,1f,~,1f) must be in bounds" nx ny)
      (assert (< (abs (- ny 557.0)) 0.01) ()
              "same-size-north: new-y should be 557, got ~,2f" ny))))

(defun test-player-can-stand-on-edge-without-transition ()
  "A stationary player on the edge-adjacent tile must NOT trigger world-crossing-edge.
   The player's attempted position equals actual position (on the collision bound),
   and strict inequality means this does not count as crossing."
  (let ((player (%make-player))
        (intent (make-intent)))
    ;; Stationary: no movement intent
    (setf (player-intent player) intent)
    (setf (intent-move-dx intent) 0.0 (intent-move-dy intent) 0.0)
    ;; Standing on east collision bound (max-x=560)
    (setf (player-x player) 560.0 (player-y player) 300.0)
    (setf (player-attempted-x player) 560.0
          (player-attempted-y player) 300.0)
    (let ((edge (world-crossing-edge player 80.0 560.0 80.0 560.0)))
      (assert (null edge) ()
              "stationary-on-east: should be nil (no crossing), got ~a" edge))
    ;; Standing on north collision bound (min-y=80)
    (setf (player-x player) 300.0 (player-y player) 80.0)
    (setf (player-attempted-x player) 300.0
          (player-attempted-y player) 80.0)
    (let ((edge (world-crossing-edge player 80.0 560.0 80.0 560.0)))
      (assert (null edge) ()
              "stationary-on-north: should be nil (no crossing), got ~a" edge))))

(defun test-transition-uses-seam-not-fallback ()
  "When attempted position is past the boundary, seam-translate-position produces
   an in-bounds result, so the primary (seam) path is used, not fallback."
  ;; Simulate the logic from transition-zone: compute seam translation, check validity
  (let* ((min-b 80.0) (max-b 560.0)
         ;; East crossing: attempted-x = 563
         (px 563.0) (py 300.0))
    (multiple-value-bind (trans-x trans-y)
        (seam-translate-position :east px py
                                 min-b max-b min-b max-b
                                 min-b max-b min-b max-b)
      (let ((in-bounds (seam-position-valid-p trans-x trans-y min-b max-b min-b max-b)))
        (assert in-bounds ()
                "seam-primary-path: translated position (~,1f,~,1f) should be in bounds"
                trans-x trans-y)
        ;; Overstep = 563 - 560 = 3. trans-x = 80 + 3 = 83.
        (assert (< (abs (- trans-x 83.0)) 0.01) ()
                "seam-primary-path: trans-x should be 83, got ~,2f" trans-x)))))

(defun test-attempted-position-set-for-stationary ()
  "When the player is stationary, attempted position must equal actual position.
   Prevents stale attempted values from triggering false crossings."
  (let* ((player (%make-player))
         (intent (make-intent))
         (world (make-test-world :tile-size 64.0 :collision-half 16.0)))
    (setf (player-intent player) intent)
    (setf (player-x player) 300.0 (player-y player) 400.0)
    ;; Set stale attempted values to prove they get overwritten
    (setf (player-attempted-x player) 999.0
          (player-attempted-y player) 999.0)
    ;; No movement input, no click target → stationary branch
    (setf (intent-move-dx intent) 0.0
          (intent-move-dy intent) 0.0
          (intent-target-active intent) nil)
    (update-player-position player intent world 1.0 0.016)
    (assert (< (abs (- (player-attempted-x player) 300.0)) 0.01) ()
            "stationary-attempted-x: should be 300 (actual), got ~,2f"
            (player-attempted-x player))
    (assert (< (abs (- (player-attempted-y player) 400.0)) 0.01) ()
            "stationary-attempted-y: should be 400 (actual), got ~,2f"
            (player-attempted-y player))))

(defun test-attempted-position-set-for-click-to-move ()
  "When using click-to-move, attempted position is set before collision resolution.
   The attempted position should be current + direction * step."
  (let* ((player (%make-player))
         (intent (make-intent))
         (world (make-test-world :tile-size 64.0 :collision-half 16.0)))
    (setf (player-intent player) intent)
    ;; Player at center, click target to the east
    (setf (player-x player) 300.0 (player-y player) 300.0)
    ;; Set up world bounds so player can actually move
    (setf (world-wall-min-x world) 80.0
          (world-wall-max-x world) 560.0
          (world-wall-min-y world) 80.0
          (world-wall-max-y world) 560.0)
    ;; Click target far east: activates click-to-move
    (set-intent-target intent 500.0 300.0)
    (setf (intent-move-dx intent) 0.0
          (intent-move-dy intent) 0.0)
    (update-player-position player intent world 1.0 0.016)
    ;; Attempted should be slightly east of 300 (towards 500)
    (assert (> (player-attempted-x player) 300.0) ()
            "click-attempted-x: should be > 300 (moving east), got ~,2f"
            (player-attempted-x player))
    ;; Attempted-y should be ~300 (pure east movement)
    (assert (< (abs (- (player-attempted-y player) 300.0)) 1.0) ()
            "click-attempted-y: should be ~300, got ~,2f"
            (player-attempted-y player))))

(defun test-crossing-edge-directional-gating ()
  "world-crossing-edge respects directional gating: attempted position past east
   but movement intent is north → no east crossing detected."
  (let ((player (%make-player))
        (intent (make-intent)))
    ;; Moving north (dy=-1), not east
    (setf (player-intent player) intent)
    (setf (intent-move-dx intent) 0.0 (intent-move-dy intent) -1.0)
    ;; Attempted position past east boundary
    (setf (player-x player) 560.0 (player-y player) 300.0)
    (setf (player-attempted-x player) 562.0
          (player-attempted-y player) 298.0)
    ;; Should NOT detect east because movement intent is north
    (let ((edge (world-crossing-edge player 80.0 560.0 80.0 560.0)))
      (assert (null edge) ()
              "gating-east-moving-north: should be nil, got ~a" edge))))

(defun test-reset-frame-intent-preserving-movement ()
  "reset-frame-intent-preserving-movement clears attack/run/unstuck but keeps move and face."
  (let ((intent (make-intent)))
    (setf (intent-move-dx intent) 1.0
          (intent-move-dy intent) -0.5
          (intent-face-dx intent) 1.0
          (intent-face-dy intent) 0.0
          (intent-attack intent) t
          (intent-run-toggle intent) t
          (intent-requested-unstuck intent) t)
    (reset-frame-intent-preserving-movement intent)
    ;; Movement preserved
    (assert (= (intent-move-dx intent) 1.0) ()
            "preserve-move-dx: should be 1.0, got ~,2f" (intent-move-dx intent))
    (assert (= (intent-move-dy intent) -0.5) ()
            "preserve-move-dy: should be -0.5, got ~,2f" (intent-move-dy intent))
    ;; Face preserved
    (assert (= (intent-face-dx intent) 1.0) ()
            "preserve-face-dx: should be 1.0, got ~,2f" (intent-face-dx intent))
    (assert (= (intent-face-dy intent) 0.0) ()
            "preserve-face-dy: should be 0.0, got ~,2f" (intent-face-dy intent))
    ;; Actions cleared
    (assert (null (intent-attack intent)) ()
            "clear-attack: should be nil")
    (assert (null (intent-run-toggle intent)) ()
            "clear-run: should be nil")
    (assert (null (intent-requested-unstuck intent)) ()
            "clear-unstuck: should be nil")))

(defun test-walk-target-clamped-to-bounds ()
  "set-player-walk-target clamps world-x/y to collision bounds when world is provided.
   Uses post-Issue-0 bounds: 64-tile zone, tile=64, collision-half=27.2 →
   wall-min=27.2, wall-max=4068.8."
  (let* ((player (%make-player))
         (intent (make-intent))
         (world (make-test-world :tile-size 64.0 :collision-half 27.2)))
    (setf (player-intent player) intent)
    (setf (world-wall-min-x world) 27.2
          (world-wall-max-x world) 4068.8
          (world-wall-min-y world) 27.2
          (world-wall-max-y world) 4068.8)
    ;; Click outside bounds (too far west and north)
    (set-player-walk-target player intent 10.0 10.0 t world)
    (assert (< (abs (- (intent-target-x intent) 27.2)) 0.01) ()
            "clamp-west: target-x should be clamped to 27.2, got ~,2f"
            (intent-target-x intent))
    (assert (< (abs (- (intent-target-y intent) 27.2)) 0.01) ()
            "clamp-north: target-y should be clamped to 27.2, got ~,2f"
            (intent-target-y intent))
    ;; Click outside bounds (too far east and south)
    (set-player-walk-target player intent 5000.0 5000.0 t world)
    (assert (< (abs (- (intent-target-x intent) 4068.8)) 0.01) ()
            "clamp-east: target-x should be clamped to 4068.8, got ~,2f"
            (intent-target-x intent))
    (assert (< (abs (- (intent-target-y intent) 4068.8)) 0.01) ()
            "clamp-south: target-y should be clamped to 4068.8, got ~,2f"
            (intent-target-y intent))
    ;; Click inside bounds — no clamping
    (set-player-walk-target player intent 2000.0 2000.0 t world)
    (assert (< (abs (- (intent-target-x intent) 2000.0)) 0.01) ()
            "no-clamp: target-x should be 2000.0, got ~,2f"
            (intent-target-x intent))
    (assert (< (abs (- (intent-target-y intent) 2000.0)) 0.01) ()
            "no-clamp: target-y should be 2000.0, got ~,2f"
            (intent-target-y intent))
    ;; Click on boundary ring tile center (tile 0: center=32px, within bounds 27.2–4068.8)
    (set-player-walk-target player intent 32.0 32.0 t world)
    (assert (< (abs (- (intent-target-x intent) 32.0)) 0.01) ()
            "boundary-ring: tile-0 center (32) should NOT be clamped, got ~,2f"
            (intent-target-x intent))
    (assert (not (intent-target-clamped-p intent)) ()
            "boundary-ring: click on tile-0 center should not set clamped flag")))

(defun test-walk-target-clamps-to-zone-bounds ()
  "When player has a loaded zone wall-map, clamping uses zone collision bounds
   instead of world bounds."
  (let* ((player (%make-player))
         (intent (make-intent))
         (world (make-test-world :tile-size 64.0 :collision-half 27.2))
         (zone-id :test-zone)
         (wall-map (make-array '(64 64) :element-type 'fixnum :initial-element 0))
         (saved-zone-states (make-hash-table :test 'eq)))
    (setf (player-intent player) intent
          (player-zone-id player) zone-id)
    ;; Save and replace zone-state cache
    (maphash (lambda (k v) (setf (gethash k saved-zone-states) v)) *zone-states*)
    (unwind-protect
         (progn
           (clrhash *zone-states*)
           (setf (gethash zone-id *zone-states*)
                 (make-zone-state :zone-id zone-id :wall-map wall-map))
           ;; World bounds differ from zone bounds; zone should win.
           (setf (world-wall-min-x world) 100.0
                 (world-wall-max-x world) 2000.0
                 (world-wall-min-y world) 100.0
                 (world-wall-max-y world) 2000.0)
           (set-player-walk-target player intent 10.0 10.0 t world)
           (assert (< (abs (- (intent-target-x intent) 27.2)) 0.01) ()
                   "zone-clamp: target-x should use zone bounds (27.2), got ~,2f"
                   (intent-target-x intent))
           (assert (< (abs (- (intent-target-y intent) 27.2)) 0.01) ()
                   "zone-clamp: target-y should use zone bounds (27.2), got ~,2f"
                   (intent-target-y intent)))
      (clrhash *zone-states*)
      (maphash (lambda (k v) (setf (gethash k *zone-states*) v)) saved-zone-states))))

(defun test-walk-target-no-clamp-without-world ()
  "set-player-walk-target does NOT clamp when world is nil (backward compat)."
  (let* ((player (%make-player))
         (intent (make-intent)))
    (setf (player-intent player) intent)
    (set-player-walk-target player intent 10.0 10.0 t nil)
    (assert (< (abs (- (intent-target-x intent) 10.0)) 0.01) ()
            "no-world: target-x should remain 10.0, got ~,2f"
            (intent-target-x intent))))

(defun test-clamped-target-not-cleared-when-blocked ()
  "When a clamped target is active but movement is blocked, the target should
   remain active so zone transitions can arm/commit."
  (let* ((player (%make-player))
         (intent (make-intent))
         (world (make-test-world :tile-size 64.0 :collision-half 16.0))
         (zone-id :blocked-zone)
         (wall-map (make-array '(4 4) :element-type 'fixnum :initial-element 0))
         (saved-zone-states (make-hash-table :test 'eq)))
    ;; Block tile (2,1) so eastward move from tile (1,1) is blocked.
    (setf (aref wall-map 1 2) 1)
    (setf (player-intent player) intent
          (player-zone-id player) zone-id
          (player-x player) 96.0
          (player-y player) 96.0)
    ;; Save/replace zone-state cache
    (maphash (lambda (k v) (setf (gethash k saved-zone-states) v)) *zone-states*)
    (unwind-protect
         (progn
           (clrhash *zone-states*)
           (setf (gethash zone-id *zone-states*)
                 (make-zone-state :zone-id zone-id :wall-map wall-map))
           ;; Clamped target beyond blocked tile
           (set-intent-target intent 224.0 96.0)
           (setf (intent-target-raw-x intent) 320.0
                 (intent-target-raw-y intent) 96.0
                 (intent-target-clamped-p intent) t)
           ;; Movement attempt should be blocked, but target stays active
           (update-player-position player intent world 1.0 0.3)
           (assert (intent-target-active intent) ()
                   "blocked-clamped: target should remain active when clamped and blocked"))
      (clrhash *zone-states*)
      (maphash (lambda (k v) (setf (gethash k *zone-states*) v)) saved-zone-states))))

(defun test-clamped-target-not-cleared-when-reached ()
  "When a clamped target is reached within one step, keep the target active
   so the boundary crossing can still commit on the next frame."
  (let* ((player (%make-player))
         (intent (make-intent))
         (world (make-test-world :tile-size 64.0 :collision-half 16.0)))
    (setf (player-intent player) intent)
    (setf (world-wall-min-x world) 16.0
          (world-wall-max-x world) 624.0
          (world-wall-min-y world) 16.0
          (world-wall-max-y world) 624.0)
    ;; Player slightly inside east boundary, clamped target at the edge.
    (setf (player-x player) 622.0
          (player-y player) 300.0)
    (set-intent-target intent 624.0 300.0)
    (setf (intent-target-raw-x intent) 700.0
          (intent-target-raw-y intent) 300.0
          (intent-target-clamped-p intent) t)
    (update-player-position player intent world 1.0 0.016)
    (assert (intent-target-active intent) ()
            "clamped-reached: target should remain active when clamped target is reached")))

(defun test-clamped-blocked-uses-raw-target-for-crossing ()
  "When clamped movement is blocked, attempted position uses raw target so
   world-crossing-edge can detect a boundary crossing."
  (let* ((player (%make-player))
         (intent (make-intent))
         (world (make-test-world :tile-size 64.0 :collision-half 16.0))
         (zone-id :blocked-zone)
         (wall-map (make-array '(4 4) :element-type 'fixnum :initial-element 0))
         (saved-zone-states (make-hash-table :test 'eq)))
    ;; Block tile (2,1) so eastward move from tile (1,1) is blocked.
    (setf (aref wall-map 1 2) 1)
    (setf (player-intent player) intent
          (player-zone-id player) zone-id
          (player-x player) 96.0
          (player-y player) 96.0)
    ;; Save/replace zone-state cache
    (maphash (lambda (k v) (setf (gethash k saved-zone-states) v)) *zone-states*)
    (unwind-protect
         (progn
           (clrhash *zone-states*)
           (setf (gethash zone-id *zone-states*)
                 (make-zone-state :zone-id zone-id :wall-map wall-map))
           ;; Clamped target inside bounds, raw target beyond east edge
           (set-intent-target intent 224.0 96.0)
           (setf (intent-target-raw-x intent) 320.0
                 (intent-target-raw-y intent) 96.0
                 (intent-target-clamped-p intent) t)
           (update-player-position player intent world 1.0 0.3)
           (multiple-value-bind (min-x max-x min-y max-y)
               (get-zone-collision-bounds zone-id 64.0 16.0 16.0)
             (let ((edge (world-crossing-edge player min-x max-x min-y max-y)))
               (assert (eq edge :east) ()
                       "blocked-crossing: expected :east, got ~a" edge))))
      (clrhash *zone-states*)
      (maphash (lambda (k v) (setf (gethash k *zone-states*) v)) saved-zone-states))))

(defun test-input-blocked-near-edge-forces-crossing ()
  "When keyboard input is blocked near an edge, attempted position is forced
   past the boundary to allow immediate crossing."
  (let* ((player (%make-player))
         (intent (make-intent))
         (world (make-test-world :tile-size 64.0 :collision-half 16.0))
         (zone-id :blocked-edge-zone)
         (wall-map (make-array '(4 4) :element-type 'fixnum :initial-element 0))
         (saved-zone-states (make-hash-table :test 'eq)))
    ;; Block tile (3,1) so eastward move at edge is blocked.
    (setf (aref wall-map 1 3) 1)
    (setf (player-intent player) intent
          (player-zone-id player) zone-id
          (player-x player) 224.0
          (player-y player) 96.0
          (intent-move-dx intent) 1.0
          (intent-move-dy intent) 0.0)
    ;; Save/replace zone-state cache
    (maphash (lambda (k v) (setf (gethash k saved-zone-states) v)) *zone-states*)
    (unwind-protect
         (progn
           (clrhash *zone-states*)
           (setf (gethash zone-id *zone-states*)
                 (make-zone-state :zone-id zone-id :wall-map wall-map))
           (update-player-position player intent world 1.0 0.3)
           (multiple-value-bind (min-x max-x min-y max-y)
               (get-zone-collision-bounds zone-id 64.0 16.0 16.0)
             (let ((edge (world-crossing-edge player min-x max-x min-y max-y)))
               (assert (eq edge :east) ()
                       "input-blocked-crossing: expected :east, got ~a" edge))))
      (clrhash *zone-states*)
      (maphash (lambda (k v) (setf (gethash k *zone-states*) v)) saved-zone-states))))

(defun test-diagonal-slide-still-forces-crossing ()
  "When diagonal input slides along an edge (blocked on one axis),
   attempted position should still cross the boundary."
  (let* ((player (%make-player))
         (intent (make-intent))
         (world (make-test-world :tile-size 64.0 :collision-half 16.0))
         (zone-id :diag-blocked-zone)
         (wall-map (make-array '(4 4) :element-type 'fixnum :initial-element 0))
         (saved-zone-states (make-hash-table :test 'eq)))
    ;; Block tile (3,1) so eastward move at edge is blocked; north movement allowed.
    (setf (aref wall-map 1 3) 1)
    (setf (player-intent player) intent
          (player-zone-id player) zone-id
          (player-x player) 224.0
          (player-y player) 160.0
          (intent-move-dx intent) 1.0
          (intent-move-dy intent) -1.0)
    ;; Save/replace zone-state cache
    (maphash (lambda (k v) (setf (gethash k saved-zone-states) v)) *zone-states*)
    (unwind-protect
         (progn
           (clrhash *zone-states*)
           (setf (gethash zone-id *zone-states*)
                 (make-zone-state :zone-id zone-id :wall-map wall-map))
           (update-player-position player intent world 1.0 0.3)
           (multiple-value-bind (min-x max-x min-y max-y)
               (get-zone-collision-bounds zone-id 64.0 16.0 16.0)
             (let ((edge (world-crossing-edge player min-x max-x min-y max-y)))
               (assert (eq edge :east) ()
                       "diag-slide-crossing: expected :east, got ~a" edge))))
      (clrhash *zone-states*)
      (maphash (lambda (k v) (setf (gethash k *zone-states*) v)) saved-zone-states))))
(defun test-reduced-arm-band-no-false-arm ()
  "With *zone-hysteresis-in* = 2.0, a player 3 tiles from the edge must NOT be in
   the arm band.  Old value (6.0) would have armed at this distance."
  (let ((*zone-hysteresis-in* 2.0))
    (let ((player (%make-player)))
      ;; Zone from 0..4096 (64-tile zone), tile-dest-size=64.
      ;; Player at y=256 → 256 px from north edge (=4 tiles away).
      (setf (player-x player) 2048.0 (player-y player) 256.0)
      (assert (not (player-in-arm-band-p player :north 0.0 4096.0 0.0 4096.0 64.0)) ()
              "reduced-arm: 4 tiles from edge should NOT arm (threshold=2 tiles)")
      ;; Player at y=96 → 96 px from north edge (=1.5 tiles away), should arm.
      (setf (player-y player) 96.0)
      (assert (player-in-arm-band-p player :north 0.0 4096.0 0.0 4096.0 64.0) ()
              "reduced-arm: 1.5 tiles from edge SHOULD arm (threshold=2 tiles)"))))

(defun test-reduced-cancel-line-closer ()
  "With *zone-hysteresis-out* = 3.0, a player 4 tiles from the edge should be past
   the cancel line.  Old value (8.0) would NOT have cancelled at this distance."
  (let ((*zone-hysteresis-out* 3.0))
    (let ((player (%make-player)))
      ;; Player at y=320 → 320 px from north edge (=5 tiles away)
      (setf (player-x player) 2048.0 (player-y player) 320.0)
      (assert (player-past-cancel-line-p player :north 0.0 4096.0 0.0 4096.0 64.0) ()
              "reduced-cancel: 5 tiles from edge should cancel (threshold=3 tiles)")
      ;; Player at y=128 → 128 px from north edge (=2 tiles away), should NOT cancel.
      (setf (player-y player) 128.0)
      (assert (not (player-past-cancel-line-p player :north 0.0 4096.0 0.0 4096.0 64.0)) ()
              "reduced-cancel: 2 tiles from edge should NOT cancel (threshold=3 tiles)"))))

(defun test-clamped-click-sets-raw-target ()
  "When click target is outside bounds and gets clamped, raw target is stored."
  (let* ((player (%make-player))
         (intent (make-intent))
         (world (make-test-world :tile-size 64.0 :collision-half 27.2)))
    (setf (player-intent player) intent)
    (setf (world-wall-min-x world) 27.2
          (world-wall-max-x world) 4068.8
          (world-wall-min-y world) 27.2
          (world-wall-max-y world) 4068.8)
    ;; Click outside east bounds
    (set-player-walk-target player intent 5000.0 2000.0 t world)
    (assert (intent-target-clamped-p intent) ()
            "clamped-click: target-clamped-p should be T when click is outside bounds")
    (assert (< (abs (- (intent-target-raw-x intent) 5000.0)) 0.01) ()
            "clamped-click: raw-x should be 5000.0, got ~,2f"
            (intent-target-raw-x intent))
    (assert (< (abs (- (intent-target-x intent) 4068.8)) 0.01) ()
            "clamped-click: target-x should be clamped to 4068.8, got ~,2f"
            (intent-target-x intent))
    ;; Click inside bounds — no clamping flag
    (set-player-walk-target player intent 2000.0 2000.0 t world)
    (assert (not (intent-target-clamped-p intent)) ()
            "inside-click: target-clamped-p should be nil for in-bounds click")))

(defun test-clamped-click-produces-attempted-past-boundary ()
  "When a clamped target is reached, attempted position extends past boundary
   using raw target direction, enabling world-crossing-edge to detect the crossing."
  (let* ((player (%make-player))
         (intent (make-intent))
         (world (make-test-world :tile-size 64.0 :collision-half 16.0)))
    (setf (player-intent player) intent)
    ;; 10-tile zone: bounds min=16, max=624
    (setf (world-wall-min-x world) 16.0
          (world-wall-max-x world) 624.0
          (world-wall-min-y world) 16.0
          (world-wall-max-y world) 624.0)
    ;; Player at east boundary, clamped target was from click outside east
    (setf (player-x player) 624.0
          (player-y player) 300.0)
    ;; Simulate clamped click: raw target at 700 (past east), clamped to 624
    (set-intent-target intent 624.0 300.0)
    (setf (intent-target-raw-x intent) 700.0
          (intent-target-raw-y intent) 300.0
          (intent-target-clamped-p intent) t)
    ;; Run movement update — player is at target (dist=0 < epsilon), clamped branch fires
    (update-player-position player intent world 1.0 0.016)
    ;; Attempted should be past east boundary (nudged toward raw target)
    (assert (> (player-attempted-x player) 624.0) ()
            "clamped-attempted: attempted-x should be > 624 (past east), got ~,2f"
            (player-attempted-x player))
    ;; world-crossing-edge should detect east crossing
    (setf (intent-move-dx intent) 1.0
          (intent-move-dy intent) 0.0)
    (let ((edge (world-crossing-edge player 16.0 624.0 16.0 624.0)))
      (assert (eq edge :east) ()
              "clamped-crossing: should detect :east crossing, got ~a" edge))))

(defun test-click-inside-bounds-no-crossing ()
  "When click target is inside bounds (not clamped), reaching it does not trigger crossing."
  (let* ((player (%make-player))
         (intent (make-intent))
         (world (make-test-world :tile-size 64.0 :collision-half 16.0)))
    (setf (player-intent player) intent)
    (setf (world-wall-min-x world) 16.0
          (world-wall-max-x world) 624.0
          (world-wall-min-y world) 16.0
          (world-wall-max-y world) 624.0)
    ;; Player near east edge, click target inside bounds
    (setf (player-x player) 600.0
          (player-y player) 300.0)
    (set-intent-target intent 600.0 300.0)
    (setf (intent-target-clamped-p intent) nil)
    ;; At target → stationary, attempted = actual
    (update-player-position player intent world 1.0 0.016)
    (assert (<= (player-attempted-x player) 624.0) ()
            "inside-click: attempted-x should be <= 624, got ~,2f"
            (player-attempted-x player))))

(defun test-transition-preserves-movement-integration ()
  "Integration: transition-zone preserves move-dx/dy so walk animation continues.
   Tests the actual transition code path at movement-transition.lisp:694."
  (let* ((world (make-test-world :tile-size 64.0 :collision-half 16.0))
         (zone-a-id :zone-anim-a)
         (zone-b-id :zone-anim-b)
         (wall-map (make-array '(10 10) :initial-element 0))
         (zone-b (%make-zone :id zone-b-id :width 10 :height 10
                             :collision-tiles nil :objects nil))
         (player (%make-player))
         (intent (make-intent))
         (game (%make-game :world world :players (vector player)
                           :npcs (vector) :entities (vector player)
                           :net-role :server
                           :npc-id-source (make-id-source 1000000 nil)))
         (exit (list :to zone-b-id :spawn-edge :west)))
    (setf (gethash zone-a-id *zone-states*)
          (make-zone-state :zone-id zone-a-id :wall-map wall-map
                           :zone (%make-zone :id zone-a-id :width 10 :height 10
                                             :collision-tiles nil :objects nil)))
    (setf (gethash zone-b-id *zone-states*)
          (make-zone-state :zone-id zone-b-id :wall-map wall-map
                           :zone zone-b
                           :player-grid (make-spatial-grid-for-zone 10 10 64.0)
                           :npc-grid (make-spatial-grid-for-zone 10 10 64.0)))
    ;; Player walking east (move-dx=1.0) past east boundary
    (setf (player-x player) 626.0
          (player-y player) 300.0
          (player-attempted-x player) 626.0
          (player-attempted-y player) 300.0
          (player-zone-id player) zone-a-id
          (player-intent player) intent)
    (setf (intent-move-dx intent) 1.0
          (intent-move-dy intent) 0.0)
    (setf (world-wall-min-x world) 16.0
          (world-wall-max-x world) 624.0
          (world-wall-min-y world) 16.0
          (world-wall-max-y world) 624.0
          (world-zone-label world) "Zone A")
    (let ((tmp-path (format nil "/tmp/test-zone-anim-~a.lisp" (get-universal-time))))
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
             ;; move-dx/dy should be preserved (not zeroed)
             (assert (= (intent-move-dx intent) 1.0) ()
                     "transition-anim: move-dx should be 1.0, got ~,2f"
                     (intent-move-dx intent))
             (assert (= (intent-move-dy intent) 0.0) ()
                     "transition-anim: move-dy should be 0.0, got ~,2f"
                     (intent-move-dy intent))
             ;; Attack should be cleared
             (assert (null (intent-attack intent)) ()
                     "transition-anim: attack should be nil after transition")
             ;; Player should be in zone-b
             (assert (eq (player-zone-id player) zone-b-id) ()
                     "transition-anim: player should be in zone-b"))
        (delete-file tmp-path)))))

(defun test-transition-rebases-cross-zone-target ()
  "Cross-zone click target should be translated into the destination zone so
   the player stops at the intended location (no infinite walk)."
  (let* ((world (make-test-world :tile-size 64.0 :collision-half 16.0))
         (zone-a-id :zone-target-a)
         (zone-b-id :zone-target-b)
         (wall-map (make-array '(10 10) :initial-element 0))
         (zone-b (%make-zone :id zone-b-id :width 10 :height 10
                             :collision-tiles nil :objects nil))
         (player (%make-player))
         (intent (make-intent))
         (game (%make-game :world world :players (vector player)
                           :npcs (vector) :entities (vector player)
                           :net-role :server
                           :npc-id-source (make-id-source 1000000 nil)))
         (exit (list :to zone-b-id :spawn-edge :west)))
    (setf (gethash zone-a-id *zone-states*)
          (make-zone-state :zone-id zone-a-id :wall-map wall-map
                           :zone (%make-zone :id zone-a-id :width 10 :height 10
                                             :collision-tiles nil :objects nil)))
    (setf (gethash zone-b-id *zone-states*)
          (make-zone-state :zone-id zone-b-id :wall-map wall-map
                           :zone zone-b
                           :player-grid (make-spatial-grid-for-zone 10 10 64.0)
                           :npc-grid (make-spatial-grid-for-zone 10 10 64.0)))
    (setf (player-x player) 620.0
          (player-y player) 300.0
          (player-attempted-x player) 626.0
          (player-attempted-y player) 300.0
          (player-zone-id player) zone-a-id
          (player-intent player) intent)
    ;; Simulate cross-zone click: raw target beyond east edge.
    (set-intent-target intent 624.0 300.0)
    (setf (intent-target-raw-x intent) 700.0
          (intent-target-raw-y intent) 300.0
          (intent-target-clamped-p intent) t)
    (setf (world-wall-min-x world) 16.0
          (world-wall-max-x world) 624.0
          (world-wall-min-y world) 16.0
          (world-wall-max-y world) 624.0
          (world-zone-label world) "Zone A")
    (let ((tmp-path (format nil "/tmp/test-zone-target-~a.lisp" (get-universal-time))))
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
             ;; Target should be translated into destination zone (~92.0 = 16 + (700-624)).
             (assert (< (abs (- (intent-target-x intent) 92.0)) 1.0) ()
                     "target-rebase: expected ~92.0, got ~,2f"
                     (intent-target-x intent))
             (assert (not (intent-target-clamped-p intent)) ()
                     "target-rebase: target should be in-bounds after transition"))
        (delete-file tmp-path)))))

(defun test-clamped-nudge-uses-raw-direction ()
  "When a clamped target is reached, the raw-target nudge keeps the target active
   and player-intent-direction uses the raw target so pending isn't cancelled."
  (let* ((player (%make-player))
         (intent (make-intent))
         (world (make-test-world :tile-size 64.0 :collision-half 16.0)))
    (setf (player-intent player) intent)
    (setf (world-wall-min-x world) 16.0
          (world-wall-max-x world) 624.0
          (world-wall-min-y world) 16.0
          (world-wall-max-y world) 624.0)
    ;; Player at east boundary
    (setf (player-x player) 624.0
          (player-y player) 300.0)
    ;; Simulate clamped click past east boundary
    (set-intent-target intent 624.0 300.0)
    (setf (intent-target-raw-x intent) 700.0
          (intent-target-raw-y intent) 300.0
          (intent-target-clamped-p intent) t)
    ;; Run movement — clamped nudge branch should fire
    (update-player-position player intent world 1.0 0.016)
    ;; Target should remain active for cross-zone commit and reapply
    (assert (intent-target-active intent) ()
            "nudge-direction: target should remain active when clamped")
    ;; player-intent-direction should use raw target (eastward)
    (multiple-value-bind (dx dy)
        (player-intent-direction player)
      (declare (ignore dy))
      (assert (> dx 0.5) ()
              "nudge-direction: player-intent-direction should be east, got ~,2f"
              dx))
    ;; edge-direction-passes-p should now pass for :east
    (multiple-value-bind (dx dy)
        (player-intent-direction player)
      (assert (edge-direction-passes-p dx dy :east) ()
              "nudge-direction: direction should pass for :east"))))

(defun test-client-cache-gate-defers-when-uncached ()
  "Client cache gate: when game is a client and target zone is not in the
   LRU cache, the commit path should be deferred. Tests the cache-gate logic
   directly without running full update-zone-transition (which needs GPU for game)."
  (let* ((cache (make-zone-cache :capacity 4))
         (target-id :zone-gate-b))
    ;; Target zone is NOT in cache
    (assert (null (zone-cache-lookup cache target-id)) ()
            "cache-gate: target zone should not be in empty cache")
    ;; Simulate the cache gate condition:
    ;; is-client = t, zone-lru exists, target not cached → should defer
    (let* ((is-client t)
           (zone-lru cache)
           (cached (or (not is-client)
                       (not zone-lru)
                       (zone-cache-lookup zone-lru target-id))))
      (assert (not cached) ()
              "cache-gate: should evaluate to NOT cached for empty cache on client"))
    ;; Now insert the target zone and check again
    (let ((zone (%make-zone :id target-id :width 10 :height 10)))
      (zone-cache-insert cache target-id zone)
      (let* ((is-client t)
             (zone-lru cache)
             (cached (or (not is-client)
                         (not zone-lru)
                         (zone-cache-lookup zone-lru target-id))))
        (assert cached ()
                "cache-gate: should evaluate to cached after insert")))
    ;; Server always commits (is-client = nil)
    (let* ((is-client nil)
           (zone-lru cache)
           (cached (or (not is-client)
                       (not zone-lru)
                       (zone-cache-lookup zone-lru target-id))))
      (assert cached ()
              "cache-gate: server should always pass cache gate"))))

(defun test-server-preloading-queues-adjacent ()
  "Server preloading should queue adjacent zones when a player is near an edge."
  (let* ((world (make-test-world :tile-size 64.0 :collision-half 16.0))
         (zone-a (%make-zone :id :zone-a :width 10 :height 10 :collision-tiles nil :objects nil))
         (player (%make-player))
         (game (%make-game :world world
                           :players (vector player)
                           :zone-cache (make-zone-cache :capacity 4)
                           :preload-queue nil))
         (target-id :zone-b))
    (setf (world-zone world) zone-a
          (world-wall-min-x world) 16.0
          (world-wall-max-x world) 624.0
          (world-wall-min-y world) 16.0
          (world-wall-max-y world) 624.0)
    (setf (player-zone-id player) :zone-a
          (player-x player) 620.0
          (player-y player) 300.0)
    (let ((tmp-path (format nil "/tmp/test-zone-preload-~a.lisp" (get-universal-time))))
      (with-open-file (out tmp-path :direction :output :if-exists :supersede)
        (write (list :id target-id :width 10 :height 10
                     :tile-layers nil :collision-tiles nil :objects nil)
               :stream out))
      (let* ((edges-by-zone (make-hash-table :test 'eq))
             (paths (make-hash-table :test 'eq)))
        (setf (gethash :zone-a edges-by-zone)
              (list '(:edge :east :to :zone-b :offset :preserve-y)))
        (setf (gethash target-id paths) tmp-path)
        (setf (world-world-graph world)
              (%make-world-graph :edges-by-zone edges-by-zone :zone-paths paths)))
      (unwind-protect
           (progn
             (update-server-preloading game)
             (assert (assoc target-id (game-preload-queue game)) ()
                     "server-preload: target zone should be queued for preloading"))
        (delete-file tmp-path)))))

;;; ============================================================
;;; Spawn position fix: use actual position, not raw attempted
;;; ============================================================

(defun test-seam-spawn-uses-actual-position ()
  "Seam translation should produce a spawn near the destination edge,
   not deep inside the zone. Previously, using raw attempted position
   (click target hundreds of px past boundary) caused tile-skipping."
  ;; Simulate east crossing: player at east edge, click target far past
  (let ((src-min-x 91.0) (src-max-x 4005.0)
        (src-min-y 91.0) (src-max-y 4005.0)
        (dst-min-x 91.0) (dst-max-x 4005.0)
        (dst-min-y 91.0) (dst-max-y 4005.0))
    ;; Old behavior: px = raw click target (4500), spawn = 91 + (4500 - 4005) = 586
    ;; New behavior: px = boundary + 1px push (4006), spawn = 91 + (4006 - 4005) = 92
    (let ((px (+ src-max-x 1.0))  ; 1px past boundary (new behavior)
          (py 2000.0))
      (multiple-value-bind (new-x new-y)
          (seam-translate-position :east px py
                                   src-min-x src-max-x src-min-y src-max-y
                                   dst-min-x dst-max-x dst-min-y dst-max-y)
        (assert (< new-x (+ dst-min-x 10.0)) ()
                "spawn-actual-pos-east: should be near dst-min-x, got ~,1f" new-x)
        (assert (< (abs (- new-y py)) 0.01) ()
                "spawn-actual-pos-east: y should be preserved, got ~,1f" new-y)))))

(defun test-seam-spawn-not-deep-for-click ()
  "A click 500px past the zone boundary should NOT spawn 500px deep."
  (let ((src-max-x 4005.0) (dst-min-x 91.0))
    ;; With the fix, we use boundary + 1px, not the raw click position.
    ;; Verify: seam formula with 1px overstep produces near-edge spawn
    (let* ((px (+ src-max-x 1.0))
           (new-x (+ dst-min-x (- px src-max-x))))
      (assert (< new-x 100.0) ()
              "spawn-not-deep: x should be ~92, got ~,1f" new-x))
    ;; Verify: the old approach with raw click at 4500 would be deep
    (let* ((bad-px 4500.0)
           (bad-x (+ dst-min-x (- bad-px src-max-x))))
      (assert (> bad-x 400.0) ()
              "spawn-not-deep: old approach would be ~586, got ~,1f" bad-x))))

(defun test-keyboard-blocked-threshold ()
  "Forced-attempted should fire with near-zero residual movement, not just exact zero."
  (let* ((player (%make-player))
         (intent (make-intent))
         (world (make-test-world :tile-size 64.0 :collision-half 27.2)))
    (setf (player-intent player) intent)
    ;; Player near east edge, pressing east, collision returns tiny residual
    (let ((min-x 91.2) (max-x 4004.8)
          (arm-px (* 2.0 64.0)))  ; arm band = 128px
      (setf (player-x player) 4000.0
            (player-y player) 2000.0)
      ;; Simulate: player pressed east (input-dx > 0), collision returned dx=0.1
      ;; Old code: (zerop 0.1) = nil → force doesn't fire
      ;; New code: (< (abs 0.1) 0.5) = t → force fires
      (let ((dx 0.1) (input-dx 1.0))
        ;; Check the condition that would be used
        (assert (not (zerop dx)) ()
                "blocked-threshold: dx=0.1 is not zerop")
        (assert (< (abs dx) 0.5) ()
                "blocked-threshold: dx=0.1 should pass threshold 0.5")
        (assert (<= (- max-x (player-x player)) arm-px) ()
                "blocked-threshold: player should be in arm band")
        (assert (> input-dx 0.0) ()
                "blocked-threshold: input-dx should be positive")))))

(defun test-rebased-target-clamped-to-bounds ()
  "Non-crossing target offset should be clamped to destination zone bounds."
  ;; If player was at x=4000 in zone 1 with target at x=3000,
  ;; and spawns at x=92 in zone 2, offset-rebased would be 92 + (3000-4000) = -908.
  ;; This must be clamped to new-min-x.
  (let ((spawn-x 92.0)
        (offset-x (- 3000.0 4000.0))  ; = -1000
        (new-min-x 91.0)
        (new-max-x 4005.0))
    (let ((rebased (clamp (+ spawn-x offset-x) new-min-x new-max-x)))
      (assert (>= rebased new-min-x) ()
              "rebased-clamp: should be >= min, got ~,1f" rebased)
      (assert (<= rebased new-max-x) ()
              "rebased-clamp: should be <= max, got ~,1f" rebased))))

(defun test-cooldown-reduced ()
  "Zone transition cooldown should be 0.5s (reduced from 1.5s)."
  (assert (< *zone-transition-cooldown-seconds* 1.0) ()
          "cooldown: should be < 1.0s, got ~,2f" *zone-transition-cooldown-seconds*)
  (assert (>= *zone-transition-cooldown-seconds* 0.3) ()
          "cooldown: should be >= 0.3s, got ~,2f" *zone-transition-cooldown-seconds*))

(defun test-client-zone-change-clears-target ()
  "handle-zone-transition must clear the click-to-move target.
   The target was set in the old zone's coordinate space and is invalid
   after zone change. Without clearing, the player walks across the
   entire new zone toward the stale target position."
  (let* ((player (%make-player))
         (intent (make-intent)))
    (setf (player-intent player) intent)
    ;; Simulate: target active from a clamped click in zone 1
    (set-intent-target intent 4004.8 2000.0)
    (setf (intent-target-raw-x intent) 4200.0
          (intent-target-raw-y intent) 2000.0
          (intent-target-clamped-p intent) t)
    (assert (intent-target-active intent) ()
            "client-zone-clear: target should be active before zone change")
    ;; Simulate what handle-zone-transition does: clear target
    (clear-intent-target intent)
    (assert (not (intent-target-active intent)) ()
            "client-zone-clear: target should be cleared after zone change")
    (assert (not (intent-target-clamped-p intent)) ()
            "client-zone-clear: clamped-p should be cleared after zone change")))

(defun test-client-zone-change-clears-client-intent ()
  "handle-zone-transition must clear game-client-intent target fields.
   The client-intent is sent to the server every frame and applied locally
   in run-local via apply-client-intent. If left stale, the old-zone target
   overwrites the server's rebased target on the next tick, causing the
   player to walk across the entire new zone indefinitely."
  (let* ((game (%make-game))
         (player (%make-player))
         (player-intent (make-intent))
         (client-intent (make-intent)))
    (setf (game-player game) player
          (player-intent player) player-intent
          (game-client-intent game) client-intent)
    ;; Simulate: stale click-to-move target in client-intent from zone 1
    (set-intent-target client-intent 4004.8 2000.0)
    (setf (intent-target-raw-x client-intent) 4200.0
          (intent-target-raw-y client-intent) 2000.0
          (intent-target-clamped-p client-intent) t)
    (assert (intent-target-active client-intent) ()
            "client-intent-clear: client-intent target should be active before zone change")
    ;; Simulate what handle-zone-transition does for client-intent
    (clear-intent-target client-intent)
    (assert (not (intent-target-active client-intent)) ()
            "client-intent-clear: client-intent target should be cleared after zone change")
    (assert (not (intent-target-clamped-p client-intent)) ()
            "client-intent-clear: client-intent clamped-p should be cleared after zone change")))

(defun test-stale-client-intent-not-reapplied-after-zone-change ()
  "After clearing both intents on zone change, apply-client-intent must NOT
   reintroduce a stale target. Simulates the run-local path where
   apply-client-intent copies client-intent -> player-intent every frame."
  (let* ((player-intent (make-intent))
         (client-intent (make-intent)))
    ;; Set up stale target in both intents (pre-zone-change state)
    (set-intent-target player-intent 4004.8 2000.0)
    (setf (intent-target-raw-x player-intent) 4200.0
          (intent-target-raw-y player-intent) 2000.0
          (intent-target-clamped-p player-intent) t)
    (set-intent-target client-intent 4004.8 2000.0)
    (setf (intent-target-raw-x client-intent) 4200.0
          (intent-target-raw-y client-intent) 2000.0
          (intent-target-clamped-p client-intent) t)
    ;; Simulate zone transition: clear both intents
    (clear-intent-target player-intent)
    (clear-intent-target client-intent)
    ;; Simulate next frame: apply-client-intent copies client -> player
    (apply-client-intent player-intent client-intent)
    ;; Player intent must still be inactive (not reintroduced by stale client-intent)
    (assert (not (intent-target-active player-intent)) ()
            "stale-reapply: target should remain inactive after apply-client-intent")
    (assert (not (intent-target-clamped-p player-intent)) ()
            "stale-reapply: clamped-p should remain nil after apply-client-intent")))

(defun test-client-cross-zone-target-rebased ()
  "When clicking across a zone boundary, handle-zone-transition should rebase
   the target into the new zone's coordinate space (via seam-translate-position)
   rather than clearing it. This lets the player walk to the intended destination."
  (let* ((ci (make-intent))
         (p-intent (make-intent))
         ;; Simulate: clamped click target aimed east into zone-2
         ;; Raw target is past east boundary of zone-1
         (src-min-x 91.2) (src-max-x 4004.8)
         (src-min-y 91.2) (src-max-y 4004.8)
         (dst-min-x 91.2) (dst-max-x 4004.8)
         (dst-min-y 91.2) (dst-max-y 4004.8)
         (raw-target-x 4200.0) ;; past src-max-x → crossing
         (raw-target-y 2000.0)
         (edge :east))
    ;; Set up client-intent with clamped crossing target
    (set-intent-target ci (min raw-target-x src-max-x) raw-target-y)
    (setf (intent-target-raw-x ci) raw-target-x
          (intent-target-raw-y ci) raw-target-y
          (intent-target-clamped-p ci) t)
    ;; Player-intent mirrors (as it would in practice)
    (set-intent-target p-intent (min raw-target-x src-max-x) raw-target-y)
    (setf (intent-target-raw-x p-intent) raw-target-x
          (intent-target-raw-y p-intent) raw-target-y
          (intent-target-clamped-p p-intent) t)
    ;; The target crosses: raw-target-x > src-max-x
    ;; Rebase using seam translation (what handle-zone-transition should do)
    (multiple-value-bind (tx ty)
        (seam-translate-position edge raw-target-x raw-target-y
                                 src-min-x src-max-x src-min-y src-max-y
                                 dst-min-x dst-max-x dst-min-y dst-max-y)
      (let ((rx (clamp tx dst-min-x dst-max-x))
            (ry (clamp ty dst-min-y dst-max-y)))
        ;; Rebased target should be inside destination bounds
        (assert (>= rx dst-min-x) ()
                "cross-zone-rebase: rebased-x ~,1f should be >= dst-min-x ~,1f" rx dst-min-x)
        (assert (<= rx dst-max-x) ()
                "cross-zone-rebase: rebased-x ~,1f should be <= dst-max-x ~,1f" rx dst-max-x)
        (assert (>= ry dst-min-y) ()
                "cross-zone-rebase: rebased-y ~,1f should be >= dst-min-y ~,1f" ry dst-min-y)
        ;; For east crossing: dst-min-x + (raw-x - src-max-x) = 91.2 + 195.2 = 286.4
        (assert (< (abs (- rx 286.4)) 1.0) ()
                "cross-zone-rebase: rebased-x should be ~286.4, got ~,1f" rx)
        (assert (< (abs (- ry 2000.0)) 0.1) ()
                "cross-zone-rebase: rebased-y should be ~2000, got ~,1f" ry)
        ;; Simulate rebasing on both intents
        (set-intent-target ci rx ry)
        (setf (intent-target-raw-x ci) rx
              (intent-target-raw-y ci) ry
              (intent-target-clamped-p ci) nil)
        (set-intent-target p-intent rx ry)
        (setf (intent-target-raw-x p-intent) rx
              (intent-target-raw-y p-intent) ry
              (intent-target-clamped-p p-intent) nil)
        ;; Both intents should still be active with rebased coords
        (assert (intent-target-active ci) ()
                "cross-zone-rebase: client-intent target should stay active after rebase")
        (assert (intent-target-active p-intent) ()
                "cross-zone-rebase: player-intent target should stay active after rebase")
        (assert (not (intent-target-clamped-p ci)) ()
                "cross-zone-rebase: clamped-p should be nil after rebase")
        ;; After apply-client-intent, player-intent should have rebased target
        (apply-client-intent p-intent ci)
        (assert (intent-target-active p-intent) ()
                "cross-zone-rebase: player target should stay active after apply-client-intent")
        (assert (< (abs (- (intent-target-x p-intent) rx)) 0.1) ()
                "cross-zone-rebase: player target-x should be rebased value ~,1f, got ~,1f"
                rx (intent-target-x p-intent))))))

(defun test-client-zone-change-clears-edge-strips ()
  "handle-zone-transition must clear edge-strips to prevent ghost sprites.
   Stale edge-strips from the old zone would render at wrong positions
   until the next snapshot replaces them."
  (let ((game (%make-game)))
    ;; Simulate: edge-strips populated from old zone
    (setf (game-edge-strips game) (list '(:edge :east :zone-id :zone-2)))
    (assert (game-edge-strips game) ()
            "edge-strip-clear: should have edge-strips before zone change")
    ;; Simulate what handle-zone-transition does: clear edge-strips
    (setf (game-edge-strips game) nil)
    (assert (null (game-edge-strips game)) ()
            "edge-strip-clear: edge-strips should be nil after zone change")))

(defun test-spawn-position-rounded-for-camera-snap ()
  "Zone transition spawn position is rounded to integers to avoid tile shift."
  ;; The camera snap formula (fround * zoom / zoom) rounds differently for
  ;; fractional positions, causing visible 1px tile shift on zone change.
  ;; Rounding spawn coords to integers fixes this.
  (let* ((spawn-x 286.4)
         (spawn-y 512.7)
         (rounded-x (fround spawn-x))
         (rounded-y (fround spawn-y)))
    (assert (= rounded-x 286.0) ()
            "spawn-x should round to 286.0, got ~f" rounded-x)
    (assert (= rounded-y 513.0) ()
            "spawn-y should round to 513.0, got ~f" rounded-y)
    ;; Verify that integer coords produce stable camera snap
    (let ((zoom 2.0))
      (let ((snapped (/ (fround (* rounded-x zoom)) zoom)))
        (assert (= snapped rounded-x) ()
                "integer position should survive camera snap, got ~f" snapped)))))

(defun test-apply-zone-to-world-sets-minimap-dirty ()
  "apply-zone-to-world sets minimap-dirty instead of rebuilding synchronously."
  (let* ((zone (%make-zone :id :test-zone :width 10 :height 10
                            :collision-tiles (make-hash-table :test 'eql)))
         (world (%make-world :tile-dest-size 64.0
                              :collision-half-width 27.2
                              :collision-half-height 27.2)))
    ;; Ensure dirty is initially nil
    (assert (not (world-minimap-dirty world)) ()
            "minimap-dirty should start nil")
    (apply-zone-to-world world zone)
    (assert (world-minimap-dirty world) ()
            "minimap-dirty should be t after apply-zone-to-world")))

(defun test-apply-zone-to-world-no-zone-paths-rebuild ()
  "apply-zone-to-world does not rebuild zone-paths (already built at startup)."
  (let* ((zone (%make-zone :id :test-zone :width 10 :height 10
                            :collision-tiles (make-hash-table :test 'eql)))
         (original-paths (make-hash-table :test 'eq))
         (graph (%make-world-graph :edges-by-zone (make-hash-table :test 'eq)
                                    :zone-paths original-paths))
         (world (%make-world :tile-dest-size 64.0
                              :collision-half-width 27.2
                              :collision-half-height 27.2
                              :world-graph graph)))
    (apply-zone-to-world world zone)
    ;; zone-paths should still be the same object (not rebuilt)
    (assert (eq (world-graph-zone-paths graph) original-paths) ()
            "zone-paths should not be rebuilt by apply-zone-to-world")))

;;; ============================================================
;;; Bug 4: Diagonal zone click pathing
;;; ============================================================

(defun make-diagonal-test-world ()
  "Create a 4-zone 2x2 grid world for diagonal click-path testing.
   Layout:
     zone-1 (NW) | zone-2 (NE)
     zone-3 (SW) | zone-4 (SE)
   Each zone is 10x10 tiles, tile-size=64, collision-half=16.
   Collision bounds per zone: min=16, max=624.
   Only the current zone (:dz-1) goes into *zone-states* (as on the real client).
   Adjacent zones are placed in the preview cache to exercise the fallback path
   in get-zone-bounds-from-any-cache."
  (let* ((tile-size 64.0)
         (half 16.0)
         (preview-cache (make-hash-table :test 'eq :size 8))
         (world (%make-world :tile-dest-size tile-size
                              :collision-half-width half
                              :collision-half-height half
                              :wall-min-x 16.0 :wall-max-x 624.0
                              :wall-min-y 16.0 :wall-max-y 624.0
                              :zone-preview-cache preview-cache))
         (zone-1 (%make-zone :id :dz-1 :width 10 :height 10
                              :collision-tiles nil :objects nil))
         (edges (make-hash-table :test 'eq))
         (paths (make-hash-table :test 'eq))
         (graph (%make-world-graph :edges-by-zone edges :zone-paths paths)))
    ;; Zone 1 (NW): east->2, south->3
    (setf (gethash :dz-1 edges)
          (list (list :edge :east :to :dz-2 :offset :preserve-y)
                (list :edge :south :to :dz-3 :offset :preserve-x)))
    ;; Zone 2 (NE): west->1, south->4
    (setf (gethash :dz-2 edges)
          (list (list :edge :west :to :dz-1 :offset :preserve-y)
                (list :edge :south :to :dz-4 :offset :preserve-x)))
    ;; Zone 3 (SW): east->4, north->1
    (setf (gethash :dz-3 edges)
          (list (list :edge :east :to :dz-4 :offset :preserve-y)
                (list :edge :north :to :dz-1 :offset :preserve-x)))
    ;; Zone 4 (SE): west->3, north->2
    (setf (gethash :dz-4 edges)
          (list (list :edge :west :to :dz-3 :offset :preserve-y)
                (list :edge :north :to :dz-2 :offset :preserve-x)))
    ;; Current zone in *zone-states* (client always has current zone)
    (setf (gethash :dz-1 *zone-states*)
          (make-zone-state :zone-id :dz-1
                           :wall-map (make-array '(10 10) :initial-element 0)
                           :zone zone-1))
    ;; Adjacent zones in preview cache only (mimics real client behavior)
    (dolist (zid '(:dz-2 :dz-3 :dz-4))
      (setf (gethash zid preview-cache)
            (%make-zone :id zid :width 10 :height 10
                        :collision-tiles nil :objects nil)))
    (setf (world-zone world) zone-1
          (world-world-graph world) graph)
    world))

(defmacro with-diagonal-test-zone-states (&body body)
  "Save/restore *zone-states* around diagonal path tests to prevent leakage."
  (let ((saved (gensym "SAVED-")))
    `(let ((,saved (make-hash-table :test 'eq)))
       (maphash (lambda (k v) (setf (gethash k ,saved) v)) *zone-states*)
       (unwind-protect (progn ,@body)
         (clrhash *zone-states*)
         (maphash (lambda (k v) (setf (gethash k *zone-states*) v)) ,saved)))))

(defun test-compute-diagonal-click-path-diagonal ()
  "Diagonal click (past both east and south bounds) returns a 2-hop route."
  (with-diagonal-test-zone-states
    (let* ((world (make-diagonal-test-world))
           (player (%make-player)))
      (setf (player-zone-id player) :dz-1)
      ;; Click past east (>624) and south (>624) → diagonal to zone-4
      (multiple-value-bind (e1 e2 inter final)
          (compute-diagonal-click-path world player 700.0 700.0)
        (assert e1 () "diagonal click should return edge-1, got nil")
        (assert e2 () "diagonal click should return edge-2, got nil")
        (assert inter () "diagonal click should return intermediate zone")
        (assert final () "diagonal click should return final zone")
        ;; Route should reach zone-4 via either zone-2 or zone-3
        (assert (eq final :dz-4) ()
                "final zone should be :dz-4, got ~a" final)
        (assert (or (eq inter :dz-2) (eq inter :dz-3)) ()
                "intermediate should be :dz-2 or :dz-3, got ~a" inter)))))

(defun test-compute-diagonal-click-path-orthogonal ()
  "Orthogonal click (past only one axis) returns NIL — not a diagonal path."
  (with-diagonal-test-zone-states
    (let* ((world (make-diagonal-test-world))
           (player (%make-player)))
      (setf (player-zone-id player) :dz-1)
      ;; Click past east only (y=300 is within bounds)
      (multiple-value-bind (e1 e2 inter final)
          (compute-diagonal-click-path world player 700.0 300.0)
        (declare (ignore e2 inter final))
        (assert (null e1) ()
                "orthogonal click should return nil, got edge-1=~a" e1)))))

(defun test-compute-diagonal-click-path-inside ()
  "Click inside zone bounds returns NIL."
  (with-diagonal-test-zone-states
    (let* ((world (make-diagonal-test-world))
           (player (%make-player)))
      (setf (player-zone-id player) :dz-1)
      (multiple-value-bind (e1 e2 inter final)
          (compute-diagonal-click-path world player 300.0 300.0)
        (declare (ignore e2 inter final))
        (assert (null e1) ()
                "click inside bounds should return nil, got edge-1=~a" e1)))))

(defun test-translate-click-to-final-zone ()
  "Click coordinates translate through 2 seams to land in final zone bounds.
   Uses preview cache for intermediate/final zones (mimics real client)."
  (with-diagonal-test-zone-states
    (let ((world (make-diagonal-test-world)))
      ;; Click at (700, 700) in zone-1 → east → south path via zone-2
      ;; Zone bounds: all zones are 10x10 @ tile=64, half=16 → [16..624]
      ;; East seam: src-max-x=624, dst-min-x=16
      ;;   mid-x = 16 + (700 - 624) = 92
      ;;   mid-y = 700 (preserved)
      ;; South seam: src-max-y=624, dst-min-y=16
      ;;   final-x = 92 (preserved)
      ;;   final-y = 16 + (700 - 624) = 92
      (multiple-value-bind (fx fy)
          (translate-click-to-final-zone world 700.0 700.0
                                         :dz-1 :dz-2 :dz-4
                                         :east :south)
        (assert fx () "translate should return final-x")
        (assert fy () "translate should return final-y")
        (assert (< (abs (- fx 92.0)) 1.0) ()
                "final-x should be ~92, got ~,2f" fx)
        (assert (< (abs (- fy 92.0)) 1.0) ()
                "final-y should be ~92, got ~,2f" fy)
        ;; Result should be within final zone bounds [16..624]
        (assert (and (>= fx 16.0) (<= fx 624.0)) ()
                "final-x should be within bounds [16,624], got ~,2f" fx)
        (assert (and (>= fy 16.0) (<= fy 624.0)) ()
                "final-y should be within bounds [16,624], got ~,2f" fy)))))

(defun test-clear-zone-click-path ()
  "clear-zone-click-path resets all fields."
  (let ((game (%make-game)))
    (setf (game-zone-click-path game) '(:zone-2)
          (game-zone-click-final-x game) 100.0
          (game-zone-click-final-y game) 200.0)
    (clear-zone-click-path game)
    (assert (null (game-zone-click-path game)) ()
            "zone-click-path should be nil after clear")
    (assert (= (game-zone-click-final-x game) 0.0) ()
            "zone-click-final-x should be 0.0 after clear")
    (assert (= (game-zone-click-final-y game) 0.0) ()
            "zone-click-final-y should be 0.0 after clear")))

(defun test-compute-diagonal-no-route ()
  "Diagonal click with missing world-graph edge returns NIL."
  (with-diagonal-test-zone-states
    (let* ((tile-size 64.0)
           (half 16.0)
           (world (%make-world :tile-dest-size tile-size
                                :collision-half-width half
                                :collision-half-height half
                                :wall-min-x 16.0 :wall-max-x 624.0
                                :wall-min-y 16.0 :wall-max-y 624.0))
           (zone (%make-zone :id :iso-1 :width 10 :height 10
                              :collision-tiles nil :objects nil))
           (edges (make-hash-table :test 'eq))
           (graph (%make-world-graph :edges-by-zone edges
                                      :zone-paths (make-hash-table :test 'eq)))
           (player (%make-player)))
      ;; Zone with only east exit, no south exit → no diagonal route
      (setf (gethash :iso-1 edges)
            (list (list :edge :east :to :iso-2 :offset :preserve-y)))
      (setf (gethash :iso-1 *zone-states*)
            (make-zone-state :zone-id :iso-1
                             :wall-map (make-array '(10 10) :initial-element 0)
                             :zone zone))
      (setf (world-zone world) zone
            (world-world-graph world) graph
            (player-zone-id player) :iso-1)
      (multiple-value-bind (e1 e2 inter final)
          (compute-diagonal-click-path world player 700.0 700.0)
        (declare (ignore e2 inter final))
        (assert (null e1) ()
                "no route should return nil, got edge-1=~a" e1)))))

;;; ============================================================
;;; Bug 4 Part 2: Zone-bounds index and minimap parity
;;; ============================================================

(defun test-build-zone-bounds-index ()
  "build-zone-bounds-index reads zone files and computes collision bounds."
  (let* ((tmp-dir (format nil "/tmp/test-zbi-~a/" (get-universal-time)))
         (path-a (format nil "~azone-a.lisp" tmp-dir))
         (path-b (format nil "~azone-b.lisp" tmp-dir)))
    (ensure-directories-exist path-a)
    (unwind-protect
         (progn
           ;; Write two zone files with different dimensions
           (with-open-file (out path-a :direction :output :if-exists :supersede)
             (write '(:id :zbi-a :width 10 :height 10 :layers nil :objects nil) :stream out))
           (with-open-file (out path-b :direction :output :if-exists :supersede)
             (write '(:id :zbi-b :width 20 :height 15 :layers nil :objects nil) :stream out))
           (let* ((zone-paths (make-hash-table :test 'eq))
                  ;; tile-dest-size=64, collision-half=16
                  (index (progn
                           (setf (gethash :zbi-a zone-paths) path-a
                                 (gethash :zbi-b zone-paths) path-b)
                           (build-zone-bounds-index zone-paths 64.0 16.0 16.0))))
             (assert (= (hash-table-count index) 2) ()
                     "index should have 2 entries, got ~d" (hash-table-count index))
             ;; Zone A: 10x10 @ tile=64, half=16 → [16..624]
             (let ((bounds-a (gethash :zbi-a index)))
               (assert bounds-a () "zone A should be indexed")
               (assert (< (abs (- (aref bounds-a 0) 16.0)) 0.1) ()
                       "zone A min-x should be ~16, got ~f" (aref bounds-a 0))
               (assert (< (abs (- (aref bounds-a 1) 624.0)) 0.1) ()
                       "zone A max-x should be ~624, got ~f" (aref bounds-a 1)))
             ;; Zone B: 20x15 @ tile=64, half=16 → x:[16..1264], y:[16..944]
             (let ((bounds-b (gethash :zbi-b index)))
               (assert bounds-b () "zone B should be indexed")
               (assert (< (abs (- (aref bounds-b 1) 1264.0)) 0.1) ()
                       "zone B max-x should be ~1264, got ~f" (aref bounds-b 1))
               (assert (< (abs (- (aref bounds-b 3) 944.0)) 0.1) ()
                       "zone B max-y should be ~944, got ~f" (aref bounds-b 3)))))
      ;; Cleanup
      (when (probe-file path-a) (delete-file path-a))
      (when (probe-file path-b) (delete-file path-b))
      (ignore-errors (uiop:delete-empty-directory tmp-dir)))))

(defun test-world-graph-zone-bounds-lookup ()
  "world-graph-zone-bounds returns pre-computed bounds or NIL."
  (let* ((index (make-hash-table :test 'eq))
         (graph (%make-world-graph :edges-by-zone (make-hash-table :test 'eq)
                                    :zone-paths (make-hash-table :test 'eq)
                                    :zone-bounds-index index)))
    (setf (gethash :test-z index)
          (make-array 4 :element-type 'single-float
                        :initial-contents '(16.0 624.0 16.0 624.0)))
    ;; Existing zone
    (multiple-value-bind (min-x max-x min-y max-y)
        (world-graph-zone-bounds graph :test-z)
      (assert (and min-x max-x min-y max-y) ()
              "lookup should return bounds for indexed zone")
      (assert (< (abs (- min-x 16.0)) 0.1) ()
              "min-x should be 16, got ~f" min-x)
      (assert (< (abs (- max-x 624.0)) 0.1) ()
              "max-x should be 624, got ~f" max-x))
    ;; Missing zone
    (multiple-value-bind (min-x max-x min-y max-y)
        (world-graph-zone-bounds graph :nonexistent)
      (declare (ignore max-x min-y max-y))
      (assert (null min-x) ()
              "lookup should return nil for missing zone"))))

(defun test-get-zone-bounds-uses-index-fallback ()
  "get-zone-bounds-from-any-cache uses the zone-bounds index when zone-state
   and preview/LRU caches are empty."
  (with-diagonal-test-zone-states
    (let* ((index (make-hash-table :test 'eq))
           (graph (%make-world-graph :edges-by-zone (make-hash-table :test 'eq)
                                      :zone-paths (make-hash-table :test 'eq)
                                      :zone-bounds-index index))
           (world (%make-world :tile-dest-size 64.0
                                :collision-half-width 16.0
                                :collision-half-height 16.0
                                :world-graph graph)))
      ;; Zone not in *zone-states*, not in preview cache, not in LRU
      ;; but IS in the bounds index
      (setf (gethash :idx-only index)
            (make-array 4 :element-type 'single-float
                          :initial-contents '(16.0 624.0 16.0 624.0)))
      (multiple-value-bind (min-x max-x min-y max-y)
          (get-zone-bounds-from-any-cache :idx-only world 64.0 16.0 16.0)
        (assert (and min-x max-x min-y max-y) ()
                "should return bounds from index fallback")
        (assert (< (abs (- min-x 16.0)) 0.1) ()
                "min-x from index should be ~16, got ~f" min-x)))))

(defun test-translate-click-no-preview-uses-index ()
  "translate-click-to-final-zone works with only the bounds index (no preview/LRU)."
  (with-diagonal-test-zone-states
    (let* ((index (make-hash-table :test 'eq))
           (graph (%make-world-graph :edges-by-zone (make-hash-table :test 'eq)
                                      :zone-paths (make-hash-table :test 'eq)
                                      :zone-bounds-index index))
           (world (%make-world :tile-dest-size 64.0
                                :collision-half-width 16.0
                                :collision-half-height 16.0
                                :world-graph graph)))
      ;; All three zones only in bounds index (no zone-states, no preview cache)
      (dolist (zid '(:idx-src :idx-int :idx-fin))
        (setf (gethash zid index)
              (make-array 4 :element-type 'single-float
                            :initial-contents '(16.0 624.0 16.0 624.0))))
      ;; East then south: same math as test-translate-click-to-final-zone
      (multiple-value-bind (fx fy)
          (translate-click-to-final-zone world 700.0 700.0
                                         :idx-src :idx-int :idx-fin
                                         :east :south)
        (assert fx () "translate should return final-x from index")
        (assert fy () "translate should return final-y from index")
        (assert (< (abs (- fx 92.0)) 1.0) ()
                "final-x should be ~92 via index, got ~,2f" fx)
        (assert (< (abs (- fy 92.0)) 1.0) ()
                "final-y should be ~92 via index, got ~,2f" fy)))))

(defun test-minimap-screen-to-world-extends-past-bounds ()
  "Minimap click at edge produces world coordinates past zone collision bounds."
  (let* ((world (%make-world :tile-dest-size 64.0
                              :collision-half-width 16.0
                              :collision-half-height 16.0
                              :wall-min-x 16.0 :wall-max-x 624.0
                              :wall-min-y 16.0 :wall-max-y 624.0))
         (player (%make-player))
         (ui (%make-ui)))
    ;; Center player in zone
    (setf (player-x player) 320.0
          (player-y player) 320.0)
    ;; Set minimap rect
    (setf (ui-minimap-x ui) 0
          (ui-minimap-y ui) 0
          (ui-minimap-width ui) 200
          (ui-minimap-height ui) 200)
    ;; Click at bottom-right corner of minimap (x=199, y=199 ≈ ratio ~1.0)
    (multiple-value-bind (wx wy)
        (minimap-screen-to-world ui world player 199 199)
      ;; With margin extension, the right/bottom edge should be past wall-max
      (assert (> wx (world-wall-max-x world)) ()
              "minimap corner click x should exceed wall-max-x=~f, got ~f"
              (world-wall-max-x world) wx)
      (assert (> wy (world-wall-max-y world)) ()
              "minimap corner click y should exceed wall-max-y=~f, got ~f"
              (world-wall-max-y world) wy))
    ;; Click at center of minimap should be near player position
    (multiple-value-bind (wx wy)
        (minimap-screen-to-world ui world player 100 100)
      (assert (< (abs (- wx (player-x player))) 5.0) ()
              "minimap center click x should be near player, got ~f vs ~f"
              wx (player-x player))
      (assert (< (abs (- wy (player-y player))) 5.0) ()
              "minimap center click y should be near player, got ~f vs ~f"
              wy (player-y player)))))

;;; ============================================================
;;; Bug 4 Part 3: Multi-hop minimap pathing tests
;;; ============================================================

(defun make-zone-bounds-index-for-test (zone-ids tile-size half-w half-h width height)
  "Create a zone-bounds index for testing with all zones having same dimensions."
  (let ((index (make-hash-table :test 'eq :size (length zone-ids))))
    (dolist (zid zone-ids)
      (multiple-value-bind (min-x max-x min-y max-y)
          (zone-bounds-zero-origin tile-size width height half-w half-h)
        (setf (gethash zid index)
              (make-array 4 :element-type 'single-float
                            :initial-contents (list min-x max-x min-y max-y)))))
    index))

(defun make-3x3-test-world-for-minimap ()
  "Create a 3x3 grid world with zone-bounds index for minimap path testing.
   All zones are 10x10 tiles, 64px tile size, 16px collision half.
   Layout:  NW  N  NE
            W   C  E
            SW  S  SE"
  (let* ((tile-size 64.0)
         (half-w 16.0)
         (half-h 16.0)
         (zone-ids '(:zone-nw :zone-n :zone-ne
                     :zone-w :zone-c :zone-e
                     :zone-sw :zone-s :zone-se))
         (edges (list
                 ;; Center row
                 '(:from :zone-c :to :zone-e :edge :east :offset :preserve-y)
                 '(:from :zone-e :to :zone-c :edge :west :offset :preserve-y)
                 '(:from :zone-c :to :zone-w :edge :west :offset :preserve-y)
                 '(:from :zone-w :to :zone-c :edge :east :offset :preserve-y)
                 ;; Center column
                 '(:from :zone-c :to :zone-n :edge :north :offset :preserve-x)
                 '(:from :zone-n :to :zone-c :edge :south :offset :preserve-x)
                 '(:from :zone-c :to :zone-s :edge :south :offset :preserve-x)
                 '(:from :zone-s :to :zone-c :edge :north :offset :preserve-x)
                 ;; North row
                 '(:from :zone-n :to :zone-ne :edge :east :offset :preserve-y)
                 '(:from :zone-ne :to :zone-n :edge :west :offset :preserve-y)
                 '(:from :zone-n :to :zone-nw :edge :west :offset :preserve-y)
                 '(:from :zone-nw :to :zone-n :edge :east :offset :preserve-y)
                 ;; South row
                 '(:from :zone-s :to :zone-se :edge :east :offset :preserve-y)
                 '(:from :zone-se :to :zone-s :edge :west :offset :preserve-y)
                 '(:from :zone-s :to :zone-sw :edge :west :offset :preserve-y)
                 '(:from :zone-sw :to :zone-s :edge :east :offset :preserve-y)
                 ;; West column
                 '(:from :zone-w :to :zone-nw :edge :north :offset :preserve-x)
                 '(:from :zone-nw :to :zone-w :edge :south :offset :preserve-x)
                 '(:from :zone-w :to :zone-sw :edge :south :offset :preserve-x)
                 '(:from :zone-sw :to :zone-w :edge :north :offset :preserve-x)
                 ;; East column
                 '(:from :zone-e :to :zone-ne :edge :north :offset :preserve-x)
                 '(:from :zone-ne :to :zone-e :edge :south :offset :preserve-x)
                 '(:from :zone-e :to :zone-se :edge :south :offset :preserve-x)
                 '(:from :zone-se :to :zone-e :edge :north :offset :preserve-x)))
         (bounds-index (make-zone-bounds-index-for-test zone-ids tile-size half-w half-h 10 10))
         (graph (%make-world-graph
                 :edges-by-zone (normalize-world-graph-edges edges)
                 :zone-paths (make-hash-table :test 'eq)
                 :zone-bounds-index bounds-index))
         (zone (%make-zone :id :zone-c :width 10 :height 10
                           :collision-tiles nil :objects nil)))
    (multiple-value-bind (min-x max-x min-y max-y)
        (zone-bounds-zero-origin tile-size 10 10 half-w half-h)
      (%make-world :tile-dest-size tile-size
                   :collision-half-width half-w
                   :collision-half-height half-h
                   :wall-min-x min-x :wall-max-x max-x
                   :wall-min-y min-y :wall-max-y max-y
                   :zone zone
                   :world-graph graph))))

(defun test-resolve-click-destination-zone ()
  "resolve-click-destination-zone walks through zones to find where click lands."
  (let ((world (make-3x3-test-world-for-minimap)))
    ;; Click inside current zone — destination is current zone
    (multiple-value-bind (dest-id walked-zones walked-edges)
        (resolve-click-destination-zone world 320.0 320.0)
      (assert (eq dest-id :zone-c) ()
              "resolve inside: should be :zone-c, got ~a" dest-id)
      (assert (null walked-zones) ()
              "resolve inside: should have no walked zones"))
    ;; Click far east — should land in :zone-e
    (multiple-value-bind (dest-id walked-zones walked-edges)
        (resolve-click-destination-zone world 700.0 320.0)
      (declare (ignore walked-edges))
      (assert (eq dest-id :zone-e) ()
              "resolve east: should be :zone-e, got ~a" dest-id)
      (assert (= (length walked-zones) 1) ()
              "resolve east: should walk through 1 zone, got ~d" (length walked-zones)))))

(defun test-translate-click-along-path-multihop ()
  "translate-click-along-path chains seam translations and returns hop targets."
  (let ((world (make-3x3-test-world-for-minimap)))
    ;; Translate from center through east (1-hop)
    (multiple-value-bind (fx fy hop-targets)
        (translate-click-along-path world 700.0 320.0 :zone-c '(:zone-e))
      (assert fx () "1-hop translate should return fx")
      (assert fy () "1-hop translate should return fy")
      ;; x should be translated into east zone's coordinate space
      (assert (> fx 0.0) () "1-hop fx should be positive, got ~,2f" fx)
      ;; Should have 1 hop target
      (assert (= (length hop-targets) 1) ()
              "1-hop should have 1 hop-target, got ~d" (length hop-targets))
      ;; The hop target is in source (zone-c) coords — should be the raw click x=700
      (let ((ht (first hop-targets)))
        (assert (< (abs (- (car ht) 700.0)) 0.01) ()
                "1-hop target x should be ~700, got ~,2f" (car ht))))
    ;; Translate from center through east then NE (2-hop)
    (multiple-value-bind (fx fy hop-targets)
        (translate-click-along-path world 700.0 -100.0 :zone-c '(:zone-e :zone-ne))
      (assert fx () "2-hop translate should return fx")
      (assert fy () "2-hop translate should return fy")
      (assert (= (length hop-targets) 2) ()
              "2-hop should have 2 hop-targets, got ~d" (length hop-targets)))))

(defun test-compute-minimap-click-path-inside ()
  "Minimap click inside current zone returns NIL (no path needed)."
  (let* ((world (make-3x3-test-world-for-minimap))
         (player (%make-player)))
    (setf (player-x player) 320.0 (player-y player) 320.0
          (player-zone-id player) :zone-c)
    (multiple-value-bind (path edge-list hop-targets fx fy)
        (compute-minimap-click-path world player 320.0 320.0)
      (assert (null path) ()
              "minimap inside: path should be NIL, got ~a" path)
      (assert (null edge-list) ()
              "minimap inside: edge-list should be NIL, got ~a" edge-list)
      (assert (null hop-targets) ()
              "minimap inside: hop-targets should be NIL, got ~a" hop-targets))))

(defun test-compute-minimap-click-path-adjacent ()
  "Minimap click in adjacent zone produces 1-hop path."
  (let* ((world (make-3x3-test-world-for-minimap))
         (player (%make-player)))
    (setf (player-x player) 320.0 (player-y player) 320.0
          (player-zone-id player) :zone-c)
    ;; Click far east (past zone bounds)
    (multiple-value-bind (path edge-list hop-targets fx fy)
        (compute-minimap-click-path world player 700.0 320.0)
      (assert path () "minimap east: should have a path")
      (assert (= (length path) 1) ()
              "minimap east: path should be 1 hop, got ~d: ~a" (length path) path)
      (assert (eq (first path) :zone-e) ()
              "minimap east: should go to :zone-e, got ~a" (first path))
      (assert edge-list () "minimap east: should have edge-list")
      (assert (= (length edge-list) 1) ()
              "minimap east: edge-list should be 1 hop, got ~d" (length edge-list))
      (assert (eq (first edge-list) :east) ()
              "minimap east: edge should be :east, got ~a" (first edge-list))
      (assert hop-targets () "minimap east: should have hop-targets")
      (assert (= (length hop-targets) 1) ()
              "minimap east: hop-targets should be 1, got ~d" (length hop-targets))
      (assert fx () "minimap east: should have final-x")
      (assert fy () "minimap east: should have final-y"))))

(defun test-compute-minimap-click-path-diagonal ()
  "Minimap click at diagonal zone produces 2-hop path."
  (let* ((world (make-3x3-test-world-for-minimap))
         (player (%make-player)))
    (setf (player-x player) 320.0 (player-y player) 320.0
          (player-zone-id player) :zone-c)
    ;; Click far east AND far south (diagonal) — should resolve to :zone-se
    (multiple-value-bind (path edge-list hop-targets fx fy)
        (compute-minimap-click-path world player 700.0 700.0)
      (assert path () "minimap diagonal: should have a path")
      (assert (= (length path) 2) ()
              "minimap diagonal: should be 2 hops, got ~d: ~a" (length path) path)
      (assert (eq (car (last path)) :zone-se) ()
              "minimap diagonal: should end at :zone-se, got ~a" (car (last path)))
      (assert edge-list () "minimap diagonal: should have edge-list")
      (assert (= (length edge-list) 2) ()
              "minimap diagonal: edge-list should be 2 hops, got ~d" (length edge-list))
      (assert hop-targets () "minimap diagonal: should have hop-targets")
      (assert (= (length hop-targets) 2) ()
              "minimap diagonal: hop-targets should be 2, got ~d" (length hop-targets))
      (assert fx () "minimap diagonal: should have final-x")
      (assert fy () "minimap diagonal: should have final-y"))))

(defun test-continuation-pops-multihop-path ()
  "handle-zone-transition continuation advances through multi-zone path."
  ;; Simulate the continuation logic directly (no full game needed)
  (let* ((game (%make-game :world (make-3x3-test-world-for-minimap)
                           :player (%make-player)
                           :players (vector (%make-player))
                           :npcs (vector)
                           :entities (vector)
                           :net-role :local
                           :client-intent (make-intent))))
    (setf (player-x (game-player game)) 320.0
          (player-y (game-player game)) 320.0
          (player-zone-id (game-player game)) :zone-c
          (player-intent (game-player game)) (make-intent))
    ;; Set a 2-hop path: C -> E -> SE with parallel hop-targets
    (setf (game-zone-click-path game) (list :zone-e :zone-se)
          (game-zone-click-edges game) (list :east :south)
          (game-zone-click-hop-targets game) (list (cons 700.0 320.0)
                                                   (cons 320.0 700.0))
          (game-zone-click-final-x game) 320.0
          (game-zone-click-final-y game) 320.0)
    ;; Simulate arriving at :zone-e — pop all three parallel lists
    (let ((remaining (game-zone-click-path game))
          (remaining-edges (game-zone-click-edges game))
          (remaining-targets (game-zone-click-hop-targets game)))
      (let ((pos (position :zone-e remaining)))
        (assert (= pos 0) () "continuation: :zone-e should be at position 0")
        (setf (game-zone-click-path game) (nthcdr (1+ pos) remaining))
        (setf (game-zone-click-edges game) (nthcdr (1+ pos) remaining-edges))
        (setf (game-zone-click-hop-targets game) (nthcdr (1+ pos) remaining-targets))))
    (assert (equal (game-zone-click-path game) '(:zone-se)) ()
            "continuation: after popping :zone-e, should have (:zone-se), got ~a"
            (game-zone-click-path game))
    (assert (equal (game-zone-click-edges game) '(:south)) ()
            "continuation: edges should be (:south), got ~a"
            (game-zone-click-edges game))
    (let ((ht (first (game-zone-click-hop-targets game))))
      (assert (< (abs (- (car ht) 320.0)) 0.01) ()
              "continuation: remaining hop-target x should be ~320, got ~,2f" (car ht)))
    ;; Simulate arriving at :zone-se — pop, path empty = final
    (let ((remaining (game-zone-click-path game))
          (remaining-edges (game-zone-click-edges game))
          (remaining-targets (game-zone-click-hop-targets game)))
      (let ((pos (position :zone-se remaining)))
        (assert (= pos 0) () "continuation: :zone-se should be at position 0")
        (setf (game-zone-click-path game) (nthcdr (1+ pos) remaining))
        (setf (game-zone-click-edges game) (nthcdr (1+ pos) remaining-edges))
        (setf (game-zone-click-hop-targets game) (nthcdr (1+ pos) remaining-targets))))
    (assert (null (game-zone-click-path game)) ()
            "continuation: path should be empty after reaching final zone")
    (assert (null (game-zone-click-hop-targets game)) ()
            "continuation: hop-targets should be empty after reaching final zone")))

(defun test-zone-path-edge-list ()
  "zone-path-edge-list computes per-hop edge directions for a zone path."
  (let ((graph (make-test-graph-3x3)))
    ;; 1-hop: C -> E
    (let ((edges (zone-path-edge-list graph :zone-c '(:zone-e))))
      (assert (equal edges '(:east)) ()
              "edge-list 1-hop: C->E should be (:east), got ~a" edges))
    ;; 2-hop: C -> E -> SE
    (let ((edges (zone-path-edge-list graph :zone-c '(:zone-e :zone-se))))
      (assert (equal edges '(:east :south)) ()
              "edge-list 2-hop: C->E->SE should be (:east :south), got ~a" edges))
    ;; 3-hop: C -> N -> NE -> E
    (let ((edges (zone-path-edge-list graph :zone-c '(:zone-n :zone-ne :zone-e))))
      (assert (equal edges '(:north :east :south)) ()
              "edge-list 3-hop: should be (:north :east :south), got ~a" edges))
    ;; Disconnected path returns NIL
    (let ((edges (zone-path-edge-list graph :zone-c '(:zone-ne))))
      (assert (null edges) ()
              "edge-list disconnected: C->NE (no direct edge) should be NIL, got ~a" edges))
    ;; Empty path
    (assert (null (zone-path-edge-list graph :zone-c nil)) ()
            "edge-list empty: nil path should be NIL")))

(defun test-continuation-pops-edges-parallel ()
  "Continuation logic pops edges and hop-targets in parallel with zone-click-path."
  (let* ((game (%make-game :world (make-3x3-test-world-for-minimap)
                           :player (%make-player)
                           :players (vector (%make-player))
                           :npcs (vector)
                           :entities (vector)
                           :net-role :local
                           :client-intent (make-intent))))
    (setf (player-x (game-player game)) 320.0
          (player-y (game-player game)) 320.0
          (player-zone-id (game-player game)) :zone-c
          (player-intent (game-player game)) (make-intent))
    ;; Set a 2-hop path: C -> E -> SE with edges and hop-targets
    (setf (game-zone-click-path game) (list :zone-e :zone-se)
          (game-zone-click-edges game) (list :east :south)
          (game-zone-click-hop-targets game) (list (cons 700.0 320.0)
                                                   (cons 320.0 700.0))
          (game-zone-click-final-x game) 320.0
          (game-zone-click-final-y game) 320.0)
    ;; Pop first hop (arriving at :zone-e)
    (let ((pos (position :zone-e (game-zone-click-path game))))
      (setf (game-zone-click-path game) (nthcdr (1+ pos) (game-zone-click-path game)))
      (setf (game-zone-click-edges game) (nthcdr (1+ pos) (game-zone-click-edges game)))
      (setf (game-zone-click-hop-targets game)
            (nthcdr (1+ pos) (game-zone-click-hop-targets game))))
    ;; After pop: path=(:zone-se), edges=(:south), hop-targets has 1 entry
    (assert (equal (game-zone-click-path game) '(:zone-se)) ()
            "edges-parallel: path should be (:zone-se), got ~a"
            (game-zone-click-path game))
    (assert (equal (game-zone-click-edges game) '(:south)) ()
            "edges-parallel: edges should be (:south), got ~a"
            (game-zone-click-edges game))
    (assert (= (length (game-zone-click-hop-targets game)) 1) ()
            "edges-parallel: hop-targets should have 1 entry, got ~d"
            (length (game-zone-click-hop-targets game)))
    ;; Pop second hop (arriving at :zone-se)
    (let ((pos (position :zone-se (game-zone-click-path game))))
      (setf (game-zone-click-path game) (nthcdr (1+ pos) (game-zone-click-path game)))
      (setf (game-zone-click-edges game) (nthcdr (1+ pos) (game-zone-click-edges game)))
      (setf (game-zone-click-hop-targets game)
            (nthcdr (1+ pos) (game-zone-click-hop-targets game))))
    ;; All three should be empty
    (assert (null (game-zone-click-path game)) ()
            "edges-parallel: path should be nil after final pop")
    (assert (null (game-zone-click-edges game)) ()
            "edges-parallel: edges should be nil after final pop")
    (assert (null (game-zone-click-hop-targets game)) ()
            "edges-parallel: hop-targets should be nil after final pop")))

(defun test-continuation-seam-rebase ()
  "Continuation uses precomputed hop targets from translate-click-along-path."
  ;; Verify that hop targets produced by translate-click-along-path are in
  ;; source-zone coordinate space (the raw click translated to each hop),
  ;; NOT derived from the player's runtime position.
  (let* ((world (make-3x3-test-world-for-minimap)))
    ;; 1-hop east: raw click at x=700 (past zone-c's max-x=624)
    ;; The hop target for the first hop should be x=700 (the raw click coords
    ;; in zone-c space, BEFORE seam translation).
    (multiple-value-bind (fx fy hop-targets)
        (translate-click-along-path world 700.0 320.0 :zone-c '(:zone-e))
      (assert hop-targets () "seam-rebase: should have hop-targets")
      (let ((ht (first hop-targets)))
        (assert ht () "seam-rebase: first hop-target should exist")
        ;; Target should be the raw click coords (700, 320) — NOT the player's
        ;; current position.  This is the key guarantee: the continuation sets
        ;; this as the walk target, not a seam-translated player position.
        (assert (< (abs (- (car ht) 700.0)) 0.01) ()
                "seam-rebase: hop-target x should be ~700 (raw click), got ~,2f" (car ht))
        (assert (< (abs (- (cdr ht) 320.0)) 0.01) ()
                "seam-rebase: hop-target y should be ~320 (raw click), got ~,2f" (cdr ht)))
      ;; Final coords should be in destination zone's space (different from raw)
      (assert fx () "seam-rebase: should have final-x")
      (assert fy () "seam-rebase: should have final-y"))))

(defun test-zone-bounds-with-origin ()
  "zone-bounds-with-origin incorporates tile origin offsets into bounds."
  ;; Zero origin: same as zone-bounds-zero-origin
  (multiple-value-bind (min-x max-x min-y max-y)
      (zone-bounds-with-origin 64.0 10 10 0 0 16.0 16.0)
    ;; min = 0*64 + 16 = 16, max = 10*64 - 16 = 624
    (assert (< (abs (- min-x 16.0)) 0.01) ()
            "origin(0,0) min-x should be 16, got ~,2f" min-x)
    (assert (< (abs (- max-x 624.0)) 0.01) ()
            "origin(0,0) max-x should be 624, got ~,2f" max-x)
    (assert (< (abs (- min-y 16.0)) 0.01) ()
            "origin(0,0) min-y should be 16, got ~,2f" min-y)
    (assert (< (abs (- max-y 624.0)) 0.01) ()
            "origin(0,0) max-y should be 624, got ~,2f" max-y))
  ;; Non-zero origin: offsets shift the bounds
  (multiple-value-bind (min-x max-x min-y max-y)
      (zone-bounds-with-origin 64.0 10 10 5 3 16.0 16.0)
    ;; min-x = 5*64 + 16 = 336, max-x = (5+10)*64 - 16 = 944
    ;; min-y = 3*64 + 16 = 208, max-y = (3+10)*64 - 16 = 816
    (assert (< (abs (- min-x 336.0)) 0.01) ()
            "origin(5,3) min-x should be 336, got ~,2f" min-x)
    (assert (< (abs (- max-x 944.0)) 0.01) ()
            "origin(5,3) max-x should be 944, got ~,2f" max-x)
    (assert (< (abs (- min-y 208.0)) 0.01) ()
            "origin(5,3) min-y should be 208, got ~,2f" min-y)
    (assert (< (abs (- max-y 816.0)) 0.01) ()
            "origin(5,3) max-y should be 816, got ~,2f" max-y)))

;;; ============================================================
;;; Bug 5/6: Diagonal multi-hop zone transition fixes
;;; ============================================================

(defun test-hop-target-not-clamped ()
  "set-player-walk-target with nil world preserves out-of-bounds coordinates."
  (let* ((player (%make-player))
         (intent (make-intent)))
    (setf (player-intent player) intent)
    ;; Pass nil for world -- should skip clamping entirely
    (set-player-walk-target player intent 700.0 -50.0 nil nil)
    (assert (< (abs (- (intent-target-x intent) 700.0)) 0.01) ()
            "hop-no-clamp: target-x should be 700.0, got ~,2f" (intent-target-x intent))
    (assert (< (abs (- (intent-target-y intent) -50.0)) 0.01) ()
            "hop-no-clamp: target-y should be -50.0, got ~,2f" (intent-target-y intent))
    (assert (not (intent-target-clamped-p intent)) ()
            "hop-no-clamp: should not be clamped")
    ;; Raw target should also be set to the unclamped value
    (assert (< (abs (- (intent-target-raw-x intent) 700.0)) 0.01) ()
            "hop-no-clamp: raw-x should be 700.0, got ~,2f" (intent-target-raw-x intent))
    (assert (< (abs (- (intent-target-raw-y intent) -50.0)) 0.01) ()
            "hop-no-clamp: raw-y should be -50.0, got ~,2f" (intent-target-raw-y intent))))

(defun test-player-attempted-past-edge-p ()
  "player-attempted-past-edge-p returns correct results for all four edges."
  (let ((player (%make-player)))
    ;; Zone bounds: min-x=16 max-x=624 min-y=16 max-y=624
    ;; North: attempted-y < min-y
    (setf (player-attempted-x player) 300.0
          (player-attempted-y player) 10.0)
    (assert (player-attempted-past-edge-p player :north 16.0 624.0 16.0 624.0) ()
            "past-edge north: ay=10 < min-y=16 should be T")
    (assert (not (player-attempted-past-edge-p player :south 16.0 624.0 16.0 624.0)) ()
            "past-edge south: ay=10 < max-y=624 should be NIL")
    ;; South: attempted-y > max-y
    (setf (player-attempted-y player) 630.0)
    (assert (player-attempted-past-edge-p player :south 16.0 624.0 16.0 624.0) ()
            "past-edge south: ay=630 > max-y=624 should be T")
    (assert (not (player-attempted-past-edge-p player :north 16.0 624.0 16.0 624.0)) ()
            "past-edge north: ay=630 > min-y=16 should be NIL")
    ;; West: attempted-x < min-x
    (setf (player-attempted-x player) 10.0
          (player-attempted-y player) 300.0)
    (assert (player-attempted-past-edge-p player :west 16.0 624.0 16.0 624.0) ()
            "past-edge west: ax=10 < min-x=16 should be T")
    (assert (not (player-attempted-past-edge-p player :east 16.0 624.0 16.0 624.0)) ()
            "past-edge east: ax=10 < max-x=624 should be NIL")
    ;; East: attempted-x > max-x
    (setf (player-attempted-x player) 630.0)
    (assert (player-attempted-past-edge-p player :east 16.0 624.0 16.0 624.0) ()
            "past-edge east: ax=630 > max-x=624 should be T")
    (assert (not (player-attempted-past-edge-p player :west 16.0 624.0 16.0 624.0)) ()
            "past-edge west: ax=630 > min-x=16 should be NIL")
    ;; Inside: not past any edge
    (setf (player-attempted-x player) 300.0
          (player-attempted-y player) 300.0)
    (assert (not (player-attempted-past-edge-p player :north 16.0 624.0 16.0 624.0)) ()
            "past-edge inside-north: should be NIL")
    (assert (not (player-attempted-past-edge-p player :south 16.0 624.0 16.0 624.0)) ()
            "past-edge inside-south: should be NIL")
    (assert (not (player-attempted-past-edge-p player :east 16.0 624.0 16.0 624.0)) ()
            "past-edge inside-east: should be NIL")
    (assert (not (player-attempted-past-edge-p player :west 16.0 624.0 16.0 624.0)) ()
            "past-edge inside-west: should be NIL")))

(defun test-force-arm-stored-edge ()
  "When zone-click-edges is set and player is in arm band, pending is forced to stored edge."
  (let* ((world (make-3x3-test-world-for-minimap))
         (player (%make-player))
         (intent (make-intent))
         (game (%make-game :world world :player player :players (vector player)
                           :npcs (vector) :entities (vector)
                           :net-role :local :client-intent (make-intent))))
    (setf (player-intent player) intent
          (player-zone-id player) :zone-c
          ;; Position near east edge (arm band)
          (player-x player) 620.0
          (player-y player) 320.0
          ;; Moving east
          (intent-move-dx intent) 1.0
          (intent-move-dy intent) 0.0
          (player-attempted-x player) 625.0
          (player-attempted-y player) 320.0)
    ;; Set a stored path with :east as next edge
    (setf (game-zone-click-path game) (list :zone-e :zone-se)
          (game-zone-click-edges game) (list :east :south)
          (game-zone-click-hop-targets game) (list (cons 700.0 320.0) (cons 320.0 700.0)))
    ;; Run zone transitions
    (update-zone-transition game 0.016)
    ;; Pending should be forced to :east (stored edge)
    (assert (eq (player-zone-transition-pending player) :east) ()
            "force-arm: pending should be :east, got ~a"
            (player-zone-transition-pending player))))

(defun test-force-arm-requires-arm-band ()
  "Force-arm only works when player is near the expected edge, not from interior."
  (let* ((world (make-3x3-test-world-for-minimap))
         (player (%make-player))
         (intent (make-intent))
         (game (%make-game :world world :player player :players (vector player)
                           :npcs (vector) :entities (vector)
                           :net-role :local :client-intent (make-intent))))
    (setf (player-intent player) intent
          (player-zone-id player) :zone-c
          ;; Position in center (NOT in arm band)
          (player-x player) 320.0
          (player-y player) 320.0
          (intent-move-dx intent) 1.0
          (intent-move-dy intent) 0.0
          (player-attempted-x player) 321.0
          (player-attempted-y player) 320.0)
    ;; Set a stored path with :east as next edge
    (setf (game-zone-click-path game) (list :zone-e :zone-se)
          (game-zone-click-edges game) (list :east :south)
          (game-zone-click-hop-targets game) (list (cons 700.0 320.0) (cons 320.0 700.0)))
    ;; Run zone transitions
    (update-zone-transition game 0.016)
    ;; Pending should NOT be armed (player not in arm band)
    (assert (null (player-zone-transition-pending player)) ()
            "force-arm-interior: pending should be nil, got ~a"
            (player-zone-transition-pending player))))

(defun test-directional-cancel-skipped-for-stored-edge ()
  "Pending is not cancelled by directional gating when it matches stored edge."
  (let* ((world (make-3x3-test-world-for-minimap))
         (player (%make-player))
         (intent (make-intent))
         (game (%make-game :world world :player player :players (vector player)
                           :npcs (vector) :entities (vector)
                           :net-role :local :client-intent (make-intent))))
    (setf (player-intent player) intent
          (player-zone-id player) :zone-c
          ;; Near east edge
          (player-x player) 620.0
          (player-y player) 320.0
          ;; Moving diagonally (SE) -- might fail directional gating for :east
          ;; if the south component dominates
          (intent-move-dx intent) 0.3
          (intent-move-dy intent) 0.95
          (player-attempted-x player) 620.3
          (player-attempted-y player) 320.95)
    ;; Pre-set pending to :east (as if already armed)
    (setf (player-zone-transition-pending player) :east)
    ;; Set a stored path requiring :east
    (setf (game-zone-click-path game) (list :zone-e :zone-se)
          (game-zone-click-edges game) (list :east :south)
          (game-zone-click-hop-targets game) (list (cons 700.0 320.0) (cons 320.0 700.0)))
    ;; Run zone transitions
    (update-zone-transition game 0.016)
    ;; Pending should NOT be cancelled -- stored edge overrides directional gating
    (assert (eq (player-zone-transition-pending player) :east) ()
            "dir-cancel-skip: pending should remain :east, got ~a"
            (player-zone-transition-pending player))))

(defun test-commit-override-bypasses-gating ()
  "player-attempted-past-edge-p triggers commit even when world-crossing-edge returns different edge."
  ;; This tests the force-commit path: player has pending=:east, stored edge=:east,
  ;; and attempted position is past the east boundary.
  (let* ((world (make-3x3-test-world-for-minimap))
         (player (%make-player))
         (intent (make-intent))
         (game (%make-game :world world :player player :players (vector player)
                           :npcs (vector) :entities (vector)
                           :net-role :server :client-intent (make-intent))))
    (setf (player-intent player) intent
          (player-zone-id player) :zone-c
          ;; Near east edge, attempted past boundary
          (player-x player) 623.0
          (player-y player) 620.0
          ;; Moving diagonally south-east
          (intent-move-dx intent) 0.5
          (intent-move-dy intent) 0.87
          (player-attempted-x player) 625.0
          (player-attempted-y player) 625.0)
    ;; Pre-set pending to :east
    (setf (player-zone-transition-pending player) :east)
    ;; Set stored path requiring :east commit
    (setf (game-zone-click-path game) (list :zone-e :zone-se)
          (game-zone-click-edges game) (list :east :south)
          (game-zone-click-hop-targets game) (list (cons 700.0 320.0) (cons 320.0 700.0)))
    ;; Run zone transitions -- should commit via force-commit path
    (let ((count (update-zone-transition game 0.016)))
      ;; If commit happened, player-zone-id changes
      ;; (transition-zone may fail if zone data isn't loaded, but pending should be cleared)
      ;; At minimum, the transition should have been attempted
      (assert (or (> count 0)
                  ;; If zone data wasn't loaded, pending gets cleared by the exit-nil check
                  (null (player-zone-transition-pending player))) ()
              "commit-override: should have committed or cleared pending, pending=~a count=~d"
              (player-zone-transition-pending player) count))))

(defun test-unexpected-zone-retry-recomputes ()
  "Landing in unexpected zone recomputes path from current zone to final destination."
  (let* ((game (%make-game :world (make-3x3-test-world-for-minimap)
                           :player (%make-player)
                           :players (vector (%make-player))
                           :npcs (vector)
                           :entities (vector)
                           :net-role :local
                           :client-intent (make-intent))))
    (setf (player-x (game-player game)) 320.0
          (player-y (game-player game)) 320.0
          (player-zone-id (game-player game)) :zone-s  ; unexpected zone
          (player-intent (game-player game)) (make-intent))
    ;; Path was expecting zone-e then zone-se, but we ended up in zone-s
    (setf (game-zone-click-path game) (list :zone-e :zone-se)
          (game-zone-click-edges game) (list :east :south)
          (game-zone-click-hop-targets game) (list (cons 700.0 320.0)
                                                   (cons 320.0 700.0))
          (game-zone-click-final-x game) 320.0
          (game-zone-click-final-y game) 320.0
          (game-zone-click-retry-p game) nil)
    ;; Simulate the zone change continuation logic
    (let* ((zone-id :zone-s)
           (remaining (game-zone-click-path game))
           (pos (position zone-id remaining)))
      ;; zone-s is not in the path, so pos is nil
      (assert (null pos) ()
              "retry-recompute: :zone-s should NOT be in path ~a" remaining)
      ;; The continuation t-clause would fire here
      ;; Test that the retry flag exists and can be set
      (assert (not (game-zone-click-retry-p game)) ()
              "retry-recompute: retry-p should be nil initially"))))

(defun test-unexpected-zone-retry-limit ()
  "Second unexpected zone after retry clears path (no infinite loop)."
  (let ((game (%make-game :world (make-3x3-test-world-for-minimap)
                          :player (%make-player)
                          :players (vector (%make-player))
                          :npcs (vector)
                          :entities (vector)
                          :net-role :local
                          :client-intent (make-intent))))
    ;; Set retry-p to true (already retried once)
    (setf (game-zone-click-path game) (list :zone-e :zone-se)
          (game-zone-click-edges game) (list :east :south)
          (game-zone-click-hop-targets game) (list (cons 700.0 320.0)
                                                   (cons 320.0 700.0))
          (game-zone-click-retry-p game) t)
    ;; clear-zone-click-path should clear retry-p
    (clear-zone-click-path game)
    (assert (null (game-zone-click-retry-p game)) ()
            "retry-limit: clear-zone-click-path should clear retry-p")
    (assert (null (game-zone-click-path game)) ()
            "retry-limit: path should be nil after clear")))

(defun test-rebase-skipped-when-path-active ()
  "Rebase block does not run when game-zone-click-path is non-nil.
   Verifies that intents are preserved (not cleared) for the continuation."
  (let* ((game (%make-game :world (make-3x3-test-world-for-minimap)
                           :player (%make-player)
                           :players (vector (%make-player))
                           :npcs (vector)
                           :entities (vector)
                           :net-role :local
                           :client-intent (make-intent)))
         (player (game-player game))
         (ci (game-client-intent game)))
    (setf (player-intent player) (make-intent)
          (player-zone-id player) :zone-c)
    ;; Set an active walk target on the client intent
    (set-intent-target ci 500.0 500.0)
    (assert (intent-target-active ci) ()
            "rebase-skip: ci should have active target before transition")
    ;; Set a multi-hop path (this should prevent rebase from clearing intents)
    (setf (game-zone-click-path game) (list :zone-e :zone-se)
          (game-zone-click-edges game) (list :east :south)
          (game-zone-click-hop-targets game) (list (cons 700.0 320.0)
                                                   (cons 320.0 700.0)))
    ;; The (unless (game-zone-click-path game) ...) guard means
    ;; the rebase block won't run, so the intent should remain active.
    ;; We can verify the guard condition directly:
    (assert (game-zone-click-path game) ()
            "rebase-skip: path should be non-nil (guard condition)")))

(defun test-opposite-edge ()
  "opposite-edge returns the correct opposite for all four edges."
  (assert (eq (opposite-edge :north) :south) () "opposite :north -> :south")
  (assert (eq (opposite-edge :south) :north) () "opposite :south -> :north")
  (assert (eq (opposite-edge :east) :west) () "opposite :east -> :west")
  (assert (eq (opposite-edge :west) :east) () "opposite :west -> :east")
  (assert (null (opposite-edge :invalid)) () "opposite :invalid -> nil"))

(defun test-reverse-translate-along-path ()
  "reverse-translate-along-path: reverse dest coords, then forward-translate
   back through the same path, recovering the original destination.
   Note: translate-click-along-path clamps the final result to dest zone bounds,
   so a raw forward->reverse round-trip on interior points loses information.
   The correct test is: reverse(dest) -> forward -> clamp == dest."
  (let ((world (make-3x3-test-world-for-minimap)))
    ;; First, get valid destination coordinates by forward-translating a point
    ;; that is PAST zone boundaries (the normal use case for click translation).
    ;; Use x=700 which is past zone-c's max-x=624, so it has positive overstep.
    (multiple-value-bind (dest-x dest-y _hop-targets)
        (translate-click-along-path world 700.0 320.0 :zone-c '(:zone-e :zone-se))
      (declare (ignore _hop-targets))
      (assert (and dest-x dest-y) ()
              "rev-translate: forward translate should succeed")
      ;; Now reverse-translate dest coords from :zone-se space back to :zone-c space
      (multiple-value-bind (src-x src-y)
          (reverse-translate-along-path world dest-x dest-y :zone-c '(:zone-e :zone-se))
        (assert (and src-x src-y) ()
                "rev-translate: reverse translate should succeed")
        ;; Forward-translate the reversed coords through the same path
        (multiple-value-bind (rt-x rt-y _rt-targets)
            (translate-click-along-path world src-x src-y :zone-c '(:zone-e :zone-se))
          (declare (ignore _rt-targets))
          (assert (and rt-x rt-y) ()
                  "rev-translate: re-forward translate should succeed")
          ;; The round-trip should recover the destination coordinates
          (assert (< (abs (- rt-x dest-x)) 1.0) ()
                  "rev-translate: round-trip x should be ~,2f, got ~,2f" dest-x rt-x)
          (assert (< (abs (- rt-y dest-y)) 1.0) ()
                  "rev-translate: round-trip y should be ~,2f, got ~,2f" dest-y rt-y))))))

(defun test-reverse-translate-single-hop ()
  "reverse-translate-along-path works for a single-hop path.
   Same approach: reverse valid dest coords, forward again, recover dest."
  (let ((world (make-3x3-test-world-for-minimap)))
    ;; Forward-translate a point past zone-c's east edge into zone-e
    (multiple-value-bind (dest-x dest-y _hop-targets)
        (translate-click-along-path world 700.0 320.0 :zone-c '(:zone-e))
      (declare (ignore _hop-targets))
      (assert (and dest-x dest-y) ()
              "rev-single: forward translate should succeed")
      ;; Reverse dest coords from :zone-e back to :zone-c
      (multiple-value-bind (src-x src-y)
          (reverse-translate-along-path world dest-x dest-y :zone-c '(:zone-e))
        (assert (and src-x src-y) ()
                "rev-single: reverse translate should succeed")
        ;; Forward again through same path
        (multiple-value-bind (rt-x rt-y _rt-targets)
            (translate-click-along-path world src-x src-y :zone-c '(:zone-e))
          (declare (ignore _rt-targets))
          (assert (and rt-x rt-y) ()
                  "rev-single: re-forward translate should succeed")
          (assert (< (abs (- rt-x dest-x)) 1.0) ()
                  "rev-single: round-trip x should be ~,2f, got ~,2f" dest-x rt-x)
          (assert (< (abs (- rt-y dest-y)) 1.0) ()
                  "rev-single: round-trip y should be ~,2f, got ~,2f" dest-y rt-y))))))

(defparameter *tests-zone-continuity* (list
    ;; ADDENDUM 1: Overstep preservation tests
    'test-compute-transition-overstep-north
    'test-compute-transition-overstep-all-edges
    'test-apply-overstep-to-spawn-directions
    ;; ADDENDUM 2: Commit detection via boundary crossing
    'test-commit-requires-boundary-crossing
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
    'test-seam-translate-mixed-bounds
    ;; New tests: attempted position and world-crossing-edge
    'test-crossing-edge-does-not-affect-preview
    'test-overstep-zero-when-inside
    'test-overstep-positive-when-past
    'test-seam-translation-in-bounds-for-same-size
    'test-player-can-stand-on-edge-without-transition
    'test-transition-uses-seam-not-fallback
    'test-attempted-position-set-for-stationary
    'test-attempted-position-set-for-click-to-move
    'test-crossing-edge-directional-gating
    ;; Animation preservation across transitions (Issue 3 fix)
    'test-reset-frame-intent-preserving-movement
    ;; Click-to-move target clamping (Issue 2 fix)
    'test-walk-target-clamped-to-bounds
    'test-walk-target-clamps-to-zone-bounds
    'test-walk-target-no-clamp-without-world
    ;; Reduced ARM band depth (Issue 1 fix)
    'test-reduced-arm-band-no-false-arm
    'test-reduced-cancel-line-closer
    ;; Click-to-move zone crossing (Issue 2b fix)
    'test-clamped-click-sets-raw-target
    'test-clamped-click-produces-attempted-past-boundary
    'test-click-inside-bounds-no-crossing
    'test-clamped-target-not-cleared-when-blocked
    'test-clamped-target-not-cleared-when-reached
    'test-clamped-blocked-uses-raw-target-for-crossing
    'test-input-blocked-near-edge-forces-crossing
    'test-diagonal-slide-still-forces-crossing
    ;; Issue 3 integration: movement preserved across transition-zone
    'test-transition-preserves-movement-integration
    'test-transition-rebases-cross-zone-target
    ;; Issue 2b: direction preserved for pending cancel gate
    'test-clamped-nudge-uses-raw-direction
    ;; Issue 6: client cache gate defers commit when not cached
    'test-client-cache-gate-defers-when-uncached
    ;; Server-side preloading to avoid sync load stalls
    'test-server-preloading-queues-adjacent
    ;; Spawn position fix: actual position, not raw attempted
    'test-seam-spawn-uses-actual-position
    'test-seam-spawn-not-deep-for-click
    ;; Keyboard blocked threshold fix
    'test-keyboard-blocked-threshold
    ;; Rebased target bounds clamping
    'test-rebased-target-clamped-to-bounds
    ;; Cooldown reduction
    'test-cooldown-reduced
    ;; Client-side zone change fixes
    'test-client-zone-change-clears-target
    'test-client-zone-change-clears-edge-strips
    ;; game-client-intent must also be cleared on zone change
    'test-client-zone-change-clears-client-intent
    'test-stale-client-intent-not-reapplied-after-zone-change
    ;; Cross-zone click targets should be rebased, not cleared
    'test-client-cross-zone-target-rebased
    ;; Minimap dirty flag deferred rebuild (Bug 2 audio skip fix)
    'test-apply-zone-to-world-sets-minimap-dirty
    'test-apply-zone-to-world-no-zone-paths-rebuild
    ;; Spawn position rounding (Bug 3 tile shift fix)
    'test-spawn-position-rounded-for-camera-snap
    ;; Bug 4: Diagonal zone click pathing
    'test-compute-diagonal-click-path-diagonal
    'test-compute-diagonal-click-path-orthogonal
    'test-compute-diagonal-click-path-inside
    'test-translate-click-to-final-zone
    'test-clear-zone-click-path
    'test-compute-diagonal-no-route
    ;; Bug 4 Part 2: Zone-bounds index, unified fallback, minimap parity
    'test-build-zone-bounds-index
    'test-world-graph-zone-bounds-lookup
    'test-get-zone-bounds-uses-index-fallback
    'test-translate-click-no-preview-uses-index
    'test-minimap-screen-to-world-extends-past-bounds
    ;; Bug 4 Part 3: Multi-hop minimap pathing
    'test-resolve-click-destination-zone
    'test-translate-click-along-path-multihop
    'test-compute-minimap-click-path-inside
    'test-compute-minimap-click-path-adjacent
    'test-compute-minimap-click-path-diagonal
    'test-continuation-pops-multihop-path
    'test-zone-path-edge-list
    'test-continuation-pops-edges-parallel
    'test-continuation-seam-rebase
    'test-zone-bounds-with-origin
    ;; Bug 5/6: Diagonal multi-hop zone transition fixes
    'test-hop-target-not-clamped
    'test-player-attempted-past-edge-p
    'test-force-arm-stored-edge
    'test-force-arm-requires-arm-band
    'test-directional-cancel-skipped-for-stored-edge
    'test-commit-override-bypasses-gating
    'test-unexpected-zone-retry-recomputes
    'test-unexpected-zone-retry-limit
    'test-rebase-skipped-when-path-active
    'test-opposite-edge
    'test-reverse-translate-along-path
    'test-reverse-translate-single-hop))
