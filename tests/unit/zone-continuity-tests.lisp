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
             ;; After east crossing: new-x = dst-min-x + (626 - src-max-x)
             ;;   = 16 + (626 - 624) = 18. Y unchanged at 300.
             ;; This is seamless: only 2px from the destination edge.
             (assert (< (abs (- (player-x player) 18.0)) 1.0) ()
                     "seam-integration: player-x should be near 18, got ~,2f"
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
    'test-server-preloading-queues-adjacent))
