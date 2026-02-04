(in-package #:mmorpg)

;;;; ========================================================================
;;;; Movement Integration & Running State
;;;; ========================================================================

(defun update-running-state (player dt moving toggle-run)
  "Update stamina and return the current speed multiplier."
  (declare (type player player)
           (type single-float dt))
  (let ((old-stamina (player-run-stamina player)))
    (declare (type single-float old-stamina))
    (when toggle-run
    (if (> (player-run-stamina player) 0.0)
        (setf (player-running player) (not (player-running player)))
        (setf (player-running player) nil)))
    (if (and (player-running player) moving (> (player-run-stamina player) 0.0))
        (progn
          (decf (player-run-stamina player) dt)
          (when (<= (player-run-stamina player) 0.0)
            (setf (player-run-stamina player) 0.0
                  (player-running player) nil)))
        (when (< (player-run-stamina player) *run-stamina-max*)
          (incf (player-run-stamina player) dt)
          (when (>= (player-run-stamina player) *run-stamina-max*)
            (setf (player-run-stamina player) *run-stamina-max*))))
    (when (/= old-stamina (player-run-stamina player))
      (setf (player-snapshot-dirty player) t))
    (if (and (player-running player) (> (player-run-stamina player) 0.0))
        *run-speed-mult*
        1.0)))

(defun update-player-position (player intent world speed-mult dt)
  "Move the player with collision and target logic.
   Uses per-zone collision when available, falls back to global world collision."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type player player)
           (type intent intent)
           (type world world)
           (type single-float speed-mult dt))
  (let* ((x (player-x player))
         (y (player-y player))
         (input-dx (intent-move-dx intent))
         (input-dy (intent-move-dy intent))
         (dx 0.0)
         (dy 0.0)
         ;; Per-zone collision support
         (player-zone (player-zone-id player))
         (zone-wall-map (when player-zone (get-zone-wall-map player-zone)))
         (half-w (world-collision-half-width world))
         (half-h (world-collision-half-height world))
         (tile-size (world-tile-dest-size world)))
    (declare (type single-float x y input-dx input-dy dx dy half-w half-h tile-size))
    ;; Precompute collision bounds for clamping/zone-crossing checks
    (multiple-value-bind (min-x max-x min-y max-y)
        (if zone-wall-map
            (get-zone-collision-bounds player-zone tile-size half-w half-h)
            (values (world-wall-min-x world) (world-wall-max-x world)
                    (world-wall-min-y world) (world-wall-max-y world)))
      ;; Define move helper that uses per-zone or global collision
      (flet ((do-move (from-x from-y dir-x dir-y step)
               (if zone-wall-map
                   (attempt-move-with-map zone-wall-map from-x from-y dir-x dir-y step
                                          half-w half-h tile-size)
                   (attempt-move world from-x from-y dir-x dir-y step
                                 half-w half-h tile-size))))
        (cond
          ((or (not (zerop input-dx))
               (not (zerop input-dy)))
           (clear-intent-target intent)
           (let ((step (* *player-speed* speed-mult dt)))
             ;; Store attempted position BEFORE collision resolution
             (setf (player-attempted-x player) (+ x (* input-dx step))
                   (player-attempted-y player) (+ y (* input-dy step)))
             (multiple-value-setq (x y dx dy)
               (do-move x y input-dx input-dy step))
             ;; If movement is blocked on the axis that pushes into an edge,
             ;; force attempted past the boundary so the transition can commit.
             ;; (Handles diagonal movement that slides along a boundary.)
             (when (and min-x max-x min-y max-y)
               (let ((arm-px (* *zone-hysteresis-in* tile-size))
                     (blocked-threshold 0.5))
                 ;; Use near-zero threshold instead of (zerop) to catch cases
                 ;; where collision returns a tiny residual from wall-sliding.
                 (when (and (> input-dx 0.0) (< (abs dx) blocked-threshold) (<= (- max-x x) arm-px))
                   (setf (player-attempted-x player) (+ max-x step)
                         (player-attempted-y player) y))
                 (when (and (< input-dx 0.0) (< (abs dx) blocked-threshold) (<= (- x min-x) arm-px))
                   (setf (player-attempted-x player) (- min-x step)
                         (player-attempted-y player) y))
                 (when (and (> input-dy 0.0) (< (abs dy) blocked-threshold) (<= (- max-y y) arm-px))
                   (setf (player-attempted-y player) (+ max-y step)
                         (player-attempted-x player) x))
                 (when (and (< input-dy 0.0) (< (abs dy) blocked-threshold) (<= (- y min-y) arm-px))
                   (setf (player-attempted-y player) (- min-y step)
                         (player-attempted-x player) x))))))
          ((intent-target-active intent)
           (let* ((target-x (intent-target-x intent))
                  (target-y (intent-target-y intent))
                  (raw-x (if (intent-target-clamped-p intent)
                             (intent-target-raw-x intent)
                             target-x))
                  (raw-y (if (intent-target-clamped-p intent)
                             (intent-target-raw-y intent)
                             target-y))
                  (to-x (- target-x x))
                  (to-y (- target-y y))
                  (dist (sqrt (+ (* to-x to-x) (* to-y to-y))))
                  (crossing-target-p (and min-x max-x min-y max-y
                                          (or (< raw-x min-x) (> raw-x max-x)
                                              (< raw-y min-y) (> raw-y max-y))))
                  (clamped-crossing-p (or (intent-target-clamped-p intent)
                                          crossing-target-p)))
             (if (<= dist *target-epsilon*)
                 (if clamped-crossing-p
                     ;; Target was clamped from an out-of-bounds click.
                     ;; Keep target active, but use raw target direction to nudge
                     ;; attempted past the boundary for commit detection.
                     (let* ((raw-dx (- raw-x x))
                            (raw-dy (- raw-y y))
                            (raw-dist (sqrt (+ (* raw-dx raw-dx) (* raw-dy raw-dy)))))
                       (if (> raw-dist 0.001)
                           (let* ((rdir-x (/ raw-dx raw-dist))
                                  (rdir-y (/ raw-dy raw-dist)))
                             (setf (player-attempted-x player) raw-x
                                   (player-attempted-y player) raw-y
                                   ;; Preserve walking animation while pushing
                                   dx rdir-x
                                   dy rdir-y))
                           (setf (player-attempted-x player) x
                                 (player-attempted-y player) y
                                 dx 0.0
                                 dy 0.0)))
                     ;; Normal stationary: clear target and stop.
                     (progn
                       (setf (intent-target-active intent) nil
                             dx 0.0
                             dy 0.0)
                       (setf (player-attempted-x player) x
                             (player-attempted-y player) y)))
                 (let* ((dir-x (/ to-x dist))
                        (dir-y (/ to-y dist))
                        (step (min (* *player-speed* speed-mult dt) dist)))
                   ;; Store attempted position BEFORE collision resolution
                   (setf (player-attempted-x player) (+ x (* dir-x step))
                         (player-attempted-y player) (+ y (* dir-y step)))
                   (multiple-value-setq (x y dx dy)
                     (do-move x y dir-x dir-y step))
                   ;; If we're blocked by collision but the target was clamped
                   ;; (click beyond edge), keep target active and use raw target
                   ;; to drive attempted position across the boundary.
                   (when (and clamped-crossing-p
                              (zerop dx) (zerop dy))
                     (let* ((raw-dx (- raw-x x))
                            (raw-dy (- raw-y y))
                            (raw-dist (sqrt (+ (* raw-dx raw-dx) (* raw-dy raw-dy)))))
                       (when (> raw-dist 0.001)
                         (let ((rdir-x (/ raw-dx raw-dist))
                               (rdir-y (/ raw-dy raw-dist)))
                           (setf (player-attempted-x player) raw-x
                                 (player-attempted-y player) raw-y
                                 dx rdir-x
                                 dy rdir-y)))))
                   (when (or (and (<= dist step)
                                  (not clamped-crossing-p))
                             (and (zerop dx) (zerop dy)
                                  (not clamped-crossing-p)))
                     (setf (intent-target-active intent) nil))))))
          (t
           (setf dx 0.0
                 dy 0.0)
           ;; Stationary: attempted = actual (prevents stale values)
           (setf (player-attempted-x player) x
                 (player-attempted-y player) y))))
      ;; Clamp to zone bounds if available, otherwise global world bounds
      (setf x (clamp x min-x max-x)
            y (clamp y min-y max-y)))
    (let ((old-x (player-x player))
          (old-y (player-y player)))
      (setf (player-x player) x
            (player-y player) y
            (player-dx player) dx
            (player-dy player) dy)
      ;; Tier-2 write: position changes should be marked dirty for batched saves
      ;; Also mark snapshot-dirty for delta compression (see docs/net.md Prong 2)
      (when (or (/= old-x x) (/= old-y y))
        (setf (player-snapshot-dirty player) t)
        (mark-player-dirty (player-id player))
        ;; Update spatial grid if cell changed
        (let ((zone-state (get-zone-state player-zone)))
          (when zone-state
            (let ((grid (zone-state-player-grid zone-state)))
              (when grid
                (multiple-value-bind (new-cx new-cy changed)
                    (spatial-grid-move grid (player-id player)
                                       (player-grid-cell-x player)
                                       (player-grid-cell-y player)
                                       x y)
                  (when changed
                    (setf (player-grid-cell-x player) new-cx
                          (player-grid-cell-y player) new-cy)))))))))))


(defun player-intent-direction (player)
  ;; Return the intended movement direction for edge transitions.
  (let* ((intent (player-intent player))
         (dx (intent-move-dx intent))
         (dy (intent-move-dy intent)))
    (cond
      ((or (not (zerop dx)) (not (zerop dy)))
       (values dx dy))
      ((intent-target-active intent)
       (let ((tx (if (intent-target-clamped-p intent)
                     (intent-target-raw-x intent)
                     (intent-target-x intent)))
             (ty (if (intent-target-clamped-p intent)
                     (intent-target-raw-y intent)
                     (intent-target-y intent))))
         (normalize-vector (- tx (player-x player))
                           (- ty (player-y player)))))
      (t
       (values 0.0 0.0)))))

;;;; ========================================================================
;;;; Directional Gating (Step 3) — Only arm/transition when movement intent
;;;; has a significant component toward the zone edge.
;;;; ========================================================================

(defun edge-direction-passes-p (dx dy edge)
  "Return T if movement direction (DX, DY) has sufficient component toward EDGE.
   Uses *zone-direction-threshold* as minimum normalized dot product.
   Returns nil for zero movement vectors (stationary players never trigger)."
  (let ((mag (sqrt (+ (* dx dx) (* dy dy)))))
    (when (< mag 0.001)
      (return-from edge-direction-passes-p nil))
    (let ((dot (case edge
                 (:north (/ (- dy) mag))
                 (:south (/ dy mag))
                 (:west  (/ (- dx) mag))
                 (:east  (/ dx mag))
                 (t 0.0))))
      (>= dot *zone-direction-threshold*))))

(defun edge-direction-dot (dx dy edge)
  "Return normalized dot product of (DX, DY) with EDGE's inward normal.
   Returns 0.0 for zero-length vectors. Used to rank edges by alignment strength."
  (let ((mag (sqrt (+ (* dx dx) (* dy dy)))))
    (if (< mag 0.001)
        0.0
        (case edge
          (:north (/ (- dy) mag))
          (:south (/ dy mag))
          (:west  (/ (- dx) mag))
          (:east  (/ dx mag))
          (t 0.0)))))

;;;; ========================================================================
;;;; Arm-Band Detection (Step 2) — Determine if player is within hysteresis
;;;; arm, cancel, or commit zones relative to zone bounds.
;;;; ========================================================================

(defun player-distance-to-edge (player edge min-x max-x min-y max-y)
  "Return the player's distance inward from EDGE in pixels.
   Larger value = further from edge = deeper in zone interior."
  (let ((x (player-x player))
        (y (player-y player)))
    (case edge
      (:north (- y min-y))
      (:south (- max-y y))
      (:west  (- x min-x))
      (:east  (- max-x x))
      (t 0.0))))

(defun player-in-arm-band-p (player edge min-x max-x min-y max-y tile-dest-size)
  "Return T if PLAYER is within the arm band for EDGE (between arm line and zone edge).
   Arm line = *zone-hysteresis-in* tiles from edge."
  (let ((dist (player-distance-to-edge player edge min-x max-x min-y max-y))
        (arm-px (* *zone-hysteresis-in* tile-dest-size)))
    (<= dist arm-px)))

(defun player-past-cancel-line-p (player edge min-x max-x min-y max-y tile-dest-size)
  "Return T if PLAYER has retreated past the cancel line for EDGE.
   Cancel line = *zone-hysteresis-out* tiles from edge (deeper into interior than arm)."
  (let ((dist (player-distance-to-edge player edge min-x max-x min-y max-y))
        (cancel-px (* *zone-hysteresis-out* tile-dest-size)))
    (> dist cancel-px)))

(defun player-attempted-past-edge-p (player edge min-x max-x min-y max-y)
  "Return T if player's attempted position is past the boundary for EDGE.
   No directional gating — used for forced multi-hop commits (Bug 5/6)."
  (let ((ax (player-attempted-x player))
        (ay (player-attempted-y player)))
    (case edge
      (:north (< ay min-y))
      (:south (> ay max-y))
      (:west  (< ax min-x))
      (:east  (> ax max-x))
      (t nil))))

(defun world-exit-edge-with-bounds (player min-x max-x min-y max-y
                                    &optional (commit-margin 0.0))
  "Return the edge the player is pushing against using specified bounds.
   COMMIT-MARGIN is added to relax the boundary check so the player doesn't
   need to push through collision to trigger commit. Applies directional gating (Step 3)."
  (multiple-value-bind (dx dy)
      (player-intent-direction player)
    (let ((edge nil)
          (weight 0.0)
          (x (player-x player))
          (y (player-y player)))
      (when (and (< dy 0.0) (<= y (+ min-y commit-margin)))
        (let ((w (abs dy)))
          (when (> w weight)
            (setf edge :north
                  weight w))))
      (when (and (> dy 0.0) (>= y (- max-y commit-margin)))
        (let ((w (abs dy)))
          (when (> w weight)
            (setf edge :south
                  weight w))))
      (when (and (< dx 0.0) (<= x (+ min-x commit-margin)))
        (let ((w (abs dx)))
          (when (> w weight)
            (setf edge :west
                  weight w))))
      (when (and (> dx 0.0) (>= x (- max-x commit-margin)))
        (let ((w (abs dx)))
          (when (> w weight)
            (setf edge :east
                  weight w))))
      ;; Step 3: Directional gating — reject if movement doesn't point toward edge
      (when (and edge (not (edge-direction-passes-p dx dy edge)))
        (when *verbose-zone-transitions*
          (log-zone "Zone transition: directional gating rejected edge ~a (dx=~,2f dy=~,2f)"
                    edge dx dy))
        (setf edge nil))
      edge)))

(defun world-exit-edge (world player)
  ;; Return the edge the player is pushing against, if any.
  ;; Uses per-zone bounds when available, falls back to global world bounds.
  (let* ((player-zone (player-zone-id player))
         (tile-size (world-tile-dest-size world))
         (half-w (world-collision-half-width world))
         (half-h (world-collision-half-height world)))
    (multiple-value-bind (min-x max-x min-y max-y)
        (if (and player-zone (get-zone-wall-map player-zone))
            (get-zone-collision-bounds player-zone tile-size half-w half-h)
            (values (world-wall-min-x world) (world-wall-max-x world)
                    (world-wall-min-y world) (world-wall-max-y world)))
      (world-exit-edge-with-bounds player min-x max-x min-y max-y))))

(defun world-crossing-edge (player min-x max-x min-y max-y)
  "Return the edge the player's attempted position crosses, if any.
   Uses strict inequality: attempted must exceed the collision bound.
   Only for commit detection — preview/minimap use world-exit-edge instead."
  (multiple-value-bind (dx dy)
      (player-intent-direction player)
    (let ((ax (player-attempted-x player))
          (ay (player-attempted-y player))
          (edge nil)
          (weight 0.0))
      (declare (type single-float ax ay weight))
      (when (and (< dy 0.0) (< ay min-y))
        (let ((w (abs dy)))
          (when (> w weight) (setf edge :north weight w))))
      (when (and (> dy 0.0) (> ay max-y))
        (let ((w (abs dy)))
          (when (> w weight) (setf edge :south weight w))))
      (when (and (< dx 0.0) (< ax min-x))
        (let ((w (abs dx)))
          (when (> w weight) (setf edge :west weight w))))
      (when (and (> dx 0.0) (> ax max-x))
        (let ((w (abs dx)))
          (when (> w weight) (setf edge :east weight w))))
      ;; Directional gating (same as existing world-exit-edge-with-bounds)
      (when (and edge (not (edge-direction-passes-p dx dy edge)))
        (when *verbose-zone-transitions*
          (log-zone "Zone crossing: directional gating rejected edge ~a (dx=~,2f dy=~,2f)"
                    edge dx dy))
        (setf edge nil))
      edge)))

;;;; ========================================================================
;;;; Spawn Position Calculation
;;;; ========================================================================

(defun edge-spawn-position-bounds (min-x max-x min-y max-y spawn-edge preserve-axis ratio)
  ;; Return spawn coordinates for a target edge and optional offset.
  (let* ((spawn-x (case spawn-edge
                    (:west min-x)
                    (:east max-x)
                    (t nil)))
         (spawn-y (case spawn-edge
                    (:north min-y)
                    (:south max-y)
                    (t nil))))
    (when (eq preserve-axis :x)
      (setf spawn-x (edge-preserve-position min-x max-x ratio)))
    (when (eq preserve-axis :y)
      (setf spawn-y (edge-preserve-position min-y max-y ratio)))
    (when (null spawn-x)
      (setf spawn-x (edge-preserve-position min-x max-x 0.5)))
    (when (null spawn-y)
      (setf spawn-y (edge-preserve-position min-y max-y 0.5)))
    (values spawn-x spawn-y)))

(defun edge-spawn-position (world spawn-edge preserve-axis ratio)
  ;; Return spawn coordinates for a target edge and optional offset.
  (edge-spawn-position-bounds (world-wall-min-x world)
                              (world-wall-max-x world)
                              (world-wall-min-y world)
                              (world-wall-max-y world)
                              spawn-edge preserve-axis ratio))

;;;; ========================================================================
;;;; NPC Transition Helpers
;;;; ========================================================================

(defun npc-transition-range-sq (npc world)
  ;; Return squared perception range for transition decisions.
  (let* ((archetype (npc-archetype npc))
         (tiles (if archetype
                    (npc-archetype-perception-tiles archetype)
                    0.0))
         (range (* tiles (world-tile-dest-size world))))
    (* range range)))

(defun npc-transition-candidate-p (npc player world)
  ;; Return true when an NPC should follow across a zone edge.
  (when (npc-alive npc)
    (let* ((dx (- (player-x player) (npc-x npc)))
           (dy (- (player-y player) (npc-y npc)))
           (dist-sq (+ (* dx dx) (* dy dy)))
           (range-sq (npc-transition-range-sq npc world)))
      (and (> range-sq 0.0)
           (<= dist-sq range-sq)
           (or (npc-provoked npc)
               (member (npc-behavior-state npc)
                       '(:aggressive :retaliate :flee)))))))

(defun collect-transition-npcs (npcs player world)
  ;; Collect NPCs that should carry across zone transitions.
  (let ((entries nil)
        (px (player-x player))
        (py (player-y player)))
    (loop :for npc :across npcs
          :when (npc-transition-candidate-p npc player world)
          :do (push (list npc (- (npc-x npc) px) (- (npc-y npc) py)) entries))
    (nreverse entries)))

(defun build-carry-npc-table (entries)
  ;; Build a lookup table for NPCs carried across a zone transition.
  (let ((table (make-hash-table :test 'eq :size 32)))
    (dolist (entry entries)
      (setf (gethash (first entry) table) t))
    table))

(defun cache-zone-npcs (zone-id npcs carried-table)
  "Cache NPCs for ZONE-ID in zone-state, excluding those in CARRIED-TABLE.
   Also populates the zone's NPC spatial grid."
  (when zone-id
    (let ((zone-state (get-zone-state zone-id)))
      (when zone-state
        (if (or (null carried-table) (null npcs))
            (progn
              (setf (zone-state-npcs zone-state) npcs)
              (populate-npc-grid zone-state npcs))
            (let ((kept 0))
              (loop :for npc :across npcs
                    :unless (gethash npc carried-table)
                      :do (incf kept))
              (let ((stored (make-array kept))
                    (index 0))
                (loop :for npc :across npcs
                      :unless (gethash npc carried-table)
                        :do (setf (aref stored index) npc)
                            (incf index))
                (setf (zone-state-npcs zone-state) stored)
                (populate-npc-grid zone-state stored))))))))

(defun cached-zone-npcs (zone-id)
  ;; Return cached NPCs for ZONE-ID from zone-state, if any.
  (when zone-id
    (let ((zone-state (get-zone-state zone-id)))
      (and zone-state (zone-state-npcs zone-state)))))

(defun reposition-transition-npcs (entries player world)
  ;; Reposition carried NPCs around the player's new spawn.
  (let ((moved nil))
    (dolist (entry entries)
      (destructuring-bind (npc dx dy) entry
        (let ((target-x (+ (player-x player) dx))
              (target-y (+ (player-y player) dy)))
          (multiple-value-bind (half-w half-h)
              (npc-collision-half world)
            (multiple-value-bind (nx ny)
                (world-open-position-for world target-x target-y half-w half-h)
              (setf (npc-x npc) nx
                    (npc-y npc) ny
                    (npc-home-x npc) nx
                    (npc-home-y npc) ny
                    (npc-wander-x npc) nx
                    (npc-wander-y npc) ny)
              (reset-frame-intent (npc-intent npc))
              (push npc moved))))))
    (nreverse moved)))

(defun merge-npc-vectors (base extras)
  ;; Append EXTRAS to BASE and return a new NPC array.
  (if (null extras)
      base
      (let* ((base-count (length base))
             (extra-count (length extras))
             (result (make-array (+ base-count extra-count))))
        (replace result base)
        (loop :for npc :in extras
              :for i :from base-count
              :do (setf (aref result i) npc))
        result)))

;;; Seam translation for continuous zone transitions (PLAN_zone_transition_continuity)

(defun seam-translate-position (edge player-x player-y
                                src-min-x src-max-x src-min-y src-max-y
                                dst-min-x dst-max-x dst-min-y dst-max-y)
  "Translate player position across a zone seam using collision bounds.
   EDGE is the direction the player crossed (east/west/north/south).
   SRC-MIN/MAX and DST-MIN/MAX are collision bounds for source and destination zones.
   Maps the player's overstep distance from the source edge to the destination opposite edge.
   Returns (values new-x new-y)."
  (declare (type single-float player-x player-y
                 src-min-x src-max-x src-min-y src-max-y
                 dst-min-x dst-max-x dst-min-y dst-max-y))
  (case edge
    ;; East: player past src-max-x → appears at dst-min-x + overstep
    (:east  (values (+ dst-min-x (- player-x src-max-x)) player-y))
    ;; West: player past src-min-x → appears at dst-max-x - overstep
    (:west  (values (+ dst-max-x (- player-x src-min-x)) player-y))
    ;; North: player past src-min-y → appears at dst-max-y + overstep
    (:north (values player-x (+ dst-max-y (- player-y src-min-y))))
    ;; South: player past src-max-y → appears at dst-min-y + overstep
    (:south (values player-x (+ dst-min-y (- player-y src-max-y))))
    (t      (values player-x player-y))))

(defun seam-position-valid-p (x y min-x max-x min-y max-y)
  "Check if a seam-translated position is within destination zone bounds."
  (declare (type single-float x y min-x max-x min-y max-y))
  (and (>= x min-x) (<= x max-x)
       (>= y min-y) (<= y max-y)))

;;; ADDENDUM 1: Overstep preservation for continuous zone transitions

(defun compute-transition-overstep (edge player src-min-x src-max-x src-min-y src-max-y)
  "Compute the distance the player's attempted position extends PAST the source zone edge.
   Returns a non-negative float: 0.0 when inside the zone, positive when past the edge.
   EDGE is the direction the player is transitioning toward.
   Uses player-attempted-x/y (pre-collision intended position)."
  (declare (type single-float src-min-x src-max-x src-min-y src-max-y))
  (let ((ax (player-attempted-x player))
        (ay (player-attempted-y player)))
    (max 0.0
         (case edge
           (:north (- src-min-y ay))    ; positive when attempted-y < src-min-y (crossed north)
           (:south (- ay src-max-y))    ; positive when attempted-y > src-max-y (crossed south)
           (:west  (- src-min-x ax))    ; positive when attempted-x < src-min-x (crossed west)
           (:east  (- ax src-max-x))    ; positive when attempted-x > src-max-x (crossed east)
           (t 0.0)))))

(defun apply-overstep-to-spawn (spawn-edge overstep base-x base-y)
  "Apply overstep offset to base spawn position for continuous world-space transition.
   SPAWN-EDGE is the edge in the destination zone where the player spawns.
   OVERSTEP is subtracted from the spawn edge position, pushing the player
   inward from the destination edge by the same amount they were from the source edge."
  (declare (type single-float overstep base-x base-y))
  (case spawn-edge
    (:north (values base-x (+ base-y overstep)))   ; push down from north edge
    (:south (values base-x (- base-y overstep)))   ; push up from south edge
    (:west  (values (+ base-x overstep) base-y))   ; push right from west edge
    (:east  (values (- base-x overstep) base-y))   ; push left from east edge
    (t (values base-x base-y))))

;;;; ========================================================================
;;;; Zone Transition (Main Logic)
;;;; ========================================================================

(defun transition-zone (game player exit edge)
  ;; Apply a zone transition using EXIT metadata for the given PLAYER.
  ;; Updates player's zone-id and position. Also updates zone-state cache.
  ;; Uses player's current zone-id (not world-zone) for per-zone bounds.
  (let* ((t0 (get-internal-real-time))
         (world (game-world game))
         (intent (player-intent player))
         ;; Use player's zone-id for source zone, not world-zone (server may have stale world-zone)
         (current-zone-id (or (player-zone-id player) *starting-zone-id*))
         (current-npcs (game-npcs game))
         (graph (world-world-graph world))
         (target-id (getf exit :to))
         (target-path (and graph (world-graph-zone-path graph target-id)))
         (had-target (intent-target-active intent))
         (target-x (when had-target
                     (if (intent-target-clamped-p intent)
                         (intent-target-raw-x intent)
                         (intent-target-x intent))))
         (target-y (when had-target
                     (if (intent-target-clamped-p intent)
                         (intent-target-raw-y intent)
                         (intent-target-y intent))))
         (target-offset-x (when had-target
                            (- target-x (player-x player))))
         (target-offset-y (when had-target
                            (- target-y (player-y player))))
         (target-crossing-p nil)
         (target-rebased-x nil)
         (target-rebased-y nil)
         (carry (collect-transition-npcs current-npcs player world))
         (carry-table (and carry (build-carry-npc-table carry))))
    (when (and target-path (probe-file target-path))
      ;; Cache current zone's NPCs in zone-state (excluding carried NPCs)
      (cache-zone-npcs current-zone-id current-npcs carry-table)
      ;; Check LRU zone cache first to avoid synchronous disk load on main thread.
      ;; Preloading (Step 5) should have warmed the cache, but best-effort fallback.
      (let* ((zone-lru (game-zone-cache game))
             (cached-zone (and zone-lru (zone-cache-lookup zone-lru target-id)))
             (zone (or cached-zone
                       (progn
                         (when *verbose-zone-transitions*
                           (log-zone "Zone transition: cache MISS for ~a — synchronous disk load" target-id))
                         (let ((loaded (with-retry-exponential (result (lambda () (load-zone target-path))
                                                   :max-retries 2
                                                   :initial-delay 100
                                                   :max-delay 200
                                                   :on-final-fail (lambda (e)
                                                                    (warn "Zone transition failed: could not load zone ~a after retries: ~a"
                                                                          target-id e)))
                                         result)))
                           ;; Insert into LRU cache so next transition to this zone hits cache
                           (when (and loaded zone-lru)
                             (zone-cache-insert zone-lru target-id loaded))
                           loaded))))
             (spawn-edge (or (getf exit :spawn-edge)
                             (getf exit :to-edge)
                             (edge-opposite edge)))
             (offset (getf exit :offset))
             (preserve-axis (edge-preserve-axis edge offset))
             ;; Source zone bounds (used for ratio AND overstep computation)
             (tile-size-for-ratio (world-tile-dest-size world))
             (half-w-for-ratio (world-collision-half-width world))
             (half-h-for-ratio (world-collision-half-height world)))
        (when zone
          ;; Compute source zone collision bounds (for ratio and overstep)
          (multiple-value-bind (src-min-x src-max-x src-min-y src-max-y)
              (if (get-zone-wall-map current-zone-id)
                  (get-zone-collision-bounds current-zone-id
                                              tile-size-for-ratio
                                              half-w-for-ratio
                                              half-h-for-ratio)
                  (values (world-wall-min-x world) (world-wall-max-x world)
                          (world-wall-min-y world) (world-wall-max-y world)))
            (let ((ratio (if preserve-axis
                             (if (eq preserve-axis :x)
                                 (edge-offset-ratio src-min-x src-max-x (player-x player))
                                 (edge-offset-ratio src-min-y src-max-y (player-y player)))
                             0.5)))
          (log-zone "Zone transition: ~a -> ~a edge=~a spawn-edge=~a"
                    current-zone-id
                    (zone-id zone)
                    edge spawn-edge)
          ;; Only set *zone-path* and apply-zone-to-world for client/local mode
          ;; Server uses per-zone collision from zone-state instead
          (let* ((is-server (eq (game-net-role game) :server))
                 (target-zone-id (zone-id zone))
                 ;; Ensure zone-state exists for the target zone
                 (target-zone-state (get-or-create-zone-state target-zone-id target-path))
                 (target-wall-map (when target-zone-state (zone-state-wall-map target-zone-state)))
                 (tile-size (world-tile-dest-size world))
                 (half-w (world-collision-half-width world))
                 (half-h (world-collision-half-height world)))
            (unless is-server
              (setf *zone-path* target-path)
              (apply-zone-to-world world zone)
              ;; Call client hook to clear render caches (set by rendering.lisp)
              (when *client-zone-change-hook*
                (funcall *client-zone-change-hook* target-zone-id)))
            (setf (world-zone-label world) (zone-label zone))
            ;; Calculate spawn position using appropriate bounds
                (multiple-value-bind (new-min-x new-max-x new-min-y new-max-y)
                    (if (and is-server target-wall-map)
                        (get-zone-collision-bounds target-zone-id tile-size half-w half-h)
                        (values (world-wall-min-x world) (world-wall-max-x world)
                                (world-wall-min-y world) (world-wall-max-y world)))
              ;; PLAN_zone_transition_continuity: Seam translation as primary path
              ;; Use actual player position (collision-resolved) and push the
              ;; crossing axis just 1px past the boundary. This prevents the
              ;; old bug where raw attempted position (e.g. click target 500px
              ;; past the edge) caused the player to spawn tiles deep into
              ;; the destination zone.
              (let* ((px (player-x player))
                     (py (player-y player))
                     (overstep (compute-transition-overstep
                                edge player
                                src-min-x src-max-x src-min-y src-max-y)))
                ;; Push crossing axis just past the boundary so seam formula
                ;; produces a position inside the destination zone.
                ;; Without this, player-x (clamped at collision wall) would
                ;; produce a spawn at or outside the destination edge.
                (let ((edge-push 1.0))
                  (case edge
                    (:east  (setf px (+ src-max-x edge-push)))
                    (:west  (setf px (- src-min-x edge-push)))
                    (:north (setf py (- src-min-y edge-push)))
                    (:south (setf py (+ src-max-y edge-push)))))
                (setf target-crossing-p
                      (and had-target
                           (or (< target-x src-min-x) (> target-x src-max-x)
                               (< target-y src-min-y) (> target-y src-max-y))))
                ;; Step 1: Try seam translation (primary path)
                (multiple-value-bind (trans-x trans-y)
                    (seam-translate-position edge px py
                                            src-min-x src-max-x src-min-y src-max-y
                                            new-min-x new-max-x new-min-y new-max-y)
                  ;; Step 2: Check if translated position is in bounds and not blocked
                  (let* ((in-bounds (seam-position-valid-p trans-x trans-y
                                                          new-min-x new-max-x
                                                          new-min-y new-max-y))
                         (blocked (and in-bounds
                                       (if (and is-server target-wall-map)
                                           (blocked-at-p-with-map target-wall-map
                                                                  trans-x trans-y
                                                                  half-w half-h tile-size)
                                           nil)))
                         (seam-valid (and in-bounds (not blocked))))
                    (when *verbose-zone-transitions*
                      (log-zone "Seam translation: ~a->~a edge=~a path=~a~%  old=(~,1f, ~,1f) attempted=(~,1f, ~,1f)~%  src-bounds=(~,1f, ~,1f, ~,1f, ~,1f)~%  dst-bounds=(~,1f, ~,1f, ~,1f, ~,1f)~%  -> trans=(~,1f, ~,1f) in-bounds=~a blocked=~a overstep=~,2f"
                                current-zone-id (zone-id zone) edge
                                (if seam-valid "seam" "fallback")
                                (player-x player) (player-y player)
                                px py
                                src-min-x src-max-x src-min-y src-max-y
                                new-min-x new-max-x new-min-y new-max-y
                                trans-x trans-y in-bounds blocked overstep))
                    ;; Step 3: Use seam result if valid, otherwise fall back to ratio-spawn
                    (multiple-value-bind (raw-x raw-y fallback-reason)
                        (if seam-valid
                            (values trans-x trans-y nil)
                            ;; Fallback: ratio-based spawn + overstep (old path)
                            ;; overstep already computed above (distance past edge)
                            (let ((reason (if (not in-bounds) "out-of-bounds" "blocked")))
                              (multiple-value-bind (base-x base-y)
                                  (edge-spawn-position-bounds new-min-x new-max-x new-min-y new-max-y
                                                              spawn-edge preserve-axis ratio)
                                (multiple-value-bind (fb-x fb-y)
                                    (apply-overstep-to-spawn spawn-edge overstep base-x base-y)
                                  (when *verbose-zone-transitions*
                                    (log-zone "Seam fallback: reason=~a overstep=~,2f spawn-edge=~a base=(~,1f,~,1f) result=(~,1f,~,1f)"
                                              reason overstep spawn-edge base-x base-y fb-x fb-y))
                                  (values fb-x fb-y reason)))))
                (multiple-value-bind (spawn-x spawn-y)
                    (if (and is-server target-wall-map)
                        (find-open-position-with-map target-wall-map raw-x raw-y
                                                     half-w half-h tile-size)
                        (world-open-position-for world raw-x raw-y half-w half-h))
                  (when *verbose-zone-transitions*
                    (log-zone "Seam final: path=~a raw=(~,1f,~,1f) spawn=(~,1f,~,1f)~@[ fallback-reason=~a~]"
                              (if fallback-reason "fallback" "seam")
                              raw-x raw-y spawn-x spawn-y fallback-reason))
                  ;; Remove player from old zone's spatial grid and zone-players cache
                  (let ((old-zone-state (get-zone-state current-zone-id)))
                    (when old-zone-state
                      ;; Remove from zone-players cache (Task 4.1)
                      (remove-player-from-zone-cache player old-zone-state)
                      ;; Remove from spatial grid
                      (let ((old-grid (zone-state-player-grid old-zone-state)))
                        (when (and old-grid
                                   (player-grid-cell-x player)
                                   (player-grid-cell-y player))
                          (spatial-grid-remove old-grid (player-id player)
                                               (player-grid-cell-x player)
                                               (player-grid-cell-y player))))))
                  ;; Round spawn to integer coords so camera snap formula
                  ;; (fround * zoom / zoom) produces consistent rounding,
                  ;; avoiding a visible 1px tile shift on zone transition.
                  (setf (player-x player) (fround spawn-x)
                        (player-y player) (fround spawn-y)
                        (player-dx player) 0.0
                        (player-dy player) 0.0
                        (player-zone-id player) target-zone-id
                        (player-snapshot-dirty player) t)
                  (when had-target
                    (if target-crossing-p
                        ;; Target was in the adjacent zone. Translate it across the seam
                        ;; so the player walks to the intended location and stops.
                        (multiple-value-bind (tx ty)
                            (seam-translate-position edge target-x target-y
                                                     src-min-x src-max-x src-min-y src-max-y
                                                     new-min-x new-max-x new-min-y new-max-y)
                          (setf target-rebased-x (clamp tx new-min-x new-max-x)
                                target-rebased-y (clamp ty new-min-y new-max-y)))
                        ;; Target was inside the current zone: preserve relative offset.
                        ;; Clamp to destination zone bounds to prevent unreachable targets.
                        (setf target-rebased-x (clamp (+ (player-x player) target-offset-x)
                                                      new-min-x new-max-x)
                              target-rebased-y (clamp (+ (player-y player) target-offset-y)
                                                      new-min-y new-max-y))))
                  ;; Insert player into new zone's spatial grid and zone-players cache
                  (when target-zone-state
                    ;; Add to zone-players cache (Task 4.1)
                    (add-player-to-zone-cache player target-zone-state)
                    ;; Add to spatial grid
                    (let ((new-grid (zone-state-player-grid target-zone-state)))
                      (when new-grid
                        (multiple-value-bind (cx cy)
                            (position-to-cell spawn-x spawn-y (spatial-grid-cell-size new-grid))
                          (spatial-grid-insert new-grid (player-id player) spawn-x spawn-y)
                          (setf (player-grid-cell-x player) cx
                                (player-grid-cell-y player) cy)))))
                  ;; Step 11: Downgrade to Tier-2 (dirty flag) — flushes within 30s checkpoint.
                  ;; Zone transitions are not critical enough for Tier-1 blocking save.
                  ;; Crash between transition and checkpoint → player reverts to pre-transition zone (acceptable).
                  (mark-player-dirty (player-id player))))))))) ; close mvb-spawn, mvb-raw, let*-seam, mvb-trans, let*-src, mvb-new-min
          ;; Step 1: Set transition cooldown
          (setf (player-zone-transition-cooldown player)
                *zone-transition-cooldown-seconds*)
          (when *verbose-zone-transitions*
            (log-zone "Zone transition: cooldown started (~,2fs) for player ~a"
                      *zone-transition-cooldown-seconds* (player-id player)))
          ;; Step 2: Clear pending transition
          (setf (player-zone-transition-pending player) nil)
          (reset-frame-intent-preserving-movement intent)
          (when had-target
            (set-intent-target intent target-rebased-x target-rebased-y)
            ;; After transition, treat target as in-bounds for the new zone.
            (setf (intent-target-raw-x intent) (intent-target-x intent)
                  (intent-target-raw-y intent) (intent-target-y intent)
                  (intent-target-clamped-p intent) nil))
          (setf (player-attacking player) nil
                (player-attack-hit player) nil
                    (player-attack-timer player) 0.0)
          (setf (world-minimap-dirty world) t)
          (let* ((players (game-players game))
                 (target-zone-id (and zone (zone-id zone)))
                 (cached (cached-zone-npcs target-zone-id))
                 (npcs (or cached
                           (make-npcs player world
                                      :id-source (game-npc-id-source game))))
                 (carried (reposition-transition-npcs carry player world)))
            ;; Update session zone-id for persistence
            (update-player-session-zone (player-id player) target-zone-id)
            (ensure-npcs-open-spawn npcs world)
            (let ((merged (merge-npc-vectors npcs carried)))
              (setf (game-npcs game) merged
                    (game-entities game) (make-entities players merged))
              ;; Update zone-state cache with NPCs for zone-filtered snapshots
              (when target-zone-id
                ;; Phase 2 perf: Use array-backed spatial grids
                (let* ((tile-dest-size (* (float *tile-size* 1.0) *tile-scale*))
                       (zone-w (or (zone-width zone) 64))
                       (zone-h (or (zone-height zone) 64))
                       (target-state (or (get-zone-state target-zone-id)
                                         (make-zone-state
                                          :zone-id target-zone-id
                                          :zone zone
                                          :wall-map (zone-wall-map zone)
                                          :objects (zone-objects zone)
                                          :player-grid (make-spatial-grid-for-zone zone-w zone-h tile-dest-size)
                                          :npc-grid (make-spatial-grid-for-zone zone-w zone-h tile-dest-size)))))
                  (setf (zone-state-npcs target-state) merged)
                  ;; Populate NPC spatial grid for proximity queries
                  (populate-npc-grid target-state merged)
                  (setf (gethash target-zone-id *zone-states*) target-state)))))
          ;; Log per-transition timing (verbose only)
          (when *verbose-zone-transitions*
            (let* ((t1 (get-internal-real-time))
                   (elapsed-ms (* (/ (float (- t1 t0) 1.0)
                                     (float internal-time-units-per-second 1.0))
                                  1000.0)))
              (log-zone "Zone transition: elapsed ~,1fms (player ~a, cache-~a)"
                        elapsed-ms (player-id player)
                        (if cached-zone "HIT" "MISS"))))
          t)))))))

;;;; ========================================================================
;;;; Zone Transition Update (Per-Tick)
;;;; ========================================================================

(defun update-zone-transition (game &optional (dt *sim-tick-seconds*))
  "Handle edge-based zone transitions with cooldown, hysteresis, and directional gating.
   Each player is checked against their own zone's exits using per-zone collision bounds.
   Two-phase arm/commit/cancel model:
   - Arm: player crosses arm line toward edge → set pending (no transition yet)
   - Commit: player with pending reaches zone edge → transition fires
   - Cancel: player retreats past cancel line → pending cleared
   Returns count of players that transitioned this frame."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type game game)
           (type single-float dt))
  ;; Config invariant: cancel line must be further from edge than arm line
  (assert (> *zone-hysteresis-out* *zone-hysteresis-in*)
          (*zone-hysteresis-out* *zone-hysteresis-in*)
          "Zone config invariant violated: *zone-hysteresis-out* (~a) must be > *zone-hysteresis-in* (~a)")
  (let* ((world (game-world game))
         (players (game-players game))
         (transition-count 0))
    (declare (type (or null world) world)
             (type (or null (simple-array t (*))) players)
             (type fixnum transition-count))
    (when (and world players (> (length players) 0))
      (loop :for player :across players
            :when player
            :do (block per-player
                  (let* ((player-zone-id (or (player-zone-id player) *starting-zone-id*))
                         (zone-path (zone-path-for-id world player-zone-id))
                         (_zone-state (when zone-path
                                        (get-or-create-zone-state player-zone-id zone-path)))
                         (tile-size (world-tile-dest-size world))
                         (half-w (world-collision-half-width world))
                         (half-h (world-collision-half-height world)))
                    (declare (ignore _zone-state))
                    (declare (type single-float tile-size half-w half-h))
                    ;; Step 1: Cooldown — decrement and skip if active
                    (when (> (player-zone-transition-cooldown player) 0.0)
                      (decf (player-zone-transition-cooldown player) dt)
                      (when (> (player-zone-transition-cooldown player) 0.0)
                        (return-from per-player)))
                    ;; Get per-zone bounds
                    (multiple-value-bind (min-x max-x min-y max-y)
                        (if (and player-zone-id (get-zone-wall-map player-zone-id))
                            (get-zone-collision-bounds player-zone-id tile-size half-w half-h)
                            (values (world-wall-min-x world) (world-wall-max-x world)
                                    (world-wall-min-y world) (world-wall-max-y world)))
                      (declare (type single-float min-x max-x min-y max-y))
                      (when (and min-x max-x min-y max-y)
                        (let ((pending (player-zone-transition-pending player)))
                          (declare (type (or null keyword) pending))
                          (cond
                            ;; === PENDING SET: evaluate commit/cancel ===
                            (pending
                             (let ((exit (world-edge-exit-for-zone world player-zone-id pending)))
                               (cond
                                 ;; Cancel: retreated past cancel line
                                 ((player-past-cancel-line-p player pending min-x max-x min-y max-y tile-size)
                                  (when *verbose-zone-transitions*
                                    (log-zone "Zone transition: cancel for edge ~a (player ~a retreated past cancel line)"
                                              pending (player-id player)))
                                  (setf (player-zone-transition-pending player) nil))
                                 ;; Cancel: no exit for this edge
                                 ((null exit)
                                  (setf (player-zone-transition-pending player) nil))
                                 ;; Cancel: intent dropped below direction threshold
                                 ;; Bug 5/6: Skip directional cancel when pending matches
                                 ;; stored edge from multi-hop path — stored edge is authoritative.
                                 ((and (multiple-value-bind (dx dy) (player-intent-direction player)
                                         (not (edge-direction-passes-p dx dy pending)))
                                       (let ((stored-edge (and (game-zone-click-edges game)
                                                               (first (game-zone-click-edges game)))))
                                         (not (and stored-edge
                                                   (game-zone-click-path game)
                                                   (eq pending stored-edge)))))
                                  (when *verbose-zone-transitions*
                                    (log-zone "Zone transition: cancel for edge ~a (intent dropped)" pending))
                                  (setf (player-zone-transition-pending player) nil))
                                 ;; Commit or edge-change: check if attempted position
                                 ;; crosses the zone boundary (strict inequality)
                                 (t
                                  (let* ((actual-edge (world-crossing-edge
                                                       player min-x max-x min-y max-y))
                                         ;; Bug 5/6: Force commit when stored edge matches
                                         ;; pending and attempted position is past the edge,
                                         ;; even if world-crossing-edge disagrees due to
                                         ;; directional gating picking a different axis.
                                         (stored-edge (and (game-zone-click-edges game)
                                                           (first (game-zone-click-edges game))))
                                         (force-commit-p (and stored-edge
                                                              (game-zone-click-path game)
                                                              (eq pending stored-edge)
                                                              (player-attempted-past-edge-p
                                                                player pending
                                                                min-x max-x min-y max-y))))
                                    (cond
                                      ;; Commit: at the pending edge (or forced by stored path)
                                      ((or (eq actual-edge pending) force-commit-p)
                                       ;; Client cache gate: defer commit if target zone
                                       ;; is not yet cached (avoids sync disk load hitch).
                                       ;; Server always commits immediately.
                                       (let* ((target-id (getf exit :to))
                                              (zone-lru (game-zone-cache game))
                                              (is-client (not (eq (game-net-role game) :server)))
                                              (cached (or (not is-client)
                                                         (not zone-lru)
                                                         (zone-cache-lookup zone-lru target-id))))
                                         (if (not cached)
                                             ;; Target zone not cached on client — defer commit,
                                             ;; keep pending so preloader can warm the cache.
                                             (when *verbose-zone-transitions*
                                               (log-zone "Zone transition: DEFERRED edge=~a (zone ~a not cached)"
                                                         pending target-id))
                                             ;; Proceed with commit
                                             (progn
                                               (let ((now (/ (float (get-internal-real-time) 1.0)
                                                             (float internal-time-units-per-second 1.0))))
                                                 (let* ((last (player-zone-transition-last-time player))
                                                        (time-since (if (> last 0.0) (- now last) -1.0)))
                                                   (log-zone "Zone transition: COMMIT edge=~a player=~a time-since-last=~,2fs"
                                                             pending (player-id player) time-since))
                                                 (setf (player-zone-transition-last-time player) now))
                                               (transition-zone game player exit pending)
                                               (incf transition-count)))))
                                      ;; Edge changed: player now at a different edge → clear pending
                                      (actual-edge
                                       (when *verbose-zone-transitions*
                                         (log-zone "Zone transition: cancel ~a (edge changed to ~a)"
                                                   pending actual-edge))
                                       (setf (player-zone-transition-pending player) nil))
                                      ;; No edge detected: still in arm band, waiting
                                      (t nil)))))))
                            ;; === NO PENDING: check for arm ===
                            (t
                             ;; Bug 5/6: When a multi-hop path is active, force-arm the stored edge
                             ;; instead of relying on directional alignment which can pick the wrong axis.
                             (let* ((stored-edge (and (game-zone-click-edges game)
                                                      (first (game-zone-click-edges game))))
                                    (force-arm-p (and stored-edge
                                                      (game-zone-click-path game)
                                                      (world-edge-exit-for-zone world player-zone-id stored-edge)
                                                      (player-in-arm-band-p player stored-edge
                                                                            min-x max-x min-y max-y tile-size))))
                               (if force-arm-p
                                   (progn
                                     (setf (player-zone-transition-pending player) stored-edge)
                                     (log-verbose "Multi-hop: force-arm edge ~a" stored-edge))
                                   ;; No stored edge or not in arm band — fall through to existing logic
                                   ;; Pick the edge with strongest directional alignment
                                   ;; (dominant direction), not first match from dolist.
                                   (let ((graph (world-world-graph world)))
                                     (when graph
                                       (multiple-value-bind (dx dy) (player-intent-direction player)
                                         (let ((best-edge nil)
                                               (best-dot 0.0))
                                           (declare (type single-float best-dot))
                                           (dolist (exit-spec (world-graph-exits graph player-zone-id))
                                             (let ((edge (getf exit-spec :edge)))
                                               (when (and edge
                                                          (player-in-arm-band-p player edge
                                                                                min-x max-x min-y max-y tile-size)
                                                          (edge-direction-passes-p dx dy edge))
                                                 ;; Rank by dot product with edge normal
                                                 (let ((dot (edge-direction-dot dx dy edge)))
                                                   (when (> dot best-dot)
                                                     (setf best-edge edge
                                                           best-dot dot))))))
                                           (when best-edge
                                             (when *verbose-zone-transitions*
                                               (log-zone "Zone transition: ARM edge ~a for player ~a (dot=~,2f)"
                                                         best-edge (player-id player) best-dot))
                                             (setf (player-zone-transition-pending player) best-edge)))))))))))))))))
    transition-count))

;;;; ========================================================================
;;;; Debug Logging & Unstuck System
;;;; ========================================================================

(defun log-player-position (player world)
  ;; Emit verbose position and tile diagnostics for debugging.
  (let* ((x (player-x player))
         (y (player-y player))
         (tile-dest-size (world-tile-dest-size world))
         (tile-x (floor x tile-dest-size))
         (tile-y (floor y tile-dest-size))
         (feet-x x)
         (feet-y (+ y (world-collision-half-height world)))
         (feet-tile-x (floor feet-x tile-dest-size))
         (feet-tile-y (floor feet-y tile-dest-size)))
    (format t "~&pos=~,2f,~,2f center=~,2f,~,2f tile=~d,~d feet=~,2f,~,2f tile-feet=~d,~d~%"
            x y
            x y
            tile-x tile-y
            feet-x feet-y
            feet-tile-x feet-tile-y)
    (finish-output)))

;;;; Player Unstuck System

(defun player-is-stuck-p (player world)
  ;; Return T if player cannot move in any cardinal direction.
  ;; Tests small movements in all 4 directions against collision.
  (let* ((px (player-x player))
         (py (player-y player))
         (test-dist 2.0)
         (half-w (world-collision-half-width world))
         (half-h (world-collision-half-height world)))
    (and (position-blocked-p world (+ px test-dist) py half-w half-h)
         (position-blocked-p world (- px test-dist) py half-w half-h)
         (position-blocked-p world px (+ py test-dist) half-w half-h)
         (position-blocked-p world px (- py test-dist) half-w half-h))))

(defun player-is-stuck-p-for-zone (player zone-id world)
  "Return T if player cannot move in any cardinal direction.
   Uses zone-state wall-map for collision, falling back to world if zone not loaded."
  (let* ((px (player-x player))
         (py (player-y player))
         (test-dist 2.0)
         (half-w (world-collision-half-width world))
         (half-h (world-collision-half-height world))
         (tile-size (world-tile-dest-size world))
         (wall-map (get-zone-wall-map zone-id)))
    (if wall-map
        ;; Use zone-specific wall-map
        (and (blocked-at-p-with-map wall-map (+ px test-dist) py half-w half-h tile-size)
             (blocked-at-p-with-map wall-map (- px test-dist) py half-w half-h tile-size)
             (blocked-at-p-with-map wall-map px (+ py test-dist) half-w half-h tile-size)
             (blocked-at-p-with-map wall-map px (- py test-dist) half-w half-h tile-size))
        ;; Fallback to global world collision
        (player-is-stuck-p player world))))

(defun get-zone-safe-spawn (world)
  ;; Return a random position within the zone's playable bounds.
  ;; If the random spot is blocked, world-open-position will find the nearest open tile.
  (let* ((min-x (world-wall-min-x world))
         (max-x (world-wall-max-x world))
         (min-y (world-wall-min-y world))
         (max-y (world-wall-max-y world))
         (rand-x (+ min-x (random (max 1.0 (- max-x min-x)))))
         (rand-y (+ min-y (random (max 1.0 (- max-y min-y))))))
    (values rand-x rand-y)))

(defun get-zone-safe-spawn-for-zone (zone-id world)
  "Return a random position within ZONE-ID's playable bounds.
   Falls back to global world bounds if zone not loaded."
  (let* ((tile-size (world-tile-dest-size world))
         (half-w (world-collision-half-width world))
         (half-h (world-collision-half-height world)))
    (multiple-value-bind (min-x max-x min-y max-y)
        (get-zone-collision-bounds zone-id tile-size half-w half-h)
      (if min-x
          ;; Use zone-specific bounds
          (let ((rand-x (+ min-x (random (max 1.0 (- max-x min-x)))))
                (rand-y (+ min-y (random (max 1.0 (- max-y min-y))))))
            (values rand-x rand-y))
          ;; Fallback to global world bounds
          (get-zone-safe-spawn world)))))

(defun process-player-unstuck (player intent world zone-id &optional event-queue)
  ;; Handle client unstuck request (server authority).
  ;; Uses player's zone-id for per-zone collision checking.
  ;; NOTE: Player's own zone-id takes precedence over the passed zone-id parameter
  ;; to ensure multi-zone correctness when global world-zone differs.
  (when (intent-requested-unstuck intent)
    (let* ((player-zone (or (player-zone-id player) zone-id *starting-zone-id*))
           (wall-map (get-zone-wall-map player-zone))
           (tile-size (world-tile-dest-size world))
           (half-w (world-collision-half-width world))
           (half-h (world-collision-half-height world)))
      (if (player-is-stuck-p-for-zone player player-zone world)
          ;; Player is stuck - teleport to random position in their zone
          (multiple-value-bind (safe-x safe-y)
              (get-zone-safe-spawn-for-zone player-zone world)
            (multiple-value-bind (final-x final-y)
                (if wall-map
                    (find-open-position-with-map wall-map safe-x safe-y
                                                 half-w half-h tile-size)
                    (world-open-position world safe-x safe-y))
              (setf (player-x player) final-x
                    (player-y player) final-y
                    (player-dx player) 0.0
                    (player-dy player) 0.0
                    (player-snapshot-dirty player) t
                    ;; Force full resync so client gets complete snapshot after teleport
                    ;; This prevents frozen sprites from stale interpolation buffers
                    (player-force-full-resync player) t)
              (mark-player-dirty (player-id player))
              (log-verbose "Player ~a unstuck in zone ~a, teleported to (~,1f, ~,1f)"
                           (player-id player) player-zone final-x final-y)
              (when event-queue
                (emit-hud-message-event event-queue "Teleported to safe location."))))
          ;; Player not stuck - deny free teleport
          (progn
            (log-verbose "Player ~a unstuck request denied - not stuck"
                         (player-id player))
            (when event-queue
              (emit-hud-message-event event-queue "You don't appear to be stuck.")))))
    (clear-requested-unstuck intent)))
