;; NOTE: If you change behavior here, update docs/movement.md :)
(in-package #:mmorpg)

(defun build-wall-map ()
  ;; Create a test wall map array with a solid border.
  (let* ((width *wall-map-width*)
         (height *wall-map-height*)
         (map (make-array (list height width) :initial-element 0)))
    (labels ((set-wall (x y)
               (when (and (<= 0 x) (< x width)
                          (<= 0 y) (< y height))
                 (setf (aref map y x) 1))))
      (loop :for x :from 0 :below width
            :do (set-wall x 0)
                (set-wall x (1- height)))
      (loop :for y :from 0 :below height
            :do (set-wall 0 y)
                (set-wall (1- width) y)))
    map))

(defun wall-occupied-p (wall-map tx ty)
  ;; Check whether a tile inside the wall map is nonzero.
  (when wall-map
    (let* ((local-x (- tx *wall-origin-x*))
           (local-y (- ty *wall-origin-y*))
           (width (array-dimension wall-map 1))
           (height (array-dimension wall-map 0)))
      (and (<= 0 local-x)
           (< local-x width)
           (<= 0 local-y)
           (< local-y height)
           (not (zerop (aref wall-map local-y local-x)))))))

(defun wall-blocked-p (wall-map tx ty)
  ;; Treat walls and out-of-bounds tiles as blocked for collision.
  (if (not wall-map)
      nil
      (let* ((local-x (- tx *wall-origin-x*))
             (local-y (- ty *wall-origin-y*))
             (width (array-dimension wall-map 1))
             (height (array-dimension wall-map 0)))
        (if (or (< local-x 0)
                (>= local-x width)
                (< local-y 0)
                (>= local-y height))
            t
            (not (zerop (aref wall-map local-y local-x)))))))

(defun world-blocked-tile-p (world tx ty)
  ;; Return true when a tile coordinate blocks movement.
  (wall-blocked-p (world-wall-map world) tx ty))

(defun set-world-blocked-tile (world tx ty value)
  ;; Update the world wall map at TX/TY for editor changes.
  (let* ((wall-map (world-wall-map world))
         (local-x (- tx *wall-origin-x*))
         (local-y (- ty *wall-origin-y*)))
    (when (and wall-map
               (<= 0 local-x)
               (<= 0 local-y)
               (< local-x (array-dimension wall-map 1))
               (< local-y (array-dimension wall-map 0)))
      (let* ((next (if (and value (not (zerop value))) 1 0))
             (prev (aref wall-map local-y local-x)))
        (when (/= prev next)
          (setf (aref wall-map local-y local-x) next)
          (setf (world-minimap-collisions world)
                (build-minimap-collisions world)))))))

(defun world-search-radius (world)
  ;; Return a max search radius in tiles for open spawn placement.
  (max (world-wall-map-width world) (world-wall-map-height world)))

(defun tile-center-position (tile-size tx ty)
  ;; Return the world position for the center of tile TX/TY.
  (values (+ (* (+ tx 0.5) tile-size))
          (+ (* (+ ty 0.5) tile-size))))

(defun edge-zone-offset (edge span-x span-y)
  ;; Return world offset to the neighboring zone along EDGE.
  (ecase edge
    (:north (values 0.0 (- span-y)))
    (:south (values 0.0 span-y))
    (:east (values span-x 0.0))
    (:west (values (- span-x) 0.0))))

(defun zone-bounds-from-dimensions (tile-dest-size width height collision-half-width collision-half-height)
  ;; Return wall bounds for a zone with WIDTH/HEIGHT in tiles.
  (let* ((wall-min-x (+ (* (+ *wall-origin-x* 1) tile-dest-size)
                        collision-half-width))
         (wall-max-x (- (* (+ *wall-origin-x* (1- width))
                           tile-dest-size)
                        collision-half-width))
         (wall-min-y (+ (* (+ *wall-origin-y* 1) tile-dest-size)
                        collision-half-height))
         (wall-max-y (- (* (+ *wall-origin-y* (1- height))
                           tile-dest-size)
                        collision-half-height)))
    (values wall-min-x wall-max-x wall-min-y wall-max-y)))

(defun build-adjacent-minimap-spawns (world &optional player)
  ;; Build cached spawn positions for adjacent zones on the minimap.
  (let* ((graph (world-world-graph world))
         (zone (world-zone world))
         (zone-id (and zone (zone-id zone)))
         (tile-size (world-tile-dest-size world))
         (min-x (world-wall-min-x world))
         (max-x (world-wall-max-x world))
         (min-y (world-wall-min-y world))
         (max-y (world-wall-max-y world))
         (span-x (max 1.0 (- max-x min-x)))
         (span-y (max 1.0 (- max-y min-y)))
         (spawns nil))
    (when (and graph zone-id)
      (dolist (exit (world-graph-exits graph zone-id))
        (let* ((edge (getf exit :edge))
               (target-id (getf exit :to))
               (target-path (and edge target-id
                                 (world-graph-zone-path graph target-id))))
          (when (and edge target-path (probe-file target-path))
            (multiple-value-bind (offset-x offset-y)
                (edge-zone-offset edge span-x span-y)
              (let* ((data (read-zone-data target-path))
                     (plist (zone-data-plist data))
                     (zone-spawns (getf plist :spawns nil)))
                (when (and zone-spawns (not (null zone-spawns)))
                  (dolist (spawn zone-spawns)
                    (let* ((tx (getf spawn :x))
                           (ty (getf spawn :y))
                           (count (max 1 (getf spawn :count 1))))
                      (when (and (numberp tx) (numberp ty))
                        (multiple-value-bind (cx cy)
                            (tile-center-position tile-size tx ty)
                          (loop :repeat count
                                :do (push (list edge
                                                (+ cx offset-x)
                                                (+ cy offset-y))
                                          spawns)))))))))))))
    (nreverse spawns)))

(defun build-minimap-collisions (world)
  ;; Build cached collision marker positions for the minimap.
  (let* ((wall-map (world-wall-map world)))
    (if (not wall-map)
        (make-array 0)
        (let* ((height (array-dimension wall-map 0))
               (width (array-dimension wall-map 1))
               (zone (world-zone world))
               (zone-id (and zone (zone-id zone)))
               (graph (world-world-graph world))
               (exits (and graph zone-id (world-graph-exits graph zone-id)))
               (tile-size (world-tile-dest-size world))
               (origin-x *wall-origin-x*)
               (origin-y *wall-origin-y*)
               (count 0)
               (perimeter-count 0))
          (when (and (> width 0) (> height 0))
            (labels ((edge-exit-p (edge)
                       (and exits
                            (find edge exits
                                  :key (lambda (exit) (getf exit :edge))
                                  :test #'eq)))
                     (count-perimeter (tx ty)
                       (when (zerop (aref wall-map ty tx))
                         (incf perimeter-count))))
              (let* ((show-north (not (edge-exit-p :north)))
                     (show-south (not (edge-exit-p :south)))
                     (show-west (not (edge-exit-p :west)))
                     (show-east (not (edge-exit-p :east)))
                     (show-top (if (= height 1)
                                   (or show-north show-south)
                                   show-north))
                     (show-bottom (and (> height 1) show-south))
                     (show-left (if (= width 1)
                                    (or show-west show-east)
                                    show-west))
                     (show-right (and (> width 1) show-east)))
                (when show-top
                  (loop :for tx :from 0 :below width
                        :do (count-perimeter tx 0)))
                (when show-bottom
                  (loop :for tx :from 0 :below width
                        :do (count-perimeter tx (1- height))))
                (when show-left
                  (let* ((start-ty (if show-top 1 0))
                         (end-ty (if show-bottom (- height 2) (1- height))))
                    (when (<= start-ty end-ty)
                      (loop :for ty :from start-ty :to end-ty
                            :do (count-perimeter 0 ty)))))
                (when show-right
                  (let* ((start-ty (if show-top 1 0))
                         (end-ty (if show-bottom (- height 2) (1- height)))
                         (tx (1- width)))
                    (when (<= start-ty end-ty)
                      (loop :for ty :from start-ty :to end-ty
                            :do (count-perimeter tx ty))))))))
          (loop :for ty :from 0 :below height
                :do (loop :for tx :from 0 :below width
                          :when (not (zerop (aref wall-map ty tx)))
                            :do (incf count)))
          (let ((points (make-array (* 2 (+ count perimeter-count))))
                (index 0))
            (when (and (> width 0) (> height 0))
              (labels ((edge-exit-p (edge)
                         (and exits
                              (find edge exits
                                    :key (lambda (exit) (getf exit :edge))
                                    :test #'eq)))
                       (store-perimeter (tx ty)
                         (when (zerop (aref wall-map ty tx))
                           (multiple-value-bind (cx cy)
                               (tile-center-position tile-size
                                                     (+ tx origin-x)
                                                     (+ ty origin-y))
                             (setf (aref points index) cx
                                   (aref points (1+ index)) cy)
                             (incf index 2)))))
                (let* ((show-north (not (edge-exit-p :north)))
                       (show-south (not (edge-exit-p :south)))
                       (show-west (not (edge-exit-p :west)))
                       (show-east (not (edge-exit-p :east)))
                       (show-top (if (= height 1)
                                     (or show-north show-south)
                                     show-north))
                       (show-bottom (and (> height 1) show-south))
                       (show-left (if (= width 1)
                                      (or show-west show-east)
                                      show-west))
                       (show-right (and (> width 1) show-east)))
                  (when show-top
                    (loop :for tx :from 0 :below width
                          :do (store-perimeter tx 0)))
                  (when show-bottom
                    (loop :for tx :from 0 :below width
                          :do (store-perimeter tx (1- height))))
                  (when show-left
                    (let* ((start-ty (if show-top 1 0))
                           (end-ty (if show-bottom (- height 2) (1- height))))
                      (when (<= start-ty end-ty)
                        (loop :for ty :from start-ty :to end-ty
                              :do (store-perimeter 0 ty)))))
                  (when show-right
                    (let* ((start-ty (if show-top 1 0))
                           (end-ty (if show-bottom (- height 2) (1- height)))
                           (tx (1- width)))
                      (when (<= start-ty end-ty)
                        (loop :for ty :from start-ty :to end-ty
                              :do (store-perimeter tx ty))))))))
            (loop :for ty :from 0 :below height
                  :do (loop :for tx :from 0 :below width
                            :when (not (zerop (aref wall-map ty tx)))
                              :do (multiple-value-bind (cx cy)
                                      (tile-center-position tile-size
                                                            (+ tx origin-x)
                                                            (+ ty origin-y))
                                    (setf (aref points index) cx
                                          (aref points (1+ index)) cy)
                                    (incf index 2))))
            points)))))

(defun position-blocked-p (world x y half-w half-h)
  ;; Return true when a collider centered at X/Y is blocked.
  (blocked-at-p world x y half-w half-h (world-tile-dest-size world)))

(defun find-open-tile (world start-tx start-ty half-w half-h &optional max-radius)
  ;; Return the nearest open tile around START-TX/START-TY.
  (let ((max-radius (max 0 (or max-radius 0)))
        (tile-size (world-tile-dest-size world)))
    (loop :for radius :from 0 :to max-radius
          :do (loop :for dy :from (- radius) :to radius
                    :do (loop :for dx :from (- radius) :to radius
                              :for tx = (+ start-tx dx)
                              :for ty = (+ start-ty dy)
                              :do (multiple-value-bind (cx cy)
                                      (tile-center-position tile-size tx ty)
                                    (when (not (position-blocked-p world cx cy half-w half-h))
                                      (return-from find-open-tile
                                        (values tx ty)))))))
  (values start-tx start-ty)))

(defun world-open-position-for (world x y half-w half-h)
  ;; Return the nearest open tile center for a collider near X/Y.
  (let* ((tile-size (world-tile-dest-size world))
         (tx (floor x tile-size))
         (ty (floor y tile-size))
         (radius (world-search-radius world)))
    (multiple-value-bind (open-tx open-ty)
        (find-open-tile world tx ty half-w half-h radius)
      (tile-center-position tile-size open-tx open-ty))))

(defun world-open-position (world x y)
  ;; Return the nearest open tile center for the player collider.
  (world-open-position-for world x y
                           (world-collision-half-width world)
                           (world-collision-half-height world)))

(defun ensure-npcs-open-spawn (npcs world)
  ;; Move NPCs to open tiles and reset their home positions.
  (loop :for npc :across npcs
        :do (multiple-value-bind (half-w half-h)
                (npc-collision-half world)
              (multiple-value-bind (nx ny)
                  (world-open-position-for world (npc-x npc) (npc-y npc)
                                           half-w half-h)
                (setf (npc-x npc) nx
                      (npc-y npc) ny
                      (npc-home-x npc) nx
                      (npc-home-y npc) ny
                      (npc-wander-x npc) nx
                      (npc-wander-y npc) ny)))))

(defun blocked-at-p (world x y half-w half-h tile-size)
  ;; Test collider bounds against blocked tiles in the world map.
  (let* ((left (- x half-w))
         (right (+ x half-w))
         (top (- y half-h))
         (bottom (+ y half-h))
         (right-edge (- right *collision-edge-epsilon*))
         (bottom-edge (- bottom *collision-edge-epsilon*))
         (tx1 (floor left tile-size))
         (tx2 (floor right-edge tile-size))
         (ty1 (floor top tile-size))
         (ty2 (floor bottom-edge tile-size)))
    (loop :for ty :from ty1 :to ty2
          :thereis (loop :for tx :from tx1 :to tx2
                         :thereis (world-blocked-tile-p world tx ty)))))

(defun attempt-move (world x y dx dy step half-w half-h tile-size)
  ;; Resolve movement per axis and cancel movement when blocked.
  (let ((nx x)
        (ny y)
        (out-dx 0.0)
        (out-dy 0.0))
    (when (not (zerop dx))
      (let ((try-x (+ x (* dx step))))
        (if (blocked-at-p world try-x y half-w half-h tile-size)
            (setf out-dx 0.0)
            (setf nx try-x
                  out-dx dx))))
    (when (not (zerop dy))
      (let ((try-y (+ ny (* dy step))))
        (if (blocked-at-p world nx try-y half-w half-h tile-size)
            (setf out-dy 0.0)
            (setf ny try-y
                  out-dy dy))))
    (values nx ny out-dx out-dy)))

(defun npc-collision-half (world)
  ;; Return NPC collider half sizes in world pixels.
  (let ((half (* (/ (world-tile-dest-size world) 2.0) *npc-collision-scale*)))
    (values half half)))

(defun update-running-state (player dt moving toggle-run)
  ;; Update stamina and return the current speed multiplier.
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
  (if (and (player-running player) (> (player-run-stamina player) 0.0))
      *run-speed-mult*
      1.0))

(defun update-player-position (player intent world speed-mult dt)
  ;; Move the player with collision and target logic.
  (let ((x (player-x player))
        (y (player-y player))
        (input-dx (intent-move-dx intent))
        (input-dy (intent-move-dy intent))
        (dx 0.0)
        (dy 0.0))
    (cond
      ((or (not (zerop input-dx))
           (not (zerop input-dy)))
       (clear-intent-target intent)
       (multiple-value-setq (x y dx dy)
         (attempt-move world x y input-dx input-dy
                       (* *player-speed* speed-mult dt)
                       (world-collision-half-width world)
                       (world-collision-half-height world)
                       (world-tile-dest-size world))))
      ((intent-target-active intent)
       (let* ((target-x (intent-target-x intent))
              (target-y (intent-target-y intent))
              (to-x (- target-x x))
              (to-y (- target-y y))
              (dist (sqrt (+ (* to-x to-x) (* to-y to-y)))))
         (if (<= dist *target-epsilon*)
             (setf (intent-target-active intent) nil
                   dx 0.0
                   dy 0.0)
             (let* ((dir-x (/ to-x dist))
                    (dir-y (/ to-y dist))
                    (step (min (* *player-speed* speed-mult dt) dist)))
               (multiple-value-setq (x y dx dy)
                 (attempt-move world x y dir-x dir-y step
                               (world-collision-half-width world)
                               (world-collision-half-height world)
                               (world-tile-dest-size world)))
               (when (or (<= dist step)
                         (and (zerop dx) (zerop dy)))
                 (setf (intent-target-active intent) nil))))))
      (t
       (setf dx 0.0
             dy 0.0)))
    (setf x (clamp x (world-wall-min-x world) (world-wall-max-x world))
          y (clamp y (world-wall-min-y world) (world-wall-max-y world)))
    (let ((old-x (player-x player))
          (old-y (player-y player)))
      (setf (player-x player) x
            (player-y player) y
            (player-dx player) dx
            (player-dy player) dy)
      ;; Tier-2 write: position changes should be marked dirty for batched saves
      (when (or (/= old-x x) (/= old-y y))
        (mark-player-dirty (player-id player))))))

(defun player-intent-direction (player)
  ;; Return the intended movement direction for edge transitions.
  (let* ((intent (player-intent player))
         (dx (intent-move-dx intent))
         (dy (intent-move-dy intent)))
    (cond
      ((or (not (zerop dx)) (not (zerop dy)))
       (values dx dy))
      ((intent-target-active intent)
       (normalize-vector (- (intent-target-x intent) (player-x player))
                         (- (intent-target-y intent) (player-y player))))
      (t
       (values 0.0 0.0)))))

(defun edge-opposite (edge)
  ;; Return the opposite world edge.
  (ecase edge
    (:north :south)
    (:south :north)
    (:east :west)
    (:west :east)))

(defun edge-preserve-axis (edge offset)
  ;; Return which axis to preserve when transitioning.
  (cond
    ((eq offset :preserve-x) :x)
    ((eq offset :preserve-y) :y)
    ((eq offset :none) nil)
    ((member edge '(:north :south)) :x)
    (t :y)))

(defun edge-offset-ratio (min-value max-value value)
  ;; Convert VALUE into a 0..1 ratio inside MIN/MAX.
  (let ((range (- max-value min-value)))
    (if (<= range 0.0)
        0.5
        (clamp (/ (- value min-value) range) 0.0 1.0))))

(defun edge-preserve-position (min-value max-value ratio)
  ;; Return a position inside MIN/MAX based on RATIO.
  (+ min-value (* (clamp ratio 0.0 1.0) (- max-value min-value))))

(defun world-exit-edge (world player)
  ;; Return the edge the player is pushing against, if any.
  (multiple-value-bind (dx dy)
      (player-intent-direction player)
    (let ((edge nil)
          (weight 0.0)
          (x (player-x player))
          (y (player-y player))
          (min-x (world-wall-min-x world))
          (max-x (world-wall-max-x world))
          (min-y (world-wall-min-y world))
          (max-y (world-wall-max-y world)))
      (when (and (< dy 0.0) (<= y min-y))
        (let ((w (abs dy)))
          (when (> w weight)
            (setf edge :north
                  weight w))))
      (when (and (> dy 0.0) (>= y max-y))
        (let ((w (abs dy)))
          (when (> w weight)
            (setf edge :south
                  weight w))))
      (when (and (< dx 0.0) (<= x min-x))
        (let ((w (abs dx)))
          (when (> w weight)
            (setf edge :west
                  weight w))))
      (when (and (> dx 0.0) (>= x max-x))
        (let ((w (abs dx)))
          (when (> w weight)
            (setf edge :east
                  weight w))))
      edge)))

(defun world-preview-edge (world player)
  ;; Return the edge to preview minimap spawns for.
  (let ((edge (world-exit-edge world player)))
    (if edge
        edge
        (let* ((zone (world-zone world))
               (zone-id (and zone (zone-id zone)))
               (graph (world-world-graph world))
               (threshold (* (world-tile-dest-size world)
                             (max 0.0 *minimap-preview-edge-tiles*)))
               (x (player-x player))
               (y (player-y player))
               (min-x (world-wall-min-x world))
               (max-x (world-wall-max-x world))
               (min-y (world-wall-min-y world))
               (max-y (world-wall-max-y world)))
          (when (and graph zone-id (> threshold 0.0))
            (let ((best-edge nil)
                  (best-distance nil))
              (dolist (exit (world-graph-exits graph zone-id))
                (let* ((edge (getf exit :edge))
                       (edge-distance (case edge
                                        (:west (max 0.0 (- x min-x)))
                                        (:east (max 0.0 (- max-x x)))
                                        (:north (max 0.0 (- y min-y)))
                                        (:south (max 0.0 (- max-y y)))
                                        (t nil))))
                  (when (and edge-distance
                             (<= edge-distance threshold))
                    (when (or (null best-distance)
                              (< edge-distance best-distance))
                      (setf best-distance edge-distance
                            best-edge edge)))))
              best-edge))))))

(defun camera-view-center (player editor)
  ;; Return the current camera focus point.
  (if (and editor (editor-active editor))
      (values (editor-camera-x editor) (editor-camera-y editor))
      (values (player-x player) (player-y player))))

(defun camera-view-bounds (camera player editor)
  ;; Return view bounds in world coordinates for the current camera focus.
  (let* ((zoom (camera-zoom camera))
         (half-view-width (/ *window-width* (* 2.0 zoom)))
         (half-view-height (/ *window-height* (* 2.0 zoom))))
    (multiple-value-bind (x y) (camera-view-center player editor)
      (values (- x half-view-width) (+ x half-view-width)
              (- y half-view-height) (+ y half-view-height)))))

(defun view-exceeds-edge-p (world view-left view-right view-top view-bottom edge)
  ;; Return true when the camera view extends beyond EDGE.
  (case edge
    (:west (< view-left (world-wall-min-x world)))
    (:east (> view-right (world-wall-max-x world)))
    (:north (< view-top (world-wall-min-y world)))
    (:south (> view-bottom (world-wall-max-y world)))
    (t nil)))

(defun world-edge-exit-for-zone (world zone-id edge)
  ;; Return the exit spec for EDGE in ZONE-ID.
  (let ((graph (world-world-graph world)))
    (when (and edge zone-id graph)
      (find edge (world-graph-exits graph zone-id)
            :key (lambda (exit) (getf exit :edge))
            :test #'eq))))

(defun world-diagonal-zone-id (world edge-a edge-b)
  ;; Return the diagonal zone id for EDGE-A + EDGE-B if the graph connects it.
  (let* ((zone (world-zone world))
         (zone-id (and zone (zone-id zone))))
    (when zone-id
      (labels ((next-zone (from edge)
                 (let ((exit (world-edge-exit-for-zone world from edge)))
                   (and exit (getf exit :to)))))
        (let ((first (next-zone zone-id edge-a)))
          (or (and first (next-zone first edge-b))
              (let ((second (next-zone zone-id edge-b)))
                (and second (next-zone second edge-a)))))))))

(defun world-preview-zone-for-edge (world edge)
  ;; Return cached preview zone data for EDGE if available.
  (let* ((cache (world-zone-preview-cache world))
         (exit (and cache (world-edge-exit world edge)))
         (target-id (and exit (getf exit :to))))
    (and target-id (gethash target-id cache))))

(defun world-preview-zone-for-corner (world edge-a edge-b)
  ;; Return cached preview zone data for EDGE-A + EDGE-B if available.
  (let* ((cache (world-zone-preview-cache world))
         (target-id (and cache (world-diagonal-zone-id world edge-a edge-b))))
    (and target-id (gethash target-id cache))))

(defun ensure-preview-zone-for-edge (world edge)
  ;; Load adjacent zone data for preview rendering on EDGE.
  (let* ((graph (world-world-graph world))
         (cache (world-zone-preview-cache world))
         (exit (and graph cache (world-edge-exit world edge)))
         (target-id (and exit (getf exit :to))))
    (when (and graph cache target-id (not (gethash target-id cache)))
      (let ((path (world-graph-zone-path graph target-id)))
        (when (and path (probe-file path))
          (let ((zone (load-zone path)))
            (when zone
              (setf (gethash target-id cache) zone))))))))

(defun ensure-preview-zone-for-corner (world edge-a edge-b)
  ;; Load diagonal zone data for preview rendering on EDGE-A + EDGE-B.
  (let* ((cache (world-zone-preview-cache world))
         (target-id (and cache (world-diagonal-zone-id world edge-a edge-b))))
    (when (and cache target-id (not (gethash target-id cache)))
      (let* ((graph (world-world-graph world))
             (path (and graph (world-graph-zone-path graph target-id))))
        (when (and path (probe-file path))
          (let ((zone (load-zone path)))
            (when zone
              (setf (gethash target-id cache) zone))))))))

(defun ensure-preview-zones (world player camera editor)
  ;; Load adjacent zone data when the camera view reaches a world edge.
  (multiple-value-bind (view-left view-right view-top view-bottom)
      (camera-view-bounds camera player editor)
    (let* ((ex-west (view-exceeds-edge-p world view-left view-right view-top view-bottom :west))
           (ex-east (view-exceeds-edge-p world view-left view-right view-top view-bottom :east))
           (ex-north (view-exceeds-edge-p world view-left view-right view-top view-bottom :north))
           (ex-south (view-exceeds-edge-p world view-left view-right view-top view-bottom :south)))
      (when ex-west
        (ensure-preview-zone-for-edge world :west))
      (when ex-east
        (ensure-preview-zone-for-edge world :east))
      (when ex-north
        (ensure-preview-zone-for-edge world :north))
      (when ex-south
        (ensure-preview-zone-for-edge world :south))
      (when (and ex-west ex-north)
        (ensure-preview-zone-for-corner world :west :north))
      (when (and ex-east ex-north)
        (ensure-preview-zone-for-corner world :east :north))
      (when (and ex-west ex-south)
        (ensure-preview-zone-for-corner world :west :south))
      (when (and ex-east ex-south)
        (ensure-preview-zone-for-corner world :east :south)))))

(defun world-edge-exit (world edge)
  ;; Return the exit spec for EDGE in the current zone.
  (let* ((zone (world-zone world))
         (zone-id (and zone (zone-id zone)))
         (graph (world-world-graph world)))
    (when (and edge zone-id graph)
      (find edge (world-graph-exits graph zone-id)
            :key (lambda (exit) (getf exit :edge))
            :test #'eq))))

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
  (let ((table (make-hash-table :test 'eq)))
    (dolist (entry entries)
      (setf (gethash (first entry) table) t))
    table))

(defun cache-zone-npcs (world zone-id npcs carried-table)
  ;; Cache NPCs for ZONE-ID, excluding those in CARRIED-TABLE.
  (let ((cache (world-zone-npc-cache world)))
    (when (and cache zone-id npcs)
      (if (null carried-table)
          (setf (gethash zone-id cache) npcs)
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
              (setf (gethash zone-id cache) stored)))))))

(defun cached-zone-npcs (world zone-id)
  ;; Return cached NPCs for ZONE-ID, if any.
  (let ((cache (world-zone-npc-cache world)))
    (and cache zone-id (gethash zone-id cache))))

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

(defun transition-zone (game player exit edge)
  ;; Apply a zone transition using EXIT metadata for the given PLAYER.
  (let* ((world (game-world game))
         (intent (player-intent player))
         (current-zone (world-zone world))
         (current-zone-id (and current-zone (zone-id current-zone)))
         (current-npcs (game-npcs game))
         (graph (world-world-graph world))
         (target-id (getf exit :to))
         (target-path (and graph (world-graph-zone-path graph target-id)))
         (had-target (intent-target-active intent))
         (target-offset-x (when had-target
                            (- (intent-target-x intent) (player-x player))))
         (target-offset-y (when had-target
                            (- (intent-target-y intent) (player-y player))))
         (carry (collect-transition-npcs current-npcs player world))
         (carry-table (and carry (build-carry-npc-table carry))))
    (when (and target-path (probe-file target-path))
      (cache-zone-npcs world current-zone-id current-npcs carry-table)
      (let* ((zone (with-retry-exponential (loaded (lambda () (load-zone target-path))
                                             :max-retries 2
                                             :initial-delay 100
                                             :max-delay 200
                                             :on-final-fail (lambda (e)
                                                              (warn "Zone transition failed: could not load zone ~a after retries: ~a"
                                                                    target-id e)))
                     loaded))
             (spawn-edge (or (getf exit :spawn-edge)
                             (getf exit :to-edge)
                             (edge-opposite edge)))
             (offset (getf exit :offset))
             (preserve-axis (edge-preserve-axis edge offset))
             (ratio (if preserve-axis
                        (if (eq preserve-axis :x)
                            (edge-offset-ratio (world-wall-min-x world)
                                               (world-wall-max-x world)
                                               (player-x player))
            (edge-offset-ratio (world-wall-min-y world)
                               (world-wall-max-y world)
                               (player-y player)))
                        0.5)))
        (when zone
          (log-verbose "Zone transition: ~a -> ~a via ~a"
                       current-zone-id
                       (zone-id zone)
                       edge)
          (setf *zone-path* target-path)
          (apply-zone-to-world world zone)
          (setf (world-zone-label world) (zone-label zone))
          (multiple-value-bind (raw-x raw-y)
              (edge-spawn-position world spawn-edge preserve-axis ratio)
            (multiple-value-bind (spawn-x spawn-y)
                (world-open-position-for world raw-x raw-y
                                         (world-collision-half-width world)
                                         (world-collision-half-height world))
              (setf (player-x player) spawn-x
                    (player-y player) spawn-y
                    (player-dx player) 0.0
                    (player-dy player) 0.0)
              ;; Tier-2 write: zone transition position changes should be marked dirty
              (mark-player-dirty (player-id player))))
          (reset-frame-intent intent)
          (when had-target
            (set-intent-target intent
                               (+ (player-x player) target-offset-x)
                               (+ (player-y player) target-offset-y)))
          (setf (player-attacking player) nil
                (player-attack-hit player) nil
                    (player-attack-timer player) 0.0)
          (setf (world-minimap-spawns world)
                (build-adjacent-minimap-spawns world player))
          (let* ((players (game-players game))
                 (target-zone-id (and zone (zone-id zone)))
                 (cached (cached-zone-npcs world target-zone-id))
                 (npcs (or cached
                           (make-npcs player world
                                      :id-source (game-npc-id-source game))))
                 (carried (reposition-transition-npcs carry player world)))
            ;; Update session zone-id for persistence
            (update-player-session-zone (player-id player) target-zone-id)
            (ensure-npcs-open-spawn npcs world)
            (let ((merged (merge-npc-vectors npcs carried)))
              (setf (game-npcs game) merged
                    (game-entities game) (make-entities players merged))))
          t)))))

(defun update-zone-transition (game)
  ;; Handle edge-based world graph transitions for all players.
  ;; NOTE: Returns nil if no players.
  (let* ((world (game-world game))
         (players (game-players game))
         (transitioned nil))
    (when (and players (> (length players) 0))
      (loop :for player :across players
            :do (let* ((edge (and world (world-exit-edge world player)))
                       (exit (and edge (world-edge-exit world edge))))
                  (when exit
                    (transition-zone game player exit edge)
                    (setf transitioned t)
                    (return)))))  ; Only transition first player that hits edge
    transitioned))

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

(defun process-player-unstuck (player intent world zone-id &optional event-queue)
  ;; Handle client unstuck request (server authority).
  ;; If player is truly stuck, teleport to a random location in the zone.
  (declare (ignore zone-id))
  (when (intent-requested-unstuck intent)
    (if (player-is-stuck-p player world)
        ;; Player is stuck - teleport to random position in zone
        (multiple-value-bind (safe-x safe-y)
            (get-zone-safe-spawn world)
          (multiple-value-bind (final-x final-y)
              (world-open-position world safe-x safe-y)
            (setf (player-x player) final-x
                  (player-y player) final-y
                  (player-dx player) 0.0
                  (player-dy player) 0.0)
            (mark-player-dirty (player-id player))
            (log-verbose "Player ~a unstuck, teleported to (~,1f, ~,1f)"
                         (player-id player) final-x final-y)
            (when event-queue
              (emit-hud-message-event event-queue "Teleported to safe location."))))
        ;; Player not stuck - deny free teleport
        (progn
          (log-verbose "Player ~a unstuck request denied - not stuck"
                       (player-id player))
          (when event-queue
            (emit-hud-message-event event-queue "You don't appear to be stuck."))))
    (clear-requested-unstuck intent)))

(defun make-world ()
  ;; Build world state and derived collision/render constants.
  (let* ((zone (load-zone *zone-path*))
         (graph (load-world-graph))
         (tile-size-f (float *tile-size* 1.0))
         (tile-dest-size (* tile-size-f *tile-scale*))
         (floor-index *floor-tile-index*)
         (wall-map (if zone
                       (zone-wall-map zone)
                       (build-wall-map)))
         (wall-map-width (array-dimension wall-map 1))
         (wall-map-height (array-dimension wall-map 0))
         (collision-half-width (* (/ tile-dest-size 2.0) *player-collision-scale*))
         (collision-half-height (* (/ tile-dest-size 2.0) *player-collision-scale*))
         (wall-min-x (+ (* (+ *wall-origin-x* 1) tile-dest-size)
                        collision-half-width))
         (wall-max-x (- (* (+ *wall-origin-x* (1- wall-map-width))
                           tile-dest-size)
                        collision-half-width))
         (wall-min-y (+ (* (+ *wall-origin-y* 1) tile-dest-size)
                        collision-half-height))
        (wall-max-y (- (* (+ *wall-origin-y* (1- wall-map-height))
                           tile-dest-size)
                        collision-half-height)))
    (let ((world (%make-world :tile-size-f tile-size-f
                              :tile-dest-size tile-dest-size
                              :floor-index floor-index
                              :zone zone
                              :zone-label (zone-label zone)
                              :world-graph graph
                              :zone-npc-cache (make-hash-table :test 'eq)
                              :zone-preview-cache (make-hash-table :test 'eq)
                              :minimap-spawns nil
                              :minimap-collisions nil
                              :wall-map wall-map
                              :wall-map-width wall-map-width
                              :wall-map-height wall-map-height
                              :collision-half-width collision-half-width
                              :collision-half-height collision-half-height
                              :wall-min-x wall-min-x
                              :wall-max-x wall-max-x
                              :wall-min-y wall-min-y
                              :wall-max-y wall-max-y)))
      (log-verbose "World ready: zone=~a walls=~dx~d tile=~,2f dest=~,2f"
                   (zone-label zone)
                   wall-map-width
                   wall-map-height
                   tile-size-f
                   tile-dest-size)
      (setf (world-minimap-spawns world)
            (build-adjacent-minimap-spawns world))
      (setf (world-minimap-collisions world)
            (build-minimap-collisions world))
      world)))

(defun apply-zone-to-world (world zone)
  ;; Replace the world's zone and rebuild wall-map-derived bounds.
  (let* ((tile-dest-size (world-tile-dest-size world))
         (wall-map (if zone
                       (zone-wall-map zone)
                       (build-wall-map)))
         (wall-map-width (array-dimension wall-map 1))
         (wall-map-height (array-dimension wall-map 0))
         (collision-half-width (world-collision-half-width world))
         (collision-half-height (world-collision-half-height world))
         (wall-min-x (+ (* (+ *wall-origin-x* 1) tile-dest-size)
                        collision-half-width))
         (wall-max-x (- (* (+ *wall-origin-x* (1- wall-map-width))
                           tile-dest-size)
                        collision-half-width))
         (wall-min-y (+ (* (+ *wall-origin-y* 1) tile-dest-size)
                        collision-half-height))
         (wall-max-y (- (* (+ *wall-origin-y* (1- wall-map-height))
                           tile-dest-size)
                        collision-half-height)))
    (setf (world-zone world) zone
          (world-zone-label world) (zone-label zone)
          (world-wall-map world) wall-map
          (world-wall-map-width world) wall-map-width
          (world-wall-map-height world) wall-map-height
          (world-wall-min-x world) wall-min-x
          (world-wall-max-x world) wall-max-x
          (world-wall-min-y world) wall-min-y
          (world-wall-max-y world) wall-max-y)
    (unless (world-zone-preview-cache world)
      (setf (world-zone-preview-cache world) (make-hash-table :test 'eq)))
    (let ((graph (world-world-graph world)))
      (when graph
        (setf (world-graph-zone-paths graph)
              (build-zone-paths (resolve-zone-path *zone-root*)))))
    (setf (world-minimap-spawns world)
          (build-adjacent-minimap-spawns world))
    (setf (world-minimap-collisions world)
          (build-minimap-collisions world))
    world))
