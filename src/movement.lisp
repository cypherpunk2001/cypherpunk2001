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
  (let ((map (world-map world)))
    (if map
        (or (map-tile-outside-bounds-p map tx ty)
            (collision-tile-p world tx ty))
        (wall-blocked-p (world-wall-map world) tx ty))))

(defun blocked-at-p (world x y half-w half-h tile-size)
  ;; Test collider bounds against blocked tiles in the world map.
  (let* ((left (- x half-w))
         (right (+ x half-w))
         (top (- y half-h))
         (bottom (+ y half-h))
         (tx1 (floor left tile-size))
         (tx2 (floor right tile-size))
         (ty1 (floor top tile-size))
         (ty2 (floor bottom tile-size)))
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
    (setf (player-x player) x
          (player-y player) y
          (player-dx player) dx
          (player-dy player) dy)))

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

(defun make-world ()
  ;; Build world state and derived collision/render constants.
  (let* ((map (load-tmx-map *map-path* *map-collision-layers*))
         (tile-size-f (float (if map
                                 (map-data-tilewidth map)
                                 *tile-size*) 1.0))
         (tile-dest-size (* tile-size-f *tile-scale*))
         (floor-index *floor-tile-index*)
         (wall-map (unless map (build-wall-map)))
         (wall-map-width (if wall-map (array-dimension wall-map 1) 0))
         (wall-map-height (if wall-map (array-dimension wall-map 0) 0))
         (collision-half-width (* (/ tile-dest-size 2.0) *player-collision-scale*))
         (collision-half-height (* (/ tile-dest-size 2.0) *player-collision-scale*))
         (wall-min-x (if map
                         (+ (* (map-data-min-x map) tile-dest-size)
                            collision-half-width)
                         (+ (* (+ *wall-origin-x* 1) tile-dest-size)
                            collision-half-width)))
         (wall-max-x (if map
                         (- (* (1+ (map-data-max-x map)) tile-dest-size)
                            collision-half-width)
                         (- (* (+ *wall-origin-x* (1- wall-map-width))
                               tile-dest-size)
                            collision-half-width)))
         (wall-min-y (if map
                         (+ (* (map-data-min-y map) tile-dest-size)
                            collision-half-height)
                         (+ (* (+ *wall-origin-y* 1) tile-dest-size)
                            collision-half-height)))
         (wall-max-y (if map
                         (- (* (1+ (map-data-max-y map)) tile-dest-size)
                            collision-half-height)
                         (- (* (+ *wall-origin-y* (1- wall-map-height))
                               tile-dest-size)
                            collision-half-height))))
    (%make-world :tile-size-f tile-size-f
                 :tile-dest-size tile-dest-size
                 :floor-index floor-index
                 :map map
                 :collision-tiles (when map (map-data-collision-tiles map))
                 :wall-map wall-map
                 :wall-map-width wall-map-width
                 :wall-map-height wall-map-height
                 :collision-half-width collision-half-width
                 :collision-half-height collision-half-height
                 :wall-min-x wall-min-x
                 :wall-max-x wall-max-x
                 :wall-min-y wall-min-y
                 :wall-max-y wall-max-y)))
