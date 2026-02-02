(in-package #:mmorpg)

;;;; ========================================================================
;;;; Wall Maps & Collision Detection
;;;; Tile-based collision checks, wall map construction, movement resolution.
;;;; ========================================================================

(defun derive-wall-map-from-zone (zone)
  "Create a wall map from zone collision data.
   Returns a 2D array where 1 = blocked, 0 = passable."
  (when zone
    (let* ((width (zone-width zone))
           (height (zone-height zone))
           (map (make-array (list height width) :initial-element 0))
           (collision-tiles (zone-collision-tiles zone)))
      (when collision-tiles
        (maphash (lambda (key _val)
                   (declare (ignore _val))
                   ;; Keys are packed integers from tile-key, not cons cells
                   (let ((tx (tile-key-x key))
                         (ty (tile-key-y key)))
                     (when (and (>= tx 0) (< tx width)
                                (>= ty 0) (< ty height))
                       (setf (aref map ty tx) 1))))
                 collision-tiles))
      map)))

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
  ;; Uses global *wall-origin-x/y* for coordinate translation.
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

(defun wall-blocked-p-zero-origin (wall-map tx ty)
  "Test if tile (TX, TY) is blocked in WALL-MAP assuming zero origin.
   Used for zone-state wall-maps which are stored without offset."
  (if (not wall-map)
      nil
      (let* ((width (array-dimension wall-map 1))
             (height (array-dimension wall-map 0)))
        (if (or (< tx 0)
                (>= tx width)
                (< ty 0)
                (>= ty height))
            t
            (not (zerop (aref wall-map ty tx)))))))

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

;;;; ========================================================================
;;;; Collision Testing & Position Helpers
;;;; ========================================================================

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

;;; Zone-state spawn helpers (Phase 1 - per-zone collision)

(defun find-open-position-with-map (wall-map x y half-w half-h tile-size)
  "Find nearest open position using WALL-MAP (zero-origin).
   Mirrors find-open-tile spiral search pattern."
  (if (not (blocked-at-p-with-map wall-map x y half-w half-h tile-size))
      (values x y)
      ;; Spiral search for open tile - use map dimensions as max radius
      (let ((max-radius (if wall-map
                            (max (array-dimension wall-map 0)
                                 (array-dimension wall-map 1))
                            20)))
        (loop :for radius :from 1 :to max-radius
              :do (loop :for dy :from (- radius) :to radius
                        :do (loop :for dx :from (- radius) :to radius
                                  :for tx = (+ (floor x tile-size) dx)
                                  :for ty = (+ (floor y tile-size) dy)
                                  :for cx = (+ (* tx tile-size) (/ tile-size 2.0))
                                  :for cy = (+ (* ty tile-size) (/ tile-size 2.0))
                                  :when (not (blocked-at-p-with-map wall-map cx cy half-w half-h tile-size))
                                  :do (return-from find-open-position-with-map (values cx cy))))
              :finally (return (values x y))))))  ; Fallback to original position

(defun zone-state-spawn-position (zone-state)
  "Return valid spawn (x, y) using ZONE-STATE's wall-map.
   Used for spawning players in a specific zone without relying on global world collision.
   Uses zero-origin bounds since zone wall-maps are zero-indexed."
  (let* ((wall-map (zone-state-wall-map zone-state))
         (tile-dest-size (* (float *tile-size* 1.0) *tile-scale*))
         ;; Match make-world formula for player collision
         (collision-half (* (/ tile-dest-size 2.0) *player-collision-scale*))
         (width (if wall-map (array-dimension wall-map 1) 64))
         (height (if wall-map (array-dimension wall-map 0) 64)))
    ;; Calculate zone bounds using zero-origin (per-zone wall-maps are zero-indexed)
    (multiple-value-bind (min-x max-x min-y max-y)
        (zone-bounds-zero-origin tile-dest-size width height collision-half collision-half)
      ;; Start from center of zone
      (let ((center-x (/ (+ min-x max-x) 2.0))
            (center-y (/ (+ min-y max-y) 2.0)))
        ;; Find open position from center
        (find-open-position-with-map wall-map center-x center-y
                                     collision-half collision-half tile-dest-size)))))

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

;;;; ========================================================================
;;;; Collision Detection (Hot Path)
;;;; ========================================================================

(defun blocked-at-p (world x y half-w half-h tile-size)
  "Test collider bounds against blocked tiles in the world map."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type world world)
           (type single-float x y half-w half-h tile-size))
  (let* ((left (the single-float (- x half-w)))
         (right (the single-float (+ x half-w)))
         (top (the single-float (- y half-h)))
         (bottom (the single-float (+ y half-h)))
         (right-edge (the single-float (- right *collision-edge-epsilon*)))
         (bottom-edge (the single-float (- bottom *collision-edge-epsilon*)))
         (tx1 (the fixnum (floor left tile-size)))
         (tx2 (the fixnum (floor right-edge tile-size)))
         (ty1 (the fixnum (floor top tile-size)))
         (ty2 (the fixnum (floor bottom-edge tile-size))))
    (declare (type single-float left right top bottom right-edge bottom-edge)
             (type fixnum tx1 tx2 ty1 ty2))
    (loop :for ty fixnum :from ty1 :to ty2
          :thereis (loop :for tx fixnum :from tx1 :to tx2
                         :thereis (world-blocked-tile-p world tx ty)))))

(defun blocked-at-p-with-map (wall-map x y half-w half-h tile-size)
  "Test collider bounds against blocked tiles using WALL-MAP (zero-origin).
   Factored from blocked-at-p to support per-zone collision checking."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type (or null array) wall-map)
           (type single-float x y half-w half-h tile-size))
  (let* ((left (the single-float (- x half-w)))
         (right (the single-float (+ x half-w)))
         (top (the single-float (- y half-h)))
         (bottom (the single-float (+ y half-h)))
         (right-edge (the single-float (- right *collision-edge-epsilon*)))
         (bottom-edge (the single-float (- bottom *collision-edge-epsilon*)))
         (tx1 (the fixnum (floor left tile-size)))
         (tx2 (the fixnum (floor right-edge tile-size)))
         (ty1 (the fixnum (floor top tile-size)))
         (ty2 (the fixnum (floor bottom-edge tile-size))))
    (declare (type single-float left right top bottom right-edge bottom-edge)
             (type fixnum tx1 tx2 ty1 ty2))
    (loop :for ty fixnum :from ty1 :to ty2
          :thereis (loop :for tx fixnum :from tx1 :to tx2
                         :thereis (wall-blocked-p-zero-origin wall-map tx ty)))))

;;;; ========================================================================
;;;; Movement Resolution
;;;; ========================================================================

(defun attempt-move (world x y dx dy step half-w half-h tile-size)
  "Resolve movement per axis and cancel movement when blocked."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type world world)
           (type single-float x y dx dy step half-w half-h tile-size))
  (let ((nx x)
        (ny y)
        (out-dx 0.0)
        (out-dy 0.0))
    (declare (type single-float nx ny out-dx out-dy))
    (when (not (zerop dx))
      (let ((try-x (the single-float (+ x (* dx step)))))
        (declare (type single-float try-x))
        (if (blocked-at-p world try-x y half-w half-h tile-size)
            (setf out-dx 0.0)
            (setf nx try-x
                  out-dx dx))))
    (when (not (zerop dy))
      (let ((try-y (the single-float (+ ny (* dy step)))))
        (declare (type single-float try-y))
        (if (blocked-at-p world nx try-y half-w half-h tile-size)
            (setf out-dy 0.0)
            (setf ny try-y
                  out-dy dy))))
    (values nx ny out-dx out-dy)))

(defun attempt-move-with-map (wall-map x y dx dy step half-w half-h tile-size)
  "Resolve movement per axis using WALL-MAP for collision.
   Used for per-zone collision checking where global world map isn't appropriate."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type (or null array) wall-map)
           (type single-float x y dx dy step half-w half-h tile-size))
  (let ((nx x)
        (ny y)
        (out-dx 0.0)
        (out-dy 0.0))
    (declare (type single-float nx ny out-dx out-dy))
    (when (not (zerop dx))
      (let ((try-x (the single-float (+ x (* dx step)))))
        (declare (type single-float try-x))
        (if (blocked-at-p-with-map wall-map try-x y half-w half-h tile-size)
            (setf out-dx 0.0)
            (setf nx try-x
                  out-dx dx))))
    (when (not (zerop dy))
      (let ((try-y (the single-float (+ ny (* dy step)))))
        (declare (type single-float try-y))
        (if (blocked-at-p-with-map wall-map nx try-y half-w half-h tile-size)
            (setf out-dy 0.0)
            (setf ny try-y
                  out-dy dy))))
    (values nx ny out-dx out-dy)))
