(in-package #:mmorpg)

(defparameter *verbose-logs* nil) ; When true, logs player position and collider info per frame.
(defparameter *debug-collision-overlay* nil) ; Draws debug grid and collision overlays.

(defparameter *window-width* 1280)
(defparameter *window-height* 720)
(defparameter *player-speed* 222.0)
(defparameter *auto-walk-enabled* t) ; When true, WASD toggles auto-walk direction.
(defparameter *camera-zoom-default* 1.0) ; Default camera zoom level.
(defparameter *camera-zoom-min* 0.5) ; Minimum zoom level.
(defparameter *camera-zoom-max* 3.0) ; Maximum zoom level.
(defparameter *camera-zoom-step* 0.1) ; Zoom step per mouse wheel tick.
(defparameter *run-speed-mult* 2.0) ; Movement speed multiplier while running.
(defparameter *run-stamina-max* 10.0) ; Seconds of run stamina when full.
(defparameter *mouse-hold-repeat-seconds* 0.25) ; Repeat rate for mouse-held updates.

(defparameter *player-sprite-dir* "../assets/1 Characters/3")
(defparameter *sprite-frame-width* 32.0)
(defparameter *sprite-frame-height* 32.0)
(defparameter *sprite-scale* 4.0)

(defparameter *tileset-path* "../assets/2 Dungeon Tileset/1 Tiles/Tileset.png") ; Atlas image used for floor tiles.
(defparameter *tile-size* 16) ; Source tile size in the atlas, in pixels.
(defparameter *tile-scale* 4.0) ; Scale factor for drawing tiles to the screen.
(defparameter *tileset-columns* 19) ; Number of columns in the atlas grid.
(defparameter *floor-tile-index* 40) ; Which atlas tile index to use for the floor fill.
(defparameter *floor-variant-indices* '(41 42)) ; Occasional variants (0 can be used for empty).
(defparameter *floor-variant-mod* 10) ; 1 in N chance to use a variant instead of main.
(defparameter *floor-cluster-size* 3) ; Size of clustered variant blocks, in tiles.
(defparameter *floor-seed* 1337) ; Seed for deterministic floor variation.
(defparameter *landmark-indices* '(41 42)) ; Sparse decorative overlays.
(defparameter *landmark-mod* 80) ; 1 in N tiles become a landmark.
(defparameter *landmark-seed* 7331) ; Seed for deterministic landmark placement.
(defparameter *wall-map-width* 40) ; Width of the test wall map in tiles.
(defparameter *wall-map-height* 24) ; Height of the test wall map in tiles.
(defparameter *wall-origin-x* 0) ; World tile X where the wall map starts.
(defparameter *wall-origin-y* 0) ; World tile Y where the wall map starts.
(defparameter *wall-tile-indices* '(107)) ; Wall tile variants.
(defparameter *wall-seed* 2468) ; Seed for wall tile variation.
(defparameter *player-collision-scale* 2.0) ; Collision box size relative to one tile.
(defparameter *target-epsilon* 6.0) ; Stop distance for click-to-move.

(defparameter *idle-frame-count* 4) ; Frames in each idle animation row.
(defparameter *walk-frame-count* 6) ; Frames in each walk animation row.
(defparameter *idle-frame-time* 0.25) ; Seconds per idle frame.
(defparameter *walk-frame-time* 0.12) ; Seconds per walk frame.

(defparameter +key-right+ (cffi:foreign-enum-value 'raylib:keyboard-key :right))
(defparameter +key-left+ (cffi:foreign-enum-value 'raylib:keyboard-key :left))
(defparameter +key-down+ (cffi:foreign-enum-value 'raylib:keyboard-key :down))
(defparameter +key-up+ (cffi:foreign-enum-value 'raylib:keyboard-key :up))
(defparameter +key-d+ (cffi:foreign-enum-value 'raylib:keyboard-key :d))
(defparameter +key-a+ (cffi:foreign-enum-value 'raylib:keyboard-key :a))
(defparameter +key-s+ (cffi:foreign-enum-value 'raylib:keyboard-key :s))
(defparameter +key-w+ (cffi:foreign-enum-value 'raylib:keyboard-key :w))
(defparameter +key-tab+ (cffi:foreign-enum-value 'raylib:keyboard-key :tab))
(defparameter +key-left-shift+ (cffi:foreign-enum-value 'raylib:keyboard-key :left-shift))
(defparameter +key-right-shift+ (cffi:foreign-enum-value 'raylib:keyboard-key :right-shift))
(defparameter +mouse-left+ (cffi:foreign-enum-value 'raylib:mouse-button :left))
(defparameter +mouse-middle+ (cffi:foreign-enum-value 'raylib:mouse-button :middle))

(defun clamp (value min-value max-value)
  (max min-value (min value max-value)))

(defun normalize-direction (dx dy)
  (if (and (not (zerop dx)) (not (zerop dy)))
      (let ((len (sqrt (+ (* dx dx) (* dy dy)))))
        (values (/ dx len) (/ dy len)))
      (values dx dy)))

(defun read-input-direction ()
  (let ((dx 0.0)
        (dy 0.0))
    (when (or (raylib:is-key-down +key-right+)
              (raylib:is-key-down +key-d+))
      (incf dx 1.0))
    (when (or (raylib:is-key-down +key-left+)
              (raylib:is-key-down +key-a+))
      (decf dx 1.0))
    (when (or (raylib:is-key-down +key-down+)
              (raylib:is-key-down +key-s+))
      (incf dy 1.0))
    (when (or (raylib:is-key-down +key-up+)
              (raylib:is-key-down +key-w+))
      (decf dy 1.0))
    (normalize-direction dx dy)))

(defun screen-to-world (screen-x screen-y target-x target-y camera-offset camera-zoom)
  (let* ((zoom (if (zerop camera-zoom) 1.0 camera-zoom))
         (sx (float screen-x 1.0))
         (sy (float screen-y 1.0)))
    (values (+ (/ (- sx (raylib:vector2-x camera-offset)) zoom)
               target-x)
            (+ (/ (- sy (raylib:vector2-y camera-offset)) zoom)
               target-y))))

(defun sprite-path (filename)
  (format nil "~a/~a" *player-sprite-dir* filename))

(defun player-direction (dx dy)
  (cond ((> (abs dx) (abs dy)) :side)
        ((< dy 0.0) :up)
        (t :down)))

(defun player-state (dx dy)
  (if (and (zerop dx) (zerop dy)) :idle :walk))

(defun u32-hash (x y &optional (seed 1337))
  (logand #xffffffff
          (+ (* x 73856093)
             (* y 19349663)
             (* seed 83492791))))

(defun floor-tile-at (x y main-index variant-indices)
  (let* ((cluster-size (max 1 *floor-cluster-size*))
         (variant-count (length variant-indices))
         (variant-mod (max 1 *floor-variant-mod*))
         (cx (floor x cluster-size))
         (cy (floor y cluster-size))
         (h (u32-hash cx cy *floor-seed*))
         (h2 (u32-hash (+ cx 17) (+ cy 31) (+ *floor-seed* 7331))))
    (if (and (> variant-count 0)
             (zerop (mod h variant-mod)))
        (nth (mod h2 variant-count) variant-indices)
        main-index)))

(defun landmark-tile-at (x y)
  (let* ((variant-count (length *landmark-indices*))
         (variant-mod (max 1 *landmark-mod*))
         (h (u32-hash x y *landmark-seed*))
         (h2 (u32-hash (+ x 19) (+ y 47) (+ *landmark-seed* 101))))
    (if (and (> variant-count 0)
             (zerop (mod h variant-mod)))
        (nth (mod h2 variant-count) *landmark-indices*)
        0)))

(defun build-wall-map ()
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
  (let* ((local-x (- tx *wall-origin-x*))
         (local-y (- ty *wall-origin-y*))
         (width (array-dimension wall-map 1))
         (height (array-dimension wall-map 0)))
    (and (<= 0 local-x)
         (< local-x width)
         (<= 0 local-y)
         (< local-y height)
         (not (zerop (aref wall-map local-y local-x))))))

(defun wall-blocked-p (wall-map tx ty)
  (let* ((local-x (- tx *wall-origin-x*))
         (local-y (- ty *wall-origin-y*))
         (width (array-dimension wall-map 1))
         (height (array-dimension wall-map 0)))
    (if (or (< local-x 0)
            (>= local-x width)
            (< local-y 0)
            (>= local-y height))
        t
        (not (zerop (aref wall-map local-y local-x))))))

(defun wall-tile-at (wall-map tx ty)
  (let ((variant-count (length *wall-tile-indices*)))
    (if (and (wall-occupied-p wall-map tx ty)
             (> variant-count 0))
        (nth (mod (u32-hash tx ty *wall-seed*) variant-count)
             *wall-tile-indices*)
        0)))

(defun blocked-at-p (wall-map x y half-w half-h tile-size)
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
                         :thereis (wall-blocked-p wall-map tx ty)))))

(defun attempt-move (wall-map x y dx dy step half-w half-h tile-size)
  (let ((nx x)
        (ny y)
        (out-dx 0.0)
        (out-dy 0.0))
    (when (not (zerop dx))
      (let ((try-x (+ x (* dx step))))
        (if (blocked-at-p wall-map try-x y half-w half-h tile-size)
            (setf out-dx 0.0)
            (setf nx try-x
                  out-dx dx))))
    (when (not (zerop dy))
      (let ((try-y (+ ny (* dy step))))
        (if (blocked-at-p wall-map nx try-y half-w half-h tile-size)
            (setf out-dy 0.0)
            (setf ny try-y
                  out-dy dy))))
    (values nx ny out-dx out-dy)))

(defun set-rectangle (rect x y width height)
  (setf (raylib:rectangle-x rect) x
        (raylib:rectangle-y rect) y
        (raylib:rectangle-width rect) width
        (raylib:rectangle-height rect) height)
  rect)

(defun set-tile-source-rect (rect tile-index tile-size-f)
  (let* ((col (mod tile-index *tileset-columns*))
         (row (floor tile-index *tileset-columns*)))
    (set-rectangle rect
                   (* col tile-size-f)
                   (* row tile-size-f)
                   tile-size-f
                   tile-size-f)))

(defun run ()
  (raylib:with-window ("Hello MMO" (*window-width* *window-height*))
    (raylib:set-target-fps 60)
    (let* ((tile-size-f (float *tile-size* 1.0))
           (tile-dest-size (* tile-size-f *tile-scale*))
           (floor-index *floor-tile-index*)
           (wall-map (build-wall-map))
           (wall-map-width (array-dimension wall-map 1))
           (wall-map-height (array-dimension wall-map 0))
           (scaled-width (* *sprite-frame-width* *sprite-scale*))
           (scaled-height (* *sprite-frame-height* *sprite-scale*))
           (half-sprite-width (/ scaled-width 2.0))
           (half-sprite-height (/ scaled-height 2.0))
           (collision-half-width (* (/ tile-dest-size 2.0) *player-collision-scale*))
           (collision-half-height (* (/ tile-dest-size 2.0) *player-collision-scale*))
           (wall-min-x (+ (* (+ *wall-origin-x* 1) tile-dest-size) collision-half-width))
           (wall-max-x (- (* (+ *wall-origin-x* (1- wall-map-width)) tile-dest-size)
                          collision-half-width))
           (wall-min-y (+ (* (+ *wall-origin-y* 1) tile-dest-size) collision-half-height))
           (wall-max-y (- (* (+ *wall-origin-y* (1- wall-map-height)) tile-dest-size)
                          collision-half-height))
           (x (/ *window-width* 2.0))
           (y (/ *window-height* 2.0))
           (dx 0.0)
           (dy 0.0)
           (player-state :idle)
           (player-direction :down)
           (frame-index 0)
           (frame-timer 0.0)
           (origin (raylib:make-vector2 :x 0.0 :y 0.0))
           (camera-offset (raylib:make-vector2
                           :x (/ *window-width* 2.0)
                           :y (/ *window-height* 2.0)))
           (camera-zoom *camera-zoom-default*)
           (target-x x)
           (target-y y)
           (target-active nil)
           (running nil)
           (run-stamina *run-stamina-max*)
           (mouse-hold-timer 0.0)
           (auto-right nil)
           (auto-left nil)
           (auto-down nil)
           (auto-up nil)
           (hud-bg-color (raylib:make-color :r 0 :g 0 :b 0 :a 160))
           (debug-grid-color (raylib:make-color :r 255 :g 255 :b 255 :a 40))
           (debug-wall-color (raylib:make-color :r 80 :g 160 :b 255 :a 90))
           (debug-collision-color (raylib:make-color :r 255 :g 0 :b 0 :a 90))
           (debug-collider-color (raylib:make-color :r 0 :g 255 :b 0 :a 180))
           (tile-source (raylib:make-rectangle))
           (tile-dest (raylib:make-rectangle))
           (player-source (raylib:make-rectangle))
           (player-dest (raylib:make-rectangle))
           (tileset (raylib:load-texture *tileset-path*))
           (down-idle (raylib:load-texture (sprite-path "D_Idle.png")))
           (down-walk (raylib:load-texture (sprite-path "D_Walk.png")))
           (up-idle (raylib:load-texture (sprite-path "U_Idle.png")))
           (up-walk (raylib:load-texture (sprite-path "U_Walk.png")))
           (side-idle (raylib:load-texture (sprite-path "S_Idle.png")))
           (side-walk (raylib:load-texture (sprite-path "S_Walk.png"))))
      (when *verbose-logs*
        (format t "~&Verbose logs on. tile-size=~,2f collider-half=~,2f,~,2f wall=[~,2f..~,2f, ~,2f..~,2f]~%"
                tile-dest-size collision-half-width collision-half-height
                wall-min-x wall-max-x wall-min-y wall-max-y)
        (finish-output))
      (unwind-protect
           (loop :until (raylib:window-should-close)
                 :do (let* ((dt (raylib:get-frame-time))
                            (input-dx 0.0)
                            (input-dy 0.0)
                            (mouse-clicked nil)
                            (speed-mult 1.0)
                            (mouse-down nil)
                            (key-pressed nil))
                         (let ((wheel (raylib:get-mouse-wheel-move)))
                           (when (not (zerop wheel))
                             (setf camera-zoom
                                   (clamp (+ camera-zoom (* wheel *camera-zoom-step*))
                                          *camera-zoom-min*
                                          *camera-zoom-max*))))
                         (when (raylib:is-mouse-button-pressed +mouse-middle+)
                           (setf camera-zoom *camera-zoom-default*))
                         (setf dx 0.0
                               dy 0.0)
                         (setf mouse-clicked (raylib:is-mouse-button-pressed +mouse-left+)
                               mouse-down (raylib:is-mouse-button-down +mouse-left+))
                         (when mouse-clicked
                           (setf auto-right nil
                                 auto-left nil
                                 auto-down nil
                                 auto-up nil)
                           (multiple-value-setq (target-x target-y)
                             (screen-to-world (raylib:get-mouse-x)
                                              (raylib:get-mouse-y)
                                              x
                                              y
                                              camera-offset
                                              camera-zoom))
                           (setf target-active t
                                 mouse-hold-timer 0.0))
                         (when (and mouse-down (not mouse-clicked))
                           (incf mouse-hold-timer dt)
                           (when (>= mouse-hold-timer *mouse-hold-repeat-seconds*)
                             (setf mouse-hold-timer 0.0
                                   auto-right nil
                                   auto-left nil
                                   auto-down nil
                                   auto-up nil)
                             (multiple-value-setq (target-x target-y)
                               (screen-to-world (raylib:get-mouse-x)
                                                (raylib:get-mouse-y)
                                                x
                                                y
                                                camera-offset
                                                camera-zoom))
                             (setf target-active t)))
                         (unless mouse-down
                           (setf mouse-hold-timer 0.0))
                         (unless mouse-clicked
                           (let* ((shift-held (or (raylib:is-key-down +key-left-shift+)
                                                  (raylib:is-key-down +key-right-shift+)))
                                  (pressed-right (or (raylib:is-key-pressed +key-right+)
                                                     (raylib:is-key-pressed +key-d+)))
                                  (pressed-left (or (raylib:is-key-pressed +key-left+)
                                                    (raylib:is-key-pressed +key-a+)))
                                  (pressed-down (or (raylib:is-key-pressed +key-down+)
                                                    (raylib:is-key-pressed +key-s+)))
                                  (pressed-up (or (raylib:is-key-pressed +key-up+)
                                                  (raylib:is-key-pressed +key-w+))))
                             (when (or pressed-right pressed-left pressed-down pressed-up)
                               (if shift-held
                                   (setf *auto-walk-enabled* t)
                                   (progn
                                     (setf *auto-walk-enabled* nil
                                           auto-right nil
                                           auto-left nil
                                           auto-down nil
                                           auto-up nil))))
                             (if *auto-walk-enabled*
                                 (progn
                                   (when pressed-right
                                     (setf key-pressed t
                                           auto-right (not auto-right))
                                     (when auto-right
                                       (setf auto-left nil)))
                                   (when pressed-left
                                     (setf key-pressed t
                                           auto-left (not auto-left))
                                     (when auto-left
                                       (setf auto-right nil)))
                                   (when pressed-down
                                     (setf key-pressed t
                                           auto-down (not auto-down))
                                     (when auto-down
                                       (setf auto-up nil)))
                                   (when pressed-up
                                     (setf key-pressed t
                                           auto-up (not auto-up))
                                     (when auto-up
                                       (setf auto-down nil)))
                                   (when key-pressed
                                     (setf target-active nil))
                                   (setf input-dx (+ (if auto-right 1.0 0.0)
                                                     (if auto-left -1.0 0.0))
                                         input-dy (+ (if auto-down 1.0 0.0)
                                                     (if auto-up -1.0 0.0)))
                                   (multiple-value-setq (input-dx input-dy)
                                     (normalize-direction input-dx input-dy)))
                                 (multiple-value-setq (input-dx input-dy)
                                   (read-input-direction)))))
                         (when (raylib:is-key-pressed +key-tab+)
                           (if (> run-stamina 0.0)
                               (setf running (not running))
                               (setf running nil)))
                         (let ((moving (or (not (zerop input-dx))
                                           (not (zerop input-dy))
                                           target-active)))
                           (if (and running moving (> run-stamina 0.0))
                               (progn
                                 (decf run-stamina dt)
                                 (when (<= run-stamina 0.0)
                                   (setf run-stamina 0.0
                                         running nil)))
                               (when (< run-stamina *run-stamina-max*)
                                 (incf run-stamina dt)
                                 (when (>= run-stamina *run-stamina-max*)
                                   (setf run-stamina *run-stamina-max*)))))
                           (setf speed-mult (if (and running (> run-stamina 0.0))
                                                *run-speed-mult*
                                                1.0))
                         (cond
                           ((or (not (zerop input-dx))
                                (not (zerop input-dy)))
                            (setf target-active nil)
                            (multiple-value-setq (x y dx dy)
                              (attempt-move wall-map x y input-dx input-dy
                                            (* *player-speed* speed-mult dt)
                                            collision-half-width
                                            collision-half-height
                                            tile-dest-size)))
                           (target-active
                            (let* ((to-x (- target-x x))
                                   (to-y (- target-y y))
                                   (dist (sqrt (+ (* to-x to-x) (* to-y to-y)))))
                              (if (<= dist *target-epsilon*)
                                  (setf target-active nil
                                        dx 0.0
                                        dy 0.0)
                                  (let* ((dir-x (/ to-x dist))
                                         (dir-y (/ to-y dist))
                                         (step (min (* *player-speed* speed-mult dt) dist)))
                                    (multiple-value-setq (x y dx dy)
                                      (attempt-move wall-map x y dir-x dir-y step
                                                    collision-half-width
                                                    collision-half-height
                                                    tile-dest-size))
                                    (when (or (<= dist step)
                                              (and (zerop dx) (zerop dy)))
                                      (setf target-active nil))))))
                           (t
                            (setf dx 0.0
                                  dy 0.0)))
                       (setf x (clamp x wall-min-x wall-max-x)
                             y (clamp y wall-min-y wall-max-y))
                       (when *verbose-logs*
                         (let* ((center-x x)
                                (center-y y)
                                (tile-x (floor center-x tile-dest-size))
                                (tile-y (floor center-y tile-dest-size))
                                (feet-x center-x)
                                (feet-y (+ center-y collision-half-height))
                                (feet-tile-x (floor feet-x tile-dest-size))
                                (feet-tile-y (floor feet-y tile-dest-size)))
                           (format t "~&pos=~,2f,~,2f center=~,2f,~,2f tile=~d,~d feet=~,2f,~,2f tile-feet=~d,~d~%"
                                   x y
                                   center-x center-y
                                   tile-x tile-y
                                   feet-x feet-y
                                   feet-tile-x feet-tile-y)
                           (finish-output)))
                       (let* ((state (player-state dx dy))
                              (direction (player-direction dx dy))
                              (frame-count (if (eq state :walk)
                                               *walk-frame-count*
                                               *idle-frame-count*))
                              (frame-time (if (eq state :walk)
                                              *walk-frame-time*
                                              *idle-frame-time*))
                              (flip (and (eq direction :side) (> dx 0.0))))
                         (unless (and (eq state player-state)
                                      (eq direction player-direction))
                           (setf player-state state
                                 player-direction direction
                                 frame-index 0
                                 frame-timer 0.0))
                         (incf frame-timer dt)
                         (loop :while (>= frame-timer frame-time)
                               :do (decf frame-timer frame-time)
                                   (setf frame-index
                                         (mod (1+ frame-index) frame-count)))
                         (raylib:with-drawing
                           (raylib:clear-background raylib:+black+)
                           (let ((camera (raylib:make-camera-2d
                                          :target (raylib:make-vector2 :x x :y y)
                                          :offset camera-offset
                                          :rotation 0.0
                                          :zoom camera-zoom)))
                            (raylib:with-mode-2d camera
                              (let* ((zoom camera-zoom)
                                     (half-view-width (/ *window-width* (* 2.0 zoom)))
                                     (half-view-height (/ *window-height* (* 2.0 zoom)))
                                     (view-left (- x half-view-width))
                                     (view-right (+ x half-view-width))
                                     (view-top (- y half-view-height))
                                     (view-bottom (+ y half-view-height))
                                     (start-col (floor view-left tile-dest-size))
                                     (end-col (ceiling view-right tile-dest-size))
                                     (start-row (floor view-top tile-dest-size))
                                     (end-row (ceiling view-bottom tile-dest-size)))
                                (loop :for row :from start-row :to end-row
                                      :for dest-y :from (* start-row tile-dest-size) :by tile-dest-size
                                      :do (loop :for col :from start-col :to end-col
                                                :for dest-x :from (* start-col tile-dest-size) :by tile-dest-size
                                                :for tile-index = (floor-tile-at col row
                                                                                 floor-index
                                                                                 *floor-variant-indices*)
                                                :do (set-rectangle tile-dest dest-x dest-y
                                                                   tile-dest-size tile-dest-size)
                                                    (when (not (zerop tile-index))
                                                      (set-tile-source-rect tile-source tile-index tile-size-f)
                                                      (raylib:draw-texture-pro tileset
                                                                               tile-source
                                                                               tile-dest
                                                                               origin
                                                                               0.0
                                                                               raylib:+white+))
                                                    (let ((landmark-index (landmark-tile-at col row)))
                                                      (when (not (zerop landmark-index))
                                                        (set-tile-source-rect tile-source landmark-index tile-size-f)
                                                        (raylib:draw-texture-pro tileset
                                                                                 tile-source
                                                                                 tile-dest
                                                                                 origin
                                                                                 0.0
                                                                                 raylib:+white+)))
                                                    (let ((wall-index (wall-tile-at wall-map col row)))
                                                      (when (not (zerop wall-index))
                                                        (set-tile-source-rect tile-source wall-index tile-size-f)
                                                        (raylib:draw-texture-pro tileset
                                                                                 tile-source
                                                                                 tile-dest
                                                                                 origin
                                                                                 0.0
                                                                                 raylib:+white+)))))
                                (when *debug-collision-overlay*
                                  (let ((tile-px (round tile-dest-size)))
                                    (loop :for row :from start-row :to end-row
                                          :for dest-y :from (* start-row tile-dest-size) :by tile-dest-size
                                          :do (loop :for col :from start-col :to end-col
                                                    :for dest-x :from (* start-col tile-dest-size) :by tile-dest-size
                                                    :for ix = (round dest-x)
                                                    :for iy = (round dest-y)
                                                    :do (when (wall-blocked-p wall-map col row)
                                                          (raylib:draw-rectangle ix iy tile-px tile-px
                                                                                 debug-collision-color))
                                                        (when (not (zerop (wall-tile-at wall-map col row)))
                                                          (raylib:draw-rectangle ix iy tile-px tile-px
                                                                                 debug-wall-color))
                                                        (raylib:draw-rectangle-lines ix iy tile-px tile-px
                                                                                     debug-grid-color))))
                                  (let ((ix (round (- x collision-half-width)))
                                        (iy (round (- y collision-half-height)))
                                        (iw (round (* 2.0 collision-half-width)))
                                        (ih (round (* 2.0 collision-half-height))))
                                    (raylib:draw-rectangle-lines ix iy iw ih debug-collider-color)))
                                (let* ((texture (ecase direction
                                                  (:down (if (eq state :walk) down-walk down-idle))
                                                  (:up (if (eq state :walk) up-walk up-idle))
                                                  (:side (if (eq state :walk) side-walk side-idle))))
                                       (src-x (* frame-index *sprite-frame-width*))
                                       (src-x (if flip
                                                  (+ src-x *sprite-frame-width*)
                                                  src-x))
                                       (src-width (if flip
                                                      (- *sprite-frame-width*)
                                                      *sprite-frame-width*)))
                                  (set-rectangle player-source
                                                 src-x 0.0
                                                 src-width *sprite-frame-height*)
                                  (set-rectangle player-dest
                                                 (- x half-sprite-width)
                                                 (- y half-sprite-height)
                                                 scaled-width scaled-height)
                                  (raylib:draw-texture-pro texture
                                                           player-source
                                                           player-dest
                                                           origin
                                                           0.0
                                                           raylib:+white+)))))
                           (let* ((run-seconds (max 0 (min (truncate run-stamina)
                                                           (truncate *run-stamina-max*))))
                                  (run-text (format nil "Stamina: ~2d" run-seconds)))
                             (raylib:draw-rectangle 6 6 110 24 hud-bg-color)
                             (raylib:draw-text run-text 10 10 20 raylib:+white+))
                         ))))
        (raylib:unload-texture tileset)
        (raylib:unload-texture down-idle)
        (raylib:unload-texture down-walk)
        (raylib:unload-texture up-idle)
        (raylib:unload-texture up-walk)
        (raylib:unload-texture side-idle)
        (raylib:unload-texture side-walk)))))
