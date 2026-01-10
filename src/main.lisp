(in-package #:mmorpg)

(defparameter *window-width* 1280)
(defparameter *window-height* 720)
(defparameter *player-speed* 222.0)

(defparameter *player-sprite-dir* "../assets/1 Characters/1")
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
(defparameter *landmark-indices* '(101 102 105)) ; Sparse decorative overlays.
(defparameter *landmark-mod* 80) ; 1 in N tiles become a landmark.
(defparameter *landmark-seed* 7331) ; Seed for deterministic landmark placement.
(defparameter *target-epsilon* 6.0) ; Stop distance for click-to-move.
(defparameter *target-marker-radius* 6.0) ; Radius of the click target marker.
(defparameter *idle-frame-count* 4) ; Frames in each idle animation row.
(defparameter *walk-frame-count* 6) ; Frames in each walk animation row.
(defparameter *idle-frame-time* 0.25) ; Seconds per idle frame.
(defparameter *walk-frame-time* 0.12) ; Seconds per walk frame.
(defparameter *gc-overlay-interval* 0.5) ; Seconds between GC stats updates.

(defparameter +key-right+ (cffi:foreign-enum-value 'raylib:keyboard-key :right))
(defparameter +key-left+ (cffi:foreign-enum-value 'raylib:keyboard-key :left))
(defparameter +key-down+ (cffi:foreign-enum-value 'raylib:keyboard-key :down))
(defparameter +key-up+ (cffi:foreign-enum-value 'raylib:keyboard-key :up))
(defparameter +key-d+ (cffi:foreign-enum-value 'raylib:keyboard-key :d))
(defparameter +key-a+ (cffi:foreign-enum-value 'raylib:keyboard-key :a))
(defparameter +key-s+ (cffi:foreign-enum-value 'raylib:keyboard-key :s))
(defparameter +key-w+ (cffi:foreign-enum-value 'raylib:keyboard-key :w))
(defparameter +key-left-bracket+ (cffi:foreign-enum-value 'raylib:keyboard-key :left-bracket))
(defparameter +key-right-bracket+ (cffi:foreign-enum-value 'raylib:keyboard-key :right-bracket))
(defparameter +key-left-shift+ (cffi:foreign-enum-value 'raylib:keyboard-key :left-shift))
(defparameter +key-right-shift+ (cffi:foreign-enum-value 'raylib:keyboard-key :right-shift))
(defparameter +mouse-left+ (cffi:foreign-enum-value 'raylib:mouse-button :left))

(defun move-player (x y dt)
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
    (values (+ x (* dx *player-speed* dt))
            (+ y (* dy *player-speed* dt))
            dx dy)))

(defun clamp (value min-value max-value)
  (max min-value (min value max-value)))

(defun sprite-path (filename)
  (format nil "~a/~a" *player-sprite-dir* filename))

(defun player-direction (dx dy)
  (cond ((> (abs dx) (abs dy)) :side)
        ((< dy 0.0) :up)
        (t :down)))

(defun player-state (dx dy)
  (if (and (zerop dx) (zerop dy)) :idle :walk))

(defun shift-held-p ()
  (or (raylib:is-key-down +key-left-shift+)
      (raylib:is-key-down +key-right-shift+)))

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

(defun build-floor-map (map-width map-height main-index variant-indices)
  (let ((map (make-array (list map-height map-width))))
    (loop :for row :below map-height
          :do (loop :for col :below map-width
                    :do (setf (aref map row col)
                              (floor-tile-at col row main-index variant-indices))))
    map))

(defun screen-to-world (screen-x screen-y target-x target-y camera-offset camera-zoom)
  (let* ((zoom (if (zerop camera-zoom) 1.0 camera-zoom))
         (sx (float screen-x 1.0))
         (sy (float screen-y 1.0)))
    (values (+ (/ (- sx (raylib:vector2-x camera-offset)) zoom)
               target-x)
            (+ (/ (- sy (raylib:vector2-y camera-offset)) zoom)
               target-y))))

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

#+sbcl
(defun gc-overlay-update (last-consed last-run last-real last-count interval fps frame-ms)
  (let* ((bytes (sb-ext:get-bytes-consed))
         (delta-bytes (- bytes last-consed))
         (gc-count (sb-ext:generation-number-of-gcs 0))
         (delta-gc (- gc-count last-count))
         (run sb-ext:*gc-run-time*)
         (real sb-ext:*gc-real-time*)
         (delta-real (- real last-real))
         (units internal-time-units-per-second)
         (secs (max interval 0.001))
         (consed-kb-per-sec (/ delta-bytes 1024.0 secs))
         (gc-ms (* 1000.0 (/ delta-real units))))
    (values (format nil "FPS: ~d (~,1fms) | GC g0: ~d (+~d) consed ~,1f KB/s gc ~,2fms"
                    fps frame-ms gc-count delta-gc consed-kb-per-sec gc-ms)
            bytes run real gc-count)))

(defun run ()
  (raylib:with-window ("Hello MMO" (*window-width* *window-height*))
    (raylib:set-target-fps 60)
    (let* ((tile-size-f (float *tile-size* 1.0))
           (tile-dest-size (* tile-size-f *tile-scale*))
           (floor-index *floor-tile-index*)
           (floor-index-text (format nil "Floor tile: ~d" floor-index))
           #+sbcl (last-bytes-consed (sb-ext:get-bytes-consed))
           #+sbcl (last-gc-run-time sb-ext:*gc-run-time*)
           #+sbcl (last-gc-real-time sb-ext:*gc-real-time*)
           #+sbcl (last-gc-count (sb-ext:generation-number-of-gcs 0))
           (gc-text #+sbcl (format nil "FPS: -- (--ms) | GC g0: ~d (+0) consed 0.0 KB/s gc 0.00ms"
                                   last-gc-count)
                    #-sbcl "GC: n/a")
           (gc-timer 0.0)
           (scaled-width (* *sprite-frame-width* *sprite-scale*))
           (scaled-height (* *sprite-frame-height* *sprite-scale*))
           (half-sprite-width (/ scaled-width 2.0))
           (half-sprite-height (/ scaled-height 2.0))
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
           (camera (raylib:make-camera-2d
                    :target (raylib:make-vector2 :x x :y y)
                    :offset camera-offset
                    :rotation 0.0
                    :zoom 1.0))
           (camera-target (raylib:camera-2d-target camera))
           (target-x x)
           (target-y y)
           (target-active nil)
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
      (unwind-protect
           (loop :until (raylib:window-should-close)
                 :do (let ((dt (raylib:get-frame-time)))
                       (multiple-value-setq (x y dx dy) (move-player x y dt))
                       (when (raylib:is-mouse-button-pressed +mouse-left+)
                         (multiple-value-setq (target-x target-y)
                           (screen-to-world (raylib:get-mouse-x)
                                            (raylib:get-mouse-y)
                                            x
                                            y
                                            camera-offset
                                            (raylib:camera-2d-zoom camera)))
                         (setf target-active t))
                       (when (and target-active (zerop dx) (zerop dy))
                         (let* ((to-x (- target-x x))
                                (to-y (- target-y y))
                                (dist (sqrt (+ (* to-x to-x) (* to-y to-y))))
                                (step (* *player-speed* dt)))
                           (if (or (<= dist *target-epsilon*)
                                   (<= dist step))
                               (setf x target-x
                                     y target-y
                                     dx 0.0
                                     dy 0.0
                                     target-active nil)
                               (let ((nx (/ to-x dist))
                                     (ny (/ to-y dist)))
                                 (setf x (+ x (* nx step))
                                       y (+ y (* ny step))
                                       dx nx
                                       dy ny)))))
                       (setf (raylib:vector2-x camera-target) x
                             (raylib:vector2-y camera-target) y)
                       (let* ((step (if (shift-held-p) 10 1))
                              (changed nil))
                         (when (raylib:is-key-pressed +key-left-bracket+)
                           (setf floor-index (clamp (- floor-index step) 0 208)
                                 changed t))
                         (when (raylib:is-key-pressed +key-right-bracket+)
                           (setf floor-index (clamp (+ floor-index step) 0 208)
                                 changed t))
                         (when changed
                           (setf floor-index-text (format nil "Floor tile: ~d" floor-index))))
                       (incf gc-timer dt)
                       #+sbcl
                       (when (>= gc-timer *gc-overlay-interval*)
                         (decf gc-timer *gc-overlay-interval*)
                         (let* ((fps (raylib:get-fps))
                                (frame-ms (if (plusp fps) (/ 1000.0 fps) 0.0)))
                           (multiple-value-setq (gc-text last-bytes-consed last-gc-run-time
                                                         last-gc-real-time last-gc-count)
                             (gc-overlay-update last-bytes-consed last-gc-run-time
                                                last-gc-real-time last-gc-count
                                                *gc-overlay-interval*
                                                fps frame-ms))))
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
                         (raylib:with-mode-2d camera
                           (let* ((zoom (raylib:camera-2d-zoom camera))
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
                                                                             raylib:+white+))))))
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
                                                        raylib:+white+))
                             (when target-active
                               (raylib:draw-circle (floor target-x)
                                                   (floor target-y)
                                                   *target-marker-radius*
                                                   raylib:+yellow+))))
                         (raylib:draw-text floor-index-text
                                           10 10 20 raylib:+white+)
                         (raylib:draw-text gc-text
                                           10 34 20 raylib:+white+)))))
        (raylib:unload-texture tileset)
        (raylib:unload-texture down-idle)
        (raylib:unload-texture down-walk)
        (raylib:unload-texture up-idle)
        (raylib:unload-texture up-walk)
        (raylib:unload-texture side-idle)
        (raylib:unload-texture side-walk))))
