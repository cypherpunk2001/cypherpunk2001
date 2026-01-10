(in-package #:mmorpg)

(defparameter *window-width* 1280)
(defparameter *window-height* 720)
(defparameter *player-speed* 222.0)

(defparameter *player-sprite-dir* "../assets/1 Characters/1")
(defparameter *sprite-frame-width* 32.0)
(defparameter *sprite-frame-height* 32.0)
(defparameter *sprite-scale* 4.0)

(defparameter *tileset-path* "../assets/2 Dungeon Tileset/1 Tiles/Tileset.png") ; Atlas image used for floor tiles.
(defparameter *tile-size* 16.0) ; Source tile size in the atlas, in pixels.
(defparameter *tile-scale* 4.0) ; Scale factor for drawing tiles to the screen.
(defparameter *tileset-columns* 19) ; Number of columns in the atlas grid.
(defparameter *floor-tile-index* 20) ; Which atlas tile index to use for the floor fill.
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

(defun tile-source-rect (tile-index)
  (let* ((col (mod tile-index *tileset-columns*))
         (row (floor tile-index *tileset-columns*)))
    (raylib:make-rectangle
     :x (* col *tile-size*)
     :y (* row *tile-size*)
     :width *tile-size*
     :height *tile-size*)))

(defun run ()
  (raylib:with-window ("Hello MMO" (*window-width* *window-height*))
    (raylib:set-target-fps 60)
    (let* ((tile-dest-size (* *tile-size* *tile-scale*))
           (map-width (ceiling (/ *window-width* tile-dest-size)))
           (map-height (ceiling (/ *window-height* tile-dest-size)))
           (floor-map (make-array (list map-height map-width)
                                  :initial-element *floor-tile-index*))
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
                       (setf x (clamp x half-sprite-width
                                      (- *window-width* half-sprite-width)))
                       (setf y (clamp y half-sprite-height
                                      (- *window-height* half-sprite-height)))
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
                           (let ((origin (raylib:make-vector2 :x 0.0 :y 0.0)))
                             (loop :for row :below map-height
                                   :for dest-y :from 0.0 :by tile-dest-size
                                   :do (loop :for col :below map-width
                                             :for dest-x :from 0.0 :by tile-dest-size
                                             :for tile-index = (aref floor-map row col)
                                             :for source = (tile-source-rect tile-index)
                                             :for dest = (raylib:make-rectangle
                                                          :x dest-x
                                                          :y dest-y
                                                          :width tile-dest-size
                                                          :height tile-dest-size)
                                             :do (raylib:draw-texture-pro tileset
                                                                          source
                                                                          dest
                                                                          origin
                                                                          0.0
                                                                          raylib:+white+)))
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
                                                   *sprite-frame-width*))
                                    (source (raylib:make-rectangle
                                             :x src-x
                                             :y 0.0
                                             :width src-width
                                             :height *sprite-frame-height*))
                                    (dest (raylib:make-rectangle
                                           :x (- x half-sprite-width)
                                           :y (- y half-sprite-height)
                                           :width scaled-width
                                           :height scaled-height)))
                               (raylib:draw-texture-pro texture
                                                        source
                                                        dest
                                                        origin
                                                        0.0
                                                        raylib:+white+)))))))
        (raylib:unload-texture tileset)
        (raylib:unload-texture down-idle)
        (raylib:unload-texture down-walk)
        (raylib:unload-texture up-idle)
        (raylib:unload-texture up-walk)
        (raylib:unload-texture side-idle)
        (raylib:unload-texture side-walk)))))
