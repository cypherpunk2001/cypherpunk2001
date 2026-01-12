(in-package #:mmorpg)

(defun read-input-direction ()
  ;; Read WASD/arrow keys and return a normalized movement vector.
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

(defun update-camera-zoom (camera)
  ;; Adjust zoom with the mouse wheel and reset on middle click.
  (let ((wheel (raylib:get-mouse-wheel-move)))
    (when (not (zerop wheel))
      (setf (camera-zoom camera)
            (clamp (+ (camera-zoom camera) (* wheel *camera-zoom-step*))
                   *camera-zoom-min*
                   *camera-zoom-max*))))
  (when (raylib:is-mouse-button-pressed +mouse-middle+)
    (setf (camera-zoom camera) *camera-zoom-default*)))

(defun clear-player-auto-walk (player)
  ;; Clear auto-walk toggles on the player.
  (setf (player-auto-right player) nil
        (player-auto-left player) nil
        (player-auto-down player) nil
        (player-auto-up player) nil))

(defun set-player-target (player target-x target-y)
  ;; Set click-to-move target and activate it.
  (setf (player-target-x player) target-x
        (player-target-y player) target-y
        (player-target-active player) t
        (player-mouse-hold-timer player) 0.0))

(defun update-target-from-mouse (player camera dt mouse-clicked mouse-down)
  ;; Handle click/hold to update the player target position.
  (when mouse-clicked
    (clear-player-auto-walk player)
    (multiple-value-bind (target-x target-y)
        (screen-to-world (raylib:get-mouse-x)
                         (raylib:get-mouse-y)
                         (player-x player)
                         (player-y player)
                         (camera-offset camera)
                         (camera-zoom camera))
      (set-player-target player target-x target-y)))
  (when (and mouse-down (not mouse-clicked))
    (incf (player-mouse-hold-timer player) dt)
    (when (>= (player-mouse-hold-timer player) *mouse-hold-repeat-seconds*)
      (setf (player-mouse-hold-timer player) 0.0)
      (clear-player-auto-walk player)
      (multiple-value-bind (target-x target-y)
          (screen-to-world (raylib:get-mouse-x)
                           (raylib:get-mouse-y)
                           (player-x player)
                           (player-y player)
                           (camera-offset camera)
                           (camera-zoom camera))
        (set-player-target player target-x target-y))))
  (unless mouse-down
    (setf (player-mouse-hold-timer player) 0.0)))

(defun update-input-direction (player mouse-clicked)
  ;; Compute input dx/dy and handle auto-walk toggles.
  (let ((input-dx 0.0)
        (input-dy 0.0))
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
                (setf *auto-walk-enabled* nil)
                (clear-player-auto-walk player))))
        (if *auto-walk-enabled*
            (let ((key-pressed nil))
              (when pressed-right
                (setf key-pressed t
                      (player-auto-right player) (not (player-auto-right player)))
                (when (player-auto-right player)
                  (setf (player-auto-left player) nil)))
              (when pressed-left
                (setf key-pressed t
                      (player-auto-left player) (not (player-auto-left player)))
                (when (player-auto-left player)
                  (setf (player-auto-right player) nil)))
              (when pressed-down
                (setf key-pressed t
                      (player-auto-down player) (not (player-auto-down player)))
                (when (player-auto-down player)
                  (setf (player-auto-up player) nil)))
              (when pressed-up
                (setf key-pressed t
                      (player-auto-up player) (not (player-auto-up player)))
                (when (player-auto-up player)
                  (setf (player-auto-down player) nil)))
              (when key-pressed
                (setf (player-target-active player) nil))
              (setf input-dx (+ (if (player-auto-right player) 1.0 0.0)
                                (if (player-auto-left player) -1.0 0.0))
                    input-dy (+ (if (player-auto-down player) 1.0 0.0)
                                (if (player-auto-up player) -1.0 0.0)))
              (multiple-value-setq (input-dx input-dy)
                (normalize-direction input-dx input-dy)))
            (multiple-value-setq (input-dx input-dy)
              (read-input-direction)))))
    (values input-dx input-dy)))

(defun make-camera ()
  ;; Initialize camera offset and zoom settings.
  (%make-camera :offset (raylib:make-vector2 :x (/ *window-width* 2.0)
                                             :y (/ *window-height* 2.0))
                :zoom *camera-zoom-default*))
