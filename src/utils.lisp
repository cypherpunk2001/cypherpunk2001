;; NOTE: If you change behavior here, update docs/utils.md :)
(in-package #:mmorpg)

(defun clamp (value min-value max-value)
  ;; Clamp VALUE between MIN-VALUE and MAX-VALUE for bounds checks.
  (max min-value (min value max-value)))

(defun normalize-direction (dx dy)
  ;; Normalize diagonal movement to unit length; keep axis-aligned values as-is.
  (if (and (not (zerop dx)) (not (zerop dy)))
      (let ((len (sqrt (+ (* dx dx) (* dy dy)))))
        (values (/ dx len) (/ dy len)))
      (values dx dy)))

(defun normalize-vector (dx dy)
  ;; Normalize an arbitrary vector to unit length (0,0 stays 0,0).
  (let ((len (sqrt (+ (* dx dx) (* dy dy)))))
    (if (zerop len)
        (values 0.0 0.0)
        (values (/ dx len) (/ dy len)))))

(defun screen-to-world (screen-x screen-y target-x target-y camera-offset camera-zoom)
  ;; Convert screen coordinates into world space using camera offset and zoom.
  (let* ((zoom (if (zerop camera-zoom) 1.0 camera-zoom))
         (sx (float screen-x 1.0))
         (sy (float screen-y 1.0)))
    (values (+ (/ (- sx (raylib:vector2-x camera-offset)) zoom)
               target-x)
            (+ (/ (- sy (raylib:vector2-y camera-offset)) zoom)
               target-y))))

(defun point-in-rect-p (x y rx ry rw rh)
  ;; Return true when point (x,y) lies inside the given rectangle bounds.
  (and (>= x rx)
       (< x (+ rx rw))
       (>= y ry)
       (< y (+ ry rh))))

(defun basename (path)
  ;; Return the filename portion of PATH for display labels.
  (let* ((path-str (string path))
         (slash (position #\/ path-str :from-end t))
         (backslash (position #\\ path-str :from-end t))
         (cut (max (or slash -1) (or backslash -1))))
    (if (>= cut 0)
        (subseq path-str (1+ cut))
        path-str)))

(defun sprite-path (filename)
  ;; Build a sprite sheet path under *player-sprite-dir*.
  (format nil "~a/~a" *player-sprite-dir* filename))

(defun npc-sprite-path (filename)
  ;; Build a sprite sheet path under *npc-sprite-dir*.
  (format nil "~a/~a" *npc-sprite-dir* filename))

(defun blood-sprite-path (filename)
  ;; Build a sprite sheet path under *blood-sprite-dir*.
  (format nil "~a/~a" *blood-sprite-dir* filename))

(defun player-direction (dx dy)
  ;; Pick a facing direction keyword from movement delta.
  (cond ((> (abs dx) (abs dy)) :side)
        ((< dy 0.0) :up)
        (t :down)))

(defun player-state (dx dy)
  ;; Return :idle or :walk based on movement delta.
  (if (and (zerop dx) (zerop dy)) :idle :walk))

(defun player-animation-params (state)
  ;; Return frame count and base frame time for STATE.
  (ecase state
    (:idle (values *idle-frame-count* *idle-frame-time*))
    (:walk (values *walk-frame-count* *walk-frame-time*))
    (:attack (values *attack-frame-count* *attack-frame-time*))))

(defun u32-hash (x y &optional (seed 1337))
  ;; Generate a deterministic 32-bit hash for tile variation.
  (logand #xffffffff
          (+ (* x 73856093)
             (* y 19349663)
             (* seed 83492791))))
