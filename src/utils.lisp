;; NOTE: If you change behavior here, update docs/utils.md :)
(in-package #:mmorpg)

(defun log-verbose (fmt &rest args)
  ;; Emit a verbose log line when *verbose* is enabled.
  (when *verbose*
    (format t "~&[VERBOSE] ")
    (apply #'format t fmt args)
    (format t "~%")
    (finish-output)))

(defun log-fatal-error (context condition)
  ;; Emit fatal error context and optional backtrace.
  (warn "~a: ~a" context condition)
  (log-verbose "~a: ~a" context condition)
  #+sbcl
  (when *verbose*
    (format t "~&[VERBOSE] Backtrace (most recent call first):~%")
    (sb-debug:print-backtrace :stream *standard-output* :count 25)))

(defmacro with-fatal-error-log ((context) &body body)
  `(handler-bind ((error (lambda (e)
                           (log-fatal-error ,context e))))
     ,@body))

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

(defun minimap-view-bounds (world player)
  ;; Return minimap view origin and size centered on the player.
  (let* ((min-x (world-wall-min-x world))
         (max-x (world-wall-max-x world))
         (min-y (world-wall-min-y world))
         (max-y (world-wall-max-y world))
         (span-x (max 1.0 (- max-x min-x)))
         (span-y (max 1.0 (- max-y min-y)))
         (center-x (player-x player))
         (center-y (player-y player))
         (view-min-x (- center-x (/ span-x 2.0)))
         (view-min-y (- center-y (/ span-y 2.0))))
    (values view-min-x view-min-y span-x span-y)))

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

(defun sanitize-identifier (name &key (package :keyword))
  ;; Convert NAME into a keyword-safe identifier.
  (let* ((clean (map 'string
                     (lambda (ch)
                       (if (or (digit-char-p ch)
                               (alpha-char-p ch))
                           ch
                           #\-))
                     name))
         (trimmed (string-trim "-" clean)))
    (intern (string-upcase trimmed) package)))

(defun plist-put (plist key value)
  ;; Return PLIST with KEY set to VALUE (adds key if missing).
  (let ((pos (position key plist :test #'eq)))
    (if pos
        (progn
          (setf (nth (1+ pos) plist) value)
          plist)
        (append plist (list key value)))))

(defun relative-path-from-root (path root)
  ;; Return PATH as a relative string from ROOT, if possible.
  (let* ((root-str (namestring (uiop:ensure-directory-pathname root)))
         (full-str (namestring path)))
    (if (and (<= (length root-str) (length full-str))
             (string= root-str full-str :end2 (length root-str)))
        (string-left-trim "/" (subseq full-str (length root-str)))
        full-str)))

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
  ;; Generate a deterministic 32-bit hash for wall tile selection.
  (logand #xffffffff
          (+ (* x 73856093)
             (* y 19349663)
             (* seed 83492791))))

;;; Retry utilities for robust error handling

(defun exponential-backoff-delay (attempt initial-delay max-delay)
  "Calculate exponential backoff delay in milliseconds for ATTEMPT (0-indexed).
   INITIAL-DELAY is the base delay, MAX-DELAY caps the growth."
  (let ((delay (* initial-delay (expt 2 attempt))))
    (min delay max-delay)))

(defmacro with-retry-exponential ((result-var fn &key
                                               (max-retries 3)
                                               (initial-delay 100)
                                               (max-delay 1000)
                                               (on-retry nil)
                                               (on-final-fail nil))
                                  &body body)
  "Execute FN with exponential backoff retry logic.
   - RESULT-VAR: Symbol to bind the result of FN
   - FN: Lambda or function to execute
   - MAX-RETRIES: Maximum number of retry attempts (default 3)
   - INITIAL-DELAY: Initial delay in milliseconds (default 100ms)
   - MAX-DELAY: Maximum delay in milliseconds (default 1000ms)
   - ON-RETRY: Lambda called on each retry with (attempt error)
   - ON-FINAL-FAIL: Lambda called when all retries exhausted with (error)
   - BODY: Forms to execute after successful call (result-var bound to return value)

   Returns the result of FN on success, or NIL after all retries fail."
  (let ((attempt-var (gensym "ATTEMPT"))
        (error-var (gensym "ERROR"))
        (delay-var (gensym "DELAY"))
        (success-var (gensym "SUCCESS")))
    `(let ((,result-var nil)
           (,success-var nil))
       (dotimes (,attempt-var (1+ ,max-retries))
         (handler-case
             (progn
               (setf ,result-var (funcall ,fn))
               (setf ,success-var t)
               (return))
           (error (,error-var)
             (if (< ,attempt-var ,max-retries)
                 (let ((,delay-var (exponential-backoff-delay
                                    ,attempt-var ,initial-delay ,max-delay)))
                   (log-verbose "Retry attempt ~d/~d after ~dms delay: ~a"
                                (1+ ,attempt-var) ,max-retries ,delay-var ,error-var)
                   ,@(when on-retry
                       `((funcall ,on-retry ,attempt-var ,error-var)))
                   (sleep (/ ,delay-var 1000.0)))
                 (progn
                   (warn "All ~d retry attempts exhausted: ~a" ,max-retries ,error-var)
                   ,@(when on-final-fail
                       `((funcall ,on-final-fail ,error-var))))))))
       ;; If body is provided, use its return value; otherwise return result-var
       ,(if body
            `(when ,success-var
               ,@body)
            result-var))))

(defmacro with-retry-linear ((result-var fn &key
                                          (max-retries 3)
                                          (delay 50)
                                          (on-retry nil)
                                          (on-final-fail nil))
                             &body body)
  "Execute FN with linear delay retry logic (for network operations).
   - RESULT-VAR: Symbol to bind the result of FN
   - FN: Lambda or function to execute
   - MAX-RETRIES: Maximum number of retry attempts (default 3)
   - DELAY: Fixed delay in milliseconds between retries (default 50ms)
   - ON-RETRY: Lambda called on each retry with (attempt error)
   - ON-FINAL-FAIL: Lambda called when all retries exhausted with (error)
   - BODY: Forms to execute after successful call (result-var bound to return value)

   Returns the result of FN on success, or NIL after all retries fail."
  (let ((attempt-var (gensym "ATTEMPT"))
        (error-var (gensym "ERROR"))
        (success-var (gensym "SUCCESS")))
    `(let ((,result-var nil)
           (,success-var nil))
       (dotimes (,attempt-var (1+ ,max-retries))
         (handler-case
             (progn
               (setf ,result-var (funcall ,fn))
               (setf ,success-var t)
               (return))
           (error (,error-var)
             (if (< ,attempt-var ,max-retries)
                 (progn
                   (log-verbose "Retry attempt ~d/~d after ~dms delay: ~a"
                                (1+ ,attempt-var) ,max-retries ,delay ,error-var)
                   ,@(when on-retry
                       `((funcall ,on-retry ,attempt-var ,error-var)))
                   (sleep (/ ,delay 1000.0)))
                 (progn
                   (warn "All ~d retry attempts exhausted: ~a" ,max-retries ,error-var)
                   ,@(when on-final-fail
                       `((funcall ,on-final-fail ,error-var))))))))
       (when ,success-var
         ,@body)
       ,result-var)))

;;; Vector Pool for Serialization (Task 4.2)
;;; Reduces allocation during hot snapshot serialization path.
;;; Vectors are acquired from pool, filled, and automatically reused on next reset.

(defstruct (vector-pool (:constructor %make-vector-pool))
  "Pool of pre-allocated vectors to reduce allocation during serialization.
   Use acquire-pooled-vector to get vectors, reset-vector-pool to reuse all."
  (vectors nil)      ; Vector of pre-allocated vectors
  (index 0)          ; Next vector to allocate
  (element-size 0)   ; Size of each pooled vector
  (capacity 0))      ; Max number of vectors

(defun make-vector-pool (capacity element-size)
  "Create a vector pool with CAPACITY vectors of ELEMENT-SIZE elements each.
   Used for pre-allocating serialization buffers."
  (let ((vectors (make-array capacity)))
    (dotimes (i capacity)
      (setf (aref vectors i) (make-array element-size :initial-element 0)))
    (%make-vector-pool :vectors vectors
                       :index 0
                       :element-size element-size
                       :capacity capacity)))

(defun reset-vector-pool (pool)
  "Reset pool index to 0, allowing all vectors to be reused.
   Call this at the start of each serialization pass."
  (when pool
    (setf (vector-pool-index pool) 0)))

(defun acquire-pooled-vector (pool)
  "Acquire a pre-allocated vector from the pool.
   Returns a fresh vector if pool is exhausted (grows the pool).
   The vector's contents are undefined - caller must fill all elements."
  (if (null pool)
      ;; No pool - create fresh vector (fallback)
      (make-array 22 :initial-element 0)
      (let ((index (vector-pool-index pool))
            (capacity (vector-pool-capacity pool)))
        (incf (vector-pool-index pool))  ; Always increment to track usage
        (if (< index capacity)
            ;; Return next pooled vector
            (aref (vector-pool-vectors pool) index)
            ;; Pool exhausted - create fresh vector (rare case)
            (make-array (vector-pool-element-size pool) :initial-element 0)))))

(defun vector-pool-stats (pool)
  "Return stats about pool usage: (values used total exhausted-count).
   Useful for tuning pool capacity."
  (if pool
      (values (vector-pool-index pool)
              (vector-pool-capacity pool)
              (max 0 (- (vector-pool-index pool) (vector-pool-capacity pool))))
      (values 0 0 0)))
