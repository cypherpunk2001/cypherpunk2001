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

;;; Profiling Infrastructure (Task 6.1)
;;; Optional timing macros for measuring hot path performance.
;;; NOTE: defvar preserves state across reloads for long-running profiling sessions.

(defvar *profile-enabled* nil
  "When T, with-timing macro collects timing data.")

(defvar *profile-log* nil
  "Ring buffer of (name . elapsed-ms) timing samples. Created on first use.")

(defvar *profile-log-size* 1000
  "Maximum number of samples to keep in profile log.")

(defvar *profile-log-index* 0
  "Current write position in profile log.")

(defun init-profile-log (&optional (size 1000))
  "Initialize the profile log ring buffer with SIZE slots."
  (setf *profile-log-size* size
        *profile-log* (make-array size :initial-element nil)
        *profile-log-index* 0))

(defun get-profile-log ()
  "Get or create the profile log ring buffer."
  (or *profile-log*
      (setf *profile-log* (make-array *profile-log-size* :initial-element nil)
            *profile-log-index* 0)))

(defun record-profile-sample (name elapsed-ms)
  "Record a timing sample to the profile log."
  (let ((log (get-profile-log)))
    (setf (aref log *profile-log-index*) (cons name elapsed-ms))
    (setf *profile-log-index* (mod (1+ *profile-log-index*) *profile-log-size*))))

(defmacro with-timing ((name) &body body)
  "Execute BODY and record timing if *profile-enabled*.
   NAME is a keyword or symbol identifying this timing point."
  (let ((start (gensym "START"))
        (result (gensym "RESULT")))
    `(if *profile-enabled*
         (let ((,start (get-internal-real-time)))
           (let ((,result (progn ,@body)))
             (record-profile-sample
              ,name
              (* 1000.0 (/ (- (get-internal-real-time) ,start)
                           internal-time-units-per-second)))
             ,result))
         (progn ,@body))))

(defun profile-summary (&key (top 10))
  "Print summary of profile data: mean, min, max, count for each timed section.
   TOP limits output to the TOP most expensive sections by mean time."
  (let ((log (get-profile-log))
        (stats (make-hash-table :test 'eq)))
    ;; Collect stats per name
    (loop :for entry :across log
          :when entry
          :do (let* ((name (car entry))
                     (ms (cdr entry))
                     (stat (or (gethash name stats)
                               (setf (gethash name stats)
                                     (list 0 0.0 most-positive-single-float 0.0)))))
                ;; stat = (count sum min max)
                (incf (first stat))
                (incf (second stat) ms)
                (setf (third stat) (min (third stat) ms))
                (setf (fourth stat) (max (fourth stat) ms))))
    ;; Sort by mean time descending
    (let ((sorted nil))
      (maphash (lambda (name stat)
                 (let ((count (first stat))
                       (sum (second stat)))
                   (push (list name (/ sum count) (first stat)
                               (third stat) (fourth stat))
                         sorted)))
               stats)
      (setf sorted (sort sorted #'> :key #'second))
      ;; Print top N
      (format t "~&=== Profile Summary (top ~d by mean) ===~%" top)
      (format t "~20a ~10a ~8a ~10a ~10a~%" "Name" "Mean(ms)" "Count" "Min(ms)" "Max(ms)")
      (format t "~20a ~10a ~8a ~10a ~10a~%" "----" "--------" "-----" "-------" "-------")
      (loop :for (name mean count min max) :in sorted
            :for i :from 0 :below top
            :do (format t "~20a ~10,3f ~8d ~10,3f ~10,3f~%"
                        name mean count min max)))))

(defun clear-profile-log ()
  "Clear all profile samples."
  (when *profile-log*
    (fill *profile-log* nil)
    (setf *profile-log-index* 0)))

;;; GC Pressure Monitoring (Task 6.2)
;;; Track allocation, GC runtime, and GC count to identify memory-heavy code.

(defvar *verbose-gc* nil
  "When T, log allocation and GC statistics per frame.")

(defvar *gc-stats-last-bytes* 0
  "Last recorded bytes consed.")

(defvar *gc-stats-last-runtime* 0
  "Last recorded GC runtime (internal time units).")

(defvar *gc-stats-frame-count* 0
  "GC count within current frame (reset each frame).")

(defvar *gc-stats-total-count* 0
  "Total GC count since startup.")

(defun get-bytes-consed ()
  "Return total bytes allocated (SBCL only)."
  #+sbcl (sb-ext:get-bytes-consed)
  #-sbcl 0)

(defun get-gc-runtime ()
  "Return cumulative GC runtime in internal time units (SBCL only)."
  #+sbcl sb-ext:*gc-run-time*
  #-sbcl 0)

(defun gc-hook-increment-count ()
  "Hook called after each GC to track count."
  (incf *gc-stats-frame-count*)
  (incf *gc-stats-total-count*))

;; Register GC hook on load (SBCL only)
#+sbcl
(pushnew 'gc-hook-increment-count sb-ext:*after-gc-hooks*)

(defun reset-gc-stats ()
  "Reset allocation/GC stats tracking for delta measurement."
  (setf *gc-stats-last-bytes* (get-bytes-consed))
  (setf *gc-stats-last-runtime* (get-gc-runtime))
  (setf *gc-stats-frame-count* 0))

(defun log-gc-delta ()
  "Log allocation and GC stats since last reset when *verbose-gc* enabled."
  (when *verbose-gc*
    (let* ((current-bytes (get-bytes-consed))
           (delta-bytes (- current-bytes *gc-stats-last-bytes*))
           (current-runtime (get-gc-runtime))
           (delta-runtime (- current-runtime *gc-stats-last-runtime*))
           (gc-ms (/ (* 1000.0 delta-runtime) internal-time-units-per-second)))
      ;; Log if significant allocation or any GC occurred
      (when (or (> delta-bytes 10000) (> *gc-stats-frame-count* 0))
        (format t "~&[GC] ~,1f KB alloc"
                (/ delta-bytes 1024.0))
        (when (> *gc-stats-frame-count* 0)
          (format t ", ~d GCs (~,2f ms)" *gc-stats-frame-count* gc-ms))
        (format t "~%"))
      (setf *gc-stats-last-bytes* current-bytes)
      (setf *gc-stats-last-runtime* current-runtime))))

(defun gc-summary ()
  "Print allocation and GC summary."
  (format t "~&=== GC Summary ===~%")
  (format t "Total bytes consed: ~,1f MB~%"
          (/ (get-bytes-consed) 1048576.0))
  (format t "Total GC count: ~d~%" *gc-stats-total-count*)
  (format t "Total GC runtime: ~,2f s~%"
          (/ (get-gc-runtime) internal-time-units-per-second))
  #+sbcl (room)
  #-sbcl nil)

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
  ;; Client-only helper (depends on raylib vector accessors).
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

(defun acquire-pooled-vector (pool &optional element-size)
  "Acquire a pre-allocated vector from the pool.
   Returns a fresh vector if pool is exhausted (grows the pool).
   The vector's contents are undefined - caller must fill all elements."
  (if (null pool)
      ;; No pool - create fresh vector (fallback)
      (make-array (or element-size 22) :initial-element 0)
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
