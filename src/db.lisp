(in-package :mmorpg)

;;; db.lisp - Storage abstraction layer and Redis persistence
;;;
;;; Implements the database architecture specified in docs/db.md.
;;; Provides storage-agnostic interface for game persistence.
;;;
;;; Key concepts:
;;; - Storage abstraction: game code never knows what DB it uses
;;; - Tiered writes: tier-1 (immediate), tier-2 (batched), tier-3 (logout)
;;; - Versioned serialization: all data includes version for migrations
;;; - Durable vs ephemeral: explicit classification of all state
;;;
;;; Storage backends:
;;; - redis-storage: Production backend using cl-redis
;;; - memory-storage: In-memory backend for testing (no external deps)

;;;; ========================================================================
;;;; REDIS METRICS (Phase 2 - Database Architecture Hardening)
;;;; See docs/db.md "Persistence Monitoring & Metrics" section
;;;; ========================================================================

(defparameter *metrics-ring-buffer-size* 100
  "Number of samples to keep in latency ring buffers for percentile calculation.")

(defparameter *metrics-p99-warn-threshold-ms* 50.0
  "Log warning when Redis p99 latency exceeds this threshold (milliseconds).")

(defparameter *metrics-log-interval-seconds* 300.0
  "Log metrics summary every N seconds when verbose mode is enabled (5 minutes).")

(defstruct redis-metrics
  "Tracks Redis operation latencies using ring buffers for percentile calculation."
  ;; Ring buffer for save operation latencies (milliseconds)
  (save-latencies (make-array *metrics-ring-buffer-size* :initial-element 0.0)
   :type simple-vector)
  (save-head 0 :type fixnum)
  (save-count 0 :type fixnum)
  ;; Ring buffer for load operation latencies (milliseconds)
  (load-latencies (make-array *metrics-ring-buffer-size* :initial-element 0.0)
   :type simple-vector)
  (load-head 0 :type fixnum)
  (load-count 0 :type fixnum)
  ;; Total operation counters (since startup)
  (total-saves 0 :type fixnum)
  (total-loads 0 :type fixnum)
  (total-save-errors 0 :type fixnum)
  (total-load-errors 0 :type fixnum)
  ;; Last metrics log time
  (last-log-time 0.0 :type float))

(defparameter *redis-metrics* nil
  "Global Redis metrics instance. Initialized when storage is connected.")

(defun init-redis-metrics ()
  "Initialize global Redis metrics tracking."
  (setf *redis-metrics* (make-redis-metrics))
  (log-verbose "Redis metrics initialized"))

(defun ring-buffer-push (buffer head-accessor count-accessor value struct)
  "Push VALUE into a ring buffer, updating head and count in STRUCT.
   BUFFER is the array, HEAD-ACCESSOR and COUNT-ACCESSOR are slot accessors."
  (let* ((size (length buffer))
         (head (funcall head-accessor struct))
         (count (funcall count-accessor struct)))
    ;; Write value at current head position
    (setf (aref buffer head) value)
    ;; Advance head (circular)
    (funcall (fdefinition `(setf ,head-accessor))
             (mod (1+ head) size) struct)
    ;; Increment count (up to buffer size)
    (when (< count size)
      (funcall (fdefinition `(setf ,count-accessor))
               (1+ count) struct))))

(defun metrics-push-save-latency (latency-ms)
  "Record a save operation latency in metrics."
  (when *redis-metrics*
    (let ((buffer (redis-metrics-save-latencies *redis-metrics*))
          (head (redis-metrics-save-head *redis-metrics*))
          (size *metrics-ring-buffer-size*))
      (setf (aref buffer head) latency-ms)
      (setf (redis-metrics-save-head *redis-metrics*) (mod (1+ head) size))
      (when (< (redis-metrics-save-count *redis-metrics*) size)
        (incf (redis-metrics-save-count *redis-metrics*)))
      (incf (redis-metrics-total-saves *redis-metrics*)))))

(defun metrics-push-load-latency (latency-ms)
  "Record a load operation latency in metrics."
  (when *redis-metrics*
    (let ((buffer (redis-metrics-load-latencies *redis-metrics*))
          (head (redis-metrics-load-head *redis-metrics*))
          (size *metrics-ring-buffer-size*))
      (setf (aref buffer head) latency-ms)
      (setf (redis-metrics-load-head *redis-metrics*) (mod (1+ head) size))
      (when (< (redis-metrics-load-count *redis-metrics*) size)
        (incf (redis-metrics-load-count *redis-metrics*)))
      (incf (redis-metrics-total-loads *redis-metrics*)))))

(defun metrics-record-save-error ()
  "Increment save error counter."
  (when *redis-metrics*
    (incf (redis-metrics-total-save-errors *redis-metrics*))))

(defun metrics-record-load-error ()
  "Increment load error counter."
  (when *redis-metrics*
    (incf (redis-metrics-total-load-errors *redis-metrics*))))

(defun ring-buffer-values (buffer count)
  "Return list of valid values from ring buffer (up to COUNT elements)."
  (let ((result nil))
    (dotimes (i (min count (length buffer)))
      (push (aref buffer i) result))
    result))

(defun calculate-percentile (values percentile)
  "Calculate PERCENTILE (0-100) from list of VALUES.
   Returns NIL if no values."
  (when (and values (> (length values) 0))
    (let* ((sorted (sort (copy-list values) #'<))
           (n (length sorted))
           (idx (min (1- n) (floor (* n (/ percentile 100.0))))))
      (nth idx sorted))))

(defun get-redis-p99-save-latency ()
  "Return p99 save latency in milliseconds, or NIL if no data."
  (when *redis-metrics*
    (let ((values (ring-buffer-values
                   (redis-metrics-save-latencies *redis-metrics*)
                   (redis-metrics-save-count *redis-metrics*))))
      (calculate-percentile values 99))))

(defun get-redis-p99-load-latency ()
  "Return p99 load latency in milliseconds, or NIL if no data."
  (when *redis-metrics*
    (let ((values (ring-buffer-values
                   (redis-metrics-load-latencies *redis-metrics*)
                   (redis-metrics-load-count *redis-metrics*))))
      (calculate-percentile values 99))))

(defun get-redis-p50-save-latency ()
  "Return median (p50) save latency in milliseconds, or NIL if no data."
  (when *redis-metrics*
    (let ((values (ring-buffer-values
                   (redis-metrics-save-latencies *redis-metrics*)
                   (redis-metrics-save-count *redis-metrics*))))
      (calculate-percentile values 50))))

(defun get-redis-p50-load-latency ()
  "Return median (p50) load latency in milliseconds, or NIL if no data."
  (when *redis-metrics*
    (let ((values (ring-buffer-values
                   (redis-metrics-load-latencies *redis-metrics*)
                   (redis-metrics-load-count *redis-metrics*))))
      (calculate-percentile values 50))))

(defun check-redis-latency-threshold ()
  "Check if p99 latency exceeds threshold and log warning if so."
  (when *redis-metrics*
    (let ((p99-save (get-redis-p99-save-latency))
          (p99-load (get-redis-p99-load-latency)))
      (when (and p99-save (> p99-save *metrics-p99-warn-threshold-ms*))
        (warn "Redis p99 SAVE latency ~,2fms exceeds threshold ~,2fms"
              p99-save *metrics-p99-warn-threshold-ms*))
      (when (and p99-load (> p99-load *metrics-p99-warn-threshold-ms*))
        (warn "Redis p99 LOAD latency ~,2fms exceeds threshold ~,2fms"
              p99-load *metrics-p99-warn-threshold-ms*)))))

(defun maybe-log-redis-metrics ()
  "Log metrics summary periodically when verbose mode is enabled."
  (when (and *verbose* *redis-metrics*)
    (let* ((now (float (get-internal-real-time) 1.0))
           (last-log (redis-metrics-last-log-time *redis-metrics*))
           (interval (* *metrics-log-interval-seconds*
                        internal-time-units-per-second)))
      (when (> (- now last-log) interval)
        (setf (redis-metrics-last-log-time *redis-metrics*) now)
        (let ((p50-save (get-redis-p50-save-latency))
              (p99-save (get-redis-p99-save-latency))
              (p50-load (get-redis-p50-load-latency))
              (p99-load (get-redis-p99-load-latency))
              (total-saves (redis-metrics-total-saves *redis-metrics*))
              (total-loads (redis-metrics-total-loads *redis-metrics*))
              (save-errors (redis-metrics-total-save-errors *redis-metrics*))
              (load-errors (redis-metrics-total-load-errors *redis-metrics*)))
          (log-verbose "Redis metrics: saves=~d (errors=~d) loads=~d (errors=~d)"
                       total-saves save-errors total-loads load-errors)
          (when p50-save
            (log-verbose "  Save latency: p50=~,2fms p99=~,2fms" p50-save p99-save))
          (when p50-load
            (log-verbose "  Load latency: p50=~,2fms p99=~,2fms" p50-load p99-load))
          ;; Check threshold
          (check-redis-latency-threshold))))))

(defmacro with-redis-timing ((operation) &body body)
  "Execute BODY and record latency for OPERATION (:save or :load)."
  (let ((start (gensym "START"))
        (result (gensym "RESULT"))
        (elapsed (gensym "ELAPSED")))
    `(let ((,start (get-internal-real-time)))
       (let ((,result (progn ,@body)))
         (let ((,elapsed (/ (- (get-internal-real-time) ,start)
                           (/ internal-time-units-per-second 1000.0))))
           ,(case operation
              (:save `(metrics-push-save-latency ,elapsed))
              (:load `(metrics-push-load-latency ,elapsed))))
         ,result))))

;;;; Utility Functions

(defun parse-number (string)
  "Parse STRING as a number. Returns NIL if parsing fails."
  (handler-case
      (let ((*read-eval* nil))
        (let ((value (read-from-string string)))
          (when (numberp value) value)))
    (error () nil)))

;;;; Storage Protocol (Abstract Interface)

(defgeneric storage-load (storage key)
  (:documentation "Load data for KEY. Returns plist or NIL if not found."))

(defgeneric storage-save (storage key data)
  (:documentation "Save DATA under KEY. Returns T on success."))

(defgeneric storage-delete (storage key)
  (:documentation "Delete KEY. Returns T if existed."))

(defgeneric storage-exists-p (storage key)
  (:documentation "Return T if KEY exists."))

(defgeneric storage-flush (storage)
  (:documentation "Force any pending writes to durable storage."))

(defgeneric storage-connect (storage)
  (:documentation "Establish connection to storage backend."))

(defgeneric storage-disconnect (storage)
  (:documentation "Close connection to storage backend."))

(defgeneric storage-keys (storage pattern)
  (:documentation "Return list of keys matching PATTERN (glob-style, e.g. 'player:*')."))

(defgeneric storage-save-batch (storage key-data-pairs)
  (:documentation "Save multiple key-data pairs in a single batch operation.
   KEY-DATA-PAIRS is a list of (key . data) cons cells.
   Returns number of successfully saved pairs."))

;;;; ========================================================================
;;;; SESSION OWNERSHIP (Phase 3 - Database Architecture Hardening)
;;;; See docs/db.md "Session Ownership & Lease" section
;;;; ========================================================================

(defparameter *session-ownership-ttl-seconds* 60
  "TTL for session ownership keys. Should be longer than heartbeat interval.")

(defparameter *session-heartbeat-interval-seconds* 30.0
  "How often to refresh session ownership TTL.")

(defparameter *server-instance-id* nil
  "Unique identifier for this server instance. Set at startup.")

(defun generate-server-instance-id ()
  "Generate a unique server instance ID."
  (format nil "server-~a-~a" (get-universal-time) (random 1000000)))

(defun session-owner-key (player-id)
  "Return the Redis key for session ownership tracking."
  (format nil "session:owner:~a" player-id))

(defgeneric storage-setnx-with-ttl (storage key value ttl-seconds)
  (:documentation "Set KEY to VALUE only if it doesn't exist, with TTL in seconds.
   Returns T if key was set (claimed), NIL if key already exists."))

(defgeneric storage-refresh-ttl (storage key ttl-seconds)
  (:documentation "Refresh TTL on KEY. Returns T if key exists, NIL otherwise."))

(defgeneric storage-load-raw (storage key)
  (:documentation "Load raw string value for KEY. Returns string or NIL.
   Does not parse - returns the exact bytes stored in Redis.
   Use storage-load for parsed plist, storage-load-raw for size checks before parsing."))

;;;; ========================================================================
;;;; VALIDATION SUPPORT (Phase 6 - 4-Outcome Validation System)
;;;; See docs/db.md "Phase 6: 4-Outcome Validation System" section
;;;; ========================================================================

(defgeneric storage-incr (storage key)
  (:documentation "Increment counter at KEY by 1. Returns new value.
   Creates key with value 1 if it doesn't exist."))

(defgeneric storage-save-with-ttl (storage key data ttl-seconds)
  (:documentation "Save DATA at KEY with TTL expiration in seconds.
   Key will be automatically deleted after TTL expires.
   For forensic storage of corrupt blobs."))

;;;; ========================================================================
;;;; STRUCTURED DATA (Phase 4 - Database Architecture Hardening)
;;;; Sorted sets for leaderboards, sets for online tracking
;;;; See docs/db.md "Structured Redis Data" section
;;;; ========================================================================

;; Sorted Set operations (for leaderboards)
(defgeneric storage-zadd (storage key score member)
  (:documentation "Add MEMBER with SCORE to sorted set KEY. Updates if exists."))

(defgeneric storage-zincrby (storage key increment member)
  (:documentation "Increment MEMBER's score in sorted set KEY by INCREMENT.
   Returns new score."))

(defgeneric storage-zrevrange (storage key start stop &key withscores)
  (:documentation "Return members from sorted set KEY in descending score order.
   START and STOP are 0-based indices. If WITHSCORES, returns ((member score) ...)."))

(defgeneric storage-zrank (storage key member)
  (:documentation "Return rank of MEMBER in sorted set KEY (0-based, ascending).
   Returns NIL if member doesn't exist."))

(defgeneric storage-zrevrank (storage key member)
  (:documentation "Return rank of MEMBER in sorted set KEY (0-based, descending).
   Returns NIL if member doesn't exist."))

(defgeneric storage-zscore (storage key member)
  (:documentation "Return score of MEMBER in sorted set KEY. NIL if not exists."))

;; Set operations (for online player tracking)
(defgeneric storage-sadd (storage key member)
  (:documentation "Add MEMBER to set KEY. Returns T if added, NIL if already exists."))

(defgeneric storage-srem (storage key member)
  (:documentation "Remove MEMBER from set KEY. Returns T if removed."))

(defgeneric storage-scard (storage key)
  (:documentation "Return cardinality (count) of set KEY."))

(defgeneric storage-smembers (storage key)
  (:documentation "Return all members of set KEY."))

;;;; ========================================================================
;;;; STORAGE ERROR CONDITION (Phase 1 - Storage Failure Semantics)
;;;; Enables retry logic by signaling failures rather than returning nil
;;;; ========================================================================

(define-condition storage-error (error)
  ((operation :initarg :operation :reader storage-error-operation
              :documentation "The operation that failed (e.g., :save, :load, :batch-save)")
   (key :initarg :key :reader storage-error-key :initform nil
        :documentation "The key(s) involved, if applicable")
   (cause :initarg :cause :reader storage-error-cause :initform nil
          :documentation "The underlying error that caused the failure"))
  (:report (lambda (condition stream)
             (format stream "Storage ~a failed~@[ for key ~a~]~@[: ~a~]"
                     (storage-error-operation condition)
                     (storage-error-key condition)
                     (storage-error-cause condition))))
  (:documentation "Condition signaled when a storage operation fails.
   Used by retry macros to distinguish transient failures from not-found results."))

;;;; ========================================================================
;;;; LUA SCRIPT EXECUTION (Phase 5 - Trade System & Atomic Operations)
;;;; Enables atomic multi-key operations via Redis Lua scripting
;;;; See docs/db.md "Atomic Operations" section
;;;; ========================================================================

(defparameter *redis-script-shas* (make-hash-table :test 'equal)
  "Cache of script name -> SHA1 hash for EVALSHA calls.")

(defparameter *redis-script-bodies* (make-hash-table :test 'equal)
  "Cache of script name -> script body for NOSCRIPT recovery.")

(defgeneric storage-eval-script (storage script-name keys args)
  (:documentation "Execute a Lua script by name with KEYS and ARGS.
   KEYS is a list of Redis keys, ARGS is a list of additional arguments.
   Returns the script result."))

(defgeneric storage-script-load (storage script-name script-body)
  (:documentation "Load a Lua script into Redis script cache.
   Returns the SHA1 hash for EVALSHA calls."))

;;;; Redis Storage Implementation

(defclass redis-storage ()
  ((host :initarg :host
         :initform "127.0.0.1"
         :accessor redis-storage-host
         :documentation "Redis server hostname")
   (port :initarg :port
         :initform 6379
         :accessor redis-storage-port
         :documentation "Redis server port")
   (connected :initform nil
              :accessor redis-storage-connected
              :documentation "Whether connection is established"))
  (:documentation "Redis-backed storage implementation using cl-redis."))

(defmethod storage-connect ((storage redis-storage))
  "Establish Redis connection. Connection is per-thread in cl-redis."
  ;; cl-redis uses dynamic variables for connection management
  ;; The actual connection happens in with-connection macro
  (setf (redis-storage-connected storage) t)
  ;; Initialize metrics tracking
  (init-redis-metrics)
  (log-verbose "Redis storage configured for ~a:~a"
               (redis-storage-host storage)
               (redis-storage-port storage))
  t)

(defmethod storage-disconnect ((storage redis-storage))
  "Mark Redis storage as disconnected."
  (setf (redis-storage-connected storage) nil)
  (log-verbose "Redis storage disconnected")
  t)

(defmethod storage-load ((storage redis-storage) key)
  "Load data from Redis. Returns plist or NIL if not found."
  (handler-case
      (with-redis-timing (:load)
        (redis:with-connection (:host (redis-storage-host storage)
                                :port (redis-storage-port storage))
          (let ((raw (red:get key)))
            (when raw
              (let ((*read-eval* nil)) ; Security: disable eval in read
                (read-from-string raw))))))
    (error (e)
      (metrics-record-load-error)
      (warn "Redis load error for key ~a: ~a" key e)
      nil)))

(defmethod storage-save ((storage redis-storage) key data)
  "Save data to Redis using atomic write-then-rename pattern.
   Writes to a temp key first, then atomically renames to the real key.
   This prevents data corruption if the server crashes during write.
   Signals STORAGE-ERROR on failure (Phase 1: enables retry logic)."
  (handler-case
      (with-redis-timing (:save)
        (let ((temp-key (format nil "temp:~a:~a" key (get-internal-real-time))))
          (redis:with-connection (:host (redis-storage-host storage)
                                  :port (redis-storage-port storage))
            ;; Write to temporary key
            (red:set temp-key (prin1-to-string data))
            ;; Atomically rename temp key to real key
            ;; RENAME overwrites the destination key if it exists
            (red:rename temp-key key))
          t))
    (error (e)
      (metrics-record-save-error)
      (warn "Redis save error for key ~a: ~a" key e)
      ;; Signal storage-error for retry logic (Phase 1)
      (error 'storage-error :operation :save :key key :cause e))))

(defmethod storage-delete ((storage redis-storage) key)
  "Delete key from Redis. Returns T if existed."
  (handler-case
      (redis:with-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (> (red:del key) 0))
    (error (e)
      (warn "Redis delete error for key ~a: ~a" key e)
      nil)))

(defmethod storage-exists-p ((storage redis-storage) key)
  "Check if key exists in Redis."
  (handler-case
      (redis:with-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (let ((result (red:exists key)))
          (if (numberp result)
              (plusp result)
              result)))
    (error (e)
      (warn "Redis exists check error for key ~a: ~a" key e)
      nil)))

(defmethod storage-keys ((storage redis-storage) pattern)
  "Return list of keys matching PATTERN from Redis."
  (handler-case
      (redis:with-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (red:keys pattern))
    (error (e)
      (warn "Redis keys error for pattern ~a: ~a" pattern e)
      nil)))

(defmethod storage-flush ((storage redis-storage))
  "Trigger Redis BGSAVE for immediate snapshot."
  (handler-case
      (redis:with-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (red:bgsave)
        (log-verbose "Redis BGSAVE triggered")
        t)
    (error (e)
      (warn "Redis flush error: ~a" e)
      nil)))

(defmethod storage-save-batch ((storage redis-storage) key-data-pairs)
  "Batch save multiple key-data pairs using Redis pipelining.
   Uses write-then-rename pattern within the pipeline for atomicity.
   Returns count of pairs processed on success.
   Signals STORAGE-ERROR on failure (Phase 1: enables retry and dirty flag preservation)."
  (when (null key-data-pairs)
    (return-from storage-save-batch 0))
  (handler-case
      (let ((temp-keys nil)
            (count (length key-data-pairs))
            (timestamp (get-internal-real-time)))
        (redis:with-connection (:host (redis-storage-host storage)
                                :port (redis-storage-port storage))
          ;; Phase 1: Write all data to temp keys (pipelined)
          (redis:with-pipelining
            (dolist (pair key-data-pairs)
              (let* ((key (car pair))
                     (data (cdr pair))
                     (temp-key (format nil "temp:~a:~a" key timestamp)))
                (push (cons temp-key key) temp-keys)
                (red:set temp-key (prin1-to-string data)))))
          ;; Phase 2: Atomically rename all temp keys to real keys (pipelined)
          (redis:with-pipelining
            (dolist (temp-real temp-keys)
              (red:rename (car temp-real) (cdr temp-real)))))
        (log-verbose "Pipelined batch save: ~a keys" count)
        count)
    (error (e)
      (warn "Redis batch save error: ~a" e)
      ;; Signal storage-error for retry logic (Phase 1)
      (error 'storage-error :operation :batch-save
             :key (mapcar #'car key-data-pairs) :cause e))))

;;; Session Ownership Methods (Redis)

(defmethod storage-setnx-with-ttl ((storage redis-storage) key value ttl-seconds)
  "Atomic SET if Not eXists with TTL. Returns T if set, NIL if key exists."
  (handler-case
      (redis:with-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        ;; SET key value NX EX ttl - atomic setnx with expiration
        ;; Returns "OK" if set, NIL if key already exists
        (let ((result (red:set key value :nx t :ex ttl-seconds)))
          (not (null result))))
    (error (e)
      (warn "Redis SETNX error for key ~a: ~a" key e)
      nil)))

(defmethod storage-refresh-ttl ((storage redis-storage) key ttl-seconds)
  "Refresh TTL on KEY. Returns T if key exists, NIL otherwise."
  (handler-case
      (redis:with-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        ;; EXPIRE returns 1 if key exists and TTL was set, 0 otherwise
        (let ((result (red:expire key ttl-seconds)))
          (and (numberp result) (= result 1))))
    (error (e)
      (warn "Redis EXPIRE error for key ~a: ~a" key e)
      nil)))

(defmethod storage-load-raw ((storage redis-storage) key)
  "Load raw string value for KEY (does not parse).
   Returns NIL if key not found. Signals STORAGE-ERROR on failure
   (Phase 1: enables retry logic and distinguishes error from not-found)."
  (handler-case
      (redis:with-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (red:get key))
    (error (e)
      (warn "Redis GET error for key ~a: ~a" key e)
      ;; Signal storage-error so callers can retry (Phase 1)
      (error 'storage-error :operation :load-raw :key key :cause e))))

;;; Validation Support Methods (Redis) - Phase 6

(defmethod storage-incr ((storage redis-storage) key)
  "Increment counter at KEY. Returns new value."
  (handler-case
      (redis:with-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (red:incr key))
    (error (e)
      (warn "Redis INCR error for key ~a: ~a" key e)
      nil)))

(defmethod storage-save-with-ttl ((storage redis-storage) key data ttl-seconds)
  "Save DATA at KEY with TTL expiration."
  (handler-case
      (redis:with-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (red:setex key ttl-seconds (prin1-to-string data))
        t)
    (error (e)
      (warn "Redis SETEX error for key ~a: ~a" key e)
      nil)))

;;; Sorted Set Methods (Redis) - for leaderboards

(defmethod storage-zadd ((storage redis-storage) key score member)
  "Add member to sorted set with score."
  (handler-case
      (redis:with-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (red:zadd key score member)
        t)
    (error (e)
      (warn "Redis ZADD error for key ~a: ~a" key e)
      nil)))

(defmethod storage-zincrby ((storage redis-storage) key increment member)
  "Increment member's score in sorted set."
  (handler-case
      (redis:with-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (let ((result (red:zincrby key increment member)))
          (if (stringp result)
              (parse-number result)
              result)))
    (error (e)
      (warn "Redis ZINCRBY error for key ~a: ~a" key e)
      nil)))

(defmethod storage-zrevrange ((storage redis-storage) key start stop &key withscores)
  "Get members in descending score order."
  (handler-case
      (redis:with-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (if withscores
            ;; ZREVRANGE returns alternating member/score when WITHSCORES
            (let ((result (red:zrevrange key start stop :withscores t)))
              ;; Convert flat list to ((member score) ...) pairs
              (loop for (member score) on result by #'cddr
                    collect (list member (if (stringp score)
                                             (parse-number score)
                                             score))))
            (red:zrevrange key start stop)))
    (error (e)
      (warn "Redis ZREVRANGE error for key ~a: ~a" key e)
      nil)))

(defmethod storage-zrank ((storage redis-storage) key member)
  "Get rank of member (ascending order, 0-based)."
  (handler-case
      (redis:with-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (red:zrank key member))
    (error (e)
      (warn "Redis ZRANK error for key ~a: ~a" key e)
      nil)))

(defmethod storage-zrevrank ((storage redis-storage) key member)
  "Get rank of member (descending order, 0-based)."
  (handler-case
      (redis:with-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (red:zrevrank key member))
    (error (e)
      (warn "Redis ZREVRANK error for key ~a: ~a" key e)
      nil)))

(defmethod storage-zscore ((storage redis-storage) key member)
  "Get score of member in sorted set."
  (handler-case
      (redis:with-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (let ((result (red:zscore key member)))
          (when result
            (if (stringp result)
                (parse-number result)
                result))))
    (error (e)
      (warn "Redis ZSCORE error for key ~a: ~a" key e)
      nil)))

;;; Set Methods (Redis) - for online player tracking

(defmethod storage-sadd ((storage redis-storage) key member)
  "Add member to set."
  (handler-case
      (redis:with-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (let ((result (red:sadd key member)))
          (and (numberp result) (> result 0))))
    (error (e)
      (warn "Redis SADD error for key ~a: ~a" key e)
      nil)))

(defmethod storage-srem ((storage redis-storage) key member)
  "Remove member from set."
  (handler-case
      (redis:with-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (let ((result (red:srem key member)))
          (and (numberp result) (> result 0))))
    (error (e)
      (warn "Redis SREM error for key ~a: ~a" key e)
      nil)))

(defmethod storage-scard ((storage redis-storage) key)
  "Get count of members in set."
  (handler-case
      (redis:with-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (let ((result (red:scard key)))
          (if (numberp result) result 0)))
    (error (e)
      (warn "Redis SCARD error for key ~a: ~a" key e)
      0)))

(defmethod storage-smembers ((storage redis-storage) key)
  "Get all members of set."
  (handler-case
      (redis:with-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (red:smembers key))
    (error (e)
      (warn "Redis SMEMBERS error for key ~a: ~a" key e)
      nil)))

;;; Lua Script Execution Methods (Redis)

(defmethod storage-script-load ((storage redis-storage) script-name script-body)
  "Load a Lua script into Redis and cache its SHA.
   Also caches the script body for NOSCRIPT recovery."
  (handler-case
      (redis:with-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (let ((sha (red:script-load script-body)))
          (setf (gethash script-name *redis-script-shas*) sha)
          ;; Cache body for NOSCRIPT recovery (Phase F)
          (setf (gethash script-name *redis-script-bodies*) script-body)
          (log-verbose "Loaded Redis script ~a with SHA ~a" script-name sha)
          sha))
    (error (e)
      (warn "Redis SCRIPT LOAD error for ~a: ~a" script-name e)
      nil)))

(defmethod storage-eval-script ((storage redis-storage) script-name keys args)
  "Execute a Lua script by name using EVALSHA with NOSCRIPT recovery.
   If Redis has evicted the script, reloads from cached body and retries."
  (let ((sha (gethash script-name *redis-script-shas*)))
    (unless sha
      (warn "Script ~a not loaded - call storage-script-load first" script-name)
      (return-from storage-eval-script nil))
    (handler-case
        (redis:with-connection (:host (redis-storage-host storage)
                                :port (redis-storage-port storage))
          ;; EVALSHA sha numkeys key1 key2 ... arg1 arg2 ...
          (let* ((num-keys (length keys))
                 (all-args (append keys args)))
            (apply #'red:evalsha sha num-keys all-args)))
      (error (e)
        ;; If NOSCRIPT error, script was evicted - reload and retry (Phase F)
        (let ((err-str (format nil "~a" e)))
          (if (search "NOSCRIPT" err-str)
              ;; Script evicted - attempt reload from cached body
              (let ((script-body (gethash script-name *redis-script-bodies*)))
                (if script-body
                    (progn
                      (log-verbose "Script ~a evicted, reloading..." script-name)
                      (handler-case
                          (redis:with-connection (:host (redis-storage-host storage)
                                                  :port (redis-storage-port storage))
                            (let ((new-sha (red:script-load script-body)))
                              (setf (gethash script-name *redis-script-shas*) new-sha)
                              ;; Retry with new SHA
                              (let* ((num-keys (length keys))
                                     (all-args (append keys args)))
                                (apply #'red:evalsha new-sha num-keys all-args))))
                        (error (retry-e)
                          (warn "Script ~a reload/retry failed: ~a" script-name retry-e)
                          nil)))
                    (progn
                      (warn "Script ~a evicted but body not cached - cannot reload" script-name)
                      nil)))
              (progn
                (warn "Redis EVALSHA error for ~a: ~a" script-name e)
                nil)))))))

;;;; Memory Storage Implementation (for testing)

(defclass memory-storage ()
  ((data :initform (make-hash-table :test 'equal)
         :accessor memory-storage-data
         :documentation "Hash table storing all data"))
  (:documentation "In-memory storage for testing. No external dependencies."))

(defmethod storage-connect ((storage memory-storage))
  "No-op for memory storage."
  (log-verbose "Memory storage initialized")
  t)

(defmethod storage-disconnect ((storage memory-storage))
  "No-op for memory storage."
  (log-verbose "Memory storage disconnected")
  t)

(defmethod storage-load ((storage memory-storage) key)
  "Load data from memory hash table. Returns NIL for expired keys."
  ;; Check expiration first (Phase G: TTL consistency)
  (when (memory-storage-key-expired-p key)
    (memory-storage-cleanup-key storage key)
    (return-from storage-load nil))
  (gethash key (memory-storage-data storage)))

(defmethod storage-save ((storage memory-storage) key data)
  "Save data to memory hash table."
  (setf (gethash key (memory-storage-data storage)) data)
  t)

(defmethod storage-delete ((storage memory-storage) key)
  "Delete key from memory hash table."
  (remhash key (memory-storage-data storage)))

(defmethod storage-exists-p ((storage memory-storage) key)
  "Check if key exists in memory hash table. Returns NIL for expired keys."
  ;; Check expiration first (Phase G: TTL consistency)
  (when (memory-storage-key-expired-p key)
    (memory-storage-cleanup-key storage key)
    (return-from storage-exists-p nil))
  (nth-value 1 (gethash key (memory-storage-data storage))))

(defmethod storage-keys ((storage memory-storage) pattern)
  "Return list of keys matching PATTERN from memory hash table.
   Supports simple prefix matching: 'player:*' matches all keys starting with 'player:'.
   Filters out expired keys (Phase G: TTL consistency)."
  (let* ((star-pos (position #\* pattern))
         (prefix (if star-pos (subseq pattern 0 star-pos) pattern))
         (result nil))
    (maphash (lambda (key value)
               (declare (ignore value))
               ;; Skip expired keys
               (unless (memory-storage-key-expired-p key)
                 (when (if star-pos
                           (and (>= (length key) (length prefix))
                                (string= prefix key :end2 (length prefix)))
                           (string= pattern key))
                   (push key result))))
             (memory-storage-data storage))
    result))

(defmethod storage-flush ((storage memory-storage))
  "No-op for memory storage (already durable in RAM)."
  t)

(defmethod storage-save-batch ((storage memory-storage) key-data-pairs)
  "Batch save for memory storage - just loops (no network benefit)."
  (let ((count 0))
    (dolist (pair key-data-pairs)
      (setf (gethash (car pair) (memory-storage-data storage)) (cdr pair))
      (incf count))
    count))

;;; Session Ownership Methods (Memory Storage)
;;; Memory storage tracks TTLs using a separate hash table with expiration times.

(defparameter *memory-storage-ttls* (make-hash-table :test 'equal)
  "Hash table mapping keys to their expiration times (universal-time).")

(defun memory-storage-key-expired-p (key)
  "Check if KEY has expired in memory storage.
   Returns T only if key has a TTL set AND that TTL has passed.
   Keys without TTLs are never considered expired."
  (let ((expiration (gethash key *memory-storage-ttls*)))
    (and expiration
         (< expiration (get-universal-time)))))

(defun memory-storage-cleanup-key (storage key)
  "Remove expired key from memory storage."
  (when (memory-storage-key-expired-p key)
    (remhash key (memory-storage-data storage))
    (remhash key *memory-storage-ttls*)))

(defmethod storage-setnx-with-ttl ((storage memory-storage) key value ttl-seconds)
  "Set key only if it doesn't exist or has expired, with TTL."
  ;; Check if key exists and is not expired
  (let ((current (gethash key (memory-storage-data storage)))
        (expiration (gethash key *memory-storage-ttls*)))
    (if (and current expiration (> expiration (get-universal-time)))
        ;; Key exists and not expired - fail
        nil
        ;; Key doesn't exist or expired - set it
        (progn
          (setf (gethash key (memory-storage-data storage)) value)
          (setf (gethash key *memory-storage-ttls*) (+ (get-universal-time) ttl-seconds))
          t))))

(defmethod storage-refresh-ttl ((storage memory-storage) key ttl-seconds)
  "Refresh TTL on KEY. Returns T if key exists and is not expired."
  (let ((current (gethash key (memory-storage-data storage)))
        (expiration (gethash key *memory-storage-ttls*)))
    (if (and current expiration (> expiration (get-universal-time)))
        ;; Key exists and not expired - refresh TTL
        (progn
          (setf (gethash key *memory-storage-ttls*) (+ (get-universal-time) ttl-seconds))
          t)
        ;; Key doesn't exist or expired
        nil)))

(defmethod storage-load-raw ((storage memory-storage) key)
  "Load raw string value for KEY from memory storage.
   If stored value is already a string, return it directly.
   Otherwise, return prin1-to-string of stored data for consistency with Redis.
   This handles both session ownership (stores strings) and player data (stores plists)."
  ;; Check expiration
  (when (memory-storage-key-expired-p key)
    (memory-storage-cleanup-key storage key)
    (return-from storage-load-raw nil))
  (let ((data (gethash key (memory-storage-data storage))))
    (when data
      ;; If data is already a string, return it directly (e.g., session ownership IDs)
      ;; If data is a plist, convert to string (e.g., player data blobs)
      (if (stringp data)
          data
          (prin1-to-string data)))))

;;; Validation Support Methods (Memory Storage) - Phase 6

(defmethod storage-incr ((storage memory-storage) key)
  "Increment counter at KEY. Returns new value."
  (let ((current (gethash key (memory-storage-data storage) 0)))
    (setf (gethash key (memory-storage-data storage)) (1+ current))))

(defmethod storage-save-with-ttl ((storage memory-storage) key data ttl-seconds)
  "Save DATA at KEY with TTL expiration."
  (setf (gethash key (memory-storage-data storage)) data)
  (setf (gethash key *memory-storage-ttls*) (+ (get-universal-time) ttl-seconds))
  t)

;;; Sorted Set Methods (Memory Storage) - for leaderboards
;;; Uses hash table mapping key -> list of (member . score) pairs

(defparameter *memory-storage-sorted-sets* (make-hash-table :test 'equal)
  "Hash table for sorted set emulation: key -> ((member . score) ...)")

(defparameter *memory-storage-sets* (make-hash-table :test 'equal)
  "Hash table for set emulation: key -> hash-table of members")

(defun memory-sorted-set-get (key)
  "Get sorted set entries for KEY as a list sorted by score (ascending)."
  (let ((entries (gethash key *memory-storage-sorted-sets*)))
    (sort (copy-list entries) #'< :key #'cdr)))

(defun memory-sorted-set-put (key entries)
  "Store sorted set ENTRIES for KEY."
  (setf (gethash key *memory-storage-sorted-sets*) entries))

(defmethod storage-zadd ((storage memory-storage) key score member)
  "Add member to sorted set with score (memory implementation)."
  (let* ((entries (gethash key *memory-storage-sorted-sets*))
         (existing (assoc member entries :test #'equal)))
    (if existing
        ;; Update existing score
        (setf (cdr existing) score)
        ;; Add new entry
        (push (cons member score) entries))
    (memory-sorted-set-put key entries)
    t))

(defmethod storage-zincrby ((storage memory-storage) key increment member)
  "Increment member's score in sorted set (memory implementation)."
  (let* ((entries (gethash key *memory-storage-sorted-sets*))
         (existing (assoc member entries :test #'equal)))
    (if existing
        (progn
          (incf (cdr existing) increment)
          (cdr existing))
        (progn
          ;; Add new entry with increment as initial score
          (push (cons member increment) entries)
          (memory-sorted-set-put key entries)
          increment))))

(defmethod storage-zrevrange ((storage memory-storage) key start stop &key withscores)
  "Get members in descending score order (memory implementation)."
  (let* ((sorted (sort (copy-list (gethash key *memory-storage-sorted-sets*))
                       #'> :key #'cdr))
         (len (length sorted))
         ;; Handle negative indices like Redis
         (actual-stop (if (< stop 0) (+ len stop) stop))
         (result (subseq sorted start (min (1+ actual-stop) len))))
    (if withscores
        (mapcar (lambda (entry) (list (car entry) (cdr entry))) result)
        (mapcar #'car result))))

(defmethod storage-zrank ((storage memory-storage) key member)
  "Get rank of member (ascending, memory implementation)."
  (let* ((sorted (sort (copy-list (gethash key *memory-storage-sorted-sets*))
                       #'< :key #'cdr)))
    (position member sorted :test #'equal :key #'car)))

(defmethod storage-zrevrank ((storage memory-storage) key member)
  "Get rank of member (descending, memory implementation)."
  (let* ((sorted (sort (copy-list (gethash key *memory-storage-sorted-sets*))
                       #'> :key #'cdr)))
    (position member sorted :test #'equal :key #'car)))

(defmethod storage-zscore ((storage memory-storage) key member)
  "Get score of member (memory implementation)."
  (let ((entry (assoc member (gethash key *memory-storage-sorted-sets*) :test #'equal)))
    (when entry (cdr entry))))

;;; Set Methods (Memory Storage) - for online player tracking

(defmethod storage-sadd ((storage memory-storage) key member)
  "Add member to set (memory implementation)."
  (let ((set (or (gethash key *memory-storage-sets*)
                 (setf (gethash key *memory-storage-sets*)
                       (make-hash-table :test 'equal)))))
    (unless (gethash member set)
      (setf (gethash member set) t)
      t)))

(defmethod storage-srem ((storage memory-storage) key member)
  "Remove member from set (memory implementation)."
  (let ((set (gethash key *memory-storage-sets*)))
    (when set
      (remhash member set)
      t)))

(defmethod storage-scard ((storage memory-storage) key)
  "Get count of members in set (memory implementation)."
  (let ((set (gethash key *memory-storage-sets*)))
    (if set (hash-table-count set) 0)))

(defmethod storage-smembers ((storage memory-storage) key)
  "Get all members of set (memory implementation)."
  (let ((set (gethash key *memory-storage-sets*))
        (members nil))
    (when set
      (maphash (lambda (k v) (declare (ignore v)) (push k members)) set))
    members))

;;; Lua Script Execution Methods (Memory Storage)
;;; For testing, we emulate known scripts by dispatching on script name.

(defparameter *memory-script-handlers* (make-hash-table :test 'equal)
  "Hash table mapping script names to handler functions for memory storage.")

(defmethod storage-script-load ((storage memory-storage) script-name script-body)
  "No-op for memory storage - scripts are emulated via handlers."
  (declare (ignore script-body))
  ;; Return a fake SHA (the script name itself)
  (setf (gethash script-name *redis-script-shas*) script-name)
  (log-verbose "Memory storage: registered script ~a" script-name)
  script-name)

(defmethod storage-eval-script ((storage memory-storage) script-name keys args)
  "Execute emulated script by name in memory storage."
  (let ((handler (gethash script-name *memory-script-handlers*)))
    (if handler
        (funcall handler storage keys args)
        (progn
          (warn "Memory storage: no handler for script ~a" script-name)
          nil))))

;; Register emulated trade_complete script handler
;; Phase 3: Updated to verify ownership and parse strings to plists
(setf (gethash "trade_complete" *memory-script-handlers*)
      (lambda (storage keys args)
        "Emulate trade_complete.lua: atomic item swap between two players.
         Phase 3: Verifies session ownership before committing.
         KEYS: [player1-key, player2-key, owner1-key, owner2-key]
         ARGS: [serialized-player1-data, serialized-player2-data, expected-owner]"
        (block trade-handler
          (let ((key1 (first keys))
                (key2 (second keys))
                (owner-key1 (third keys))
                (owner-key2 (fourth keys))
                (data1 (first args))
                (data2 (second args))
                (expected-owner (third args)))
            ;; Validate inputs
            (unless (and key1 key2 data1 data2)
              (return-from trade-handler "TRADE_ERROR: Missing required parameters"))
            ;; Phase 3: Verify ownership (same logic as Lua script)
            ;; Use storage-load-raw for TTL-aware reads (P2 fix: respect key expiration)
            (when (and owner-key1 owner-key2 expected-owner)
              (let ((actual-owner1 (storage-load-raw storage owner-key1))
                    (actual-owner2 (storage-load-raw storage owner-key2)))
                (unless (equal actual-owner1 expected-owner)
                  (return-from trade-handler
                    (format nil "TRADE_ERROR: Ownership mismatch for player 1 (expected ~a, got ~a)"
                            expected-owner actual-owner1)))
                (unless (equal actual-owner2 expected-owner)
                  (return-from trade-handler
                    (format nil "TRADE_ERROR: Ownership mismatch for player 2 (expected ~a, got ~a)"
                            expected-owner actual-owner2)))))
            ;; Phase 3: Parse strings to plists before storing (memory backend consistency)
            ;; This ensures db-load-player works correctly after trades
            (let* ((*read-eval* nil)  ; Security: disable eval in read
                   (plist1 (read-from-string data1))
                   (plist2 (read-from-string data2)))
              ;; Atomically update both player records
              ;; In memory storage, this is inherently atomic (single-threaded)
              (setf (gethash key1 (memory-storage-data storage)) plist1)
              (setf (gethash key2 (memory-storage-data storage)) plist2)
              "OK")))))

;;;; Key Schema Functions

(defun player-key (player-id)
  "Generate Redis key for player data."
  (format nil "player:~a" player-id))

(defun zone-objects-key (zone-id)
  "Generate Redis key for zone object state."
  (format nil "zone:~a:objects" zone-id))

(defun server-config-key ()
  "Generate Redis key for server configuration."
  "server:config")

(defun server-id-counter-key ()
  "Generate Redis key for global ID counter."
  "server:id-counter")

;;; Leaderboard Keys (Phase 4 - Structured Data)
;;; See docs/db.md "Structured Redis Data" section

(defun leaderboard-key (category)
  "Generate Redis key for leaderboard. CATEGORY is :xp, :level, or :deaths."
  (format nil "leaderboard:~a" (string-downcase (symbol-name category))))

(defun online-players-key ()
  "Generate Redis key for online players set."
  "online:players")

;;; Future leaderboards (stubs for later implementation):
;;; - (defun leaderboard-kills-key () "leaderboard:pvp-kills")  ; PvP kills tracking
;;; - (defun leaderboard-skill-key (skill) ...)                  ; Skill levels (smithing, fishing, etc.)

;;;; Global Storage Instance

(defparameter *storage* nil
  "Global storage backend instance. Set at startup to redis-storage or memory-storage.")

(defun init-storage (&key (backend :redis) (host "127.0.0.1") (port 6379))
  "Initialize global storage backend.
   BACKEND can be :redis or :memory.
   For :redis, HOST and PORT specify the Redis server."
  ;; Generate server instance ID for session ownership
  (setf *server-instance-id* (generate-server-instance-id))
  (log-verbose "Server instance ID: ~a" *server-instance-id*)
  ;; Initialize storage backend
  (setf *storage*
        (ecase backend
          (:redis (make-instance 'redis-storage :host host :port port))
          (:memory (make-instance 'memory-storage))))
  (storage-connect *storage*)
  (log-verbose "Storage initialized: ~a" backend)
  *storage*)

(defun shutdown-storage ()
  "Disconnect from storage backend."
  (when *storage*
    (storage-disconnect *storage*)
    (setf *storage* nil)))

;;;; ========================================================================
;;;; FORENSIC STORAGE (Phase 6 - 4-Outcome Validation System)
;;;; Store corrupt blobs for admin inspection, validation metrics
;;;; ========================================================================

(defparameter *corrupt-blob-ttl-seconds* 604800
  "TTL for corrupt blob forensic storage (7 days = 604800 seconds).")

(defun store-corrupt-blob (player-id raw-string report)
  "Store corrupt player data for forensic inspection with TTL.
   KEY: corrupt:{player-id}:{timestamp}
   DATA: (:raw raw-string :report report-list :timestamp unix-time)
   TTL: 7 days (auto-expires to prevent unbounded growth)"
  (when *storage*
    (let ((key (format nil "corrupt:~a:~a" player-id (get-universal-time)))
          (data (list :raw raw-string
                      :report report
                      :timestamp (get-universal-time))))
      (storage-save-with-ttl *storage* key data *corrupt-blob-ttl-seconds*)
      (log-verbose "Stored corrupt blob for player ~a: ~{~a~^, ~}" player-id report))))

(defun increment-validation-metric (action)
  "Increment validation outcome counter.
   ACTION should be :ok, :clamp, :quarantine, or :reject."
  (when *storage*
    (storage-incr *storage* (format nil "metrics:validation:~a"
                                     (string-downcase (symbol-name action))))))

(defun get-validation-metrics ()
  "Return validation metrics as a plist.
   Returns (:ok N :clamp N :quarantine N :reject N)"
  (when *storage*
    (list :ok (or (storage-load *storage* "metrics:validation:ok") 0)
          :clamp (or (storage-load *storage* "metrics:validation:clamp") 0)
          :quarantine (or (storage-load *storage* "metrics:validation:quarantine") 0)
          :reject (or (storage-load *storage* "metrics:validation:reject") 0))))

(defun db-save-id-counter (counter-value)
  "Save the global ID counter to storage."
  (storage-save *storage* (server-id-counter-key) counter-value))

(defun db-load-id-counter ()
  "Load the global ID counter from storage with retry (critical for server startup).
   Returns 1 if not found or all retries exhausted (ID 0 is reserved/invalid)."
  (let ((data (with-retry-exponential (loaded (lambda () (storage-load *storage* (server-id-counter-key)))
                                        :max-retries 5
                                        :initial-delay 200
                                        :max-delay 2000
                                        :on-final-fail (lambda (e)
                                                         (warn "CRITICAL: Failed to load ID counter after all retries: ~a. Starting from 1 may cause ID collisions!" e)))
                loaded)))
    (if (and data (integerp data) (> data 0))
        data
        1)))

;;;; Migration System
;;;;
;;;; Core migration logic is in migrations.lisp (schema version, migration functions).
;;;; migrate-player-data is called on each player load (lazy migration).
;;;; migrate-all-players below provides eager migration for admin use.

(defun migrate-all-players (&key (dry-run nil) (verbose t))
  "Migrate all players in storage to current schema version.
   Use this before major deploys to pre-migrate inactive players.
   Options:
     :dry-run t   - Report what would be migrated without saving
     :verbose t   - Print progress (default)
   Returns: (values migrated-count skipped-count error-count)"
  (unless *storage*
    (warn "No storage backend initialized")
    (return-from migrate-all-players (values 0 0 0)))
  (let ((keys (storage-keys *storage* "player:*"))
        (migrated 0)
        (skipped 0)
        (errors 0))
    (when verbose
      (format t "~&Found ~a player records to check~%" (length keys)))
    (dolist (key keys)
      (handler-case
          (let ((data (storage-load *storage* key)))
            (if (null data)
                (progn
                  (when verbose
                    (format t "  ~a: no data (skipped)~%" key))
                  (incf skipped))
                (let ((version (getf data :version 0)))
                  (if (>= version *player-schema-version*)
                      (progn
                        (when verbose
                          (format t "  ~a: v~a (current, skipped)~%" key version))
                        (incf skipped))
                      (progn
                        (when verbose
                          (format t "  ~a: v~a -> v~a~%" key version *player-schema-version*))
                        (let ((migrated-data (migrate-player-data data)))
                          (unless dry-run
                            (storage-save *storage* key migrated-data)))
                        (incf migrated))))))
        (error (e)
          (when verbose
            (format t "  ~a: ERROR ~a~%" key e))
          (incf errors))))
    (when verbose
      (format t "~&Migration complete: ~a migrated, ~a skipped, ~a errors~%"
              migrated skipped errors)
      (when dry-run
        (format t "(dry-run mode - no changes saved)~%")))
    (values migrated skipped errors)))

(export 'migrate-all-players)

;;;; High-Level Convenience Functions

(defun db-save-player (player)
  "Save player to storage using current schema version.
   Returns T on success. Signals STORAGE-ERROR on failure (Phase 1).
   Callers using with-retry-exponential will automatically retry on error."
  (when (and *storage* player)
    (let* ((player-id (player-id player))
           (key (player-key player-id))
           (session (gethash player-id *player-sessions*))
           (zone-id (and session (player-session-zone-id session)))
           ;; Use serialize-player from save.lisp (no visuals for DB, include zone-id)
           (data (serialize-player player :include-visuals nil :zone-id zone-id)))
      ;; Add version to serialized data
      (setf data (plist-put data :version *player-schema-version*))
      ;; storage-save signals storage-error on failure (Phase 1)
      (storage-save *storage* key data)
      ;; Only update counters and log on success
      (when (boundp '*server-total-saves*)
        (incf *server-total-saves*))
      (log-verbose "Saved player ~a to storage (zone: ~a)" player-id zone-id)
      t)))

(defun db-load-player (player-id)
  "Load player by ID, running migrations if needed.
   Returns player struct or NIL if not found."
  (when *storage*
    (let* ((key (player-key player-id))
           (data (storage-load *storage* key)))
      (when data
        ;; Run migrations
        (setf data (migrate-player-data data))
        ;; Deserialize using save.lisp functions
        (let* ((player (deserialize-player data
                                           *inventory-size*
                                           (length *equipment-slot-ids*)))
               (zone-id (getf data :zone-id)))
          (log-verbose "Loaded player ~a from storage (version ~a)"
                       player-id (getf data :version))
          (values player zone-id))))))

(defun db-load-player-validated (player-id)
  "Load player with 4-outcome validation.
   Returns (values player zone-id action).

   PRECONDITION: Caller has already claimed session ownership for player-id.
   This ensures that if the :clamp branch needs to save corrected data,
   the ownership-safe save path will succeed.

   Actions returned:
   - :ok          = Data valid, player loaded normally
   - :clamp       = Minor issues fixed, corrected data saved, player loaded
   - :quarantine  = Suspicious data, quarantine player returned (can't play)
   - :reject      = Exploit-adjacent data, login denied (nil player)
   - :not-found   = No data for player-id (nil player)

   The load pipeline is:
   1. Load raw string (storage-load-raw)
   2. Size check BEFORE parsing (prevents allocation attacks)
   3. Parse with read-from-string (*read-eval* nil)
   4. Basic sanity (is it a plist? has :id? :version OK?)
   5. Migrate to current schema
   6. 4-way validation against current schema
   7. Handle outcome

   See docs/db.md 'Phase 6: 4-Outcome Validation System' for details."
  (unless *storage*
    (return-from db-load-player-validated (values nil nil :not-found)))

  (let* ((key (player-key player-id))
         (raw-string (storage-load-raw *storage* key)))

    ;; No data found
    (unless raw-string
      (return-from db-load-player-validated (values nil nil :not-found)))

    ;; 1. Size check BEFORE parsing (prevents allocation attacks)
    (when (> (length raw-string) *max-player-blob-size*)
      (store-corrupt-blob player-id raw-string
                          (list (format nil "Blob size ~d exceeds max ~d bytes"
                                        (length raw-string) *max-player-blob-size*)))
      (increment-validation-metric :reject)
      (return-from db-load-player-validated (values nil nil :reject)))

    ;; 2. Parse with error handling
    (let ((raw-data
            (handler-case
                (let ((*read-eval* nil))
                  (multiple-value-bind (parsed-plist end-pos)
                      (read-from-string raw-string)
                    (declare (ignore end-pos))
                    parsed-plist))
              (error (e)
                (store-corrupt-blob player-id raw-string
                                    (list (format nil "Parse error: ~a" e)))
                (increment-validation-metric :reject)
                (return-from db-load-player-validated (values nil nil :reject))))))

      ;; 3. Basic sanity (pre-migration) - can we migrate at all?
      ;; - Must be a list
      ;; - Must have :id (integer)
      ;; - :version missing is OK (treated as 0 by migration)
      ;; - :version present must be integer
      (let ((version-val (getf raw-data :version)))
        (unless (and (listp raw-data)
                     (integerp (getf raw-data :id))
                     (or (null version-val) (integerp version-val)))
          (store-corrupt-blob player-id raw-string
                              '("Malformed structure: not a valid player data"))
          (increment-validation-metric :reject)
          (return-from db-load-player-validated (values nil nil :reject))))

      ;; 4. Migrate to current schema (adds missing fields)
      (let ((migrated-data (migrate-player-data raw-data)))

        ;; 5. Full 4-way validation (against current schema, after migration)
        (multiple-value-bind (action issues fixed-plist)
            (validate-player-plist-4way migrated-data)
          (increment-validation-metric action)

          (case action
            (:ok
             (let ((player (deserialize-player migrated-data *inventory-size*
                                               (length *equipment-slot-ids*)))
                   (zone-id (getf migrated-data :zone-id)))
               (log-verbose "Loaded and validated player ~a from storage (version ~a)"
                            player-id (getf migrated-data :version))
               (values player zone-id :ok)))

            (:clamp
             (log-verbose "Player ~a clamped: ~{~a~^, ~}" player-id issues)
             ;; Tier-1 save of corrected data using ownership-safe path
             (let ((player (deserialize-player fixed-plist *inventory-size*
                                               (length *equipment-slot-ids*))))
               (db-save-player-immediate player)
               (values player (getf fixed-plist :zone-id) :clamp)))

            (:quarantine
             (store-corrupt-blob player-id raw-string issues)
             (warn "Player ~a QUARANTINED: ~{~a~^, ~}" player-id issues)
             (values (make-quarantined-player player-id) :quarantine :quarantine))

            (:reject
             (store-corrupt-blob player-id raw-string issues)
             (warn "Player ~a REJECTED: ~{~a~^, ~}" player-id issues)
             (values nil nil :reject))))))))

(defun make-quarantined-player (player-id)
  "Create minimal player for quarantine state.
   The player cannot play but their account is recoverable.
   They are placed in the :quarantine zone (special handling)."
  (let ((player (make-player 0.0 0.0)))
    (setf (player-id player) player-id)
    (setf (player-zone-id player) :quarantine)
    player))

(defun db-delete-player (player-id)
  "Delete player from storage."
  (when *storage*
    (let ((key (player-key player-id)))
      (storage-delete *storage* key)
      (log-verbose "Deleted player ~a from storage" player-id))))

(defun db-player-exists-p (player-id)
  "Check if player exists in storage."
  (when *storage*
    (storage-exists-p *storage* (player-key player-id))))

(defun db-save-zone-objects (zone-id objects)
  "Save zone objects to storage."
  (when *storage*
    (let ((key (zone-objects-key zone-id))
          (data (list :version 1
                      :zone-id zone-id
                      :objects (mapcar #'serialize-object objects))))
      (storage-save *storage* key data)
      (log-verbose "Saved zone ~a objects to storage" zone-id))))

(defun db-load-zone-objects (zone-id)
  "Load zone objects from storage. Returns list of objects or NIL."
  (when *storage*
    (let* ((key (zone-objects-key zone-id))
           (data (storage-load *storage* key)))
      (when data
        (mapcar #'deserialize-object (getf data :objects))))))

;;;; ========================================================================
;;;; LEADERBOARD SYSTEM (Phase 4 - Structured Data)
;;;; See docs/db.md "Structured Redis Data" section
;;;; ========================================================================

(defun db-update-leaderboard-xp (player-id xp)
  "Update player's XP on the XP leaderboard."
  (when *storage*
    (storage-zadd *storage* (leaderboard-key :xp) xp (prin1-to-string player-id))))

(defun db-update-leaderboard-level (player-id level)
  "Update player's level on the level leaderboard."
  (when *storage*
    (storage-zadd *storage* (leaderboard-key :level) level (prin1-to-string player-id))))

(defun db-update-leaderboard-deaths (player-id)
  "Increment player's death count on the deaths leaderboard."
  (when *storage*
    (storage-zincrby *storage* (leaderboard-key :deaths) 1 (prin1-to-string player-id))))

(defun db-get-leaderboard (category &key (top 10) (withscores t))
  "Get top N entries from leaderboard CATEGORY (:xp, :level, :deaths).
   Returns list of (player-id score) pairs if WITHSCORES, else list of player-ids."
  (when *storage*
    (let ((key (leaderboard-key category))
          (result (storage-zrevrange *storage* key 0 (1- top) :withscores withscores)))
      (if withscores
          ;; Convert string IDs back to numbers
          (mapcar (lambda (entry)
                    (list (parse-integer (first entry) :junk-allowed t)
                          (second entry)))
                  result)
          (mapcar (lambda (id-str)
                    (parse-integer id-str :junk-allowed t))
                  result)))))

(defun db-get-player-rank (player-id category)
  "Get player's rank on CATEGORY leaderboard (0-based descending).
   Returns NIL if player not on leaderboard."
  (when *storage*
    (storage-zrevrank *storage* (leaderboard-key category) (prin1-to-string player-id))))

(defun db-get-player-leaderboard-score (player-id category)
  "Get player's score on CATEGORY leaderboard.
   Returns NIL if player not on leaderboard."
  (when *storage*
    (storage-zscore *storage* (leaderboard-key category) (prin1-to-string player-id))))

;;; Online Player Tracking

(defun db-add-online-player (player-id)
  "Add player to online players set."
  (when *storage*
    (storage-sadd *storage* (online-players-key) (prin1-to-string player-id))))

(defun db-remove-online-player (player-id)
  "Remove player from online players set."
  (when *storage*
    (storage-srem *storage* (online-players-key) (prin1-to-string player-id))))

(defun db-get-online-count ()
  "Get count of online players."
  (if *storage*
      (storage-scard *storage* (online-players-key))
      0))

(defun db-get-online-players ()
  "Get list of online player IDs."
  (when *storage*
    (mapcar (lambda (id-str)
              (parse-integer id-str :junk-allowed t))
            (storage-smembers *storage* (online-players-key)))))

;;;; Dirty Flag System (for Tier-2 Batched Writes)

(defstruct player-session
  "Tracks persistence state for a connected player."
  (player nil :type (or null player))
  (zone-id nil :type (or null symbol))
  (username nil :type (or null string))  ; For reverse lookup on ownership loss
  (dirty-p nil :type boolean)
  (last-flush 0.0 :type float)
  (tier1-pending nil :type list))

(defparameter *player-sessions* (make-hash-table :test 'eql)
  "Map of player-id -> player-session for connected players.")

#+sbcl
(defvar *player-sessions-lock* (sb-thread:make-mutex :name "player-sessions-lock")
  "Mutex protecting *player-sessions* for thread-safe access.")

(defmacro with-player-sessions-lock (&body body)
  "Execute BODY with *player-sessions-lock* held for thread-safe session operations."
  #+sbcl
  `(sb-thread:with-mutex (*player-sessions-lock*)
     ,@body)
  #-sbcl
  `(progn ,@body))

(defparameter *batch-flush-interval* 30.0
  "Seconds between batch flushes for tier-2 writes.
   TRADEOFF: Server crash loses up to 30s of routine state (XP, position, HP).
   Critical state (death, level-up, equipment, zone transitions) uses Tier-1
   immediate saves and is not affected. 30s chosen to balance durability vs
   database load. Reduce for more durability, increase for less DB pressure.")

(defun mark-player-dirty (player-id)
  "Mark player as needing a database flush. Thread-safe."
  (with-player-sessions-lock
    (let ((session (gethash player-id *player-sessions*)))
      (when session
        (setf (player-session-dirty-p session) t)
        (log-verbose "Marked player ~a as dirty" player-id)))))

;;; Session Ownership Functions (Phase 3 - Database Hardening)

(defun claim-session-ownership (player-id)
  "Attempt to claim ownership of a player session.
   Returns T if claimed successfully, NIL if session is owned elsewhere (double-login).
   On storage error, returns NIL (safe default - treats as claim failure)."
  (unless *storage*
    (return-from claim-session-ownership t))  ; No storage = always succeed
  ;; Ensure server instance ID exists
  (unless *server-instance-id*
    (setf *server-instance-id* (generate-server-instance-id)))
  ;; Catch storage-error to prevent crashing server on transient Redis failures
  (handler-case
      (let ((owner-key (session-owner-key player-id)))
        (let ((claimed (storage-setnx-with-ttl *storage* owner-key *server-instance-id*
                                               *session-ownership-ttl-seconds*)))
          (if claimed
              (progn
                (log-verbose "Claimed session ownership for player ~a" player-id)
                t)
              ;; Check if we already own it (reconnect case)
              (let ((current-owner (storage-load-raw *storage* owner-key)))
                (if (and current-owner (string= current-owner *server-instance-id*))
                    (progn
                      ;; We already own it, refresh TTL
                      (storage-refresh-ttl *storage* owner-key *session-ownership-ttl-seconds*)
                      (log-verbose "Refreshed existing session ownership for player ~a" player-id)
                      t)
                    (progn
                      (log-verbose "Session ownership claim REJECTED for player ~a (owned by ~a)"
                                   player-id current-owner)
                      nil))))))
    (storage-error (e)
      (warn "Session ownership claim failed for player ~a (storage error): ~a" player-id e)
      nil)))

(defun release-session-ownership (player-id)
  "Release session ownership for a player. Called on logout.
   On storage error, logs warning but continues (cleanup still proceeds locally)."
  (when *storage*
    ;; Catch storage-error to prevent crashing server on transient Redis failures
    (handler-case
        (let ((owner-key (session-owner-key player-id)))
          ;; Only delete if we own it
          (let ((current-owner (storage-load-raw *storage* owner-key)))
            (when (and current-owner *server-instance-id*
                       (string= current-owner *server-instance-id*))
              (storage-delete *storage* owner-key)
              (log-verbose "Released session ownership for player ~a" player-id))))
      (storage-error (e)
        (warn "Release session ownership failed for player ~a (storage error): ~a - local cleanup continues"
              player-id e)))))

(defun verify-session-ownership (player-id)
  "Verify we still own the session. Returns T if we own it, NIL otherwise.
   On storage error, returns NIL (safe default - treat as 'unknown, don't save')."
  (unless *storage*
    (return-from verify-session-ownership t))  ; No storage = always succeed
  (unless *server-instance-id*
    (return-from verify-session-ownership nil))  ; No server ID = don't own anything
  ;; Catch storage-error to keep server loop alive on transient Redis failures
  (handler-case
      (let* ((owner-key (session-owner-key player-id))
             (current-owner (storage-load-raw *storage* owner-key)))
        (and current-owner (string= current-owner *server-instance-id*)))
    (storage-error (e)
      (log-verbose "Ownership check failed for player ~a (transient): ~a" player-id e)
      nil)))

(defun refresh-session-ownership (player-id)
  "Refresh TTL on session ownership. Called periodically as heartbeat."
  (when *storage*
    (let ((owner-key (session-owner-key player-id)))
      (storage-refresh-ttl *storage* owner-key *session-ownership-ttl-seconds*))))

(defparameter *ownership-refresh-interval* 30.0
  "Seconds between session ownership refreshes (half of TTL).
   Refreshing at half the TTL ensures ownership never expires during normal operation.")

(defun refresh-all-session-ownerships ()
  "Refresh TTL on all active session ownerships. Called periodically.
   Returns list of player-ids that failed to refresh (ownership truly lost).

   Phase 2 improvement: On verification failure, attempts to re-claim ownership
   before reporting as lost. This handles transient Redis hiccups where the key
   expired but no other server took ownership.

   Catches storage-error to keep server loop alive on transient Redis failures."
  (let ((failed-ids nil))
    (with-player-sessions-lock
      (maphash (lambda (player-id session)
                 (declare (ignore session))
                 ;; Verify we still own before refreshing
                 ;; verify-session-ownership already catches storage-error
                 (if (verify-session-ownership player-id)
                     ;; Catch errors during refresh to avoid unwinding loop
                     (handler-case
                         (refresh-session-ownership player-id)
                       (error (e)
                         (log-verbose "Refresh failed for player ~a (transient): ~a" player-id e)
                         ;; Don't mark as failed - will retry next cycle
                         nil))
                     ;; Ownership verification failed - try to re-claim before giving up
                     ;; This handles the case where Redis key expired but no other
                     ;; server has claimed ownership yet (transient outage)
                     (if (claim-session-ownership player-id)
                         (progn
                           (log-verbose "Re-claimed ownership for player ~a after verification failure" player-id)
                           ;; Successfully re-claimed, refresh TTL
                           (handler-case
                               (refresh-session-ownership player-id)
                             (error (e)
                               (log-verbose "Refresh after re-claim failed for ~a: ~a" player-id e))))
                         ;; Re-claim failed - another server owns it now, truly lost
                         (progn
                           (log-verbose "Ownership truly lost for player ~a (another server owns)" player-id)
                           (push player-id failed-ids)))))
               *player-sessions*))
    failed-ids))

(defun register-player-session (player &key (zone-id nil) (username nil))
  "Register a player session when they login. Thread-safe.
   USERNAME is stored for reverse lookup on ownership loss cleanup.
   Returns T on success, NIL if session is owned elsewhere (double-login rejected)."
  (let ((player-id (player-id player)))
    ;; Attempt to claim ownership first
    (unless (claim-session-ownership player-id)
      (warn "Double-login rejected for player ~a - session owned elsewhere" player-id)
      (return-from register-player-session nil))
    ;; Ownership claimed, register local session
    (with-player-sessions-lock
      (setf (gethash player-id *player-sessions*)
            (make-player-session :player player
                                 :zone-id zone-id
                                 :username username
                                 :dirty-p nil
                                 :last-flush (float (get-internal-real-time) 1.0)
                                 :tier1-pending nil)))
    ;; Add to online players set (Phase 4)
    (db-add-online-player player-id)
    ;; Update leaderboards with current player stats (Phase 4)
    (db-update-leaderboard-xp player-id (player-lifetime-xp player))
    (let ((level (combat-level (player-stats player))))
      (db-update-leaderboard-level player-id level))
    (log-verbose "Registered session for player ~a in zone ~a" player-id zone-id)
    t))

(defun update-player-session-zone (player-id zone-id)
  "Update the zone-id for a player session (called on zone transitions). Thread-safe."
  (with-player-sessions-lock
    (let ((session (gethash player-id *player-sessions*)))
      (when session
        (setf (player-session-zone-id session) zone-id)
        (log-verbose "Updated session zone for player ~a to ~a" player-id zone-id)))))

(defun unregister-player-session (player-id)
  "Unregister a player session when they logout. Thread-safe.
   Releases session ownership before removing local session."
  ;; Remove from online players set (Phase 4)
  (db-remove-online-player player-id)
  ;; Release ownership first (before final save)
  (release-session-ownership player-id)
  ;; Remove local session
  (with-player-sessions-lock
    (remhash player-id *player-sessions*))
  (log-verbose "Unregistered session for player ~a" player-id))

(defun unregister-player-session-local (player-id)
  "Unregister a player session locally only (no online-set or ownership changes).
   Used when ownership is already lost to another server - we should not touch
   the global online set since the new owner controls that now.
   Thread-safe."
  (with-player-sessions-lock
    (remhash player-id *player-sessions*))
  (log-verbose "Unregistered local session for player ~a (ownership lost)" player-id))

(defun flush-dirty-players (&key force)
  "Flush all dirty players to storage (tier-2 batched writes). Thread-safe.
   Uses pipelined batch save for efficiency (up to 300x faster than sequential).
   If FORCE is T, flush all players regardless of dirty flag.
   Verifies session ownership before saving - players with lost ownership are
   NOT saved (to prevent stale server overwrites) and their sessions are cleaned up."
  (let ((to-flush nil)
        (key-data-pairs nil)
        (lost-player-ids nil)
        (current-time (get-internal-real-time)))
    ;; Collect players that need flushing and build save data (under lock)
    (with-player-sessions-lock
      (maphash
       (lambda (player-id session)
         (let ((player (player-session-player session))
               (dirty (player-session-dirty-p session))
               (last-flush (player-session-last-flush session)))
           (when (and player
                      (or force
                          dirty
                          (> (- current-time last-flush)
                             (* *batch-flush-interval* internal-time-units-per-second))))
             ;; Check ownership BEFORE adding to flush list
             (if (verify-session-ownership player-id)
                 (progn
                   (push (cons player-id session) to-flush)
                   ;; Build save data while under lock to get consistent snapshot
                   (let* ((zone-id (player-session-zone-id session))
                          (key (player-key player-id))
                          (data (serialize-player player :include-visuals nil :zone-id zone-id)))
                     (setf data (plist-put data :version *player-schema-version*))
                     (push (cons key data) key-data-pairs)))
                 ;; Lost ownership - track for cleanup (don't save - would overwrite)
                 (push player-id lost-player-ids)))))
       *player-sessions*))
    ;; Handle lost ownership - DO NOT cleanup here, just skip saves
    ;; Phase 2 fix: Let the ownership refresh loop (refresh-all-session-ownerships)
    ;; handle cleanup so it can attempt re-claim first and do full net-client de-auth.
    ;; If we cleanup here, the session is removed before the refresh loop sees it,
    ;; leaving stale authenticated clients in the server.
    (when lost-player-ids
      (warn "Batch flush: lost ownership for ~d players, skipping save (cleanup deferred to refresh loop): ~a"
            (length lost-player-ids) lost-player-ids))
    ;; Execute batch save outside lock (IO can be slow)
    (when to-flush
      (unless *storage*
        (log-verbose "flush-dirty-players: no storage backend initialized, skipping")
        (return-from flush-dirty-players 0))
      ;; Phase 1: Only clear dirty flags on successful save
      ;; If batch save fails, flags remain set for next flush cycle to retry
      (handler-case
          (let ((saved-count (storage-save-batch *storage* key-data-pairs)))
            ;; Success: Update session state for all flushed players (under lock)
            (with-player-sessions-lock
              (dolist (entry to-flush)
                (let ((session (cdr entry)))
                  (setf (player-session-dirty-p session) nil)
                  (setf (player-session-last-flush session) (float current-time 1.0)))))
            ;; Update stats counter
            (when (boundp '*server-total-saves*)
              (incf *server-total-saves* saved-count))
            (when (> saved-count 0)
              (log-verbose "Batch flushed ~a player(s) to storage (pipelined)" saved-count))
            saved-count)
        (storage-error (e)
          ;; Failure: Keep dirty flags set so next flush cycle retries
          (warn "Batch flush failed, dirty flags preserved for retry: ~a" e)
          0)))
    (length to-flush)))

;;;; Tier-1 Immediate Write Operations

(defun db-save-player-immediate (player)
  "Tier-1 immediate write: save player and wait for confirmation.
   Use for critical operations: trade, bank, death, level-up, item destruction.
   Verifies session ownership before saving to prevent stale server writes.
   Returns T on success. Signals STORAGE-ERROR on failure (Phase 1: enables retry)."
  (let ((player-id (player-id player)))
    ;; Verify we still own this session (prevents stale server writes)
    (unless (verify-session-ownership player-id)
      (warn "REJECTED: Tier-1 save for player ~a - session not owned by this server" player-id)
      (return-from db-save-player-immediate nil))
    ;; Ownership verified, proceed with save
    ;; db-save-player signals storage-error on failure (Phase 1)
    (db-save-player player)
    ;; Only update session state if save succeeded (we get here only on success)
    ;; For Redis with AOF, write is durable after return
    ;; Mark as not dirty since we just saved
    (let ((session (gethash player-id *player-sessions*)))
      (when session
        (setf (player-session-dirty-p session) nil)
        (setf (player-session-last-flush session) (float (get-internal-real-time) 1.0))))
    (log-verbose "Tier-1 immediate save for player ~a" player-id)
    t))

;;;; Login/Logout Operations

(defun db-login-player (player-id)
  "Load player from storage on login. Returns player or NIL."
  (let ((player (db-load-player player-id)))
    (when player
      (register-player-session player)
      (log-verbose "Player ~a logged in from storage" player-id))
    player))

(defun db-logout-player (player)
  "Save player to storage on logout (tier-3 write).
   Ensures session cleanup happens even if save fails (transient Redis errors)."
  (when player
    (let ((player-id (player-id player)))
      ;; Try to save, but don't let save failure block cleanup
      ;; Cleanup must happen to prevent stale sessions
      (handler-case
          (db-save-player-immediate player)
        (storage-error (e)
          (warn "Logout save failed for player ~a (will lose unsaved progress): ~a"
                player-id e))
        (error (e)
          (warn "Logout save error for player ~a: ~a" player-id e)))
      ;; Always unregister session, even if save failed
      (unregister-player-session player-id)
      (log-verbose "Player ~a logged out" player-id))))

;;;; Password Hashing (using ironclad PBKDF2-SHA256)

(defparameter *password-hash-iterations* 100000
  "Number of PBKDF2 iterations for password hashing.
   Higher = more secure but slower. 100k is industry standard as of 2024.")

(defparameter *password-salt-bytes* 16
  "Salt length in bytes. 16 bytes = 128 bits, sufficient for security.")

(defun generate-salt ()
  "Generate a cryptographically secure random salt."
  (ironclad:random-data *password-salt-bytes*))

(defun derive-password-key (password salt)
  "Derive a key from PASSWORD using PBKDF2-SHA256 with SALT.
   Returns the derived key as a byte vector."
  (let ((password-bytes (ironclad:ascii-string-to-byte-array password)))
    (ironclad:derive-key
     (ironclad:make-kdf 'ironclad:pbkdf2 :digest 'ironclad:sha256)
     password-bytes
     salt
     *password-hash-iterations*
     32)))  ; 32 bytes = 256 bits output

(defun bytes-to-hex (bytes)
  "Convert byte vector to lowercase hex string."
  (ironclad:byte-array-to-hex-string bytes))

(defun hex-to-bytes (hex-string)
  "Convert hex string to byte vector."
  (ironclad:hex-string-to-byte-array hex-string))

(defun hash-password (password)
  "Hash PASSWORD using PBKDF2-SHA256 with random salt.
   Returns a string in format 'salt$hash' for storage."
  (let* ((salt (generate-salt))
         (key (derive-password-key password salt))
         (salt-hex (bytes-to-hex salt))
         (key-hex (bytes-to-hex key)))
    (concatenate 'string salt-hex "$" key-hex)))

(defun verify-password (password stored-hash)
  "Verify PASSWORD against STORED-HASH (format: 'salt$hash').
   Returns T if password matches, NIL otherwise."
  (let* ((dollar-pos (position #\$ stored-hash)))
    (unless dollar-pos
      ;; Invalid hash format
      (return-from verify-password nil))
    (let* ((salt-hex (subseq stored-hash 0 dollar-pos))
           (stored-key-hex (subseq stored-hash (1+ dollar-pos)))
           (salt (hex-to-bytes salt-hex))
           (computed-key (derive-password-key password salt))
           (computed-key-hex (bytes-to-hex computed-key)))
      ;; Constant-time comparison to prevent timing attacks
      (ironclad:constant-time-equal
       (ironclad:ascii-string-to-byte-array computed-key-hex)
       (ironclad:ascii-string-to-byte-array stored-key-hex)))))

;;;; Network Encryption (X25519 + AES-256-GCM)
;;;;
;;;; Encrypts authentication messages (login/register) to prevent wifi sniffing.
;;;; Game traffic (snapshots, intents) remains unencrypted for latency reasons.
;;;;
;;;; Protocol:
;;;; 1. Server has static X25519 keypair (generated once, persisted)
;;;; 2. Client generates ephemeral X25519 keypair per auth request
;;;; 3. Client computes shared secret via ECDH (ephemeral-private + server-public)
;;;; 4. Client encrypts credentials with AES-256-GCM using SHA256(shared-secret)
;;;; 5. Client sends: ephemeral-public-key || nonce || ciphertext || tag
;;;; 6. Server computes same shared secret (server-private + ephemeral-public)
;;;; 7. Server decrypts credentials, verifies tag

(defparameter *server-keypair* nil
  "Server's X25519 keypair for auth encryption. NIL until initialized.")

(defparameter *server-public-key-hex* nil
  "Server's public key as hex string for embedding in client config.")

(defun generate-server-keypair ()
  "Generate a new X25519 keypair for the server.
   Returns (values private-key public-key)."
  (let ((keypair (ironclad:generate-key-pair :curve25519)))
    (values (ironclad:destructure-private-key keypair)
            (ironclad:destructure-public-key keypair))))

(defun init-server-encryption ()
  "Initialize server encryption keypair. Generates new keys if none exist."
  (multiple-value-bind (priv pub) (generate-server-keypair)
    (setf *server-keypair* (cons priv pub))
    (setf *server-public-key-hex* (bytes-to-hex pub))
    (log-verbose "Server encryption initialized. Public key: ~a..."
                 (subseq *server-public-key-hex* 0 16))
    *server-public-key-hex*))

(defun get-server-public-key ()
  "Get server's public key as hex string."
  (unless *server-keypair*
    (init-server-encryption))
  *server-public-key-hex*)

(defun compute-shared-secret (my-private-key their-public-key)
  "Compute X25519 shared secret from private and public keys (byte vectors)."
  (ironclad:diffie-hellman :curve25519 my-private-key their-public-key))

(defun derive-encryption-key (shared-secret)
  "Derive a 256-bit encryption key from shared secret using SHA-256.
   This adds key derivation to the raw ECDH output."
  (ironclad:digest-sequence :sha256 shared-secret))

(defun encrypt-auth-payload (payload server-public-key-hex)
  "Encrypt PAYLOAD (string) for server using its public key.
   Returns hex string: ephemeral-public || nonce || ciphertext || tag
   PAYLOAD should be a plist serialized as string."
  (let* (;; Generate ephemeral keypair
         (ephemeral-keypair (ironclad:generate-key-pair :curve25519))
         (ephemeral-private (ironclad:destructure-private-key ephemeral-keypair))
         (ephemeral-public (ironclad:destructure-public-key ephemeral-keypair))
         ;; Compute shared secret
         (server-public (hex-to-bytes server-public-key-hex))
         (shared-secret (compute-shared-secret ephemeral-private server-public))
         (encryption-key (derive-encryption-key shared-secret))
         ;; Generate random nonce (12 bytes for AES-GCM)
         (nonce (ironclad:random-data 12))
         ;; Encrypt with AES-256-GCM
         (plaintext (ironclad:ascii-string-to-byte-array payload))
         (cipher (ironclad:make-authenticated-encryption-mode
                  :gcm
                  :cipher-name :aes
                  :key encryption-key
                  :initialization-vector nonce))
         (ciphertext (make-array (length plaintext)
                                 :element-type '(unsigned-byte 8)))
         (tag (make-array 16 :element-type '(unsigned-byte 8))))
    ;; Perform encryption
    (ironclad:process-associated-data cipher #())
    (ironclad:encrypt cipher plaintext ciphertext)
    (ironclad:produce-tag cipher :tag tag)
    ;; Concatenate: ephemeral-public (32) || nonce (12) || ciphertext || tag (16)
    (let* ((result-len (+ 32 12 (length ciphertext) 16))
           (result (make-array result-len :element-type '(unsigned-byte 8))))
      (replace result ephemeral-public :start1 0)
      (replace result nonce :start1 32)
      (replace result ciphertext :start1 44)
      (replace result tag :start1 (+ 44 (length ciphertext)))
      (bytes-to-hex result))))

(defun decrypt-auth-payload (encrypted-hex)
  "Decrypt an auth payload encrypted by client.
   ENCRYPTED-HEX is: ephemeral-public || nonce || ciphertext || tag
   Returns decrypted payload string, or NIL if decryption fails."
  (unless *server-keypair*
    (warn "Server encryption not initialized")
    (return-from decrypt-auth-payload nil))
  (handler-case
      (let* ((encrypted (hex-to-bytes encrypted-hex))
             (len (length encrypted)))
        (when (< len (+ 32 12 16))  ; minimum: pubkey + nonce + tag
          (warn "Encrypted payload too short")
          (return-from decrypt-auth-payload nil))
        ;; Extract components
        (let* ((ephemeral-public (subseq encrypted 0 32))
               (nonce (subseq encrypted 32 44))
               (ciphertext-end (- len 16))
               (ciphertext (subseq encrypted 44 ciphertext-end))
               (tag (subseq encrypted ciphertext-end))
               ;; Compute shared secret using server's private key
               (server-private (car *server-keypair*))
               (shared-secret (compute-shared-secret server-private ephemeral-public))
               (encryption-key (derive-encryption-key shared-secret))
               ;; Decrypt with AES-GCM, providing tag for verification
               ;; If tag doesn't match, ironclad signals bad-authentication-tag
               (cipher (ironclad:make-authenticated-encryption-mode
                        :gcm
                        :cipher-name :aes
                        :key encryption-key
                        :initialization-vector nonce
                        :tag tag))
               (plaintext (make-array (length ciphertext)
                                      :element-type '(unsigned-byte 8))))
          ;; Decrypt (tag verified automatically via cipher creation)
          (ironclad:process-associated-data cipher #())
          (ironclad:decrypt cipher ciphertext plaintext)
          ;; Convert to string
          (let ((result (make-string (length plaintext))))
            (dotimes (i (length plaintext))
              (setf (aref result i) (code-char (aref plaintext i))))
            result)))
    (ironclad:bad-authentication-tag ()
      (warn "Auth payload tag verification failed - possible tampering")
      nil)
    (error (e)
      (warn "Failed to decrypt auth payload: ~a" e)
      nil)))

;;;; Account Management

(defparameter *account-schema-version* 2
  "Current schema version for account records.
   v1: plaintext passwords
   v2: PBKDF2-SHA256 hashed passwords (salt$hash format)")

(defun account-key (username)
  "Return storage key for account USERNAME."
  (format nil "account:~a" (string-downcase username)))

(defun db-save-account (username password-hash character-id)
  "Save account to storage. PASSWORD-HASH should be pre-hashed.
   Returns T on success."
  (when (and *storage* username password-hash)
    (let* ((key (account-key username))
           (data (list :version *account-schema-version*
                      :username (string-downcase username)
                      :password-hash password-hash
                      :character-id character-id)))
      (storage-save *storage* key data)
      (log-verbose "Saved account ~a to storage" username)
      t)))

(defun db-load-account (username)
  "Load account from storage. Returns plist or NIL if not found."
  (when (and *storage* username)
    (let ((key (account-key username)))
      (storage-load *storage* key))))

(defun db-delete-account (username)
  "Delete account from storage. Returns T if deleted."
  (when (and *storage* username)
    (let ((key (account-key username)))
      (when (storage-delete *storage* key)
        (log-verbose "Deleted account ~a from storage" username)
        t))))

(defun db-account-exists-p (username)
  "Return T if account USERNAME exists in storage."
  (when (and *storage* username)
    (let ((key (account-key username)))
      (storage-exists-p *storage* key))))

(defun db-create-account (username password)
  "Create new account with USERNAME and PASSWORD (hashes before storing).
   Returns T on success, NIL if username taken."
  (when (db-account-exists-p username)
    (log-verbose "Account creation failed: username ~a already exists" username)
    (return-from db-create-account nil))
  (let ((password-hash (hash-password password)))
    (db-save-account username password-hash nil))
  (log-verbose "Created new account: ~a" username)
  t)

(defun db-verify-credentials (username password)
  "Verify username/password. Returns T if credentials are valid, NIL otherwise."
  (let ((account (db-load-account username)))
    (when account
      (let ((stored-hash (getf account :password-hash)))
        (when stored-hash
          (verify-password password stored-hash))))))

(defun db-get-character-id (username)
  "Get character-id for account USERNAME. Returns character-id or NIL."
  (let ((account (db-load-account username)))
    (when account
      (getf account :character-id))))

(defun db-set-character-id (username character-id)
  "Set character-id for account USERNAME. Returns T on success."
  (let ((account (db-load-account username)))
    (when account
      (let ((password-hash (getf account :password-hash)))
        (when password-hash
          (db-save-account username password-hash character-id))))))

;;;; Graceful Shutdown

(defun db-shutdown-flush ()
  "Gracefully flush all data during server shutdown.
   1. Flush all dirty players
   2. Trigger Redis BGSAVE
   3. Close storage connection"
  (log-verbose "Starting graceful shutdown flush")
  (flush-dirty-players :force t)
  (when *storage*
    (storage-flush *storage*))
  (shutdown-storage)
  (log-verbose "Graceful shutdown flush completed"))

;;;; Durable vs Ephemeral State Documentation

;;; As specified in AGENTS.md and docs/db.md:
;;;
;;; DURABLE (must persist):
;;; - Progression: XP, levels, skill levels, combat stats
;;; - Health: Current HP (prevents logout-heal exploit)
;;; - Inventory: Items, equipment, stack counts
;;; - Currency: Gold, bank contents (when implemented)
;;; - Position: Zone ID, X/Y coordinates
;;; - Quests, Achievements, Social data, Settings (when implemented)
;;;
;;; EPHEMERAL (OK to lose):
;;; - Temporary buffs, debuffs, cooldowns
;;; - Current attack/follow target (reset on login)
;;; - AI state, pathfinding cache
;;; - Animation frame, visual effects (client-side)
;;; - Party invites, trade windows (session-only)
;;;
;;; The serialize-player function in save.lisp handles this classification.
;;; Visual fields are excluded from DB saves (:include-visuals nil).
