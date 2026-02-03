(in-package :mmorpg)

;;; db-storage.lisp - Storage abstraction layer, backends, and key schema
;;;
;;; Implements the storage protocol (generic functions), Redis and memory backends,
;;; metrics, key schema, and initialization. Split from db.lisp.
;;;
;;; Load order: db-storage -> db-players -> db-accounts -> db-admin -> db

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

(defgeneric storage-refresh-ttl-batch (storage keys ttl-seconds)
  (:documentation "Refresh TTL on all KEYS in one operation. Returns count of keys refreshed.
   Uses pipelining on Redis for efficiency."))

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
        (redis:with-recursive-connection (:host (redis-storage-host storage)
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
          (redis:with-recursive-connection (:host (redis-storage-host storage)
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
      (redis:with-recursive-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (> (red:del key) 0))
    (error (e)
      (warn "Redis delete error for key ~a: ~a" key e)
      nil)))

(defmethod storage-exists-p ((storage redis-storage) key)
  "Check if key exists in Redis."
  (handler-case
      (redis:with-recursive-connection (:host (redis-storage-host storage)
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
      (redis:with-recursive-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (red:keys pattern))
    (error (e)
      (warn "Redis keys error for pattern ~a: ~a" pattern e)
      nil)))

(defmethod storage-flush ((storage redis-storage))
  "Trigger Redis BGSAVE for immediate snapshot."
  (handler-case
      (redis:with-recursive-connection (:host (redis-storage-host storage)
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
        (redis:with-recursive-connection (:host (redis-storage-host storage)
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
  "SET if Not eXists with TTL. Returns T if set, NIL if key exists.
   Uses SETNX + EXPIRE (not atomic but sufficient for session ownership)."
  (handler-case
      (redis:with-recursive-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        ;; SETNX returns T or 1 if set, NIL or 0 if key exists (cl-redis varies)
        (let ((result (red:setnx key value)))
          (when (or (eq result t) (eql result 1))
            ;; Key was set, now add TTL
            (red:expire key ttl-seconds)
            t)))
    (error (e)
      (warn "Redis SETNX error for key ~a: ~a" key e)
      nil)))

(defmethod storage-refresh-ttl ((storage redis-storage) key ttl-seconds)
  "Refresh TTL on KEY. Returns T if key exists, NIL otherwise."
  (handler-case
      (redis:with-recursive-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        ;; EXPIRE returns 1 if key exists and TTL was set, 0 otherwise
        (let ((result (red:expire key ttl-seconds)))
          (and (numberp result) (= result 1))))
    (error (e)
      (warn "Redis EXPIRE error for key ~a: ~a" key e)
      nil)))

(defmethod storage-refresh-ttl-batch ((storage redis-storage) keys ttl-seconds)
  "Refresh TTL on all KEYS in a single pipelined round trip.
   Returns count of keys that were successfully refreshed."
  (when (null keys)
    (return-from storage-refresh-ttl-batch 0))
  (handler-case
      (redis:with-recursive-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (let ((results nil))
          ;; Pipeline all EXPIRE commands
          (redis:with-pipelining
            (dolist (key keys)
              (push (red:expire key ttl-seconds) results)))
          ;; Count successful refreshes (EXPIRE returns 1 if key existed)
          (count-if (lambda (r) (and (numberp r) (= r 1))) results)))
    (error (e)
      (warn "Redis batch EXPIRE error: ~a" e)
      0)))

(defmethod storage-load-raw ((storage redis-storage) key)
  "Load raw string value for KEY (does not parse).
   Returns NIL if key not found. Signals STORAGE-ERROR on failure
   (Phase 1: enables retry logic and distinguishes error from not-found)."
  (handler-case
      (redis:with-recursive-connection (:host (redis-storage-host storage)
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
      (redis:with-recursive-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (red:incr key))
    (error (e)
      (warn "Redis INCR error for key ~a: ~a" key e)
      nil)))

(defmethod storage-save-with-ttl ((storage redis-storage) key data ttl-seconds)
  "Save DATA at KEY with TTL expiration."
  (handler-case
      (redis:with-recursive-connection (:host (redis-storage-host storage)
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
      (redis:with-recursive-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (red:zadd key score member)
        t)
    (error (e)
      (warn "Redis ZADD error for key ~a: ~a" key e)
      nil)))

(defmethod storage-zincrby ((storage redis-storage) key increment member)
  "Increment member's score in sorted set."
  (handler-case
      (redis:with-recursive-connection (:host (redis-storage-host storage)
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
      (redis:with-recursive-connection (:host (redis-storage-host storage)
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
      (redis:with-recursive-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (red:zrank key member))
    (error (e)
      (warn "Redis ZRANK error for key ~a: ~a" key e)
      nil)))

(defmethod storage-zrevrank ((storage redis-storage) key member)
  "Get rank of member (descending order, 0-based)."
  (handler-case
      (redis:with-recursive-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (red:zrevrank key member))
    (error (e)
      (warn "Redis ZREVRANK error for key ~a: ~a" key e)
      nil)))

(defmethod storage-zscore ((storage redis-storage) key member)
  "Get score of member in sorted set."
  (handler-case
      (redis:with-recursive-connection (:host (redis-storage-host storage)
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
      (redis:with-recursive-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (let ((result (red:sadd key member)))
          (and (numberp result) (> result 0))))
    (error (e)
      (warn "Redis SADD error for key ~a: ~a" key e)
      nil)))

(defmethod storage-srem ((storage redis-storage) key member)
  "Remove member from set."
  (handler-case
      (redis:with-recursive-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (let ((result (red:srem key member)))
          (and (numberp result) (> result 0))))
    (error (e)
      (warn "Redis SREM error for key ~a: ~a" key e)
      nil)))

(defmethod storage-scard ((storage redis-storage) key)
  "Get count of members in set."
  (handler-case
      (redis:with-recursive-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (let ((result (red:scard key)))
          (if (numberp result) result 0)))
    (error (e)
      (warn "Redis SCARD error for key ~a: ~a" key e)
      0)))

(defmethod storage-smembers ((storage redis-storage) key)
  "Get all members of set."
  (handler-case
      (redis:with-recursive-connection (:host (redis-storage-host storage)
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
      (redis:with-recursive-connection (:host (redis-storage-host storage)
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
        (redis:with-recursive-connection (:host (redis-storage-host storage)
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
                          (redis:with-recursive-connection (:host (redis-storage-host storage)
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

(defmethod storage-refresh-ttl-batch ((storage memory-storage) keys ttl-seconds)
  "Refresh TTL on all KEYS. Returns count of keys successfully refreshed."
  (let ((count 0)
        (now (get-universal-time))
        (new-expiration (+ (get-universal-time) ttl-seconds)))
    (dolist (key keys)
      (let ((current (gethash key (memory-storage-data storage)))
            (expiration (gethash key *memory-storage-ttls*)))
        (when (and current expiration (> expiration now))
          (setf (gethash key *memory-storage-ttls*) new-expiration)
          (incf count))))
    count))

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
