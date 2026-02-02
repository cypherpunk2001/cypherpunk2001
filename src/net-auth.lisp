;; net-auth.lisp — login/register handling, auth queues, auth message processing
;; Split from net.lisp. Load order: net-protocol → net-auth → net-snapshot → net-client → net-server → net
(in-package #:mmorpg)

;;; Session management

(defparameter *active-sessions* (make-hash-table :test 'equal :size 512)
  "Map of username (lowercase) -> net-client for logged-in accounts.")

#+sbcl
(defvar *session-lock* (sb-thread:make-mutex :name "session-lock")
  "Mutex protecting *active-sessions* for atomic check-and-set operations.")

(defmacro with-session-lock (&body body)
  "Execute BODY with *session-lock* held for atomic session operations."
  #+sbcl
  `(sb-thread:with-mutex (*session-lock*)
     ,@body)
  #-sbcl
  `(progn ,@body))

(defun session-clear-all ()
  "Clear all active sessions. Called during server shutdown/startup for clean state."
  (with-session-lock
    (clrhash *active-sessions*)))

(defun session-try-register (username client)
  "Atomically check if USERNAME is available and register CLIENT.
   Returns T if registered, NIL if already logged in."
  (with-session-lock
    (let ((key (string-downcase username)))
      (if (gethash key *active-sessions*)
          nil
          (progn
            (setf (gethash key *active-sessions*) client)
            t)))))

(defun session-unregister (username)
  "Atomically remove USERNAME from active sessions."
  (with-session-lock
    (remhash (string-downcase username) *active-sessions*)))

(defun session-get (username)
  "Get the client for USERNAME if logged in."
  (with-session-lock
    (gethash (string-downcase username) *active-sessions*)))

(defparameter *client-timeout-seconds* 30.0
  "Seconds of inactivity before a client is considered disconnected.")

;;;; Auth Rate Limiting

(defparameter *auth-max-attempts* 5
  "Maximum failed auth attempts before lockout.")

(defparameter *auth-lockout-seconds* 300.0
  "Lockout duration in seconds after max failed attempts (5 minutes).")

(defparameter *auth-attempt-window* 60.0
  "Time window in seconds to count failed attempts.")

(defstruct auth-rate-entry
  "Tracks auth attempts for rate limiting."
  (attempts 0 :type fixnum)
  (first-attempt-time 0.0 :type single-float)
  (lockout-until 0.0 :type single-float))

(defparameter *auth-rate-limits* (make-hash-table :test 'equal :size 256)
  "Map of IP address -> auth-rate-entry for rate limiting.")

;;; Auth Metrics (Step 6) - Thread-safe atomic counters via struct slots
;;; sb-ext:atomic-incf requires CAS-able places: struct slots with :type sb-ext:word.
(defstruct (auth-metrics (:constructor make-auth-metrics))
  "Atomic counters for auth throughput monitoring."
  (queued 0 :type sb-ext:word)
  (processed 0 :type sb-ext:word)
  (expired 0 :type sb-ext:word)
  (rejected-busy 0 :type sb-ext:word)
  (success 0 :type sb-ext:word)
  (fail 0 :type sb-ext:word))

(defvar *auth-metrics* (make-auth-metrics)
  "Global auth metrics counters (atomic on SBCL).")

;;; Convenience accessors matching old *auth-metric-* names
(defmacro auth-metric-incf (slot)
  "Atomically increment an auth metric counter (SBCL), or plain incf otherwise."
  #+sbcl `(sb-ext:atomic-incf (,(intern (format nil "AUTH-METRICS-~a" slot)) *auth-metrics*))
  #-sbcl `(incf (,(intern (format nil "AUTH-METRICS-~a" slot)) *auth-metrics*)))

;;; Thread-safe auth rate limit access
#+sbcl
(defvar *auth-rate-limits-lock* (sb-thread:make-mutex :name "auth-rate-limits-lock")
  "Mutex protecting *auth-rate-limits* for thread-safe access.")

(defmacro with-auth-rate-limits-lock (&body body)
  "Execute BODY with *auth-rate-limits-lock* held for thread-safe rate limit operations."
  #+sbcl
  `(sb-thread:with-mutex (*auth-rate-limits-lock*)
     ,@body)
  #-sbcl
  `(progn ,@body))

(defun auth-rate-check (host current-time)
  "Check if HOST is rate-limited. Returns T if allowed, NIL if blocked.
   Thread-safe: protects rate limit table from concurrent access."
  (with-auth-rate-limits-lock
    (let ((entry (gethash host *auth-rate-limits*)))
      (cond
        ;; No entry - allowed
        ((null entry) t)
        ;; Currently locked out
        ((> (auth-rate-entry-lockout-until entry) current-time)
         nil)
        ;; Lockout expired - reset and allow
        ((> (auth-rate-entry-lockout-until entry) 0.0)
         (setf (auth-rate-entry-attempts entry) 0
               (auth-rate-entry-lockout-until entry) 0.0
               (auth-rate-entry-first-attempt-time entry) 0.0)
         t)
        ;; Window expired - reset attempts and allow
        ((> (- current-time (auth-rate-entry-first-attempt-time entry))
            *auth-attempt-window*)
         (setf (auth-rate-entry-attempts entry) 0
               (auth-rate-entry-first-attempt-time entry) 0.0)
         t)
        ;; Under limit - allowed
        (t t)))))

(defun auth-rate-record-failure (host current-time)
  "Record a failed auth attempt for HOST. Returns T if now locked out.
   Thread-safe: protects rate limit table from concurrent access."
  (with-auth-rate-limits-lock
    (let ((entry (gethash host *auth-rate-limits*)))
      (unless entry
        (setf entry (make-auth-rate-entry)
              (gethash host *auth-rate-limits*) entry))
      ;; Reset if window expired
      (when (and (> (auth-rate-entry-first-attempt-time entry) 0.0)
                 (> (- current-time (auth-rate-entry-first-attempt-time entry))
                    *auth-attempt-window*))
        (setf (auth-rate-entry-attempts entry) 0
              (auth-rate-entry-first-attempt-time entry) 0.0))
      ;; Record attempt
      (when (zerop (auth-rate-entry-first-attempt-time entry))
        (setf (auth-rate-entry-first-attempt-time entry) current-time))
      (incf (auth-rate-entry-attempts entry))
      ;; Check if lockout triggered
      (when (>= (auth-rate-entry-attempts entry) *auth-max-attempts*)
        (setf (auth-rate-entry-lockout-until entry)
              (+ current-time *auth-lockout-seconds*))
        (warn "Rate limit: ~a locked out for ~ds after ~d failed attempts"
              host (round *auth-lockout-seconds*) (auth-rate-entry-attempts entry))
        t))))

(defun auth-rate-record-success (host)
  "Clear rate limit tracking for HOST after successful auth.
   Thread-safe: protects rate limit table from concurrent access."
  (with-auth-rate-limits-lock
    (remhash host *auth-rate-limits*)))

(defun auth-rate-clear-all ()
  "Clear all rate limit state. For testing only.
   Thread-safe: protects rate limit table from concurrent access."
  (with-auth-rate-limits-lock
    (clrhash *auth-rate-limits*)))

;;;; Auth Replay Protection

(defparameter *auth-timestamp-window* 60
  "Maximum age in seconds for auth timestamps. Older messages rejected.")

(defparameter *auth-nonce-cache* (make-hash-table :test 'equal :size 1024)
  "Cache of recently seen nonces to prevent replay attacks.")

(defparameter *auth-nonce-cleanup-interval* 120
  "Seconds between nonce cache cleanup runs.")

(defvar *auth-nonce-last-cleanup* 0
  "Last time nonce cache was cleaned up.")

;;; Thread-safe nonce cache access
#+sbcl
(defvar *auth-nonce-lock* (sb-thread:make-mutex :name "auth-nonce-lock")
  "Mutex protecting *auth-nonce-cache* for thread-safe access.")

(defmacro with-auth-nonce-lock (&body body)
  "Execute BODY with *auth-nonce-lock* held for thread-safe nonce operations."
  #+sbcl
  `(sb-thread:with-mutex (*auth-nonce-lock*)
     ,@body)
  #-sbcl
  `(progn ,@body))

(defun auth-nonce-cleanup (current-time)
  "Remove expired nonces from cache.
   Internal: must be called with *auth-nonce-lock* held."
  (when (> (- current-time *auth-nonce-last-cleanup*) *auth-nonce-cleanup-interval*)
    (let ((cutoff (- current-time *auth-timestamp-window* 10)))
      (maphash (lambda (nonce timestamp)
                 (when (< timestamp cutoff)
                   (remhash nonce *auth-nonce-cache*)))
               *auth-nonce-cache*))
    (setf *auth-nonce-last-cleanup* current-time)))

(defun auth-check-replay (encrypted-payload timestamp)
  "Check if this auth request is a replay. Returns T if valid, NIL if replay.
   ENCRYPTED-PAYLOAD is used as the nonce (unique per request).
   TIMESTAMP is the claimed time of the request.
   Thread-safe: protects nonce cache from concurrent access."
  (with-auth-nonce-lock
    (let* ((current-time (get-universal-time))
           (age (- current-time timestamp)))
      ;; Cleanup old nonces periodically
      (auth-nonce-cleanup current-time)
      (cond
        ;; Timestamp too old
        ((> age *auth-timestamp-window*)
         (warn "Auth replay check: timestamp too old (~d seconds)" age)
         nil)
        ;; Timestamp in future (clock skew tolerance of 5 seconds)
        ((< age -5)
         (warn "Auth replay check: timestamp in future (~d seconds)" (- age))
         nil)
        ;; Already seen this nonce
        ((gethash encrypted-payload *auth-nonce-cache*)
         (warn "Auth replay check: duplicate nonce detected")
         nil)
        ;; Valid - record nonce
        (t
         (setf (gethash encrypted-payload *auth-nonce-cache*) current-time)
         t)))))

;;;; Auth Worker Thread (Non-Blocking Login)
;;;;
;;;; Auth requests (register/login) are queued and processed on a worker thread
;;;; to avoid blocking the main game loop with DB operations and retries.

(defstruct auth-queue
  "Thread-safe queue for auth work items.
   Uses two-list FIFO: push-list for producers, pop-list for consumers.
   O(1) amortized push/pop instead of O(n) last/butlast."
  (push-list nil :type list)   ; Producers push here
  (pop-list nil :type list)    ; Consumers pop from here
  (count 0 :type fixnum)       ; Current depth for backpressure
  (max-depth 200 :type fixnum) ; Max items before rejecting (0 = unlimited)
  (lock nil)
  (condvar nil))

(defstruct auth-request
  "Request to process on auth worker thread."
  (type nil :type (member :register :login nil))
  (host nil :type (or null string))
  (port 0 :type integer)
  (username nil :type (or null string))
  (password nil :type (or null string))
  (client nil)
  (timestamp 0.0 :type single-float)
  (stop-signal nil :type boolean))  ; Sentinel to stop worker

(defstruct auth-result
  "Result from auth worker to integrate on main thread."
  (type nil :type (member :register :login nil))
  (success nil :type boolean)
  (host nil :type (or null string))
  (port 0 :type integer)
  (username nil :type (or null string))
  (client nil)
  (player nil)
  (player-id nil :type (or null integer))
  (error-reason nil)  ; :bad-credentials, :username-taken, etc.
  (zone-id nil))

(defun make-auth-queue-instance (&key (max-depth *auth-queue-max-depth*))
  "Create a new thread-safe auth queue with bounded depth."
  #+sbcl
  (make-auth-queue :push-list nil
                   :pop-list nil
                   :count 0
                   :max-depth max-depth
                   :lock (sb-thread:make-mutex :name "auth-queue-lock")
                   :condvar (sb-thread:make-waitqueue :name "auth-queue-condvar"))
  #-sbcl
  (make-auth-queue :push-list nil :pop-list nil :count 0 :max-depth max-depth
                   :lock nil :condvar nil))

(defun auth-queue-push (queue item)
  "Thread-safe push ITEM to QUEUE (unbounded). Signals waiting threads."
  #+sbcl
  (sb-thread:with-mutex ((auth-queue-lock queue))
    (push item (auth-queue-push-list queue))
    (incf (auth-queue-count queue))
    (sb-thread:condition-notify (auth-queue-condvar queue)))
  #-sbcl
  (progn
    (push item (auth-queue-push-list queue))
    (incf (auth-queue-count queue))))

(defun auth-queue-try-push (queue item)
  "Thread-safe push ITEM if queue is below max depth. Returns T on success, NIL if full."
  #+sbcl
  (sb-thread:with-mutex ((auth-queue-lock queue))
    (when (or (zerop (auth-queue-max-depth queue))
              (< (auth-queue-count queue) (auth-queue-max-depth queue)))
      (push item (auth-queue-push-list queue))
      (incf (auth-queue-count queue))
      (sb-thread:condition-notify (auth-queue-condvar queue))
      t))
  #-sbcl
  (when (or (zerop (auth-queue-max-depth queue))
            (< (auth-queue-count queue) (auth-queue-max-depth queue)))
    (push item (auth-queue-push-list queue))
    (incf (auth-queue-count queue))
    t))

(defun auth-queue-pop-blocking (queue)
  "Block until an item is available, then pop and return it (FIFO).
   O(1) amortized via two-list technique. Used by worker thread."
  #+sbcl
  (sb-thread:with-mutex ((auth-queue-lock queue))
    (loop :while (and (null (auth-queue-pop-list queue))
                      (null (auth-queue-push-list queue)))
          :do (sb-thread:condition-wait (auth-queue-condvar queue)
                                         (auth-queue-lock queue)))
    ;; Transfer push-list to pop-list if pop-list is empty
    (when (null (auth-queue-pop-list queue))
      (setf (auth-queue-pop-list queue) (nreverse (auth-queue-push-list queue)))
      (setf (auth-queue-push-list queue) nil))
    (let ((item (pop (auth-queue-pop-list queue))))
      (decf (auth-queue-count queue))
      item))
  #-sbcl
  (progn
    (loop :while (and (null (auth-queue-pop-list queue))
                      (null (auth-queue-push-list queue)))
          :do (sleep 0.01))
    (when (null (auth-queue-pop-list queue))
      (setf (auth-queue-pop-list queue) (nreverse (auth-queue-push-list queue)))
      (setf (auth-queue-push-list queue) nil))
    (let ((item (pop (auth-queue-pop-list queue))))
      (decf (auth-queue-count queue))
      item)))

(defun auth-queue-drain-nonblocking (queue)
  "Return all items from QUEUE without blocking. For main thread.
   Returns items in FIFO order (oldest first)."
  #+sbcl
  (sb-thread:with-mutex ((auth-queue-lock queue))
    (let* ((popped (auth-queue-pop-list queue))
           (pushed (nreverse (auth-queue-push-list queue)))
           (items (nconc popped pushed)))
      (setf (auth-queue-push-list queue) nil)
      (setf (auth-queue-pop-list queue) nil)
      (setf (auth-queue-count queue) 0)
      items))
  #-sbcl
  (let* ((popped (auth-queue-pop-list queue))
         (pushed (nreverse (auth-queue-push-list queue)))
         (items (nconc popped pushed)))
    (setf (auth-queue-push-list queue) nil)
    (setf (auth-queue-pop-list queue) nil)
    (setf (auth-queue-count queue) 0)
    items))

(defun auth-queue-depth (queue)
  "Return current queue depth. Thread-safe."
  #+sbcl
  (sb-thread:with-mutex ((auth-queue-lock queue))
    (auth-queue-count queue))
  #-sbcl
  (auth-queue-count queue))

(defun process-register-async (request world id-source)
  "Process registration on worker thread. Returns auth-result.
   Step 10: Reordered flow - spawn player first, then pipelined account creation.
   Reduces 4-5 Redis connections to 1 per registration."
  (let* ((username (auth-request-username request))
         (password (auth-request-password request))
         (host (auth-request-host request))
         (port (auth-request-port request))
         (client (auth-request-client request))
         (timestamp (auth-request-timestamp request)))
    (declare (ignorable timestamp))
    ;; Step 9: Rate limit check removed from worker - main thread is authoritative gate.
    ;; Step 10: Spawn player FIRST to get character-id for pipelined account creation
    (handler-case
        (let* ((player (handler-case
                           (spawn-player-at-world world id-source)
                         (error (e)
                           (warn "Registration failed: ID allocation error for ~a: ~a" username e)
                           (return-from process-register-async
                             (make-auth-result :type :register
                                               :success nil
                                               :host host
                                               :port port
                                               :username username
                                               :client client
                                               :error-reason :internal-error))))))
          (unless player
            (return-from process-register-async
              (make-auth-result :type :register
                                :success nil
                                :host host
                                :port port
                                :username username
                                :client client
                                :error-reason :internal-error)))
          (let* ((player-id (player-id player))
                 (zone (world-zone world))
                 (zone-id (and zone (zone-id zone))))
            ;; Step 10: Pipelined account creation - EXISTS + hash + SET + RENAME in 1 connection
            ;; db-create-account-pipelined returns:
            ;;   T              = success
            ;;   :USERNAME-TAKEN = username already exists (not an error)
            ;;   signals        = infra error (with-retry-exponential retries, returns NIL if exhausted)
            (let ((result (with-retry-exponential
                              (r (lambda ()
                                   (db-create-account-pipelined username password player-id))
                               :max-retries 3
                               :initial-delay 50
                               :max-delay 200)
                            r)))
              (cond
                ((eq result t)
                 ;; Account created with character-id already linked
                 (session-try-register username client)
                 ;; Step 5: Register DB session (ownership, online set, leaderboards) in worker
                 (register-player-session-db player-id player)
                 (make-auth-result :type :register
                                   :success t
                                   :host host
                                   :port port
                                   :username username
                                   :client client
                                   :player player
                                   :player-id player-id
                                   :zone-id zone-id))
                ((eq result :username-taken)
                 ;; Username taken (player ID wasted but IDs are cheap)
                 (log-verbose "Registration failed from ~a:~d - username ~a already taken"
                              host port username)
                 (make-auth-result :type :register
                                   :success nil
                                   :host host
                                   :port port
                                   :username username
                                   :client client
                                   :error-reason :username-taken))
                (t
                 ;; NIL = retries exhausted (infra failure)
                 (warn "Registration failed from ~a:~d - infrastructure error for ~a"
                       host port username)
                 (make-auth-result :type :register
                                   :success nil
                                   :host host
                                   :port port
                                   :username username
                                   :client client
                                   :error-reason :internal-error))))))
      (error (e)
        (warn "Registration error for ~a: ~a" username e)
        (auth-rate-record-failure host timestamp)
        (make-auth-result :type :register
                          :success nil
                          :host host
                          :port port
                          :username username
                          :client client
                          :error-reason :internal-error)))))

(defun process-login-async (request world id-source)
  "Process login on worker thread. Returns auth-result.
   Step 10: Uses db-verify-and-load-account to load account once instead of twice."
  (let* ((username (auth-request-username request))
         (password (auth-request-password request))
         (host (auth-request-host request))
         (port (auth-request-port request))
         (client (auth-request-client request))
         (timestamp (auth-request-timestamp request))
         (zone (world-zone world))
         (zone-id (and zone (zone-id zone))))
    (declare (ignorable timestamp zone-id))
    ;; Step 9: Rate limit check removed from worker - main thread is authoritative gate.
    (handler-case
        ;; Step 10: Verify credentials AND get character-id in one account load
        (multiple-value-bind (character-id verified-p)
            (with-retry-exponential
                (result (lambda ()
                          (multiple-value-list (db-verify-and-load-account username password)))
                 :max-retries 3
                 :initial-delay 50
                 :max-delay 200)
              (values (first result) (second result)))
          (cond
            ;; Bad credentials or account not found
            ((not verified-p)
             (auth-rate-record-failure host timestamp)
             (log-verbose "Login failed from ~a:~d - bad credentials for ~a" host port username)
             (make-auth-result :type :login
                               :success nil
                               :host host
                               :port port
                               :username username
                               :client client
                               :error-reason :bad-credentials))
            ;; Check session availability (already thread-safe)
            ((not (session-try-register username client))
             (log-verbose "Login rejected: session-try-register failed for ~a" username)
             (make-auth-result :type :login
                               :success nil
                               :host host
                               :port port
                               :username username
                               :client client
                               :error-reason :already-logged-in))
            ;; Credentials valid, session available - load or create character
            (t
             (let ((player nil))
             (cond
               ;; Existing character - load from DB with Phase 6 validation
               (character-id
                ;; Claim session ownership FIRST (required by db-load-player-validated)
                (unless (claim-session-ownership character-id)
                  (log-verbose "Login rejected: claim-session-ownership failed for player ~d (Redis ownership conflict)"
                               character-id)
                  (session-unregister username)
                  (return-from process-login-async
                    (make-auth-result :type :login
                                      :success nil
                                      :host host
                                      :port port
                                      :username username
                                      :client client
                                      :error-reason :already-logged-in)))
                ;; Load with 4-outcome validation
                (multiple-value-bind (loaded-player loaded-zone-id action)
                    (with-retry-exponential
                        (result (lambda ()
                                  (multiple-value-list (db-load-player-validated character-id)))
                         :max-retries 3
                         :initial-delay 100
                         :max-delay 300)
                      (values (first result) (second result) (third result)))
                  (case action
                    ((:ok :clamp)
                     ;; Normal load or corrected data - proceed
                     (when (eq action :clamp)
                       (log-verbose "Player ~a loaded with corrections" character-id))
                     (setf player loaded-player)
                     (log-verbose "Loaded existing character ~d for account ~a" character-id username))
                    (:quarantine
                     ;; Account needs admin repair - reject login
                     (release-session-ownership character-id)
                     (session-unregister username)
                     (return-from process-login-async
                       (make-auth-result :type :login
                                         :success nil
                                         :host host
                                         :port port
                                         :username username
                                         :client client
                                         :error-reason :account-quarantined)))
                    (:reject
                     ;; Dangerous data - reject login
                     (release-session-ownership character-id)
                     (session-unregister username)
                     (return-from process-login-async
                       (make-auth-result :type :login
                                         :success nil
                                         :host host
                                         :port port
                                         :username username
                                         :client client
                                         :error-reason :data-corrupted)))
                    ((:not-found nil)
                     ;; No character data - should not happen for existing account
                     (release-session-ownership character-id)
                     (session-unregister username)
                     (return-from process-login-async
                       (make-auth-result :type :login
                                         :success nil
                                         :host host
                                         :port port
                                         :username username
                                         :client client
                                         :error-reason :load-failed))))))
               ;; No character yet - spawn new one
               (t
                (handler-case
                    (setf player (spawn-player-at-world world id-source))
                  (error (e)
                    ;; ID allocation failed - return login failure
                    (warn "Login failed: ID allocation error for ~a: ~a" username e)
                    (session-unregister username)
                    (return-from process-login-async
                      (make-auth-result :type :login
                                        :success nil
                                        :host host
                                        :port port
                                        :username username
                                        :client client
                                        :error-reason :internal-error))))
                (unless player
                  (session-unregister username)
                  (return-from process-login-async
                    (make-auth-result :type :login
                                      :success nil
                                      :host host
                                      :port port
                                      :username username
                                      :client client
                                      :error-reason :internal-error)))
                (let ((linked (with-retry-exponential
                                  (set-result (lambda () (db-set-character-id username (player-id player)))
                                   :max-retries 3
                                   :initial-delay 50
                                   :max-delay 200
                                   :on-final-fail (lambda (e)
                                                    (warn "Failed to set character-id for ~a: ~a" username e)))
                                set-result)))
                  (unless linked
                    (warn "Login rollback: unable to link character for ~a" username)
                    (session-unregister username)
                    (return-from process-login-async
                      (make-auth-result :type :login
                                        :success nil
                                        :host host
                                        :port port
                                        :username username
                                        :client client
                                        :error-reason :internal-error)))
                  (log-verbose "Created new character ~d for account ~a" (player-id player) username))))
             ;; Phase 2.5: Clamp nil/unknown zone-id to starting zone
             ;; Use player's zone-id (not global world zone) and validate it exists
             (let ((player-zone (player-zone-id player)))
               (when (null player-zone)
                 (warn "Player ~d has nil zone-id, clamping to ~a"
                       (player-id player) *starting-zone-id*)
                 (setf (player-zone-id player) *starting-zone-id*)
                 (setf player-zone *starting-zone-id*)
                 (mark-player-dirty (player-id player)))  ; Persist the fix
               ;; Step 5: Register DB session (online set, leaderboards) in worker
               ;; Ownership already claimed above for existing characters;
               ;; for new characters, claim it now
               (unless character-id
                 (claim-session-ownership (player-id player)))
               (register-player-session-db (player-id player) player)
               ;; Return success with player's zone-id (now guaranteed non-nil)
               (make-auth-result :type :login
                                 :success t
                                 :host host
                                 :port port
                                 :username username
                                 :client client
                                 :player player
                                 :player-id (player-id player)
                                 :zone-id player-zone))))))
      (error (e)
        (warn "Login error for ~a: ~a" username e)
        ;; Clean up session registration on error
        (session-unregister username)
        (auth-rate-record-failure host timestamp)
        (make-auth-result :type :login
                          :success nil
                          :host host
                          :port port
                          :username username
                          :client client
                          :error-reason :internal-error)))))

(defun auth-worker-loop (request-queue result-queue game)
  "Main loop for auth worker thread.
   Blocks on request-queue, processes DB operations, pushes to result-queue.
   Includes stale request expiry: requests older than *auth-request-max-age* are
   skipped to avoid wasting worker time on clients that have already timed out.
   Step 10c: Opens a persistent Redis connection per worker (if Redis backend)."
  (let ((world (game-world game))
        (id-source (game-id-source game)))
    (flet ((worker-main-loop ()
    (loop
      (let ((request (auth-queue-pop-blocking request-queue)))
        ;; Check for stop signal
        (when (auth-request-stop-signal request)
          (log-verbose "Auth worker received stop signal, exiting")
          (return))
        ;; Step 4: Check if request is stale (client likely timed out)
        (let* ((now-rt (/ (get-internal-real-time)
                          (float internal-time-units-per-second 1.0d0)))
               (age (- now-rt (auth-request-timestamp request))))
          (if (> age (float *auth-request-max-age* 1.0d0))
              ;; Stale - skip processing, return expired result
              (progn
                (auth-metric-incf expired)
                (auth-queue-push result-queue
                  (make-auth-result :type (auth-request-type request)
                                    :success nil
                                    :host (auth-request-host request)
                                    :port (auth-request-port request)
                                    :username (auth-request-username request)
                                    :client (auth-request-client request)
                                    :error-reason :request-expired)))
              ;; Fresh - process normally
              (let ((result (handler-case
                                (case (auth-request-type request)
                                  (:register (process-register-async request world id-source))
                                  (:login (process-login-async request world id-source))
                                  (otherwise
                                   (warn "Auth worker: unknown request type ~a" (auth-request-type request))
                                   nil))
                              (error (e)
                                (warn "Auth worker: unhandled error processing ~a: ~a"
                                      (auth-request-type request) e)
                                (make-auth-result :type (auth-request-type request)
                                                  :success nil
                                                  :host (auth-request-host request)
                                                  :port (auth-request-port request)
                                                  :username (auth-request-username request)
                                                  :client (auth-request-client request)
                                                  :error-reason :internal-error)))))
                (auth-metric-incf processed)
                ;; Push result for main thread to integrate
                (when result
                  (auth-queue-push result-queue result)))))))))
      ;; Step 10c: Wrap in persistent Redis connection if using Redis backend
      (if (and *storage* (typep *storage* 'redis-storage))
          (redis:with-persistent-connection (:host (redis-storage-host *storage*)
                                             :port (redis-storage-port *storage*))
            (worker-main-loop))
          (worker-main-loop)))))

(defun start-auth-worker (request-queue result-queue game &key (name "auth-worker"))
  "Start auth worker thread. Returns thread object."
  #+sbcl
  ;; Capture special variables in closure to ensure visibility in child thread
  ;; (works around SBCL special variable visibility edge cases in some contexts)
  (let ((sessions *active-sessions*)
        (lock *session-lock*)
        (storage *storage*))
    (sb-thread:make-thread
     (lambda ()
       ;; Re-bind for thread-local access
       (let ((*active-sessions* sessions)
             (*session-lock* lock)
             (*storage* storage))
         (auth-worker-loop request-queue result-queue game)))
     :name name))
  #-sbcl
  nil)

(defun start-auth-workers (request-queue result-queue game count)
  "Start COUNT auth worker threads. Returns list of thread objects."
  (loop :for i :from 1 :to count
        :collect (start-auth-worker request-queue result-queue game
                                    :name (format nil "auth-worker-~d" i))))

(defun stop-auth-workers (request-queue worker-threads)
  "Stop all auth worker threads gracefully."
  ;; Send one stop signal per worker
  (dolist (wt worker-threads)
    (declare (ignore wt))
    (auth-queue-push request-queue (make-auth-request :stop-signal t)))
  ;; Wait for all threads to exit
  #+sbcl
  (dolist (wt worker-threads)
    (when wt
      (sb-thread:join-thread wt :timeout 5.0)))
  #-sbcl
  nil)

(defun integrate-auth-results (result-queue socket game elapsed)
  "Process completed auth results on main thread.
   Integrates players into game, updates clients, sends responses.
   Called each frame from main loop - non-blocking."
  (let ((results (auth-queue-drain-nonblocking result-queue)))
    (dolist (result results)
      (let ((host (auth-result-host result))
            (port (auth-result-port result))
            (username (auth-result-username result))
            (client (auth-result-client result))
            (success (auth-result-success result))
            (player (auth-result-player result))
            (player-id (auth-result-player-id result))
            (error-reason (auth-result-error-reason result))
            (zone-id (auth-result-zone-id result)))
        (cond
          ;; Success - integrate player into game
          (success
           (auth-metric-incf success)
           ;; For login: remove any existing player with same ID (stale session)
           (when (and (eq (auth-result-type result) :login) player)
             (let ((existing (find player-id (game-players game) :key #'player-id)))
               (when existing
                 (remove-player-from-game game existing)
                 (log-verbose "Removed stale player ~d before re-adding" player-id))))
           ;; Add player to game
           (when player
             (add-player-to-game game player)
             ;; Update client state
             (setf (net-client-player client) player)
             (setf (net-client-authenticated-p client) t)
             (setf (net-client-account-username client) (string-downcase username))
             (setf (net-client-last-heard client) elapsed)
             ;; Phase 5: Initialize zone tracking - first snapshot is always full
             (setf (net-client-zone-id client) zone-id)
             (setf (net-client-needs-full-resync client) t)
             ;; Step 5: Register local session only (DB calls moved to worker thread)
             (register-player-session-local player :zone-id zone-id
                                            :username (string-downcase username))
             (queue-private-state client player)
             (setf (player-inventory-dirty player) nil
                   (player-hud-stats-dirty player) nil)
             ;; Clear rate limit on success
             (auth-rate-record-success host)
             ;; Send success response
             (send-net-message-with-retry socket
                                          (list :type :auth-ok :player-id player-id)
                                          :host host :port port
                                          :max-retries 3
                                          :delay 50)
             (log-verbose "~a successful: ~a (~a:~d) -> player-id=~d"
                         (if (eq (auth-result-type result) :register) "Registration" "Login")
                         username host port player-id)))
          ;; Failure - send error response
          (t
           (auth-metric-incf fail)
           ;; Clean up session registration for login failures (was registered in worker)
           (when (and (eq (auth-result-type result) :login)
                      (not (eq error-reason :already-logged-in))
                      (not (eq error-reason :bad-credentials)))
             ;; Session was registered in worker before DB operations failed
             (session-unregister username))
           ;; Send failure response
           (let ((fail-message (list :type :auth-fail :reason error-reason)))
             (when (and (eq error-reason :wrong-zone) zone-id)
               (setf fail-message (append fail-message (list :zone-id zone-id))))
             (send-net-message-with-retry socket
                                          fail-message
                                          :host host :port port
                                          :max-retries 3
                                          :delay 50))
           (log-verbose "~a failed from ~a:~d - ~a"
                       (if (eq (auth-result-type result) :register) "Registration" "Login")
                       host port error-reason)))))))

;;; Auth message send/receive helpers

(defun send-auth-message (socket msg-type username password &key host port)
  "Send an authentication message (login or register).
   If *auth-encryption-enabled* and *server-auth-public-key* are set,
   encrypts the credentials with timestamp for replay protection.
   Otherwise sends plaintext."
  (if (and *auth-encryption-enabled* *server-auth-public-key*)
      ;; Encrypted auth: send encrypted credentials payload with timestamp
      (let* ((timestamp (get-universal-time))
             (creds-plist (format nil "(:username ~s :password ~s :timestamp ~d)"
                                 username password timestamp))
             (encrypted (encrypt-auth-payload creds-plist *server-auth-public-key*)))
        (send-net-message socket
                          (list :type msg-type
                                :encrypted-payload encrypted)
                          :host host :port port)
        (log-verbose "Sent encrypted ~a request for ~a" msg-type username))
      ;; Plaintext auth: send username and password directly
      (send-net-message socket
                        (list :type msg-type
                              :username username
                              :password password)
                        :host host :port port)))

(defun extract-auth-credentials (message)
  "Extract username and password from an auth message.
   Supports both encrypted (:encrypted-payload) and plaintext (:username/:password).
   When *auth-require-encryption* is T, rejects plaintext auth.
   Encrypted auth includes replay protection via timestamp and nonce tracking.
   Returns (values username password) or (values NIL NIL) on failure."
  (let ((encrypted-payload (getf message :encrypted-payload)))
    (if encrypted-payload
        ;; Encrypted auth: decrypt and parse
        (let ((decrypted (decrypt-auth-payload encrypted-payload)))
          (if decrypted
              (let* ((*read-eval* nil)
                     (creds (ignore-errors (read-from-string decrypted))))
                (if (listp creds)
                    (let ((username (getf creds :username))
                          (password (getf creds :password))
                          (timestamp (getf creds :timestamp)))
                      ;; Check for replay attack if timestamp present
                      (if (and timestamp (not (auth-check-replay encrypted-payload timestamp)))
                          (progn
                            (log-verbose "Auth rejected: replay attack detected")
                            (values nil nil))
                          (values username password)))
                    (progn
                      (warn "Failed to parse decrypted auth credentials")
                      (values nil nil))))
              (progn
                (log-verbose "Failed to decrypt auth payload")
                (values nil nil))))
        ;; Plaintext auth: check if allowed
        (if *auth-require-encryption*
            (progn
              (warn "Rejecting plaintext auth - encryption required")
              (values nil nil))
            (values (getf message :username)
                    (getf message :password))))))

;;;; Account Authentication Handlers (used by server)

(defun handle-register-request (client host port message socket game elapsed)
  "Handle account registration request.
   Supports both encrypted and plaintext credentials."
  ;; Rate limiting check (use wall-clock time, not sim elapsed)
  (let ((now-rt (/ (get-internal-real-time) (float internal-time-units-per-second 1.0))))
    (declare (ignorable now-rt))
  (unless (auth-rate-check host now-rt)
    (send-net-message-with-retry socket
                                 (list :type :auth-fail :reason :rate-limited)
                                 :host host :port port
                                 :max-retries 3
                                 :delay 50)
    (log-verbose "Registration blocked from ~a:~d - rate limited" host port)
    (return-from handle-register-request nil))
  (multiple-value-bind (username password)
      (extract-auth-credentials message)
    (cond
      ((not (and username password))
       (send-net-message-with-retry socket
                                    (list :type :auth-fail :reason :missing-credentials)
                                    :host host :port port
                                    :max-retries 3
                                    :delay 50)
       (auth-rate-record-failure host now-rt)
       (log-verbose "Registration failed from ~a:~d - missing credentials" host port))
      ((with-retry-exponential (created (lambda () (db-create-account username password))
                                 :max-retries 3
                                 :initial-delay 50
                                 :max-delay 200)
         created)
       ;; Account created successfully - spawn new character
       (let* ((world (game-world game))
              (player (handler-case
                          (spawn-player-at-world world (game-id-source game))
                        (error (e)
                          ;; ID allocation failed (storage outage) - rollback account creation
                          (warn "Registration failed: ID allocation error for ~a: ~a" username e)
                          (with-retry-exponential
                              (deleted (lambda () (db-delete-account username))
                               :max-retries 3
                               :initial-delay 50
                               :max-delay 200
                               :on-final-fail (lambda (e2)
                                                (warn "CRITICAL: Failed to delete account ~a after spawn failure: ~a"
                                                      username e2)))
                            deleted)
                          (send-net-message-with-retry socket
                                                       (list :type :auth-fail :reason :internal-error)
                                                       :host host :port port
                                                       :max-retries 3
                                                       :delay 50)
                          (return-from handle-register-request nil)))))
         (unless player
           (return-from handle-register-request nil))
         (let ((player-id (player-id player)))
           ;; Link account to new character (with retry)
           (let ((linked (with-retry-exponential
                           (set-result (lambda () (db-set-character-id username player-id))
                            :max-retries 3
                            :initial-delay 50
                            :max-delay 200
                            :on-final-fail (lambda (e)
                                             (warn "Failed to set character-id for ~a: ~a" username e)))
                         set-result)))
           (if linked
               (progn
                 ;; Add player to game
                 (add-player-to-game game player)
                 ;; Mark client as authenticated
                 (setf (net-client-player client) player)
                 (setf (net-client-authenticated-p client) t)
                 (setf (net-client-account-username client) (string-downcase username))
                 ;; Register session for persistence
                 (let* ((zone (world-zone world))
                        (zone-id (and zone (zone-id zone))))
                   (register-player-session player :zone-id zone-id
                                            :username (string-downcase username)))
                 ;; Track active session (atomic, should always succeed for new account)
                 (session-try-register username client)
                 ;; Clear rate limit on success
                 (auth-rate-record-success host)
                 ;; Send success response (with retry - critical auth message)
                 (send-net-message-with-retry socket
                                              (list :type :auth-ok :player-id player-id)
                                              :host host :port port
                                              :max-retries 3
                                              :delay 50)
                 (log-verbose "Registration successful: ~a (~a:~d) -> player-id=~d"
                             username host port player-id))
               (progn
                 (warn "Registration rollback: unable to link character for ~a" username)
                 (with-retry-exponential
                     (deleted (lambda () (db-delete-account username))
                      :max-retries 3
                      :initial-delay 50
                      :max-delay 200
                      :on-final-fail (lambda (e)
                                       (warn "CRITICAL: Failed to delete account ~a after link failure: ~a"
                                             username e)))
                   deleted)
                 (send-net-message-with-retry socket
                                              (list :type :auth-fail :reason :internal-error)
                                              :host host :port port
                                              :max-retries 3
                                              :delay 50)
                 (log-verbose "Registration failed from ~a:~d - internal error (link failure)"
                              host port)))))))
      (t
       ;; Username already exists - don't count as rate limit failure (could be probing)
       (send-net-message-with-retry socket
                                    (list :type :auth-fail :reason :username-taken)
                                    :host host :port port
                                    :max-retries 3
                                    :delay 50)
       (log-verbose "Registration failed from ~a:~d - username ~a already taken"
                   host port username))))))

(defun handle-login-request (client host port message socket game clients elapsed)
  "Handle account login request.
   Supports both encrypted and plaintext credentials."
  (declare (ignore clients))
  ;; Use wall-clock time for rate limiting (elapsed is sim-time, wrong timebase)
  (let ((now-rt (/ (get-internal-real-time) (float internal-time-units-per-second 1.0))))
    (declare (ignorable now-rt))
  ;; Rate limiting check
  (unless (auth-rate-check host now-rt)
    (send-net-message-with-retry socket
                                 (list :type :auth-fail :reason :rate-limited)
                                 :host host :port port
                                 :max-retries 3
                                 :delay 50)
    (log-verbose "Login blocked from ~a:~d - rate limited" host port)
    (return-from handle-login-request nil))
  (multiple-value-bind (username password)
      (extract-auth-credentials message)
    (cond
      ((not (and username password))
       (send-net-message-with-retry socket
                                    (list :type :auth-fail :reason :missing-credentials)
                                    :host host :port port
                                    :max-retries 3
                                    :delay 50)
       (auth-rate-record-failure host now-rt)
       (log-verbose "Login failed from ~a:~d - missing credentials" host port))
      ;; Verify credentials first (with retry for transient DB failures)
      ((not (with-retry-exponential (verified (lambda () (db-verify-credentials username password))
                                      :max-retries 3
                                      :initial-delay 50
                                      :max-delay 200)
              verified))
       (send-net-message-with-retry socket
                                    (list :type :auth-fail :reason :bad-credentials)
                                    :host host :port port
                                    :max-retries 3
                                    :delay 50)
       ;; Bad credentials - count toward rate limit
       (auth-rate-record-failure host now-rt)
       (log-verbose "Login failed from ~a:~d - bad credentials for ~a"
                   host port username))
      ;; Atomically try to register session (prevents double-login race)
      ((not (session-try-register username client))
       (send-net-message-with-retry socket
                                    (list :type :auth-fail :reason :already-logged-in)
                                    :host host :port port
                                    :max-retries 3
                                    :delay 50)
       (log-verbose "Login failed from ~a:~d - account ~a already logged in"
                   host port username))
      (t
       ;; Login successful
       (let* ((character-id (with-retry-exponential (id (lambda () (db-get-character-id username))
                                                      :max-retries 3
                                                      :initial-delay 50
                                                      :max-delay 200)
                              id))
              (world (game-world game))
              (zone (world-zone world))
              (zone-id (and zone (zone-id zone)))
              (player nil))
         (cond
           ;; Existing character - load from DB with Phase 6 validation
           (character-id
            ;; Claim session ownership FIRST (required by db-load-player-validated)
            (unless (claim-session-ownership character-id)
              (session-unregister username)
              (send-net-message-with-retry socket
                                           (list :type :auth-fail :reason :ownership-conflict)
                                           :host host :port port
                                           :max-retries 3
                                           :delay 50)
              (log-verbose "Login failed from ~a:~d - ownership conflict for ~a"
                          host port username)
              (return-from handle-login-request nil))
            ;; Load with 4-outcome validation
            (multiple-value-bind (loaded-player loaded-zone-id action)
                (with-retry-exponential
                    (result (lambda ()
                              (multiple-value-list (db-load-player-validated character-id)))
                     :max-retries 3
                     :initial-delay 100
                     :max-delay 300)
                  (values (first result) (second result) (third result)))
              (case action
                ((:ok :clamp)
                 ;; Normal load or corrected data - proceed
                 (when (eq action :clamp)
                   (log-verbose "Player ~a loaded with corrections" character-id))
                 (setf player loaded-player)
                 ;; Remove any existing player with same ID (from stale session)
                 (let ((existing (find (player-id player) (game-players game)
                                       :key #'player-id)))
                   (when existing
                     (remove-player-from-game game existing)
                     (log-verbose "Removed stale player ~d before re-adding"
                                  (player-id player))))
                 ;; Add loaded player to game
                 (add-player-to-game game player)
                 (log-verbose "Loaded existing character ~d for account ~a"
                              character-id username))
                (:quarantine
                 ;; Account needs admin repair - reject login
                 (release-session-ownership character-id)
                 (session-unregister username)
                 (send-net-message-with-retry socket
                                              (list :type :auth-fail :reason :account-quarantined)
                                              :host host :port port
                                              :max-retries 3
                                              :delay 50)
                 (log-verbose "Login failed from ~a:~d - account quarantined for ~a"
                              host port username)
                 (return-from handle-login-request nil))
                (:reject
                 ;; Dangerous data - reject login
                 (release-session-ownership character-id)
                 (session-unregister username)
                 (send-net-message-with-retry socket
                                              (list :type :auth-fail :reason :data-corrupted)
                                              :host host :port port
                                              :max-retries 3
                                              :delay 50)
                 (log-verbose "Login failed from ~a:~d - data corrupted for ~a"
                              host port username)
                 (return-from handle-login-request nil))
                ((:not-found nil)
                 ;; No character data - should not happen for existing account
                 (release-session-ownership character-id)
                 (session-unregister username)
                 (send-net-message-with-retry socket
                                              (list :type :auth-fail :reason :load-failed)
                                              :host host :port port
                                              :max-retries 3
                                              :delay 50)
                 (log-verbose "Login failed from ~a:~d - load failed for ~a"
                              host port username)
                 (return-from handle-login-request nil)))))
           ;; No character yet - spawn new one (shouldn't happen after registration)
           (t
            (handler-case
                (setf player (spawn-player-at-world world (game-id-source game)))
              (error (e)
                ;; ID allocation failed (storage outage) - return login failure
                (warn "Login failed: ID allocation error for ~a: ~a" username e)
                (session-unregister username)
                (send-net-message-with-retry socket
                                             (list :type :auth-fail :reason :internal-error)
                                             :host host :port port
                                             :max-retries 3
                                             :delay 50)
                (return-from handle-login-request nil)))
            (unless player
              (return-from handle-login-request nil))
            (let ((linked (with-retry-exponential
                              (set-result (lambda () (db-set-character-id username (player-id player)))
                               :max-retries 3
                               :initial-delay 50
                               :max-delay 200
                               :on-final-fail (lambda (e)
                                                (warn "Failed to set character-id for ~a: ~a" username e)))
                            set-result)))
              (if linked
                  (progn
                    (add-player-to-game game player)
                    (log-verbose "Created new character ~d for account ~a"
                                (player-id player) username))
                  (progn
                    (warn "Login rollback: unable to link character for ~a" username)
                    (session-unregister username)
                    (send-net-message-with-retry socket
                                                 (list :type :auth-fail :reason :internal-error)
                                                 :host host :port port
                                                 :max-retries 3
                                                 :delay 50)
                    (log-verbose "Login failed from ~a:~d - internal error (link failure)"
                                host port)
                    (return-from handle-login-request nil))))))

         (when player
           ;; Mark client as authenticated
           (setf (net-client-player client) player)
           (setf (net-client-authenticated-p client) t)
           (setf (net-client-account-username client) (string-downcase username))
           (setf (net-client-last-heard client) elapsed)
           ;; Register session for persistence
           (register-player-session player :zone-id zone-id
                                    :username (string-downcase username))
           ;; Note: active session already registered atomically via session-try-register
           ;; Clear rate limit on success
           (auth-rate-record-success host)
           ;; Send success response (with retry - critical auth message)
           (send-net-message-with-retry socket
                                        (list :type :auth-ok :player-id (player-id player))
                                        :host host :port port
                                        :max-retries 3
                                        :delay 50)
           (log-verbose "Login successful: ~a (~a:~d) -> player-id=~d"
                       username host port (player-id player)))))))))

(defun handle-logout-request (client host port game)
  "Handle logout request from authenticated client."
  (when (and client (net-client-authenticated-p client))
    (let ((username (net-client-account-username client))
          (player (net-client-player client)))
      ;; Clear authentication first to prevent further state changes
      (setf (net-client-authenticated-p client) nil)
      (setf (net-client-account-username client) nil)
      ;; Phase 5: Reset zone tracking to prevent stale state on reconnect
      (setf (net-client-zone-id client) nil)
      (setf (net-client-player client) nil)
      (setf (net-client-needs-full-resync client) nil)
      (when player
        ;; Save and unregister session BEFORE removing from active sessions
        ;; This ensures all state changes are persisted
        (db-logout-player player)
        ;; Remove player from game world
        (remove-player-from-game game player))
      (when username
        ;; Remove from active sessions AFTER save completes
        (session-unregister username)
        (log-verbose "Logout: ~a (~a:~d)" username host port)))))

(defun check-client-timeouts (clients elapsed game)
  "Check for timed-out clients and free their sessions. Returns updated client list."
  (let ((updated-clients nil))
    (dolist (client clients)
      (let ((inactive-time (- elapsed (net-client-last-heard client))))
        (if (and (net-client-authenticated-p client)
                 (> inactive-time *client-timeout-seconds*))
            ;; Client timed out - free the session
            (let ((username (net-client-account-username client))
                  (player (net-client-player client)))
              ;; Clear auth first to prevent further state changes
              (setf (net-client-authenticated-p client) nil)
              (setf (net-client-account-username client) nil)
              ;; Phase 5: Reset zone tracking to prevent stale state on reconnect
              (setf (net-client-zone-id client) nil)
              (setf (net-client-player client) nil)
              (setf (net-client-needs-full-resync client) nil)
              (when player
                ;; Save BEFORE removing from active sessions
                (db-logout-player player)
                ;; Remove player from game world
                (remove-player-from-game game player))
              (when username
                ;; Remove from active sessions AFTER save
                (session-unregister username)
                (log-verbose "Client timeout: ~a (inactive for ~,1fs)"
                            username inactive-time))
              ;; Don't add to updated list - remove this client
              )
            ;; Client still active - keep it
            (push client updated-clients))))
    (nreverse updated-clients)))
