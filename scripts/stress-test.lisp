;;;; scripts/stress-test.lisp
;;;; Headless client stress testing
;;;;
;;;; Usage:
;;;;   sbcl --script scripts/stress-test.lisp [target-count] [duration]
;;;;
;;;; Environment variables:
;;;;   STRESS_RATE=10         - Clients per second (optional; default: exponential batches)
;;;;   STRESS_CLIENTS=300     - Target active client count (or pass as arg 1)
;;;;   STRESS_DURATION=60     - Duration in seconds (or pass as arg 2). 0 = run forever
;;;;   STRESS_SEND_HZ=10      - Intent send rate per client (Hz)
;;;;   STRESS_TICK=0.05       - Main loop sleep in seconds
;;;;   STRESS_DIR_SECONDS=3   - Direction change interval in seconds
;;;;   STRESS_DRAIN=5         - Max snapshots drained per tick per client
;;;;   STRESS_STAGGER=0.05    - Delay between spawns inside a batch (seconds)
;;;;   STRESS_PENDING_MAX=200 - Max pending (unauthenticated) clients
;;;;   STRESS_AUTH=register|login|auto - Auth mode (default: register)
;;;;   STRESS_AUTH_RETRY=1.0  - Seconds between auth retries (UDP loss)
;;;;   STRESS_AUTH_MAX=10     - Max auth attempts before drop
;;;;   STRESS_AUTH_BACKOFF=5.0 - Backoff seconds on :rate-limited
;;;;
;;;; Examples:
;;;;   STRESS_RATE=10 make stress
;;;;   STRESS_RATE=5 STRESS_CLIENTS=100 make stress

(defun load-quicklisp ()
  (let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (unless (probe-file setup)
      (format *error-output* "Quicklisp not found~%")
      (sb-ext:exit :code 1))
    (load setup)))

(defvar *run-id* (get-universal-time))
(defvar *client-id* 0)

(defvar *mmorpg-make-net-buffer* nil)
(defvar *mmorpg-make-intent* nil)
(defvar *mmorpg-send-auth* nil)
(defvar *mmorpg-send-net* nil)
(defvar *mmorpg-receive-net* nil)
(defvar *mmorpg-intent->plist* nil)
(defvar *mmorpg-set-dx* nil)
(defvar *mmorpg-set-dy* nil)
(defvar *usocket-connect* nil)
(defvar *usocket-close* nil)
(defvar *ql-register* nil)
(defvar *ql-quickload* nil)

(defvar *auth-ok-count* 0)
(defvar *auth-fail-count* 0)
(defvar *auth-timeout-count* 0)
(defvar *auth-rate-limited-count* 0)
(defvar *auth-bad-credentials-count* 0)
(defvar *auth-username-taken-count* 0)
(defvar *auth-missing-credentials-count* 0)
(defvar *auth-internal-error-count* 0)
(defvar *auth-other-fail-count* 0)
(defvar *auth-retry-count* 0)
(defvar *auth-drop-count* 0)

(defun reset-auth-counts ()
  (setf *auth-ok-count* 0
        *auth-fail-count* 0
        *auth-timeout-count* 0
        *auth-rate-limited-count* 0
        *auth-bad-credentials-count* 0
        *auth-username-taken-count* 0
        *auth-missing-credentials-count* 0
        *auth-internal-error-count* 0
        *auth-other-fail-count* 0
        *auth-retry-count* 0
        *auth-drop-count* 0))

(defparameter *dir-dx*
  #(-1.0f0 1.0f0 0.0f0 0.0f0 -0.7f0 0.7f0 -0.7f0 0.7f0 0.0f0))
(defparameter *dir-dy*
  #(0.0f0 0.0f0 -1.0f0 1.0f0 -0.7f0 -0.7f0 0.7f0 0.7f0 0.0f0))

(defstruct (stress-client (:constructor %make-stress-client))
  id username socket buffer intent authenticated-p
  auth-mode auth-attempts last-auth-time next-auth-time
  next-send-time next-dir-change)

(defun parse-int (s)
  (ignore-errors (parse-integer s)))

(defun parse-float (s)
  (ignore-errors (float (read-from-string s) 1.0f0)))

(defun env-int (name default)
  (let ((s (sb-ext:posix-getenv name)))
    (or (and s (parse-int s)) default)))

(defun env-float (name default)
  (let ((s (sb-ext:posix-getenv name)))
    (or (and s (parse-float s)) default)))

(defun command-args ()
  ;; sb-ext:*posix-argv* includes: sbcl --script script.lisp arg1 arg2 ...
  (let ((argv sb-ext:*posix-argv*))
    (if (and argv (>= (length argv) 4))
        (nthcdr 3 argv)
        '())))

(defun arg-int (index default)
  (let ((args (command-args)))
    (or (and (< index (length args))
             (parse-int (nth index args)))
        default)))

(defun now-seconds ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun next-id ()
  (incf *client-id*))

(defun random-direction ()
  (let ((i (random (length *dir-dx*))))
    (values (aref *dir-dx* i) (aref *dir-dy* i))))

(defun resolve-mmorpg-symbols ()
  "Resolve MMORPG function symbols after quickload to avoid package read-time errors."
  (flet ((sym (name)
           (or (find-symbol name "MMORPG")
               (error "MMORPG symbol not found: ~a" name))))
    (setf *mmorpg-make-net-buffer* (symbol-function (sym "MAKE-NET-BUFFER"))
          *mmorpg-make-intent* (symbol-function (sym "MAKE-INTENT"))
          *mmorpg-send-auth* (symbol-function (sym "SEND-AUTH-MESSAGE"))
          *mmorpg-send-net* (symbol-function (sym "SEND-NET-MESSAGE"))
          *mmorpg-receive-net* (symbol-function (sym "RECEIVE-NET-MESSAGE"))
          *mmorpg-intent->plist* (symbol-function (sym "INTENT->PLIST"))
          *mmorpg-set-dx* (fdefinition (list 'setf (sym "INTENT-MOVE-DX")))
          *mmorpg-set-dy* (fdefinition (list 'setf (sym "INTENT-MOVE-DY"))))))

(defun resolve-usocket-symbols ()
  "Resolve usocket function symbols after quickload to avoid package read-time errors."
  (flet ((sym (name)
           (or (find-symbol name "USOCKET")
               (error "USOCKET symbol not found: ~a" name))))
    (setf *usocket-connect* (symbol-function (sym "SOCKET-CONNECT"))
          *usocket-close* (symbol-function (sym "SOCKET-CLOSE")))))

(defun resolve-ql-symbols ()
  "Resolve Quicklisp function symbols after loading quicklisp."
  (flet ((sym (name)
           (or (find-symbol name "QL")
               (error "QL symbol not found: ~a" name))))
    (setf *ql-register* (symbol-function (sym "REGISTER-LOCAL-PROJECTS"))
          *ql-quickload* (symbol-function (sym "QUICKLOAD")))))

(defun normalize-auth-policy (value)
  (cond
    ((or (null value) (string= value "")) :register)
    ((string-equal value "register") :register)
    ((string-equal value "login") :login)
    ((string-equal value "auto") :auto)
    (t :register)))

(defun initial-auth-mode (policy)
  (if (eq policy :auto) :login policy))

(defun send-auth-for-client (client host port now retry-seconds)
  (incf (stress-client-auth-attempts client))
  (when (> (stress-client-auth-attempts client) 1)
    (incf *auth-retry-count*))
  (setf (stress-client-last-auth-time client) now
        (stress-client-next-auth-time client) (+ now retry-seconds))
  (funcall *mmorpg-send-auth*
           (stress-client-socket client)
           (stress-client-auth-mode client)
           (stress-client-username client)
           "test"
           :host host :port port))

(defun spawn-client (host port auth-policy now retry-seconds)
  "Create a client socket and send auth request. Returns client or NIL on failure.
   Auth completes asynchronously in the main loop."
  (let* ((id (next-id))
         (username (format nil "stress-~d-~d" *run-id* id))
         (socket nil)
         (buffer (funcall *mmorpg-make-net-buffer*))
         (intent (funcall *mmorpg-make-intent*)))
    (handler-case
        (progn
          (setf socket (funcall *usocket-connect* host port :protocol :datagram))
          (let* ((mode (initial-auth-mode auth-policy))
                 (client (%make-stress-client
                          :id id
                          :username username
                          :socket socket
                          :buffer buffer
                          :intent intent
                          :authenticated-p nil
                          :auth-mode mode
                          :auth-attempts 0
                          :last-auth-time 0.0f0
                          :next-auth-time 0.0f0
                          :next-send-time 0.0f0
                          :next-dir-change 0.0f0)))
            (send-auth-for-client client host port now retry-seconds)
            client))
      (error ()
        (when socket
          (ignore-errors (funcall *usocket-close* socket)))
        nil))))

(defun drain-auth (client host port auth-policy now retry-seconds max-attempts backoff-seconds)
  "Check for auth response on a pending client. Returns :ok, :fail, or :pending."
  (multiple-value-bind (msg _host _port)
      (funcall *mmorpg-receive-net*
               (stress-client-socket client)
               (stress-client-buffer client))
    (declare (ignore _host _port))
    (cond
      ((and msg (eq (getf msg :type) :auth-ok))
       (incf *auth-ok-count*)
       :ok)
      ((and msg (eq (getf msg :type) :auth-fail))
       (incf *auth-fail-count*)
       (let ((reason (getf msg :reason)))
         (cond
           ((eq reason :rate-limited)
            (incf *auth-rate-limited-count*)
            (setf (stress-client-next-auth-time client) (+ now backoff-seconds))
            :pending)
           ((eq reason :bad-credentials)
            (incf *auth-bad-credentials-count*)
            (if (and (eq auth-policy :auto)
                     (< (stress-client-auth-attempts client) max-attempts)
                     (eq (stress-client-auth-mode client) :login))
                (progn
                  (setf (stress-client-auth-mode client) :register)
                  (send-auth-for-client client host port now retry-seconds)
                  :pending)
                :fail))
           ((eq reason :username-taken)
            (incf *auth-username-taken-count*)
            (if (and (eq auth-policy :auto)
                     (< (stress-client-auth-attempts client) max-attempts)
                     (eq (stress-client-auth-mode client) :register))
                (progn
                  (setf (stress-client-auth-mode client) :login)
                  (send-auth-for-client client host port now retry-seconds)
                  :pending)
                :fail))
           ((eq reason :missing-credentials)
            (incf *auth-missing-credentials-count*)
            :fail)
           ((or (eq reason :internal-error)
                (eq reason :load-failed)
                (eq reason :data-corrupted))
            (incf *auth-internal-error-count*)
            :fail)
           (t
            (incf *auth-other-fail-count*)
            :fail))))
      (t
       ;; No message; retry on timeout
       (when (and (>= now (stress-client-next-auth-time client))
                  (< (stress-client-auth-attempts client) max-attempts))
         (incf *auth-timeout-count*)
         (send-auth-for-client client host port now retry-seconds))
       (if (>= (stress-client-auth-attempts client) max-attempts)
           :fail
           :pending)))))

(defun step-client (client host port now send-interval dir-interval drain-max)
  "Advance one client state: change direction, send intent, drain snapshots."
  (when (>= now (stress-client-next-dir-change client))
    (multiple-value-bind (dx dy) (random-direction)
      (funcall *mmorpg-set-dx* dx (stress-client-intent client))
      (funcall *mmorpg-set-dy* dy (stress-client-intent client))
      (setf (stress-client-next-dir-change client) (+ now dir-interval))))
  (when (>= now (stress-client-next-send-time client))
    (funcall *mmorpg-send-net*
             (stress-client-socket client)
             (list :type :intent
                   :payload (funcall *mmorpg-intent->plist*
                                     (stress-client-intent client)))
             :host host :port port)
    (setf (stress-client-next-send-time client) (+ now send-interval)))
  ;; Drain snapshots (non-blocking)
  (dotimes (_ drain-max)
    (multiple-value-bind (msg _host _port)
        (funcall *mmorpg-receive-net*
                 (stress-client-socket client)
                 (stress-client-buffer client))
      (declare (ignore _host _port))
      (unless msg
        (return)))))

(defun main ()
  (load-quicklisp)
  (resolve-ql-symbols)
  (funcall *ql-register*)
  (funcall *ql-quickload* :mmorpg :silent t)
  (resolve-mmorpg-symbols)
  (resolve-usocket-symbols)
  (reset-auth-counts)

  (let* ((host (or (sb-ext:posix-getenv "MMORPG_HOST") "127.0.0.1"))
         (port (parse-integer (or (sb-ext:posix-getenv "MMORPG_PORT") "1337")))
         (target (arg-int 0 (env-int "STRESS_CLIENTS" 300)))
         (duration (arg-int 1 (env-int "STRESS_DURATION" 60)))
         (rate (env-float "STRESS_RATE" nil))
         (send-hz (env-float "STRESS_SEND_HZ" 10.0f0))
         (tick (env-float "STRESS_TICK" 0.05f0))
         (dir-interval (env-float "STRESS_DIR_SECONDS" 3.0f0))
         (drain-max (env-int "STRESS_DRAIN" 5))
         (stagger (env-float "STRESS_STAGGER" 0.05f0))
         (pending-max (env-int "STRESS_PENDING_MAX" 200))
         (auth-policy (normalize-auth-policy (sb-ext:posix-getenv "STRESS_AUTH")))
         (auth-retry (env-float "STRESS_AUTH_RETRY" 1.0f0))
         (auth-max (env-int "STRESS_AUTH_MAX" 10))
         (auth-backoff (env-float "STRESS_AUTH_BACKOFF" 5.0f0))
         (send-interval (if (> send-hz 0.0f0) (/ 1.0f0 send-hz) 0.1f0))
         (pending (make-array 0 :adjustable t :fill-pointer 0))
         (clients (make-array 0 :adjustable t :fill-pointer 0))
         (total-spawned 0)
         (active 0)
         (dropped 0)
         (start (now-seconds))
         (end (if (and duration (> duration 0))
                  (+ start duration)
                  most-positive-single-float))
         (next-spawn start)
         (batch 10)
         (next-batch start)
         (target-reached nil)
         (last-report start))

    (format t "~&=== Stress Test ===~%")
    (format t "Target clients: ~d~%" target)
    (format t "Duration: ~a seconds~%" duration)
    (if rate
        (format t "Spawn rate: ~,2f clients/sec~%" rate)
        (format t "Spawn mode: exponential batches (use STRESS_RATE=N for fixed rate)~%"))
    (format t "Send rate: ~,2f Hz per client~%" send-hz)
    (format t "Tick: ~,2f s | Dir change: ~,2f s | Drain: ~d~%" tick dir-interval drain-max)
    (format t "Auth mode: ~a | Pending max: ~d | Retry: ~,2fs | Max: ~d | Backoff: ~,2fs~%"
            auth-policy pending-max auth-retry auth-max auth-backoff)
    (finish-output)

    (unwind-protect
         (loop :while (< (now-seconds) end)
               :do (let ((now (now-seconds)))
                     ;; Spawn clients (rate or batch)
                     (let ((inflight (+ active (length pending))))
                       (cond
                         (rate
                          (let ((spawn-interval (if (> rate 0.0f0) (/ 1.0f0 rate) 1.0f0)))
                            (loop :while (and (< inflight target)
                                              (>= now next-spawn))
                                  :do (if (>= (length pending) pending-max)
                                          (progn
                                            (setf next-spawn (+ now spawn-interval))
                                            (return))
                                          (let ((client (spawn-client host port auth-policy now auth-retry)))
                                            (when client
                                              (vector-push-extend client pending)
                                              (incf total-spawned)
                                              (incf inflight))))
                                      (setf next-spawn (+ next-spawn spawn-interval))
                                      (when (and stagger (> stagger 0.0f0))
                                        (sleep stagger)))))
                         (t
                          (when (and (< inflight target) (>= now next-batch))
                            (if (>= (length pending) pending-max)
                                (setf next-batch (+ now 2.0f0))
                                (let ((to-spawn (min batch (- target inflight))))
                                  (dotimes (_ to-spawn)
                                    (let ((client (spawn-client host port auth-policy now auth-retry)))
                                      (when client
                                        (vector-push-extend client pending)
                                        (incf total-spawned)))
                                    (when (and stagger (> stagger 0.0f0))
                                      (sleep stagger)))
                                  (setf batch (max batch (truncate (* batch 1.2))))
                                  (setf next-batch (+ now 10.0f0))))))))

                     ;; Promote pending clients that authenticated
                     (let ((i 0))
                       (loop :while (< i (length pending))
                             :do (let ((client (aref pending i)))
                                   (case (drain-auth client host port auth-policy now auth-retry auth-max auth-backoff)
                                     (:ok
                                      (setf (stress-client-authenticated-p client) t)
                                      (multiple-value-bind (dx dy) (random-direction)
                                        (funcall *mmorpg-set-dx* dx (stress-client-intent client))
                                        (funcall *mmorpg-set-dy* dy (stress-client-intent client)))
                                      (setf (stress-client-next-send-time client)
                                            (+ now (random send-interval))
                                            (stress-client-next-dir-change client)
                                            (+ now (random dir-interval)))
                                      (vector-push-extend client clients)
                                      (incf active)
                                      (let ((last-idx (1- (length pending))))
                                        (setf (aref pending i) (aref pending last-idx))
                                        (decf (fill-pointer pending))))
                                     (:fail
                                      (incf *auth-drop-count*)
                                      (incf dropped)
                                      (ignore-errors (funcall *usocket-close*
                                                              (stress-client-socket client)))
                                      (let ((last-idx (1- (length pending))))
                                        (setf (aref pending i) (aref pending last-idx))
                                        (decf (fill-pointer pending))))
                                     (t
                                      (incf i))))))

                     (when (and (not target-reached) (>= active target))
                       (setf target-reached t)
                       (format t "[STRESS] Target active clients reached: ~d~%" active)
                       (finish-output))

                     ;; Step active clients
                     (loop :for i :from 0 :below (length clients)
                           :for client = (aref clients i)
                           :do (step-client client host port now send-interval dir-interval drain-max))

                     ;; Periodic report
                     (when (>= (- now last-report) 5.0f0)
                       (setf last-report now)
                       (format t "[STRESS] Active: ~d | Pending: ~d | Spawned: ~d | Dropped: ~d~%"
                               active (length pending) total-spawned dropped)
                       (format t "         Auth ok=~d fail=~d timeout=~d rate=~d retry=~d drop=~d~%"
                               *auth-ok-count* *auth-fail-count* *auth-timeout-count*
                               *auth-rate-limited-count* *auth-retry-count* *auth-drop-count*)
                       (finish-output))

                     (sleep tick)))
      ;; Cleanup
      (loop :for client :across clients
            :do (ignore-errors (funcall *usocket-close*
                                        (stress-client-socket client))))
      (loop :for client :across pending
            :do (ignore-errors (funcall *usocket-close*
                                        (stress-client-socket client))))
      (format t "~&[STRESS] done. Spawned=~d Active=~d Pending=~d Dropped=~d~%"
              total-spawned active (length pending) dropped)
      (finish-output))))

(main)
