;; NOTE: If you change behavior here, update docs/net-server.md :)
(in-package #:mmorpg)

;;; net-server.lisp - Server UDP loop, dispatch, connection tracking
;;;
;;; Contains `run-server`: the main server entry point that initializes
;;; storage, binds a UDP socket, runs the fixed-tick simulation, and
;;; broadcasts snapshots to connected clients.

(defun run-server (&key (host *net-default-host*)
                        (port *net-default-port*)
                        (max-seconds 0.0)
                        (max-frames 0)
                        (worker-threads 1))
  ;; Run a UDP server that simulates the game and streams snapshots.
  ;; Scaling: One process can host MANY zones. When you scale horizontally,
  ;; assign each process a cluster/shard of zones based on capacity (CPU/network),
  ;; not a 1-process-per-zone model.
  ;; See SERVER_PERFORMANCE.md for horizontal scaling strategy.
  ;;
  ;; WORKER-THREADS: Number of threads for parallel snapshot sending (default 1).
  ;;   - 1 = serial sending (simple, default)
  ;;   - N = parallel sending across N threads (for high client counts)
  ;;   - Recommended: (get-nproc) on multi-core machines
  ;;   - Safe: Only parallelizes network I/O, simulation remains serial
  (with-fatal-error-log ((format nil "Server runtime (~a:~d)" host port))
    ;; Initialize storage backend from environment variables:
    ;; MMORPG_DB_BACKEND: "memory" or "redis" (default: redis)
    ;; MMORPG_REDIS_HOST: Redis host (default: 127.0.0.1)
    ;; MMORPG_REDIS_PORT: Redis port (default: 6379)
    (let* ((backend-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_DB_BACKEND")
                            #-sbcl (uiop:getenv "MMORPG_DB_BACKEND")
                            "redis"))
           (backend (cond
                      ((string-equal backend-str "redis") :redis)
                      ((string-equal backend-str "memory") :memory)
                      (t (progn
                           (warn "Unknown MMORPG_DB_BACKEND=~a, defaulting to redis" backend-str)
                           :redis))))
           (redis-host (or #+sbcl (sb-ext:posix-getenv "MMORPG_REDIS_HOST")
                           #-sbcl (uiop:getenv "MMORPG_REDIS_HOST")
                           "127.0.0.1"))
           (redis-port-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_REDIS_PORT")
                               #-sbcl (uiop:getenv "MMORPG_REDIS_PORT")
                               "6379"))
           (redis-port (or (ignore-errors (parse-integer redis-port-str)) 6379)))
      (init-storage :backend backend :host redis-host :port redis-port)
      ;; Load Redis Lua scripts for atomic operations (Phase 5 - Trade System)
      (load-trade-scripts))
    ;; Initialize profiling from environment variables (Phase 6)
    ;; MMORPG_PROFILE: "1" to enable timing hooks
    ;; MMORPG_VERBOSE_GC: "1" to log GC stats per frame
    (let ((profile-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_PROFILE")
                           #-sbcl (uiop:getenv "MMORPG_PROFILE")))
          (gc-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_VERBOSE_GC")
                      #-sbcl (uiop:getenv "MMORPG_VERBOSE_GC")))
          (snapshot-rate-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_SNAPSHOT_RATE")
                                 #-sbcl (uiop:getenv "MMORPG_SNAPSHOT_RATE")))
          (binary-snapshots-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_BINARY_SNAPSHOTS")
                                    #-sbcl (uiop:getenv "MMORPG_BINARY_SNAPSHOTS"))))
      (when (and profile-str (string= profile-str "1"))
        (setf *profile-enabled* t)
        (init-profile-log 10000)
        (format t "~&SERVER: Profiling enabled~%"))
      (when (and gc-str (string= gc-str "1"))
        (setf *verbose-gc* t)
        (format t "~&SERVER: GC monitoring enabled~%"))
      ;; Phase 3: Configurable snapshot rate (default 20Hz)
      (when snapshot-rate-str
        (let ((rate (parse-integer snapshot-rate-str :junk-allowed t)))
          (when (and rate (> rate 0) (<= rate 60))
            (setf *snapshot-rate-hz* rate
                  *snapshot-interval* (/ 1.0 rate))
            (format t "~&SERVER: Snapshot rate set to ~dHz~%" rate))))
      ;; Phase 3 Task 3.1: Binary snapshot encoding
      (when (and binary-snapshots-str (string= binary-snapshots-str "1"))
        (setf *use-binary-snapshots* t)
        (ensure-binary-send-buffer)
        (format t "~&SERVER: Binary snapshots enabled~%")))
    ;; Seamless zone transitions: verbose zone diagnostics
    (let ((zone-verbose-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_VERBOSE_ZONES")
                                #-sbcl (uiop:getenv "MMORPG_VERBOSE_ZONES"))))
      (when (and zone-verbose-str (string= zone-verbose-str "1"))
        (setf *verbose-zone-transitions* t)
        (format t "~&SERVER: Zone transition diagnostics enabled~%")))
    ;; Phase 4 Task 4.4: NPC object pooling
    (let ((npc-pool-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_NPC_POOL")
                            #-sbcl (uiop:getenv "MMORPG_NPC_POOL"))))
      (when (and npc-pool-str (string= npc-pool-str "1"))
        (setf *use-npc-pool* t)
        (prewarm-npc-pool 256)
        (format t "~&SERVER: NPC object pooling enabled (256 pre-warmed)~%")))
    ;; Phase 5 Task 5.4: Strategic GC scheduling
    (let ((gc-sched-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_GC_SCHEDULING")
                            #-sbcl (uiop:getenv "MMORPG_GC_SCHEDULING"))))
      (when (and gc-sched-str (string= gc-sched-str "1"))
        (setf *gc-scheduling-enabled* t
              *gc-last-time* 0.0)
        (format t "~&SERVER: Strategic GC scheduling enabled (~,0fs interval)~%"
                *gc-interval-seconds*)))
    ;; Auth throughput: configurable worker count, queue depth, message cap
    (let ((auth-workers-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_AUTH_WORKERS")
                                #-sbcl (uiop:getenv "MMORPG_AUTH_WORKERS")))
          (auth-queue-max-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_AUTH_QUEUE_MAX")
                                  #-sbcl (uiop:getenv "MMORPG_AUTH_QUEUE_MAX")))
          (max-msg-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_MAX_MESSAGES_PER_TICK")
                           #-sbcl (uiop:getenv "MMORPG_MAX_MESSAGES_PER_TICK")))
          (auth-metrics-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_AUTH_METRICS")
                                #-sbcl (uiop:getenv "MMORPG_AUTH_METRICS"))))
      (when auth-workers-str
        (let ((n (parse-integer auth-workers-str :junk-allowed t)))
          (when (and n (> n 0) (<= n 32))
            (setf *auth-worker-count* n))))
      (when auth-queue-max-str
        (let ((n (parse-integer auth-queue-max-str :junk-allowed t)))
          (when (and n (>= n 0))
            (setf *auth-queue-max-depth* n))))
      (when max-msg-str
        (let ((n (parse-integer max-msg-str :junk-allowed t)))
          (when (and n (> n 0))
            (setf *max-messages-per-tick* n))))
      (when (and auth-metrics-str (string= auth-metrics-str "1"))
        (setf *auth-metrics-logging* t)
        (format t "~&SERVER: Auth metrics logging enabled~%")))
    ;; Step 11: Configurable PBKDF2 iteration count
    (let ((hash-iter-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_PASSWORD_HASH_ITERATIONS")
                              #-sbcl (uiop:getenv "MMORPG_PASSWORD_HASH_ITERATIONS"))))
      (when hash-iter-str
        (let ((n (parse-integer hash-iter-str :junk-allowed t)))
          (when (and n (>= n 1000) (<= n 1000000))
            (setf *password-hash-iterations* n)
            (format t "~&SERVER: PBKDF2 iterations set to ~d~%" n)))))
    ;; Clear any stale session state from previous REPL runs
    ;; (Important when restarting server without restarting the Lisp process)
    (log-verbose "Clearing stale sessions (active=~d, player=~d)"
                 (hash-table-count *active-sessions*)
                 (hash-table-count *player-sessions*))
    (session-clear-all)
    (clear-all-player-sessions)
    (log-verbose "Sessions cleared")
    ;; Validate zone transition config invariants at startup
    (validate-zone-config)
    ;; Initialize auth encryption (generates server keypair)
    ;; The server public key should be shared with clients for encrypted auth
    (when *auth-encryption-enabled*
      (let ((pub-key (init-server-encryption)))
        (format t "~&SERVER: Auth encryption enabled. Public key: ~a~%" pub-key)
        (finish-output)))
    (let* ((socket (usocket:socket-connect nil nil
                                          :protocol :datagram
                                          :local-host host
                                          :local-port port))
           (recv-buffer (make-net-buffer))
           (game (make-server-game))
           (clients nil)
           (stop-flag nil)
           (stop-reason nil)
           (last-flush-time 0.0)
           (last-ownership-refresh-time 0.0)
           (last-auth-metrics-time 0.0)
           ;; Delta tracking for auth metrics (only log when changed)
           (prev-auth-queued 0)
           (prev-auth-processed 0)
           (prev-auth-success 0)
           (prev-auth-fail 0)
           (prev-auth-expired 0)
           (prev-auth-rejected 0)
           ;; Auth worker threads for non-blocking login/registration
           (auth-request-queue (make-auth-queue-instance))
           (auth-result-queue (make-auth-queue-instance :max-depth 0)) ; result queue unbounded
           (auth-workers (start-auth-workers auth-request-queue auth-result-queue
                                             game *auth-worker-count*)))
      ;; Set global admin variables
      (setf *server-game* game)
      (setf *server-socket* socket)
      (setf *server-clients* clients)
      (setf *server-start-time* (get-internal-real-time))
      (setf *server-total-saves* 0)
      (format t "~&SERVER: listening on ~a:~d (worker-threads=~d, auth-workers=~d, queue-max=~d)~%"
              host port worker-threads *auth-worker-count* *auth-queue-max-depth*)
      (log-verbose "Server config: tick=~,3fs buffer=~d workers=~d auth-workers=~d max-msg/tick=~d"
                   *sim-tick-seconds* *net-buffer-size* worker-threads
                   *auth-worker-count* *max-messages-per-tick*)
      (finish-output)
      (unwind-protect
           (handler-case
               ;; Wrap main loop in persistent Redis connection (same pattern as auth workers)
               ;; This reduces connection churn for batch flush + session ownership refresh
               (flet ((server-main-loop ()
                        (loop :with elapsed = 0.0
                     :with frames = 0
                     :with accumulator = 0.0
                     :with snapshot-seq = 0  ; Delta compression sequence counter
                     :with snapshot-accumulator = 0.0  ; Phase 3: decouple snapshot rate
                     ;; Phase 3 (Code Standards): Pooled event plist buffers to avoid per-tick allocation
                     ;; Size must match +event-ring-size+ to avoid silent event loss
                     :with event-plist-pool = (loop :repeat +event-ring-size+ :collect (list :type nil :text nil))
                     :with event-cons-pool = (loop :repeat +event-ring-size+ :collect (cons nil nil))
                     :with tick-units = (floor (* *sim-tick-seconds*
                                                  internal-time-units-per-second))
                     :until (or stop-flag
                                (and (> max-seconds 0.0)
                                     (>= elapsed max-seconds))
                                (and (> max-frames 0)
                                     (>= frames max-frames)))
                     :do ;; Track frame start time for accurate sleep
                         (let ((frame-start (get-internal-real-time)))
                         ;; 1. Receive pending intents (non-blocking UDP receive)
                         ;; Step 7: Cap messages per tick to prevent starvation
                         (let ((msg-count 0)
                               (now-rt (/ (float (get-internal-real-time) 1.0d0)
                                          (float internal-time-units-per-second 1.0d0))))
                         (loop
                           (when (>= msg-count *max-messages-per-tick*)
                             (return))
                           (multiple-value-bind (message host port)
                               (receive-net-message socket recv-buffer)
                             (unless message
                               (return))
                             (incf msg-count)
                             (multiple-value-bind (client next-clients _new)
                                 (register-net-client game clients host port
                                                      :timestamp elapsed)
                               (declare (ignore _new))
                               (setf clients next-clients)
                               (setf *server-clients* next-clients)
                               (case (getf message :type)
                                 (:hello
                                  (log-verbose "Handshake received from ~a:~d" host port)
                                  ;; Respond so client knows server is online
                                  (send-net-message socket (list :type :hello-ack)
                                                    :host host :port port))
                                 (:register
                                  ;; Queue for async processing with backpressure (Steps 3, 6)
                                  (if (auth-rate-check host now-rt)
                                      (multiple-value-bind (username password)
                                          (extract-auth-credentials message)
                                        (if (and username password)
                                            ;; Step 3: try-push with backpressure
                                            (if (auth-queue-try-push auth-request-queue
                                                             (make-auth-request :type :register
                                                                                :host host
                                                                                :port port
                                                                                :username username
                                                                                :password password
                                                                                :client client
                                                                                :timestamp (float now-rt 1.0)))
                                                (auth-metric-incf queued)
                                                ;; Queue full - immediate rejection
                                                (progn
                                                  (auth-metric-incf rejected-busy)
                                                  (send-net-message-with-retry socket
                                                    (list :type :auth-fail :reason :server-busy)
                                                    :host host :port port :max-retries 3 :delay 50)))
                                            (progn
                                              (send-net-message-with-retry socket
                                                                           (list :type :auth-fail :reason :missing-credentials)
                                                                           :host host :port port :max-retries 3 :delay 50)
                                              (auth-rate-record-failure host now-rt))))
                                      (send-net-message-with-retry socket
                                                                   (list :type :auth-fail :reason :rate-limited)
                                                                   :host host :port port :max-retries 3 :delay 50)))
                                 (:login
                                  ;; Queue for async processing with backpressure (Steps 3, 6)
                                  (if (auth-rate-check host now-rt)
                                      (multiple-value-bind (username password)
                                          (extract-auth-credentials message)
                                        (if (and username password)
                                            ;; Step 3: try-push with backpressure
                                            (if (auth-queue-try-push auth-request-queue
                                                             (make-auth-request :type :login
                                                                                :host host
                                                                                :port port
                                                                                :username username
                                                                                :password password
                                                                                :client client
                                                                                :timestamp (float now-rt 1.0)))
                                                (auth-metric-incf queued)
                                                ;; Queue full - immediate rejection
                                                (progn
                                                  (auth-metric-incf rejected-busy)
                                                  (send-net-message-with-retry socket
                                                    (list :type :auth-fail :reason :server-busy)
                                                    :host host :port port :max-retries 3 :delay 50)))
                                            (progn
                                              (send-net-message-with-retry socket
                                                                           (list :type :auth-fail :reason :missing-credentials)
                                                                           :host host :port port :max-retries 3 :delay 50)
                                              (auth-rate-record-failure host now-rt))))
                                      (send-net-message-with-retry socket
                                                                   (list :type :auth-fail :reason :rate-limited)
                                                                   :host host :port port :max-retries 3 :delay 50)))
                                 (:logout
                                  (handle-logout-request client host port game))
                                 (:intent
                                  (when (and client (net-client-authenticated-p client))
                                    ;; Parse ack for delta compression
                                    (let ((ack (getf message :ack)))
                                      (when ack
                                        (setf (net-client-last-acked-seq client) ack)
                                        (setf (net-client-needs-full-resync client) nil)))
                                    (let* ((payload (getf message :payload))
                                           (seq (and payload (getf payload :sequence)))
                                           (player (net-client-player client)))
                                      (when (and player (integerp seq))
                                        (setf (player-last-sequence player) seq))
                                      (apply-intent-plist
                                       (net-client-intent client)
                                       payload))))
                                 (t
                                  (log-verbose "Unknown message type from ~a:~d -> ~s"
                                               host port (getf message :type))))))))
                         ;; 1b. Integrate completed auth results (non-blocking)
                         ;; This adds newly authenticated players to the game
                         (integrate-auth-results auth-result-queue socket game elapsed)
                         ;; 2. Apply client intents to player state (O(clients), cheap)
                         (apply-client-intents clients)
                         ;; 3. Run fixed-tick simulation (O(players * npcs), main bottleneck)
                         (let ((dt *sim-tick-seconds*))
                           (incf elapsed dt)
                           (incf frames)
                           (incf snapshot-accumulator dt)  ; Phase 3: track time for snapshot rate
                           (multiple-value-bind (new-acc transitions)
                               (server-step game nil dt accumulator)
                             (setf accumulator new-acc)
                             (dotimes (_ transitions)
                               (declare (ignore _)))))
                         ;; 3b. Periodic batch flush (tier-2 writes every ~30s)
                         (when (>= (- elapsed last-flush-time) *batch-flush-interval*)
                           (flush-dirty-players)
                           (setf last-flush-time elapsed))
                         ;; 3b2. Periodic auth metrics logging (every 30s, only when enabled and changed)
                         (when (>= (- elapsed last-auth-metrics-time) 30.0)
                           (when *auth-metrics-logging*
                             (let* ((cur-queued (auth-metrics-queued *auth-metrics*))
                                    (cur-processed (auth-metrics-processed *auth-metrics*))
                                    (cur-success (auth-metrics-success *auth-metrics*))
                                    (cur-fail (auth-metrics-fail *auth-metrics*))
                                    (cur-expired (auth-metrics-expired *auth-metrics*))
                                    (cur-rejected (auth-metrics-rejected-busy *auth-metrics*))
                                    (d-queued (- cur-queued prev-auth-queued))
                                    (d-processed (- cur-processed prev-auth-processed))
                                    (d-success (- cur-success prev-auth-success))
                                    (d-fail (- cur-fail prev-auth-fail))
                                    (d-expired (- cur-expired prev-auth-expired))
                                    (d-rejected (- cur-rejected prev-auth-rejected)))
                               ;; Only log if any metric changed
                               (when (or (> d-queued 0) (> d-processed 0) (> d-success 0)
                                         (> d-fail 0) (> d-expired 0) (> d-rejected 0))
                                 (format t "[AUTH] queued=~d(+~d) processed=~d(+~d) success=~d(+~d) fail=~d(+~d) expired=~d(+~d) rejected=~d(+~d) depth=~d~%"
                                         cur-queued d-queued
                                         cur-processed d-processed
                                         cur-success d-success
                                         cur-fail d-fail
                                         cur-expired d-expired
                                         cur-rejected d-rejected
                                         (auth-queue-count auth-request-queue))
                                 (finish-output))
                               ;; Update previous values for next delta
                               (setf prev-auth-queued cur-queued
                                     prev-auth-processed cur-processed
                                     prev-auth-success cur-success
                                     prev-auth-fail cur-fail
                                     prev-auth-expired cur-expired
                                     prev-auth-rejected cur-rejected)))
                           (setf last-auth-metrics-time elapsed))
                         ;; 3c. Periodic session ownership refresh (every ~30s, half of TTL)
                         (when (>= (- elapsed last-ownership-refresh-time) *ownership-refresh-interval*)
                           (let ((lost-sessions (refresh-all-session-ownerships)))
                             (when lost-sessions
                               ;; Handle lost ownership - full cleanup including net clients
                               ;; Phase 2: refresh-all-session-ownerships now attempts re-claim before
                               ;; reporting as lost, so these are truly lost to another server
                               (dolist (player-id lost-sessions)
                                 (warn "Session ownership lost for player ~a - forcing cleanup" player-id)
                                 (let ((session (gethash player-id *player-sessions*)))
                                   (when session
                                     (let ((player (player-session-player session))
                                           (username (player-session-username session)))
                                       ;; 1. Find and de-auth the net client (Phase 2)
                                       (when username
                                         (let ((client (session-get username)))
                                           (when client
                                             ;; Clear auth fields
                                             (setf (net-client-authenticated-p client) nil)
                                             (setf (net-client-player client) nil)
                                             ;; Remove from clients list
                                             (setf clients (remove client clients :test #'eq))
                                             (log-verbose "De-authed client for ~a due to ownership loss" username)))
                                         ;; Remove from active sessions (by username)
                                         (session-unregister username))
                                       ;; 2. Remove from player sessions locally only (Phase 2)
                                       ;; Don't touch online set - new owner controls that now
                                       (unregister-player-session-local player-id)
                                       ;; 3. Remove player from game world
                                       (when player
                                         (remove-player-from-game game player))))))))
                           (setf last-ownership-refresh-time elapsed))
                         ;; 3d. Check for timed-out clients (free sessions after 30s inactivity)
                         (setf clients (check-client-timeouts clients elapsed game))
                         ;; 4. Send snapshots to all clients (Phase 3: rate-limited)
                         ;; Toggle between Prong 1 (full) and Prong 2 (delta) via *delta-compression-enabled*
                         ;; EXCEPTION HANDLING: Snapshot errors are non-fatal, skip frame and continue.
                         ;; Phase 3: Decouple snapshot rate from sim rate (default 20Hz vs 60Hz sim)
                         (when (and clients (>= snapshot-accumulator *snapshot-interval*))
                           (decf snapshot-accumulator *snapshot-interval*)
                           (handler-case
                               ;; Pop events directly into pooled buffers (no intermediate list allocation)
                               (let* ((event-plists (pop-combat-events-into
                                                     (game-combat-events game)
                                                     event-plist-pool
                                                     event-cons-pool
                                                     #'combat-event->plist-into))
                                      (current-seq (incf snapshot-seq)))
                                 (if *delta-compression-enabled*
                                     ;; Prong 2: Delta compression - dirty entities only
                                     (broadcast-snapshots-with-delta socket clients game
                                                                     current-seq event-plists)
                                     ;; Prong 1: Full snapshots - all entities every frame
                                     (let ((state (serialize-game-state-compact game)))
                                       (setf state (plist-put state :seq current-seq))
                                       (send-snapshots-parallel socket clients state event-plists
                                                                worker-threads))))
                             (error (e)
                               (warn "Failed to serialize/send snapshot (frame ~d): ~a" frames e)
                               (log-verbose "Snapshot error, skipping frame: ~a" e))))
                         ;; 4b. Send private state updates (inventory/equipment/stats)
                         (when clients
                           (send-private-states socket clients))
                         ;; 4c. Strategic GC scheduling (Task 5.4)
                         ;; Trigger GC at safe points (after snapshots) to reduce worst-case spikes
                         (when (and *gc-scheduling-enabled*
                                    (>= (- elapsed *gc-last-time*) *gc-interval-seconds*))
                           (setf *gc-last-time* elapsed)
                           #+sbcl (sb-ext:gc :full nil)  ; Incremental, not full
                           (when *verbose-gc*
                             (format t "~&SERVER: Scheduled GC at ~,1fs~%" elapsed)))
                         ;; 5. Sleep for remaining tick time (accounts for processing duration)
                         ;; This ensures consistent tick rate regardless of frame complexity.
                         (let* ((frame-end (get-internal-real-time))
                                (frame-elapsed (- frame-end frame-start))
                                (remaining-units (- tick-units frame-elapsed)))
                           (when (> remaining-units 0)
                             (sleep (/ remaining-units internal-time-units-per-second))))))))
                 ;; Call server-main-loop with or without persistent Redis connection
                 (if (and *storage* (typep *storage* 'redis-storage))
                     (redis:with-persistent-connection (:host (redis-storage-host *storage*)
                                                        :port (redis-storage-port *storage*))
                       (server-main-loop))
                     (server-main-loop)))
             #+sbcl
             (sb-sys:interactive-interrupt ()
               (setf stop-flag t stop-reason "interrupt")))
        (when stop-reason
          (format t "~&SERVER: shutdown requested (~a).~%" stop-reason)
          (finish-output))
        ;; Stop auth worker threads
        (stop-auth-workers auth-request-queue auth-workers)
        ;; Graceful shutdown: flush all dirty players and close storage
        (db-shutdown-flush)
        (usocket:socket-close socket)))))
