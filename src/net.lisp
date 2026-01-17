;; NOTE: If you change behavior here, update docs/net.md :)
(in-package #:mmorpg)

(defun make-net-buffer (&optional (size *net-buffer-size*))
  ;; Allocate a reusable UDP buffer.
  (make-array size :element-type '(unsigned-byte 8) :initial-element 0))

(defun string-to-octets (string)
  ;; Encode STRING as ASCII octets.
  (let* ((len (length string))
         (bytes (make-array len :element-type '(unsigned-byte 8))))
    (dotimes (i len)
      (setf (aref bytes i) (char-code (aref string i))))
    bytes))

(defun octets-to-string (octets length)
  ;; Decode LENGTH octets into an ASCII string.
  (let ((chars (make-string length)))
    (dotimes (i length)
      (setf (aref chars i) (code-char (aref octets i))))
    chars))

(defun encode-net-message (message)
  ;; Serialize MESSAGE as a readable string.
  (with-output-to-string (out)
    (prin1 message out)))

(defun decode-net-message (string)
  ;; Parse a message string into a Lisp object (safe read).
  (let ((*read-eval* nil))
    (handler-case
        (multiple-value-bind (form _) (read-from-string string nil nil)
          (declare (ignore _))
          (when (listp form)
            form))
      (error () nil))))

(defun send-net-message (socket message &key host port)
  ;; Send MESSAGE over SOCKET to HOST/PORT (or connected peer).
  ;; Returns T on success, NIL on failure (non-fatal, logs warning).
  (handler-case
      (let* ((text (encode-net-message message))
             (octets (string-to-octets text))
             (size (length octets)))
        (when (> size *net-buffer-size*)
          (warn "Dropping UDP payload (~d bytes) over buffer size ~d" size *net-buffer-size*)
          (log-verbose "UDP payload too large: ~d bytes (limit ~d)" size *net-buffer-size*)
          (return-from send-net-message nil))
        (usocket:socket-send socket octets size :host host :port port)
        t)
    (error (e)
      (log-verbose "Failed to send message to ~a:~d: ~a" host port e)
      nil)))

(defun get-nproc ()
  ;; Return number of CPU cores, or 1 if unable to determine.
  #+sbcl
  (or (ignore-errors
       (parse-integer
        (with-output-to-string (s)
          (sb-ext:run-program "nproc" nil :output s :search t))
        :junk-allowed t))
      1)
  #-sbcl
  1)

(defun host-to-string (host)
  ;; Convert a host (byte vector or string) to string.
  (if (stringp host)
      host
      (format nil "~{~d~^.~}" (coerce host 'list))))

(defun receive-net-message (socket buffer)
  ;; Receive a single UDP message if ready; returns message and sender.
  (when (usocket:wait-for-input socket :timeout 0 :ready-only t)
    (multiple-value-bind (recv size host port)
        (usocket:socket-receive socket buffer (length buffer))
      (declare (ignore recv))
      (when (and size (> size 0))
        (let* ((text (octets-to-string buffer size))
               (message (decode-net-message text)))
          (unless message
            (log-verbose "Dropped malformed packet from ~a:~d (~d bytes)"
                         (host-to-string host)
                         port
                         size))
          (values message (host-to-string host) port))))))

(defstruct (net-client (:constructor %make-net-client))
  ;; Server-side view of a connected client.
  host port player intent last-heard
  authenticated-p       ; T after successful login, NIL before
  account-username)     ; Username of logged-in account

(defparameter *active-sessions* (make-hash-table :test 'equal)
  "Map of username (lowercase) -> net-client for logged-in accounts.")

(defparameter *client-timeout-seconds* 30.0
  "Seconds of inactivity before a client is considered disconnected.")

(defun make-net-client (host port player)
  ;; Build a client record with an intent seeded to PLAYER position.
  ;; If player is nil (unauthenticated), creates a default intent at origin.
  (%make-net-client :host host
                    :port port
                    :player player
                    :intent (if player
                                (make-intent :target-x (player-x player)
                                             :target-y (player-y player))
                                (make-intent :target-x 0 :target-y 0))
                    :last-heard 0.0
                    :authenticated-p nil
                    :account-username nil))

(defun find-net-client (clients host port)
  ;; Return the matching CLIENT for HOST/PORT, if any.
  (loop :for client :in clients
        :when (and (string= host (net-client-host client))
                   (= port (net-client-port client)))
          :do (return client)))

(defun client-uses-player-p (clients player)
  ;; Return true when PLAYER is already assigned to a CLIENT.
  (loop :for client :in clients
        :when (eq (net-client-player client) player)
          :do (return t)))

(defun find-unassigned-player (game clients)
  ;; Return a player without a client assignment, if any.
  (let ((players (game-players game)))
    (when players
      (loop :for player :across players
            :unless (client-uses-player-p clients player)
              :do (return player)))))

(defun reset-player-for-client (player)
  ;; Clear transient state when binding a player to a client.
  (when player
    (let ((intent (player-intent player)))
      (when intent
        (reset-frame-intent intent)
        (clear-intent-target intent)))
    (clear-player-auto-walk player)
    (setf (player-attacking player) nil
          (player-attack-hit player) nil
          (player-hit-active player) nil
          (player-attack-target-id player) 0
          (player-follow-target-id player) 0)))

(defun add-player-to-game (game player)
  ;; Append PLAYER to GAME and refresh entity list.
  (let* ((players (game-players game))
         (count (if players (length players) 0))
         (new-players (make-array (1+ count))))
    (when players
      (replace new-players players))
    (setf (aref new-players count) player)
    (setf (game-players game) new-players
          (game-entities game)
          (make-entities new-players (game-npcs game))))
  player)

(defun remove-player-from-game (game player)
  "Remove PLAYER from GAME and refresh entity list."
  (let* ((players (game-players game))
         (filtered (remove player players :test #'eq)))
    (when (< (length filtered) (length players))
      ;; Player was found and removed
      (setf (game-players game) filtered
            (game-entities game)
            (make-entities filtered (game-npcs game)))
      t)))

(defun register-net-client (game clients host port &key (timestamp 0.0))
  ;; Ensure a client exists for HOST/PORT and return updated list.
  ;; Note: Does NOT spawn a player - players are spawned during authentication
  (declare (ignore game))
  (let ((client (find-net-client clients host port)))
    (if client
        (progn
          (setf (net-client-last-heard client) timestamp)
          (values client clients nil))
        (let ((client (make-net-client host port nil)))
          (log-verbose "New client registered: ~a:~d (unauthenticated)" host port)
          (setf (net-client-last-heard client) timestamp)
          (values client (cons client clients) t)))))

(defun apply-client-intents (clients)
  ;; Copy each client intent into its assigned player intent.
  (dolist (client clients)
    (let* ((player (net-client-player client))
           (server-intent (and player (player-intent player)))
           (client-intent (net-client-intent client)))
      (apply-client-intent server-intent client-intent)
      (when (and client-intent
                 (intent-requested-chat-message client-intent))
        (clear-requested-chat-message client-intent)))))

(defun reconcile-net-clients (game clients)
  ;; Rebind clients to players that exist in the current game state.
  ;; Only reconciles authenticated clients; unauthenticated clients stay as nil.
  (let ((players (game-players game)))
    (dolist (client clients)
      (when (net-client-authenticated-p client)
        (let* ((current (net-client-player client))
               (current-id (and current (player-id current)))
               (matching (and current-id (find-player-by-id players current-id))))
          (cond
            ((and matching (not (eq matching current)))
             (setf (net-client-player client) matching))
            ((null matching)
             (let ((assigned (or (find-unassigned-player game clients)
                                 (add-player-to-game
                                  game
                                  (spawn-player-at-world (game-world game)
                                                         (game-id-source game))))))
               (reset-player-for-client assigned)
               (setf (net-client-player client) assigned))))))))
  clients)

(defun intent->plist (intent)
  ;; Convert INTENT into a plist suitable for network transport.
  (when intent
    (list :move-dx (intent-move-dx intent)
          :move-dy (intent-move-dy intent)
          :face-dx (intent-face-dx intent)
          :face-dy (intent-face-dy intent)
          :target-x (intent-target-x intent)
          :target-y (intent-target-y intent)
          :target-active (intent-target-active intent)
          :attack (intent-attack intent)
          :run-toggle (intent-run-toggle intent)
          :requested-attack-target-id (intent-requested-attack-target-id intent)
          :requested-follow-target-id (intent-requested-follow-target-id intent)
          :requested-pickup-target-id (intent-requested-pickup-target-id intent)
          :requested-pickup-tx (intent-requested-pickup-tx intent)
          :requested-pickup-ty (intent-requested-pickup-ty intent)
          :requested-chat-message (intent-requested-chat-message intent))))

(defun %float-or (value default)
  (if (numberp value)
      (float value 1.0)
      (float default 1.0)))

(defun apply-intent-plist (intent plist)
  ;; Apply PLIST values to INTENT in place.
  (when (and intent plist)
    (setf (intent-move-dx intent) (%float-or (getf plist :move-dx) 0.0)
          (intent-move-dy intent) (%float-or (getf plist :move-dy) 0.0)
          (intent-face-dx intent) (%float-or (getf plist :face-dx) 0.0)
          (intent-face-dy intent) (%float-or (getf plist :face-dy) 0.0)
          (intent-target-x intent) (%float-or (getf plist :target-x) 0.0)
          (intent-target-y intent) (%float-or (getf plist :target-y) 0.0)
          (intent-target-active intent) (getf plist :target-active nil)
          (intent-attack intent) (getf plist :attack nil)
          (intent-run-toggle intent) (getf plist :run-toggle nil)
          (intent-requested-attack-target-id intent)
          (getf plist :requested-attack-target-id 0)
          (intent-requested-follow-target-id intent)
          (getf plist :requested-follow-target-id 0)
          (intent-requested-pickup-target-id intent)
          (getf plist :requested-pickup-target-id nil)
          (intent-requested-pickup-tx intent)
          (getf plist :requested-pickup-tx nil)
          (intent-requested-pickup-ty intent)
          (getf plist :requested-pickup-ty nil)
          (intent-requested-chat-message intent)
          (getf plist :requested-chat-message nil)))
  intent)

(defun combat-event->plist (event)
  ;; Convert a combat EVENT to a serializable plist.
  (when event
    (list :type (combat-event-type event)
          :text (combat-event-text event))))

(defun plist->combat-event (plist)
  ;; Convert a plist back into a combat event.
  (when plist
    (make-combat-event :type (getf plist :type)
                       :text (getf plist :text))))

(defun send-snapshots-parallel (socket clients state event-plists worker-threads)
  ;; Send snapshots to clients using WORKER-THREADS parallel threads.
  ;; Falls back to serial sending if worker-threads <= 1.
  ;; Only sends to authenticated clients with a player.
  (let ((authenticated-clients (remove-if-not
                                (lambda (c)
                                  (and (net-client-authenticated-p c)
                                       (net-client-player c)))
                                clients)))
    (if (<= worker-threads 1)
        ;; Serial: Send to each client in order
        (dolist (client authenticated-clients)
          (send-net-message socket
                            (list :type :snapshot
                                  :state state
                                  :events event-plists
                                  :player-id (player-id (net-client-player client)))
                            :host (net-client-host client)
                            :port (net-client-port client)))
        ;; Parallel: Distribute clients across worker threads with socket synchronization
        #+sbcl
        (let* ((client-count (length authenticated-clients))
               (chunk-size (ceiling client-count worker-threads))
               (socket-lock (sb-thread:make-mutex :name "snapshot-socket-lock"))
               (threads nil))
          (loop :for start :from 0 :below client-count :by chunk-size
                :do (let ((end (min (+ start chunk-size) client-count)))
                      (push
                       (sb-thread:make-thread
                        (lambda ()
                          (loop :for i :from start :below end
                                :for client = (nth i authenticated-clients)
                                ;; Synchronize socket access across threads
                                :do (sb-thread:with-mutex (socket-lock)
                                      (send-net-message socket
                                                        (list :type :snapshot
                                                              :state state
                                                              :events event-plists
                                                              :player-id (player-id
                                                                          (net-client-player client)))
                                                        :host (net-client-host client)
                                                        :port (net-client-port client)))))
                        :name (format nil "snapshot-sender-~d" start))
                       threads)))
          ;; Wait for all threads to complete
          (dolist (thread threads)
            (sb-thread:join-thread thread)))
        #-sbcl
        ;; Non-SBCL: Fall back to serial
        (dolist (client authenticated-clients)
          (send-net-message socket
                            (list :type :snapshot
                                  :state state
                                  :events event-plists
                                  :player-id (player-id (net-client-player client)))
                            :host (net-client-host client)
                            :port (net-client-port client))))))

(defun apply-snapshot (game state event-plists &key player-id)
  ;; Apply a snapshot state and queue HUD/combat events for UI.
  ;; Returns zone-id on success, NIL on failure (non-fatal).
  (handler-case
      (progn
        (when player-id
          (setf (game-net-player-id game) player-id))
        (multiple-value-bind (zone-id zone-changed)
            (apply-game-state game state :apply-zone t)
          (when zone-changed
            (log-verbose "Client zone transitioned to ~a" zone-id)
            (handle-zone-transition game))
          (let ((queue (game-combat-events game)))
            (dolist (event-plist event-plists)
              (let ((event (plist->combat-event event-plist)))
                (when event
                  (push-combat-event queue event)))))
          (let ((player (game-player game)))
            (when player
              (mark-player-hud-stats-dirty player)
              (mark-player-inventory-dirty player)))
          zone-id))
    (error (e)
      (warn "Failed to apply snapshot: ~a" e)
      (log-verbose "Snapshot application error: ~a" e)
      nil)))

(defun send-intent-message (socket intent &key host port)
  ;; Send the current INTENT as a UDP message.
  (send-net-message socket
                    (list :type :intent :payload (intent->plist intent))
                    :host host
                    :port port))

(defun handle-server-load (game)
  ;; Load game state on the server and reset transient flags.
  (let* ((event-queue (game-combat-events game))
         (players (game-players game))
         (player (game-player game))
         (world (game-world game))
         (zone-id (load-game game *save-filepath*)))
    (if zone-id
        (progn
          (when players
            (loop :for current-player :across players
                  :do (let ((server-intent (player-intent current-player)))
                        (when server-intent
                          (reset-frame-intent server-intent)
                          (clear-intent-target server-intent)))
                      (clear-player-auto-walk current-player)
                      (setf (player-attacking current-player) nil
                            (player-attack-hit current-player) nil
                            (player-hit-active current-player) nil)
                      (mark-player-hud-stats-dirty current-player)
                      (mark-player-inventory-dirty current-player)))
          (when world
            (setf (world-minimap-spawns world)
                  (build-adjacent-minimap-spawns world player))
            (setf (world-minimap-collisions world)
                  (build-minimap-collisions world)))
          (emit-hud-message-event event-queue
                                  (format nil "Game loaded (~a)." zone-id)))
        (emit-hud-message-event event-queue "Load failed."))
    zone-id))

;;;; Account Authentication Handlers

(defun handle-register-request (client host port message socket game)
  "Handle account registration request."
  (let ((username (getf message :username))
        (password (getf message :password)))
    (cond
      ((not (and username password))
       (send-net-message socket
                        (list :type :auth-fail :reason :missing-credentials)
                        :host host :port port)
       (log-verbose "Registration failed from ~a:~d - missing credentials" host port))
      ((db-create-account username password)
       ;; Account created successfully - spawn new character
       (let* ((world (game-world game))
              (player (spawn-player-at-world world (game-id-source game)))
              (player-id (player-id player)))
         ;; Link account to new character
         (db-set-character-id username player-id)
         ;; Add player to game
         (add-player-to-game game player)
         ;; Mark client as authenticated
         (setf (net-client-player client) player)
         (setf (net-client-authenticated-p client) t)
         (setf (net-client-account-username client) (string-downcase username))
         ;; Register session for persistence
         (let* ((zone (world-zone world))
                (zone-id (and zone (zone-id zone))))
           (register-player-session player :zone-id zone-id))
         ;; Track active session
         (setf (gethash (string-downcase username) *active-sessions*) client)
         ;; Send success response
         (send-net-message socket
                          (list :type :auth-ok :player-id player-id)
                          :host host :port port)
         (log-verbose "Registration successful: ~a (~a:~d) -> player-id=~d"
                     username host port player-id)))
      (t
       ;; Username already exists
       (send-net-message socket
                        (list :type :auth-fail :reason :username-taken)
                        :host host :port port)
       (log-verbose "Registration failed from ~a:~d - username ~a already taken"
                   host port username)))))

(defun handle-login-request (client host port message socket game clients elapsed)
  "Handle account login request."
  (let ((username (getf message :username))
        (password (getf message :password)))
    (cond
      ((not (and username password))
       (send-net-message socket
                        (list :type :auth-fail :reason :missing-credentials)
                        :host host :port port)
       (log-verbose "Login failed from ~a:~d - missing credentials" host port))
      ;; Check if already logged in
      ((gethash (string-downcase username) *active-sessions*)
       (send-net-message socket
                        (list :type :auth-fail :reason :already-logged-in)
                        :host host :port port)
       (log-verbose "Login failed from ~a:~d - account ~a already logged in"
                   host port username))
      ;; Verify credentials
      ((not (db-verify-credentials username password))
       (send-net-message socket
                        (list :type :auth-fail :reason :bad-credentials)
                        :host host :port port)
       (log-verbose "Login failed from ~a:~d - bad credentials for ~a"
                   host port username))
      (t
       ;; Login successful
       (let* ((character-id (db-get-character-id username))
              (world (game-world game))
              (zone (world-zone world))
              (zone-id (and zone (zone-id zone)))
              (player nil))
         (cond
           ;; Existing character - load from DB
           (character-id
            (setf player (db-load-player character-id))
            (when player
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
                          character-id username)))
           ;; No character yet - spawn new one (shouldn't happen after registration)
           (t
            (setf player (spawn-player-at-world world (game-id-source game)))
            (db-set-character-id username (player-id player))
            (add-player-to-game game player)
            (log-verbose "Created new character ~d for account ~a"
                        (player-id player) username)))

         (when player
           ;; Mark client as authenticated
           (setf (net-client-player client) player)
           (setf (net-client-authenticated-p client) t)
           (setf (net-client-account-username client) (string-downcase username))
           (setf (net-client-last-heard client) elapsed)
           ;; Register session for persistence
           (register-player-session player :zone-id zone-id)
           ;; Track active session
           (setf (gethash (string-downcase username) *active-sessions*) client)
           ;; Send success response
           (send-net-message socket
                            (list :type :auth-ok :player-id (player-id player))
                            :host host :port port)
           (log-verbose "Login successful: ~a (~a:~d) -> player-id=~d"
                       username host port (player-id player))))))))

(defun handle-logout-request (client host port game)
  "Handle logout request from authenticated client."
  (when (and client (net-client-authenticated-p client))
    (let ((username (net-client-account-username client))
          (player (net-client-player client)))
      (when username
        ;; Remove from active sessions
        (remhash username *active-sessions*)
        (log-verbose "Logout: ~a (~a:~d)" username host port))
      (when player
        ;; Save and unregister session
        (db-logout-player player)
        ;; Remove player from game world
        (remove-player-from-game game player))
      ;; Clear authentication
      (setf (net-client-authenticated-p client) nil)
      (setf (net-client-account-username client) nil))))

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
              (when username
                (remhash username *active-sessions*)
                (log-verbose "Client timeout: ~a (inactive for ~,1fs)"
                            username inactive-time))
              (when player
                (db-logout-player player)
                ;; Remove player from game world
                (remove-player-from-game game player))
              ;; Don't add to updated list - remove this client
              )
            ;; Client still active - keep it
            (push client updated-clients))))
    (nreverse updated-clients)))

(defun run-server (&key (host *net-default-host*)
                        (port *net-default-port*)
                        (max-seconds 0.0)
                        (max-frames 0)
                        (worker-threads 1))
  ;; Run a UDP server that simulates the game and streams snapshots.
  ;; Scaling: This runs ONE zone. For 10k users @ 500/zone, run 20 server processes.
  ;; See SERVER_PERFORMANCE.md for horizontal scaling strategy.
  ;;
  ;; WORKER-THREADS: Number of threads for parallel snapshot sending (default 1).
  ;;   - 1 = serial sending (simple, default)
  ;;   - N = parallel sending across N threads (for high client counts)
  ;;   - Recommended: (get-nproc) on multi-core machines
  ;;   - Safe: Only parallelizes network I/O, simulation remains serial
  (with-fatal-error-log ((format nil "Server runtime (~a:~d)" host port))
    ;; Initialize storage backend from environment variables:
    ;; MMORPG_DB_BACKEND: "memory" or "redis" (default: memory)
    ;; MMORPG_REDIS_HOST: Redis host (default: 127.0.0.1)
    ;; MMORPG_REDIS_PORT: Redis port (default: 6379)
    (let* ((backend-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_DB_BACKEND")
                            #-sbcl (uiop:getenv "MMORPG_DB_BACKEND")
                            "memory"))
           (backend (cond
                      ((string-equal backend-str "redis") :redis)
                      ((string-equal backend-str "memory") :memory)
                      (t (progn
                           (warn "Unknown MMORPG_DB_BACKEND=~a, defaulting to memory" backend-str)
                           :memory))))
           (redis-host (or #+sbcl (sb-ext:posix-getenv "MMORPG_REDIS_HOST")
                           #-sbcl (uiop:getenv "MMORPG_REDIS_HOST")
                           "127.0.0.1"))
           (redis-port-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_REDIS_PORT")
                               #-sbcl (uiop:getenv "MMORPG_REDIS_PORT")
                               "6379"))
           (redis-port (or (ignore-errors (parse-integer redis-port-str)) 6379)))
      (init-storage :backend backend :host redis-host :port redis-port))
    (let* ((socket (usocket:socket-connect nil nil
                                          :protocol :datagram
                                          :local-host host
                                          :local-port port))
           (recv-buffer (make-net-buffer))
           (game (make-server-game))
           (clients nil)
           (stop-flag nil)
           (stop-reason nil)
           (last-flush-time 0.0))
      (format t "~&SERVER: listening on ~a:~d (worker-threads=~d)~%" host port worker-threads)
      (log-verbose "Server config: tick=~,3fs buffer=~d workers=~d"
                   *sim-tick-seconds* *net-buffer-size* worker-threads)
      (finish-output)
      (unwind-protect
           (handler-case
               (loop :with elapsed = 0.0
                     :with frames = 0
                     :with accumulator = 0.0
                     :until (or stop-flag
                                (and (> max-seconds 0.0)
                                     (>= elapsed max-seconds))
                                (and (> max-frames 0)
                                     (>= frames max-frames)))
                     :do ;; 1. Receive all pending intents (non-blocking UDP receive)
                         (loop
                           (multiple-value-bind (message host port)
                               (receive-net-message socket recv-buffer)
                             (unless message
                               (return))
                             (multiple-value-bind (client next-clients _new)
                                 (register-net-client game clients host port
                                                      :timestamp elapsed)
                               (declare (ignore _new))
                               (setf clients next-clients)
                               (case (getf message :type)
                                 (:hello
                                  (log-verbose "Handshake received from ~a:~d" host port))
                                 (:register
                                  (handle-register-request client host port message socket game))
                                 (:login
                                  (handle-login-request client host port message socket game
                                                       clients elapsed))
                                 (:logout
                                  (handle-logout-request client host port game))
                                 (:intent
                                  (when (and client (net-client-authenticated-p client))
                                    (apply-intent-plist
                                     (net-client-intent client)
                                     (getf message :payload))))
                                 (t
                                  (log-verbose "Unknown message type from ~a:~d -> ~s"
                                               host port (getf message :type)))))))
                         ;; 2. Apply client intents to player state (O(clients), cheap)
                         (apply-client-intents clients)
                         ;; 3. Run fixed-tick simulation (O(players * npcs), main bottleneck)
                         (let ((dt *sim-tick-seconds*))
                           (incf elapsed dt)
                           (incf frames)
                           (multiple-value-bind (new-acc transitions)
                               (server-step game nil dt accumulator)
                             (setf accumulator new-acc)
                             (dotimes (_ transitions)
                               (declare (ignore _)))))
                         ;; 3b. Periodic batch flush (tier-2 writes every ~30s)
                         (when (>= (- elapsed last-flush-time) *batch-flush-interval*)
                           (flush-dirty-players)
                           (setf last-flush-time elapsed))
                         ;; 3c. Check for timed-out clients (free sessions after 30s inactivity)
                         (setf clients (check-client-timeouts clients elapsed game))
                         ;; 4. Send snapshots to all clients (serialize once, send N times)
                         ;; OPTIMIZATION: Serialization happens once and is shared across clients.
                         ;; For 500 clients this is much better than 500 serializations.
                         ;; OPTIONAL: Use worker-threads > 1 to parallelize network sends.
                         ;; EXCEPTION HANDLING: Snapshot errors are non-fatal, skip frame and continue.
                         (when clients
                           (handler-case
                               (let* ((events (pop-combat-events (game-combat-events game)))
                                      (event-plists (mapcar #'combat-event->plist events))
                                      (state (serialize-game-state game :include-visuals t)))
                                 (send-snapshots-parallel socket clients state event-plists
                                                          worker-threads))
                             (error (e)
                               (warn "Failed to serialize/send snapshot (frame ~d): ~a" frames e)
                               (log-verbose "Snapshot error, skipping frame: ~a" e))))
                         (sleep *sim-tick-seconds*))
             #+sbcl
             (sb-sys:interactive-interrupt ()
               (setf stop-flag t stop-reason "interrupt")))
        (when stop-reason
          (format t "~&SERVER: shutdown requested (~a).~%" stop-reason)
          (finish-output))
        ;; Graceful shutdown: flush all dirty players and close storage
        (db-shutdown-flush)
        (usocket:socket-close socket)))))

(defun run-client (&key (host *net-default-host*)
                        (port *net-default-port*)
                        (max-seconds 0.0)
                        (max-frames 0)
                        (auto-login-username nil)
                        (auto-login-password nil))
  ;; Run a client that connects to the UDP server and renders snapshots.
  ;; If auto-login credentials are provided, attempts register first, then login on failure.
  (with-fatal-error-log ((format nil "Client runtime (~a:~d)" host port))
    (log-verbose "Client starting: connecting to ~a:~d" host port)
    (raylib:with-window ("Hello MMO" (*window-width* *window-height*))
      (raylib:set-target-fps 60)
      (raylib:set-exit-key 0)
      (raylib:init-audio-device)
      (let* ((socket (usocket:socket-connect host port :protocol :datagram))
             (recv-buffer (make-net-buffer))
             (game (make-game))
             (ui (game-ui game)))
        (setf (game-net-role game) :client)
        (unwind-protect
             (loop :with elapsed = 0.0
                   :with frames = 0
                   :with auto-login-attempted = nil
                   :with auto-login-register-failed = nil
                   :until (or (raylib:window-should-close)
                              (ui-exit-requested ui)
                              (and (> max-seconds 0.0)
                                   (>= elapsed max-seconds))
                              (and (> max-frames 0)
                                   (>= frames max-frames)))
                   :do (let ((dt (raylib:get-frame-time)))
                         (incf elapsed dt)
                         (incf frames)

                         ;; Login screen phase
                         (cond
                           ((and (ui-login-active ui) (not (ui-auth-complete ui)))
                            ;; Auto-login: Try register first, then login if taken
                            (when (and auto-login-username auto-login-password
                                      (not auto-login-attempted))
                              (send-net-message socket
                                               (list :type :register
                                                     :username auto-login-username
                                                     :password auto-login-password))
                              (setf auto-login-attempted t)
                              (log-verbose "Auto-login: attempting register for ~a" auto-login-username))
                            ;; Update login input
                            (update-login-input ui)
                            ;; Check for login messages from server
                            (loop
                              (multiple-value-bind (message _host _port)
                                  (receive-net-message socket recv-buffer)
                                (declare (ignore _host _port))
                                (unless message
                                  (return))
                                (case (getf message :type)
                                  (:auth-ok
                                   (setf (ui-auth-complete ui) t)
                                   (setf (ui-login-active ui) nil)
                                   (log-verbose "Authentication successful"))
                                  (:auth-fail
                                   (let ((reason (getf message :reason)))
                                     ;; Auto-login: if register failed because username taken, try login
                                     (when (and auto-login-username auto-login-password
                                               auto-login-attempted
                                               (not auto-login-register-failed)
                                               (eq reason :username-taken))
                                       (send-net-message socket
                                                        (list :type :login
                                                              :username auto-login-username
                                                              :password auto-login-password))
                                       (setf auto-login-register-failed t)
                                       (log-verbose "Auto-login: register failed, trying login for ~a"
                                                   auto-login-username))
                                     (setf (ui-auth-error-message ui)
                                           (case reason
                                             (:bad-credentials "Invalid username or password")
                                             (:username-taken "Username already taken")
                                             (:already-logged-in "Account already logged in")
                                             (:missing-credentials "Missing username or password")
                                             (t "Authentication failed")))
                                     (log-verbose "Authentication failed: ~a" reason)))
                                  (t
                                   (log-verbose "Unexpected message during login: ~s"
                                               (getf message :type))))))
                            ;; Draw login screen and handle button clicks
                            (raylib:begin-drawing)
                            (let ((action (draw-login-screen ui)))
                              (when action
                                (let ((username (ui-username-buffer ui)))
                                  (when (and username (plusp (length username)))
                                    ;; For MVP: password = username
                                    (case action
                                      (:login
                                       (send-net-message socket
                                                        (list :type :login
                                                              :username username
                                                              :password username))
                                       (setf (ui-auth-error-message ui) nil)
                                       (log-verbose "Sending login request for ~a" username))
                                      (:register
                                       (send-net-message socket
                                                        (list :type :register
                                                              :username username
                                                              :password username))
                                       (setf (ui-auth-error-message ui) nil)
                                       (log-verbose "Sending register request for ~a" username)))))))
                            (raylib:end-drawing))

                           ;; Gameplay phase (after authentication)
                           (t
                            (update-client-input game dt)
                            (dolist (request (drain-net-requests game))
                              (send-net-message socket request))
                            (send-intent-message socket (game-client-intent game))
                            (clear-requested-chat-message (game-client-intent game))
                            (let ((latest-state nil)
                                  (latest-events nil)
                                  (latest-player-id nil))
                              (loop
                                (multiple-value-bind (message _host _port)
                                    (receive-net-message socket recv-buffer)
                                  (declare (ignore _host _port))
                                  (unless message
                                    (return))
                                  (case (getf message :type)
                                    (:snapshot
                                     (setf latest-state (getf message :state)
                                           latest-player-id (or (getf message :player-id)
                                                                latest-player-id))
                                     (dolist (event (getf message :events))
                                       (push event latest-events)))
                                    (t
                                     (log-verbose "Unknown message type from server: ~s"
                                                  (getf message :type)))))
                              (when latest-state
                                (apply-snapshot game latest-state (nreverse latest-events)
                                                :player-id latest-player-id)))
                            (process-combat-events game)
                            (draw-game game))))))
          (shutdown-game game)
          (usocket:socket-close socket)
          (raylib:close-audio-device))))))
