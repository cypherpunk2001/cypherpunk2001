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
  host port player intent last-heard)

(defun make-net-client (host port player)
  ;; Build a client record with an intent seeded to PLAYER position.
  (%make-net-client :host host
                    :port port
                    :player player
                    :intent (make-intent :target-x (player-x player)
                                         :target-y (player-y player))
                    :last-heard 0.0))

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

(defun register-net-client (game clients host port &key (timestamp 0.0))
  ;; Ensure a client exists for HOST/PORT and return updated list.
  (let ((client (find-net-client clients host port)))
    (if client
        (progn
          (setf (net-client-last-heard client) timestamp)
          (values client clients nil))
        (let* ((player (or (find-unassigned-player game clients)
                           (add-player-to-game
                            game
                            (spawn-player-at-world (game-world game)
                                                   (game-id-source game)))))
               (client (make-net-client host port player)))
          (log-verbose "New client registered: ~a:~d -> player-id=~d"
                       host port (player-id player))
          (reset-player-for-client player)
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
  (let ((players (game-players game)))
    (dolist (client clients)
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
             (setf (net-client-player client) assigned)))))))
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
  (if (<= worker-threads 1)
      ;; Serial: Send to each client in order
      (dolist (client clients)
        (send-net-message socket
                          (list :type :snapshot
                                :state state
                                :events event-plists
                                :player-id (player-id (net-client-player client)))
                          :host (net-client-host client)
                          :port (net-client-port client)))
      ;; Parallel: Distribute clients across worker threads
      #+sbcl
      (let* ((client-count (length clients))
             (chunk-size (ceiling client-count worker-threads))
             (threads nil))
        (loop :for start :from 0 :below client-count :by chunk-size
              :do (let ((end (min (+ start chunk-size) client-count)))
                    (push
                     (sb-thread:make-thread
                      (lambda ()
                        (loop :for i :from start :below end
                              :for client = (nth i clients)
                              :do (send-net-message socket
                                                    (list :type :snapshot
                                                          :state state
                                                          :events event-plists
                                                          :player-id (player-id
                                                                      (net-client-player client)))
                                                    :host (net-client-host client)
                                                    :port (net-client-port client))))
                      :name (format nil "snapshot-sender-~d" start))
                     threads)))
        ;; Wait for all threads to complete
        (dolist (thread threads)
          (sb-thread:join-thread thread)))
      #-sbcl
      ;; Non-SBCL: Fall back to serial
      (dolist (client clients)
        (send-net-message socket
                          (list :type :snapshot
                                :state state
                                :events event-plists
                                :player-id (player-id (net-client-player client)))
                          :host (net-client-host client)
                          :port (net-client-port client)))))

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
    ;; Initialize storage backend (memory-storage for now, redis-storage later)
    (init-storage :backend :memory)
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
                                 (:intent
                                  (when client
                                    (apply-intent-plist
                                     (net-client-intent client)
                                     (getf message :payload))))
                                 (:save
                                  (log-verbose "Save requested by ~a:~d" host port)
                                  (when (save-game game *save-filepath*)
                                    (emit-hud-message-event (game-combat-events game)
                                                            "Game saved.")))
                                 (:load
                                  (log-verbose "Load requested by ~a:~d" host port)
                                  (handle-server-load game)
                                  (setf clients
                                        (reconcile-net-clients game clients)))
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
                        (max-frames 0))
  ;; Run a client that connects to the UDP server and renders snapshots.
  (with-fatal-error-log ((format nil "Client runtime (~a:~d)" host port))
    (log-verbose "Client starting: connecting to ~a:~d" host port)
    (raylib:with-window ("Hello MMO" (*window-width* *window-height*))
      (raylib:set-target-fps 60)
      (raylib:set-exit-key 0)
      (raylib:init-audio-device)
      (let* ((socket (usocket:socket-connect host port :protocol :datagram))
             (recv-buffer (make-net-buffer))
             (game (make-game)))
        (setf (game-net-role game) :client)
        (send-net-message socket (list :type :hello))
        (unwind-protect
             (loop :with elapsed = 0.0
                   :with frames = 0
                   :until (or (raylib:window-should-close)
                              (ui-exit-requested (game-ui game))
                              (and (> max-seconds 0.0)
                                   (>= elapsed max-seconds))
                              (and (> max-frames 0)
                                   (>= frames max-frames)))
                   :do (let ((dt (raylib:get-frame-time)))
                         (incf elapsed dt)
                         (incf frames)
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
                                             :player-id latest-player-id))))
                         (process-combat-events game)
                         (draw-game game)))
          (shutdown-game game)
          (usocket:socket-close socket)
          (raylib:close-audio-device))))))
