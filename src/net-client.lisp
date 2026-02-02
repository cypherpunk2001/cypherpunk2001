;; NOTE: If you change behavior here, update docs/net-client.md :)
(in-package #:mmorpg)

;;; net-client.lisp - Client networking loop, reconnect, client handlers
;;;
;;; Contains `run-client`: the main client entry point that connects to
;;; the UDP server, handles login UI, sends intents, receives snapshots,
;;; and renders the game.

(defun run-client (&key (host *net-default-host*)
                        (port *net-default-port*)
                        (max-seconds 0.0)
                        (max-frames 0)
                        (auto-login-username nil)
                        (auto-login-password nil))
  ;; Run a client that connects to the UDP server and renders snapshots.
  ;; If auto-login credentials are provided, attempts register first, then login on failure.
  (with-fatal-error-log ((format nil "Client runtime (~a:~d)" host port))
    (validate-zone-config)
    (log-verbose "Client starting: connecting to ~a:~d" host port)
    ;; Set window flags before init (must be called before with-window)
    (when *window-resize-enabled*
      (raylib:set-config-flags +flag-window-resizable+))
    (raylib:with-window ("Hello MMO" (*window-width* *window-height*))
      (raylib:set-target-fps *client-target-fps*)
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
                   :with busy-retry-count = 0       ; Step 8: server-busy retry counter
                   :with busy-retry-time = 0.0      ; Step 8: when to retry after :server-busy
                   :with busy-retry-type = nil       ; :register or :login
                   :with busy-retry-username = nil
                   :with busy-retry-password = nil
                   :with last-auth-type = nil        ; Tracks last sent auth type for busy retry
                   :with last-snapshot-seq = nil  ; Delta compression: last received seq
                   :with chunk-buf = (make-chunk-buffer)  ; UDP fragmentation: reassembly buffer
                   :until (or (raylib:window-should-close)
                              (ui-exit-requested ui)
                              (and (> max-seconds 0.0)
                                   (>= elapsed max-seconds))
                              (and (> max-frames 0)
                                   (>= frames max-frames)))
                   :do (let ((dt (raylib:get-frame-time)))
                         (incf elapsed dt)
                         (incf frames)

                         ;; Window resize handling (when enabled)
                         (handle-window-resize game)

                         ;; Login screen phase
                         (cond
                           ((and (ui-login-active ui) (not (ui-auth-complete ui)))
                            ;; Periodic ping to detect server availability and measure RTT
                            (when (>= elapsed (ui-server-next-ping ui))
                              (handler-case
                                  (progn
                                    (setf (ui-ping-send-time ui) elapsed)
                                    (send-net-message socket (list :type :hello)))
                                (error ()
                                  ;; Send failed - server unreachable
                                  (setf (ui-server-status ui) :offline)))
                              ;; Schedule next ping: random 3-7s
                              (setf (ui-server-next-ping ui)
                                    (+ elapsed (+ 3.0 (random 4.0)))))
                            ;; Auto-login: Try register first, then login if taken
                            (when (and auto-login-username auto-login-password
                                      (not auto-login-attempted))
                              (handler-case
                                  (progn
                                    (send-auth-message socket :register
                                                       auto-login-username auto-login-password)
                                    (setf auto-login-attempted t
                                          last-auth-type :register)
                                    (log-verbose "Auto-login: attempting register for ~a" auto-login-username))
                                (error ()
                                  (setf (ui-server-status ui) :offline))))
                            ;; Step 8: Retry auth after :server-busy with exponential backoff
                            (when (and busy-retry-type
                                      (> busy-retry-time 0.0)
                                      (>= elapsed busy-retry-time)
                                      (<= busy-retry-count 3))
                              (handler-case
                                  (progn
                                    (send-auth-message socket busy-retry-type
                                                       busy-retry-username busy-retry-password)
                                    (log-verbose "Retrying ~a after server-busy (attempt ~d)"
                                                busy-retry-type busy-retry-count))
                                (error ()
                                  (setf (ui-server-status ui) :offline)))
                              (setf busy-retry-time 0.0)) ; Clear so we don't resend
                            ;; Update login input
                            (update-login-input ui)
                            ;; F11 toggles fullscreen on login screen
                            (when (raylib:is-key-pressed +key-f11+)
                              (raylib:toggle-fullscreen))
                            ;; Enter key triggers login (only if server online)
                            (when (and (raylib:is-key-pressed +key-enter+)
                                      (ui-username-buffer ui)
                                      (plusp (length (ui-username-buffer ui))))
                              (if (eq (ui-server-status ui) :offline)
                                  (setf (ui-auth-error-message ui) "Server is offline")
                                  (handler-case
                                      (progn
                                        (send-auth-message socket :login
                                                           (ui-username-buffer ui)
                                                           (ui-username-buffer ui))
                                        (setf (ui-auth-error-message ui) nil
                                              last-auth-type :login)
                                        (log-verbose "Sending login request for ~a (Enter key)" (ui-username-buffer ui)))
                                    (error ()
                                      (setf (ui-server-status ui) :offline
                                            (ui-auth-error-message ui) "Server is offline")))))
                            ;; Check for login messages from server
                            (let ((got-message nil))
                              (loop
                                (multiple-value-bind (message _host _port)
                                    (receive-net-message socket recv-buffer)
                                  (declare (ignore _host _port))
                                  (unless message
                                    (return))
                                  (setf got-message t)
                                  (case (getf message :type)
                                  (:auth-ok
                                   ;; Store player-id from auth-ok (used to find local player in snapshots)
                                   ;; Critical since network-only snapshots don't include per-client player-id
                                   (let ((player-id (getf message :player-id)))
                                     (when player-id
                                       (setf (game-net-player-id game) player-id)
                                       (log-verbose "Assigned player ID: ~d" player-id)))
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
                                       (send-auth-message socket :login
                                                          auto-login-username auto-login-password)
                                       (setf auto-login-register-failed t
                                             last-auth-type :login)
                                       (log-verbose "Auto-login: register failed, trying login for ~a"
                                                   auto-login-username))
                                     ;; Step 8: Handle :server-busy with exponential backoff retry
                                     (when (eq reason :server-busy)
                                       (if (< busy-retry-count 3)
                                           (let* ((delay (expt 2.0 busy-retry-count)) ; 1s, 2s, 4s
                                                  ;; Re-derive credentials for retry
                                                  ;; MVP: password = username (ui-password-buffer unused)
                                                  (uname (or auto-login-username
                                                             (ui-username-buffer ui)))
                                                  (pwd (or auto-login-password
                                                           (ui-username-buffer ui))))
                                             (setf busy-retry-time (+ elapsed delay))
                                             (incf busy-retry-count)
                                             ;; Use last-auth-type to retry the same action
                                             (setf busy-retry-type (or last-auth-type :login))
                                             (setf busy-retry-username uname
                                                   busy-retry-password pwd)
                                             (setf (ui-auth-error-message ui)
                                                   (format nil "Server busy, retrying (~d/3)..." busy-retry-count))
                                             (log-verbose "Server busy, retry ~d in ~,1fs" busy-retry-count delay))
                                           ;; Max retries exceeded
                                           (setf (ui-auth-error-message ui)
                                                 "Server is too busy. Please try again later.")))
                                     (unless (eq reason :server-busy)
                                       (setf (ui-auth-error-message ui)
                                             (case reason
                                               (:bad-credentials "Invalid username or password")
                                               (:username-taken "Username already taken")
                                               (:already-logged-in "Account already logged in")
                                               (:missing-credentials "Missing username or password")
                                               (:request-expired "Request timed out, please try again")
                                               (:wrong-zone (let ((zone-id (getf message :zone-id)))
                                                              (if zone-id
                                                                  (format nil "Wrong server for this character (zone: ~a)" zone-id)
                                                                  "Wrong server for this character")))
                                               (t "Authentication failed"))))
                                     (log-verbose "Authentication failed: ~a" reason)))
                                  (:private-state
                                   (apply-private-state game
                                                        (getf message :payload)
                                                        :player-id (getf message :player-id)))
                                  (:hello-ack
                                   ;; Server responded to ping - calculate RTT
                                   (let ((rtt-ms (round (* 1000.0 (- elapsed (ui-ping-send-time ui))))))
                                     (setf (ui-ping-rtt-ms ui) rtt-ms)
                                     (log-verbose "Server ping: ~dms" rtt-ms)))
                                  (t
                                   (log-verbose "Unexpected message during login: ~s"
                                               (getf message :type))))))
                              ;; Update server status based on whether we got a message
                              ;; Timeout (10s) must exceed max ping interval (7s) to avoid false offline
                              (if got-message
                                  (setf (ui-server-status ui) :online
                                        (ui-server-last-heard ui) elapsed)
                                  (when (> (- elapsed (ui-server-last-heard ui)) 10.0)
                                    (setf (ui-server-status ui) :offline))))
                            ;; Draw login screen and handle button clicks
                            (raylib:begin-drawing)
                            (let ((action (draw-login-screen ui)))
                              (when action
                                (let ((username (ui-username-buffer ui)))
                                  (when (and username (plusp (length username)))
                                    (if (eq (ui-server-status ui) :offline)
                                        (setf (ui-auth-error-message ui) "Server is offline")
                                        ;; For MVP: password = username
                                        (handler-case
                                            (case action
                                              (:login
                                               (send-auth-message socket :login username username)
                                               (setf (ui-auth-error-message ui) nil
                                                     last-auth-type :login)
                                               (log-verbose "Sending login request for ~a" username))
                                              (:register
                                               (send-auth-message socket :register username username)
                                               (setf (ui-auth-error-message ui) nil
                                                     last-auth-type :register)
                                               (log-verbose "Sending register request for ~a" username)))
                                          (error ()
                                            (setf (ui-server-status ui) :offline
                                                  (ui-auth-error-message ui) "Server is offline"))))))))
                            (raylib:end-drawing))

                           ;; Gameplay phase (after authentication)
                           (t
                            ;; Update client time for interpolation
                            (when (game-interpolation-buffer game)
                              (incf (game-client-time game) dt))

                            ;; Periodic ping to measure RTT (every 5-8 seconds)
                            (when (>= elapsed (ui-server-next-ping ui))
                              (handler-case
                                  (progn
                                    (setf (ui-ping-send-time ui) elapsed)
                                    (send-net-message socket (list :type :hello)))
                                (error () nil))
                              (setf (ui-server-next-ping ui)
                                    (+ elapsed (+ 5.0 (random 3.0)))))

                            (update-client-input game dt)
                            (dolist (request (drain-net-requests game))
                              (send-net-message socket request))

                            ;; Handle intent sending (with optional prediction)
                            ;; Includes ack for delta compression if we received a snapshot
                            (let ((intent (game-client-intent game))
                                  (pred (game-prediction-state game)))
                              (if (and *client-prediction-enabled* pred)
                                  ;; Prediction enabled: store input, apply locally, send with sequence
                                  (let ((seq (store-prediction-input pred intent (game-client-time game))))
                                    (apply-local-prediction game intent dt)
                                    (send-intent-message socket intent :sequence seq :ack last-snapshot-seq))
                                  ;; No prediction: just send intent normally (with ack for delta)
                                  (send-intent-message socket intent :ack last-snapshot-seq)))
                            ;; Clear one-shot intent fields after sending
                            ;; NOTE: Do NOT clear pickup target here - it needs to persist
                            ;; until player walks to target and picks up. Server handles
                            ;; clearing via sync-player-pickup-target when pickup completes.
                            (clear-requested-chat-message (game-client-intent game))
                            (clear-requested-drop-item (game-client-intent game))

                            ;; Receive and apply snapshots
                            (let ((latest-state nil)
                                  (latest-events nil)
                                  (latest-player-id nil)
                                  (latest-sequence 0)
                                  (latest-snapshot-seq nil)  ; Delta compression seq
                                  (latest-private nil)
                                  (latest-private-player-id nil))
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
                                                                latest-player-id)
                                           latest-sequence (or (getf message :last-sequence) 0))
                                     ;; Extract delta compression sequence from state
                                     (let ((state-seq (getf latest-state :seq)))
                                       (when state-seq
                                         (setf latest-snapshot-seq state-seq)))
                                     (dolist (event (getf message :events))
                                       (push event latest-events)))
                                    ;; Fragmented snapshot chunk (Prong 3)
                                    (:snapshot-chunk
                                     (multiple-value-bind (reassembled-state reassembled-events)
                                         (receive-snapshot-chunk chunk-buf message
                                                                 (game-client-time game))
                                       (when reassembled-state
                                         ;; Complete snapshot reassembled - use it
                                         (setf latest-state reassembled-state)
                                         (let ((state-seq (getf reassembled-state :seq)))
                                           (when state-seq
                                             (setf latest-snapshot-seq state-seq)))
                                         (dolist (event reassembled-events)
                                           (push event latest-events)))))
                                    (:private-state
                                     (setf latest-private (getf message :payload)
                                           latest-private-player-id (getf message :player-id)))
                                    (:hello-ack
                                     ;; Server responded to ping - calculate RTT
                                     (let ((rtt-ms (round (* 1000.0 (- elapsed (ui-ping-send-time ui))))))
                                       (setf (ui-ping-rtt-ms ui) rtt-ms)))
                                    (t
                                     (log-verbose "Unknown message type from server: ~s"
                                                  (getf message :type)))))
                              ;; Update last-snapshot-seq for next frame's ack
                              (when latest-snapshot-seq
                                (setf last-snapshot-seq latest-snapshot-seq))
                              (when latest-state
                                (multiple-value-bind (zone-id delta-positions)
                                    (apply-snapshot game latest-state (nreverse latest-events)
                                                    :player-id latest-player-id)
                                  (declare (ignore zone-id))
                                  ;; Buffer snapshot for interpolation
                                  (when (game-interpolation-buffer game)
                                    (let* ((buffer (game-interpolation-buffer game))
                                           (local-id (or (game-net-player-id game) 0))
                                           ;; For deltas, get previous positions to avoid
                                           ;; capturing stale interpolated values
                                           (prev-snap
                                            (when (> (interpolation-buffer-count buffer) 0)
                                              (get-interpolation-snapshot-at-index
                                               buffer
                                               (1- (interpolation-buffer-count buffer)))))
                                           (prev-pos
                                            (when prev-snap
                                              (interpolation-snapshot-entity-positions prev-snap)))
                                           (positions
                                            (capture-entity-positions game local-id
                                                                      :delta-positions delta-positions
                                                                      :previous-positions prev-pos
                                                                      :buffer buffer))
                                           (snapshot (%make-interpolation-snapshot
                                                      :timestamp (game-client-time game)
                                                      :entity-positions positions)))
                                      (push-interpolation-snapshot buffer snapshot)
                                      (setf (game-last-snapshot-time game)
                                            (game-client-time game))))
                                  ;; Reconcile prediction if enabled
                                  (when (and *client-prediction-enabled*
                                             (game-prediction-state game)
                                             (> latest-sequence 0))
                                    (let ((player (game-player game)))
                                      (when player
                                        (reconcile-prediction game
                                                             (player-x player)
                                                             (player-y player)
                                                             latest-sequence))))))
                              (when latest-private
                                (apply-private-state game latest-private
                                                     :player-id latest-private-player-id)))

                            ;; Interpolate remote entities before drawing
                            (interpolate-remote-entities game)
                            (process-combat-events game)
                            (draw-game game))))))
          (shutdown-game game)
          (usocket:socket-close socket)
          (raylib:close-audio-device))))))
