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
  (let* ((text (encode-net-message message))
         (octets (string-to-octets text))
         (size (length octets)))
    (when (> size *net-buffer-size*)
      (warn "Dropping UDP payload (~d bytes) over buffer size ~d" size *net-buffer-size*)
      (return-from send-net-message nil))
    (usocket:socket-send socket octets size :host host :port port)))

(defun receive-net-message (socket buffer)
  ;; Receive a single UDP message if ready; returns message and sender.
  (when (usocket:wait-for-input socket :timeout 0 :ready-only t)
    (multiple-value-bind (recv size host port)
        (usocket:socket-receive socket buffer (length buffer))
      (declare (ignore recv))
      (when (and size (> size 0))
        (let ((text (octets-to-string buffer size)))
          (values (decode-net-message text) host port))))))

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

(defun apply-snapshot (game state event-plists)
  ;; Apply a snapshot state and queue HUD/combat events for UI.
  (multiple-value-bind (zone-id zone-changed)
      (apply-game-state game state :apply-zone t)
    (when zone-changed
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

(defun send-intent-message (socket intent &key host port)
  ;; Send the current INTENT as a UDP message.
  (send-net-message socket
                    (list :type :intent :payload (intent->plist intent))
                    :host host
                    :port port))

(defun handle-server-load (game)
  ;; Load game state on the server and reset transient flags.
  (let* ((event-queue (game-combat-events game))
         (player (game-player game))
         (world (game-world game))
         (zone-id (load-game game *save-filepath*)))
    (if zone-id
        (progn
          (let ((server-intent (and player (player-intent player))))
            (when server-intent
              (reset-frame-intent server-intent)
              (clear-intent-target server-intent)))
          (when player
            (clear-player-auto-walk player)
            (setf (player-attacking player) nil
                  (player-attack-hit player) nil
                  (player-hit-active player) nil)
            (mark-player-hud-stats-dirty player)
            (mark-player-inventory-dirty player))
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
                        (max-frames 0))
  ;; Run a UDP server that simulates the game and streams snapshots.
  (let* ((socket (usocket:socket-connect nil nil
                                        :protocol :datagram
                                        :local-host host
                                        :local-port port))
         (recv-buffer (make-net-buffer))
         (game (make-server-game))
         (player (game-player game))
         (client-intent (make-intent :target-x (player-x player)
                                     :target-y (player-y player)))
         (client-host nil)
         (client-port nil))
    (format t "~&SERVER: listening on ~a:~d~%" host port)
    (finish-output)
    (unwind-protect
         (loop :with elapsed = 0.0
               :with frames = 0
               :with accumulator = 0.0
               :until (or (and (> max-seconds 0.0)
                               (>= elapsed max-seconds))
                          (and (> max-frames 0)
                               (>= frames max-frames)))
               :do (loop
                     (multiple-value-bind (message host port)
                         (receive-net-message socket recv-buffer)
                       (unless message
                         (return))
                       (setf client-host host
                             client-port port)
                       (case (getf message :type)
                         (:hello nil)
                         (:intent
                          (apply-intent-plist
                           client-intent
                           (getf message :payload)))
                         (:save
                          (when (save-game game *save-filepath*)
                            (emit-hud-message-event (game-combat-events game)
                                                    "Game saved.")))
                         (:load
                          (handle-server-load game)))))
                   (let ((dt *sim-tick-seconds*))
                     (incf elapsed dt)
                     (incf frames)
                     (multiple-value-bind (new-acc transitions)
                         (server-step game client-intent dt accumulator)
                       (setf accumulator new-acc)
                       (dotimes (_ transitions)
                         (declare (ignore _)))))
                   (when (and client-host client-port)
                     (let* ((events (pop-combat-events (game-combat-events game)))
                            (message (list :type :snapshot
                                           :state (serialize-game-state game
                                                                        :include-visuals t)
                                           :events (mapcar #'combat-event->plist events))))
                       (send-net-message socket message
                                         :host client-host
                                         :port client-port)))
                   (sleep *sim-tick-seconds*))
      (usocket:socket-close socket))))

(defun run-client (&key (host *net-default-host*)
                        (port *net-default-port*)
                        (max-seconds 0.0)
                        (max-frames 0))
  ;; Run a client that connects to the UDP server and renders snapshots.
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
                             (latest-events nil))
                         (loop
                           (multiple-value-bind (message _host _port)
                               (receive-net-message socket recv-buffer)
                             (declare (ignore _host _port))
                             (unless message
                               (return))
                             (case (getf message :type)
                               (:snapshot
                                (setf latest-state (getf message :state))
                                (dolist (event (getf message :events))
                                  (push event latest-events))))))
                         (when latest-state
                           (apply-snapshot game latest-state (nreverse latest-events))))
                       (process-combat-events game)
                       (draw-game game)))
        (shutdown-game game)
        (usocket:socket-close socket)
        (raylib:close-audio-device)))))
