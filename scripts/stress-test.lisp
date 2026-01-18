;;;; scripts/stress-test.lisp
;;;; Headless client stress testing for MMORPG server
;;;; Run from repo root:
;;;;   sbcl --script scripts/stress-test.lisp [num-clients] [duration-seconds]
;;;; Example:
;;;;   sbcl --script scripts/stress-test.lisp 10 60  # 10 clients for 60 seconds
;;;;
;;;; IMPORTANT: Uses unique timestamped usernames (stress-<timestamp>-<id>) to avoid
;;;; conflicts across multiple test runs when using persistent storage (Redis).
;;;;
;;;; NOTE: This stress test helped discover a critical bug where clients with
;;;; multiple connections could "teleport" to other players' positions if player
;;;; ID lookup failed (fixed in src/save.lisp apply-player-plists).

(defun die (code fmt &rest args)
  (apply #'format *error-output* (concatenate 'string "~&" fmt "~%") args)
  (sb-ext:exit :code code))

(defun load-quicklisp ()
  (let* ((home (user-homedir-pathname))
         (setup (merge-pathnames "quicklisp/setup.lisp" home)))
    (unless (probe-file setup)
      (die 2 "Quicklisp not found at ~a" setup))
    (load setup)))

(defun env-int (name default)
  (let ((raw (sb-ext:posix-getenv name)))
    (if raw
        (let ((value (ignore-errors (parse-integer raw :junk-allowed t))))
          (if (and value (integerp value))
              value
              default))
        default)))

(defun env-string (name default)
  (or (sb-ext:posix-getenv name) default))

(defvar *client-counter* 0)
(defvar *client-counter-lock* (sb-thread:make-mutex :name "client-counter-lock"))
(defvar *stress-run-id* (get-universal-time))

(defun generate-client-id ()
  "Generate unique client ID"
  (sb-thread:with-mutex (*client-counter-lock*)
    (incf *client-counter*)))

(defun generate-random-username ()
  "Generate random username for stress test with timestamp to ensure uniqueness across runs"
  (let ((id (generate-client-id)))
    (format nil "stress-~d-~3,'0d" *stress-run-id* id)))

(defun random-movement-intent ()
  "Generate random movement intent (walk in random direction)"
  (let ((directions '((-1.0 . 0.0)   ; west
                      (1.0 . 0.0)    ; east
                      (0.0 . -1.0)   ; north
                      (0.0 . 1.0)    ; south
                      (-0.707 . -0.707)  ; northwest
                      (0.707 . -0.707)   ; northeast
                      (-0.707 . 0.707)   ; southwest
                      (0.707 . 0.707)    ; southeast
                      (0.0 . 0.0))))     ; idle
    (let* ((dir (nth (random (length directions)) directions))
           (intent (funcall (read-from-string "mmorpg::make-intent")))
           (set-move-dx (fdefinition (read-from-string "(setf mmorpg::intent-move-dx)")))
           (set-move-dy (fdefinition (read-from-string "(setf mmorpg::intent-move-dy)"))))
      ;; Set movement direction
      (funcall set-move-dx (car dir) intent)
      (funcall set-move-dy (cdr dir) intent)
      intent)))

(defun headless-client-run (client-id host port duration)
  "Run a single headless client that registers, authenticates, and walks randomly"
  (let ((username (generate-random-username))
        (password "stress-test")
        (buffer (funcall (read-from-string "mmorpg::make-net-buffer")))
        (socket nil)
        (start-time (get-universal-time))
        (end-time (+ (get-universal-time) duration))
        (authenticated nil)
        (intent-interval 0.1)  ; Send intents every 100ms
        (last-intent-time 0.0)
        (direction-change-interval 3.0)  ; Change direction every 3 seconds
        (last-direction-change 0.0)
        (current-intent nil))
    (handler-case
        (progn
          ;; Create UDP socket
          (setf socket (funcall (read-from-string "usocket:socket-connect")
                                host port :protocol :datagram))
          (format t "[Client ~d] Connected as ~a~%" client-id username)
          (finish-output)

          ;; Send registration request
          (funcall (read-from-string "mmorpg::send-auth-message")
                   socket :register username password :host host :port port)

          ;; Wait for auth-ok
          (let ((auth-deadline (+ (get-internal-real-time)
                                  (floor (* 5 internal-time-units-per-second)))))
            (loop :while (and (not authenticated)
                              (< (get-internal-real-time) auth-deadline))
                  :do (multiple-value-bind (message _host _port)
                          (funcall (read-from-string "mmorpg::receive-net-message")
                                   socket buffer)
                        (declare (ignore _host _port))
                        (when message
                          (let ((msg-type (getf message :type)))
                            (cond
                              ((eq msg-type :auth-ok)
                               (setf authenticated t)
                               (format t "[Client ~d] Authenticated successfully~%" client-id)
                               (finish-output))
                              ((eq msg-type :auth-fail)
                               (format *error-output* "[Client ~d] Auth failed: ~a~%"
                                       client-id (getf message :reason))
                               (finish-output *error-output*)
                               (return))))))
                      (sleep 0.01)))

          (unless authenticated
            (format *error-output* "[Client ~d] Authentication timeout~%" client-id)
            (finish-output *error-output*)
            (return-from headless-client-run nil))

          ;; Initialize with random movement
          (setf current-intent (random-movement-intent))
          (setf last-direction-change (get-universal-time))

          ;; Main loop: send intents and receive snapshots
          (loop :while (< (get-universal-time) end-time)
                :do (let ((now (get-universal-time))
                          (messages-received 0))

                      ;; Change direction periodically
                      (when (> (- now last-direction-change) direction-change-interval)
                        (setf current-intent (random-movement-intent))
                        (setf last-direction-change now))

                      ;; Send intent periodically
                      (when (> (- now last-intent-time) intent-interval)
                        (funcall (read-from-string "mmorpg::send-net-message")
                                 socket
                                 (list :type :intent
                                       :payload (funcall (read-from-string "mmorpg::intent->plist")
                                                         current-intent))
                                 :host host :port port)
                        (setf last-intent-time now))

                      ;; Receive and discard snapshots (just to keep connection alive)
                      (loop :for i :from 0 :below 10
                            :do (multiple-value-bind (message _host _port)
                                    (funcall (read-from-string "mmorpg::receive-net-message")
                                             socket buffer)
                                  (declare (ignore _host _port))
                                  (when (and message (eq (getf message :type) :snapshot))
                                    (incf messages-received))))

                      (sleep 0.05))  ; 50ms tick
                :finally (let ((elapsed (- (get-universal-time) start-time)))
                          (format t "[Client ~d] Completed after ~d seconds~%" client-id elapsed)
                          (finish-output))))
      (error (e)
        (format *error-output* "[Client ~d] Error: ~a~%" client-id e)
        (finish-output *error-output*)))

    ;; Cleanup
    (when socket
      (ignore-errors
       (funcall (read-from-string "usocket:socket-close") socket)))))

(defun run-stress-test (num-clients duration host port)
  "Spawn multiple headless clients and run stress test"
  (format t "~&=== MMORPG Stress Test ===~%")
  (format t "Clients:  ~d~%" num-clients)
  (format t "Duration: ~d seconds~%" duration)
  (format t "Server:   ~a:~d~%" host port)
  (format t "~%Starting clients...~%")
  (finish-output)

  (let ((threads nil)
        (start-time (get-universal-time)))

    ;; Spawn client threads
    (dotimes (i num-clients)
      (let ((client-id (1+ i)))
        (push (sb-thread:make-thread
               (lambda ()
                 ;; Stagger client starts slightly to avoid thundering herd
                 (sleep (* i 0.1))
                 (headless-client-run client-id host port duration))
               :name (format nil "stress-client-~d" client-id))
              threads)))

    (format t "~%All ~d clients started~%" num-clients)
    (format t "Running for ~d seconds...~%" duration)
    (finish-output)

    ;; Wait for all clients to complete
    (dolist (thread threads)
      (sb-thread:join-thread thread))

    (let ((total-time (- (get-universal-time) start-time)))
      (format t "~%=== Stress Test Complete ===~%")
      (format t "Total time: ~d seconds~%" total-time)
      (format t "All ~d clients finished~%" num-clients)
      (finish-output))))

;; Main entry point
(handler-case
    (progn
      ;; Parse command line arguments
      (let* ((args (cdr sb-ext:*posix-argv*))
             (num-clients (if (and args (> (length args) 0))
                              (or (ignore-errors (parse-integer (first args))) 1)
                              1))
             (duration (if (and args (> (length args) 1))
                           (or (ignore-errors (parse-integer (second args))) 60)
                           60))
             (host (env-string "MMORPG_HOST" "127.0.0.1"))
             (port (env-int "MMORPG_PORT" 1337)))

        ;; Load system
        (load-quicklisp)
        (funcall (read-from-string "ql:register-local-projects"))
        (funcall (read-from-string "ql:quickload") :mmorpg :silent t)

        ;; Run stress test
        (run-stress-test num-clients duration host port)

        (sb-ext:exit :code 0)))
  (error (e)
    (die 1 "FAILED: ~a" e)))
