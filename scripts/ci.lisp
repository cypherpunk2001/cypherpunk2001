;;;; scripts/ci.lisp
;;;; Run from repo root:
;;;;   sbcl --script scripts/ci.lisp

(defun die (code fmt &rest args)
  (apply #'format *error-output* (concatenate 'string "~&" fmt "~%") args)
  (sb-ext:exit :code code))

(defun load-quicklisp ()
  (let* ((home (user-homedir-pathname))
         (setup (merge-pathnames "quicklisp/setup.lisp" home)))
    (unless (probe-file setup)
      (die 2 "Quicklisp not found at ~a" setup))
    (load setup)))

(defun env-float (name default)
  (let ((raw (sb-ext:posix-getenv name)))
    (if raw
        (let ((value (ignore-errors (let ((*read-eval* nil))
                                      (read-from-string raw)))))
          (if (and value (numberp value))
              (float value 1.0)
              default))
        default)))

(defun env-int (name default)
  (let ((raw (sb-ext:posix-getenv name)))
    (if raw
        (let ((value (ignore-errors (parse-integer raw :junk-allowed t))))
          (if (and value (integerp value))
              value
              default))
        default)))

(defun net-smoke-test ()
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1337))
         (server-seconds (env-float "MMORPG_NET_TEST_SECONDS" 1.5))
         (buffer (funcall (read-from-string "mmorpg::make-net-buffer")))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (funcall (read-from-string "mmorpg:run-server")
                                    :host host
                                    :port port
                                    :max-seconds server-seconds)))))
    (format t "~&NET-SMOKE: server thread started (~a:~d)~%" host port)
    (finish-output)
    (sleep 0.1)
    (let ((socket (funcall (read-from-string "usocket:socket-connect")
                           host port :protocol :datagram)))
      (unwind-protect
           (progn
             ;; Authenticate first (required for snapshots)
             (funcall (read-from-string "mmorpg::send-net-message") socket
                      (list :type :register
                            :username "smoke-test"
                            :password "smoke-test"))
             ;; Wait for auth-ok
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (authenticated nil))
               (loop :while (and (not authenticated)
                                 (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _host _port)
                             (funcall (read-from-string "mmorpg::receive-net-message")
                                      socket buffer)
                           (declare (ignore _host _port))
                           (when (and message
                                      (eq (getf message :type) :auth-ok))
                             (setf authenticated t)))
                         (sleep 0.01))
               (unless authenticated
                 (die 1 "NET-SMOKE: authentication failed")))
             ;; Send intent
             (funcall (read-from-string "mmorpg::send-net-message") socket
                      (list :type :intent
                            :payload (funcall (read-from-string "mmorpg::intent->plist")
                                              (funcall (read-from-string "mmorpg::make-intent")))))
             ;; Wait for snapshot
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (snapshot nil))
               (loop :while (and (not snapshot)
                                 (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _host _port)
                             (funcall (read-from-string "mmorpg::receive-net-message")
                                      socket buffer)
                           (declare (ignore _host _port))
                           (when (and message
                                      (eq (getf message :type) :snapshot))
                             (setf snapshot t)))
                         (sleep 0.01))
               (unless snapshot
                 (die 1 "NET-SMOKE: no snapshot received"))))
        (funcall (read-from-string "usocket:socket-close") socket)))
    (sb-thread:join-thread server-thread)
    (format t "~&NET-SMOKE: ok~%")
    (finish-output)))

(handler-case
    (progn
      ;; 1) Quicklisp (brings in ASDF integration)
      (load-quicklisp)

      ;; 2) Register local projects (your README step)
      (funcall (read-from-string "ql:register-local-projects"))

      ;; 3) Load your system via Quicklisp (ensures deps)
      (funcall (read-from-string "ql:quickload") :mmorpg)

      ;; 4) Force a compile pass too (optional but useful)
      (funcall (read-from-string "asdf:operate")
               (read-from-string "asdf:compile-op") :mmorpg)

      ;; 5) Smoke test UDP server/client handshake without opening a window
      (net-smoke-test)

      (format t "~&OK (quickload + compile)~%")
      (sb-ext:exit :code 0))
  (error (e)
    (die 1 "FAILED: ~a" e)))
