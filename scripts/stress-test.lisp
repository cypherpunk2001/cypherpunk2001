;;;; scripts/stress-test.lisp
;;;; Headless client stress testing - ramps up indefinitely
;;;; Run: sbcl --script scripts/stress-test.lisp
;;;; Adds 10 clients every 10 seconds until you Ctrl+C

(defun load-quicklisp ()
  (let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (unless (probe-file setup)
      (format *error-output* "Quicklisp not found~%")
      (sb-ext:exit :code 1))
    (load setup)))

(defvar *active-clients* 0)
(defvar *active-lock* (sb-thread:make-mutex))
(defvar *run-id* (get-universal-time))
(defvar *client-id* 0)

;; Set to T to stagger client spawns within each batch
(defvar *stagger* t)
(defvar *stagger-delay* 0.05) ; 50ms between each client

(defun inc-active ()
  (sb-thread:with-mutex (*active-lock*) (incf *active-clients*)))

(defun dec-active ()
  (sb-thread:with-mutex (*active-lock*) (decf *active-clients*)))

(defun get-active ()
  (sb-thread:with-mutex (*active-lock*) *active-clients*))

(defun next-id ()
  (sb-thread:with-mutex (*active-lock*) (incf *client-id*)))

(defun random-direction ()
  "Return random dx,dy for movement"
  (let ((dirs '((-1.0 . 0.0) (1.0 . 0.0) (0.0 . -1.0) (0.0 . 1.0)
                (-0.7 . -0.7) (0.7 . -0.7) (-0.7 . 0.7) (0.7 . 0.7)
                (0.0 . 0.0))))
    (nth (random (length dirs)) dirs)))

(defun run-client (host port)
  "Run one client forever, walking randomly"
  (inc-active)
  (let* ((id (next-id))
         (username (format nil "stress-~d-~d" *run-id* id))
         (socket nil)
         (buffer (funcall (read-from-string "mmorpg::make-net-buffer")))
         (intent (funcall (read-from-string "mmorpg::make-intent")))
         (set-dx (fdefinition (read-from-string "(setf mmorpg::intent-move-dx)")))
         (set-dy (fdefinition (read-from-string "(setf mmorpg::intent-move-dy)")))
         (dir-change-time 0))
    (unwind-protect
         (handler-case
             (progn
               (setf socket (funcall (read-from-string "usocket:socket-connect")
                                     host port :protocol :datagram))
               ;; Register
               (funcall (read-from-string "mmorpg::send-auth-message")
                        socket :register username "test" :host host :port port)
               ;; Wait for auth
               (loop :repeat 500
                     :do (let ((msg (funcall (read-from-string "mmorpg::receive-net-message")
                                             socket buffer)))
                           (when (and msg (eq (getf msg :type) :auth-ok))
                             (return)))
                         (sleep 0.01))
               ;; Main loop - walk randomly
               (loop
                 ;; Change direction every ~3 seconds
                 (when (>= (- (get-internal-real-time) dir-change-time)
                           (* 3 internal-time-units-per-second))
                   (let ((dir (random-direction)))
                     (funcall set-dx (car dir) intent)
                     (funcall set-dy (cdr dir) intent))
                   (setf dir-change-time (get-internal-real-time)))
                 ;; Send intent
                 (funcall (read-from-string "mmorpg::send-net-message")
                          socket
                          (list :type :intent
                                :payload (funcall (read-from-string "mmorpg::intent->plist") intent))
                          :host host :port port)
                 ;; Receive snapshots
                 (loop :repeat 5
                       :do (funcall (read-from-string "mmorpg::receive-net-message") socket buffer))
                 (sleep 0.1)))
           (error () nil))
      (dec-active)
      (when socket
        (ignore-errors (funcall (read-from-string "usocket:socket-close") socket))))))

(defun main ()
  (load-quicklisp)
  (funcall (read-from-string "ql:register-local-projects"))
  (funcall (read-from-string "ql:quickload") :mmorpg :silent t)

  (let ((host (or (sb-ext:posix-getenv "MMORPG_HOST") "127.0.0.1"))
        (port (parse-integer (or (sb-ext:posix-getenv "MMORPG_PORT") "1337")))
        (total 0))
    (format t "~&=== Stress Test ===~%")
    (format t "Adding 10 clients every 10 seconds. Ctrl+C to stop.~%~%")
    (finish-output)

    (let ((batch 10))
      (loop
        ;; Spawn batch (staggered if enabled)
        (dotimes (i batch)
          (sb-thread:make-thread (lambda () (run-client host port)))
          (when *stagger*
            (sleep *stagger-delay*)))
        (incf total batch)

        ;; Log
        (format t "[STRESS] +~d spawned | Total: ~d | Active: ~d~%" batch total (get-active))
        (finish-output)

        ;; Grow batch by 1.2x each round (doubles every ~40 seconds)
        (setf batch (max batch (truncate (* batch 1.2))))

        ;; Wait 10 seconds (minus stagger time already spent)
        (let ((wait (- 10 (if *stagger* (* batch *stagger-delay*) 0))))
          (when (> wait 0)
            (sleep wait)))))))

(main)
