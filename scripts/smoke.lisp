;;;; scripts/smoke.lisp
;;;; Run from repo root:
;;;;   sbcl --script scripts/smoke.lisp

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

(defun smoke-src-dir ()
  (let* ((script (or *load-truename* *load-pathname*))
         (dir (and script (pathname-directory script))))
    (when dir
      (let* ((root-dir (butlast dir))
             (root (make-pathname :directory root-dir
                                  :name nil
                                  :type nil
                                  :defaults script))
             (src (merge-pathnames "src/" root)))
        (when (probe-file src)
          src)))))

(handler-case
    (progn
      (load-quicklisp)
      (let ((src (smoke-src-dir)))
        (when src
          (funcall (read-from-string "uiop:chdir") (namestring src))
          (setf *default-pathname-defaults* src)
          (format t "~&SMOKE: cwd -> ~a~%" (namestring src))
          (finish-output)))
      (funcall (read-from-string "ql:register-local-projects"))
      (funcall (read-from-string "ql:quickload") :mmorpg)
      (let ((seconds (env-float "MMORPG_SMOKE_SECONDS" 5.0))
            (frames (env-int "MMORPG_SMOKE_FRAMES" 0))
            (port (env-int "MMORPG_NET_TEST_PORT" 1337)))
        (format t "~&SMOKE: starting (seconds=~a frames=~a)~%" seconds frames)
        (finish-output)
        (let ((server-thread (sb-thread:make-thread
                              (lambda ()
                                (funcall (read-from-string "mmorpg:run-server")
                                         :host "127.0.0.1"
                                         :port port
                                         :max-seconds (+ seconds 1.0))))))
          (sleep 0.1)
          (funcall (read-from-string "mmorpg:run-client")
                   :host "127.0.0.1"
                   :port port
                   :max-seconds seconds
                   :max-frames frames)
          (sb-thread:join-thread server-thread)))
      (format t "~&SMOKE: ok~%")
      (sb-ext:exit :code 0))
  (error (e)
    (die 1 "SMOKE FAILED: ~a" e)))
