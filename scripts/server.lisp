;;;; scripts/server.lisp
;;;; Run from repo root:
;;;;   sbcl --script scripts/server.lisp

(defun die (code fmt &rest args)
  (apply #'format *error-output* (concatenate 'string "~&" fmt "~%") args)
  (sb-ext:exit :code code))

(defun load-quicklisp ()
  (let* ((home (user-homedir-pathname))
         (setup (merge-pathnames "quicklisp/setup.lisp" home)))
    (unless (probe-file setup)
      (die 2 "Quicklisp not found at ~a" setup))
    (load setup)))

(defun start-src-dir ()
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

(defun env-int (name default)
  (let ((raw (sb-ext:posix-getenv name)))
    (if raw
        (let ((value (ignore-errors (parse-integer raw :junk-allowed t))))
          (if (and value (integerp value))
              value
              default))
        default)))

(handler-case
    (progn
      (load-quicklisp)
      (let ((src (start-src-dir)))
        (when src
          (funcall (read-from-string "uiop:chdir") (namestring src))
          (setf *default-pathname-defaults* src)
          (format t "~&SERVER: cwd -> ~a~%" (namestring src))
          (finish-output)))
      (funcall (read-from-string "ql:register-local-projects"))
      (funcall (read-from-string "ql:quickload") :mmorpg)
      (let ((worker-threads (env-int "MMORPG_WORKER_THREADS" 1)))
        (format t "~&SERVER: running (worker-threads=~d)~%" worker-threads)
        (finish-output)
        (funcall (read-from-string "mmorpg:run-server")
                 :worker-threads worker-threads))
      (format t "~&SERVER: ok~%")
      (sb-ext:exit :code 0))
  (error (e)
    (die 1 "SERVER FAILED: ~a" e)))
