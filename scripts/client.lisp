;;;; scripts/client.lisp
;;;; Run from repo root:
;;;;   sbcl --script scripts/client.lisp

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

(defun env-bool (name)
  (let ((raw (sb-ext:posix-getenv name)))
    (and raw (not (string= raw "")) (not (string= raw "0")))))

(handler-case
    (progn
      (load-quicklisp)
      (let ((src (start-src-dir)))
        (when src
          (funcall (read-from-string "uiop:chdir") (namestring src))
          (setf *default-pathname-defaults* src)
          (format t "~&CLIENT: cwd -> ~a~%" (namestring src))
          (finish-output)))
      (funcall (read-from-string "ql:register-local-projects"))
      (funcall (read-from-string "ql:quickload") :mmorpg)
      ;; Set verbose modes from environment
      (when (env-bool "MMORPG_VERBOSE")
        (setf (symbol-value (read-from-string "mmorpg:*verbose*")) t)
        (format t "~&CLIENT: verbose mode enabled~%"))
      (when (env-bool "MMORPG_VERBOSE_COORDS")
        (setf (symbol-value (read-from-string "mmorpg:*verbose-coordinates*")) t)
        (format t "~&CLIENT: verbose coordinates mode enabled~%"))
      (when (env-bool "MMORPG_VERBOSE_ZONES")
        (setf (symbol-value (read-from-string "mmorpg:*verbose-zone-transitions*")) t)
        (format t "~&CLIENT: verbose zone transitions mode enabled~%"))
      (format t "~&CLIENT: running~%")
      (finish-output)
      (funcall (read-from-string "mmorpg:run-client"))
      (format t "~&CLIENT: ok~%")
      (sb-ext:exit :code 0))
  (error (e)
    (die 1 "CLIENT FAILED: ~a" e)))
