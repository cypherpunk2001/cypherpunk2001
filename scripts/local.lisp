;;;; scripts/local.lisp
;;;; Run from repo root:
;;;;   sbcl --script scripts/local.lisp
;;;;
;;;; Runs the game in local/standalone mode with full editor access.
;;;; Use this for zone editing and single-player testing.

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
          (format t "~&LOCAL: cwd -> ~a~%" (namestring src))
          (finish-output)))
      (funcall (read-from-string "ql:register-local-projects"))
      (funcall (read-from-string "ql:quickload") :mmorpg)
      ;; Set verbose modes from environment
      (when (env-bool "MMORPG_VERBOSE")
        (setf (symbol-value (read-from-string "mmorpg:*verbose*")) t)
        (format t "~&LOCAL: verbose mode enabled~%"))
      (when (env-bool "MMORPG_VERBOSE_COORDS")
        (setf (symbol-value (read-from-string "mmorpg:*verbose-coordinates*")) t)
        (format t "~&LOCAL: verbose coordinates mode enabled~%"))
      (format t "~&LOCAL: running (editor access enabled)~%")
      (finish-output)
      (funcall (read-from-string "mmorpg:run-local"))
      (format t "~&LOCAL: ok~%")
      (sb-ext:exit :code 0))
  (error (e)
    (die 1 "LOCAL FAILED: ~a" e)))
