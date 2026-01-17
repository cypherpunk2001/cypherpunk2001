;;;; scripts/test-security.lisp
;;;; Run from repo root:
;;;;   MMORPG_DB_BACKEND=memory sbcl --script scripts/test-security.lisp

(defun die (code fmt &rest args)
  (apply #'format *error-output* (concatenate 'string "~&" fmt "~%") args)
  (sb-ext:exit :code code))

(defun load-quicklisp ()
  (let* ((home (user-homedir-pathname))
         (setup (merge-pathnames "quicklisp/setup.lisp" home)))
    (unless (probe-file setup)
      (die 2 "Quicklisp not found at ~a" setup))
    (load setup)))

(handler-case
    (progn
      ;; 1) Load Quicklisp
      (load-quicklisp)

      ;; 2) Register local projects
      (funcall (read-from-string "ql:register-local-projects"))

      ;; 3) Load the system
      (funcall (read-from-string "ql:quickload") :mmorpg)

      ;; 4) Load test file
      (load (merge-pathnames "tests/security-test.lisp"))

      ;; 5) Run tests
      (let ((success (funcall (read-from-string "mmorpg:run-security-tests"))))
        (format t "~%")
        (if success
            (progn
              (format t "OK: All security tests passed~%")
              (sb-ext:exit :code 0))
            (progn
              (format t "FAILED: Some security tests failed~%")
              (sb-ext:exit :code 1)))))
  (error (e)
    (die 1 "TEST-SECURITY ERROR: ~a" e)))
