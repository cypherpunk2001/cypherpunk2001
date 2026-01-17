;;;; scripts/test-persistence.lisp
;;;; Run from repo root:
;;;;   sbcl --script scripts/test-persistence.lisp

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
      (load (merge-pathnames "src/tests/persistence-test.lisp"))

      ;; 5) Run tests
      (let ((success (funcall (read-from-string "mmorpg:run-persistence-tests"))))
        (format t "~%")
        (if success
            (progn
              (format t "OK: All persistence tests passed~%")
              (sb-ext:exit :code 0))
            (progn
              (format t "FAILED: Some persistence tests failed~%")
              (sb-ext:exit :code 1)))))
  (error (e)
    (die 1 "TEST-PERSISTENCE ERROR: ~a" e)))
