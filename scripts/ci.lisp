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

      (format t "~&OK (quickload + compile)~%")
      (sb-ext:exit :code 0))
  (error (e)
    (die 1 "FAILED: ~a" e)))
