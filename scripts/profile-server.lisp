;;;; scripts/profile-server.lisp
;;;; Run server with statistical profiling enabled
;;;; Usage: MMORPG_DB_BACKEND=memory sbcl --script scripts/profile-server.lisp

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

      ;; 4) Load profiler
      (require :sb-sprof)

      ;; 5) Get duration from args or default to 90 seconds
      (let ((duration (or (ignore-errors
                            (parse-integer (second sb-ext:*posix-argv*)))
                          90)))

        (format t "~&=== PROFILING SERVER ===~%")
        (format t "Duration: ~d seconds~%" duration)
        (format t "Starting profiler...~%")
        (finish-output)

        ;; Start CPU profiling (use funcall to avoid early symbol resolution)
        (funcall (read-from-string "sb-sprof:start-profiling")
                 :mode :cpu :sample-interval 0.001)

        ;; Run server with time limit
        (handler-case
            (funcall (read-from-string "mmorpg:run-server")
                     :host "127.0.0.1"
                     :port 1337
                     :max-seconds (float duration))
          (error (e)
            (format t "~&Server error: ~a~%" e)))

        ;; Stop profiling
        (funcall (read-from-string "sb-sprof:stop-profiling"))

        (format t "~&~%=== PROFILING RESULTS (Top 30 by CPU) ===~%~%")
        (finish-output)

        ;; Generate flat report
        (funcall (read-from-string "sb-sprof:report") :type :flat :max 30)

        (format t "~&~%=== CALL GRAPH (Top 10) ===~%~%")
        (finish-output)

        ;; Call graph for top functions
        (funcall (read-from-string "sb-sprof:report") :type :graph :max 10)

        (format t "~&~%=== PROFILING COMPLETE ===~%")
        (finish-output)))
  (error (e)
    (die 1 "PROFILE ERROR: ~a" e)))
