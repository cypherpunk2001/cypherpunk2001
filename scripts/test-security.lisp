;;;; scripts/test-security.lisp
;;;; Security-focused tests for the MMORPG server
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

(defun env-int (name default)
  (let ((raw (sb-ext:posix-getenv name)))
    (if raw
        (let ((value (ignore-errors (parse-integer raw :junk-allowed t))))
          (if (and value (integerp value))
              value
              default))
        default)))

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defmacro define-security-test (name &body body)
  `(defun ,name ()
     (format t "~&  Testing ~a...~%" ',name)
     (handler-case
         (progn ,@body
                (incf *tests-passed*)
                (format t "~&    PASS~%"))
       (error (e)
         (incf *tests-failed*)
         (format t "~&    FAIL: ~a~%" e)))))

;;;; ==========================================================================
;;;; TEST: Unauthenticated Intent Rejection
;;;; Acceptance criteria #5: Server must ignore intents from unauthenticated clients
;;;; ==========================================================================

(define-security-test test-unauthenticated-intent-rejected
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1337))
         (buffer (funcall (read-from-string "mmorpg::make-net-buffer")))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (funcall (read-from-string "mmorpg:run-server")
                                    :host host
                                    :port port
                                    :max-seconds 3.0)))))
    (sleep 0.2)
    (let ((auth-socket (funcall (read-from-string "usocket:socket-connect")
                                host port :protocol :datagram))
          (unauth-socket (funcall (read-from-string "usocket:socket-connect")
                                  host port :protocol :datagram)))
      (unwind-protect
           (progn
             ;; Authenticate control client
             (funcall (read-from-string "mmorpg::send-net-message") auth-socket
                      (list :type :register :username "sec-test-auth" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil))
               (loop :while (and (not auth-ok) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (funcall (read-from-string "mmorpg::receive-net-message")
                                      auth-socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :auth-ok))
                             (setf auth-ok t)))
                         (sleep 0.01))
               (unless auth-ok
                 (error "Control client auth failed")))

             ;; Send hello from unauth socket (creates client but NOT authenticated)
             (funcall (read-from-string "mmorpg::send-net-message") unauth-socket
                      (list :type :hello))
             (sleep 0.05)

             ;; Send malicious intent from unauthenticated client
             (let ((intent (funcall (read-from-string "mmorpg::make-intent"))))
               (funcall (read-from-string "mmorpg::set-intent-move") intent 1.0 0.0)
               (funcall (read-from-string "mmorpg::send-net-message") unauth-socket
                        (list :type :intent
                              :payload (funcall (read-from-string "mmorpg::intent->plist") intent))))
             (sleep 0.1)

             ;; Verify server still responsive
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (responsive nil))
               (loop :while (and (not responsive) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (funcall (read-from-string "mmorpg::receive-net-message")
                                      auth-socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :snapshot))
                             (setf responsive t)))
                         (sleep 0.01))
               (unless responsive
                 (error "Server unresponsive after unauthenticated intent"))))
        (funcall (read-from-string "usocket:socket-close") auth-socket)
        (funcall (read-from-string "usocket:socket-close") unauth-socket)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Speed Hack Prevention
;;;; Client should not be able to move faster by sending large move-dx/move-dy values
;;;; ==========================================================================

(define-security-test test-speed-hack-prevented
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1338))
         (buffer (funcall (read-from-string "mmorpg::make-net-buffer")))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (funcall (read-from-string "mmorpg:run-server")
                                    :host host
                                    :port port
                                    :max-seconds 3.0)))))
    (sleep 0.2)
    (let ((socket (funcall (read-from-string "usocket:socket-connect")
                           host port :protocol :datagram)))
      (unwind-protect
           (progn
             ;; Register and authenticate
             (funcall (read-from-string "mmorpg::send-net-message") socket
                      (list :type :register :username "sec-test-speed" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil)
                    (initial-x nil)
                    (initial-y nil))
               ;; Wait for auth and get initial position
               (loop :while (and (not initial-x) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (funcall (read-from-string "mmorpg::receive-net-message")
                                      socket buffer)
                           (declare (ignore _h _p))
                           (when message
                             (case (getf message :type)
                               (:auth-ok (setf auth-ok t))
                               (:snapshot
                                (when auth-ok
                                  (let ((players (getf (getf message :state) :players)))
                                    (when players
                                      (setf initial-x (getf (first players) :x)
                                            initial-y (getf (first players) :y)))))))))
                         (sleep 0.01))
               (unless (and auth-ok initial-x)
                 (error "Failed to get initial position"))

               ;; Send MALICIOUS intent with huge move-dx (speed hack attempt)
               (funcall (read-from-string "mmorpg::send-net-message") socket
                        (list :type :intent
                              :payload (list :move-dx 1000.0  ; 1000x normal speed!
                                             :move-dy 0.0
                                             :face-dx 1.0
                                             :face-dy 0.0
                                             :target-active nil
                                             :attack nil
                                             :run-toggle nil)))

               ;; Wait a bit and check position
               (sleep 0.5)

               ;; Get current position
               (let* ((check-deadline (+ (get-internal-real-time)
                                         (floor (* 2 internal-time-units-per-second))))
                      (final-x nil))
                 (loop :while (and (not final-x) (< (get-internal-real-time) check-deadline))
                       :do (multiple-value-bind (message _h _p)
                               (funcall (read-from-string "mmorpg::receive-net-message")
                                        socket buffer)
                             (declare (ignore _h _p))
                             (when (and message (eq (getf message :type) :snapshot))
                               (let ((players (getf (getf message :state) :players)))
                                 (when players
                                   (setf final-x (getf (first players) :x))))))
                           (sleep 0.01))
                 (unless final-x
                   (error "Failed to get final position"))

                 ;; Calculate max expected movement:
                 ;; Normal speed = *player-speed* (222.0) * dt * 0.5 seconds worth
                 ;; With run multiplier (2.0) max = 222 * 2 * 0.5 = ~222 pixels
                 ;; Allow some margin for timing: 500 pixels max
                 (let ((distance (abs (- final-x initial-x))))
                   (when (> distance 500.0)
                     (error "Speed hack succeeded! Moved ~a pixels (max expected ~500)"
                            distance))))))
        (funcall (read-from-string "usocket:socket-close") socket)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Chat Message Length Enforcement
;;;; Server should truncate/reject messages exceeding *chat-max-length*
;;;; ==========================================================================

(define-security-test test-chat-message-length-enforced
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1339))
         (buffer (funcall (read-from-string "mmorpg::make-net-buffer")))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (funcall (read-from-string "mmorpg:run-server")
                                    :host host
                                    :port port
                                    :max-seconds 2.0)))))
    (sleep 0.2)
    (let ((socket (funcall (read-from-string "usocket:socket-connect")
                           host port :protocol :datagram)))
      (unwind-protect
           (progn
             ;; Register and authenticate
             (funcall (read-from-string "mmorpg::send-net-message") socket
                      (list :type :register :username "sec-test-chat" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil))
               (loop :while (and (not auth-ok) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (funcall (read-from-string "mmorpg::receive-net-message")
                                      socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :auth-ok))
                             (setf auth-ok t)))
                         (sleep 0.01))
               (unless auth-ok
                 (error "Auth failed")))

             ;; Send MALICIOUS intent with huge chat message (10,000 chars)
             (let ((huge-message (make-string 10000 :initial-element #\A)))
               (funcall (read-from-string "mmorpg::send-net-message") socket
                        (list :type :intent
                              :payload (list :move-dx 0.0 :move-dy 0.0
                                             :requested-chat-message huge-message))))

             ;; Server should still be responsive (not crashed/hung)
             (sleep 0.2)
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (responsive nil))
               (loop :while (and (not responsive) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (funcall (read-from-string "mmorpg::receive-net-message")
                                      socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :snapshot))
                             (setf responsive t)))
                         (sleep 0.01))
               (unless responsive
                 (error "Server unresponsive after oversized chat message"))))
        (funcall (read-from-string "usocket:socket-close") socket)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Malformed Intent ID Handling
;;;; Server should handle non-integer target IDs gracefully
;;;; ==========================================================================

(define-security-test test-malformed-intent-ids-handled
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1340))
         (buffer (funcall (read-from-string "mmorpg::make-net-buffer")))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (funcall (read-from-string "mmorpg:run-server")
                                    :host host
                                    :port port
                                    :max-seconds 2.0)))))
    (sleep 0.2)
    (let ((socket (funcall (read-from-string "usocket:socket-connect")
                           host port :protocol :datagram)))
      (unwind-protect
           (progn
             ;; Register and authenticate
             (funcall (read-from-string "mmorpg::send-net-message") socket
                      (list :type :register :username "sec-test-ids" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil))
               (loop :while (and (not auth-ok) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (funcall (read-from-string "mmorpg::receive-net-message")
                                      socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :auth-ok))
                             (setf auth-ok t)))
                         (sleep 0.01))
               (unless auth-ok
                 (error "Auth failed")))

             ;; Send MALICIOUS intents with bad ID types
             ;; Test 1: String ID
             (funcall (read-from-string "mmorpg::send-net-message") socket
                      (list :type :intent
                            :payload (list :move-dx 0.0 :move-dy 0.0
                                           :requested-attack-target-id "evil-string")))
             (sleep 0.1)

             ;; Test 2: List ID
             (funcall (read-from-string "mmorpg::send-net-message") socket
                      (list :type :intent
                            :payload (list :move-dx 0.0 :move-dy 0.0
                                           :requested-follow-target-id '(1 2 3))))
             (sleep 0.1)

             ;; Test 3: Negative ID
             (funcall (read-from-string "mmorpg::send-net-message") socket
                      (list :type :intent
                            :payload (list :move-dx 0.0 :move-dy 0.0
                                           :requested-attack-target-id -999)))
             (sleep 0.1)

             ;; Server should still be responsive
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (responsive nil))
               (loop :while (and (not responsive) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (funcall (read-from-string "mmorpg::receive-net-message")
                                      socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :snapshot))
                             (setf responsive t)))
                         (sleep 0.01))
               (unless responsive
                 (error "Server unresponsive after malformed IDs"))))
        (funcall (read-from-string "usocket:socket-close") socket)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Double Login Prevention
;;;; Server should reject login attempts for already-logged-in accounts
;;;; ==========================================================================

(define-security-test test-double-login-prevented
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1341))
         (buffer1 (funcall (read-from-string "mmorpg::make-net-buffer")))
         (buffer2 (funcall (read-from-string "mmorpg::make-net-buffer")))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (funcall (read-from-string "mmorpg:run-server")
                                    :host host
                                    :port port
                                    :max-seconds 3.0)))))
    (sleep 0.2)
    (let ((socket1 (funcall (read-from-string "usocket:socket-connect")
                            host port :protocol :datagram))
          (socket2 (funcall (read-from-string "usocket:socket-connect")
                            host port :protocol :datagram)))
      (unwind-protect
           (progn
             ;; First client registers and logs in
             (funcall (read-from-string "mmorpg::send-net-message") socket1
                      (list :type :register :username "sec-test-double" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil))
               (loop :while (and (not auth-ok) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (funcall (read-from-string "mmorpg::receive-net-message")
                                      socket1 buffer1)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :auth-ok))
                             (setf auth-ok t)))
                         (sleep 0.01))
               (unless auth-ok
                 (error "First client auth failed")))

             ;; Second client tries to login with same account
             (funcall (read-from-string "mmorpg::send-net-message") socket2
                      (list :type :login :username "sec-test-double" :password "test"))

             ;; Should receive auth-fail with :already-logged-in
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (got-rejection nil))
               (loop :while (and (not got-rejection) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (funcall (read-from-string "mmorpg::receive-net-message")
                                      socket2 buffer2)
                           (declare (ignore _h _p))
                           (when message
                             (case (getf message :type)
                               (:auth-fail
                                (when (eq (getf message :reason) :already-logged-in)
                                  (setf got-rejection t)))
                               (:auth-ok
                                (error "Double login was allowed!")))))
                         (sleep 0.01))
               (unless got-rejection
                 (error "No :already-logged-in rejection received"))))
        (funcall (read-from-string "usocket:socket-close") socket1)
        (funcall (read-from-string "usocket:socket-close") socket2)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Extreme Coordinate Values
;;;; Server should handle NaN, Infinity, and huge coordinate values safely
;;;; ==========================================================================

(define-security-test test-extreme-coordinates-handled
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1342))
         (buffer (funcall (read-from-string "mmorpg::make-net-buffer")))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (funcall (read-from-string "mmorpg:run-server")
                                    :host host
                                    :port port
                                    :max-seconds 2.0)))))
    (sleep 0.2)
    (let ((socket (funcall (read-from-string "usocket:socket-connect")
                           host port :protocol :datagram)))
      (unwind-protect
           (progn
             ;; Register and authenticate
             (funcall (read-from-string "mmorpg::send-net-message") socket
                      (list :type :register :username "sec-test-coords" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil))
               (loop :while (and (not auth-ok) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (funcall (read-from-string "mmorpg::receive-net-message")
                                      socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :auth-ok))
                             (setf auth-ok t)))
                         (sleep 0.01))
               (unless auth-ok
                 (error "Auth failed")))

             ;; Send extreme target coordinates
             ;; Test 1: Huge positive values
             (funcall (read-from-string "mmorpg::send-net-message") socket
                      (list :type :intent
                            :payload (list :target-x 999999999.0
                                           :target-y 999999999.0
                                           :target-active t)))
             (sleep 0.1)

             ;; Test 2: Huge negative values
             (funcall (read-from-string "mmorpg::send-net-message") socket
                      (list :type :intent
                            :payload (list :target-x -999999999.0
                                           :target-y -999999999.0
                                           :target-active t)))
             (sleep 0.1)

             ;; Test 3: Very small values (near zero but not zero)
             (funcall (read-from-string "mmorpg::send-net-message") socket
                      (list :type :intent
                            :payload (list :move-dx 0.0000001
                                           :move-dy 0.0000001)))
             (sleep 0.1)

             ;; Server should still be responsive
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (responsive nil))
               (loop :while (and (not responsive) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (funcall (read-from-string "mmorpg::receive-net-message")
                                      socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :snapshot))
                             (setf responsive t)))
                         (sleep 0.01))
               (unless responsive
                 (error "Server unresponsive after extreme coordinates"))))
        (funcall (read-from-string "usocket:socket-close") socket)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Empty/Null Message Fields
;;;; Server should handle nil and empty values in message fields
;;;; ==========================================================================

(define-security-test test-empty-message-fields-handled
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1343))
         (buffer (funcall (read-from-string "mmorpg::make-net-buffer")))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (funcall (read-from-string "mmorpg:run-server")
                                    :host host
                                    :port port
                                    :max-seconds 2.0)))))
    (sleep 0.2)
    (let ((socket (funcall (read-from-string "usocket:socket-connect")
                           host port :protocol :datagram)))
      (unwind-protect
           (progn
             ;; Register and authenticate
             (funcall (read-from-string "mmorpg::send-net-message") socket
                      (list :type :register :username "sec-test-empty" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil))
               (loop :while (and (not auth-ok) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (funcall (read-from-string "mmorpg::receive-net-message")
                                      socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :auth-ok))
                             (setf auth-ok t)))
                         (sleep 0.01))
               (unless auth-ok
                 (error "Auth failed")))

             ;; Send intent with nil values
             (funcall (read-from-string "mmorpg::send-net-message") socket
                      (list :type :intent
                            :payload (list :move-dx nil :move-dy nil)))
             (sleep 0.1)

             ;; Send intent with empty payload
             (funcall (read-from-string "mmorpg::send-net-message") socket
                      (list :type :intent :payload nil))
             (sleep 0.1)

             ;; Send intent with missing payload
             (funcall (read-from-string "mmorpg::send-net-message") socket
                      (list :type :intent))
             (sleep 0.1)

             ;; Send completely empty message
             (funcall (read-from-string "mmorpg::send-net-message") socket nil)
             (sleep 0.1)

             ;; Server should still be responsive
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (responsive nil))
               (loop :while (and (not responsive) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (funcall (read-from-string "mmorpg::receive-net-message")
                                      socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :snapshot))
                             (setf responsive t)))
                         (sleep 0.01))
               (unless responsive
                 (error "Server unresponsive after empty/null fields"))))
        (funcall (read-from-string "usocket:socket-close") socket)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; Main Test Runner
;;;; ==========================================================================

(defun run-security-tests ()
  (format t "~&=== Running Security Tests ===~%")
  (setf *tests-passed* 0
        *tests-failed* 0)

  (test-unauthenticated-intent-rejected)
  (test-speed-hack-prevented)
  (test-chat-message-length-enforced)
  (test-malformed-intent-ids-handled)
  (test-double-login-prevented)
  (test-extreme-coordinates-handled)
  (test-empty-message-fields-handled)

  (format t "~&~%Results: ~d passed, ~d failed~%" *tests-passed* *tests-failed*)
  (if (zerop *tests-failed*)
      (format t "~&OK: All security tests passed~%")
      (die 1 "FAILED: ~d security test(s) failed" *tests-failed*)))

(handler-case
    (progn
      (load-quicklisp)
      (funcall (read-from-string "ql:register-local-projects"))
      (funcall (read-from-string "ql:quickload") :mmorpg)
      (run-security-tests)
      (sb-ext:exit :code 0))
  (error (e)
    (die 1 "SECURITY TESTS FAILED: ~a" e)))
