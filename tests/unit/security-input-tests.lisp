(in-package #:mmorpg)

;;; ============================================================================
;;; Security Input Tests
;;; Input validation, protocol security, and boundary testing.
;;; Shared helpers (env-int, define-security-test, etc.) are in 00-test-helpers.lisp
;;; ============================================================================

;;;; ==========================================================================
;;;; TEST: Unauthenticated Intent Rejection
;;;; Acceptance criteria #5: Server must ignore intents from unauthenticated clients
;;;; ==========================================================================

(define-security-test test-unauthenticated-intent-rejected
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1337))
         (buffer (make-net-buffer))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (mmorpg:run-server
                            :host host
                            :port port
                            :max-seconds 3.0)))))
    (sleep 0.2)
    (let ((auth-socket (usocket:socket-connect host port :protocol :datagram))
          (unauth-socket (usocket:socket-connect host port :protocol :datagram)))
      (unwind-protect
           (progn
             ;; Authenticate control client
             (send-net-message auth-socket
                      (list :type :register :username "sec-test-auth" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil))
               (loop :while (and (not auth-ok) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (receive-net-message auth-socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :auth-ok))
                             (setf auth-ok t)))
                         (sleep 0.01))
               (unless auth-ok
                 (error "Control client auth failed")))

             ;; Send hello from unauth socket (creates client but NOT authenticated)
             (send-net-message unauth-socket (list :type :hello))
             (sleep 0.05)

             ;; Send malicious intent from unauthenticated client
             (let ((intent (make-intent)))
               (set-intent-move intent 1.0 0.0)
               (send-net-message unauth-socket
                        (list :type :intent
                              :payload (intent->plist intent))))
             (sleep 0.1)

             ;; Verify server still responsive
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (responsive nil))
               (loop :while (and (not responsive) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (receive-net-message auth-socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :snapshot))
                             (setf responsive t)))
                         (sleep 0.01))
               (unless responsive
                 (error "Server unresponsive after unauthenticated intent"))))
        (usocket:socket-close auth-socket)
        (usocket:socket-close unauth-socket)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Speed Hack Prevention
;;;; Client should not be able to move faster by sending large move-dx/move-dy values
;;;; ==========================================================================

(define-security-test test-speed-hack-prevented
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1338))
         (buffer (make-net-buffer))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (mmorpg:run-server
                            :host host
                            :port port
                            :max-seconds 3.0)))))
    (sleep 0.2)
    (let ((socket (usocket:socket-connect host port :protocol :datagram)))
      (unwind-protect
           (progn
             ;; Register and authenticate
             (send-net-message socket
                      (list :type :register :username "sec-test-speed" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil)
                    (initial-x nil)
                    (initial-y nil))
               ;; Wait for auth and get initial position
               (loop :while (and (not initial-x) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when message
                             (case (getf message :type)
                               (:auth-ok (setf auth-ok t))
                               (:snapshot
                                (when auth-ok
                                  (multiple-value-bind (x y)
                                      (extract-first-player-from-snapshot (getf message :state))
                                    (when x
                                      (setf initial-x x
                                            initial-y y))))))))
                         (sleep 0.01))
               (unless (and auth-ok initial-x)
                 (error "Failed to get initial position"))

               ;; Send MALICIOUS intent with huge move-dx (speed hack attempt)
               (send-net-message socket
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
                               (receive-net-message socket buffer)
                             (declare (ignore _h _p))
                             (when (and message (eq (getf message :type) :snapshot))
                               (multiple-value-bind (x y)
                                   (extract-first-player-from-snapshot (getf message :state))
                                 (declare (ignore y))
                                 (when x
                                   (setf final-x x)))))
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
        (usocket:socket-close socket)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Chat Message Length Enforcement
;;;; Server should truncate/reject messages exceeding *chat-max-length*
;;;; ==========================================================================

(define-security-test test-chat-message-length-enforced
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1339))
         (buffer (make-net-buffer))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (mmorpg:run-server
                            :host host
                            :port port
                            :max-seconds 2.0)))))
    (sleep 0.2)
    (let ((socket (usocket:socket-connect host port :protocol :datagram)))
      (unwind-protect
           (progn
             ;; Register and authenticate
             (send-net-message socket
                      (list :type :register :username "sec-test-chat" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil))
               (loop :while (and (not auth-ok) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :auth-ok))
                             (setf auth-ok t)))
                         (sleep 0.01))
               (unless auth-ok
                 (error "Auth failed")))

             ;; Send MALICIOUS intent with huge chat message (10,000 chars)
             (let ((huge-message (make-string 10000 :initial-element #\A)))
               (send-net-message socket
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
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :snapshot))
                             (setf responsive t)))
                         (sleep 0.01))
               (unless responsive
                 (error "Server unresponsive after oversized chat message"))))
        (usocket:socket-close socket)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Malformed Intent ID Handling
;;;; Server should handle non-integer target IDs gracefully
;;;; ==========================================================================

(define-security-test test-malformed-intent-ids-handled
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1340))
         (buffer (make-net-buffer))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (mmorpg:run-server
                            :host host
                            :port port
                            :max-seconds 2.0)))))
    (sleep 0.2)
    (let ((socket (usocket:socket-connect host port :protocol :datagram)))
      (unwind-protect
           (progn
             ;; Register and authenticate
             (send-net-message socket
                      (list :type :register :username "sec-test-ids" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil))
               (loop :while (and (not auth-ok) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :auth-ok))
                             (setf auth-ok t)))
                         (sleep 0.01))
               (unless auth-ok
                 (error "Auth failed")))

             ;; Send MALICIOUS intents with bad ID types
             ;; Test 1: String ID
             (send-net-message socket
                      (list :type :intent
                            :payload (list :move-dx 0.0 :move-dy 0.0
                                           :requested-attack-target-id "evil-string")))
             (sleep 0.1)

             ;; Test 2: List ID
             (send-net-message socket
                      (list :type :intent
                            :payload (list :move-dx 0.0 :move-dy 0.0
                                           :requested-follow-target-id '(1 2 3))))
             (sleep 0.1)

             ;; Test 3: Negative ID
             (send-net-message socket
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
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :snapshot))
                             (setf responsive t)))
                         (sleep 0.01))
               (unless responsive
                 (error "Server unresponsive after malformed IDs"))))
        (usocket:socket-close socket)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Double Login Prevention
;;;; Server should reject login attempts for already-logged-in accounts
;;;; ==========================================================================

(define-security-test test-double-login-prevented
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1341))
         (buffer1 (make-net-buffer))
         (buffer2 (make-net-buffer))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (mmorpg:run-server
                            :host host
                            :port port
                            :max-seconds 3.0)))))
    (sleep 0.2)
    (let ((socket1 (usocket:socket-connect host port :protocol :datagram))
          (socket2 (usocket:socket-connect host port :protocol :datagram)))
      (unwind-protect
           (progn
             ;; First client registers and logs in
             (send-net-message socket1
                      (list :type :register :username "sec-test-double" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil))
               (loop :while (and (not auth-ok) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (receive-net-message socket1 buffer1)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :auth-ok))
                             (setf auth-ok t)))
                         (sleep 0.01))
               (unless auth-ok
                 (error "First client auth failed")))

             ;; Second client tries to login with same account
             (send-net-message socket2
                      (list :type :login :username "sec-test-double" :password "test"))

             ;; Should receive auth-fail with :already-logged-in
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (got-rejection nil)
                    (all-messages nil))
               (loop :while (and (not got-rejection) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (receive-net-message socket2 buffer2)
                           (declare (ignore _h _p))
                           (when message
                             (push message all-messages)
                             (case (getf message :type)
                               (:auth-fail
                                (when (eq (getf message :reason) :already-logged-in)
                                  (setf got-rejection t)))
                               (:auth-ok
                                (error "Double login was allowed!")))))
                         (sleep 0.01))
               (unless got-rejection
                 (error "No :already-logged-in rejection received. Got messages: ~a"
                        (nreverse all-messages)))))
        (usocket:socket-close socket1)
        (usocket:socket-close socket2)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Auth Rate Limiting
;;;; Server should lock out IP after multiple failed login attempts
;;;; ==========================================================================

(define-security-test test-auth-rate-limiting
  ;; Note: This test triggers a rate limit lockout. We clear the rate limit state
  ;; at the end to avoid affecting subsequent tests.
  (unwind-protect
      (let* ((host "127.0.0.1")
             (port (env-int "MMORPG_NET_TEST_PORT" 1349))
             (buffer (make-net-buffer))
             (server-thread (sb-thread:make-thread
                             (lambda ()
                               (mmorpg:run-server
                                :host host
                                :port port
                                :max-seconds 4.0)))))
        (sleep 0.2)
        (let ((socket (usocket:socket-connect host port :protocol :datagram)))
          (unwind-protect
               (progn
                 ;; First register a valid account
                 (send-net-message socket
                          (list :type :register :username "rate-limit-test" :password "correct"))
                 (let* ((deadline (+ (get-internal-real-time)
                                     (floor (* 2 internal-time-units-per-second))))
                        (auth-ok nil))
                   (loop :while (and (not auth-ok) (< (get-internal-real-time) deadline))
                         :do (multiple-value-bind (message _h _p)
                                 (receive-net-message socket buffer)
                               (declare (ignore _h _p))
                               (when (and message (eq (getf message :type) :auth-ok))
                                 (setf auth-ok t)))
                             (sleep 0.01))
                   (unless auth-ok
                     (error "Failed to register test account")))

                 ;; Logout so we can test login rate limiting
                 (send-net-message socket (list :type :logout))
                 (sleep 0.2)

                 ;; Send 6 failed login attempts (wrong password) - *auth-max-attempts* is 5
                 (dotimes (i 6)
                   (send-net-message socket
                            (list :type :login :username "rate-limit-test" :password "wrong"))
                   (sleep 0.05))

                 ;; Wait for rate limit to kick in and collect responses
                 (sleep 0.3)

                 ;; Now try to login with CORRECT password - should be rate-limited
                 (send-net-message socket
                          (list :type :login :username "rate-limit-test" :password "correct"))

                 ;; Should receive :rate-limited rejection
                 (let* ((deadline (+ (get-internal-real-time)
                                     (floor (* 2 internal-time-units-per-second))))
                        (got-rate-limited nil)
                        (got-auth-ok nil))
                   (loop :while (and (not got-rate-limited) (not got-auth-ok)
                                     (< (get-internal-real-time) deadline))
                         :do (multiple-value-bind (message _h _p)
                                 (receive-net-message socket buffer)
                               (declare (ignore _h _p))
                               (when message
                                 (case (getf message :type)
                                   (:auth-fail
                                    (when (eq (getf message :reason) :rate-limited)
                                      (setf got-rate-limited t)))
                                   (:auth-ok
                                    (setf got-auth-ok t)))))
                             (sleep 0.01))
                   (when got-auth-ok
                     (error "Rate limiting failed - login succeeded despite lockout"))
                   (unless got-rate-limited
                     (error "No :rate-limited rejection received after 6 failed attempts"))))
            (usocket:socket-close socket)))
        (sb-thread:join-thread server-thread))
    ;; Cleanup: Clear rate limit state so subsequent tests aren't affected
    (auth-rate-clear-all)))

;;;; ==========================================================================
;;;; TEST: Double Login Race Condition
;;;; Two simultaneous login attempts for same account should result in only one success
;;;; ==========================================================================

(define-security-test test-double-login-race-condition
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1350))
         (buffer1 (make-net-buffer))
         (buffer2 (make-net-buffer))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (mmorpg:run-server
                            :host host
                            :port port
                            :max-seconds 4.0)))))
    (sleep 0.2)
    (let ((socket1 (usocket:socket-connect host port :protocol :datagram))
          (socket2 (usocket:socket-connect host port :protocol :datagram)))
      (unwind-protect
           (progn
             ;; First, register the account
             (send-net-message socket1
                      (list :type :register :username "race-test-user" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil))
               (loop :while (and (not auth-ok) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (receive-net-message socket1 buffer1)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :auth-ok))
                             (setf auth-ok t)))
                         (sleep 0.01))
               (unless auth-ok
                 (error "Failed to register test account")))

             ;; Logout
             (send-net-message socket1 (list :type :logout))
             (sleep 0.2)

             ;; NOW simulate race: send two login requests nearly simultaneously
             ;; from two different sockets
             (send-net-message socket1
                      (list :type :login :username "race-test-user" :password "test"))
             (send-net-message socket2
                      (list :type :login :username "race-test-user" :password "test"))

             ;; Collect responses - should get exactly ONE :auth-ok and ONE :already-logged-in
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 3 internal-time-units-per-second))))
                    (auth-ok-count 0)
                    (already-logged-in-count 0))
               (loop :while (and (< (+ auth-ok-count already-logged-in-count) 2)
                                 (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (msg1 _h1 _p1)
                             (receive-net-message socket1 buffer1)
                           (declare (ignore _h1 _p1))
                           (when msg1
                             (case (getf msg1 :type)
                               (:auth-ok (incf auth-ok-count))
                               (:auth-fail
                                (when (eq (getf msg1 :reason) :already-logged-in)
                                  (incf already-logged-in-count))))))
                         (multiple-value-bind (msg2 _h2 _p2)
                             (receive-net-message socket2 buffer2)
                           (declare (ignore _h2 _p2))
                           (when msg2
                             (case (getf msg2 :type)
                               (:auth-ok (incf auth-ok-count))
                               (:auth-fail
                                (when (eq (getf msg2 :reason) :already-logged-in)
                                  (incf already-logged-in-count))))))
                         (sleep 0.01))

               ;; Critical check: should have exactly ONE success
               (when (> auth-ok-count 1)
                 (error "RACE CONDITION: Got ~d auth-ok responses - both logins succeeded!"
                        auth-ok-count))
               (when (zerop auth-ok-count)
                 (error "Neither login attempt succeeded"))
               (unless (= already-logged-in-count 1)
                 (error "Expected 1 :already-logged-in rejection, got ~d"
                        already-logged-in-count))))
        (usocket:socket-close socket1)
        (usocket:socket-close socket2)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Extreme Coordinate Values
;;;; Server should handle NaN, Infinity, and huge coordinate values safely
;;;; ==========================================================================

(define-security-test test-extreme-coordinates-handled
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1342))
         (buffer (make-net-buffer))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (mmorpg:run-server
                            :host host
                            :port port
                            :max-seconds 2.0)))))
    (sleep 0.2)
    (let ((socket (usocket:socket-connect host port :protocol :datagram)))
      (unwind-protect
           (progn
             ;; Register and authenticate
             (send-net-message socket
                      (list :type :register :username "sec-test-coords" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil))
               (loop :while (and (not auth-ok) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :auth-ok))
                             (setf auth-ok t)))
                         (sleep 0.01))
               (unless auth-ok
                 (error "Auth failed")))

             ;; Send extreme target coordinates
             ;; Test 1: Huge positive values
             (send-net-message socket
                      (list :type :intent
                            :payload (list :target-x 999999999.0
                                           :target-y 999999999.0
                                           :target-active t)))
             (sleep 0.1)

             ;; Test 2: Huge negative values
             (send-net-message socket
                      (list :type :intent
                            :payload (list :target-x -999999999.0
                                           :target-y -999999999.0
                                           :target-active t)))
             (sleep 0.1)

             ;; Test 3: Very small values (near zero but not zero)
             (send-net-message socket
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
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :snapshot))
                             (setf responsive t)))
                         (sleep 0.01))
               (unless responsive
                 (error "Server unresponsive after extreme coordinates"))))
        (usocket:socket-close socket)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Empty/Null Message Fields
;;;; Server should handle nil and empty values in message fields
;;;; ==========================================================================

(define-security-test test-empty-message-fields-handled
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1343))
         (buffer (make-net-buffer))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (mmorpg:run-server
                            :host host
                            :port port
                            :max-seconds 2.0)))))
    (sleep 0.2)
    (let ((socket (usocket:socket-connect host port :protocol :datagram)))
      (unwind-protect
           (progn
             ;; Register and authenticate
             (send-net-message socket
                      (list :type :register :username "sec-test-empty" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil))
               (loop :while (and (not auth-ok) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :auth-ok))
                             (setf auth-ok t)))
                         (sleep 0.01))
               (unless auth-ok
                 (error "Auth failed")))

             ;; Send intent with nil values
             (send-net-message socket
                      (list :type :intent
                            :payload (list :move-dx nil :move-dy nil)))
             (sleep 0.1)

             ;; Send intent with empty payload
             (send-net-message socket
                      (list :type :intent :payload nil))
             (sleep 0.1)

             ;; Send intent with missing payload
             (send-net-message socket (list :type :intent))
             (sleep 0.1)

             ;; Send completely empty message
             (send-net-message socket nil)
             (sleep 0.1)

             ;; Server should still be responsive
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (responsive nil))
               (loop :while (and (not responsive) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :snapshot))
                             (setf responsive t)))
                         (sleep 0.01))
               (unless responsive
                 (error "Server unresponsive after empty/null fields"))))
        (usocket:socket-close socket)))
    (sb-thread:join-thread server-thread)))
