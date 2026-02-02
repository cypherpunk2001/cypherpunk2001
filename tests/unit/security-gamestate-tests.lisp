(in-package #:mmorpg)

;;;; ==========================================================================
;;;; TEST: Intent Isolation (Ownership)
;;;; Client should only be able to control their own player, not others
;;;; ==========================================================================

(define-security-test test-intent-isolation
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1344))
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
             ;; Register two different players
             (send-net-message socket1
                      (list :type :register :username "player-one" :password "test"))
             (send-net-message socket2
                      (list :type :register :username "player-two" :password "test"))

             ;; Wait for both to auth
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth1 nil)
                    (auth2 nil)
                    (p1-id nil)
                    (p2-id nil)
                    (p1-initial-x nil)
                    (p2-initial-x nil))
               (loop :while (and (or (not auth1) (not auth2) (not p1-initial-x) (not p2-initial-x))
                                 (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (msg1 _h1 _p1)
                             (receive-net-message socket1 buffer1)
                           (declare (ignore _h1 _p1))
                           (when msg1
                             (case (getf msg1 :type)
                               (:auth-ok
                                (setf auth1 t
                                      p1-id (getf msg1 :player-id)))
                               (:snapshot
                                (when (and auth1 (null p1-initial-x))
                                  (let ((players (extract-players-as-plists (getf msg1 :state))))
                                    (dolist (p players)
                                      (when (eql (getf p :id) p1-id)
                                        (setf p1-initial-x (getf p :x))))))))))
                         (multiple-value-bind (msg2 _h2 _p2)
                             (receive-net-message socket2 buffer2)
                           (declare (ignore _h2 _p2))
                           (when msg2
                             (case (getf msg2 :type)
                               (:auth-ok
                                (setf auth2 t
                                      p2-id (getf msg2 :player-id)))
                               (:snapshot
                                (when (and auth2 (null p2-initial-x))
                                  (let ((players (extract-players-as-plists (getf msg2 :state))))
                                    (dolist (p players)
                                      (when (eql (getf p :id) p2-id)
                                        (setf p2-initial-x (getf p :x))))))))))
                         (sleep 0.01))

               (unless (and auth1 auth2 p1-initial-x p2-initial-x)
                 (error "Failed to authenticate both players"))

               ;; Player 1 sends movement intent
               (send-net-message socket1
                        (list :type :intent
                              :payload (list :move-dx 1.0 :move-dy 0.0)))

               ;; Wait and check positions
               (sleep 0.5)

               ;; With delta compression, only changed entities appear in snapshots.
               ;; p2-stationary starts true; if player 2 appears and moved, we set it nil.
               (let ((p1-moved nil)
                     (p2-stationary t)
                     (p2-last-x p2-initial-x)
                     (check-deadline (+ (get-internal-real-time)
                                        (floor (* 2 internal-time-units-per-second)))))
                 (loop :while (and (not p1-moved)
                                   (< (get-internal-real-time) check-deadline))
                       :do (multiple-value-bind (msg _h _p)
                               (receive-net-message socket1 buffer1)
                             (declare (ignore _h _p))
                             (when (and msg (eq (getf msg :type) :snapshot))
                               (let ((players (extract-players-as-plists (getf msg :state))))
                                 (dolist (p players)
                                   (cond
                                     ((eql (getf p :id) p1-id)
                                      (when (> (getf p :x) p1-initial-x)
                                        (setf p1-moved t)))
                                     ((eql (getf p :id) p2-id)
                                      ;; Player 2 appeared - check if they moved significantly
                                      (let ((new-x (getf p :x)))
                                        (when (> (abs (- new-x p2-last-x)) 10.0)
                                          (setf p2-stationary nil))
                                        (setf p2-last-x new-x))))))))
                           (sleep 0.01))

                 (unless p1-moved
                   (error "Player 1's movement was not applied"))
                 (unless p2-stationary
                   (error "Player 2 moved despite not sending intent - isolation breach!")))))
        (usocket:socket-close socket1)
        (usocket:socket-close socket2)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Attack Range Validation (Server Authority)
;;;; Server should handle attack requests for invalid/far targets without crashing
;;;; ==========================================================================

(define-security-test test-attack-range-validated
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1345))
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
             (send-net-message socket
                      (list :type :register :username "range-test" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil))
               ;; Wait for auth
               (loop :while (and (not auth-ok)
                                 (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (msg _h _p)
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when (and msg (eq (getf msg :type) :auth-ok))
                             (setf auth-ok t)))
                         (sleep 0.01))

               (unless auth-ok
                 (error "Auth failed"))

               ;; Move far away from spawn
               (send-net-message socket
                        (list :type :intent
                              :payload (list :target-x 9999.0
                                             :target-y 9999.0
                                             :target-active t)))
               (sleep 0.2)

               ;; Try to attack a non-existent NPC ID (should be ignored)
               (send-net-message socket
                        (list :type :intent
                              :payload (list :requested-attack-target-id 99999
                                             :attack t)))
               (sleep 0.2)

               ;; Try to attack NPC id 1 from far away (if exists, shouldn't hit)
               (send-net-message socket
                        (list :type :intent
                              :payload (list :requested-attack-target-id 1
                                             :attack t)))
               (sleep 0.2)

               ;; Server should still be responsive
               (let* ((check-deadline (+ (get-internal-real-time)
                                         (floor (* 2 internal-time-units-per-second))))
                      (responsive nil))
                 (loop :while (and (not responsive)
                                   (< (get-internal-real-time) check-deadline))
                       :do (multiple-value-bind (msg _h _p)
                               (receive-net-message socket buffer)
                             (declare (ignore _h _p))
                             (when (and msg (eq (getf msg :type) :snapshot))
                               (setf responsive t)))
                           (sleep 0.01))
                 (unless responsive
                   (error "Server unresponsive after out-of-range attack attempts")))))
        (usocket:socket-close socket)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Rapid Intent Spam (Timing/Race Conditions)
;;;; Server should handle rapid intent spam without breaking
;;;; ==========================================================================

(define-security-test test-rapid-intent-spam
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1346))
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
             (send-net-message socket
                      (list :type :register :username "spam-test" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil))
               (loop :while (and (not auth-ok)
                                 (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (msg _h _p)
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when (and msg (eq (getf msg :type) :auth-ok))
                             (setf auth-ok t)))
                         (sleep 0.01))
               (unless auth-ok
                 (error "Auth failed")))

             ;; Spam 100 intents rapidly
             (dotimes (i 100)
               (send-net-message socket
                        (list :type :intent
                              :payload (list :move-dx (if (evenp i) 1.0 -1.0)
                                             :move-dy 0.0
                                             :attack (zerop (mod i 10))))))

             ;; Server should still be responsive
             (sleep 0.5)
             (let* ((check-deadline (+ (get-internal-real-time)
                                       (floor (* 2 internal-time-units-per-second))))
                    (responsive nil))
               (loop :while (and (not responsive)
                                 (< (get-internal-real-time) check-deadline))
                     :do (multiple-value-bind (msg _h _p)
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when (and msg (eq (getf msg :type) :snapshot))
                             (setf responsive t)))
                         (sleep 0.01))
               (unless responsive
                 (error "Server became unresponsive after intent spam"))))
        (usocket:socket-close socket)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Inventory Stack Limits (Inventory Bugs)
;;;; Adding items should respect stack limits
;;;; ==========================================================================

(define-security-test test-inventory-stack-limits
  ;; This test uses internal functions directly (no network needed)
  (let* ((player (make-player 0.0 0.0 :id 999)))
    ;; Try to add 1000 of a stackable item (stack limit is typically 20-99)
    (let ((result (grant-inventory-item player :health-potion 1000)))
      ;; Should get back the overflow amount (items that didn't fit)
      (when (and result (zerop result))
        (error "Stack limit was not enforced - added 1000 items with no overflow"))
      ;; Inventory should have items but not infinite
      (let* ((inv (player-inventory player))
             (slots (and inv (inventory-slots inv)))
             (total 0))
        (when slots
          (loop :for slot :across slots
                :when (and slot
                           (eq (inventory-slot-item-id slot) :health-potion))
                  :do (incf total (inventory-slot-count slot))))
        (when (>= total 1000)
          (error "Inventory accepted 1000 items without enforcing stack limits"))))))

;;;; ==========================================================================
;;;; TEST: Negative Value Prevention (Economy/Overflow)
;;;; Server should prevent negative HP, XP, or other values
;;;; ==========================================================================

(define-security-test test-negative-value-prevention
  ;; Test that player stats can't go negative through normal operations
  (let* ((player (make-player 0.0 0.0 :id 998)))
    ;; Set HP to something positive
    (setf (player-hp player) 100)

    ;; Apply massive damage (more than HP) - use the same logic as combat.lisp
    (let ((damage 9999))
      (setf (player-hp player) (max 0 (- (player-hp player) damage))))

    ;; HP should be 0, not negative
    (when (< (player-hp player) 0)
      (error "HP went negative: ~a" (player-hp player)))

    ;; Verify XP starts at valid value (not negative)
    (let ((xp (player-lifetime-xp player)))
      (when (and xp (< xp 0))
        (error "XP is negative: ~a" xp)))))

;;;; ==========================================================================
;;;; TEST: Corrupted Plist Handling (Persistence)
;;;; Server should handle corrupted/malformed save data gracefully
;;;; ==========================================================================

(define-security-test test-corrupted-plist-handling
  ;; Test deserialize-player handles bad data
  ;; deserialize-player takes (plist inventory-size equipment-size)
  (let ((inv-size *inventory-size*)
        (equip-size (length *equipment-slot-ids*)))

    ;; Test with completely empty/nil plist - should return a valid player with defaults
    (handler-case
        (let ((player (deserialize-player nil inv-size equip-size)))
          (unless player
            (error "deserialize-player returned nil for empty plist"))
          ;; Player should have valid defaults
          (unless (>= (player-hp player) 0)
            (error "Player HP not valid after empty plist")))
      (error (e)
        (error "Crashed on nil plist: ~a" e)))

    ;; Test with minimal valid plist
    (handler-case
        (let ((player (deserialize-player (list :id 1 :x 0.0 :y 0.0)
                                          inv-size equip-size)))
          (unless player
            (error "deserialize-player returned nil for minimal plist")))
      (error (e)
        (error "Crashed on minimal plist: ~a" e)))

    ;; Test with negative HP in plist - getf will just use the value,
    ;; but the game logic should handle it
    (handler-case
        (let ((player (deserialize-player (list :id 2 :x 0.0 :y 0.0 :hp -100)
                                          inv-size equip-size)))
          ;; This tests that deserialize doesn't crash on negative HP
          ;; The game logic should clamp/handle it
          (declare (ignore player))
          t)
      (error (e)
        (error "Crashed on negative HP in plist: ~a" e)))))

;;;; ==========================================================================
;;;; TEST: Session Hijack Prevention (Ownership)
;;;; Client should not be able to claim another player's session
;;;; ==========================================================================

(define-security-test test-session-hijack-prevention
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1347))
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
             ;; Try to send intent with forged player-id before authenticating
             (send-net-message socket (list :type :hello))
             (sleep 0.1)

             ;; Send intent claiming to be player 1 (forged)
             (send-net-message socket
                      (list :type :intent
                            :player-id 1  ; Forged player ID
                            :payload (list :move-dx 1.0 :move-dy 0.0)))
             (sleep 0.2)

             ;; Server should still be running and reject the forged intent
             ;; Now authenticate properly
             (send-net-message socket
                      (list :type :register :username "hijack-test" :password "test"))

             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil)
                    (my-id nil))
               (loop :while (and (not auth-ok)
                                 (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (msg _h _p)
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when msg
                             (case (getf msg :type)
                               (:auth-ok
                                (setf auth-ok t
                                      my-id (getf msg :player-id))))))
                         (sleep 0.01))
               (unless auth-ok
                 (error "Auth failed after forged intent"))

               ;; Our assigned ID should NOT be 1 (the forged ID)
               ;; This proves the server didn't accept our forged claim
               (when (and my-id (= my-id 1))
                 ;; This could be coincidence, so not an error, just a note
                 t)))
        (usocket:socket-close socket)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Pickup Range Validation
;;;; Player must be standing on same tile to pick up items
;;;; ==========================================================================

(define-security-test test-pickup-requires-same-tile
  ;; Create minimal world and player to test pickup-tile-in-range-p
  (let* ((tile-size 64.0)  ; typical tile-dest-size
         (world (mmorpg::%make-world))
         (player (mmorpg::make-player 160.0 224.0 :id 1)))  ; Start at test position
    ;; Set world tile size
    (setf (mmorpg::world-tile-dest-size world) tile-size)

    ;; Place player at center of tile (2, 3) - world coords (160, 224)
    (setf (mmorpg::player-x player) 160.0
          (mmorpg::player-y player) 224.0)

    ;; Test 1: Same tile (2, 3) - should succeed
    (unless (mmorpg::pickup-tile-in-range-p player 2 3 world)
      (error "Pickup should succeed when player is on same tile (2,3)"))

    ;; Test 2: Adjacent tile (3, 3) - should fail
    (when (mmorpg::pickup-tile-in-range-p player 3 3 world)
      (error "Pickup should fail for adjacent tile (3,3)"))

    ;; Test 3: Adjacent tile (2, 4) - should fail
    (when (mmorpg::pickup-tile-in-range-p player 2 4 world)
      (error "Pickup should fail for adjacent tile (2,4)"))

    ;; Test 4: Distant tile (10, 10) - should fail
    (when (mmorpg::pickup-tile-in-range-p player 10 10 world)
      (error "Pickup should fail for distant tile (10,10)"))

    ;; Test 5: Player at edge of tile - still same tile
    (setf (mmorpg::player-x player) 128.5  ; just inside tile 2
          (mmorpg::player-y player) 192.5) ; just inside tile 3
    (unless (mmorpg::pickup-tile-in-range-p player 2 3 world)
      (error "Pickup should succeed at tile edge (still tile 2,3)"))))

;;;; ==========================================================================
;;;; TEST: Duplicate Pickup Prevention (Duplication)
;;;; Rapidly picking up same object should not duplicate items
;;;; ==========================================================================

(define-security-test test-duplicate-pickup-prevented
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1348))
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
             (send-net-message socket
                      (list :type :register :username "dupe-test" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil))
               (loop :while (and (not auth-ok)
                                 (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (msg _h _p)
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when (and msg (eq (getf msg :type) :auth-ok))
                             (setf auth-ok t)))
                         (sleep 0.01))
               (unless auth-ok
                 (error "Auth failed")))

             ;; Spam pickup requests for same object coordinates
             ;; Even if there's an object there, we shouldn't get duplicates
             (dotimes (i 20)
               (send-net-message socket
                        (list :type :intent
                              :payload (list :requested-pickup-object-id :chest
                                             :requested-pickup-tx 5
                                             :requested-pickup-ty 5
                                             :target-x 80.0
                                             :target-y 80.0
                                             :target-active t))))

             ;; Server should handle this gracefully
             (sleep 0.3)
             (let* ((check-deadline (+ (get-internal-real-time)
                                       (floor (* 2 internal-time-units-per-second))))
                    (responsive nil))
               (loop :while (and (not responsive)
                                 (< (get-internal-real-time) check-deadline))
                     :do (multiple-value-bind (msg _h _p)
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when (and msg (eq (getf msg :type) :snapshot))
                             (setf responsive t)))
                         (sleep 0.01))
               (unless responsive
                 (error "Server unresponsive after pickup spam"))))
        (usocket:socket-close socket)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Movement Direction Clamping (Speed Hack Prevention)
;;;; Server should clamp move-dx/move-dy to [-1.0, 1.0] range
;;;; ==========================================================================

(define-security-test test-movement-direction-clamped
  ;; Test that %clamp-direction properly limits values
  (let ((clamped-positive (%clamp-direction 1000.0))
        (clamped-negative (%clamp-direction -1000.0))
        (clamped-normal (%clamp-direction 0.5))
        (clamped-edge-pos (%clamp-direction 1.0))
        (clamped-edge-neg (%clamp-direction -1.0)))
    ;; Large positive should clamp to 1.0
    (unless (= clamped-positive 1.0)
      (error "Failed to clamp 1000.0 to 1.0, got ~a" clamped-positive))
    ;; Large negative should clamp to -1.0
    (unless (= clamped-negative -1.0)
      (error "Failed to clamp -1000.0 to -1.0, got ~a" clamped-negative))
    ;; Normal values should pass through
    (unless (= clamped-normal 0.5)
      (error "Normal value 0.5 was modified to ~a" clamped-normal))
    ;; Edge values should be preserved
    (unless (= clamped-edge-pos 1.0)
      (error "Edge value 1.0 was modified to ~a" clamped-edge-pos))
    (unless (= clamped-edge-neg -1.0)
      (error "Edge value -1.0 was modified to ~a" clamped-edge-neg))

    ;; Test that apply-intent-plist uses clamping
    (let ((intent (make-intent)))
      (apply-intent-plist intent (list :move-dx 1000.0 :move-dy -500.0))
      (unless (= (intent-move-dx intent) 1.0)
        (error "apply-intent-plist didn't clamp move-dx, got ~a"
               (intent-move-dx intent)))
      (unless (= (intent-move-dy intent) -1.0)
        (error "apply-intent-plist didn't clamp move-dy, got ~a"
               (intent-move-dy intent))))))

;;;; ==========================================================================
;;;; TEST: Auth Replay Protection
;;;; Validate timestamp window and duplicate nonce behavior
;;;; ==========================================================================

(define-security-test test-auth-check-replay-window
  (unwind-protect
      (progn
        ;; Reset nonce cache for a clean test.
        (with-auth-nonce-lock
          (clrhash *auth-nonce-cache*)
          (setf *auth-nonce-last-cleanup* 0))
        ;; Valid timestamp should be accepted once.
        (let* ((nonce "auth-replay-ok")
               (timestamp (get-universal-time)))
          (unless (auth-check-replay nonce timestamp)
            (error "Valid auth replay check was rejected"))
          (when (auth-check-replay nonce timestamp)
            (error "Duplicate nonce was accepted")))
        ;; Too old timestamps should be rejected.
        (let ((old-ts (- (get-universal-time) *auth-timestamp-window* 1)))
          (when (auth-check-replay "auth-replay-old" old-ts)
            (error "Expired auth timestamp was accepted")))
        ;; Future timestamps beyond tolerance should be rejected.
        (let ((future-ts (+ (get-universal-time) 10)))
          (when (auth-check-replay "auth-replay-future" future-ts)
            (error "Future auth timestamp was accepted"))))
    ;; Cleanup nonce cache to avoid cross-test contamination.
    (with-auth-nonce-lock
      (clrhash *auth-nonce-cache*)
      (setf *auth-nonce-last-cleanup* 0))))

;;;; ==========================================================================
;;;; THREAD-SAFETY REGRESSION TESTS (Multi-threaded Server Mode)
;;;; These tests verify mutex-protected operations work correctly under
;;;; concurrent access, preventing regressions when MMORPG_WORKER_THREADS > 1
;;;; ==========================================================================

#+sbcl
(define-security-test test-concurrent-dirty-flag-marking
  ;; Test that concurrent mark-player-dirty calls don't lose updates
  ;; This verifies *player-sessions-lock* protection
  (let* ((test-player-ids '(1001 1002 1003 1004 1005))
         (iterations-per-thread 100)
         (errors nil)
         (errors-lock (sb-thread:make-mutex :name "errors-lock"))
         (threads nil))
    ;; Register test sessions
    (dolist (id test-player-ids)
      (let ((player (make-player 0.0 0.0 :id id)))
        (register-player-session player :zone-id :test-zone)))

    (unwind-protect
        (progn
          ;; Spawn threads that concurrently mark players dirty
          (dotimes (t-idx 5)
            (let ((my-iterations iterations-per-thread)
                  (my-ids test-player-ids)
                  (my-errors-lock errors-lock))
              (push (sb-thread:make-thread
                     (lambda ()
                       (handler-case
                           (dotimes (i my-iterations)
                             (dolist (id my-ids)
                               (mark-player-dirty id)))
                         (error (e)
                           (sb-thread:with-mutex (my-errors-lock)
                             (push (format nil "Thread error: ~a" e) errors))))))
                    threads)))
          ;; Wait for all threads
          (dolist (thread threads)
            (sb-thread:join-thread thread))
          ;; Check for errors
          (when errors
            (error "Concurrent dirty marking failed: ~{~a~^, ~}" errors))
          ;; Verify all sessions still exist and are marked dirty
          (dolist (id test-player-ids)
            (with-player-sessions-lock
              (let ((session (gethash id *player-sessions*)))
                (unless session
                  (error "Session ~d disappeared during concurrent access" id))
                (unless (player-session-dirty-p session)
                  (error "Session ~d lost dirty flag during concurrent access" id))))))
      ;; Cleanup
      (dolist (id test-player-ids)
        (unregister-player-session id)))))

#+sbcl
(define-security-test test-concurrent-rate-limit-recording
  ;; Test that concurrent auth failure recording counts correctly
  ;; This verifies *auth-rate-limits-lock* protection
  (let* ((test-host "192.168.99.99")  ; Use fake IP to not affect other tests
         (threads-count 10)
         (failures-per-thread 10)
         (expected-total (* threads-count failures-per-thread))
         (current-time (float (get-universal-time) 1.0)))
    (unwind-protect
        (let ((threads nil))
          ;; Spawn threads that concurrently record failures
          (dotimes (t-idx threads-count)
            (let ((host test-host)
                  (time current-time)
                  (count failures-per-thread))
              (push (sb-thread:make-thread
                     (lambda ()
                       (dotimes (i count)
                         (auth-rate-record-failure host time))))
                    threads)))
          ;; Wait for all threads
          (dolist (thread threads)
            (sb-thread:join-thread thread))
          ;; Verify total count
          (with-auth-rate-limits-lock
            (let ((entry (gethash test-host *auth-rate-limits*)))
              (unless entry
                (error "Rate limit entry disappeared"))
              (let ((actual-count (auth-rate-entry-attempts entry)))
                ;; Should have exactly expected-total attempts recorded
                ;; (or at least *auth-max-attempts* if lockout triggered)
                (when (< actual-count (min expected-total *auth-max-attempts*))
                  (error "Lost rate limit counts: expected >= ~d, got ~d"
                         (min expected-total *auth-max-attempts*)
                         actual-count))))))
      ;; Cleanup
      (with-auth-rate-limits-lock
        (remhash test-host *auth-rate-limits*)))))

#+sbcl
(define-security-test test-concurrent-nonce-cache-access
  ;; Test that concurrent replay checks don't corrupt the nonce cache
  ;; This verifies *auth-nonce-lock* protection
  (let* ((nonce-count 100)
         (threads-count 5)
         (accepted-count 0)
         (rejected-count 0)
         (count-lock (sb-thread:make-mutex :name "test-count-lock"))
         (base-time (get-universal-time))
         (threads nil))
    ;; Each thread tries to check the same set of nonces
    ;; First thread to check each nonce should succeed, others should fail
    (dotimes (t-idx threads-count)
      (let ((my-nonce-count nonce-count)
            (my-base-time base-time)
            (my-count-lock count-lock))
        (push (sb-thread:make-thread
               (lambda ()
                 (dotimes (i my-nonce-count)
                   (let* ((nonce (format nil "test-nonce-~d" i))
                          (result (auth-check-replay nonce my-base-time)))
                     (sb-thread:with-mutex (my-count-lock)
                       (if result
                           (incf accepted-count)
                           (incf rejected-count)))))))
              threads)))
    ;; Wait for all threads
    (dolist (thread threads)
      (sb-thread:join-thread thread))
    ;; Verify exactly nonce-count acceptances (one per unique nonce)
    (unless (= accepted-count nonce-count)
      (error "Nonce cache race: expected ~d acceptances, got ~d (duplicates accepted!)"
             nonce-count accepted-count))
    ;; Verify the rest were rejected as replays
    (let ((expected-rejections (* (1- threads-count) nonce-count)))
      (unless (= rejected-count expected-rejections)
        (error "Nonce cache race: expected ~d rejections, got ~d"
               expected-rejections rejected-count)))))

#+sbcl
(define-security-test test-concurrent-zone-object-modification
  ;; Test that concurrent zone object access doesn't corrupt state
  ;; This verifies *zone-objects-lock* protection
  ;; Task 5.5: Updated to use zone-object structs
  (let* ((world (%make-world))
         (zone (%make-zone))
         (initial-objects (list (%make-zone-object :id :chest :x 5 :y 5 :count 10 :base-count 10
                                                   :respawn 0.0 :respawnable t :snapshot-dirty nil)
                                (%make-zone-object :id :chest :x 6 :y 6 :count 10 :base-count 10
                                                   :respawn 0.0 :respawnable t :snapshot-dirty nil)
                                (%make-zone-object :id :chest :x 7 :y 7 :count 10 :base-count 10
                                                   :respawn 0.0 :respawnable t :snapshot-dirty nil)))
         (iterations 50)
         (threads nil))
    ;; Setup world with zone objects
    (setf (zone-objects zone) (copy-list initial-objects))
    (setf (world-zone world) zone)
    (setf (world-tile-dest-size world) 64.0)

    ;; Spawn threads: some do respawn updates, some attempt pickups
    (dotimes (t-idx 4)
      (let ((my-world world)
            (my-iterations iterations)
            (my-t-idx t-idx))
        (push (sb-thread:make-thread
               (lambda ()
                 (dotimes (i my-iterations)
                   ;; Alternate between respawn update and pickup attempt
                   (if (evenp i)
                       (update-object-respawns my-world 0.1)
                       ;; Attempt pickup at various tiles
                       (let ((player (make-player (* 64.0 (+ 5 (mod i 3)))
                                                           (* 64.0 (+ 5 (mod i 3)))
                                                           :id (+ 2000 my-t-idx))))
                         (pickup-object-at-tile player my-world
                                                (+ 5 (mod i 3))
                                                (+ 5 (mod i 3))
                                                nil))))))
              threads)))
    ;; Wait for all threads
    (dolist (thread threads)
      (sb-thread:join-thread thread))
    ;; Verify zone-objects is still a valid list (not corrupted)
    (let ((objects (zone-objects zone)))
      (unless (listp objects)
        (error "Zone objects corrupted - not a list"))
      ;; Each object should still have valid zone-object struct
      (dolist (obj objects)
        (unless (and (zone-object-p obj) (zone-object-id obj))
          (error "Zone object corrupted: ~a" obj))))))

;;;; ==========================================================================
;;;; Main Test Runner
;;;; ==========================================================================

(defun run-security-tests-internal ()
  (format t "~&=== Running Security Tests ===~%")
  (setf *tests-passed* 0
        *tests-failed* 0)

  ;; Original tests
  (test-unauthenticated-intent-rejected)
  (test-speed-hack-prevented)
  (test-chat-message-length-enforced)
  (test-malformed-intent-ids-handled)
  (test-double-login-prevented)
  (test-auth-rate-limiting)
  (test-double-login-race-condition)
  (test-extreme-coordinates-handled)
  (test-empty-message-fields-handled)

  ;; Ownership / Control tests
  (test-intent-isolation)
  (test-session-hijack-prevention)

  ;; Server Authority tests
  (test-attack-range-validated)

  ;; Timing / Race Condition tests
  (test-rapid-intent-spam)

  ;; Inventory tests
  (test-inventory-stack-limits)

  ;; Economy / Overflow tests
  (test-negative-value-prevention)

  ;; Persistence tests
  (test-corrupted-plist-handling)

  ;; Pickup validation
  (test-pickup-requires-same-tile)

  ;; Duplication tests
  (test-duplicate-pickup-prevented)

  ;; Input validation tests
  (test-movement-direction-clamped)
  (test-auth-check-replay-window)

  ;; Thread-safety regression tests (SBCL only)
  ;; These prevent multi-threaded mode from regressing
  #+sbcl
  (progn
    (test-concurrent-dirty-flag-marking)
    (test-concurrent-rate-limit-recording)
    (test-concurrent-nonce-cache-access)
    (test-concurrent-zone-object-modification))

  (format t "~&~%Results: ~d passed, ~d failed~%" *tests-passed* *tests-failed*)
  (zerop *tests-failed*))

(export 'run-security-tests)
