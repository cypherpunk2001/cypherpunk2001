(in-package #:mmorpg)

;;; ============================================================================
;;; Trade Tests
;;; ============================================================================


(defun run-trade-tests-internal ()
  "Run all trade system tests. Returns T if all pass, NIL otherwise."
  (let ((passed 0)
        (failed 0)
        (tests '(test-trade-session-creation
                 test-trade-validation-same-player
                 test-trade-validation-distance
                 test-trade-validation-already-trading
                 test-trade-offer-add-remove
                 test-trade-offer-confirm
                 test-trade-offer-confirm-resets-on-change
                 test-trade-cancel
                 test-trade-timeout
                 test-offer-to-item-list
                 test-add-items-to-inventory-array
                 ;; Phase 3: Trade safety tests
                 test-trade-ownership-mismatch-aborts
                 test-trade-memory-backend-consistency
                 test-trade-expired-ownership-aborts)))
    (format t "~%=== Running Trade Tests ===~%")
    ;; Ensure game data is loaded
    (unless *game-data-loaded-p*
      (load-game-data))
    ;; Clear trade state before tests
    (clrhash *active-trades*)
    (clrhash *player-trade-map*)
    (dolist (test tests)
      (handler-case
          (progn
            ;; Clean up before each test
            (clrhash *active-trades*)
            (clrhash *player-trade-map*)
            (funcall test)
            (incf passed)
            (format t "~a ~a~%" "OK" (symbol-name test)))
        (error (e)
          (incf failed)
          (format t "~a ~a: ~a~%" "FAIL" (symbol-name test) e))))
    (format t "~%Results: ~d passed, ~d failed~%" passed failed)
    (zerop failed)))

;;;; ========================================================================
;;;; Trade Session Tests
;;;; ========================================================================

(defun test-trade-session-creation ()
  "Test creating a trade session between two players."
  (let ((session (create-trade-session 1 2)))
    (assert (not (null session)) () "Session created")
    (assert (= (trade-session-player1-id session) 1) () "Player 1 ID correct")
    (assert (= (trade-session-player2-id session) 2) () "Player 2 ID correct")
    (assert (eq (trade-session-state session) :pending) () "State is pending")
    ;; Check registration
    (assert (player-in-trade-p 1) () "Player 1 registered as trading")
    (assert (player-in-trade-p 2) () "Player 2 registered as trading")
    (assert (eq (get-player-trade 1) session) () "Can find session for player 1")
    (assert (eq (get-player-trade 2) session) () "Can find session for player 2")))

(defun test-trade-validation-same-player ()
  "Test that players cannot trade with themselves."
  (let ((player (make-player 0.0 0.0 :id 1)))
    (multiple-value-bind (valid-p error-msg)
        (validate-trade-request player player)
      (assert (not valid-p) () "Self-trade rejected")
      (assert (search "yourself" error-msg) () "Error mentions self"))))

(defun test-trade-validation-distance ()
  "Test that players must be within trade distance."
  (let ((player1 (make-player 0.0 0.0 :id 1))
        (player2-near (make-player 100.0 100.0 :id 2))  ; ~7 tiles away (within 10)
        (player2-far (make-player 500.0 500.0 :id 3)))  ; ~44 tiles away (too far)
    ;; Same zone for both
    (setf (player-zone-id player1) :zone-1
          (player-zone-id player2-near) :zone-1
          (player-zone-id player2-far) :zone-1)
    ;; Near player should be valid
    (multiple-value-bind (valid-p error-msg)
        (validate-trade-request player1 player2-near)
      (declare (ignore error-msg))
      (assert valid-p () "Near player trade valid"))
    ;; Far player should be invalid
    (multiple-value-bind (valid-p error-msg)
        (validate-trade-request player1 player2-far)
      (assert (not valid-p) () "Far player trade rejected")
      (assert (search "far" error-msg) () "Error mentions distance"))))

(defun test-trade-validation-already-trading ()
  "Test that players in a trade cannot start another."
  (let ((player1 (make-player 0.0 0.0 :id 1))
        (player2 (make-player 50.0 0.0 :id 2))
        (player3 (make-player 50.0 50.0 :id 3)))
    ;; Same zone
    (setf (player-zone-id player1) :zone-1
          (player-zone-id player2) :zone-1
          (player-zone-id player3) :zone-1)
    ;; Start trade between 1 and 2
    (create-trade-session 1 2)
    ;; Player 1 trying to trade with player 3 should fail
    (multiple-value-bind (valid-p error-msg)
        (validate-trade-request player1 player3)
      (assert (not valid-p) () "Already trading rejected")
      (assert (search "already" error-msg) () "Error mentions already trading"))))

;;;; ========================================================================
;;;; Trade Offer Tests
;;;; ========================================================================

(defun test-trade-offer-add-remove ()
  "Test adding and removing items from trade offer."
  (let ((session (create-trade-session 1 2)))
    ;; Add item to offer
    (assert (add-to-trade-offer session 1 0 5) () "Add to offer returns T")
    ;; Check offer updated
    (let ((offer (get-player-offer session 1)))
      (assert (= (gethash 0 (trade-offer-slots offer)) 5) () "Slot 0 has count 5"))
    ;; Remove item
    (remove-from-trade-offer session 1 0)
    (let ((offer (get-player-offer session 1)))
      (assert (null (gethash 0 (trade-offer-slots offer))) () "Slot 0 removed"))))

(defun test-trade-offer-confirm ()
  "Test confirming trade offers."
  (let ((session (create-trade-session 1 2)))
    ;; Player 1 confirms
    (assert (not (confirm-trade-offer session 1)) () "One confirm returns NIL")
    (assert (trade-offer-confirmed (trade-session-player1-offer session))
            () "Player 1 confirmed")
    ;; Player 2 confirms
    (assert (confirm-trade-offer session 2) () "Both confirm returns T")
    (assert (eq (trade-session-state session) :both-confirmed)
            () "State is both-confirmed")))

(defun test-trade-offer-confirm-resets-on-change ()
  "Test that changing offer unconfirms both players."
  (let ((session (create-trade-session 1 2)))
    ;; Both players confirm
    (confirm-trade-offer session 1)
    (confirm-trade-offer session 2)
    (assert (eq (trade-session-state session) :both-confirmed)
            () "Both confirmed initially")
    ;; Player 1 modifies offer
    (add-to-trade-offer session 1 0 3)
    ;; Both should be unconfirmed
    (assert (not (trade-offer-confirmed (trade-session-player1-offer session)))
            () "Player 1 unconfirmed after change")
    (assert (not (trade-offer-confirmed (trade-session-player2-offer session)))
            () "Player 2 unconfirmed after change")
    (assert (eq (trade-session-state session) :pending)
            () "State back to pending")))

(defun test-trade-cancel ()
  "Test cancelling a trade session."
  (let ((session (create-trade-session 1 2)))
    (cancel-trade-session session "test cancel")
    ;; Check cleanup
    (assert (not (player-in-trade-p 1)) () "Player 1 no longer trading")
    (assert (not (player-in-trade-p 2)) () "Player 2 no longer trading")
    (assert (null (gethash (trade-session-id session) *active-trades*))
            () "Session removed from active trades")
    (assert (eq (trade-session-state session) :cancelled)
            () "State is cancelled")))

(defun test-trade-timeout ()
  "Test trade timeout detection."
  (let ((session (create-trade-session 1 2)))
    ;; Fresh session should not be timed out
    (assert (not (check-trade-timeout session)) () "Fresh session not timed out")
    ;; Manually set old activity time
    (let ((old-time (- (float (get-internal-real-time) 1.0)
                       (* 120 internal-time-units-per-second))))  ; 120 seconds ago
      (setf (trade-offer-last-activity (trade-session-player1-offer session)) old-time)
      (setf (trade-offer-last-activity (trade-session-player2-offer session)) old-time))
    ;; Now should be timed out
    (assert (check-trade-timeout session) () "Old session timed out")))

;;;; ========================================================================
;;;; Utility Function Tests
;;;; ========================================================================

(defun test-offer-to-item-list ()
  "Test converting trade offer to item list."
  (let* ((player (make-player 0.0 0.0 :id 1))
         (inventory (player-inventory player))
         (slots (inventory-slots inventory)))
    ;; Set up inventory with some items (use actual items from game-data)
    (setf (aref slots 0) (make-inventory-slot :item-id :coins :count 10))
    (setf (aref slots 2) (make-inventory-slot :item-id :bones :count 1))
    ;; Create offer with slot 0 (5 coins) and slot 2 (1 bones)
    (let ((offer (make-trade-offer)))
      (setf (gethash 0 (trade-offer-slots offer)) 5)
      (setf (gethash 2 (trade-offer-slots offer)) 1)
      (let ((items (offer-to-item-list player offer)))
        (assert (= (length items) 2) () "Two items in list")
        ;; Check items (order may vary)
        (assert (member '(:coins 5) items :test #'equal)
                () "Coins offer present")
        (assert (member '(:bones 1) items :test #'equal)
                () "Bones offer present")))))

(defun test-add-items-to-inventory-array ()
  "Test adding items to inventory array."
  (let* ((slots (make-array 5)))
    ;; Initialize with empty slots
    (dotimes (i 5)
      (setf (aref slots i) (make-inventory-slot :item-id nil :count 0)))
    ;; Add 3 coins (stack-size 9999 allows large stacks)
    (assert (add-items-to-inventory-array slots :coins 3)
            () "Add 3 coins succeeds")
    (assert (eq (inventory-slot-item-id (aref slots 0)) :coins)
            () "Coins in slot 0")
    (assert (= (inventory-slot-count (aref slots 0)) 3)
            () "Count is 3")
    ;; Add more coins - should stack
    (assert (add-items-to-inventory-array slots :coins 5)
            () "Add 5 more coins succeeds")
    (assert (= (inventory-slot-count (aref slots 0)) 8)
            () "Stacked to 8")
    ;; Add different item (bones has stack-size 1)
    (assert (add-items-to-inventory-array slots :bones 1)
            () "Add bones succeeds")
    (assert (eq (inventory-slot-item-id (aref slots 1)) :bones)
            () "Bones in slot 1")))

;;;; ========================================================================
;;;; Phase 3: Trade Safety Tests
;;;; ========================================================================

(defun test-trade-ownership-mismatch-aborts ()
  "Test that trade aborts if session ownership doesn't match.
   Phase 3: Prevents stale servers from committing trades after losing ownership."
  (let* ((storage (make-instance 'memory-storage))
         (*storage* storage)
         (*server-instance-id* "test-server-001"))
    (storage-connect storage)
    ;; Set up two players with different owners
    (let* ((p1-key "player:100")
           (p2-key "player:200")
           (owner1-key (session-owner-key 100))
           (owner2-key (session-owner-key 200))
           (p1-data '(:id 100 :version 3 :x 0.0 :y 0.0))
           (p2-data '(:id 200 :version 3 :x 10.0 :y 10.0)))
      ;; Save initial player data
      (storage-save storage p1-key p1-data)
      (storage-save storage p2-key p2-data)
      ;; Set ownership - player 1 owned by this server, player 2 owned by OTHER server
      (storage-save storage owner1-key *server-instance-id*)
      (storage-save storage owner2-key "other-server-002")  ; Different owner!
      ;; Try to execute trade script - should fail on ownership mismatch
      (let ((result (storage-eval-script storage "trade_complete"
                                         (list p1-key p2-key owner1-key owner2-key)
                                         (list "((:id 100 :version 3))"
                                               "((:id 200 :version 3))"
                                               *server-instance-id*))))
        ;; Should return an error, not "OK"
        (assert (or (null result)
                    (and (stringp result)
                         (search "TRADE_ERROR" result)))
                () "Trade should abort on ownership mismatch, got: ~a" result))
      ;; Verify original data unchanged
      (let ((p1-loaded (storage-load storage p1-key))
            (p2-loaded (storage-load storage p2-key)))
        (assert (equal (getf p1-loaded :x) (getf p1-data :x))
                () "Player 1 data unchanged after failed trade")
        (assert (equal (getf p2-loaded :x) (getf p2-data :x))
                () "Player 2 data unchanged after failed trade")))))

(defun test-trade-memory-backend-consistency ()
  "Test that memory backend trade results load cleanly via db-load-player.
   Phase 3: Memory backend must parse strings to plists, not store raw strings."
  (let* ((storage (make-instance 'memory-storage))
         (*storage* storage)
         (*server-instance-id* "test-server-001"))
    (storage-connect storage)
    ;; Set up two players
    (let* ((p1-key (player-key 100))
           (p2-key (player-key 200))
           (owner1-key (session-owner-key 100))
           (owner2-key (session-owner-key 200))
           ;; Full valid player plists with inventory
           (p1-data (list :id 100 :version *player-schema-version*
                          :username "trader1" :x 0.0 :y 0.0
                          :hp 100 :max-hp 100 :xp 0 :level 1
                          :inventory nil :equipment nil
                          :zone-id 'test-zone :gold 50
                          :lifetime-xp 0 :playtime 0
                          :created-at (get-universal-time)))
           (p2-data (list :id 200 :version *player-schema-version*
                          :username "trader2" :x 10.0 :y 10.0
                          :hp 100 :max-hp 100 :xp 0 :level 1
                          :inventory nil :equipment nil
                          :zone-id 'test-zone :gold 100
                          :lifetime-xp 0 :playtime 0
                          :created-at (get-universal-time))))
      ;; Save initial player data as plists (normal storage)
      (storage-save storage p1-key p1-data)
      (storage-save storage p2-key p2-data)
      ;; Set ownership - both owned by this server
      (storage-save storage owner1-key *server-instance-id*)
      (storage-save storage owner2-key *server-instance-id*)
      ;; Execute trade script with stringified data (as execute-trade-atomic does)
      (let* ((p1-updated (copy-list p1-data))
             (p2-updated (copy-list p2-data)))
        ;; Simulate trade: swap gold
        (setf (getf p1-updated :gold) 100)  ; p1 receives 50 from p2
        (setf (getf p2-updated :gold) 50)   ; p2 gives 50 to p1
        (let ((result (storage-eval-script storage "trade_complete"
                                           (list p1-key p2-key owner1-key owner2-key)
                                           (list (prin1-to-string p1-updated)
                                                 (prin1-to-string p2-updated)
                                                 *server-instance-id*))))
          (assert (and result (string= result "OK"))
                  () "Trade should succeed, got: ~a" result)))
      ;; Key test: load player data via storage-load (simulating db-load-player)
      ;; This MUST return a plist, not a string
      (let ((p1-loaded (storage-load storage p1-key))
            (p2-loaded (storage-load storage p2-key)))
        ;; Verify loaded data is a plist, not a string
        (assert (listp p1-loaded)
                () "Player 1 loaded data should be a plist, not ~a" (type-of p1-loaded))
        (assert (listp p2-loaded)
                () "Player 2 loaded data should be a plist, not ~a" (type-of p2-loaded))
        ;; Verify trade effects persisted correctly
        (assert (= (getf p1-loaded :gold) 100)
                () "Player 1 should have 100 gold after trade")
        (assert (= (getf p2-loaded :gold) 50)
                () "Player 2 should have 50 gold after trade")
        ;; Verify other fields intact
        (assert (equal (getf p1-loaded :username) "trader1")
                () "Player 1 username intact")
        (assert (equal (getf p2-loaded :username) "trader2")
                () "Player 2 username intact")))))

(defun test-trade-expired-ownership-aborts ()
  "Test that trade aborts if ownership key has expired (TTL-aware check).
   Phase 3 P2: Memory backend must respect TTL expiration on ownership keys."
  (let* ((storage (make-instance 'memory-storage))
         (*storage* storage)
         (*server-instance-id* "test-server-001"))
    (storage-connect storage)
    ;; Set up two players
    (let* ((p1-key "player:100")
           (p2-key "player:200")
           (owner1-key (session-owner-key 100))
           (owner2-key (session-owner-key 200))
           (p1-data '(:id 100 :version 3 :x 0.0 :y 0.0))
           (p2-data '(:id 200 :version 3 :x 10.0 :y 10.0)))
      ;; Save initial player data
      (storage-save storage p1-key p1-data)
      (storage-save storage p2-key p2-data)
      ;; Set ownership with TTL - player 1 has valid ownership
      (storage-save-with-ttl storage owner1-key *server-instance-id* 60)
      ;; Player 2 ownership is EXPIRED (simulate by setting TTL in the past)
      (setf (gethash owner2-key (memory-storage-data storage)) *server-instance-id*)
      (setf (gethash owner2-key *memory-storage-ttls*) (- (get-universal-time) 10))  ; Expired 10 seconds ago
      ;; Try to execute trade script - should fail because player 2's ownership expired
      (let ((result (storage-eval-script storage "trade_complete"
                                         (list p1-key p2-key owner1-key owner2-key)
                                         (list "((:id 100 :version 3))"
                                               "((:id 200 :version 3))"
                                               *server-instance-id*))))
        ;; Should return an error because expired ownership reads as NIL
        (assert (or (null result)
                    (and (stringp result)
                         (search "TRADE_ERROR" result)))
                () "Trade should abort when ownership expired, got: ~a" result))
      ;; Verify original data unchanged
      (let ((p1-loaded (storage-load storage p1-key))
            (p2-loaded (storage-load storage p2-key)))
        (assert (equal (getf p1-loaded :x) (getf p1-data :x))
                () "Player 1 data unchanged after failed trade")
        (assert (equal (getf p2-loaded :x) (getf p2-data :x))
                () "Player 2 data unchanged after failed trade")))))

