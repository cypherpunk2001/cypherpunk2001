(in-package #:mmorpg)

;;; Trade System Tests
;;; Focus: Trade validation, session management, atomic execution
;;; Run: make test-trade

(defun run-trade-tests ()
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
                 test-add-items-to-inventory-array)))
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
