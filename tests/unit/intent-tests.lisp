(in-package #:mmorpg)

;;; ============================================================
;;; INTENT.LISP TESTS
;;; ============================================================

(defun test-set-intent-move ()
  "Test setting intent movement."
  (let ((intent (make-intent)))
    (set-intent-move intent 1.0 0.0)
    (assert (= (intent-move-dx intent) 1.0) () "intent-move: dx set")
    (assert (= (intent-move-dy intent) 0.0) () "intent-move: dy set")
    ;; Check face-dx/face-dy instead of intent-face
    (assert (= (intent-face-dx intent) 1.0) () "intent-move: face-dx updated")
    (assert (= (intent-face-dy intent) 0.0) () "intent-move: face-dy updated")))

(defun test-set-intent-face ()
  "Test setting intent facing."
  (let ((intent (make-intent)))
    (set-intent-face intent 0.0 -1.0)
    (assert (= (intent-face-dx intent) 0.0) () "intent-face: up dx")
    (assert (= (intent-face-dy intent) -1.0) () "intent-face: up dy")
    (set-intent-face intent 0.0 1.0)
    (assert (= (intent-face-dy intent) 1.0) () "intent-face: down dy")
    (set-intent-face intent 1.0 0.0)
    (assert (= (intent-face-dx intent) 1.0) () "intent-face: side dx")))

(defun test-apply-intent-plist-rejects-bad-pickup ()
  "Ensure malformed pickup/drop intent fields are sanitized."
  (let ((intent (make-intent)))
    (apply-intent-plist intent (list :requested-pickup-target-id "bad-id"
                                     :requested-drop-item-id 123
                                     :requested-pickup-tx "bad"
                                     :requested-pickup-ty -3
                                     :requested-drop-slot-index "oops"))
    (assert (null (intent-requested-pickup-target-id intent)) () "pickup-id invalid -> nil")
    (assert (null (intent-requested-drop-item-id intent)) () "drop-id invalid -> nil")
    (assert (null (intent-requested-pickup-tx intent)) () "pickup-tx invalid -> nil")
    (assert (null (intent-requested-pickup-ty intent)) () "pickup-ty invalid -> nil")
    (assert (null (intent-requested-drop-slot-index intent)) () "drop-slot invalid -> nil")
    (apply-intent-plist intent (list :requested-pickup-target-id :arrows
                                     :requested-drop-item-id :coins
                                     :requested-pickup-tx 2
                                     :requested-pickup-ty 3
                                     :requested-drop-slot-index 1))
    (assert (eq (intent-requested-pickup-target-id intent) :arrows) () "pickup-id valid -> :arrows")
    (assert (eq (intent-requested-drop-item-id intent) :coins) () "drop-id valid -> :coins")
    (assert (= (intent-requested-pickup-tx intent) 2) () "pickup-tx valid -> 2")
    (assert (= (intent-requested-pickup-ty intent) 3) () "pickup-ty valid -> 3")
    (assert (= (intent-requested-drop-slot-index intent) 1) () "drop-slot valid -> 1")))

;;; ============================================================

;;; NEW INTENT TESTS (Priority 5)
;;; ============================================================

(defun test-reset-frame-intent ()
  "Test resetting per-frame intent signals."
  (let ((intent (make-intent)))
    ;; Set some values
    (set-intent-move intent 1.0 1.0)
    (setf (intent-attack intent) t
          (intent-run-toggle intent) t)
    ;; Reset
    (reset-frame-intent intent)
    ;; Check cleared
    (assert (= (intent-move-dx intent) 0.0) () "reset-intent: dx cleared")
    (assert (= (intent-move-dy intent) 0.0) () "reset-intent: dy cleared")
    (assert (null (intent-attack intent)) () "reset-intent: attack cleared")
    (assert (null (intent-run-toggle intent)) () "reset-intent: run cleared")))

(defun test-consume-intent-actions ()
  "Test consuming one-shot actions."
  (let ((intent (make-intent)))
    (setf (intent-attack intent) t
          (intent-run-toggle intent) t)
    (consume-intent-actions intent)
    (assert (null (intent-attack intent)) () "consume-intent: attack cleared")
    (assert (null (intent-run-toggle intent)) () "consume-intent: run cleared")))

(defun test-set-intent-target ()
  "Test setting intent target."
  (let ((intent (make-intent)))
    (set-intent-target intent 100.0 200.0)
    (assert (= (intent-target-x intent) 100.0) () "set-target: x")
    (assert (= (intent-target-y intent) 200.0) () "set-target: y")
    (assert (intent-target-active intent) () "set-target: active")))

(defun test-clear-intent-target ()
  "Test clearing intent target."
  (let ((intent (make-intent)))
    (set-intent-target intent 100.0 200.0)
    (clear-intent-target intent)
    (assert (not (intent-target-active intent)) () "clear-target: inactive")))

(defun test-request-pickup-target ()
  "Test pickup target request."
  (let ((intent (make-intent)))
    (request-pickup-target intent :coins 5 10)
    (assert (eq (intent-requested-pickup-target-id intent) :coins) () "pickup: id")
    (assert (= (intent-requested-pickup-tx intent) 5) () "pickup: tx")
    (assert (= (intent-requested-pickup-ty intent) 10) () "pickup: ty")
    ;; Clear
    (clear-requested-pickup-target intent)
    (assert (null (intent-requested-pickup-target-id intent)) () "pickup: cleared")))

(defun test-request-drop-item ()
  "Test drop item request."
  (let ((intent (make-intent)))
    (request-drop-item intent :health-potion 5 2)
    (assert (eq (intent-requested-drop-item-id intent) :health-potion) () "drop: item-id")
    (assert (= (intent-requested-drop-count intent) 5) () "drop: count")
    (assert (= (intent-requested-drop-slot-index intent) 2) () "drop: slot")
    ;; Clear
    (clear-requested-drop-item intent)
    (assert (null (intent-requested-drop-item-id intent)) () "drop: cleared")))

(defun test-request-inventory-swap ()
  "Test inventory swap request."
  (let ((intent (make-intent)))
    (request-inventory-swap intent 0 3)
    (assert (= (intent-requested-swap-slot-a intent) 0) () "swap: slot-a")
    (assert (= (intent-requested-swap-slot-b intent) 3) () "swap: slot-b")
    ;; Clear
    (clear-requested-inventory-swap intent)
    (assert (null (intent-requested-swap-slot-a intent)) () "swap: cleared")))

(defun test-trade-intent-functions ()
  "Test trade intent request functions."
  (let ((intent (make-intent)))
    ;; Request trade
    (request-trade-with-player intent 123)
    (assert (= (intent-requested-trade-target-id intent) 123) () "trade: target-id")
    ;; Request offer
    (request-trade-offer intent 2 10)
    (assert (= (intent-requested-trade-offer-slot intent) 2) () "trade: offer-slot")
    (assert (= (intent-requested-trade-offer-count intent) 10) () "trade: offer-count")
    ;; Confirm
    (request-trade-confirm intent)
    (assert (intent-requested-trade-confirm intent) () "trade: confirm set")
    ;; Cancel
    (request-trade-cancel intent)
    (assert (intent-requested-trade-cancel intent) () "trade: cancel set")
    ;; Clear all
    (clear-all-trade-requests intent)
    (assert (null (intent-requested-trade-target-id intent)) () "trade: cleared target")
    (assert (null (intent-requested-trade-confirm intent)) () "trade: cleared confirm")))

;;; ============================================================


(defvar *tests-intent*
  '(test-set-intent-move
    test-set-intent-face
    test-reset-frame-intent
    test-consume-intent-actions
    test-set-intent-target
    test-clear-intent-target
    test-request-pickup-target
    test-request-drop-item
    test-request-inventory-swap
    test-trade-intent-functions
    test-apply-intent-plist-rejects-bad-pickup)
  "Intent domain test functions.")
