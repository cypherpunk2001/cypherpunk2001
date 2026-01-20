;; NOTE: If you change behavior here, update docs/trade.md :)
(in-package #:mmorpg)

;;; trade.lisp - Player-to-player trading system (Phase 5)
;;;
;;; Implements secure, atomic item trading between players.
;;; All trade operations are validated server-side with Redis Lua scripts
;;; ensuring atomicity even in crash scenarios.
;;;
;;; Key concepts:
;;; - Trade sessions are ephemeral (in-memory, not persisted)
;;; - Both players must confirm before execution
;;; - Atomic swap via Lua script prevents item duplication/loss
;;; - Proximity check: players must be within 10 tiles
;;; - Inactivity timeout: 60 seconds

;;;; ========================================================================
;;;; Configuration
;;;; ========================================================================

(defparameter *trade-max-distance-tiles* 10
  "Maximum tile distance between trading players.")

(defparameter *trade-timeout-seconds* 60
  "Trade session times out after this many seconds of inactivity.")

(defparameter *trade-max-offer-slots* 12
  "Maximum number of inventory slots that can be offered in a trade.")

;;;; ========================================================================
;;;; Trade Session Structure
;;;; ========================================================================

(defstruct trade-offer
  "Represents one player's offer in a trade."
  (slots (make-hash-table) :type hash-table)  ; slot-index -> count
  (confirmed nil :type boolean)               ; player confirmed their offer
  (last-activity 0.0 :type float))            ; internal-real-time of last action

(defstruct trade-session
  "An active trade between two players. Ephemeral, not persisted."
  (id 0 :type fixnum)
  (player1-id 0 :type fixnum)
  (player2-id 0 :type fixnum)
  (player1-offer (make-trade-offer) :type trade-offer)
  (player2-offer (make-trade-offer) :type trade-offer)
  (created-at 0.0 :type float)
  (state :pending :type symbol))  ; :pending, :both-confirmed, :executing, :completed, :cancelled

;;;; ========================================================================
;;;; Active Trades Registry
;;;; ========================================================================

(defparameter *active-trades* (make-hash-table)
  "Hash table: trade-id -> trade-session")

(defparameter *player-trade-map* (make-hash-table)
  "Hash table: player-id -> trade-id (for quick lookup)")

(defparameter *next-trade-id* 1
  "Counter for generating unique trade IDs.")

(defun allocate-trade-id ()
  "Allocate a unique trade session ID."
  (prog1 *next-trade-id*
    (incf *next-trade-id*)))

(defun get-player-trade (player-id)
  "Get the active trade session for a player, or NIL if not trading."
  (let ((trade-id (gethash player-id *player-trade-map*)))
    (when trade-id
      (gethash trade-id *active-trades*))))

(defun player-in-trade-p (player-id)
  "Check if player is currently in a trade."
  (not (null (gethash player-id *player-trade-map*))))

;;;; ========================================================================
;;;; Trade Validation
;;;; ========================================================================

(defun players-within-trade-distance-p (player1 player2)
  "Check if two players are close enough to trade."
  (let* ((dx (- (player-x player1) (player-x player2)))
         (dy (- (player-y player1) (player-y player2)))
         (tile-size 16.0)  ; standard tile size
         (max-dist (* *trade-max-distance-tiles* tile-size))
         (dist-sq (+ (* dx dx) (* dy dy)))
         (max-dist-sq (* max-dist max-dist)))
    (<= dist-sq max-dist-sq)))

(defun players-same-zone-p (player1 player2)
  "Check if two players are in the same zone."
  (eq (player-zone-id player1) (player-zone-id player2)))

(defun validate-trade-request (initiator target)
  "Validate that a trade can be initiated between INITIATOR and TARGET.
   Returns (values valid-p error-message)."
  (cond
    ;; Check players aren't the same
    ((= (player-id initiator) (player-id target))
     (values nil "Cannot trade with yourself"))
    ;; Check neither player is already trading
    ((player-in-trade-p (player-id initiator))
     (values nil "You are already in a trade"))
    ((player-in-trade-p (player-id target))
     (values nil "That player is already trading"))
    ;; Check same zone
    ((not (players-same-zone-p initiator target))
     (values nil "Player is in a different zone"))
    ;; Check distance
    ((not (players-within-trade-distance-p initiator target))
     (values nil "Player is too far away"))
    ;; All checks passed
    (t (values t nil))))

(defun validate-trade-offer-slot (player slot-index count)
  "Validate that PLAYER can offer COUNT items from SLOT-INDEX.
   Returns (values valid-p error-message)."
  (let* ((inventory (player-inventory player))
         (slots (inventory-slots inventory))
         (slot (when (and (>= slot-index 0) (< slot-index (length slots)))
                 (aref slots slot-index))))
    (cond
      ((null slot)
       (values nil "Invalid inventory slot"))
      ((inventory-slot-empty-p slot)
       (values nil "Slot is empty"))
      ((or (< count 1) (> count (inventory-slot-count slot)))
       (values nil "Invalid item count"))
      (t (values t nil)))))

;;;; ========================================================================
;;;; Trade Session Management
;;;; ========================================================================

(defun create-trade-session (player1-id player2-id)
  "Create a new trade session between two players."
  (let* ((trade-id (allocate-trade-id))
         (now (float (get-internal-real-time) 1.0))
         (session (make-trade-session
                   :id trade-id
                   :player1-id player1-id
                   :player2-id player2-id
                   :player1-offer (make-trade-offer :last-activity now)
                   :player2-offer (make-trade-offer :last-activity now)
                   :created-at now
                   :state :pending)))
    ;; Register in both tables
    (setf (gethash trade-id *active-trades*) session)
    (setf (gethash player1-id *player-trade-map*) trade-id)
    (setf (gethash player2-id *player-trade-map*) trade-id)
    (log-verbose "Created trade session ~a between players ~a and ~a"
                 trade-id player1-id player2-id)
    session))

(defun cancel-trade-session (session &optional (reason "cancelled"))
  "Cancel a trade session and clean up registrations."
  (when session
    (let ((trade-id (trade-session-id session))
          (p1-id (trade-session-player1-id session))
          (p2-id (trade-session-player2-id session)))
      (setf (trade-session-state session) :cancelled)
      (remhash trade-id *active-trades*)
      (remhash p1-id *player-trade-map*)
      (remhash p2-id *player-trade-map*)
      (log-verbose "Cancelled trade session ~a: ~a" trade-id reason))))

(defun get-player-offer (session player-id)
  "Get the offer struct for a player in a trade session."
  (cond
    ((= player-id (trade-session-player1-id session))
     (trade-session-player1-offer session))
    ((= player-id (trade-session-player2-id session))
     (trade-session-player2-offer session))
    (t nil)))

(defun get-other-player-id (session player-id)
  "Get the ID of the other player in a trade session."
  (if (= player-id (trade-session-player1-id session))
      (trade-session-player2-id session)
      (trade-session-player1-id session)))

(defun update-trade-activity (session player-id)
  "Update the last activity time for a player's offer."
  (let ((offer (get-player-offer session player-id)))
    (when offer
      (setf (trade-offer-last-activity offer)
            (float (get-internal-real-time) 1.0)))))

;;;; ========================================================================
;;;; Trade Offer Manipulation
;;;; ========================================================================

(defun add-to-trade-offer (session player-id slot-index count)
  "Add items from SLOT-INDEX to a player's trade offer.
   Modifying offer resets both players' confirmations.
   Returns T on success, NIL on failure."
  (let ((offer (get-player-offer session player-id)))
    (when offer
      ;; Modifying offer unconfirms both players
      (setf (trade-offer-confirmed (trade-session-player1-offer session)) nil)
      (setf (trade-offer-confirmed (trade-session-player2-offer session)) nil)
      (setf (trade-session-state session) :pending)
      ;; Update offer
      (setf (gethash slot-index (trade-offer-slots offer)) count)
      (update-trade-activity session player-id)
      (log-verbose "Player ~a added slot ~a (count ~a) to trade ~a"
                   player-id slot-index count (trade-session-id session))
      t)))

(defun remove-from-trade-offer (session player-id slot-index)
  "Remove items at SLOT-INDEX from a player's trade offer.
   Modifying offer resets both players' confirmations."
  (let ((offer (get-player-offer session player-id)))
    (when offer
      ;; Modifying offer unconfirms both players
      (setf (trade-offer-confirmed (trade-session-player1-offer session)) nil)
      (setf (trade-offer-confirmed (trade-session-player2-offer session)) nil)
      (setf (trade-session-state session) :pending)
      ;; Update offer
      (remhash slot-index (trade-offer-slots offer))
      (update-trade-activity session player-id)
      t)))

(defun confirm-trade-offer (session player-id)
  "Confirm a player's trade offer. Returns T if both players confirmed."
  (let ((offer (get-player-offer session player-id)))
    (when offer
      (setf (trade-offer-confirmed offer) t)
      (update-trade-activity session player-id)
      (log-verbose "Player ~a confirmed trade ~a" player-id (trade-session-id session))
      ;; Check if both confirmed
      (when (and (trade-offer-confirmed (trade-session-player1-offer session))
                 (trade-offer-confirmed (trade-session-player2-offer session)))
        (setf (trade-session-state session) :both-confirmed)
        (log-verbose "Trade ~a: both players confirmed" (trade-session-id session))
        t))))

;;;; ========================================================================
;;;; Trade Execution (Atomic Operations)
;;;; ========================================================================

(defun compute-post-trade-inventory (player offer-given offer-received)
  "Compute the player's inventory after trade execution.
   OFFER-GIVEN: hash-table of slot-index -> count (items player gives away)
   OFFER-RECEIVED: list of (item-id count) pairs (items player receives)
   Returns the updated player plist ready for serialization, or NIL on error."
  (let* ((inventory (player-inventory player))
         (slots (inventory-slots inventory))
         (new-slots (make-array (length slots))))
    ;; Copy current inventory
    (dotimes (i (length slots))
      (let ((slot (aref slots i)))
        (setf (aref new-slots i)
              (make-inventory-slot :item-id (inventory-slot-item-id slot)
                                   :count (inventory-slot-count slot)))))
    ;; Remove given items
    (maphash (lambda (slot-index count)
               (let ((slot (aref new-slots slot-index)))
                 (when slot
                   (decf (inventory-slot-count slot) count)
                   (when (<= (inventory-slot-count slot) 0)
                     (setf (inventory-slot-item-id slot) nil
                           (inventory-slot-count slot) 0)))))
             offer-given)
    ;; Add received items (find empty slots or stack)
    (dolist (item offer-received)
      (let ((item-id (first item))
            (count (second item)))
        (unless (add-items-to-inventory-array new-slots item-id count)
          ;; Failed to add items - inventory full
          (warn "Trade failed: inventory full for player ~a" (player-id player))
          (return-from compute-post-trade-inventory nil))))
    ;; Create updated player with new inventory for serialization
    (let ((updated-player (copy-player player)))
      (setf (inventory-slots (player-inventory updated-player)) new-slots)
      updated-player)))

(defun add-items-to-inventory-array (slots item-id count)
  "Add COUNT of ITEM-ID to inventory SLOTS array. Returns T on success."
  (let ((stack-size (item-stack-size item-id))
        (remaining count))
    ;; First try to stack with existing items
    (dotimes (i (length slots))
      (let ((slot (aref slots i)))
        (when (and (eq (inventory-slot-item-id slot) item-id)
                   (< (inventory-slot-count slot) stack-size))
          (let ((can-add (min remaining (- stack-size (inventory-slot-count slot)))))
            (incf (inventory-slot-count slot) can-add)
            (decf remaining can-add)
            (when (<= remaining 0)
              (return-from add-items-to-inventory-array t))))))
    ;; Then fill empty slots
    (dotimes (i (length slots))
      (let ((slot (aref slots i)))
        (when (inventory-slot-empty-p slot)
          (let ((can-add (min remaining stack-size)))
            (setf (inventory-slot-item-id slot) item-id
                  (inventory-slot-count slot) can-add)
            (decf remaining can-add)
            (when (<= remaining 0)
              (return-from add-items-to-inventory-array t))))))
    ;; Failed - not enough space
    nil))

(defun offer-to-item-list (player offer)
  "Convert a trade-offer's slot hash to a list of (item-id count) pairs."
  (let* ((inventory (player-inventory player))
         (slots (inventory-slots inventory))
         (items nil))
    (maphash (lambda (slot-index count)
               (let ((slot (aref slots slot-index)))
                 (when slot
                   (push (list (inventory-slot-item-id slot) count) items))))
             (trade-offer-slots offer))
    items))

(defun execute-trade-atomic (session player1 player2)
  "Execute the trade atomically using Redis Lua script.
   Returns T on success, NIL on failure (trade cancelled on failure)."
  (setf (trade-session-state session) :executing)
  (let* ((offer1 (trade-session-player1-offer session))
         (offer2 (trade-session-player2-offer session))
         ;; What each player gives
         (p1-gives (trade-offer-slots offer1))
         (p2-gives (trade-offer-slots offer2))
         ;; What each player receives (the other's offer)
         (p1-receives (offer-to-item-list player2 offer2))
         (p2-receives (offer-to-item-list player1 offer1)))
    ;; Compute post-trade inventories
    (let ((updated-p1 (compute-post-trade-inventory player1 p1-gives p1-receives))
          (updated-p2 (compute-post-trade-inventory player2 p2-gives p2-receives)))
      (unless (and updated-p1 updated-p2)
        (cancel-trade-session session "inventory error")
        (return-from execute-trade-atomic nil))
      ;; Look up zone-ids from sessions (same pattern as flush-dirty-players)
      (let* ((session-1 (gethash (player-id player1) *player-sessions*))
             (session-2 (gethash (player-id player2) *player-sessions*))
             (zone-1 (and session-1 (player-session-zone-id session-1)))
             (zone-2 (and session-2 (player-session-zone-id session-2))))
        ;; Serialize with version tagging (Phase F: trade version tagging)
        (let* ((data-1 (serialize-player updated-p1 :include-visuals nil :zone-id zone-1))
               (data-2 (serialize-player updated-p2 :include-visuals nil :zone-id zone-2)))
          ;; Add version before stringifying (same pattern as flush-dirty-players)
          (setf data-1 (plist-put data-1 :version *player-schema-version*))
          (setf data-2 (plist-put data-2 :version *player-schema-version*))
          (let ((p1-data (prin1-to-string data-1))
                (p2-data (prin1-to-string data-2))
                (p1-key (player-key (player-id player1)))
                (p2-key (player-key (player-id player2))))
        ;; Execute atomic swap
        (let ((result (storage-eval-script *storage* "trade_complete"
                                           (list p1-key p2-key)
                                           (list p1-data p2-data))))
          (if (and result (string= result "OK"))
              (progn
                ;; Update in-memory player state
                (setf (inventory-slots (player-inventory player1))
                      (inventory-slots (player-inventory updated-p1)))
                (setf (inventory-slots (player-inventory player2))
                      (inventory-slots (player-inventory updated-p2)))
                (setf (trade-session-state session) :completed)
                (log-verbose "Trade ~a completed successfully" (trade-session-id session))
                ;; Clean up registrations
                (remhash (trade-session-id session) *active-trades*)
                (remhash (player-id player1) *player-trade-map*)
                (remhash (player-id player2) *player-trade-map*)
                t)
              (progn
                (warn "Trade ~a atomic execution failed: ~a"
                      (trade-session-id session) result)
                (cancel-trade-session session "atomic execution failed")
                nil)))))))))

;;;; ========================================================================
;;;; Trade Timeout Handling
;;;; ========================================================================

(defun check-trade-timeout (session)
  "Check if a trade session has timed out. Returns T if timed out."
  (let* ((now (float (get-internal-real-time) 1.0))
         (timeout-ms (* *trade-timeout-seconds* internal-time-units-per-second))
         (offer1 (trade-session-player1-offer session))
         (offer2 (trade-session-player2-offer session))
         (last-activity (max (trade-offer-last-activity offer1)
                             (trade-offer-last-activity offer2))))
    (> (- now last-activity) timeout-ms)))

(defun cleanup-timed-out-trades ()
  "Clean up all timed out trade sessions. Call periodically from server tick."
  (let ((to-cancel nil))
    (maphash (lambda (trade-id session)
               (declare (ignore trade-id))
               (when (check-trade-timeout session)
                 (push session to-cancel)))
             *active-trades*)
    (dolist (session to-cancel)
      (cancel-trade-session session "timeout"))))

;;;; ========================================================================
;;;; Server-Side Intent Processing
;;;; ========================================================================

(defun process-trade-intents (player players)
  "Process trade-related intents for PLAYER. Called from server tick."
  (let ((intent (player-intent player))
        (player-id (player-id player)))
    ;; Handle trade request
    (let ((target-id (intent-requested-trade-target-id intent)))
      (when target-id
        (clear-requested-trade-target intent)
        (let ((target (find-player-by-id players target-id)))
          (when target
            (multiple-value-bind (valid-p error-msg)
                (validate-trade-request player target)
              (if valid-p
                  (create-trade-session player-id target-id)
                  (log-verbose "Trade request denied: ~a" error-msg)))))))
    ;; Handle trade cancel
    (when (intent-requested-trade-cancel intent)
      (clear-requested-trade-cancel intent)
      (let ((session (get-player-trade player-id)))
        (when session
          (cancel-trade-session session "player cancelled"))))
    ;; Handle trade offer
    (let ((slot-index (intent-requested-trade-offer-slot intent))
          (count (intent-requested-trade-offer-count intent)))
      (when slot-index
        (clear-requested-trade-offer intent)
        (let ((session (get-player-trade player-id)))
          (when session
            (if (zerop count)
                (remove-from-trade-offer session player-id slot-index)
                (multiple-value-bind (valid-p error-msg)
                    (validate-trade-offer-slot player slot-index count)
                  (if valid-p
                      (add-to-trade-offer session player-id slot-index count)
                      (log-verbose "Trade offer denied: ~a" error-msg))))))))
    ;; Handle trade confirm
    (when (intent-requested-trade-confirm intent)
      (clear-requested-trade-confirm intent)
      (let ((session (get-player-trade player-id)))
        (when session
          (when (confirm-trade-offer session player-id)
            ;; Both confirmed - execute trade
            (let* ((p1-id (trade-session-player1-id session))
                   (p2-id (trade-session-player2-id session))
                   (player1 (find-player-by-id players p1-id))
                   (player2 (find-player-by-id players p2-id)))
              (when (and player1 player2)
                (execute-trade-atomic session player1 player2)))))))))

;;;; ========================================================================
;;;; Script Loading (called at server startup)
;;;; ========================================================================

(defun load-trade-scripts ()
  "Load trade-related Lua scripts into Redis. Call at server startup."
  (when (and *storage* (typep *storage* 'redis-storage))
    (let ((script-path "data/redis-scripts/trade_complete.lua"))
      (when (probe-file script-path)
        (let ((script-body (uiop:read-file-string script-path)))
          (storage-script-load *storage* "trade_complete" script-body))))))

;;;; ========================================================================
;;;; Exports
;;;; ========================================================================

(export '(*trade-max-distance-tiles*
          *trade-timeout-seconds*
          trade-session
          get-player-trade
          player-in-trade-p
          create-trade-session
          cancel-trade-session
          process-trade-intents
          cleanup-timed-out-trades
          load-trade-scripts))
