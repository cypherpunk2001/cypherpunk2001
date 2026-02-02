(in-package :mmorpg)

;;; save-validate.lisp - Schema checks, bounds validation, data integrity checks
;;;
;;; Depends on save-serialize.lisp for: *save-format-version*

;;;; ========================================================================
;;;; SCHEMA VALIDATION (Phase 1 - Database Architecture Hardening)
;;;; See docs/db.md "Deserialization Validation" section
;;;; ========================================================================

(defparameter *max-player-blob-size* 65536
  "Maximum allowed size in bytes for serialized player data (64KB).
   Prevents memory exhaustion from malformed or malicious data.")

(defparameter *player-schema*
  '(;; Required fields with type and bounds
    (:id        :type integer  :required t :min 1)
    (:version   :type integer  :required nil :min 1 :max 100)
    (:x         :type number   :required t :min -1000000.0 :max 1000000.0)
    (:y         :type number   :required t :min -1000000.0 :max 1000000.0)
    (:hp        :type integer  :required t :min 0 :max 99999)
    (:lifetime-xp :type integer :required nil :min 0 :max 999999999)
    (:playtime  :type number   :required nil :min 0)
    (:created-at :type integer :required nil :min 0)
    (:deaths    :type integer  :required nil :min 0 :max 999999999)  ; Phase 4
    (:zone-id   :type symbol   :required nil)
    ;; Nested structures (validated separately)
    (:stats     :type plist    :required nil)
    (:inventory :type plist    :required nil)
    (:equipment :type plist    :required nil))
  "Schema definition for player data validation.
   Each entry: (field-name :type TYPE :required BOOL [:min MIN] [:max MAX])
   Types: integer, number (float or int), symbol, plist, string")

(defun validate-field-type (value expected-type)
  "Return T if VALUE matches EXPECTED-TYPE, NIL otherwise."
  (case expected-type
    (integer (integerp value))
    (number (numberp value))
    (symbol (symbolp value))
    (plist (or (null value) (listp value)))
    (string (stringp value))
    (t t)))  ; Unknown type passes

(defun validate-field-bounds (value min-val max-val)
  "Return T if VALUE is within [MIN-VAL, MAX-VAL] bounds (if specified)."
  (and (or (null min-val) (>= value min-val))
       (or (null max-val) (<= value max-val))))

(defun validate-player-plist (plist &key (log-errors t))
  "Validate PLIST against *player-schema*.
   Returns (values valid-p error-list) where:
   - valid-p is T if all validations pass
   - error-list is a list of error description strings

   When LOG-ERRORS is T (default), logs errors via WARN.

   Validation REJECTS invalid data entirely (returns NIL) rather than
   clamping values. This ensures data integrity - if something is wrong,
   we want to know about it rather than silently 'fixing' it."
  (let ((errors nil))
    ;; Check blob size first (prevent memory exhaustion)
    (let ((serialized-size (length (prin1-to-string plist))))
      (when (> serialized-size *max-player-blob-size*)
        (push (format nil "Blob size ~d exceeds max ~d bytes"
                      serialized-size *max-player-blob-size*)
              errors)))
    ;; Validate each schema field
    (dolist (field-spec *player-schema*)
      (let* ((field-name (car field-spec))
             (props (cdr field-spec))
             (expected-type (getf props :type))
             (required-p (getf props :required))
             (min-val (getf props :min))
             (max-val (getf props :max))
             (value (getf plist field-name))
             (present-p (member field-name plist)))
        ;; Check required fields
        (when (and required-p (not present-p))
          (push (format nil "Missing required field: ~a" field-name) errors))
        ;; Validate type if field is present
        (when (and present-p value)
          (unless (validate-field-type value expected-type)
            (push (format nil "Field ~a: expected ~a, got ~a (~s)"
                          field-name expected-type (type-of value) value)
                  errors))
          ;; Validate bounds for numeric types
          (when (and (numberp value) (or min-val max-val))
            (unless (validate-field-bounds value min-val max-val)
              (push (format nil "Field ~a: value ~a out of bounds [~a, ~a]"
                            field-name value
                            (or min-val "-inf")
                            (or max-val "+inf"))
                    errors))))))
    ;; Log errors if requested
    (when (and log-errors errors)
      (warn "Player data validation failed with ~d error(s):~%~{  - ~a~%~}"
            (length errors) (nreverse (copy-list errors))))
    ;; Return validation result
    (values (null errors) (nreverse errors))))

(defun validate-skill-plist (plist field-path)
  "Validate a skill plist (nested in stats). Returns error list."
  (let ((errors nil))
    (when plist
      (let ((level (getf plist :level))
            (xp (getf plist :xp)))
        (when (and level (not (integerp level)))
          (push (format nil "~a :level must be integer, got ~a" field-path (type-of level)) errors))
        (when (and level (integerp level) (or (< level 1) (> level 999)))
          (push (format nil "~a :level ~d out of bounds [1, 999]" field-path level) errors))
        (when (and xp (not (integerp xp)))
          (push (format nil "~a :xp must be integer, got ~a" field-path (type-of xp)) errors))
        (when (and xp (integerp xp) (< xp 0))
          (push (format nil "~a :xp ~d cannot be negative" field-path xp) errors))))
    errors))

(defun validate-stats-plist (plist)
  "Validate stats nested structure. Returns error list."
  (let ((errors nil))
    (when plist
      (dolist (stat-key '(:attack :strength :defense :hitpoints))
        (let ((skill-plist (getf plist stat-key)))
          (when skill-plist
            (setf errors (append errors
                                 (validate-skill-plist skill-plist
                                                       (format nil ":stats ~a" stat-key))))))))
    errors))

(defun validate-inventory-plist (plist)
  "Validate inventory nested structure. Returns error list."
  (let ((errors nil))
    (when plist
      (let ((slots (getf plist :slots)))
        (when (and slots (not (listp slots)))
          (push "Inventory :slots must be a list" errors))
        (when (listp slots)
          (loop for slot in slots
                for i from 0
                do (when slot
                     (let ((item-id (getf slot :item-id))
                           (count (getf slot :count)))
                       (when (and item-id (not (symbolp item-id)))
                         (push (format nil "Inventory slot ~d :item-id must be symbol" i) errors))
                       (when (and count (not (integerp count)))
                         (push (format nil "Inventory slot ~d :count must be integer" i) errors))
                       (when (and count (integerp count) (< count 0))
                         (push (format nil "Inventory slot ~d :count cannot be negative" i) errors))))))))
    errors))

(defun validate-player-plist-deep (plist &key (log-errors t))
  "Deep validation including nested structures (stats, inventory, equipment).
   Returns (values valid-p error-list)."
  ;; First do top-level validation
  (multiple-value-bind (top-valid top-errors)
      (validate-player-plist plist :log-errors nil)
    (let ((all-errors top-errors))
      ;; Validate nested stats
      (let ((stats (getf plist :stats)))
        (when stats
          (setf all-errors (append all-errors (validate-stats-plist stats)))))
      ;; Validate nested inventory
      (let ((inventory (getf plist :inventory)))
        (when inventory
          (setf all-errors (append all-errors (validate-inventory-plist inventory)))))
      ;; Log all errors if requested
      (when (and log-errors all-errors)
        (warn "Player data validation failed with ~d error(s):~%~{  - ~a~%~}"
              (length all-errors) all-errors))
      (values (null all-errors) all-errors))))

;;;; ========================================================================
;;;; 4-OUTCOME VALIDATION SYSTEM (Phase 6)
;;;; See docs/db.md "Phase 6: 4-Outcome Validation System" section
;;;; ========================================================================

(defparameter *default-spawn-x* 500.0
  "Default X spawn position for clamping invalid coordinates.")

(defparameter *default-spawn-y* 300.0
  "Default Y spawn position for clamping invalid coordinates.")

(defparameter *max-item-stack-size* 999999
  "Maximum stack size for any item. Exceeding this triggers :reject.")

;; clamp is already defined in utils.lisp

(defun validate-player-plist-4way (plist)
  "4-outcome validation for player data.
   Returns (values action issues fixed-plist) where:
   - action: :ok, :clamp, :quarantine, or :reject
   - issues: list of warning/error strings
   - fixed-plist: corrected data for :clamp, nil for others

   This function assumes PLIST has already been migrated to current schema.
   It validates against the current schema and classifies issues:
   - :ok       = Data is valid, no corrections needed
   - :clamp    = Minor issues fixed by safe coercions (HP out of range, etc.)
   - :quarantine = Suspicious data requiring admin review (unknown zones/items)
   - :reject   = Exploit-adjacent data, deny login (negative XP, dupes, etc.)

   Field Classification Policy (from docs/db.md):

   Clamp-Only (safe coercions):
   - :hp < 0 → 0
   - :hp > max-hp → max-hp (99999 schema limit)
   - :x, :y out of world bounds → spawn point
   - :playtime < 0 → 0
   - :deaths < 0 → 0
   - :created-at missing → current time

   Reject (exploit-adjacent):
   - :id missing → can't identify player
   - :lifetime-xp < 0 → exploit indicator
   - Inventory slot negative count → exploit indicator
   - Inventory slot count > max stack → possible dupe
   - Structure not a proper plist

   Quarantine (recoverable but suspicious):
   - :zone-id not a symbol → zone data corrupt
   - :zone-id unknown (not in *known-zone-ids*) → zone removed/renamed
   - Inventory item-id unknown (not in *item-archetypes*) → item deprecated
   - Equipment item-id unknown (not in *item-archetypes*) → item deprecated
   - :version much older than supported (future enhancement)"
  (let ((issues nil)
        (fixed-plist (copy-list plist))
        (needs-clamp nil)
        (needs-quarantine nil)
        (needs-reject nil))

    ;; === REJECT CHECKS (exploit-adjacent, check first) ===

    ;; Missing :id is fatal - can't identify player
    (let ((id (getf plist :id)))
      (unless (and id (integerp id) (> id 0))
        (push "Missing or invalid :id field" issues)
        (setf needs-reject t)))

    ;; Required field type checks - reject on wrong types (corruption/exploit)
    (let ((x (getf plist :x))
          (y (getf plist :y))
          (hp (getf plist :hp)))
      ;; :x must be a number
      (unless (numberp x)
        (push (format nil "Field :x has wrong type: expected number, got ~a (~s)"
                      (type-of x) x) issues)
        (setf needs-reject t))
      ;; :y must be a number
      (unless (numberp y)
        (push (format nil "Field :y has wrong type: expected number, got ~a (~s)"
                      (type-of y) y) issues)
        (setf needs-reject t))
      ;; :hp must be an integer
      (unless (integerp hp)
        (push (format nil "Field :hp has wrong type: expected integer, got ~a (~s)"
                      (type-of hp) hp) issues)
        (setf needs-reject t)))

    ;; Negative lifetime-xp is exploit indicator
    (let ((lifetime-xp (getf plist :lifetime-xp)))
      (when (and lifetime-xp (integerp lifetime-xp) (< lifetime-xp 0))
        (push (format nil "Negative lifetime-xp (~d) - exploit indicator" lifetime-xp) issues)
        (setf needs-reject t)))

    ;; Check inventory for exploits
    (let ((inventory (getf plist :inventory)))
      (when inventory
        ;; Guard: inventory must be a list to access nested fields
        (unless (listp inventory)
          (push (format nil "Field :inventory has wrong type: expected list, got ~a" (type-of inventory)) issues)
          (setf needs-reject t))
        (when (listp inventory)
          (let ((slots (getf inventory :slots)))
            ;; Guard: slots must be a list
            (unless (or (null slots) (listp slots))
              (push (format nil "Field :inventory :slots has wrong type: expected list, got ~a" (type-of slots)) issues)
              (setf needs-reject t))
            (when (listp slots)
              (loop for slot in slots
                    for i from 0
                    when slot
                    do (cond
                         ((not (listp slot))
                          (push (format nil "Inventory slot ~d is not a plist: ~s" i slot) issues)
                          (setf needs-reject t))
                         (t
                          (let ((count (getf slot :count))
                                (item-id (getf slot :item-id)))
                            ;; Negative count is exploit
                            (when (and count (integerp count) (< count 0))
                              (push (format nil "Inventory slot ~d has negative count (~d)" i count) issues)
                              (setf needs-reject t))
                            ;; Excessive count is potential dupe
                            (when (and count (integerp count) (> count *max-item-stack-size*))
                              (push (format nil "Inventory slot ~d count (~d) exceeds max (~d)"
                                            i count *max-item-stack-size*)
                                    issues)
                              (setf needs-reject t))
                            ;; Item-id must be a symbol if present
                            (when (and item-id (not (symbolp item-id)))
                              (push (format nil "Inventory slot ~d has non-symbol item-id: ~s" i item-id) issues)
                              (setf needs-reject t)))))))))))

    ;; Stats skill XP cannot be negative (exploit indicator)
    (let ((stats (getf plist :stats)))
      (when stats
        ;; Guard: stats must be a list to access nested fields
        (unless (listp stats)
          (push (format nil "Field :stats has wrong type: expected list, got ~a" (type-of stats)) issues)
          (setf needs-reject t))
        (when (listp stats)
          (dolist (stat-key '(:attack :strength :defense :hitpoints))
            (let ((skill (getf stats stat-key)))
              (when skill
                ;; Guard: each skill must be a list
                (unless (listp skill)
                  (push (format nil "Stats ~a has wrong type: expected list, got ~a" stat-key (type-of skill)) issues)
                  (setf needs-reject t))
                (when (listp skill)
                  (let ((xp (getf skill :xp)))
                    (when (and xp (integerp xp) (< xp 0))
                      (push (format nil "Stats ~a :xp is negative (~d) - exploit indicator" stat-key xp) issues)
                      (setf needs-reject t))))))))))

    ;; Equipment structure must be a plist with a list of :items
    (let ((equipment (getf plist :equipment)))
      (when equipment
        ;; Guard: equipment must be a list to access nested fields
        (unless (listp equipment)
          (push (format nil "Field :equipment has wrong type: expected list, got ~a" (type-of equipment)) issues)
          (setf needs-reject t))
        (when (listp equipment)
          (let ((items (getf equipment :items)))
            ;; Guard: items must be a list
            (unless (or (null items) (listp items))
              (push (format nil "Field :equipment :items has wrong type: expected list, got ~a"
                            (type-of items)) issues)
              (setf needs-reject t))))))

    ;; If any reject condition, return immediately
    (when needs-reject
      (return-from validate-player-plist-4way
        (values :reject issues nil)))

    ;; === QUARANTINE CHECKS (suspicious but recoverable) ===

    ;; Zone-id must be a symbol (if corrupt, quarantine)
    (let ((zone-id (getf plist :zone-id)))
      (when (and zone-id (not (symbolp zone-id)))
        (push (format nil "Zone-id is not a symbol: ~s" zone-id) issues)
        (setf needs-quarantine t))
      ;; Phase 6: Unknown zone check (zone removed/renamed)
      ;; Only check if *known-zone-ids* is populated (world-graph loaded)
      (when (and zone-id
                 (symbolp zone-id)
                 *known-zone-ids*
                 (not (gethash zone-id *known-zone-ids*)))
        (push (format nil "Unknown zone-id ~s (zone may have been removed)" zone-id) issues)
        (setf needs-quarantine t)))

    ;; Phase 6: Unknown item check (item deprecated)
    ;; Only check if game data is loaded
    (when *game-data-loaded-p*
      ;; Check inventory items
      (let ((inventory (getf plist :inventory)))
        (when (listp inventory)
          (let ((slots (getf inventory :slots)))
            (when (listp slots)
              (loop for slot in slots
                    for i from 0
                    when slot
                    do (let ((item-id (getf slot :item-id)))
                         (when (and item-id
                                    (symbolp item-id)
                                    (not (gethash item-id *item-archetypes*)))
                           (push (format nil "Unknown item-id ~s in inventory slot ~d (item may have been removed)"
                                         item-id i) issues)
                           (setf needs-quarantine t))))))))
      ;; Check equipment items (P2 fix: equipment can also reference deprecated items)
      (let ((equipment (getf plist :equipment)))
        (when (listp equipment)
          (let ((equip-items (getf equipment :items)))
            (when (listp equip-items)
              (loop for item-id in equip-items
                    for i from 0
                    when (and item-id (symbolp item-id))
                    do (unless (gethash item-id *item-archetypes*)
                         (push (format nil "Unknown item-id ~s in equipment slot ~d (item may have been removed)"
                                       item-id i) issues)
                         (setf needs-quarantine t))))))))

    ;; If any quarantine condition, return
    (when needs-quarantine
      (return-from validate-player-plist-4way
        (values :quarantine issues nil)))

    ;; === CLAMP CHECKS (safe coercions) ===
    ;; Phase 6: Use plist-put instead of setf getf to avoid PLIST_SETF_GETF_PITFALL

    ;; HP clamping: < 0 → 0, > max → max
    (let ((hp (getf fixed-plist :hp)))
      (when (and hp (numberp hp))
        (cond
          ((< hp 0)
           (push (format nil "HP ~d clamped to 0" hp) issues)
           (setf fixed-plist (plist-put fixed-plist :hp 0))
           (setf needs-clamp t))
          ((> hp 99999)
           (push (format nil "HP ~d clamped to 99999" hp) issues)
           (setf fixed-plist (plist-put fixed-plist :hp 99999))
           (setf needs-clamp t)))))

    ;; Position clamping: out of bounds → spawn point
    (let ((x (getf fixed-plist :x))
          (y (getf fixed-plist :y)))
      (when (and x (numberp x))
        (when (or (< x -1000000.0) (> x 1000000.0))
          (push (format nil "X position ~f clamped to spawn ~f" x *default-spawn-x*) issues)
          (setf fixed-plist (plist-put fixed-plist :x *default-spawn-x*))
          (setf needs-clamp t)))
      (when (and y (numberp y))
        (when (or (< y -1000000.0) (> y 1000000.0))
          (push (format nil "Y position ~f clamped to spawn ~f" y *default-spawn-y*) issues)
          (setf fixed-plist (plist-put fixed-plist :y *default-spawn-y*))
          (setf needs-clamp t))))

    ;; Playtime clamping: < 0 → 0.0
    (let ((playtime (getf fixed-plist :playtime)))
      (when (and playtime (numberp playtime) (< playtime 0))
        (push (format nil "Playtime ~f clamped to 0" playtime) issues)
        (setf fixed-plist (plist-put fixed-plist :playtime 0.0))
        (setf needs-clamp t)))

    ;; Deaths clamping: < 0 → 0
    (let ((deaths (getf fixed-plist :deaths)))
      (when (and deaths (integerp deaths) (< deaths 0))
        (push (format nil "Deaths ~d clamped to 0" deaths) issues)
        (setf fixed-plist (plist-put fixed-plist :deaths 0))
        (setf needs-clamp t)))

    ;; Created-at: missing → current time
    (unless (getf fixed-plist :created-at)
      (push "Missing :created-at, set to current time" issues)
      (setf fixed-plist (plist-put fixed-plist :created-at (get-universal-time)))
      (setf needs-clamp t))

    ;; Ensure version is set to current schema version (validator contract)
    (setf fixed-plist (plist-put fixed-plist :version *player-schema-version*))

    ;; Return result based on what we found
    (if needs-clamp
        (values :clamp issues fixed-plist)
        (values :ok issues nil))))
