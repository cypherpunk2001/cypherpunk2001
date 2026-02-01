(in-package :mmorpg)

;;; save.lisp - Save/load game state serialization
;;;
;;; Provides deterministic snapshot serialization for:
;;; - Player state (position, HP, stats, inventory, equipment, timers)
;;; - NPC state (position, HP, respawn timers, behavior)
;;; - Object state (counts, respawn timers)
;;; - World context (zone ID, entity ID source)
;;;
;;; Format is versioned plist for future-proof migrations.
;;; This becomes the snapshot sync format for future networking.

(defparameter *save-format-version* 2
  "Current save file format version for migration support.")

;;; Vector Pool for Compact Serialization (Task 4.2)
;;; Reduces allocation during hot snapshot path.

(defparameter *player-compact-vector-size* 20
  "Size of compact player serialization vector (indices 0-19).")

(defparameter *npc-compact-vector-size* 15
  "Size of compact NPC serialization vector (indices 0-14).")

(defparameter *player-vector-pool-capacity* 600
  "Initial capacity for player vector pool. Sized for ~500 players per zone + headroom.")

(defvar *player-vector-pool* nil
  "Global vector pool for player compact serialization. Created lazily on first use.")

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

;;; Serialization helpers

(defun serialize-skill (skill)
  ;; Convert skill to plist.
  (when skill
    (list :level (skill-level skill)
          :xp (skill-xp skill))))

(defun deserialize-skill (plist)
  ;; Restore skill from plist.
  (when plist
    (make-skill :level (getf plist :level 1)
                :xp (getf plist :xp 0))))

(defun serialize-stat-block (stats)
  ;; Convert stat-block to plist.
  (when stats
    (list :attack (serialize-skill (stat-block-attack stats))
          :strength (serialize-skill (stat-block-strength stats))
          :defense (serialize-skill (stat-block-defense stats))
          :hitpoints (serialize-skill (stat-block-hitpoints stats)))))

(defun deserialize-stat-block (plist)
  ;; Restore stat-block from plist.
  (when plist
    (make-stat-block
     :attack (deserialize-skill (getf plist :attack))
     :strength (deserialize-skill (getf plist :strength))
     :defense (deserialize-skill (getf plist :defense))
     :hitpoints (deserialize-skill (getf plist :hitpoints)))))

(defun serialize-inventory-slot (slot)
  ;; Convert inventory slot to plist.
  (when slot
    (list :item-id (inventory-slot-item-id slot)
          :count (inventory-slot-count slot))))

(defun deserialize-inventory-slot (plist)
  ;; Restore inventory slot from plist.
  (when plist
    (make-inventory-slot
     :item-id (getf plist :item-id)
     :count (getf plist :count 0))))

(defun serialize-inventory (inventory)
  ;; Convert inventory to plist.
  (when inventory
    (let* ((slots (inventory-slots inventory))
           (slot-count (and slots (length slots)))
           (slot-list nil))
      (when slots
        (loop :for i :below slot-count
              :for slot = (aref slots i)
              :when slot
                :do (push (serialize-inventory-slot slot) slot-list)))
      (list :slots (nreverse slot-list)))))

(defun deserialize-inventory (plist size)
  ;; Restore inventory from plist with SIZE slots.
  (let ((slots (make-array size :initial-element nil)))
    (when plist
      (let ((slot-list (getf plist :slots)))
        (loop :for slot-plist :in slot-list
              :for i :from 0
              :when (< i size)
                :do (setf (aref slots i) (deserialize-inventory-slot slot-plist)))))
    (%make-inventory :slots slots)))

(defun serialize-equipment (equipment)
  ;; Convert equipment to plist (array of item IDs).
  (when equipment
    (let* ((items (equipment-items equipment))
           (item-list (and items (coerce items 'list))))
      (list :items item-list))))

(defun deserialize-equipment (plist size)
  ;; Restore equipment from plist with SIZE slots.
  (let ((items (make-array size :initial-element nil)))
    (when plist
      (let ((item-list (getf plist :items)))
        (loop :for item-id :in item-list
              :for i :from 0
              :when (< i size)
                :do (setf (aref items i) item-id))))
    (%make-equipment :items items)))

(defun serialize-player (player &key (include-visuals nil) (zone-id nil) (network-only nil))
  ;; Convert player state to plist.
  ;; Modes:
  ;;   - Default: Full durable state for DB saves (inventory, equipment, stats, etc.)
  ;;   - :include-visuals t: Add visual/ephemeral fields for rendering
  ;;   - :network-only t: Minimal state for network snapshots (position + visuals only)
  ;;                      Excludes inventory/equipment/stats to reduce snapshot size.
  ;;                      Critical for 60+ player scalability (72KB -> ~15KB).
  (let ((payload
          (if network-only
              ;; NETWORK-ONLY: Minimal state for rendering other players
              ;; Excludes: inventory, equipment, stats, playtime, created-at, lifetime-xp
              (list :id (player-id player)
                    :x (player-x player)
                    :y (player-y player)
                    :hp (player-hp player)
                    :dx (player-dx player)
                    :dy (player-dy player)
                    :anim-state (player-anim-state player)
                    :facing (player-facing player)
                    :facing-sign (player-facing-sign player)
                    :frame-index (player-frame-index player)
                    :frame-timer (player-frame-timer player)
                    :attacking (player-attacking player)
                    :attack-hit (player-attack-hit player)
                    :hit-active (player-hit-active player)
                    :hit-frame (player-hit-frame player)
                    :hit-facing (player-hit-facing player)
                    :hit-facing-sign (player-hit-facing-sign player)
                    :running (player-running player)
                    :attack-timer (player-attack-timer player)
                    :hit-timer (player-hit-timer player)
                    :run-stamina (player-run-stamina player)
                    :last-sequence (player-last-sequence player))
              ;; FULL: Complete durable state for DB saves
              (list :id (player-id player)
                    :x (player-x player)
                    :y (player-y player)
                    :hp (player-hp player)
                    :lifetime-xp (player-lifetime-xp player)
                    :playtime (player-playtime player)
                    :created-at (player-created-at player)
                    :deaths (player-deaths player)  ; Phase 4 - leaderboards
                    :stats (serialize-stat-block (player-stats player))
                    :inventory (serialize-inventory (player-inventory player))
                    :equipment (serialize-equipment (player-equipment player))))))
    ;; Add zone-id to durable state (for DB saves)
    ;; Use player's zone-id if available, otherwise use keyword argument
    (let ((effective-zone-id (or (player-zone-id player) zone-id)))
      (when (and effective-zone-id (not network-only))
        (setf payload (append payload (list :zone-id effective-zone-id)))))
    ;; Add visual fields if requested (for full snapshots, not network-only which already has them)
    (when (and include-visuals (not network-only))
      (setf payload (append payload
                            (list :dx (player-dx player)
                                  :dy (player-dy player)
                                  :anim-state (player-anim-state player)
                                  :facing (player-facing player)
                                  :facing-sign (player-facing-sign player)
                                  :frame-index (player-frame-index player)
                                  :frame-timer (player-frame-timer player)
                                  :attacking (player-attacking player)
                                  :attack-hit (player-attack-hit player)
                                  :hit-active (player-hit-active player)
                                  :hit-frame (player-hit-frame player)
                                  :hit-facing (player-hit-facing player)
                                  :hit-facing-sign (player-hit-facing-sign player)
                                  :running (player-running player)
                                  ;; Ephemeral fields (for network snapshots only, not DB)
                                  :attack-timer (player-attack-timer player)
                                  :hit-timer (player-hit-timer player)
                                  :run-stamina (player-run-stamina player)))))
    payload))

(defun deserialize-player (plist inventory-size equipment-size)
  ;; Restore player from plist.
  (let ((player (make-player (getf plist :x 0.0)
                             (getf plist :y 0.0)
                             :id (getf plist :id 1)
                             :zone-id (getf plist :zone-id))))
    (setf (player-hp player) (getf plist :hp 10)
          (player-zone-id player) (getf plist :zone-id)  ; Also set explicitly for clarity
          (player-lifetime-xp player) (getf plist :lifetime-xp 0)
          (player-playtime player) (float (getf plist :playtime 0) 1.0)
          (player-created-at player) (getf plist :created-at (get-universal-time))
          (player-deaths player) (getf plist :deaths 0)  ; Phase 4 - leaderboards
          (player-stats player) (deserialize-stat-block (getf plist :stats))
          (player-inventory player) (deserialize-inventory (getf plist :inventory) inventory-size)
          (player-equipment player) (deserialize-equipment (getf plist :equipment) equipment-size)
          (player-attack-timer player) (getf plist :attack-timer 0.0)
          (player-hit-timer player) (getf plist :hit-timer 0.0)
          (player-run-stamina player) (getf plist :run-stamina 1.0)
          (player-attack-target-id player) (getf plist :attack-target-id 0)
          (player-follow-target-id player) (getf plist :follow-target-id 0)
          (player-last-sequence player) (getf plist :last-sequence 0)
          (player-dx player) (getf plist :dx 0.0)
          (player-dy player) (getf plist :dy 0.0)
          (player-anim-state player) (getf plist :anim-state :idle)
          (player-facing player) (getf plist :facing :down)
          (player-facing-sign player) (getf plist :facing-sign 1.0)
          (player-frame-index player) (getf plist :frame-index 0)
          (player-frame-timer player) (getf plist :frame-timer 0.0)
          (player-attacking player) (getf plist :attacking nil)
          (player-attack-hit player) (getf plist :attack-hit nil)
          (player-hit-active player) (getf plist :hit-active nil)
          (player-hit-frame player) (getf plist :hit-frame 0)
          (player-hit-facing player) (getf plist :hit-facing :down)
          (player-hit-facing-sign player) (getf plist :hit-facing-sign 1.0)
          (player-running player) (getf plist :running nil))
    player))

(defun apply-player-plist (player plist)
  ;; Apply plist fields onto an existing PLAYER, preserving client-only state.
  ;; For network-only snapshots, inventory/equipment/stats may be nil - preserve existing.
  (when (and player plist)
    ;; Save client-only state before applying server snapshot
    (let ((saved-click-x (player-click-marker-x player))
          (saved-click-y (player-click-marker-y player))
          (saved-click-kind (player-click-marker-kind player))
          (saved-click-timer (player-click-marker-timer player))
          (saved-mouse-timer (player-mouse-hold-timer player))
          (saved-auto-right (player-auto-right player))
          (saved-auto-left (player-auto-left player))
          (saved-auto-down (player-auto-down player))
          (saved-auto-up (player-auto-up player))
          ;; Check which heavy fields are present in plist
          (has-stats (getf plist :stats))
          (has-inventory (getf plist :inventory))
          (has-equipment (getf plist :equipment)))
      ;; NOTE: player-id is NOT set here - IDs are immutable after creation
      ;; (avoids index map desync; plist :id is used for lookup, not to change identity)
      (setf (player-x player) (getf plist :x (player-x player))
            (player-y player) (getf plist :y (player-y player))
            (player-dx player) (getf plist :dx 0.0)
            (player-dy player) (getf plist :dy 0.0)
            (player-hp player) (getf plist :hp (player-hp player))
            (player-lifetime-xp player) (getf plist :lifetime-xp (player-lifetime-xp player))
            (player-playtime player) (getf plist :playtime (player-playtime player))
            (player-created-at player) (getf plist :created-at (player-created-at player))
            ;; Phase 5: Apply deaths from snapshot for client-side display
            (player-deaths player) (getf plist :deaths (player-deaths player))
            (player-attack-timer player) (getf plist :attack-timer 0.0)
            (player-hit-timer player) (getf plist :hit-timer 0.0)
            (player-run-stamina player) (getf plist :run-stamina 1.0)
            (player-attack-target-id player) (getf plist :attack-target-id 0)
            (player-follow-target-id player) (getf plist :follow-target-id 0)
            (player-anim-state player) (getf plist :anim-state :idle)
            (player-facing player) (getf plist :facing :down)
            (player-facing-sign player) (getf plist :facing-sign 1.0)
            (player-frame-index player) (getf plist :frame-index 0)
            (player-frame-timer player) (getf plist :frame-timer 0.0)
            (player-attacking player) (getf plist :attacking nil)
            (player-attack-hit player) (getf plist :attack-hit nil)
            (player-hit-active player) (getf plist :hit-active nil)
            (player-hit-frame player) (getf plist :hit-frame 0)
            (player-hit-facing player) (getf plist :hit-facing :down)
            (player-hit-facing-sign player) (getf plist :hit-facing-sign 1.0)
            (player-running player) (getf plist :running nil)
            (player-last-sequence player) (getf plist :last-sequence (player-last-sequence player)))
      ;; Only update heavy fields if present (network-only snapshots exclude these)
      (when has-stats
        (setf (player-stats player) (deserialize-stat-block has-stats)))
      (when has-inventory
        (setf (player-inventory player) (deserialize-inventory has-inventory *inventory-size*)))
      (when has-equipment
        (setf (player-equipment player) (deserialize-equipment has-equipment (length *equipment-slot-ids*))))
      ;; Restore client-only state after applying server snapshot
      (setf (player-click-marker-x player) saved-click-x
            (player-click-marker-y player) saved-click-y
            (player-click-marker-kind player) saved-click-kind
            (player-click-marker-timer player) saved-click-timer
            (player-mouse-hold-timer player) saved-mouse-timer
            (player-auto-right player) saved-auto-right
            (player-auto-left player) saved-auto-left
            (player-auto-down player) saved-auto-down
            (player-auto-up player) saved-auto-up)))
  player)

(defun serialize-player-private (player)
  ;; Serialize private player state (inventory/equipment/stats) for owner-only updates.
  (when player
    (list :stats (serialize-stat-block (player-stats player))
          :inventory (serialize-inventory (player-inventory player))
          :equipment (serialize-equipment (player-equipment player)))))

(defun apply-player-private-plist (player plist)
  ;; Apply private player state from PLIST onto PLAYER (owner-only data).
  (when (and player plist)
    (let ((stats (getf plist :stats))
          (inventory (getf plist :inventory))
          (equipment (getf plist :equipment)))
      (when stats
        (setf (player-stats player) (deserialize-stat-block stats))
        (mark-player-hud-stats-dirty player))
      (when inventory
        (setf (player-inventory player) (deserialize-inventory inventory *inventory-size*))
        (mark-player-inventory-dirty player))
      (when equipment
        (setf (player-equipment player)
              (deserialize-equipment equipment (length *equipment-slot-ids*)))
        (mark-player-inventory-dirty player))))
  player)

(defun serialize-npc (npc &key (include-visuals nil))
  ;; Convert NPC state to plist (server-authoritative state only).
  (let ((payload (list :id (npc-id npc)
                       :x (npc-x npc)
                       :y (npc-y npc)
                       :home-x (npc-home-x npc)
                       :home-y (npc-home-y npc)
                       :hits-left (npc-hits-left npc)
                       :alive (npc-alive npc)
                       :respawn-timer (npc-respawn-timer npc)
                       :provoked (npc-provoked npc)
                       :behavior-state (npc-behavior-state npc)
                       :attack-timer (npc-attack-timer npc))))
    (when include-visuals
      (setf payload (append payload
                            (list :anim-state (npc-anim-state npc)
                                  :facing (npc-facing npc)
                                  :frame-index (npc-frame-index npc)
                                  :frame-timer (npc-frame-timer npc)
                                  :hit-active (npc-hit-active npc)
                                  :hit-timer (npc-hit-timer npc)
                                  :hit-frame (npc-hit-frame npc)
                                  :hit-facing (npc-hit-facing npc)
                                  :hit-facing-sign (npc-hit-facing-sign npc)))))
    payload))

(defun deserialize-npc (plist npcs index)
  ;; Restore NPC from plist into existing NPC array at INDEX.
  (when (and plist npcs (< index (length npcs)))
    (let ((npc (aref npcs index)))
      (when npc
        (setf (npc-id npc) (getf plist :id 0)
              (npc-x npc) (getf plist :x 0.0)
              (npc-y npc) (getf plist :y 0.0)
              (npc-home-x npc) (getf plist :home-x 0.0)
              (npc-home-y npc) (getf plist :home-y 0.0)
              (npc-hits-left npc) (getf plist :hits-left 1)
              (npc-alive npc) (getf plist :alive t)
              (npc-respawn-timer npc) (getf plist :respawn-timer 0.0)
              (npc-provoked npc) (getf plist :provoked nil)
              (npc-behavior-state npc) (getf plist :behavior-state :idle)
              (npc-attack-timer npc) (getf plist :attack-timer 0.0)
              (npc-anim-state npc) (getf plist :anim-state :idle)
              (npc-facing npc) (getf plist :facing :down)
              (npc-frame-index npc) (getf plist :frame-index 0)
              (npc-frame-timer npc) (getf plist :frame-timer 0.0)
              (npc-hit-active npc) (getf plist :hit-active nil)
              (npc-hit-timer npc) (getf plist :hit-timer 0.0)
              (npc-hit-frame npc) (getf plist :hit-frame 0)
              (npc-hit-facing npc) (getf plist :hit-facing :down)
              (npc-hit-facing-sign npc) (getf plist :hit-facing-sign 1.0))))))

(defun players-match-order-p (players plists)
  ;; Return true when PLAYERS and PLISTS share the same ID ordering.
  (and players
       (= (length players) (length plists))
       (loop :for i :from 0 :below (length players)
             :for plist :in plists
             :for id = (getf plist :id 0)
             :always (and (> id 0)
                          (= (player-id (aref players i)) id)))))

(defun apply-player-plists (game player-plists)
  ;; Apply PLAYER-PLISTS to GAME players, preserving local player state.
  (when player-plists
    (let* ((players (game-players game))
           (reuse-order (players-match-order-p players player-plists))
           (players-changed nil))
      (if reuse-order
          (loop :for i :from 0 :below (length players)
                :for plist :in player-plists
                :do (apply-player-plist (aref players i) plist))
          (let ((new-players (make-array (length player-plists)))
                (old-local-player (game-player game)))
            (loop :for plist :in player-plists
                  :for index :from 0
                  :for id = (getf plist :id 0)
                  ;; Use O(1) fast lookup via game's index map
                  :for existing = (and players (find-player-by-id-fast game id))
                  :do (if existing
                          (apply-player-plist existing plist)
                          (progn
                            (setf existing
                                  (deserialize-player plist
                                                      *inventory-size*
                                                      (length *equipment-slot-ids*)))
                            ;; Preserve client-only state from old local player when creating new player
                            (when old-local-player
                              (setf (player-click-marker-x existing) (player-click-marker-x old-local-player)
                                    (player-click-marker-y existing) (player-click-marker-y old-local-player)
                                    (player-click-marker-kind existing) (player-click-marker-kind old-local-player)
                                    (player-click-marker-timer existing) (player-click-marker-timer old-local-player)
                                    (player-mouse-hold-timer existing) (player-mouse-hold-timer old-local-player)
                                    (player-auto-right existing) (player-auto-right old-local-player)
                                    (player-auto-left existing) (player-auto-left old-local-player)
                                    (player-auto-down existing) (player-auto-down old-local-player)
                                    (player-auto-up existing) (player-auto-up old-local-player)))))
                      (setf (aref new-players index) existing))
            (setf (game-players game) new-players
                  players new-players
                  players-changed t)
            ;; Rebuild index map for the new players array
            (rebuild-player-index-map game)))
      (let* ((local-id (or (game-net-player-id game)
                           (and (game-player game)
                                (player-id (game-player game)))))
             ;; Use O(1) fast lookup for local player
             (local-player (and local-id (plusp local-id) (find-player-by-id-fast game local-id))))
        (cond
          ;; Found local player - update game-player if changed
          ((and local-player (not (eq (game-player game) local-player)))
           (setf (game-player game) local-player))
          ;; CRITICAL: Valid player ID but not found in snapshot
          ;; DO NOT fall back to first player - it could be another client!
          ;; Log warning (verbose only to avoid spam) and keep current player
          ((and (null local-player) local-id (plusp local-id) (> (length players) 0))
           (log-verbose "Client player ID ~d not found in snapshot (~d players). Keeping current."
                        local-id (length players)))
          ;; No valid local ID set yet - keep current game-player
          ;; The auth-ok message sets game-net-player-id, so this case
          ;; should only happen before authentication completes
          ((and (or (null local-id) (zerop local-id)) (> (length players) 0))
           (log-verbose "No local player ID set, keeping current game-player"))))
      (when players-changed
        (setf (game-entities game)
              (make-entities (game-players game) (game-npcs game)))))))

;;;; ========================================================================
;;;; COMPACT SERIALIZATION (Network Optimization - see docs/net.md 4-Prong)
;;;; Uses positional vectors instead of plists to minimize snapshot size.
;;;; Target: <100 bytes per entity (vs ~232 bytes with plist format)
;;;; ========================================================================

;;; Helper functions for quantization and enum encoding

(defun quantize-coord (f)
  "Quantize coordinate to 0.1 pixel precision for network transmission."
  (round (* (or f 0.0) *coord-scale*)))

(defun dequantize-coord (i)
  "Restore coordinate from quantized integer."
  (/ (or i 0) (float *coord-scale*)))

(defun quantize-timer (f)
  "Quantize timer to 0.01 second precision for network transmission."
  (round (* (or f 0.0) *timer-scale*)))

(defun dequantize-timer (i)
  "Restore timer from quantized integer."
  (/ (or i 0) (float *timer-scale*)))

(defun encode-anim-state (state)
  "Encode animation state keyword to integer code."
  (or (cdr (assoc state *anim-state-to-code*)) 0))

(defun decode-anim-state (code)
  "Decode integer code to animation state keyword."
  (or (cdr (assoc code *code-to-anim-state*)) :idle))

(defun encode-facing (facing)
  "Encode facing direction keyword to integer code."
  (or (cdr (assoc facing *facing-to-code*)) 1))  ; default :down

(defun decode-facing (code)
  "Decode integer code to facing direction keyword."
  (or (cdr (assoc code *code-to-facing*)) :down))

(defun pack-player-flags (player)
  "Pack player boolean flags into a single integer.
   Bit 0: attacking, Bit 1: attack-hit, Bit 2: hit-active, Bit 3: running"
  (logior (if (player-attacking player) 1 0)
          (if (player-attack-hit player) 2 0)
          (if (player-hit-active player) 4 0)
          (if (player-running player) 8 0)))

(defun unpack-player-flags (flags)
  "Unpack player boolean flags from integer.
   Returns (values attacking attack-hit hit-active running)"
  (values (logbitp 0 flags)
          (logbitp 1 flags)
          (logbitp 2 flags)
          (logbitp 3 flags)))

(defun pack-npc-flags (npc)
  "Pack NPC boolean flags into a single integer.
   Bit 0: alive, Bit 1: provoked, Bit 2: hit-active"
  (logior (if (npc-alive npc) 1 0)
          (if (npc-provoked npc) 2 0)
          (if (npc-hit-active npc) 4 0)))

(defun unpack-npc-flags (flags)
  "Unpack NPC boolean flags from integer.
   Returns (values alive provoked hit-active)"
  (values (logbitp 0 flags)
          (logbitp 1 flags)
          (logbitp 2 flags)))

;;; Compact Player Serialization (v5)
;;; Vector format (20 elements):
;;; [0] id           [1] x*10        [2] y*10        [3] hp
;;; [4] dx*10        [5] dy*10       [6] anim-code   [7] facing-code
;;; [8] facing-sign  [9] frame-idx   [10] frame-timer*100
;;; [11] flags       [12] attack-timer*100  [13] hit-timer*100
;;; [14] hit-frame   [15] hit-facing-code   [16] hit-facing-sign
;;; [17] run-stamina*100  [18] last-sequence  [19] zone-id-hash
;;; v4 compatibility: vectors with length >= 22 include attack/follow targets
;;; at indices 17/18 and zone-id-hash at index 21.

(defun encode-zone-id (zone-id)
  "Encode zone-id symbol to integer for compact serialization.
   Returns 0 for nil, otherwise a deterministic hash of the symbol name.
   Uses djb2 hash algorithm for cross-process stability (sxhash is not stable)."
  (if zone-id
      (let* ((name (symbol-name zone-id))
             (hash 5381))  ; djb2 initial value
        (loop :for char :across name
              :do (setf hash (logand (+ (ash hash 5) hash (char-code char))
                                     #xFFFFFFFF)))  ; Keep 32-bit
        (logand hash #xFFFF))  ; Return 16-bit
      0))

(defun player-compact-zone-hash (vec)
  "Return zone-id-hash from compact player vector (v4/v5)."
  (let ((len (length vec)))
    (cond
      ((>= len 22) (aref vec 21))  ; v4
      ((>= len 20) (aref vec 19))  ; v5
      (t 0))))

(defun get-player-vector-pool ()
  "Get or lazily create the global player vector pool."
  (or *player-vector-pool*
      (setf *player-vector-pool*
            (make-vector-pool *player-vector-pool-capacity*
                              *player-compact-vector-size*))))

(defun reset-player-vector-pool ()
  "Reset the player vector pool for a new serialization pass.
   Call this before serializing players for a zone."
  (when *player-vector-pool*
    (reset-vector-pool *player-vector-pool*)))

(defun serialize-player-compact (player &key use-pool)
  "Serialize player to compact vector for network transmission.
   Excludes inventory (private to owning client).
   Format v5: Removed attack/follow target IDs from public snapshots.
   When USE-POOL is T, uses pre-allocated vector from pool (Task 4.2)."
  (let ((vec (if use-pool
                 (acquire-pooled-vector (get-player-vector-pool)
                                        *player-compact-vector-size*)
                 (make-array *player-compact-vector-size* :initial-element 0))))
    (setf (aref vec 0) (player-id player)
          (aref vec 1) (quantize-coord (player-x player))
          (aref vec 2) (quantize-coord (player-y player))
          (aref vec 3) (or (player-hp player) 0)
          (aref vec 4) (quantize-coord (player-dx player))
          (aref vec 5) (quantize-coord (player-dy player))
          (aref vec 6) (encode-anim-state (player-anim-state player))
          (aref vec 7) (encode-facing (player-facing player))
          (aref vec 8) (round (or (player-facing-sign player) 1.0))
          (aref vec 9) (or (player-frame-index player) 0)
          (aref vec 10) (quantize-timer (player-frame-timer player))
          (aref vec 11) (pack-player-flags player)
          (aref vec 12) (quantize-timer (player-attack-timer player))
          (aref vec 13) (quantize-timer (player-hit-timer player))
          (aref vec 14) (or (player-hit-frame player) 0)
          (aref vec 15) (encode-facing (player-hit-facing player))
          (aref vec 16) (round (or (player-hit-facing-sign player) 1.0))
          (aref vec 17) (quantize-timer (player-run-stamina player))
          (aref vec 18) (or (player-last-sequence player) 0)
          (aref vec 19) (encode-zone-id (player-zone-id player)))
    vec))

(defun deserialize-player-compact (vec)
  "Deserialize compact vector to player plist for apply-player-plist.
   Converts back to plist format for compatibility with existing code."
  (when (and vec (>= (length vec) 17))
    (let* ((len (length vec))
           (v4-p (>= len 22)))
      (multiple-value-bind (attacking attack-hit hit-active running)
          (unpack-player-flags (aref vec 11))
        (let* ((run-stamina (cond
                              (v4-p (if (>= len 20)
                                        (dequantize-timer (aref vec 19))
                                        1.0))
                              ((>= len 18) (dequantize-timer (aref vec 17)))
                              (t 1.0)))
               (last-sequence (cond
                                (v4-p (if (>= len 21) (aref vec 20) 0))
                                ((>= len 19) (aref vec 18))
                                (t 0)))
               (plist (list :id (aref vec 0)
                            :x (dequantize-coord (aref vec 1))
                            :y (dequantize-coord (aref vec 2))
                            :hp (aref vec 3)
                            :dx (dequantize-coord (aref vec 4))
                            :dy (dequantize-coord (aref vec 5))
                            :anim-state (decode-anim-state (aref vec 6))
                            :facing (decode-facing (aref vec 7))
                            :facing-sign (float (aref vec 8))
                            :frame-index (aref vec 9)
                            :frame-timer (dequantize-timer (aref vec 10))
                            :attacking attacking
                            :attack-hit attack-hit
                            :hit-active hit-active
                            :running running
                            :attack-timer (dequantize-timer (aref vec 12))
                            :hit-timer (dequantize-timer (aref vec 13))
                            :hit-frame (aref vec 14)
                            :hit-facing (decode-facing (aref vec 15))
                            :hit-facing-sign (float (aref vec 16))
                            :run-stamina run-stamina
                            :last-sequence last-sequence
                            :zone-id-hash (player-compact-zone-hash vec))))
          ;; v4 vectors included attack/follow target IDs at indices 17/18
          (when v4-p
            (setf plist (plist-put plist :attack-target-id (aref vec 17))
                  plist (plist-put plist :follow-target-id (aref vec 18))))
          plist)))))

(defun apply-player-compact-direct (player vec)
  "Apply compact vector directly to player struct, bypassing intermediate plist.
   Preserves client-only state (click markers, mouse timer, auto-walk).
   NOTE: Does NOT set player-id - IDs are immutable after creation. The vec[0] ID
   is used for lookup, not to change the player's identity.
   Returns the zone-id-hash for filtering, or 0 if not present."
  (declare (optimize (speed 3) (safety 1)))
  (when (and player vec (>= (length vec) 17))
    (let* ((len (length vec))
           (v4-p (>= len 22)))
      ;; Unpack flags once
      (multiple-value-bind (attacking attack-hit hit-active running)
          (unpack-player-flags (aref vec 11))
        ;; Apply network fields directly - client-only state untouched
        ;; NOTE: player-id is NOT set here - IDs are immutable (avoids index map desync)
        (setf (player-x player) (dequantize-coord (aref vec 1))
              (player-y player) (dequantize-coord (aref vec 2))
              (player-hp player) (aref vec 3)
              (player-dx player) (dequantize-coord (aref vec 4))
              (player-dy player) (dequantize-coord (aref vec 5))
              (player-anim-state player) (decode-anim-state (aref vec 6))
              (player-facing player) (decode-facing (aref vec 7))
              (player-facing-sign player) (float (aref vec 8))
              (player-frame-index player) (aref vec 9)
              (player-frame-timer player) (dequantize-timer (aref vec 10))
              (player-attacking player) attacking
              (player-attack-hit player) attack-hit
              (player-hit-active player) hit-active
              (player-running player) running
              (player-attack-timer player) (dequantize-timer (aref vec 12))
              (player-hit-timer player) (dequantize-timer (aref vec 13))
              (player-hit-frame player) (aref vec 14)
              (player-hit-facing player) (decode-facing (aref vec 15))
              (player-hit-facing-sign player) (float (aref vec 16)))
        (if v4-p
            (progn
              ;; v4 vectors included attack/follow target IDs
              (setf (player-attack-target-id player) (aref vec 17)
                    (player-follow-target-id player) (aref vec 18))
              (when (>= len 20)
                (setf (player-run-stamina player) (dequantize-timer (aref vec 19))))
              (when (>= len 21)
                (setf (player-last-sequence player) (aref vec 20))))
            (progn
              ;; v5 vectors omit attack/follow target IDs from public snapshots
              (when (>= len 18)
                (setf (player-run-stamina player) (dequantize-timer (aref vec 17))))
              (when (>= len 19)
                (setf (player-last-sequence player) (aref vec 18))))))
      ;; Return zone-id-hash for filtering (0 if not present)
      (player-compact-zone-hash vec))))

;;; Compact NPC Serialization
;;; Vector format (14 elements):
;;; [0] id           [1] x*10        [2] y*10        [3] hits-left
;;; [4] flags        [5] behavior-state-code  [6] attack-timer*100
;;; [7] anim-code    [8] facing-code [9] frame-idx   [10] frame-timer*100
;;; [11] hit-timer*100  [12] hit-frame  [13] hit-facing-code

(defun encode-behavior-state (state)
  "Encode NPC behavior state to integer code."
  (case state
    (:idle 0) (:wandering 1) (:chasing 2) (:attacking 3)
    (:fleeing 4) (:returning 5) (:dead 6) (otherwise 0)))

(defun decode-behavior-state (code)
  "Decode integer code to NPC behavior state."
  (case code
    (0 :idle) (1 :wandering) (2 :chasing) (3 :attacking)
    (4 :fleeing) (5 :returning) (6 :dead) (otherwise :idle)))

;;; Compact NPC Serialization (v4)
;;; Vector format (15 elements):
;;; [0] id        [1] x*10       [2] y*10       [3] hits-left  [4] flags
;;; [5] behavior  [6] attack-timer*100  [7] anim-code  [8] facing-code
;;; [9] frame-idx [10] frame-timer*100  [11] hit-timer*100  [12] hit-frame
;;; [13] hit-facing-code  [14] zone-id-hash

(defun serialize-npc-compact (npc &optional zone-id)
  "Serialize NPC to compact vector for network transmission.
   ZONE-ID is the zone containing this NPC (for client-side filtering)."
  (vector (npc-id npc)
          (quantize-coord (npc-x npc))
          (quantize-coord (npc-y npc))
          (or (npc-hits-left npc) 0)
          (pack-npc-flags npc)
          (encode-behavior-state (npc-behavior-state npc))
          (quantize-timer (npc-attack-timer npc))
          (encode-anim-state (npc-anim-state npc))
          (encode-facing (npc-facing npc))
          (or (npc-frame-index npc) 0)
          (quantize-timer (npc-frame-timer npc))
          (quantize-timer (npc-hit-timer npc))
          (or (npc-hit-frame npc) 0)
          (encode-facing (npc-hit-facing npc))
          (encode-zone-id zone-id)))

(defun deserialize-npc-compact (vec)
  "Deserialize compact vector to NPC plist for compatibility.
   Supports v3 (14 elements) and v4 (15 elements with zone-id-hash)."
  (when (and vec (>= (length vec) 14))
    (multiple-value-bind (alive provoked hit-active)
        (unpack-npc-flags (aref vec 4))
      (list :id (aref vec 0)
            :x (dequantize-coord (aref vec 1))
            :y (dequantize-coord (aref vec 2))
            :hits-left (aref vec 3)
            :alive alive
            :provoked provoked
            :behavior-state (decode-behavior-state (aref vec 5))
            :attack-timer (dequantize-timer (aref vec 6))
            :anim-state (decode-anim-state (aref vec 7))
            :facing (decode-facing (aref vec 8))
            :frame-index (aref vec 9)
            :frame-timer (dequantize-timer (aref vec 10))
            :hit-active hit-active
            :hit-timer (dequantize-timer (aref vec 11))
            :hit-frame (aref vec 12)
            :hit-facing (decode-facing (aref vec 13))
            :hit-facing-sign 1.0
            ;; v4 adds zone-id-hash at index 14 (0 if v3 format)
            :zone-id-hash (if (>= (length vec) 15) (aref vec 14) 0)))))  ; Default, not stored compactly

(defun apply-npc-compact-direct (npc vec)
  "Apply compact vector directly to NPC struct, bypassing intermediate plist.
   NOTE: Does NOT set npc-id - IDs are immutable after creation. The vec[0] ID
   is used for lookup, not to change the NPC's identity.
   Returns the zone-id-hash for filtering, or 0 if not present."
  (declare (optimize (speed 3) (safety 1)))
  (when (and npc vec (>= (length vec) 14))
    (multiple-value-bind (alive provoked hit-active)
        (unpack-npc-flags (aref vec 4))
      ;; Apply network fields directly
      ;; NOTE: npc-id is NOT set here - IDs are immutable
      (setf (npc-x npc) (dequantize-coord (aref vec 1))
            (npc-y npc) (dequantize-coord (aref vec 2))
            (npc-hits-left npc) (aref vec 3)
            (npc-alive npc) alive
            (npc-provoked npc) provoked
            (npc-behavior-state npc) (decode-behavior-state (aref vec 5))
            (npc-attack-timer npc) (dequantize-timer (aref vec 6))
            (npc-anim-state npc) (decode-anim-state (aref vec 7))
            (npc-facing npc) (decode-facing (aref vec 8))
            (npc-frame-index npc) (aref vec 9)
            (npc-frame-timer npc) (dequantize-timer (aref vec 10))
            (npc-hit-active npc) hit-active
            (npc-hit-timer npc) (dequantize-timer (aref vec 11))
            (npc-hit-frame npc) (aref vec 12)
            (npc-hit-facing npc) (decode-facing (aref vec 13))
            (npc-hit-facing-sign npc) 1.0))
    ;; Return zone-id-hash for filtering (0 if not present)
    (if (>= (length vec) 15) (aref vec 14) 0)))

;;; Compact Game State Serialization

(defun serialize-game-state-compact (game)
  "Serialize game state using compact format for network transmission.
   Returns plist with :format :compact-v5 and vector-based entity data.
   v5 removes attack/follow target IDs from public player vectors and keeps
   zone-id-hash for client-side zone filtering as defense-in-depth."
  (let* ((players (game-players game))
         (npcs (game-npcs game))
         (world (game-world game))
         (zone (world-zone world))
         (objects (and zone (zone-objects zone)))
         (player-vectors nil)
         (npc-vectors nil)
         (object-list nil))
    ;; Serialize players to compact vectors
    (when players
      (loop :for player :across players
            :when player
            :do (push (serialize-player-compact player) player-vectors)))
    ;; Serialize NPCs to compact vectors (with zone-id for client filtering)
    (let ((npc-zone-id (and zone (zone-id zone))))
      (when npcs
        (loop :for npc :across npcs
              :when npc
              :do (push (serialize-npc-compact npc npc-zone-id) npc-vectors))))
    ;; Serialize zone objects (all objects - dropped items and respawning pickups)
    (when objects
      (dolist (object objects)
        (push (serialize-object object) object-list)))
    ;; Build compact snapshot
    (list :format :compact-v5
          :zone-id (and zone (zone-id zone))
          :players (coerce (nreverse player-vectors) 'vector)
          :npcs (coerce (nreverse npc-vectors) 'vector)
          :objects (nreverse object-list))))

;;;; ========================================================================
;;;; Edge-Strip Serialization (Step 7) — Server-side boundary AOI streaming.
;;;; Each strip is a spatially-filtered mini-snapshot of an adjacent zone.
;;;; ========================================================================

(defun opposite-edge (edge)
  "Return the opposite cardinal direction of EDGE."
  (case edge
    (:north :south)
    (:south :north)
    (:east :west)
    (:west :east)
    (t nil)))

(defun entity-in-edge-strip-p (x y edge zone strip-width-px
                               &optional (tile-dest-size (float (* *tile-size* *tile-scale*) 1.0)))
  "Return T if position (X,Y) is within the edge strip of ZONE.
   The strip extends STRIP-WIDTH-PX from the shared boundary into the zone.
   EDGE is the shared edge from the perspective of the adjacent (querying) zone —
   so the strip is at the OPPOSITE side of this zone.
   TILE-DEST-SIZE is the rendered tile size in pixels (default: *tile-size* * *tile-scale*)."
  (let* ((zone-w (* (zone-width zone) tile-dest-size))
         (zone-h (* (zone-height zone) tile-dest-size)))
    (case edge
      ;; Adjacent zone is to the north → strip is at south edge of adj zone
      (:north (>= y (- zone-h strip-width-px)))
      ;; Adjacent zone is to the south → strip is at north edge of adj zone
      (:south (<= y strip-width-px))
      ;; Adjacent zone is to the west → strip is at east edge of adj zone
      (:west (>= x (- zone-w strip-width-px)))
      ;; Adjacent zone is to the east → strip is at west edge of adj zone
      (:east (<= x strip-width-px))
      (t nil))))

;;;; ========================================================================
;;;; Entity-Type Registry for Type-Agnostic Edge-Strip Pipeline
;;;; To add a new entity type to edge strips (and full-zone snapshots),
;;;; add a spec to *edge-entity-specs* and define the required helper functions.
;;;; All pipeline functions (serialize, deserialize, draw) iterate this registry.
;;;; ========================================================================

;;; --- Source helpers: extract entity collection from zone-state ---

(defun edge-source-players (zone-state)
  "Get player array from zone-state for serialization."
  (when zone-state (zone-state-zone-players zone-state)))

(defun edge-source-npcs (zone-state)
  "Get NPC array from zone-state for serialization."
  (when zone-state (zone-state-npcs zone-state)))

(defun edge-source-objects (zone-state)
  "Get zone objects list from zone-state for serialization."
  (let ((zone (when zone-state (zone-state-zone zone-state))))
    (when zone (zone-objects zone))))

;;; --- Validity helpers: should this entity be included in serialization? ---

(defun edge-valid-player-p (entity)
  "Player is valid for serialization if non-nil."
  (not (null entity)))

(defun edge-valid-npc-p (entity)
  "NPC is valid for serialization if non-nil and alive."
  (and entity (npc-alive entity)))

(defun edge-valid-object-p (entity)
  "Objects are always valid for serialization."
  (declare (ignore entity))
  t)

;;; --- Pixel-position helpers: get pixel coords for spatial filtering ---

(defun edge-player-pixel-pos (entity tile-dest-size)
  "Return pixel position of player (already in pixel coords)."
  (declare (ignore tile-dest-size))
  (values (player-x entity) (player-y entity)))

(defun edge-npc-pixel-pos (entity tile-dest-size)
  "Return pixel position of NPC (already in pixel coords)."
  (declare (ignore tile-dest-size))
  (values (npc-x entity) (npc-y entity)))

(defun edge-object-pixel-pos (entity tile-dest-size)
  "Return pixel position of zone-object (convert from tile coords)."
  (values (* (zone-object-x entity) tile-dest-size)
          (* (zone-object-y entity) tile-dest-size)))

;;; --- Serialize helpers: uniform interface for per-entity serialization ---

(defun edge-serialize-player (entity zone-id use-pool)
  "Serialize one player for snapshot/edge-strip."
  (declare (ignore zone-id))
  (serialize-player-compact entity :use-pool use-pool))

(defun edge-serialize-npc (entity zone-id use-pool)
  "Serialize one NPC for snapshot/edge-strip."
  (declare (ignore use-pool))
  (serialize-npc-compact entity zone-id))

(defun edge-serialize-object (entity zone-id use-pool)
  "Serialize one zone-object for snapshot/edge-strip."
  (declare (ignore zone-id use-pool))
  (serialize-object entity))

;;; --- Offset helpers: apply world-space offset for edge-strip rendering ---

(defun edge-offset-player (entity offset-x offset-y tile-dest-size)
  "Apply pixel offset to player position for edge-strip rendering."
  (declare (ignore tile-dest-size))
  (incf (player-x entity) offset-x)
  (incf (player-y entity) offset-y))

(defun edge-offset-npc (entity offset-x offset-y tile-dest-size)
  "Apply pixel offset to NPC position for edge-strip rendering."
  (declare (ignore tile-dest-size))
  (incf (npc-x entity) offset-x)
  (incf (npc-y entity) offset-y))

(defun edge-offset-object (entity offset-x offset-y tile-dest-size)
  "Apply tile offset to zone-object position for edge-strip rendering."
  (incf (zone-object-x entity) (round (/ offset-x tile-dest-size)))
  (incf (zone-object-y entity) (round (/ offset-y tile-dest-size))))

;;; --- The Registry ---

(defparameter *edge-entity-specs*
  '((:key :players
     :source edge-source-players
     :valid-p edge-valid-player-p
     :pixel-pos edge-player-pixel-pos
     :serialize-one edge-serialize-player
     :deser-compact deserialize-player-compact
     :ctor make-edge-strip-player
     :apply-offset edge-offset-player
     :cull-one edge-cull-player
     :draw-one edge-draw-player-one
     :collection-type :array)
    (:key :npcs
     :source edge-source-npcs
     :valid-p edge-valid-npc-p
     :pixel-pos edge-npc-pixel-pos
     :serialize-one edge-serialize-npc
     :deser-compact deserialize-npc-compact
     :ctor make-edge-strip-npc
     :apply-offset edge-offset-npc
     :cull-one edge-cull-npc
     :draw-one edge-draw-npc-one
     :collection-type :array)
    (:key :objects
     :source edge-source-objects
     :valid-p edge-valid-object-p
     :pixel-pos edge-object-pixel-pos
     :serialize-one edge-serialize-object
     :deser-compact identity
     :ctor make-edge-strip-object
     :apply-offset edge-offset-object
     :cull-one edge-cull-object
     :draw-one edge-draw-object-one
     :collection-type :list))
  "Registry of entity types for the edge-strip pipeline.
   To add a new entity type: add a spec here and define the required functions.
   Pipeline functions (serialize, deserialize, draw) all iterate this registry.")

;;;; ========================================================================
;;;; Unified Serialization Path (serialize-zone-state-filtered)
;;;; ========================================================================

(defun serialize-edge-strip (adj-zone-state edge strip-width-px adj-zone-id
                             tile-dest-size &key use-pool)
  "Serialize entities from ADJ-ZONE-STATE within the edge strip.
   Delegates to serialize-zone-state-filtered (registry-driven)."
  (serialize-zone-state-filtered adj-zone-state adj-zone-id tile-dest-size
                                 :edge edge :strip-width-px strip-width-px
                                 :use-pool use-pool))

(defun serialize-zone-state-filtered (zone-state zone-id tile-dest-size
                                      &key edge strip-width-px use-pool
                                           entity-overrides)
  "Serialize all registered entity types from ZONE-STATE into snapshot format.
   Iterates *edge-entity-specs* so new entity types are included automatically.
   When EDGE and STRIP-WIDTH-PX are provided, only entities within the
   edge strip are included (spatial filter). Otherwise all entities are serialized.
   ENTITY-OVERRIDES is an optional plist of (:key collection) to override sources
   (used by serialize-game-state-for-zone for player fallback)."
  (let ((filter-p (and edge strip-width-px))
        (zone (when zone-state (zone-state-zone zone-state))))
    (loop :for spec :in *edge-entity-specs*
          :nconc
          (let* ((key (getf spec :key))
                 (source-fn (getf spec :source))
                 (valid-fn (getf spec :valid-p))
                 (pos-fn (getf spec :pixel-pos))
                 (ser-fn (getf spec :serialize-one))
                 (coll-type (getf spec :collection-type))
                 ;; Use override if provided, otherwise source from zone-state
                 (entities (or (getf entity-overrides key)
                               (funcall source-fn zone-state)))
                 (serialized nil))
            ;; Iterate entities and serialize (with optional spatial filter)
            (when entities
              (flet ((process-entity (entity)
                       (when (and (funcall valid-fn entity)
                                  (or (not filter-p)
                                      (multiple-value-bind (px py)
                                          (funcall pos-fn entity tile-dest-size)
                                        (entity-in-edge-strip-p
                                         px py edge zone strip-width-px tile-dest-size))))
                         (push (funcall ser-fn entity zone-id use-pool) serialized))))
                (if (eq coll-type :array)
                    (loop :for entity :across entities :do (process-entity entity))
                    (dolist (entity entities) (process-entity entity)))))
            ;; Return (key value) pair for nconc
            (list key (if (eq coll-type :array)
                          (coerce (nreverse serialized) 'vector)
                          (nreverse serialized)))))))

(defun serialize-edge-strips-for-zone (game zone-id)
  "Build edge strips for ZONE-ID by checking all adjacent zones in the world graph.
   Returns a list of (:edge <kw> :zone-id <kw> :strip <plist>) or nil if none."
  (let* ((world (game-world game))
         (graph (and world (world-world-graph world)))
         (strips nil))
    (when graph
      (let* ((tile-dest-size (float (world-tile-dest-size world)))
             (strip-width-px (* *zone-edge-visibility-tiles* tile-dest-size)))
        (dolist (exit-spec (world-graph-exits graph zone-id))
          ;; Only spatial exits (preserve-x/y) get edge strips; skip teleports
          (when (spatial-exit-p exit-spec)
          (let* ((edge (getf exit-spec :edge))
                 (adj-zone-id (getf exit-spec :to))
                 (adj-zone-state (and adj-zone-id (get-zone-state adj-zone-id))))
            (when adj-zone-state
              (let ((strip (serialize-edge-strip adj-zone-state edge strip-width-px adj-zone-id
                                                 tile-dest-size)))
                (let ((np (length (getf strip :players)))
                      (nn (length (getf strip :npcs)))
                      (no (length (getf strip :objects))))
                  (when (or (> np 0) (> nn 0) (> no 0))
                    (log-zone "Edge strip ~a->~a: ~d players, ~d npcs, ~d objects"
                                 edge adj-zone-id np nn no)))
                (push (list :edge edge :zone-id adj-zone-id :strip strip) strips))))))))
    (nreverse strips)))

(defun serialize-game-state-for-zone (game zone-id zone-state &key use-pool)
  "Serialize game state filtered to a specific zone.
   Delegates to serialize-zone-state-filtered (registry-driven) so all entity
   types are handled by the same code path as edge strips.
   Uses cached zone-players array when available (Task 4.1) for O(zone-players)
   instead of O(total-players). Falls back to filtering all players when cache
   is not populated."
  (with-timing (:serialize-zone)
    ;; Reset vector pool for this serialization pass (Task 4.2)
    (when use-pool
      (reset-player-vector-pool))
    ;; Build entity-overrides for fallback when zone-state is incomplete
    (let* ((zone-players (when zone-state (zone-state-zone-players zone-state)))
           (overrides nil))
      ;; Player fallback: when zone-players cache is not populated, filter game-players
      (when (or (null zone-players) (zerop (length zone-players)))
        (let ((fallback (make-array 8 :adjustable t :fill-pointer 0)))
          (when (game-players game)
            (loop :for player :across (game-players game)
                  :for pz = (or (player-zone-id player) *starting-zone-id*)
                  :when (and player (eq pz zone-id))
                  :do (vector-push-extend player fallback)))
          (setf (getf overrides :players) fallback)))
      ;; NPC/object fallback when zone-state is nil
      (unless zone-state
        (setf (getf overrides :npcs) (game-npcs game))
        (let ((zone (world-zone (game-world game))))
          (when zone
            (setf (getf overrides :objects) (zone-objects zone)))))
      ;; Delegate to unified registry-driven serialization
      (let* ((tile-dest-size (float (world-tile-dest-size (game-world game)) 1.0))
             (payload (serialize-zone-state-filtered zone-state zone-id tile-dest-size
                                                     :use-pool use-pool
                                                     :entity-overrides overrides))
             (snapshot (list* :format :compact-v5 :zone-id zone-id payload)))
        ;; Step 7: Append edge strips for all loaded adjacent zones
        (let ((edge-strips (serialize-edge-strips-for-zone game zone-id)))
          (when edge-strips
            (setf (getf snapshot :edge-strips) edge-strips)))
        snapshot))))

;;;; ========================================================================
;;;; Edge-Strip Entity Constructors (Step 8)
;;;; Create minimal player/npc structs from deserialized plists for rendering.
;;;; Only visual fields are set — these entities are render-only.
;;;; ========================================================================

(defun make-edge-strip-player (plist)
  "Create a minimal player struct from compact PLIST for edge-strip rendering."
  (let ((p (%make-player)))
    (setf (player-id p) (getf plist :id 0)
          (player-x p) (float (getf plist :x 0.0) 1.0)
          (player-y p) (float (getf plist :y 0.0) 1.0)
          (player-dx p) (float (getf plist :dx 0.0) 1.0)
          (player-dy p) (float (getf plist :dy 0.0) 1.0)
          (player-hp p) (getf plist :hp 1)
          (player-anim-state p) (getf plist :anim-state :idle)
          (player-facing p) (getf plist :facing :down)
          (player-facing-sign p) (float (getf plist :facing-sign 1.0) 1.0)
          (player-frame-index p) (getf plist :frame-index 0)
          (player-frame-timer p) (float (getf plist :frame-timer 0.0) 1.0)
          (player-attacking p) (getf plist :attacking nil)
          (player-attack-hit p) (getf plist :attack-hit nil)
          (player-hit-active p) (getf plist :hit-active nil)
          (player-attack-timer p) (float (getf plist :attack-timer 0.0) 1.0)
          (player-hit-timer p) (float (getf plist :hit-timer 0.0) 1.0)
          (player-hit-frame p) (getf plist :hit-frame 0)
          (player-hit-facing p) (getf plist :hit-facing :down)
          (player-hit-facing-sign p) (float (getf plist :hit-facing-sign 1.0) 1.0)
          (player-running p) (getf plist :running nil))
    p))

(defun make-edge-strip-npc (plist)
  "Create a minimal NPC struct from compact PLIST for edge-strip rendering."
  (let ((n (%make-npc))
        (hits (getf plist :hits-left 1)))
    (setf (npc-id n) (getf plist :id 0)
          (npc-x n) (float (getf plist :x 0.0) 1.0)
          (npc-y n) (float (getf plist :y 0.0) 1.0)
          (npc-anim-state n) (getf plist :anim-state :idle)
          (npc-facing n) (getf plist :facing :down)
          (npc-frame-index n) (getf plist :frame-index 0)
          (npc-frame-timer n) (float (getf plist :frame-timer 0.0) 1.0)
          (npc-hits-left n) hits
          (npc-alive n) (getf plist :alive t)
          (npc-hit-active n) (getf plist :hit-active nil)
          (npc-hit-timer n) (float (getf plist :hit-timer 0.0) 1.0)
          (npc-hit-frame n) (getf plist :hit-frame 0)
          (npc-hit-facing n) (getf plist :hit-facing :down)
          (npc-hit-facing-sign n) (float (getf plist :hit-facing-sign 1.0) 1.0))
    ;; Set stats for health bar rendering (npc-max-hp requires stats)
    ;; Use hits-left as the hitpoints level for a reasonable health bar
    (setf (npc-stats n) (make-npc-stats nil))
    ;; Try to set archetype from plist if available
    (let ((archetype-name (getf plist :archetype)))
      (when archetype-name
        (let ((archetype (find-npc-archetype archetype-name)))
          (when archetype
            (setf (npc-archetype n) archetype
                  (npc-stats n) (make-npc-stats archetype))))))
    n))

(defun make-edge-strip-object (plist)
  "Create a zone-object struct from serialized PLIST for edge-strip rendering."
  (%make-zone-object :id (getf plist :id)
                     :x (getf plist :x 0)
                     :y (getf plist :y 0)
                     :count (getf plist :count 1)
                     :respawn (float (getf plist :respawn 0.0) 1.0)
                     :respawnable (getf plist :respawnable t)))

(defun compute-edge-strip-offset (edge world)
  "Compute world-space offset for rendering entities from an adjacent zone at EDGE.
   Uses the current zone's dimensions to determine where the adjacent zone starts."
  (let* ((zone (and world (world-zone world)))
         (tile-size (world-tile-dest-size world))
         (span-x (if zone (* (zone-width zone) tile-size) 0.0))
         (span-y (if zone (* (zone-height zone) tile-size) 0.0)))
    (case edge
      (:north (values 0.0 (- span-y)))
      (:south (values 0.0 span-y))
      (:east  (values span-x 0.0))
      (:west  (values (- span-x) 0.0))
      (t (values 0.0 0.0)))))

(defun deserialize-edge-strips (state game)
  "Process :edge-strips from snapshot STATE and store in game-edge-strips.
   Registry-driven: iterates *edge-entity-specs* so new entity types are
   deserialized automatically. Step 8: Replaces (not accumulates) each frame."
  (let ((raw-strips (getf state :edge-strips))
        (world (game-world game))
        (result nil))
    (when raw-strips
      (let ((tile-dest-size (if world (float (world-tile-dest-size world) 1.0) 64.0)))
        (dolist (strip-entry raw-strips)
          (let* ((edge (getf strip-entry :edge))
                 (strip-zone-id (getf strip-entry :zone-id))
                 (strip-data (getf strip-entry :strip)))
            ;; Compute world-space offset for this edge
            (multiple-value-bind (offset-x offset-y)
                (compute-edge-strip-offset edge world)
              ;; Build deserialized strip by iterating entity-type registry
              (let ((deserialized (list :edge edge :zone-id strip-zone-id
                                       :offset-x offset-x :offset-y offset-y)))
                (dolist (spec *edge-entity-specs*)
                  (let* ((key (getf spec :key))
                         (raw (getf strip-data key))
                         (deser-fn (getf spec :deser-compact))
                         (ctor-fn (getf spec :ctor))
                         (offset-fn (getf spec :apply-offset))
                         (coll-type (getf spec :collection-type)))
                    (let ((entities
                            (cond
                              ;; Array collection: compact vectors
                              ((and (eq coll-type :array) raw
                                    (> (length raw) 0))
                               (coerce
                                (loop :for vec :across raw
                                      :for plist = (funcall deser-fn vec)
                                      :when plist
                                      :collect (let ((e (funcall ctor-fn plist)))
                                                 (funcall offset-fn e offset-x offset-y
                                                          tile-dest-size)
                                                 e))
                                'vector))
                              ;; List collection: plists directly
                              ((and (eq coll-type :list) raw)
                               (loop :for item :in raw
                                     :for plist = (funcall deser-fn item)
                                     :when plist
                                     :collect (let ((e (funcall ctor-fn plist)))
                                                (funcall offset-fn e offset-x offset-y
                                                         tile-dest-size)
                                                e)))
                              (t nil))))
                      ;; Store under the entity key (use empty vector for array types)
                      (setf (getf deserialized key)
                            (or entities
                                (if (eq coll-type :array) #() nil))))))
                (push deserialized result)))))))
    ;; Replace edge strips wholesale
    (setf (game-edge-strips game) (nreverse result))))

(defun deserialize-game-state-compact (state game)
  "Apply compact game state to GAME, converting vectors back to entity updates."
  (let ((player-vectors (getf state :players))
        (npc-vectors (getf state :npcs))
        (object-plists (getf state :objects)))
    ;; Convert compact player vectors to plists and apply
    (when player-vectors
      (let ((player-plists (loop :for vec :across player-vectors
                                 :collect (deserialize-player-compact vec))))
        (apply-player-plists game player-plists)))
    ;; Convert compact NPC vectors to plists and apply
    (when npc-vectors
      (let ((npcs (game-npcs game)))
        (loop :for vec :across npc-vectors
              :for index :from 0
              :for plist = (deserialize-npc-compact vec)
              :do (deserialize-npc plist npcs index))))
    ;; Apply object states from server (authoritative - replaces client objects)
    (when object-plists
      (let* ((world (game-world game))
             (zone (and world (world-zone world))))
        (when zone
          ;; Replace client zone objects with server state
          ;; This ensures dropped items appear and picked up items disappear
          ;; Task 5.5: Convert server plists to zone-object structs
          (setf (zone-objects zone)
                (loop :for server-obj :in object-plists
                      :collect (make-zone-object-from-plist server-obj))))))
    ;; Step 8: Process edge strips (separate deserialization path, no zone-hash check)
    (deserialize-edge-strips state game)))

;;;; ========================================================================
;;;; DELTA COMPRESSION - See docs/net.md Prong 2
;;;; ========================================================================

(defun serialize-game-state-delta (game seq baseline-seq)
  "Serialize only dirty entities as delta snapshot.
   SEQ is current snapshot sequence number.
   BASELINE-SEQ is client's last acknowledged sequence.
   Returns compact delta snapshot with only changed entities."
  (let* ((players (game-players game))
         (npcs (game-npcs game))
         (world (game-world game))
         (zone (world-zone world))
         (objects (and zone (zone-objects zone)))
         (changed-players nil)
         (changed-npcs nil)
         (object-list nil))
    ;; Collect dirty players
    (when players
      (loop :for player :across players
            :when (and player (player-snapshot-dirty player))
            :do (push (serialize-player-compact player) changed-players)))
    ;; Collect dirty NPCs (with zone-id for client filtering)
    (let ((npc-zone-id (and zone (zone-id zone))))
      (when npcs
        (loop :for npc :across npcs
              :when (and npc (npc-snapshot-dirty npc))
              :do (push (serialize-npc-compact npc npc-zone-id) changed-npcs))))
    ;; Collect all zone objects (dropped items + respawning pickups)
    (when objects
      (dolist (object objects)
        (push (serialize-object object) object-list)))
    ;; Build delta snapshot
    (list :format :delta-v5
          :seq seq
          :baseline-seq baseline-seq
          :zone-id (and zone (zone-id zone))
          :changed-players (coerce (nreverse changed-players) 'vector)
          :changed-npcs (coerce (nreverse changed-npcs) 'vector)
          :objects (nreverse object-list))))

(defun serialize-game-state-delta-for-zone (game zone-id zone-state seq &key use-pool)
  "Serialize delta snapshot filtered to ZONE-ID.
   Only includes dirty players in that zone and NPCs from zone-state.
   Uses cached zone-players array when available (Task 4.1).
   When USE-POOL is T, uses pre-allocated vectors (Task 4.2).
   SEQ is current snapshot sequence number."
  ;; Reset vector pool for this serialization pass (Task 4.2)
  (when use-pool
    (reset-player-vector-pool))
  (let* ((zone-players (when zone-state (zone-state-zone-players zone-state)))
         (npcs (if zone-state (zone-state-npcs zone-state) nil))
         (zone (if zone-state (zone-state-zone zone-state) nil))
         (objects (and zone (zone-objects zone)))
         (changed-players nil)
         (changed-npcs nil)
         (object-list nil))
    ;; Filter dirty players using cached zone-players if available (Task 4.1)
    (if (and zone-players (> (length zone-players) 0))
        ;; Fast path: iterate only zone players
        (loop :for player :across zone-players
              :when (and player (player-snapshot-dirty player))
              :do (push (serialize-player-compact player :use-pool use-pool) changed-players))
        ;; Fallback: filter all players by zone
        (let ((players (game-players game)))
          (when players
            (loop :for player :across players
                  :for player-zone = (or (player-zone-id player) *starting-zone-id*)
                  :when (and player
                             (player-snapshot-dirty player)
                             (eq player-zone zone-id))
                  :do (push (serialize-player-compact player :use-pool use-pool) changed-players)))))
    ;; Collect dirty NPCs from zone-state only (with zone-id for client filtering)
    (when npcs
      (loop :for npc :across npcs
            :when (and npc (npc-snapshot-dirty npc))
            :do (push (serialize-npc-compact npc zone-id) changed-npcs)))
    ;; Zone objects
    (when objects
      (dolist (object objects)
        (push (serialize-object object) object-list)))
    ;; Build delta with EXPLICIT zone-id (matches existing delta-v5 format)
    (let ((snapshot (list :format :delta-v5
                          :seq seq
                          :baseline-seq nil  ; Keep shape stable with existing delta format
                          :zone-id zone-id
                          :changed-players (coerce (nreverse changed-players) 'vector)
                          :changed-npcs (coerce (nreverse changed-npcs) 'vector)
                          :objects (nreverse object-list))))
      ;; Step 7: Edge strips always sent in full (not as deltas) per plan.
      ;; Small (~1.6KB worst case), ephemeral, no cross-zone dirty tracking needed.
      (let ((edge-strips (serialize-edge-strips-for-zone game zone-id)))
        (when edge-strips
          (setf (getf snapshot :edge-strips) edge-strips)))
      snapshot)))

(defun clear-snapshot-dirty-flags (game)
  "Reset dirty flags on all entities after snapshot broadcast.
   Called once per frame after sending to all clients."
  (let* ((players (game-players game))
         (npcs (game-npcs game))
         (world (game-world game))
         (zone (and world (world-zone world)))
         (objects (and zone (zone-objects zone))))
    (when players
      (loop :for player :across players
            :when player
            :do (setf (player-snapshot-dirty player) nil)))
    (when npcs
      (loop :for npc :across npcs
            :when npc
            :do (setf (npc-snapshot-dirty npc) nil)))
    ;; Clear object dirty flags
    ;; Task 5.5: Use zone-object struct accessor
    (when objects
      (dolist (object objects)
        (setf (zone-object-snapshot-dirty object) nil)))))

(defun deserialize-game-state-delta (delta game)
  "Apply delta snapshot to GAME, updating changed entities and adding new ones.
   Preserves existing entities not included in the delta.
   For players: updates existing by ID, adds new players not seen before.
   For NPCs: updates existing by index (NPCs use fixed array positions).
   Returns hash table of updated entity positions for interpolation buffer.
   Client-side zone filtering: only processes entities matching snapshot's zone."
  (with-timing (:deserialize-delta)
    (let* ((changed-players (getf delta :changed-players))
         (changed-npcs (getf delta :changed-npcs))
         (players (game-players game))
         ;; Use snapshot's :zone-id for filtering (more reliable than player-zone-id
         ;; which may not be set from compact format). Defense-in-depth filter.
         (snapshot-zone-id (getf delta :zone-id))
         (local-zone-hash (when snapshot-zone-id
                            (encode-zone-id snapshot-zone-id)))
         (updated-positions (make-hash-table :test 'eql)))  ; Track what we updated
    ;; Apply changed players - update existing OR add new
    (when (and changed-players (> (length changed-players) 0))
      (let ((new-players nil))  ; Collect players we need to add
        ;; First pass: update existing, collect new
        ;; Optimization: extract values directly from vector, skip plist for existing
        (loop :for vec :across changed-players
              :for id = (aref vec 0)
              :for x = (dequantize-coord (aref vec 1))
              :for y = (dequantize-coord (aref vec 2))
              :for entity-zone-hash = (player-compact-zone-hash vec)
              :for existing = (and players (find-player-by-id-fast game id))
              ;; Client-side zone filter: skip if zone-hash mismatch (0 = unset/legacy, allow)
              :when (or (null local-zone-hash)
                        (zerop entity-zone-hash)
                        (= entity-zone-hash local-zone-hash))
              :do (progn
                    ;; Track position from snapshot (not interpolated)
                    (setf (gethash id updated-positions) (list x y))
                    (if existing
                        ;; Update existing player directly from compact vector (no plist)
                        (apply-player-compact-direct existing vec)
                        ;; New player - create plist for full deserialization
                        (let ((plist (deserialize-player-compact vec)))
                          (push (deserialize-player plist
                                                    *inventory-size*
                                                    (length *equipment-slot-ids*))
                                new-players)))))
        ;; Second pass: if we have new players, expand the array
        (when new-players
          (let* ((old-count (if players (length players) 0))
                 (new-count (+ old-count (length new-players)))
                 (expanded (make-array new-count)))
            ;; Copy existing players
            (when players
              (loop :for i :from 0 :below old-count
                    :do (setf (aref expanded i) (aref players i))))
            ;; Add new players
            (loop :for player :in new-players
                  :for i :from old-count
                  :do (setf (aref expanded i) player))
            ;; Update both players and entities arrays
            (setf (game-players game) expanded
                  (game-entities game)
                  (make-entities expanded (game-npcs game)))
            ;; Rebuild player index map after expanding array
            (rebuild-player-index-map game)))))
    ;; Apply changed NPCs - find by ID and update
    ;; NOTE: NPC IDs are entity IDs, not array indices. Must search for matching NPC.
    ;; Client-side zone filter: skip NPCs not in local player's zone (defense-in-depth).
    ;; Optimization: extract values directly from vector, skip plist creation
    (when changed-npcs
      (let ((npcs (game-npcs game)))
        (when npcs
          (loop :for vec :across changed-npcs
                :for id = (aref vec 0)
                :for x = (dequantize-coord (aref vec 1))
                :for y = (dequantize-coord (aref vec 2))
                :for npc-zone-hash = (if (>= (length vec) 15) (aref vec 14) 0)
                :for index = (position id npcs :key #'npc-id)
                ;; Client-side zone filter: skip if zone-hash mismatch (0 = unset/legacy, allow)
                :when (and index
                           (or (null local-zone-hash)
                               (zerop npc-zone-hash)
                               (= npc-zone-hash local-zone-hash)))
                :do (progn
                      ;; Track NPC position (use negative ID like capture-entity-positions)
                      (setf (gethash (- id) updated-positions) (list x y))
                      (apply-npc-compact-direct (aref npcs index) vec))))))
    ;; Apply object states from server (authoritative - replaces client objects)
    ;; This ensures dropped items appear and picked up items disappear
    (let ((object-plists (getf delta :objects)))
      (when object-plists
        (log-verbose "DELTA-DESER: received ~a objects" (length object-plists))
        (let* ((world (game-world game))
               (zone (and world (world-zone world))))
          (when zone
            ;; Replace client zone objects with server state
            ;; Task 5.5: Convert server plists to zone-object structs
            (setf (zone-objects zone)
                  (loop :for server-obj :in object-plists
                        :collect (make-zone-object-from-plist server-obj)))))))
    ;; Step 8: Process edge strips in delta snapshots (always full, not deltas)
    (deserialize-edge-strips delta game)
    ;; Return updated positions for interpolation buffer
    updated-positions)))

;;;; ========================================================================
;;;; ZONE OBJECT SERIALIZATION
;;;; ========================================================================

(defun serialize-object (object)
  ;; Convert zone object struct to plist (ID, position, count, respawn timer).
  ;; Task 5.5: Use zone-object struct accessors for O(1) field access.
  (list :id (zone-object-id object)
        :x (zone-object-x object)
        :y (zone-object-y object)
        :count (zone-object-count object)
        :respawn (zone-object-respawn object)
        :respawnable (zone-object-respawnable object)))

(defun deserialize-object (plist)
  ;; Restore zone object from plist to struct.
  ;; Task 5.5: Use make-zone-object-from-plist for proper struct creation.
  (make-zone-object-from-plist plist))

(defun serialize-game-state (game &key (include-visuals nil) (network-only nil))
  ;; Serialize authoritative game state to plist (server snapshot).
  ;; When network-only is true, uses minimal player serialization (no inventory/stats)
  ;; to keep snapshot size under UDP limits (~65KB).
  (let* ((players (game-players game))
         (npcs (game-npcs game))
         (world (game-world game))
         (zone (world-zone world))
         (id-source (game-id-source game))
         (player-list nil)
         (npc-list nil)
         (object-list nil))
    ;; Serialize players (use network-only mode for smaller snapshots)
    (when players
      (loop :for player :across players
            :do (push (serialize-player player
                                        :include-visuals include-visuals
                                        :network-only network-only)
                      player-list)))
    ;; Serialize NPCs
    (when npcs
      (loop :for npc :across npcs
            :do (push (serialize-npc npc :include-visuals include-visuals) npc-list)))
    ;; Serialize zone objects
    (when zone
      (let ((objects (zone-objects zone)))
        (dolist (object objects)
          (push (serialize-object object) object-list))))
    ;; Build save state
    (list :version *save-format-version*
          :zone-id (and zone (zone-id zone))
          :id-next (and id-source (id-source-next-id id-source))
          :players (nreverse player-list)
          :npcs (nreverse npc-list)
          :objects (nreverse object-list))))

(defun deserialize-game-state (plist game)
  ;; Restore authoritative game state from plist into existing GAME.
  ;; Supports legacy plist, compact-v1/v2/v3/v4/v5, and delta-v1/v2/v3/v4/v5 formats.
  ;; Returns: (values zone-id delta-positions-or-nil)
  ;; delta-positions is non-nil only for delta-v1 format, for interpolation fix.
  (let ((format (getf plist :format)))
    (cond
      ;; Delta format (incremental updates, see docs/net.md Prong 2)
      ((or (eq format :delta-v1)
           (eq format :delta-v2)
           (eq format :delta-v3)
           (eq format :delta-v4)
           (eq format :delta-v5))
       (let ((delta-positions (deserialize-game-state-delta plist game)))
         (values (getf plist :zone-id) delta-positions)))
      ;; Compact format (network-optimized, see docs/net.md 4-Prong)
      ((or (eq format :compact-v1)
           (eq format :compact-v2)
           (eq format :compact-v3)
           (eq format :compact-v4)
           (eq format :compact-v5))
       (deserialize-game-state-compact plist game)
       (values (getf plist :zone-id) nil))
      ;; Legacy plist format (DB saves, compatibility)
      (t
       (let* ((version (getf plist :version 0))
              (zone-id (getf plist :zone-id))
              (id-next (getf plist :id-next 1))
              (player-plists (or (getf plist :players)
                                 (let ((player-plist (getf plist :player)))
                                   (and player-plist (list player-plist)))))
              (npc-plists (getf plist :npcs))
              (object-plists (getf plist :objects))
              (npcs (game-npcs game))
              (world (game-world game))
              (zone (world-zone world))
              (id-source (game-id-source game)))
         ;; Version check (for future migrations)
         (when (> version *save-format-version*)
           (warn "Save file version ~d is newer than current version ~d"
                 version *save-format-version*))
         ;; Restore ID source
         (when id-source
           (setf (id-source-next-id id-source) id-next))
         ;; Restore players
         (apply-player-plists game player-plists)
         ;; Restore NPCs
         (loop :for npc-plist :in npc-plists
               :for index :from 0
               :do (deserialize-npc npc-plist npcs index))
         ;; Restore zone objects
         (when zone
           (setf (zone-objects zone)
                 (mapcar #'deserialize-object object-plists)))
         ;; Return zone ID for zone switching if needed
         (values zone-id nil))))))

(defun apply-game-state (game state &key (apply-zone t))
  ;; Apply serialized STATE into GAME, loading zones when needed.
  ;; Returns: (values zone-id zone-loaded delta-positions-or-nil)
  ;; When a zone change is needed but fails (client cache miss), the entire
  ;; snapshot is dropped to prevent desync (player in wrong zone with new-zone data).
  (when (and game state)
    (let* ((zone-id (getf state :zone-id))
           (world (game-world game))
           (current-zone (and world (world-zone world)))
           (current-zone-id (and current-zone (zone-id current-zone)))
           (zone-loaded nil)
           (zone-change-failed nil))
      (when (and apply-zone zone-id (not (eql zone-id current-zone-id)))
        (let* ((graph (and world (world-world-graph world)))
               (path (and graph (world-graph-zone-path graph zone-id)))
               ;; Step 4: Check LRU cache before loading from disk
               (zone-lru (and game (game-zone-cache game)))
               (cached-zone (and zone-lru (zone-cache-lookup zone-lru zone-id))))
          ;; ADDENDUM 3: No sync load on client transition path.
          ;; Preloading (Step 5) + urgent preload should guarantee cache hit.
          ;; Network clients skip the sync fallback entirely to prevent hitches.
          ;; Server/local modes still have the disk fallback (they don't preload).
          (let* ((is-client (eq (game-net-role game) :client))
                 (zone (or cached-zone
                           (cond
                             ;; Client: warn and drop entire snapshot — applying new-zone
                             ;; positions into old zone would cause desync
                             ((and is-client (not cached-zone) path)
                              (warn "Zone ~a: cache MISS on client transition — snapshot dropped (preload gap)"
                                    zone-id)
                              (setf zone-change-failed t)
                              nil)
                             ;; Server/local: sync load fallback with retry
                             ((and path (probe-file path))
                              (let ((loaded (load-zone path)))
                                ;; Insert into LRU cache for future transitions
                                (when (and loaded zone-lru)
                                  (zone-cache-insert zone-lru zone-id loaded))
                                loaded))))))
            (when zone
              (when path (setf *zone-path* path))
              (apply-zone-to-world world zone)
              ;; Call client hook to clear render caches (set by rendering.lisp)
              (when *client-zone-change-hook*
                (funcall *client-zone-change-hook* (zone-id zone)))
              (setf zone-loaded t)))))
      ;; If zone change was needed but failed on client, drop the entire snapshot.
      ;; Next server tick will resend; by then preload should have the zone cached.
      (when zone-change-failed
        (return-from apply-game-state (values zone-id nil nil)))
      (when zone-loaded
        (let* ((player (game-player game))
               (players (game-players game))
               (npcs (make-npcs player world
                                :id-source (game-npc-id-source game))))
          (ensure-npcs-open-spawn npcs world)
          (setf (game-npcs game) npcs
                (game-entities game) (make-entities players npcs))))
      (multiple-value-bind (result-zone-id delta-positions)
          (deserialize-game-state state game)
        (values result-zone-id zone-loaded delta-positions)))))

(defun save-game (game filepath)
  ;; Save game state to FILEPATH. Returns T on success, NIL on failure (non-fatal).
  (handler-case
      (progn
        (log-verbose "Serializing game state for save")
        (let ((state (serialize-game-state game)))
          (with-open-file (out filepath
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
            (prin1 state out))
          (format t "~&Saved game to ~a~%" filepath)
          (log-verbose "Save completed successfully")
          t))
    (error (e)
      (warn "Failed to save game to ~a: ~a" filepath e)
      (log-verbose "Save error: ~a" e)
      nil)))

(defun load-game (game filepath &key (apply-zone t))
  ;; Load game state from FILEPATH into existing GAME.
  ;; Returns zone-id on success, NIL on failure (non-fatal).
  (if (probe-file filepath)
      (handler-case
          (progn
            (log-verbose "Loading game from ~a" filepath)
            (let* ((state (with-open-file (in filepath :direction :input)
                            (let ((*read-eval* nil)) ; Security: disable eval in read
                              (read in)))))
              (multiple-value-bind (loaded-zone-id _zone-loaded)
                  (apply-game-state game state :apply-zone apply-zone)
                (declare (ignore _zone-loaded))
                (format t "~&Loaded game from ~a (zone: ~a)~%" filepath loaded-zone-id)
                (log-verbose "Load completed, zone transitioned to ~a" loaded-zone-id)
                loaded-zone-id)))
        (error (e)
          (warn "Failed to load game from ~a: ~a" filepath e)
          (log-verbose "Load error (possibly corrupt save): ~a" e)
          nil))
      (progn
        (warn "Save file not found: ~a" filepath)
        nil)))
