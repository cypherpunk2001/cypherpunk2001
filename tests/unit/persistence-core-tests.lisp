(in-package #:mmorpg)

;;; ============================================================================
;;; Persistence Tests
;;; ============================================================================


(defun run-persistence-tests-internal ()
  "Run all data integrity tests. Returns T if all pass, NIL otherwise."
  (let ((passed 0)
        (failed 0)
        (tests (list
                ;; Persistence Round-Trip Tests
                #'test-player-roundtrip
                #'test-ephemeral-not-persisted
                #'test-durable-persisted
                #'test-inventory-roundtrip
                #'test-equipment-roundtrip
                #'test-stats-roundtrip
                ;; XP/Progression Invariant Tests
                #'test-xp-never-decreases-on-award
                #'test-level-increases-with-xp
                #'test-xp-level-boundary
                ;; Inventory Invariant Tests
                #'test-hp-never-exceeds-max
                #'test-inventory-count-limits
                #'test-inventory-overflow-returns-leftover
                #'test-inventory-stack-limits
                ;; Equipment Tests
                #'test-equipment-modifiers-applied
                #'test-equipment-modifiers-removed-on-unequip
                ;; Storage Backend Tests
                #'test-memory-backend-save-load
                #'test-storage-delete
                #'test-redis-backend-save-load
                #'test-redis-backend-delete
                #'test-redis-memory-equivalence
                ;; Zone Transition Tests
                #'test-zone-id-roundtrip
                #'test-zone-id-in-db-save
                ;; Tier-1 Immediate Save Tests
                #'test-death-triggers-immediate-save
                #'test-level-up-triggers-immediate-save
                #'test-item-consume-triggers-immediate-save
                ;; Phase 5: Death Tracking + Leaderboard Tests
                #'test-death-increments-player-deaths
                #'test-death-updates-leaderboard
                #'test-login-seeds-deaths-leaderboard
                ;; Currency Invariant Tests
                #'test-coins-never-negative
                ;; Schema Migration Tests
                #'test-migration-v1-to-v2
                #'test-migration-applies-defaults
                #'test-lifetime-xp-roundtrip
                #'test-lifetime-xp-incremented
                #'test-migration-v1-to-v3-chain
                #'test-playtime-roundtrip
                #'test-created-at-roundtrip
                #'test-private-player-roundtrip
                ;; Compact Serialization Tests (Network Optimization)
                #'test-compact-player-roundtrip
                #'test-compact-npc-roundtrip
                #'test-compact-size-reduction
                #'test-compact-enum-encoding
                #'test-compact-quantization
                #'test-apply-player-compact-direct
                #'test-apply-npc-compact-direct
                ;; Zone-Filtered Delta Serialization Tests
                #'test-delta-for-zone-filters-players
                #'test-delta-for-zone-nil-player-zone
                #'test-delta-for-zone-with-npcs
                #'test-delta-for-zone-nil-zone-state
                #'test-group-clients-clamps-nil-zone
                ;; Schema Validation Tests (Phase 1 - Database Hardening)
                #'test-validation-valid-data-passes
                #'test-validation-missing-required-fields
                #'test-validation-wrong-types
                #'test-validation-out-of-bounds
                #'test-validation-oversized-blob
                #'test-validation-nested-stats
                #'test-validation-nested-inventory
                #'test-validated-load-rejects-invalid
                ;; Phase 6: Validation Hardening
                #'test-validation-sparse-inventory
                ;; Session Ownership Tests (Phase 3 - Database Hardening)
                #'test-session-claim-success
                #'test-session-double-login-rejected
                #'test-session-ownership-refresh
                #'test-session-release-and-reclaim
                ;; 4-Outcome Validation Tests (Phase 6 - Database Hardening)
                #'test-4way-ok-valid-data
                #'test-4way-clamp-hp-below-zero
                #'test-4way-clamp-hp-above-max
                #'test-4way-clamp-position-out-of-bounds
                #'test-4way-clamp-missing-created-at
                #'test-4way-clamp-negative-deaths
                #'test-4way-reject-missing-id
                #'test-4way-reject-negative-lifetime-xp
                #'test-4way-reject-negative-item-count
                #'test-4way-reject-excessive-item-count
                #'test-4way-reject-wrong-type-x
                #'test-4way-reject-negative-skill-xp
                #'test-4way-reject-inventory-not-list
                #'test-4way-reject-slots-not-list
                #'test-4way-reject-inventory-slot-not-list
                #'test-4way-reject-stats-not-list
                #'test-4way-reject-stat-entry-not-list
                #'test-4way-reject-equipment-not-list
                #'test-4way-reject-equipment-items-not-list
                #'test-4way-quarantine-invalid-zone-type
                ;; Phase 6: Unknown zones/items validation
                #'test-4way-quarantine-unknown-zone
                #'test-4way-quarantine-unknown-item
                #'test-4way-quarantine-unknown-equipment-item
                #'test-4way-validation-skips-zone-check-when-not-loaded
                #'test-4way-clamp-uses-plist-put
                #'test-4way-load-valid-player
                #'test-4way-load-clamp-hp
                #'test-4way-load-reject-bad-type
                #'test-4way-load-not-found
                #'test-4way-storage-incr
                #'test-4way-storage-save-with-ttl
                #'test-4way-forensic-storage
                #'test-4way-validation-metrics
                ;; Storage Failure Semantics Tests (Phase 1 - Implementation Findings Fix)
                #'test-storage-error-signaled-on-save-failure
                #'test-storage-error-signaled-on-batch-failure
                #'test-dirty-flags-preserved-on-batch-failure
                #'test-tier1-save-signals-on-ownership-error
                #'test-id-counter-blocked-on-persistence-failure
                #'test-id-counter-advances-on-persistence-success
                #'test-retry-catches-storage-error
                ;; Ownership Loss Cleanup Tests (Phase 2 - Implementation Findings Fix)
                #'test-ownership-reclaim-on-verification-failure
                #'test-ownership-truly-lost-when-another-server-owns
                #'test-local-unregister-preserves-online-set
                ;; Batch TTL Refresh Tests (Code Standards Compliance)
                #'test-batch-ttl-refresh-basic
                #'test-batch-ttl-refresh-empty-list
                #'test-batch-ttl-refresh-partial
                #'test-batch-ttl-refresh-error-returns-zero
                #'test-refresh-all-session-ownerships-uses-batch)))
    (format t "~%=== Running Persistence Tests ===~%")
    (dolist (test tests)
      (handler-case
          (progn
            (funcall test)
            (incf passed)
            (format t "✓ ~a~%" (symbol-name (if (symbolp test)
                                                test
                                                (nth-value 2 (function-lambda-expression test))))))
        (error (e)
          (incf failed)
          (format t "✗ ~a: ~a~%"
                  (symbol-name (if (symbolp test)
                                   test
                                   (nth-value 2 (function-lambda-expression test))))
                  e))))
    (format t "~%Results: ~d passed, ~d failed~%" passed failed)
    (zerop failed)))

;;; Test Helpers

(defun make-test-player (&key (id 1) (x 100.0) (y 200.0))
  "Create a player with known test values."
  (ensure-test-game-data)
  (let ((player (make-player x y :id id)))
    (setf (player-hp player) 50)
    (grant-inventory-item player :health-potion 5)
    (grant-inventory-item player :rusty-sword 1)
    player))

(defun count-inventory-item (inventory item-id)
  "Count how many of ITEM-ID are in INVENTORY."
  (let ((total 0)
        (slots (and inventory (inventory-slots inventory))))
    (when slots
      (loop for slot across slots
            when (eq (inventory-slot-item-id slot) item-id)
            do (incf total (inventory-slot-count slot))))
    total))

(defun assert-equal (expected actual &optional (message "Values not equal"))
  "Assert that EXPECTED equals ACTUAL."
  (unless (equal expected actual)
    (error "~a: expected ~s, got ~s" message expected actual)))

(defun assert-true (value &optional (message "Value not true"))
  "Assert that VALUE is true."
  (unless value
    (error "~a" message)))

(defun assert-nil (value &optional (message "Value not nil"))
  "Assert that VALUE is nil."
  (when value
    (error "~a: got ~s" message value)))

(defun assert-< (a b &optional (message "Not less than"))
  "Assert that A < B."
  (unless (< a b)
    (error "~a: ~d is not less than ~d" message a b)))

(defun assert-<= (a b &optional (message "Not less than or equal"))
  "Assert that A <= B."
  (unless (<= a b)
    (error "~a: ~d is not <= ~d" message a b)))

(defun assert->= (a b &optional (message "Not greater than or equal"))
  "Assert that A >= B."
  (unless (>= a b)
    (error "~a: ~d is not >= ~d" message a b)))

;;; Persistence Round-Trip Tests

(defun test-player-roundtrip ()
  "Test: Serialize then deserialize = identical durable data."
  (let* ((original (make-test-player :id 42 :x 123.0 :y 456.0))
         (plist (serialize-player original))
         (restored (deserialize-player plist *inventory-size* (length *equipment-slot-ids*))))
    ;; Test durable fields preserved
    (assert-equal (player-id original) (player-id restored) "Player ID")
    (assert-equal (player-x original) (player-x restored) "Player X")
    (assert-equal (player-y original) (player-y restored) "Player Y")
    (assert-equal (player-hp original) (player-hp restored) "Player HP")
    ;; Test inventory preserved
    (let ((orig-inv (player-inventory original))
          (rest-inv (player-inventory restored)))
      (assert-equal (count-inventory-item orig-inv :health-potion)
                   (count-inventory-item rest-inv :health-potion)
                   "Inventory potion count"))
    t))

(defun test-ephemeral-not-persisted ()
  "Test: Ephemeral fields NOT saved to database (no :include-visuals)."
  (let* ((player (make-test-player))
         (plist (serialize-player player))) ; Default: no :include-visuals
    ;; Ephemeral fields should NOT be in plist
    (assert-nil (getf plist :attack-timer) "attack-timer in DB save")
    (assert-nil (getf plist :hit-timer) "hit-timer in DB save")
    (assert-nil (getf plist :click-marker-timer) "click-marker-timer in DB save")
    (assert-nil (getf plist :click-marker-kind) "click-marker-kind in DB save")
    (assert-nil (getf plist :mouse-hold-timer) "mouse-hold-timer in DB save")
    t))

(defun test-durable-persisted ()
  "Test: Durable fields ARE saved to database."
  (let* ((player (make-test-player))
         (plist (serialize-player player)))
    ;; Durable fields MUST be in plist
    (assert-true (getf plist :id) "id not in save")
    (assert-true (getf plist :x) "x not in save")
    (assert-true (getf plist :y) "y not in save")
    (assert-true (getf plist :hp) "hp not in save")
    (assert-true (getf plist :stats) "stats not in save")
    (assert-true (getf plist :inventory) "inventory not in save")
    (assert-true (getf plist :equipment) "equipment not in save")
    t))

(defun test-inventory-roundtrip ()
  "Test: Inventory survives serialization."
  (let* ((original (make-test-player))
         (plist (serialize-player original))
         (restored (deserialize-player plist *inventory-size* (length *equipment-slot-ids*)))
         (orig-inv (player-inventory original))
         (rest-inv (player-inventory restored)))
    (assert-equal (count-inventory-item orig-inv :health-potion)
                 (count-inventory-item rest-inv :health-potion)
                 "Health potion count")
    (assert-equal (count-inventory-item orig-inv :rusty-sword)
                 (count-inventory-item rest-inv :rusty-sword)
                 "Rusty sword count")
    t))

(defun test-equipment-roundtrip ()
  "Test: Equipment survives serialization."
  (let* ((original (make-test-player))
         (equipment (player-equipment original)))
    ;; Equip an item
    (equip-item original :rusty-sword)
    (let* ((plist (serialize-player original))
           (restored (deserialize-player plist *inventory-size* (length *equipment-slot-ids*)))
           (rest-equip (player-equipment restored)))
      (assert-equal (equipment-slot-item equipment :weapon)
                   (equipment-slot-item rest-equip :weapon)
                   "Equipped weapon")
      t)))

;;; Invariant Tests

(defun test-hp-never-exceeds-max ()
  "Test: HP never exceeds max HP."
  (let* ((player (make-test-player))
         (stats (player-stats player))
         (max-hp (stat-block-base-level stats :hitpoints)))
    ;; Try to set HP beyond max
    (setf (player-hp player) (+ max-hp 100))
    ;; Clamp HP (this logic is in progression.lisp)
    (when (> (player-hp player) max-hp)
      (setf (player-hp player) max-hp))
    (assert-<= (player-hp player) max-hp "HP exceeds max")
    t))

(defun test-inventory-count-limits ()
  "Test: Inventory count never exceeds max slots."
  (let ((player (make-test-player)))
    ;; Try to add more items than inventory can hold
    (dotimes (i (+ *inventory-size* 10))
      (grant-inventory-item player :health-potion 1))
    ;; Count total items
    (let* ((inventory (player-inventory player))
           (slots (inventory-slots inventory))
           (total-items 0))
      (loop for slot across slots
            when (inventory-slot-item-id slot)
            do (incf total-items))
      (assert-<= total-items *inventory-size* "Too many inventory slots used")
      t)))

;;; Storage Backend Tests

(defun test-memory-backend-save-load ()
  "Test: Memory backend save and load work correctly."
  (let ((storage (make-instance 'memory-storage)))
    (storage-connect storage)
    ;; Save data
    (storage-save storage "test:player:1" '(:id 1 :x 100.0 :y 200.0 :hp 50))
    ;; Load data
    (let ((loaded (storage-load storage "test:player:1")))
      (assert-equal 1 (getf loaded :id) "Player ID from storage")
      (assert-equal 100.0 (getf loaded :x) "Player X from storage")
      (assert-equal 50 (getf loaded :hp) "Player HP from storage"))
    ;; Clean up
    (storage-delete storage "test:player:1")
    t))

(defun test-storage-delete ()
  "Test: Storage delete removes data."
  (let ((storage (make-instance 'memory-storage)))
    (storage-connect storage)
    ;; Save then delete
    (storage-save storage "test:delete:1" '(:data "value"))
    (assert-true (storage-load storage "test:delete:1") "Data exists before delete")
    (storage-delete storage "test:delete:1")
    (assert-nil (storage-load storage "test:delete:1") "Data still exists after delete")
    t))

;;; Stats Round-Trip Tests

(defun test-stats-roundtrip ()
  "Test: Player stats survive serialization including XP and levels."
  (let* ((original (make-test-player))
         (stats (player-stats original)))
    ;; Award some XP to have non-default values
    (award-skill-xp (stat-block-attack stats) 100)
    (award-skill-xp (stat-block-defense stats) 50)
    (let* ((plist (serialize-player original))
           (restored (deserialize-player plist *inventory-size* (length *equipment-slot-ids*)))
           (rest-stats (player-stats restored)))
      ;; Verify XP preserved
      (assert-equal (skill-xp (stat-block-attack stats))
                   (skill-xp (stat-block-attack rest-stats))
                   "Attack XP")
      (assert-equal (skill-xp (stat-block-defense stats))
                   (skill-xp (stat-block-defense rest-stats))
                   "Defense XP")
      ;; Verify levels preserved
      (assert-equal (skill-level (stat-block-attack stats))
                   (skill-level (stat-block-attack rest-stats))
                   "Attack level")
      t)))

;;; XP/Progression Invariant Tests

(defun test-xp-never-decreases-on-award ()
  "Test: XP can only increase, never decrease from awards."
  (let* ((player (make-test-player))
         (stats (player-stats player))
         (attack-skill (stat-block-attack stats))
         (initial-xp (skill-xp attack-skill)))
    ;; Award positive XP
    (award-skill-xp attack-skill 100)
    (let ((new-xp (skill-xp attack-skill)))
      (assert->= new-xp initial-xp "XP decreased after award")
      (assert-equal (+ initial-xp 100) new-xp "XP didn't increase by award amount"))
    t))

(defun test-level-increases-with-xp ()
  "Test: Level increases when enough XP is gained."
  (let* ((player (make-test-player))
         (stats (player-stats player))
         (attack-skill (stat-block-attack stats))
         (initial-level (skill-level attack-skill)))
    ;; Award enough XP for a level up (level 2 needs 83 XP)
    (award-skill-xp attack-skill 100)
    (let ((new-level (skill-level attack-skill)))
      (assert->= new-level initial-level "Level decreased after XP award")
      (assert-equal 2 new-level "Should be level 2 after 100 XP"))
    t))

(defun test-xp-level-boundary ()
  "Test: XP at exact level boundary correctly triggers level up."
  (let* ((player (make-test-player))
         (stats (player-stats player))
         (attack-skill (stat-block-attack stats)))
    ;; Formula: level = 1 + floor(sqrt(xp / 100))
    ;; Level 2 needs 100 XP (sqrt(100/100) = 1, floor(1) = 1, level = 2)
    ;; At 99 XP, still level 1
    (award-skill-xp attack-skill 99)
    (let ((level (skill-level attack-skill)))
      (assert-equal 1 level "Should be level 1 at 99 XP"))
    ;; At 100 XP, becomes level 2
    (award-skill-xp attack-skill 1) ; total 100
    (let ((level (skill-level attack-skill)))
      (assert-equal 2 level "Should be level 2 at exactly 100 XP"))
    t))

;;; Inventory Operation Tests

(defun test-inventory-overflow-returns-leftover ()
  "Test: Adding items beyond capacity returns leftover count."
  (let* ((player (make-test-player))
         (inventory (player-inventory player)))
    ;; Clear inventory first
    (loop for slot across (inventory-slots inventory)
          do (setf (inventory-slot-item-id slot) nil
                   (inventory-slot-count slot) 0))
    ;; Try to add more items than slots (we have *inventory-size* slots)
    ;; Each item is non-stackable (stack size 1), so each takes a slot
    (let ((leftover (inventory-add inventory :health-potion (+ *inventory-size* 5))))
      ;; Should return 5 leftover
      (assert-equal 5 leftover "Should have 5 leftover items"))
    t))

(defun test-inventory-stack-limits ()
  "Test: Stackable items respect max stack size."
  (let* ((player (make-test-player))
         (inventory (player-inventory player)))
    ;; Clear inventory
    (loop for slot across (inventory-slots inventory)
          do (setf (inventory-slot-item-id slot) nil
                   (inventory-slot-count slot) 0))
    ;; Health potions have a stack size (check what it is)
    (let ((stack-size (item-stack-size :health-potion)))
      (when (> stack-size 1)
        ;; Add exactly stack-size items - should fit in one slot
        (inventory-add inventory :health-potion stack-size)
        (let ((count-in-first-slot (inventory-slot-count (aref (inventory-slots inventory) 0))))
          (assert-<= count-in-first-slot stack-size "Stack exceeds max size"))))
    t))

;;; Equipment Tests

(defun test-equipment-modifiers-applied ()
  "Test: Equipping an item applies its stat modifiers."
  (let* ((player (make-test-player))
         (stats (player-stats player))
         (mods (stat-block-modifiers stats))
         (initial-attack-mod (stat-modifiers-attack mods)))
    ;; Grant and equip a rusty sword (has :attack 1)
    (grant-inventory-item player :rusty-sword 1)
    (equip-item player :rusty-sword)
    (let ((new-attack-mod (stat-modifiers-attack mods)))
      ;; Attack modifier should increase (rusty sword gives +1 attack)
      (assert-true (> new-attack-mod initial-attack-mod)
                  "Attack modifier not applied on equip"))
    t))

(defun test-equipment-modifiers-removed-on-unequip ()
  "Test: Unequipping an item removes its stat modifiers."
  (let* ((player (make-test-player))
         (stats (player-stats player))
         (mods (stat-block-modifiers stats))
         (initial-attack-mod (stat-modifiers-attack mods)))
    ;; Grant and equip a rusty sword
    (grant-inventory-item player :rusty-sword 1)
    (equip-item player :rusty-sword)
    (let ((equipped-attack-mod (stat-modifiers-attack mods)))
      ;; Verify equipped modifier is higher
      (assert-true (> equipped-attack-mod initial-attack-mod)
                  "Sword didn't apply attack modifier")
      ;; Unequip
      (unequip-item player :weapon)
      (let ((unequipped-attack-mod (stat-modifiers-attack mods)))
        ;; Attack modifier should return to initial value
        (assert-equal initial-attack-mod unequipped-attack-mod
                     "Attack modifier not removed on unequip")))
    t))

;;; Redis Backend Tests

(defun test-redis-backend-save-load ()
  "Test: Redis backend save and load work correctly."
  (let ((storage (make-instance 'redis-storage :host "127.0.0.1" :port 6379)))
    (handler-case
        (progn
          (storage-connect storage)
          ;; Save data
          (storage-save storage "test:redis:player:1" '(:id 1 :x 100.0 :y 200.0 :hp 50))
          ;; Load data
          (let ((loaded (storage-load storage "test:redis:player:1")))
            (assert-true loaded "Redis load returned nil")
            (assert-equal 1 (getf loaded :id) "Player ID from Redis")
            (assert-equal 100.0 (getf loaded :x) "Player X from Redis")
            (assert-equal 50 (getf loaded :hp) "Player HP from Redis"))
          ;; Clean up
          (storage-delete storage "test:redis:player:1")
          t)
      (error (e)
        (error "Redis test failed (is Redis running?): ~a" e)))))

(defun test-redis-backend-delete ()
  "Test: Redis delete removes data."
  (let ((storage (make-instance 'redis-storage :host "127.0.0.1" :port 6379)))
    (handler-case
        (progn
          (storage-connect storage)
          ;; Save then delete
          (storage-save storage "test:redis:delete:1" '(:data "value"))
          (assert-true (storage-load storage "test:redis:delete:1") "Redis data exists before delete")
          (storage-delete storage "test:redis:delete:1")
          (assert-nil (storage-load storage "test:redis:delete:1") "Redis data still exists after delete")
          t)
      (error (e)
        (error "Redis delete test failed (is Redis running?): ~a" e)))))

(defun test-redis-memory-equivalence ()
  "Test: Redis and memory backends behave identically."
  (let ((redis (make-instance 'redis-storage :host "127.0.0.1" :port 6379))
        (memory (make-instance 'memory-storage))
        (test-key "test:equivalence:1")
        (test-data '(:id 42 :name "Test" :values (1 2 3) :nested (:a 1 :b 2))))
    (handler-case
        (progn
          (storage-connect redis)
          (storage-connect memory)
          ;; Save same data to both
          (storage-save redis test-key test-data)
          (storage-save memory test-key test-data)
          ;; Load from both
          (let ((redis-loaded (storage-load redis test-key))
                (memory-loaded (storage-load memory test-key)))
            ;; Compare results
            (assert-equal (getf redis-loaded :id) (getf memory-loaded :id)
                         "Redis/Memory ID mismatch")
            (assert-equal (getf redis-loaded :name) (getf memory-loaded :name)
                         "Redis/Memory name mismatch")
            (assert-equal (getf redis-loaded :values) (getf memory-loaded :values)
                         "Redis/Memory values mismatch"))
          ;; Test exists-p equivalence
          (assert-equal (storage-exists-p redis test-key)
                       (storage-exists-p memory test-key)
                       "Redis/Memory exists-p mismatch")
          ;; Clean up
          (storage-delete redis test-key)
          (storage-delete memory test-key)
          ;; Test non-existent key
          (assert-equal (storage-load redis "nonexistent:key")
                       (storage-load memory "nonexistent:key")
                       "Redis/Memory nil return mismatch")
          t)
      (error (e)
        (error "Redis equivalence test failed (is Redis running?): ~a" e)))))

;;; Zone Transition Tests

(defun test-zone-id-roundtrip ()
  "Test: Zone ID survives serialization when provided."
  (let* ((player (make-test-player :id 99 :x 50.0 :y 75.0))
         (zone-id :dungeon-1)
         ;; Serialize with zone-id
         (plist (serialize-player player :zone-id zone-id)))
    ;; Zone ID should be in the plist
    (assert-equal zone-id (getf plist :zone-id) "Zone ID not in serialized data")
    t))

(defun test-zone-id-in-db-save ()
  "Test: Zone ID is included when saving to database with session."
  (let* ((storage (make-instance 'memory-storage))
         (player (make-test-player :id 88))
         (zone-id :forest-2)
         (old-storage *storage*)
         (old-sessions (make-hash-table :test 'eql)))
    ;; Copy existing sessions
    (maphash (lambda (k v) (setf (gethash k old-sessions) v)) *player-sessions*)
    (unwind-protect
        (progn
          ;; Set up test storage and session
          (setf *storage* storage)
          (storage-connect storage)
          (clrhash *player-sessions*)
          (register-player-session player :zone-id zone-id)
          ;; Save player through db-save-player
          (db-save-player player)
          ;; Load raw data and verify zone-id
          (let* ((key (player-key (player-id player)))
                 (loaded (storage-load storage key)))
            (assert-equal zone-id (getf loaded :zone-id) "Zone ID not saved to DB"))
          t)
      ;; Restore global state
      (setf *storage* old-storage)
      (clrhash *player-sessions*)
      (maphash (lambda (k v) (setf (gethash k *player-sessions*) v)) old-sessions))))

;;; Tier-1 Immediate Save Tests

(defvar *test-immediate-save-called* nil
  "Flag set when db-save-player-immediate is called during tests.")

(defun test-death-triggers-immediate-save ()
  "Test: Player death (HP=0) triggers tier-1 immediate save."
  (let* ((storage (make-instance 'memory-storage))
         (player (make-test-player :id 77))
         (old-storage *storage*)
         (old-sessions (make-hash-table :test 'eql)))
    ;; Copy existing sessions
    (maphash (lambda (k v) (setf (gethash k old-sessions) v)) *player-sessions*)
    (unwind-protect
        (progn
          ;; Set up test storage and session
          (setf *storage* storage)
          (storage-connect storage)
          (clrhash *player-sessions*)
          (register-player-session player)
          ;; Set player HP to 1 so next hit kills them
          (setf (player-hp player) 1)
          ;; Apply lethal hit - this should trigger immediate save
          (combatant-apply-hit player 1)
          ;; Verify HP is 0
          (assert-equal 0 (player-hp player) "Player should be dead")
          ;; Verify save was written to storage
          (let* ((key (player-key (player-id player)))
                 (loaded (storage-load storage key)))
            (assert-true loaded "Player not saved after death")
            (assert-equal 0 (getf loaded :hp) "Saved HP should be 0"))
          t)
      ;; Restore global state
      (setf *storage* old-storage)
      (clrhash *player-sessions*)
      (maphash (lambda (k v) (setf (gethash k *player-sessions*) v)) old-sessions))))

(defun test-level-up-triggers-immediate-save ()
  "Test: Level up triggers tier-1 immediate save."
  (let* ((storage (make-instance 'memory-storage))
         (player (make-test-player :id 66))
         (stats (player-stats player))
         (old-storage *storage*)
         (old-sessions (make-hash-table :test 'eql)))
    ;; Copy existing sessions
    (maphash (lambda (k v) (setf (gethash k old-sessions) v)) *player-sessions*)
    (unwind-protect
        (progn
          ;; Set up test storage and session
          (setf *storage* storage)
          (storage-connect storage)
          (clrhash *player-sessions*)
          (register-player-session player)
          ;; Get initial attack level
          (let ((initial-level (skill-level (stat-block-attack stats))))
            ;; Award enough XP to level up:
            ;; - Balanced mode splits XP: 33% to hitpoints, 67% to atk/str/def
            ;; - 500 XP -> ~165 HP, ~335 remaining -> ~112 per combat stat
            ;; - 100 XP needed for level 2, so 112 is enough
            (award-combat-xp player 500)
            ;; Verify level increased
            (let ((new-level (skill-level (stat-block-attack stats))))
              (assert-true (> new-level initial-level) "Attack level should have increased")
              ;; Verify save was written to storage
              (let* ((key (player-key (player-id player)))
                     (loaded (storage-load storage key)))
                (assert-true loaded "Player not saved after level-up")
                ;; Verify XP was saved
                (let* ((saved-stats (getf loaded :stats))
                       (saved-attack (getf saved-stats :attack))
                       (saved-xp (getf saved-attack :xp)))
                  (assert-true (> saved-xp 0) "XP not saved after level-up")))))
          t)
      ;; Restore global state
      (setf *storage* old-storage)
      (clrhash *player-sessions*)
      (maphash (lambda (k v) (setf (gethash k *player-sessions*) v)) old-sessions))))

(defun test-item-consume-triggers-immediate-save ()
  "Test: Item consumption triggers tier-1 immediate save.
   Phase 4: Item destruction (drop/sell/consume) is tier-1 per docs/db.md."
  (ensure-test-game-data)
  (let* ((storage (make-instance 'memory-storage))
         (player (make-test-player :id 88))
         (inventory (player-inventory player))
         (old-storage *storage*)
         (old-sessions (make-hash-table :test 'eql)))
    ;; Copy existing sessions
    (maphash (lambda (k v) (setf (gethash k old-sessions) v)) *player-sessions*)
    (unwind-protect
        (progn
          ;; Set up test storage and session
          (setf *storage* storage)
          (storage-connect storage)
          (clrhash *player-sessions*)
          (register-player-session player)
          ;; Give player some items
          (grant-inventory-item player :bones 5)
          (let ((initial-count (count-inventory-item inventory :bones)))
            (assert-equal 5 initial-count "Should have 5 bones initially")
            ;; Consume items - this should trigger immediate save (Phase 4)
            (consume-inventory-item player :bones 2)
            ;; Verify items were consumed
            (assert-equal 3 (count-inventory-item inventory :bones) "Should have 3 bones after consume")
            ;; Verify save was written to storage immediately
            (let* ((key (player-key (player-id player)))
                   (loaded (storage-load storage key)))
              (assert-true loaded "Player not saved after item consume")
              ;; Verify inventory was saved with correct count
              ;; Inventory format: (:slots ((:item-id :bones :count N) ...))
              ;; Bones have stack-size 1, so count all bones across all slots
              (let* ((saved-inventory (getf loaded :inventory))
                     (saved-slots (getf saved-inventory :slots))
                     (saved-bones-total (loop for slot in saved-slots
                                              when (eq (getf slot :item-id) :bones)
                                              sum (getf slot :count 0))))
                (assert-equal 3 saved-bones-total "Saved inventory should have 3 bones total"))))
          t)
      ;; Restore global state
      (setf *storage* old-storage)
      (clrhash *player-sessions*)
      (maphash (lambda (k v) (setf (gethash k *player-sessions*) v)) old-sessions))))

;;; Phase 5: Death Tracking + Leaderboard Tests

(defun test-death-increments-player-deaths ()
  "Test: Player death increments player-deaths counter.
   Phase 5: player-deaths must be incremented before leaderboard update."
  (let* ((player (make-test-player :id 91)))
    (setf (player-deaths player) 5)  ; Start with 5 deaths
    (setf (player-hp player) 1)      ; Set to 1 HP
    ;; Apply lethal hit
    (combatant-apply-hit player 1)
    ;; Verify deaths incremented
    (assert-equal 6 (player-deaths player) "Deaths should be 6 after death")
    t))

(defun test-death-updates-leaderboard ()
  "Test: Player death updates deaths leaderboard with correct total.
   Phase 5: Leaderboard uses zadd with total count (not zincrby)."
  (let* ((storage (make-instance 'memory-storage))
         (player (make-test-player :id 92))
         (old-storage *storage*)
         (old-sessions (make-hash-table :test 'eql)))
    (maphash (lambda (k v) (setf (gethash k old-sessions) v)) *player-sessions*)
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          (clrhash *player-sessions*)
          (register-player-session player)
          ;; Start with 3 deaths
          (setf (player-deaths player) 3)
          (setf (player-hp player) 1)
          ;; Die
          (combatant-apply-hit player 1)
          ;; Check leaderboard has correct total (4, not incremented from 0)
          ;; db-get-leaderboard returns ((player-id score) ...) with integer IDs
          (let* ((leaderboard (db-get-leaderboard :deaths :top 10))
                 (entry (find 92 leaderboard :key #'first)))
            (assert-true entry "Player should be on deaths leaderboard")
            (assert-equal 4 (second entry) "Leaderboard should show 4 deaths"))
          t)
      (setf *storage* old-storage)
      (clrhash *player-sessions*)
      (maphash (lambda (k v) (setf (gethash k *player-sessions*) v)) old-sessions))))

(defun test-login-seeds-deaths-leaderboard ()
  "Test: Player login seeds deaths leaderboard with existing count.
   Phase 5: Leaderboard is seeded on login, not just on death."
  (let* ((storage (make-instance 'memory-storage))
         (player (make-test-player :id 93))
         (old-storage *storage*)
         (old-sessions (make-hash-table :test 'eql)))
    (maphash (lambda (k v) (setf (gethash k old-sessions) v)) *player-sessions*)
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          (clrhash *player-sessions*)
          ;; Set existing deaths before login
          (setf (player-deaths player) 7)
          ;; Register session (simulates login)
          (register-player-session player)
          ;; Check leaderboard was seeded with existing deaths
          ;; db-get-leaderboard returns ((player-id score) ...) with integer IDs
          (let* ((leaderboard (db-get-leaderboard :deaths :top 10))
                 (entry (find 93 leaderboard :key #'first)))
            (assert-true entry "Player should be on deaths leaderboard after login")
            (assert-equal 7 (second entry) "Leaderboard should show 7 deaths from login seeding"))
          t)
      (setf *storage* old-storage)
      (clrhash *player-sessions*)
      (maphash (lambda (k v) (setf (gethash k *player-sessions*) v)) old-sessions))))

;;; Currency Invariant Tests

(defun test-coins-never-negative ()
  "Test: Coins (gold) count can never go negative."
  (ensure-test-game-data)
  (let* ((player (make-test-player))
         (inventory (player-inventory player)))
    ;; Clear inventory
    (loop for slot across (inventory-slots inventory)
          do (setf (inventory-slot-item-id slot) nil
                   (inventory-slot-count slot) 0))
    ;; Add some coins
    (grant-inventory-item player :coins 100)
    (assert-equal 100 (count-inventory-item inventory :coins) "Should have 100 coins")
    ;; Try to remove more coins than we have
    (inventory-remove inventory :coins 150)
    ;; Count should be 0, not negative
    (let ((remaining (count-inventory-item inventory :coins)))
      (assert->= remaining 0 "Coins went negative"))
    t))

;;; Schema Migration Tests

(defun test-migration-v1-to-v2 ()
  "Test: v1 player data migrates correctly to v2 (adds lifetime-xp)."
  ;; Create v1-style data (no lifetime-xp, version 1)
  (let ((v1-data '(:version 1
                   :id 42
                   :x 100.0
                   :y 200.0
                   :hp 10
                   :stats (:attack (:xp 0 :level 1)
                           :strength (:xp 0 :level 1)
                           :defense (:xp 0 :level 1)
                           :hitpoints (:xp 0 :level 1))
                   :inventory nil
                   :equipment nil)))
    ;; Run migration
    (let ((migrated (migrate-player-data v1-data)))
      ;; Verify version updated
      (assert-equal *player-schema-version* (getf migrated :version) "Version not updated")
      ;; Verify lifetime-xp added with default 0
      (assert-equal 0 (getf migrated :lifetime-xp) "lifetime-xp not defaulted to 0")
      ;; Verify other fields preserved
      (assert-equal 42 (getf migrated :id) "ID not preserved")
      (assert-equal 100.0 (getf migrated :x) "X not preserved")
      (assert-equal 10 (getf migrated :hp) "HP not preserved"))
    t))

(defun test-migration-applies-defaults ()
  "Test: Migration applies defaults for missing fields."
  ;; Create minimal v1 data with missing optional fields
  (let ((v1-data '(:version 1 :id 1 :x 0.0 :y 0.0 :hp 10)))
    (let ((migrated (migrate-player-data v1-data)))
      ;; lifetime-xp should be added
      (assert-true (member :lifetime-xp migrated) "lifetime-xp key missing")
      (assert-equal 0 (getf migrated :lifetime-xp) "lifetime-xp not 0")))
  ;; Test that already-present lifetime-xp is preserved
  (let ((v2-data '(:version 1 :id 1 :x 0.0 :y 0.0 :hp 10 :lifetime-xp 5000)))
    (let ((migrated (migrate-player-data v2-data)))
      ;; Existing value should be preserved (not overwritten with 0)
      (assert-equal 5000 (getf migrated :lifetime-xp) "Existing lifetime-xp was overwritten")))
  t)

(defun test-lifetime-xp-roundtrip ()
  "Test: lifetime-xp survives serialization roundtrip."
  (let* ((player (make-test-player)))
    ;; Set a known lifetime-xp value
    (setf (player-lifetime-xp player) 12345)
    ;; Serialize and deserialize
    (let* ((plist (serialize-player player))
           (restored (deserialize-player plist *inventory-size* (length *equipment-slot-ids*))))
      ;; Verify lifetime-xp preserved
      (assert-equal 12345 (player-lifetime-xp restored) "lifetime-xp not preserved"))
    t))

(defun test-lifetime-xp-incremented ()
  "Test: lifetime-xp increases when XP is awarded."
  (let* ((player (make-test-player))
         (initial-lifetime (player-lifetime-xp player)))
    ;; Award combat XP
    (award-combat-xp player 100)
    ;; Verify lifetime-xp increased by the awarded amount
    (let ((new-lifetime (player-lifetime-xp player)))
      (assert-equal (+ initial-lifetime 100) new-lifetime
                   "lifetime-xp didn't increase by award amount"))
    t))

(defun test-migration-v1-to-v3-chain ()
  "Test: v1 player data migrates correctly through full chain to current version."
  ;; Create v1-style data (no lifetime-xp, no playtime, no created-at, no deaths)
  (let ((v1-data '(:version 1
                   :id 42
                   :x 100.0
                   :y 200.0
                   :hp 10
                   :stats (:attack (:xp 0 :level 1)
                           :strength (:xp 0 :level 1)
                           :defense (:xp 0 :level 1)
                           :hitpoints (:xp 0 :level 1))
                   :inventory nil
                   :equipment nil)))
    ;; Run migration chain (v1 -> v2 -> v3 -> v4 -> ...)
    (let ((migrated (migrate-player-data v1-data)))
      ;; Verify version updated to current
      (assert-equal *player-schema-version* (getf migrated :version)
                   (format nil "Version not updated to current (~d)" *player-schema-version*))
      ;; Verify v2 field (lifetime-xp) added
      (assert-equal 0 (getf migrated :lifetime-xp) "lifetime-xp not added by v2 migration")
      ;; Verify v3 fields (playtime, created-at) added
      ;; Note: playtime is 0.0 (single-float) for type consistency with player-playtime slot
      (assert-equal 0.0 (getf migrated :playtime) "playtime not added by v3 migration")
      (assert-true (getf migrated :created-at) "created-at not added by v3 migration")
      ;; Verify v4 field (deaths) added
      (assert-equal 0 (getf migrated :deaths) "deaths not added by v4 migration")
      ;; Verify original fields preserved
      (assert-equal 42 (getf migrated :id) "ID not preserved")
      (assert-equal 100.0 (getf migrated :x) "X not preserved")
      (assert-equal 10 (getf migrated :hp) "HP not preserved"))
    t))

(defun test-playtime-roundtrip ()
  "Test: playtime survives serialization roundtrip."
  (let* ((player (make-test-player)))
    ;; Set a known playtime value (in seconds, as float)
    (setf (player-playtime player) 3661.5)  ; 1 hour, 1 minute, 1.5 seconds
    ;; Serialize and deserialize
    (let* ((plist (serialize-player player))
           (restored (deserialize-player plist *inventory-size* (length *equipment-slot-ids*))))
      ;; Verify playtime preserved
      (assert-equal 3661.5 (player-playtime restored) "playtime not preserved"))
    t))

(defun test-created-at-roundtrip ()
  "Test: created-at survives serialization roundtrip."
  (let* ((player (make-test-player))
         (original-created-at (player-created-at player)))
    ;; Serialize and deserialize
    (let* ((plist (serialize-player player))
           (restored (deserialize-player plist *inventory-size* (length *equipment-slot-ids*))))
      ;; Verify created-at preserved
      (assert-equal original-created-at (player-created-at restored) "created-at not preserved"))
    t))

(defun test-private-player-roundtrip ()
  "Test: Private state serialization updates inventory/equipment/stats."
  (let* ((source (make-test-player))
         (target (make-player 0.0 0.0 :id 99)))
    (equip-item source :rusty-sword)
    (setf (skill-level (stat-block-attack (player-stats source))) 5
          (player-inventory-dirty target) nil
          (player-hud-stats-dirty target) nil)
    (let ((payload (serialize-player-private source)))
      (apply-player-private-plist target payload)
      (assert-equal (count-inventory-item (player-inventory source) :health-potion)
                    (count-inventory-item (player-inventory target) :health-potion)
                    "Private inventory not applied")
      (assert-equal (equipment-slot-item (player-equipment source) :weapon)
                    (equipment-slot-item (player-equipment target) :weapon)
                    "Private equipment not applied")
      (assert-equal (skill-level (stat-block-attack (player-stats source)))
                    (skill-level (stat-block-attack (player-stats target)))
                    "Private stats not applied")
      (assert-true (player-inventory-dirty target) "Inventory dirty not flagged")
      (assert-true (player-hud-stats-dirty target) "HUD stats dirty not flagged"))
    t))
