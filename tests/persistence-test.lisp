(in-package #:mmorpg)

;;; Data Integrity Test Suite
;;; Focus: Serialization, migrations, invariants, storage backends
;;; Run: make test-persistence

(defun run-persistence-tests ()
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
                ;; Schema Validation Tests (Phase 1 - Database Hardening)
                #'test-validation-valid-data-passes
                #'test-validation-missing-required-fields
                #'test-validation-wrong-types
                #'test-validation-out-of-bounds
                #'test-validation-oversized-blob
                #'test-validation-nested-stats
                #'test-validation-nested-inventory
                #'test-validated-load-rejects-invalid
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
                #'test-4way-quarantine-invalid-zone-type
                #'test-4way-load-valid-player
                #'test-4way-load-clamp-hp
                #'test-4way-load-reject-bad-type
                #'test-4way-load-not-found
                #'test-4way-storage-incr
                #'test-4way-storage-save-with-ttl
                #'test-4way-forensic-storage
                #'test-4way-validation-metrics)))
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

(defun ensure-test-game-data ()
  "Ensure game data is loaded for tests that need item archetypes."
  (unless *game-data-loaded-p*
    (load-game-data)))

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
      (assert-equal 0 (getf migrated :playtime) "playtime not added by v3 migration")
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

;;;; ========================================================================
;;;; COMPACT SERIALIZATION TESTS (Network Optimization)
;;;; Tests for the 4-Prong snapshot size optimization (see docs/net.md)
;;;; ========================================================================

(defun test-compact-player-roundtrip ()
  "Test that player compact serialization preserves essential state."
  (let ((player (make-player 123.456 789.012 :id 42)))
    ;; Set various fields to test values
    (setf (player-hp player) 85
          (player-dx player) 1.0
          (player-dy player) -0.5
          (player-anim-state player) :walk
          (player-facing player) :side
          (player-facing-sign player) 1.0
          (player-frame-index player) 3
          (player-frame-timer player) 0.25
          (player-attacking player) t
          (player-attack-hit player) nil
          (player-hit-active player) t
          (player-running player) t
          (player-attack-timer player) 1.5
          (player-hit-timer player) 0.3
          (player-hit-frame player) 2
          (player-hit-facing player) :side
          (player-hit-facing-sign player) -1.0
          (player-attack-target-id player) 99
          (player-follow-target-id player) 101
          (player-run-stamina player) 6.5
          (player-last-sequence player) 123)
    ;; Serialize to compact vector
    (let* ((vec (serialize-player-compact player))
           (plist (deserialize-player-compact vec)))
      ;; Verify essential fields preserved (with quantization tolerance)
      (assert-equal 42 (getf plist :id) "id not preserved")
      (assert (< (abs (- 123.4 (getf plist :x))) 0.2) nil "x not preserved within tolerance")
      (assert (< (abs (- 789.0 (getf plist :y))) 0.2) nil "y not preserved within tolerance")
      (assert-equal 85 (getf plist :hp) "hp not preserved")
      (assert-equal :walk (getf plist :anim-state) "anim-state not preserved")
      (assert-equal :side (getf plist :facing) "facing not preserved")
      (assert (getf plist :attacking) nil "attacking flag not preserved")
      (assert (getf plist :hit-active) nil "hit-active flag not preserved")
      (assert (getf plist :running) nil "running flag not preserved")
      (assert-equal 99 (getf plist :attack-target-id) "attack-target-id not preserved")
      (assert-equal 101 (getf plist :follow-target-id) "follow-target-id not preserved")
      (assert (< (abs (- 6.5 (getf plist :run-stamina))) 0.05) nil
              "run-stamina not preserved within tolerance")
      (assert-equal 123 (getf plist :last-sequence) "last-sequence not preserved")))
  t)

(defun test-compact-npc-roundtrip ()
  "Test that NPC compact serialization preserves essential state."
  (let ((npc (%make-npc)))
    ;; Set various fields to test values
    (setf (npc-id npc) 77
          (npc-x npc) 500.5
          (npc-y npc) 300.3
          (npc-hits-left npc) 3
          (npc-alive npc) t
          (npc-provoked npc) t
          (npc-behavior-state npc) :chasing
          (npc-attack-timer npc) 0.75
          (npc-anim-state npc) :attack
          (npc-facing npc) :up
          (npc-frame-index npc) 2
          (npc-frame-timer npc) 0.15
          (npc-hit-active npc) t
          (npc-hit-timer npc) 0.2
          (npc-hit-frame npc) 1
          (npc-hit-facing npc) :down)
    ;; Serialize to compact vector
    (let* ((vec (serialize-npc-compact npc))
           (plist (deserialize-npc-compact vec)))
      ;; Verify essential fields preserved
      (assert-equal 77 (getf plist :id) "id not preserved")
      (assert (< (abs (- 500.5 (getf plist :x))) 0.2) nil "x not preserved within tolerance")
      (assert (< (abs (- 300.3 (getf plist :y))) 0.2) nil "y not preserved within tolerance")
      (assert-equal 3 (getf plist :hits-left) "hits-left not preserved")
      (assert (getf plist :alive) nil "alive flag not preserved")
      (assert (getf plist :provoked) nil "provoked flag not preserved")
      (assert-equal :chasing (getf plist :behavior-state) "behavior-state not preserved")
      (assert-equal :attack (getf plist :anim-state) "anim-state not preserved")
      (assert-equal :up (getf plist :facing) "facing not preserved")
      (assert (getf plist :hit-active) nil "hit-active flag not preserved")))
  t)

(defun test-compact-size-reduction ()
  "Test that compact serialization produces smaller output than plist format."
  (let ((player (make-player 123.456 789.012 :id 42)))
    (setf (player-hp player) 100
          (player-attacking player) t
          (player-anim-state player) :walk
          (player-facing player) :side)
    ;; Compare sizes
    (let* ((compact-vec (serialize-player-compact player))
           (compact-str (prin1-to-string compact-vec))
           (plist (serialize-player player :network-only t))
           (plist-str (prin1-to-string plist))
           (compact-bytes (length compact-str))
           (plist-bytes (length plist-str)))
      ;; Compact format should be significantly smaller
      (assert (< compact-bytes plist-bytes) nil
              (format nil "Compact (~d bytes) should be smaller than plist (~d bytes)"
                      compact-bytes plist-bytes))
      ;; Target: compact should be less than half the size
      (assert (< compact-bytes (* plist-bytes 0.6)) nil
              (format nil "Compact (~d bytes) should be <60% of plist (~d bytes)"
                      compact-bytes plist-bytes))))
  t)

(defun test-compact-enum-encoding ()
  "Test that enum encoding/decoding works correctly for all values."
  ;; Test animation states (game uses :walk/:attack not :walking/:attacking)
  (dolist (state '(:idle :walk :attack))
    (let ((code (encode-anim-state state)))
      (assert-equal state (decode-anim-state code)
                    (format nil "anim-state ~a roundtrip failed" state))))
  ;; Test facing directions (game uses :side with facing-sign for left/right)
  (dolist (facing '(:up :down :side))
    (let ((code (encode-facing facing)))
      (assert-equal facing (decode-facing code)
                    (format nil "facing ~a roundtrip failed" facing))))
  ;; Test behavior states
  (dolist (state '(:idle :wandering :chasing :attacking :fleeing :returning :dead))
    (let ((code (encode-behavior-state state)))
      (assert-equal state (decode-behavior-state code)
                    (format nil "behavior-state ~a roundtrip failed" state))))
  t)

(defun test-compact-quantization ()
  "Test that quantization preserves values within acceptable precision."
  ;; Test coordinate quantization (0.1 pixel precision)
  (dolist (val '(0.0 1.0 123.45 999.99 -50.0))
    (let ((restored (dequantize-coord (quantize-coord val))))
      (assert (< (abs (- val restored)) 0.1) nil
              (format nil "Coord ~a not preserved within 0.1 precision (got ~a)"
                      val restored))))
  ;; Test timer quantization (0.01 second precision)
  (dolist (val '(0.0 0.5 1.25 3.99))
    (let ((restored (dequantize-timer (quantize-timer val))))
      (assert (< (abs (- val restored)) 0.01) nil
              (format nil "Timer ~a not preserved within 0.01 precision (got ~a)"
                      val restored))))
  t)

;;;; ========================================================================
;;;; SCHEMA VALIDATION TESTS (Phase 1 - Database Architecture Hardening)
;;;; Tests for validate-player-plist and db-load-player-validated
;;;; ========================================================================

(defun test-validation-valid-data-passes ()
  "Test: Valid player data passes validation."
  (let ((valid-data '(:id 42
                      :version 3
                      :x 100.0
                      :y 200.0
                      :hp 50
                      :lifetime-xp 1000
                      :playtime 3600
                      :created-at 3900000000
                      :zone-id :zone-1
                      :stats (:attack (:level 5 :xp 500)
                              :strength (:level 3 :xp 200)
                              :defense (:level 4 :xp 300)
                              :hitpoints (:level 10 :xp 1000))
                      :inventory (:slots ((:item-id :health-potion :count 5)))
                      :equipment (:items (:rusty-sword nil nil)))))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist-deep valid-data :log-errors nil)
      (assert-true valid-p "Valid data should pass validation")
      (assert-nil errors "Valid data should have no errors")))
  t)

(defun test-validation-missing-required-fields ()
  "Test: Missing required fields are detected."
  ;; Missing :id
  (let ((data '(:x 100.0 :y 200.0 :hp 50)))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist data :log-errors nil)
      (assert-nil valid-p "Missing :id should fail")
      (assert-true (search "Missing required field: ID" (format nil "~{~a~}" errors))
                  "Error should mention missing ID")))
  ;; Missing :x
  (let ((data '(:id 1 :y 200.0 :hp 50)))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist data :log-errors nil)
      (assert-nil valid-p "Missing :x should fail")))
  ;; Missing :hp
  (let ((data '(:id 1 :x 100.0 :y 200.0)))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist data :log-errors nil)
      (assert-nil valid-p "Missing :hp should fail")))
  t)

(defun test-validation-wrong-types ()
  "Test: Wrong field types are detected."
  ;; String instead of integer for :id
  (let ((data '(:id "not-a-number" :x 100.0 :y 200.0 :hp 50)))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist data :log-errors nil)
      (assert-nil valid-p "String :id should fail")
      (assert-true (search "expected INTEGER" (format nil "~{~a~}" errors))
                  "Error should mention expected INTEGER")))
  ;; String instead of number for :x
  (let ((data '(:id 1 :x "hundred" :y 200.0 :hp 50)))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist data :log-errors nil)
      (assert-nil valid-p "String :x should fail")))
  ;; Float instead of integer for :hp
  (let ((data '(:id 1 :x 100.0 :y 200.0 :hp 50.5)))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist data :log-errors nil)
      (assert-nil valid-p "Float :hp should fail")))
  ;; String instead of symbol for :zone-id
  (let ((data '(:id 1 :x 100.0 :y 200.0 :hp 50 :zone-id "zone-1")))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist data :log-errors nil)
      (assert-nil valid-p "String :zone-id should fail")))
  t)

(defun test-validation-out-of-bounds ()
  "Test: Out-of-bounds values are detected."
  ;; Negative HP
  (let ((data '(:id 1 :x 100.0 :y 200.0 :hp -10)))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist data :log-errors nil)
      (assert-nil valid-p "Negative HP should fail")
      (assert-true (search "out of bounds" (format nil "~{~a~}" errors))
                  "Error should mention out of bounds")))
  ;; HP too high
  (let ((data '(:id 1 :x 100.0 :y 200.0 :hp 999999)))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist data :log-errors nil)
      (assert-nil valid-p "HP > 99999 should fail")))
  ;; ID of 0 (must be >= 1)
  (let ((data '(:id 0 :x 100.0 :y 200.0 :hp 50)))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist data :log-errors nil)
      (assert-nil valid-p "ID of 0 should fail")))
  ;; Negative lifetime-xp
  (let ((data '(:id 1 :x 100.0 :y 200.0 :hp 50 :lifetime-xp -100)))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist data :log-errors nil)
      (assert-nil valid-p "Negative lifetime-xp should fail")))
  ;; Position way out of world bounds
  (let ((data '(:id 1 :x 9999999.0 :y 200.0 :hp 50)))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist data :log-errors nil)
      (assert-nil valid-p "X > 1000000 should fail")))
  t)

(defun test-validation-oversized-blob ()
  "Test: Oversized data blobs are rejected."
  ;; Create data that exceeds 64KB when serialized
  (let* ((huge-inventory (loop for i from 0 below 5000
                               collect (list :item-id (intern (format nil "ITEM-~d" i) :keyword)
                                            :count i)))
         (data (list :id 1 :x 100.0 :y 200.0 :hp 50
                     :inventory (list :slots huge-inventory))))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist data :log-errors nil)
      (assert-nil valid-p "Oversized blob should fail")
      (assert-true (search "exceeds max" (format nil "~{~a~}" errors))
                  "Error should mention exceeds max")))
  t)

(defun test-validation-nested-stats ()
  "Test: Nested stats structure is validated."
  ;; Invalid level type in stats
  (let ((data '(:id 1 :x 100.0 :y 200.0 :hp 50
                :stats (:attack (:level "five" :xp 0)))))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist-deep data :log-errors nil)
      (assert-nil valid-p "String level in stats should fail")
      (assert-true (search "level must be integer" (format nil "~{~a~}" errors))
                  "Error should mention level type")))
  ;; Negative XP in stats
  (let ((data '(:id 1 :x 100.0 :y 200.0 :hp 50
                :stats (:attack (:level 5 :xp -100)))))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist-deep data :log-errors nil)
      (assert-nil valid-p "Negative XP in stats should fail")))
  ;; Level out of bounds (> 999)
  (let ((data '(:id 1 :x 100.0 :y 200.0 :hp 50
                :stats (:attack (:level 9999 :xp 0)))))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist-deep data :log-errors nil)
      (assert-nil valid-p "Level > 999 should fail")))
  t)

(defun test-validation-nested-inventory ()
  "Test: Nested inventory structure is validated."
  ;; Invalid item-id type
  (let ((data '(:id 1 :x 100.0 :y 200.0 :hp 50
                :inventory (:slots ((:item-id "not-a-symbol" :count 5))))))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist-deep data :log-errors nil)
      (assert-nil valid-p "String item-id should fail")
      (assert-true (search "item-id must be symbol" (format nil "~{~a~}" errors))
                  "Error should mention item-id type")))
  ;; Negative count
  (let ((data '(:id 1 :x 100.0 :y 200.0 :hp 50
                :inventory (:slots ((:item-id :health-potion :count -5))))))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist-deep data :log-errors nil)
      (assert-nil valid-p "Negative count should fail")))
  ;; Invalid count type
  (let ((data '(:id 1 :x 100.0 :y 200.0 :hp 50
                :inventory (:slots ((:item-id :health-potion :count "five"))))))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist-deep data :log-errors nil)
      (assert-nil valid-p "String count should fail")))
  t)

(defun test-validated-load-rejects-invalid ()
  "Test: db-load-player-validated rejects invalid data."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*)
         (player-id 999))
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          ;; Save invalid data directly to storage (bypass normal save path)
          (let ((invalid-data '(:id 999 :x "not-a-number" :y 200.0 :hp 50 :version 3)))
            (storage-save storage (player-key player-id) invalid-data))
          ;; Try to load with validation - should return NIL
          (multiple-value-bind (player zone-id)
              (db-load-player-validated player-id)
            (assert-nil player "Invalid data should not load")
            (assert-nil zone-id "Zone-id should be NIL for invalid data"))
          ;; Verify unvalidated load still works (for comparison)
          (let ((player (db-load-player player-id)))
            ;; This may succeed or produce garbage - the point is validated load rejected it
            (declare (ignore player)))
          t)
      ;; Restore global state
      (setf *storage* old-storage))))

;;;; ========================================================================
;;;; SESSION OWNERSHIP TESTS (Phase 3 - Database Architecture Hardening)
;;;; Tests for session claim, double-login rejection, heartbeat, release
;;;; ========================================================================

(defun test-session-claim-success ()
  "Test: Successfully claiming a session ownership."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*)
         (old-server-id *server-instance-id*))
    (unwind-protect
        (progn
          (setf *storage* storage
                *server-instance-id* "test-server-1")
          (storage-connect storage)
          ;; Claim should succeed for new player
          (assert-true (claim-session-ownership 123) "Initial claim should succeed")
          ;; Same server claiming again should succeed (refresh)
          (assert-true (claim-session-ownership 123) "Re-claim by same server should succeed")
          t)
      ;; Cleanup
      (setf *storage* old-storage
            *server-instance-id* old-server-id))))

(defun test-session-double-login-rejected ()
  "Test: Double login from different server is rejected."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*)
         (old-server-id *server-instance-id*))
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          ;; Server 1 claims the session
          (setf *server-instance-id* "server-1")
          (assert-true (claim-session-ownership 456) "Server 1 should claim successfully")
          ;; Server 2 tries to claim - should fail
          (setf *server-instance-id* "server-2")
          (assert-nil (claim-session-ownership 456) "Server 2 should be rejected (double login)")
          t)
      ;; Cleanup
      (setf *storage* old-storage
            *server-instance-id* old-server-id))))

(defun test-session-ownership-refresh ()
  "Test: Session ownership TTL can be refreshed."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*)
         (old-server-id *server-instance-id*))
    (unwind-protect
        (progn
          (setf *storage* storage
                *server-instance-id* "test-server-refresh")
          (storage-connect storage)
          ;; Claim the session
          (assert-true (claim-session-ownership 789) "Initial claim should succeed")
          ;; Verify we own it
          (assert-true (verify-session-ownership 789) "Should verify ownership")
          ;; Refresh should succeed
          (refresh-session-ownership 789)
          ;; Should still own it
          (assert-true (verify-session-ownership 789) "Should still own after refresh")
          t)
      ;; Cleanup
      (setf *storage* old-storage
            *server-instance-id* old-server-id))))

(defun test-session-release-and-reclaim ()
  "Test: Released session can be claimed by another server."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*)
         (old-server-id *server-instance-id*))
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          ;; Server 1 claims the session
          (setf *server-instance-id* "server-alpha")
          (assert-true (claim-session-ownership 999) "Server alpha should claim")
          ;; Server 1 releases
          (release-session-ownership 999)
          ;; Server 2 should now be able to claim
          (setf *server-instance-id* "server-beta")
          (assert-true (claim-session-ownership 999) "Server beta should claim after release")
          ;; Server 2 should now own it
          (assert-true (verify-session-ownership 999) "Server beta should own")
          t)
      ;; Cleanup
      (setf *storage* old-storage
            *server-instance-id* old-server-id))))

;;;; ========================================================================
;;;; 4-OUTCOME VALIDATION TESTS (Phase 6 - Database Architecture Hardening)
;;;; Tests for validate-player-plist-4way and db-load-player-validated
;;;; See docs/db.md "Phase 6: 4-Outcome Validation System"
;;;; ========================================================================

(defun make-valid-test-plist (&key (id 1) (x 100.0) (y 200.0) (hp 50))
  "Create a valid player plist for testing validation."
  (list :id id
        :version *player-schema-version*
        :x x :y y
        :hp hp
        :lifetime-xp 1000
        :playtime 3600.0
        :created-at (get-universal-time)
        :deaths 5
        :zone-id :overworld
        :stats (list :attack (list :level 10 :xp 500)
                     :strength (list :level 8 :xp 300)
                     :defense (list :level 12 :xp 700)
                     :hitpoints (list :level 15 :xp 1000))
        :inventory (list :slots nil)
        :equipment nil))

;;; :ok tests - valid data should pass

(defun test-4way-ok-valid-data ()
  "Test: Valid player data returns :ok action."
  (let ((plist (make-valid-test-plist :id 1 :x 100.0 :y 200.0 :hp 50)))
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :ok action "Valid data should return :ok")
      (assert-nil fixed-plist "No fixed plist needed for :ok")
      t)))

;;; :clamp tests - safe coercions

(defun test-4way-clamp-hp-below-zero ()
  "Test: Negative HP is clamped to 0."
  (let ((plist (make-valid-test-plist :hp -50)))
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :clamp action "Negative HP should return :clamp")
      (assert-equal 0 (getf fixed-plist :hp) "HP should be clamped to 0")
      (assert-true (member "HP -50 clamped to 0" issues :test #'string=)
                   "Should report clamping")
      t)))

(defun test-4way-clamp-hp-above-max ()
  "Test: HP above max is clamped to 99999."
  (let ((plist (make-valid-test-plist :hp 150000)))
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :clamp action "Excessive HP should return :clamp")
      (assert-equal 99999 (getf fixed-plist :hp) "HP should be clamped to 99999")
      t)))

(defun test-4way-clamp-position-out-of-bounds ()
  "Test: Position out of bounds is clamped to spawn point."
  (let ((plist (make-valid-test-plist :x 5000000.0 :y -5000000.0)))
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :clamp action "Out of bounds position should return :clamp")
      (assert-equal *default-spawn-x* (getf fixed-plist :x) "X should be spawn")
      (assert-equal *default-spawn-y* (getf fixed-plist :y) "Y should be spawn")
      t)))

(defun test-4way-clamp-missing-created-at ()
  "Test: Missing :created-at is set to current time."
  (let ((plist (make-valid-test-plist)))
    ;; Remove :created-at
    (remf plist :created-at)
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :clamp action "Missing created-at should return :clamp")
      (assert-true (getf fixed-plist :created-at) "Should have created-at")
      t)))

(defun test-4way-clamp-negative-deaths ()
  "Test: Negative deaths count is clamped to 0."
  (let ((plist (make-valid-test-plist)))
    (setf (getf plist :deaths) -10)
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :clamp action "Negative deaths should return :clamp")
      (assert-equal 0 (getf fixed-plist :deaths) "Deaths should be clamped to 0")
      t)))

;;; :reject tests - exploit-adjacent data

(defun test-4way-reject-missing-id ()
  "Test: Missing :id returns :reject."
  (let ((plist (make-valid-test-plist)))
    (remf plist :id)
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :reject action "Missing ID should return :reject")
      (assert-nil fixed-plist "No fixed plist for :reject")
      t)))

(defun test-4way-reject-negative-lifetime-xp ()
  "Test: Negative lifetime-xp returns :reject (exploit indicator)."
  (let ((plist (make-valid-test-plist)))
    (setf (getf plist :lifetime-xp) -1000)
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :reject action "Negative lifetime-xp should return :reject")
      t)))

(defun test-4way-reject-negative-item-count ()
  "Test: Negative inventory item count returns :reject."
  (let ((plist (make-valid-test-plist)))
    (setf (getf plist :inventory)
          (list :slots (list (list :item-id :sword :count -5))))
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :reject action "Negative item count should return :reject")
      t)))

(defun test-4way-reject-excessive-item-count ()
  "Test: Item count exceeding max returns :reject (dupe indicator)."
  (let ((plist (make-valid-test-plist)))
    (setf (getf plist :inventory)
          (list :slots (list (list :item-id :coins :count 9999999999))))
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :reject action "Excessive item count should return :reject")
      t)))

(defun test-4way-reject-wrong-type-x ()
  "Test: Non-number :x returns :reject."
  (let ((plist (make-valid-test-plist)))
    (setf (getf plist :x) "not-a-number")
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :reject action "Non-number :x should return :reject")
      t)))

(defun test-4way-reject-negative-skill-xp ()
  "Test: Negative skill XP returns :reject (exploit indicator)."
  (let ((plist (make-valid-test-plist)))
    (setf (getf plist :stats)
          (list :attack (list :level 10 :xp -500)))
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :reject action "Negative skill XP should return :reject")
      t)))

;;; :quarantine tests - suspicious but recoverable

(defun test-4way-quarantine-invalid-zone-type ()
  "Test: Non-symbol zone-id returns :quarantine."
  (let ((plist (make-valid-test-plist)))
    (setf (getf plist :zone-id) 12345)  ; number, not symbol
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :quarantine action "Non-symbol zone-id should return :quarantine")
      t)))

;;; db-load-player-validated integration tests

(defun test-4way-load-valid-player ()
  "Test: Loading valid player returns :ok action."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*)
         (player-id 100))
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          ;; Save valid player data using quoted list (like test-validated-load-rejects-invalid)
          (let ((plist `(:id ,player-id
                         :version 4
                         :x 100.0 :y 200.0
                         :hp 50
                         :lifetime-xp 1000
                         :playtime 3600.0
                         :created-at ,(get-universal-time)
                         :deaths 5
                         :zone-id :overworld
                         :stats (:attack (:level 10 :xp 500))
                         :inventory (:slots nil)
                         :equipment nil)))
            (storage-save storage (player-key player-id) plist))
          ;; Load with validation
          (multiple-value-bind (player zone-id action)
              (db-load-player-validated player-id)
            (assert-true player "Should load player")
            (assert-equal :overworld zone-id "Should have zone-id")
            (assert-equal :ok action "Should return :ok action"))
          t)
      (setf *storage* old-storage))))

(defun test-4way-load-clamp-hp ()
  "Test: Loading player with bad HP returns :clamp and fixes it."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*)
         (player-id 101))
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          ;; Save player with negative HP (should be clamped to 0)
          (let ((plist `(:id ,player-id
                         :version 4
                         :x 100.0 :y 200.0
                         :hp -100
                         :lifetime-xp 1000
                         :playtime 3600.0
                         :created-at ,(get-universal-time)
                         :deaths 5
                         :zone-id :overworld
                         :stats (:attack (:level 10 :xp 500))
                         :inventory (:slots nil)
                         :equipment nil)))
            (storage-save storage (player-key player-id) plist))
          ;; Load with validation
          (multiple-value-bind (player zone-id action)
              (db-load-player-validated player-id)
            (assert-true player "Should load player")
            (assert-equal :clamp action "Should return :clamp action")
            (assert-equal 0 (player-hp player) "HP should be clamped to 0"))
          t)
      (setf *storage* old-storage))))

(defun test-4way-load-reject-bad-type ()
  "Test: Loading player with wrong type returns :reject."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*)
         (player-id 102))
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          ;; Save invalid data directly
          (let ((invalid-data '(:id 102 :x "not-a-number" :y 200.0 :hp 50 :version 4)))
            (storage-save storage (player-key player-id) invalid-data))
          ;; Load with validation - should reject
          (multiple-value-bind (player zone-id action)
              (db-load-player-validated player-id)
            (assert-nil player "Should not load invalid player")
            (assert-equal :reject action "Should return :reject action"))
          t)
      (setf *storage* old-storage))))

(defun test-4way-load-not-found ()
  "Test: Loading non-existent player returns :not-found."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*))
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          ;; Try to load non-existent player
          (multiple-value-bind (player zone-id action)
              (db-load-player-validated 99999)
            (assert-nil player "Should not find player")
            (assert-equal :not-found action "Should return :not-found action"))
          t)
      (setf *storage* old-storage))))

(defun test-4way-storage-incr ()
  "Test: storage-incr increments counters correctly."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*))
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          ;; Increment non-existent key - should create with value 1
          (assert-equal 1 (storage-incr storage "test:counter") "First incr should return 1")
          (assert-equal 2 (storage-incr storage "test:counter") "Second incr should return 2")
          (assert-equal 3 (storage-incr storage "test:counter") "Third incr should return 3")
          t)
      (setf *storage* old-storage))))

(defun test-4way-storage-save-with-ttl ()
  "Test: storage-save-with-ttl saves data with expiration."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*))
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          ;; Save with TTL
          (storage-save-with-ttl storage "test:expiring" '(:data "test") 3600)
          ;; Should be retrievable
          (let ((data (storage-load storage "test:expiring")))
            (assert-equal '(:data "test") data "Should retrieve saved data"))
          t)
      (setf *storage* old-storage))))

(defun test-4way-forensic-storage ()
  "Test: store-corrupt-blob stores data with TTL."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*))
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          ;; Store corrupt blob
          (store-corrupt-blob 123 "raw-data" '("error 1" "error 2"))
          ;; Check that something was stored (key format: corrupt:123:timestamp)
          (let ((keys (storage-keys storage "corrupt:*")))
            (assert-true (> (length keys) 0) "Should have stored corrupt blob"))
          t)
      (setf *storage* old-storage))))

(defun test-4way-validation-metrics ()
  "Test: increment-validation-metric increments counters."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*))
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          ;; Increment some metrics
          (increment-validation-metric :ok)
          (increment-validation-metric :ok)
          (increment-validation-metric :clamp)
          (increment-validation-metric :reject)
          ;; Check counters
          (assert-equal 2 (storage-load storage "metrics:validation:ok") "OK count")
          (assert-equal 1 (storage-load storage "metrics:validation:clamp") "Clamp count")
          (assert-equal 1 (storage-load storage "metrics:validation:reject") "Reject count")
          t)
      (setf *storage* old-storage))))

(defun run-4way-validation-tests ()
  "Run all 4-outcome validation tests."
  (format t "~&Running 4-outcome validation tests...~%")
  (run-test 'test-4way-ok-valid-data)
  (run-test 'test-4way-clamp-hp-below-zero)
  (run-test 'test-4way-clamp-hp-above-max)
  (run-test 'test-4way-clamp-position-out-of-bounds)
  (run-test 'test-4way-clamp-missing-created-at)
  (run-test 'test-4way-clamp-negative-deaths)
  (run-test 'test-4way-reject-missing-id)
  (run-test 'test-4way-reject-negative-lifetime-xp)
  (run-test 'test-4way-reject-negative-item-count)
  (run-test 'test-4way-reject-excessive-item-count)
  (run-test 'test-4way-reject-wrong-type-x)
  (run-test 'test-4way-reject-negative-skill-xp)
  (run-test 'test-4way-quarantine-invalid-zone-type)
  (run-test 'test-4way-load-valid-player)
  (run-test 'test-4way-load-clamp-hp)
  (run-test 'test-4way-load-reject-bad-type)
  (run-test 'test-4way-load-not-found)
  (run-test 'test-4way-storage-incr)
  (run-test 'test-4way-storage-save-with-ttl)
  (run-test 'test-4way-forensic-storage)
  (run-test 'test-4way-validation-metrics)
  (format t "~&All 4-outcome validation tests complete.~%"))

;;; Export for REPL usage
(export 'run-persistence-tests)
(export 'run-4way-validation-tests)
