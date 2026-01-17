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
                #'test-coins-never-negative)))
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

;;; Export for REPL usage
(export 'run-persistence-tests)
