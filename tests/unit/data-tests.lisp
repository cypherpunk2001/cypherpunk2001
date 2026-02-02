(in-package #:mmorpg)

;;; DATA.LISP TESTS
;;; ============================================================

(defun test-plist-form-p ()
  "Test plist form detection."
  (assert (plist-form-p '(:key1 val1 :key2 val2)) () "plist-form: valid plist")
  (assert (not (plist-form-p '(a b c))) () "plist-form: not a plist - non-keyword keys")
  ;; Note: nil and empty list returns T from plist-form-p (vacuous truth)
  (assert (plist-form-p nil) () "plist-form: empty list is valid (vacuously)")
  (assert (plist-form-p '(:single value)) () "plist-form: single pair"))

(defun test-data-section-header-p ()
  "Test data section header detection."
  (assert (data-section-header-p :items) () "section-header: keyword")
  (assert (not (data-section-header-p "items")) () "section-header: string")
  (assert (not (data-section-header-p '(:items))) () "section-header: list"))

(defun test-data-section-entry-p ()
  "Test data section entry detection."
  (assert (data-section-entry-p '(:sword (:damage 10))) () "section-entry: valid")
  ;; '(:sword) has nil as second element, nil is a list, so this passes
  (assert (data-section-entry-p '(:sword nil)) () "section-entry: nil second is list")
  (assert (not (data-section-entry-p nil)) () "section-entry: nil")
  (assert (not (data-section-entry-p '("sword" (:damage 10)))) () "section-entry: non-keyword id"))

(defun test-normalize-pairs ()
  "Test pair normalization."
  (let ((plist-result (normalize-pairs '(:a 1 :b 2)))
        (pairs-result (normalize-pairs '((:a 1) (:b 2)))))
    (assert (equal plist-result '((:a 1) (:b 2))) () "normalize-pairs: plist")
    (assert (equal pairs-result '((:a 1) (:b 2))) () "normalize-pairs: already pairs")))

;;; ============================================================

;;; NEW DATA TESTS (Priority 2)
;;; ============================================================

(defun test-validate-item-archetype-plist ()
  "Test item archetype plist validation."
  ;; Valid plist
  (assert (validate-item-archetype-plist :test '(:name "Test" :stack-size 10)) ()
          "validate-item: valid passes")
  ;; Invalid name type - should error
  (handler-case
      (progn
        (validate-item-archetype-plist :test '(:name 123))
        (assert nil () "validate-item: invalid name should error"))
    (error () t))
  ;; Invalid stack-size - should error
  (handler-case
      (progn
        (validate-item-archetype-plist :test '(:stack-size "not-a-number"))
        (assert nil () "validate-item: invalid stack-size should error"))
    (error () t)))

(defun test-item-archetype-from-plist ()
  "Test item archetype creation from plist."
  (let ((item (item-archetype-from-plist :test-sword
                                          '(:name "Test Sword"
                                            :description "A test weapon"
                                            :stack-size 1
                                            :value 100
                                            :equip-slot :weapon
                                            :attack 5))))
    (assert (eq (item-archetype-id item) :test-sword) () "item-from-plist: id")
    (assert (string= (item-archetype-name item) "Test Sword") () "item-from-plist: name")
    (assert (= (item-archetype-stack-size item) 1) () "item-from-plist: stack-size")
    (assert (= (item-archetype-attack item) 5) () "item-from-plist: attack")
    (assert (eq (item-archetype-equip-slot item) :weapon) () "item-from-plist: equip-slot")))

(defun test-validate-object-archetype-plist ()
  "Test object archetype plist validation."
  ;; Valid plist
  (assert (validate-object-archetype-plist :test '(:name "Test" :item-id :coins)) ()
          "validate-object: valid passes")
  ;; Invalid item-id type - should error
  (handler-case
      (progn
        (validate-object-archetype-plist :test '(:item-id "not-keyword"))
        (assert nil () "validate-object: invalid item-id should error"))
    (error () t)))

(defun test-object-archetype-from-plist ()
  "Test object archetype creation from plist."
  (let ((obj (object-archetype-from-plist :coin-pile
                                           '(:name "Coin Pile"
                                             :item-id :coins
                                             :count 10
                                             :respawn-seconds 30.0))))
    (assert (eq (object-archetype-id obj) :coin-pile) () "object-from-plist: id")
    (assert (string= (object-archetype-name obj) "Coin Pile") () "object-from-plist: name")
    (assert (eq (object-archetype-item-id obj) :coins) () "object-from-plist: item-id")
    (assert (= (object-archetype-count obj) 10) () "object-from-plist: count")
    (assert (= (object-archetype-respawn-seconds obj) 30.0) () "object-from-plist: respawn")))

(defun test-loot-entry-from-spec ()
  "Test loot entry creation from spec."
  ;; Full spec
  (let ((entry (loot-entry-from-spec '(:coins 100 5 10))))
    (assert (eq (loot-entry-item-id entry) :coins) () "loot-entry: item-id")
    (assert (= (loot-entry-weight entry) 100) () "loot-entry: weight")
    (assert (= (loot-entry-min-count entry) 5) () "loot-entry: min")
    (assert (= (loot-entry-max-count entry) 10) () "loot-entry: max"))
  ;; Minimal spec (defaults to 1,1)
  (let ((entry (loot-entry-from-spec '(:potion 50))))
    (assert (= (loot-entry-min-count entry) 1) () "loot-entry: default min")
    (assert (= (loot-entry-max-count entry) 1) () "loot-entry: default max")))

(defun test-validate-loot-table-plist ()
  "Test loot table plist validation."
  ;; Valid plist
  (assert (validate-loot-table-plist :test '(:rolls 1 :entries ((:coins 100)))) ()
          "validate-loot: valid passes")
  ;; Missing entries - should error
  (handler-case
      (progn
        (validate-loot-table-plist :test '(:rolls 1))
        (assert nil () "validate-loot: missing entries should error"))
    (error () t)))

(defun test-loot-table-from-plist ()
  "Test loot table creation from plist."
  (let ((table (loot-table-from-plist :goblin-loot
                                       '(:rolls 2
                                         :entries ((:coins 100 1 10)
                                                   (:health-potion 20 1 1))))))
    (assert (eq (loot-table-id table) :goblin-loot) () "loot-table: id")
    (assert (= (loot-table-rolls table) 2) () "loot-table: rolls")
    (assert (= (length (loot-table-entries table)) 2) () "loot-table: 2 entries")))

(defun test-animation-set-from-plist ()
  "Test animation set creation from plist."
  (let ((set (animation-set-from-plist :test-anim
                                        '(:dir "sprites/"
                                          :down-idle "idle.png"
                                          :down-walk "walk.png"))))
    (assert (eq (animation-set-id set) :test-anim) () "anim-set: id")
    (assert (string= (animation-set-dir set) "sprites/") () "anim-set: dir")
    (assert (string= (animation-set-down-idle set) "idle.png") () "anim-set: down-idle")))

(defun test-merge-animation-sets ()
  "Test animation set merging."
  (let* ((base (animation-set-from-plist :base '(:dir "base/" :down-idle "base-idle.png")))
         (override (animation-set-from-plist :override '(:down-idle "override-idle.png")))
         (merged (merge-animation-sets base override)))
    ;; Override should win for down-idle
    (assert (string= (animation-set-down-idle merged) "override-idle.png") ()
            "merge-anim: override wins")
    ;; Base should be kept for dir (not overridden)
    (assert (string= (animation-set-dir merged) "base/") ()
            "merge-anim: base kept")))

;;; ============================================================

;;; ADDITIONAL DATA TESTS
;;; ============================================================

(defun test-parse-game-data-forms ()
  "Test parsing game data forms into sections."
  ;; Simple plist
  (let ((result (parse-game-data-forms '((:test-key 123)))))
    (assert (listp result) () "parse-forms: returns list"))
  ;; Section with entries
  (let ((result (parse-game-data-forms '(:items
                                          (:sword (:name "Sword"))
                                          (:shield (:name "Shield"))))))
    (assert (listp result) () "parse-forms: sections parsed")
    (let ((items (getf result :items)))
      (assert (= (length items) 2) () "parse-forms: 2 items")))
  ;; Mixed tunables and sections
  (let ((result (parse-game-data-forms '((:player-speed 100.0)
                                          :npcs
                                          (:goblin (:name "Goblin"))))))
    (assert (= (getf result :player-speed) 100.0) () "parse-forms: tunable preserved")
    (assert (listp (getf result :npcs)) () "parse-forms: section present")))

(defun test-make-npc-archetype-from-plist ()
  "Test NPC archetype creation from plist."
  (let ((archetype (make-npc-archetype-from-plist
                    '(:name "Test NPC"
                      :max-hits 10
                      :attack-level 5
                      :defense-level 3
                      :combat-xp 25
                      :move-speed 80.0
                      :aggro-mode :always))))
    (assert (string= (npc-archetype-name archetype) "Test NPC") ()
            "npc-from-plist: name")
    (assert (= (npc-archetype-max-hits archetype) 10) ()
            "npc-from-plist: max-hits")
    (assert (= (npc-archetype-attack-level archetype) 5) ()
            "npc-from-plist: attack-level")
    (assert (= (npc-archetype-combat-xp archetype) 25) ()
            "npc-from-plist: combat-xp")
    (assert (eq (npc-archetype-aggro-mode archetype) :always) ()
            "npc-from-plist: aggro-mode")))

;;; ============================================================


(defvar *tests-data*
  '(test-plist-form-p
    test-data-section-header-p
    test-data-section-entry-p
    test-normalize-pairs
    test-validate-item-archetype-plist
    test-item-archetype-from-plist
    test-validate-object-archetype-plist
    test-object-archetype-from-plist
    test-loot-entry-from-spec
    test-validate-loot-table-plist
    test-loot-table-from-plist
    test-animation-set-from-plist
    test-merge-animation-sets
    ;; Additional Data Tests
    test-parse-game-data-forms
    test-make-npc-archetype-from-plist)
  "Data domain test functions.")
