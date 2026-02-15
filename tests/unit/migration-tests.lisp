(in-package #:mmorpg)

;;; ============================================================
;;; MIGRATIONS.LISP TESTS
;;; ============================================================

(defun test-migrate-player-v1-to-v2 ()
  "Test migration from v1 to v2 adds lifetime-xp."
  (let* ((v1-data '(:version 1 :id 123 :x 100.0 :y 200.0 :hp 50))
         (migrated (migrate-player-v1->v2 (copy-list v1-data))))
    ;; Should have lifetime-xp added
    (assert (= (getf migrated :lifetime-xp) 0) () "v1->v2: adds lifetime-xp = 0")
    ;; Original fields preserved
    (assert (= (getf migrated :id) 123) () "v1->v2: preserves id")
    (assert (= (getf migrated :x) 100.0) () "v1->v2: preserves x")))

(defun test-migrate-player-v2-to-v3 ()
  "Test migration from v2 to v3 adds playtime and created-at."
  (let* ((v2-data '(:version 2 :id 456 :lifetime-xp 1000))
         (before-time (get-universal-time))
         (migrated (migrate-player-v2->v3 (copy-list v2-data)))
         (after-time (get-universal-time)))
    ;; Should have playtime added
    (assert (= (getf migrated :playtime) 0) () "v2->v3: adds playtime = 0")
    ;; Should have created-at added (between before and after time)
    (let ((created (getf migrated :created-at)))
      (assert (>= created before-time) () "v2->v3: created-at >= start")
      (assert (<= created after-time) () "v2->v3: created-at <= end"))
    ;; Original fields preserved
    (assert (= (getf migrated :lifetime-xp) 1000) () "v2->v3: preserves lifetime-xp")))

(defun test-migrate-player-v3-to-v4 ()
  "Test migration from v3 to v4 adds deaths field."
  (let* ((v3-data '(:version 3 :id 789 :lifetime-xp 5000 :playtime 3600 :created-at 1000000))
         (migrated (migrate-player-v3->v4 (copy-list v3-data))))
    ;; Should have deaths added
    (assert (= (getf migrated :deaths) 0) () "v3->v4: adds deaths = 0")
    ;; Original fields preserved
    (assert (= (getf migrated :id) 789) () "v3->v4: preserves id")
    (assert (= (getf migrated :lifetime-xp) 5000) () "v3->v4: preserves lifetime-xp")
    (assert (= (getf migrated :playtime) 3600) () "v3->v4: preserves playtime")
    (assert (= (getf migrated :created-at) 1000000) () "v3->v4: preserves created-at")))

(defun test-migrate-player-data-chain ()
  "Test full migration chain from v1 to current version."
  (let* ((v1-data '(:version 1 :id 789 :x 50.0 :y 75.0 :hp 100))
         (migrated (migrate-player-data (copy-list v1-data))))
    ;; Should be at current version
    (assert (= (getf migrated :version) *player-schema-version*) () "chain: at current version")
    ;; Should have all migration fields
    (assert (numberp (getf migrated :lifetime-xp)) () "chain: has lifetime-xp")
    (assert (numberp (getf migrated :playtime)) () "chain: has playtime")
    (assert (numberp (getf migrated :created-at)) () "chain: has created-at")
    ;; Original fields preserved
    (assert (= (getf migrated :id) 789) () "chain: preserves id")
    (assert (= (getf migrated :hp) 100) () "chain: preserves hp")))

(defun test-migrate-player-v4-to-v5 ()
  "Test migration from v4 to v5 renames fantasy item IDs in inventory."
  ;; Player with rat-tail and goblin-ear in inventory
  (let* ((v4-data (list :version 4 :id 100 :x 0.0 :y 0.0
                        :inventory (list :slots (list (list :item-id :rat-tail :count 3)
                                                      (list :item-id :coins :count 50)
                                                      (list :item-id :goblin-ear :count 1)))))
         (migrated (migrate-player-v4->v5 (copy-tree v4-data))))
    ;; :rat-tail should become :drone-scrap
    (let* ((inv (getf migrated :inventory))
           (slots (getf inv :slots)))
      (assert (eq (getf (first slots) :item-id) :drone-scrap)
              () "v4->v5: rat-tail -> drone-scrap")
      (assert (= (getf (first slots) :count) 3)
              () "v4->v5: preserves count")
      ;; :coins should be unchanged
      (assert (eq (getf (second slots) :item-id) :coins)
              () "v4->v5: coins unchanged")
      ;; :goblin-ear should become :punk-tag
      (assert (eq (getf (third slots) :item-id) :punk-tag)
              () "v4->v5: goblin-ear -> punk-tag")))
  ;; Player with no inventory should not error
  (let* ((v4-no-inv '(:version 4 :id 200 :x 0.0 :y 0.0))
         (migrated (migrate-player-v4->v5 (copy-list v4-no-inv))))
    (assert (= (getf migrated :id) 200)
            () "v4->v5: no-inventory preserves data"))
  ;; Player with empty inventory should not error
  (let* ((v4-empty '(:version 4 :id 300 :inventory (:slots nil)))
         (migrated (migrate-player-v4->v5 (copy-list v4-empty))))
    (assert (= (getf migrated :id) 300)
            () "v4->v5: empty-inventory preserves data")))

;;; ============================================================


(defvar *tests-migration*
  '(test-migrate-player-v1-to-v2
    test-migrate-player-v2-to-v3
    test-migrate-player-v3-to-v4
    test-migrate-player-v4-to-v5
    test-migrate-player-data-chain)
  "Migration domain test functions.")
