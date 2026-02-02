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

;;; ============================================================


(defvar *tests-migration*
  '(test-migrate-player-v1-to-v2
    test-migrate-player-v2-to-v3
    test-migrate-player-v3-to-v4
    test-migrate-player-data-chain)
  "Migration domain test functions.")
