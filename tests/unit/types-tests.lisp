(in-package #:mmorpg)

;;; ============================================================
;;; TYPES.LISP TESTS
;;; ============================================================

(defun test-skill-xp-for-level ()
  "Test XP required for level calculation."
  ;; Level 1 requires 0 XP
  (assert (= (skill-xp-for-level 1) 0) () "skill-xp: level 1 = 0")
  ;; Higher levels need more XP
  (assert (> (skill-xp-for-level 2) 0) () "skill-xp: level 2 > 0")
  (assert (> (skill-xp-for-level 10) (skill-xp-for-level 5)) () "skill-xp: level 10 > level 5")
  ;; XP increases per level
  (let ((xp5 (skill-xp-for-level 5))
        (xp6 (skill-xp-for-level 6)))
    (assert (> xp6 xp5) () "skill-xp: monotonic increase")))

(defun test-allocate-entity-id ()
  "Test entity ID allocation."
  ;; make-id-source takes positional args: (next-id persistent)
  (let ((source (make-id-source 1)))
    ;; First allocation
    (let ((id1 (allocate-entity-id source)))
      (assert (= id1 1) () "entity-id: first = 1"))
    ;; Second allocation
    (let ((id2 (allocate-entity-id source)))
      (assert (= id2 2) () "entity-id: second = 2"))
    ;; IDs are unique
    (let ((id3 (allocate-entity-id source)))
      (assert (= id3 3) () "entity-id: third = 3"))
    ;; Source tracks next
    (assert (= (id-source-next-id source) 4) () "entity-id: next is 4")))

(defun test-find-player-by-id ()
  "Test finding player by ID."
  (ensure-test-game-data)
  (let* ((p1 (make-player 0.0 0.0 :id 100))
         (p2 (make-player 10.0 10.0 :id 200))
         ;; find-player-by-id expects a vector, not list
         (players (vector p1 p2)))
    ;; Find existing
    (assert (eq (find-player-by-id players 100) p1) () "find-player: id 100")
    (assert (eq (find-player-by-id players 200) p2) () "find-player: id 200")
    ;; Not found
    (assert (null (find-player-by-id players 999)) () "find-player: not found")
    ;; Empty vector
    (assert (null (find-player-by-id (vector) 100)) () "find-player: empty vector")
    ;; Nil
    (assert (null (find-player-by-id nil 100)) () "find-player: nil")))



(defvar *tests-types*
  '(test-skill-xp-for-level
    test-allocate-entity-id
    test-find-player-by-id)
  "Types domain test functions.")
