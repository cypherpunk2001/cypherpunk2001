(in-package #:mmorpg)

;;; Modular Unit Test Suite
;;; Domain test files live under tests/unit/*.lisp
;;; Each file defines test functions and a *tests-<domain>* list.
;;; This aggregator loads them all and provides the unified runner.
;;; Run: make test-unit

;;; ============================================================
;;; Load all domain test files in deterministic order
;;; ============================================================

(defun load-test-file (relative-path)
  "Load a test file relative to the repo root."
  (let ((path (merge-pathnames relative-path
                               (make-pathname :directory
                                              (pathname-directory
                                               (or *load-truename*
                                                   *compile-file-truename*
                                                   *default-pathname-defaults*))))))
    (load path)))

;; Load helpers first, then domain files in alphabetical order
(let ((test-dir (merge-pathnames
                 "unit/"
                 (make-pathname :directory
                                (pathname-directory
                                 (or *load-truename*
                                     *compile-file-truename*
                                     *default-pathname-defaults*))))))
  (dolist (file (sort (directory (merge-pathnames "*.lisp" test-dir))
                      #'string<
                      :key #'namestring))
    (load file)))

;;; ============================================================
;;; Master test list (built from per-domain lists)
;;; ============================================================

(defun build-unit-test-list ()
  "Build the master unit test list from all domain *tests-* variables."
  (append *tests-utils*
          *tests-net*
          *tests-combat*
          *tests-progression*
          *tests-movement*
          *tests-ai*
          *tests-data*
          *tests-zone*
          *tests-intent*
          *tests-save*
          *tests-migration*
          *tests-world-graph*
          *tests-chat*
          *tests-types*
          *tests-db*
          *tests-spatial*
          *tests-profiling*
          *tests-combat-targeting*
          *tests-auth*
          *tests-zone-cache*
          *tests-zone-continuity*
          *tests-editor*))

;;; ============================================================
;;; Test runners
;;; ============================================================

(defun run-unit-tests ()
  "Run all unit tests. Returns T if all pass, NIL otherwise."
  (let ((passed 0))
    (format t "~%=== Running All Tests ===~%")

    ;; Run persistence tests
    (format t "~%--- Persistence Tests ---~%")
    (when (run-persistence-tests-internal)
      (incf passed))

    ;; Run security tests
    (format t "~%--- Security Tests ---~%")
    (when (run-security-tests-internal)
      (incf passed))

    ;; Run trade tests
    (format t "~%--- Trade Tests ---~%")
    (when (run-trade-tests-internal)
      (incf passed))

    ;; Run original unit tests
    (format t "~%--- Unit Tests ---~%")
    (when (run-unit-tests-internal)
      (incf passed))

    (format t "~%=== Test Summary ===~%")
    (format t "Test suites passed: ~d/4~%" passed)
    (= passed 4)))

(defun run-unit-tests-internal ()
  "Run all unit tests. Returns T if all pass, NIL otherwise."
  (let ((passed 0)
        (failed 0)
        (tests (build-unit-test-list)))
    (format t "~%=== Running Unit Tests ===~%")
    (dolist (test tests)
      (handler-case
          (progn
            (funcall test)
            (incf passed)
            (format t "~a ~a~%" "OK" (symbol-name test)))
        (error (e)
          (incf failed)
          (format t "~a ~a: ~a~%" "FAIL" (symbol-name test) e))))
    (format t "~%Results: ~d passed, ~d failed~%" passed failed)
    (zerop failed)))
