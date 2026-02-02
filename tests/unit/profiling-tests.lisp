(in-package #:mmorpg)

;;; Profiling & GC Tests (Phase 6)

(defun test-profile-log-init ()
  "Test initializing the profile log ring buffer."
  (let ((*profile-log* nil)
        (*profile-log-size* 0)
        (*profile-log-index* 99))
    (mmorpg::init-profile-log 5)
    (assert (= (length *profile-log*) 5) () "Profile log should have 5 slots")
    (assert (= *profile-log-index* 0) () "Profile log index should reset to 0")))

(defun test-record-profile-sample ()
  "Test recording a profile timing sample."
  (let ((*profile-log* nil)
        (*profile-log-size* 3)
        (*profile-log-index* 0))
    (mmorpg::init-profile-log 3)
    (mmorpg::record-profile-sample :unit-test 1.5)
    (assert (= *profile-log-index* 1) () "Profile log index should advance")
    (let ((entry (aref *profile-log* 0)))
      (assert (eq (car entry) :unit-test) () "Sample name should match")
      (assert (numberp (cdr entry)) () "Sample time should be numeric"))))

(defun test-with-timing-enabled ()
  "Test with-timing when profiling is enabled."
  (let ((*profile-enabled* t)
        (*profile-log* nil)
        (*profile-log-size* 2)
        (*profile-log-index* 0))
    (mmorpg::init-profile-log 2)
    (let ((result (mmorpg::with-timing (:timed) (+ 1 2))))
      (assert (= result 3) () "with-timing should return body result"))
    (assert (= *profile-log-index* 1) () "with-timing should record one sample")
    (let ((entry (aref *profile-log* 0)))
      (assert (eq (car entry) :timed) () "Timing sample name should match")
      (assert (numberp (cdr entry)) () "Timing sample should be numeric"))))

(defun test-clear-profile-log ()
  "Test clearing the profile log."
  (let ((*profile-log* nil)
        (*profile-log-size* 3)
        (*profile-log-index* 0))
    (mmorpg::init-profile-log 3)
    (mmorpg::record-profile-sample :a 1.0)
    (mmorpg::record-profile-sample :b 2.0)
    (mmorpg::clear-profile-log)
    (assert (= *profile-log-index* 0) () "Profile index should reset to 0")
    (assert (every #'null *profile-log*) () "Profile log should be cleared")))

(defun test-gc-stats-reset ()
  "Test GC stats reset sets frame count to zero."
  (let ((*gc-stats-frame-count* 5)
        (*gc-stats-last-bytes* 123)
        (*gc-stats-last-runtime* 456))
    (mmorpg::reset-gc-stats)
    (assert (= *gc-stats-frame-count* 0) () "Frame GC count should reset to 0")
    (assert (numberp *gc-stats-last-bytes*) () "Last bytes should be numeric")
    (assert (numberp *gc-stats-last-runtime*) () "Last runtime should be numeric")))

(defun test-gc-hook-increment-count ()
  "Test GC hook increment function."
  (let ((*gc-stats-frame-count* 0)
        (*gc-stats-total-count* 0))
    (mmorpg::gc-hook-increment-count)
    (assert (= *gc-stats-frame-count* 1) () "Frame GC count should increment")
    (assert (= *gc-stats-total-count* 1) () "Total GC count should increment")))



(defvar *tests-profiling*
  '(test-profile-log-init
    test-record-profile-sample
    test-with-timing-enabled
    test-clear-profile-log
    test-gc-stats-reset
    test-gc-hook-increment-count)
  "Profiling & GC domain test functions.")
