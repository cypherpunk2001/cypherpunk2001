(in-package #:mmorpg)

;;; ============================================================
;;; REDIS METRICS TESTS (Phase 2 - Database Hardening)
;;; ============================================================

(defun test-calculate-percentile ()
  "Test percentile calculation from list of values."
  ;; Basic cases
  (let ((values '(1 2 3 4 5 6 7 8 9 10)))
    ;; p50 (median) should be around 5
    (let ((p50 (calculate-percentile values 50)))
      (assert (and p50 (>= p50 4) (<= p50 6)) ()
              (format nil "p50 of 1-10 should be around 5, got ~a" p50)))
    ;; p99 should be close to 10
    (let ((p99 (calculate-percentile values 99)))
      (assert (and p99 (>= p99 9) (<= p99 10)) ()
              (format nil "p99 of 1-10 should be around 10, got ~a" p99)))
    ;; p0 should be close to 1
    (let ((p0 (calculate-percentile values 0)))
      (assert (and p0 (= p0 1)) ()
              (format nil "p0 of 1-10 should be 1, got ~a" p0)))))

(defun test-calculate-percentile-edge-cases ()
  "Test percentile edge cases."
  ;; Empty list
  (assert (null (calculate-percentile nil 50)) () "percentile: nil -> nil")
  (assert (null (calculate-percentile '() 50)) () "percentile: empty -> nil")
  ;; Single value
  (let ((p50 (calculate-percentile '(42) 50)))
    (assert (= p50 42) () "percentile: single value"))
  ;; Two values
  (let ((p50 (calculate-percentile '(10 20) 50)))
    (assert (and p50 (>= p50 10) (<= p50 20)) ()
            (format nil "percentile: two values, got ~a" p50))))

(defun test-ring-buffer-values ()
  "Test ring buffer value extraction."
  ;; Create buffer with some values
  (let ((buffer (make-array 10 :initial-element 0.0)))
    (setf (aref buffer 0) 1.0
          (aref buffer 1) 2.0
          (aref buffer 2) 3.0)
    ;; Extract with count=3
    (let ((values (ring-buffer-values buffer 3)))
      (assert (= (length values) 3) ()
              (format nil "ring-buffer: count=3 should return 3 values, got ~d" (length values))))
    ;; Extract with count=1
    (let ((values (ring-buffer-values buffer 1)))
      (assert (= (length values) 1) () "ring-buffer: count=1"))
    ;; Extract with count=0
    (let ((values (ring-buffer-values buffer 0)))
      (assert (= (length values) 0) () "ring-buffer: count=0"))
    ;; Extract with count > buffer size
    (let ((values (ring-buffer-values buffer 100)))
      (assert (= (length values) 10) () "ring-buffer: clamps to buffer size"))))

(defun test-metrics-push-latency ()
  "Test metrics latency recording."
  ;; Initialize metrics
  (init-redis-metrics)
  ;; Push some save latencies
  (metrics-push-save-latency 1.0)
  (metrics-push-save-latency 2.0)
  (metrics-push-save-latency 3.0)
  ;; Check save count
  (assert (= (redis-metrics-save-count *redis-metrics*) 3) ()
          "metrics: save count = 3")
  (assert (= (redis-metrics-total-saves *redis-metrics*) 3) ()
          "metrics: total saves = 3")
  ;; Push some load latencies
  (metrics-push-load-latency 0.5)
  (metrics-push-load-latency 1.5)
  ;; Check load count
  (assert (= (redis-metrics-load-count *redis-metrics*) 2) ()
          "metrics: load count = 2")
  (assert (= (redis-metrics-total-loads *redis-metrics*) 2) ()
          "metrics: total loads = 2")
  ;; Check p50/p99 values exist
  (let ((p99-save (get-redis-p99-save-latency))
        (p50-save (get-redis-p50-save-latency)))
    (assert (numberp p99-save) () "metrics: p99 save is number")
    (assert (numberp p50-save) () "metrics: p50 save is number"))
  ;; Record errors
  (metrics-record-save-error)
  (metrics-record-load-error)
  (assert (= (redis-metrics-total-save-errors *redis-metrics*) 1) ()
          "metrics: save errors = 1")
  (assert (= (redis-metrics-total-load-errors *redis-metrics*) 1) ()
          "metrics: load errors = 1"))



(defvar *tests-db*
  '(test-calculate-percentile
    test-calculate-percentile-edge-cases
    test-ring-buffer-values
    test-metrics-push-latency)
  "DB/metrics domain test functions.")
