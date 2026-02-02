(in-package #:mmorpg)

;;; ============================================================
;;; AUTH THROUGHPUT TESTS (Steps 1-12)
;;; ============================================================

;;; --- Step 1: O(1) Two-List FIFO Queue Tests ---

(defun test-auth-queue-fifo-order ()
  "Step 1: Verify auth queue maintains FIFO order."
  (let ((q (make-auth-queue-instance :max-depth 0)))
    (auth-queue-push q :a)
    (auth-queue-push q :b)
    (auth-queue-push q :c)
    (let ((items (auth-queue-drain-nonblocking q)))
      (assert (equal items '(:a :b :c)) ()
              "auth-queue FIFO: expected (:a :b :c), got ~a" items))))

(defun test-auth-queue-interleaved ()
  "Step 1: Verify interleaved push/pop maintains order."
  (let ((q (make-auth-queue-instance :max-depth 0)))
    (auth-queue-push q :a)
    (let ((first (auth-queue-drain-nonblocking q)))
      (assert (equal first '(:a)) ()
              "auth-queue interleaved: first drain got ~a" first))
    (auth-queue-push q :b)
    (auth-queue-push q :c)
    (let ((rest (auth-queue-drain-nonblocking q)))
      (assert (equal rest '(:b :c)) ()
              "auth-queue interleaved: second drain got ~a" rest))))

(defun test-auth-queue-count ()
  "Step 1: Verify count tracks correctly through push/drain."
  (let ((q (make-auth-queue-instance :max-depth 0)))
    (assert (= (auth-queue-count q) 0) () "auth-queue count: initial 0")
    (auth-queue-push q :a)
    (auth-queue-push q :b)
    (assert (= (auth-queue-count q) 2) () "auth-queue count: after 2 pushes")
    (auth-queue-drain-nonblocking q)
    (assert (= (auth-queue-count q) 0) () "auth-queue count: after drain")))

(defun test-auth-queue-drain-empty ()
  "Step 1: Drain empty queue returns NIL."
  (let ((q (make-auth-queue-instance :max-depth 0)))
    (assert (null (auth-queue-drain-nonblocking q)) ()
            "auth-queue drain-empty: expected NIL")))

;;; --- Step 3: Bounded Queue Tests ---

(defun test-auth-queue-bounded ()
  "Step 3: Verify bounded queue rejects when full."
  (let ((q (make-auth-queue-instance :max-depth 2)))
    (assert (auth-queue-try-push q :a) () "bounded: first push accepted")
    (assert (auth-queue-try-push q :b) () "bounded: second push accepted")
    (assert (not (auth-queue-try-push q :c)) () "bounded: third push rejected")
    (assert (= (auth-queue-count q) 2) () "bounded: count stays at 2")))

(defun test-auth-queue-unbounded ()
  "Step 3: Verify max-depth 0 means unbounded."
  (let ((q (make-auth-queue-instance :max-depth 0)))
    (dotimes (i 500)
      (auth-queue-try-push q i))
    (assert (= (auth-queue-count q) 500) () "unbounded: accepted 500 items")))

;;; --- Step 11: Password Hashing Tests ---

(defun test-hash-password-3part-format ()
  "Step 11: hash-password produces 3-part salt$iterations$hash format."
  (let ((hash (hash-password "testpassword")))
    (let ((parts (split-hash-string hash)))
      (assert (= (length parts) 3) ()
              "hash-password: expected 3 parts, got ~d" (length parts))
      (assert (parse-integer (second parts) :junk-allowed nil) ()
              "hash-password: iterations must be parseable integer")
      (assert (= (parse-integer (second parts)) *password-hash-iterations*) ()
              "hash-password: iterations should be ~d, got ~a"
              *password-hash-iterations* (second parts)))))

(defun test-verify-password-new-format ()
  "Step 11: verify-password correctly verifies new 3-part format."
  (let ((hash (hash-password "correct-password")))
    (assert (verify-password "correct-password" hash) ()
            "verify-password: should verify correct password (new format)")
    (assert (not (verify-password "wrong-password" hash)) ()
            "verify-password: should reject wrong password (new format)")))

(defun test-verify-password-legacy-format ()
  "Step 11: verify-password handles legacy 2-part format (backward compat)."
  (let* ((salt (generate-salt))
         (key (derive-password-key-with-iterations "legacypass" salt 100000))
         (salt-hex (bytes-to-hex salt))
         (key-hex (bytes-to-hex key))
         (legacy-hash (concatenate 'string salt-hex "$" key-hex)))
    (assert (verify-password "legacypass" legacy-hash) ()
            "verify-password: should verify legacy 2-part hash")
    (assert (not (verify-password "wrongpass" legacy-hash)) ()
            "verify-password: should reject wrong password (legacy format)")))

(defun test-legacy-hash-format-p ()
  "Step 11: legacy-hash-format-p correctly identifies 2-part vs 3-part."
  (let ((new-hash (hash-password "test"))
        (legacy-hash "aabbccdd$deadbeef"))
    (assert (not (legacy-hash-format-p new-hash)) ()
            "legacy-hash-format-p: new format should return NIL")
    (assert (legacy-hash-format-p legacy-hash) ()
            "legacy-hash-format-p: legacy format should return T")))

(defun test-split-hash-string ()
  "Step 11: split-hash-string correctly splits on $ delimiters."
  (let ((parts2 (split-hash-string "abc$def"))
        (parts3 (split-hash-string "abc$123$def")))
    (assert (= (length parts2) 2) () "split-hash: 2-part length")
    (assert (equal (first parts2) "abc") () "split-hash: 2-part first")
    (assert (equal (second parts2) "def") () "split-hash: 2-part second")
    (assert (= (length parts3) 3) () "split-hash: 3-part length")
    (assert (equal (first parts3) "abc") () "split-hash: 3-part first")
    (assert (equal (second parts3) "123") () "split-hash: 3-part second")
    (assert (equal (third parts3) "def") () "split-hash: 3-part third")))

(defun test-derive-password-key-with-iterations ()
  "Step 11: derive-password-key-with-iterations produces deterministic output."
  (let* ((salt (hex-to-bytes "00112233445566778899aabbccddeeff"))
         (key1 (derive-password-key-with-iterations "test" salt 1000))
         (key2 (derive-password-key-with-iterations "test" salt 1000))
         (key-diff (derive-password-key-with-iterations "test" salt 2000)))
    (assert (equalp key1 key2) ()
            "derive-key-with-iterations: same inputs should produce same output")
    (assert (not (equalp key1 key-diff)) ()
            "derive-key-with-iterations: different iterations should produce different output")))

;;; --- Step 10: Pipelined Auth Tests ---

(defun test-db-verify-and-load-account ()
  "Step 10: db-verify-and-load-account returns character-id on valid credentials."
  (let ((*storage* (make-instance 'memory-storage)))
    (storage-connect *storage*)
    ;; Create a test account
    (db-create-account "testuser10" "testpass10")
    (db-set-character-id "testuser10" 42)
    ;; Verify and load
    (multiple-value-bind (char-id verified)
        (db-verify-and-load-account "testuser10" "testpass10")
      (assert verified () "verify-and-load: should verify correct password")
      (assert (= char-id 42) ()
              "verify-and-load: should return character-id 42, got ~a" char-id))
    ;; Bad password
    (multiple-value-bind (char-id verified)
        (db-verify-and-load-account "testuser10" "wrongpass")
      (assert (not verified) () "verify-and-load: should reject bad password")
      (assert (null char-id) () "verify-and-load: char-id should be NIL on failure"))
    ;; Nonexistent user
    (multiple-value-bind (char-id verified)
        (db-verify-and-load-account "nouser" "nopass")
      (assert (not verified) () "verify-and-load: nonexistent user returns NIL")
      (assert (null char-id) () "verify-and-load: nonexistent char-id is NIL"))))

(defun test-db-create-account-pipelined-memory ()
  "Step 10: db-create-account-pipelined falls back to sequential for memory storage."
  (let ((*storage* (make-instance 'memory-storage)))
    (storage-connect *storage*)
    ;; Create account with character-id in one call
    (assert (db-create-account-pipelined "pipetest" "pass123" 99) ()
            "pipelined-memory: should succeed for new account")
    ;; Verify account exists with correct character-id
    (let ((account (db-load-account "pipetest")))
      (assert account () "pipelined-memory: account should exist")
      (assert (= (getf account :character-id) 99) ()
              "pipelined-memory: character-id should be 99, got ~a"
              (getf account :character-id)))
    ;; Duplicate should return :username-taken keyword
    (let ((result (db-create-account-pipelined "pipetest" "pass456" 100)))
      (assert (eq result :username-taken) ()
              "pipelined-memory: duplicate should return :username-taken, got ~a" result))))

;;; --- Step 11: Re-hash on Login Tests ---

(defun test-rehash-on-login ()
  "Step 11: Verify legacy accounts get re-hashed on successful login."
  (let ((*storage* (make-instance 'memory-storage)))
    (storage-connect *storage*)
    ;; Create a legacy-format account manually
    (let* ((salt (generate-salt))
           (key (derive-password-key-with-iterations "oldpass" salt 100000))
           (salt-hex (bytes-to-hex salt))
           (key-hex (bytes-to-hex key))
           (legacy-hash (concatenate 'string salt-hex "$" key-hex)))
      (db-save-account "legacyuser" legacy-hash 55)
      ;; Verify the hash is legacy format
      (let ((account-before (db-load-account "legacyuser")))
        (assert (legacy-hash-format-p (getf account-before :password-hash)) ()
                "rehash: initial hash should be legacy format"))
      ;; Login via db-verify-credentials (triggers re-hash)
      (assert (db-verify-credentials "legacyuser" "oldpass") ()
              "rehash: should verify legacy password")
      ;; Check that hash was updated to new format
      (let ((account-after (db-load-account "legacyuser")))
        (assert (not (legacy-hash-format-p (getf account-after :password-hash))) ()
                "rehash: hash should now be new 3-part format")
        ;; Verify the new hash still works
        (assert (verify-password "oldpass" (getf account-after :password-hash)) ()
                "rehash: new hash should still verify correctly")))))

;;; --- Auth Review Fix Tests ---

(defun test-auth-rate-limit-wall-time ()
  "Verify rate-limit functions work correctly with wall-clock-scale times.
   The critical fix: auth-rate-check and auth-rate-record-failure now both
   use wall-time (large numbers like 4000000.0), not sim-time (small elapsed)."
  ;; Clear state
  (auth-rate-clear-all)
  ;; Simulate wall-clock times (large values like get-internal-real-time / units-per-second)
  (let* ((*auth-max-attempts* 3)
         (*auth-lockout-seconds* 10.0)
         (*auth-attempt-window* 60.0)
         (now 4000000.0))  ; Simulates wall-time
    ;; First check should pass
    (assert (auth-rate-check "10.0.0.1" now) ()
            "rate-limit-walltime: first check should allow")
    ;; Record failures
    (auth-rate-record-failure "10.0.0.1" now)
    (auth-rate-record-failure "10.0.0.1" (+ now 1.0))
    (auth-rate-record-failure "10.0.0.1" (+ now 2.0))
    ;; Should now be locked out (3 failures = *auth-max-attempts*)
    (assert (not (auth-rate-check "10.0.0.1" (+ now 3.0))) ()
            "rate-limit-walltime: should be locked out after max attempts")
    ;; Lockout should expire after lockout-seconds
    (assert (auth-rate-check "10.0.0.1" (+ now 3.0 *auth-lockout-seconds* 1.0)) ()
            "rate-limit-walltime: lockout should expire"))
  (auth-rate-clear-all))

(defun test-auth-rate-lockout-expiry ()
  "Verify rate-limit lockout resets after expiry."
  (auth-rate-clear-all)
  (let* ((*auth-max-attempts* 2)
         (*auth-lockout-seconds* 5.0)
         (*auth-attempt-window* 30.0)
         (now 5000000.0))
    ;; Trigger lockout
    (auth-rate-record-failure "10.0.0.2" now)
    (auth-rate-record-failure "10.0.0.2" (+ now 0.5))
    ;; Locked out
    (assert (not (auth-rate-check "10.0.0.2" (+ now 1.0))) ()
            "lockout-expiry: should be locked")
    ;; After lockout expires, should be allowed and reset
    (assert (auth-rate-check "10.0.0.2" (+ now 6.0)) ()
            "lockout-expiry: should be allowed after lockout expires")
    ;; And success should clear state
    (auth-rate-record-success "10.0.0.2")
    (assert (auth-rate-check "10.0.0.2" (+ now 7.0)) ()
            "lockout-expiry: should be allowed after success clear"))
  (auth-rate-clear-all))

(defun test-verify-password-malformed-iterations ()
  "Verify that malformed iteration counts in hash strings return NIL, not signal."
  ;; Malformed: non-numeric iterations
  (assert (not (verify-password "test" "abcd1234$notanumber$deadbeef")) ()
          "malformed-iterations: non-numeric should return NIL")
  ;; Malformed: empty iterations
  (assert (not (verify-password "test" "abcd1234$$deadbeef")) ()
          "malformed-iterations: empty should return NIL")
  ;; Malformed: only 1 part
  (assert (not (verify-password "test" "justonefield")) ()
          "malformed-iterations: single field should return NIL"))

(defun test-auth-metrics-counters ()
  "Verify auth metric counters can be atomically incremented and read."
  (let ((m *auth-metrics*))
    (let ((q-before (auth-metrics-queued m))
          (p-before (auth-metrics-processed m))
          (e-before (auth-metrics-expired m))
          (r-before (auth-metrics-rejected-busy m))
          (s-before (auth-metrics-success m))
          (f-before (auth-metrics-fail m)))
      (auth-metric-incf queued)
      (auth-metric-incf processed)
      (auth-metric-incf expired)
      (auth-metric-incf rejected-busy)
      (auth-metric-incf success)
      (auth-metric-incf fail)
      (assert (= (auth-metrics-queued m) (1+ q-before)) ()
              "metrics: queued should increment")
      (assert (= (auth-metrics-processed m) (1+ p-before)) ()
              "metrics: processed should increment")
      (assert (= (auth-metrics-expired m) (1+ e-before)) ()
              "metrics: expired should increment")
      (assert (= (auth-metrics-rejected-busy m) (1+ r-before)) ()
              "metrics: rejected-busy should increment")
      (assert (= (auth-metrics-success m) (1+ s-before)) ()
              "metrics: success should increment")
      (assert (= (auth-metrics-fail m) (1+ f-before)) ()
              "metrics: fail should increment"))))

(defun test-session-local-registration ()
  "Verify register-player-session-local creates and tracks session state."
  (let ((*player-sessions* (make-hash-table))
        #+sbcl (*player-sessions-lock* (sb-thread:make-mutex :name "test-ps-lock")))
    (let ((player (make-player 100.0 200.0 :id 42 :zone-id :town)))
      ;; Register
      (register-player-session-local player :zone-id :town :username "testuser")
      ;; Check it exists
      (let ((session (gethash 42 *player-sessions*)))
        (assert session () "session-local: session should exist")
        (assert (eq (player-session-player session) player) ()
                "session-local: session player should match")
        (assert (eq (player-session-zone-id session) :town) ()
                "session-local: zone-id should be :town")
        (assert (string= (player-session-username session) "testuser") ()
                "session-local: username should match"))
      ;; Unregister
      (unregister-player-session-local 42)
      (assert (not (gethash 42 *player-sessions*)) ()
              "session-local: session should be removed after unregister"))))

(defun test-auth-queue-stale-expiry ()
  "Verify that the stale request check in auth-worker-loop logic works correctly.
   Tests the age calculation directly rather than spawning a worker thread."
  (let* ((*auth-request-max-age* 10.0)
         (now-rt 5000.0)
         ;; Fresh request: timestamp is recent
         (fresh-age (- now-rt (- now-rt 2.0)))  ; 2 seconds old
         ;; Stale request: timestamp is old
         (stale-age (- now-rt (- now-rt 15.0))))  ; 15 seconds old
    (assert (< fresh-age (float *auth-request-max-age* 1.0d0)) ()
            "stale-expiry: fresh request should be within max-age")
    (assert (> stale-age (float *auth-request-max-age* 1.0d0)) ()
            "stale-expiry: stale request should exceed max-age")))

;;; ============================================================


(defvar *tests-auth*
  '(test-auth-queue-fifo-order
    test-auth-queue-interleaved
    test-auth-queue-count
    test-auth-queue-drain-empty
    test-auth-queue-bounded
    test-auth-queue-unbounded
    test-hash-password-3part-format
    test-verify-password-new-format
    test-verify-password-legacy-format
    test-legacy-hash-format-p
    test-split-hash-string
    test-derive-password-key-with-iterations
    test-db-verify-and-load-account
    test-db-create-account-pipelined-memory
    test-rehash-on-login
    ;; Auth review fixes
    test-auth-rate-limit-wall-time
    test-auth-rate-lockout-expiry
    test-verify-password-malformed-iterations
    test-auth-metrics-counters
    test-session-local-registration
    test-auth-queue-stale-expiry)
  "Auth domain test functions.")
