# Auth Throughput Fix: Implementation Plan

Based on the consolidated investigation in `findings.md`, this plan addresses all 8 identified bottlenecks to ensure login and registration remain functional at 1000+ concurrent players.

---

## Guiding Principles

- **Auth must never become unusable.** Under any server load, a new client must either authenticate within a reasonable time or receive an immediate `:server-busy` rejection so it can retry.
- **Main thread tick budget is sacred.** No blocking DB calls on the main thread. Auth integration must be pure in-memory.
- **Existing tests must pass.** Every change ships with new unit tests covering the added functionality.
- **Incremental delivery.** Each step is independently testable and deployable. Steps are ordered so that the highest-impact fix lands first.

---

## Step 1: Replace Auth Queue with O(1) Two-List FIFO

**Bottleneck addressed:** #5 (O(n) pop)

**Why first:** This is a self-contained data-structure swap with no behavioral changes. It removes the O(n^2) dequeue degradation that compounds every other bottleneck under load, and it's the foundation for the bounded-queue and multi-worker changes that follow.

**Files to modify:**
- `src/net.lisp` (lines 679-757: `auth-queue` struct and all operations)

**Implementation:**
1. Replace the `auth-queue` struct internals. Keep the existing struct name and public API (`auth-queue-push`, `auth-queue-pop-blocking`, `auth-queue-drain-nonblocking`, `make-auth-queue-instance`).
2. Internally, use a two-list FIFO: `push-list` (for producers) and `pop-list` (for consumers). When `pop-list` is empty, atomically reverse `push-list` into `pop-list` under the mutex. This gives O(1) amortized push and pop.
3. Add a `count` field (fixnum) to the struct, incremented on push, decremented on pop/drain. This supports the bounded-queue check in Step 3.
4. `auth-queue-drain-nonblocking` takes the entire `push-list`, reverses it, prepends `pop-list`, returns all items, resets both lists and count to empty/0.

**New fields on `auth-queue`:**
```
push-list  : list   (producers push here)
pop-list   : list   (consumers pop from here)
count      : fixnum (current depth, for backpressure)
```

**Tests to add** (`tests/unit-test.lisp`):
- FIFO ordering: push A, B, C; pop returns A, B, C.
- Interleaved push/pop: push A, pop A, push B, push C, pop B, pop C.
- Drain returns all items in FIFO order and resets count to 0.
- Count tracks correctly through push, pop, and drain operations.
- Empty pop blocks (verify with a short timeout or a secondary thread that pushes after a delay).

**Verification:** `make test-unit` passes. Existing CI/smoke tests pass unchanged.

---

## Step 2: Multiple Auth Worker Threads

**Bottleneck addressed:** #1 (single worker), #2 (PBKDF2 serialized), #3 (Redis calls serialized)

**Why second:** This is the highest-impact fix. With N workers, throughput scales to N × 3-5 req/s. At N=4, that's 12-20 req/s, enough to drain 1000 registrations in ~50-80s instead of ~333s.

**Files to modify:**
- `src/net.lisp` (lines 1120-1146: `start-auth-worker`, `stop-auth-worker`)
- `src/net.lisp` (line 2858: server startup, single `start-auth-worker` call)
- `src/net.lisp` (line 3068: server shutdown, single `stop-auth-worker` call)
- `src/config-server.lisp` (new parameter)

**Implementation:**

1. Add a new config parameter in `src/config-server.lisp`:
   ```lisp
   (defparameter *auth-worker-count* 4
     "Number of auth worker threads. Each independently processes auth requests.
      Override via MMORPG_AUTH_WORKERS environment variable.")
   ```

2. Read `MMORPG_AUTH_WORKERS` env var at server startup (in `run-server`, near the existing `MMORPG_WORKER_THREADS` parsing around line 2790), defaulting to `*auth-worker-count*`.

3. Modify server startup (line 2858) to start N workers instead of 1:
   ```lisp
   (let ((auth-workers
           (loop :repeat auth-worker-count
                 :collect (start-auth-worker auth-request-queue auth-result-queue game))))
     ...)
   ```
   Store the list of threads where `auth-worker` was previously a single thread.

4. Modify `stop-auth-worker` (line 1137-1145) into `stop-auth-workers` that sends N stop signals and joins N threads:
   ```lisp
   (defun stop-auth-workers (request-queue worker-threads)
     (dolist (wt worker-threads)
       (declare (ignore wt))
       (auth-queue-push request-queue (make-auth-request :stop-signal t)))
     #+sbcl
     (dolist (wt worker-threads)
       (when wt
         (sb-thread:join-thread wt :timeout 5.0))))
   ```

5. Update shutdown (line 3068) to call `stop-auth-workers` with the list.

6. Update the server startup log line (line 2865) to show `auth-workers=N`.

**Thread safety audit:** The shared request queue already uses a mutex + condvar. The result queue also uses a mutex. `session-try-register` uses `*session-lock*`. `claim-session-ownership` operates on Redis (atomic). `spawn-player-at-world` uses `game-id-source` which needs to be verified as thread-safe (likely an atomic counter). If not, wrap ID allocation in a lock.

**Tests to add:**
- Start 2+ workers, push N requests, verify all N results arrive.
- Verify concurrent registrations with different usernames all succeed.
- Verify concurrent registrations with the same username: exactly one succeeds, others get `:username-taken`.
- Verify stop signals correctly shut down all N workers.

**Verification:** `make tests` passes. Stress test shows improved auth throughput.

---

## Step 3: Bounded Queue with Backpressure

**Bottleneck addressed:** #4 (unbounded queue, no backpressure)

**Files to modify:**
- `src/net.lisp` (line 718-725: `auth-queue-push`)
- `src/net.lisp` (lines 2906-2925, 2928-2947: `:register`/`:login` message handling in main loop)
- `src/config-server.lisp` (new parameter)

**Implementation:**

1. Add config parameter:
   ```lisp
   (defparameter *auth-queue-max-depth* 200
     "Maximum auth requests queued. Beyond this, server responds :server-busy.
      Override via MMORPG_AUTH_QUEUE_MAX environment variable.")
   ```

2. Add `max-depth` field to `auth-queue` struct (set from `*auth-queue-max-depth*` at creation).

3. Add a `auth-queue-try-push` function that checks `count < max-depth` before pushing. Returns T on success, NIL if full:
   ```lisp
   (defun auth-queue-try-push (queue item)
     "Push ITEM if queue is below max depth. Returns T on success, NIL if full."
     (sb-thread:with-mutex ((auth-queue-lock queue))
       (when (< (auth-queue-count queue) (auth-queue-max-depth queue))
         (push item (auth-queue-push-list queue))
         (incf (auth-queue-count queue))
         (sb-thread:condition-notify (auth-queue-condvar queue))
         t)))
   ```

4. In the main loop message handlers for `:register` and `:login` (lines 2906-2947), replace `auth-queue-push` with `auth-queue-try-push`. When it returns NIL, send `:auth-fail :reason :server-busy` immediately:
   ```lisp
   (unless (auth-queue-try-push auth-request-queue request)
     (send-net-message-with-retry socket
       (list :type :auth-fail :reason :server-busy)
       :host host :port port :max-retries 3 :delay 50))
   ```

5. On the client side: ensure `:server-busy` is handled (display message to player, auto-retry with backoff). Check `src/net.lisp` client-side auth response handling for how `:auth-fail` reasons are processed and add `:server-busy` if not already handled.

**Tests to add:**
- Push `max-depth` items: all succeed.
- Push `max-depth + 1`: last push returns NIL.
- After popping one, push succeeds again.
- After drain, count resets and pushes succeed.

**Verification:** `make tests` passes.

---

## Step 4: Stale Request Expiry in Workers

**Bottleneck addressed:** #4 (wasted worker time on timed-out clients)

**Files to modify:**
- `src/net.lisp` (lines 1087-1118: `auth-worker-loop`)
- `src/config-server.lisp` (new parameter)

**Implementation:**

1. Add config parameter:
   ```lisp
   (defparameter *auth-request-max-age* 25.0
     "Seconds before a queued auth request is considered stale and skipped.
      Should be less than client auth timeout (30s) to avoid wasted work.")
   ```

2. The `auth-request` struct already has a `timestamp` field (line 693). In `auth-worker-loop`, after popping a request and before processing, check if the request has been queued too long:
   ```lisp
   (let* ((request (auth-queue-pop-blocking request-queue))
          (age (- current-elapsed (auth-request-timestamp request))))
     (when (> age *auth-request-max-age*)
       (incf *auth-expired-count*)
       (auth-queue-push result-queue
         (make-auth-result :type (auth-request-type request)
                           :success nil
                           :host (auth-request-host request)
                           :port (auth-request-port request)
                           :username (auth-request-username request)
                           :client (auth-request-client request)
                           :error-reason :request-expired))
       ;; Skip processing, loop to next request
       ...))
   ```

3. **Elapsed time in worker thread:** The worker doesn't currently have access to the server's elapsed clock. Two options:
   - **Option A (preferred):** Use `get-internal-real-time` wall clock in both the main thread (when setting `auth-request-timestamp`) and the worker (when checking age). Change the main loop to set `timestamp` to `(/ (get-internal-real-time) internal-time-units-per-second)` instead of the game-clock `elapsed` value.
   - **Option B:** Pass a shared atomic elapsed counter that the main loop updates each frame. Workers read it to compute age.

   Option A is simpler and avoids shared mutable state.

4. The double rate-limit check (Bottleneck #7) is partially addressed here: if a request sits in queue long enough to become stale, it's expired before the worker wastes time re-checking rate limits and doing DB work.

**Tests to add:**
- Create a request with timestamp 30s in the past; verify worker returns `:request-expired` without doing DB work.
- Create a request with timestamp 1s ago; verify it's processed normally.

**Verification:** `make tests` passes.

---

## Step 5: Move `register-player-session` DB Calls Off Main Thread

**Bottleneck addressed:** #8 (blocking DB calls in `integrate-auth-results` on main thread)

**Files to modify:**
- `src/net.lisp` (lines 1148-1219: `integrate-auth-results`)
- `src/net.lisp` (lines 760-882: `process-register-async`)
- `src/net.lisp` (lines 884-983: `process-login-async`)
- `src/db.lisp` (line 1699: `register-player-session`)
- `src/net.lisp` (`auth-result` struct, line 696)

**Implementation:**

Currently `integrate-auth-results` (main thread) calls `register-player-session` which does:
1. `claim-session-ownership` (Redis SET) -- already done in auth worker for login
2. Local session hash-table insert (in-memory, fast)
3. `db-add-online-player` (Redis SADD)
4. `db-update-leaderboard-xp` (Redis ZADD)
5. `db-update-leaderboard-level` (Redis ZADD)
6. `db-update-leaderboard-deaths` (Redis ZADD)

**Plan:** Split `register-player-session` into two phases:

- **Phase A (worker thread):** All Redis calls -- `claim-session-ownership`, `db-add-online-player`, `db-update-leaderboard-*`. Performed in `process-register-async` / `process-login-async` before building the auth-result. Add a boolean field `session-db-registered` to `auth-result` to signal success.

- **Phase B (main thread):** Only the local in-memory session hash-table insert (`with-player-sessions-lock` + `setf gethash`). This is O(1) and non-blocking.

Create a new function `register-player-session-local` that only does the in-memory part:
```lisp
(defun register-player-session-local (player &key zone-id username)
  "Register local session state only (no DB calls). For main-thread use."
  (let ((player-id (player-id player)))
    (with-player-sessions-lock
      (setf (gethash player-id *player-sessions*)
            (make-player-session :player player
                                 :zone-id zone-id
                                 :username username
                                 :dirty-p nil
                                 :last-flush (float (get-internal-real-time) 1.0)
                                 :tier1-pending nil)))
    (log-verbose "Registered local session for player ~a in zone ~a" player-id zone-id)
    t))
```

Create a new function `register-player-session-db` that does only the Redis calls:
```lisp
(defun register-player-session-db (player-id player)
  "Register session in DB: online set + leaderboards. For worker-thread use."
  (db-add-online-player player-id)
  (db-update-leaderboard-xp player-id (player-lifetime-xp player))
  (let ((level (combat-level (player-stats player))))
    (db-update-leaderboard-level player-id level))
  (db-update-leaderboard-deaths player-id (player-deaths player)))
```

In `process-register-async` and `process-login-async`, call `register-player-session-db` after the existing `session-try-register` / `claim-session-ownership` calls.

In `integrate-auth-results`, replace the `register-player-session` call (line 1184) with `register-player-session-local`.

Keep the original `register-player-session` function intact for use by other callers (tests, admin, `db-player-login`). It continues to do both phases.

**Tests to add:**
- `register-player-session-local` creates a session entry without touching storage.
- `register-player-session-db` updates online set and leaderboards in storage.
- Full round-trip: worker calls `register-player-session-db`, main thread calls `register-player-session-local`, session is fully registered.

**Verification:** `make tests` passes. No main-thread Redis calls during auth integration.

---

## Step 6: Auth Metrics

**Bottleneck addressed:** #5 from proposed fixes (observability)

**Files to modify:**
- `src/net.lisp` (new defparameters for counters, updates in auth code paths)
- `src/config-server.lisp` (metric logging interval)

**Implementation:**

1. Add global atomic counters (defparameters):
   ```lisp
   (defparameter *auth-metric-queued* 0 "Total auth requests queued.")
   (defparameter *auth-metric-processed* 0 "Total auth requests processed by workers.")
   (defparameter *auth-metric-expired* 0 "Total stale requests expired without processing.")
   (defparameter *auth-metric-rejected-busy* 0 "Total requests rejected due to queue full.")
   (defparameter *auth-metric-success* 0 "Total successful auths.")
   (defparameter *auth-metric-fail* 0 "Total failed auths.")
   ```

2. Increment at appropriate points:
   - `*auth-metric-queued*`: on successful `auth-queue-try-push`
   - `*auth-metric-processed*`: in `auth-worker-loop` after processing a request
   - `*auth-metric-expired*`: in `auth-worker-loop` when skipping stale request
   - `*auth-metric-rejected-busy*`: in main loop when `auth-queue-try-push` returns NIL
   - `*auth-metric-success*` / `*auth-metric-fail*`: in `integrate-auth-results`

3. Add queue depth to periodic server stats logging. The auth queue's `count` field provides this. Log every 10s or on verbose:
   ```
   [AUTH] queue=12 queued=456 processed=440 expired=3 busy=13 ok=420 fail=20
   ```

4. Use `sb-ext:atomic-incf` for thread-safe counter increments (counters updated from multiple worker threads).

**Tests to add:**
- Process a request, verify `*auth-metric-processed*` increments.
- Reject a request (queue full), verify `*auth-metric-rejected-busy*` increments.

**Verification:** `make tests` passes. Metrics visible in verbose server output.

---

## Step 7: Bound Main Loop Message Drain

**Bottleneck addressed:** #6 (unbounded message drain starves auth integration)

**Files to modify:**
- `src/net.lisp` (lines 2887-2967: message receive loop)
- `src/config-server.lisp` (new parameter)

**Implementation:**

1. Add config parameter:
   ```lisp
   (defparameter *max-messages-per-tick* 2000
     "Maximum UDP messages processed per tick before yielding to simulation.
      Prevents message flood from starving auth integration and snapshots.
      Override via MMORPG_MAX_MESSAGES_PER_TICK environment variable.")
   ```

2. Add a counter to the message receive loop (line 2887):
   ```lisp
   (let ((msg-count 0))
     (loop
       (when (>= msg-count *max-messages-per-tick*)
         (return))
       (multiple-value-bind (message host port)
           (receive-net-message socket recv-buffer)
         (unless message (return))
         (incf msg-count)
         ;; ... existing message processing ...
         )))
   ```

3. The remaining messages stay in the OS UDP buffer and are read next tick. Under sustained overload, some UDP packets may be dropped by the OS -- this is acceptable and preferred over starving the simulation/auth.

**Impact analysis:** At 1000 clients × 5 Hz = 5000 messages/tick at 60 Hz. A cap of 2000 means ~2.5 ticks to drain all messages (42ms). Intents arriving one tick late is invisible to players. Auth integration and simulation run every tick instead of being starved.

**Tests to add:**
- Unit test: verify the cap is respected (mock receive to return unlimited messages, confirm loop exits after `*max-messages-per-tick*`).

**Verification:** `make tests` passes. Server tick time stays under budget at high message rates.

---

## Step 8: Handle `:server-busy` on Client Side

**Bottleneck addressed:** Completes the backpressure loop (client must respond to `:server-busy`)

**Files to modify:**
- `src/net.lisp` (client-side auth response handling)

**Implementation:**

1. Find where the client handles `:auth-fail` responses. Add `:server-busy` as a recognized reason.
2. Display a user-visible message: "Server is busy, retrying..." (or similar).
3. Implement client-side auto-retry with exponential backoff (e.g., 1s, 2s, 4s, max 3 attempts).
4. After max retries, show "Server is too busy. Please try again later."

**Tests to add:**
- Smoke test covers client auth flow; verify `:server-busy` doesn't crash the client.

**Verification:** `make smoke` passes.

---

## Step 9: Remove Redundant Worker-Side Rate-Limit Check

**Bottleneck addressed:** #7 (double rate-limit check wastes queue slots and worker time)

**Problem:** The main thread checks `auth-rate-check` before queueing (lines 2906, 2928). The worker thread checks `auth-rate-check` again after dequeuing (lines 770, 896). If the host became rate-limited while queued, the worker rejects the request -- but it already occupied a queue slot and waited for a worker pop, displacing a legitimate request.

**Files to modify:**
- `src/net.lisp` (lines 770-779: `process-register-async` re-check)
- `src/net.lisp` (lines 896-905: `process-login-async` re-check)

**Implementation:**

1. Remove the `auth-rate-check` calls at lines 770 and 896 in the worker-side functions `process-register-async` and `process-login-async`.

2. The main-thread check (lines 2906, 2928) is the authoritative gate. It runs synchronously on the only thread that enqueues, so there is no TOCTOU race for the enqueue decision. Once a request passes the main-thread check and enters the queue, it should be processed -- the worker should not second-guess it.

3. The stale-expiry check added in Step 4 provides a stronger guard than the redundant rate-limit re-check: if a request sat in the queue long enough for circumstances to change, it's expired entirely rather than just re-checking one condition.

4. Keep the `auth-rate-record-failure` calls in the worker (lines 875, 914) so that failed auth attempts (bad credentials, internal errors) still feed the rate limiter for future enqueue decisions on the main thread.

**Tests to add:**
- Verify that a request enqueued while the host was allowed is processed even if the host would now be rate-limited (i.e., worker does not reject it).
- Verify that `auth-rate-record-failure` still fires on actual auth failures (bad credentials), so subsequent main-thread checks correctly rate-limit the host.

**Verification:** `make tests` passes.

---

## Step 10: Reduce Auth Redis Round-Trips via Pipelining

**Bottleneck addressed:** #3 (multiple blocking Redis calls per auth request, new connection per call)

**Problem:** Each `storage-*` method opens a fresh `redis:with-connection` (see `db.lisp:420`, `439`, `456`, `466`). A single registration performs 4-5 sequential Redis calls, each opening and closing a TCP connection. Under load with multiple workers, this multiplies connection overhead and adds latency.

The codebase already has a `storage-save-batch` method (`db.lisp:498-529`) that demonstrates Redis pipelining within a single connection. The auth path does not use it.

**Files to modify:**
- `src/db.lisp` (new: `db-create-account-pipelined`, `db-verify-and-load-account`)
- `src/net.lisp` (`process-register-async`, `process-login-async` to call new functions)

**Implementation:**

### 10a. Pipelined Registration

Currently `db-create-account` (`db.lisp:2119-2128`) makes 3 sequential connection-per-call Redis operations:
1. `db-account-exists-p` -> `storage-exists-p` (EXISTS, own connection)
2. `hash-password` (CPU, no Redis)
3. `db-save-account` -> `storage-save` (SET + RENAME, own connection)

Then `db-set-character-id` (`db.lisp:2144-2150`) makes 2 more:
4. `db-load-account` -> `storage-load` (GET, own connection)
5. `db-save-account` -> `storage-save` (SET + RENAME, own connection)

Create `db-create-account-pipelined` that opens **one** connection and pipelines where possible:
```lisp
(defun db-create-account-pipelined (username password character-id)
  "Create account, hash password, and link character-id in one Redis connection.
   Returns the password-hash on success, NIL if username taken."
  (let* ((key (account-key username))
         (password-hash nil))
    ;; Phase 1: Check existence (must be synchronous - conditional on result)
    (redis:with-connection (:host ... :port ...)
      (when (plusp (red:exists key))
        (return-from db-create-account-pipelined nil))
      ;; Phase 2: Hash password (CPU work, no Redis)
      (setf password-hash (hash-password password))
      ;; Phase 3: Save account with character-id in one pipelined write
      (let* ((data (list :version *account-schema-version*
                         :username (string-downcase username)
                         :password-hash password-hash
                         :character-id character-id))
             (temp-key (format nil "temp:~a:~a" key (get-internal-real-time))))
        (redis:with-pipelining
          (red:set temp-key (prin1-to-string data))
          (red:rename temp-key key))))
    password-hash))
```

This reduces 4 connections + 5 Redis commands to **1 connection + 3 commands** (EXISTS, SET, RENAME). The password hash happens mid-connection while the TCP socket is idle.

Update `process-register-async` to call `db-create-account-pipelined` instead of the separate `db-create-account` + `db-set-character-id` sequence. The new function takes `character-id` upfront (from `spawn-player-at-world` which must still happen first for ID allocation).

Reorder the registration flow:
1. `spawn-player-at-world` (get player-id) -- this must happen before the pipelined call since we need the character-id
2. `db-create-account-pipelined username password player-id` (one connection: EXISTS + hash + SET + RENAME)
3. On failure (username taken), no rollback needed since no account was created

### 10b. Pipelined Login Credential Check + Character Load

Currently login makes 3+ separate connections:
1. `db-verify-credentials` -> `db-load-account` (GET) + `verify-password` (CPU)
2. `db-get-character-id` -> `db-load-account` (GET) -- loads the same account again
3. `db-load-player-validated` -> `storage-load` (GET)

Create `db-verify-and-load-account` that loads the account **once** and returns all needed fields:
```lisp
(defun db-verify-and-load-account (username password)
  "Verify credentials and return (values character-id T) on success,
   (values nil nil) on bad credentials or not found."
  (let ((account (db-load-account username)))
    (when account
      (let ((stored-hash (getf account :password-hash)))
        (when (and stored-hash (verify-password password stored-hash))
          (values (getf account :character-id) t))))))
```

This eliminates the redundant second `db-load-account` call in `process-login-async`. One GET instead of two for the account record.

Update `process-login-async` to use `db-verify-and-load-account` which returns the character-id directly, skipping the separate `db-get-character-id` call.

### 10c. Connection Reuse Strategy

The `redis:with-connection` macro in cl-redis opens a new TCP connection each time. For auth workers that make multiple Redis calls per request, this is wasteful.

Add a per-worker persistent Redis connection by wrapping the worker loop body in a single `redis:with-connection`:
```lisp
(defun auth-worker-loop (request-queue result-queue game)
  (let ((world (game-world game))
        (id-source (game-id-source game)))
    ;; One persistent Redis connection for the lifetime of this worker
    (redis:with-connection (:host (redis-storage-host *storage*)
                            :port (redis-storage-port *storage*))
      (loop
        (let ((request (auth-queue-pop-blocking request-queue)))
          (when (auth-request-stop-signal request) (return))
          ;; All Redis calls within process-register/login-async
          ;; reuse this connection via cl-redis's dynamic binding
          ...)))))
```

cl-redis uses a special variable `redis:*connection*` for the active connection. Inner `redis:with-connection` calls will create nested connections unless we use the raw `red:*` commands directly inside the outer connection scope. The pipelined functions from 10a/10b should use `red:*` commands directly (as `storage-save-batch` already does) rather than going through `storage-*` methods that each open their own connection.

**Alternative (simpler):** If modifying the storage abstraction is too invasive, create auth-specific DB functions that take a connection parameter or operate within an already-established `redis:with-connection` scope. The pipelined functions in 10a/10b already do this naturally.

**Tests to add:**
- `db-create-account-pipelined` creates account with character-id in one call; verify account exists with correct fields.
- `db-create-account-pipelined` returns NIL for duplicate username.
- `db-verify-and-load-account` returns character-id for valid credentials, NIL for invalid.
- Verify registration and login still work end-to-end through smoke test.

**Verification:** `make tests` passes. Auth Redis calls per registration reduced from 5 connections to 1.

---

## Step 11: Reduce PBKDF2 Cost

**Bottleneck addressed:** #2 (100k-iteration PBKDF2 is a primary throughput limiter)

**Problem:** `*password-hash-iterations*` is 100,000 (`db.lisp:1883`). Each `hash-password` or `verify-password` call takes ~50-200ms of CPU. Even with 4 workers (Step 2), each worker is CPU-bound during hashing, limiting per-worker throughput to ~5-10 req/s. This is the single largest per-request cost.

**Files to modify:**
- `src/db.lisp` (line 1883: `*password-hash-iterations*`)
- `src/config-server.lisp` (new parameter + env var override)
- `src/db.lisp` (migration logic for re-hashing on login)

**Implementation:**

### 11a. Reduce Iteration Count

Move the iteration count to `config-server.lisp` as a configurable parameter with env var override:
```lisp
(defparameter *password-hash-iterations* 10000
  "PBKDF2-SHA256 iteration count for password hashing.
   OWASP recommends 100,000+ for banking. For game auth with multiple
   worker threads, 10,000 provides adequate security with ~10x faster hashing.
   Override via MMORPG_PASSWORD_HASH_ITERATIONS environment variable.")
```

**Rationale for 10,000:**
- OWASP 100k is designed for single-threaded web servers protecting high-value financial accounts. A game account has lower value than a bank account.
- At 10k iterations, hashing drops from ~100ms to ~10ms -- a 10× improvement.
- With 4 workers at ~10ms/hash, auth throughput becomes ~400 req/s (was ~20 req/s), making hashing no longer the bottleneck.
- 10k PBKDF2-SHA256 iterations still requires ~months of GPU time to brute-force a strong password.

### 11b. Transparent Re-Hash on Login (Migration)

Existing accounts were hashed with 100k iterations. When reducing iterations, new registrations use the new count, but existing logins must still verify against the old hash. PBKDF2 output includes the iteration count implicitly (different iterations = different hash), so we need a migration strategy.

**Option A (recommended): Store iteration count in hash string.**

Change the hash format from `salt$hash` to `salt$iterations$hash`:
```lisp
;; New format
(defun hash-password (password)
  (let* ((salt (generate-salt))
         (key (derive-password-key password salt))
         (salt-hex (bytes-to-hex salt))
         (key-hex (bytes-to-hex key)))
    (format nil "~a$~d$~a" salt-hex *password-hash-iterations* key-hex)))

(defun verify-password (password stored-hash)
  (let* ((parts (split-hash-string stored-hash)))  ; split on $
    (cond
      ;; New format: salt$iterations$hash
      ((= (length parts) 3)
       (let* ((salt (hex-to-bytes (first parts)))
              (iterations (parse-integer (second parts)))
              (stored-key-hex (third parts))
              (computed-key (derive-password-key-with-iterations password salt iterations))
              (computed-key-hex (bytes-to-hex computed-key)))
         (ironclad:constant-time-equal ...)))
      ;; Legacy format: salt$hash (assumes 100k iterations)
      ((= (length parts) 2)
       (let* ((salt (hex-to-bytes (first parts)))
              (stored-key-hex (second parts))
              (computed-key (derive-password-key-with-iterations password salt 100000))
              (computed-key-hex (bytes-to-hex computed-key)))
         (ironclad:constant-time-equal ...))))))
```

Add `derive-password-key-with-iterations` that takes an explicit iteration count parameter (the current `derive-password-key` uses the global `*password-hash-iterations*`).

**Option B: Re-hash on successful login.**

After verifying a legacy hash (100k iterations), re-hash the password with the new iteration count and save the updated hash. This transparently migrates accounts:
```lisp
;; In db-verify-credentials, after successful verify:
(when (legacy-hash-format-p stored-hash)
  (let ((new-hash (hash-password password)))  ; uses new *password-hash-iterations*
    (db-save-account username new-hash (getf account :character-id))))
```

Recommend implementing **both A and B**: A for correctness (always verify with the right iteration count), B for gradual migration (accounts re-hash to the new count on next login).

### 11c. Account Schema Version Bump

Bump `*account-schema-version*` from 2 to 3 in `db.lisp:2077`. Add a migration in `migrations.lisp` that handles the format change. Since the hash format is self-describing (2-part = legacy, 3-part = new), no batch migration is needed -- accounts migrate on next login.

**Tests to add:**
- `hash-password` produces 3-part format `salt$iterations$hash`.
- `verify-password` correctly verifies new 3-part format.
- `verify-password` correctly verifies legacy 2-part format (backward compatibility with 100k iterations).
- Re-hash on login: verify legacy account gets updated hash after successful login.
- Performance: verify `hash-password` at 10k iterations completes in <20ms.

**Verification:** `make tests` passes. Auth per-request CPU cost reduced ~10×.

---

## Step 12: Update Stress Test with Latency Metrics

**Addresses:** Stress test observability gap

**Files to modify:**
- `scripts/stress-test.lisp`

**Implementation:**

1. Record wall-clock time when auth request is sent (`stress-client-last-auth-time` already exists).
2. On auth success, compute `auth-latency = now - last-auth-time`. Track min/avg/max/p99.
3. Add a `:server-busy` counter alongside existing `:auth-fail` counter.
4. Print latency stats in periodic report:
   ```
   [STRESS] Active: 500 | Pending: 30 | Auth ok=480 fail=5 timeout=10 busy=5
            Auth latency: min=0.2s avg=3.1s max=12.4s p99=11.8s
   ```

**Tests:** Manual verification by running the stress test.

**Verification:** Stress test runs and shows latency metrics.

---

## Execution Order and Dependencies

```
Step 1: O(1) Queue          (no dependencies, foundation for 2+3)
  |
  +---> Step 2: Multi-Worker   (depends on Step 1 for efficient queue)
  |       |
  |       +---> Step 4: Stale Expiry     (depends on Step 2, workers check age)
  |       |
  |       +---> Step 5: Move DB Off Main (depends on Step 2, workers do DB)
  |       |
  |       +---> Step 9: Remove Double Rate Check (depends on Step 2+4, cleanup)
  |       |
  |       +---> Step 10: Redis Pipelining (depends on Step 2, workers own connections)
  |
  +---> Step 3: Bounded Queue   (depends on Step 1 for count field)
          |
          +---> Step 8: Client :server-busy  (depends on Step 3, needs new reason)

Step 6: Auth Metrics          (independent, can be done alongside any step)
Step 7: Bound Message Drain   (independent, can be done alongside any step)
Step 11: Reduce PBKDF2 Cost   (independent, can be done alongside any step)
Step 12: Stress Test Metrics   (independent, can be done alongside any step)
```

Steps 6, 7, 11, and 12 are independent and can be implemented in parallel with the main chain.

---

## Expected Outcome

| Metric | Before | After (projected) |
|--------|--------|--------------------|
| Auth throughput | 3-5 req/s (1 worker) | 100-400 req/s (4 workers + fast hash + pipelining) |
| Per-request hash cost | ~100ms (100k PBKDF2) | ~10ms (10k PBKDF2) |
| Redis connections per registration | 4-5 (one per call) | 1 (pipelined) |
| Redis connections per login | 3-4 (one per call) | 1-2 (deduplicated + reused) |
| Queue pop cost | O(n) per pop | O(1) amortized |
| Queue growth | Unbounded | Capped at 200 |
| Client feedback on overload | 30s timeout | Immediate `:server-busy` |
| Main thread DB stall from auth | 10-50ms/frame | 0ms (in-memory only) |
| Stale request waste | Workers process all | Workers skip requests >25s old |
| Wasted rate-limit re-checks | Every dequeued request | None (single check at enqueue) |
| Message drain starvation | Unbounded | Capped at 2000/tick |
| Auth observability | ok/fail/timeout counts | + queue depth, latency, expiry, busy |

With all steps applied, a burst of 1000 registrations:
- Queue accepts first 200, rejects remainder immediately with `:server-busy`
- 4 workers at ~100 req/s each (10ms hash + 5ms pipelined Redis) drain 200 requests in ~0.5s
- Clients receiving `:server-busy` retry with backoff, second wave drains in <1s
- Steady-state: workers keep up with any realistic arrival rate, queue stays near-empty
- **Zero auth timeouts under normal operation**

---

## Files Modified (Summary)

| File | Changes |
|------|---------|
| `src/config-server.lisp` | New: `*auth-worker-count*`, `*auth-queue-max-depth*`, `*auth-request-max-age*`, `*max-messages-per-tick*`, `*password-hash-iterations*` (moved from db.lisp + env var override) |
| `src/net.lisp` | Rewrite auth-queue internals (O(1) two-list FIFO + count + max-depth), `auth-queue-try-push`, multi-worker start/stop, stale expiry in worker loop, remove redundant worker-side rate-limit checks, per-worker persistent Redis connection in `auth-worker-loop`, bounded message drain, metric counters, `integrate-auth-results` uses `register-player-session-local`, client `:server-busy` handling |
| `src/db.lisp` | New: `register-player-session-local`, `register-player-session-db` (split of existing), `db-create-account-pipelined` (single-connection registration), `db-verify-and-load-account` (eliminates redundant account load), `derive-password-key-with-iterations`, updated `hash-password` / `verify-password` for 3-part format with iteration count, re-hash on login migration, `*account-schema-version*` bump to 3 |
| `src/migrations.lisp` | Account schema v2 -> v3 migration (hash format self-describing, no batch migration needed) |
| `tests/unit-test.lisp` | New tests for: O(1) queue, bounded queue, multi-worker, stale expiry, session split, metrics, message drain cap, pipelined registration, deduplicated login, PBKDF2 iteration reduction, hash format backward compatibility, re-hash on login |
| `scripts/stress-test.lisp` | Add latency tracking (min/avg/max/p99), `:server-busy` counter |
