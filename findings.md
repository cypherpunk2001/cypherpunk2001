# Auth Throughput Under Load: Investigation Findings

## Summary

**Yes, auth (login/registration) becomes unusable under high server load.** The root cause is a single auth worker thread processing requests serially, combined with expensive per-request PBKDF2 hashing and multiple blocking Redis calls. With ~1000 concurrent players, the auth queue backs up faster than it drains, causing clients to time out.

---

## Architecture Overview

The current auth flow:

1. Main thread receives UDP `:register`/`:login` messages in an unbounded read loop (`net.lisp:2887-2967`)
2. Messages are pushed to an `auth-queue` (thread-safe list, `net.lisp:679-725`)
3. A **single** auth worker thread (`net.lisp:1087-1135`) pops requests one at a time via `auth-queue-pop-blocking`
4. Each request performs blocking DB operations (Redis) and CPU-expensive password hashing
5. Results are pushed to a result queue, drained by the main thread once per frame (`net.lisp:2970`)

---

## Bottleneck #1: Single Auth Worker Thread (CRITICAL)

**Location:** `src/net.lisp:1087-1135` (`auth-worker-loop`, `start-auth-worker`)

There is exactly **one** auth worker thread. It processes requests sequentially via a blocking pop:

```lisp
(loop
  (let ((request (auth-queue-pop-blocking request-queue)))
    (let ((result (case (auth-request-type request)
                    (:register (process-register-async request world id-source))
                    (:login (process-login-async request world id-source)))))
      (when result
        (auth-queue-push result-queue result)))))
```

If each auth request takes 200-500ms (see below), throughput is **2-5 requests/second**. With 1000 clients trying to register at 2/sec spawn rate, the queue grows unboundedly and most clients time out at 30s.

**Math:** 1000 registrations / 3 per second = ~333 seconds to drain. Client timeout is 30s. Result: **~91% of clients will time out.**

---

## Bottleneck #2: PBKDF2 Password Hashing (100,000 iterations)

**Location:** `src/db.lisp:1883, 1894-1903`

Each registration calls `hash-password` which runs PBKDF2-SHA256 with **100,000 iterations**:

```lisp
(defparameter *password-hash-iterations* 100000)

(defun derive-password-key (password salt)
  (ironclad:derive-key
   (ironclad:make-kdf 'ironclad:pbkdf2 :digest 'ironclad:sha256)
   password-bytes salt *password-hash-iterations* 32))
```

Each login calls `verify-password` which re-derives the key (same cost). On typical hardware, 100k PBKDF2-SHA256 iterations take **50-200ms per call**. This runs on the single auth worker thread, blocking all other auth requests.

- Registration: 1x `hash-password` (~100ms)
- Login: 1x `verify-password` (~100ms)

This alone limits throughput to ~5-10 auth ops/second on a single thread.

---

## Bottleneck #3: Multiple Blocking Redis Calls Per Auth Request

**Registration** (`net.lisp:760-882`, `db.lisp:2119-2128`) performs:

1. `db-account-exists-p` -> `storage-exists-p` (Redis EXISTS)
2. `hash-password` (CPU, ~100ms)
3. `db-save-account` -> `storage-save` (Redis SET + RENAME)
4. `spawn-player-at-world` (ID allocation)
5. `db-set-character-id` -> `db-load-account` + `db-save-account` (Redis GET + SET + RENAME)

Each Redis call wrapped in `with-retry-exponential` (3 retries, 50-200ms delays).

**Login** (`net.lisp:884-983`) performs:

1. `db-verify-credentials` -> `db-load-account` (Redis GET) + `verify-password` (CPU, ~100ms)
2. `db-get-character-id` -> `db-load-account` (Redis GET)
3. `claim-session-ownership` (Redis operation)
4. `db-load-player-validated` -> `storage-load` (Redis GET) + validation

**Per-call Redis overhead:** Each `storage-save` in `db.lisp` opens a **new Redis connection** per call (no connection pooling evident), does SET + RENAME. Under load, Redis connection establishment adds latency.

**Worst case per registration:** ~100ms hashing + 4-5 Redis round trips (~5-20ms each) + retry delays = **200-500ms total**.

---

## Bottleneck #4: Unbounded Auth Queue (No Backpressure)

**Location:** `src/net.lisp:679-725`

The auth queue is an unbounded list with no size limit:

```lisp
(defstruct auth-queue
  (items nil :type list)   ;; NO SIZE LIMIT
  (lock nil)
  (condvar nil))

(defun auth-queue-push (queue item)
  (sb-thread:with-mutex ((auth-queue-lock queue))
    (push item (auth-queue-items queue))          ;; Always succeeds
    (sb-thread:condition-notify (auth-queue-condvar queue))))
```

When auth requests arrive faster than the single worker can process them:
- Queue grows without bound
- Memory pressure increases
- No "server busy" response sent to clients
- Clients wait 30s then time out, wasting the worker's effort on requests whose clients have already given up

---

## Bottleneck #5: Queue Pop is O(n)

**Location:** `src/net.lisp:727-745`

The `auth-queue-pop-blocking` uses `(car (last items))` and `(butlast items)` to get FIFO ordering from a push-front list:

```lisp
(let ((item (car (last (auth-queue-items queue)))))
  (setf (auth-queue-items queue) (butlast (auth-queue-items queue)))
  item)
```

Both `last` and `butlast` are **O(n)** list traversals. With 500 items queued, each pop traverses the entire list twice. This adds overhead as the queue grows under load.

---

## Bottleneck #6: Main Loop Message Drain is Unbounded

**Location:** `src/net.lisp:2887-2967`

The main loop reads **all** pending UDP messages before doing anything else:

```lisp
(loop
  (multiple-value-bind (message host port)
      (receive-net-message socket recv-buffer)
    (unless message (return))
    ;; process message...
    ))
;; Only AFTER all messages drained:
(integrate-auth-results ...)
(server-step ...)
(send-snapshots ...)
```

With 1000 active clients sending intents at 5 Hz = 5000 messages/frame. Reading and parsing all of them before integrating auth results or running simulation means:
- Auth result delivery is delayed
- Simulation tick budget is consumed by message parsing
- Under extreme load, the frame takes >16.67ms, causing cascading delays

---

## Bottleneck #7: Rate Limit Checked Twice (Wasted Work)

**Location:** `src/net.lisp:2906` (main thread) and `src/net.lisp:770,896` (worker thread)

The main thread checks `auth-rate-check` before queueing. The worker thread checks again when it dequeues (since the host may have been rate-limited while queued). If a host triggers rate-limiting between queue and dequeue, the request was queued for nothing, consuming queue space and worker time to reject it.

---

## Bottleneck #8: Auth Integration Does Blocking DB Work on Main Thread

**Location:** `src/net.lisp:1148-1219` (`integrate-auth-results`) -> `register-player-session` -> `src/db.lisp`

Even after the auth worker finishes, the main thread's `integrate-auth-results` calls `register-player-session`, which performs **additional blocking storage operations**:

- `claim-session-ownership` (Redis SET with ownership flag)
- `db-add-online-player` (Redis SADD to online-players set)
- `db-update-leaderboard-*` (Redis ZADD to leaderboard sorted sets)

These are synchronous Redis calls executed **on the main game loop thread**. Under high load with many auth results completing in the same frame, the main thread stalls on DB I/O during what should be a lightweight integration step. This directly eats into the 16.67ms tick budget and delays simulation, snapshot sending, and processing of the next batch of incoming messages.

With 1000+ players, even a few auth completions per frame can add 10-50ms of main-thread DB latency, compounding all other bottlenecks.

---

## Impact Analysis

### At 100 concurrent players
- Auth queue rarely exceeds 5-10 items
- Worker processes fast enough, most clients authenticate within 1-5s
- **Auth works fine**

### At 500 concurrent players
- Auth queue grows during registration bursts
- Some clients experience 10-20s auth delays
- Occasional timeouts at 30s
- **Auth is degraded**

### At 1000 concurrent players
- Auth queue backs up to hundreds of items
- Worker throughput (~3-5/sec) cannot keep up with arrival rate
- Most new clients time out before being processed
- Queue memory grows, O(n) pop gets slower
- Main thread message drain takes longer, further delaying auth result integration
- **Auth is effectively unusable for new connections**

---

## Proposed Fix Areas

### 1. Multiple Auth Worker Threads
Scale from 1 worker to N workers (configurable via `MMORPG_AUTH_WORKERS`). Each worker independently pops from the shared queue and processes in parallel. The queue already has mutex protection. This directly multiplies throughput by N.

### 2. Bounded Queue with Backpressure
Add a max queue depth (e.g., 200). When full, immediately respond with `:auth-fail :reason :server-busy`. This prevents unbounded memory growth and gives clients fast feedback to retry later.

### 3. O(1) Queue Operations
Replace the list-based queue with a proper deque or ring buffer. Current `last`/`butlast` are O(n); a two-list queue or `sb-concurrency:queue` gives O(1) amortized push/pop.

### 4. Stale Request Expiry
Before processing a queued request, check if the client's timeout has likely elapsed (e.g., request queued >25s ago). Skip stale requests to avoid wasting worker time on clients that already gave up.

### 5. Auth Metrics
Add counters for: queue depth, processing latency, requests expired, worker utilization. Expose via logging or a metrics endpoint so load problems are visible.

### 6. Consider Reducing PBKDF2 Iterations
100,000 iterations is OWASP-recommended but expensive for a game server. Alternatives:
- Use argon2id (memory-hard, faster at equivalent security)
- Reduce iterations for game context (this isn't banking)
- Offload hashing to a dedicated auth microservice

### 7. Bound Main Loop Message Drain
Cap messages read per tick (e.g., 500) to ensure auth integration and simulation get CPU time even under flood conditions.

### 8. Move Auth Integration DB Work Off the Main Thread
`register-player-session` (called from `integrate-auth-results`) performs blocking Redis calls (`claim-session-ownership`, `db-add-online-player`, `db-update-leaderboard-*`) on the main thread. Move these storage operations into the auth worker threads or a dedicated persistence worker so auth integration on the main thread is a pure in-memory operation that cannot stall the tick.

---

## Stress Test Characteristics

`scripts/stress-test.lisp` spawns 1 client every 0.5s up to 2000 total. Each client sends a single `:register` message with **no retries**. Any response taking longer than 30s (`*auth-timeout*`) is counted as a failure. A dropped UDP packet or any server-side backlog beyond 30s shows up as an auth timeout. The test does not measure queue depth, enqueue-to-dequeue latency, or per-request processing time, so it can confirm the problem exists but cannot pinpoint which bottleneck dominates without additional instrumentation.

---

## Key File References

| File | Lines | What |
|------|-------|------|
| `src/net.lisp` | 679-757 | Auth queue (unbounded, O(n) pop) |
| `src/net.lisp` | 760-882 | `process-register-async` (worker-thread registration) |
| `src/net.lisp` | 884-983 | `process-login-async` (worker-thread login) |
| `src/net.lisp` | 1087-1135 | `auth-worker-loop` + `start-auth-worker` (single thread) |
| `src/net.lisp` | 1148-1219 | `integrate-auth-results` (main-thread, once per frame) |
| `src/net.lisp` | 2886-2970 | Main loop message receive + auth integration |
| `src/db.lisp` | 1883-1903 | PBKDF2 config (100k iterations) + `derive-password-key` |
| `src/db.lisp` | 1913-1937 | `hash-password` / `verify-password` |
| `src/db.lisp` | 2119-2150 | `db-create-account`, `db-verify-credentials`, `db-set-character-id` |
| `scripts/stress-test.lisp` | 46 | 30s auth timeout |
| `scripts/stress-test.lisp` | 178-282 | Stress test main loop (no latency metrics) |
