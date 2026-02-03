Redis Persistent Connection Plan (Consolidated)
================================================

*Consolidated from PLAN-redis.md (strategy) and PLAN-redis-claude.md (implementation)*

Goal
----
Reduce Redis connection churn by reusing per-thread persistent connections in
high-traffic server paths while keeping the existing storage abstraction intact.

Decision
--------
- **Yes, do this** for high-frequency Redis paths.
- **No global pool** — cl-redis is per-thread and a pool adds complexity.
- **Follow auth worker pattern** — proven approach already in codebase.

Background
----------
Currently, the main server thread makes Redis calls every 30 seconds:
- `flush-dirty-players`: Saves dirty player data via `storage-save-batch`
- `refresh-all-session-ownerships`: Verifies ownership and refreshes TTL on
  all active session keys

Each storage call uses `redis:with-recursive-connection`, which establishes
a connection per call. For N active sessions, this means roughly ~2N+1
connections every 30 seconds (1 for batch save, N ownership checks, N TTL
refreshes), plus extra calls when re-claim attempts occur.

Auth workers already solve this by wrapping their loop in
`redis:with-persistent-connection` (net-auth.lisp:724-729).

---

Implementation
--------------

### Step 1: Wrap main server loop with persistent connection

**File:** `src/net-server.lisp`

**Location:** Around line 186 (before the main `loop` form)

**Pattern:** Same as auth workers (net-auth.lisp:724-729)

```lisp
;; Define server-main-loop as a local function containing the existing loop
(flet ((server-main-loop ()
         (loop :with elapsed = 0.0
               ...existing loop body...)))
  ;; Wrap in persistent Redis connection if using Redis backend
  (if (and *storage* (typep *storage* 'redis-storage))
      (redis:with-persistent-connection (:host (redis-storage-host *storage*)
                                         :port (redis-storage-port *storage*))
        (server-main-loop))
      (server-main-loop)))
```

**Why this works:**
- `redis:with-persistent-connection` keeps one TCP connection open **per thread**
- All nested `redis:with-recursive-connection` calls in the *same thread* reuse it
- If the connection drops, cl-redis reconnects automatically on next call
- Memory backend (tests/dev) skips the wrapper entirely

### Step 2: Keep pipelining for multi-op sequences

**No changes needed** — continue using `with-pipelining` in:
- `storage-save-batch` (db-storage.lisp:504-514)
- Account creation (db-accounts.lisp:351-370)

With persistent connections, pipelining becomes more effective (no connection
overhead per batch).

### Step 3: Validate with existing Redis metrics

**File:** `src/db-storage.lisp` (no changes needed)

Use existing metrics infrastructure:
- `redis-save-latency-p99` and `redis-load-latency-p99`
- `redis-metrics-total-saves` and `redis-metrics-total-loads`
- Enable verbose: `MMORPG_VERBOSE=1 make server`

**Expected improvement:**
- Reduced p99 latency for batch saves
- Reduced p99 latency for TTL refreshes
- No change in error rates

### Step 4: Pipeline TTL refreshes

**File:** `src/db-players.lisp` (via storage abstraction)

Currently each session's TTL refresh is a separate call. Add a **storage-level**
batch TTL refresh so we do not violate the storage abstraction.

```lisp
;; Add storage-refresh-ttl-batch in db-storage.lisp (Redis impl uses pipelining)
;; Then call it from refresh-all-session-ownerships in db-players.lisp
(storage-refresh-ttl-batch *storage* owner-keys *session-ownership-ttl-seconds*)
```

Do this after Step 1; it reduces per-session round trips without changing
game logic.

---

Files Modified
--------------
| File | Change |
|------|--------|
| `src/net-server.lisp` | Wrap main loop in persistent connection |
| `docs/net-server.md` | Document connection strategy (optional) |
| `src/db-storage.lisp` | Add `storage-refresh-ttl-batch` |
| `src/db-players.lisp` | Use batch TTL refresh |

---

Scope
-----
**In scope:**
- Main server loop (batch flush + session ownership refresh)
- Auth workers (already done, verify no regression)

**Out of scope:**
- Global connection pool
- Refactoring storage abstraction
- Non-Redis backends (memory backend unaffected)
- Low-frequency/admin paths (acceptable overhead)
- Trade/leaderboard loops (not continuous in current impl)

---

Testing
-------
1. `make tests` — All existing tests must pass (runs checkparens → ci → smoke → unit → docs)
2. Manual test with Redis:
   ```bash
   redis-server &
   MMORPG_VERBOSE=1 make server
   # In another terminal:
   make client
   # Play for 2+ minutes, perform logins
   # Check server logs for Redis latency metrics
   ```
3. Compare p99 latencies before/after change

---

Rollback
--------
If issues arise, remove the `redis:with-persistent-connection` wrapper.
Storage abstraction unchanged, fallback is clean.

---

Done Criteria
-------------
- [ ] Main server loop uses persistent Redis connection
- [ ] Auth workers continue using persistent connections (no regression)
- [ ] TTL refresh uses batch storage call (no per-session round trips)
- [ ] All tests pass (`make tests`)
- [ ] Redis metrics show improvement (or no regression) in p99 latency
- [ ] No increase in Redis connection errors during extended runs
- [ ] No functional regressions in auth, save/load, or session ownership

---

Risk Assessment
---------------
**Low risk:**
- Pattern is proven (auth workers use it successfully)
- Storage abstraction unchanged
- Automatic reconnect on connection drop
- Easy rollback (remove wrapper)

**Estimated effort:** ~10 lines of code change in net-server.lisp
