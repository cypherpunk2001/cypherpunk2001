Redis Persistent Connection Plan (Claude)
=========================================

Goal
----
Reduce Redis connection overhead in the main server loop by reusing a single
persistent connection for all periodic Redis operations (batch flush, session
ownership refresh).

Background
----------
Currently, the main server thread makes Redis calls every 30 seconds:
- `flush-dirty-players`: Saves dirty player data via `storage-save-batch`
- `refresh-all-session-ownerships`: Refreshes TTL on all active session keys

Each storage call uses `redis:with-recursive-connection`, which establishes
a connection per call. For N active sessions, this means N+1 connections
every 30 seconds (1 for batch save, N for TTL refreshes).

Auth workers already solve this problem by wrapping their main loop in
`redis:with-persistent-connection` (net-auth.lisp:724-729). The main server
loop should follow the same pattern.

Implementation
--------------

### Step 1: Wrap main server loop with persistent connection

**File:** `src/net-server.lisp`

**Location:** Around line 186 (before the main `loop` form)

**Change:** Wrap the main loop in a conditional persistent connection, same
pattern as auth workers:

```lisp
;; Before the main loop, define a helper:
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
- `redis:with-persistent-connection` keeps one TCP connection open for the
  duration of the form
- All nested `redis:with-recursive-connection` calls reuse this connection
  instead of opening new ones
- If the connection drops, cl-redis reconnects automatically on next call
- Memory backend (no Redis) skips the wrapper entirely

### Step 2: Verify metrics before/after

**File:** `src/db-storage.lisp` (no changes needed)

Use existing Redis metrics to measure improvement:
- `redis-save-latency-p99` and `redis-load-latency-p99` for latency
- `redis-metrics-total-saves` and `redis-metrics-total-loads` for throughput
- Enable verbose mode: `MMORPG_VERBOSE=1 make server`

**Expected improvement:**
- Reduced p99 latency for batch saves (no connection overhead)
- Reduced p99 latency for TTL refreshes (connection reuse)
- No change in error rates (same retry logic applies)

### Step 3: (Optional) Pipeline TTL refreshes

**File:** `src/db-players.lisp`

**Location:** `refresh-all-session-ownerships` function (~line 452)

Currently, each session's TTL refresh is a separate `storage-refresh-ttl` call.
With a persistent connection in place, these could be pipelined:

```lisp
;; Inside refresh-all-session-ownerships, after collecting owner-keys:
(redis:with-pipelining
  (dolist (owner-key owner-keys)
    (red:expire owner-key *session-ownership-ttl-seconds*)))
```

This would reduce N round-trips to 1 for N active sessions. However, this
requires modifying the storage abstraction or adding a new batch method.
Defer to a follow-up if latency is still a concern after Step 1.

Files Modified
--------------
| File | Change |
|------|--------|
| `src/net-server.lisp` | Wrap main loop in persistent connection |
| `docs/net-server.md` | Document connection strategy |

Testing
-------
1. `make tests` - All existing tests must pass
2. `make ci` - Cold compile + smoke test
3. Manual test with Redis:
   - Start Redis: `redis-server`
   - Start server: `MMORPG_VERBOSE=1 make server`
   - Connect client, perform logins, play for 2+ minutes
   - Check logs for Redis latency metrics
   - Compare p99 latencies before/after change

Rollback
--------
If issues arise, simply remove the `redis:with-persistent-connection` wrapper.
The storage abstraction remains unchanged, so fallback is clean.

Success Criteria
----------------
- [ ] Main server loop uses persistent Redis connection
- [ ] Auth workers continue using persistent connections (no regression)
- [ ] All tests pass
- [ ] Redis latency metrics show improvement (or no regression)
- [ ] No increase in Redis connection errors during extended runs

Out of Scope
------------
- Global connection pool (unnecessary complexity for per-thread model)
- Refactoring storage abstraction (keep it simple)
- Pipelining TTL refreshes (follow-up optimization)
- Client-side Redis usage (client doesn't use Redis directly)
