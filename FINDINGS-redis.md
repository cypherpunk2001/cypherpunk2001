Redis Connection Usage — Findings & Recommendations
===================================================

Scope
-----
Review current Redis connection usage and whether auth-style persistence should
be applied elsewhere (e.g., storage calls via db-storage).

Findings
--------
1) Auth uses persistent per-worker connections
   - Auth worker loop wraps Redis calls in `redis:with-persistent-connection`.
     This keeps one connection per auth worker thread.
   - Locations: `src/net-auth.lisp:724-729`.

2) General storage calls are per-call connections
   - `redis-storage` methods wrap each operation in
     `redis:with-recursive-connection`, which opens/uses a connection on each call.
   - Locations: `src/db-storage.lisp:408-757` (load/save/delete/etc.).

3) Pipelining exists but still inside per-call connections
   - `storage-save-batch` and account creation use pipelining to reduce round trips,
     but still open a connection for each call.
   - Locations: `src/db-storage.lisp:490-512`, `src/db-accounts.lisp:341-370`.

4) No general-purpose pool abstraction
   - There is no explicit connection pool; cl-redis uses per-thread dynamic
     connection state. Only auth keeps a connection alive per worker.

Recommendations
---------------
1) Reuse persistent connections in high-traffic threads
   - Wrap long-lived server threads that perform frequent Redis I/O with
     `redis:with-persistent-connection` so `storage-*` calls reuse that thread
     connection.
   - Candidates: main server loop (batch flush + session ownership refresh),
     any dedicated DB/worker threads, trade/leaderboard loops.
   - This mirrors the auth worker pattern without adding a pool.

2) Avoid a global pool unless you need cross-thread sharing
   - cl-redis is per-thread; a pool adds complexity and potential misuse.
   - Prefer per-thread persistent connections for simplicity and safety.

3) Keep pipelining for multi-op sequences
   - For bulk saves and multi-key operations, continue using `with-pipelining`.
   - If persistent connections are added, pipelining becomes even more effective
     (no connect overhead per batch).

4) Add light instrumentation if you expand persistence
   - Reuse existing Redis metrics in `src/db-storage.lisp` to confirm latency
     improvements and catch regressions after switching to persistent connections.

Notes
-----
- This recommendation does not change the storage abstraction; it only changes
  how/where Redis connections are established in long-lived threads.
- No code changes were made in this review.

---

Claude's Codebase Analysis (2026-02-03)
---------------------------------------

**Verification of Findings:**

All findings are accurate. I confirmed:

1. **Auth workers** (net-auth.lisp:724-729): Each of the 4 default auth worker
   threads wraps its main loop in `redis:with-persistent-connection`. This is
   the correct pattern - one long-lived connection per worker thread.

2. **Storage calls** (db-storage.lisp): There are 25+ methods using
   `redis:with-recursive-connection`, meaning each call opens/reuses a
   connection independently. High-frequency callers pay connection overhead
   repeatedly.

3. **Main server loop** (net-server.lisp:321-362): Makes Redis calls every 30s:
   - `flush-dirty-players` at line 323 (batch save via storage-save-batch)
   - `refresh-all-session-ownerships` at line 362 (TTL refresh per session)
   Neither is wrapped in a persistent connection.

4. **Pipelining** (db-storage.lisp:504-514): Already used in `storage-save-batch`
   for batch player saves. This is good but still inside per-call connections.

5. **Redis metrics** (db-storage.lisp:24-178): Full latency tracking exists with
   ring buffers for save/load latencies, error counts, and periodic logging.
   This can measure improvement after changes.

**Impact Assessment:**

Current connection pattern for a typical 30s cycle:
- `flush-dirty-players`: 1 connection for batch save (pipelined inside)
- `refresh-all-session-ownerships`: N connections for N active sessions
  (each TTL refresh is a separate storage call)

With persistent connection on main server thread:
- All 30s-cycle Redis calls would reuse one connection
- Pipelining would have zero connection overhead
- TTL refreshes could potentially be pipelined too (currently not)

**Estimated Effort:**

Low. The change is ~10 lines in net-server.lisp:
- Wrap the main loop in `redis:with-persistent-connection` (same pattern as auth workers)
- Conditionally check for redis-storage backend (same guard as auth workers)

**Risk Assessment:**

Low. The storage abstraction remains unchanged. Only the connection lifecycle
changes for the main server thread. If Redis disconnects mid-loop, cl-redis
will reconnect on next call (same behavior as auth workers).

**Additional Observations:**

1. **TTL refresh could be pipelined**: `refresh-all-session-ownerships` loops
   over sessions and calls `storage-refresh-ttl` individually. With a persistent
   connection, this could be optimized to pipeline all TTL refreshes in one
   round-trip. This is a potential follow-up optimization.

2. **Tier-1 immediate saves** (combat.lisp, progression.lisp): These are
   scattered throughout game logic and use retry wrappers. They would NOT
   benefit from the main loop's persistent connection since they run from
   different call sites. However, they're infrequent (death, level-up) so
   connection overhead is acceptable.

3. **Memory backend unaffected**: The memory-storage backend doesn't use Redis,
   so changes must be guarded with `(typep *storage* 'redis-storage)`.

**Recommendation:**

Proceed with the plan. The auth worker pattern is proven and the same approach
should work for the main server loop. Measure before/after with existing Redis
metrics to confirm latency improvement.
