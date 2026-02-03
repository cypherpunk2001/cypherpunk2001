Redis Connection Strategy Plan
==============================

Goal
----
Reduce Redis connection churn by reusing per-thread persistent connections in
high-traffic server paths while keeping the existing storage abstraction intact.

Decision
--------
- **Yes, do this** for high-frequency Redis paths.
- **No global pool** for now; cl-redis is per-thread and a pool adds complexity.

Scope
-----
- Apply only to long-lived threads that perform frequent Redis I/O.
- Keep low-frequency/admin paths unchanged.

Plan
----
1) Identify hot Redis call sites (per-thread loops)
   - Server main loop: periodic batch flush + session ownership refresh
   - Any dedicated DB/worker threads (if present)
   - Trade/leaderboard loops if they run continuously

2) Wrap hot threads with persistent connections
   - Use `redis:with-persistent-connection` around the thread loop.
   - Ensure all `storage-*` calls inside the loop reuse that thread connection.

3) Keep pipelining for multi-op sequences
   - Continue `with-pipelining` in `storage-save-batch` and account creation.

4) Validate with existing Redis metrics
   - Use `*redis-metrics*` and latency logs to confirm improvement.
   - Verify no regressions in save/load error counts.

Out of Scope (for now)
----------------------
- Global connection pool
- Refactoring storage abstraction
- Non-Redis backends

Testing
-------
- Run `make tests`.
- Manual: start server with Redis, perform repeated logins + save/load loop,
  verify no reconnect spam and stable latency metrics.

Done Criteria
-------------
- Persistent connections are active in identified hot threads.
- Redis metrics show reduced p99 load/save latency or fewer spikes.
- No functional regressions in auth, save/load, or session ownership refresh.
