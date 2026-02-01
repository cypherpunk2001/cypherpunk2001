# Auth Throughput Investigation (2026-02-01)

## Summary
- It is plausible for auth to time out or drop under high load. The current design has a single auth worker, O(n) queue operations, main-thread DB work during auth integration, and no backpressure, so backlog growth can quickly exceed the stress test's 30s timeout.

## Evidence And Likely Bottlenecks
- `src/net.lisp` auth queue is a list; `auth-queue-pop-blocking` uses `last` and `butlast`, making dequeue O(n) and allocating a new list each pop. Under backlog, this becomes O(n^2) work plus GC pressure.
- `src/net.lisp` starts exactly one auth worker (`start-auth-worker` called once during server startup). There is no configuration for multiple auth workers, so DB-bound auth work is serialized.
- `src/net.lisp` integrates auth results on the main loop via `integrate-auth-results`. If the main loop is overloaded (1000+ players), auth results and responses are delayed even if the worker finished them.
- `src/net.lisp` `integrate-auth-results` calls `register-player-session`, which in `src/db.lisp` performs storage calls (`claim-session-ownership`, `db-add-online-player`, `db-update-leaderboard-*`). These are blocking DB operations on the main thread and can stall the tick under load.
- `src/net.lisp` enqueues auth requests without any queue length limit or backpressure. With sustained load, the queue can grow without bound and older requests can exceed client timeouts.

## Stress Test Factors
- `scripts/stress-test.lisp` sends a single auth attempt per client, no retries, and treats any response taking longer than 30s as a failure. Any server-side backlog or dropped UDP packet shows as an auth timeout.

## Suggested Follow-Ups (No Code Changes Applied)
- Replace the auth queue list with an O(1) FIFO (ring buffer or two-list queue) to remove O(n^2) dequeue and GC churn.
- Add a configurable pool of auth workers and a clean stop mechanism for N workers.
- Move storage calls in `register-player-session` off the main loop (either into auth workers or a persistence worker) so auth integration cannot block the tick.
- Add metrics: auth queue length, enqueue-to-dequeue latency, auth processing time, and auth response latency.
- Add backpressure: reject or defer auth when queue length or latency exceeds a threshold (respond with `:server-busy`), and consider auth-priority handling to keep login usable during heavy snapshot/intent traffic.
