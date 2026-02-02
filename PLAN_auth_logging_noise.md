Auth Logging Noise Reduction Plan
=================================

Summary of Investigation
------------------------
- The repeated `[AUTH] queued=...` lines come from periodic auth metrics logging
  in `src/net-server.lisp` (every 30s).
- Metrics are cumulative and never reset, so once any auth occurs the server
  prints the same totals every 30s forever.
- The `fail` count in your log is expected if you entered wrong usernames,
  bad passwords, or used auto-login/register retries.
- With two clients, multiple auth attempts will increase `queued/processed`
  even in casual use.

Why It Feels Wrong
------------------
- The metrics are valid but too chatty and easy to misinterpret.
- Because totals are cumulative, repeated lines look like ongoing failures.

Goal
----
Keep auth metrics available for debugging while eliminating noisy logs
for normal local play.

Plan
----
1) Add a logging gate (config + env flag)
   - New flag: `*auth-metrics-logging*` (default NIL)
   - Env override: `MMORPG_AUTH_METRICS=1`
   - Only log auth metrics when flag is enabled.

2) Log only when metrics change (delta logging)
   - Track the last printed totals (queued/processed/success/fail/etc.)
   - Compute deltas every 30s; if all deltas = 0, skip logging.
   - Optionally print both totals and deltas for clarity.

3) Make failure reasons clearer (optional improvement)
   - Include a per-reason count (bad-credentials, username-taken, etc.), or
     add a short suffix when fail delta > 0: "fail+=N (likely bad credentials)".
   - This reduces confusion when testing locally.

Files to Touch
--------------
- `src/net-server.lisp` — gate logging and compute deltas
- `src/config.lisp` — define `*auth-metrics-logging*` + env parsing
- `src/net-auth.lisp` (optional) — per-reason counters if desired

Testing
-------
- Start server with/without `MMORPG_AUTH_METRICS=1` and confirm logging behavior.
- Verify no `[AUTH]` spam when no auth activity occurs.
- Verify deltas appear only after actual login/register attempts.

Notes
-----
- The existing `fail` counts in your logs are normal when using wrong usernames.
- The “Killed” line from `make server` is likely an external termination (OS or manual).
