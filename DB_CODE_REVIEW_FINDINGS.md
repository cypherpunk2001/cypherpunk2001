# DB Code Review Findings (Phase 1-6 vs docs/db.md)

Scope
- Reviewed `docs/db.md` and implementation in `src/db.lisp`, `src/save.lisp`, `src/net.lisp`, `src/progression.lisp`, `src/combat.lisp`, `src/trade.lisp`, and relevant tests/docs.
- This report focuses on spec alignment and correctness; no code changes were made.

Severity legend
- Critical: data loss/security or core gameplay integrity broken
- High: serious correctness issues or spec violations likely to surface
- Medium: correctness gaps with limited blast radius
- Low: minor gaps, test fidelity, or documentation drift

## Critical

1) Session ownership TTL never refreshed; tier-1 saves fail after 60s
- Evidence: `refresh-session-ownership`/`refresh-all-session-ownerships` are defined in `src/db.lisp` but have no call sites; ownership keys expire after `*session-ownership-ttl-seconds*`.
- Impact: after TTL expiry, `verify-session-ownership` fails and `db-save-player-immediate` returns NIL (no retry/error), so death/level-up/trade tier-1 saves are silently dropped. This defeats the lease system and allows double-login after TTL expiration.
- Fix: call `refresh-all-session-ownerships` on a heartbeat (server tick or net loop) and/or refresh on every authenticated client update. When ownership is lost, either mark dirty + disconnect or re-claim before continuing.

## High

2) Phase 6 validation is not wired into login; unvalidated loads still parse blobs
- Evidence: login flows in `src/net.lisp` call `db-load-player` (e.g., `process-login-async` around `db-load-player` and `handle-login-request` around `db-load-player`). `db-load-player-validated` is never used.
- Impact: oversized or malformed blobs are parsed without the Phase 6 size check or 4-way validation. Quarantine/reject metrics and forensic storage never trigger; corrupted data can still load or crash parse paths.
- Fix: switch both login paths to `db-load-player-validated` and handle its action (`:ok`, `:clamp`, `:quarantine`, `:reject`). Also claim session ownership before calling it (as required by `db-load-player-validated`).

3) Tier-2 batch flush ignores session ownership
- Evidence: `flush-dirty-players` in `src/db.lisp` calls `storage-save-batch` for all sessions without `verify-session-ownership` gating.
- Impact: stale servers can overwrite data after ownership transfer or TTL expiry, negating the lease safety goals in `docs/db.md`.
- Fix: filter `to-flush` by `verify-session-ownership`, or use an ownership-checked Lua save for batch operations (e.g., a batched safe-save script), or drop sessions when ownership is lost.

4) 4-way validator can throw on malformed nested structures
- Evidence: `validate-player-plist-4way` in `src/save.lisp` calls `(getf inventory :slots)` and `(getf stats stat-key)` without guarding that `inventory`/`stats` are plists.
- Impact: corrupted blobs with non-list `:inventory`/`:stats` can raise a type error, bypassing the intended `:reject`/`:quarantine` handling and potentially crashing login.
- Fix: add `listp` checks before `getf` on nested structures and treat wrong types as `:reject` (or `:quarantine`) with an issue message.

## Medium

5) Leaderboards/deaths tracking not wired to gameplay updates
- Evidence: `award-combat-xp` in `src/progression.lisp` updates XP/level but never calls `db-update-leaderboard-xp` or `db-update-leaderboard-level`. `combatant-apply-hit` in `src/combat.lisp` never increments `player-deaths` or calls `db-update-leaderboard-deaths`. Leaderboards are only updated on login in `register-player-session`.
- Impact: leaderboard data is stale or wrong; `player-deaths` remains 0 for active players, diverging from Phase 4 spec.
- Fix: update leaderboards on XP gain (batched), on level-up (tier-1), and on death (tier-1). Increment `player-deaths` when HP reaches 0 before saving.

6) Quarantine path not handled in login flow
- Evidence: `db-load-player-validated` can return `:quarantine` and `make-quarantined-player`, but login handlers treat any loaded player as success and add them to the world (no special-case in `src/net.lisp`).
- Impact: once Phase 6 load is wired, quarantined accounts will still spawn into the world instead of being blocked for repair.
- Fix: handle `:quarantine` in login and return an auth failure or a dedicated repair flow (do not add player to game).

7) Redis script execution lacks NOSCRIPT fallback
- Evidence: `storage-eval-script` in `src/db.lisp` logs NOSCRIPT and returns NIL instead of reloading or evaluating the script body.
- Impact: if Redis script cache is flushed or Redis restarts, trade/atomic ops fail until server restart.
- Fix: on NOSCRIPT, reload the script body (keep it cached or read from file) and retry via `EVAL` or re-`SCRIPT LOAD` + `EVALSHA`.

8) Trade atomic save bypasses version tagging
- Evidence: `execute-trade-atomic` in `src/trade.lisp` serializes players via `serialize-player` but never adds `:version` before writing to Redis.
- Impact: player blobs written by trades are versionless, forcing loads to treat them as v0 and re-run migrations unnecessarily; this also diverges from the “all records include :version” invariant in `docs/db.md`.
- Fix: add `:version` to the serialized payloads before passing them to `trade_complete`.

## Low

9) Memory backend TTL semantics do not match Redis
- Evidence: `storage-save-with-ttl` sets expiration, but `storage-load` and `storage-keys` in `src/db.lisp` ignore TTL; only `storage-load-raw` checks expiry.
- Impact: tests using the memory backend do not reflect TTL behavior (corrupt blob retention and session ownership expiration).
- Fix: apply TTL cleanup in `storage-load`, `storage-keys`, and `storage-exists-p`, or add a periodic cleanup sweep.

10) docs/save.md still describes reject-only validation
- Evidence: `docs/save.md` states “Schema validation rejects invalid data rather than clamping,” but `validate-player-plist-4way` now clamps and quarantines.
- Impact: documentation contradicts current behavior and Phase 6 spec.
- Fix: update `docs/save.md` to reflect 4-way outcomes and clamp behavior.

