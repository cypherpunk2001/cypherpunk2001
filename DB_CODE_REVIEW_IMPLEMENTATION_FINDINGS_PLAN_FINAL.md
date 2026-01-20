# DB Code Review Implementation Plan (Final)

Purpose
- Address all findings in `DB_CODE_REVIEW_IMPLEMENTATION_FINDINGS.md`, including the addendum.
- No code changes in this plan; this is a sequencing guide.

Guiding principles
- Fix data-loss and ownership integrity first.
- Keep storage abstraction intact (no direct Redis calls in game logic).
- Add tests for every new behavior that is testable.

## Phase 1: Storage Failure Semantics + Retry Integrity (High)

Goals
- Make retries effective for tier-1 saves, ID counter persistence, and validated loads.
- Prevent clearing dirty flags when batch save fails.

Tasks
- Adjust storage methods to signal failures (raise conditions) or return explicit success flags.
- Update `db-save-player`, `db-save-player-immediate`, `db-save-id-counter`, and `flush-dirty-players` to check success and retry/fallback as needed.
- Ensure `db-load-player-validated` retries on storage errors (not treated as :not-found).

Tests
- Simulate storage failures and assert retries occur.
- Ensure dirty flags remain set when batch save fails.
- Verify ID counter does not advance if persistence fails.

## Phase 2: Ownership Loss Cleanup (High/Medium)

Goals
- Prevent unnecessary disconnects on transient ownership loss.
- Fully clean up net clients and avoid incorrect online-set updates.

Tasks
- Attempt `claim-session-ownership` for each lost ID before disconnecting.
- On real ownership loss, de-auth net clients, remove them from `clients`, and `session-unregister`.
- Add a local-only session cleanup path that avoids `db-remove-online-player` when ownership is already lost.

Tests
- Ownership refresh re-claims when Redis hiccups.
- Ownership loss triggers full net-client cleanup.
- Online set remains correct when ownership is transferred.

## Phase 3: Trade Safety + Memory Backend Consistency (High/Medium)

Goals
- Prevent stale servers from committing trades.
- Ensure memory backend behaves like Redis.

Tasks
- Extend `trade_complete.lua` to verify ownership keys and expected owner ID.
- Update `execute-trade-atomic` to pass ownership keys/owner ID.
- Fix memory script handler to store parsed plists (not strings).

Tests
- Trade aborts if ownership mismatch.
- Memory backend trade results load cleanly via `db-load-player`.

## Phase 4: Tier-1 Alignment for Item Destruction (High)

Goals
- Ensure drop/consume/destruction are tier-1 writes per `docs/db.md`.

Tasks
- Upgrade `consume-inventory-item` (and drop paths) to use `db-save-player-immediate` with retries; keep dirty fallback.

Tests
- Drop/consume triggers immediate save (and fallback to dirty on failure).

## Phase 5: Death Tracking + Leaderboard Correctness (Medium)

Goals
- Keep `player-deaths` and `leaderboard:deaths` consistent.

Tasks
- Increment `player-deaths` on death.
- Update deaths leaderboard using `zadd` with the total.
- Seed deaths leaderboard on login (`register-player-session`).
- Apply `:deaths` in `apply-player-plist`.

Tests
- Death increments both player data and leaderboard.
- Login seeds leaderboard with existing death count.

## Phase 6: Validation Hardening (Medium)

Goals
- Match `docs/db.md` quarantine triggers.
- Fix clamp behavior for missing keys.

Tasks
- Add validation for unknown zones/items and orphaned equipment → `:quarantine`.
- Replace `setf (getf ...)` clamp writes with `plist-put` (or prefill keys).

Tests
- Unknown zone/item/orphan equip → `:quarantine`.
- Missing `:created-at` is added in `:clamp`.

## Phase 7: Atomic Save + Index Updates (Medium)

Goals
- Align with db.md “save_with_indexes.lua” model or update docs to match reality.

Tasks (Option A: implement)
- Add `save_with_indexes.lua` (blob + leaderboard + summary in one script).
- Route tier-1 saves and batch flush through the script.
- Implement optional `player:{id}:summary` and `username:lookup` if required.

Tasks (Option B: doc alignment)
- Explicitly document that index updates are best-effort and non-atomic.

Tests
- Save updates both blob and leaderboard in one operation (Option A).

## Phase 8: Documentation Cleanup (Low)

Goals
- Ensure docs reflect actual behavior after fixes.

Tasks
- Update `docs/save.md` and `docs/db.md` for validation, tier-1 behavior, and index updates.

## Phase 9: Test & Verification

Run (order per repo policy):
- `make checkparens && make ci && make test-persistence && make test-security && make checkdocs && make smoke`

Add any new unit tests to the appropriate suite:
- `tests/persistence-test.lisp` (validation, storage, leaderboards)
- `tests/security-test.lisp` (login validation outcomes)
- `tests/unit-test.lisp` (pure functions, new helpers)
