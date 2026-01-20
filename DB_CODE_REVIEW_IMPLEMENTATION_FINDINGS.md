# DB Code Review Implementation Findings (Plan vs Commits)

Scope
- Reviewed commits: `5fb5d40` (Phase A), `d04a123` (Phase B), `a2856e5` (Phase C),
  `06fe080` (Phase D), `1f54d2c` (Phase E), `2097718` (Phase F), `569dc00` (Phase G).
- Compared implementation against `DB_CODE_REVIEW_PLAN.md` and the Phase 6 spec in `docs/db.md`.
- No code changes were made; this is a findings-only review.

Severity legend
- Critical: data loss/security or core gameplay integrity broken
- High: serious correctness issues likely to surface
- Medium: correctness gaps with limited blast radius
- Low: minor gaps, test fidelity, or documentation drift

## High

1) Phase A: Ownership loss cleanup skips re-claim
- Evidence: `refresh-all-session-ownerships` in `src/db.lisp` only verifies ownership and reports failures; the server loop in `src/net.lisp` immediately disconnects any failed IDs.
- Impact: a transient Redis hiccup or a missed TTL refresh (with no other owner) forces disconnects and drops tier-1 saves, even though the server could reclaim ownership and continue.
- Proposed fix: for each failed ID, attempt `claim-session-ownership` before disconnect; if it succeeds, refresh TTL and keep the session, only disconnect on a failed claim.

## Medium

2) Phase A/D: Ownership-loss cleanup does not de-auth/remove net clients
- Evidence: Phase A cleanup in `src/net.lisp` removes sessions and players but does not clear `net-client-authenticated-p`, `net-client-player`, or remove the client from the `clients` list. Phase D cleanup in `src/db.lisp` calls `unregister-player-session`, which also does not touch `*active-sessions*` or net clients.
- Impact: stale authenticated clients can remain in the server loop, receive snapshots without a player entity, and continue sending intents against a player object no longer in the game. In Phase D, stale `*active-sessions*` can also block relogin.
- Proposed fix: locate the net client (via `session-get` before unregister), clear auth fields, and remove it from `clients` (or reuse `check-client-timeouts` style cleanup). Ensure `session-unregister` is called when dropping a session due to ownership loss.

3) Phase D: Lost-ownership cleanup removes players from global online set
- Evidence: `flush-dirty-players` calls `unregister-player-session` for lost ownership, and `unregister-player-session` always calls `db-remove-online-player` in `src/db.lisp`.
- Impact: if another server already owns the session, this removal drops the player from the global online set even though they are still online.
- Proposed fix: add a "local-only" cleanup path that removes only from `*player-sessions*` (no `db-remove-online-player`), or gate online-set removal on ownership verification.

4) Phase E: Death leaderboard updates use stale death count
- Evidence: `src/combat.lisp` now calls `db-update-leaderboard-deaths` on death but never increments `(player-deaths player)`.
- Impact: `:deaths` remains unchanged (often 0), so the leaderboard and persisted deaths diverge from actual gameplay.
- Proposed fix: `(incf (player-deaths player))` before the leaderboard update and ensure the death save path persists the new value.

## Low

5) Phase G: `docs/save.md` describes validation APIs and rules that do not exist
- Evidence: `docs/save.md` references `validate-and-clamp-player-data`, `db-quarantine-player`, "> 5 clamped fields", repeated-failure quarantine, and lifetime-xp clamping/zone-bound clamping; none of these are implemented in `src/save.lisp`/`src/db.lisp`.
- Impact: documentation diverges from actual 4-way validation behavior, creating confusion for maintenance and future tests.
- Proposed fix: update `docs/save.md` to reflect `validate-player-plist-4way` as implemented (current clamp fields, quarantine triggers, and available APIs).

## Test Gaps (per plan and repo policy)

The plan requires tests for each phase; only Phase B added tests.

- Phase A: ownership refresh + ownership-loss cleanup tests missing.
- Phase C: login validation outcomes + rollback tests missing.
- Phase D: batch flush ownership filter + cleanup tests missing.
- Phase E: leaderboard updates on XP/level/death tests missing.
- Phase F: NOSCRIPT recovery + trade version tagging tests missing.
- Phase G: memory TTL semantics tests missing.

---

## Addendum: Holistic Review (Current `src/` vs `docs/db.md`)

Scope
- Reviewed `src/` holistically against `docs/db.md`, `DB_CODE_REVIEW_FINDINGS.md`,
  and `DB_CODE_REVIEW_PLAN.md` (assumed fully implemented).
- Findings below are additional deviations not covered in the phase-by-phase review.

## High

A1) Retry logic is ineffective for storage failures; tier-1 and batch writes can silently drop
- Evidence: `storage-save`/`storage-save-batch` in `src/db.lisp` swallow Redis errors and return NIL/0; `db-save-player` returns T regardless; `with-retry-exponential` only retries on errors, so tier-1 saves (combat/progression/movement) and ID counter saves (`src/types.lisp`) do not retry on failure. `flush-dirty-players` clears dirty flags even when `storage-save-batch` returns 0.
- Impact: transient Redis failures can silently lose tier-1 saves (death, level-up, equipment), ID counter persistence, and tier-2 batch saves, violating durability guarantees.
- Proposed fix: propagate storage errors (signal conditions) or return explicit success flags that callers must check; only clear dirty flags on confirmed success; treat batch failure as retryable; ensure ID counter save failures prevent increment.

A2) Trades bypass session ownership checks, allowing stale servers to overwrite data
- Evidence: `execute-trade-atomic` calls `storage-eval-script` with only player keys; `data/redis-scripts/trade_complete.lua` does not check `session:{id}:owner`.
- Impact: if a server loses ownership, it can still commit a trade and overwrite the authoritative server’s data, violating the lease safety model.
- Proposed fix: add ownership keys + expected owner ID to the Lua script and abort if either ownership key does not match; also verify ownership before calling the script.

A3) Item destruction is still tier-2 (batched), not tier-1 (immediate)
- Evidence: `consume-inventory-item` only calls `mark-player-dirty` (tier-2) in `src/progression.lisp`; drop/consume paths rely on it.
- Impact: crashes or disconnects within the batch window can restore “destroyed” items and create dupes; this violates the tier-1 list in `docs/db.md` (drop/sell/consume).
- Proposed fix: use `db-save-player-immediate` with retry (and dirty fallback) for item destruction paths; keep pickups as tier-2.

## Medium

A4) 4-way validation misses quarantine checks required by `docs/db.md`
- Evidence: `validate-player-plist-4way` only checks that `:zone-id` is a symbol and `:item-id` is a symbol; it does not quarantine unknown zones/items or orphaned equipment.
- Impact: corrupted data from removed zones/items can load and create undefined behavior instead of quarantining.
- Proposed fix: validate zone existence (via world graph/zone registry), item existence (`find-item-archetype`), and equipment↔inventory consistency; return `:quarantine` with issue list.

A5) Clamp fixes can be no-ops when keys are missing (plist `setf getf` pitfall)
- Evidence: `validate-player-plist-4way` uses `(setf (getf fixed-plist :created-at) ...)` and similar for clamp fixes; if the key is missing in a current-version blob, the setf does not add it.
- Impact: clamped results may still be missing required fields, defeating the “fixed-plist” contract.
- Proposed fix: use `plist-put` (or ensure keys exist before clamp) for all clamp writes.

A6) Memory backend trade handler stores strings, not plists
- Evidence: memory script handler in `src/db.lisp` writes `data1`/`data2` (strings) into `memory-storage-data`; `storage-load` expects plists.
- Impact: after a trade in memory backend, `db-load-player` will read a string and fail migrations/deserialization.
- Proposed fix: parse `data` with `read-from-string` (with `*read-eval* nil`) before storing, or pass plist data to the memory script handler.

A7) Death leaderboard uses increments only and is not seeded from persisted counts
- Evidence: `db-update-leaderboard-deaths` uses `storage-zincrby` and is only called on deaths; `register-player-session` never seeds `leaderboard:deaths` from `player-deaths`.
- Impact: after restart or migration, players with existing deaths can appear with incorrect leaderboard totals (increments start from 0).
- Proposed fix: change deaths leaderboard updates to `zadd` with current `player-deaths`, and seed on login.

A8) Index updates are not atomic with blob saves (spec mismatch)
- Evidence: `docs/db.md` calls for a `save_with_indexes.lua` pattern (blob + leaderboard/summary updates in one transaction); code updates leaderboards separately or not at all.
- Impact: a crash between blob save and index updates yields inconsistent leaderboards/summary.
- Proposed fix: add a Lua script for atomic blob + index updates and route tier-1/tier-2 saves through it, or update docs to match non-atomic behavior.

## Low

A9) Client-side snapshots never apply `:deaths`
- Evidence: `apply-player-plist` in `src/save.lisp` updates lifetime-xp/playtime but never sets `player-deaths`.
- Impact: client state can drift if deaths are displayed or used in UI.
- Proposed fix: apply `:deaths` when present in snapshot plist.

A10) XP leaderboard updates are not batched as described
- Evidence: `award-combat-xp` calls `db-update-leaderboard-xp` on every XP gain; `docs/db.md` suggests batched updates.
- Impact: extra Redis writes in hot combat loops.
- Proposed fix: update XP leaderboard during batch flush (tier-2) or throttle updates.
