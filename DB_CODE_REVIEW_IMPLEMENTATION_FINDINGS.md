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
