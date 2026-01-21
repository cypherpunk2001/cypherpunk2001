# MAKE_STRESS_FINDINGS_CODEX.md

## Scope
- Diagnose the two stress-test symptoms (multi-zone spawns, client warping) without changing code.

## Symptoms reported
- NPCs/players appear in every zone instead of only spawning in Zone-1.
- Client view "warps" between zones when `make stress` runs.

## Findings (likely root causes)
1) Global world zone is mutated by any player transition, so new spawns inherit the last active zone.
- Evidence: `src/movement.lisp:862-967` updates global `world` and `*zone-path*` during `transition-zone`, and `src/server.lisp:4-15` spawns new players using the current `world-zone`.
- Stress clients move randomly (`scripts/stress-test.lisp:47-98`), so frequent transitions flip the global world zone.
- Result: new registrations/logins can spawn in whatever zone was last loaded (not necessarily Zone-1), and each first-time zone visit spawns that zone's NPCs (so NPCs appear "everywhere").

2) Delta snapshots are not zone-filtered; they use the global world zone and all dirty entities.
- Evidence: `src/net.lisp:1217-1261` groups clients by zone but sends delta snapshots from `serialize-game-state-delta` without per-zone filtering. `src/save.lisp:1187-1221` builds delta snapshots from `game-players`, `game-npcs`, and `world-zone` (no zone filtering).
- Clients apply snapshots with zone switching enabled (`src/net.lisp:1350-1362`, `src/save.lisp:1440-1468`), so a delta snapshot with a different `:zone-id` forces a zone load.
- Result: clients in other zones receive deltas stamped with the server's current world zone, causing visible warping and cross-zone player/NPC leakage.

3) Login/register zone-id is taken from the global world zone, not the player's stored zone.
- Evidence: `src/net.lisp:539-719` captures `zone-id` from `world-zone` once and returns it in the auth result; `src/net.lisp:800-828` registers the session using that zone-id. `src/db.lisp:1273-1284` persists zone-id from the session.
- Result: existing characters can be persisted into the wrong zone if the global world zone changed, compounding the "spawn in random zones" symptom over time.

## Why this matches the reported symptoms
- NPCs/players in every zone: random stress-walkers trigger `transition-zone`, which loads multiple zones and spawns NPCs; subsequent spawns inherit the last active zone instead of Zone-1.
- Client warping: delta snapshots broadcast the current global `:zone-id` to all zone groups; clients eagerly load that zone on apply, so their view bounces as the active world zone changes.

## Secondary risks to keep in mind
- If a player logs in to a zone that is not loaded into `*zone-states*`, the full snapshot fallback uses `serialize-game-state-compact` (global, not zone-filtered) in `src/net.lisp:1241-1244`, which can also leak cross-zone entities.
- The documented multi-zone caveat notes collision uses the active world zone (`docs/movement.md`), so players in non-active zones may have incorrect collision/edge logic, potentially amplifying transitions and zone churn.
