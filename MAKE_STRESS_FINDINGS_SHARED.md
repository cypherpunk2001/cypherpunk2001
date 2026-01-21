# MAKE_STRESS_FINDINGS_SHARED.md

## Scope
Shared diagnosis combining CODEX + CLAUDE stress-test findings to explain:
1) NPC/players appearing in every zone.
2) Client warping between zones/players under `make stress`.

## Common ground (high confidence)
- Delta snapshots are NOT zone-filtered, even though clients are grouped by zone.
  - `src/net.lisp:1217-1261` groups by zone but sends deltas from `serialize-game-state-delta`.
  - `src/save.lisp:1187-1221` collects dirty players/NPCs from the global arrays with no zone filter.
- Clients render all entities in `game-entities` without any zone filter.
  - `src/rendering.lisp:1592-1593` draws everything in `game-entities`.
- This combination allows cross-zone data to leak into every client once any cross-zone entity becomes dirty.

## Differences and reconciliation
- CODEX emphasis: the global world zone is mutated by any player transition, so new spawns and session zone-id can drift to the last active zone.
  - `src/movement.lisp:862-967` mutates global `world` and `*zone-path*` on transition.
  - `src/server.lisp:4-15` spawns new players using the current `world-zone`.
  - `src/net.lisp:539-719` uses `world-zone` when registering/logging in, and `src/db.lisp:1273-1284` persists that session zone-id.
- CLAUDE emphasis: camera warping may happen because client arrays become polluted and the local player reference can shift.
  - This is plausible when local ID is absent or unstable, but the client does keep a stable `game-net-player-id` after auth. So I treat this as a secondary contributor, not the primary cause.
- Reconciliation: the warping can be explained without relying on local player pointer swaps:
  - Delta snapshots carry the global `:zone-id` (from the current `world-zone`), which can change due to other players transitioning.
  - `apply-game-state` loads the zone whenever `:zone-id` changes (`src/save.lisp:1440-1468`).
  - Combined with unfiltered deltas, the client sees players/NPCs from other zones and the map flips to their zone, which feels like warping.

## Final diagnosis (GO)
The stress-test symptoms are caused by two interacting architectural leaks:

1) **Global world-zone mutation leaks into spawn/session state.**
   - Zone transitions by any player mutate the global `world` and `*zone-path*`.
   - New spawns and session zone-id are derived from that global world, so stress logins can appear in whatever zone was most recently loaded.
   - This explains NPC/players spawning outside Zone-1 and the "every zone" population growth.

2) **Delta snapshots are global, not per-zone, and carry the global zone-id.**
   - Clients are grouped by zone, but deltas include dirty entities from ALL zones and stamp the snapshot with the current global `world-zone`.
   - Clients apply deltas with zone switching enabled; when the global zone flips, clients in other zones load that zone and render its entities.
   - Rendering is not zone-filtered, so cross-zone players/NPCs are visible even if the client never truly transitioned.
   - This explains the repeated "warping" and cross-zone visibility under heavy stress.

In short: multi-zone support is partially implemented (full snapshots are filtered), but the global world state and delta path are still single-zone in practice. Under stress, that mismatch manifests as cross-zone spawns, entity leakage, and client zone thrash.
