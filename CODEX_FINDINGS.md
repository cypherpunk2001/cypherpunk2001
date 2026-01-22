# CODEX Findings — Stress Test FPS Drop (2026-01-22)

Observed (user report)
- 300–400 players: stable 60 FPS
- 500 players: ~55–60 FPS
- 700 players: ~40–50 FPS
- 900 players: ~35–45 FPS
- Ping stable (17–19ms localhost) → network not the limiter; client frame time is.

Scope
- No code changes made. This document is a codebase deep‑dive diagnosis of likely FPS bottlenecks.
- File references are to current code in `src/`.

## Executive Summary
The current client frame time scales roughly linearly with the number of entities in the zone because:
1) **Rendering draws every entity every frame with no camera culling.**
2) **Delta apply is O(N²) on players (linear search per player).**
3) **Interpolation and snapshot buffering allocate per-entity objects every snapshot/frame.**

These three together explain why FPS collapses as player count rises, even with low latency.

## Primary Bottlenecks (Highest Impact)

### 1) No entity culling in render loop (draws ALL entities)
Evidence
- `src/rendering.lisp:1568-1594` — `draw-game` loops across `entities` and calls `draw-entity` for every entry.
- `src/rendering.lisp:763-805` (player) / `:807-811` (npc) — draw path has no camera bounds test; always draws sprite + health bar + hit effect.

Impact
- O(N) draw calls per frame, regardless of how many entities are actually visible in the camera.
- With 900–2000 players in one zone, this alone can push frame time above budget.

### 2) Delta apply uses linear search per player (O(N²) per snapshot)
Evidence
- `src/save.lisp:1338-1356` — for each player in `changed-players`, calls `find-player-by-id`.
- `src/types.lisp:491-496` — `find-player-by-id` is a linear scan.

Impact
- If most players are moving every tick, delta includes most players.
- For N players, this becomes ~N² comparisons per snapshot.
- At 900 players, that’s ~810k comparisons *per snapshot* on the client.

### 3) Interpolation snapshot path allocates per-entity objects every snapshot
Evidence
- `src/net.lisp:1474-1520` — `capture-entity-positions` allocates a new hash table per snapshot and stores `(list x y)` per entity.
- `src/net.lisp:1550-1584` — `interpolate-remote-entities` walks all players and NPCs each frame.

Impact
- O(N) per frame for interpolation + heavy allocation pressure.
- GC pressure scales with entity count → FPS drops even when GPU load is reasonable.

## Secondary Bottlenecks / Scaling Risks

### 4) Compact decode allocates per-entity plists each snapshot
Evidence
- `src/save.lisp:1001-1040` — `deserialize-player-compact` constructs a plist.
- `src/save.lisp:1067-1110` — `deserialize-npc-compact` constructs a plist.
- `src/save.lisp:1338-1360` — delta apply runs this per entity per snapshot.

Impact
- For 1000+ players, this creates thousands of cons cells every snapshot.
- Likely contributes to GC spikes and FPS instability.

### 5) Server-side simulation scales superlinearly (server perf risk at 2k)
Evidence
- `src/main.lisp:468-506` — melee hit checks: nested loop players × NPCs per zone.
- `src/ai.lisp:4-17` — `closest-player` is O(P) per NPC; called for each NPC.

Impact
- For high player counts, server tick time may become a bottleneck (even if client FPS is the reported issue).

## Why FPS Drops but Ping Stays Low
- Network is fine: snapshot delivery is local, RTT stable.
- The client main loop is doing too much work per frame (render + interpolation + snapshot apply), so frame time grows while ping remains low.

## Diagnostic Hypothesis (Most Likely Root Cause)
1) Rendering all entities every frame (no culling)
2) O(N²) delta apply on players
3) Per-frame interpolation allocations and hash table usage

Any one of these can drop FPS at 900–2000 players; combined, they match the observed drop.

## Suggested Measurements (No Code Changes)
- Use SBCL profiler (`sb-sprof`) around the client loop to confirm time spent in:
  - `draw-game`
  - `deserialize-game-state-delta`
  - `capture-entity-positions` / `interpolate-remote-entities`
- Run with the same stress count but disable rendering (headless) to isolate render cost.

## Path to 2000 Players (High-Level Targets)
- **Render culling**: Only draw entities within camera bounds.
- **O(1) player lookup**: Use an ID→index map on client to apply deltas in O(N).
- **Reduce allocations**: Avoid per-entity plist creation in hot paths; use structs or reusable buffers.
- **Interpolation optimization**: Store positions in arrays rather than hash tables + lists.

These changes are structural, not tweaks. They’re the likely key to moving from ~400 to ~2000 players.

---

If you want, I can prioritize a profiling plan or a staged optimization plan next.
