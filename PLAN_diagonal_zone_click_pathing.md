Diagonal Zone Click Pathing Plan
================================

Problem
-------
Click-to-move across diagonal zones (e.g., zone 1 → zone 4 or zone 2 → zone 3)
selects a single cardinal edge and enters an orthogonal neighbor, never reaching
 the clicked tile. Current transition logic only supports one edge hop.

Goal
----
When a player clicks a tile in a diagonally adjacent zone, the client should
navigate across multiple zone boundaries and eventually arrive at the clicked
destination tile.

Root Cause Summary
------------------
- Transition system is edge-based (cardinal only).
- Directional gating selects one edge (dominant axis), not a route.
- Target rebasing is single-hop only; no multi-zone destination state.

Plan
----

Phase 1: Encode Multi-Zone Destination
--------------------------------------
1) Determine destination zone-id for the click target:
   - Use preview zones (movement-preview.lisp) or world-graph to identify
     which zone contains the raw click (screen→world).
2) Compute a zone path from current zone → destination zone:
   - Use world-graph shortest path (BFS over edges).
3) Store a client-only “zone path” with the click target:
   - Add to intent or separate client-only field on player/game state.
   - Ensure it is not persisted and not sent to other clients.

Phase 2: Multi-Hop Rebase on Transition
---------------------------------------
1) On each zone transition (client-side):
   - Pop the next zone from the stored path.
   - Identify the edge to cross (from current zone to next zone).
   - Rebase target across that edge using seam-translate-position.
2) Continue until the destination zone is reached, then clear path.

Phase 3: Fallback Behavior
--------------------------
- If path cannot be found, revert to current single-edge behavior.
- If target is in an orthogonal neighbor, path length = 1 (current behavior).

Part 2: Minimap Diagonal Click Parity
-------------------------------------
Goal: minimap clicks (including far/diagonal targets) behave identically to
floor-map clicks, even when preview zones are not loaded.

Phase 4: Zone-Bounds Index (No Preview Dependency)
--------------------------------------------------
1) Build a lightweight zone-bounds index at startup:
   - For every zone in world-graph, store world-min/max bounds.
   - Prefer reading bounds from data metadata; if absent, derive from zone
     tile dimensions and zone origin.
2) Store the index on the world struct (client-only):
   - Use a simple mapping: zone-id -> (min-x max-x min-y max-y).
   - Ensure it is always available, independent of preview/LRU caches.

Phase 5: Unify Click Translation
--------------------------------
1) Refactor click translation to consult the bounds index first:
   - Determine destination zone for any click (floor or minimap) by bounds.
   - Only fall back to preview/LRU caches for optional details.
2) Use the bounds index for multi-hop rebasing:
   - Each hop uses bounds for current and next zone from the index.
   - Avoid loading zones just to compute rebases.

Phase 6: Minimap Click Flow
---------------------------
1) On minimap click:
   - Compute dest-zone-id via bounds index.
   - Compute zone path (BFS over world-graph).
   - Store zone-click path + final target in game state as in Part 1.
2) Keep current fallback semantics:
   - If bounds are missing (should not happen), revert to single-edge behavior.

Testing Addendum
----------------
- Clear preview/LRU caches, then minimap-click diagonally; verify multi-hop.
- Minimap-click to far zones (not adjacent); verify path is computed and
  player arrives at destination.
- Regression: floor clicks unchanged; orthogonal minimap clicks unchanged.

Decision (Scope)
----------------
Minimap clicks should be treated as true world-space destinations. Use the
zone-bounds index to resolve the destination zone and compute a full multi-hop
path across the world-graph (BFS), not just a 2-hop diagonal. This gives the
best player experience and keeps the implementation generalized and predictable.

UI/UX Notes
-----------
- Keep click marker in the original world position (optional) or update
  marker to the rebased target each hop for clarity.
- Ensure camera/transition visuals remain stable across multi-hop traversal.

Testing
-------
- Click from zone 3 → zone 2 (diagonal): verify player crosses two edges
  and stops at the clicked tile.
- Click from zone 1 → zone 4 and zone 2 → zone 3 (both directions).
- Orthogonal clicks still work (no regression).
- Path not found → fallback to single-edge behavior.

Notes
-----
- This is a pathing/zone-routing enhancement; it should be client-only.
- Server remains authoritative for actual movement and transitions.
- Keep the zone-path state ephemeral; clear it if the player cancels movement
  or starts keyboard movement.
