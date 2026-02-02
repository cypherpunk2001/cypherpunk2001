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
