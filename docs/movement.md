# movement.lisp

Purpose
- Resolve movement and collisions against the world.

Why we do it this way
- Separating movement from input allows AI, networking, and scripted events
  to reuse the same collision logic.
- Per-axis movement resolution avoids "tunneling" and feels more responsive.

Key concepts
- `world-blocked-tile-p` decides if a tile is blocked (zone collision tiles are converted into the wall map at load time).
- `set-world-blocked-tile` lets the editor update the wall map at runtime.
- `build-minimap-collisions` caches collision marker positions for minimap rendering,
  including zone boundary tiles so world edges are visible; edges with world-graph
  exits are omitted so internal borders are not shown.
- `attempt-move` applies per-axis movement with collision checks.
- `make-world` derives bounds and collision sizes from map data.
- `apply-zone-to-world` swaps the active zone and rebuilds world bounds.
- `update-zone-transition` checks the world graph and swaps zones on edge exit.
- Zone transitions log source/target IDs in verbose mode to aid debugging.
- `world-open-position` finds the nearest open tile that fits the player collider.
- `ensure-npcs-open-spawn` snaps NPCs to open tiles (using NPC collider sizes).
- Zone transitions carry engaged NPCs that are within perception range into the next zone,
  preserving their offset from the player.
- Zone transitions cache NPCs per zone (excluding carried NPCs) so returning to a zone
  restores its population without duplicating spawns.
- Zone transitions rebuild the entity array using the current players plus NPCs.
- NPC spawns allocate stable entity IDs from the simulation ID source.
- `*collision-edge-epsilon*` avoids treating exact tile-edge contact as blocked.

Walkthrough: from intent to position
1) Read movement intent (dx/dy) and optional target.
2) If the player has a target, compute direction toward it.
3) Call `attempt-move` which resolves collisions per axis.
4) Clamp to world bounds.
5) If the player pushes against a world edge, consult the world graph and
   transition zones (preserving edge offset and click-to-move targets).
6) Store final position and velocity.

Example: applying intent
```lisp
(let* ((moving (or (not (zerop (intent-move-dx intent)))
                   (not (zerop (intent-move-dy intent)))
                   (intent-target-active intent)))
       (speed-mult (update-running-state player dt moving
                                         (intent-run-toggle intent))))
  (update-player-position player intent world speed-mult dt))
```

Design note
- World bounds are enforced to keep navigation stable even when collision
  layers are sparse or missing.
- Spawning on open tiles prevents actors from getting stuck inside blocked
  collision layers.
- Zone transitions only happen on edges defined in `data/world-graph.lisp`.
- Active click-to-move targets persist across zone transitions so long walks
  can continue without re-clicking.
- Zone swaps refresh cached minimap spawn previews from adjacent zones that
  define explicit spawns. Previews are offset using world collision bounds so
  they align with adjacent zone placement, and render when the player is
  pushing against a valid exit edge or within `*minimap-preview-edge-tiles*`
  tiles of one.
- When the camera view reaches a zone edge or corner, `ensure-preview-zones`
  loads adjacent and diagonal zones into a preview cache for seamless cross-zone
  rendering.
