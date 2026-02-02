# movement.lisp (and movement-*.lisp modules)

**Module structure:** `movement.lisp` is a thin glue file. The movement code is split into:
- `movement-core.lisp` — movement integration, intent processing, player position updates
- `movement-collision.lisp` — collision checks, wall maps, bounds, open spawn placement
- `movement-transition.lisp` — zone transitions, hysteresis, cooldown, NPC carry-across
- `movement-preview.lisp` — preview zones, camera edge checks, minimap collision markers

See `docs/movement-core.md`, `docs/movement-collision.md`, `docs/movement-transition.md`, `docs/movement-preview.md` for per-file details.

Purpose
- Resolve movement and collisions against the world.

Why we do it this way
- Separating movement from input allows AI, networking, and scripted events
  to reuse the same collision logic.
- Per-axis movement resolution avoids "tunneling" and feels more responsive.

Key functions

**Collision:**
- `world-blocked-tile-p` - Decide if a tile is blocked.
- `set-world-blocked-tile` - Let editor update wall map at runtime.
- `blocked-at-p` - Test collider bounds against blocked tiles.
- `position-blocked-p` - Return true when collider centered at X/Y is blocked.
- `attempt-move` - Resolve movement per axis and cancel when blocked.
- `npc-collision-half` - Return NPC collider half sizes in world pixels.

**World Construction:**
- `make-world` - Build world state and derived collision/render constants.
- `apply-zone-to-world` - Replace world's zone and rebuild wall-map-derived bounds.
- `build-wall-map` - Create a test wall map array with solid border.

**Open Spawn Placement:**
- `world-open-position` - Return nearest open tile center for player collider.
- `world-open-position-for` - Return nearest open tile center for given collider.
- `find-open-tile` - Return nearest open tile around given tile coordinates.
- `ensure-npcs-open-spawn` - Snap NPCs to open tiles with correct collider sizes.

**Zone Transitions:**
- `update-zone-transition` - Handle edge-based world graph transitions.
- `transition-zone` - Apply zone transition using exit metadata.
- `world-exit-edge` - Return edge player is pushing against.
- `world-edge-exit` - Return exit spec for edge in current zone.
- `edge-opposite` - Return opposite world edge.
- `edge-spawn-position` - Return spawn coordinates for target edge.

**Multi-Zone Collision (Current Implementation):**
Per-player zone transitions are fully supported with per-zone collision. Each player's
movement uses their zone's collision bounds from zone-state:
- Player movement uses per-zone collision via `attempt-move-with-map` and zone-state wall maps
- NPC movement uses per-zone collision when zone-state is available (falls back to world)
- Edge detection uses per-zone bounds via `get-zone-collision-bounds` for accurate transitions
- Unstuck teleportation uses per-zone collision from zone-state (correct for each player)
- New player spawns use per-zone collision from zone-state (spawns in correct zone bounds)
- Zone transition ratio calculation uses source zone bounds for position preservation

**NPC Transition:**
- `collect-transition-npcs` - Collect NPCs that should carry across zones.
- `npc-transition-candidate-p` - Return true when NPC should follow across edge.
- `reposition-transition-npcs` - Reposition carried NPCs around player's new spawn.
- `cache-zone-npcs` - Cache NPCs in zone-state, excluding carried ones.
- `cached-zone-npcs` - Return cached NPCs from zone-state.
- `merge-npc-vectors` - Append extras to base NPC array.

**Minimap:**
- `build-minimap-collisions` - Cache collision marker positions for minimap.
- `build-adjacent-minimap-spawns` - Build cached spawn positions for adjacent zones.
- `world-preview-edge` - Return edge to preview minimap spawns for.

**Zone Preview (Camera-Based):**
- `ensure-preview-zones` - Load adjacent zone data when camera reaches world edge.
- `ensure-preview-zone-for-edge` - Load adjacent zone data for preview on edge.
- `ensure-preview-zone-for-corner` - Load diagonal zone data for corner preview.
- `world-preview-zone-for-edge` - Return cached preview zone for edge.
- `world-preview-zone-for-corner` - Return cached preview zone for corner.

**Camera:**
- `camera-view-center` - Return current camera focus point.
- `camera-view-bounds` - Return view bounds in world coordinates.
- `view-exceeds-edge-p` - Return true when camera view extends beyond edge.

**Player Movement:**
- `update-player-position` - Move player with collision and target logic.
- `update-running-state` - Update stamina and return speed multiplier.
- `player-intent-direction` - Return intended movement direction for edge transitions.

**Player Unstuck System:**
- `player-is-stuck-p` - Return T if player cannot move in any cardinal direction (uses global world).
- `player-is-stuck-p-for-zone` - Return T if player is stuck using per-zone collision from zone-state.
- `get-zone-safe-spawn` - Return random position within zone bounds (global world).
- `get-zone-safe-spawn-for-zone` - Return random position within specified zone's bounds from zone-state.
- `process-player-unstuck` - Handle unstuck request (server authority, uses per-zone collision).

**Per-Zone Collision Helpers:**
- `get-zone-wall-map` - Return wall-map from zone-state cache for given zone-id.
- `get-zone-collision-bounds` - Calculate movement bounds for a zone using its wall-map dimensions.
- `blocked-at-p-with-map` - Test collider bounds against a specific wall-map (factored from blocked-at-p).
- `find-open-position-with-map` - Find nearest open position using a specific wall-map.
- `zone-state-spawn-position` - Return valid spawn (x, y) using zone-state's wall-map.

**Utility:**
- `tile-center-position` - Return world position for center of tile.
- `wall-occupied-p` - Check whether tile inside wall map is nonzero.
- `wall-blocked-p` - Treat walls and out-of-bounds as blocked for collision.
- `world-search-radius` - Return max search radius in tiles for open spawn placement.
- `zone-bounds-from-dimensions` - Return wall bounds for zone with given dimensions.
- `log-player-position` - Emit verbose position diagnostics for debugging.

Key concepts
- Zone collision tiles are converted into the wall map at load time.
- Minimap collision markers include zone boundaries (except at world-graph exits).
- Zone transitions log source/target IDs in verbose mode to aid debugging.
- Zone transitions carry engaged NPCs within perception range into next zone, preserving offset.
- Zone transitions cache NPCs per zone (excluding carried) so returning restores population.
- Zone transitions rebuild entity array using current players plus NPCs.
- NPC spawns allocate stable entity IDs from simulation ID source.
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
