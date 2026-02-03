# movement-core.lisp

Purpose
- Zone state management, movement helpers, running/stamina logic, and player position updates with per-zone collision support.

Key responsibilities
- Zone state cache (`*zone-states*`): track loaded zones with NPCs, collision data, and spatial grids.
- `get-zone-state`, `get-or-create-zone-state`: lazy zone loading and caching.
- Occupied zones cache (`*occupied-zones-cache*`): pre-computed per-tick list of active zone IDs to avoid allocation.
- `refresh-occupied-zones-cache`: rebuild the occupied zone set each tick.
- `update-running-state`: stamina drain/regen and speed multiplier calculation.
- `update-player-position`: move player with collision, per-zone wall maps, and target tracking.
- Zone wall map lookup helpers for per-zone collision resolution.
- Diagonal zone click path (Bug 4): `clear-zone-click-path`, `compute-diagonal-click-path`, `translate-click-to-final-zone` for multi-hop diagonal zone transitions via click-to-move.

Multi-hop click pathing (Bug 4 Part 3)
- All click types (floor and minimap) use the same bounds-based destination resolution. Floor clicks have camera-limited raw coordinates (typically 1-2 hop); minimap clicks can reach further. Both use the same resolve/translate code path for consistency.
- `resolve-click-destination-zone`: walk raw click coordinates through zone graph to find destination zone.
- `compute-minimap-click-path`: compute a full multi-hop zone path for any click. Returns (values zone-path edge-list hop-targets final-x final-y). Uses `resolve-click-destination-zone` to find the destination, then BFS (`world-graph-find-path`) for shortest-path selection, then `translate-click-along-path` for coordinate translation along the BFS path.
- `translate-click-along-path`: chain seam translations along an N-hop path. Returns (values final-x final-y hop-targets) where hop-targets is a list of per-hop walk targets precomputed from the original raw click.
- `zone-path-edge-list`: compute per-hop edge directions for a zone path.
- Multi-hop continuation uses precomputed per-hop targets (derived from the original raw click, not the player's runtime position) so the walk direction is always correct regardless of where the player stands in the zone.
- Note: `zone-bounds-zero-origin` moved to utils.lisp to fix load-order inversion.
- Bug 5/6: `clear-zone-click-path` also clears `zone-click-retry-p` (single retry flag for unexpected-zone recompute).

Zone bounds
- `build-zone-bounds-index` reads `:origin-x`/`:origin-y` from zone files (defaulting to 0) and incorporates origin offsets into bounds calculation via `zone-bounds-with-origin`.
- Logs a warning at startup if non-uniform zone bounds are detected.
- Hop limits are configurable: `*minimap-resolve-max-hops*` (default 8), `*bfs-max-hops*` (default 32). Both log warnings when hit.

Load order
- Loaded first among movement files: `movement-core` -> `movement-collision` -> `movement-preview` -> `movement-transition`.
- Depends on `zone.lisp` for zone loading; other movement files depend on zone state functions defined here.
