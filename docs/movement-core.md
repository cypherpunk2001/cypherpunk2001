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

Load order
- Loaded first among movement files: `movement-core` -> `movement-collision` -> `movement-preview` -> `movement-transition`.
- Depends on `zone.lisp` for zone loading; other movement files depend on zone state functions defined here.
