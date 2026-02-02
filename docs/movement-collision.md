# movement-collision.lisp

Purpose
- Wall map construction from zone collision data and tile-based collision detection for movement resolution.

Key responsibilities
- `derive-wall-map-from-zone`: create a 2D array from zone collision tiles where 1 = blocked, 0 = passable.
- `build-wall-map`: construct a test wall map with solid border walls.
- `wall-occupied-p`: check whether a tile coordinate in the wall map is blocked.
- Collision detection helpers for movement sliding and resolution.
- Wall map coordinate translation between world space and local tile indices.

Load order
- Loaded second among movement files: `movement-core` -> `movement-collision` -> `movement-preview` -> `movement-transition`.
- Depends on `movement-core` for zone state; used by `movement-transition` for collision during zone changes.
