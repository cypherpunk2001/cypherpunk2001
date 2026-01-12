# movement.lisp

Purpose: collision testing and movement resolution.

Key responsibilities:
- Procedural wall map for fallback worlds.
- Map-aware blocking via `world-blocked-tile-p`.
- Per-axis movement resolution with collision checks.

Key functions:
- `attempt-move`, `blocked-at-p`, `world-blocked-tile-p`.
- `update-player-position`, `update-running-state`.
- `make-world` builds derived bounds and collision sizes.

Notes:
- When a TMX map exists, bounds come from map data and collision layers.
