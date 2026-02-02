# rendering-core.lisp

Purpose
- Core rendering helpers including tile atlas lookups, rectangle manipulation, texture loading with color key detection, and shared rendering state.

Key responsibilities
- `floor-tile-at` / `wall-tile-at`: tile index lookups for floor and wall rendering with variant selection.
- `set-rectangle`: mutate raylib rectangle structs with new bounds.
- `set-tile-source-rect`: compute atlas source rectangle from tile index, tile size, and column count.
- `image-border-color-key`: detect appropriate color key from image borders for transparency.
- Color utility functions for rendering effects.
- Shared rendering state and constants used across all rendering subsystems.
- Tileset column and row calculations for sprite atlas management.

Load order
- Loaded first among rendering files: `rendering-core` -> `rendering-tiles` -> `rendering-entities` -> `rendering-ui`.
- All other rendering files depend on draw helpers and shared state defined here.
