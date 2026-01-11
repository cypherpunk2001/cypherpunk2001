# Collision System

This project uses a grid-based collision system tied to the same tile grid used
for floors and walls. The player is treated as a simple axis-aligned box in
world space, and walls are defined by a discrete tile map. The implementation
lives in `src/main.lisp`.

## Overview

- **World space** is measured in pixels.
- **Tile space** is measured in tile coordinates.
- **Tile size in world space** is `*tile-size* * *tile-scale*`.
- **Collisions** are resolved against wall tiles (1 = blocked, 0 = empty).

## Wall Map and Blocked Tiles

Walls are stored in a 2D array built by `build-wall-map` in `src/main.lisp`.
The current map is a simple border box to validate collisions.

Key inputs:

- `*wall-map-width*`, `*wall-map-height*` — wall map size in tiles.
- `*wall-origin-x*`, `*wall-origin-y*` — where the wall map starts in world tiles.
- `*wall-tile-indices*` — which atlas tiles to draw for walls.

Helpers:

- `wall-occupied-p` — checks whether a tile inside the wall map is nonzero.
- `wall-blocked-p` — returns true for wall tiles **and** for any tile outside
  the wall map. This keeps the player constrained inside the test box.
- `wall-tile-at` — selects a wall tile index for rendering using a hash.

## Player Collider Size

The player collider is an axis-aligned rectangle centered on the player:

- `tile-dest-size = *tile-size* * *tile-scale*`
- `collision-half-width = tile-dest-size / 2 * *player-collision-scale*`
- `collision-half-height = tile-dest-size / 2 * *player-collision-scale*`

This keeps the collider in the same coordinate system as the wall grid.
The knob to tune size is `*player-collision-scale*` in `src/main.lisp`.

## Collision Test (Tile Overlap)

`blocked-at-p` converts the collider bounds into tile coordinates:

- `left/right/top/bottom` are computed from collider center and half size.
- Tile coordinates use `floor(coord / tile-dest-size)`.
- Any overlap with a blocked tile returns true.

This is robust and consistent because both rendering and collision use the same
`tile-dest-size` conversion.

## Movement Resolution

`attempt-move` resolves motion in two steps:

1. Try X movement; cancel if blocked.
2. Try Y movement; cancel if blocked.

This axis-separated approach allows sliding along walls without sticking.
After movement, the position is clamped to the interior bounds defined by the
wall map to avoid drifting out of the test box.

## Debugging Tools

There are two toggles in `src/main.lisp`:

- `*debug-collision-overlay*` — draws a world-space overlay:
  - Red tiles = blocked collision cells (`wall-blocked-p`)
  - Blue tiles = wall tiles (`wall-tile-at`)
  - Green box = player collider
- `*verbose-logs*` — prints per-frame position and tile coordinates.

These are meant for development only and can be left off for normal play.

## Scaling Path

The collision system is designed to scale without changing the renderer:

- Swap `build-wall-map` for a dungeon generator or loaded map.
- Move from a single wall map to chunked collision layers.
- Keep the same `(x, y) -> blocked?` contract in `blocked-at-p`.

As long as wall tiles and collision tiles stay aligned to the same grid, the
player will collide exactly where the walls are drawn.
