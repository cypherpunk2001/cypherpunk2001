# AGENTS.md

Minimal Common Lisp + raylib project slowly iterating in to a single player rpg.

## Project layout

```
mmorpg/
  AGENTS.md
  mmorpg.asd
  assets/*
    ...
  src/
    package.lisp
    main.lisp
  docs/
    raylib_cheatsheet.md
    claw-raylib-readme.org
```

---

## Current Task

# AGENTS.md

Goal: render a simple dungeon floor tile layer behind the player. Keep it minimal.

## Tileset
- Tile size: 16×16
- Atlas: assets/2 Dungeon Tileset/1 Tiles/Tileset.png
- Atlas size: 304×176 → grid = 19 cols × 11 rows
- Tile indices: 0..208 (row-major)

## Tile index → source rect
Given i:
- col = i mod 19
- row = floor(i / 19)
- src = (col*16, row*16, 16, 16)

## Floor map
- Represent floor as a 2D array of tile indices (map-h × map-w)
- Start with constant fill using one FLOOR tile index (pick any, adjust later)

## Render order
1) draw floor tilemap
2) draw player

## Constraints
- One layer only (floor)
- No walls, collision, camera, or Tiled integration yet
- Use Tileset.png atlas (don’t load individual Tile_XX.png)

## How to keep it smooth as we scale

- Avoid per‑frame consing in hot loops: reuse rectangles, vectors, strings, and animation state.
- Cull off‑screen tiles/sprites; draw only what’s visible.
- Chunk the map (e.g., 32×32 tiles) and cache static chunks in a render texture.
- Keep entity data in arrays/structs, not lists; use object pools.
- Separate update/draw; keep animation state lightweight.
