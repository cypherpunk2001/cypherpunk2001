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

## Next Task: Walls + Collision (Simple Structure)

Goal: create navigable spaces so the world feels like a place, not just terrain.

### Requirements
- Add a wall/obstacle tile layer (separate from the floor layer).
- Choose a small set of wall tile indices from the existing tileset.
- Render order:
  1) floor
  2) walls/obstacles
  3) player
  4) debug/UI
- Build a collision grid: a tile is blocked if the wall layer at (tx,ty) is non-zero.
- Update movement:
  - Keyboard: prevent stepping into blocked tiles.
  - Click-to-move: stop movement when the next step would enter a blocked tile.
  - No pathfinding yet; do not route around walls.

### Map Source
- Hardcode a small test map first (e.g. one room with a doorway + a corridor).
- Keep it deterministic and simple (arrays, not an editor pipeline).

### Definition of Done
- Visible walls exist on top of the floor.
- Player cannot walk through wall tiles.
- Click-to-move respects collision (stops at obstacles).
- You can navigate a simple room/corridor layout.

## How to keep it smooth as we scale

- Avoid per‑frame consing in hot loops: reuse rectangles, vectors, strings, and animation state.
- Cull off‑screen tiles/sprites; draw only what’s visible.
- Chunk the map (e.g., 32×32 tiles) and cache static chunks in a render texture.
- Keep entity data in arrays/structs, not lists; use object pools.
- Separate update/draw; keep animation state lightweight.
