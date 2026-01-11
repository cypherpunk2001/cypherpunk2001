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

## Current Task - Fix wall collisions

approx: top left of literal visual square wall tile: pos=336.60,197.20 tile=5,3
top right lit vis sq wall tile: pos=943.40,197.20 tile=14,3
bottom left lit vis sq wall tile: pos=336.60,552.40 tile=5,8
bottom right lit vis sq wall tile: pos=947.11,552.40 tile=14,8

now this is how far in the map i can actually get before i hit collision square:

top left extremity colission hit: pos=129.40,130.60 tile=2,2
top right: pos=1149.44,128.06 tile=17,2
bottom left: pos=129.40,637.50 tile=2,9
bottom right: pos=1149.44,638.71 tile=17,9

Debug overlay needed to fix wall/collision mismatch:

- Draw world tile grid in world space (inside camera mode):
  - tile_px = *tile-size* * *tile-scale*
  - for visible tiles: draw thin grid lines or faint rect outlines

- Draw blocked collision cells (from collision grid) as translucent red rects:
  - dst rect for tile (tx,ty):
    x = tx * tile_px
    y = ty * tile_px
    w = tile_px
    h = tile_px

- Draw wall-layer tile dst rects as translucent blue rects using the SAME dst math.

- Draw player collision box as a green rect (in world space).

- Print, each frame:
  - player world pos
  - collider center world pos
  - computed tile coords from collider center:
    tx = floor(cx / tile_px), ty = floor(cy / tile_px)
  - computed tile coords from collider "feet" point:
    fx = cx, fy = cy + collider_h/2
    tx_feet = floor(fx / tile_px), ty_feet = floor(fy / tile_px)

Goal: red collision tiles must exactly overlap blue wall tiles.

## How to keep it smooth as we scale

- Avoid per‑frame consing in hot loops: reuse rectangles, vectors, strings, and animation state.
- Cull off‑screen tiles/sprites; draw only what’s visible.
- Chunk the map (e.g., 32×32 tiles) and cache static chunks in a render texture.
- Keep entity data in arrays/structs, not lists; use object pools.
- Separate update/draw; keep animation state lightweight.
