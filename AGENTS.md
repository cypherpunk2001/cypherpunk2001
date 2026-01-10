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

Next task: add a world-space camera that follows the player.

Requirements:
- Introduce a camera offset so the world scrolls under the player
- Player should remain near the screen center while moving
- Floor tiles and player are drawn in world coordinates
- No gameplay changes (movement logic stays the same)
- No collision, walls, or map generation changes
- No point-and-click yet

Definition of done:
- Walking causes the floor to move beneath the player
- The player can travel indefinitely without hitting a “screen edge”

## How to keep it smooth as we scale

- Avoid per‑frame consing in hot loops: reuse rectangles, vectors, strings, and animation state.
- Cull off‑screen tiles/sprites; draw only what’s visible.
- Chunk the map (e.g., 32×32 tiles) and cache static chunks in a render texture.
- Keep entity data in arrays/structs, not lists; use object pools.
- Separate update/draw; keep animation state lightweight.
