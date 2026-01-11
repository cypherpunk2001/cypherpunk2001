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
    ...
  docs/
    raylib_cheatsheet.md
    claw-raylib-readme.org
```

---

## Current Task

At this point in development it's a good time to pause and ask, are we writing modular and reuseable code?

We need to ensure that we're making reuseable functions and objects that we can repurpose as we develop further the game worlds.

---

## How to keep it smooth as we scale

- Avoid per‑frame consing in hot loops: reuse rectangles, vectors, strings, and animation state.
- Cull off‑screen tiles/sprites; draw only what’s visible.
- Chunk the map (e.g., 32×32 tiles) and cache static chunks in a render texture.
- Keep entity data in arrays/structs, not lists; use object pools.
- Separate update/draw; keep animation state lightweight.
