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

## Next Task: World Travel + Mouse Navigation + Landmarks

Goal: make the game feel like a real world you travel through, not a repeating fishtank.

### 1) Infinite Non-Repeating Floor
- Remove modulo/repeating lookup into a prebuilt floor array.
- For each visible world tile coordinate `(tx, ty)`, compute the atlas tile index directly via `floor-tile-at(tx, ty, ...)`.
- Use world tile coordinates (derived from camera bounds), not local map indices.
- Keep deterministic behavior via `*floor-seed*`.
- Continue culling tiles to only those visible in the camera view.

### 2) Point-and-Click Movement (No Pathfinding)
- On left mouse click, convert mouse position from screen space to world space.
- Set a player target position in world coordinates.
- Move the player toward the target at constant speed.
- Stop movement when within a small epsilon distance and return to Idle.
- Keyboard movement may remain enabled and override mouse movement.
- Draw a small debug marker at the target position.

### 3) Sparse World Landmarks (Visual Only)
- Add a second, optional decorative overlay selection (still walkable).
- Use a deterministic hash of `(tx, ty)` to place decorations very sparsely (e.g. 1 in 60–100 tiles).
- Draw overlay tiles after the floor layer (and before or after the player as appropriate).
- No collision or gameplay logic associated with decorations.

### Constraints
- No collision, walls, or pathfinding.
- No gameplay changes beyond movement targeting.
- Keep all rendering in world coordinates under the existing camera.
- Keep systems simple and deterministic.

### Definition of Done
- The floor no longer repeats as the player travels.
- The player can click to move and visibly travel across the world.
- Sparse landmarks make movement and distance readable.
- The world feels continuous and explorable.

## How to keep it smooth as we scale

- Avoid per‑frame consing in hot loops: reuse rectangles, vectors, strings, and animation state.
- Cull off‑screen tiles/sprites; draw only what’s visible.
- Chunk the map (e.g., 32×32 tiles) and cache static chunks in a render texture.
- Keep entity data in arrays/structs, not lists; use object pools.
- Separate update/draw; keep animation state lightweight.
