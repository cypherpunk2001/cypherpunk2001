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
    config.lisp
    types.lisp
    utils.lisp
    input.lisp
    movement.lisp
    combat.lisp
    ai.lisp
    audio.lisp
    ui.lisp
    rendering.lisp
    main.lisp
  docs/
    raylib_cheatsheet.md
    claw-raylib-readme.org
```

---

## Current Task

- Main loop stays an orchestrator; implement behavior inside the system files (input, movement/collision, combat, AI, rendering, audio, UI).

---

## Future Tasks / Roadmap (Do not perform these at this time)

- Generalize entity handling: move from single npc to an array/pool of entities and treat the player as just another entity with shared combat/animation systems.

- Make NPC archetypes, animation sets, and tunables data-driven (S-expr/JSON), so new NPCs/classes don’t require code edits and can be reused across worlds.

- Build a real map pipeline (TMX load, collision layers, chunked render/cull) instead of the test wall map; keep the tile variation as a reusable decoration system.

- Add an action/event layer so input, AI, and future networking feed the same “intent” API; keeps behavior decoupled from rendering and makes AI/NPCs first-class.

---

## Coding Guidelines: Modular & Reusable by Default

We are building reusable game systems, not one-off demo code.

**Bias:** write small, composable functions, CLOS objects and data-driven systems that can be reused by players, NPCs, and future worlds.

### Rules
- No gameplay logic in the main loop — it only orchestrates systems
- Entities hold data; systems implement behavior
- Rendering is separate from game logic
- Avoid hardcoded values that may vary later
- If it could apply to NPCs, write it generically now

### Agent Self-Check
Before outputting code, ensure:
- reusable beyond a single entity
- logic works without rendering
- behavior is not special-cased
- future client/server split wouldn’t break it

If unsure, refactor toward reuse.

---

## Performance Matters!

- Avoid per‑frame consing in hot loops: reuse rectangles, vectors, strings, and animation state.
- Cull off‑screen tiles/sprites; draw only what’s visible.
- Chunk the map (e.g., 32×32 tiles) and cache static chunks in a render texture.
- Keep entity data in arrays/structs, not lists; use object pools.
- Separate update/draw; keep animation state lightweight.
