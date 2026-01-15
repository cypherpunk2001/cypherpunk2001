# AGENTS.md

Common Lisp + raylib project slowly iterating in to a mmorpg.

## Project layout

```
mmorpg/
  AGENTS.md
  mmorpg.asd
  assets/*
    ...
  data/*
    game-data.lisp
    ...
  src/*
    main.lisp
    ...
  docs/*
    raylib_cheatsheet.md
    claw-raylib-readme.org
    ...
```

---

## Current Task

Define the next milestone and sequence the work so the core gameplay loop
(move -> fight -> loot -> progress) is data-driven and reusable.

1) Simulation boundary: pure state + intents, fixed-tick update, stable entity IDs
2) Stats + modifiers system (base stats, buffs/debuffs, derived values)

---

## Future Tasks / Roadmap

### Near-term (foundation, priority order)
3) Inventory/equipment/loot + XP progression (data-driven archetypes)
4) Ability/skill system built on intent (cooldowns, cast times, resource costs)
5) Save/load + snapshot serialization (no rendering dependency)
6) NPC spawn/respawn tables, simple faction/aggro rules, tuning via data

### Mid-term (world + UX)
- Headless server loop in-process (client sends intents, server updates state)
- Pathfinding/nav graph for NPCs; steering that reuses movement system
- Zone streaming + chunked rendering cache; stricter culling
- Combat effects pipeline (projectiles, hitboxes, damage types)
- UI: inventory, character sheet, hotbar, chat, settings

### Long-term (MMO readiness)
- Real client/server split (server-authoritative simulation, snapshot sync)
- Prediction + interpolation for smooth client UX
- Persistent world storage and migrations
- Editor upgrades for world-graph, spawns, and content validation
- Asset pipeline for animation sets, atlases, and build-time validation

---

## Client/Server Timing Guidance

- Do not do the full network split yet; enforce server-authoritative boundaries now.
- Keep all gameplay in pure state updates driven by intents; rendering reads only.
- Add save/load + snapshot serialization early; it becomes the future net format.
- Prove a local headless "server loop" before committing to real networking.
- Add real networking only after core progression systems are stable.

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

---

## Documentation Requirements

- When behavior changes in `src/*.lisp`, update the corresponding doc in `docs/*.md` (e.g., `src/movement.lisp` -> `docs/movement.md`).

## Testing Requirements

### Mandatory self-check

-   After **every code change**, run:

``` sh
make checkparens

make ci

make smoke

make checkdocs
```

#### `make checkparens`

Checks all `.lisp` files in `data/` and `src/` for balanced parentheses and general sexp structure.

- Reports results **per file**.
- Fails immediately on the first unmatched bracket/quote.
- Very fast and cheap to run.

This is the agent’s most useful quick check alongside `make ci`.

**Run it often** — before, during, and after making changes.

#### make ci

- cold SBCL start
- loads Quicklisp
- (ql:register-local-projects)
- (ql:quickload :mmorpg)
- compiles system
- no window, no GPU

#### make smoke

- loads system
- runs (mmorpg:run :max-seconds … :max-frames …)
- opens a real window briefly
- exits automatically

#### make checkdocs

After adding new code files, there should be new docs files to match.

After refactoring or removing code, leftover references to the old code should always be cleaned, updated or fixed.

- Checks that every `src/foo.lisp` has a matching `docs/foo.md`, errors if any are missing, otherwise prints a friendly reminder when all pass.
