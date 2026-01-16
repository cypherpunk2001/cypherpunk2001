# AGENTS.md

Common Lisp + raylib MMORPG prototype focused on clean architecture and system separation. The codebase is structured to teach modern game design habits: data-driven content, intent-based actions, and a strict update/draw split.

## Project layout

```txt
mmorpg/
  AGENTS.md
  README.md
  mmorpg.asd
  assets/*
    ...
  data/*
    game-data.lisp
    world-graph.lisp
    ...
    zones/*
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

Regression discovered:

We used to be able to hold down the mouse click button anywhere on the world tiles and it would continually walk in that direction. It no longer works. I mean, yes we want the yellow X and so forth to behave the same, when click it makes yellow x and moves char there, but without breaking that -- if player continues to hold down the mouse button, it just continue walking around and refresh with the mouse every X second of (defparameter *mouse-hold-repeat-seconds* 0.25) ;; Repeat rate for mouse-held updates.


---

## Future Tasks / Roadmap

### Near-term (foundation, priority order)

4) Ability/skill system built on intent (cooldowns, cast times, resource costs)

I consider that we already have developed this with the mele system and the run/stamina timer cooldown system. Therefore, I believe we can skip developing further in 4, if you agree Chat, just double check over that our mele and stamina systems do observe the following principles in preparation for the upcoming client/server split.
- client sends intent
- server validates
- server updates state
- client renders result

Just ensure we already follow the authority/intents boundary in mele/stamina code.

5) Save/load + snapshot serialization (no rendering dependency)
(For whatever it's worth, when we do the final server/client split, postgresql is planned.)
- Even if the game is always-online, you still need ways to persist and restore the authoritative world state.
Not optional. Do this early.
This is your “server-shaped core.” It also becomes your snapshot sync format later.
Minimum slice:
Serialize: world tick, entities (id, pos, hp), inventory list, cooldown timers
Version tag in the file
That’s enough

6) NPC spawn/respawn tables, simple faction/aggro rules, tuning via data

We have already built a real AI. Let's ensure that our current npc spawn mechanisms are ready for the client/server split as much as we can per usual principles of server authority, etc.

Just ensure we already follow the authority/intents boundary in npc/item spawn code.

### Mid-term (world + UX)
- Headless server loop in-process (client sends intents, server updates state)
This is the split, just without sockets. Not optional.
Do this ASAP after save/load.

### Long-term (MMO readiness)
- Real client/server split (server-authoritative simulation, snapshot sync)
- Prediction + interpolation for smooth client UX
- Persistent world storage and migrations
- Editor upgrades for world-graph, spawns, and content validation
- Asset pipeline for animation sets, atlases, and build-time validation

---

## Client/Server Timing Guidance

- Until we do the full network split: enforce server-authoritative boundaries now.
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
