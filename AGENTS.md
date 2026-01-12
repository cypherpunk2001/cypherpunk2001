# AGENTS.md

Common Lisp + raylib project slowly iterating in to a mmorpg.

## Project layout

```
mmorpg/
  AGENTS.md
  mmorpg.asd
  assets/*
    ...
  data/
    game-data.lisp
  src/
    package.lisp
    config.lisp
    types.lisp
    utils.lisp
    map.lisp
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

Currently the world map generates such that if there are two unblocked squares to the human eye it looks like i should be able to walk through them. but they're the same size as my character (green square in debug) so i'm unable to walk through an area that looks like I should be able to walk through. See screenshot attached.


---

## Future Tasks / Roadmap



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

- Checks that every `src/foo.lisp` has a matching `docs/foo.md`, errors if any are missing, otherwise prints a friendly reminder when all pass.
