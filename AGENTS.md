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
    zone.lisp
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

- Currently the sprite sheets used by the characters and npcs are fine, for now, but we will be moving away from the other sprites in use currently. We are moving to using 16x16 everywhere (I am not sure if we are using 32x32 anywhere but just stating for certainty.) Particuarly we have new 16x16 assets in "assets/Zelda-like" that need to be integrated in to the world zone editor mode. These are all 16x16 sprite sheets of various sizes (feel free to use imagemagick and other cli tools to determine more information about them as needed)

- One thing I would really like is instead of having to press q|e to cycle through tiles one by one, is just a way to cycle through the various sprite sheets (q|e for that would be fine) but then it literally opens up the entire sheet and allows me to preview them all together so that I can click on the one I want to paint with directly. This would allow me to see the entire context of the various tiles available in my sprite sheets.

- Also I'm not sure what we need z/x for the docs say "Z/X cycle objects or spawns." But we already have 1,2,3,4 I think which should be enough for this? The docs state "1 tile mode, 2 collision mode, 3 object mode, 4 spawn mode." So clean this up or clarify.

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

After adding new code files, there should be new docs files to match.

After refactoring or removing code, leftover references to the old code should always be cleaned, updated or fixed.

- Checks that every `src/foo.lisp` has a matching `docs/foo.md`, errors if any are missing, otherwise prints a friendly reminder when all pass.
