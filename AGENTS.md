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

## Current Tasks / TODO
- Ensure there is no remaining single player code in the codebase, we will be doing all development and testing with the new make server and make client infrastructure going forwards.
If you make a change, pass tests, stage and commit this one.

- New performance optimization: Consider multi-threading on the server side, but only where it is safe and makes sense. Imagine 10,000 users connected simultaneously, with 500 players active in each zone.
I do think the client is probably safe? I have tested single player already with my character and hundreds of npcs in a zone and it was smooth and did not experience any hiccups -- so if you agree with that logic, lets just focus on server optimizations.
Pass tests, stage and commit this issue as well

- Prediction + interpolation for smooth client UX
This client optimization idea has me intrigued, while we don't have any known issues, explore this rabbit trail, leave the work staged, I'll test and review with you when I get back later.
I know we use UDP, maybe we can take advtantage of this fact, or is it a given?

- Consider best practices, we are data driven functional with some use of objects where they suit us, I am wondering how we're doing meeting these goals? Leave a file in the repo root: CodeQualityReview.md

- Consider ECS architecture, does this fit with what we are doing, should we consider it in some refactor, does it help or hurt us, are we already doing this? Leave a file in the repo root: ECSShouldWeDoIt.md

## Future Tasks / Roadmap
- Persistent world storage and migrations
- Editor upgrades for world-graph, spawns, and content validation
- Asset pipeline for animation sets, atlases, and build-time validation

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
- runs a UDP handshake smoke test (server thread + client receive)
- env overrides: `MMORPG_NET_TEST_PORT`, `MMORPG_NET_TEST_SECONDS`

#### make smoke

- loads system
- runs server + client (mmorpg:run-server + mmorpg:run-client)
- opens a real client window briefly
- exits automatically
- env overrides: `MMORPG_NET_TEST_PORT`, `MMORPG_SMOKE_SECONDS`, `MMORPG_SMOKE_FRAMES`

#### make checkdocs

After adding new code files, there should be new docs files to match.

After refactoring or removing code, leftover references to the old code should always be cleaned, updated or fixed.

- Checks that every `src/foo.lisp` has a matching `docs/foo.md`, errors if any are missing, otherwise prints a friendly reminder when all pass.
