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
~~Real client/server split (server-authoritative simulation, snapshot sync)~~ DONE

Now that we have working UDP with make server and make client with a save/load working...

- let's ensure the server correctly handles SIGTERM because I Control-C'd it earlier and threw scary errors but I think it was fine, but if there's a proper a shutdown mechanism let's build it. It'd be wise to do that before we have serious database things and so forth.

- let's do what it takes to ensure multiple clients can connect simultaneously and stay updated.

Currently,

make server #term 0

make client #term 1
make client #term 2

The result here is that the same character shows in both clients. Your choice, this might be a good first test, do the terminals track with each other?

But I'm wondering if this could be causing some kind of infinite crazy server loads with two clients yelling at the server simultaneously about the same exact character?

I notice that it does track the active client i am controlling the character with, but it's definitely not smooth the second one just teleports to the correct place every few seconds. A real test I think would be to introduce a second character id maybe and see if I can play with them together smoothly in two windows? Can you arrange this and tell me how to do it?


## Future Tasks / Roadmap

### Long-term (MMO readiness)
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
