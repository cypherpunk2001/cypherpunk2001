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

1. performance

Do this if it's not done already: per your SERVER_PERFORMANCE.md file.

+## Immediate Optimizations (Low-Hanging Fruit)
+
+### 1. Non-Blocking UDP Receive
+**Current**: Tight loop with blocking receive
+**Fix**: Add timeout to `usocket:wait-for-input`
+
+```lisp
+(when (usocket:wait-for-input socket :timeout 0.001 :ready-only t)
+  (receive-net-message socket recv-buffer))
+```

- I note that you say - Bottleneck would be network bandwidth, not CPU
but i'm probably going to run this on a monolith so if there's a way to make server run the server taking optional argument from nproc
to use my threads that would be helpful, if it's low hanging and smart?

- stage and commit anything at this point from above.

2. Go ahead and make sure you add an optional verbose mode to the client and the server runtimes. Leave the visual collision debug mode alone, that is useful for me to keep. But the other verbose mode is more verbose about coordinates so it should be a very particular verbose coordinates option. A brand new verbose mode needs to be created which is just standard that our server and client will log what is occuring in a very noisy way when this is enabled, to be the typical thing when we are chasing after gremlins later. So go through every line of code in the codebase and make sure that we are outputting additional useful information if we have this new verbose mode enabled.

stage and commit this option and update README.md about how to run server and client with make in optional verbose mode.

3. Similarly, we need to make sure that we have all possible exceptions covered, and determine as much as possible what should be a fatal exception vs what should not be a fatal exception, lets just do our best to follow best programming practices and cover our exceptions code-base-wide.

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
