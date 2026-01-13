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

- Define a Lisp data format for zones/chunks (chunked tile data with collision layers) that we can load directly.


---

## Future Tasks / Roadmap

0.5) the Floor tile randomization autogeneration code can be very much simplified because we are about to create 1.) interactive map editor, so clean this code up before proceeding to 1.

1) Build an interactive map editor that exports to our custom map format.
   This is to be an interactive in-game map editor (Minecraft-like) that exports our custom map format (accessible from ESC), and saves the custom maps to the data/ location.
   (we can lock it down so players cant modify maps later on.)
Intent:
- This is a *playable* editor mode where you move around the world camera/player and paint tiles/objects live.
- The editor should require no authoring tools beyond an assets folder of PNGs.
Core UX:
- Enter “Editor Mode” in-game.
- Navigate around the world freely (Minecraft-ish: pan/scroll/wasd, zoom optional).
- Select a tile from our PNG tile palette and “paint” it onto the world.
- Place and remove objects (e.g., houses, props) from an object palette.
- Changes are visible immediately in the running game.
- When satisfied, export to our Common Lisp metadata format.
Assets-driven authoring:
- Tiles and placeable objects are discovered from the assets folder (PNG-based).
- No external map files, no hand-written metadata required to start building.
- Each tile/object corresponds to an asset identifier (derived from filename or manifest).
World / Zones model:
- Treat the edit space as one large conceptual world you can roam around.
- Persistence/export is done in *zones* (and/or chunks) rather than one monolithic file.
- The editor supports selecting a zone region and exporting it as a zone file.
- Runtime loads zones (and their chunked tile + collision layers) as the player enters them.
Export format requirement:
- Export output is our custom Lisp data format for zones/chunks.
- Zone files contain chunked tile layers + collision layers + object placements.
- Loading the game loads the current zone(s); editing updates the zone data and can be re-exported.

2) Add a world graph (town nodes + path edges) that references zone files.

3) Cache static chunks into render textures for large-world rendering performance.

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
