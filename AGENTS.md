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

**Remember to run `make checkparens` often while generating code. This tool will help you and is faster and more reliable than using python to analyze for missing parentheses.**

Proposed model

Zone remains the unit of edit/save/load (zone-1.lisp, zone-2.lisp, ...).
Add a “world graph” layer that defines connections between zones.
Each connection is an exit with:
from-zone id
edge or region (e.g., :north, or a rectangular boundary/portal)
to-zone id
spawn rule (where to place the player in the target zone)
Two flavors of exits (pick one, or mix)

Edge‑based transitions (simple, Runescape‑like)
You walk off the north edge of zone‑1, you load zone‑2 and spawn at its south edge.
Data: (:from :zone-1 :edge :north :to :zone-2 :spawn-edge :south :offset :preserve-x)
Portal/region transitions (for gates, doors, caves)
You define a rectangular region in zone‑1 that triggers a transition.
Data: (:from :zone-1 :rect (x y w h) :to :zone-2 :spawn (x y))
Runtime flow

On movement update, check if the player crosses any defined exit trigger.
If so:
Save dirty zone (optional auto‑save or explicit).
Load target zone.
Place player using the target spawn rule.
Update world bounds/collision.
Where to store the graph

Separate file world-graph.lisp for clarity, or embed in each zone under :exits (I’d lean separate so zones stay simple and reusable).
Editor could eventually place exits visually (future roadmap).
Questions to refine

Do you want transitions only on the four edges, or also explicit portals?

- Why don't we only make zone transitions on the 4 edges North, South, East, West, and then we can add portals later (they are def planned.)?

Should edges preserve the player’s X/Y offset (so it feels continuous), or always spawn at a fixed point?

- Yes, preserve. It should feel continuous when loading from one zone region in to another. One thing that might help also is if the HUD showed what zone we are currently in while walking (since we have not designed a minimap yet)

Do you want a single “world map” file, or per‑zone exits embedded in each zone?

- I am not certain, I do want it feel like Runescape while walking though from zone to zone, hopefully this will scale since we can kind of chunk it while walking? I will let you make the engineer decision on this.

Should transitions auto‑save dirty zones, or require a manual save?

- Likewise I am not sure yet, you decide please.

If you answer those, I can propose a concrete data schema and runtime checks that match your intent.


---

## Future Tasks / Roadmap

2) Add zone lifecycle tools: create, delete, rename, resize, and list zones.

3) Add spawn tables: editor placements for NPC/monster spawns, and runtime consumption.

4) Add a world graph (town nodes + path edges) that references zone files.

5) Cache static chunks into render textures for large-world rendering performance.

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
