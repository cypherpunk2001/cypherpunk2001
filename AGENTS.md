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



## Future Tasks / Roadmap
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
- future client/server split wouldn't break it
- any new state is classified as durable or ephemeral (see Persistence Guidelines)
- no direct database calls from game logic (use storage abstraction)

If unsure, refactor toward reuse.

---

## Persistence Guidelines (see docs/db.md and docs/save.md)

All code that touches persistent state MUST follow these principles.

### Storage Abstraction (MANDATORY)

**Game code MUST NOT know what database it's using.**

```lisp
;; WRONG - game code calling Redis directly
(redis:with-connection ()
  (red:set "player:123" data))

;; CORRECT - game code uses abstract interface
(db-save-player player)
(storage-save *storage* key data)
```

When implementing persistence features:
- Use the storage protocol defined in db.md (`storage-load`, `storage-save`, etc.)
- Never import or call cl-redis (or any DB client) from game logic files
- All DB access goes through `src/db.lisp` (or equivalent abstraction layer)

### Data Classification (MANDATORY)

Every piece of state MUST be explicitly classified as **durable** or **ephemeral**.

**Durable (MUST persist):**
- Progression: XP, levels, skill levels, combat stats
- Health: Current HP (prevents logout-heal exploit)
- Inventory: Items, equipment, stack counts
- Currency: Gold, bank contents
- Position: Zone ID, X/Y coordinates
- Quests, Achievements, Social data, Settings

**Ephemeral (OK to lose):**
- Temporary buffs, debuffs, cooldowns
- Current attack/follow target
- AI state, pathfinding cache
- Animation frame, visual effects
- Party invites, trade windows in progress

**When adding new features:**
1. Identify all new state fields
2. Classify each as durable or ephemeral
3. Add durable fields to serialization
4. Document in `*durable-player-fields*` or `*ephemeral-player-fields*`

### Write Tiers (MANDATORY)

Not all writes are equal. Use the correct tier:

| Tier | When | Examples |
|------|------|----------|
| **Tier 1: Immediate** | Before ACK to client | Trade, bank, death, level-up, item destruction |
| **Tier 2: Batched** | Every 30s checkpoint | XP gains, HP changes, position, quest progress |
| **Tier 3: Logout** | Session end | Final snapshot, cleanup |

**Rule:** If a player would be upset losing it, and it's irreversible, it's Tier 1.

### Versioned Serialization (MANDATORY)

All persistent records MUST include a version number:

```lisp
(:version 1
 :player-id 12345
 :hp 85
 ...)
```

**When changing the schema:**
1. Increment `*player-schema-version*` (or equivalent)
2. Write a migration function `migrate-player-vN->vN+1`
3. Add to `*player-migrations*` list
4. NEVER delete old migration code (players skip versions)
5. Test migration on copy of production data

### Security (MANDATORY)

**Untrusted client principle:**
```lisp
;; WRONG - persisting client-provided values
(setf (player-gold player) (intent-claimed-gold intent))

;; CORRECT - server calculates, then persists
(let ((sale-price (calculate-sale-value item)))
  (incf (player-gold player) sale-price))
```

**Serialization safety:**
```lisp
;; ALWAYS use *read-eval* nil when reading persisted data
(let ((*read-eval* nil))
  (read-from-string stored-data))
```

### Exploit Prevention

**HP must be durable** to prevent logout-heal:
- Player at 10 HP cannot logout and return at full health
- HP changes are Tier 2 (batched)
- Death (HP = 0) is Tier 1 (immediate)

**Combat disconnect:**
- If player disconnects during combat, persist combat state
- On reconnect, resume where they left off (no free escape)

### Agent Self-Check for Persistence

Before outputting code that touches game state, verify:
- [ ] Is this state durable or ephemeral? (classified explicitly)
- [ ] Am I using the storage abstraction? (no direct DB calls)
- [ ] What write tier is appropriate? (Tier 1/2/3)
- [ ] Does the serialization format have a version?
- [ ] Am I trusting client data? (never persist client claims)
- [ ] Am I using `*read-eval* nil` when reading? (always)

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
