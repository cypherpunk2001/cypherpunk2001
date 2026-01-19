# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Common Lisp + raylib MMORPG prototype with a clean client/server UDP architecture. The codebase emphasizes data-driven design, intent-based actions, server authority, and strict separation between game logic and rendering.

**Stack:** SBCL, Quicklisp, raylib, claw-raylib, usocket (UDP), cl-redis (Redis/Valkey persistence)

## Essential Commands

### Building and Testing

**CRITICAL: Before claiming any task is complete, ALL tests must pass:**

```bash
make checkparens        # Verify balanced parentheses in all .lisp files
make ci                 # Cold compile + UDP handshake test (no GPU needed)
make smoke              # Full client/server smoke test with window (2s default)
make test-persistence   # Data integrity tests (serialization, migrations, invariants)
make test-security      # Security tests (input validation, exploit prevention)
make checkdocs          # Verify docs/foo.md exists for each src/foo.lisp
```

**Never skip tests.** If you implement a feature but don't run all test targets, the work is incomplete.

### Stress Testing

Test server performance with headless clients:

```bash
make stress                           # 10 clients for 60 seconds (default)
STRESS_CLIENTS=50 make stress         # 50 clients for 60 seconds
STRESS_DURATION=300 make stress       # 10 clients for 5 minutes
STRESS_CLIENTS=100 STRESS_DURATION=120 make stress  # 100 clients for 2 minutes
```

Each headless client:
- Registers with unique username (`stress000001`, `stress000002`, ...)
- Authenticates via UDP
- Walks randomly (changes direction every 3 seconds)
- Sends movement intents every 100ms
- Receives and processes snapshots

Useful for:
- Finding server bottlenecks
- Testing concurrent connection limits
- Stress testing dirty flag batching
- Profiling CPU/memory usage under load

**Note:** Start server separately before running stress test, or it will timeout trying to connect.

### When to Write Tests

**Write tests for:**
- ✅ **Data corruption risk**: Serialization, migrations, database writes
- ✅ **Invariants**: Things that MUST always be true (HP ≤ max, gold ≥ 0)
- ✅ **Backend equivalence**: Storage abstraction must work identically across backends
- ✅ **Player-facing bugs**: Anything that loses progress, duplicates items, corrupts saves

**Skip tests for:**
- ❌ **Visual bugs**: Rendering, animations, UI layout (manual testing fine)
- ❌ **Input handling**: Mouse clicks, keyboard (already tested via smoke test)
- ❌ **Gameplay feel**: AI behavior, movement smoothness, combat balance
- ❌ **Helper functions**: Utils that don't touch persistent state

**Rule of thumb**: If a bug loses player progress or corrupts their save, write a test. If it's just annoying or ugly, manual testing is fine.

**Test location**: `tests/persistence-test.lisp` for data integrity tests.

### Proactive Test Writing Requirements (For Claude)

**CRITICAL**: When implementing new features, you MUST write accompanying tests if ANY of these conditions are met:

**Auto-write tests when:**
1. **Adding persistent fields to player/NPC/world structs**
   - Write round-trip serialization test
   - Verify field classified as durable or ephemeral
   - Test that durable fields survive save/load, ephemeral fields don't

2. **Implementing schema migrations**
   - Write migration test with old-version plist
   - Verify defaults applied correctly for new fields
   - Test migration chain (v1→v2→v3)

3. **Adding new persistence tiers or save mechanisms**
   - Write test comparing behavior to existing tier
   - Verify atomic/non-corrupt behavior
   - Test both storage backends (memory + Redis)

4. **Implementing economy/progression systems**
   - Write invariant tests (currency ≥ 0, XP never decreases)
   - Test edge cases (overflow, underflow, max values)
   - Verify transaction atomicity

5. **Adding inventory/equipment/trade systems**
   - Write tests for duplication exploits
   - Test item loss scenarios
   - Verify count limits enforced

6. **Modifying zone transition or respawn logic**
   - Test player state preserved across transitions
   - Verify no item/progress loss on death/respawn
   - Test edge case: transition during combat/trade

**How to implement:**
- Add test to `tests/persistence-test.lisp` BEFORE claiming feature complete
- Run `make test-persistence` to verify test passes
- Update this list if you discover new test-worthy patterns

**When NOT to auto-write tests:**
- Rendering changes (colors, positions, animations)
- UI layout adjustments
- Sound/music changes
- AI behavior tuning (aggro range, wander patterns)
- Performance optimizations that don't change behavior
- Refactoring that doesn't touch persistence

**If uncertain:** Ask yourself: "If this breaks, will players lose progress or items?" If yes → write a test.

## Code Quality Checklist (MANDATORY)

**Review EVERY change against these criteria before claiming work complete:**

### 1. Tests Written?
- **Persistent state changes** → Write test (XP, HP, inventory, position, level)
- **New invariants** → Write test (HP ≤ max, coins ≥ 0, XP never decreases)
- **Database operations** → Write test (save/load round-trip, backend equivalence)
- **Schema changes** → Write migration test
- **Visual/UI only** → Skip test (manual verification fine)

### 2. Retry Logic Added?
- **Tier-1 saves (death, level-up)** → Use `with-retry-exponential` 5 retries, 100-500ms
- **Database reads on critical paths (login, load-player)** → Use `with-retry-exponential` 3 retries, 50-200ms
- **Auth UDP messages** → Use `with-retry-linear` 3 retries, 50ms delay
- **Zone/asset loading** → Use `with-retry-exponential` 2 retries, 100-200ms
- **Snapshots, UI updates, best-effort operations** → No retry (fail silently acceptable)

### 3. Logging Added?
**CRITICAL: Any logging we develop that helps us solve problems together should always remain in verbose mode to help us again in the future.**
- **Critical failures** → `(warn "...")` always
- **Tier-1 save failures** → `(warn "CRITICAL: ...")` + fallback
- **State transitions** → `(log-verbose "...")` when `*verbose*` enabled
- **Network events** → `(log-verbose "...")` for auth, connects, disconnects
- **Hot loops (update-sim, draw)** → No logging (use `*verbose-coords*` flag if needed)
- **Helper functions** → No logging unless they fail

### 4. Variable Scope Correct?
**Globals (`defparameter`) - Use for:**
- Configuration: `*net-buffer-size*`, `*inventory-size*`
- Server state: `*storage*`, `*player-sessions*`, `*active-sessions*`
- Feature flags: `*verbose*`, `*debug-npc-logs*`

**Locals (`let`, `let*`) - Use for:**
- Temporary computation results
- Loop variables
- Function parameters

**NEVER:**
- Global mutable game state that belongs in structs (use `game-players`, `world-npcs`, not `*global-players*`)
- Globals for what should be function parameters
- `setf` on function parameters (pass values, return new values)

### 5. CLOS/Data-Driven Design Consistent?
**Data-driven (good):**
- Game data in `data/*.lisp` files (zones, NPCs, items, world-graph)
- Behavior driven by data fields (npc-archetype, equipment-modifiers)
- Generic functions dispatch on struct types (`combatant-apply-hit`)

**Hard-coded (bad):**
- Special-case player ID checks in game logic
- NPC behavior in main loop instead of ai.lisp
- Item effects inline instead of data-driven

**Modularity check:**
- Can this code work without rendering? (game logic must)
- Does this function do ONE thing? (not save + update + log + validate)
- Could NPCs use this too? (if yes, make it generic, not player-only)

**Before claiming complete, verify:**
- [ ] Tests written (if persistent state touched)
- [ ] Retry logic added (if critical operation can fail)
- [ ] Logging added (if failure is critical or debugging needed)
- [ ] No new globals (unless config/server state)
- [ ] Data-driven, not hard-coded

### Running Client/Server
Server must start first. Uses UDP port 1337 by default.

```bash
make server         # Start server
make client         # Start client (in separate terminal)

# Environment variable overrides
MMORPG_VERBOSE=1 make server                    # Enable diagnostic logging
MMORPG_VERBOSE_COORDS=1 make server             # Log entity positions (very verbose)
MMORPG_WORKER_THREADS=4 make server             # Parallel snapshot sends (default: 1)
MMORPG_WORKER_THREADS=$(nproc) make server      # Use all CPU cores

# Database backend configuration (Redis is default)
MMORPG_DB_BACKEND=memory make server            # Use in-memory storage (for testing without Redis)
MMORPG_REDIS_HOST=127.0.0.1 make server         # Redis host (default: 127.0.0.1)
MMORPG_REDIS_PORT=6379 make server              # Redis port (default: 6379)
```

### Development with SLIME
Use two separate Emacs sessions (server + client):

```lisp
;; Server REPL
(ql:register-local-projects)
(ql:quickload :mmorpg)
(mmorpg:run-server :host "127.0.0.1" :port 1337)

;; Client REPL (separate session)
(ql:register-local-projects)
(ql:quickload :mmorpg)
(mmorpg:run-client :host "127.0.0.1" :port 1337)
```

## Architecture Overview

### Load Order (mmorpg.asd)
Files load in dependency order:
1. **package, config, utils** - Foundation
2. **data, intent, types** - Data structures and protocols
3. **progression, save, migrations, db** - Game systems, serialization, and persistence
4. **zone, world-graph, movement** - World and navigation
5. **input, combat, chat, ai** - Game mechanics
6. **audio, ui, editor, rendering** - Presentation layer
7. **server, main, net** - Entry points and networking

### Core Architectural Patterns

#### Client/Server Split (UDP)
- **Server**: Authoritative simulation. Receives intents from clients, runs fixed-tick game loop, broadcasts snapshots.
- **Client**: Rendering and input. Sends intents to server, applies snapshots for display.
- **Message Format**: ASCII plists (`:type :hello`, `:type :intent`, `:type :snapshot`, `:type :save`, `:type :load`)
- **Snapshot Optimization**: State serialized once per frame, shared across all clients. Optional parallel sends via worker threads.

#### Intent-Based Actions
Clients never send state - only intent (what they want to do). Server validates and applies changes.
```lisp
;; Client sends intent
(:move-dx 1.0 :move-dy 0.0 :attack t)

;; Server validates, updates state, broadcasts result
```

#### Serialization Strategy
- **save.lisp**: Game state ↔ plist conversion
- **Database saves**: Exclude ephemeral fields (attack timers, targets, run stamina)
- **Network snapshots**: Include visual fields (`:include-visuals t`) for rendering
- **Versioning**: All persistent data includes `:version N` for migrations

#### Storage Abstraction (CRITICAL)
**Game code MUST NOT directly call Redis or any database client.**

```lisp
;; WRONG - direct database access
(redis:set "player:123" data)

;; CORRECT - use storage abstraction
(db-save-player player)
(storage-save *storage* key data)
```

All persistence goes through `src/db.lisp`. Supports memory (testing/dev) and Redis (production) backends via environment variables.

#### Persistence Tiers
| Tier | When | Examples | Implementation |
|------|------|----------|----------------|
| **Tier 1: Immediate** | Before ACK to client | Death, level-up, trade, bank | `db-save-player-immediate` |
| **Tier 2: Batched** | Every 30s checkpoint | HP/XP/position changes | `mark-player-dirty` + `flush-dirty-players` |
| **Tier 3: Logout** | Session end | Final save + cleanup | `db-player-logout` |

**Dirty flag system**: Session tracking with `register-player-session`, `mark-player-dirty`, periodic batch flushes.

### Data Classification

When adding features, classify ALL new state as **durable** or **ephemeral**:

**Durable (must persist to DB):**
- XP, levels, stats, HP, inventory, equipment, currency, position, zone-id, quests

**Ephemeral (OK to lose on crash/logout):**
- Attack/follow targets, combat timers, animation frames, AI state, buffs/debuffs, trade windows

Add durable fields to `serialize-player` (base payload), add ephemeral fields only to `:include-visuals` section.

### Zone System
- **Zone data**: `data/zones/*.lisp` - tile layers, collision, spawns, objects
- **World graph**: `data/world-graph.lisp` - zone connections and transitions
- **Multi-zone**: Players carry zone-id; transitions update session zone and trigger dirty flag
- **Scaling**: Each server process runs ONE zone. For 10k users @ 500/zone = 20 server processes (horizontal scaling).

### Update/Draw Separation
- **update-sim**: Fixed timestep game logic (combat, AI, movement). No rendering dependencies.
- **draw-game**: Rendering only. Reads state, never modifies it.
- Server runs only update-sim. Client runs both.

## Memory vs Redis Storage

**Redis Storage** (`:backend :redis`, **default**):
- Persists to disk via Valkey/Redis (AOF + RDB)
- Survives restarts/crashes
- Requires Valkey running on port 6379
- This is the default for `make server` - dev close to production

**Memory Storage** (`:backend :memory`, for CI/testing only):
- Hash table in RAM, lost on shutdown
- No external dependencies
- Use via `MMORPG_DB_BACKEND=memory make server`

**Dev Workflow**: Use Redis by default (matches production). Use `:memory` only for CI tests or when Redis is unavailable.

## Schema Migrations (CRITICAL)

**Location**: `src/migrations.lisp` (core logic), `src/db.lisp` (admin command)

### When You MUST Write a Migration

**Write a migration when adding/changing DURABLE fields:**
- Adding a new persistent field to player struct (e.g., `lifetime-xp`, `quest-progress`)
- Changing the type or structure of an existing field
- Renaming a field (migration copies old → new, preserves data)
- Splitting or merging fields

**You do NOT need a migration for:**
- Adding ephemeral fields (attack timers, animation state, targets)
- Adding new item types to `game-data.lisp` (data-driven, no schema change)
- Adding new zones (data files, not player schema)
- Changing game logic that doesn't affect saved data format

### Current Schema

```lisp
*player-schema-version* = 3

*player-migrations* = '((2 . migrate-player-v1->v2)
                        (3 . migrate-player-v2->v3))
;; v1→v2: Added lifetime-xp field (default 0)
;; v2→v3: Added playtime (default 0) and created-at (default current time)
```

### How Migrations Work

1. **Lazy migration (default)**: When player logs in, `db-load-player` calls `migrate-player-data`
2. Migration chain runs: v1→v2→v3→... until current version
3. Migrated data saved on next write (dirty flag or logout)

**Eager migration (admin command)**:
```lisp
;; Preview what would migrate (dry run)
(migrate-all-players :dry-run t :verbose t)

;; Actually migrate all players before deploy
(migrate-all-players :verbose t)
```

### Writing a New Migration (Step-by-Step)

```lisp
;; 1. INCREMENT VERSION in src/migrations.lisp
(defparameter *player-schema-version* 3)  ; was 2

;; 2. WRITE MIGRATION FUNCTION in src/migrations.lisp
(defun migrate-player-v2->v3 (data)
  "v2->v3: Add quest-log field, defaulting to empty list."
  (unless (getf data :quest-log)
    (setf (getf data :quest-log) nil))
  data)

;; 3. REGISTER MIGRATION in src/migrations.lisp
(defparameter *player-migrations*
  '((2 . migrate-player-v1->v2)
    (3 . migrate-player-v2->v3))  ; ← add new entry
  "Alist of (version . migration-function)")

;; 4. UPDATE STRUCT in src/types.lisp
(defstruct player ... quest-log ...)
;; And in make-player: :quest-log nil

;; 5. UPDATE SERIALIZATION in src/save.lisp
;; serialize-player: add :quest-log (player-quest-log player)
;; deserialize-player: add (setf (player-quest-log p) (getf plist :quest-log nil))
;; apply-player-plist: add quest-log handling

;; 6. WRITE TESTS in tests/persistence-test.lisp
(defun test-migration-v2-to-v3 ()
  (let* ((old-data '(:version 2 :id 1 :x 0.0 :y 0.0 ...))
         (migrated (migrate-player-data old-data)))
    (assert (= (getf migrated :version) 3))
    (assert (null (getf migrated :quest-log)))))
```

### Migration Rules (NEVER VIOLATE)

| Rule | Why |
|------|-----|
| **Never delete old migrations** | Player at v1 must migrate through v2, v3, v4... |
| **Migrations must be pure** | Take plist, return plist, no side effects |
| **Always provide defaults** | Missing fields get sensible defaults |
| **Never modify existing migrations** | Would break players mid-migration |
| **Test migration chain** | v1→v3 must work (runs v1→v2, v2→v3) |
| **Append only to registry** | New migrations go at end of `*player-migrations*` |

### Migration Checklist (For Claude)

Before marking a feature complete that adds persistent state:

- [ ] Did I add a new durable field? → Write migration
- [ ] Incremented `*player-schema-version*`?
- [ ] Added migration function `migrate-player-vN->vN+1`?
- [ ] Registered in `*player-migrations*` alist?
- [ ] Updated `serialize-player` in save.lisp?
- [ ] Updated `deserialize-player` in save.lisp?
- [ ] Updated `apply-player-plist` in save.lisp?
- [ ] Added field to struct in types.lisp?
- [ ] Wrote migration test in persistence-test.lisp?
- [ ] Ran `make test-persistence` and all pass?

## Code Style Rules

### Modular & Reusable by Default

We are building reusable game systems, not one-off demo code.

**Bias:** Write small, composable functions, CLOS objects, and data-driven systems that can be reused by players, NPCs, and future worlds.

**Rules:**
- No gameplay logic in the main loop — it only orchestrates systems
- Entities hold data; systems implement behavior
- Rendering is separate from game logic
- Avoid hardcoded values that may vary later
- If it could apply to NPCs, write it generically now

**Agent Self-Check** - Before outputting code, ensure:
- Reusable beyond a single entity
- Logic works without rendering
- Behavior is not special-cased
- Future client/server split wouldn't break it
- Any new state is classified as durable or ephemeral
- No direct database calls from game logic (use storage abstraction)

If unsure, refactor toward reuse.

### Plist Mutation: The `setf getf` Pitfall (CRITICAL)

In Common Lisp, `setf getf` **only modifies existing keys** - it does NOT add new keys to a plist. The modification is silently ignored with no error.

```lisp
;; THIS SILENTLY FAILS - key doesn't exist
(let ((plist '(:name "arrow")))
  (setf (getf plist :respawn) 5.0)
  plist)
;; => (:name "arrow")  ;; :respawn was NOT added!
```

**Prevention Rules:**

1. **Always initialize mutable plist keys** when creating plists:
   ```lisp
   ;; BAD - missing keys that will be setf'd later
   (list :id id :x x :y y)

   ;; GOOD - all mutable keys present
   (list :id id :x x :y y :count nil :respawn 0.0 :dirty nil)
   ```

2. **Use structs for complex mutable state** - structs don't have this problem:
   ```lisp
   (defstruct zone-object id x y count respawn dirty)
   (setf (zone-object-respawn obj) 5.0)  ;; Always works
   ```

3. **Add assertions during debugging** to catch missing keys:
   ```lisp
   (assert (member :respawn object) ()
           "Object missing :respawn key - was it initialized?")
   ```

4. **Document required keys** for plist-based features (which are mutable).

**See `docs/PLIST_SETF_GETF_PITFALL.md` for the full debugging story.**

### Security: Untrusted Client Principle
```lisp
;; WRONG - trusting client claims
(setf (player-gold player) (intent-claimed-gold intent))

;; CORRECT - server calculates truth
(let ((sale-price (calculate-sale-value item)))
  (incf (player-gold player) sale-price))
```

Always use `*read-eval* nil` when deserializing data.

### Versioned Serialization
All persistent data includes `:version N`. When changing schema:
1. Increment `*player-schema-version*`
2. Write migration function `migrate-player-vN->vN+1`
3. Add to migrations list
4. NEVER delete old migrations (players skip versions)
5. Test on copy of production data

### Performance Matters

**Hot loop optimization:**
- Avoid per-frame consing in hot loops: reuse rectangles, vectors, strings, and animation state
- Keep entity data in arrays/structs, not lists; use object pools
- Separate update/draw; keep animation state lightweight

**Rendering optimization:**
- Cull off-screen tiles/sprites; draw only what's visible
- Chunk the map (e.g., 32×32 tiles) and cache static chunks in a render texture
- Batch draw calls when possible

## Key Files and Their Roles

| File | Purpose |
|------|---------|
| **net.lisp** | UDP client/server, message protocol, snapshot streaming |
| **db.lisp** | Storage abstraction, Redis/memory backends, dirty flag system, `migrate-all-players` |
| **migrations.lisp** | Schema versions, migration functions, `migrate-player-data` |
| **save.lisp** | Game state serialization (durable vs ephemeral classification) |
| **server.lisp** | Server-only game loop (no rendering), fixed timestep simulation |
| **main.lisp** | Client entry point with rendering |
| **movement.lisp** | Physics, collision, zone transitions |
| **combat.lisp** | Damage, HP, death, tier-1 immediate saves |
| **progression.lisp** | XP, levels, inventory, equipment, tier-2 dirty marking |
| **ai.lisp** | NPC behaviors (idle, wander, aggressive, flee, retaliate) |
| **zone.lisp** | Zone loading, tile layers, collision maps |
| **world-graph.lisp** | Zone connections, edge transitions |

## Common Development Patterns

### Adding a New Persistent Field
```lisp
;; 1. Add to player struct (src/types.lisp)
(defstruct player ... new-field ...)

;; 2. Classify: durable or ephemeral?

;; 3a. If DURABLE - add to serialize-player base payload
(defun serialize-player (player &key ...)
  (list :id ... :new-field (player-new-field player) ...))

;; 3b. If EPHEMERAL - add only to :include-visuals section
(when include-visuals
  (append payload (list :new-field (player-new-field player))))

;; 4. If durable - mark dirty when changed
(setf (player-new-field player) value)
(mark-player-dirty (player-id player))

;; 5. Update deserialize-player
(setf (player-new-field player) (getf plist :new-field default))
```

### Triggering a Save
```lisp
;; Tier 1: Immediate (critical, irreversible)
(db-save-player-immediate player)

;; Tier 2: Batched (routine state changes)
(mark-player-dirty (player-id player))
;; Server auto-flushes dirty players every 30s
```

### Handling Zone Transitions
```lisp
;; movement.lisp does this automatically, but if manual:
(update-player-session-zone (player-id player) new-zone-id)
(mark-player-dirty (player-id player))
```

## Testing Redis Persistence

1. Start Valkey: `sudo systemctl start valkey`
2. Configure AOF: `appendonly yes` in `/etc/valkey/valkey.conf`
3. Run server: `make server` (Redis is default)
4. Test crash recovery:
   - Create character, gain XP, move around
   - Kill server (Ctrl+C)
   - Restart server
   - Reconnect client - verify HP/XP/position/inventory intact

## Performance Notes

- **Current scale**: Tested smooth with hundreds of NPCs on client
- **Server capacity**: ~500 concurrent users per zone per server process
- **Horizontal scaling**: For 10k users @ 500/zone = 20 separate server processes
- **Worker threads**: Use `MMORPG_WORKER_THREADS=$(nproc)` for parallel snapshot sends (high client counts)
- **Snapshot optimization**: State serialized once per tick, shared across clients (efficient)

See `docs/SERVER_PERFORMANCE.md` for detailed scaling strategies.

## Documentation

Every `src/foo.lisp` must have a matching `docs/foo.md`. Run `make checkdocs` to verify.

Key design docs:
- `docs/db.md` - Persistence architecture, storage abstraction, write tiers
- `docs/migrations.md` - Schema versioning, migration functions, admin commands
- `docs/save.md` - Serialization format, durable vs ephemeral classification
- `docs/net.md` - UDP protocol, message format, snapshot streaming
- `docs/SERVER_PERFORMANCE.md` - Scaling strategies, bottleneck analysis
- `docs/movement.md` - Physics, collision, zone transitions

## Important Reminders

- **ALL TESTS MUST PASS**: Before claiming work complete, run ALL test targets in order: `make checkparens && make ci && make test-persistence && make test-security && make checkdocs && make smoke`. No exceptions.
- **Never commit with unbalanced parens**: Run `make checkparens` before committing
- **CI must pass**: `make ci` runs cold compile + UDP handshake test
- **Data integrity tests must pass**: `make test-persistence` ensures no save corruption
- **Docs must be complete**: `make checkdocs` verifies every src file has matching documentation
- **Smoke test must work**: `make smoke` tests actual client/server with graphics
- **Storage abstraction is mandatory**: Never call Redis/database directly from game logic
- **Classify all new state**: Every field must be marked durable or ephemeral
- **Use correct persistence tier**: Tier-1 for critical, tier-2 for routine, tier-3 for logout
- **Write migrations for durable fields**: New persistent fields require schema version bump + migration function
- **Server is authoritative**: Clients send intents, not state
- **Test both backends**: Memory for dev, Redis for persistence testing
