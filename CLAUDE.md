# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Common Lisp + raylib MMORPG prototype with a clean client/server UDP architecture. The codebase emphasizes data-driven design, intent-based actions, server authority, and strict separation between game logic and rendering.

**Stack:** SBCL, Quicklisp, raylib, claw-raylib, usocket (UDP), cl-redis (Redis/Valkey persistence)

## Essential Commands

### Building and Testing

**CRITICAL: Before claiming any task is complete, ALL tests must pass:**

```bash
make tests              # Run ALL tests including smoke test (recommended)
```

**CRITICAL TEST ORDER**: The first three tests MUST run in this exact order:
1. `make checkparens` - Verify balanced parentheses (fastest, catches syntax errors)
2. `make ci` - Cold compile + UDP handshake (catches compile errors)
3. `make smoke` - Full client/server smoke test (catches runtime errors early)

This order ensures we fail fast on basic errors before running slower tests.

Individual test targets (run by `make tests` in this order):
```bash
make checkparens        # 1st - Verify balanced parentheses in all .lisp files
make ci                 # 2nd - Cold compile + UDP handshake test (no GPU needed)
make smoke              # 3rd - Full client/server smoke test with window (2s default)
make test-unit          # All tests (unit, persistence, security, trade) in single file
make checkdocs          # Verify docs/foo.md exists for each src/foo.lisp
```

**Test consolidation:** All test types (unit, persistence, security, trade) are now in a single file `tests/unit-test.lisp` and run via `make test-unit` for simplicity.

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

**POLICY: If a unit test CAN be written, it SHOULD be written.**

We aim for 99% test coverage. Tests prevent regressions and catch bugs early. Every testable function deserves a test.

**Write tests for:**
- ✅ **All pure functions**: Functions that take input and return output without side effects
- ✅ **Game logic**: Combat calculations, XP/leveling, inventory operations, movement physics
- ✅ **Data transformations**: Serialization, migrations, protocol encoding/decoding
- ✅ **Invariants**: Things that MUST always be true (HP ≤ max, gold ≥ 0, valid state transitions)
- ✅ **Edge cases**: Boundary conditions, empty inputs, max values, error paths
- ✅ **AI behavior**: Decision-making logic, state transitions, target selection
- ✅ **Utility functions**: All helpers in utils.lisp, data.lisp, etc.
- ✅ **Protocol handling**: Message parsing, validation, response generation

**Skip tests ONLY for:**
- ❌ **Rendering code**: Functions that call raylib directly (require GPU)
- ❌ **Audio code**: Functions that play sounds (require audio device)
- ❌ **Interactive input**: Real-time mouse/keyboard handling (tested via smoke test)

**Rule of thumb**: If a function has logic that could break, write a test. The only exception is code that requires hardware (GPU, audio, input devices).

**Test locations**:
- `tests/persistence-test.lisp` - Data integrity, serialization, migrations
- `tests/security-test.lisp` - Input validation, exploit prevention
- `tests/unit-test.lisp` - Pure functions, game logic, utilities
- `tests/trade-test.lisp` - Trade system, player-to-player trading

### Proactive Test Writing Requirements (For Claude)

**CRITICAL**: When implementing ANY new code, you MUST write accompanying tests.

**The rule is simple: If you can test it, test it.**

**For every new function, ask:**
1. Can I call this function in isolation? → Write a unit test
2. Does it transform data? → Test input/output pairs
3. Does it have edge cases? → Test boundaries (0, 1, max, empty, nil)
4. Can it fail? → Test error conditions
5. Does it interact with state? → Test state before/after

**Test categories (all in `tests/unit-test.lisp`):**

| Test Category | What to Test |
|---------------|--------------|
| Unit Tests | Pure functions, utilities, game logic, AI decisions |
| Persistence Tests | Serialization, migrations, database operations |
| Security Tests | Input validation, exploit prevention, auth |
| Trade Tests | Trade system, offer/accept flow, gold/item validation |

All test categories are consolidated into a single file for simplicity.

**How to implement:**
1. Write the test BEFORE or ALONGSIDE the implementation
2. Add your test function to the appropriate section in `tests/unit-test.lisp`
3. Run `make test-unit` to verify
4. If a function is hard to test, refactor it to be testable (extract pure logic)

**When NOT to write tests (the ONLY exceptions):**
- Code that directly calls raylib (rendering, textures, fonts)
- Code that directly plays audio
- Code that reads real-time input devices
- Top-level entry points (main, run-server, run-client)

**If uncertain:** Write the test. It's always better to have a test you might not need than to skip a test you did need.

## Code Quality Checklist (MANDATORY)

**Review EVERY change against these criteria before claiming work complete:**

### 1. Tests Written?
- **Any new function with logic** → Write test (if it can break, test it)
- **Pure functions** → Write unit test with input/output pairs
- **State-changing functions** → Test before/after state
- **Edge cases** → Test boundaries (0, 1, max, empty, nil)
- **Only skip tests for**: Direct raylib calls, audio playback, real-time input

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

### Network Data Classification (CRITICAL for Scale)

When adding features, classify ALL new state for network transmission:

**Public Snapshots (60 Hz broadcast to ALL clients in zone):**
- What other players NEED TO SEE about you
- Position (x, y), velocity (dx, dy)
- HP (for health bars above heads)
- Animation state, facing direction
- Combat state (attacking, hit animation)
- Visual timers (animation sync)
- **Size budget: ~64-80 bytes per player** (keep this small!)

**Private State (sent only to owner, only when dirty flag set):**
- What YOU need to know about YOURSELF
- Inventory contents, equipment slots
- Detailed stats (strength, dexterity, intelligence, etc.)
- Gold, currency
- **No bandwidth impact on public snapshots**

**Load-Once Data (sent on login or zone change only):**
- What rarely or never changes
- Zone data (tiles, collision, objects)
- Friends list, guild roster
- Quest journal
- Skill tree unlocks
- **Zero ongoing bandwidth**

#### Design Rules for New Features

| Feature Type | Where It Goes | Example |
|--------------|---------------|---------|
| **Visual effects others see** | Public snapshot | Buff icon above head (+1-2 bytes) |
| **Character progression** | Private state, mark dirty on change | Level up, skill points |
| **UI state** | Client-only, no sync | Minimap zoom, UI panel visibility |
| **Static game data** | Load once on startup | Item definitions, NPC archetypes |
| **Skill cooldowns** | Client tracks locally after `:skill-used` confirm | No snapshot overhead |
| **Chat messages** | Separate `:type :chat` message | Not in snapshots |

#### Anti-Patterns (These Break Scale)

**BAD - Adding to 60 Hz snapshot when not needed:**
```lisp
;; Don't add inventory to public snapshots
#(id x y hp ... item1 item2 item3 ...)  ; Everyone sees your inventory 60 times/sec
```

**GOOD - Use private state channel:**
```lisp
;; Server marks inventory dirty when changed
(mark-player-inventory-dirty player)
;; Sent once via :private-state message to owner only
```

**BAD - Syncing client-tracked state:**
```lisp
;; Don't send skill cooldowns in snapshot
#(id x y hp ... skill1-cd skill2-cd skill3-cd ...)
```

**GOOD - Server confirms, client tracks:**
```lisp
;; Server confirms skill use once
(send-net-message socket '(:type :skill-used :skill-id 5 :cooldown 10.0) ...)
;; Client decrements timer locally (no 60 Hz sync needed)
```

**Rule of thumb:** If it doesn't affect how you LOOK to other players, it doesn't belong in public snapshots.

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
- [ ] Wrote migration test in unit-test.lisp (Persistence Tests section)?
- [ ] Ran `make test-unit` and all pass?

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

# AGENTS.md — Ultimate SBCL Common Lisp Codegen + Performance Rules
**Target runtime:** SBCL
**Target behavior:** stable 60 Hz simulation + rendering + networking with predictable GC and low allocation.

This document is written for LLM agents generating or modifying code.
Follow it literally. Do not “simplify” away details.

---

## 0) Prime Directive
Any code that runs:
- every tick/frame
- per packet
- per entity
- per client

**must be allocation-free, type-stable, and bounded-time.**
If you can’t prove it, assume it violates the directive.

---

## 1) Performance Matters

### Tick budget discipline (60 Hz)
- At 60 Hz, every tick/frame has a fixed budget (~16.67ms).
- Any periodic pause (GC, serialization spikes) will surface as jitter/stutter.
- Therefore: any work that runs every tick must be strictly bounded in time and allocation behavior.

### Hot loop optimization
- Avoid per-frame/tick consing in hot loops: reuse rectangles, vectors, strings, and animation state
- Keep entity/component data in arrays/structs (SoA-style when useful), not lists
- Separate update/draw; keep animation state lightweight
- **Hot path rule:** if it runs every tick or per-packet, it must be allocation-free and bounded-time
  (prove with allocation profiling)
- Avoid hidden allocations in hot code paths:
  - No `format`, string concatenation, or ad-hoc string building in-frame/in-tick (UI/debug/logging)
  - Avoid allocating closures/lambdas inside hot loops (especially capturing lambdas)
  - Avoid per-tick hash-table growth/rehash and transient key creation; pre-size tables and reuse keys/containers
  - Be cautious with generic dispatch/abstraction layers that encourage temporary allocations
  - Avoid accidental symbol/keyword interning at runtime in hot paths

### Allocation & reuse policy (pools / scratch / arenas)
- Use object pools/freelists not just for entities, but also for support churn:
  - intents/events (combat, movement, AI)
  - query result containers (nearby entities, visibility sets)
  - pathfinding nodes and temporary spatial data
- Prefer per-thread/per-system scratch buffers (arrays with fill-pointers)
- Use arena-style “dies-together” allocation for tick-local or snapshot-local work
- Pools must be bounded and actively managed:
  - cap growth and recycle aggressively
  - avoid unbounded pools that grow at peak load and never shrink
  - watch for large pooled objects/buffers that inflate heap size and make rare full GCs expensive
- Prefer specialized arrays and typed structs for pooled/scratch data to reduce boxing and improve locality

### Networking / serialization optimization (often the real hot path)
- Treat snapshot building and serialization as a first-class hot loop
- Avoid constructing intermediate “message objects” that are later serialized
- Write directly into reusable byte buffers when possible
- Maintain reusable per-connection or per-worker buffers with explicit write cursors
- Reuse delta-state and snapshot scratch structures; avoid per-send allocations
- **Schedule network output intentionally:** keep simulation at 60 Hz, but consider sending snapshots/deltas at a lower
  or adaptive rate (e.g., 20–30 Hz) to reduce serialization load and allocation/spike risk while preserving sim stability
- Prefer specialized byte buffers (e.g., `(simple-array (unsigned-byte 8) (*))`) and typed cursors for encoding

### Rendering optimization
- Cull off-screen tiles and sprites; draw only what’s visible
- Chunk the map (e.g., 32×32 tiles) and cache static chunks in render textures
- Batch draw calls whenever possible


## 2) Hacks that Actually Improve GC Predictability

### 1) Schedule GC at safe points (make it your decision)
If you have a 60Hz tick, pick deliberate moments where a pause hurts least:
- right after you finish a snapshot send
- right after zone handoff / shard transfer
- during menu / loading / between matches (client)
- during low-pop intervals (server)

Even a simple “GC every N seconds” policy can prevent rare, catastrophic full collections.

### 2) Give the heap more headroom (reduce GC frequency spikes)
A too-small heap makes the runtime collect constantly and unpredictably.
- Bigger heap → fewer collections → fewer “surprise” pauses
- You trade memory for smoothness

This is one of the most common “we fixed the hitch” changes on servers.

### 3) Tune for *small + frequent* vs *rare + huge*
Most modern Common Lisp implementations use generational GC. You generally want:
- young-generation collections to be cheap and frequent
- avoid accidental promotion of short-lived garbage into old generations
- avoid heap growth oscillation (collect → grow → collect → grow)

**Practical hack:** warm up the server (load maps, create common objects, run a few ticks) so long-lived data settles
before peak load. This reduces later promotion churn.

### 4) Keep big buffers and OS resources out of the GC heap
Even with good discipline, large byte arrays, strings, and foreign handles can cause:
- increased scanning pressure
- larger old generations
- longer full GC pauses

Common approaches:
- reusable foreign buffers (or static/pinned arrays if your implementation supports them)
- keep only small metadata objects in the Lisp heap

### 5) Avoid features that make GC timing messier
Depending on the implementation, these often correlate with unpredictable pauses:
- lots of finalizers (cleanup hooks)
- weak hash tables / ephemeron-heavy caches
- excessive interning of new symbols or keywords at runtime
- “log everything” strategies with string building in hot paths

Not forbidden — just be aware they can turn GC into a wildcard.


## 3) SBCL Compiler Optimization Tips (For Best Code Generation)

SBCL can reach performance comparable to C in hot code, but it is not the default.
To get C-like performance, use declarations and policies that guide SBCL’s optimizing compiler,
trading some dynamic flexibility for static efficiency.

These are SBCL-specific rules for producing **maximum-performance Common Lisp**.

### 3.1 Use optimization policy deliberately (and locally)
- SBCL honors `(optimize ...)` strongly.
- Prefer localized policies:
  - package/file level: balanced defaults
  - hot functions: speed-focused

Typical patterns (adjust per project):
- Dev: `(speed 2) (safety 2) (debug 2)`
- Hot loop: `(speed 3) (safety 0-1) (debug 0-1) (compilation-speed 0)`

Guidelines:
- Use `(declaim (optimize ...))` for file/package defaults.
- Use `(declare (optimize ...))` inside the 5–20 hottest functions.
- Be careful with `(safety 0)` in server code: it can turn bugs into silent corruption.
- `(safety 0)` omits runtime safety checks (including type and array bounds checks); use it only in trusted hot code.

### 3.2 Type declarations are performance features in SBCL (single most important step)
SBCL generates unboxed, fast machine code when it knows types.

Declare aggressively in hot code:
- numeric types: `fixnum`, `(unsigned-byte 32)`, `single-float`, etc.
- function argument types and return types
- loop indices and accumulators
- array element types: specialized arrays avoid boxing
- struct slot types: typed `defstruct` slots are a big win

Why it matters:
- prevents generic arithmetic (boxed numbers)
- enables specialized array access (fast element access)
- reduces dispatch and runtime checks

Preferred data patterns:
- `simple-array` / `simple-vector` where possible
- specialized arrays like:
  - `(simple-array single-float (*))`
  - `(simple-array (unsigned-byte 8) (*))`
- `defstruct` with typed slots (and keep types “obvious” for SBCL inference)

### 3.3 Avoid “accidental generic” operations
If SBCL can’t infer a type (value becomes `T`), it will often fall back to generic operations:
- arithmetic becomes generic (boxing + slower)
- comparisons become generic
- sequence operations become generic

Hot code rule:
- ensure loop indices, accumulators, and frequently accessed fields are type-declared
- keep types stable across calls (don’t mix floats/integers unpredictably)

### 3.4 Avoid CLOS generic dispatch in the innermost loops
- CLOS is fine at the edges.
- In the innermost hot loops, generic function dispatch is often too expensive.
- Use direct functions and typed structs in hot paths; keep polymorphism at system boundaries.

### 3.5 Use inlining selectively for tiny hot helpers
- `(declaim (inline foo))` can remove call overhead and help SBCL optimize across function boundaries.
- Only inline small helpers that are called extremely often.
- Do not blanket-inline everything.

### 3.6 SIMD vectorization (for heavy numeric/data-parallel hotspots)
For heavily numerical and data-parallel work:
- consider SIMD libraries (e.g., `sb-simd`) to leverage CPU SIMD instructions
- this can match or exceed optimized C/C++ for certain workloads

Use SIMD only where profiling shows it matters.

### 3.7 Profiling with SBCL tooling (do not guess)
Measure:
- time in hot functions
- allocation rate in hot paths

Useful tools:
- `time` / `sb-ext:time` for quick checks
- `sb-sprof` for sampling profiler
- `sb-profile` for call-count/time profiling

### 3.8 Serialization hot spots: use specialized byte buffers
For networking/snapshots in SBCL, prefer:
- `(simple-array (unsigned-byte 8) (*))` for byte buffers
- typed cursor indices into those buffers
- direct writing into buffers (avoid intermediate message objects)

This reduces allocation/GC pressure and improves throughput.


## 4) Code Generation Checklist (LLM Must Follow)
When generating or editing code:

1) **Identify the hot path**
- Does it run every tick, per packet, per entity, or per client?
  - If yes: it must not allocate and must be bounded-time.

2) **Choose data layout that SBCL optimizes well**
- arrays/structs, typed slots, specialized arrays
- avoid list pipelines in tight loops

3) **Add SBCL-friendly declarations**
- `(declare (type ...))` for parameters, locals, indices, accumulators
- typed arrays and buffer cursors
- `(declare (optimize ...))` locally in the hot functions

4) **Reject hidden allocation sources**
- no `format`/string building in hot paths
- no capturing lambdas in hot loops
- avoid hash-table growth/rehash in hot loops
- avoid CLOS dispatch in inner loops

5) **Networking is a hot loop**
- no intermediate message objects
- reuse byte buffers
- typed cursors and specialized arrays

6) **Verify**
- use SBCL profiling tools to validate time and allocation behavior

---

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
- `docs/movement.md` - Physics, collision, zone transitions

## Important Reminders

- **GIT: READ-ONLY**: Never run git commands that modify state (commit, stash, reset, checkout, push, add, etc.) without explicit user permission. Read-only commands (status, log, diff, show) are fine. The user manages version control.
- **ALL TESTS MUST PASS**: Before claiming work complete, run `make tests` which runs ALL test targets in the correct order. If running individually, you MUST follow this order: `make checkparens && make ci && make smoke && make test-unit && make checkdocs`. The first three (checkparens, ci, smoke) MUST run in that exact order - no exceptions.
- **Never commit with unbalanced parens**: Run `make checkparens` before committing
- **CI must pass**: `make ci` runs cold compile + UDP handshake test
- **All tests must pass**: `make test-unit` runs all tests (unit, persistence, security, trade)
- **Docs must be complete**: `make checkdocs` verifies every src file has matching documentation
- **Smoke test must work**: `make smoke` tests actual client/server with graphics
- **Storage abstraction is mandatory**: Never call Redis/database directly from game logic
- **Classify all new state**: Every field must be marked durable or ephemeral
- **Use correct persistence tier**: Tier-1 for critical, tier-2 for routine, tier-3 for logout
- **Write migrations for durable fields**: New persistent fields require schema version bump + migration function
- **Server is authoritative**: Clients send intents, not state
- **Test both backends**: Memory for dev, Redis for persistence testing
