# CLAUDE.md — Repo Guidance for Claude Code (SBCL + raylib MMORPG)

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

---

## Project Overview

Common Lisp + raylib MMORPG prototype with a clean client/server UDP architecture. The codebase emphasizes data-driven design, intent-based actions, server authority, and strict separation between game logic and rendering.

**Stack:** SBCL, Quicklisp, raylib, claw-raylib, usocket (UDP), cl-redis (Redis/Valkey persistence)

---

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
make test-unit          # All tests (unit, persistence, security, trade) in ONE file
make checkdocs          # Verify docs/foo.md exists for each src/foo.lisp
```

**Test consolidation:** All test types (unit, persistence, security, trade) are consolidated into **one file**:
- `tests/unit-test.lisp` (run via `make test-unit`)

**Never skip tests.** If you implement a feature but don't run all test targets, the work is incomplete.

---

## When to Write Tests

**POLICY: If a unit test CAN be written, it SHOULD be written.**

We aim for **very high** test coverage. Tests prevent regressions and catch bugs early.

**Write tests for:**
- ✅ **All pure functions**: Input → output without side effects
- ✅ **Game logic**: Combat calculations, XP/leveling, inventory operations, movement physics
- ✅ **Data transformations**: Serialization, migrations, protocol encoding/decoding
- ✅ **Invariants**: Things that MUST always be true (HP ≤ max, gold ≥ 0, valid state transitions)
- ✅ **Edge cases**: Boundary conditions, empty inputs, max values, error paths
- ✅ **AI behavior**: Decision-making logic, state transitions, target selection
- ✅ **Utility functions**: Helpers in utils.lisp, data.lisp, etc.
- ✅ **Protocol handling**: Message parsing, validation, response generation

**Skip tests ONLY for:**
- ❌ **Rendering code**: Functions that call raylib directly (require GPU)
- ❌ **Audio code**: Functions that play sounds (require audio device)
- ❌ **Interactive input**: Real-time mouse/keyboard handling (tested via smoke test)

**Rule of thumb:** If a function has logic that could break, write a test.
The only exception is code that requires hardware (GPU/audio/input).

**Test file (single source of truth):**
- `tests/unit-test.lisp`

---

## Proactive Test Writing Requirements (For Claude)

**CRITICAL**: When implementing ANY new code, you MUST write accompanying tests.

**The rule is simple: If you can test it, test it.**

For every new function, ask:
1. Can I call this function in isolation? → Write a unit test
2. Does it transform data? → Test input/output pairs
3. Does it have edge cases? → Test boundaries (0, 1, max, empty, nil)
4. Can it fail? → Test error conditions
5. Does it interact with state? → Test state before/after

How to implement:
1. Write the test BEFORE or ALONGSIDE the implementation
2. Add your test to the appropriate section in `tests/unit-test.lisp`
3. Run `make test-unit` to verify
4. If a function is hard to test, refactor it to be testable (extract pure logic)

When NOT to write tests (ONLY exceptions):
- Code that directly calls raylib (rendering, textures, fonts)
- Code that directly plays audio
- Code that reads real-time input devices
- Top-level entry points (main, run-server, run-client)

If uncertain: **write the test**.

---

## Code Quality Checklist (MANDATORY)

Review EVERY change against these criteria before claiming work complete:

### 1) Tests Written?
- Any new function with logic → write test (if it can break, test it)
- Pure functions → unit tests with input/output pairs
- State-changing functions → test before/after state
- Edge cases → boundaries (0, 1, max, empty, nil)
- Only skip tests for: direct raylib calls, audio playback, real-time input

### 2) Retry Logic Added?
- Tier-1 saves (death, level-up) → `with-retry-exponential` 5 retries, 100–500ms
- Database reads on critical paths (login, load-player) → `with-retry-exponential` 3 retries, 50–200ms
- Auth UDP messages → `with-retry-linear` 3 retries, 50ms delay
- Zone/asset loading → `with-retry-exponential` 2 retries, 100–200ms
- Snapshots, UI updates, best-effort operations → no retry (fail silently acceptable)

### 3) Logging Added?
**CRITICAL:** Debug logging that helps diagnose problems should remain behind `*verbose*` and be useful long-term.
- Critical failures → `(warn "...")` always
- Tier-1 save failures → `(warn "CRITICAL: ...")` + fallback
- State transitions → `(log-verbose "...")` when `*verbose*` enabled
- Network events → `(log-verbose "...")` for auth, connects, disconnects
- Hot loops (update-sim, draw) → no logging (use `*verbose-coords*` flag if needed)
- Helper functions → no logging unless they fail

### 4) Variable Scope Correct?
Globals (`defparameter`) — use for:
- Configuration: `*net-buffer-size*`, `*inventory-size*`
- Server state: `*storage*`, `*player-sessions*`, `*active-sessions*`
- Feature flags: `*verbose*`, `*debug-npc-logs*`

Locals (`let`, `let*`) — use for:
- Temporary computation results
- Loop variables
- Function parameters

NEVER:
- Global mutable game state that belongs in structs
- Globals for what should be function parameters
- `setf` on function parameters (pass values, return new values)

### 5) Data-Driven Design Consistent?
Data-driven (good):
- Game data in `data/*.lisp` files (zones, NPCs, items, world-graph)
- Behavior driven by data fields (npc-archetype, equipment-modifiers)

Polymorphism rule (important):
- CLOS generic functions are fine at **system boundaries**
- **Do NOT** use CLOS dynamic dispatch in the **innermost per-tick loops**
  (hot loops must be direct calls + typed structs/arrays)

Hard-coded (bad):
- Special-case player ID checks in game logic
- NPC behavior in main loop instead of ai.lisp
- Item effects inline instead of data-driven

Before claiming complete, verify:
- [ ] Tests written (if persistent state touched)
- [ ] Retry logic added (if critical operation can fail)
- [ ] Logging added (if failure is critical or debugging needed)
- [ ] No new globals (unless config/server state)
- [ ] Data-driven, not hard-coded

---

## Running Client/Server
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
MMORPG_DB_BACKEND=memory make server            # Use in-memory storage (for tests/dev without Redis)
MMORPG_REDIS_HOST=127.0.0.1 make server         # Redis host (default: 127.0.0.1)
MMORPG_REDIS_PORT=6379 make server              # Redis port (default: 6379)
```

---

## Development with SLIME
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

---

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
Clients never send state — only intent (what they want to do). Server validates and applies changes.

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
Game code MUST NOT directly call Redis or any database client.

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

Dirty flag system: `register-player-session`, `mark-player-dirty`, periodic batch flushes.

---

## Data Classification

When adding features, classify ALL new state as **durable** or **ephemeral**:

Durable (must persist to DB):
- XP, levels, stats, HP, inventory, equipment, currency, position, zone-id, quests

Ephemeral (OK to lose on crash/logout):
- Attack/follow targets, combat timers, animation frames, AI state, buffs/debuffs, trade windows

Add durable fields to `serialize-player` (base payload).
Add ephemeral fields only to `:include-visuals` section.

---

## Network Data Classification (CRITICAL for Scale)

When adding features, classify ALL new state for network transmission:

Public snapshots (60 Hz broadcast to ALL clients in zone):
- What other players NEED TO SEE about you
- Position (x, y), velocity (dx, dy)
- HP (for health bars above heads)
- Animation state, facing direction
- Combat state (attacking, hit animation)
- Visual timers (animation sync)
- **Size budget: ~64–80 bytes per player** (keep this small!)

Private state (sent only to owner, only when dirty flag set):
- Inventory contents, equipment slots
- Detailed stats (strength, dexterity, intelligence, etc.)
- Gold, currency
- No bandwidth impact on public snapshots

Load-once data (sent on login or zone change only):
- Zone data (tiles, collision, objects)
- Friends list, guild roster
- Quest journal
- Skill tree unlocks

Rule of thumb: **If it doesn't affect how you LOOK to other players, it doesn't belong in public snapshots.**

---

## Zone System
- Zone data: `data/zones/*.lisp` - tile layers, collision, spawns, objects
- World graph: `data/world-graph.lisp` - zone connections and transitions
- Multi-zone: Players carry zone-id; transitions update session zone and trigger dirty flag
- Scaling: Each server process runs ONE zone. For 10k users @ 500/zone = 20 server processes (horizontal scaling).

---

## Update/Draw Separation
- `update-sim`: Fixed timestep game logic (combat, AI, movement). No rendering dependencies.
- `draw-game`: Rendering only. Reads state, never modifies it.
- Server runs only update-sim. Client runs both.

---

## Schema Migrations (CRITICAL)

Location:
- `src/migrations.lisp` (core logic)
- `src/db.lisp` (admin command)

Write a migration when adding/changing DURABLE fields:
- Adding a new persistent field to player struct
- Changing the type/structure of an existing field
- Renaming a field (migration copies old → new)
- Splitting/merging fields

You do NOT need a migration for:
- Adding ephemeral fields
- Adding new item types to game data
- Adding new zones (data files)
- Changing game logic that doesn't affect saved format

Migration rules:
- Never delete old migrations
- Migrations must be pure (plist in → plist out, no side effects)
- Always provide defaults
- Never modify existing migrations
- Append only to the migration registry
- Always test the full migration chain

---

## Code Style Rules

### Modular & Reusable by Default
We are building reusable game systems, not one-off demo code.

Bias:
- small composable functions
- reusable systems for players + NPCs
- game logic independent of rendering

Rules:
- No gameplay logic in the main loop (it orchestrates systems)
- Entities hold data; systems implement behavior
- Rendering separate from game logic
- Avoid hardcoded values that may vary later
- If it could apply to NPCs, write it generically now

Agent self-check:
- Reusable beyond a single entity
- Logic works without rendering
- Not special-cased
- Client/server split won't break it
- New state classified durable vs ephemeral
- No direct database calls from game logic

### Plist Mutation: The `setf getf` Pitfall (CRITICAL)
In Common Lisp, `setf getf` only modifies existing keys — it does NOT add new keys to a plist.
The modification is silently ignored.

Prevention rules:
1. Always initialize mutable plist keys when creating plists
2. Prefer structs for complex mutable state
3. Add assertions during debugging to catch missing keys
4. Document required keys for mutable plist-based features

### Security: Untrusted Client Principle
Clients are untrusted. Server computes truth.
Always use `*read-eval* nil` when deserializing data.

---

# SBCL Performance Directives (MMORPG / 60Hz)

This section defines how Claude/LLM agents must generate **max-performance SBCL Common Lisp** for MMO tick loops.

## Prime Directive (Non‑Negotiable)
Any code that runs:
- every tick/frame
- per packet
- per entity
- per client

MUST be allocation-free, type-stable, and bounded-time.

## Performance Matters
- Tick budget discipline (60 Hz): every tick has ~16.67ms.
- Any GC/serialization spikes cause stutter.
- Anything that runs every tick must be bounded and allocation-free.

### Hot loop optimization
Agents MUST:
- Avoid per-tick consing in hot loops (reuse objects/state)
- Prefer arrays/structs (SoA-style when useful), not lists
- Separate update/draw
- Avoid hidden allocations:
  - no `format`/string building
  - no capturing lambdas
  - no per-tick hashtable growth/rehash (pre-size + reuse)
  - avoid generic abstraction layers in inner loops

### Data layout
Agents MUST default to:
- typed `defstruct` for hot data (when CLOS is not required)
- typed `simple-array` for hot data

Required defaults:
- `(simple-array single-float (*))` numeric vectors
- `(simple-array fixnum (*))` ids/indices
- `(simple-array (unsigned-byte 8) (*))` byte buffers

### Instantiation (SBCL CLOS)
Agents MUST:
- avoid `(make-instance var)` in hot paths
- prefer `(make-instance 'my-class)` via dedicated constructors
- treat initforms carefully (constant-class constructors optimize them)

### Numeric rules
Agents MUST:
- declare loop vars/accumulators/arguments
- keep numeric lanes stable (`fixnum` vs `single-float`)
- use `THE` when needed (only when ranges are proven)
- avoid integer division in tight loops unless proven safe

### Pools/scratch/arenas
Agents MUST:
- pool entities + support churn (events/intents/query containers/path nodes)
- use per-system scratch arrays w/ fill-pointers
- keep pools bounded and managed

### Networking/serialization
Agents MUST:
- serialize into reusable `(unsigned-byte 8)` buffers
- reuse per-connection buffers/cursors
- avoid intermediate message objects
- consider sending snapshots at 20–30 Hz even with 60 Hz sim

### Rendering
Agents MUST:
- cull off-screen
- chunk map + cache static chunks in render textures
- batch draw calls

## GC predictability hacks
Agents SHOULD:
- schedule GC at safe points (post-snapshot, zone handoff, menus, low-pop)
- give heap headroom (trade RAM for smoothness)
- warm up server before peak load
- keep big buffers/foreign handles out of GC heap
- avoid finalizers/weak tables/runtime interning/log-spam in hot loops

## SBCL compiler usage
Agents MUST:
- use localized `(optimize ...)` policies
- use type declarations everywhere hot
- avoid CLOS dispatch in innermost loops (CLOS ok at system boundaries)
- inline only tiny hot helpers
- profile; do not guess

Optimization policy must be environment-driven. In development, compile/load with safe debugging defaults (e.g. (speed 2) (safety 2–3) (debug 2–3)) to maximize correctness, stack traces, bounds/type checks, and fast iteration. In production, compile/load with performance defaults (e.g. (speed 3) (safety 1) (debug 0–1) (compilation-speed 0)), but never use “global (safety 0)” project-wide—production should still catch most accidental corruption. The build system must expose this via a single toggle (e.g. MMORPG_ENV=dev|prod), and that toggle must select a single place where declaim (optimize ...) defaults are applied consistently across the whole build.

Hot functions remain localized and explicit in both modes. Regardless of environment, the 5–20 proven hottest functions may contain a local (declare (optimize (speed 3) (safety 0) (debug 0))) only when correctness is already validated by tests, and only with full type declarations for args/locals/arrays. In dev mode, those functions may optionally stay at (safety 1) unless you’re actively profiling. The agent must not sprinkle optimize randomly—defaults come from the env toggle; exceptions are only inside measured hot loops.

- Default policy
At file/package top:
(declaim (optimize (speed 2) (safety 2) (debug 2)))
- Hot-path policy (only when proven hot + correct)
Inside the function:
(declare (optimize (speed 3) (safety 0) (debug 0)))
Add full type declarations for args + locals + arrays.
- Don’t global-nuke safety
Never set (declaim (optimize (safety 0))) project-wide.
Only localize it.
- FASL expectation
Build/test runs should go through make/ASDF so code is compiled.
Don’t assume .fasl sitting next to .lisp will be used automatically.

## SIMD / auto-vectorization (compute-bound kernels only)
SIMD is a deliberate tradeoff: it maximizes arithmetic throughput in compute-bound kernels, but increases code complexity and offers little benefit (or harm) in memory- or branch-bound loops, so it should be applied selectively, not universally.
SIMD should be “default” only inside isolated numeric kernels (typed arrays, low branching, batch processing), which in your codebase most naturally live in:
spatial.lisp (broadphase/grid/partition math, AABB overlap, distance-to-cell, packed queries) — best SIMD ROI
movement.lisp (SoA position/velocity integration, batch collision broadphase math, distance checks) — SIMD if entity data is packed/branch-light
combat.lisp (only sub-kernels) (AoE radius filtering, LOS/distance filtering, numeric buff math) — not the rule-heavy branching parts
rendering.lisp (only if CPU-side transforms are heavy) (world→screen transforms for many entities, building instance buffers/rects/sort keys) — only when it’s already array-ish
Usually NOT worth SIMD (mostly memory/branch/I/O bound):
net.lisp (serialization/copies/branchy protocol) — SIMD only for rare byte-scan/encode/checksum-style bulk loops
ai.lisp (branchy state machines) — SIMD only if you extract packed numeric scoring across many agents
db.lisp, save.lisp, zone.lisp, world-graph.lisp, editor.lisp, ui.lisp, data.lisp, trade.lisp, migrations.lisp, utils.lisp — mostly not SIMD territory
Pattern to standardize (so it’s consistent and scalable):
Put SIMD work in a small “kernel” surface area (e.g., simd-kernels.lisp or src/kernels/)
Kernels accept typed arrays + fixnum bounds, and have:
scalar reference implementation (correctness/tests)
SIMD variants (SSE2 baseline + AVX/AVX2 fast paths)
runtime dispatch via instruction-set-case for portability
That keeps “SIMD as default” true where it pays off, without infecting the entire MMO codebase with SIMD complexity.
Agents MUST:
- use typed arrays + SIMD pack types (sb-simd/Loopus)
- structure SIMD as vector loop + remainder loop
- keep scalar reference implementations
- dispatch by instruction set at runtime (`instruction-set-case`) for portable images
  (SIMD compatibility is CPU-feature-dependent, not OS-dependent)

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

---

## Important Reminders

- **GIT: READ-ONLY**: Never run git commands that modify state (commit, stash, reset, checkout, push, add, etc.) without explicit user permission. Read-only commands (status, log, diff, show) are fine.
- **ALL TESTS MUST PASS**: Before claiming work complete, run `make tests`.
- **Storage abstraction is mandatory**: Never call Redis/database directly from game logic.
- **Classify all new state**: Every field must be durable or ephemeral.
- **Write migrations for durable fields**: Schema version bump + migration function + tests.
- **Server is authoritative**: Clients send intents, not state.
