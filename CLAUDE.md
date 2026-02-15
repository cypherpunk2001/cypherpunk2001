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
make test-unit          # All tests (unit, persistence, security, trade) via modular files
make checkdocs          # Verify docs/foo.md exists for each src/foo.lisp
```

**Test layout:** Tests are split into modular domain files under `tests/unit/`, loaded by the aggregator `tests/unit-test.lisp`:
- `tests/unit-test.lisp` — aggregator that loads all `tests/unit/*.lisp` files and runs test suites
- `tests/unit/*.lisp` — domain-specific test files (e.g., `combat-tests.lisp`, `persistence-core-tests.lisp`)
- Run via `make test-unit`
- **File size guideline:** Around ~1000 lines, split test files at natural domain boundaries to avoid files exceeding ~2000 lines. Prefer descriptive names (e.g., `persistence-core-tests.lisp`, `persistence-hardening-tests.lisp`) over generic suffixes.

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

**Test files (modular layout):**
- `tests/unit/*.lisp` — domain test files, each with a `*tests-<domain>*` list
- `tests/unit-test.lisp` — aggregator that loads them all

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
2. Add your test to the appropriate domain file in `tests/unit/` and its `*tests-<domain>*` list
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

# Build environment (optimization level)
MMORPG_ENV=dev make server                      # Development (default): safety checks, debug info
MMORPG_ENV=prod make server                     # Production: max speed, minimal safety
make server-prod                                # Shortcut for production build

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

### File Size Guideline
Prefer ~1000 LOC per source file when there's a natural domain split. Don't force it — if a file is 1200 LOC and cohesive, that's fine. When a file exceeds ~1500 LOC, look for natural boundaries to split (e.g., `net.lisp` → `net-protocol.lisp`, `net-auth.lisp`, `net-snapshot.lisp`, `net-server.lisp`, `net-client.lisp`). Split files use a thin glue file that loads last. Each split file gets a matching `docs/<name>.md`.

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
`(setf (getf place key) value)` works correctly (adds or updates) **only when `place` is a setf-able location** (a variable, struct slot, or array element). The danger is when the plist is a temporary or computed form — `setf getf` may prepend a new cons cell that is silently lost because nothing holds the updated head of the list.

```lisp
;; SAFE — x is a setf-able variable
(let ((x '(:a 1)))
  (setf (getf x :b) 2)  ; x is now (:b 2 :a 1)
  x)

;; DANGEROUS — return value of (foo) is not a setf-able place
(setf (getf (foo) :b) 2)  ; new cons may be lost
```

Prevention rules:
1. Only use `setf getf` on variables, struct slots, or other genuine places
2. Prefer structs for complex mutable state (compile-time slot access, no place ambiguity)
3. When passing plists through function boundaries, return the updated plist rather than relying on in-place mutation
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
**Current decision: DEFER SIMD for now.** Fix allocations, type declarations, and hot-path dispatch first; only introduce SIMD after profiling shows compute-bound kernels.
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
- **PixelLab MCP**: Available as an MCP server for AI pixel art generation. API docs: https://api.pixellab.ai/mcp/docs

---

# PixelLab MCP Tools - AI Assistant Guide

> Generate pixel art assets directly from your AI coding assistant using the Model Context Protocol (MCP)

You have access to PixelLab's MCP tools for creating game-ready pixel art. This guide explains what tools are available and how to use them effectively.

## IMPORTANT: These are MCP tools, not REST endpoints

- If you see PixelLab tools available (like `create_character`, `animate_character`), use them directly
- If you don't see these tools, tell the user MCP isn't configured - don't try curl or v2 API
- Tool names may be prefixed (`mcp__pixellab__create_character`) or bare (`create_character`) depending on client
- Download URLs like `/mcp/characters/{id}/download` are the only direct HTTP endpoints

## What is PixelLab MCP?

PixelLab MCP (also called "Vibe Coding") lets you generate pixel art characters, animations, and tilesets while coding. The tools are non-blocking - they return job IDs immediately and process in the background (typically 2-5 minutes).

## Other PixelLab Interfaces

If users mention these, they're using different PixelLab interfaces:
- **Web interfaces**: Character Creator, Map Workshop, Simple Creator (visual tools at pixellab.ai)
- **Editor plugins**: Aseprite extension, Pixelorama integration
- **API v1**: Legacy REST endpoints (deprecated)
- **API v2**: Modern REST API for programmatic usage
  - Documentation: https://api.pixellab.ai/v2/llms.txt
  - You can use these endpoints directly via HTTP requests when writing code
  - Same functionality as MCP but through REST calls
  - Useful for: batch processing, custom integrations, live in-game asset generation
  - Also useful when you need endpoints not available as MCP tools (use at own risk - MCP tools are made for easy AI usage)

Note: Characters and tilesets created via MCP will appear in Character Creator and Map Workshop web interfaces respectively (if using the same account).

## MCP/Vibe Coding Setup

1. Get your API token at https://api.pixellab.ai/mcp
2. Configure your AI assistant (Cursor, Claude Code, VS Code, etc.)
3. Start generating game assets while coding

### MCP Configuration
```json
{
  "mcpServers": {
    "pixellab": {
      "url": "https://api.pixellab.ai/mcp",
      "transport": "http",
      "headers": {
        "Authorization": "Bearer YOUR_API_TOKEN"
      }
    }
  }
}
```

## Key Concepts

### Non-Blocking Operations
All creation tools return immediately with job IDs:
- Submit request -> Get job ID instantly
- Process runs in background (2-5 minutes)
- Check status with corresponding `get_*` tool
- Download when ready (UUID serves as access key)
- **No authentication for downloads**: Share download links freely - UUID acts as the key

### Workflow Pattern
```python
# 1. Create (returns immediately)
result = create_character(description='wizard', n_directions=8)
character_id = result.character_id

# 2. Queue animations immediately (no waiting!)
animate_character(character_id, 'walk')
animate_character(character_id, 'idle')

# 3. Check status later
status = get_character(character_id)
```

### Connected Tilesets
Create seamless terrain transitions by chaining tilesets:
```python
# First tileset returns base_tile_ids immediately
t1 = create_topdown_tileset('ocean', 'beach')

# Chain next tileset using base tile ID (no waiting!)
t2 = create_topdown_tileset('beach', 'grass', lower_base_tile_id=t1.beach_base_id)
```

## Available Tools

### Character & Animation Tools

**Tips:**
- Characters are stored permanently and can be reused
- Animations can be queued immediately after character creation
- 4 directions: south, west, east, north
- 8 directions: adds south-east, north-east, north-west, south-west
- Canvas size is total area; character will be ~60% of canvas height

#### `create_character`
Queue a character creation job with 4 or 8 directional views.

**Examples:**
```python
# Humanoid character (default)
create_character(
    description='brave knight with shining armor',
    n_directions=8,
    size=48,
    proportions='{"type": "preset", "name": "heroic"}'
)

# Quadruped character
create_character(
    description='orange tabby cat',
    body_type='quadruped',
    template='cat',  # bear, cat, dog, horse, lion
    n_directions=8,
    size=48
)
```

**Parameters:**
- `description`: str (optional) [default: PydanticUndefined]
- `name`: Optional (optional) [default: None]
- `body_type`: Literal (optional) [default: humanoid] - 'humanoid' for people/robots (default), 'quadruped' for animals (requires template)
- `template`: Optional (optional) [default: None] - Required for quadrupeds: bear, cat, dog, horse, lion
- `n_directions`: Literal (optional) [default: 8] - Use 8 for full rotation, 4 for cardinal directions
- `proportions`: Optional (optional) [default: {"type": "preset", "name": "default"}] - Presets: default, chibi, cartoon, stylized, realistic_male, realistic_female, heroic (humanoid only)
- `size`: int (optional) [default: 48] - Canvas size in pixels (character ~60% of height)
- `outline`: Optional (optional) [default: single color black outline]
- `shading`: Optional (optional) [default: basic shading]
- `detail`: Optional (optional) [default: medium detail]
- `ai_freedom`: float (optional) [default: 750]
- `view`: Literal (optional) [default: low top-down]

#### `animate_character`
Queue animation jobs for an existing character (humanoid or quadruped).

**Example:**
```python
animate_character(
    character_id='uuid-from-create',
    template_animation_id='walking',  # Check tool description for full list
    action_description='walking proudly'  # optional customization
)
```

**Parameters:**
- `character_id`: str (optional) [default: PydanticUndefined]
- `template_animation_id`: str (optional) [default: PydanticUndefined]
- `action_description`: Optional (optional) [default: None]
- `animation_name`: Optional (optional) [default: None]

#### `get_character`
Get complete character information including rotations, animations, and download link.

Returns:
- Character details and metadata
- All rotation image URLs
- List of animations with their status
- Pending jobs for this character
- ZIP download URL
- Optional preview image

**Parameters:**
- `character_id`: str (optional) [default: PydanticUndefined]
- `include_preview`: bool (optional) [default: True]

#### `list_characters`
List all your created characters.

**Parameters:**
- `limit`: int (optional) [default: 10]
- `offset`: int (optional) [default: 0]
- `tags`: str | None (optional) [default: None]

#### `delete_character`
Delete a character and all its associated data.

**Parameters:**
- `character_id`: str (optional) [default: PydanticUndefined]

### Top-Down Tileset Tools

**Wang Tileset System:**
- Creates 16 tiles covering all corner combinations
- Perfect for seamless terrain transitions
- Use base_tile_ids to chain multiple tilesets
- tile_size: typically 16x16 or 32x32 pixels
- transition_size: 0=sharp, 0.25=medium, 0.5=wide blend

#### `create_topdown_tileset`
Generate a Wang tileset for top-down game maps with corner-based autotiling.

**Example (chained tilesets):**
```python
# Ocean -> Beach -> Grass -> Forest
t1 = create_topdown_tileset('ocean water', 'sandy beach')
t2 = create_topdown_tileset('sandy beach', 'green grass',
                           lower_base_tile_id=t1.beach_base_id)
t3 = create_topdown_tileset('green grass', 'dense forest',
                           lower_base_tile_id=t2.grass_base_id)
```

**Parameters:**
- `lower_description`: str (optional) [default: PydanticUndefined]
- `upper_description`: str (optional) [default: PydanticUndefined]
- `transition_size`: float (optional) [default: 0.0] - 0=sharp edge, 0.25=medium blend, 0.5=wide transition
- `transition_description`: Optional (optional) [default: None]
- `tile_size`: Dict (optional) [default: {'width': 16, 'height': 16}]
- `outline`: Optional (optional) [default: None]
- `shading`: Optional (optional) [default: None]
- `detail`: Optional (optional) [default: None]
- `view`: Literal (optional) [default: high top-down] - 'high top-down' for RTS, 'low top-down' for RPG
- `tile_strength`: float (optional) [default: 1.0]
- `lower_base_tile_id`: Optional (optional) [default: None] - Use base_tile_id from previous tileset for continuity
- `upper_base_tile_id`: Optional (optional) [default: None]
- `tileset_adherence`: float (optional) [default: 100.0]
- `tileset_adherence_freedom`: float (optional) [default: 500.0]
- `text_guidance_scale`: float (optional) [default: 8.0]

#### `get_topdown_tileset`
Retrieve a topdown tileset by ID.

**Parameters:**
- `tileset_id`: str (optional) [default: PydanticUndefined]

#### `list_topdown_tilesets`
List all tilesets created by the authenticated user.

**Parameters:**
- `limit`: int (optional) [default: 10]
- `offset`: int (optional) [default: 0]

#### `delete_topdown_tileset`
Delete a tileset by ID.

**Parameters:**
- `tileset_id`: str (optional) [default: PydanticUndefined]

### Sidescroller Tileset Tools

**2D Platformer Tips:**
- Designed for side-view perspective (not top-down)
- Creates 16 tiles with transparent backgrounds
- Platform tiles have flat surfaces for gameplay
- Use `transition_description` for decorative top layers
- Chain tilesets using `base_tile_id` for consistency

#### `create_sidescroller_tileset`
Generate a sidescroller tileset for 2D platformer games with side-view perspective.

**Example (platform variety):**
```python
# Stone -> Wood -> Metal platforms
stone = create_sidescroller_tileset(
    lower_description='stone brick',
    transition_description='moss and vines'
)
wood = create_sidescroller_tileset(
    lower_description='wooden planks',
    transition_description='grass',
    base_tile_id=stone.base_tile_id
)
```

**Parameters:**
- `lower_description`: str (optional) [default: PydanticUndefined] - Platform material (stone, wood, metal, ice, etc.)
- `transition_description`: str (optional) [default: PydanticUndefined] - Top decoration (grass, snow, moss, rust, etc.)
- `transition_size`: float (optional) [default: 0.0] - 0=no top layer, 0.25=light decoration, 0.5=heavy coverage
- `tile_size`: Dict (optional) [default: {'width': 16, 'height': 16}]
- `outline`: Optional (optional) [default: None]
- `shading`: Optional (optional) [default: None]
- `detail`: Optional (optional) [default: None]
- `tile_strength`: float (optional) [default: 1.0]
- `base_tile_id`: Optional (optional) [default: None] - From previous tileset for visual consistency
- `tileset_adherence`: float (optional) [default: 100.0]
- `tileset_adherence_freedom`: float (optional) [default: 500.0]
- `text_guidance_scale`: float (optional) [default: 8.0]
- `seed`: Optional (optional) [default: None]

#### `get_sidescroller_tileset`
Get a sidescroller tileset by ID, including generation status and download information.

**Parameters:**
- `tileset_id`: str (optional) [default: PydanticUndefined]
- `include_example_map`: bool (optional) [default: True] - Shows how tiles work in a platformer level

#### `list_sidescroller_tilesets`
List all sidescroller tilesets created by the authenticated user.

**Parameters:**
- `limit`: int (optional) [default: 20]
- `offset`: int (optional) [default: 0]

#### `delete_sidescroller_tileset`
Delete a sidescroller tileset by ID.

**Parameters:**
- `tileset_id`: str (optional) [default: PydanticUndefined]

### Isometric Tile Tools

**Isometric Design Tips:**
- Creates individual 3D-looking tiles for game assets
- Sizes above 24px produce better quality (32px recommended)
- `tile_shape` controls thickness: thin (~10%), thick (~25%), block (~50%)
- Perfect for blocks, items, terrain pieces, buildings
- Use consistent settings across tiles for cohesive look

#### `create_isometric_tile`
Create an isometric tile with pixel art style.

**Examples:**
```python
# Terrain tiles
grass = create_isometric_tile('grass on top of dirt', size=32)
stone = create_isometric_tile('stone brick wall with moss', size=32)

# Game objects
chest = create_isometric_tile(
    description='wooden treasure chest with gold trim',
    tile_shape='block',  # Full height for objects
    detail='highly detailed'
)
```

**Parameters:**
- `description`: str (optional) [default: PydanticUndefined]
- `size`: int (optional) [default: 32] - 32px recommended for best quality
- `tile_shape`: Literal (optional) [default: block] - thin (floors), thick (platforms), block (cubes/objects)
- `outline`: Optional (optional) [default: lineless] - 'lineless' for modern look, 'single color' for retro
- `shading`: Optional (optional) [default: basic shading]
- `detail`: Optional (optional) [default: medium detail] - Higher detail works better with larger sizes
- `text_guidance_scale`: float (optional) [default: 8.0] - Higher values follow description more closely
- `seed`: Optional (optional) [default: None] - Use same seed for consistent style across tiles

#### `get_isometric_tile`
Retrieve an isometric tile by ID. Returns tile data if completed, or status information if still processing.

The tile will include:
- Base64 PNG image data
- Metadata about generation parameters
- Download URL for the tile image

Check the 'status' field:
- 'completed': Tile is ready with full data
- 'processing': Still generating (check 'eta_seconds')
- 'failed': Generation failed (see 'error' message)
- 'not_found': Invalid tile ID

**Parameters:**
- `tile_id`: str (optional) [default: PydanticUndefined]

#### `list_isometric_tiles`
List all your created isometric tiles.

Returns a paginated list of tiles with basic information.
Use get_isometric_tile for detailed information about a specific tile.
Tiles are sorted by creation date (newest first).

**Parameters:**
- `limit`: int (optional) [default: 10]
- `offset`: int (optional) [default: 0]

#### `delete_isometric_tile`
Delete an isometric tile by ID. Only the owner can delete their own tiles.
This action is permanent and cannot be undone.

**Parameters:**
- `tile_id`: str (optional) [default: PydanticUndefined]

### Map Object Tools

#### `create_map_object`
Create pixel art objects with transparent backgrounds for game maps. Supports style matching to blend with existing maps.

**Example:**
```python
create_map_object(
    description='oak tree',
    background_image='{"type": "base64", "base64": "..."}'
)
```

- Auto-deleted after 8 hours
- Width/height auto-detected from background image
- Use style matching (background_image) to blend objects with your map's existing art style
- Good for: props, decorations, unique one-off objects placed on top of tiled structures

## Available Resources

MCP also provides documentation resources:

- `pixellab://docs/python/sidescroller-tilesets` - Quick Python implementation guide for PixelLab sidescroller tilesets
- `pixellab://docs/godot/sidescroller-tilesets` - Complete Godot 4.x sidescroller tileset implementation guide
- `pixellab://docs/unity/isometric-tilemaps-2d` - Complete Unity 2D isometric tilemap implementation guide
- `pixellab://docs/godot/isometric-tiles` - Complete Godot 4.x isometric tiles guide
- `pixellab://docs/godot/wang-tilesets` - Complete Godot 4.x Wang tileset implementation guide
- `pixellab://docs/python/wang-tilesets` - Quick Python implementation guide for PixelLab Wang tilesets
- `pixellab://docs/overview` - Complete PixelLab platform overview

## Tool Response Format

All tools return status indicators:
- Success - Operation completed
- Processing - Background job running
- Error - Operation failed

## Background Jobs

Creation tools return immediately with job IDs.
Use the corresponding `get_*` tool to check status.

## Support & Resources

- Setup Guide: https://pixellab.ai/vibe-coding
- Discord Community: https://discord.gg/pBeyTBF8T7
- API v2 Documentation: https://api.pixellab.ai/v2/llms.txt

---

# Pixel Lab API - v2 API Documentation
Version: dev
Generated: 2025-12-11

## Overview

## Base URL
https://api.pixellab.ai/v2

## Authentication
All endpoints require Bearer token authentication:
```
Authorization: Bearer YOUR_API_TOKEN
```

Get your API token at: https://pixellab.ai/account

## Response Format
All responses follow this structure:
```json
{
  "success": true,
  "data": {},
  "error": null,
  "usage": {
    "credits_used": 0,
    "generations_used": 0,
    "remaining_credits": 100,
    "remaining_generations": 50
  }
}
```

## Available Endpoints

# Account

## GET /balance
**Get balance**
Tags: Account

Returns the current balance for your account.

### Responses
- **200**: Successfully retrieved balance
- **401**: Invalid API token

# Animate

## POST /animate-with-skeleton
**Animate with skeleton**
Tags: Animate

Creates a pixel art animation based on the provided parameters. Called "Animate with skeleton" in the plugin.

### Parameters

### Request Body
- `image_size`: object (required)
- `image_size.width`: integer (min=16.0, max=256.0) (required)
  Image width in pixels
- `image_size.height`: integer (min=16.0, max=256.0) (required)
  Image height in pixels
- `guidance_scale`: number (min=1.0, max=20.0, default=4.0) (optional)
  How closely to follow the reference image and skeleton keypoints
- `view`: enum[side, low top-down, high top-down] (optional)
- `direction`: enum[north, north-east, east, ...] (optional)
- `isometric`: boolean (default=False) (optional)
  Generate in isometric view
- `oblique_projection`: boolean (default=False) (optional)
  Generate in oblique projection
- `init_images`: array | null (optional)
  Initial images to start the generation from
- `init_image_strength`: integer (min=1.0, max=999.0, default=300) (optional)
  Strength of the initial image influence
- `skeleton_keypoints`: array[array] (optional)
  Skeleton points
- `reference_image`: object (required)
  A base64 encoded image.
- `reference_image.type`: string (default=base64) (optional)
  Image data type
- `reference_image.base64`: string (required)
  Base64 encoded image data
- `reference_image.format`: string (default=png) (optional)
  Image format
- `inpainting_images`: array[any] (optional)
  Images used for showing the model with connected skeleton
- `mask_images`: array[any] (optional)
  Inpainting / mask image (black and white image, where the white is where the model should inpaint)
- `color_image`: object | null (optional)
  Forced color palette, image containing colors used for palette
- `seed`: integer | null (optional)
  Seed decides the starting noise

### Responses
- **200**: Successfully generated image
- **401**: Invalid API token
- **402**: Insufficient credits
- **422**: Validation error
- **429**: Too many requests
- **529**: Rate limit exceeded

## POST /animate-with-text
**Animate with text**
Tags: Animate

Creates a pixel art animation based on text description and parameters.

### Parameters

### Request Body
- `image_size`: object (required)
- `image_size.width`: integer (min=64.0, max=64.0) (required)
  Image width in pixels
- `image_size.height`: integer (min=64.0, max=64.0) (required)
  Image height in pixels
- `description`: string (required)
  Character description
- `negative_description`: string | null (optional)
  Negative prompt to guide what not to generate
- `action`: string (required)
  Action description
- `text_guidance_scale`: number | null (optional)
  How closely to follow the text prompts
- `image_guidance_scale`: number | null (optional)
  How closely to follow the reference image
- `n_frames`: integer | null (optional)
  Length of full animation (the model will always generate 4 frames)
- `start_frame_index`: integer | null (optional)
  Starting frame index of the full animation
- `view`: enum[side, low top-down, high top-down] (optional)
- `direction`: enum[north, north-east, east, ...] (optional)
- `init_images`: array | null (optional)
  Initial images to start the generation from
- `init_image_strength`: integer (min=1.0, max=999.0, default=300) (optional)
  Strength of the initial image influence
- `reference_image`: object (required)
  A base64 encoded image.
- `reference_image.type`: string (default=base64) (optional)
  Image data type
- `reference_image.base64`: string (required)
  Base64 encoded image data
- `reference_image.format`: string (default=png) (optional)
  Image format
- `inpainting_images`: array[any] (optional)
  Existing animation frames to guide the generation
- `mask_images`: array | null (optional)
  Inpainting / mask image (black and white image, where the white is where the model should inpaint)
- `color_image`: object | null (optional)
  Forced color palette, image containing colors used for palette
- `seed`: integer | null (optional)
  Seed for reproducible results (0 for random)

### Responses
- **200**: Successfully generated animation
- **401**: Invalid API token
- **402**: Insufficient credits
- **422**: Validation error
- **429**: Too many requests
- **529**: Rate limit exceeded

## POST /animate-with-text-v2
**Animate with text (v2)**
Tags: Animate

Generate pixel art animation from text.

### Parameters

### Request Body
- `reference_image`: object (required)
  A base64 encoded image.
- `reference_image.type`: string (default=base64) (optional)
  Image data type
- `reference_image.base64`: string (required)
  Base64 encoded image data
- `reference_image.format`: string (default=png) (optional)
  Image format
- `reference_image_size`: object (required)
- `reference_image_size.width`: integer (min=32.0, max=128.0) (required)
  Reference image width in pixels (32x32, 64x64, or 128x128)
- `reference_image_size.height`: integer (min=32.0, max=128.0) (required)
  Reference image height in pixels (32x32, 64x64, or 128x128)
- `action`: string (minLen=1, maxLen=500) (required)
  Action description (e.g., 'walk', 'jump', 'attack')
- `image_size`: object (required)
- `image_size.width`: integer (min=32.0, max=128.0) (required)
  Image width in pixels (32x32, 64x64, or 128x128)
- `image_size.height`: integer (min=32.0, max=128.0) (required)
  Image height in pixels (32x32, 64x64, or 128x128)
- `seed`: integer | null (optional)
  Seed for reproducible generation (0 for random)
- `no_background`: boolean | null (optional)
  Remove background from generated frames

### Responses
- **200**: Successfully generated animation
- **401**: Invalid API token
- **402**: Insufficient credits
- **422**: Validation error
- **429**: Too many requests

## POST /estimate-skeleton
**Estimate skeleton**
Tags: Animate

Estimates the skeleton of a character, returning a list of keypoints to use with the skeleton animation tool.

### Parameters

### Request Body
- `image`: object (optional)
  A base64 encoded image.
- `image.type`: string (default=base64) (optional)
  Image data type
- `image.base64`: string (required)
  Base64 encoded image data
- `image.format`: string (default=png) (optional)
  Image format

### Responses
- **200**: Successfully generated image
- **401**: Invalid API token
- **402**: Insufficient credits
- **422**: Validation error
- **429**: Too many requests
- **529**: Rate limit exceeded

# Background Jobs

## GET /background-jobs/{job_id}
**Get background job status**
Tags: Background Jobs

Check the status and results of a background job.

### Parameters
- `job_id` [path]: string (required)

### Responses
- **200**: Successfully retrieved job status
- **401**: Invalid API token
- **404**: Job not found or doesn't belong to user
- **429**: Too many requests
- **422**: Validation Error

# Character Management

## GET /characters
**List user's characters**
Tags: Character Management

List all characters created by the authenticated user.

### Parameters
- `limit` [query]: integer (min=1, max=100) (optional)
  Maximum number of characters to return
- `offset` [query]: integer (min=0) (optional)
  Number of characters to skip

### Responses
- **200**: Successfully retrieved character list
- **401**: Invalid API token
- **422**: Invalid pagination parameters
- **429**: Too many requests

## GET /characters/{character_id}
**Get character details**
Tags: Character Management

Get detailed information about a specific character.

### Parameters
- `character_id` [path]: string (required)

### Responses
- **200**: Successfully retrieved character details
- **401**: Invalid API token
- **403**: Character belongs to another user
- **404**: Character not found
- **429**: Too many requests
- **422**: Validation Error

## DELETE /characters/{character_id}
**Delete a character and all associated data**
Tags: Character Management

Delete a character (v2 API for external customers).

### Parameters
- `character_id` [path]: string (required)

### Responses
- **200**: Successful Response
- **422**: Validation Error

## GET /characters/{character_id}/zip
**Export character as ZIP**
Tags: Character Management

Download a character with all animations as a ZIP file.

### Parameters
- `character_id` [path]: string (required)

### Responses
- **200**: ZIP file download containing character data
- **423**: Character or animations still being generated
- **404**: Character not found
- **422**: Validation Error

## PATCH /characters/{character_id}/tags
**Update character tags**
Tags: Character Management

Update the tags for a specific character.

### Parameters
- `character_id` [path]: string (required)

### Request Body
- `tags`: array[string] (required)
  List of tags to assign to the character

### Responses
- **200**: Tags updated successfully
- **400**: Invalid tag format or validation error
- **401**: Invalid API token
- **403**: Character belongs to another user
- **404**: Character not found
- **429**: Too many requests
- **422**: Validation Error

# Character from template

## POST /create-character-with-4-directions
**Create character with 4 directions**
Tags: Character from template

Generate a character or object facing 4 cardinal directions (south, west, east, north).

### Parameters

### Request Body
- `description`: string (minLen=1, maxLen=2000) (required)
  Description of the character or object to generate
- `image_size`: object (required)
- `image_size.width`: integer (min=32.0, max=400.0) (required)
  Canvas width in pixels (character will be ~60% of canvas size)
- `image_size.height`: integer (min=32.0, max=400.0) (required)
  Canvas height in pixels (character will be ~60% of canvas size)
- `async_mode`: boolean | null (optional)
  Process asynchronously (always true for character creation)
- `text_guidance_scale`: number | null (optional)
  How closely to follow the text description (higher = more faithful)
- `outline`: string | null (optional)
  Outline style (thin, medium, thick, none)
- `shading`: string | null (optional)
  Shading style (soft, hard, flat, none)
- `detail`: string | null (optional)
  Detail level (low, medium, high)
- `view`: string | null (optional)
  Camera view angle (side, low top-down, high top-down, perspective)
- `isometric`: boolean | null (optional)
  Generate in isometric view
- `color_image`: object | null (optional)
  Color palette reference image
- `force_colors`: boolean | null (optional)
  Force the use of colors from color_image
- `proportions`: null (optional)
  Character body proportions (preset or custom values)
- `seed`: integer | null (optional)
  Seed for reproducible generation
- `output_type`: string | null (optional)
  Output format (always dict for external API)

### Responses
- **200**: Successfully generated 4-rotation images
- **401**: Invalid API token
- **402**: Insufficient credits
- **422**: Validation error
- **429**: Too many requests

## POST /create-character-with-8-directions
**Create character with 8 directions**
Tags: Character from template

Generate a character or object facing 8 directions (all cardinal and diagonal directions).

### Parameters

### Request Body
- `description`: string (minLen=1, maxLen=2000) (required)
  Description of the character or object to generate
- `image_size`: object (required)
- `image_size.width`: integer (min=32.0, max=400.0) (required)
  Canvas width in pixels (character will be ~60% of canvas size)
- `image_size.height`: integer (min=32.0, max=400.0) (required)
  Canvas height in pixels (character will be ~60% of canvas size)
- `async_mode`: boolean | null (optional)
  Process asynchronously (always true - no synchronous processing yet)
- `text_guidance_scale`: number | null (optional)
  How closely to follow the text description (higher = more faithful)
- `outline`: string | null (optional)
  Outline style (thin, medium, thick, none)
- `shading`: string | null (optional)
  Shading style (soft, hard, flat, none)
- `detail`: string | null (optional)
  Detail level (low, medium, high)
- `view`: string | null (optional)
  Camera view angle (side, low top-down, high top-down, perspective)
- `isometric`: boolean | null (optional)
  Generate in isometric view
- `color_image`: object | null (optional)
  Color palette reference image
- `force_colors`: boolean | null (optional)
  Force the use of colors from color_image
- `proportions`: null (optional)
  Character body proportions (preset or custom values)
- `seed`: integer | null (optional)
  Seed for reproducible generation
- `output_type`: string | null (optional)
  Output format (always dict for external API)

### Responses
- **200**: Successfully generated 8-rotation images
- **401**: Invalid API token
- **402**: Insufficient credits
- **422**: Validation error
- **429**: Too many requests

## POST /characters/animations
**Create Character Animation**
Tags: Character from template

Animate an existing character (background processing)

### Parameters

### Request Body
- `character_id`: string (required)
  ID of existing character to animate
- `animation_name`: string | null (optional)
  Name for this animation (defaults to action_description if not provided)
- `description`: string | null (optional)
  Description of the character or object to animate (uses character's original if not specified)
- `action_description`: string | null (optional)
  Action description (e.g., 'walking', 'running', 'jumping'). If not provided, uses default description based on template_animation_id.
- `async_mode`: boolean | null (optional)
  Process in background (always true - no foreground processing yet)
- `template_animation_id`: enum[backflip, breathing-idle, cross-punch, crouched-walking, crouching, drinking, falling-back-death, fight-stance-idle-8-frames, fireball, flying-kick, ...] (required)
  Animation template ID
- `text_guidance_scale`: number | null (optional)
  How closely to follow the text description (higher = more faithful)
- `outline`: string | null (optional)
  Outline style (uses character's original if not specified)
- `shading`: string | null (optional)
  Shading style (uses character's original if not specified)
- `detail`: string | null (optional)
  Detail level (uses character's original if not specified)
- `directions`: array | null (optional)
  List of directions to animate (south, north, east, west, etc.). If None, animates all available directions.
- `isometric`: boolean | null (optional)
  Generate in isometric view
- `color_image`: object | null (optional)
  Color palette reference image
- `force_colors`: boolean | null (optional)
  Force the use of colors from color_image
- `seed`: integer | null (optional)
  Seed for reproducible generation

### Responses
- **200**: Successful Response
- **422**: Validation Error

## POST /animate-character
**Animate character with template**
Tags: Character from template

Animate an existing character with multiple frames showing movement or action.

### Parameters

### Request Body
- `character_id`: string (required)
  ID of existing character to animate
- `animation_name`: string | null (optional)
  Name for this animation (defaults to action_description if not provided)
- `description`: string | null (optional)
  Description of the character or object to animate (uses character's original if not specified)
- `action_description`: string | null (optional)
  Action description (e.g., 'walking', 'running', 'jumping'). If not provided, uses default description based on template_animation_id.
- `async_mode`: boolean | null (optional)
  Process in background (always true - no foreground processing yet)
- `template_animation_id`: enum[backflip, breathing-idle, cross-punch, crouched-walking, crouching, drinking, falling-back-death, fight-stance-idle-8-frames, fireball, flying-kick, ...] (required)
  Animation template ID
- `text_guidance_scale`: number | null (optional)
  How closely to follow the text description (higher = more faithful)
- `outline`: string | null (optional)
  Outline style (uses character's original if not specified)
- `shading`: string | null (optional)
  Shading style (uses character's original if not specified)
- `detail`: string | null (optional)
  Detail level (uses character's original if not specified)
- `directions`: array | null (optional)
  List of directions to animate (south, north, east, west, etc.). If None, animates all available directions.
- `isometric`: boolean | null (optional)
  Generate in isometric view
- `color_image`: object | null (optional)
  Color palette reference image
- `force_colors`: boolean | null (optional)
  Force the use of colors from color_image
- `seed`: integer | null (optional)
  Seed for reproducible generation

### Responses
- **200**: Successfully started character animation in background
- **401**: Invalid API token
- **402**: Insufficient credits
- **404**: Character not found
- **422**: Validation error
- **429**: Too many requests

# Create Image

## POST /create-image-pixflux
**Create image (pixflux)**
Tags: Create Image

Creates a pixel art image based on the provided parameters. Called "Create image (new)" in the plugin.

### Parameters

### Request Body
- `description`: string (required)
  Text description of the image to generate
- `negative_description`: string (default=) (optional)
  (Deprecated)
- `image_size`: object (required)
- `image_size.width`: integer (min=16.0, max=400.0) (required)
  Image width in pixels
- `image_size.height`: integer (min=16.0, max=400.0) (required)
  Image height in pixels
- `text_guidance_scale`: number (min=1.0, max=20.0, default=8) (optional)
  How closely to follow the text description
- `outline`: string | null (optional)
  Outline style reference (weakly guiding)
- `shading`: string | null (optional)
  Shading style reference (weakly guiding)
- `detail`: string | null (optional)
  Detail style reference (weakly guiding)
- `view`: string | null (optional)
  Camera view angle (weakly guiding)
- `direction`: string | null (optional)
  Subject direction (weakly guiding)
- `isometric`: boolean (default=False) (optional)
  Generate in isometric view (weakly guiding)
- `no_background`: boolean (default=False) (optional)
  Generate with transparent background, (blank background over 200x200 area)
- `background_removal_task`: enum[remove_simple_background, remove_complex_background] (default=remove_simple_background) (optional)
  Background removal complexity
- `init_image`: object | null (optional)
  Initial image to start from
- `init_image_strength`: integer (min=1.0, max=999.0, default=300) (optional)
  Strength of the initial image influence
- `color_image`: object | null (optional)
  Forced color palette, image containing colors used for palette
- `seed`: integer | null (optional)
  Seed decides the starting noise

### Responses
- **200**: Successfully generated image
- **401**: Invalid API token
- **402**: Insufficient credits
- **422**: Validation error
- **429**: Too many requests
- **529**: Rate limit exceeded

## POST /create-image-bitforge
**Create image (bitforge)**
Tags: Create Image

Generates a pixel art image based on the provided parameters. Called "Create S-M image" in the plugin.

### Parameters

### Request Body
- `description`: string (required)
  Text description of the image to generate
- `negative_description`: string (default=) (optional)
  Text description of what to avoid in the generated image
- `image_size`: object (required)
- `image_size.width`: integer (min=16.0, max=200.0) (required)
  Image width in pixels
- `image_size.height`: integer (min=16.0, max=200.0) (required)
  Image height in pixels
- `text_guidance_scale`: number (min=1.0, max=20.0, default=8.0) (optional)
  How closely to follow the text description
- `extra_guidance_scale`: number (min=0.0, max=20.0, default=3.0) (optional)
  (Deprecated)
- `style_strength`: number (min=0.0, max=100.0, default=0.0) (optional)
  Strength of the style transfer (0-100)
- `outline`: string | null (optional)
  Outline style reference
- `shading`: string | null (optional)
  Shading style reference
- `detail`: string | null (optional)
  Detail style reference
- `view`: string | null (optional)
  Camera view angle
- `direction`: string | null (optional)
  Subject direction
- `isometric`: boolean (default=False) (optional)
  Generate in isometric view
- `oblique_projection`: boolean (default=False) (optional)
  Generate in oblique projection
- `no_background`: boolean (default=False) (optional)
  Generate with transparent background
- `coverage_percentage`: number | null (optional)
  Percentage of the canvas to cover
- `init_image`: object | null (optional)
  Initial image to start from
- `init_image_strength`: integer (min=1.0, max=999.0, default=300) (optional)
  Strength of the initial image influence
- `style_image`: object | null (optional)
  Reference image for style transfer
- `inpainting_image`: object | null (optional)
  Reference image which is inpainted
- `mask_image`: object | null (optional)
  Inpainting / mask image (black and white image, where the white is where the model should inpaint)
- `color_image`: object | null (optional)
  Forced color palette, image containing colors used for palette
- `skeleton_guidance_scale`: number (min=0.0, max=5.0, default=1.0) (optional)
  How closely to follow the skeleton keypoints
- `skeleton_keypoints`: array | null (optional)
  Skeleton points. Warning! Sizes that are not 16x16, 32x32 and 64x64 can cause the generations to be lower quality
- `seed`: integer | null (optional)
  Seed decides the starting noise

### Responses
- **200**: Successfully generated image
- **401**: Invalid API token
- **402**: Insufficient credits
- **422**: Validation error
- **429**: Too many requests
- **529**: Rate limit exceeded

## POST /generate-image-v2
**Generate pixel art images (v2)**
Tags: Create Image

Generate pixel art images from text description.

### Parameters

### Request Body
- `description`: string (minLen=1, maxLen=2000) (required)
  Description of the image to generate
- `image_size`: object (required)
- `image_size.width`: integer (min=32.0, max=256.0) (required)
  Image width in pixels (32, 64, 128, or 256)
- `image_size.height`: integer (min=32.0, max=256.0) (required)
  Image height in pixels (32, 64, 128, or 256)
- `seed`: integer | null (optional)
  Seed for reproducible generation
- `no_background`: boolean | null (optional)
  Remove background from generated images
- `reference_images`: array | null (optional)
  Optional reference images for subject guidance (up to 4)
- `style_image`: object | null (optional)
  Optional style image for pixel size and style reference
- `style_options`: object (optional)
  Options for what to copy from the style image.
- `style_options.color_palette`: boolean (default=True) (optional)
  Copy color palette from style image
- `style_options.outline`: boolean (default=True) (optional)
  Copy outline style
- `style_options.detail`: boolean (default=True) (optional)
  Copy detail level
- `style_options.shading`: boolean (default=True) (optional)
  Copy shading style

### Responses
- **200**: Successfully generated images
- **401**: Invalid API token
- **402**: Insufficient credits
- **422**: Validation error
- **429**: Too many requests

## POST /generate-with-style-v2
**Generate images matching a style (v2)**
Tags: Create Image

Generate new pixel art images that match the style of reference images.

### Parameters

### Request Body
- `style_images`: array[object] (required)
  Style reference images (1-4 images)
- `description`: string (minLen=1, maxLen=2000) (required)
  Description of what to generate
- `image_size`: object (required)
- `image_size.width`: integer (min=32.0, max=256.0) (required)
  Image width (32, 64, 128, or 256)
- `image_size.height`: integer (min=32.0, max=256.0) (required)
  Image height (32, 64, 128, or 256)
- `style_description`: string | null (optional)
  Description of the style to match
- `seed`: integer | null (optional)
  Seed for reproducible generation
- `no_background`: boolean | null (optional)
  Remove background from generated images

### Responses
- **200**: Successfully generated images
- **401**: Invalid API token
- **402**: Insufficient credits
- **422**: Validation error
- **429**: Too many requests

# Create map

## POST /tilesets
**Create a tileset asynchronously**
Tags: Create map

Creates a Wang tileset (16 tiles for standard, 23 for transition_size=1.0) in the background and returns immediately with job ID

### Parameters

### Request Body
- `lower_description`: string (minLen=1) (required)
  Description of the lower/base terrain level (e.g., 'ocean', 'grass', 'lava')
- `upper_description`: string (minLen=1) (required)
  Description of the upper/elevated terrain level (e.g., 'sand', 'stone', 'snow')
- `transition_description`: string (default=) (optional)
  Optional description of transition area between lower and upper
- `lower_base_tile_id`: string | null (optional)
  Optional ID to identify the lower base tile in metadata
- `upper_base_tile_id`: string | null (optional)
  Optional ID to identify the upper base tile in metadata
- `tile_size`: object (optional)
- `tile_size.width`: enum[16, 32] (default=16) (optional)
  Individual tile width in pixels (16 or 32)
- `tile_size.height`: enum[16, 32] (default=16) (optional)
  Individual tile height in pixels (16 or 32)
- `text_guidance_scale`: number (min=1.0, max=20.0, default=8.0) (optional)
  How closely to follow the text descriptions (default: 8.0)
- `outline`: string | null (optional)
  Outline style reference
- `shading`: string | null (optional)
  Shading style reference
- `detail`: string | null (optional)
  Detail style reference
- `view`: enum[low top-down, high top-down] (optional)
  Camera view options supported for tileset generation
- `tile_strength`: number (min=0.1, max=2.0, default=1.0) (optional)
  Strength of tile pattern adherence
- `tileset_adherence_freedom`: number (min=0.0, max=900.0, default=500.0) (optional)
  How flexible it will be when following tileset structure, higher values means more flexibility
- `tileset_adherence`: number (min=0.0, max=500.0, default=100.0) (optional)
  How much it will follow the reference/texture image and follow tileset structure
- `transition_size`: enum[0.0, 0.25, 0.5, ...] (default=0.0) (optional)
  Size of transition area (0 = no transition, 0.25 = quarter tile, 0.5 = half tile, 1.0 = full tile)
- `lower_reference_image`: object | null (optional)
  Reference image for lower terrain style
- `upper_reference_image`: object | null (optional)
  Reference image for upper terrain style
- `transition_reference_image`: object | null (optional)
  Reference image for transition area style
- `color_image`: object | null (optional)
  Reference image for color palette
- `seed`: integer | null (optional)
  Seed for reproducible generation

### Responses
- **202**: Tileset creation started, returns job ID
- **401**: Invalid API token
- **402**: Insufficient credits
- **422**: Validation error
- **429**: Too many requests
- **529**: Rate limit exceeded

## POST /create-tileset
**Create top-down tileset (async processing)**
Tags: Create map

Creates a complete tileset for game development with seamlessly connecting tiles.

### Parameters

### Request Body
- `lower_description`: string (minLen=1) (required)
  Description of the lower/base terrain level (e.g., 'ocean', 'grass', 'lava')
- `upper_description`: string (minLen=1) (required)
  Description of the upper/elevated terrain level (e.g., 'sand', 'stone', 'snow')
- `transition_description`: string (default=) (optional)
  Optional description of transition area between lower and upper
- `lower_base_tile_id`: string | null (optional)
  Optional ID to identify the lower base tile in metadata
- `upper_base_tile_id`: string | null (optional)
  Optional ID to identify the upper base tile in metadata
- `tile_size`: object (optional)
- `tile_size.width`: enum[16, 32] (default=16) (optional)
  Individual tile width in pixels (16 or 32)
- `tile_size.height`: enum[16, 32] (default=16) (optional)
  Individual tile height in pixels (16 or 32)
- `text_guidance_scale`: number (min=1.0, max=20.0, default=8.0) (optional)
  How closely to follow the text descriptions (default: 8.0)
- `outline`: string | null (optional)
  Outline style reference
- `shading`: string | null (optional)
  Shading style reference
- `detail`: string | null (optional)
  Detail style reference
- `view`: enum[low top-down, high top-down] (optional)
  Camera view options supported for tileset generation
- `tile_strength`: number (min=0.1, max=2.0, default=1.0) (optional)
  Strength of tile pattern adherence
- `tileset_adherence_freedom`: number (min=0.0, max=900.0, default=500.0) (optional)
  How flexible it will be when following tileset structure, higher values means more flexibility
- `tileset_adherence`: number (min=0.0, max=500.0, default=100.0) (optional)
  How much it will follow the reference/texture image and follow tileset structure
- `transition_size`: enum[0.0, 0.25, 0.5, ...] (default=0.0) (optional)
  Size of transition area (0 = no transition, 0.25 = quarter tile, 0.5 = half tile, 1.0 = full tile)
- `lower_reference_image`: object | null (optional)
  Reference image for lower terrain style
- `upper_reference_image`: object | null (optional)
  Reference image for upper terrain style
- `transition_reference_image`: object | null (optional)
  Reference image for transition area style
- `color_image`: object | null (optional)
  Reference image for color palette
- `seed`: integer | null (optional)
  Seed for reproducible generation

### Responses
- **202**: Successful Response
- **200**: Successfully generated tileset
- **401**: Invalid API token
- **402**: Insufficient credits
- **422**: Validation error
- **429**: Too many requests
- **529**: Rate limit exceeded

## GET /tilesets/{tileset_id}
**Get generated tileset by ID**
Tags: Create map

Retrieve a completed tileset by its UUID.

### Parameters
- `tileset_id` [path]: string (required)

### Responses
- **200**: Successfully retrieved tileset
- **423**: Tileset is still being generated
- **404**: Tileset not found
- **401**: Invalid API token
- **422**: Validation Error

## POST /create-isometric-tile
**Create isometric tile (async processing)**
Tags: Create map

Creates a isometric tile based on the provided parameters.

### Parameters

### Request Body
- `description`: string (required)
  Text description of the image to generate
- `image_size`: object (required)
- `image_size.width`: integer (min=16.0, max=64.0) (required)
  Image width in pixels. Sizes above 24px often give better results.
- `image_size.height`: integer (min=16.0, max=64.0) (required)
  Image height in pixels. Sizes above 24px often give better results.
- `text_guidance_scale`: number (min=1.0, max=20.0, default=8) (optional)
  How closely to follow the text description
- `outline`: string | null (optional)
  Outline style for the tile
- `shading`: string | null (optional)
  Shading complexity
- `detail`: string | null (optional)
  Level of detail in the tile
- `init_image`: object | null (optional)
  Initial image to start from
- `init_image_strength`: integer (min=1.0, max=999.0, default=300) (optional)
  Strength of the initial image influence
- `isometric_tile_size`: integer | null (optional)
  Size of the isometric tile. Recommended sizes: 16, 32. Can be omitted for default.
- `isometric_tile_shape`: enum[thick tile, thin tile, block] (default=block) (optional)
  Tile thickness. thin tile: ~15% canvas height, thick tile: ~25% height, block: ~50% height
- `color_image`: object | null (optional)
  Forced color palette, image containing colors used for palette
- `seed`: integer | null (optional)
  Seed decides the starting noise

### Responses
- **202**: Successful Response
- **200**: Successfully generated image
- **401**: Invalid API token
- **402**: Insufficient credits
- **422**: Validation error
- **429**: Too many requests
- **529**: Rate limit exceeded

## GET /isometric-tiles/{tile_id}
**Get generated isometric tile by ID**
Tags: Create map

Retrieve a completed isometric tile by its UUID.

### Parameters
- `tile_id` [path]: string (required)

### Responses
- **200**: Successfully retrieved tile
- **404**: Tile not found
- **401**: Invalid API token
- **423**: Tile still processing
- **422**: Validation Error

# Documentation

## GET /llms.txt
**Get LLM-friendly API documentation**
Tags: Documentation

Returns API documentation formatted for Large Language Models (LLMs).

### Responses
- **200**: LLM-friendly API documentation

# Edit

## POST /edit-image
**Edit image**
Tags: Edit

Edit an existing pixel art image based on a text description.

### Parameters

### Request Body
- `image`: object (required)
  A base64 encoded image.
- `image.type`: string (default=base64) (optional)
  Image data type
- `image.base64`: string (required)
  Base64 encoded image data
- `image.format`: string (default=png) (optional)
  Image format
- `image_size`: object (required)
- `image_size.width`: integer (min=16.0, max=400.0) (required)
  Image width in pixels (16-400)
- `image_size.height`: integer (min=16.0, max=400.0) (required)
  Image height in pixels (16-400)
- `description`: string (minLen=1, maxLen=500) (required)
  Text description of the edit to apply
- `width`: integer (min=16.0, max=400.0) (required)
  Target canvas width in pixels (16-400)
- `height`: integer (min=16.0, max=400.0) (required)
  Target canvas height in pixels (16-400)
- `seed`: integer | null (optional)
  Seed for reproducible generation (0 for random)
- `no_background`: boolean | null (optional)
  Generate with transparent background
- `text_guidance_scale`: number | null (optional)
  How closely to follow the text description (1.0-10.0)
- `color_image`: object | null (optional)
  Color reference image for style guidance

### Responses
- **200**: Successful Response
- **422**: Validation Error

## POST /edit-images-v2
**Edit pixel art images (v2)**
Tags: Edit

Edit pixel art images using text or reference image.

### Parameters

### Request Body
- `method`: enum[edit_with_text, edit_with_reference] (default=edit_with_text) (optional)
  Edit method: 'edit_with_text' or 'edit_with_reference'
- `edit_images`: array[object] (required)
  Images to edit (1-16 images depending on size)
- `image_size`: object (required)
- `image_size.width`: integer (min=32.0, max=256.0) (required)
  Image width in pixels
- `image_size.height`: integer (min=32.0, max=256.0) (required)
  Image height in pixels
- `description`: string | null (optional)
  Edit description (required for edit_with_text method)
- `reference_image`: object | null (optional)
  Reference image (required for edit_with_reference method)
- `seed`: integer | null (optional)
  Seed for reproducible generation
- `no_background`: boolean | null (optional)
  Remove background from edited images

### Responses
- **200**: Successfully edited images
- **401**: Invalid API token
- **402**: Insufficient credits
- **422**: Validation error
- **429**: Too many requests

# Image Operations

## POST /image-to-pixelart
**Convert image to pixel art**
Tags: Image Operations

Convert regular images to pixel art style.

### Parameters

### Request Body
- `image`: object (required)
  A base64 encoded image.
- `image.type`: string (default=base64) (optional)
  Image data type
- `image.base64`: string (required)
  Base64 encoded image data
- `image.format`: string (default=png) (optional)
  Image format
- `image_size`: object (required)
  Image dimensions
- `image_size.width`: integer (min=16.0, max=1280.0) (required)
  Width in pixels
- `image_size.height`: integer (min=16.0, max=1280.0) (required)
  Height in pixels
- `output_size`: object (required)
  Output dimensions
- `output_size.width`: integer (min=16.0, max=320.0) (required)
  Width in pixels
- `output_size.height`: integer (min=16.0, max=320.0) (required)
  Height in pixels
- `text_guidance_scale`: number | null (optional)
  How closely to follow pixel art style
- `seed`: integer | null (optional)
  Seed for reproducible generation

### Responses
- **200**: Successfully converted image
- **400**: Invalid image size constraints
- **401**: Invalid API token
- **402**: Insufficient credits
- **422**: Validation error
- **429**: Too many requests

## POST /resize
**Resize pixel art image**
Tags: Image Operations

Intelligently resize pixel art images while maintaining pixel art aesthetics.

### Parameters

### Request Body
- `description`: string (minLen=1, maxLen=2000) (required)
  Description of your character
- `reference_image`: object (required)
  A base64 encoded image.
- `reference_image.type`: string (default=base64) (optional)
  Image data type
- `reference_image.base64`: string (required)
  Base64 encoded image data
- `reference_image.format`: string (default=png) (optional)
  Image format
- `reference_image_size`: object (required)
  Image dimensions
- `reference_image_size.width`: integer (min=16.0, max=200.0) (required)
  Width in pixels
- `reference_image_size.height`: integer (min=16.0, max=200.0) (required)
  Height in pixels
- `target_size`: object (required)
  Image dimensions
- `target_size.width`: integer (min=16.0, max=200.0) (required)
  Width in pixels
- `target_size.height`: integer (min=16.0, max=200.0) (required)
  Height in pixels
- `view`: string | null (optional)
  Camera view angle
- `direction`: string | null (optional)
  Directional view
- `isometric`: boolean | null (optional)
  Isometric perspective
- `oblique_projection`: boolean | null (optional)
  Oblique projection (beta)
- `no_background`: boolean | null (optional)
  Remove background
- `color_image`: object | null (optional)
  Forced color palette, image containing colors used for palette
- `init_image`: object | null (optional)
  Initial image to start from
- `init_image_strength`: number | null (optional)
  Strength of initial image influence
- `seed`: integer | null (optional)
  Seed for reproducible generation

### Responses
- **200**: Successfully resized image
- **400**: Invalid image size constraints
- **401**: Invalid API token
- **402**: Insufficient credits
- **422**: Validation error
- **429**: Too many requests

# Inpaint

## POST /inpaint
**Inpaint image**
Tags: Inpaint

Creates a pixel art image based on the provided parameters. Called "Inpaint" in the plugin.

### Parameters

### Request Body
- `description`: string (required)
  Text description of the image to generate
- `negative_description`: string (default=) (optional)
  Text description of what to avoid in the generated image
- `image_size`: object (required)
- `image_size.width`: integer (min=16.0, max=200.0) (required)
  Image width in pixels
- `image_size.height`: integer (min=16.0, max=200.0) (required)
  Image height in pixels
- `text_guidance_scale`: number (min=1.0, max=10.0, default=3.0) (optional)
  How closely to follow the text description
- `extra_guidance_scale`: number (min=0.0, max=20.0, default=3.0) (optional)
  (Deprecated)
- `outline`: string | null (optional)
  Outline style reference
- `shading`: string | null (optional)
  Shading style reference
- `detail`: string | null (optional)
  Detail style reference
- `view`: string | null (optional)
  Camera view angle
- `direction`: string | null (optional)
  Subject direction
- `isometric`: boolean (default=False) (optional)
  Generate in isometric view
- `oblique_projection`: boolean (default=False) (optional)
  Generate in oblique projection
- `no_background`: boolean (default=False) (optional)
  Generate with transparent background
- `init_image`: object | null (optional)
  Initial image to start from
- `init_image_strength`: integer (min=1.0, max=999.0, default=300) (optional)
  Strength of the initial image influence
- `inpainting_image`: object (required)
  A base64 encoded image.
- `inpainting_image.type`: string (default=base64) (optional)
  Image data type
- `inpainting_image.base64`: string (required)
  Base64 encoded image data
- `inpainting_image.format`: string (default=png) (optional)
  Image format
- `mask_image`: object (required)
  A base64 encoded image.
- `mask_image.type`: string (default=base64) (optional)
  Image data type
- `mask_image.base64`: string (required)
  Base64 encoded image data
- `mask_image.format`: string (default=png) (optional)
  Image format
- `color_image`: object | null (optional)
  Forced color palette, image containing colors used for palette
- `seed`: integer | null (optional)
  Seed decides the starting noise

### Responses
- **200**: Successfully generated image
- **401**: Invalid API token
- **402**: Insufficient credits
- **422**: Validation error
- **429**: Too many requests
- **529**: Rate limit exceeded

# Map Objects

## POST /map-objects
**Create map object**
Tags: Map Objects

Creates a pixel art object with transparent background for game maps.

### Parameters

### Request Body
- `description`: string (minLen=1, maxLen=2000) (required)
  Object description (e.g., 'wooden barrel', 'stone fountain')
- `image_size`: object (optional)
  Image dimensions for map objects.
  Supports any aspect ratio:
  - Both width and height: 32px minimum, 400px maximum
  - Basic mode (no inpainting): max 400x400 total area (160,000 pixels)
  - Inpainting mode: max 192x192 total area (36,864 pixels)
  - Common sizes: 64x64, 128x128, 192x192, 256x128, 384x96
- `image_size.width`: integer (min=32.0, max=400.0) (required)
  Width in pixels (32-400)
- `image_size.height`: integer (min=32.0, max=400.0) (required)
  Height in pixels (32-400)
- `view`: enum[low top-down, high top-down, side] (default=high top-down) (optional)
  Camera angle
- `outline`: string | null (optional)
  Outline style
- `shading`: string | null (optional)
  Shading complexity
- `detail`: string | null (optional)
  Level of detail
- `text_guidance_scale`: number (min=1.0, max=20.0, default=8.0) (optional)
  How closely to follow the description
- `init_image`: object | null (optional)
  Initial image to start from
- `init_image_strength`: integer (min=1.0, max=999.0, default=300) (optional)
  Strength of initial image influence
- `color_image`: object | null (optional)
  Image containing colors for forced palette
- `background_image`: object | null (optional)
  Background/map image for style matching. Required when using inpainting.
- `inpainting`: object | object | object | null (optional)
  Inpainting configuration for style matching. Options: mask (custom), oval (auto-generated), rectangle (auto-generated)
- `seed`: integer | null (optional)
  Seed for reproducible generation

### Responses
- **200**: Object generation queued
- **401**: Invalid API token
- **402**: Insufficient credits
- **422**: Validation error
- **429**: Too many requests

# Rotate

## POST /rotate
**Rotate character or object**
Tags: Rotate

Rotates a pixel art image based on the provided parameters. Called "Rotate" in the plugin.

### Parameters

### Request Body
- `image_size`: object (required)
- `image_size.width`: integer (min=16.0, max=200.0) (required)
  Image width in pixels
- `image_size.height`: integer (min=16.0, max=200.0) (required)
  Image height in pixels
- `image_guidance_scale`: number (min=1.0, max=20.0, default=3.0) (optional)
  How closely to follow the reference image
- `view_change`: integer | null (optional)
  How many degrees to tilt the subject
- `direction_change`: integer | null (optional)
  How many degrees to rotate the subject
- `from_view`: string | null (optional)
  From camera view angle
- `to_view`: string | null (optional)
  To camera view angle
- `from_direction`: string | null (optional)
  From subject direction
- `to_direction`: string | null (optional)
  To subject direction
- `isometric`: boolean (default=False) (optional)
  Generate in isometric view
- `oblique_projection`: boolean (default=False) (optional)
  Generate in oblique projection
- `init_image`: object | null (optional)
  Initial image to start from
- `init_image_strength`: integer (min=1.0, max=999.0, default=300) (optional)
  Strength of the initial image influence
- `mask_image`: object | null (optional)
  Inpainting / mask image. Requires init image!
- `from_image`: object (required)
  A base64 encoded image.
- `from_image.type`: string (default=base64) (optional)
  Image data type
- `from_image.base64`: string (required)
  Base64 encoded image data
- `from_image.format`: string (default=png) (optional)
  Image format
- `color_image`: object | null (optional)
  Forced color palette, image containing colors used for palette
- `seed`: integer | null (optional)
  Seed decides the starting noise

### Responses
- **200**: Successfully generated image
- **401**: Invalid API token
- **402**: Insufficient credits
- **422**: Validation error
- **429**: Too many requests
- **529**: Rate limit exceeded

## POST /generate-8-rotations-v2
**Generate 8 rotational views (v2)**
Tags: Rotate

Generate 8 rotational views of a character or object.

### Parameters

### Request Body
- `method`: enum[rotate_character, create_with_style, create_from_concept] (default=rotate_character) (optional)
  Generation method: 'rotate_character' rotates an existing character, 'create_with_style' creates new character matching style, 'create_from_concept' creates from concept art
- `image_size`: object (required)
- `image_size.width`: integer (min=32.0, max=84.0) (required)
  Image width (32-84 pixels)
- `image_size.height`: integer (min=32.0, max=84.0) (required)
  Image height (32-84 pixels)
- `reference_image`: object | null (optional)
  Image to rotate (rotate_character) or style reference
- `concept_image`: object | null (optional)
  Concept art image (only for create_from_concept method)
- `description`: string | null (optional)
  Description of the character/item
- `style_description`: string | null (optional)
  Description of the visual style
- `view`: enum[low top-down, high top-down, side] (default=low top-down) (optional)
  Camera perspective angle
- `seed`: integer | null (optional)
  Seed for reproducible generation
- `no_background`: boolean | null (optional)
  Remove background from generated images

### Responses
- **200**: Successfully generated 8 rotations
- **401**: Invalid API token
- **402**: Insufficient credits
- **422**: Validation error
- **429**: Too many requests

## Usage Examples

### Create a Character (Python)
```python
import requests

response = requests.post(
    "https://api.pixellab.ai/v2/create-character-with-4-directions",
    headers={
        "Authorization": "Bearer YOUR_TOKEN",
        "Content-Type": "application/json"
    },
    json={
        "description": "brave knight with shining armor",
        "image_size": {"width": 64, "height": 64}
    }
)

job_id = response.json()['background_job_id']
character_id = response.json()['character_id']
```

### Check Job Status
```python
status_response = requests.get(
    "https://api.pixellab.ai/v2/background-jobs/{job_id}",
    headers={
        "Authorization": "Bearer YOUR_TOKEN"
    }
)

if status_response.json()['status'] == 'completed':
    print('Character ready!')
```

## Error Codes
- **400**: Bad Request - Invalid parameters
- **401**: Unauthorized - Invalid or missing token
- **402**: Payment Required - Insufficient credits
- **403**: Forbidden - Feature not available for your tier
- **404**: Not Found - Resource doesn't exist
- **423**: Locked - Resource still being generated
- **429**: Too Many Requests - Rate limit exceeded
- **500**: Internal Server Error

## Support
- Documentation: https://api.pixellab.ai/v2/docs
- Python Client: https://github.com/pixellab-code/pixellab-python
- Discord: https://discord.gg/pBeyTBF8T7
- Email: support@pixellab.ai
