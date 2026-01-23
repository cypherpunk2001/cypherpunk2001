# MMORPG Performance Analysis - Unified Report

**Date**: 2026-01-23
**Sources**: Claude analysis + Codex analysis
**Scope**: Analysis against CLAUDE.md / AGENTS.md SBCL Performance Directives
**Target**: 60 Hz tick loop (~16.67ms budget)
**Files Analyzed**: `src/*.lisp` (runtime code), `data/*.lisp` (load-once data definitions)

---

## Attribution Key

- **[COMMON]** - Found by both Claude and Codex analyses
- **[CLAUDE-ONLY]** - Found uniquely by Claude analysis
- **[CODEX-ONLY]** - Found uniquely by Codex analysis

---

## Executive Summary

### Common Assessment (Both Analyses Agree)
The codebase demonstrates **solid foundational architecture** for performance:
- Uses `defstruct` for hot data (not CLOS classes)
- Implements spatial grid for O(1) proximity queries
- Has render chunk caching with FBO pre-rendering
- Network code uses buffer pooling and delta compression
- Arrays are used for players/NPCs and entity iteration

### Critical Gaps Identified (Both Analyses Agree)
1. **Missing type declarations throughout hot paths**
2. **No environment-driven optimization policy (dev vs prod)**
3. **Hot-loop allocations** (spatial grid, snapshots, zone tracking)
4. **No SIMD kernels in compute-bound areas**

### Additional Critical Gap (Codex-Only)
5. **CLOS generic dispatch in innermost tick/draw loops** - violates "no CLOS in innermost loops" requirement

---

## Detailed Findings by Category

---

### 1. Hot Loop Optimization

#### 1.1 [CLAUDE-ONLY] GOOD: Update/Draw Separation
The codebase correctly separates simulation from rendering:
- `server.lisp`: Runs only `update-sim` (no rendering)
- `main.lisp`: Client runs both update and draw phases

#### 1.2 [COMMON] VIOLATION: String/Allocation in Snapshot Processing

**Claude's Finding** - String Concatenation in Hot Path:
**File**: `src/net.lisp` (snapshot chunk reassembly)
```lisp
(let* ((combined (apply #'concatenate 'string (nreverse parts))))
```
**Issue**: Creates new string on every chunked snapshot reassembly.

**Codex's Finding** - Snapshot serialization allocates every tick:
- `encode-net-message` uses `with-output-to-string` and `prin1`: `src/net.lisp:23-26`
- `string-to-octets` allocates a new `(unsigned-byte 8)` array per message: `src/net.lisp:8-14`
- Server sends snapshots every tick and creates event plists each tick: `src/net.lisp:2465-2481`, `src/net.lisp:2470-2472`, `src/net.lisp:1170-1174`

**Combined Recommendation**:
```lisp
;; Claude's fix for chunk reassembly:
(defparameter *reassembly-buffer*
  (make-array *max-snapshot-size* :element-type 'character :fill-pointer 0))

(setf (fill-pointer *reassembly-buffer*) 0)
(dolist (part parts)
  (let ((start (fill-pointer *reassembly-buffer*)))
    (incf (fill-pointer *reassembly-buffer*) (length part))
    (replace *reassembly-buffer* part :start1 start)))

;; Codex's broader recommendation:
;; Move to binary serialization into reusable unsigned-byte 8 buffers
;; (per connection or per frame) and avoid intermediate plist/string objects.
;; Pre-allocate event serialization buffers or use a ring buffer of structs.
```

#### 1.3 [CLAUDE-ONLY] ACCEPTABLE: Format String in Verbose Logging
**Files**: Multiple (movement.lisp, net.lisp, combat.lisp)
```lisp
(log-verbose "Player ~A moved to (~,2F, ~,2F)" player-id x y)
```
**Issue**: Even when `*verbose*` is nil, the format string is still parsed.
**Status**: Acceptable - `log-verbose` macro short-circuits before format. No action needed.

#### 1.4 [CLAUDE-ONLY] OPPORTUNITY: Hash Table Iteration in Hot Loops
**File**: `src/server.lisp`, `src/movement.lisp`
```lisp
(maphash (lambda (id player) ...) *players*)
```
**Issue**: Hash table iteration allocates closure on each call.
**Recommendation**: For truly hot loops (per-tick entity iteration), consider:
- Pre-extracted player arrays for iteration
- `with-hash-table-iterator` macro (still allocates, but less)
- Or accept this as bounded overhead (player count is bounded)

#### 1.5 [CODEX-ONLY] HIGH: CLOS Generic Dispatch in Innermost Tick/Draw Loops
**Evidence**:
- Per-entity update uses generics in tick loop: `src/main.lisp:488-490` calling `update-entity-animation` and `combatant-update-hit-effect` (generic methods defined at `src/combat.lisp:678-682`, `src/types.lisp:674-681`)
- Render loop uses `draw-entity` generic per entity: `src/rendering.lisp:75-95`, methods at `src/rendering.lisp:1578-1582`
- `combatant-position` generic used in per-entity visibility checks: `src/rendering.lisp:47-50`, `src/types.lisp:656-657`

**Impact**: Dynamic dispatch per entity per frame increases overhead and hinders type-specialized optimization.

**Recommendation**:
- Split hot loops by entity type (separate NPC and player loops) and call direct functions (`update-npc-animation`, `update-player-animation`, `draw-npc`, `draw-player`)
- If a unified loop is required, add an explicit type tag in structs and dispatch via `case`/`etypecase` (still avoids generic dispatch)

#### 1.6 [CODEX-ONLY] MEDIUM: Per-Tick List/Vector Construction for Zone Tracking
**Evidence**:
- `occupied-zone-ids` allocates a list each call: `src/movement.lisp:51-58`; used twice per tick in `update-sim`: `src/main.lisp:450-454` and `src/main.lisp:493-497`
- `players-in-zone` allocates list + vector on fallback path: `src/movement.lisp:42-49`; used in `update-sim`: `src/main.lisp:501-509`
- `zone-state-player-count` uses `count-if` with lambda over vector: `src/movement.lisp:38-40` (called in tick path at `src/main.lisp:504-507`)

**Impact**: Avoidable per-tick consing and extra passes over player arrays.

**Recommendation**:
- Maintain a per-tick cached vector of active zone IDs, built once per tick and reused
- Prefer the existing `zone-state-zone-players` cache for all per-zone iteration; ensure it is always populated to avoid `players-in-zone` allocations
- Replace `count-if` with a simple `loop` + fixnum accumulator to avoid closure/generic sequence overhead

---

### 2. Data Layout

#### 2.1 [COMMON] GOOD: Defstruct Usage
All core game entities use `defstruct`:
- `player` struct in types.lisp
- `npc` struct in types.lisp
- `zone-state` struct in types.lisp
- `render-chunk-cache` struct in types.lisp

This follows CLAUDE.md directive to use typed defstruct over CLOS for hot data.

**Codex Compliance Note**: Arrays are used for players/NPCs and entity iteration (`src/main.lisp` loops), which is compatible with hot-loop performance requirements.

#### 2.2 [COMMON] VIOLATION: Missing Type Declarations in Structs
**File**: `src/types.lisp`

**Claude's Detail**:
```lisp
;; Current (types.lisp)
(defstruct player
  (id nil)
  (x 0.0)
  (y 0.0)
  (dx 0.0)
  (dy 0.0)
  (hp 100)
  (max-hp 100)
  ...)
```

**Codex's Detail**:
- Core hot data structs are untyped: `src/types.lisp:4-42` (player/npc), `src/types.lisp:95-102` (world)

**Combined Recommendation**:
```lisp
(defstruct player
  (id nil :type (or null fixnum))
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (dx 0.0 :type single-float)
  (dy 0.0 :type single-float)
  (hp 100 :type fixnum)
  (max-hp 100 :type fixnum)
  ...)
```

**Affected structs requiring type annotations**:
- `player` - x, y, dx, dy, hp, max-hp, xp, level, gold
- `npc` - x, y, dx, dy, hp, max-hp
- `zone-state` - width, height, tile arrays
- `interpolation-buffer` - timestamps, positions
- `prediction-state` - numeric fields
- `world` - (per Codex)

#### 2.3 [CLAUDE-ONLY] VIOLATION: No Typed Arrays for Numeric Data
**File**: `src/types.lisp`, `src/spatial.lisp`

Grid cells use generic lists:
```lisp
(defstruct spatial-grid
  (cells nil)  ; Should be typed 2D array
  (cell-size 64))
```

**Recommended for spatial grid**:
```lisp
(defstruct spatial-grid
  (cells nil :type (simple-array list (* *)))
  (cell-size 64 :type fixnum)
  (width 0 :type fixnum)
  (height 0 :type fixnum))
```

---

### 3. Numeric Rules

#### 3.1 [CLAUDE-ONLY] VIOLATION: Undeclared Loop Variables
**File**: `src/movement.lisp`

```lisp
(defun blocked-at-p (zone-state x y)
  (let ((tile-x (floor x +tile-size+))
        (tile-y (floor y +tile-size+)))
    ...))
```

**Issue**: `tile-x`, `tile-y` not declared as fixnum. SBCL may box intermediate values.

**Recommended**:
```lisp
(defun blocked-at-p (zone-state x y)
  (declare (type single-float x y))
  (let ((tile-x (the fixnum (floor x +tile-size+)))
        (tile-y (the fixnum (floor y +tile-size+))))
    (declare (type fixnum tile-x tile-y))
    ...))
```

#### 3.2 [CLAUDE-ONLY] VIOLATION: Mixed Numeric Types
**File**: `src/combat.lisp`

```lisp
(let ((damage (- attack defense)))
  (decf (player-hp target) damage))
```

**Issue**: If `attack`/`defense` are fixnum but subtraction could theoretically overflow, SBCL may use generic arithmetic.

**Recommended**:
```lisp
(let ((damage (the fixnum (- (the fixnum attack) (the fixnum defense)))))
  (declare (type fixnum damage))
  (decf (player-hp target) (max 0 damage)))
```

#### 3.3 [CLAUDE-ONLY] VIOLATION: Undeclared Function Parameters
**File**: `src/movement.lisp`

```lisp
(defun attempt-move (player dx dy dt zone-state)
  ...)
```

**Recommended**:
```lisp
(defun attempt-move (player dx dy dt zone-state)
  (declare (type player player)
           (type single-float dx dy dt)
           (type zone-state zone-state))
  ...)
```

---

### 4. Pools/Scratch/Arenas

#### 4.1 [COMMON] GOOD: Snapshot/Interpolation Pooling

**Claude's Finding**:
**File**: `src/net.lisp`
```lisp
(serialize-game-state-for-zone ... :use-pool t)
```
The network code supports pooled snapshot serialization.

**Codex's Finding**:
- Interpolation buffer uses a pool of position tables to avoid per-snapshot allocation: `src/net.lisp:1487-1543`, `src/types.lisp:231-244`
- Snapshot encoding is done once per tick and reused for all clients (`send-snapshots-parallel`): `src/net.lisp:1375-1408`

#### 4.2 [COMMON] VIOLATION: Hash Tables Without Pre-sizing / Spatial Grid Allocations

**Claude's Finding**:
**File**: `src/movement.lisp`
```lisp
(defparameter *zone-states* (make-hash-table :test 'eq))
```

**File**: `src/net.lisp`
```lisp
(defparameter *player-sessions* (make-hash-table :test 'equal))
```

**Issue**: Hash tables grow dynamically, causing GC pressure and rehashing during gameplay.

**Codex's Finding** (more detailed):
- Grid uses cons keys + list values: `src/spatial.lisp:16-46`
- Neighbor/rect queries build fresh lists using `copy-list` + `nconc`: `src/spatial.lisp:92-146`
- Hot call sites: AI target selection `src/ai.lisp:12-18`, melee resolution `src/combat.lisp:695-704`, render culling `src/rendering.lisp:75-78`

**Combined Recommendation**:
```lisp
;; Claude's pre-sizing recommendation:
(defparameter *zone-states*
  (make-hash-table :test 'eq :size 64))  ; Expected max zones

(defparameter *player-sessions*
  (make-hash-table :test 'equal :size 512))  ; Expected max concurrent players

;; Codex's spatial grid overhaul recommendation:
;; - Replace cons/list keys with a packed fixnum cell key:
;;   (logior (ash cx 32) (ldb (byte 32 0) cy)) and use :test 'eql
;; - Replace per-query list building with a reusable scratch vector
;;   (simple-array fixnum with fill-pointer) or a visitor-style iterator
;; - Consider array-backed grids sized from zone dimensions to avoid
;;   hash-table growth/re-hash
```

#### 4.3 [COMMON] OPPORTUNITY: Entity/Event Pooling

**Claude's Finding**:
No explicit entity pool found for NPCs or projectiles.

**Recommendation**: Implement object pools for:
- NPCs (spawn/despawn churn)
- Projectiles/effects
- Event/intent objects
- Path nodes (if A* pathfinding added)

Example pool pattern:
```lisp
(defstruct entity-pool
  (free-list nil :type list)
  (allocated 0 :type fixnum)
  (capacity 0 :type fixnum))

(defun pool-acquire (pool constructor)
  (if (entity-pool-free-list pool)
      (pop (entity-pool-free-list pool))
      (funcall constructor)))

(defun pool-release (pool entity)
  (push entity (entity-pool-free-list pool)))
```

**Codex's Finding** (Event queue specific):
- Combat event queue uses `nconc` with `(list event)` for each push: `src/types.lisp:683-687`
- Server tick maps events to plists per frame: `src/net.lisp:2470-2472`

**Codex Recommendation**: Replace event list with a ring buffer (simple-vector with fill-pointer) and serialize in-place.

---

### 5. Networking/Serialization

#### 5.1 [CLAUDE-ONLY] GOOD: Buffer Reuse Pattern
**File**: `src/net.lisp`
```lisp
(defparameter *net-buffer*
  (make-array *net-buffer-size* :element-type '(unsigned-byte 8)))
```
Network buffers are pre-allocated and reused.

#### 5.2 [CLAUDE-ONLY] GOOD: Zone-Filtered Snapshots
```lisp
(serialize-game-state-for-zone zone-id ...)
```
Snapshots are filtered by zone, reducing bandwidth for multi-zone setups.

#### 5.3 [CLAUDE-ONLY] GOOD: Delta Compression
The snapshot system supports delta encoding to reduce bandwidth.

#### 5.4 [COMMON] OPPORTUNITY: Snapshot Rate Decoupling
**Current**: Snapshots appear to be sent at sim rate (60 Hz).

**Claude's Recommendation**:
```lisp
(defparameter *snapshot-interval* (/ 1.0 30.0))  ; 30 Hz snapshots
(defparameter *snapshot-accumulator* 0.0)

;; In server loop:
(incf *snapshot-accumulator* dt)
(when (>= *snapshot-accumulator* *snapshot-interval*)
  (decf *snapshot-accumulator* *snapshot-interval*)
  (broadcast-snapshots))
```

**Codex's Recommendation**: Consider decoupling snapshot send rate from sim tick (20–30 Hz snapshots with 60 Hz sim) to lower bandwidth and allocation pressure.

#### 5.5 [CODEX-ONLY] LOW: host-to-string Per-Packet Allocations
**Evidence**:
- `host-to-string` uses `format` and `coerce` to list: `src/net.lisp:75-79`, called from `receive-net-message`: `src/net.lisp:81-97`

**Impact**: Minor per-packet allocation; offers quick win if traffic is high.

**Recommendation**: Cache host strings in `net-client` or use a preallocated string buffer for conversion.

---

### 6. Rendering

#### 6.1 [COMMON] GOOD: Chunk Caching (with caveat)

**Claude's Finding** - GOOD:
**File**: `src/rendering.lisp`
```lisp
(defstruct render-chunk-cache
  (texture nil)
  (chunk-x 0 :type fixnum)
  (chunk-y 0 :type fixnum)
  (layer-key nil)
  (dirty t :type boolean)
  (last-access 0 :type fixnum))
```
Static terrain is pre-rendered to textures.

**Codex's Finding** - VIOLATION in cache key creation:
- Chunk cache key is a list/cons: `src/rendering.lisp:406-410`
- The cache hash table uses `:test 'equal`: `src/types.lisp:220-223`
- Keys are created in hot render paths (`get-or-render-chunk` and related): `src/rendering.lisp:683-686`

**Impact**: Per-frame consing in rendering; hash-table `equal` adds overhead.

**Codex Recommendation**:
- Precompute a fixnum `layer-key-id` and pack `(layer-key-id, chunk-x, chunk-y)` into a single fixnum key or a small struct reused from a pool
- Switch to `:test 'eql` for numeric keys

#### 6.2 [CLAUDE-ONLY] GOOD: View Frustum Culling
```lisp
(let ((start-chunk-x (max 0 (floor view-left chunk-pixel-size)))
      (end-chunk-x (min (1- max-chunk-x) (ceiling view-right chunk-pixel-size))))
  ...)
```
Only visible chunks are drawn.

**Codex Compliance Note**: Render chunk cache and spatial culling exist (reducing draw calls): `src/rendering.lisp` chunk cache and `draw-entities-with-spatial-culling`.

#### 6.3 [CLAUDE-ONLY] OPPORTUNITY: Draw Call Batching
**Observation**: Entity drawing appears to be individual draw calls per entity.

**Recommendation**: Batch entity sprites by texture atlas:
```lisp
(defun draw-entities-batched (entities)
  ;; Sort by texture to minimize state changes
  (let ((sorted (sort (copy-seq entities) #'<
                      :key #'entity-texture-id)))
    (with-texture-batch ()
      (dolist (e sorted)
        (batch-draw-sprite e)))))
```

---

### 7. SBCL Compiler Usage

#### 7.1 [COMMON] CRITICAL VIOLATION: No Environment-Driven Optimization

**Claude's Finding**:
**File**: `src/package.lisp`, `mmorpg.asd`

**Issue**: No `(declaim (optimize ...))` found at package/system level.

**CLAUDE.md Requirement**:
> The build system must expose this via a single toggle (e.g. MMORPG_ENV=dev|prod)

**Codex's Finding**:
- No global optimize policy or env toggle; only local declarations in `src/save.lisp:1096` and `src/save.lisp:1218`

**Combined Recommendation**:

In `src/config.lisp`:
```lisp
(defparameter *environment*
  (or (uiop:getenv "MMORPG_ENV") "dev"))

;; Set optimization policy based on environment
(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (string= (or (uiop:getenv "MMORPG_ENV") "dev") "prod")
      (declaim (optimize (speed 3) (safety 1) (debug 0) (compilation-speed 0)))
      (declaim (optimize (speed 2) (safety 2) (debug 2) (compilation-speed 2)))))
```

#### 7.2 [COMMON] VIOLATION: No Localized Hot-Path Optimization
**Files**: `src/movement.lisp`, `src/spatial.lisp`, `src/combat.lisp`

**Issue**: Hot functions lack local optimization declarations.

**Claude's Recommendation** for proven hot functions:
```lisp
(defun blocked-at-p (zone-state x y)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type zone-state zone-state)
           (type single-float x y))
  ...)
```

**Claude's Candidate hot functions**:
- `blocked-at-p` (movement.lisp)
- `attempt-move` (movement.lisp)
- `update-player-position` (movement.lisp)
- `spatial-grid-query` (spatial.lisp)
- `spatial-grid-insert` (spatial.lisp)
- `calculate-damage` (combat.lisp)
- `entities-in-range` (spatial.lisp)

**Codex's Recommendation**:
- Add local `(declare (type single-float ...))` and `(declare (type fixnum ...))` in the top 5–20 hottest functions (update-sim, movement, AI, combat, rendering culling)
- Type critical struct slots (positions, velocities, timers, ids) with `:type` to stabilize numeric lanes

---

### 8. SIMD Opportunities

#### 8.1 [COMMON] spatial.lisp - High Priority
**Current**: Scalar distance calculations for proximity queries.

**Claude's SIMD Opportunity**: Batch distance checks for entities in grid cells.
```lisp
;; Scalar (current)
(defun distance-squared (x1 y1 x2 y2)
  (+ (* (- x2 x1) (- x2 x1))
     (* (- y2 y1) (- y2 y1))))

;; SIMD kernel (recommended)
(defun batch-distance-squared (cx cy xs ys results n)
  "Compute squared distances from (cx,cy) to N points."
  (declare (optimize (speed 3) (safety 0))
           (type single-float cx cy)
           (type (simple-array single-float (*)) xs ys results)
           (type fixnum n))
  ;; SSE2/AVX implementation using sb-simd
  ...)
```

**Codex's Note**: Candidate kernels per AGENTS.md: spatial queries and movement integration. If and only if profiling shows compute-bound math kernels, isolate into a `src/kernels/` area with scalar + SIMD variants and runtime dispatch.

#### 8.2 [CLAUDE-ONLY] movement.lisp - Medium Priority
**SIMD Opportunity**: Batch position integration for many entities.
```lisp
(defun integrate-positions (xs ys dxs dys dt n)
  "Integrate N positions by velocities * dt."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array single-float (*)) xs ys dxs dys)
           (type single-float dt)
           (type fixnum n))
  ;; SIMD: x[i] += dx[i] * dt, y[i] += dy[i] * dt
  ...)
```

#### 8.3 [CLAUDE-ONLY] combat.lisp - Low Priority (Branchy)
**Limited SIMD**: Only for sub-kernels like AoE radius filtering.
```lisp
(defun filter-entities-in-radius (cx cy radius entities)
  "Return entities within radius using SIMD distance batch."
  ...)
```

---

### 9. Data Files (`data/`)

#### 9.1 [CLAUDE-ONLY] GOOD: Load-Once Data
**Files**: `data/game-data.lisp`, `data/world-graph.lisp`, `data/zones/*.lisp`

These are **pure data definitions** loaded once at startup:
- `game-data.lisp`: Tunables, animation sets, items, NPC archetypes, loot tables
- `world-graph.lisp`: Zone connection edges
- `zones/*.lisp`: Tile layers, collision, spawns (chunked format)

**Status**: No per-tick performance concern - these are not runtime code.

#### 9.2 [CLAUDE-ONLY] OBSERVATION: Zone Data Uses Chunked Storage
```lisp
(:CHUNKS
 ((:X 4 :Y 4 :TILES (735 364 365 ...) :FILL NIL)
  (:X 3 :Y 4 :TILES (...) :FILL NIL)
  ...))
```
This aligns well with the render chunk caching system in rendering.lisp.

#### 9.3 [CLAUDE-ONLY] VERIFY: Runtime Access Patterns
**Potential concern**: If game code accesses NPC archetypes or items via plist lookups (`getf`) in hot paths, that's inefficient.

**Files to check**: `src/ai.lisp`, `src/combat.lisp`, `src/progression.lisp`

**Recommended pattern**: Transform plist data into typed structs/hash-tables at load time:
```lisp
;; At load time (once):
(defparameter *npc-archetypes* (make-hash-table :test 'eq :size 64))
(dolist (archetype (load-archetypes-from-file))
  (setf (gethash (archetype-id archetype) *npc-archetypes*)
        (make-npc-archetype-struct archetype)))

;; At runtime (hot path):
(gethash archetype-id *npc-archetypes*)  ; O(1) lookup
```

If archetypes are already pre-transformed to structs, this is fine. If not, add to priority items.

---

### 10. GC Predictability

#### 10.1 [CLAUDE-ONLY] OPPORTUNITY: Strategic GC Scheduling
**Recommendation**: Add GC trigger points at safe moments:
```lisp
;; In server.lisp, after snapshot broadcast
(when (and *gc-enabled*
           (zerop (mod tick-count *gc-interval*)))
  (sb-ext:gc :full nil))  ; Incremental GC

;; Or trigger during zone transitions (player is loading anyway)
(defun handle-zone-transition (player new-zone)
  (sb-ext:gc :full nil)  ; Safe point for GC
  ...)
```

#### 10.2 [CLAUDE-ONLY] OBSERVATION: No Finalizers/Weak Tables in Hot Paths
**Status**: Good. No problematic patterns found.

---

## Consolidated Priority Action Items

### Critical (Blocks 60Hz at Scale) - Both Analyses Agree

| # | Item | Source | Files |
|---|------|--------|-------|
| 1 | **Add environment-driven optimization policy** | COMMON | config.lisp |
| 2 | **Add type declarations to all struct slots** | COMMON | types.lisp |
| 3 | **Pre-size hash tables** | COMMON | movement.lisp, net.lisp |
| 4 | **Spatial grid overhaul** - pack cell keys, replace list-based cells with vectors, no-cons iteration | CODEX | spatial.lisp |
| 5 | **Remove CLOS dispatch from hot loops** - split update/draw by entity type | CODEX | main.lisp, rendering.lisp, combat.lisp |

### High (Performance Improvement)

| # | Item | Source | Files |
|---|------|--------|-------|
| 6 | **Add type declarations to hot functions** | COMMON | movement.lisp, spatial.lisp, combat.lisp |
| 7 | **Fix snapshot serialization allocations** - binary encoder, reusable U8 buffers | COMMON | net.lisp |
| 8 | **Implement entity/event pools** | COMMON | types.lisp |
| 9 | **Fix zone tracking per-tick allocations** - cache occupied-zone-ids, use zone-state-zone-players | CODEX | movement.lisp, main.lisp |
| 10 | **Fix render chunk cache keys** - pack to fixnum, use :test 'eql | CODEX | rendering.lisp, types.lisp |

### Medium (Optimization Opportunities)

| # | Item | Source | Files |
|---|------|--------|-------|
| 11 | **Implement SIMD kernels** for spatial distance calculations | COMMON | spatial.lisp |
| 12 | **Decouple snapshot rate** from sim rate (60Hz sim, 30Hz snapshots) | COMMON | net.lisp, server.lisp |
| 13 | **Add draw call batching** for entity rendering | CLAUDE | rendering.lisp |
| 14 | **Cache host strings** in net-client | CODEX | net.lisp |

### Low (Polish)

| # | Item | Source | Files |
|---|------|--------|-------|
| 15 | **Add strategic GC scheduling** at safe points | CLAUDE | server.lisp |
| 16 | **Profile and identify** remaining allocation hotspots with sb-sprof | CLAUDE | - |
| 17 | **Verify data file runtime access** - ensure archetypes pre-transformed | CLAUDE | ai.lisp, combat.lisp |

---

## Codex's Targeted Recommendations (Actionable Roadmap)

1. **Spatial grid overhaul (highest ROI)**: pack cell keys, switch to `eql` hash, replace list-based cell contents with vectors or pooled arrays, and provide no-cons neighbor iteration. (Addresses the largest per-tick allocator.)

2. **Remove CLOS dispatch from hot loops**: split update/draw loops by entity type and call direct functions. (Low refactor risk, immediate perf win.)

3. **Serialization buffering**: implement a binary encoder into reusable U8 buffers; avoid plist/string allocations; add snapshot rate limiter (20–30 Hz). (Major GC reduction.)

4. **Global optimize policy & types**: add env-driven `(declaim (optimize ...))` and type key hot structs/locals to stabilize numeric lanes. (Foundational for SBCL performance.)

5. **Zone tracking cache**: compute occupied zones once per tick and reuse; rely on `zone-state-zone-players` without per-tick `players-in-zone` allocations.

---

## Verification Commands

After implementing fixes:
```bash
# Ensure all tests still pass
make tests

# Profile for allocations (requires sb-sprof)
MMORPG_PROFILE=1 make server

# Check for type inference issues
sbcl --eval '(declaim (optimize (speed 3)))' --load mmorpg.asd
# Watch for compiler notes about type inference failures
```

---

## Summary of Attribution

### Findings Common to Both Analyses
- Missing type declarations in structs and hot functions
- No environment-driven optimization policy
- Hash table pre-sizing issues
- Spatial grid allocation patterns
- Snapshot serialization allocations
- Snapshot rate decoupling opportunity
- SIMD opportunities in spatial.lisp
- Entity/event pooling needs
- Defstruct usage (GOOD)
- Interpolation buffer pooling (GOOD)

### Findings Unique to Claude
- Update/Draw separation (GOOD)
- Format string logging analysis (ACCEPTABLE)
- Hash table iteration closure allocation
- View frustum culling (GOOD)
- Draw call batching opportunity
- Specific numeric rule violations with code examples
- Data files analysis (load-once, chunked storage, runtime access patterns)
- GC predictability / strategic scheduling
- Buffer reuse pattern (GOOD)
- Zone-filtered snapshots (GOOD)
- Delta compression (GOOD)
- No finalizers/weak tables (GOOD)

### Findings Unique to Codex
- **CLOS generic dispatch in innermost loops** (HIGH - major finding)
- Per-tick zone tracking list/vector allocations (MEDIUM)
- Render chunk cache key allocations (MEDIUM)
- Event queue/list churn (LOW)
- host-to-string per-packet allocations (LOW)
- Specific line number references throughout
- Packed fixnum cell key technique recommendation

---

## Conclusion

Both analyses agree on the **solid architectural foundation** (defstruct, spatial grids, chunk caching, buffer pooling). The main gaps are:

1. **Missing SBCL-specific optimizations** (type declarations, optimization policies)
2. **Hot-loop allocations** (spatial grid, snapshots, zone tracking, chunk cache keys)
3. **CLOS dispatch in hot paths** (Codex-only finding - major)
4. **Missing object pooling** for entity churn
5. **No SIMD utilization** in compute-bound kernels

Addressing the **Critical** items (especially spatial grid overhaul and CLOS removal per Codex) will have the largest impact on achieving stable 60Hz performance at scale.

---

*End of Unified Report*
