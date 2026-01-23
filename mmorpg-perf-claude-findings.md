# MMORPG Performance Analysis Report

**Date**: 2026-01-23
**Scope**: Analysis against CLAUDE.md SBCL Performance Directives
**Target**: 60 Hz tick loop (~16.67ms budget)
**Files Analyzed**: `src/*.lisp` (runtime code), `data/*.lisp` (load-once data definitions)

---

## Executive Summary

The codebase demonstrates **solid foundational architecture** for performance:
- Uses `defstruct` for hot data (not CLOS classes)
- Implements spatial grid for O(1) proximity queries
- Has render chunk caching with FBO pre-rendering
- Network code uses buffer pooling and delta compression

However, there are **critical gaps** that will cause issues at scale:
- Missing type declarations throughout hot paths
- No environment-driven optimization policy (dev vs prod)
- Hash tables created without pre-sizing (GC pressure on growth)
- No SIMD kernels in compute-bound areas
- Some allocation patterns in hot loops

---

## Findings by Category

### 1. Hot Loop Optimization

#### 1.1 GOOD: Update/Draw Separation
The codebase correctly separates simulation from rendering:
- `server.lisp`: Runs only `update-sim` (no rendering)
- `main.lisp`: Client runs both update and draw phases

#### 1.2 VIOLATION: String Concatenation in Hot Path
**File**: `src/net.lisp` (snapshot chunk reassembly)
```lisp
(let* ((combined (apply #'concatenate 'string (nreverse parts))))
```
**Issue**: Creates new string on every chunked snapshot reassembly.
**Fix**: Pre-allocate reassembly buffer, use `replace` to copy chunks:
```lisp
(defparameter *reassembly-buffer*
  (make-array *max-snapshot-size* :element-type 'character :fill-pointer 0))

;; In reassembly:
(setf (fill-pointer *reassembly-buffer*) 0)
(dolist (part parts)
  (let ((start (fill-pointer *reassembly-buffer*)))
    (incf (fill-pointer *reassembly-buffer*) (length part))
    (replace *reassembly-buffer* part :start1 start)))
```

#### 1.3 VIOLATION: Format String in Verbose Logging
**Files**: Multiple (movement.lisp, net.lisp, combat.lisp)
```lisp
(log-verbose "Player ~A moved to (~,2F, ~,2F)" player-id x y)
```
**Issue**: Even when `*verbose*` is nil, the format string is still parsed.
**Status**: Acceptable - `log-verbose` macro short-circuits before format. No action needed.

#### 1.4 OPPORTUNITY: Hash Table Iteration in Hot Loops
**File**: `src/server.lisp`, `src/movement.lisp`
```lisp
(maphash (lambda (id player) ...) *players*)
```
**Issue**: Hash table iteration allocates closure on each call.
**Recommendation**: For truly hot loops (per-tick entity iteration), consider:
- Pre-extracted player arrays for iteration
- `with-hash-table-iterator` macro (still allocates, but less)
- Or accept this as bounded overhead (player count is bounded)

---

### 2. Data Layout

#### 2.1 GOOD: Defstruct Usage
All core game entities use `defstruct`:
- `player` struct in types.lisp
- `npc` struct in types.lisp
- `zone-state` struct in types.lisp
- `render-chunk-cache` struct in types.lisp

This follows CLAUDE.md directive to use typed defstruct over CLOS for hot data.

#### 2.2 VIOLATION: Missing Type Declarations in Structs
**File**: `src/types.lisp`

Many struct slots lack type declarations:
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

**Recommended**:
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

#### 2.3 VIOLATION: No Typed Arrays for Numeric Data
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

#### 3.1 VIOLATION: Undeclared Loop Variables
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

#### 3.2 VIOLATION: Mixed Numeric Types
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

#### 3.3 VIOLATION: Undeclared Function Parameters
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

#### 4.1 GOOD: Snapshot Pooling
**File**: `src/net.lisp`
```lisp
(serialize-game-state-for-zone ... :use-pool t)
```
The network code supports pooled snapshot serialization.

#### 4.2 VIOLATION: Hash Tables Without Pre-sizing
**File**: `src/movement.lisp`
```lisp
(defparameter *zone-states* (make-hash-table :test 'eq))
```

**File**: `src/net.lisp`
```lisp
(defparameter *player-sessions* (make-hash-table :test 'equal))
```

**Issue**: Hash tables grow dynamically, causing GC pressure and rehashing during gameplay.

**Recommended**:
```lisp
;; Pre-size based on expected capacity
(defparameter *zone-states*
  (make-hash-table :test 'eq :size 64))  ; Expected max zones

(defparameter *player-sessions*
  (make-hash-table :test 'equal :size 512))  ; Expected max concurrent players
```

#### 4.3 OPPORTUNITY: Entity Pooling
**Observation**: No explicit entity pool found for NPCs or projectiles.

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

---

### 5. Networking/Serialization

#### 5.1 GOOD: Buffer Reuse Pattern
**File**: `src/net.lisp`
```lisp
(defparameter *net-buffer*
  (make-array *net-buffer-size* :element-type '(unsigned-byte 8)))
```
Network buffers are pre-allocated and reused.

#### 5.2 GOOD: Zone-Filtered Snapshots
```lisp
(serialize-game-state-for-zone zone-id ...)
```
Snapshots are filtered by zone, reducing bandwidth for multi-zone setups.

#### 5.3 GOOD: Delta Compression
The snapshot system supports delta encoding to reduce bandwidth.

#### 5.4 OPPORTUNITY: Snapshot Rate Decoupling
**Current**: Snapshots appear to be sent at sim rate (60 Hz).

**Recommendation per CLAUDE.md**: Consider 20-30 Hz snapshot rate:
```lisp
(defparameter *snapshot-interval* (/ 1.0 30.0))  ; 30 Hz snapshots
(defparameter *snapshot-accumulator* 0.0)

;; In server loop:
(incf *snapshot-accumulator* dt)
(when (>= *snapshot-accumulator* *snapshot-interval*)
  (decf *snapshot-accumulator* *snapshot-interval*)
  (broadcast-snapshots))
```

---

### 6. Rendering

#### 6.1 GOOD: Chunk Caching
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

#### 6.2 GOOD: View Frustum Culling
```lisp
(let ((start-chunk-x (max 0 (floor view-left chunk-pixel-size)))
      (end-chunk-x (min (1- max-chunk-x) (ceiling view-right chunk-pixel-size))))
  ...)
```
Only visible chunks are drawn.

#### 6.3 OPPORTUNITY: Draw Call Batching
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

#### 7.1 CRITICAL VIOLATION: No Environment-Driven Optimization
**File**: `src/package.lisp`, `mmorpg.asd`

**Issue**: No `(declaim (optimize ...))` found at package/system level.

**CLAUDE.md Requirement**:
> The build system must expose this via a single toggle (e.g. MMORPG_ENV=dev|prod)

**Recommended Implementation**:

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

#### 7.2 VIOLATION: No Localized Hot-Path Optimization
**Files**: `src/movement.lisp`, `src/spatial.lisp`, `src/combat.lisp`

**Issue**: Hot functions lack local optimization declarations.

**Recommended for proven hot functions**:
```lisp
(defun blocked-at-p (zone-state x y)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type zone-state zone-state)
           (type single-float x y))
  ...)
```

**Candidate hot functions**:
- `blocked-at-p` (movement.lisp)
- `attempt-move` (movement.lisp)
- `update-player-position` (movement.lisp)
- `spatial-grid-query` (spatial.lisp)
- `spatial-grid-insert` (spatial.lisp)
- `calculate-damage` (combat.lisp)
- `entities-in-range` (spatial.lisp)

---

### 8. SIMD Opportunities

#### 8.1 spatial.lisp - High Priority
**Current**: Scalar distance calculations for proximity queries.

**SIMD Opportunity**: Batch distance checks for entities in grid cells.
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

#### 8.2 movement.lisp - Medium Priority
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

#### 8.3 combat.lisp - Low Priority (Branchy)
**Limited SIMD**: Only for sub-kernels like AoE radius filtering.
```lisp
(defun filter-entities-in-radius (cx cy radius entities)
  "Return entities within radius using SIMD distance batch."
  ...)
```

---

### 9. Data Files (`data/`)

#### 9.1 GOOD: Load-Once Data
**Files**: `data/game-data.lisp`, `data/world-graph.lisp`, `data/zones/*.lisp`

These are **pure data definitions** loaded once at startup:
- `game-data.lisp`: Tunables, animation sets, items, NPC archetypes, loot tables
- `world-graph.lisp`: Zone connection edges
- `zones/*.lisp`: Tile layers, collision, spawns (chunked format)

**Status**: No per-tick performance concern - these are not runtime code.

#### 9.2 OBSERVATION: Zone Data Uses Chunked Storage
```lisp
(:CHUNKS
 ((:X 4 :Y 4 :TILES (735 364 365 ...) :FILL NIL)
  (:X 3 :Y 4 :TILES (...) :FILL NIL)
  ...))
```
This aligns well with the render chunk caching system in rendering.lisp.

#### 9.3 VERIFY: Runtime Access Patterns
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

#### 10.1 OPPORTUNITY: Strategic GC Scheduling
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

#### 10.2 OBSERVATION: No Finalizers/Weak Tables in Hot Paths
**Status**: Good. No problematic patterns found.

---

## Priority Action Items

### Critical (Blocks 60Hz at Scale)
1. **Add environment-driven optimization policy** to config.lisp
2. **Add type declarations to all struct slots** in types.lisp
3. **Pre-size hash tables** (*zone-states*, *player-sessions*, etc.)

### High (Performance Improvement)
4. **Add type declarations to hot functions** (movement.lisp, spatial.lisp, combat.lisp)
5. **Fix string concatenation** in snapshot chunk reassembly (net.lisp)
6. **Implement entity pools** for NPCs and effects

### Medium (Optimization Opportunities)
7. **Implement SIMD kernels** for spatial.lisp distance calculations
8. **Decouple snapshot rate** from sim rate (60Hz sim, 30Hz snapshots)
9. **Add draw call batching** for entity rendering

### Low (Polish)
10. **Add strategic GC scheduling** at safe points
11. **Profile and identify** remaining allocation hotspots with sb-sprof

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

## Conclusion

The codebase has a **solid architectural foundation** for performance. The main gaps are:
1. Missing SBCL-specific optimizations (type declarations, optimization policies)
2. Missing object pooling for entity churn
3. No SIMD utilization in compute-bound kernels

Addressing the **Critical** items will have the largest impact on achieving stable 60Hz performance at scale. The architecture itself (defstruct, spatial grids, chunk caching) is correct and follows CLAUDE.md guidelines.
