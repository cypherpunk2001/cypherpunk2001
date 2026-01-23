# MMORPG Performance Optimization Plan - UNIFIED FINAL

**Date**: 2026-01-23
**Sources**: Claude Plan + Codex Plan (unified from `mmorpg-perf-unified-findings.md`)
**Goal**: Achieve stable 60 Hz sim (16.67ms tick budget) with allocation-free hot loops
**Target**: 2000 concurrent players
**Status**: APPROVED

---

## Document Overview

This unified plan combines the best of both Claude and Codex planning work:
- **Codex contributions**: Phase 0 baseline, findings coverage matrix, binary serialization approach, estimation methodology
- **Claude contributions**: Detailed code examples, step-by-step instructions, verification commands, task granularity

Nothing has been lost from either plan.

---

## Design Decisions (APPROVED)

The following architectural decisions have been approved for the 2000 concurrent player target:

### 1. Spatial Grid: Array-Backed for Active Zones
**Decision**: Use array-backed grids for active zones with known dimensions.
**Rationale**: Array-backed grids give deterministic O(1) and avoid hashing/consing. For 2000 players, predictable hot-loop performance is critical. Use hybrid (array for active zones, hash for sparse/large) only if needed.

### 2. CLOS Removal: Hot Paths Only
**Decision**: Remove CLOS dispatch from per-tick/per-entity loops (sim + render + combat). Keep generics at system boundaries (editor/debug/tools).
**Rationale**: Preserves flexibility for tooling without harming tick time. No need for full removal.

### 3. Snapshot Rate: 20Hz Default with Dynamic Tier
**Decision**: Default 20Hz for public snapshots with interpolation. Optionally 30Hz for nearby/party/critical entities.
**Rationale**: At 2000 players, bandwidth and serialization load are the bigger threat to stability. 20Hz feels fine with interpolation; 30Hz reserved for high-salience targets.

### 4. SIMD: Deferred
**Decision**: Defer SIMD until after allocations, types, and dispatch are fixed.
**Rationale**: Allocation/type/dispatch fixes are larger, safer wins. Only do SIMD after profiling shows compute-bound kernels.

### 5. Binary Serialization: Incremental with Feature Flag
**Decision**: Start with binary snapshots only (behind feature flag), keep control/auth messages on plists. Move to full binary later if needed.
**Rationale**: Cuts the biggest per-tick allocation without immediate full protocol rewrite. Maintains compatibility path during rollout.

### 6. Protocol Change Risk: Acceptable (Incremental)
**Decision**: Protocol changes acceptable if done incrementally and feature-flagged.
**Rationale**: Binary snapshots (server + client) with old plist format for auth/control provides big perf wins without destabilizing everything at once.

### 7. Automated Perf Regression Tests: Deferred
**Decision**: Defer until server is decoupled from localhost to a separate LAN machine.
**Rationale**: Perf testing on localhost is noisy due to resource contention between client and server. Will implement lightweight, repeatable headless perf test once server runs on dedicated hardware.

---

## Assumptions and Estimation Notes

- Estimates are **relative tick-time improvements** (server sim + snapshot send) or client frame time where noted
- Ranges reflect variability by entity count, zone size, and network load
- Percentages are **non-additive**; later improvements see diminishing returns
- Priorities consider risk, ROI, and cross-cutting impact
- Benefits are more pronounced at scale (100+ entities, 50+ players)

---

## Master Priority Summary

| Priority | Category | Items |
|----------|----------|-------|
| **P0** | Foundation | Env optimize policy, type declarations (structs + functions) |
| **P0** | Critical Fixes | Spatial grid overhaul, CLOS dispatch removal from hot loops |
| **P1** | High ROI | Snapshot serialization/buffering, snapshot rate decoupling |
| **P2** | High Value | Zone tracking cache, chunk cache key packing, event ring buffer, hash table pre-sizing |
| **P3** | Conditional | SIMD kernels, draw-call batching, GC scheduling, host string caching |
| **P4** | Polish | Data access validation, final profiling |

---

## Master Impact Summary Table

| Priority | Phase | Task | Est. Perf Gain (Claude) | Est. Perf Gain (Codex) | Effort | ROI |
|----------|-------|------|-------------------------|------------------------|--------|-----|
| P0 | 1 | Env-Driven Optimize Policy | 2-8% | 2-8% | Low | High |
| P0 | 1 | Type Declarations (structs) | 10-15% | 5-15% | Low | Very High |
| P0 | 1 | Type Declarations (hot funcs) | 5-10% | 3-12% | Low | High |
| P0 | 1 | CLOS Dispatch Removal | 15-25% | 3-10% | Medium | Very High |
| P0 | 2 | Spatial Grid Overhaul | 25-35% | 10-35% | High | **Highest** |
| P1 | 3 | Snapshot Serialization (binary) | 5-10% | 10-25% | High | High |
| P1 | 3 | Chunk Reassembly Buffer | 2-6% | 2-6% | Low | High |
| P1 | 3 | Snapshot Rate Decoupling | 2-3% + BW | 5-15% | Low | High |
| P2 | 2 | Hash Table Pre-sizing | 1-3% | 1-3% | Low | Medium |
| P2 | 4 | Zone Tracking Cache | 3-5% | 2-8% | Low | High |
| P2 | 4 | Chunk Cache Key Packing | 2-4% | 1-5% | Low | High |
| P2 | 4 | Event Ring Buffer | 2-5% | 1-4% | Low | Medium |
| P2 | 4 | Object Pooling (NPCs) | 2-5% | - | Medium | Medium |
| P3 | 5 | SIMD Kernels | 3-8% | 5-15% | High | Conditional |
| P3 | 5 | Draw-Call Batching | 1-3% | 5-20% (GPU) | Medium | Conditional |
| P3 | 5 | GC Scheduling | 1-2% | 0-5% | Low | Low |
| P4 | 5 | Host String Caching | <1% | <1-2% | Low | Low |
| P4 | 5 | Data Access Validation | - | 2-8% | Low | Conditional |

---

## Expected Outcomes

### Before Optimization
- Tick time: Variable, spikes to 20-30ms under load
- GC pauses: Frequent, 5-15ms spikes
- Entity limit: ~50-100 before frame drops
- Allocation rate: High per-tick consing

### After Phase 0-1 (Foundation + P0 Critical)
- Tick time: Reduced 25-55% (Codex) / 40-60% (Claude)
- GC spikes: Significantly reduced
- Entity limit: 500-1000+
- Numeric stability: Improved

### After Phase 2-4 (Full Implementation)
- Tick time: Stable 5-8ms
- GC pauses: Very rare, <2ms
- Concurrent players: 2000+ (target)
- Allocation rate: Near-zero in hot loops

---

## Phase 0: Baseline and Guardrails (DEFERRED)

**Priority**: Pre-requisite
**Source**: CODEX
**Estimated Benefit**: 0% (measurement enables safe iteration)
**Risk**: None
**Status**: DEFERRED until server runs on separate LAN machine (localhost perf testing too noisy)

**Note**: The performance optimizations in this plan are well-supported by independent Common Lisp doctoral research on SBCL optimization patterns (type declarations, allocation-free hot loops, CLOS dispatch costs, numeric stability). Baseline measurements will be valuable for validation once proper test infrastructure exists, but the changes themselves are grounded in established research rather than speculative.

### Task 0.1: Baseline Profiling and Allocation Capture (Deferred)

**Purpose**: Establish measurable baselines before any changes.
**Status**: DEFERRED until server on separate LAN machine.

**Steps**:
1. Use existing profiling hooks + GC logging
2. Capture server tick time and allocations at realistic load:
   - 200-500 NPCs per zone
   - 50-200 players if possible
3. Output baseline numbers for:
   - `update-sim` duration
   - Snapshot send duration
   - Allocations per tick
   - GC frequency and pause times

**Verification**:
```bash
MMORPG_VERBOSE=1 make server
# Run load test, capture metrics
```

---

### Task 0.2: Add Minimal Performance Regression Harness (Deferred)

**Purpose**: Enable safe iteration with measurable before/after comparison.

**Steps**:
1. Create script to run headless server for N seconds
2. Output avg tick time and allocation count
3. Add to CI or manual verification workflow

**Example Script**:
```bash
#!/bin/bash
# perf-baseline.sh
MMORPG_HEADLESS=1 timeout 60 make server 2>&1 | grep -E "tick_time|alloc"
```

---

## Phase 1: Foundation - Types + Compiler Policy + Dispatch

**Priority**: P0 (CRITICAL)
**Sources**: COMMON (both Claude and Codex)
**Estimated Benefit**: 25-55% tick time reduction (combined)
**Risk**: Low-Medium
**Rationale**: These are prerequisite optimizations that unlock SBCL performance. Must complete before Phases 2-4 to get full benefit.

---

### Task 1.1: Add Environment-Driven Optimization Policy

**Files**: `src/config.lisp`
**Source**: COMMON
**Est. Benefit**: 2-8%
**Risk**: Low

**Steps**:
1. Read `src/config.lisp` to understand current structure
2. Add at the TOP of config.lisp (must be early in load order):

```lisp
;;; Environment-driven optimization policy
;;; Set MMORPG_ENV=prod for production builds
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *mmorpg-environment*
    (or (uiop:getenv "MMORPG_ENV") "dev"))

  (if (string= *mmorpg-environment* "prod")
      ;; Production: maximize speed, maintain basic safety
      (declaim (optimize (speed 3) (safety 1) (debug 0) (compilation-speed 0)))
      ;; Development: balance speed with debugging
      (declaim (optimize (speed 2) (safety 2) (debug 2) (compilation-speed 2)))))
```

3. Update `Makefile` to support `MMORPG_ENV=prod make server`
4. Document in README/CLAUDE.md

**Verification**:
```bash
MMORPG_ENV=dev make tests
MMORPG_ENV=prod make tests
```

---

### Task 1.2: Add Type Declarations to Core Structs

**Files**: `src/types.lisp`
**Source**: COMMON
**Est. Benefit**: 5-15%
**Risk**: Medium (requires fixing type mismatches)

**Target Structs and Fields**:

**player struct**:
```lisp
(defstruct player
  (id nil :type (or null fixnum))
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (dx 0.0 :type single-float)
  (dy 0.0 :type single-float)
  (hp 100 :type fixnum)
  (max-hp 100 :type fixnum)
  (xp 0 :type fixnum)
  (level 1 :type fixnum)
  (gold 0 :type fixnum)
  (zone-id :zone-1 :type keyword)
  ;; timestamps/timers
  (attack-timer 0.0 :type single-float)
  (hit-timer 0.0 :type single-float)
  ...)
```

**npc struct**:
```lisp
(defstruct npc
  (id 0 :type fixnum)
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (dx 0.0 :type single-float)
  (dy 0.0 :type single-float)
  (hp 100 :type fixnum)
  (max-hp 100 :type fixnum)
  (zone-id :zone-1 :type keyword)
  ...)
```

**zone-state struct**:
```lisp
(defstruct zone-state
  (width 64 :type fixnum)
  (height 64 :type fixnum)
  ;; tile arrays should be typed
  ...)
```

**Additional structs** (per Codex):
- `world` - types per `src/types.lisp:95-102`
- `interpolation-buffer` - timestamps (single-float), positions (single-float)
- `prediction-state` - numeric fields

**Steps**:
1. Read `src/types.lisp` to identify all structs
2. Add `:type` declarations to each slot
3. Run `make tests` after each struct modification
4. Fix any type mismatches revealed by compiler warnings

**Verification**:
```bash
make tests
# Check for SBCL compiler notes about type inference
sbcl --eval '(declaim (optimize (speed 3)))' --load mmorpg.asd 2>&1 | grep -i "note\|warning"
```

---

### Task 1.3: Add Type Declarations to Hot Functions

**Files**: `src/movement.lisp`, `src/spatial.lisp`, `src/combat.lisp`, `src/ai.lisp`
**Source**: COMMON
**Est. Benefit**: 3-12%
**Risk**: Medium

**Target Functions**:

**movement.lisp**:
```lisp
(defun blocked-at-p (zone-state x y)
  (declare (type zone-state zone-state)
           (type single-float x y))
  (let ((tile-x (the fixnum (floor x +tile-size+)))
        (tile-y (the fixnum (floor y +tile-size+))))
    (declare (type fixnum tile-x tile-y))
    ...))

(defun attempt-move (player dx dy dt zone-state)
  (declare (type player player)
           (type single-float dx dy dt)
           (type zone-state zone-state))
  ...)

(defun update-player-position (player dt zone-state)
  (declare (type player player)
           (type single-float dt)
           (type zone-state zone-state))
  ...)
```

**spatial.lisp**:
```lisp
(defun spatial-grid-query (grid x y radius)
  (declare (type spatial-grid grid)
           (type single-float x y radius))
  ...)

(defun spatial-grid-insert (grid entity-id x y)
  (declare (type spatial-grid grid)
           (type fixnum entity-id)
           (type single-float x y))
  ...)
```

**combat.lisp**:
```lisp
(defun calculate-damage (attack defense)
  (declare (type fixnum attack defense))
  (let ((damage (the fixnum (- attack defense))))
    (declare (type fixnum damage))
    (max 0 damage)))
```

**ai.lisp**:
- Hot AI tick functions (identify via profiling from Phase 0)

**Steps**:
1. For each function, add parameter declarations
2. Add local variable declarations in let forms
3. Use `(the type expr)` for intermediate values where needed
4. Run tests after each file

**Verification**:
```bash
make tests
```

---

### Task 1.4: Add Localized Hot-Path Optimization Declarations

**Files**: `src/movement.lisp`, `src/spatial.lisp`, `src/combat.lisp`
**Source**: COMMON
**Est. Benefit**: Included in 1.2/1.3 estimates
**Risk**: Low (after type coverage)

**Steps**:
1. After Task 1.3, add optimization declarations to proven hot functions:

```lisp
(defun blocked-at-p (zone-state x y)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type zone-state zone-state)
           (type single-float x y))
  ...)
```

2. Only add `(safety 0)` to functions that:
   - Have full type coverage
   - Are tested thoroughly
   - Don't do array access without bounds being provably safe

**Candidate hot functions** (Claude list):
- `blocked-at-p` (movement.lisp)
- `attempt-move` (movement.lisp)
- `update-player-position` (movement.lisp)
- `spatial-grid-query` (spatial.lisp)
- `spatial-grid-insert` (spatial.lisp)
- `calculate-damage` (combat.lisp)
- `entities-in-range` (spatial.lisp)

**Codex additions**:
- `update-sim` and related tick functions
- AI hot functions
- Rendering culling functions

**Verification**:
```bash
make tests
```

---

### Task 1.5: Remove CLOS Dispatch from Hot Loops (Hot Paths Only)

**Files**: `src/main.lisp`, `src/rendering.lisp`, `src/combat.lisp`, `src/types.lisp`
**Source**: CODEX (major finding)
**Est. Benefit**: 3-25% (scales with entity count)
**Risk**: Medium

**APPROVED DECISION**: Remove CLOS dispatch from per-tick/per-entity loops (sim + render + combat) only. Keep generics at system boundaries (editor/debug/tools). This preserves flexibility for tooling without harming tick time. No need for full removal.

**Current Problems** (per Codex):
- `update-entity-animation` generic in tick loop: `main.lisp:488-490`
- `combatant-update-hit-effect` generic: `combat.lisp:678-682`, `types.lisp:674-681`
- `draw-entity` generic per entity: `rendering.lisp:75-95`, `rendering.lisp:1578-1582`
- `combatant-position` generic in visibility checks: `rendering.lisp:47-50`, `types.lisp:656-657`

**Strategy A: Split Loops by Entity Type (Preferred)**
```lisp
;; Instead of:
(loop for entity across entities
      do (update-entity-animation entity dt))

;; Do:
(loop for player across players
      do (update-player-animation player dt))
(loop for npc across npcs
      do (update-npc-animation npc dt))
```

**Strategy B: Explicit Type Dispatch (If Unified Loop Needed)**
```lisp
(defun update-entity-animation-fast (entity dt)
  (declare (optimize (speed 3) (safety 1)))
  (etypecase entity
    (player (update-player-animation entity dt))
    (npc (update-npc-animation npc dt))))
```

**Steps**:

**Step 1.5.1**: Create type-specific functions
- `update-player-animation`, `update-npc-animation`
- `draw-player`, `draw-npc`
- `player-position`, `npc-position` (or use struct accessors directly)
- `update-player-hit-effect`, `update-npc-hit-effect`

**Step 1.5.2**: Update main.lisp tick loop
- Split player and NPC update loops

**Step 1.5.3**: Update rendering.lisp draw loop
- Split player and NPC draw loops

**Step 1.5.4**: Update combat.lisp
- Replace `combatant-update-hit-effect` calls

**Step 1.5.5**: Keep generic functions for non-hot paths
- Editor, debugging, tooling can still use generics

**Verification**:
```bash
make tests
make smoke
```

---

## Phase 2: Allocation-Free Hot Loops - Core

**Priority**: P0 (Spatial Grid) / P2 (Hash Tables)
**Sources**: COMMON (both Claude and Codex)
**Estimated Benefit**: 10-45% tick time reduction + major GC spike reduction
**Risk**: Medium-High
**Rationale**: Per-tick allocations are the primary cause of GC spikes. Spatial grid is the HIGHEST ROI item in the entire plan.

---

### Task 2.1: Pre-size All Hash Tables

**Files**: `src/movement.lisp`, `src/net.lisp`, `src/types.lisp`, `src/spatial.lisp`
**Source**: COMMON
**Est. Benefit**: 1-3%
**Risk**: Low

**Steps**:
1. Search for all `(make-hash-table` calls
2. Add `:size` parameter based on expected capacity:

```lisp
;; movement.lisp
(defparameter *zone-states*
  (make-hash-table :test 'eq :size 64))  ; Expected max zones

;; net.lisp
(defparameter *player-sessions*
  (make-hash-table :test 'equal :size 512))  ; Expected max concurrent players

(defparameter *active-sessions*
  (make-hash-table :test 'equal :size 512))

;; spatial.lisp - grid cells
(make-hash-table :test 'eql :size 1024)  ; After Task 2.2
```

3. Document sizing assumptions with comments
4. Run tests

**Verification**:
```bash
make tests
```

---

### Task 2.2: Spatial Grid Overhaul (HIGHEST ROI)

**Files**: `src/spatial.lisp`
**Source**: CODEX (highest ROI per both analyses)
**Est. Benefit**: 10-35%
**Risk**: Medium-High
**Complexity**: High

**APPROVED DECISION**: Use **array-backed grids** for active zones with known dimensions. Array-backed grids give deterministic O(1) and avoid hashing/consing. For 2000 players, predictable hot-loop performance is critical.

**Current Problems**:
- Cons keys for cells: `(cons cx cy)` at `spatial.lisp:16-46`
- List values for cell contents
- `copy-list` + `nconc` in queries: `spatial.lisp:92-146`
- `:test 'equal` hash table
- Hot call sites: AI target selection (`ai.lisp:12-18`), melee resolution (`combat.lisp:695-704`), render culling (`rendering.lisp:75-78`)

---

#### Primary Implementation: Array-Backed Grid (APPROVED)

Array-backed grids sized from zone dimensions. Avoids hash-table entirely for deterministic O(1) access. Zone dimensions are known at load time.

**Step 2.2.1**: Define array-backed grid structure

```lisp
(defstruct spatial-grid
  (cells nil :type (simple-array list (* *)))  ; 2D array of entity lists
  (cell-size 64 :type fixnum)
  (width 0 :type fixnum)   ; Grid width in cells
  (height 0 :type fixnum)) ; Grid height in cells

(defun make-spatial-grid-for-zone (zone-width zone-height cell-size)
  "Create array-backed spatial grid sized for zone."
  (let* ((grid-w (ceiling zone-width cell-size))
         (grid-h (ceiling zone-height cell-size)))
    (make-spatial-grid
     :cells (make-array (list grid-w grid-h) :initial-element nil)
     :cell-size cell-size
     :width grid-w
     :height grid-h)))
```

**Step 2.2.2**: Provide scratch vectors for query results (no per-query allocation)

```lisp
(defparameter *spatial-scratch-vector*
  (make-array 256 :element-type 'fixnum :fill-pointer 0))

(defparameter *spatial-scratch-vector-2*
  (make-array 256 :element-type 'fixnum :fill-pointer 0))  ; For nested queries
```

**Step 2.2.3**: Implement no-cons query iteration

```lisp
(defun spatial-grid-query-into (grid x y radius result-vector)
  "Query entities near (x,y) into pre-allocated result-vector.
   Returns the fill-pointer (count of results).
   Does not allocate."
  (declare (optimize (speed 3) (safety 0))
           (type spatial-grid grid)
           (type single-float x y radius)
           (type (simple-array fixnum (*)) result-vector))
  (setf (fill-pointer result-vector) 0)
  (let* ((cell-size (spatial-grid-cell-size grid))
         (cells (spatial-grid-cells grid))
         (cx (the fixnum (floor x cell-size)))
         (cy (the fixnum (floor y cell-size)))
         (cell-radius (the fixnum (ceiling radius cell-size))))
    (declare (type fixnum cell-size cx cy cell-radius))
    ;; Direct 2D array access - O(1), no hashing
    (loop for dx fixnum from (- cell-radius) to cell-radius do
      (loop for dy fixnum from (- cell-radius) to cell-radius do
        (let* ((nx (+ cx dx))
               (ny (+ cy dy)))
          (when (and (>= nx 0) (< nx (spatial-grid-width grid))
                     (>= ny 0) (< ny (spatial-grid-height grid)))
            (let ((cell (aref cells nx ny)))
              (when cell
                (loop for entity-id across cell do
                  (vector-push entity-id result-vector))))))))
    (fill-pointer result-vector)))
```

**Step 2.2.4**: Update all call sites
- `src/ai.lisp:12-18` (AI target selection)
- `src/combat.lisp:695-704` (melee resolution)
- `src/rendering.lisp:75-78` (render culling)

---

#### Hash-Table Fallback (For Sparse/Unbounded Zones Only)

Only use if zone dimensions are unknown or extremely large/sparse. Not recommended for 2000-player target.

```lisp
;; Packed fixnum keys avoid cons allocation
(defun pack-cell-key (cx cy)
  "Pack cell coordinates into a single fixnum key."
  (declare (optimize (speed 3) (safety 0))
           (type fixnum cx cy))
  (the fixnum (logior (the fixnum (ash cx 20))
                      (the fixnum (logand cy #xFFFFF)))))

;; Hash-table with :test 'eql (not 'equal) for fixnum keys
(defstruct spatial-grid-hash
  (cells (make-hash-table :test 'eql :size 1024) :type hash-table)
  (cell-size 64 :type fixnum))
```

**Verification**:
```bash
make tests
# Verify no new allocations in spatial queries via sb-sprof
MMORPG_PROFILE=1 make server
```

---

## Phase 3: Serialization/Networking Improvements

**Priority**: P1 (HIGH)
**Sources**: COMMON (both Claude and Codex)
**Estimated Benefit**: 7-25% tick time reduction + bandwidth savings
**Risk**: Medium-High (protocol changes)
**Rationale**: Snapshot serialization is called every tick with high allocation rate.

---

### Task 3.1: Snapshot Serialization to Reusable Buffers

**Files**: `src/net.lisp`
**Source**: CODEX (detailed), CLAUDE (incremental)
**Est. Benefit**: 10-25% (Codex) / 5-10% (Claude)
**Risk**: High (protocol change)

**Current Problems** (per Codex):
- `encode-net-message` uses `with-output-to-string` and `prin1`: `net.lisp:23-26`
- `string-to-octets` allocates new `(unsigned-byte 8)` array per message: `net.lisp:8-14`
- Server sends snapshots every tick with event plists: `net.lisp:2465-2481`, `net.lisp:1170-1174`

**Approach A: Binary Snapshots (Incremental, Feature-Flagged) — APPROVED**

```lisp
;; Per-connection reusable buffer
(defstruct net-connection
  (send-buffer (make-array 65536 :element-type '(unsigned-byte 8) :fill-pointer 0)
               :type (simple-array (unsigned-byte 8) (*)))
  ...)

;; Direct binary encoding
(defun encode-snapshot-binary (snapshot buffer)
  "Encode snapshot directly into buffer. Returns bytes written."
  (declare (optimize (speed 3) (safety 1))
           (type (simple-array (unsigned-byte 8) (*)) buffer))
  (setf (fill-pointer buffer) 0)
  ;; Write header
  ;; Write entity data directly as binary
  ;; No intermediate plist/string
  (fill-pointer buffer))
```

**Approach B: Plist Buffer Reuse (Fallback/Legacy)**

```lisp
;; Pre-allocated reassembly buffer
(defparameter *snapshot-reassembly-buffer*
  (make-array 65536 :element-type 'character :fill-pointer 0 :adjustable nil))

(defun reassemble-snapshot-chunks (parts)
  "Reassemble chunked snapshot without allocating."
  (setf (fill-pointer *snapshot-reassembly-buffer*) 0)
  (dolist (part (nreverse parts))
    (let ((start (fill-pointer *snapshot-reassembly-buffer*)))
      (incf (fill-pointer *snapshot-reassembly-buffer*) (length part))
      (replace *snapshot-reassembly-buffer* part :start1 start)))
  *snapshot-reassembly-buffer*)
```

**APPROVED DECISION**: Start with binary snapshots only (Approach A), behind a feature flag (e.g., `*use-binary-snapshots*`). Keep control/auth messages on plist format. This cuts the biggest per-tick allocation without immediate full protocol rewrite. Maintains compatibility path during rollout.

**Implementation Steps**:
1. Add feature flag: `(defparameter *use-binary-snapshots* nil)`
2. Implement binary snapshot encoding with reusable buffers
3. Keep plist encoding for auth/control messages
4. Enable flag after client/server sync is verified

**Verification**:
```bash
make tests
make ci  ; Network handshake test
```

---

### Task 3.2: Decouple Snapshot Rate from Sim Rate

**Files**: `src/server.lisp` or `src/net.lisp`
**Source**: COMMON
**Est. Benefit**: 5-15% server tick + bandwidth reduction
**Risk**: Low (must verify client interpolation)

**APPROVED DECISION**: Default **20Hz** for public snapshots with interpolation. At 2000 players, bandwidth and serialization load are the bigger threat to stability. 20Hz feels fine with interpolation. Optionally support 30Hz for nearby/party/critical entities (dynamic tier).

**Steps**:
1. Add snapshot rate configuration:

```lisp
(defparameter *snapshot-rate-default* 20)  ; Hz - public snapshots
(defparameter *snapshot-rate-high* 30)     ; Hz - nearby/party/critical (optional)
(defparameter *snapshot-interval* (/ 1.0 *snapshot-rate-default*))
(defparameter *snapshot-accumulator* 0.0)
```

2. Modify server loop:

```lisp
;; In server tick loop:
(incf *snapshot-accumulator* dt)
(when (>= *snapshot-accumulator* *snapshot-interval*)
  (decf *snapshot-accumulator* *snapshot-interval*)
  (broadcast-snapshots))
```

3. Update client interpolation for 20Hz (should already work)
4. Make snapshot rate configurable via environment variable
5. (Optional) Implement dynamic tier: 30Hz for nearby entities, 20Hz for distant

**Verification**:
```bash
make tests
make smoke  ; Verify client smoothness
```

---

### Task 3.3: Cache Host Strings

**Files**: `src/net.lisp`
**Source**: CODEX
**Est. Benefit**: <1-2%
**Risk**: Low

**Current Problem**:
- `host-to-string` uses `format` and `coerce` to list: `net.lisp:75-79`
- Called from `receive-net-message`: `net.lisp:81-97`

**Steps**:
1. Add `host-string` slot to `net-client` struct or session
2. Compute host string once on connection establishment
3. Cache and reuse for all subsequent packets
4. Remove per-packet `host-to-string` calls

**Verification**:
```bash
make tests
make ci
```

---

## Phase 4: Targeted Allocation/CPU Cleanup

**Priority**: P2 (MEDIUM)
**Sources**: COMMON (both Claude and Codex)
**Estimated Benefit**: 5-15% additional
**Risk**: Low-Medium
**Rationale**: Secondary hotspots that are easy wins once core is stable.

---

### Task 4.1: Zone Tracking Cache

**Files**: `src/movement.lisp`, `src/main.lisp`
**Source**: CODEX
**Est. Benefit**: 2-8%
**Risk**: Low

**Current Problems**:
- `occupied-zone-ids` allocates list each call: `movement.lisp:51-58`
- Used twice per tick in `update-sim`: `main.lisp:450-454`, `main.lisp:493-497`
- `players-in-zone` allocates list + vector: `movement.lisp:42-49`
- `zone-state-player-count` uses `count-if` with lambda: `movement.lisp:38-40`

**Step 4.1.1**: Add cached zone-id vector

**IMPORTANT**: Rebuild from actual player presence, NOT from `*zone-states*` (which may include empty zones).

```lisp
(defparameter *occupied-zones-cache*
  (make-array 64 :element-type t :fill-pointer 0))  ; keywords

(defparameter *occupied-zones-seen*
  (make-hash-table :test 'eq :size 64))  ; Dedup helper

(defun refresh-occupied-zones-cache (players)
  "Rebuild occupied zones cache from actual player presence.
   Call once per tick. Only includes zones with players."
  (declare (optimize (speed 3) (safety 1)))
  (setf (fill-pointer *occupied-zones-cache*) 0)
  (clrhash *occupied-zones-seen*)
  ;; Iterate players to find actually-occupied zones
  (loop for player across players
        when player
        do (let ((zone-id (player-zone-id player)))
             (unless (gethash zone-id *occupied-zones-seen*)
               (setf (gethash zone-id *occupied-zones-seen*) t)
               (vector-push zone-id *occupied-zones-cache*)))))
```

**Alternative** (if zone-state-zone-players is authoritative):
```lisp
(defun refresh-occupied-zones-cache ()
  "Rebuild from zone-states, but only include zones with players."
  (setf (fill-pointer *occupied-zones-cache*) 0)
  (maphash (lambda (zone-id zone-state)
             (when (and (zone-state-zone-players zone-state)
                        (plusp (length (zone-state-zone-players zone-state))))
               (vector-push zone-id *occupied-zones-cache*)))
           *zone-states*))
```

**Step 4.1.2**: Replace `count-if` with tight loop

```lisp
(defun zone-state-player-count (zone-state)
  "Count players in zone without allocating."
  (declare (optimize (speed 3) (safety 1))
           (type zone-state zone-state))
  (let ((count 0)
        (players (zone-state-zone-players zone-state)))
    (declare (type fixnum count))
    (when players
      (loop for player across players
            when player do (incf count)))
    count))
```

**Step 4.1.3**: Ensure `zone-state-zone-players` always populated
- Audit call sites to avoid fallback path in `players-in-zone`

**Step 4.1.4**: Update `update-sim` to use cached data

**Verification**:
```bash
make tests
```

---

### Task 4.2: Render Chunk Cache Key Packing

**Files**: `src/rendering.lisp`, `src/types.lisp`
**Source**: CODEX
**Est. Benefit**: 1-5%
**Risk**: Low-Medium

**Current Problems**:
- Chunk cache key is list/cons: `rendering.lisp:406-410`
- Hash table uses `:test 'equal`: `types.lisp:220-223`
- Keys created in hot render paths: `rendering.lisp:683-686`

**Step 4.2.1**: Create layer-key-id mapping

```lisp
(defparameter *layer-key-ids* (make-hash-table :test 'eq :size 16))
(defvar *next-layer-key-id* 0)

(defun get-layer-key-id (layer-key)
  "Get or create numeric ID for layer-key."
  (or (gethash layer-key *layer-key-ids*)
      (setf (gethash layer-key *layer-key-ids*)
            (incf *next-layer-key-id*))))
```

**Step 4.2.2**: Pack cache key into fixnum

```lisp
(defun pack-chunk-cache-key (layer-key-id chunk-x chunk-y)
  "Pack chunk cache key into single fixnum."
  (declare (optimize (speed 3) (safety 0))
           (type fixnum layer-key-id chunk-x chunk-y))
  (the fixnum (logior (the fixnum (ash layer-key-id 40))
                      (the fixnum (ash chunk-x 20))
                      (the fixnum (logand chunk-y #xFFFFF)))))
```

**Step 4.2.3**: Change cache hash table

```lisp
(defparameter *chunk-cache*
  (make-hash-table :test 'eql :size 1024))
```

**Step 4.2.4**: Update `get-or-render-chunk` and related functions

**Verification**:
```bash
make tests
make smoke  ; Visual verification
```

---

### Task 4.3: Event Queue Ring Buffer

**Files**: `src/types.lisp`, `src/net.lisp`
**Source**: CODEX
**Est. Benefit**: 1-4%
**Risk**: Low

**Current Problems**:
- Combat event queue uses `nconc` with `(list event)`: `types.lisp:683-687`
- Server tick maps events to plists per frame: `net.lisp:2470-2472`

**Steps**:
1. Create fixed-size event ring buffer:

```lisp
(defstruct event-ring
  (buffer (make-array 256) :type simple-vector)
  (head 0 :type fixnum)
  (tail 0 :type fixnum)
  (size 256 :type fixnum))

(defun event-ring-push (ring event)
  "Push event to ring buffer. Overwrites oldest if full."
  (declare (optimize (speed 3) (safety 1)))
  (let ((tail (event-ring-tail ring))
        (size (event-ring-size ring)))
    (setf (aref (event-ring-buffer ring) tail) event)
    (setf (event-ring-tail ring) (mod (1+ tail) size))))

(defun event-ring-clear (ring)
  "Clear ring buffer for next tick."
  (setf (event-ring-head ring) 0
        (event-ring-tail ring) 0))
```

2. Replace `nconc` event pushing with ring buffer insert
3. Update event serialization to iterate ring buffer

**Verification**:
```bash
make tests
```

---

### Task 4.4: Object Pooling (NPCs and Events)

**Files**: `src/types.lisp`, NPC spawn/despawn code
**Source**: CLAUDE
**Est. Benefit**: 2-5%
**Risk**: Low-Medium

**Steps**:
1. Add generic pool infrastructure:

```lisp
(defstruct (object-pool (:constructor make-object-pool (constructor &key (initial-size 64))))
  (free-list nil :type list)
  (constructor nil :type function)
  (allocated 0 :type fixnum)
  (peak 0 :type fixnum))

(defun pool-acquire (pool)
  "Acquire object from pool, creating new if necessary."
  (if (object-pool-free-list pool)
      (pop (object-pool-free-list pool))
      (progn
        (incf (object-pool-allocated pool))
        (setf (object-pool-peak pool)
              (max (object-pool-peak pool) (object-pool-allocated pool)))
        (funcall (object-pool-constructor pool)))))

(defun pool-release (pool object)
  "Return object to pool for reuse."
  (push object (object-pool-free-list pool)))

(defun pool-prewarm (pool count)
  "Pre-allocate COUNT objects into pool."
  (dotimes (i count)
    (pool-release pool (funcall (object-pool-constructor pool)))))
```

2. Create NPC pool and reset function:

```lisp
(defparameter *npc-pool*
  (make-object-pool #'make-npc :initial-size 256))

(defun reset-npc (npc)
  "Reset NPC state for reuse from pool."
  (setf (npc-id npc) 0
        (npc-x npc) 0.0
        (npc-y npc) 0.0
        ;; ... reset all fields
        )
  npc)

(defun acquire-npc ()
  (reset-npc (pool-acquire *npc-pool*)))

(defun release-npc (npc)
  (pool-release *npc-pool* npc))
```

3. Replace `make-npc` calls with `acquire-npc`
4. Call `release-npc` on NPC death/despawn
5. Pre-warm pool at server startup

**Verification**:
```bash
make tests
```

---

## Phase 5: Conditional/Profiling-Gated Optimizations

**Priority**: P3-P4 (LOW/POLISH)
**Sources**: COMMON (both Claude and Codex)
**Estimated Benefit**: 5-20% in specific scenarios
**Risk**: Medium-High
**Rationale**: Only implement after profiling confirms bottlenecks.

---

### Task 5.1: Profile to Identify Compute-Bound Kernels (DEFERRED)

**Purpose**: Validate need for SIMD and identify remaining hotspots.
**Status**: DEFERRED - Only needed if Task 5.2 (SIMD) is pursued.

**Steps**:
1. Run server under load with sb-sprof
2. Capture CPU time breakdown by function
3. Identify if spatial/movement math is CPU-bound (not allocation-bound)
4. Document findings before proceeding with SIMD

**Verification**:
```bash
MMORPG_PROFILE=1 make server
# Analyze sb-sprof output
```

---

### Task 5.2: SIMD Kernels for Spatial/Movement Math (DEFERRED)

**Files**: New `src/kernels.lisp` or in `src/spatial.lisp`
**Source**: COMMON
**Est. Benefit**: 5-15% (only if compute-bound)
**Risk**: High (complexity)
**Status**: DEFERRED

**APPROVED DECISION**: Defer SIMD until after allocations, types, and dispatch are fixed. Allocation/type/dispatch fixes (Phases 1-4) are larger, safer wins. Only implement SIMD after profiling shows compute-bound kernels.

**Only proceed if Task 5.1 shows compute-bound kernels.**

**Steps**:
1. Create `src/kernels/` directory with:
   - Scalar reference implementations
   - SIMD variants using sb-simd
   - Runtime dispatch based on CPU features

2. Example batch distance kernel:

```lisp
(defun batch-distance-squared-scalar (cx cy xs ys results n)
  "Scalar reference implementation for testing."
  (declare (optimize (speed 3) (safety 0))
           (type single-float cx cy)
           (type (simple-array single-float (*)) xs ys results)
           (type fixnum n))
  (loop for i fixnum from 0 below n
        do (let ((dx (- (aref xs i) cx))
                 (dy (- (aref ys i) cy)))
             (declare (type single-float dx dy))
             (setf (aref results i) (+ (* dx dx) (* dy dy))))))

;; SIMD variant (requires sb-simd)
#+sb-simd
(defun batch-distance-squared-simd (cx cy xs ys results n)
  "SIMD implementation using SSE2/AVX."
  ...)
```

3. Add runtime dispatch

**Verification**:
```bash
make test-unit
# Benchmark comparison: scalar vs SIMD
```

---

### Task 5.3: Draw-Call Batching (DEFERRED)

**Files**: `src/rendering.lisp`
**Source**: COMMON
**Est. Benefit**: 5-20% (GPU-bound scenes only)
**Risk**: Medium
**Status**: DEFERRED - raylib already batches internally when textures don't change.
  Chunk caching system provides major draw efficiency gains. Low priority.

**Steps**:
1. Sort entities by texture atlas before drawing
2. Implement batch drawing if raylib supports it
3. Maintain draw ordering correctness (Z-order)

```lisp
(defun draw-entities-batched (entities)
  "Draw entities batched by texture to minimize state changes."
  (let ((sorted (stable-sort (copy-seq entities) #'<
                             :key #'entity-texture-id)))
    (let ((current-texture nil))
      (dolist (entity sorted)
        (let ((tex (entity-texture-id entity)))
          (unless (eql tex current-texture)
            (setf current-texture tex)
            ;; Bind new texture
            ))
        (draw-entity-sprite entity)))))
```

**Verification**:
```bash
make smoke  ; Visual verification
# Benchmark frame time
```

---

### Task 5.4: Strategic GC Scheduling

**Files**: `src/server.lisp`
**Source**: CLAUDE
**Est. Benefit**: 0-5% (reduces worst-case spikes)
**Risk**: Low-Medium

**Steps**:
1. Add GC trigger at safe points:

```lisp
(defparameter *gc-interval-ticks* 3600)  ; Every 60 seconds at 60Hz
(defparameter *gc-enabled* t)

;; In server tick loop, after snapshot broadcast:
(when (and *gc-enabled*
           (zerop (mod tick-count *gc-interval-ticks*)))
  (sb-ext:gc :full nil))  ; Incremental, not full
```

2. Optionally trigger during zone transitions (player is loading anyway)

**Verification**:
```bash
make tests
# Monitor GC behavior under load
```

---

### Task 5.5: Data Access Validation

**Files**: `src/ai.lisp`, `src/combat.lisp`, `src/progression.lisp`
**Source**: CLAUDE
**Est. Benefit**: 2-8% (if plist lookups in hot paths)
**Risk**: Low

**Steps**:
1. Audit for `getf` plist lookups in hot paths
2. If found, ensure data is pre-transformed to structs/hash-tables at load time

```lisp
;; At load time (once):
(defparameter *npc-archetypes* (make-hash-table :test 'eq :size 64))

(defun load-archetypes ()
  (dolist (archetype-plist (load-archetypes-from-file))
    (let ((archetype (plist-to-archetype-struct archetype-plist)))
      (setf (gethash (archetype-id archetype) *npc-archetypes*)
            archetype))))

;; At runtime (hot path):
(gethash archetype-id *npc-archetypes*)  ; O(1) lookup, no consing
```

3. Add type declarations to archetype lookup functions

**Verification**:
```bash
make tests
```

---

## Findings Coverage Matrix

This matrix maps every finding from `mmorpg-perf-unified-findings.md` to a plan task:

| Finding | Source | Plan Task |
|---------|--------|-----------|
| Missing type declarations (structs) | COMMON | Task 1.2 |
| Missing type declarations (functions) | COMMON | Task 1.3 |
| No env-driven optimize policy | COMMON | Task 1.1 |
| CLOS dispatch in hot loops | CODEX | Task 1.5 |
| Spatial grid allocations (cons keys, lists) | COMMON | Task 2.2 |
| Hash table growth/rehash | COMMON | Task 2.1 |
| Snapshot string/plist/octets allocations | COMMON | Task 3.1 |
| Snapshot chunk reassembly | CLAUDE | Task 3.1 |
| Snapshot rate decoupling | COMMON | Task 3.2 |
| Zone tracking per-tick allocations | CODEX | Task 4.1 |
| Render chunk cache key allocations | CODEX | Task 4.2 |
| Event queue churn | CODEX | Task 4.3 |
| Entity pooling | CLAUDE | Task 4.4 |
| host-to-string allocations | CODEX | Task 3.3 |
| SIMD opportunities | COMMON | Task 5.2 |
| Draw-call batching | COMMON | Task 5.3 |
| GC predictability | CLAUDE | Task 5.4 |
| Data runtime access patterns | CLAUDE | Task 5.5 |
| Hash table iteration closures | CLAUDE | ACCEPTED (Low Priority) - Spatial grid is main hotspot; other maphash loops (zone-states, sessions) are bounded by zone/player count, not per-entity. Monitor via profiling. |
| View frustum culling | CLAUDE | Already implemented (GOOD) |
| Buffer reuse pattern | CLAUDE | Already implemented (GOOD) |
| Zone-filtered snapshots | CLAUDE | Already implemented (GOOD) |
| Delta compression | CLAUDE | Already implemented (GOOD) |
| Defstruct usage | COMMON | Already implemented (GOOD) |
| Interpolation buffer pooling | CODEX | Already implemented (GOOD) |

---

## Estimated Aggregate Impact

### Phase-by-Phase (Non-Additive)

| Phase | Est. Benefit | Cumulative (Optimistic) |
|-------|--------------|-------------------------|
| Phase 0 | 0% (baseline) | 0% |
| Phase 1 | 25-55% | 25-55% |
| Phase 2 | 10-35% | 35-70% |
| Phase 3 | 7-25% | 40-80% |
| Phase 4 | 5-15% | 45-85% |
| Phase 5 | 5-20% (conditional) | 50-90% |

### Realistic Expectations

- **After Phases 0-2**: ~40-60% tick time reduction, major GC spike reduction
- **After Phases 3-4**: ~50-75% tick time reduction, smoother frame pacing
- **After Phase 5** (if applicable): ~60-85% total improvement

---

## Validation / Testing Protocol

Per CLAUDE.md, run full test suite in required order after each major task:

```bash
# Required test order (fail-fast)
make checkparens    # 1st - Verify balanced parentheses
make ci             # 2nd - Cold compile + UDP handshake
make smoke          # 3rd - Full client/server smoke test
make test-unit      # 4th - All unit tests
make checkdocs      # 5th - Verify documentation
```

Add performance regression measurements before/after each phase (once server on LAN).

---

## Suggested Execution Order (If Timeboxed)

### Week 1: Foundation + Critical (Phase 1)
*Phase 0 deferred until server on separate LAN machine*
1. Task 1.1: Env-driven optimize policy
2. Task 1.2: Type declarations (structs)
3. Task 1.3: Type declarations (hot functions)
4. Task 1.5: CLOS dispatch removal

### Week 2: Highest ROI (Phase 2)
5. Task 2.1: Hash table pre-sizing
6. Task 2.2: Spatial grid overhaul (largest task)

### Week 3: Serialization (Phase 3)
7. Task 3.1: Snapshot serialization fix
8. Task 3.2: Snapshot rate decoupling
9. Task 3.3: Host string caching

### Week 4: Cleanup (Phase 4)
10. Task 4.1: Zone tracking cache
11. Task 4.2: Chunk cache key packing
12. Task 4.3: Event ring buffer
13. Task 4.4: Object pooling

### Later: Conditional (Phase 5 + Phase 0)
14. Task 5.1: Profile for compute-bound kernels
15. Tasks 5.2-5.5: As warranted by profiling
16. Phase 0: Baseline/regression harness (when server on LAN)

---

## Risk Mitigation

1. **Git commits after each task** - Easy rollback if issues
2. **Run tests frequently** - Catch regressions early (per CLAUDE.md test order)
3. **Profile before SIMD** - Don't optimize non-bottlenecks
4. **Keep generic functions for non-hot paths** - Maintain flexibility where perf doesn't matter
5. **Incremental deployment** - Can ship partial improvements
6. **Baseline when LAN hardware available** - Phase 0 deferred but will validate progress once server on dedicated machine

---

## Design Questions (RESOLVED)

All design questions have been reviewed and answered. Decisions are captured in the **Design Decisions (APPROVED)** section above.

| # | Question | Resolution |
|---|----------|------------|
| 1 | Spatial grid: Array-backed vs hash-table? | **Array-backed** for active zones |
| 2 | CLOS removal: Full or hot paths only? | **Hot paths only** (keep generics for tools) |
| 3 | Snapshot rate: 30Hz vs 20Hz? | **20Hz default**, optional 30Hz tier |
| 4 | SIMD priority: Now or defer? | **Deferred** until after Phases 1-4 |
| 5 | Binary serialization: Full rewrite or incremental? | **Incremental** with feature flag |
| 6 | Protocol change risk: Acceptable? | **Yes**, if incremental |
| 7 | Perf regression harness: Worth it? | **Deferred** until server on separate LAN machine |

---

## Verification Checklist

### After Each Task:
- [ ] `make checkparens` passes
- [ ] `make ci` passes
- [ ] `make smoke` passes
- [ ] `make test-unit` passes
- [ ] No new compiler warnings
- [ ] No regressions in functionality

### After Each Phase:
- [ ] All task checklists complete
- [ ] Profile shows improvement vs baseline
- [ ] Document metrics in performance log

### After All Phases:
- [ ] Profile shows reduced allocation rate
- [ ] 60Hz stability under load (2000 concurrent players target)
- [ ] GC pauses minimized (<5ms typical, <10ms worst case)
- [ ] Horizontal scaling ready (multiple zone servers if needed)

---

## Summary

This unified plan combines:
- **Codex's** Phase 0 baseline, findings coverage matrix, binary serialization approach, estimation methodology
- **Claude's** detailed code examples, step-by-step instructions, verification commands, task granularity

**Key priorities**:
1. **Phase 1** (Foundation) - Unlocks all other optimizations
2. **Task 2.2** (Spatial Grid) - Single highest ROI item
3. **Task 1.5** (CLOS Removal) - Major Codex finding
4. **Phase 3** (Serialization) - High impact, medium effort

**Expected outcome**: 60-80% tick time reduction, stable 60Hz with 2000 concurrent players, near-zero per-tick allocations in hot loops.

---

*End of Unified Final Plan - APPROVED*
