# Unified Performance Optimization Plan: 2000 Players at 60 FPS

## Comparison: CLAUDE_FINDINGS.md vs CODEX_FINDINGS.md

### Common Findings (Both Models Identified)

| Issue | Claude Location | Codex Location | Severity |
|-------|-----------------|----------------|----------|
| Unculled entity rendering | `rendering.lisp:1592-1593` | `rendering.lisp:1568-1594` | CRITICAL |
| O(n²) melee combat checks | `main.lisp:476-479` | `main.lisp:468-506` | HIGH |
| O(n²) NPC targeting (`closest-player`) | `ai.lisp:4-17` | `ai.lisp:4-17` | HIGH |
| Interpolation per-entity processing | `net.lisp:1550-1584` | `net.lisp:1550-1584` | MEDIUM |
| No spatial indexing infrastructure | Confirmed absent | Confirmed absent | HIGH |
| Ping stable / CPU-bound not network-bound | Confirmed | Confirmed | (Diagnosis) |

### Unique to Claude

| Issue | Location | Severity |
|-------|----------|----------|
| Minimap draws all NPCs without culling | `rendering.lisp:1130-1139` | LOW-MEDIUM |
| Snapshot serialization iterates all players per zone | `save.lisp:1166-1174` | MEDIUM |
| Two separate entity animation loops | `main.lisp:462-463, 494-495` | LOW |
| Detailed FPS budget analysis (16.67ms breakdown) | N/A | (Analysis) |
| Payload size analysis (88 bytes/player) | N/A | (Analysis) |

### Unique to Codex

| Issue | Location | Severity |
|-------|----------|----------|
| O(N²) delta apply via linear `find-player-by-id` | `save.lisp:1338-1356`, `types.lisp:491-496` | HIGH |
| Compact decode allocates plist per entity per snapshot | `save.lisp:1001-1040`, `1067-1110` | MEDIUM |
| `capture-entity-positions` allocates hash table + lists | `net.lisp:1474-1520` | MEDIUM |
| Suggested `sb-sprof` profiling approach | N/A | (Methodology) |
| O(1) player lookup via ID→index map | N/A | (Solution) |

---

## Master Task List: All Optimizations

Every item below will be addressed. No deferrals.

### Phase 1: Client-Side Rendering (Highest Impact)

#### Task 1.1: Viewport Culling for Entity Rendering
**Source:** Both (Claude #1, Codex #1)
**Location:** `src/rendering.lisp:1592-1593`
**Problem:** Loop draws ALL entities regardless of camera bounds
**Current Code:**
```lisp
(loop :for entity :across entities
      :do (draw-entity entity assets render))
```
**Solution:**
1. Calculate viewport rectangle from camera position and screen dimensions
2. Add `entity-in-viewport-p` predicate checking entity AABB vs viewport
3. Filter entities before draw loop OR early-exit in draw-entity

**HARD REQUIREMENTS:**
- **Margin for pop-in prevention:** Viewport bounds MUST include a margin of at least `(assets-half-sprite-width assets)` and `(assets-half-sprite-height assets)` on all sides. Without this margin, entities will visibly pop in/out at screen edges.
- **Editor mode consistency:** When in editor mode, use `editor-camera-target` (same as draw path in `rendering.lisp:1583`) to calculate viewport bounds, ensuring culling matches the current editor view.
- **Culling predicate signature:** `(entity-in-viewport-p entity viewport-rect margin)` where margin is sprite half-size.

**Expected Impact:** 40-60% FPS improvement (reduce 2100 draws to ~100)
**Files:** `src/rendering.lisp`
**Tests:** Visual verification + stress test FPS comparison + edge-of-screen entity visibility test

#### Task 1.2: Minimap Entity Culling
**Source:** Claude #5
**Location:** `src/rendering.lisp:1130-1139`
**Problem:** Draws all NPCs on minimap without distance culling
**Current Code:**
```lisp
(loop :for npc :across npcs
      :when (npc-alive npc)
      :do ...)
```
**Solution:**
1. Add minimap radius constant (e.g., 2000 world pixels)
2. Skip NPCs outside player's minimap view radius
3. Use squared distance to avoid sqrt
**Expected Impact:** 5-10% improvement in minimap rendering
**Files:** `src/rendering.lisp`
**Tests:** Visual verification, stress test

---

### Phase 2: Client-Side Data Processing

#### Task 2.1: O(1) Player Lookup for Delta Apply
**Source:** Codex #2
**Location:** `src/save.lisp:1338-1356`, `src/types.lisp:491-496`
**Problem:** `find-player-by-id` is O(n) linear scan, called per player in delta = O(n²)
**Current Code:**
```lisp
(defun find-player-by-id (players id)
  (when (and players (> id 0))
    (loop :for player :across players
          :when (= (player-id player) id)
          :return player)))
```
**Solution:**
1. Add `player-index-map` hash table to game struct (ID → array index)
2. Update map on player add/remove
3. Replace `find-player-by-id` calls with hash lookup

**HARD REQUIREMENTS - Map Update Hooks:**
The index map MUST be updated at these exact locations or it will drift and corrupt delta apply:

| Location | Function | Action |
|----------|----------|--------|
| `net.lisp:931` | `add-player-to-game` | Insert new entry: `(setf (gethash id map) index)` |
| `net.lisp:944` | `remove-player-from-game` | Delete entry AND rebuild indices if array compacts (shifting indices invalidates map) |
| `save.lisp` | `apply-player-plists` | If this rebuilds the players array, MUST rebuild entire map |

**Index Rebuild Strategy:**
When removing a player causes array compaction (shifting elements), either:
- Option A: Rebuild entire map in O(n) - simpler, acceptable for infrequent removes
- Option B: Use stable indices (sparse array with nil slots) - faster removes but wastes memory
**Recommended:** Option A for simplicity; removes are rare compared to lookups.

**Expected Impact:** O(n²) → O(n) for delta apply
**Files:** `src/types.lisp`, `src/save.lisp`, `src/net.lisp`
**Tests:** Unit test for lookup + add + remove + rebuild, stress test with player churn

#### Task 2.2: Reduce Plist Allocations in Compact Decode
**Source:** Codex #4
**Location:** `src/save.lisp:1001-1040` (player), `1067-1110` (npc)
**Problem:** `deserialize-player-compact` creates fresh plist per entity per snapshot
**Current Code:**
```lisp
(defun deserialize-player-compact (vec)
  (when (and vec (>= (length vec) 19))
    (list :id (aref vec 0)
          :x (dequantize-coord (aref vec 1))
          ...)))
```
**Solution Options:**
- Option A: Apply directly to struct without intermediate plist
- Option B: Reuse a thread-local plist buffer, overwriting values
- Option C: Use structs with pre-allocated slots
**Recommended:** Option A - direct struct application

**HARD REQUIREMENT - Client-Only State Preservation:**
Direct compact apply MUST preserve the same client-only fields as `apply-player-plist` currently does. See `save.lisp` in `apply-player-plist` for the authoritative list.

Client-only fields that MUST NOT be overwritten by network data:
- Input/control state (click markers, input buffers)
- Local UI state (selection, highlights)
- Client-side prediction state
- Any field not present in the compact vector

**Implementation Pattern:**
```lisp
(defun apply-player-compact-direct (player vec)
  "Apply compact vector directly to player struct, preserving client-only state."
  ;; Only update fields that ARE in the compact vector
  (setf (player-x player) (dequantize-coord (aref vec 1)))
  (setf (player-y player) (dequantize-coord (aref vec 2)))
  ;; ... network fields only
  ;; DO NOT touch: player-click-x, player-click-y, player-local-*, etc.
  player)
```

**Expected Impact:** Reduce GC pressure by ~50% on snapshot apply
**Files:** `src/save.lisp`
**Tests:** Unit test verifying client-only fields preserved after apply, GC profiling with `sb-sprof`

#### Task 2.3: Optimize Interpolation Buffer Allocations
**Source:** Codex #3, Claude #4
**Location:** `src/net.lisp:1474-1520` (capture), `1550-1584` (interpolate)
**Problem:**
- `capture-entity-positions` allocates new hash table per snapshot
- Stores `(list x y)` per entity = cons cell allocation
**Current Code:**
```lisp
(defun capture-entity-positions (game)
  (let ((positions (make-hash-table :test 'eql)))  ; New hash table every call
    (loop :for player :across players
          :do (setf (gethash id positions) (list x y)))  ; New list per entity
    ...))
```
**Solution:**
1. Pre-allocate position hash table in interpolation-buffer struct
2. Clear and reuse instead of recreating
3. Store positions as reusable 2-element vectors instead of fresh lists
4. Consider parallel arrays (ids[], xs[], ys[]) for cache locality

**HARD REQUIREMENT - Aliasing Risk Prevention:**
If reusing a positions table across frames, the "previous snapshot" stored in the interpolation buffer MUST NOT be mutated after being stored. Otherwise interpolation will read corrupted data (self-overwrite).

**Safe Implementation Patterns:**

Option A: Double-buffering (recommended)
```lisp
;; interpolation-buffer has TWO position tables
(defstruct interpolation-buffer
  (positions-a (make-hash-table))  ; Snapshot N
  (positions-b (make-hash-table))  ; Snapshot N-1
  (current-is-a t))                ; Which is "current"

;; On new snapshot: swap and overwrite the OLD one
(defun capture-entity-positions (buffer game)
  (let ((target (if (interpolation-buffer-current-is-a buffer)
                    (interpolation-buffer-positions-b buffer)  ; Write to B
                    (interpolation-buffer-positions-a buffer)))) ; Write to A
    (clrhash target)
    ;; Fill target...
    (setf (interpolation-buffer-current-is-a buffer)
          (not (interpolation-buffer-current-is-a buffer)))))
```

Option B: Copy-on-capture
- Deep copy positions when storing in buffer (defeats allocation reduction goal)

Option C: Ring buffer of N snapshots
- Pre-allocate N position tables, cycle through them
- N = interpolation delay in snapshots + 1

**Recommended:** Option A (double-buffer) - simple, safe, minimal allocation.

**Expected Impact:** 30-50% reduction in GC pressure during interpolation
**Files:** `src/net.lisp`, `src/types.lisp`
**Tests:** Interpolation correctness test (verify lerp produces expected positions), GC profiling, stress test stability

---

### Phase 3: Server-Side Spatial Indexing

#### Task 3.1: Implement Spatial Grid Data Structure
**Source:** Both (foundation for tasks 3.2, 3.3)
**Location:** New file `src/spatial.lisp`
**Problem:** No spatial data structure exists for efficient proximity queries
**Solution:**
1. Create `spatial-grid` struct:
   - Cell size: tile-sized (e.g., 32x32 pixels)
   - Hash table: `(cell-x . cell-y)` → list of entity IDs
2. Implement core functions:
   - `spatial-grid-create` - initialize for zone dimensions
   - `spatial-grid-insert` - add entity to cell
   - `spatial-grid-remove` - remove entity from cell
   - `spatial-grid-move` - update on position change (remove from old cell, insert to new)
   - `spatial-grid-query-radius` - get entities within N cells
   - `spatial-grid-query-neighbors` - get entities in adjacent cells
3. Integrate with zone-state struct

**HARD REQUIREMENT - Entity Cell Tracking:**
For O(1) cell updates on movement, entities MUST track their current cell. Add fields:

```lisp
;; In player struct (types.lisp)
(defstruct player
  ...
  (grid-cell-x nil)  ; Current spatial grid cell X (or nil if not in grid)
  (grid-cell-y nil)) ; Current spatial grid cell Y

;; In npc struct (types.lisp)
(defstruct npc
  ...
  (grid-cell-x nil)
  (grid-cell-y nil))
```

**HARD REQUIREMENT - Update Hooks:**
Grid cell membership MUST be updated at these exact locations:

| Location | Function | Action |
|----------|----------|--------|
| `movement.lisp` | `update-player-position` | After position change, call `spatial-grid-move` if cell changed |
| `ai.lisp` | `update-npc-movement` | After NPC position change, call `spatial-grid-move` if cell changed |
| `movement.lisp` | `transition-zone` | Remove from old zone grid, insert to new zone grid |
| `net.lisp` | `add-player-to-game` | Insert player into zone's spatial grid |
| `net.lisp` | `remove-player-from-game` | Remove player from zone's spatial grid |

**Cell Change Detection:**
```lisp
(defun position-to-cell (x y cell-size)
  (values (floor x cell-size) (floor y cell-size)))

(defun entity-cell-changed-p (entity new-x new-y cell-size)
  (multiple-value-bind (new-cx new-cy) (position-to-cell new-x new-y cell-size)
    (or (not (eql new-cx (entity-grid-cell-x entity)))
        (not (eql new-cy (entity-grid-cell-y entity))))))
```

**HARD REQUIREMENT - Zone-Local Grids:**
Each zone-state MUST have its own spatial grid. Do NOT use a global grid.
```lisp
(defstruct zone-state
  ...
  (player-grid nil)  ; Spatial grid for players in this zone
  (npc-grid nil))    ; Spatial grid for NPCs in this zone
```

**HARD REQUIREMENT - Build System Integration:**
1. Add `spatial.lisp` to `mmorpg.asd` in correct load order (after `types.lisp`, before `movement.lisp`)
2. Create `docs/spatial.md` documenting the grid API and update hooks

**Expected Impact:** Foundation for O(n²) → O(n) improvements
**Files:** `src/spatial.lisp` (NEW), `src/zone.lisp`, `src/types.lisp`, `mmorpg.asd`, `docs/spatial.md` (NEW)
**Tests:** Unit tests for all spatial operations, cell tracking verification, zone transition grid update test

#### Task 3.2: Spatial Grid for NPC Targeting
**Source:** Both (Claude #2, Codex #5)
**Location:** `src/ai.lisp:4-17`
**Problem:** `closest-player` scans ALL players for EVERY NPC
**Current Code:**
```lisp
(defun closest-player (players npc)
  (loop :for player :across players
        :when (combatant-alive-p player)
        :do ...))
```
**Solution:**
1. Query spatial grid for players within aggro radius cells
2. Only check distance to players in nearby cells
3. Typical aggro radius = 3-5 tiles = query ~25 cells max

**HARD REQUIREMENT - Zone-Local Grid Access:**
`closest-player` MUST use the zone-state's player-grid, NOT a global grid.

**New Signature:**
```lisp
(defun closest-player (zone-state npc)
  "Return closest alive player to NPC using zone's spatial grid."
  (let* ((grid (zone-state-player-grid zone-state))
         (nearby-ids (spatial-grid-query-radius grid
                                                 (npc-x npc)
                                                 (npc-y npc)
                                                 *npc-aggro-radius-cells*)))
    ;; Only check distance to nearby players
    ...))
```

**Caller Update:**
`simulate-zone-npcs` in `main.lisp` already has `zone-state` - pass it to `closest-player`.

**Expected Impact:** O(n×m) → O(n×k) where k ≈ 10-20 nearby players
**Files:** `src/ai.lisp`, `src/spatial.lisp`, `src/main.lisp` (caller update)
**Tests:** Unit test, AI behavior verification (NPC still targets closest player), stress test

#### Task 3.3: Spatial Grid for Melee Combat
**Source:** Both (Claude #3, Codex #5)
**Location:** `src/main.lisp:476-479`
**Problem:** Nested loop checks every player against every NPC
**Current Code:**
```lisp
(loop :for current-player :across zone-players
      :do (loop :for npc :across zone-npcs
                :do (apply-melee-hit current-player npc world event-queue)))
```
**Solution:**
1. For each attacking player, query spatial grid for nearby NPCs only
2. Melee range = ~1 tile = query 9 cells (3x3)
3. Skip players not in attack state (early filter)

**HARD REQUIREMENT - Zone-Local Grid Access:**
Combat checks MUST use the zone-state's npc-grid, NOT a global grid.

**New Pattern:**
```lisp
(when (and zone-state zone-players)
  (let ((npc-grid (zone-state-npc-grid zone-state)))
    (loop :for current-player :across zone-players
          :when (player-attacking-p current-player)  ; Early filter
          :do (let ((nearby-npc-ids (spatial-grid-query-neighbors
                                      npc-grid
                                      (player-x current-player)
                                      (player-y current-player))))
                (dolist (npc-id nearby-npc-ids)
                  (let ((npc (find-npc-by-id zone-npcs npc-id)))
                    (when npc
                      (apply-melee-hit current-player npc world event-queue))))))))
```

**Optimization - Attack State Filter:**
Add early check `(player-attacking-p current-player)` to skip players not currently attacking. This reduces grid queries to only active attackers.

**Expected Impact:** O(n×m) → O(n×k) where k ≈ 5-10 nearby NPCs
**Files:** `src/main.lisp`, `src/combat.lisp`, `src/spatial.lisp`
**Tests:** Combat behavior verification (damage still applies correctly), stress test, edge case: player on cell boundary hits NPC in adjacent cell

---

### Phase 4: Server-Side Serialization

#### Task 4.1: Cache Zone-Filtered Player Arrays
**Source:** Claude #6
**Location:** `src/save.lisp:1166-1174`
**Problem:** Iterates ALL players to filter by zone on every serialization
**Current Code:**
```lisp
(loop :for player :across players
      :for player-zone = (or (player-zone-id player) *starting-zone-id*)
      :when (and player (eq player-zone zone-id))
      :do (push (serialize-player-compact player) player-vectors))
```
**Solution:**
1. Maintain per-zone player arrays in zone-state
2. Update on player zone transition (already have dirty flag system)
3. Serialization reads cached array directly
**Expected Impact:** O(total-players) → O(zone-players) per serialization
**Files:** `src/zone.lisp`, `src/save.lisp`, `src/net.lisp`
**Tests:** Zone transition tests, stress test

#### Task 4.2: Pre-allocate Compact Vectors for Serialization
**Source:** Claude #4 (Priority 4)
**Location:** `src/save.lisp:974-999`
**Problem:** `serialize-player-compact` creates fresh vector (22 elements) per player
**Current Code:**
```lisp
(defun serialize-player-compact (player)
  (vector (player-id player)
          (quantize-coord (player-x player))
          ...))
```
**Solution:**
1. Add reusable vector pool for snapshot serialization
2. Acquire vector from pool, fill, use, return to pool
3. Pool size = max expected zone players
**Expected Impact:** Reduce allocation during hot snapshot path
**Files:** `src/save.lisp`, `src/utils.lisp` (pool implementation)
**Tests:** Unit test for pool, stress test GC profile

---

### Phase 5: Server-Side Simulation Cleanup

#### Task 5.1: Merge Entity Animation Loops
**Source:** Claude #7
**Location:** `src/main.lisp:462-463, 494-495`
**Problem:** Two separate loops over all entities
**Current Code:**
```lisp
(loop :for entity :across entities
      :do (update-entity-animation entity dt))
...
(loop :for entity :across entities
      :do (combatant-update-hit-effect entity dt))
```
**Solution:**
1. Merge into single loop:
```lisp
(loop :for entity :across entities
      :do (update-entity-animation entity dt)
          (combatant-update-hit-effect entity dt))
```
**Expected Impact:** Minor - reduces loop overhead, improves cache locality
**Files:** `src/main.lisp`
**Tests:** Animation behavior verification

---

### Phase 6: Profiling Infrastructure

#### Task 6.1: Add Profiling Hooks
**Source:** Codex (suggested methodology)
**Problem:** No easy way to measure where time is spent
**Solution:**
1. Add optional timing macros around hot paths:
   - `with-timing` macro that logs to `*profile-log*`
   - Controlled by `*profile-enabled*` flag
2. Key measurement points:
   - `draw-game` total time
   - `deserialize-game-state-delta` time
   - `interpolate-remote-entities` time
   - `serialize-game-state-for-zone` time
   - `update-sim` time (server)
3. Add `make profile` target that runs with profiling enabled
**Expected Impact:** Data-driven optimization decisions
**Files:** `src/utils.lisp`, `src/main.lisp`, `src/net.lisp`, `src/rendering.lisp`
**Tests:** Profile output verification

#### Task 6.2: Add GC Pressure Monitoring
**Source:** Codex (suggested methodology)
**Problem:** Hard to identify allocation hotspots
**Solution:**
1. Add `(sb-ext:gc-run-time)` tracking before/after hot paths
2. Log GC counts per frame when `*verbose-gc*` enabled
3. Identify functions causing most allocations
**Expected Impact:** Pinpoint allocation-heavy code
**Files:** `src/utils.lisp`, `src/main.lisp`
**Tests:** GC logging verification

---

## Implementation Order (Dependencies)

```
Phase 1 (Client Rendering) - No dependencies, highest impact
  └── Task 1.1: Viewport Culling [CRITICAL]
  └── Task 1.2: Minimap Culling [LOW]

Phase 2 (Client Data) - No dependencies
  └── Task 2.1: O(1) Player Lookup [HIGH]
  └── Task 2.2: Reduce Plist Allocations [MEDIUM]
  └── Task 2.3: Interpolation Buffer Optimization [MEDIUM]

Phase 3 (Server Spatial) - Task 3.1 is prerequisite for 3.2 and 3.3
  └── Task 3.1: Spatial Grid Infrastructure [HIGH - FOUNDATION]
      └── Task 3.2: Spatial NPC Targeting [HIGH]
      └── Task 3.3: Spatial Melee Combat [HIGH]

Phase 4 (Server Serialization) - Independent
  └── Task 4.1: Cache Zone-Filtered Arrays [MEDIUM]
  └── Task 4.2: Pre-allocate Compact Vectors [LOW]

Phase 5 (Server Cleanup) - Independent
  └── Task 5.1: Merge Animation Loops [LOW]

Phase 6 (Profiling) - Can run in parallel with anything
  └── Task 6.1: Profiling Hooks [DIAGNOSTIC]
  └── Task 6.2: GC Monitoring [DIAGNOSTIC]
```

---

## Recommended Execution Sequence

### Sprint 1: Maximum Client Impact
1. **Task 1.1** - Viewport culling (CRITICAL - biggest single win)
2. **Task 2.1** - O(1) player lookup (HIGH - fixes O(n²))
3. **Task 6.1** - Profiling hooks (enables measurement)

**Checkpoint:** Run `STRESS_CLIENTS=900 make stress`, expect 50-55 FPS

### Sprint 2: Server Spatial Foundation
4. **Task 3.1** - Spatial grid data structure
5. **Task 3.2** - Spatial NPC targeting
6. **Task 3.3** - Spatial melee combat

**Checkpoint:** Run `STRESS_CLIENTS=1200 make stress`, expect 55-60 FPS

### Sprint 3: Allocation Reduction
7. **Task 2.2** - Reduce plist allocations
8. **Task 2.3** - Interpolation buffer optimization
9. **Task 4.1** - Cache zone-filtered arrays

**Checkpoint:** Run `STRESS_CLIENTS=1500 make stress`, expect 55-60 FPS

### Sprint 4: Polish and Cleanup
10. **Task 4.2** - Pre-allocate compact vectors
11. **Task 5.1** - Merge animation loops
12. **Task 1.2** - Minimap culling
13. **Task 6.2** - GC monitoring

**Final Checkpoint:** Run `STRESS_CLIENTS=2000 make stress`, target 60 FPS

---

## Success Criteria

| Metric | Current | Target |
|--------|---------|--------|
| FPS at 400 players | 60 | 60 |
| FPS at 900 players | 35-45 | 60 |
| FPS at 1500 players | N/A | 55-60 |
| FPS at 2000 players | N/A | 55-60 |
| Ping (localhost) | 17-19ms | 17-19ms (no regression) |
| All tests passing | Yes | Yes |

---

## Testing Protocol

After EACH task completion:
1. `make tests` - All tests must pass
2. `STRESS_CLIENTS=500 make stress` - No regression
3. `STRESS_CLIENTS=900 make stress` - Measure improvement
4. Document FPS results in commit message

After EACH sprint:
1. Full stress test progression: 500 → 900 → 1200 → 1500 → 2000
2. Profile hot paths if FPS target not met
3. Adjust priorities based on measured data

---

## Files Summary

| File | Tasks Affecting It |
|------|-------------------|
| `src/rendering.lisp` | 1.1, 1.2, 6.1 |
| `src/save.lisp` | 2.1, 2.2, 4.1, 4.2 |
| `src/net.lisp` | 2.1, 2.3, 3.1, 6.1 |
| `src/types.lisp` | 2.1, 2.3, 3.1 |
| `src/ai.lisp` | 3.1, 3.2 |
| `src/main.lisp` | 3.2, 3.3, 5.1, 6.1, 6.2 |
| `src/zone.lisp` | 3.1, 4.1 |
| `src/movement.lisp` | 3.1 |
| `src/spatial.lisp` (NEW) | 3.1, 3.2, 3.3 |
| `src/utils.lisp` | 4.2, 6.1, 6.2 |
| `src/combat.lisp` | 3.3 |
| `mmorpg.asd` | 3.1 |
| `docs/spatial.md` (NEW) | 3.1 |

---

## Hard Requirements Summary (Quick Reference)

This section consolidates all hard requirements for easy review during implementation.

### Task 1.1 - Viewport Culling
- [ ] Margin = sprite half-size on all viewport edges
- [ ] Editor mode uses `editor-camera-target` for viewport calculation

### Task 2.1 - O(1) Player Lookup
- [ ] Map updated in `add-player-to-game` (net.lisp:931)
- [ ] Map updated in `remove-player-from-game` (net.lisp:944)
- [ ] Map rebuilt if `apply-player-plists` rebuilds array

### Task 2.2 - Direct Compact Apply
- [ ] Preserves ALL client-only fields from `apply-player-plist`
- [ ] Does NOT overwrite: click markers, local UI state, prediction state

### Task 2.3 - Interpolation Buffer
- [ ] Double-buffer or ring-buffer to prevent aliasing
- [ ] Previous snapshot NEVER mutated after storage

### Task 3.1 - Spatial Grid
- [ ] `grid-cell-x`, `grid-cell-y` fields added to player AND npc structs
- [ ] Grid updated in `update-player-position` (movement.lisp)
- [ ] Grid updated in `update-npc-movement` (ai.lisp)
- [ ] Grid updated in `transition-zone` (movement.lisp)
- [ ] Grid updated in `add-player-to-game` (net.lisp)
- [ ] Grid updated in `remove-player-from-game` (net.lisp)
- [ ] ONE grid PER zone-state (not global)
- [ ] `spatial.lisp` added to `mmorpg.asd` load order
- [ ] `docs/spatial.md` created

### Task 3.2 - Spatial NPC Targeting
- [ ] Uses `zone-state-player-grid`, NOT global
- [ ] `simulate-zone-npcs` passes zone-state to `closest-player`

### Task 3.3 - Spatial Melee Combat
- [ ] Uses `zone-state-npc-grid`, NOT global
- [ ] Early filter: skip players not in attack state

---

## Risk Assessment

| Risk | Mitigation |
|------|------------|
| Spatial grid adds overhead for low player counts | Early-exit if zone has < 50 players |
| Object pooling complexity | Start with simple fixed-size pool |
| Viewport culling edge cases (entities partially visible) | Add margin = sprite half-size to viewport bounds |
| Breaking existing tests | Run full test suite after each change |
| GC pauses during stress test | Monitor with `sb-sprof`, adjust pool sizes |
| **Player index map drift (Task 2.1)** | Update map at ALL specified hooks; unit test with add/remove churn |
| **Client-only state regression (Task 2.2)** | Test that click markers, UI state survive snapshot apply |
| **Interpolation aliasing (Task 2.3)** | Use double-buffering; never mutate stored snapshots |
| **Grid cell tracking out of sync (Task 3.1)** | Update at ALL specified movement hooks; unit test cell accuracy |
| **Cross-zone grid coupling (Task 3.2/3.3)** | Enforce zone-local grids in code review; no global grid variable |
| **Editor mode culling mismatch (Task 1.1)** | Use same camera target for culling as draw path |

---

## Conclusion

This plan addresses **all 13 identified issues** from both analyses:
- 6 common findings (addressed once each)
- 5 Claude-unique findings
- 5 Codex-unique findings (3 issues + 2 methodology items)

No deferrals. Every item has a specific task, location, solution, and test criteria.

Estimated total effort: 4 sprints of focused work.
Expected outcome: 2000 simultaneous players at 60 FPS.
