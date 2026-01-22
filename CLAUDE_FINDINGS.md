# Performance Analysis: Scaling to 2000 Simultaneous Players

## Executive Summary

**Current State:** Smooth 60 FPS at 300-400 players, degrading to 35-45 FPS at 900 players.

**Target:** 2000 simultaneous players at 60 FPS.

**Root Cause:** The FPS drop is caused by a combination of:
1. **Client-side:** Unculled entity rendering (draws ALL entities every frame regardless of visibility)
2. **Server-side:** O(n²) algorithms in NPC AI targeting and melee combat checks
3. **Both:** Linear scans through all entities for various operations

**Key Insight:** Ping stays constant (17-19ms) because the bottleneck is NOT network bandwidth. The server is keeping up with sending packets - the issue is CPU-bound simulation/rendering.

---

## Detailed Bottleneck Analysis

### 1. CRITICAL: Unculled Entity Rendering (Client)

**Location:** `src/rendering.lisp:1592-1593`

```lisp
(loop :for entity :across entities
      :do (draw-entity entity assets render))
```

**Problem:**
- The `entities` array combines ALL NPCs + ALL players (see `types.lisp:480-489`)
- At 900 players + ~100 NPCs = **1000 draw calls per frame**
- **Zero viewport culling** - even entities far off-screen are processed
- The world tiles DO have excellent culling (lines 356-359), but entities bypass this

**Per-Entity Cost:**
Each `draw-entity` call performs (from `rendering.lisp:763-805`):
1. Animation state lookup (`player-texture-for`)
2. Rectangle calculations (source and dest)
3. `raylib:draw-texture-pro` call
4. Health bar rendering (`draw-health-bar`)
5. Hit effect rendering if active (`draw-hit-effect`)

**Impact at Scale:**
| Players | NPCs | Total Entities | Draw Calls/Frame | FPS Impact |
|---------|------|----------------|------------------|------------|
| 400 | 100 | 500 | 500 | Acceptable |
| 700 | 100 | 800 | 800 | Noticeable |
| 900 | 100 | 1000 | 1000 | Significant |
| 2000 | 100 | 2100 | 2100 | Unplayable |

**Severity:** CRITICAL - Primary cause of client FPS degradation

---

### 2. CRITICAL: O(n²) NPC-to-Player Targeting (Server)

**Location:** `src/ai.lisp:4-17`

```lisp
(defun closest-player (players npc)
  (let ((best nil)
        (best-dist nil))
    (when players
      (loop :for player :across players
            :when (combatant-alive-p player)
              :do (let* ((dx (- (player-x player) (npc-x npc)))
                         (dy (- (player-y player) (npc-y npc)))
                         (dist (+ (* dx dx) (* dy dy))))
                    (when (or (null best-dist) (< dist best-dist))
                      (setf best player
                            best-dist dist)))))
    best))
```

**Called From:** `src/main.lisp:366` inside `simulate-zone-npcs`:
```lisp
(loop :for npc :across zone-npcs
      :for target-player = (closest-player zone-players npc)
      :do ...)
```

**Problem:**
- **Every NPC** scans **every player** in the zone to find the closest one
- Complexity: O(NPCs × Players) = O(n²) when NPCs scale with players
- Uses squared distance (good - avoids sqrt), but still iterates all players

**Impact at Scale (assuming 100 NPCs per zone):**
| Zone Players | Distance Calculations/Frame |
|--------------|----------------------------|
| 200 | 20,000 |
| 450 | 45,000 |
| 500 | 50,000 |
| 1000 | 100,000 |

**Note:** Zone filtering (Phase 5) helps significantly, but within a single zone this is still O(n²).

**Severity:** HIGH - Quadratic scaling on server

---

### 3. CRITICAL: O(n²) Melee Combat Detection (Server)

**Location:** `src/main.lisp:476-479`

```lisp
(when (and zone-npcs zone-players)
  (loop :for current-player :across zone-players
        :do (loop :for npc :across zone-npcs
                  :do (apply-melee-hit current-player npc world event-queue))))
```

**Problem:**
- **Every player** is checked against **every NPC** for melee collision
- This is a classic O(n × m) nested loop, effectively O(n²)
- Each `apply-melee-hit` call performs:
  - Hitbox calculation (`attack-hitbox`)
  - AABB overlap check (`aabb-overlap-p`)
  - Damage calculation if hit
  - XP/gold awards if kill

**Impact at Scale (100 NPCs per zone):**
| Zone Players | Collision Checks/Frame |
|--------------|------------------------|
| 200 | 20,000 |
| 450 | 45,000 |
| 500 | 50,000 |

**Severity:** HIGH - Quadratic scaling, no spatial indexing

---

### 4. HIGH: Interpolation Buffer Processing (Client)

**Location:** `src/net.lisp:1550-1584`

```lisp
(defun interpolate-remote-entities (game)
  ...
  (let ((players (game-players game)))
    (when players
      (loop :for player :across players
            :for id = (player-id player)
            :when (and (> id 0) (/= id local-id))
            :do ...)))  ; Interpolate position
  (let ((npcs (game-npcs game)))
    (when npcs
      (loop :for npc :across npcs
            :for id = (npc-id npc)
            :when (> id 0)
            :do ...)))  ; Interpolate position
```

**Problem:**
- Runs **every client frame** (60+ FPS)
- Iterates through ALL players and ALL NPCs
- Each iteration does hash table lookups (`gethash id pos-a`, `gethash id pos-b`)
- Performs lerp calculations for each entity

**Impact:**
- At 1000 entities × 60 FPS = 60,000 iterations/second
- Hash table lookups are O(1), but constant factor adds up
- Not the primary bottleneck, but contributes to overhead

**Severity:** MEDIUM - Linear but runs at 60 FPS

---

### 5. MEDIUM: Minimap Rendering (Client)

**Location:** `src/rendering.lisp:1130-1139`

```lisp
(loop :for npc :across npcs
      :when (npc-alive npc)
      :do (multiple-value-bind (nx ny)
              (minimap-world-to-screen ui world player (npc-x npc) (npc-y npc))
            (when (and nx ny)
              (raylib:draw-rectangle ...))))
```

**Problem:**
- Draws ALL NPCs on minimap regardless of visibility
- No distance-based culling from player position
- Each NPC requires coordinate transform + draw call

**Severity:** LOW-MEDIUM - Usually fewer NPCs than players

---

### 6. MEDIUM: Snapshot Serialization (Server)

**Location:** `src/save.lisp:1166-1174`

```lisp
(loop :for player :across players
      :for player-zone = (or (player-zone-id player) *starting-zone-id*)
      :when (and player (eq player-zone zone-id))
      :do (push (serialize-player-compact player) player-vectors))
```

**Architecture Note:** Snapshot is serialized ONCE per zone, then sent to all clients in that zone. This is already well-optimized ("encode once, send to many" pattern).

**Problem:**
- Still iterates ALL players to filter by zone
- Each `serialize-player-compact` creates a fresh vector (22 elements)
- Runs 15 times/second (15 Hz tick rate)

**Payload Size:** 22 fields per player × 4 bytes average = ~88 bytes/player
- 500 players = ~44 KB per snapshot
- 1000 players = ~88 KB per snapshot
- Network is NOT the bottleneck here

**Severity:** MEDIUM - Good architecture, but serialization cost grows linearly

---

### 7. LOW: Entity Animation Updates (Server)

**Location:** `src/main.lisp:462-463, 494-495`

```lisp
(loop :for entity :across entities
      :do (update-entity-animation entity dt))

(loop :for entity :across entities
      :do (combatant-update-hit-effect entity dt))
```

**Problem:**
- Two separate loops over all entities per frame
- Each does simple timer/frame updates

**Severity:** LOW - O(n) but trivial per-iteration cost

---

## No Spatial Indexing

**Finding:** Searched for "spatial", "quadtree", "grid" (in indexing context) - none found.

The codebase has no spatial data structure for:
- Finding nearby entities
- Collision detection
- Visibility culling

All entity queries use linear scans.

---

## Scaling Math: Why 2000 Players Fails

### Current Algorithm Complexity

| Operation | Complexity | At 400 players | At 2000 players |
|-----------|-----------|----------------|-----------------|
| Entity rendering | O(n) | 500 draws | 2100 draws |
| NPC targeting | O(n×m) | 40,000 | 200,000 |
| Melee combat | O(n×m) | 40,000 | 200,000 |
| Interpolation | O(n) | 500 | 2100 |
| Serialization | O(n) | 500 | 2100 |

### FPS Budget Analysis

At 60 FPS target: 16.67ms per frame budget

**Client frame time breakdown (estimated at 900 players):**
- Tile rendering: ~2ms (culled, efficient)
- Entity rendering: ~8ms (1000 unculled draw calls)
- Interpolation: ~1ms
- Input/UI: ~1ms
- Buffer swap: ~1ms
- **Total: ~13ms** - Leaves ~3ms headroom

**At 2000 players:**
- Entity rendering: ~16-20ms (2100 draw calls)
- **Would exceed 16.67ms budget alone**

---

## Recommendations Summary

### Priority 1: Client-Side Viewport Culling (Highest Impact)

**Estimated Improvement:** 40-60% FPS recovery

Only draw entities within camera viewport:
- Typical viewport shows ~50-100 entities max
- Reduces draw calls from 2100 to ~100 at 2000 players
- Single biggest win for client performance

**Implementation approach:**
- Calculate viewport bounds from camera position
- Filter entities by AABB intersection before draw loop
- Can reuse existing `minimap-world-to-screen` type transforms

### Priority 2: Spatial Hashing for NPC AI (Server)

**Estimated Improvement:** 10-20x speedup for NPC targeting

Grid-based spatial index:
- Divide zone into 32x32 pixel cells (or tile-sized)
- Track which entities occupy which cells
- NPC targeting queries only neighboring cells

**Complexity change:** O(n²) → O(n × k) where k is ~10 nearby cells

### Priority 3: Spatial Collision Detection (Server)

**Estimated Improvement:** 10-20x speedup for melee combat

Same grid structure for collision:
- Only check player-NPC pairs in same/adjacent cells
- Skip most collision checks entirely

### Priority 4: Entity Pooling / Pre-allocated Vectors

Reduce per-frame allocations:
- Pre-allocate player compact vectors
- Reuse rectangles in rendering
- Object pool for snapshot building

---

## Files That Would Need Changes

| File | Change Type | Bottleneck Addressed |
|------|-------------|---------------------|
| `src/rendering.lisp` | Viewport culling before entity loop | #1 Entity rendering |
| `src/ai.lisp` | Spatial grid query for closest player | #2 NPC targeting |
| `src/main.lisp` | Spatial grid for melee checks | #3 Combat detection |
| `src/zone.lisp` (or new `spatial.lisp`) | Spatial grid data structure | All spatial queries |
| `src/types.lisp` | Grid cell membership in entity structs | All spatial queries |

---

## Quick Wins vs. Major Refactors

### Quick Wins (1-2 hours each)
1. Add viewport culling to entity rendering loop
2. Cache zone-filtered player arrays instead of re-filtering

### Medium Effort (4-8 hours)
3. Implement simple spatial grid (hash table keyed by cell coordinates)
4. Update NPC AI to use spatial queries

### Major Refactor (1-2 days)
5. Full spatial indexing system with automatic cell updates
6. Collision detection via spatial grid
7. Object pooling for high-frequency allocations

---

## Testing Recommendations

After any changes:
1. `make tests` - All tests must pass
2. `STRESS_CLIENTS=500 make stress` - Verify no regression at 500
3. `STRESS_CLIENTS=1000 make stress` - Test improvement at 1000
4. `STRESS_CLIENTS=2000 make stress` - Verify 2000 player target

---

## Conclusion

The architecture is fundamentally sound (zone filtering, encode-once snapshots, server authority). The bottlenecks are:

1. **Client:** Drawing all entities regardless of visibility
2. **Server:** Brute-force O(n²) algorithms that could use spatial indexing

With viewport culling + spatial hashing, 2000 players at 60 FPS is achievable. The network layer and persistence systems are already well-optimized and won't be the limiting factor.
