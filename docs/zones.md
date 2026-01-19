# Zone System Architecture

This document describes the zone system: where we are, where we want to be, and the path forward.

## Overview

Zones are **world map regions**, not separate servers. A player walking from the town square to the forest crosses a zone boundary and sees a brief "Loading..." overlay while the new area loads. This is purely a client-side optimization to avoid loading the entire world map at once.

## Current State ("We Are Here")

### What Works

**Zone Definition and Data:**
- Zones defined in `data/zones/zone-X.lisp` as chunked tile data
- Each zone has: id, dimensions, tile layers, collision data, objects, NPC spawns
- World graph (`data/world-graph.lisp`) defines zone connectivity as a 2x2 grid:
  ```
  zone-1 ←→ zone-2
    ↕         ↕
  zone-3 ←→ zone-4
  ```

**Zone Transitions:**
- Player position checked against world bounds each tick
- When crossing edge, `transition-zone` in movement.lisp:
  1. Caches current zone's NPCs
  2. Loads target zone (with retry logic)
  3. Repositions player at correct entry edge
  4. Restores cached NPCs if returning to previously visited zone
  5. Shows "Loading..." overlay during transition
  6. Saves player position immediately (tier-1) to prevent loss on crash

**Persistence:**
- Player's zone-id saved to database with position
- On login, player spawns in their saved zone at saved position
- Zone transitions trigger immediate save

**Preview System:**
- Adjacent zones lazy-loaded for minimap edge previews
- Cached in `world-zone-preview-cache` to avoid repeated loads

### What Doesn't Work

**Single-Zone Server Architecture:**
The server currently runs ONE zone at a time in a shared `game-world`. This creates problems:

1. **Only first player can transition** - Other players are locked in current zone
2. **Zone change affects everyone** - If player A crosses to zone-2, the server loads zone-2 for ALL connected players
3. **No true multi-zone support** - All players in `game-players` array are assumed to be in the same zone

**The Login Bug (Now Fixed):**
There was code rejecting logins if player's saved zone didn't match server's "current zone". This was designed for a multi-server architecture (one server per zone) that we're not using. Removed in this session.

### Key Files

| File | Purpose |
|------|---------|
| `src/zone.lisp` | Zone struct, loading, chunked tile I/O |
| `src/world-graph.lisp` | Zone connectivity graph |
| `src/movement.lisp` | Zone transitions, NPC caching, player repositioning |
| `src/db.lisp` | Session zone tracking, persistence |
| `src/save.lisp` | Player serialization with zone-id |
| `data/zones/*.lisp` | Zone data files |
| `data/world-graph.lisp` | Zone connection definitions |

## Intended Design ("We Want To Get Here")

### Core Concept

Zones are **map regions within a single server**, not separate servers. A single server process should handle players spread across multiple zones simultaneously.

**Player Experience:**
- Login → spawn in saved zone at saved position
- Walk to zone edge → brief "Loading..." → appear in new zone
- Other players in different zones are simply not visible (different map region)
- No "wrong server" errors, no zone restrictions

**Server Behavior:**
- Track which zone each player is in
- Send snapshots containing only players/NPCs in same zone as recipient
- Handle zone transitions per-player, not globally
- Support hundreds of players spread across all zones

### Target Architecture

```
Single Server Process
├── Zone 1: [Player A, Player B, NPCs...]
├── Zone 2: [Player C, NPCs...]
├── Zone 3: [Player D, Player E, Player F, NPCs...]
└── Zone 4: [NPCs...]

Snapshots:
- Player A receives: Zone 1 state (sees Player B)
- Player C receives: Zone 2 state (alone)
- Player D receives: Zone 3 state (sees E, F)
```

### Scaling Strategy

For massive player counts (goal: 8,000 players, 2,000 per zone):

**Phase 1: Single Server, Multi-Zone (Current Target)**
- One server handles all zones
- Players filtered by zone in snapshots
- Target: ~500 concurrent players total

**Phase 2: Horizontal Scaling**
- Multiple server processes, each handling a subset of zones
- Shared database for player state
- Target: ~2,000 players per zone server

**Phase 3: Zone Sharding (Future)**
- Dynamic zone instances for overflow
- Load balancing across zone instances
- Target: Unlimited with auto-scaling

## Path Forward

### Step 1: Per-Player Zone Tracking (Required)

**Current:** `game-world` has one zone, all players assumed to be in it.

**Change:** Track zone-id per player in the player struct or session.

```lisp
;; Option A: Add to player struct
(defstruct player
  ...
  (zone-id nil)  ; Which zone this player is in
  ...)

;; Option B: Use existing session tracking (already has zone-id)
;; Just need to use it for filtering
```

### Step 2: Zone-Filtered Snapshots (Required)

**Current:** `serialize-game-state-compact` includes all players.

**Change:** Filter players/NPCs by recipient's zone before serialization.

```lisp
;; Pseudocode
(defun serialize-snapshot-for-player (game player)
  (let* ((player-zone (player-zone-id player))
         (same-zone-players (remove-if-not
                              (lambda (p) (eq (player-zone-id p) player-zone))
                              (game-players game)))
         (zone-npcs (get-npcs-for-zone player-zone)))
    (serialize-state same-zone-players zone-npcs ...)))
```

### Step 3: Per-Player Zone State (Required)

**Current:** One `world-zone` shared by all.

**Change:** Load zones on-demand, cache loaded zones, each player references their zone.

```lisp
;; Zone cache
(defparameter *loaded-zones* (make-hash-table))

(defun get-or-load-zone (zone-id)
  (or (gethash zone-id *loaded-zones*)
      (setf (gethash zone-id *loaded-zones*)
            (load-zone (zone-path zone-id)))))
```

### Step 4: Independent Zone Transitions (Required)

**Current:** `transition-zone` modifies shared `game-world`.

**Change:** Transition only affects the transitioning player.

```lisp
;; Pseudocode
(defun transition-player-zone (player from-zone to-zone entry-edge)
  ;; Update player's zone-id
  (setf (player-zone-id player) to-zone)
  ;; Reposition at entry edge
  (reposition-player-at-edge player to-zone entry-edge)
  ;; Save immediately
  (db-save-player-immediate player)
  ;; Update session
  (update-player-session-zone (player-id player) to-zone))
```

### Step 5: NPC Per-Zone Management (Enhancement)

**Current:** NPCs cached when leaving zone, restored on return.

**Change:** NPCs exist independently per zone, always running.

```lisp
;; Each zone has its own NPC array
(defstruct zone-state
  (zone nil)           ; Zone data
  (npcs (vector))      ; NPCs in this zone
  (objects nil))       ; Respawning objects

(defparameter *zone-states* (make-hash-table))
```

### Implementation Priority

| Priority | Task | Complexity | Impact |
|----------|------|------------|--------|
| 1 | Per-player zone-id tracking | Low | Foundation for everything |
| 2 | Zone-filtered snapshots | Medium | Enables multi-zone play |
| 3 | Zone cache (load on demand) | Medium | Memory efficiency |
| 4 | Independent transitions | Medium | Removes "first player" lock |
| 5 | Per-zone NPC simulation | High | Full zone independence |

### Considerations

**Memory:**
- Each loaded zone: ~50-200KB depending on size
- 4 zones loaded: <1MB overhead
- NPC state per zone: ~1KB per NPC
- Acceptable for single server

**CPU:**
- Snapshot serialization: O(players in zone) not O(total players)
- NPC AI: Only simulate NPCs in zones with players
- Collision: Per-zone wall maps already separate

**Network:**
- Snapshots smaller (zone-filtered)
- No change to protocol
- Delta compression still works

**Database:**
- No schema changes needed
- Zone-id already persisted
- Session tracking already has zone-id

## Summary

| Aspect | Current | Target |
|--------|---------|--------|
| Zones per server | 1 | All (4+) |
| Players per zone | All share one | Filtered by zone |
| Zone transitions | Global (affects all) | Per-player |
| NPC simulation | Current zone only | All occupied zones |
| Login restriction | None (fixed) | None |
| Scaling model | One server = one zone | One server = all zones |

The path from current to target is incremental. Each step builds on the previous and can be tested independently. The core insight is: **zones are data partitions, not server partitions**.

---

## Implementation Plan (Detailed)

This section contains the concrete implementation steps, organized into phases that can each be tested independently.

### Phase 1: Per-Player Zone Tracking

**Goal:** Each player knows which zone they're in. No behavior change yet.

**Files to modify:**
- `src/types.lisp` - Add `zone-id` slot to player struct
- `src/save.lisp` - Serialize/deserialize player zone-id
- `src/movement.lisp` - Set player zone-id on spawn and transition
- `src/db.lisp` - Load player with zone-id from database

**Changes:**

1. Add `zone-id` to player struct (types.lisp)
2. Update `serialize-player` to include zone-id in base payload (save.lisp)
3. Update `deserialize-player` to restore zone-id (save.lisp)
4. Set `player-zone-id` when spawning new player (movement.lisp)
5. Set `player-zone-id` when loading existing player (db.lisp)
6. Update `player-zone-id` on zone transition (movement.lisp)

**Test:** Players have correct zone-id after spawn, load, and transition. No functional change.

### Phase 2: Zone State Management

**Goal:** Decouple zone data from single `world` struct. Multiple zones can be loaded.

**Files to modify:**
- `src/types.lisp` - Add `zone-state` struct
- `src/movement.lisp` - Create zone-state cache, refactor world to use it

**Changes:**

1. Create `zone-state` struct with: zone, npcs, wall-map, objects
2. Create `*zone-states*` hash table (zone-id -> zone-state)
3. Create `get-or-create-zone-state` function
4. Migrate `world-zone`, `world-npcs` to use zone-state for current zone
5. Keep `world` as player's "view" - references a zone-state

**Test:** Server works exactly as before, but zone data is in zone-state structs.

### Phase 3: Zone-Filtered Snapshots

**Goal:** Players only see other players/NPCs in their zone.

**Files to modify:**
- `src/save.lisp` - Add zone-filtered serialization
- `src/net.lisp` - Use per-player snapshot serialization

**Changes:**

1. Create `serialize-game-state-for-zone` that filters by zone-id
2. Modify snapshot sending to call per-player serialization
3. Filter `game-players` to only same-zone players
4. Get NPCs from player's zone-state

**Test:** Player A in zone-1 doesn't see Player B in zone-2. Each sees correct NPCs.

### Phase 4: Independent Zone Transitions

**Goal:** Players transition zones independently without affecting others.

**Files to modify:**
- `src/movement.lisp` - Refactor `transition-zone` for per-player

**Changes:**

1. Remove "first player only" restriction
2. `transition-zone` updates only the transitioning player's zone-id
3. Load target zone-state if not cached
4. Reposition player in target zone
5. Don't modify `world-zone` globally (or make it per-player concept)
6. Each player's collision checks use their zone-state's wall-map

**Test:** Player A can walk to zone-2 while Player B stays in zone-1. Both play normally.

### Phase 5: Per-Zone NPC Simulation

**Goal:** NPCs in all occupied zones run AI and can be fought.

**Files to modify:**
- `src/server.lisp` - Update sim to process all zone-states
- `src/ai.lisp` - No changes needed (already per-NPC)
- `src/combat.lisp` - Ensure combat uses correct zone context

**Changes:**

1. `update-sim` iterates all zone-states with players
2. Run NPC AI for each occupied zone
3. Combat lookups use player's zone-state NPCs
4. Respawn objects per zone-state

**Test:** Fight NPCs in zone-2 while another player fights in zone-1. Both work.

### Phase 6: Cleanup and Optimization

**Goal:** Remove legacy single-zone assumptions, optimize.

**Changes:**

1. Remove `world-zone-npc-cache` (NPCs now persistent in zone-states)
2. Simplify zone preview cache (reuse zone-states)
3. Only simulate NPCs in zones with players (CPU optimization)
4. Add zone-state unloading for empty zones (memory optimization, optional)

---

## Implementation Checklist

- [x] **Phase 1:** Per-player zone-id tracking
  - [x] Add zone-id to player struct (types.lisp)
  - [x] Serialize/deserialize zone-id (save.lisp)
  - [x] Set zone-id on spawn (server.lisp: spawn-player-at-world)
  - [x] Set zone-id on load (save.lisp: deserialize-player)
  - [x] Update zone-id on transition (movement.lisp: transition-zone)
  - [x] Tests pass

- [x] **Phase 2:** Zone state management
  - [x] Create zone-state struct (types.lisp)
  - [x] Create *zone-states* cache (movement.lisp)
  - [x] Helper functions: get-zone-state, get-or-create-zone-state
  - [x] Initial zone registered in cache on world creation
  - [x] Tests pass

- [x] **Phase 3:** Zone-filtered snapshots
  - [x] Filter players by zone (save.lisp: serialize-game-state-for-zone)
  - [x] Filter NPCs by zone (using zone-state)
  - [x] Group clients by zone for efficient broadcasting (net.lisp)
  - [x] Per-zone snapshot serialization
  - [x] Tests pass

- [x] **Phase 4:** Independent transitions
  - [x] Remove first-player restriction (movement.lisp: update-zone-transition)
  - [x] All players checked for zone edge transitions
  - [x] Zone-state cache updated on transition
  - [x] NPCs stored in zone-state for filtered snapshots
  - [x] Tests pass
  - [ ] Per-player collision detection (deferred - uses shared world boundaries)

- [x] **Phase 5:** Per-zone NPC simulation
  - [x] Simulate all occupied zones (main.lisp: per-zone NPC AI loop)
  - [x] Combat works across zones (melee hit detection per zone)
  - [x] Object respawn per zone (update-zone-objects-respawns)
  - [x] NPC respawn per zone
  - [x] Tests pass

- [ ] **Phase 6:** Cleanup
  - [ ] Remove legacy caches (world-zone-npc-cache)
  - [ ] Optimize empty zone handling
  - [ ] Final testing

### Current Limitations

After Phase 5 implementation, the following limitations remain:

1. **Shared Collision Map:** All players use the world's current zone for collision detection.
   Players who transition to a different zone may have imperfect collision until the world
   zone switches to match theirs.

2. **Zone Edge Detection:** Players can only trigger zone transitions when they're in the
   same zone as the world's current zone. This prevents stranded players in other zones
   from transitioning further until someone else triggers a world zone change.

3. **Legacy NPC Cache:** The `world-zone-npc-cache` is still used alongside zone-states.
   Phase 6 will consolidate these caches.
