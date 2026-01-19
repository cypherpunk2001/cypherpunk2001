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
