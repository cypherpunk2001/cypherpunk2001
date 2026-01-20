# Zone System Architecture

This document describes the multi-zone system implementation.

## Overview

Zones are **world map regions**, not separate servers. A player walking from the town square to the forest crosses a zone boundary and sees a brief "Loading..." overlay while the new area loads. This is purely a client-side optimization to avoid loading the entire world map at once.

A single server process handles players spread across multiple zones simultaneously. Players in different zones don't see each other - they're in different map regions.

## Architecture

### Zone Layout

Zones defined in `data/zones/zone-X.lisp` as chunked tile data. Each zone has: id, dimensions, tile layers, collision data, objects, NPC spawns.

World graph (`data/world-graph.lisp`) defines zone connectivity as a 2x2 grid:
```
zone-1 <-> zone-2
  ^         ^
  v         v
zone-3 <-> zone-4
```

### Core Data Structures

**Zone State (`zone-state` struct):**
```lisp
(defstruct zone-state
  zone-id    ; Symbol like :zone-1
  zone       ; Zone data (tiles, collision)
  wall-map   ; Collision bitmap
  npcs       ; Vector of NPCs in this zone
  objects)   ; Respawning objects
```

**Global Zone Cache:**
```lisp
(defparameter *zone-states* (make-hash-table :test 'eq))
;; Maps zone-id -> zone-state
```

**Per-Player Zone Tracking:**
- Each player struct has `zone-id` slot
- Saved to database with position
- Updated on zone transitions

### How It Works

**Player Experience:**
- Login -> spawn in saved zone at saved position
- Walk to zone edge -> brief "Loading..." -> appear in new zone
- Other players in different zones are not visible
- No zone restrictions or "wrong server" errors

**Server Behavior:**
- Tracks which zone each player is in via `player-zone-id`
- Sends snapshots filtered by zone (players only see same-zone entities)
- Handles zone transitions per-player, not globally
- Simulates NPCs only in zones that have players (CPU optimization)

**Data Flow:**
```
Single Server Process
+-- Zone 1: [Player A, Player B, NPCs...]
+-- Zone 2: [Player C, NPCs...]
+-- Zone 3: [Player D, Player E, Player F, NPCs...]
+-- Zone 4: [NPCs...]  (no players, NPCs idle)

Snapshots:
- Player A receives: Zone 1 state (sees Player B)
- Player C receives: Zone 2 state (alone)
- Player D receives: Zone 3 state (sees E, F)
```

### Zone Transitions

When player crosses zone edge, `transition-zone` in movement.lisp:
1. Caches current zone's NPCs in zone-state
2. Loads target zone (with retry logic)
3. Repositions player at correct entry edge
4. Restores cached NPCs if returning to previously visited zone
5. Shows "Loading..." overlay during transition
6. Saves player position immediately (tier-1) to prevent loss on crash
7. Updates player's zone-id

### Zone-Filtered Snapshots

Snapshots are serialized per-zone for efficiency:
1. `broadcast-snapshots` groups connected clients by zone-id
2. For each zone group, `serialize-game-state-for-zone` filters:
   - Players: only those with matching zone-id
   - NPCs: from zone-state cache
3. Single serialization shared by all clients in same zone

### Per-Zone NPC Simulation

NPCs are simulated independently per zone:
1. `occupied-zone-ids` returns zones that have players
2. `update-sim` iterates each occupied zone
3. `simulate-zone-npcs` runs AI for that zone's NPCs against that zone's players
4. Combat hit detection is per-zone
5. Object respawns are per-zone via `update-zone-objects-respawns`

Empty zones (no players) have idle NPCs - no AI processing.

## Key Files

| File | Purpose |
|------|---------|
| `src/types.lisp` | `zone-state` struct, `player` with `zone-id` slot |
| `src/zone.lisp` | Zone struct, loading, chunked tile I/O |
| `src/world-graph.lisp` | Zone connectivity graph |
| `src/movement.lisp` | Zone transitions, `*zone-states*` cache, NPC caching |
| `src/save.lisp` | Zone-filtered serialization (`serialize-game-state-for-zone`) |
| `src/net.lisp` | Per-zone snapshot broadcasting |
| `src/main.lisp` | Per-zone NPC simulation loop |
| `src/db.lisp` | Session zone tracking, persistence |
| `data/zones/*.lisp` | Zone data files |
| `data/world-graph.lisp` | Zone connection definitions |

## Key Functions

**Zone State Management (movement.lisp):**
- `get-zone-state` - Get cached zone-state by id
- `get-or-create-zone-state` - Get or create zone-state
- `cache-zone-npcs` - Store NPCs in zone-state
- `cached-zone-npcs` - Retrieve NPCs from zone-state
- `players-in-zone` - Filter players by zone-id
- `occupied-zone-ids` - Get zones that have players

**Zone Transitions (movement.lisp):**
- `transition-zone` - Handle player crossing zone boundary
- `update-zone-transition` - Check all players for zone edge crossing

**Snapshots (save.lisp, net.lisp):**
- `serialize-game-state-for-zone` - Zone-filtered state serialization
- `broadcast-snapshots` - Group clients by zone, send filtered snapshots

**NPC Simulation (main.lisp):**
- `simulate-zone-npcs` - Run AI for one zone's NPCs
- Per-zone loop in `update-sim`

## Scaling Strategy

**Current: Single Server, Multi-Zone**
- One server handles all zones
- Players filtered by zone in snapshots
- Target: ~500 concurrent players total

**Future: Horizontal Scaling**
- Multiple server processes, each handling a subset of zones
- Shared database for player state
- Target: ~2,000 players per zone server

**Future: Zone Sharding**
- Dynamic zone instances for overflow
- Load balancing across zone instances
- Target: Unlimited with auto-scaling

## Performance Characteristics

**Memory:**
- Each loaded zone: ~50-200KB depending on size
- 4 zones loaded: <1MB overhead
- NPC state per zone: ~1KB per NPC

**CPU:**
- Snapshot serialization: O(players in zone) not O(total players)
- NPC AI: Only simulates NPCs in zones with players
- Collision: Per-zone wall maps

**Network:**
- Snapshots smaller (zone-filtered)
- No change to protocol
- Delta compression still works

**Database:**
- Zone-id persisted with player
- Session tracking has zone-id
- No special schema for zones

## Current Limitations

1. **Shared Collision Map:** The `world` struct holds a single collision wall map derived
   from the most recently loaded zone. When multiple players are active across different
   zones, only the current `world` zone's collision bounds are used for movement and
   edge detection. Players in other zones may experience inaccurate collision or
   transition detection until their zone is loaded into `world`.

## Implementation History

The multi-zone system was implemented in six phases:

- **Phase 1:** Per-player zone-id tracking - Added zone-id to player struct, serialization
- **Phase 2:** Zone state management - Created zone-state struct and `*zone-states*` cache
- **Phase 3:** Zone-filtered snapshots - Players only see same-zone entities
- **Phase 4:** Independent transitions - Players transition zones without affecting others
- **Phase 5:** Per-zone NPC simulation - NPCs in all occupied zones run AI
- **Phase 6:** Cleanup - Removed legacy `world-zone-npc-cache`, consolidated to zone-states

All phases complete. Tests pass.
