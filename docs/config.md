# config.lisp

Purpose
- Shared configuration parameters used by both client and server.
- Static CLOS class definitions and raylib key constants.

Why we do it this way
- Game feel is iteration heavy. Keeping tunables in one place makes it cheap
  to tweak speeds, timings, and UI without hunting through systems.
- Static CLOS classes (`character-class`, `npc-archetype`) let us store design
  intent (like combat style and perception) in a structured way.
- Server-specific options are in `config-server.lisp`.
- Client-specific options are in `config-client.lisp`.

How it connects
- Values can be overridden by `data/game-data.lisp` via `load-game-data`.
- New tunables should be added to `*tunable-keys*` in `data.lisp` so they
  can be data-driven.

---

## Related Files

| File | Purpose |
|------|---------|
| `config.lisp` | Shared options + static data (this file) |
| `config-server.lisp` | Server-only options (network, NPC behavior, combat) |
| `config-client.lisp` | Client-only options (window, assets, UI, rendering) |

---

## Restart Required Parameters

### Zone/World Paths
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*starting-zone-id*` | `:zone-1` | Zone where new players spawn. Must exist in world-graph. |
| `*zone-path*` | `nil` | Zone data path relative to repo (nil uses wall map) |
| `*zone-root*` | `"data/zones"` | Directory for zone files |
| `*zone-default-width*` | `64` | Default zone width in tiles for new zones |
| `*zone-default-height*` | `64` | Default zone height in tiles for new zones |
| `*zone-default-chunk-size*` | `8` | Default chunk size in tiles for new zones |
| `*world-graph-path*` | `"data/world-graph.lisp"` | World graph data path relative to repo |
| `*known-zone-ids*` | `nil` | Set of zone IDs from `load-world-graph` for validation (nil skips validation) |
| `*save-filepath*` | `"data/savegame.lisp"` | Default save file path used by `save-game`/`load-game` helpers (not wired to client menu) |

*Why restart:* World graph and initial zone loaded once at startup.

### Tileset/Map Layout
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*tile-size*` | `16` | Source tile size in the atlas, in pixels |
| `*tile-scale*` | `4.0` | Scale factor for drawing tiles to the screen |
| `*tileset-columns*` | `40` | Number of columns in the atlas grid |
| `*floor-tile-index*` | `0` | Atlas tile index for floor fill (0 disables fill) |
| `*wall-map-width*` | `40` | Width of the test wall map in tiles |
| `*wall-map-height*` | `24` | Height of the test wall map in tiles |
| `*wall-origin-x*` | `0` | World tile X where the wall map starts |
| `*wall-origin-y*` | `0` | World tile Y where the wall map starts |
| `*wall-tile-indices*` | `#(107)` | Wall tile variants |
| `*wall-seed*` | `2468` | Seed for wall tile variation |

*Why restart:* Zone files and tileset atlases are parsed using these values at load time.

---

## Immediate Effect Parameters

### Debug Flags
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*verbose*` | `nil` | General verbose mode: logs network events, state changes, diagnostics |
| `*verbose-coordinates*` | `nil` | Logs entity positions and collider info per frame (very noisy) |
| `*verbose-logs*` | `nil` | DEPRECATED: Use `*verbose-coordinates*` instead |

### Minimap Multi-Hop Pathing
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*minimap-resolve-max-hops*` | `8` | Maximum zone hops when walking click coordinates through zone boundaries. Should be >= world diameter for full minimap coverage. |
| `*bfs-max-hops*` | `32` | Maximum BFS depth when searching for shortest path between zones. Safety bound; should be >= number of zones in world graph. |

### Player Movement
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*player-speed*` | `222.0` | Base movement speed in pixels per second |
| `*run-speed-mult*` | `2.0` | Movement speed multiplier while running |
| `*run-stamina-max*` | `10.0` | Seconds of run stamina when full |
| `*target-epsilon*` | `6.0` | Stop distance for click-to-move |

### Collision
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*player-collision-scale*` | `2.0` | Collision box size relative to one tile |
| `*collision-edge-epsilon*` | `0.01` | Avoid counting exact edge contact as a blocked tile |

### Camera Leash
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*camera-leash-enabled*` | `t` | When T, player can move within a radius before camera follows. Reduces jitter for small movements. Feature flag for quick disable. |
| `*camera-leash-radius-tiles*` | `2.0` | Radius in tiles that the player can move before the camera follows. Diameter ~4 tiles. Larger = more freedom before camera shifts. |

*How it works:* The camera maintains a "leash target" position. When the player moves within the leash radius, the camera stays put. When the player exceeds the radius, the camera follows just enough to keep the player on the boundary circle. This creates a subtle dead zone that reduces camera jitter during small movements while maintaining smooth following for larger movements.

---

## Static Data (Evaluated at Load Time)

### CLOS Class Definitions

**character-class** - Static player class data:
- `name` - Class display name
- `max-hp` - Maximum hitpoints for this class

**npc-archetype** - Static NPC archetype data:
- `name`, `description` - Display info
- `max-hits` - Durability
- `attack-level`, `strength-level`, `defense-level`, `hitpoints-level` - Combat stats
- `combat-xp` - XP awarded on defeat
- `loot-table-id` - Loot table reference
- `move-speed`, `attack-range-tiles`, `attack-cooldown`, `attack-damage` - Combat behavior
- `home-radius-tiles`, `wander-interval` - Roaming behavior
- `respawn-seconds` - Respawn timer
- `flee-speed-mult`, `flee-at-hits` - Flee behavior
- `animation-set-id` - Sprite set reference
- `aggro-mode` - `:never`, `:on-sight`, etc.
- `retaliate` - Whether NPC fights back when attacked
- `perception-tiles` - Aggro detection range

### Raylib Key Constants
Constants for input handling: `+key-right+`, `+key-left+`, `+key-down+`, `+key-up+`, `+key-escape+`, `+key-d+`, `+key-a+`, `+key-s+`, `+key-w+`, `+key-tab+`, `+key-space+`, `+key-q+`, `+key-e+`, `+key-t+`, `+key-z+`, `+key-i+`, `+key-x+`, `+key-one+` through `+key-four+`, `+key-f5+` through `+key-f12+`, `+key-left-shift+`, `+key-right-shift+`, `+key-enter+`, `+key-backspace+`, `+mouse-left+`, `+mouse-right+`, `+mouse-middle+`.

---

## Walkthroughs

### Change movement speed
1. Edit `data/game-data.lisp` and set `:player-speed`.
2. `load-game-data` applies the tunable at startup.
3. Movement uses the updated value the next frame.

### Adjust wall-map bounds
1. Set `:wall-map-width` and `:wall-map-height` in `data/game-data.lisp`.
2. `make-world` rebuilds the wall map at startup.
3. Movement clamps to the new bounds.

### Load a zone file
1. Set `:zone-path` in `data/game-data.lisp`.
2. `load-zone` builds collision tiles from chunked layers.
3. The world uses the zone's collision data for blocking tiles.

### Configure the world graph
1. Set `:world-graph-path` in `data/game-data.lisp`.
2. `load-world-graph` reads edge links for zone transitions.
3. Movement consults the graph when the player exits a zone edge.

---

## Examples

### Tuning player speed
```lisp
;; In data/game-data.lisp
(:tunables
 (:player-speed 260.0))
```

### Adding a new archetype
```lisp
;; In data/game-data.lisp
(:npc-archetypes
 (:slime
  (:name "Slime"
   :max-hits 2
   :move-speed 80.0
   :attack-range-tiles 0.6
   :attack-cooldown 1.2
   :attack-damage 1
   :home-radius-tiles 1.5
   :wander-interval 1.3
   :respawn-seconds 5.0
   :animation-set-id :npc-slime)))
```

---

## Compact Serialization (Network Optimization)

These parameters control the compact snapshot format used for network transmission. See `docs/net.md` "4-Prong Approach" for details.

### Enum Mappings
| Parameter | Description |
|-----------|-------------|
| `*anim-state-to-code*` | Animation state keyword → integer code |
| `*code-to-anim-state*` | Integer code → animation state keyword |
| `*facing-to-code*` | Facing direction keyword → integer code |
| `*code-to-facing*` | Integer code → facing direction keyword |

### Quantization Parameters
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*coord-scale*` | `10` | Scale factor for coordinate quantization (10 = 0.1 pixel precision) |
| `*timer-scale*` | `100` | Scale factor for timer quantization (100 = 0.01 second precision) |

These parameters reduce snapshot size from ~232 bytes/player to ~64 bytes/player, enabling 3-4x more players per UDP packet.
