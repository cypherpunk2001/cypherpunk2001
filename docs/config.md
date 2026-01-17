# config.lisp

Purpose
- Central home for tunable parameters and static class definitions.

Why we do it this way
- Game feel is iteration heavy. Keeping tunables in one place makes it cheap
  to tweak speeds, timings, and UI without hunting through systems.
- Static CLOS classes (`character-class`, `npc-archetype`) let us store design
  intent (like combat style and perception) in a structured way.

How it connects
- Values can be overridden by `data/game-data.lisp` via `load-game-data`.
- New tunables should be added to `*tunable-keys*` in `data.lisp` so they
  can be data-driven.

---

## Parameter Categories

Parameters are grouped by when changes take effect:

**Immediate Effect (via SLIME):** Read every frame/tick. Change with `(setf *param* value)` and see results immediately.

**Restart Required:** Read once at initialization. Changing via SLIME won't take effect until client/server restart.

**Static Data:** Evaluated at load time. Effectively constants after the file loads.

---

## Immediate Effect Parameters

### Debug Flags
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*verbose*` | `nil` | General verbose mode: logs network events, state changes, diagnostics |
| `*verbose-coordinates*` | `nil` | Logs entity positions and collider info per frame (very noisy) |
| `*verbose-logs*` | `nil` | DEPRECATED: Use `*verbose-coordinates*` instead |
| `*debug-collision-overlay*` | `nil` | Draws debug grid and collision overlays |
| `*debug-npc-logs*` | `nil` | Logs NPC AI/combat events and enables AI debug text overlay |

### Feature Flags
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*auto-walk-enabled*` | `t` | When true, WASD toggles auto-walk direction |
| `*editor-start-enabled*` | `nil` | When true, editor mode starts enabled |

### Interpolation & Prediction
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*interpolation-delay-seconds*` | `0.1` | Render delay for interpolation. Higher = smoother, more perceived lag |
| `*client-prediction-enabled*` | `nil` | Enable client-side prediction for local player. Toggle via SLIME for testing |
| `*prediction-error-threshold*` | `5.0` | Max prediction error in pixels before correction |

### Simulation Timing
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*sim-tick-seconds*` | `1/60` | Fixed simulation tick length in seconds |
| `*sim-max-steps-per-frame*` | `5` | Max sim ticks per frame to avoid spiral of death |

### Player Movement
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*player-speed*` | `222.0` | Base movement speed in pixels per second |
| `*run-speed-mult*` | `2.0` | Movement speed multiplier while running |
| `*run-stamina-max*` | `10.0` | Seconds of run stamina when full |
| `*target-epsilon*` | `6.0` | Stop distance for click-to-move |

### Camera
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*camera-zoom-default*` | `1.0` | Default camera zoom level |
| `*camera-zoom-min*` | `0.5` | Minimum zoom level |
| `*camera-zoom-max*` | `3.0` | Maximum zoom level |
| `*camera-zoom-step*` | `0.1` | Zoom step per mouse wheel tick |

### Combat & XP
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*stat-xp-per-level*` | `100` | XP needed for each quadratic level step |
| `*stat-max-level*` | `99` | Maximum attainable level per stat |
| `*xp-per-damage*` | `4` | XP awarded per point of damage dealt |
| `*combat-hitpoints-xp-multiplier*` | `0.33` | HP XP multiplier applied to focused combat XP |
| `*attack-hitbox-scale*` | `1.0` | Attack hitbox size relative to one tile |

### NPC Behavior
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*npc-max-hits*` | `3` | Hits required to defeat the NPC |
| `*npc-walk-speed*` | `120.0` | Base NPC movement speed in pixels per second |
| `*npc-flee-speed-mult*` | `1.4` | Speed multiplier while fleeing |
| `*npc-attack-range-tiles*` | `0.9` | NPC melee range in tiles |
| `*npc-attack-cooldown*` | `0.9` | Seconds between NPC attacks |
| `*npc-attack-damage*` | `1` | Damage per NPC hit |
| `*npc-home-radius-tiles*` | `2.0` | Roam radius around spawn in tiles |
| `*npc-wander-interval*` | `1.1` | Seconds between wander target changes |
| `*npc-wander-arrive-distance*` | `6.0` | Pixels to consider wander target reached |
| `*npc-respawn-seconds*` | `5.0` | Default respawn cooldown in seconds |
| `*npc-default-archetype-id*` | `:rat` | Default NPC archetype ID to spawn |
| `*npc-default-loot-table-id*` | `nil` | Fallback loot table when NPC archetypes omit one |
| `*npc-collision-scale*` | `2.0` | Collision box size relative to one tile |

### Collision
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*player-collision-scale*` | `2.0` | Collision box size relative to one tile |
| `*collision-edge-epsilon*` | `0.01` | Avoid counting exact edge contact as a blocked tile |

### Click Marker
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*click-marker-duration*` | `0.6` | Seconds a click marker stays visible |
| `*click-marker-size-scale*` | `0.35` | Marker size as a fraction of a tile |
| `*click-marker-thickness*` | `5` | Marker line thickness in pixels |
| `*click-marker-walk-color*` | yellow | Marker color for walk targets |
| `*click-marker-attack-color*` | red | Marker color for attack targets |

### HUD & UI Timing
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*hud-log-line-seconds*` | `30.0` | Seconds a HUD log line stays visible |
| `*hud-log-fade-seconds*` | `0.4` | Seconds to fade out HUD log lines |
| `*mouse-hold-repeat-seconds*` | `0.25` | Repeat rate for mouse-held updates |
| `*chat-max-length*` | `180` | Maximum characters in a chat message |
| `*zone-loading-seconds*` | `0.35` | Seconds to show the zone loading overlay after transitions |

### Minimap
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*minimap-width*` | `220` | Minimap width in pixels |
| `*minimap-height*` | `220` | Minimap height in pixels |
| `*minimap-padding*` | `12` | Padding from screen edges for minimap placement |
| `*minimap-point-size*` | `4` | Size of player/NPC markers on the minimap |
| `*minimap-preview-edge-tiles*` | `1.5` | Tiles from an exit edge to show adjacent zone spawn previews |

### Minimap Colors
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*minimap-bg-color*` | dark blue | Minimap background color |
| `*minimap-border-color*` | light gray | Minimap border color |
| `*minimap-player-color*` | cyan | Minimap player marker color |
| `*minimap-npc-color*` | orange | Minimap NPC marker color |
| `*minimap-collision-color*` | gray | Minimap collision marker color |

### Animation Timing
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*idle-frame-count*` | `4` | Frames in each idle animation row |
| `*walk-frame-count*` | `6` | Frames in each walk animation row |
| `*attack-frame-count*` | `4` | Frames in each attack animation row |
| `*idle-frame-time*` | `0.25` | Seconds per idle frame |
| `*walk-frame-time*` | `0.12` | Seconds per walk frame |
| `*attack-frame-time*` | `0.1` | Seconds per attack frame |
| `*blood-frame-count*` | `4` | Frames in each blood animation row |
| `*blood-frame-time*` | `0.08` | Seconds per blood frame |

### Health Bar
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*health-bar-height*` | `6` | Height of the health bar in pixels |
| `*health-bar-offset*` | `10` | Vertical offset above the sprite center |
| `*health-bar-back-color*` | dark gray | Health bar background color |
| `*health-bar-fill-color*` | green | Health bar fill color |
| `*health-bar-border-color*` | light gray | Health bar outline color |

### Debug Text
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*debug-npc-text-size*` | `12` | Debug text size for NPC AI overlay |
| `*debug-npc-text-offset*` | `18` | Extra vertical offset for NPC debug text |
| `*debug-npc-text-color*` | yellow | NPC AI debug text color |

### Editor Visual Parameters
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*editor-move-speed*` | `360.0` | Movement speed for editor camera |
| `*editor-cursor-color*` | cyan | Editor cursor highlight color |
| `*editor-spawn-color*` | orange | Editor spawn marker color |
| `*editor-tileset-preview-padding*` | `12` | Padding for the tileset preview panel |
| `*editor-tileset-preview-max-width*` | `480` | Max width for the tileset preview panel |
| `*editor-tileset-preview-max-height*` | `360` | Max height for the tileset preview panel |
| `*editor-tileset-preview-bg-color*` | dark | Tileset preview background color |
| `*editor-tileset-preview-border-color*` | gray | Tileset preview border color |
| `*editor-tileset-preview-highlight-color*` | yellow | Tileset preview selection color |

### Music Volume
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*music-volume-steps*` | `10` | Number of volume steps for music controls |
| `*music-default-volume-level*` | `1` | Default music volume step (0 mutes) |

---

## Restart Required Parameters

### Window Size
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*window-width*` | `1280` | Window width in pixels |
| `*window-height*` | `720` | Window height in pixels |

*Why restart:* Raylib creates the window once at startup. Changing these affects UI layout calculations immediately, but NOT the actual window dimensions.

### Network
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*net-default-host*` | `"127.0.0.1"` | Default UDP host for client/server |
| `*net-default-port*` | `1337` | Default UDP port for client/server |
| `*net-buffer-size*` | `65507` | Max UDP payload size for snapshot messages |

*Why restart:* UDP receive buffer created once per socket. Changing this doesn't resize existing buffers.

### Auth Encryption
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*auth-encryption-enabled*` | `nil` | Enable auth payload encryption (X25519 + AES-256-GCM) |
| `*server-auth-public-key*` | `nil` | Server's X25519 public key as hex string |

*Why restart:* Client must know server's public key to encrypt credentials. Server generates this at startup.

### Inventory & Equipment
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*inventory-size*` | `20` | Player inventory slots |
| `*inventory-grid-columns*` | `5` | Inventory grid columns |
| `*inventory-slot-gap*` | `8` | Inventory slot gap in pixels |
| `*equipment-slot-ids*` | `#(:head :body :legs :weapon :offhand :accessory)` | Equipment slot order |

*Why restart:* Inventory/equipment arrays allocated with fixed size when player struct is created.

### New Player Stats
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*player-base-attack*` | `1` | Base attack level for new players |
| `*player-base-strength*` | `1` | Base strength level for new players |
| `*player-base-defense*` | `1` | Base defense level for new players |
| `*player-base-hitpoints*` | `10` | Base hitpoints level for new players |
| `*player-training-mode*` | `:balanced` | Training focus (:attack/:strength/:defense/:hitpoints/:balanced) |

*Why restart:* These set initial values for newly created players. Existing players retain their stats.

### Asset Paths
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*player-sprite-dir*` | `"../assets/1 Characters/3"` | Directory for player sprite sheets |
| `*npc-sprite-dir*` | `"../assets/3 Dungeon Enemies/1"` | Directory for NPC sprite sheets |
| `*blood-sprite-dir*` | `"../assets/1 Characters/Other"` | Directory for blood effect sprites |
| `*tileset-path*` | `"../assets/Zelda-like/Overworld.png"` | Atlas image for floor tiles |
| `*soundtrack-dir*` | `"../assets/6 Soundtrack"` | Directory for soundtrack files |
| `*soundtrack-tracks*` | (vector of paths) | Vector of soundtrack file paths |
| `*soundtrack-display-names*` | (vector of names) | Vector of display names for soundtrack |

*Why restart:* Assets are loaded into GPU/audio memory once at startup.

### Sprite Frame Sizes
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*sprite-frame-width*` | `32.0` | Width of a single sprite frame in pixels |
| `*sprite-frame-height*` | `32.0` | Height of a single sprite frame in pixels |
| `*sprite-scale*` | `4.0` | Scale factor applied when drawing sprites |
| `*player-animation-set-id*` | `:player` | Animation set ID for the player sprite set |

*Why restart:* Frame dimensions define how sprites are sliced from sheets at load time.

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

### Zone/World Paths
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*zone-path*` | `nil` | Zone data path relative to repo (nil uses wall map) |
| `*zone-root*` | `"data/zones"` | Directory for zone files |
| `*zone-default-width*` | `64` | Default zone width in tiles for new zones |
| `*zone-default-height*` | `64` | Default zone height in tiles for new zones |
| `*zone-default-chunk-size*` | `8` | Default chunk size in tiles for new zones |
| `*world-graph-path*` | `"data/world-graph.lisp"` | World graph data path relative to repo |
| `*save-filepath*` | `"data/savegame.lisp"` | Default save file path for ESC menu Save/Load |

*Why restart:* World graph and initial zone loaded once at startup.

### Editor Config
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*editor-tileset-paths*` | `nil` | Optional list of tileset sheets for editor |
| `*editor-tileset-root*` | `"../assets/Zelda-like"` | Directory for editor tileset sheets |
| `*editor-export-path*` | `"data/zones/editor-zone.lisp"` | Default export path for editor zones |
| `*editor-tile-layer-id*` | `:floor` | Zone layer ID for tile painting |
| `*editor-collision-layer-id*` | `:walls` | Zone layer ID for collision painting |
| `*editor-object-layer-id*` | `:objects` | Zone layer ID for object painting |

*Why restart:* Layer IDs parsed at zone load time.

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

### Tweak zone loading overlay
1. Set `:zone-loading-seconds` in `data/game-data.lisp`.
2. UI uses the value to display a brief "Loading..." overlay on zone swaps.

### Adjust the minimap size
1. Set `:minimap-width`, `:minimap-height`, `:minimap-padding`, and
   `:minimap-preview-edge-tiles` in `data/game-data.lisp`.
2. UI repositions the minimap each boot using the new values.

### Configure default zone sizing
1. Set `:zone-default-width`, `:zone-default-height`, and `:zone-default-chunk-size`.
2. New zones created in the editor use these values.
3. Resize hotkeys step by the active zone's chunk size.

### Customize editor export
1. Set `:editor-export-path` in `data/game-data.lisp`.
2. Editor Mode writes zone files to the new location on export.

### Change zone root
1. Set `:zone-root` in `data/game-data.lisp`.
2. Editor zone create/delete/list operations use the new folder.

### Configure editor tileset sheets
1. Set `:editor-tileset-paths` (explicit list) or `:editor-tileset-root` in `data/game-data.lisp`.
2. Editor Mode builds the tileset catalog at startup and `Q/E` cycles sheets.

### Start in editor mode
1. Set `:editor-start-enabled` in `data/game-data.lisp`.
2. The game boots with editor mode enabled for quick iteration.

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
