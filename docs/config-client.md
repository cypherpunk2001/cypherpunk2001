# config-client.lisp

Purpose
- Client-only configuration parameters.
- Separated from server config for clarity and to prevent loading unnecessary options.

Why we do it this way
- Clear separation: developers know which options affect client vs server.
- Reduced cognitive load: client developers only see relevant settings.
- All rendering, UI, and visual parameters are isolated here.

How it connects
- Values can be overridden by `data/game-data.lisp` via `load-game-data`.
- New tunables should be added to `*tunable-keys*` in `data.lisp` so they can be data-driven.

---

## Restart Required Parameters

### Window Size
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*window-width*` | `1280` | Window width in pixels |
| `*window-height*` | `720` | Window height in pixels |

*Why restart:* Raylib creates the window once at startup. Changing these affects UI layout calculations immediately, but NOT the actual window dimensions.

### Frame Rate Target
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*client-target-fps*` | `60` | Target FPS for client rendering (0 = unlimited/vsync) |

*Why restart:* Target FPS is set at startup. Changing it at runtime requires calling `raylib:set-target-fps` manually.

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

### Inventory UI Layout
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*inventory-grid-columns*` | `5` | Inventory grid columns |
| `*inventory-slot-gap*` | `8` | Inventory slot gap in pixels |

*Why restart:* UI layout calculated once at startup.

### Editor Setup
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*editor-tileset-paths*` | `nil` | Optional list of tileset sheets for editor |
| `*editor-tileset-root*` | `"../assets/Zelda-like"` | Directory for editor tileset sheets |
| `*editor-export-path*` | `"data/zones/editor-zone.lisp"` | Default export path for editor zones |
| `*editor-tile-layer-id*` | `:floor` | Zone layer ID for tile painting |
| `*editor-collision-layer-id*` | `:walls` | Zone layer ID for collision painting |
| `*editor-object-layer-id*` | `:objects` | Zone layer ID for object painting |

*Why restart:* Layer IDs and tileset catalog parsed at zone load time.

---

## Immediate Effect Parameters

### Debug Flags
| Parameter | Default | Description |
|-----------|---------|-------------|
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

### Rendering
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*tile-point-filter*` | `t` | Use point (nearest-neighbor) filtering for tiles |

### Camera
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*camera-zoom-default*` | `1.0` | Default camera zoom level |
| `*camera-zoom-min*` | `0.5` | Minimum zoom level |
| `*camera-zoom-max*` | `3.0` | Maximum zoom level |
| `*camera-zoom-step*` | `0.1` | Zoom step per mouse wheel tick |

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
