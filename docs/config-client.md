# config-client.lisp

Purpose
- Client-only configuration parameters.
- Separated from server config for clarity and to prevent loading unnecessary options.

Status
- **Implemented.** This doc reflects `src/config-client.lisp` as of 2026-02-03.

Why we do it this way
- Clear separation: developers know which options affect client vs server.
- Reduced cognitive load: client developers only see relevant settings.
- All rendering, UI, and visual parameters are isolated here.

How it connects
- Values can be overridden by `data/game-data.lisp` via `load-game-data` when included in `*tunable-keys*`.

---

## Restart Required Parameters

### Window Size
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*window-width*` | `1280` | Window width in pixels |
| `*window-height*` | `720` | Window height in pixels |
| `*window-resize-enabled*` | `nil` | When T, creates resizable window and uses dynamic screen dimensions |

*Why restart:* Raylib creates the window once at startup.

### Frame Rate Target
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*client-target-fps*` | `60` | Target FPS for client rendering (0 = unlimited/vsync) |

*Note:* Changing at runtime requires `raylib:set-target-fps` manually.

### Asset Paths
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*player-sprite-dir*` | `"../assets/1 Characters/3"` | Player sprite sheets |
| `*npc-sprite-dir*` | `"../assets/3 Dungeon Enemies/1"` | NPC sprite sheets |
| `*blood-sprite-dir*` | `"../assets/1 Characters/Other"` | Blood effect sprites |
| `*tileset-path*` | `"../assets/Zelda-like/Overworld.png"` | Tileset atlas |
| `*soundtrack-dir*` | `"../assets/6 Soundtrack"` | Soundtrack directory |
| `*soundtrack-tracks*` | vector | Soundtrack file paths |
| `*soundtrack-display-names*` | vector | Soundtrack display names |

*Why restart:* Assets are loaded into GPU/audio memory once at startup.

### Sprite Frame Sizes
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*sprite-frame-width*` | `32.0` | Width of a single sprite frame in pixels |
| `*sprite-frame-height*` | `32.0` | Height of a single sprite frame in pixels |
| `*sprite-scale*` | `4.0` | Scale factor applied when drawing sprites |
| `*player-animation-set-id*` | `:player` | Animation set ID for the player |

### Inventory UI Layout
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*inventory-grid-columns*` | `5` | Inventory grid columns |
| `*inventory-slot-gap*` | `8` | Inventory slot gap in pixels |

### Editor Setup
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*editor-tileset-paths*` | `nil` | Optional list of tileset sheets for editor |
| `*editor-tileset-root*` | `"../assets/Zelda-like"` | Directory for editor tileset sheets |
| `*editor-export-path*` | `"data/zones/editor-zone.lisp"` | Default export path |
| `*editor-tile-layer-id*` | `:floor` | Zone layer for tile painting |
| `*editor-collision-layer-id*` | `:walls` | Zone layer for collision painting |
| `*editor-object-layer-id*` | `:objects` | Zone layer for object painting |

### Render Chunk Size
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*render-chunk-size*` | `16` | Tiles per render chunk side (restart required) |

### Client Prediction
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*client-prediction-enabled*` | `t` | Enable local prediction (must be T at startup to create state) |
| `*prediction-error-threshold*` | `5.0` | Pixels of error before snapping to server |

*Note:* Prediction state is created at startup. You can disable it at runtime, but enabling after startup has no effect.

---

## Immediate Effect Parameters

### Debug Flags
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*debug-collision-overlay*` | `nil` | Draw debug collision overlays |
| `*debug-npc-logs*` | `nil` | Enable NPC AI/combat logs and overlays |

### Feature Flags
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*auto-walk-enabled*` | `t` | WASD toggles auto-walk direction |
| `*editor-start-enabled*` | `nil` | Start in editor mode |

### Interpolation / Teleport Handling
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*interpolation-delay-seconds*` | `0.1` | Render delay for interpolation |
| `*teleport-distance-threshold-sq*` | `10000.0` | Squared distance to detect teleports (reset buffers) |
| `*soft-reset-threshold-sq*` | `1024.0` | Squared distance below which prediction/interpolation buffers are preserved on zone transition |

### Rendering
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*tile-point-filter*` | `t` | Nearest-neighbor filtering for tiles |
| `*render-cache-enabled*` | `t` | Enable chunk render cache |
| `*render-cache-max-chunks*` | `64` | Max cached chunk textures before LRU eviction |
| `*debug-render-cache*` | `nil` | Log cache stats and evictions |
| `*entity-render-max-distance*` | `nil` | NPC render distance (pixels); players unaffected |

**Note:** Use `toggle-tile-point-filter` / `toggle-render-cache-enabled` to change filtering/cache at runtime (they clear caches correctly).

### Camera
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*camera-zoom-default*` | `1.0` | Default zoom |
| `*camera-zoom-min*` | `0.5` | Min zoom |
| `*camera-zoom-max*` | `3.0` | Max zoom |
| `*camera-zoom-step*` | `0.1` | Zoom step per wheel tick |

### Click Marker
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*click-marker-duration*` | `0.6` | Marker lifetime (seconds) |
| `*click-marker-size-scale*` | `0.35` | Marker size as fraction of tile |
| `*click-marker-thickness*` | `5` | Marker line thickness |
| `*click-marker-walk-color*` | yellow | Marker color for walk targets |
| `*click-marker-attack-color*` | red | Marker color for attack targets |

### HUD & UI Timing
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*hud-log-line-seconds*` | `30.0` | HUD log line lifetime |
| `*hud-log-fade-seconds*` | `0.4` | HUD log fade duration |
| `*mouse-hold-repeat-seconds*` | `0.25` | Repeat rate for held mouse updates |
| `*chat-max-length*` | `180` | Max chat length |
| `*zone-loading-seconds*` | `0.35` | Loading overlay duration |

### Minimap
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*minimap-width*` | `220` | Minimap width |
| `*minimap-height*` | `220` | Minimap height |
| `*minimap-padding*` | `12` | Screen padding |
| `*minimap-point-size*` | `4` | Marker size |
| `*minimap-preview-edge-tiles*` | `1.5` | Edge distance to show preview spawns |
| `*minimap-npc-view-radius*` | `2000.0` | NPC minimap view radius (pixels) |

### Animation Timing
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*idle-frame-count*` | `4` | Idle frames per row |
| `*walk-frame-count*` | `6` | Walk frames per row |
| `*attack-frame-count*` | `4` | Attack frames per row |
| `*idle-frame-time*` | `0.25` | Idle frame time |
| `*walk-frame-time*` | `0.12` | Walk frame time |
| `*attack-frame-time*` | `0.1` | Attack frame time |
| `*blood-frame-count*` | `4` | Blood frames per row |
| `*blood-frame-time*` | `0.08` | Blood frame time |

### Health Bar
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*health-bar-height*` | `6` | Health bar height |
| `*health-bar-offset*` | `10` | Vertical offset above sprite center |
| `*health-bar-back-color*` | dark gray | Background color |
| `*health-bar-fill-color*` | green | Fill color |
| `*health-bar-border-color*` | light gray | Border color |

### Debug Text
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*debug-npc-text-size*` | `12` | NPC debug text size |
| `*debug-npc-text-offset*` | `18` | NPC debug text offset |
| `*debug-npc-text-color*` | yellow | NPC debug text color |

### Editor Visual Parameters
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*editor-move-speed*` | `360.0` | Editor camera movement speed |
| `*editor-cursor-color*` | cyan | Editor cursor highlight color |
| `*editor-spawn-color*` | orange | Editor spawn marker color |
| `*editor-tileset-preview-padding*` | `12` | Tileset preview padding |
