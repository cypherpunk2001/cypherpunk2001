# Graphics Upgrade Plan

## Current State

The game uses a grab bag of free assets from OpenGameArt with no visual cohesion:

- **Tiles**: 16x16 pixel atlas (`Zelda-like/Overworld.png`), scaled 4x to 64x64 on screen
- **Sprites**: 32x32 pixel frames (Archer, Warrior, Wizard, Rat, Goblin, Orc, Witch Doctor), scaled 4x to 128x128
- **Perspective**: Top-down 2D with raylib 2D camera
- **Window**: 1280x720, not fullscreen
- **Zone building**: Interactive tile editor, manual tile-by-tile placement
- **Pain point**: ~30 minutes of editor work produces ~13 seconds of walking content. Art is functional but visually incoherent.

## Target

Hyper Light Drifter-style gameplay reimagining of Cyberpunk 2077 as a top-down MMO. Same atmospheric pixel art approach as HLD but at 32x32 (double HLD's 16x16) for higher definition. Dark, neon-lit, gritty cyberpunk world. Fullscreen support from 1080p to 4K.

RuneScape Classic is an inspiration for **game loops only** (PvP/PvE combat, skilling systems) - not for art style or graphics.

---

## Locked Specs

### Perspective: Top-Down (LOCKED)

We evaluated isometric and decided against it:

- **PixelLab's isometric tooling is bare-bones** - `create_isometric_tile` makes one tile at a time with no Wang tileset transitions. Top-down has full seamless tileset generation.
- **Isometric requires significant engine surgery** - coordinate system overhaul, diamond-grid projection, depth sorting, mouse picking rework, zone transition changes. Touches nearly every spatial file.
- **Hyper Light Drifter proves top-down works** - atmospheric, visceral, beautiful pixel art without isometric projection. The feel comes from art direction, not camera angle.

### Tile Size: 32x32 (LOCKED)

**This is final.** 32x32 is the only tile size that integer-scales to all three target resolutions
with no letterboxing and a consistent field of view:

```
32x32 tiles, 20 tiles across:
  1080p (1920x1080): scale 3x -> 96px tiles  integer
  1440p (2560x1440): scale 4x -> 128px tiles integer
  4K    (3840x2160): scale 6x -> 192px tiles integer

64x64 fails: 1920/(64x20) = 1.5x (1440p kills every tile count)
```

32x32 gives 1024 pixels of detail per tile - Hyper Light Drifter looks stunning at half that (16x16).
Sharpness comes from art quality and post-processing (lighting, glow, particles), not pixel count.

### Virtual Resolution: 640x360 (LOCKED)

Pixel art must scale by integer multiples to stay crisp. Game renders into a 640x360
RenderTexture, then point-filter blits to the display:

| Display | Scale Factor | Result |
|---------|-------------|--------|
| Windowed (default) | 2x (1280x720) | Pixel-perfect |
| Fullscreen 1080p | 3x | Pixel-perfect |
| Fullscreen 1440p | 4x | Pixel-perfect |
| Fullscreen 4K | 6x | Pixel-perfect |

With 32x32 tiles in the 640x360 virtual buffer:
- **20 tiles across, ~11 tiles tall** visible at default zoom
- Similar field of view to Hyper Light Drifter
- Existing zoom range (0.5x-3.0x) works within the virtual buffer

---
---

# Human Work: Art & Creative Direction

Everything below is the human's responsibility. Claude does not judge visual quality
or make creative decisions about art style.

---

## Tooling

- **PixelLab.ai** -- AI pixel art generation (web UI, Aseprite extension). Used for
  generating base sprites and tilesets. Human curates and refines output.
- **Aseprite** -- Pixel art editor for manual refinement, animation, and sprite sheet assembly.
- **In-game tile editor** -- Manual tile placement and collision marking.

## Asset Specifications

All assets must be created at these sizes to match the engine spec.

| Asset | Size |
|-------|------|
| Floor tiles | 32x32 |
| Wall/roof tiles | 32x32 |
| Player sprites | 32x32/frame |
| NPC sprites | 32x32/frame |
| Small props | 32x32 |
| Medium props | 64x64 (spans 2x2 tiles) |
| Large/special | 128x128 (spans 4x4 tiles) |
| Item icons | 32x32 |

### Character Sprite Sheets

**Directions: 3 + flip.** Draw Down, Up, and Side. Engine flips Side horizontally for
left vs right. Diagonal movement shows nearest cardinal facing (standard for top-down
pixel art -- same as HLD, Zelda, Stardew Valley).

**Animation states and frame counts:**

| State | Frames |
|-------|--------|
| Idle | 4 |
| Walk | 6 |
| Attack | 4 |

**Per character: 42 frames total** (14 frames x 3 directions), each 32x32.

**Sheet layout** (one PNG per character):

```
        F1    F2    F3    F4    F5    F6
Row 0: [idle ][idle ][idle ][idle ]                <- Down
Row 1: [walk ][walk ][walk ][walk ][walk ][walk ]  <- Down
Row 2: [atk  ][atk  ][atk  ][atk  ]               <- Down
Row 3: [idle ][idle ][idle ][idle ]                <- Up
Row 4: [walk ][walk ][walk ][walk ][walk ][walk ]  <- Up
Row 5: [atk  ][atk  ][atk  ][atk  ]               <- Up
Row 6: [idle ][idle ][idle ][idle ]                <- Side
Row 7: [walk ][walk ][walk ][walk ][walk ][walk ]  <- Side
Row 8: [atk  ][atk  ][atk  ][atk  ]               <- Side

Canvas: 192x288 pixels (6 cols x 9 rows x 32px)
```

### Buildings

Buildings are **not** one big image. They're composed from wall/roof Wang tilesets:

```
+----------+----------+----------+----------+
| wall-NW  | wall-N   | wall-N   | wall-NE  |
+----------+----------+----------+----------+
| wall-W   | roof     | roof     | wall-E   |
+----------+----------+----------+----------+
| wall-W   | roof     | roof     | wall-E   |
+----------+----------+----------+----------+
| wall-SW  | door     | wall-S   | wall-SE  |
+----------+----------+----------+----------+
         Each cell = one 32x32 tile
```

Create a cyberpunk wall Wang tileset (edges, corners, inner corners, roof fill, door
variations). Buildings of any size use the same tileset.

## Art Workflow

1. Find/create cyberpunk pixel art style references
2. Create assets using PixelLab + Aseprite at the sizes above
3. Manually refine and curate (human eye judges quality)
4. Place tiles in the interactive editor
5. Mark collision in the editor
6. Hand finished assets to Claude for engine integration

---
---

# Claude Work: Engine & Integration

Everything below is Claude's responsibility. Code changes, config wiring, editor
improvements, rendering pipeline.

---

## 1. RenderTexture Pipeline (Required -- Pixel-Perfect Scaling)

**Problem**: Currently the game renders directly to the screen at 1280x720. When F11
fullscreen is toggled, raylib stretches the content via bilinear filtering to fill the
monitor. This produces blurry, non-integer-scaled pixel art.

**Current behavior**:
| Mode | What Happens | Pixel Quality |
|------|-------------|---------------|
| Windowed | 1280x720, direct render | Native pixels (OK) |
| F11 fullscreen | Raylib stretches 1280x720 to fill monitor | Blurry bilinear stretch |

**Target behavior**:
| Mode | What Happens | Pixel Quality |
|------|-------------|---------------|
| Windowed | 640x360 RenderTexture, blit at 2x = 1280x720 | Pixel-perfect |
| Fullscreen 1080p | 640x360 RenderTexture, blit at 3x | Pixel-perfect |
| Fullscreen 1440p | 640x360 RenderTexture, blit at 4x | Pixel-perfect |
| Fullscreen 4K | 640x360 RenderTexture, blit at 6x | Pixel-perfect |

**What needs to change**:
- Create a 640x360 `RenderTexture2D` at startup
- Render all game content (world, entities, UI) into this texture
- Blit to screen with `TEXTURE_FILTER_POINT` (nearest-neighbor) at integer scale
- **Scale factor calculation**: `floor(monitor_height / 360)` for fullscreen,
  `2` for default windowed mode
- **F11 toggle**: recalculate scale factor when switching modes
- **Mouse input**: divide screen coordinates by scale factor to get virtual coordinates
- **Camera offset**: always `(320, 180)` in virtual space, not `(screen_width/2, screen_height/2)`
- **`current-screen-width` / `current-screen-height`**: always return 640/360 (virtual),
  add separate `display-screen-width` / `display-screen-height` for actual monitor queries

**Files touched**:
- `src/config-client.lisp` -- add virtual resolution constants
- `src/main.lisp` + `src/net-client.lisp` -- RenderTexture creation, blit step in main loop
- `src/rendering.lisp` -- draw into RenderTexture instead of screen
- `src/input.lisp` -- mouse coordinate transform (screen -> virtual)
- `src/utils.lisp` -- `current-screen-width/height` returns virtual resolution

**What does NOT change**: game logic, networking, zones, combat, AI, serialization.

## 2. Asset Config Updates

Once the human delivers new assets, Claude wires them in:

- Update animation set definitions in `data/animation-sets.lisp` for new frame counts and sprite sheets
- Point texture loader at new cyberpunk PNGs
- `*tile-size*`: 16 -> 32
- `*tile-scale*`: 4.0 -> 1.0 (tiles are native size in virtual buffer)
- `*sprite-scale*`: 4.0 -> 1.0 (sprites are native size in virtual buffer)

## 3. Editor Improvements (As Needed)

The existing interactive tile editor works at the current tile size. May need adjustments
for 32x32 tiles (grid snapping, preview sizes, tileset browser). Will address as we
encounter issues during asset integration.

## What Already Works (No Changes Needed)

- Zone format: chunk-based tiles, collision maps, multiple layers
- Animation system: data-driven, just update config values
- Chunk render cache: works at any tile size
- Spatial grid, viewport culling: resolution-independent
