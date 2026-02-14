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

- **PixelLab.ai** (https://www.pixellab.ai/create) -- AI pixel art generation (web UI + Aseprite extension)
- **Aseprite** -- Pixel art editor with PixelLab extension installed for AI-assisted touchups
- **In-game tile editor** -- Manual tile placement and collision marking

## Art Pipelines

Starting point: https://www.pixellab.ai/create

**Pipeline A** (characters/objects): HD image -> "Image to pixel art" -> seed sprite -> "Style reference" -> 16 variations at 64x64 -> downscale 32x32 -> "8-directional" -> Aseprite touchup

**Pipeline B** (tilesets): Tileset creator -> define Lower/Upper terrain pair -> set transition/style -> Generate 32x32 Wang tileset -> chain via Connect Existing for multi-terrain

### Pipeline A: Characters, NPCs, Objects, Icons (Pro required)

**Step 1: Seed sprite**
- Use **"Image to pixel art"** (free) to bootstrap
- Take a Cyberpunk 2077 screenshot, ArtStation reference, or ChatGPT-generated image
- Convert to pixel art -- this gives the first seed to establish visual style

**Step 2: Batch variations**
- Use **"Create from style reference"** (Pro)
- Upload up to 8 seed sprites as style references
- Describe what to generate (e.g., "cyberpunk street thug", "neon weapons", "med kits")
- Outputs 16 frames (4x4 grid) at 64x64 each
- Downscale to 32x32 in Aseprite (Sprite > Sprite Size > 32x32, Nearest Neighbor)
- Pick the best variations from each batch

**Step 3: Character directions**
- Use **"Create 8-directional sprite"** (Pro)
- Feed a character sprite you like from Step 2
- Generates all 8 directional views

**Step 4: Touchups**
- Aseprite with PixelLab extension for cleanup, animation, sprite sheet assembly
- Create Map tool (Aseprite extension) for hand-crafted unique areas (boss rooms, landmarks)
  using inpainting -- paint a map tile by tile, expanding outward

### Pipeline B: Tilesets (Terrain, Walls, Floors)

Uses the dedicated tileset creator at https://www.pixellab.ai/create (Maps section).
Generates 32x32 Wang tilesets natively -- no downscaling needed.

**How it works:**
- **Lower Terrain** = base ground (e.g., cracked asphalt)
- **Upper Terrain** = what sits on top (e.g., concrete sidewalk, metal grating)
- Generator produces all Wang transition tiles between the pair

**Terrain buttons:**
- **Create New** -- describe a terrain in text, generates from scratch
- **Upload Image** -- upload an existing tile as the terrain base
- **Connect Existing** -- chain to a previously created terrain for multi-terrain consistency

**Transition** (how terrains meet):
- **None** -- flat, same height (street to sidewalk)
- **Small (25%)** -- slight step (sidewalk to curb)
- **Large (50%)** -- noticeable elevation (ground to raised platform)
- **Full (100%)** -- cliff edge (street to building wall base)

**Style Options:**
- **Outline**: Lineless (fits HLD aesthetic, no black outlines)
- **Shading**: Detailed or Highly Detailed (cyberpunk surface grime and texture)
- **Detail Level**: High (32x32 has the pixel budget)

**Chaining terrains for a cyberpunk city:**
```
1. Create: "dark asphalt road" (Lower) -> "cracked concrete sidewalk" (Upper), Transition: Small
2. Connect Existing: sidewalk (Lower) -> Create: "metal building wall base" (Upper), Transition: Full
3. Connect Existing: asphalt (Lower) -> Create: "neon-lit puddle" (Upper), Transition: None
```

Each Generate call produces all Wang transition tiles for that pair.
Connect Existing keeps the chain visually consistent across terrain types.

### Other Useful Tools on the Create Page

| Tool | Use |
|------|-----|
| Create S-M image | One-off sprites at 16-64px (needs init image to work well) |
| Create M-XL image | Larger assets (64px+) |
| Create image (Pro) | High quality single generation |
| Create UI elements | HUD/menu components |

## Asset Specifications

All assets must be created at these sizes to match the engine spec.

| Asset | Size | Pipeline |
|-------|------|----------|
| Floor/terrain tiles | 32x32 | B (tileset creator) |
| Wall/roof tiles | 32x32 | B (tileset creator) |
| Player sprites | 32x32/frame | A (style reference) |
| NPC sprites | 32x32/frame | A (style reference) |
| Small props (doors, signs, vents) | 32x32 | A (style reference) |
| Medium props (vehicles, machines) | 64x64 (spans 2x2 tiles) | A (style reference) |
| Large/special (bosses, landmarks) | 128x128 (spans 4x4 tiles) | A (style reference) |
| Item icons | 32x32 | A (style reference) |

### Character Sprite Sheets

Directions, frame counts, and sheet layout TBD -- will spec after generating base content
and assessing what PixelLab produces.

### Buildings (Both Pipelines)

Buildings use **both** pipelines:

**Pipeline B (structure):** Wall surfaces, roof fills, floor textures -- anything that
repeats and tiles. Generate a wall/roof Wang tileset, then paint the building shape
in the editor. A 10x8 warehouse is mostly the same wall tile repeated with corner pieces.

```
+----------+----------+----------+----------+
| wall-NW  | wall-N   | wall-N   | wall-NE  |
+----------+----------+----------+----------+
| wall-W   | roof     | roof     | wall-E   |
+----------+----------+----------+----------+
| wall-W   | roof     | roof     | wall-E   |
+----------+----------+----------+----------+
| wall-SW  | wall-S   | wall-S   | wall-SE  |
+----------+----------+----------+----------+
         Each cell = one 32x32 tile (Pipeline B)
```

**Pipeline A (details):** Unique doors, neon signs, AC units, windows with holograms,
vendor awnings -- one-off details placed on top of the tiled structure as decoration
layer objects.

## Art Workflow Summary

**Characters/Objects (Pipeline A):**
1. Seed: HD image -> "Image to pixel art" -> first sprite
2. Batch: seed sprites -> "Create from style reference" -> 16 variations at 64x64
3. Downscale: 64x64 -> 32x32 in Aseprite (Nearest Neighbor)
4. Directions: best sprites -> "Create 8-directional sprite" -> all facings
5. Touchup: Aseprite + PixelLab extension for cleanup and animation

**Tilesets (Pipeline B):**
1. Create first terrain pair in tileset creator (32x32 native)
2. Chain additional terrains via Connect Existing
3. Set transition/style/detail options per pair
4. Generate Wang tilesets -- ready to use, no downscaling

**Final steps (both):**
1. Place tiles in the interactive editor
2. Mark collision in the editor
3. Hand finished assets to Claude for engine integration

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
