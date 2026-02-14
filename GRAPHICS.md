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

## Resolution Strategy

### Virtual Resolution: 640x360

Pixel art must scale by **integer multiples** to stay crisp (no fractional scaling). We render the entire game into a 640x360 RenderTexture, then point-filter blit it to the display:

| Display | Scale Factor | Result |
|---------|-------------|--------|
| 1080p (1920x1080) | 3x | Exact fit |
| 1440p (2560x1440) | 4x | Exact fit |
| 4K (3840x2160) | 6x | Exact fit |

All integer. All pixel-perfect. No fuzzy scaling at any resolution.

### What This Means for Gameplay

With 32x32 tiles in a 640x360 virtual buffer:
- **20 tiles across, ~11 tiles tall** visible at default zoom
- Similar field of view to Hyper Light Drifter
- Existing zoom range (0.5x-3.0x) works within the virtual buffer: zoomed out shows ~40x22 tiles

### Engine Changes

- Render all game content to a `RenderTexture2D` at 640x360
- Draw that texture to screen with `TEXTURE_FILTER_POINT` (nearest-neighbor)
- Scale factor = `floor(display_height / 360)` (always integer)
- Camera zoom still works within the virtual buffer (unchanged)
- All game coordinate math stays the same

---

## Standardized Asset Sizes

### The Rule: Everything is 32x32 (LOCKED)

**This is final.** 32x32 is the only tile size that integer-scales to all three target resolutions
with no letterboxing and a consistent field of view:

```
32x32 tiles, 20 tiles across:
  1080p (1920x1080): scale 3x → 96px tiles  ✓ integer
  1440p (2560x1440): scale 4x → 128px tiles ✓ integer
  4K    (3840x2160): scale 6x → 192px tiles ✓ integer

64x64 fails: 1920/(64×20) = 1.5x ✗ (1440p kills every tile count)
```

32x32 gives 1024 pixels of detail per tile - Hyper Light Drifter looks stunning at half that (16x16).
Sharpness comes from art quality and post-processing (lighting, glow, particles), not pixel count.

This gives us:
- Maximum PixelLab efficiency (64 frames per style reference call at 32x32)
- Clean alignment with the 640x360 virtual resolution (20x11 tile grid)
- Same sprite frame size we already use (no animation system changes)

### Floor / Terrain Tiles: 32x32

```
PixelLab tool: create_topdown_tileset
Wang tiles handle seamless transitions automatically
"concrete" -> "neon pavement" -> "metal grating"
64 variations per style reference call
```

Placed manually in tile editor. Collision marked manually in editor.

Engine changes:
- `*tile-size*`: 16 -> 32
- `*tile-scale*`: 4.0 -> 1.0 (tiles are now native size in virtual buffer)

### Player & NPC Sprites: 32x32 per frame

```
PixelLab tool: create_character(n_directions=4)
Then: animate_character(animation="walk/idle/attack")
Character fills most of the 32x32 frame
Occupies ~1 tile on the grid
```

Same frame size as current sprites. Animation system in `data/animation-sets.lisp` barely changes.

Engine changes:
- `*sprite-scale*`: 4.0 -> 1.0 (sprites are native size in virtual buffer)

### Buildings: 32x32 Tile Assemblies (Not Big Sprites)

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

Generate a cyberpunk **wall Wang tileset** via PixelLab (edges, corners, inner corners, roof fill, door variations). Buildings of any size use the same tileset - 3x3 shack, 8x12 megacorp tower, same tiles, different footprint.

Collision is trivial: wall tiles = blocked, door tiles = walkable, roof tiles = blocked.

### Small Props: 32x32

```
PixelLab tool: create_map_object or style reference
Crates, trash cans, fire hydrants, small terminals, barrels
64 per generation at this size
Placed on decoration layer above ground tiles
```

### Medium Props: 64x64

```
PixelLab tool: create_map_object
Vehicles, large vending machines, dumpsters, vendor stalls
16 per generation at this size
Spans 2x2 tiles on the map
```

### Large / Special: 128x128

```
PixelLab tool: create_map_object
Boss sprites, unique landmarks, large neon signs
4 per generation at this size
Spans 4x4 tiles on the map
```

### Asset Size Summary

| Asset | Size | PixelLab Frames/Call | Tool |
|-------|------|---------------------|------|
| Floor tiles | 32x32 | 64 | `create_topdown_tileset` / style ref |
| Wall/roof tiles | 32x32 | 64 | `create_topdown_tileset` / style ref |
| Player sprites | 32x32/frame | N/A | `create_character` + `animate_character` |
| NPC sprites | 32x32/frame | N/A | `create_character` + `animate_character` |
| Small props | 32x32 | 64 | `create_map_object` / style ref |
| Medium props | 64x64 | 16 | `create_map_object` |
| Large/special | 128x128 | 4 | `create_map_object` |
| Item icons | 32x32 | 64 | Style reference |

---

## Tooling: PixelLab.ai

PixelLab is an AI pixel art generation platform with an MCP (Model Context Protocol) server that integrates directly into Claude Code. Once configured, Claude can call PixelLab's generation tools mid-session.

### Setup

```bash
claude mcp add pixellab https://api.pixellab.ai/mcp -t http -H "Authorization: Bearer YOUR_API_KEY"
```

Requires a Tier 1 subscription for style reference features.

### Available MCP Tools

| Tool | What It Does | Our Use |
|------|-------------|---------|
| `create_topdown_tileset` | Generates Wang tilesets with seamless terrain transitions (e.g., concrete-to-metal) | Cyberpunk ground, walls, terrain |
| `create_character` | Pixel art characters with 4 or 8 directional views | Player classes, NPCs |
| `animate_character` | Adds walk/run/idle/attack animations to existing characters | Full animation sets |
| `create_map_object` | Props with transparent backgrounds, supports style matching | Neon signs, terminals, crates, dumpsters |
| `create_isometric_tile` | Individual isometric tiles | NOT using (see decisions below) |

### Style Reference System (Pro Feature)

Provide up to 5 reference images that define the visual style. All subsequent generations stay in that visual lane. The references do the heavy lifting - the text prompt is just a one-liner saying *what* to generate (e.g., `"cyberpunk street thug with neon visor"`). Not a multi-page prompt - one sentence, the images carry the style.

Frame output by size:
- 32x32 -> 64 frames (8x8 grid) — up to 64 style images
- 64x64 -> 16 frames (4x4 grid) — up to 16 style images
- 128x128 -> 4 frames (2x2 grid) — up to 4 style images
- 256x256 -> 1 frame — 1 style image

Costs 40 generations per style reference call.

### Workflow

1. **User** picks 3-5 cyberpunk pixel art reference images (creative direction)
2. **Claude** calls MCP tools with simple descriptions (`"concrete sidewalk"`, `"neon sign"`, `"hooded hacker"`)
3. **User** picks which generated variations look good
4. **Claude** wires chosen assets into engine config

---

## Key Decisions

### Staying Top-Down (Not Isometric)

We evaluated isometric and decided against it:

- **PixelLab's isometric tooling is bare-bones** - `create_isometric_tile` makes one tile at a time with no Wang tileset transitions. Top-down has full seamless tileset generation.
- **Isometric requires significant engine surgery** - coordinate system overhaul, diamond-grid projection, depth sorting, mouse picking rework, zone transition changes. Touches nearly every spatial file.
- **Hyper Light Drifter proves top-down works** - atmospheric, visceral, beautiful pixel art without isometric projection. The feel comes from art direction, not camera angle.

### Everything at 32x32 (LOCKED)

32x32 is the only size that integer-scales to 1080p/1440p/4K with no letterboxing and
consistent 20-tile field of view. This is final - see "Standardized Asset Sizes" for the math.

- Same sprite frame size we already use (32x32) - minimal animation system changes
- Only go bigger (64x64, 128x128) for multi-tile props and special assets

---

## The Plan: Better Art, Same Editor Workflow

### Approach

Generate cohesive cyberpunk tilesets and sprites via PixelLab + Aseprite, then place them manually in the existing tile editor. Same workflow as today, dramatically better art.

**Division of labor**:
- **Human**: Creative direction, style references, asset creation (PixelLab/Aseprite), tile placement in editor, visual quality judgment
- **Claude**: Engine code, editor improvements, asset integration, animation config, resolution pipeline, format wiring

**What changes**:
- Tiles go from mismatched OpenGameArt to cohesive cyberpunk Wang tilesets
- Characters go from generic fantasy sprites to cyberpunk pixel art matching style references
- Editor gets improvements as needed to support new tile sizes and workflow

**What stays the same**:
- Manual tile-by-tile placement in the interactive editor
- Manual collision marking in the editor
- Zone format, chunk system, all engine architecture
- Human eye judges visual coherence (AI can't reliably do this)

---

## Engine Integration Points

The animation system is already data-driven (`data/animation-sets.lisp`), so new assets are mostly config changes:

- Update animation set definitions for new frame counts and sprite sheets
- Point texture loader at new PNGs
- `*tile-size*`: 16 -> 32
- `*tile-scale*`: 4.0 -> 1.0
- `*sprite-scale*`: 4.0 -> 1.0
- Add RenderTexture pipeline (640x360 virtual resolution -> integer scale to display)
- Zone format already supports everything: chunk-based tiles, collision maps, multiple layers

No engine architecture changes required for the art swap itself.

---

## Asset Generation Budget (Estimated)

| Asset Category | PixelLab Calls | Output |
|---|---|---|
| Ground terrain variants | 1-2 style ref calls | 64-128 tile variations |
| Wall/roof building tiles | 1-2 style ref calls | 64-128 wall variants |
| Cyberpunk small props | 2-3 style ref calls | 128-192 small objects |
| Medium props (vehicles etc.) | 1-2 map object calls | 16-32 medium objects |
| Item icons (inventory) | 1-2 style ref calls | 64-128 icons |
| Player characters | Character creator | 4-dir animated sprites per class |
| NPCs | Character creator | 4-dir animated sprites per type |

~8-12 PixelLab calls for the base tileset and props, plus character generation for sprites.
