# CYBERPUNK ISO MMO --- PIXEL ART PROMPT LIBRARY (ZERO → HERO)

*Updated to include a practical generation workflow and "strict"
sprite-sheet prompts.*

This document assumes: - **2D Isometric** - **Pixel art only** -
**Cyberpunk 2001** - **Dark Diablo-ish mood** - **MMO readability
priority** - **Character target: 96px tall at runtime** - **Ground tile
footprint: 64×32 diamond (2:1 iso)**

The key idea: **Don't try to get a perfect 48-frame sheet on day one.**\
Validate style with single frames → one direction strips → then assemble
into full sheets.

------------------------------------------------------------------------

## GLOBAL BASE PROMPT (prepend to everything)

Copy/paste this at the top of every Pixellab prompt:

**BASE:**\
2D isometric pixel art, 64x32 diamond grid system, fixed camera, no
perspective distortion, early 2000s cyberpunk city, gritty industrial
atmosphere, muted cool palette, dark ambient lighting, warm sodium
streetlights, subtle neon accents, strong silhouette readability for MMO
gameplay, clean pixel clusters, no anti-aliasing, no painterly style, no
3D render look, consistent top-left light source, designed for 96px tall
characters

### Add these "pixel discipline" constraints whenever Pixellab supports them

-   transparent background
-   no anti-aliasing
-   no blur
-   clean pixel clusters
-   limited palette per asset
-   no dithering noise / no noisy texture
-   consistent light direction

------------------------------------------------------------------------

## Folder & naming (so your outputs don't turn into chaos)

Suggested structure (adjust to your repo):

-   `art/tiles/iso64x32/`
-   `art/props/`
-   `art/buildings/`
-   `art/characters/<archetype>/`
-   `art/icons/`
-   `art/fx/`

Suggested file naming: - Tiles: `tile_asphalt_A_64x32.png` - Props:
`prop_lamp_01.png` - Characters: `char_merc_idle_S.png`,
`char_merc_walk_S_strip6.png`, `char_merc_walk_sheet8x6.png` - Icons:
`icon_medkit_64.png`

------------------------------------------------------------------------

# 0 → HERO GENERATION WORKFLOW (recommended order)

## Phase 1 --- World foundation (get playable fast)

1)  Ground tiles (asphalt/sidewalk/markings)\
2)  Transitions (curb edges, road-to-sidewalk)\
3)  Props (lamp, dumpster, pole, fence)\
4)  One building kit (wall segments + doors + roof)\
5)  Neon signage kit (small set)

**Only after the world reads well:** start characters.

## Phase 2 --- Characters & items

6)  One character archetype (idle + walk)\
7)  A minimal weapon set\
8)  Inventory icons for the first 20 items\
9)  UI polish later (not required for world feel)

## Phase 3 --- FX & atmosphere

10) Rain overlay, steam vents, flicker elements\
11) Spell telegraphs / hit sparks (simple)

------------------------------------------------------------------------

# 1️⃣ GROUND TILES (64×32 diamond)

Ground tiles should be **tileable** and **low-noise**.\
You will generate: (A) a single tile, then (B) a tileset sheet of
variants.

## 1A. Single "style lock" tile (generate first)

Use this to lock the overall look before generating a sheet.

**PROMPT --- Asphalt (single tile):** BASE + 2D isometric pixel art
asphalt street ground tile, **single isolated tile**, **64x32 diamond
footprint**, seamless edges, dark wet asphalt, subtle cracks and oil
stains, muted blue-gray tones, clean pixel shading, minimal texture
noise, **transparent background**, centered tile, no extra objects

**PROMPT --- Sidewalk (single tile):** BASE + 2D isometric pixel art
cracked concrete sidewalk ground tile, **single isolated tile**, **64x32
diamond footprint**, seamless edges, light gray concrete with subtle
grime, clean pixel clusters, minimal noise, **transparent background**,
centered tile

## 1B. Variant tileset sheet (after style lock)

Make a sheet you can slice.

**Tileset spec (recommended):** - **Each tile:** 64×32 - **Layout:** 4
columns × 4 rows (16 tiles) - **Canvas:** 256×128 - **No spacing between
tiles** (or specify spacing if your slicer expects it)

**PROMPT --- Asphalt Variants tileset (4×4):** BASE + Isometric pixel
art **tileset sprite sheet** of urban asphalt street ground tiles. **16
tiles total** arranged in a **4 columns x 4 rows grid**. Each tile is a
**64x32 diamond** footprint. Variants include: clean asphalt, cracked
asphalt, oil stain, puddle hint, patched asphalt, subtle manhole decal,
faded paint scuff, light debris marks (very subtle). **Seamless
tiling**. **Transparent background**. **No spacing** between tiles. No
props or characters.

**PROMPT --- Sidewalk Variants tileset (4×4):** BASE + Isometric pixel
art **tileset sprite sheet** of cracked concrete sidewalk ground tiles.
**16 tiles** in **4x4 grid**, each tile **64x32 diamond**. Variants
include: clean slab, cracked slab, grime edge, gum stain hint, small
litter speck (very subtle), repair patch, drain edge hint. **Seamless**.
Transparent background. No spacing. No props.

## 1C. Transitions (curb / edge tiles) --- do after you have base tiles

You likely need road↔sidewalk transitions to avoid ugly seams.

**PROMPT --- Road-to-sidewalk transition set (4×4):** BASE + Isometric
pixel art **transition tileset sprite sheet** for road-to-sidewalk
edges. **16 tiles** in **4x4 grid**, each tile **64x32 diamond**.
Include curb edge variations: straight edges, inner corners, outer
corners, T-junction style edges, small curb damage. Must align perfectly
with existing asphalt and sidewalk style. Transparent background. No
spacing.

------------------------------------------------------------------------

# 2️⃣ PROPS (lamps, dumpsters, fences)

Props are **free-form size** but must have a clear **ground contact**
point. Assume props "sit" on one 64×32 tile unless specified.

## 2A. Street Lamp (single prop)

Generate the prop first; lighting/glow will be handled in-engine later.

**PROMPT --- Street Lamp (prop only):** BASE + Isometric pixel art
industrial street lamp prop, **single isolated object**, height around
**128px**, metal pole with lamp head, warm sodium light housing, subtle
grime, strong silhouette, clean pixel clusters, **transparent
background**, centered, no scene, no floor (or a tiny base shadow only)

## 2B. Dumpster / Utility Pole / Fence (single props)

**PROMPT --- Dumpster:** BASE + Isometric pixel art industrial dumpster,
single isolated object, worn paint and rust, gritty but not noisy,
strong silhouette, transparent background, centered

**PROMPT --- Utility Pole:** BASE + Isometric pixel art wooden utility
pole with cables, single isolated object, dark desaturated wood, clear
cable silhouette, transparent background, centered

**PROMPT --- Fence Section (tileable prop segment):** BASE + Isometric
pixel art industrial metal fence section, **repeatable segment**, strong
silhouette, transparent background, centered, designed to connect
seamlessly end-to-end

## 2C. Optional: "prop sheet" (multiple props on one sheet)

If Pixellab can reliably place multiple objects in a grid, you can
request a sheet.

**PROMPT --- Prop sheet (3×2 grid):** BASE + Isometric pixel art **prop
sprite sheet** with **6 isolated props** arranged in **3 columns x 2
rows**. Each cell contains one prop: street lamp, dumpster, utility
pole, trash bag pile, traffic barrier, crate stack. Transparent
background, no spacing, consistent scale and lighting.

------------------------------------------------------------------------

# 3️⃣ BUILDINGS (modular kit)

Buildings are easiest when modular: - wall segments - door segment -
window segment - corner segment - roof pieces

## 3A. Wall kit (single "style lock" wall)

**PROMPT --- Wall segment (single):** BASE + Isometric pixel art
industrial building wall segment, single isolated modular piece,
concrete with subtle cracks and grime, metal trims, strong silhouette,
transparent background, designed to tile horizontally with matching
edges

## 3B. Wall kit sheet (recommended 4×2)

**Kit spec:** - 8 pieces total in a 4×2 grid - Each piece can be 128×128
(safe), or specify the exact px size you want

**PROMPT --- Building wall kit sheet (4×2):** BASE + Isometric pixel art
**modular building kit sprite sheet**, **8 pieces** arranged **4 columns
x 2 rows**, each piece isolated with transparent background, consistent
scale and angle. Include: straight wall A, straight wall B (variant),
corner wall, door frame + door, window section, wall with vent, wall
with pipes, short wall cap piece. No scene. No characters. No spacing.

## 3C. Roof kit (optional early)

**PROMPT --- Roof kit sheet (4×2):** BASE + Isometric pixel art
**rooftop modular kit sprite sheet**, 8 pieces in 4x2 grid, flat tar
roof textures, subtle vents, duct units, access hatch, parapet edge
pieces, consistent angle, transparent background, no spacing.

------------------------------------------------------------------------

# 4️⃣ NEON SIGNAGE KIT (mood maker)

Neon is where cyberpunk reads instantly. Keep it restrained.

## 4A. Single neon sign (style lock)

**PROMPT --- Neon sign (single):** BASE + Isometric pixel art vertical
neon sign, single isolated object, muted metal casing, glowing cyan or
magenta core, subtle pixel glow (but no blur), readable shape,
transparent background, centered

## 4B. Signage sheet (4×2)

**PROMPT --- Neon kit sheet (4×2):** BASE + Isometric pixel art **neon
signage sprite sheet**, **8 isolated signs** in **4 columns x 2 rows**,
transparent background, no spacing, consistent style and scale. Include:
vertical sign, horizontal sign, small door sign, arrow sign, vending
machine panel glow sign, warning sign, billboard mini, hanging sign.

------------------------------------------------------------------------

# 5️⃣ INVENTORY ICONS (NOT isometric)

Icons are square and should be readable at small sizes.

## 5A. Single icon (style lock)

**PROMPT --- Medkit icon:** 2D pixel art inventory icon, 64x64,
cyberpunk medkit, clean shading, limited palette, strong silhouette,
transparent background, no isometric perspective

## 5B. Icon sheet (8×8 = 64 icons) --- only after you've tested 10--20 singles

Many generators struggle with huge sheets; keep it smaller.

**PROMPT --- Icon sheet (4×4 = 16 icons):** 2D pixel art inventory icon
sprite sheet, 16 icons total arranged 4 columns x 4 rows, each icon
64x64, transparent background, no spacing, cyberpunk tactical items:
medkit, ammo box, battery, keycard, stim, grenade, wrench, data chip,
goggles, boots, jacket, pistol mag, radio, lockpick, credits, energy
drink. Clean pixel shading, limited palette per icon.

------------------------------------------------------------------------

# 6️⃣ CHARACTERS (ZERO → FULL SHEETS)

**Important:** Generators often fail at "48-frame perfect sheet" in one
go.\
Use this progression to lock style and reduce waste.

## 6A. One direction single-frame (style lock)

Pick **S (south / facing camera)** first (usually most readable).

**PROMPT --- Character idle, single frame (S):** BASE + Isometric pixel
art cyberpunk mercenary character, **single frame**, facing **South**,
full body, **96px tall**, semi-realistic proportions, tactical jacket,
boots, gloves, subtle cyber augment details, strong silhouette, 3-tone
shading, clean pixel clusters, **transparent background**, centered, no
sprite sheet

## 6B. One direction walk strip (6 frames)

**Spec:** 1 row × 6 columns, each frame 96×96 (safe cell size)

**PROMPT --- Walk strip (S, 6 frames):** BASE + Isometric pixel art
character **walk cycle strip**, facing **South**, **6 frames**, arranged
**1 row x 6 columns**, each frame in a **96x96 cell**, consistent
proportions and lighting, clear leg motion, minimal motion blur,
transparent background, **no spacing** between frames, cyberpunk
mercenary

## 6C. Four directions (S, SE, E, NE) as separate strips

Do these as separate runs so the model stays consistent.

**PROMPT template --- Walk strip (DIRECTION):** BASE + Isometric pixel
art character walk cycle strip, facing **{DIRECTION}**, 6 frames, 1 row
x 6 columns, 96x96 cells, transparent background, no spacing, same
character design as previous

Directions to generate: - S, SE, E, NE, N, NW, W, SW (8 total)

## 6D. 8-direction idle sheet (8×1) --- optional

If you want idle in all directions.

**PROMPT --- Idle sheet (8 directions):** BASE + Isometric pixel art
**idle pose sprite sheet**, **8 directions** (N, NE, E, SE, S, SW, W,
NW), arranged **8 rows x 1 column**, each frame in a **96x96 cell**,
transparent background, no spacing, same character

## 6E. Full walk sheet (8×6 = 48 frames)

Once you have consistent single-direction strips, you can try a full
sheet.

**PROMPT --- Full walk sheet (8×6):** BASE + Isometric pixel art **walk
cycle sprite sheet**, **48 frames total**, **8 directions** (N, NE, E,
SE, S, SW, W, NW) and **6 frames per direction**. Arrange as **8 rows
(directions) x 6 columns (frames)**. Each frame in a **96x96 cell**.
Transparent background. No spacing. Same character design and palette
across all frames. Clean pixel clusters, no anti-aliasing.

### If the full sheet fails (common):

-   Generate each direction strip separately (6B/6C) and assemble
    yourself.
-   This is normal and often faster than fighting the generator.

------------------------------------------------------------------------

# 7️⃣ ATMOSPHERE FX (optional)

## Rain overlay

**PROMPT --- Rain overlay:** Pixel art rain overlay, subtle vertical
streak pattern, transparent background, no blur, no anti-aliasing,
designed for dark cyberpunk environment, seamless tiling

## Steam vent puff (simple)

**PROMPT --- Steam puff (4 frames):** Isometric pixel art steam puff
animation, 4 frames, arranged 1 row x 4 columns, transparent background,
soft but pixel-clean edges, no blur

------------------------------------------------------------------------

# QUICK "DO / DON'T"

## DO

-   Start with single tiles/props to lock style
-   Keep ground tiles low noise
-   Keep characters silhouette-first
-   Use strips per direction if sheets fail
-   Keep naming consistent

## DON'T

-   Don't generate massive sheets until you've validated style
-   Don't rely on downscaling for pixel art detail
-   Don't mix light directions between batches
-   Don't let ground textures become noisy "static"

------------------------------------------------------------------------

# GENERATION ORDER (short)

1)  Asphalt tile (single) → asphalt tileset\
2)  Sidewalk tile (single) → sidewalk tileset\
3)  Transitions/curbs\
4)  Lamp + dumpster + pole + fence\
5)  Building wall kit\
6)  Neon kit\
7)  Character single (S) → strip (S) → strips (8 dirs) → full sheet
    (optional)\
8)  Icons (singles → small sheets)\
9)  FX overlays
