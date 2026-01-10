# Floor Tiling System

This project uses a deterministic, atlas-based floor tiling system designed to
avoid visual repetition today and scale into more complex world generation later.

## Goals (Current Phase)

- Keep visuals stable (no per-frame randomization or flicker).
- Add subtle variation to break repetition.
- Keep everything walkable (no gameplay meaning yet).
- Keep renderer independent from map-generation logic.

## Tileset and Indexing

- Tiles come from a single atlas: `assets/2 Dungeon Tileset/1 Tiles/Tileset.png`.
- Tile size: `16x16` in the atlas.
- Atlas grid: `19` columns.
- Tile index is row-major.
- Given an index `i`:
  - `col = i mod 19`
  - `row = floor(i / 19)`
  - Source rect = `(col*16, row*16, 16, 16)`

## The Contract

**Contract:** given `(x, y)` tile coordinates, return a tile index.

Today, that contract is implemented with a deterministic hash and a small
variant palette. Later, the same contract can be satisfied by:

- A dungeon generator
- An authored map
- Streamed chunks
- Server-provided data (MMO)
- Biome or zone rules

The renderer does not care how the index is chosen.

## Deterministic Variation (Current Implementation)

The floor is filled by a stable function:

- A main floor tile index (ex: 40).
- A small list of variant indices (ex: 41, 42).
- A clustered hash to create patchy variation instead of speckles.
- Optional empty tiles using index `0` (treated as "skip draw").

Key knobs (in `src/main.lisp`):

- `*floor-tile-index*` — base floor tile.
- `*floor-variant-indices*` — cracks/variants.
- `*floor-variant-mod*` — 1 in N chance a cluster uses a variant.
- `*floor-cluster-size*` — cluster size in tiles.

### Why Clustering

Picking variants per tile creates "salt and pepper" noise. Clustering causes
small patches of cracks so the floor feels more "placed" and less random.

## Rendering Rules

- Floor is drawn first, then the player.
- Tile index `0` is treated as empty and is **not drawn**.
- The floor map is generated once and stored in memory.
- The renderer only draws what is on-screen and does not re-randomize.

## Scaling Path (Why This Helps Later)

This system is a foundation, not a final map generator. It is designed to scale:

- **Camera and scrolling**: the floor can scroll under the player without change.
- **Chunking**: large maps can be generated or streamed in chunks.
- **Biomes**: swap tile palettes by zone without touching the renderer.
- **Networking**: use a shared seed or server-sent tile indices for consistency.
- **Gameplay layers**: walls, collision, and interaction can be added later
  without changing floor rendering.

## Practical Next Steps (Future)

- Add a `*floor-seed*` to control layout per zone or session.
- Add walls as a second visual layer (no collision yet).
- Add chunk caching to reduce draw calls on large maps.
- Add biome palettes to vary floor appearance by area.
