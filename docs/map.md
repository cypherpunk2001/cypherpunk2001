# map.lisp

Purpose: TMX parsing, tileset loading, and collision queries.

Key responsibilities:
- Parse TMX layers and chunked CSV tile data.
- Build `map-data` with bounds, tilesets, and collision tiles.
- Load tileset textures for rendering.

Key functions:
- `load-tmx-map`, `parse-layers`, `parse-layer`, `parse-chunk`.
- `build-collision-tiles`, `collision-tile-p`.
- `map-bounds`, `map-tile-outside-bounds-p`.
- `load-map-tilesets`, `map-tileset-for-gid`.

Notes:
- Collision layers are defined by names in `*map-collision-layers*`.
