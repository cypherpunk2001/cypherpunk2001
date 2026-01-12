# map.lisp

Purpose
- Parse TMX maps, load tilesets, and build collision data.

Why we do it this way
- TMX gives us a production-ready pipeline with Tiled while keeping runtime
  parsing simple.
- Chunked layers enable culling and scale to large worlds.
- Collision lives in data, not in code, so levels are easy to iterate.

What gets built
- `map-data`: tileset metadata, layers, bounds, and collision hash table.
- Collision tiles are stored as a hash for O(1) lookups in hot loops.

Key functions
- `load-tmx-map`: orchestrates parsing and assembly.
- `parse-layer`, `parse-chunk`, `parse-csv-ints`: parse TMX data.
- `build-collision-tiles`: reads named layers into a blocked set.
- `map-bounds`, `map-tile-outside-bounds-p`: world limits.

Walkthrough: collision from TMX
1) TMX layers are parsed into chunked tile grids.
2) Layers listed in `*map-collision-layers*` are scanned for nonzero tiles.
3) Each blocked tile is stored in a hash for fast collision queries.
4) Movement and debug overlay query the same hash.

Example: configure collision layers
```lisp
;; In data/game-data.lisp
(:tunables
 (:collision-layers ("Objects1" "Objects2" "Objects3")))
```

Design note
- The map system is data-only. Rendering and collision reference the same
  source, which prevents visual/physics drift.
