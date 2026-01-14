# zone.lisp

Purpose
- Define a chunked zone data format and load it into runtime structures.

Why we do it this way
- Chunked tiles scale to large worlds and support streaming later.
- Collision is data-driven, so gameplay does not depend on hardcoded walls.
- The format is editor-friendly and can be exported without external tools.

Data format (zone file)
- A zone file is a single plist (or `(:zone <plist>)`) stored under `data/`.
- Required keys: `:id`, `:chunk-size`, `:width`, `:height`, `:layers`.
- Layers are plists with `:id`, `:collision`, and `:chunks`.
- Chunks are plists with `:x`, `:y`, and either `:tiles` or `:fill` + `:overrides`.
- `:overrides` entries are `(x y value)` in chunk-local tile coordinates.
- `:objects` is optional and reserved for placed entities/props.
- `:spawns` is optional and stores NPC spawn points as `(:id :rat :x 4 :y 2)` entries.

Example zone
```lisp
(:id :demo
 :chunk-size 8
 :width 8
 :height 8
 :layers
 ((:id :floor :collision nil
   :chunks ((:x 0 :y 0 :fill 0)))
  (:id :walls :collision t
   :chunks
   ((:x 0 :y 0
     :fill 0
     :overrides
     ((0 0 1) (1 0 1) (2 0 1) (3 0 1) (4 0 1) (5 0 1) (6 0 1) (7 0 1)
      (0 1 1) (7 1 1)
      (0 2 1) (7 2 1)
      (0 3 1) (7 3 1)
      (0 4 1) (7 4 1)
      (0 5 1) (7 5 1)
      (0 6 1) (7 6 1)
      (0 7 1) (1 7 1) (2 7 1) (3 7 1) (4 7 1) (5 7 1) (6 7 1) (7 7 1))))))
 :objects
 ((:id :chest :x 3 :y 3))
 :spawns
 ((:id :rat :x 5 :y 5)))
```

Key functions
- `load-zone`: reads a zone file and builds chunk/layer structures.
- `zone-wall-map`: converts collision layers into a wall map array.
- `zone-layer-tile-at`: fetches a tile index from chunk data.
- `ensure-zone-layer`, `zone-layer-set-tile`: editor helpers for mutating layers.
- `zone-add-object`, `zone-remove-object-at`: editor helpers for object placement.
- `zone-add-spawn`, `zone-remove-spawn-at`: editor helpers for spawn placement.
- `make-empty-zone`, `zone-resize`: helpers used by editor zone lifecycle tools.
- `zone-slice`, `zone-to-plist`, `write-zone`: export helpers for editor saves.

Design note
- Collision tiles are precomputed into a hash for fast lookups.
- The wall map is derived once at load time to keep movement cheap.
- Export helpers preserve the chunked format so later streaming is easy.
- Spawns live alongside objects so zones can drive NPC placement without extra files.
