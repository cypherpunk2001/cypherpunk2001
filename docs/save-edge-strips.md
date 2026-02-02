# save-edge-strips.lisp

Purpose
- Edge strip serialization and deserialization for streaming zone boundary entity data to clients near zone edges.

Key responsibilities
- `opposite-edge`: cardinal direction reversal for edge strip spatial logic.
- `entity-in-edge-strip-p`: spatial filter to check if an entity position falls within a zone's edge strip region.
- Edge strip serialization: produce mini-snapshots of adjacent zone entities near shared boundaries.
- Entity-type registry (`*edge-entity-specs*`): type-agnostic pipeline for adding new entity types to edge strips.
- Per-edge spatial filtering with configurable strip width in pixels.
- Compact player and NPC serialization within edge strip payloads.
- Edge strip deserialization for client-side rendering of adjacent zone entities.
- Zone dimension-aware boundary calculation using tile size and scale.

Load order
- Loaded fourth among save files: `save-serialize` -> `save-deserialize` -> `save-delta` -> `save-edge-strips` -> `save-validate`.
- Depends on `save-serialize` for compact serialization and `save-deserialize` for compact deserialization.
