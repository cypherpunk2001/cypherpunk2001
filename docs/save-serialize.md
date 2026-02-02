# save-serialize.lisp

Purpose
- All serialization functions for converting game structs to plists and compact vector formats, plus shared constants and vector pool management.

Key responsibilities
- Shared save format constants (`*save-format-version*`) and compact vector size parameters.
- `serialize-skill`, `serialize-stat-block`, `serialize-inventory-slot`: struct-to-plist conversion for nested game data.
- `serialize-player-compact` / `serialize-npc-compact`: compact vector serialization for bandwidth-efficient snapshots.
- Quantization/encoding helpers for coordinates, timers, animation state, and facing direction.
- Player and NPC flag packing into single integers for compact wire format.
- Vector pool management (`*player-vector-pool*`) to reduce allocation during hot snapshot paths.
- Zone ID encoding for compact representation.
- Object serialization for world items and pickups.

Load order
- Loaded first among save files: `save-serialize` -> `save-deserialize` -> `save-delta` -> `save-edge-strips` -> `save-validate`.
- All other save files depend on serialization helpers and constants defined here.
