# save-delta.lisp

Purpose
- Delta encoding and decoding for network snapshots, transmitting only dirty entities to minimize bandwidth usage.

Key responsibilities
- `serialize-game-state-delta`: build delta snapshot containing only entities with dirty flags set since last acknowledged baseline.
- Dirty player and NPC collection with compact serialization for changed entities only.
- Zone object serialization (dropped items, respawning pickups) included in delta payloads.
- Delta snapshot format (`:format :delta-v5`) with sequence numbers for client acknowledgment tracking.
- Edge strip integration for zone boundary entity streaming.
- Delta deserialization and application on client side.
- Baseline sequence tracking for reliable delta delivery.

Load order
- Loaded third among save files: `save-serialize` -> `save-deserialize` -> `save-delta` -> `save-edge-strips` -> `save-validate`.
- Depends on `save-serialize` for compact serialization and `save-deserialize` for compact deserialization.
