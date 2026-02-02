# save-deserialize.lisp

Purpose
- Deserialization functions for restoring game structs from plists and compact vectors, plus apply-state helpers for snapshot application.

Key responsibilities
- `deserialize-skill`, `deserialize-stat-block`, `deserialize-inventory-slot`: plist-to-struct restoration for nested game data.
- `deserialize-inventory` / `deserialize-equipment`: rebuild array-backed collections from saved plists.
- `deserialize-player-compact` / `deserialize-npc-compact`: restore entities from compact vector format.
- `apply-player-compact-direct` / `apply-npc-compact-direct`: update existing structs in-place from compact data (avoids allocation).
- `deserialize-player` / `deserialize-npc`: full plist deserialization for database loads.
- Dequantization helpers for coordinates, timers, and encoded state fields.
- Flag unpacking for player and NPC boolean fields.

Load order
- Loaded second among save files: `save-serialize` -> `save-deserialize` -> `save-delta` -> `save-edge-strips` -> `save-validate`.
- Depends on `save-serialize` for dequantization helpers and constants.
