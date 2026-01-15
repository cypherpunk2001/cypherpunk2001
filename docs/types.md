# types.lisp

Purpose
- Define all core state containers and the engine's generic interfaces.

Why we do it this way
- Game state is easiest to reason about when it is plain data. Systems can
  operate on this data without hidden side effects.
- Generics define the "capabilities" of entities (position, health, drawing)
  without forcing an inheritance-heavy object model.

Key structs
- `player`, `npc`: runtime entities with an `id`, `intent`, stats, animation/combat state, cached HUD stats lines, attack/follow targets, click markers, and NPC respawn timers.
- `skill`, `stat-block`, `stat-modifiers`: reusable stat containers for combat progression.
- `inventory`, `inventory-slot`: simple inventory storage for stackable items.
- `id-source`: monotonic ID generator used for stable entity IDs.
- `world`: zone metadata, world graph, per-zone NPC cache, preview zone cache, wall-map data, collision bounds, derived sizes, minimap spawn previews, and minimap collision markers.
- `audio`, `ui`, `render`, `assets`, `camera`: subsystem state (UI includes loading overlay timer, minimap layout/colors, a combat log ring buffer, and a context menu).
- `editor`: editor mode state (camera, tileset catalog/selection, selection brush size, layer selections, zone list/history, spawn palette).
- `game`: top-level aggregator passed to update/draw functions.

Key constructors
- `make-player`, `make-npc`: construct entities with default fields.
- `make-npcs`: spawn a pool using explicit zone spawns (zones without spawns produce no NPCs).
- `world-spawn-center`: returns a center point inside world collision bounds.
- `make-entities`: pack NPCs + player into a stable array.

Key generics
- Combat: `combatant-position`, `combatant-health`, `combatant-apply-hit`.
- Identity: `entity-id`.
- Stats: `combatant-stats`.
- Rendering: `draw-entity`.
- Animation/effects: `update-entity-animation`, `combatant-update-hit-effect`.

Walkthrough: entity lifecycle
1) `make-player` / `make-npc` constructs the struct.
2) Input/AI populates intent each frame.
3) Systems update position, health, and animation fields.
4) Rendering reads the state to draw.

Design note
- Arrays are used for entity collections to keep iteration fast and predictable.
- Spawn spacing accounts for collider sizes so entities don't overlap at start.
- `world-zone-label` is cached for HUD display and updates on zone swaps.
- Stable entity IDs keep snapshot serialization and future networking deterministic.
