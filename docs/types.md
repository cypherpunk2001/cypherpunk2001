# types.lisp

Purpose
- Define all core state containers and the engine's generic interfaces.

Why we do it this way
- Game state is easiest to reason about when it is plain data. Systems can
  operate on this data without hidden side effects.
- Generics define the "capabilities" of entities (position, health, drawing)
  without forcing an inheritance-heavy object model.

Key structs
- `player`, `npc`: runtime entities with an `id`, `intent`, stats, inventory/equipment, animation/combat state, cached HUD stats and inventory lines, attack/follow/pickup targets, click markers, respawn timers, and spatial grid cell tracking (`grid-cell-x`/`grid-cell-y`).
- `skill`, `stat-block`, `stat-modifiers`: reusable stat containers for combat progression.
- `inventory`, `inventory-slot`: simple inventory storage for stackable items.
- `equipment`: equipped item IDs aligned to `*equipment-slot-ids*`.
- `id-source`: monotonic ID generator used for stable entity IDs.
- `world`: zone metadata, world graph, per-zone NPC cache, preview zone cache, wall-map data, collision bounds, derived sizes, minimap spawn previews, and minimap collision markers.
- `zone-state`: per-zone derived state (zone data + wall-map), spatial grids for players/NPCs, NPC index map for O(1) ID lookup, and a cached `zone-players` array for fast per-zone serialization.
- `audio`, `ui`, `render`, `assets`, `camera`: subsystem state (UI includes loading overlay timer, inventory toggle state, chat buffer/active flag, hovered NPC name, HUD/combat log ring buffers with HUD fade timers, minimap layout/colors, menu layout for music/debug/editor/fullscreen/prediction/tile-filter toggles plus logout/unstuck actions, and a context menu with target metadata; assets include object and item textures).
- `editor`: editor mode state (camera, tileset catalog/selection, selection brush size, layer selections, zone list/history, spawn palette, object palette).
- `combat-event`, `combat-event-queue`: event system for decoupling simulation from UI (server emits events, client renders them).
- `game`: top-level aggregator passed to update/draw functions (includes the primary player plus a `players` array, combat-events queue, client-side intent buffer, net role flag, queued net request list, client `net-player-id`, player index map for O(1) lookup, and interpolation/prediction state).

Key constructors
- `make-player`, `make-npc`: construct entities with default fields.
- `make-npcs`: spawn a pool using explicit zone spawns (zones without spawns produce no NPCs).
- `world-spawn-center`: returns a center point inside world collision bounds.
- `make-entities`: pack NPCs + players into a stable array.

Key helpers
- `rebuild-player-index-map`, `find-player-by-id-fast`: O(1) player lookup by ID.
- `rebuild-npc-index-map`, `find-npc-by-id-fast`: O(1) NPC lookup by ID within a zone.
- `add-player-to-zone-cache`, `remove-player-from-zone-cache`, `rebuild-zone-players-cache`: maintain per-zone player arrays for serialization.

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

Combat event queue (client/server separation)
- `make-combat-event`: creates an event with :type and :text
- `emit-combat-log-event`: queues a :combat-log event
- `emit-hud-message-event`: queues a :hud-message event
- `pop-combat-events`: returns all events and clears the queue
- This decouples server-side simulation from client-side UI rendering

Network request queue
- `queue-net-request`: push a client request (currently used for `:logout`) for network transport
- `drain-net-requests`: return queued requests (FIFO) and clear the queue

Design note
- Arrays are used for entity collections to keep iteration fast and predictable.
- Spawn spacing accounts for collider sizes so entities don't overlap at start.
- `world-zone-label` is cached for HUD display and updates on zone swaps.
- Stable entity IDs keep snapshot serialization and future networking deterministic.
