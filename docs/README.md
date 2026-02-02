# Documentation Guide

These docs are meant to teach the architecture and the design choices behind this RPG prototype.
The goal is to help you learn the reasoning, not just the API.

## Why This Architecture
- **Client/Server split**: Server is authoritative; clients send intents, receive snapshots.
- **Systems over scripts**: behavior lives in systems, not in the main loop.
- **Intent layer**: input and AI both write intent; movement/combat consume it.
- **Data-driven**: tunables and archetypes live in `data/game-data.lisp`.
- **Rendering is read-only**: the render pipeline never mutates gameplay state.
- **Storage abstraction**: game logic never calls database directly (uses db.lisp).

## How To Read

**Architecture overview:**
1) `docs/net.md` (UDP protocol, client/server split, snapshots)
2) `docs/db.md` (persistence tiers, storage abstraction, dirty flags)
3) `docs/save.md` (serialization format, durable vs ephemeral)

**Game loop and data flow:**
1) `docs/main.md` (client orchestration)
2) `docs/server.md` (server-side game loop)
3) `docs/types.md` and `docs/intent.md` (data layout and action layer)

**Core systems:**
1) `docs/progression.md` (stats + XP + loot)
2) `docs/input.md`, `docs/ai.md`, `docs/movement.md`, `docs/combat.md` (core systems)
3) `docs/chat.md` (chat processing)

**World and rendering:**
1) `docs/zone.md`, `docs/world-graph.md` (world data + travel)
2) `docs/rendering.md`, `docs/editor.md` (draw pipeline + editor)
3) `docs/ui.md` and `docs/audio.md` (player-facing polish)

**Configuration and data:**
1) `docs/config.md` (all tunables, organized by category)
2) `docs/data.md` (data-driven behavior, archetypes)
3) `docs/migrations.md` (schema versioning, migration functions)

**Supporting files:**
1) `docs/utils.md` and `docs/package.md` (helpers)
2) `docs/admin.md` (admin commands)

## Design Principles Used Here
- Server is authoritative; clients are untrusted.
- Behavior lives in systems, not in the main loop.
- Entities hold data; systems consume data and intent.
- Rendering never drives gameplay.
- Data is external and editable (tunable configs and archetypes).
- Performance matters: reuse objects and cull work early.
- All persistence goes through the storage abstraction layer.
- State is classified as durable (persisted) or ephemeral (lost on crash).

## Flow Diagrams

Client/Server flow
```
Client                          Server
------                          ------
Input -> Intent
        -----> UDP :intent ---->
                                Validate + Apply
                                Simulation tick
        <---- UDP :snapshot <---
Apply snapshot
Render
```

Update flow (one simulation tick)
```
Receive intents from clients
   |
   v
Validate + sync targets (server authority)
   |
   v
Movement + Combat + AI
   |
   v
Animation + Effects
   |
   v
Broadcast snapshots
```

Render flow (client, one frame)
```
World (tiles + walls) -> Entities -> HUD -> Menu
```

Persistence flow
```
Game event (death, level-up, etc.)
   |
   v
Tier-1: Immediate save (critical)
   or
Tier-2: Mark dirty (batched every 30s)
   or
Tier-3: Logout save (session end)
   |
   v
serialize-player (save.lisp)
   |
   v
storage-save (db.lisp)
   |
   v
Redis / Memory backend
```

Data flow (startup)
```
config.lisp defaults
   |
   v
data/game-data.lisp overrides
   |
   v
registries (animation sets, archetypes)
   |
   v
world + assets
```

## File Index

**Networking & Persistence**
- [net.md](net.md) - UDP protocol, client/server, snapshots, interpolation, prediction
  - [net-protocol.md](net-protocol.md) - Message formats, encode/decode, validation
  - [net-auth.md](net-auth.md) - Login/register, auth queues, rate limiting
  - [net-snapshot.md](net-snapshot.md) - Snapshot transmission, delta compression
  - [net-server.md](net-server.md) - Server UDP loop, dispatch, connection tracking
  - [net-client.md](net-client.md) - Client networking loop, login UI
- [db.md](db.md) - Storage abstraction, Redis/memory backends, write tiers, dirty flags
  - [db-storage.md](db-storage.md) - Storage abstraction, backend selection
  - [db-players.md](db-players.md) - Player save/load, dirty flags, session management
  - [db-accounts.md](db-accounts.md) - Account creation/verification
  - [db-admin.md](db-admin.md) - Admin tooling, migrate-all, metrics
- [save.md](save.md) - Serialization format, durable vs ephemeral classification
  - [save-serialize.md](save-serialize.md) - Serialize functions, compact vectors
  - [save-deserialize.md](save-deserialize.md) - Deserialize functions, apply-state
  - [save-delta.md](save-delta.md) - Delta encoding/decoding
  - [save-edge-strips.md](save-edge-strips.md) - Edge strip serialization
  - [save-validate.md](save-validate.md) - Schema checks, 4-outcome validation
- [migrations.md](migrations.md) - Schema versioning, migration functions
- [server.md](server.md) - Server-side game loop

**Core Systems**
- [types.md](types.md) - Data structures (player, NPC, world, etc.)
- [intent.md](intent.md) - Intent layer (what entities want to do)
- [input.md](input.md) - Client input handling
- [movement.md](movement.md) - Physics, collision, zone transitions
  - [movement-core.md](movement-core.md) - Movement integration, intent processing
  - [movement-collision.md](movement-collision.md) - Collision checks, wall maps
  - [movement-transition.md](movement-transition.md) - Zone transitions, NPC carry-across
  - [movement-preview.md](movement-preview.md) - Preview zones, camera edge checks
- [combat.md](combat.md) - Damage, HP, death, XP awards
- [ai.md](ai.md) - NPC behaviors (idle, wander, aggressive, flee)
- [progression.md](progression.md) - Stats, XP, levels, inventory, equipment
- [chat.md](chat.md) - Chat message processing

**World & Rendering**
- [zone.md](zone.md) - Zone loading, tile layers, collision maps
- [world-graph.md](world-graph.md) - Zone connections, edge transitions
- [rendering.md](rendering.md) - Draw pipeline, sprites, tiles
  - [rendering-core.md](rendering-core.md) - Core draw helpers, asset loading
  - [rendering-tiles.md](rendering-tiles.md) - Map/tileset rendering, chunk caching
  - [rendering-entities.md](rendering-entities.md) - Player/NPC/object rendering, culling
  - [rendering-ui.md](rendering-ui.md) - HUD, minimap, inventory, menus
- [editor.md](editor.md) - Zone editor mode
  - [editor-core.md](editor-core.md) - Editor state, modes, primary loop
  - [editor-tools.md](editor-tools.md) - Brush/tools, hotkeys
  - [editor-io.md](editor-io.md) - Loading/saving map data

**UI & Polish**
- [ui.md](ui.md) - Menus, HUD, login screen
- [audio.md](audio.md) - Music playback, volume control

**Configuration**
- [config.md](config.md) - All tunables (immediate effect vs restart required)
- [data.md](data.md) - Data-driven behavior, archetypes, tunables

**Supporting**
- [main.md](main.md) - Client entry point
- [admin.md](admin.md) - Admin commands and roadmap
- [utils.md](utils.md) - Helper functions
- [package.md](package.md) - Package definition

**Reference**
- [raylib_cheatsheet.md](raylib_cheatsheet.md)
- [claw-raylib-readme.org](claw-raylib-readme.org)
