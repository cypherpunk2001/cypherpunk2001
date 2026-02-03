# rendering-entities.lisp

Purpose
- Player, NPC, and object rendering with viewport culling, spatial grid acceleration, name plates, health bars, and animation support.

Status
- **Implemented.** This doc reflects `src/rendering-entities.lisp` as of 2026-02-03.

Key responsibilities
- `npc-in-viewport-p` / `player-in-viewport-p`: type-specific viewport checks avoiding CLOS dispatch in hot render loops.
- **Spatial grid culling for NPCs** via `zone-state-npc-grid` and `spatial-grid-query-rect-into`.
- Fallback rendering over `game-npcs` if no zone-state grid exists.
- NPC render distance clamp via `*entity-render-max-distance*`.
- Name plate rendering above entities.
- Health bar rendering with color‑coded HP display.
- Animation frame selection and sprite drawing.
- Player rendering always on top of NPCs (draw order preserved).
- Object/item rendering for dropped items and world pickups.

Data sources
- When a zone-state exists, NPCs are drawn from `zone-state-npcs` (kept in sync with `game-npcs`).
- When no zone-state exists, renderer falls back to `game-npcs`.

Load order
- Loaded third among rendering files: `rendering-core` -> `rendering-tiles` -> `rendering-entities` -> `rendering-ui`.
- Depends on `rendering-core` for draw helpers; uses entity structs from `types.lisp`.
