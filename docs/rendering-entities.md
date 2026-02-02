# rendering-entities.lisp

Purpose
- Player, NPC, and object rendering with viewport culling, name plates, health bars, and animation support.

Key responsibilities
- `entity-in-viewport-p`: generic viewport bounds check for any entity with margin.
- `npc-in-viewport-p` / `player-in-viewport-p`: type-specific viewport checks avoiding CLOS dispatch in hot render loops.
- Viewport culling to skip drawing off-screen entities.
- Name plate rendering above entities.
- Health bar rendering with color-coded HP display.
- Animation frame selection and sprite drawing.
- Entity sorting for correct draw order (y-sorting).
- Object/item rendering for dropped items and world pickups.

Load order
- Loaded third among rendering files: `rendering-core` -> `rendering-tiles` -> `rendering-entities` -> `rendering-ui`.
- Depends on `rendering-core` for draw helpers; uses entity structs from `types.lisp`.
