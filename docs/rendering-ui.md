# rendering-ui.lisp

Purpose
- HUD overlays, minimap, debug information display, chat rendering, and inventory/equipment UI drawing.

Status
- **Implemented.** This doc reflects `src/rendering-ui.lisp` as of 2026-02-03.

Key responsibilities
- `draw-hud-log`: render HUD feedback messages with fade-out timers in screen space.
- Minimap rendering with player/NPC markers and preview spawn indicators.
- **Lazy minimap rebuild:** `ensure-minimap-data` rebuilds collisions/spawns only when `world-minimap-dirty` is set.
- Debug overlay: FPS, coordinates, entity counts, cache stats display.
- Chat message rendering with scrollback and input line.
- Inventory UI: grid-based slot rendering with item icons and counts.
- Equipment UI: paper‑doll style equipment slot display.
- Menu rendering: login, character select, and in‑game menus.
- Health/stamina/XP bar HUD elements.

Notes
- Minimap collision and spawn data are rebuilt lazily after zone transitions to avoid sim‑tick hitches.
- NPC minimap rendering is distance‑culled via `*minimap-npc-view-radius*`.

Load order
- Loaded last among rendering files: `rendering-core` -> `rendering-tiles` -> `rendering-entities` -> `rendering-ui`.
- Depends on all other rendering files and UI state from `ui.lisp`.
