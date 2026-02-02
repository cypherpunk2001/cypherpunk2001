# rendering-ui.lisp

Purpose
- HUD overlays, minimap, debug information display, chat rendering, and inventory/equipment UI drawing.

Key responsibilities
- `draw-combat-log`: render combat log lines when debug overlay is enabled.
- `draw-hud-log`: render HUD feedback messages with fade-out timers in screen space.
- Minimap rendering with player position and zone preview indicators.
- Debug overlay: FPS, coordinates, entity counts, cache stats display.
- Chat message rendering with scrollback and input line.
- Inventory UI: grid-based slot rendering with item icons and counts.
- Equipment UI: paper-doll style equipment slot display.
- Menu rendering: login, character select, and in-game menus.
- Health/stamina/XP bar HUD elements.

Load order
- Loaded last among rendering files: `rendering-core` -> `rendering-tiles` -> `rendering-entities` -> `rendering-ui`.
- Depends on all other rendering files and UI state from `ui.lisp`.
