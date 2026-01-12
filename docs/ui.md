# ui.lisp

Purpose
- Menu and HUD layout plus menu interaction.

Why we do it this way
- UI state is precomputed to avoid per-frame layout work.
- UI should toggle systems, not implement them.

What it does
- Builds layout constants (panel sizes, button positions, labels).
- Handles menu click actions (quit, music, volume, debug, fullscreen).
- Provides precomputed stamina labels to avoid consing.

Key functions
- `make-ui`, `make-stamina-labels`.
- `update-ui-input`, `handle-menu-click`.

Walkthrough: debug toggle
1) Player opens the menu with Escape.
2) Click on the debug checkbox.
3) UI flips `*debug-collision-overlay*`.
4) Rendering reads the flag and draws the overlay.

Design note
- UI toggles debug overlays without touching the rendering logic directly.
