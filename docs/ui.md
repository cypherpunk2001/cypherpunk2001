# ui.lisp

Purpose: menu and HUD layout plus menu interaction.

Key responsibilities:
- Precompute menu layout constants and colors.
- Render menu and HUD labels (used by rendering).
- Handle menu clicks for quit/music/volume/debug/fullscreen.

Key functions:
- `make-ui`, `make-stamina-labels`.
- `update-ui-input`, `handle-menu-click`.

Notes:
- UI toggles `*debug-collision-overlay*` and fullscreen.
