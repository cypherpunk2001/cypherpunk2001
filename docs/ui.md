# ui.lisp

Purpose
- Menu and HUD layout plus menu interaction.

Why we do it this way
- UI state is precomputed to avoid per-frame layout work.
- UI should toggle systems, not implement them.

What it does
- Builds layout constants (panel sizes, button positions, labels).
- Handles menu click actions (quit, music, volume, debug, editor mode, fullscreen).
- Exposes Save/Load menu actions for snapshot testing.
- Provides precomputed stamina labels to avoid consing.
- Tracks a short loading overlay timer for zone transitions.
- Tracks inventory overlay visibility for the `I` hotkey.
- Stores minimap layout/colors (including collision marker color) for rendering and input hit-testing.
- Caches combat log lines for the debug overlay plus a lightweight HUD feedback log with per-line fade timers.
- Tracks a right-click context menu (position, options, target type, and Walk/Attack/Follow/Pick up/Examine/Drop labels, including inventory examine).
- Tracks the hovered NPC name for top-middle HUD display.
- Provides inventory grid layout and slot hit-testing for context menus.
- Context menu selection logic supports follow-only layouts when attack is unavailable.

Key functions
- `make-ui`, `make-stamina-labels`.
- `update-ui-input`, `handle-menu-click` (returns menu actions like editor toggle).
- `ui-push-combat-log` for debug overlay logging, `ui-push-hud-log` and `update-ui-hud-log` for gameplay feedback.
- `open-context-menu`, `close-context-menu`, `handle-context-menu-click`, `context-menu-action-for-index`.
- `inventory-grid-layout`, `inventory-slot-at-screen` for inventory overlay hit-testing.
- `ui-trigger-loading`, `update-ui-loading` for zone transition overlays.

Walkthrough: debug toggle
1) Player opens the menu with Escape.
2) Click on the debug checkbox.
3) UI flips `*debug-collision-overlay*` and `*debug-npc-logs*`.
4) Rendering reads the flag and draws the overlay.

Walkthrough: zone loading overlay
1) Movement triggers a zone transition.
2) `ui-trigger-loading` starts a short timer.
3) `update-ui-loading` counts down each frame.
4) Rendering draws "Loading..." while the timer is active.

Design note
- UI toggles debug overlays without touching the rendering logic directly.
- Editor Mode is exposed as a checkbox, but gameplay systems own the behavior.
