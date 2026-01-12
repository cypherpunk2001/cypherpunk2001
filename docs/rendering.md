# rendering.lisp

Purpose: asset loading and draw pipeline.

Key responsibilities:
- Load/unload textures for players, NPCs, blood, and tilesets.
- Render world layers with chunk culling and debug overlays.
- Render entities, HUD, and menu.

Key functions:
- `load-assets`, `unload-assets`.
- `draw-world`, `draw-map-layer`.
- `draw-player`, `draw-npc`, `draw-hit-effect`, `draw-health-bar`.
- `draw-hud`, `draw-menu`, `draw-game`.

Notes:
- Rendering reads world state and intent; it does not mutate game logic.
