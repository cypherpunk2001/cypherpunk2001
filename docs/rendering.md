# rendering.lisp

Purpose
- Load visual assets and render the world, entities, HUD, and menus.

Why we do it this way
- Rendering reads state, it does not create it. This keeps logic deterministic
  and makes it easy to add a headless/server mode later.
- Chunk culling reduces draw calls and keeps large maps performant.

Pipeline overview
1) Load textures in `load-assets`.
2) Draw world layers and debug overlays in `draw-world`.
3) Draw entities via `draw-entity` (NPCs and player).
4) Draw HUD and menu overlays.

Key functions
- `load-assets`, `unload-assets`.
- `draw-world`, `draw-map-layer`.
- `draw-player`, `draw-npc`, `draw-health-bar`, `draw-hit-effect`.
- `draw-hud`, `draw-menu`, `draw-game`.

Walkthrough: world rendering
1) Compute visible tile bounds from camera and player position.
2) Draw floor tiles and TMX layers with chunk culling.
3) If debug is enabled, overlay collision/bounds grid.
4) Draw player and NPCs in world space.
5) Draw HUD and pause menu in screen space.

Example: draw flow
```lisp
(raylib:with-mode-2d camera
  (draw-world world render assets camera player npcs ui)
  (loop :for entity :across entities
        :do (draw-entity entity assets render)))
```

Design note
- The debug overlay draws both collision tiles and map bounds, which helps
  validate that collision and visuals are aligned.
