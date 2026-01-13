# rendering.lisp

Purpose
- Load visual assets and render the world, entities, HUD, and menus.

Why we do it this way
- Rendering reads state, it does not create it. This keeps logic deterministic
  and makes it easy to add a headless/server mode later.
- The draw pipeline is tile-based so we can add chunk culling and caching later.

Pipeline overview
1) Load textures in `load-assets`.
2) Draw world layers, zone objects, and debug overlays in `draw-world`.
3) Draw entities via `draw-entity` (NPCs and player).
4) When `*debug-npc-logs*` is on, NPCs render an AI text overlay (state/hits).
5) Draw HUD, editor overlays, and menu overlays.

Key functions
- `load-assets`, `unload-assets`.
- `draw-world`, `draw-zone-objects`.
- `draw-player`, `draw-npc`, `draw-health-bar`, `draw-hit-effect`.
- `draw-hud`, `draw-menu`, `draw-game`.

Walkthrough: world rendering
1) Compute visible tile bounds from camera and player position.
2) Draw floor tiles, zone layers, and wall map tiles.
3) Draw placed zone objects inside the view bounds.
4) If debug is enabled, overlay collision/bounds grid.
5) Draw player and NPCs in world space.
6) Draw HUD, editor overlays, and pause menu in screen space.

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
- NPC AI debug text is only drawn when explicitly enabled, keeping the
  default render path clean and fast.
- The camera target follows the editor camera when Editor Mode is active.
