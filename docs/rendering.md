# rendering.lisp

Purpose
- Load visual assets and render the world, entities, HUD, and menus.

Why we do it this way
- Rendering reads state, it does not create it. This keeps logic deterministic
  and makes it easy to add a headless/server mode later.
- The draw pipeline is tile-based so we can add chunk culling and caching later.

Pipeline overview
1) Load textures in `load-assets` (tileset columns derive from texture width; required textures fail fast, optional object/item sprites are skipped with warnings).
2) Draw world layers and debug overlays in `draw-world` (layers can bind their own tilesets).
3) Draw placed zone objects and entities (NPCs + player).
4) When `*debug-npc-logs*` is on, NPCs render an AI text overlay (state/hits).
5) Draw click markers above entities in world space, then HUD (stamina + zone label + stats + hover name + HUD log), inventory overlay, minimap (centered on player with adjacent zone spawn previews), loading overlay, editor overlays (including the tileset preview), debug combat log, context menu, and menu overlays.

Key functions
- `load-assets`, `unload-assets` (logs asset counts in verbose mode).
- `draw-world`, `draw-zone-objects`.
- `draw-player`, `draw-npc`, `draw-health-bar`, `draw-hit-effect`.
- `draw-hud`, `draw-inventory`, `draw-combat-log`, `draw-click-marker`, `draw-context-menu`,
  `draw-minimap`, `draw-editor-tileset-preview`, `draw-menu`, `draw-game`.
- `draw-loading-overlay` for zone swap feedback.

Walkthrough: world rendering
1) Compute visible tile bounds from camera and player position.
2) Draw floor tiles and zone layers; wall map tiles render only when no zone is loaded.
3) Draw placed zone objects in world space.
4) If debug is enabled, overlay collision/bounds grid.
5) Draw player and NPCs in world space.
6) Draw click markers in world space above entities.
7) Draw HUD, inventory overlay, minimap, loading overlay, editor overlays, and pause menu in screen space.

Example: draw flow
```lisp
(raylib:with-mode-2d camera
  (draw-world world render assets camera player npcs ui)
  (loop :for entity :across entities
        :when (entity-in-viewport-p entity camera-x camera-y zoom margin-x margin-y)
        :do (draw-entity entity assets render)))
```

Design note
- Entity rendering uses viewport culling via `entity-in-viewport-p` to skip off-screen
  entities. A margin equal to sprite half-size prevents pop-in at screen edges.
- The debug overlay draws both collision tiles and map bounds, which helps
  validate that collision and visuals are aligned.
- NPC AI debug text is only drawn when explicitly enabled, keeping the
  default render path clean and fast.
- Zone layers can reference specific tilesets so multi-sheet maps render correctly.
- The camera target follows the editor camera when Editor Mode is active.
- The HUD reads the world zone label so you always know which zone is active.
- The HUD shows the hovered NPC name at the top-center when the cursor is over one.
- The HUD log area shows gameplay feedback when the debug overlay is off, fading lines out over time.
- When chat input is active, the HUD draws a "Say:" line above the HUD log area.
- The inventory overlay renders a grid panel with item sprites and stack counts.
- The minimap recenters on the player, so you can always click ahead to set a target.
- Minimap NPC rendering uses distance-based culling via `*minimap-npc-view-radius*` to
  skip NPCs far from the player, reducing overhead with many entities.
- The pause menu includes music controls, debug/editor toggles, and logout/unstuck actions.
- Object/item sprites treat an opaque border color as a transparency key to remove solid backdrops.
- Zone objects render only when active (count > 0 and no respawn timer) so pickups can disappear.
- The minimap draws small preview markers for spawns in adjacent zones so you can
  see potential enemies before crossing. Previews render while you are pushing
  against a connected edge or standing within `*minimap-preview-edge-tiles*` tiles
  of one to avoid confusing them with in-zone NPCs.
- Click marker line thickness is driven by `*click-marker-thickness*` so feedback remains readable.
- Context menu options highlight on hover for clearer selection feedback.
- When a preview zone is cached, `draw-world` renders its layers offset beyond any
  edges or corners that the camera view extends past, so approaching a boundary feels
  continuous instead of a hard cutoff.
- Collision tiles render as faint minimap markers so navigational blockers are visible,
  including zone boundary tiles to show world edges (internal edges with world-graph
  exits are suppressed).
