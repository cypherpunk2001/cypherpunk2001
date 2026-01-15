# editor.lisp

Purpose
- Provide an in-game, Minecraft-style map editor that paints tiles/objects, places spawns, and exports zones.

Why we do it this way
- Editing in the running game keeps collisions and visuals aligned.
- We can author content with only PNG assets, no external tooling required.
- Exporting to our Lisp zone format keeps the pipeline data-driven and inspectable.

Core UX
- Toggle Editor Mode from the Escape menu.
- Move the editor camera freely to inspect tiles.
- Paint base tiles, paint collision tiles, paint top-layer objects, or place spawn points.
- Export the full zone to `data/`.
- Create, delete, and cycle zones from hotkeys.

Quick start (recommended flow)
1) Esc -> toggle Editor Mode.
2) Tile mode (`1`), use `Q/E` to cycle tileset sheets, then click a tile in the sheet preview.
3) Left-click to paint, right-click to erase.
4) Collision mode (`2`) to paint blocked tiles (LMB add, RMB remove).
5) Object mode (`3`) to paint detail tiles above the base (LMB add, RMB remove).
6) Spawn mode (`4`) to place NPC spawns (LMB add, RMB remove); `Q/E` cycles spawn types.
7) Press `F5` to export the full zone to `data/`.

Zone files and cycling
- `F6` creates zones under `*zone-root*` and shows the full path in the status line.
- `F8/F9` cycle all zones under `*zone-root*` plus any recently loaded zones
  that live outside the root (they get pinned automatically).

What the on-screen text means
- `Zone` = current zone name, index, and size.
- `Mode` = current brush (tile/collision/object/spawn).
- `Sheet` = active tileset sheet and its index in the catalog.
- `Tile` = current tile index out of the tileset sheet (shows brush size when >1x1).
- `Layer` = layer ID used by the active paint mode.
- `Spawn` = current NPC archetype selection.

Controls (default)
- `1` tile mode (base), `2` collision mode (blocking + top), `3` object mode (top), `4` spawn mode.
- `Q`/`E` cycle tileset sheets (tile/collision/object modes), click a tile to select it.
- `Shift` + `LMB` in the sheet selects a rectangular brush for multi-tile painting.
- `Q`/`E` cycle spawns (spawn mode).
- `WASD`/arrows move the editor camera.
- `LMB` paint, `RMB` erase.
- `F5` export the full zone.
- `F6` create zone, `F7` delete zone, `F8`/`F9` cycle zones.

Editor asset paths (configurable)
- Tileset sheets: `*editor-tileset-paths*` or `*editor-tileset-root*` (`:editor-tileset-paths` /
  `:editor-tileset-root` in `data/game-data.lisp`).
- Active tileset path: `*tileset-path*` (`:tileset-path` in `data/game-data.lisp`).
- Export path: `*editor-export-path*` (`:editor-export-path` in `data/game-data.lisp`).
- Layer IDs used by brushes: `*editor-tile-layer-id*`, `*editor-collision-layer-id*`,
  and `*editor-object-layer-id*`.
- Zone root (for create/delete/list): `*zone-root*` (`:zone-root` in `data/game-data.lisp`).

Spawn palette
- Spawn selections are pulled from NPC archetypes loaded in `data/game-data.lisp`.

Key responsibilities
- Maintain editor state (`editor` struct) and cached UI labels.
- Build a tileset catalog from `*editor-tileset-paths*`/`*editor-tileset-root*` and sync the active sheet.
- Mutate zone layers and collision tiles in-place for immediate feedback.
- Update the world wall-map so collisions update live.
- Export zones with `zone-to-plist` and `zone-slice`.
- Reset player/NPC spawns and refresh minimap previews when the active zone changes.

Design note
- Painting only touches data: zones drive rendering and collisions, not the editor.
- Use object mode for sprites with transparency so the base layer stays visible.
- Collision mode paints the selected tile into the collision layer and updates the collision map.
- Export defaults to `*editor-export-path*` unless a zone path is already set.
- Spawn mode writes `:spawns` into the zone file so runtime NPC placement can be data-driven.
- Tile/object layers remember the tileset ID used for each layer, so switching sheets
  does not reinterpret existing tiles.
