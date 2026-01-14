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
- Paint tiles, paint collision, place objects, or place spawn points.
- Export the full zone to `data/`.
- Create, delete, cycle, rename, and resize zones from hotkeys.

Quick start (recommended flow)
1) Esc -> toggle Editor Mode.
2) Tile mode (`1`), then `Q/E` to pick a tile index.
3) Left-click to paint, right-click to erase.
4) Collision mode (`2`) to paint blocked tiles (LMB add, RMB remove).
5) Spawn mode (`4`) to place NPC spawns (LMB add, RMB remove).
6) Press `F5` to export the full zone to `data/`.

Zone files and cycling
- `F6` creates zones under `*zone-root*` and shows the full path in the status line.
- `F8/F9` cycle all zones under `*zone-root*` plus any recently loaded zones
  that live outside the root (they get pinned automatically).

What the on-screen text means
- `Zone` = current zone name, index, and size.
- `Mode` = current brush (tile/collision/object).
- `Tile` = current tile index out of the tileset atlas.
- `Object` / `Spawn` = current object sprite or NPC archetype selection.

Controls (default)
- `1` tile mode, `2` collision mode, `3` object mode, `4` spawn mode.
- `Q`/`E` cycle tiles, `Z`/`X` cycle objects or spawns.
- `WASD`/arrows move the editor camera.
- `LMB` paint, `RMB` erase.
- `F5` export the full zone.
- `F6` create zone, `F7` delete zone, `F8`/`F9` cycle zones.
- `F10` shrink zone, `F11` grow zone, `F12` rename zone.

Editor asset paths (configurable)
- Tileset atlas path: `*tileset-path*` (`:tileset-path` in `data/game-data.lisp`).
- Object palette root: `*editor-object-root*` (`:editor-object-root` in `data/game-data.lisp`).
- Export path: `*editor-export-path*` (`:editor-export-path` in `data/game-data.lisp`).
- Layer IDs used by brushes: `*editor-tile-layer-id*` and `*editor-collision-layer-id*`.
- Zone root (for create/delete/list): `*zone-root*` (`:zone-root` in `data/game-data.lisp`).

Spawn palette
- Spawn selections are pulled from NPC archetypes loaded in `data/game-data.lisp`.

Key responsibilities
- Maintain editor state (`editor` struct) and cached UI labels.
- Build an object palette from `*editor-object-root*` (PNG scan).
- Mutate zone layers and collision tiles in-place for immediate feedback.
- Update the world wall-map so collisions update live.
- Export zones with `zone-to-plist` and `zone-slice`.
- Reset player/NPC spawns and refresh minimap previews when the active zone changes.

Design note
- Painting only touches data: zones drive rendering and collisions, not the editor.
- Export defaults to `*editor-export-path*` unless a zone path is already set.
- Spawn mode writes `:spawns` into the zone file so runtime NPC placement can be data-driven.
