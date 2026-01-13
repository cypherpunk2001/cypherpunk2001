# editor.lisp

Purpose
- Provide an in-game, Minecraft-style map editor that paints tiles/objects and exports zones.

Why we do it this way
- Editing in the running game keeps collisions and visuals aligned.
- We can author content with only PNG assets, no external tooling required.
- Exporting to our Lisp zone format keeps the pipeline data-driven and inspectable.

Core UX
- Toggle Editor Mode from the Escape menu.
- Move the editor camera freely to inspect tiles.
- Paint tiles, paint collision, or place objects.
- Export the current selection (or full zone) to `data/`.

Quick start (recommended flow)
1) Esc -> toggle Editor Mode.
2) Tile mode (`1`), then `Q/E` to pick a tile index.
3) Left-click to paint, right-click to erase.
4) Collision mode (`2`) to paint blocked tiles (LMB add, RMB remove).
5) Press `F5` to export the full zone to `data/`.

Selection export (optional)
1) Hover a tile, press `B` to set selection start.
2) Hover another tile, press `N` to set selection end.
3) Press `F5` to export just the selection.
4) Press `C` to clear the selection.

What the on-screen text means
- `Mode` = current brush (tile/collision/object).
- `Tile` = current tile index out of the tileset atlas.
- `Object` = current object sprite from the palette folder.

Controls (default)
- `1` tile mode, `2` collision mode, `3` object mode.
- `Q`/`E` cycle tiles, `Z`/`X` cycle objects.
- `WASD`/arrows move the editor camera.
- `LMB` paint, `RMB` erase.
- `B` set selection start, `N` set selection end, `C` clear selection.
- `F5` export the selection (or full zone).

Key responsibilities
- Maintain editor state (`editor` struct) and cached UI labels.
- Build an object palette from `*editor-object-root*` (PNG scan).
- Mutate zone layers and collision tiles in-place for immediate feedback.
- Update the world wall-map so collisions update live.
- Export zones with `zone-to-plist` and `zone-slice`.

Design note
- Painting only touches data: zones drive rendering and collisions, not the editor.
- Selection export is chunk-aware, so we can evolve to streaming later.
- Export defaults to `*editor-export-path*` unless a zone path is already set.
