# editor-tools.lisp

Purpose
- Brush/paint logic, hotkeys, tool-specific interaction, tileset picker, and mode switching for the editor.

Key responsibilities
- Tile/spawn/object/tileset selection adjustment (`editor-adjust-selection`, `editor-adjust-tileset`, `editor-adjust-spawn`, `editor-adjust-object`).
- Tileset preview panel layout and mouse-over detection (`editor-tileset-preview-layout`, `editor-mouse-over-tileset-preview-p`).
- Tileset selection from the preview panel (`editor-handle-tileset-picker`, `editor-update-tileset-selection`).
- Mouse-to-tile coordinate conversion (`editor-mouse-tile`).
- Camera movement (`editor-update-camera`).
- Mode switching via numeric keys (`editor-update-mode`).
- Multi-tile brush painting for tile, collision, object, and spawn modes (`editor-apply-paint`).
- Layer tile clearing across tilesets (`editor-clear-layer-tiles`).

Load order
- Loaded after `editor-core`, before `editor-io` and `editor`.
- Depends on core helpers like `editor-current-tileset`, `update-editor-labels`, `editor-clamp-selection`, etc.
