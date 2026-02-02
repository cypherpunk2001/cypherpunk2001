# editor-core.lisp

Purpose
- Editor state, initialization, shared variables/constants, labels, and helper functions used by all editor subsystems.

Key responsibilities
- Define the `editor-tileset` struct for tileset metadata.
- Path resolution helpers (`resolve-editor-root`, `resolve-zone-root`, `normalize-zone-path`, etc.).
- Tileset catalog: loading, indexing, texture management, and metric computation.
- Zone file collection, refresh, and tracking (including out-of-root pinning).
- Spawn and object palette loading from NPC/object archetypes.
- Editor construction (`make-editor`) and mode toggling (`toggle-editor-mode`).
- Camera target selection and status label management.
- Label refresh (`update-editor-labels`, `update-editor-zone-label`).
- Selection clamping and floor index clamping.
- Active tileset switching (`editor-set-active-tileset`).
- Zone sync after external zone changes (`editor-sync-zone`).
- Default layer tileset assignment (`editor-assign-default-layer-tilesets`).
- Tileset unloading on editor teardown.

Load order
- Loaded first among editor files (before `editor-tools`, `editor-io`, `editor`).
- Other editor files depend on functions defined here.
