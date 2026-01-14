# data.lisp

Purpose
- Load data-driven tunables, animation sets, and NPC archetypes.

Why we do it this way
- Designers should be able to change the game without editing logic.
- Data-driven content makes it easier to test different balance settings
  and prepare for tool-driven workflows.

Key responsibilities
- Read `data/game-data.lisp` without evaluating code, including keyword section
  headers (e.g., `:animation-sets`) and single-plist variants.
- Apply tunables to config variables.
- Register animation sets and NPC archetypes into hash tables.
- Validate section entries so malformed data fails fast.

Key functions
- `load-game-data`: entry point; clears registries, loads data, registers defaults.
- `ensure-game-data`: guard that loads once when needed.
- `get-animation-set`, `find-npc-archetype`, `npc-archetype-ids`: lookup helpers.

Walkthrough: startup data load
1) `load-game-data` reads `data/game-data.lisp` as plain data (single plist or
   section header style with keyword markers).
2) Tunables are applied to config variables.
3) Animation sets and archetypes are registered in hash tables.
4) Defaults are ensured so the game always has a baseline.

Common tunables
- `:player-collision-scale` and `:npc-collision-scale` to loosen/tighten collider fit.
- `:collision-edge-epsilon` to control edge contact tolerance.
- `:zone-path` to load a zone file from `data/`.
- `:zone-root` for editor zone file discovery and creation.
- `:zone-default-width`, `:zone-default-height`, `:zone-default-chunk-size` for new zones.
- `:editor-object-root` and `:editor-export-path` to drive editor palettes/exports.
- `:editor-tile-layer-id` and `:editor-collision-layer-id` to map editor brushes to layers.
- `:editor-move-speed` to tune editor camera navigation.

Example: new animation set
```lisp
;; In data/game-data.lisp (section header style)
:animation-sets
 (:npc-slime
  (:dir "../assets/3 Dungeon Enemies/5"
   :down-idle "D_Idle.png"
   :up-idle "U_Idle.png"
   :side-idle "S_Idle.png"))
```

Design note
- Registries are in-memory hash tables, which keeps lookup cheap during play
  without forcing gameplay systems to parse files every frame.
- Using multiple top-level sections keeps the data file readable while still
  supporting a single plist if you prefer that style.
