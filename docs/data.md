# data.lisp

Purpose
- Load data-driven tunables, animation sets, NPC archetypes, items, and loot tables.

Why we do it this way
- Designers should be able to change the game without editing logic.
- Data-driven content makes it easier to test different balance settings
  and prepare for tool-driven workflows.

Key responsibilities
- Read `data/game-data.lisp` without evaluating code, including keyword section
  headers (e.g., `:animation-sets`) and single-plist variants.
- Apply tunables to config variables.
- Register animation sets, NPC archetypes, items, and loot tables into hash tables.
- Validate section entries so malformed data fails fast.

Key functions
- `load-game-data`: entry point; clears registries, loads data, registers defaults.
- `ensure-game-data`: guard that loads once when needed.
- `get-animation-set`, `find-npc-archetype`, `npc-archetype-ids`: lookup helpers.
- `find-item-archetype`, `find-loot-table`: loot/inventory lookups.

Walkthrough: startup data load
1) `load-game-data` reads `data/game-data.lisp` as plain data (single plist or
   section header style with keyword markers).
2) Tunables are applied to config variables.
3) Animation sets and archetypes are registered in hash tables.
4) Defaults are ensured so the game always has a baseline.

Common tunables
- `:player-collision-scale` and `:npc-collision-scale` to loosen/tighten collider fit.
- `:sim-tick-seconds` and `:sim-max-steps-per-frame` to tune fixed-step simulation timing.
- `:collision-edge-epsilon` to control edge contact tolerance.
- `:player-base-attack`, `:player-base-strength`, `:player-base-defense`, `:player-base-hitpoints`.
- `:player-training-mode`, `:stat-xp-per-level`, `:stat-max-level`, `:xp-per-damage`.
- `:combat-hitpoints-xp-multiplier` to award HP XP alongside focused training modes.
- `:inventory-size` to control player inventory slots.
- `:zone-path` to load a zone file from `data/`.
- `:zone-root` for editor zone file discovery and creation.
- `:zone-default-width`, `:zone-default-height`, `:zone-default-chunk-size` for new zones.
- `:world-graph-path` to load the world graph edges for zone transitions.
- `:zone-loading-seconds` to control how long the loading overlay displays.
- `:minimap-width`, `:minimap-height`, `:minimap-padding`, `:minimap-point-size`,
  `:minimap-preview-edge-tiles` for minimap layout and preview distance.
- `:editor-export-path` for editor exports.
- `:editor-start-enabled` to toggle editor mode at startup.
- `:music-volume-steps` and `:music-default-volume-level` for startup volume.
- `:tileset-path` and `:tileset-columns` for the active tileset atlas.
- `:floor-tile-index` for the base floor fill (use `0` for no fill).
- `:editor-tileset-paths` or `:editor-tileset-root` to build the editor tileset catalog.
- `:editor-tileset-preview-padding`, `:editor-tileset-preview-max-width`,
  `:editor-tileset-preview-max-height` for the tileset preview layout.
- `:editor-tile-layer-id`, `:editor-collision-layer-id`, and `:editor-object-layer-id` to map editor brushes to layers.
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

Example: item + loot table
```lisp
:items
 (:coins (:name "Coins" :stack-size 9999))

:loot-tables
 (:rat
  (:rolls 1
   :entries
   ((:coins 10 1 5))))
```

Design note
- Registries are in-memory hash tables, which keeps lookup cheap during play
  without forcing gameplay systems to parse files every frame.
- Using multiple top-level sections keeps the data file readable while still
  supporting a single plist if you prefer that style.
- The loader defensively re-parses single-form data so legacy layouts still load.
