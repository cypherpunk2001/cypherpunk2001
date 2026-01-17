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
- Register animation sets, NPC archetypes (including descriptions), items (including optional sprites and descriptions), object archetypes, and loot tables into hash tables.
- Validate section entries so malformed data fails fast.
- Treat data read/parse/apply failures as non-fatal (warn + fall back to defaults).
- Item archetypes can include equipment slots and stat modifier values for progression.
- Object archetypes can include respawn cooldowns for repeatable pickups.

Key functions
- `load-game-data`: entry point; clears registries, loads data, registers defaults, and logs counts in verbose mode.
- `ensure-game-data`: guard that loads once when needed.
- `get-animation-set`, `find-npc-archetype`, `npc-archetype-ids`: lookup helpers.
- `find-item-archetype`, `item-archetype-ids`, `find-object-archetype`, `find-object-archetype-by-item`,
  `find-loot-table`: loot/inventory lookups.

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
- `:npc-respawn-seconds` for the default NPC respawn cooldown (per-archetype overrides are supported).
- `:player-base-attack`, `:player-base-strength`, `:player-base-defense`, `:player-base-hitpoints`.
- `:player-training-mode` (`:attack`, `:strength`, `:defense`, `:balanced`),
  `:stat-xp-per-level`, `:stat-max-level`, `:xp-per-damage`.
- `:combat-hitpoints-xp-multiplier` to auto-train hitpoints on all combat XP awards.
- `:click-marker-duration`, `:click-marker-size-scale`, and `:click-marker-thickness` for target feedback markers.
- `:hud-log-line-seconds` and `:hud-log-fade-seconds` for HUD feedback timing.
- `:chat-max-length` to cap chat message length.
- `:inventory-size` to control player inventory slots.
- `:inventory-grid-columns` and `:inventory-slot-gap` to tune the inventory grid layout.
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
 (:rusty-sword (:name "Rusty Sword" :stack-size 1 :equip-slot :weapon :attack 1))

:object-archetypes
 (:arrows
  (:name "Arrows"
   :sprite "../assets/1 Characters/Other/Arrow.png"
   :item-id :arrows
   :count 5
   :respawn-seconds 5.0))

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
