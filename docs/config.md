# config.lisp

Purpose
- Central home for tunable parameters and static class definitions.

Why we do it this way
- Game feel is iteration heavy. Keeping tunables in one place makes it cheap
  to tweak speeds, timings, and UI without hunting through systems.
- Static CLOS classes (`character-class`, `npc-archetype`) let us store design
  intent (like combat style and perception) in a structured way.

What lives here
- Window, camera, input, and debug flags (collision overlay + NPC AI logs).
- Sprite, tileset, zone, world layout, and audio defaults.
- World graph file path for zone-to-zone transitions.
- Editor defaults (palette root, export path, layer IDs, overlay colors).
- Zone defaults and editor zone root for creating new zones.
- Loading overlay duration for zone transitions.
- Minimap sizing, placement, preview edge distance, and minimap color defaults.
- Movement, combat, NPC behavior, and animation timings.
- Debug overlay sizing/color for NPC AI text when logs are enabled.
- Collision edge epsilon for fine-tuning tile contact behavior.

How it connects
- Values can be overridden by `data/game-data.lisp` via `load-game-data`.
- New tunables should be added to `*tunable-keys*` in `data.lisp` so they
  can be data-driven.

Walkthrough: change movement speed
1) Edit `data/game-data.lisp` and set `:player-speed`.
2) `load-game-data` applies the tunable at startup.
3) Movement uses the updated value the next frame.

Walkthrough: adjust wall-map bounds
1) Set `:wall-map-width` and `:wall-map-height` in `data/game-data.lisp`.
2) `make-world` rebuilds the wall map at startup.
3) Movement clamps to the new bounds.

Walkthrough: load a zone file
1) Set `:zone-path` in `data/game-data.lisp`.
2) `load-zone` builds collision tiles from chunked layers.
3) The world uses the zone's collision data for blocking tiles.

Walkthrough: configure the world graph
1) Set `:world-graph-path` in `data/game-data.lisp`.
2) `load-world-graph` reads edge links for zone transitions.
3) Movement consults the graph when the player exits a zone edge.

Walkthrough: tweak zone loading overlay
1) Set `:zone-loading-seconds` in `data/game-data.lisp`.
2) UI uses the value to display a brief "Loading..." overlay on zone swaps.

Walkthrough: adjust the minimap size
1) Set `:minimap-width`, `:minimap-height`, `:minimap-padding`, and
   `:minimap-preview-edge-tiles` in `data/game-data.lisp`.
2) UI repositions the minimap each boot using the new values.

Walkthrough: configure default zone sizing
1) Set `:zone-default-width`, `:zone-default-height`, and `:zone-default-chunk-size`.
2) New zones created in the editor use these values.
3) Resize hotkeys step by the active zone's chunk size.

Walkthrough: customize editor export
1) Set `:editor-export-path` in `data/game-data.lisp`.
2) Editor Mode writes zone files to the new location on export.

Walkthrough: change zone root
1) Set `:zone-root` in `data/game-data.lisp`.
2) Editor zone create/delete/list operations use the new folder.

Example: tuning player speed
```lisp
;; In data/game-data.lisp
(:tunables
 (:player-speed 260.0))
```

Example: adding a new archetype
```lisp
;; In data/game-data.lisp
(:npc-archetypes
 (:slime
  (:name "Slime"
   :max-hits 2
   :move-speed 80.0
   :attack-range-tiles 0.6
   :attack-cooldown 1.2
   :attack-damage 1
   :home-radius-tiles 1.5
   :wander-interval 1.3
   :animation-set-id :npc-slime)))
```
