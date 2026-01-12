# data.lisp

Purpose
- Load data-driven tunables, animation sets, and NPC archetypes.

Why we do it this way
- Designers should be able to change the game without editing logic.
- Data-driven content makes it easier to test different balance settings
  and prepare for tool-driven workflows.

Key responsibilities
- Read `data/game-data.lisp` without evaluating code.
- Apply tunables to config variables.
- Register animation sets and NPC archetypes into hash tables.

Key functions
- `load-game-data`: entry point; clears registries, loads data, registers defaults.
- `ensure-game-data`: guard that loads once when needed.
- `get-animation-set`, `find-npc-archetype`: lookup helpers.

Walkthrough: startup data load
1) `load-game-data` reads `data/game-data.lisp` as plain data.
2) Tunables are applied to config variables.
3) Animation sets and archetypes are registered in hash tables.
4) Defaults are ensured so the game always has a baseline.

Example: new animation set
```lisp
;; In data/game-data.lisp
(:animation-sets
 (:npc-slime
  (:dir "../assets/3 Dungeon Enemies/5"
   :down-idle "D_Idle.png"
   :up-idle "U_Idle.png"
   :side-idle "S_Idle.png")))
```

Design note
- Registries are in-memory hash tables, which keeps lookup cheap during play
  without forcing gameplay systems to parse files every frame.
