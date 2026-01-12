# data.lisp

Purpose: load data-driven tunables, animation sets, and NPC archetypes.

Key responsibilities:
- Load `data/game-data.lisp` without evaluation and apply tunables.
- Register animation sets and NPC archetypes in hash tables.
- Provide access helpers like `get-animation-set` and `find-npc-archetype`.

Key functions:
- `load-game-data`: entry point for reading and applying data once.
- `ensure-game-data`: guard that registers defaults when needed.
- `npc-animation-set-ids`: collects animation set IDs referenced by archetypes.

Notes:
- Prefer adding new tunables to `data/game-data.lisp` rather than hardcoding in code.
