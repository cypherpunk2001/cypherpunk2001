# config.lisp

Purpose: central tunables and static data classes.

Key responsibilities:
- Global defparameters for window, input, camera, movement, tiles, audio, combat, and debug flags.
- CLOS classes for `character-class` and `npc-archetype` (static tuning data).
- Raylib key/mouse constants used by input handling.

Notes:
- Values can be overridden by `data/game-data.lisp` via `load-game-data`.
- Keep new tunables in sync with `*tunable-keys*` in `data.lisp`.
