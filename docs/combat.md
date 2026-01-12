# combat.lisp

Purpose: combat resolution, hit effects, and animation timing.

Key responsibilities:
- Combatant generics for position, health, collisions, and hits.
- Player attack hitbox and NPC melee checks.
- Animation state progression for player/NPCs.

Key functions:
- `attack-hitbox`, `start-player-attack`, `apply-melee-hit`.
- `update-player-animation`, `update-npc-animation`.
- `update-npc-attack` (consumes intent + cooldowns).

Notes:
- Uses world tile size to scale hitboxes and ranges.
