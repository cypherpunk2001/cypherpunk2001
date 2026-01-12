# types.lisp

Purpose: defines core state structs and generics.

Key structs:
- `player`, `npc`, `world`, `audio`, `ui`, `render`, `assets`, `camera`, `game`.
- `intent` is owned by player and NPCs to decouple actions.

Key constructors:
- `make-player`, `make-npc`, `make-npcs`, `make-entities`.

Key generics:
- Combat: `combatant-position`, `combatant-alive-p`, `combatant-collision-half`, `combatant-apply-hit`, `combatant-health`.
- Effects/animation: `combatant-trigger-hit-effect`, `combatant-update-hit-effect`, `update-entity-animation`.
- Rendering: `draw-entity`.
