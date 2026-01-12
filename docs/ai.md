# ai.lisp

Purpose: NPC decision-making and intent population.

Key responsibilities:
- Behavior state machine (`idle`, `aggressive`, `retaliate`, `flee`, `dead`).
- Wander target selection and perception checks.
- Fill NPC intent with movement and attack requests.

Key functions:
- `update-npc-behavior`, `update-npc-intent`, `update-npc-movement`.
- `npc-wander-direction`, `npc-pick-wander-target`.
- `npc-in-perception-range-p`, `npc-perception-range-sq`.

Notes:
- AI writes intent only; movement and combat consume it.
