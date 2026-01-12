# ai.lisp

Purpose
- NPC decision-making and intent population.

Why we do it this way
- AI should be data-driven and symmetric with player input.
- Separating decision from movement allows the same movement system to serve
  NPCs, players, and scripted events.

Behavior model
- Simple state machine: `idle`, `aggressive`, `retaliate`, `flee`, `dead`.
- Behavior is driven by archetype tuning (aggro, flee thresholds, perception).
- Being provoked overrides perception checks so hit NPCs respond immediately.
- Any provoked NPC enters `:retaliate` so they fight back after taking a hit.
- Flee ignores the home-radius tether so low-health NPCs actually run away.
- Flee is decided by `npc-should-flee-p` and enforced in intent so low-health
  NPCs flee even if behavior state lags a frame behind.

Key functions
- `npc-should-flee-p`: low-health flee check shared by behavior and intent.
- `update-npc-behavior`: choose the behavior state.
- `update-npc-intent`: produce movement/attack intent.
- `update-npc-movement`: apply intent and update facing.

Walkthrough: aggressive chase
1) Perception check sees the player within range.
2) Behavior switches to `:aggressive`.
3) Intent sets move direction toward player and face direction.
4) Movement applies intent and clamps to world bounds.
5) Combat checks attack range and applies hits.

Example: intent from behavior
```lisp
(update-npc-behavior npc player world)
(update-npc-intent npc player world dt)
(update-npc-movement npc world dt)
```

Design note
- This AI is intentionally simple. The same intent layer can be used later
  for pathfinding, steering, or server-authoritative decisions.
- When `*debug-npc-logs*` is enabled, AI state transitions are logged to
  help trace flee/aggro decisions frame by frame.
