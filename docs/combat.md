# combat.lisp

Purpose
- Combat resolution, hit effects, and animation timing.

Why we do it this way
- Combat is a system. It reads state and intent, applies outcomes, and lets
  animation and rendering respond. This keeps gameplay deterministic.

What it does
- Defines combatant generics for position, health, collisions, and hits.
- Creates a directional attack hitbox for the player.
- Applies hits once per attack window.
- Runs attack cooldowns and hit effect animations.

Key functions
- `attack-hitbox`, `start-player-attack`, `apply-melee-hit`.
- `update-player-animation`, `update-npc-animation`.
- `update-npc-attack` (consumes NPC intent).

Walkthrough: player melee hit
1) Player input requests an attack intent.
2) Combat starts the attack animation and sets a hit window.
3) `apply-melee-hit` checks hitbox overlap once per attack.
4) Target health is reduced and a hit effect starts.

Example: applying a melee hit
```lisp
(when (and (player-attacking player)
           (not (player-attack-hit player)))
  (apply-melee-hit player npc world))
```

Design note
- Damage is applied by systems, not by rendering or input.
