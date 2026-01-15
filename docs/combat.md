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
- Rolls hit/miss and damage based on attack/strength/defense stats.
- Emits combat log lines (hit/miss, chance/roll, XP) to HUD + stdout when debug overlay is enabled.
- Runs attack cooldowns and hit effect animations.
- Emits NPC hit logs when `*debug-npc-logs*` is enabled.
- Supports attack-target follow (auto-move + auto-attack while in range).
- Starts NPC respawn cooldowns on death and restores NPCs when timers expire.

Key functions
- `attack-hitbox`, `start-player-attack`, `apply-melee-hit`.
- `update-player-animation`, `update-npc-animation`.
- `update-npc-attack` (consumes NPC intent).
- `update-npc-respawns` (counts down and restores dead NPCs).

Walkthrough: player melee hit
1) Player input requests an attack intent.
2) Combat starts the attack animation and sets a hit window.
3) `apply-melee-hit` checks hitbox overlap and rolls a hit/miss.
4) On hit, damage is rolled and health is reduced.
5) XP and loot are awarded on successful hits and kills, and debug log lines are queued.

Hit detection note
- The AABB overlap treats touching edges as a hit so adjacent contact lands.
- Attack facing is taken from intent input (or target) when the attack starts,
  so attacking while blocked still swings in the expected direction.
- NPC hits are clamped to their archetype max on hit to avoid stale/inflated values.
- NPC melee range includes collider sizes so enemies can actually reach you.

Example: applying a melee hit
```lisp
(when (and (player-attacking player)
           (not (player-attack-hit player)))
  (apply-melee-hit player npc world))
```

Design note
- Damage is applied by systems, not by rendering or input.
- Stats are used for hit chance and max hit, keeping combat data-driven.
