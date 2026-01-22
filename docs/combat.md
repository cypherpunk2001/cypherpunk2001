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
- Formats combat log lines with the per-hit attack/defense levels.
- Emits HUD feedback for level-ups, combat level increases, and "under attack" alerts.
- Runs attack cooldowns and hit effect animations.
- Emits NPC hit logs when `*debug-npc-logs*` is enabled.
- Supports attack-target follow (auto-move + auto-attack while in range).
- Starts NPC respawn cooldowns on death and restores NPCs when timers expire.

Key functions

**Combat Resolution:**
- `attack-hitbox` - Return attack hitbox center and half sizes for current facing.
- `start-player-attack` - Start attack animation if not already active.
- `apply-melee-hit` - Apply melee damage once per attack if hitbox overlaps target.
- `roll-melee-hit` - Roll hit/miss based on attack/defense stats.
- `roll-melee-damage` - Roll damage based on strength stat.

**Animation:**
- `update-player-animation` - Advance animation timers and set facing/state.
- `update-npc-animation` - Advance idle animation frames for NPC.
- `update-entity-animation` - Generic method dispatching to player/NPC animations.

**NPC Combat:**
- `update-npc-attack` - Handle NPC melee attacks and cooldowns.
- `update-npc-respawns` - Count down respawn timers and restore dead NPCs.
- `respawn-npc` - Reset NPC state after respawn cooldown.
- `npc-respawn-seconds` - Return respawn cooldown for NPC.
- `npc-attack-range` - Return NPC melee range in world pixels.
- `npc-attack-cooldown` - Return NPC melee cooldown in seconds.
- `npc-attack-damage` - Return NPC melee damage per hit.

**Target Management:**
- `player-attack-target` - Return active NPC attack target for player.
- `player-follow-target` - Return active NPC follow target for player.
- `sync-player-attack-target` - Validate requested attack target (server authority). Accepts targets at any distance; player walks to target and auto-attacks when in melee range.
- `sync-player-follow-target` - Validate requested follow target (server authority). Accepts targets at any distance.
- `sync-player-pickup-target` - Validate requested pickup target (server authority).
- `update-player-attack-intent` - Request attacks when target is in range.
- `player-attack-target-in-range-p` - Return true when target is inside player's melee hitbox.

**Combat Events:**
- `emit-combat-log` - Emit combat log events to queue and stdout in debug mode.
- `emit-hud-message` - Emit HUD feedback event to queue.
- `emit-level-up-messages` - Emit HUD messages for stat level-ups.
- `push-combat-log` - Build and emit combat log event.
- `format-combat-log` - Build a combat log line for hit or miss.

**Combatant Protocol (CLOS):**
- `combatant-position` - Return (x, y) position.
- `combatant-alive-p` - Return true if alive.
- `combatant-collision-half` - Return (half-w, half-h) collision sizes.
- `combatant-health` - Return (current-hp, max-hp).
- `combatant-apply-hit` - Apply damage and trigger persistence.
- `combatant-trigger-hit-effect` - Start blood effect animation.
- `combatant-update-hit-effect` - Advance hit effect timer.
- `combatant-display-name` - Return short display name for logs.

**Utility:**
- `aabb-overlap-p` - Return true when two axis-aligned boxes overlap.
- `find-npc-by-id` - Return NPC with given ID, if present.
- `intent-attack-direction` - Choose attack direction from intent input or target.

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

Persistence
- Player HP changes trigger tier-2 dirty marking for batched saves.
- Player death (HP reaches 0) triggers tier-1 immediate save to prevent
  logout-to-survive exploits. The save happens before any response to client.
- `combatant-apply-hit` captures old HP before damage to detect the alive→dead
  transition correctly.

Client/Server Authority Boundaries (preparation for future networking)
- Sync functions validate client-requested targets and set authoritative state:
  - `sync-player-attack-target`: validates `requested-attack-target-id`, sets `player-attack-target-id`
  - `sync-player-follow-target`: validates `requested-follow-target-id`, sets `player-follow-target-id`
  - `sync-player-pickup-target`: validates `requested-pickup-target-id`, sets `player-pickup-target-*`
- Combat functions emit events to a queue instead of writing UI directly:
  - `emit-combat-log`: emits :combat-log events
  - `emit-hud-message`: emits :hud-message events
  - Events are processed after simulation and written to UI (client-side rendering)
- This enforces: server updates state → server emits events → client renders result
- Server never touches UI state; client never modifies authoritative game state
