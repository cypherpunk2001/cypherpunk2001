# progression.lisp

Purpose
- Own combat progression: stats, XP leveling, training modes, inventory, and loot.

Why we do it this way
- Progression logic should be reusable by players, NPCs, and future server sims.
- Data-driven loot tables keep content outside of gameplay code.

What it does
- Defines the XP -> level curve and clamps levels at `*stat-max-level*`.
- Applies training-mode XP splits (`:attack`, `:strength`, `:defense`, `:balanced`).
- Grants hitpoints XP on every combat award using `*combat-hitpoints-xp-multiplier*`.
- Computes simple combat levels and melee hit chance/damage from stats.
- Adds inventory stacking, removal, and loot-table rolls for NPC drops.
- Applies equipment stat modifiers when items are equipped or unequipped.
- Clamps current HP to the effective max after equipment modifier changes.
- Refreshes cached inventory lines for the inventory overlay.
- Picks up placed world objects into inventory when the player steps on them.

Key functions
- `xp->level`, `level->xp`, `award-combat-xp`
- `melee-hit-p`, `roll-melee-hit`, `roll-melee-damage`
- `ensure-player-hud-stats`, `refresh-player-hud-stats`
- `combat-level`, `combatant-max-hp`
- `inventory-add`, `inventory-remove`, `equip-item`, `unequip-item`, `award-npc-loot`
- `ensure-player-inventory`, `update-object-pickups`

Walkthrough: awarding XP on a hit
1) A successful hit computes damage.
2) XP is derived from damage and `*xp-per-damage*`.
3) Training mode splits remaining XP into the selected stat(s) while hitpoints always auto-train.
4) Hitpoints leveling raises the player's current HP.

Walkthrough: rolling loot
1) NPC archetype supplies a loot table ID.
2) The loot table rolls weighted entries.
3) Items are added to the player inventory if space allows.

Design note
- Stat modifiers are additive so equipment can layer in later.
- Loot tables and items live in `data/game-data.lisp` for quick iteration.
