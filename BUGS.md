# Bugs and Code Quality Findings

Scope: `src/` and `data/` (server/client logic, persistence, gameplay systems).

## Critical
- None found.

## High
- Untrusted intent fields can crash the server. `apply-intent-plist` assigns `:requested-pickup-tx`, `:requested-pickup-ty`, and `:requested-drop-slot-index` without type/range checks (`src/net.lisp:1063-1074`). These flow into `sync-player-pickup-target` and `update-player-pickup-target` (`src/combat.lisp:367-394`, `src/progression.lisp:720-744`) and into `inventory-remove` via `drop-inventory-item` (`src/progression.lisp:415-437`). A client can send strings or negative indices and trigger type errors (`abs`, `<`, `aref`), crashing the server. Fix: sanitize pickup tx/ty and drop-slot with `%int-or` plus `>= 0` bounds checks, and reject malformed values early.

## Medium
- Admin teleport leaves `player-zone-id` stale. `admin-teleport` only updates the session zone (`update-player-session-zone`) and player coordinates, but never updates `(player-zone-id player)` (`src/admin.lisp:240-275`). This can desync zone-filtered snapshots, trade checks, and persistence. Fix: set `player-zone-id` alongside session update (and mark snapshot dirty).
- Admin set XP only increases level. `admin-set-xp` recalculates by incrementing until XP exceeds thresholds but never decreases the level when XP is reduced (`src/admin.lisp:183-192`). This leaves invalid states (XP below level minimum). Fix: set level from XP (e.g., `xp->level`) or recalc from scratch.
- Respawning direct-item pickups never restore count. `pickup-object-at-tile` allows objects whose `:id` is an item (no object archetype) and can set a respawn timer via `:respawn-seconds` (`src/progression.lisp:685-707`). `update-zone-objects-respawns` only restores counts when an object archetype exists (`src/progression.lisp:651-657`), so direct-item objects with respawn overrides will never respawn. Fix: store a base count per object (e.g., `:base-count`) or restore via a saved value when `archetype` is nil.
- Multi-zone transitions still depend on a single world collision map. `world-exit-edge` uses the current `world` bounds for all players (`src/movement.lisp:549-581`), and `update-zone-transition` applies this for every player (`src/movement.lisp:970-985`). Players in zones that are not the current `world` zone may get incorrect edge detection or transitions. Fix: use per-zone bounds from `zone-state` for edge checks, or gate transitions to players in the active `world` zone until per-zone collision is implemented.

## Low
- None found.
