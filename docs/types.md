# types.lisp

Purpose
- Define all core state containers and the engine's generic interfaces.

Why we do it this way
- Game state is easiest to reason about when it is plain data. Systems can
  operate on this data without hidden side effects.
- Generics define the "capabilities" of entities (position, health, drawing)
  without forcing an inheritance-heavy object model.

Key structs
- `player`, `npc`: runtime entities with an `intent` and animation/combat state.
- `world`: map data, collision, bounds, and derived sizes.
- `audio`, `ui`, `render`, `assets`, `camera`: subsystem state.
- `game`: top-level aggregator passed to update/draw functions.

Key constructors
- `make-player`, `make-npc`: construct entities with default fields.
- `make-npcs`: spawn a pool with data-driven archetypes and safe spacing.
- `world-spawn-center`: returns a center point inside world collision bounds.
- `make-entities`: pack NPCs + player into a stable array.

Key generics
- Combat: `combatant-position`, `combatant-health`, `combatant-apply-hit`.
- Rendering: `draw-entity`.
- Animation/effects: `update-entity-animation`, `combatant-update-hit-effect`.

Walkthrough: entity lifecycle
1) `make-player` / `make-npc` constructs the struct.
2) Input/AI populates intent each frame.
3) Systems update position, health, and animation fields.
4) Rendering reads the state to draw.

Design note
- Arrays are used for entity collections to keep iteration fast and predictable.
- Spawn spacing accounts for collider sizes so entities don't overlap at start.
