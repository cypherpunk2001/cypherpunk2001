# Documentation Guide

These docs are meant to teach the architecture and the design choices behind this RPG prototype.
The goal is to help you learn the reasoning, not just the API.

## Why This Architecture
- **Systems over scripts**: behavior lives in systems, not in the main loop.
- **Intent layer**: input and AI both write intent; movement/combat consume it.
- **Data-driven**: tunables and archetypes live in `data/game-data.lisp`.
- **Rendering is read-only**: the render pipeline never mutates gameplay state.

## How To Read
Start with the game loop, then follow the data flow:
1) `docs/main.md` (orchestration)
2) `docs/types.md` and `docs/intent.md` (data layout and action layer)
3) `docs/input.md`, `docs/ai.md`, `docs/movement.md`, `docs/combat.md` (core systems)
4) `docs/zone.md`, `docs/rendering.md`, `docs/editor.md` (world data + draw pipeline + editor)
5) `docs/ui.md` and `docs/audio.md` (player-facing polish)
6) `docs/data.md` and `docs/config.md` (tuning and data-driven behavior)
7) `docs/utils.md` and `docs/package.md` (supporting helpers)

## Design Principles Used Here
- Behavior lives in systems, not in the main loop.
- Entities hold data; systems consume data and intent.
- Rendering never drives gameplay.
- Data is external and editable (tunable configs and archetypes).
- Performance matters: reuse objects and cull work early.

## Flow Diagrams

Update flow (one frame)
```
Input + UI
   |
   v
Intent (player + NPC)
   |
   v
Movement + Combat
   |
   v
Animation + Effects
```

Render flow (one frame)
```
World (tiles + walls) -> Entities -> HUD -> Menu
```

Data flow (startup)
```
config.lisp defaults
   |
   v
data/game-data.lisp overrides
   |
   v
registries (animation sets, archetypes)
   |
   v
world + assets
```

## File Index

Engine files
- [package.md](package.md)
- [config.md](config.md)
- [data.md](data.md)
- [zone.md](zone.md)
- [intent.md](intent.md)
- [types.md](types.md)
- [utils.md](utils.md)
- [input.md](input.md)
- [movement.md](movement.md)
- [combat.md](combat.md)
- [ai.md](ai.md)
- [audio.md](audio.md)
- [ui.md](ui.md)
- [editor.md](editor.md)
- [rendering.md](rendering.md)
- [main.md](main.md)

Reference docs
- [raylib_cheatsheet.md](raylib_cheatsheet.md)
- [claw-raylib-readme.org](claw-raylib-readme.org)
