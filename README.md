# mmorpg

Minimal Common Lisp + raylib RPG prototype focused on clean architecture and
system separation. The codebase is intentionally small but structured to teach
modern game design habits: data-driven content, intent-based actions, and a
strict update/draw split.

## Why This Architecture
- **Systems over scripts**: behavior lives in systems, not in the main loop.
- **Intent layer**: input and AI both write intent; movement/combat consume it.
- **Data-driven**: tunables and archetypes live in `data/game-data.lisp`.
- **Rendering is read-only**: the render pipeline never mutates gameplay state.

## Requirements
- SBCL + Quicklisp
- Emacs + SLIME (or any Common Lisp REPL)
- raylib + raygui
- claw-raylib (prebuild branch recommended)

## Setup
- Follow the claw-raylib build instructions; on the prebuild branch, skip steps 1-2 and start at step 3.
- Register the repo with Quicklisp once per session.

## Run
Open `src/main.lisp`, start SLIME (or your REPL), then:

```lisp
(ql:register-local-projects) ; once per session
(ql:quickload :mmorpg)
(mmorpg:run)
```

## Learn The Codebase
Start with the documentation index and follow the suggested reading order.

- `docs/README.md`
- `docs/main.md` (orchestration)
- `docs/intent.md` + `docs/types.md` (data + action layer)
- `docs/input.md`, `docs/ai.md`, `docs/movement.md`, `docs/combat.md` (core systems)
- `docs/map.md` and `docs/rendering.md` (world and draw pipeline)
- `docs/ui.md` and `docs/audio.md` (player-facing systems)

The docs explain not just *what* the code does, but *why* the architecture
is designed this way.
