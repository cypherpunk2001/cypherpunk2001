# mmorpg

Common Lisp + raylib MMORPG prototype focused on clean architecture and
system separation. The codebase is structured to teach modern game design habits: data-driven content, intent-based actions, and a strict update/draw split.

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

## Tests
- `make checkparens` Checks all `.lisp` files in `data/` and `src/` for balanced parentheses and general sexp structure.
- `make ci` runs a cold compile check (no window, no GPU needed).
- `make smoke` opens the game for a short run and exits automatically.
  It runs from `src/` so existing `../assets` paths resolve correctly.
  The smoke target is wrapped in a Linux `timeout` to kill hung runs.
  Defaults: 2 seconds runtime, 5 seconds timeout.
- `make checkdocs` Checks that every `src/foo.lisp` has a matching `docs/foo.md`, errors if any are missing, otherwise prints a friendly reminder when all pass.

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
