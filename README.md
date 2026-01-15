# mmorpg

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

OR

```shell
make start
```

## Tests
- `make checkparens` Checks all `.lisp` files in `data/` and `src/` for balanced parentheses and general sexp structure.
- `make ci` runs a cold compile check (no window, no GPU needed).
- `make smoke` opens the game for a short run and exits automatically.
  It runs from `src/` so existing `../assets` paths resolve correctly.
  The smoke target is wrapped in a Linux `timeout` to kill hung runs.
  Defaults: 2 seconds runtime, 5 seconds timeout.
- `make checkdocs` Checks that every `src/foo.lisp` has a matching `docs/foo.md`, errors if any are missing, otherwise prints a friendly reminder when all pass.
