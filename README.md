# mmorpg

Minimal Common Lisp + raylib RPG prototype.

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

Docs index: `docs/README.md`.
