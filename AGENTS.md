# AGENTS.md

Minimal “prove-it” Common Lisp + raylib project.

Goal: open a raylib window from Common Lisp via `claw-raylib`, and move a pixel (tiny rectangle) on screen.

---

## What this project is (right now)

- One executable-ish entrypoint you run from SLIME/REPL.
- One moving “player pixel”.
- No networking, no database, no assets pipeline.
- A foundation we can later split into client/server.

This exists purely to prove:
> **Common Lisp + raylib works and feels good to iterate on.**

---

## Prerequisites (Arch Linux)

System packages:

```bash
sudo pacman -S raylib sbcl
```

You also need:
- SBCL (DONE)
- Quicklisp (DONE)
- Emacs + SLIME (DONE)

---

## Project layout

```
mmorpg/
  AGENTS.md
  mmorpg.asd
  src/
    package.lisp
    main.lisp
```

This repo should depend **only** on `claw-raylib` for the graphics proof.

---

## “Hello MMO” proof target

We want the absolute minimum:

- A window
- A 2x2 or 4x4 rectangle (the “player”)
- Move with arrow keys or WASD
- 60 FPS loop
- Runnable from SLIME

Entry point:
```lisp
(mmorpg:run)
```

---

## Development workflow

1. Start SBCL via SLIME.
2. Load project:
   ```lisp
   (ql:quickload :mmorpg)
   ```
3. Run:
   ```lisp
   (mmorpg:run)
   ```
4. Edit code.
5. `C-c C-c` functions to redefine live.
6. Observe changes instantly.
7. Close window via Escape or window close.
