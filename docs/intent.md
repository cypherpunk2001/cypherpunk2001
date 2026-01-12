# intent.lisp

Purpose
- Provide a shared action layer that input, AI, and future networking can
  all write to and systems can consume.

Why we do it this way
- It keeps decision-making separate from execution.
- It makes AI and player controls symmetric: both express intent, and the
  movement/combat systems apply it.
- It supports a future client/server split without rewriting logic.

What an intent contains
- Movement direction (dx, dy)
- Facing direction (face-dx, face-dy)
- A target point (for click-to-move)
- One-frame actions (attack, run toggle)

Key functions
- `reset-frame-intent`: clears per-frame signals without erasing targets.
- `set-intent-move`, `set-intent-face`: standardize motion signals.
- `set-intent-target`, `clear-intent-target`: manage click-to-move.
- `request-intent-attack`, `request-intent-run-toggle`.

Walkthrough: click-to-move
1) Mouse click is converted to a world-space target.
2) Input writes `intent-target-x/y` and sets `intent-target-active`.
3) Movement reads the target and moves toward it until close enough.
4) Movement clears the target when it arrives or gets stuck.

Example flow
```lisp
(reset-frame-intent player-intent)
(update-input-direction player player-intent mouse-clicked)
(update-input-actions player-intent (not mouse-clicked))
(update-player-position player player-intent world speed-mult dt)
```

Design note
- Think of intent as a "what I want" packet. Systems decide "what happens."
