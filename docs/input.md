# input.lisp

Purpose
- Translate device input into intent.

Why we do it this way
- Input should not move entities directly. It should only express what the
  player wants to do. This keeps input, AI, and future network input aligned.

What it does
- Converts WASD/arrow keys into a movement intent.
- Handles auto-walk toggles.
- Converts mouse clicks into a target intent (click-to-move).
- Emits action intents for running and attacking.
- Updates camera zoom from the mouse wheel.

Key functions
- `update-input-direction`, `update-target-from-mouse`, `update-input-actions`.

Walkthrough: mouse click to target
1) Convert screen coordinates to world coordinates with camera offset/zoom.
2) Write `intent-target-x/y` and mark the target active.
3) Movement system consumes that target later in the frame.

Example flow
```lisp
(update-target-from-mouse player player-intent camera dt mouse-clicked mouse-down)
(update-input-direction player player-intent mouse-clicked)
(update-input-actions player-intent (not mouse-clicked))
```

Design note
- All logic here is input-to-intent. Movement happens elsewhere.
