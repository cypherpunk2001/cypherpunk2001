# utils.lisp

Purpose
- Provide small, reusable helpers for math and common game tasks.

Why we do it this way
- Small, pure helpers reduce duplication and make systems easier to test.
- Keeping helpers side-effect free avoids subtle bugs in the update loop.

Key helpers
- `clamp`: world bounds and UI limits.
- `normalize-direction`, `normalize-vector`: avoid faster diagonals.
- `screen-to-world`: convert mouse to world coordinates with camera zoom.
- `player-direction`, `player-state`, `player-animation-params`: animation logic helpers.
- `u32-hash`: deterministic variation for wall tile selection.

Example: diagonal movement normalization
```lisp
(multiple-value-bind (dx dy) (normalize-direction 1.0 1.0)
  ;; dx/dy are length 1.0, not sqrt(2.0)
  (values dx dy))
```
