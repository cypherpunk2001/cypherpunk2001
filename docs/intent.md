# intent.lisp

Purpose: shared action layer for player and NPC decisions.

Key responsibilities:
- Store per-frame signals: move, face, attack, run toggle.
- Store persistent targets (click-to-move).
- Provide helpers for writing intent consistently.

Key functions:
- `make-intent`, `reset-frame-intent`.
- `set-intent-move`, `set-intent-face`, `set-intent-target`, `clear-intent-target`.
- `request-intent-attack`, `request-intent-run-toggle`.

Notes:
- Input and AI fill intents; movement and combat consume them.
