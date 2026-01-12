# main.lisp

Purpose: game assembly and main loop orchestration.

Key responsibilities:
- `make-game` wires world, entities, audio, UI, render, and assets.
- `update-game` orchestrates input -> intent -> movement/combat -> animation.
- `run` owns the raylib window lifecycle and shutdown.

Notes:
- The main loop avoids gameplay logic beyond orchestration.
