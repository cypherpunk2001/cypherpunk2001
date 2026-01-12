# input.lisp

Purpose: translate device input into intent.

Key responsibilities:
- Map keyboard to movement and auto-walk toggles.
- Map mouse clicks to target intent.
- Emit attack and run-toggle intents.
- Update camera zoom.

Key functions:
- `update-input-direction`, `update-target-from-mouse`, `update-input-actions`.
- `read-input-direction`, `clear-player-auto-walk`, `update-camera-zoom`.

Notes:
- Input does not move entities directly; it only writes intent.
