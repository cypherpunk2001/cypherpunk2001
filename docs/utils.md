# utils.lisp

Purpose: math and utility helpers shared across systems.

Key helpers:
- Math: `clamp`, `normalize-direction`, `normalize-vector`.
- Camera: `screen-to-world`.
- Interaction: `point-in-rect-p`.
- Sprite paths: `sprite-path`, `npc-sprite-path`, `blood-sprite-path`.
- Player helpers: `player-direction`, `player-state`, `player-animation-params`.
- Deterministic noise: `u32-hash`.

Notes:
- Keep helpers pure and reusable; no rendering or input side effects.
