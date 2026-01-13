# movement.lisp

Purpose
- Resolve movement and collisions against the world.

Why we do it this way
- Separating movement from input allows AI, networking, and scripted events
  to reuse the same collision logic.
- Per-axis movement resolution avoids "tunneling" and feels more responsive.

Key concepts
- `world-blocked-tile-p` decides if a tile is blocked (currently using the wall map).
- `attempt-move` applies per-axis movement with collision checks.
- `make-world` derives bounds and collision sizes from map data.
- `world-open-position` finds the nearest open tile that fits the player collider.
- `ensure-npcs-open-spawn` snaps NPCs to open tiles (using NPC collider sizes).
- `*collision-edge-epsilon*` avoids treating exact tile-edge contact as blocked.

Walkthrough: from intent to position
1) Read movement intent (dx/dy) and optional target.
2) If the player has a target, compute direction toward it.
3) Call `attempt-move` which resolves collisions per axis.
4) Clamp to world bounds.
5) Store final position and velocity.

Example: applying intent
```lisp
(let* ((moving (or (not (zerop (intent-move-dx intent)))
                   (not (zerop (intent-move-dy intent)))
                   (intent-target-active intent)))
       (speed-mult (update-running-state player dt moving
                                         (intent-run-toggle intent))))
  (update-player-position player intent world speed-mult dt))
```

Design note
- World bounds are enforced to keep navigation stable even when collision
  layers are sparse or missing.
- Spawning on open tiles prevents actors from getting stuck inside blocked
  collision layers.
