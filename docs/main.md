# main.lisp

Purpose
- Build the game and orchestrate the update/render loop.

Why we do it this way
- The main loop should be a conductor, not a composer. It wires systems
  together and calls them in a consistent order.

Update flow (high level)
1) Input/UI -> intent
2) Movement/combat -> state updates
3) Animation/effects -> visuals ready to render

Key functions
- `make-game`: assembles world, entities, audio, UI, render, assets, camera.
- Uses world bounds and collision data to choose a safe spawn center.
- Ensures player/NPC spawns land on open tiles sized to their colliders.
- `update-game`: orchestrates system updates.
- `run`: owns the raylib window lifecycle and can auto-exit for smoke tests.

Walkthrough: one frame
1) Read input and UI; write intent for player and NPCs.
2) Update movement/combat; change positions, hit points, and cooldowns.
3) Advance animation/effect timers.
4) Render the frame (world -> entities -> HUD/menu).

Example: core loop
```lisp
(loop :until (or (raylib:window-should-close)
                 (ui-exit-requested (game-ui game)))
      :do (let ((dt (raylib:get-frame-time)))
            (update-game game dt)
            (draw-game game)))
```

Run options (smoke testing)
- `run` accepts `:max-seconds` and `:max-frames`. Values <= 0 disable the limit.
- This lets us open the game, collect runtime output, and exit automatically.

Example: auto-exit
```lisp
(mmorpg:run :max-seconds 5.0)
(mmorpg:run :max-frames 300)
```

Design note
- Keeping gameplay logic out of the main loop makes it easier to test and
  to introduce networking or replay systems later.
