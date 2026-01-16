# main.lisp

Purpose
- Build the game and orchestrate the update/render loop.

Why we do it this way
- The main loop should be a conductor, not a composer. It wires systems
  together and calls them in a consistent order.

Update flow (high level)
1) Input/UI -> intent, camera/UI updates, preview cache
2) Fixed-tick simulation -> follow/attack/pickup sync, movement, combat, AI, respawns
3) Combat events -> process event queue and write to UI (client-side rendering)
4) Zone transitions (edge exits) -> load new zone if needed
5) Animation/effects -> visuals ready to render
6) UI timers (loading overlay, menus) -> update per frame
7) Editor mode (when enabled) overrides gameplay updates

Key functions
- `make-game`: assembles world, entities, audio, UI, render, assets, camera, editor.
- Uses world bounds and collision data to choose a safe spawn center.
- Ensures player/NPC spawns land on open tiles sized to their colliders.
- Refreshes adjacent minimap spawn previews after the player spawn is known.
- `shutdown-game`: unloads editor tilesets and rendering assets.
- Uses `*editor-start-enabled*` to optionally boot straight into editor mode.
- `update-client-input`: reads raylib input, writes player intent, updates hovered NPC UI, toggles the inventory overlay, and drives the right-click context menus (NPC attack/follow/examine, object pickup/examine, inventory examine/drop). Left mouse click-to-move uses a repeat timer while held on world tiles to refresh the walk target. Examine/drop actions emit HUD message events instead of writing to UI directly.
- `update-sim`: runs one fixed-tick simulation step from intent, resolves object pickups, and feeds UI combat logging.
- `update-game`: orchestrates fixed-step simulation and returns the accumulator.
- `run`: owns the raylib window lifecycle and can auto-exit for smoke tests.

Walkthrough: one frame
1) Read input and UI; write intent for player and NPCs.
2) If editor mode is active, update editor camera/painting/zone tools and skip gameplay.
3) Otherwise, run as many fixed-tick simulation steps as the accumulator allows (including follow sync and NPC respawns).
4) Advance animation/effect timers during simulation ticks.
5) Update UI timers for loading overlays.
6) Render the frame (world -> entities -> HUD/loading/menu/editor overlay).

Example: core loop
```lisp
(loop :with sim-accumulator = 0.0
      :until (or (raylib:window-should-close)
                 (ui-exit-requested (game-ui game)))
      :do (let ((dt (raylib:get-frame-time)))
            (setf sim-accumulator
                  (update-game game dt sim-accumulator))
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

Client/Server Separation (preparation for future networking)
- `make-game` initializes a combat event queue for decoupling simulation from UI
- `update-sim` calls sync functions (`sync-player-attack-target`, `sync-player-follow-target`, `sync-player-pickup-target`) that validate client intent requests and set authoritative state (server-side)
- Combat functions emit events (:combat-log, :hud-message) to the queue instead of writing UI directly
- `process-combat-events` reads the event queue and writes to UI (client-side rendering)
- This enforces: client sends intent → server validates → server updates state → server emits events → client renders result
