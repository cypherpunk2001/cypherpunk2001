# main.lisp

Purpose
- Build the client game state and provide core client-side utilities.
- The actual game loop is in net.lisp (`run-client`) for networked play.

Why we do it this way
- Client/server architecture keeps authority on the server.
- The client is a thin rendering layer that sends intents and displays snapshots.

Client update flow (high level)
1) Input/UI -> client intent, camera/UI updates, preview cache
2) Send intent to server via UDP
3) Receive snapshot from server
4) Apply snapshot to local game state
5) Process combat events and write to UI (client-side rendering)
6) Zone transitions (edge exits) -> load new zone if needed
7) Animation/effects -> visuals ready to render
8) UI timers (loading overlay, menus) -> update per frame
9) Editor mode (when enabled in local testing, disabled in client mode)

Key functions
- `make-game`: assembles the client game state with audio/UI/render subsystems; defaults the net role to `:client`.
- `make-sim-state` (server.lisp): builds world, player + players array, NPCs, entities, and combat events without client-only subsystems (server-side only).
- `shutdown-game`: unloads editor tilesets and rendering assets.
- `update-client-input`: reads raylib input, writes client intent (including chat), updates hovered NPC UI, toggles the inventory overlay, handles ESC menu Save/Load actions (queues save/load requests to server when in `:client` net role), and drives the right-click context menus (NPC attack/follow/examine, object pickup/examine, inventory examine/drop). Left mouse click-to-move uses a repeat timer while held on world tiles to refresh the walk target. Examine/drop actions emit HUD message events instead of writing to UI directly. Editor toggles are disabled in client mode.
- `server-step` (server.lisp): applies client intent and runs fixed-tick simulation steps, returning transition counts (server-side only).
- `update-sim`: runs one fixed-tick simulation step across all players, resolves object pickups, processes chat broadcasts, and feeds UI combat logging (server-side only).
- `process-combat-events`: reads combat event queue from server snapshots and writes to UI (client-side rendering).

Client/Server Architecture
- See `net.lisp` for `run-client` (client entry point) and `run-server` (server entry point)
- Client sends intents to server via UDP
- Server runs authoritative simulation and sends snapshots to all clients
- Client receives snapshot and applies to local game state for rendering
- Combat functions emit events (:combat-log, :hud-message) to the queue instead of writing UI directly
- This enforces: client sends intent → server validates → server updates state → server emits events → client renders result

Design note
- Keeping gameplay logic on the server makes it easier to prevent cheating and
  ensures all players see the same authoritative game state.
