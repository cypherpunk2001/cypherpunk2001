# server.lisp

Purpose
- Provide an in-process headless server loop that runs the authoritative simulation.

Why we do it this way
- Separating client input from server simulation now makes the eventual network split straightforward.
- Headless simulation lets us test server behavior without rendering or audio.

What it does
- Builds authoritative simulation state without client-only subsystems.
- Spawns a primary player plus a `players` array for multi-client simulation.
- Runs fixed-tick simulation steps and reports zone transitions.

Key functions
- `spawn-player-at-world`: spawn a player on a valid open tile near the world spawn center.
- `make-sim-state`: build world/player/players/NPCs/entities/id-source/combat-events without client subsystems; logs counts in verbose mode.
- `make-server-game`: construct a headless game struct (audio/ui/render/assets/camera/editor are nil; net role is `:server`).
- `apply-client-intent`: copy the client intent payload into the server intent.
- `server-step`: apply client intent and run fixed-tick simulation steps.
- `run-headless`: convenience loop for headless simulation (no rendering), with fatal error context logging.

Walkthrough: server tick
1) Client input writes into `client-intent`.
2) `server-step` copies the client intent into the authoritative intent on the player.
3) Fixed-tick simulation steps run (`update-sim`), validating targets and updating state.
4) Zone transitions are returned to the caller for client-side sync.

Design note
- The server loop owns authoritative state; the client only sends intent.
