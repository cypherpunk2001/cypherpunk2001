# server.lisp

Purpose
- Provide an in-process headless server loop that runs the authoritative simulation.

Why we do it this way
- Separating client input from server simulation now makes the eventual network split straightforward.
- Headless simulation lets us test server behavior without rendering or audio.

What it does
- Builds authoritative simulation state without client-only subsystems.
- Copies client intent into the authoritative server intent each frame.
- Runs fixed-tick simulation steps and reports zone transitions.

Key functions
- `make-sim-state`: build world/player/NPCs/entities/id-source/combat-events without client subsystems.
- `make-server-game`: construct a headless game struct (audio/ui/render/assets/camera/editor are nil).
- `apply-client-intent`: copy the client intent payload into the server intent.
- One-shot requests like chat are cleared from the client intent after being applied.
- `server-step`: apply client intent and run fixed-tick simulation steps.
- `run-headless`: convenience loop for headless simulation (no rendering).

Walkthrough: server tick
1) Client input writes into `client-intent`.
2) `server-step` copies the client intent into the authoritative intent on the player.
3) Fixed-tick simulation steps run (`update-sim`), validating targets and updating state.
4) Zone transitions are returned to the caller for client-side sync.

Design note
- The server loop owns authoritative state; the client only sends intent.
