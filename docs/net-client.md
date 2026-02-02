# net-client.lisp

Purpose
- Client networking loop entry point: connects to UDP server, handles login UI, sends intents, receives snapshots, and renders the game.

Key responsibilities
- `run-client`: main client entry point that orchestrates all client-side systems.
- UDP socket connection to server.
- Login screen phase: periodic server pings, auto-login (register-then-login), manual login/register via UI.
- Server-busy retry with exponential backoff (up to 3 retries).
- Auth message handling (`:auth-ok`, `:auth-fail`, `:hello-ack`).
- Server availability tracking with online/offline status.
- Gameplay phase: intent sending with optional client-side prediction.
- Snapshot receiving, applying, and buffering for interpolation.
- Fragmented snapshot chunk reassembly.
- Private state updates (inventory/equipment/stats).
- Delta compression acknowledgment via `:ack` field in intent messages.
- Prediction reconciliation when server state diverges from local prediction.
- Remote entity interpolation before drawing.
- Window resize handling and fullscreen toggle.
- Graceful shutdown: close socket, close audio device.

Load order
- Loaded fifth among net files: `net-protocol` -> `net-auth` -> `net-snapshot` -> `net-server` -> `net-client` -> `net`.
- Depends on all preceding net files, plus `rendering`, `input`, `ui`, and `audio` modules.
