# net.lisp

Purpose
- Provide the initial UDP client/server split with lightweight snapshot streaming.

Why we do it this way
- Keeps transport minimal while reusing the existing snapshot serializer.
- Uses intent-only client input to preserve server authority.
- Datagram transport keeps the loop simple for the first networking cut.

Message format (plist, printed with `prin1`)
- `(:type :hello)` -> client announces itself to the server
- `(:type :intent :payload <intent-plist>)` -> client input for the current frame
- `(:type :save)` -> request server save
- `(:type :load)` -> request server load
- `(:type :snapshot :state <game-state> :events (<event> ...) :player-id <id>)` -> server snapshot + HUD/combat events + assigned player id for this client

Key functions
- `run-server`: bind UDP on `*net-default-host*`/`*net-default-port*`, track multiple clients, apply intents, tick `server-step`, send per-client snapshots.
- `run-client`: connect to the server, send intents each frame, apply snapshots for rendering.
- `send-net-message` / `receive-net-message`: ASCII message helpers with UDP buffers.
- `intent->plist` / `apply-intent-plist`: serialize/deserialize intent payloads.
- `apply-snapshot`: apply a snapshot via `apply-game-state`, update the client player id, and queue HUD/combat events.

Design note
- Serialization is ASCII for now; keep payloads under `*net-buffer-size*`.
- `*read-eval*` is disabled during message parsing for safety.
- Snapshots include visual fields by calling `serialize-game-state` with `:include-visuals`.
- Server installs SIGTERM/SIGINT handlers (SBCL) to exit cleanly.
