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
- `run-server`: bind UDP on `*net-default-host*`/`*net-default-port*`, track multiple clients, apply intents, tick `server-step`, send per-client snapshots. Logs handshake/save/load traffic and unknown message types when verbose.
- `run-client`: connect to the server, send intents each frame, apply snapshots for rendering. Logs unknown message types when verbose.
- `send-net-message` / `receive-net-message`: ASCII message helpers with UDP buffers; malformed packets are dropped with verbose logs.
- `intent->plist` / `apply-intent-plist`: serialize/deserialize intent payloads.
- `apply-snapshot`: apply a snapshot via `apply-game-state`, update the client player id, and queue HUD/combat events; logs zone transitions when verbose.

Design note
- Serialization is ASCII for now; keep payloads under `*net-buffer-size*`.
- `*read-eval*` is disabled during message parsing for safety.
- Snapshots include visual fields by calling `serialize-game-state` with `:include-visuals`.
- Server uses non-blocking UDP receive via `usocket:wait-for-input` with `:timeout 0`.
- Fatal runtime errors log context (and a backtrace in verbose mode) before exiting.

Security: Input Validation
- Server validates ALL client input before processing (untrusted client principle).
- Authentication required: Server ignores intents from unauthenticated clients (`net-client-authenticated-p`).
- Type validation helpers prevent type confusion attacks:
  - `%float-or` - Reject non-float values (default to safe value)
  - `%int-or` - Reject non-integer values (prevents string/list injection in ID fields)
  - `%sanitize-chat-message` - Enforce string type + length limit (`*chat-max-length*`)
- Speed hack prevention: Server uses own `*player-speed*` constant, ignores magnitude of move-dx/dy.
- Double-login prevention: `*active-sessions*` tracks logged-in usernames; second login attempt rejected.
- Malformed packet handling: Invalid plists or types are dropped gracefully without crashing.
- See `scripts/test-security.lisp` for the security test suite (7 tests).

Client-Side Interpolation
- Remote entities (other players, NPCs) are rendered slightly in the past for smooth movement.
- Interpolation is automatic and always-on (no toggle needed).
- Configuration:
  - `*interpolation-delay-seconds*` - Render delay (default 0.1 = 100ms). Higher = smoother, more perceived lag.
- Key functions:
  - `make-interpolation-buffer` - Create ring buffer for snapshot history
  - `capture-entity-positions` - Capture entity positions after snapshot applied
  - `find-interpolation-bounds` - Find two snapshots bracketing render time
  - `interpolate-remote-entities` - Apply lerped positions before drawing
- Zone transitions clear the interpolation buffer (stale positions are invalid).

Client-Side Prediction (Optional)
- Local player movement can be predicted client-side for instant feedback.
- Controlled by `*client-prediction-enabled*` flag (default nil = disabled).
- Toggle via SLIME: `(setf *client-prediction-enabled* t)` takes effect immediately.
- When enabled:
  - Client applies local movement immediately using same physics as server
  - Intent messages include `:sequence` number for tracking
  - Server snapshots include `:last-sequence` for reconciliation
  - Mispredictions beyond `*prediction-error-threshold*` (default 5.0 pixels) snap to server position
- Key functions:
  - `make-prediction-state` - Create prediction state for local player
  - `store-prediction-input` - Buffer input with sequence number
  - `apply-local-prediction` - Apply movement locally for instant feedback
  - `reconcile-prediction` - Compare server state to prediction, correct if needed
- Mispredictions are logged when `*verbose*` is enabled.

Performance & Scaling
- Current server runs ONE zone (tested smooth with hundreds of entities on client).
- For 10k users @ 500/zone: run 20 separate server processes (horizontal scaling).
- Snapshot optimization: state serialized once per frame, shared across all clients.
- Optional parallel snapshot sending: Use `worker-threads` parameter to parallelize network sends across multiple threads.
  - Default: 1 (serial sending, simple)
  - Recommended for high client counts: `(get-nproc)` to use all CPU cores
  - Safe: Only network I/O is parallelized, simulation remains deterministic and serial
- See `SERVER_PERFORMANCE.md` for detailed scaling strategies and threading considerations.
