# net.lisp

Purpose
- Provide the initial UDP client/server split with lightweight snapshot streaming.

Why we do it this way
- Keeps transport minimal while reusing the existing snapshot serializer.
- Uses intent-only client input to preserve server authority.
- Datagram transport keeps the loop simple for the first networking cut.

Message format (plist, printed with `prin1`)

**Client -> Server:**
- `(:type :hello)` -> client announces itself to the server
- `(:type :register :username <str> :password <str>)` -> create new account
- `(:type :login :username <str> :password <str>)` -> authenticate existing account
- `(:type :logout)` -> graceful disconnect (saves player state)
- `(:type :intent :payload <intent-plist>)` -> client input for the current frame
- `(:type :save)` -> request server save
- `(:type :load)` -> request server load

**Server -> Client:**
- `(:type :auth-ok :player-id <id>)` -> authentication successful
- `(:type :auth-fail :reason <keyword>)` -> authentication failed
  - Reasons: `:missing-credentials`, `:username-taken`, `:bad-credentials`, `:already-logged-in`
- `(:type :snapshot :state <game-state> :events (<event> ...) :player-id <id> :last-sequence <n>)` -> server snapshot + HUD/combat events + assigned player id + prediction sequence

**Encrypted auth (optional):**
- When `*auth-encryption-enabled*` is true, auth messages use `:encrypted-payload` instead of plaintext credentials
- See `docs/db.md` "Network Encryption" section for protocol details

Key functions

**Server/Client Entry Points:**
- `run-server`: bind UDP, track clients, handle auth, apply intents, tick `server-step`, send snapshots. Accepts `:worker-threads` for parallel sends.
- `run-client`: connect to server, handle login UI, send intents, apply snapshots. Accepts `:auto-login-username`/`:auto-login-password` for testing.

**Message Handling:**
- `send-net-message` / `receive-net-message`: ASCII message helpers with UDP buffers; malformed packets dropped.
- `send-net-message-with-retry`: Send critical messages (auth responses) with exponential retry.
- `send-auth-message`: Send login/register with optional encryption.
- `intent->plist` / `apply-intent-plist`: serialize/deserialize intent payloads.
- `apply-snapshot`: apply game state, update player id, queue events; clears interpolation buffer on zone change.

**Authentication:**
- `handle-register-request`: Create account, spawn character, register session.
- `handle-login-request`: Verify credentials, load character, register session.
- `handle-logout-request`: Save player, unregister session.
- `extract-auth-credentials`: Parse credentials from encrypted or plaintext auth messages.

**Session Management:**
- `*active-sessions*`: Hash table mapping username -> net-client for logged-in accounts.
- `*client-timeout-seconds*`: Inactivity threshold (default 30s) before auto-logout.
- `check-client-timeouts`: Remove inactive clients, free sessions.
- `net-client-authenticated-p`: Check if client has logged in.
- `net-client-account-username`: Get username of logged-in account.

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
- Accurate tick timing: Server tracks frame processing time and only sleeps for remaining duration. This ensures consistent tick rate regardless of frame complexity (no slowdown under load).
- Optional parallel snapshot sending: Use `worker-threads` parameter to parallelize network sends across multiple threads.
  - Default: 1 (serial sending, simple)
  - Recommended for high client counts: `(get-nproc)` to use all CPU cores
  - Safe: Only network I/O is parallelized, simulation remains deterministic and serial
- See `SERVER_PERFORMANCE.md` for detailed scaling strategies and threading considerations.
