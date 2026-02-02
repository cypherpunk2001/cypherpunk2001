# net.lisp (and net-*.lisp modules)

**Module structure:** `net.lisp` is a thin glue file. The networking code is split into:
- `net-protocol.lisp` — message formats, encode/decode, validation, client management
- `net-auth.lisp` — login/register handling, auth queues, rate limiting, session management
- `net-snapshot.lisp` — snapshot send/receive, delta compression, interpolation, prediction
- `net-server.lisp` — server UDP loop, dispatch, connection tracking (`run-server`)
- `net-client.lisp` — client networking loop, login UI, snapshot application (`run-client`)

See `docs/net-protocol.md`, `docs/net-auth.md`, `docs/net-snapshot.md`, `docs/net-server.md`, `docs/net-client.md` for per-file details.

Purpose
- Provide the UDP client/server split with optimized snapshot streaming.

Why we do it this way
- Keeps transport minimal while reusing the existing snapshot serializer.
- Uses intent-only client input to preserve server authority.
- Datagram transport keeps the loop simple and low-latency.
- Compact serialization and delta compression support hundreds of concurrent players.

Message format (plist, printed with `prin1`)

**Client -> Server:**
- `(:type :hello)` -> client announces itself to the server
- `(:type :register :username <str> :password <str>)` -> create new account
- `(:type :login :username <str> :password <str>)` -> authenticate existing account
- `(:type :logout)` -> graceful disconnect (saves player state)
- `(:type :intent :payload <intent-plist>)` -> client input for the current frame

**Server -> Client:**
- `(:type :hello-ack)` -> server is online (response to :hello ping)
- `(:type :auth-ok :player-id <id>)` -> authentication successful
- `(:type :auth-fail :reason <keyword>)` -> authentication failed
  - Reasons: `:rate-limited`, `:missing-credentials`, `:username-taken`, `:bad-credentials`,
    `:already-logged-in`, `:ownership-conflict`, `:account-quarantined`, `:data-corrupted`,
    `:load-failed`, `:internal-error`, `:wrong-zone`
  - When `:wrong-zone`, the server includes `:zone-id` with the player's saved zone.
- `(:type :snapshot :format <format> :state <game-state> ...)` -> server snapshot
  - Formats: `:compact-v5` (full state), `:delta-v5` (changed entities only)
  - v5 removes attack/follow target IDs from public player vectors and keeps zone-id-hash
    for client filtering (player index 19, NPC index 14)
- `(:type :snapshot-chunk :seq <n> :chunk <i> :total <n> :data <str>)` -> fragmented snapshot piece
- `(:type :private-state :player-id <id> :payload <plist>)` -> owner-only inventory/equipment/stats update

**Encrypted auth (optional):**
- When `*auth-encryption-enabled*` is true, auth messages use `:encrypted-payload` instead of plaintext credentials
- See `docs/db.md` "Network Encryption" section for protocol details

Key functions

**Server/Client Entry Points:**
- `run-server`: Initialize storage backend, load Lua scripts for atomic operations, bind UDP, track clients, handle auth, apply intents, tick `server-step`, send snapshots. Accepts `:worker-threads` for parallel sends.
- `run-client`: connect to server, handle login UI, send intents, apply snapshots. Accepts `:auto-login-username`/`:auto-login-password` for testing.

**Message Handling:**
- `send-net-message` / `receive-net-message`: ASCII message helpers with UDP buffers; malformed packets dropped.
- `send-net-message-with-retry`: Send critical messages (auth responses) with linear retry.
- `send-auth-message`: Send login/register with optional encryption.
- `intent->plist` / `apply-intent-plist`: serialize/deserialize intent payloads.
- `apply-snapshot`: apply game state, update player id, queue events; clears interpolation buffer on zone change.
- `apply-private-state`: apply owner-only inventory/equipment/stats updates to the local player.

**Authentication:**
- `handle-register-request`: Create account, spawn character, register session. (Legacy, replaced by async)
- `handle-login-request`: Verify credentials, load character, register session. (Legacy, replaced by async)
- `handle-logout-request`: Save player, unregister session.
- `extract-auth-credentials`: Parse credentials from encrypted or plaintext auth messages.

**Async Auth Worker (Non-Blocking Login):**
- Auth requests (register/login) are processed asynchronously to prevent game freezes during DB operations.
- Architecture: Main loop queues auth requests -> Worker thread processes DB operations -> Main loop integrates results
- `auth-request-queue` / `auth-result-queue`: Thread-safe queues for async communication.
- `process-register-async`: Worker thread processes registration (db-create-account, spawn, db-set-character-id).
- `process-login-async`: Worker thread processes login (db-verify-credentials, db-load-player).
- `integrate-auth-results`: Main thread integrates completed results (add player to game, send response).
- `start-auth-worker` / `stop-auth-worker`: Lifecycle management for worker thread.
- Benefits: Game world continues smoothly while 10+ players log in simultaneously.

**Session Management:**
- `*active-sessions*`: Hash table mapping username -> net-client for logged-in accounts.
- `*client-timeout-seconds*`: Inactivity threshold (default 30s) before auto-logout.
- `check-client-timeouts`: Remove inactive clients, free sessions.
- `net-client-authenticated-p`: Check if client has logged in.
- `net-client-account-username`: Get username of logged-in account.
- `register-player-session`: Claims Redis session ownership (rejects double-logins), adds player to online set, updates leaderboards.
- `unregister-player-session`: Releases Redis session ownership, removes from online set.

**Zone Tracking:**
- `net-client-zone-id`: Cached zone-id for this client, used to detect zone changes.
  - Initialized to player's zone-id on auth completion
  - Updated when zone change detected in broadcast loop
  - Reset to nil on logout/disconnect
- `group-clients-by-zone`: Groups authenticated clients by their player's zone-id. Nil zone-ids are clamped to `*starting-zone-id*`.
- `occupied-zone-ids`: Returns list of unique zone-ids where players are currently located.
- `players-in-zone`: Returns array of players matching a specific zone-id.

Design note
- Serialization is ASCII for now; keep payloads under `*net-buffer-size*`.
- `*read-eval*` is disabled during message parsing for safety.
- Snapshots use `serialize-game-state-compact` / `serialize-game-state-delta`; compact vectors always include visual fields.
- Server uses non-blocking UDP receive via `usocket:wait-for-input` with `:timeout 0`.
- Fatal runtime errors log context (and a backtrace in verbose mode) before exiting.
- Private state (inventory/equipment/stats) is sent via `:private-state` messages to the owning client only.

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
- See `tests/security-test.lisp` (driven by `scripts/test-security.lisp`) for the security test suite (23 tests).

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
  - Player compact vectors include `:last-sequence` (server's last processed input)
  - Mispredictions beyond `*prediction-error-threshold*` (default 5.0 pixels) snap to server position
- Key functions:
  - `make-prediction-state` - Create prediction state for local player
  - `store-prediction-input` - Buffer input with sequence number
  - `apply-local-prediction` - Apply movement locally for instant feedback
  - `reconcile-prediction` - Compare server state to prediction, correct if needed
- Mispredictions are logged when `*verbose*` is enabled.

Performance & Scaling
- Single server process can host multiple zones; snapshots and NPC simulation are filtered per zone.
- Player and NPC collision use per-zone bounds from zone-state wall maps (see `docs/movement.md`).
- For 10k users @ 500/zone: run 20 separate server processes (horizontal scaling).
- Snapshot optimization:
  - State serialized once per zone per frame via `serialize-game-state-for-zone`
  - Snapshot encoded to bytes once per zone and sent to all clients in that zone (encode-once-send-many)
  - Previously encoded O(clients × state_size), now O(zone_count × state_size)
- Accurate tick timing: Server tracks frame processing time and only sleeps for remaining duration. This ensures consistent tick rate regardless of frame complexity (no slowdown under load).
- Optional parallel snapshot sending: Use `worker-threads` parameter to parallelize network sends across multiple threads.
  - Default: 1 (serial sending, simple)
  - Recommended for high client counts: `(get-nproc)` to use all CPU cores
  - Safe: Only network I/O is parallelized, simulation remains deterministic and serial
- See `SERVER_PERFORMANCE.md` for detailed scaling strategies and threading considerations.

---

## Snapshot Size Optimization (4-Prong Approach)

### Implementation Status

| Prong | Description | Status |
|-------|-------------|--------|
| **Prong 1** | Compact Serialization | ✅ IMPLEMENTED |
| **Prong 2** | Delta Compression | ✅ IMPLEMENTED |
| **Prong 3** | UDP Fragmentation | ✅ IMPLEMENTED |
| **Prong 4** | TCP Fallback | ❌ Not needed yet |

### Problem Statement (Historical)

**Observed behavior during stress testing:**

| Player Count | Snapshot Size | Status |
|--------------|---------------|--------|
| 0-200 | <65KB | Smooth |
| 200-300 | ~65KB | At limit |
| 300-500 | 65-116KB | Degraded |
| 500+ | 116KB+ | Broken |

**Root cause:** UDP datagram limit is 65,507 bytes. Original plist serialization produced ~232 bytes per player.

**Solution:** Three-prong optimization reduced this dramatically.

---

### Prong 1: Compact Serialization ✅ IMPLEMENTED

**Location:** `src/save.lisp`

Replaces verbose plist serialization with compact vector format:

```lisp
;; OLD: ~232 bytes per player
(:id 5 :x 123.4 :y 567.8 :hp 100 :dx 1.0 :dy 0.0 :anim-state :walking ...)

;; NEW: ~64 bytes per player
#(5 1234 5678 100 10 0 1 3 1 0 0 9 0 0 0)
```

**Key functions:**
- `serialize-player-compact` / `deserialize-player-compact` - Player vector encoding
- `serialize-npc-compact` / `deserialize-npc-compact` - NPC vector encoding
- `serialize-game-state-compact` - Full game state in compact format
- `quantize-coord` / `dequantize-coord` - Position quantization (0.1 precision)
- `quantize-timer` / `dequantize-timer` - Timer quantization (0.01 precision)
- `pack-player-flags` / `unpack-player-flags` - Boolean packing
- `encode-anim-state` / `decode-anim-state` - Enum encoding

**Private state:** Inventory, equipment, and stats are excluded from compact snapshots and sent
to the owning client via `:private-state` messages.

**Player vector fields:** The compact player vector includes `run-stamina` at index 17,
`last-sequence` at index 18 (server's last processed input sequence for prediction), and
`zone-id-hash` at index 19 (djb2 hash of zone-id for client-side filtering).

**NPC vector fields:** The compact NPC vector includes `zone-id-hash` at index 14 for
client-side zone filtering.

**Optimizations applied:**
1. **Positional encoding**: Fixed-position vector instead of keyword plists
2. **Float quantization**: Coordinates scaled by 10, timers by 100
3. **Boolean packing**: 4 booleans packed into single integer
4. **Enum compression**: Animation states and facing as small integers

**Result:** ~232 bytes → ~64 bytes per player (72% reduction)

---

### Prong 2: Delta Compression ✅ IMPLEMENTED

**Location:** `src/save.lisp`, `src/net.lisp`

Only transmits entities that changed since last acknowledged snapshot.

**Protocol:**
```lisp
;; Server tracks per-client state
(defstruct net-client
  ...
  last-acked-seq        ; Sequence number client confirmed
  needs-full-resync)    ; T after zone change or reconnect

;; Delta snapshot format
(:format :delta-v5
 :seq 12345
 :baseline-seq 12340
 :zone-id :world
 :changed-players #(...)    ; Only dirty players (with zone-id-hash)
 :changed-npcs #(...)       ; Only dirty NPCs (with zone-id-hash)
 :objects (...))            ; Objects with respawn state
```

**Key functions:**
- `serialize-game-state-delta` - Collect only dirty entities
- `deserialize-game-state-delta` - Apply incremental changes, add new players
- `clear-snapshot-dirty-flags` - Reset dirty flags after broadcast
- `broadcast-snapshots-with-delta` - Partition clients into resync vs delta groups

**Dirty flag system:**
- `player-snapshot-dirty` - Set when player state changes
- `npc-snapshot-dirty` - Set when NPC state changes
- Object `:snapshot-dirty` - Set when zone object respawns

**Client partitioning:**
```lisp
;; Each frame, server partitions clients:
resync-clients  → Get full compact snapshot (new connections, zone changes)
delta-clients   → Get delta snapshot (synced clients)

Resync triggers when the client's last ack is missing or the gap exceeds `*max-delta-gap*`
(or `*max-delta-age*` for long stalls).

;; Encode-once optimization preserved: each group gets single encoded message
```

**Result:** 90%+ reduction for typical gameplay (most entities idle)

---

### Prong 3: UDP Fragmentation ✅ IMPLEMENTED

**Location:** `src/net.lisp`

Handles snapshots larger than 65KB by splitting into numbered chunks.

**Chunk format:**
```lisp
(:type :snapshot-chunk
 :seq 12345           ; Snapshot sequence
 :chunk 0             ; Chunk index (0-based)
 :total 3             ; Total chunk count
 :data "...")         ; Partial payload
```

**Key functions:**
- `send-snapshot-chunks` - Split large snapshot into chunks
- `receive-snapshot-chunk` - Reassemble chunks on client
- `chunk-buffer` struct - Client-side reassembly buffer

**Reassembly:**
- Client maintains `chunk-buffer` with received chunks
- When all chunks received, concatenates and decodes
- Timeout discards incomplete snapshots (next snapshot corrects state)

**Trigger:** Automatic when encoded snapshot exceeds `*net-buffer-size*`

---

### Prong 4: TCP Fallback ❌ NOT IMPLEMENTED

Reserved for future if UDP fragmentation proves unreliable.

**When we'd need it:**
- Severe packet loss environments
- Snapshots consistently exceeding fragmentation limits
- Deployment environments blocking UDP

**Current status:** Not needed. Prongs 1-3 handle all tested scenarios.

---

### Zone Object Sync

**Added for object pickup/respawn in client/server mode.**

**What syncs:**
- Object position (`:x`, `:y`)
- Object count (`:count`) - items available
- Respawn timer (`:respawn`) - seconds until respawn
- Dirty flag (`:snapshot-dirty`) - triggers sync when respawn completes

**Serialization path:**
```lisp
;; In serialize-game-state-compact / serialize-game-state-delta:
(when objects
  (dolist (object objects)
    (when (or (> respawn 0.0) dirty)  ; Respawning OR just respawned
      (push (serialize-object object) object-list))))
```

**Client-side:**
```lisp
;; In deserialize-game-state-delta:
;; Match server objects by (id, x, y) and update local respawn/count
```

**IMPORTANT:** Zone objects are plists loaded from data files. All mutable keys must be initialized when loading:
```lisp
;; In zone.lisp load-zone:
(list :id (getf obj :id)
      :x (getf obj :x)
      :y (getf obj :y)
      :count (getf obj :count nil)      ;; REQUIRED for setf getf
      :respawn 0.0                      ;; REQUIRED for setf getf
      :respawnable (getf obj :respawnable t)
      :snapshot-dirty nil)              ;; REQUIRED for setf getf
```

See `docs/PLIST_SETF_GETF_PITFALL.md` for why this initialization is critical.

---

### Private State Sync

**Inventory, equipment, and stats are owner-only.** Compact snapshots omit them entirely.

**Server-side:** When `player-inventory-dirty` or `player-hud-stats-dirty` is set, the server
queues a `:private-state` message for that client (see `send-private-states`).

**Client-side:** `apply-private-state` routes the payload to `apply-player-private-plist`,
refreshing inventory/HUD caches locally.

---

### Verification

```bash
# Basic tests
make checkparens && make ci && make test-persistence

# Visual verification
make smoke

# Stress test (measures snapshot sizes, player capacity)
STRESS_CLIENTS=100 make stress
```

### Current Capacity

| Scenario | Bytes/Player | Max Players (smooth) |
|----------|--------------|---------------------|
| All moving | ~64 | ~1000 |
| 10% moving (typical) | ~6.4 avg | 2000+ |
| Idle | ~0 (delta) | 5000+ |

---

### Configuration

| Variable | Default | Description |
|----------|---------|-------------|
| `*delta-compression-enabled*` | `t` | Enable Prong 2 delta compression |
| `*net-buffer-size*` | 65507 | UDP datagram limit |
| `*chunk-timeout*` | 1.0 | Seconds before discarding incomplete chunks |
| `*max-delta-age*` | 60 | Max snapshots behind before forcing resync |
| `*max-delta-gap*` | 5 | Max snapshot gap tolerated before forcing resync |
