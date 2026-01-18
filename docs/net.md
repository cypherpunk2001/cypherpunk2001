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
- Snapshot optimization:
  - State serialized once per frame via `serialize-game-state`
  - Snapshot encoded to bytes ONCE and sent to all clients (encode-once-send-many)
  - Previously encoded O(clients × state_size), now O(state_size) - critical for 40+ clients
- Accurate tick timing: Server tracks frame processing time and only sleeps for remaining duration. This ensures consistent tick rate regardless of frame complexity (no slowdown under load).
- Optional parallel snapshot sending: Use `worker-threads` parameter to parallelize network sends across multiple threads.
  - Default: 1 (serial sending, simple)
  - Recommended for high client counts: `(get-nproc)` to use all CPU cores
  - Safe: Only network I/O is parallelized, simulation remains deterministic and serial
- See `SERVER_PERFORMANCE.md` for detailed scaling strategies and threading considerations.

---

## Going Forward: Snapshot Size Optimization (4-Prong Approach)

### Problem Statement

**Observed behavior during stress testing:**

| Player Count | Snapshot Size | Status |
|--------------|---------------|--------|
| 0-200 | <65KB | Smooth |
| 200-300 | ~65KB | At limit |
| 300-500 | 65-116KB | Degraded (noticeable slowdown) |
| 500+ | 116KB+ | Broken (snapshots dropped) |

**Root cause:** UDP datagram limit is 65,507 bytes. Current serialization produces ~232 bytes per player in network-only mode. At 300 players: 300 × 232 = 69,600 bytes (exceeds limit).

**Goal:** Support 500+ players per zone, targeting 2,000 players per zone.

**Approach:** Implement four complementary optimizations in sequence, measuring impact of each before proceeding.

---

### Current Snapshot Architecture

**Message format** (`net.lisp:928`):
```lisp
(:type :snapshot
 :state <game-state-plist>
 :events (<event-plists>...))
```

**Serialization path:**
1. `serialize-game-state` (save.lisp:412) creates plist with `:players`, `:npcs`, `:objects`
2. `serialize-player` with `:network-only t` (save.lisp:103) produces minimal player plist
3. `encode-net-message` converts to ASCII string via `prin1-to-string`
4. `string-to-octets` converts to UTF-8 bytes
5. Single encoded buffer sent to all clients (encode-once-send-many optimization)

**Current player network-only serialization** (save.lisp:111-132):
```lisp
(:id <int> :x <float> :y <float> :hp <int>
 :dx <float> :dy <float>
 :anim-state <keyword> :facing <keyword>
 :facing-sign <int> :frame-index <int> :frame-timer <float>
 :attacking <bool> :attack-hit <bool>
 :hit-active <bool> :hit-frame <int>
 :hit-facing <keyword> :hit-facing-sign <int>
 :running <bool> :attack-timer <float>)
```

**Byte breakdown per player (ASCII plist):**
- Keywords: `:id`, `:x`, `:y`, etc. = ~120 bytes of key names
- Values: floats print as "123.456", integers as "42" = ~80 bytes
- Structural: spaces, parens = ~32 bytes
- **Total: ~232 bytes/player**

**NPC serialization** (save.lisp:273-296) adds similar overhead per NPC.

---

### Prong 1: Compact Serialization

**Goal:** Reduce per-entity bytes from ~232 to <100 bytes without changing protocol.

#### 1.1 Positional Encoding (Replace Keywords with Indices)

Current format wastes bytes on repeated keyword names:
```lisp
(:id 5 :x 123.4 :y 567.8 :hp 100 :dx 1.0 :dy 0.0 ...)
```

Compact format uses fixed-position vector:
```lisp
#(5 123.4 567.8 100 1.0 0.0 ...)  ; [id x y hp dx dy ...]
```

**Implementation:**
- Define `*player-snapshot-fields*` ordering in `config.lisp`
- New function `serialize-player-compact` returns simple-vector
- New function `deserialize-player-compact` uses position-based access
- Version field in snapshot header: `:format :compact-v1`

**Field ordering (24 fields):**
```lisp
(defparameter *player-snapshot-fields*
  '(id x y hp dx dy
    anim-state facing facing-sign
    frame-index frame-timer
    attacking attack-hit attack-timer
    hit-active hit-frame hit-facing hit-facing-sign
    running
    ;; Reserved slots for future fields
    nil nil nil nil nil))
```

**Estimated savings:** 120 bytes (keywords) → 0 bytes = ~52% reduction

#### 1.2 Float Quantization

Current: Floats print as "123.45678901" (12+ chars)
Optimized: Fixed-precision printing or integer scaling

**Position coordinates:**
- World coordinates range: 0-10000 pixels typical
- Precision needed: 0.1 pixel (sub-pixel rendering)
- Solution: Scale by 10, transmit as integer: `1234` instead of `123.4`
- Savings: ~6 bytes per float × 4 floats = 24 bytes/entity

**Timers (frame-timer, attack-timer):**
- Range: 0.0 - 5.0 seconds typical
- Precision: 0.01 seconds sufficient
- Solution: Transmit as centiseconds integer: `150` instead of `1.5`

**Implementation in `serialize-player-compact`:**
```lisp
(defun quantize-coord (f) (round (* f 10)))      ; 0.1 precision
(defun quantize-timer (f) (round (* f 100)))     ; 0.01 precision
(defun dequantize-coord (i) (/ i 10.0))
(defun dequantize-timer (i) (/ i 100.0))
```

#### 1.3 Boolean Packing

Current: Each boolean is `T` or `NIL` (1-3 chars each)
Optimized: Pack 8 booleans into single byte

**Boolean fields in player snapshot:**
- `attacking`, `attack-hit`, `hit-active`, `running` = 4 booleans

**Implementation:**
```lisp
(defun pack-player-flags (player)
  (logior (if (player-attacking player) 1 0)
          (if (player-attack-hit player) 2 0)
          (if (player-hit-active player) 4 0)
          (if (player-running player) 8 0)))

(defun unpack-player-flags (flags)
  (values (logbitp 0 flags)   ; attacking
          (logbitp 1 flags)   ; attack-hit
          (logbitp 2 flags)   ; hit-active
          (logbitp 3 flags))) ; running
```

#### 1.4 Enum Compression

Current: `:anim-state :walking`, `:facing :right` (long keywords)
Optimized: Map to small integers

**Animation states** (define in `config.lisp`):
```lisp
(defparameter *anim-state-codes*
  '((:idle . 0) (:walking . 1) (:attacking . 2) (:hit . 3) (:dead . 4)))

(defparameter *facing-codes*
  '((:up . 0) (:down . 1) (:left . 2) (:right . 3)))
```

#### 1.5 Compact Format Summary

**New serialized player (vector of integers):**
```lisp
#(player-id          ; int
  x*10 y*10          ; quantized coords
  hp                 ; int
  dx*10 dy*10        ; quantized velocity (or direction enum)
  anim-code          ; 0-4
  facing-code        ; 0-3
  facing-sign        ; -1 or 1
  frame-index        ; 0-10
  frame-timer*100    ; quantized
  flags              ; packed booleans
  attack-timer*100   ; quantized
  hit-frame          ; 0-5
  hit-facing-code    ; 0-3
  hit-facing-sign)   ; -1 or 1
```

**Estimated size:** 16 integers × ~4 chars avg = ~64 bytes/player (vs 232 current)
**Capacity at 65KB:** 65000 / 64 = ~1000 players

#### 1.6 Implementation Files

| File | Changes |
|------|---------|
| `src/config.lisp` | Add `*player-snapshot-fields*`, `*anim-state-codes*`, `*facing-codes*` |
| `src/save.lisp` | Add `serialize-player-compact`, `deserialize-player-compact` |
| `src/net.lisp` | Add `:format` field to snapshot, use compact serialization |
| `tests/persistence-test.lisp` | Add round-trip tests for compact format |

#### 1.7 Verification

```bash
# Before: measure current bytes/player
MMORPG_VERBOSE=1 make server  # Log snapshot sizes

# After: compare
make test-persistence  # Ensure no data loss
make smoke             # Visual verification
STRESS_RATE=10 make stress  # Load test to 500+ players
```

---

### Prong 2: Delta Compression

**Goal:** Only transmit entities that changed since client's last acknowledged snapshot.

#### 2.1 Sequence Number Protocol

**Current:** Server sends snapshots, client applies blindly.

**Enhanced protocol:**
1. Each snapshot includes `:seq N` (monotonic sequence number)
2. Client sends `:ack N` in intent messages for last received snapshot
3. Server tracks `last-acked-seq` per client
4. Server computes delta: entities changed since `last-acked-seq`

**Message format changes:**

```lisp
;; Server -> Client (enhanced snapshot)
(:type :snapshot
 :seq 12345                    ; NEW: sequence number
 :format :delta-v1             ; NEW: format indicator
 :baseline-seq 12340           ; NEW: client's last ack (delta base)
 :full-state nil               ; full state only on zone change or resync
 :changed-players #(...)       ; only players that moved/changed
 :changed-npcs #(...)          ; only NPCs that changed
 :removed-ids (5 12 89)        ; entities that left the zone
 :events (...))

;; Client -> Server (add ack to intent)
(:type :intent
 :ack 12344                    ; NEW: last received snapshot seq
 :payload <intent-plist>)
```

#### 2.2 Server-Side Change Tracking

**Per-entity dirty flags** (add to player/npc structs):
```lisp
;; In types.lisp, add to player struct:
snapshot-dirty      ; t if changed since last broadcast
last-snapshot-seq   ; seq when last included in snapshot
```

**Change detection in simulation** (server.lisp, combat.lisp, movement.lisp):
```lisp
;; After any state change:
(setf (player-snapshot-dirty player) t)

;; Example locations to mark dirty:
;; - movement.lisp: after position update
;; - combat.lisp: after HP change, attack state change
;; - ai.lisp: after NPC state change
```

**Per-client state tracking** (net.lisp):
```lisp
;; Add to net-client struct:
last-acked-seq      ; sequence number client confirmed
baseline-state      ; hash-table of entity-id -> last-sent-state (for delta calc)
needs-full-resync   ; t after zone change or timeout
```

#### 2.3 Delta Computation Algorithm

```lisp
(defun compute-snapshot-delta (client game current-seq)
  "Compute minimal delta snapshot for CLIENT based on their last-acked-seq."
  (let ((baseline-seq (net-client-last-acked-seq client))
        (changed-players nil)
        (changed-npcs nil)
        (removed-ids nil))

    ;; If no ack or too old, send full state
    (when (or (null baseline-seq)
              (> (- current-seq baseline-seq) *max-delta-age*))
      (return-from compute-snapshot-delta
        (make-full-snapshot game current-seq)))

    ;; Collect changed players
    (loop for player across (game-players game)
          when (and player (player-snapshot-dirty player))
          do (push (serialize-player-compact player) changed-players))

    ;; Collect changed NPCs
    (loop for npc across (game-npcs game)
          when (and npc (npc-snapshot-dirty npc))
          do (push (serialize-npc-compact npc) changed-npcs))

    ;; Track removed entities (logged out, died, left zone)
    ;; Compare current entity set vs baseline-state

    (list :seq current-seq
          :baseline-seq baseline-seq
          :changed-players (coerce (nreverse changed-players) 'vector)
          :changed-npcs (coerce (nreverse changed-npcs) 'vector)
          :removed-ids removed-ids)))
```

#### 2.4 Dirty Flag Reset

After broadcasting snapshot to all clients:
```lisp
(defun clear-snapshot-dirty-flags (game)
  "Reset dirty flags after snapshot broadcast."
  (loop for player across (game-players game)
        when player do (setf (player-snapshot-dirty player) nil))
  (loop for npc across (game-npcs game)
        when npc do (setf (npc-snapshot-dirty npc) nil)))
```

#### 2.5 Client-Side Delta Application

```lisp
(defun apply-delta-snapshot (game delta)
  "Apply incremental changes from delta snapshot."
  ;; Update changed entities
  (loop for player-data across (getf delta :changed-players)
        do (apply-player-compact-update game player-data))

  (loop for npc-data across (getf delta :changed-npcs)
        do (apply-npc-compact-update game npc-data))

  ;; Remove departed entities
  (loop for id in (getf delta :removed-ids)
        do (remove-entity-by-id game id)))
```

#### 2.6 Full Resync Triggers

Send full snapshot (not delta) when:
- Client connects or reconnects
- Client changes zones
- Client's `last-acked-seq` is too old (> 60 snapshots behind)
- Server detects desync (via checksum or explicit request)

#### 2.7 Estimated Savings

**Typical scenario:** 500 players, 10% moving at any moment
- Current: 500 × 64 bytes = 32,000 bytes per snapshot
- With delta: 50 × 64 bytes = 3,200 bytes per snapshot
- **Reduction: 90%**

**Worst case:** All players moving (battle, event)
- Falls back to near-full snapshots
- Still benefits from compact serialization (Prong 1)

#### 2.8 Implementation Files

| File | Changes |
|------|---------|
| `src/types.lisp` | Add `snapshot-dirty`, `last-snapshot-seq` to player/npc |
| `src/net.lisp` | Add `last-acked-seq`, delta computation, `:ack` handling |
| `src/movement.lisp` | Mark player dirty on position change |
| `src/combat.lisp` | Mark entities dirty on HP/attack state change |
| `src/ai.lisp` | Mark NPCs dirty on behavior state change |
| `src/save.lisp` | Add `serialize-player-compact`, delta application |

---

### Prong 3: UDP Fragmentation

**Goal:** Handle snapshots larger than 65KB by splitting into numbered chunks.

#### 3.1 Chunk Protocol

**Fragmented snapshot format:**
```lisp
;; First chunk (index 0) - includes metadata
(:type :snapshot-chunk
 :seq 12345           ; snapshot sequence
 :chunk 0             ; chunk index (0-based)
 :total 3             ; total chunk count
 :format :compact-v1  ; serialization format
 :data "...")         ; partial payload

;; Subsequent chunks
(:type :snapshot-chunk
 :seq 12345
 :chunk 1
 :total 3
 :data "...")
```

#### 3.2 Chunk Size Calculation

```lisp
(defparameter *chunk-overhead* 100)  ; bytes for chunk header
(defparameter *max-chunk-payload* (- *net-buffer-size* *chunk-overhead*))
;; = 65507 - 100 = 65407 bytes per chunk payload
```

#### 3.3 Server-Side Fragmentation

```lisp
(defun send-fragmented-snapshot (socket client snapshot-data host port)
  "Split SNAPSHOT-DATA into chunks and send each."
  (let* ((payload (encode-snapshot-payload snapshot-data))
         (total-size (length payload))
         (chunk-count (ceiling total-size *max-chunk-payload*))
         (seq (snapshot-seq snapshot-data)))

    (loop for chunk-idx from 0 below chunk-count
          for start = (* chunk-idx *max-chunk-payload*)
          for end = (min (+ start *max-chunk-payload*) total-size)
          for chunk-data = (subseq payload start end)
          do (send-net-message socket
                              (list :type :snapshot-chunk
                                    :seq seq
                                    :chunk chunk-idx
                                    :total chunk-count
                                    :data chunk-data)
                              :host host :port port))))
```

#### 3.4 Client-Side Reassembly

**Reassembly buffer structure:**
```lisp
(defstruct chunk-buffer
  (seq nil)                    ; sequence being assembled
  (total 0)                    ; expected chunk count
  (received (make-hash-table)) ; chunk-idx -> data
  (timestamp 0.0))             ; for timeout

(defparameter *chunk-timeout* 1.0)  ; seconds before discarding incomplete
```

**Reassembly logic:**
```lisp
(defun receive-snapshot-chunk (buffer chunk-message)
  "Add chunk to buffer, return complete snapshot or nil."
  (let ((seq (getf chunk-message :seq))
        (idx (getf chunk-message :chunk))
        (total (getf chunk-message :total))
        (data (getf chunk-message :data)))

    ;; New sequence? Reset buffer
    (when (or (null (chunk-buffer-seq buffer))
              (/= seq (chunk-buffer-seq buffer)))
      (setf (chunk-buffer-seq buffer) seq
            (chunk-buffer-total buffer) total
            (chunk-buffer-received buffer) (make-hash-table)
            (chunk-buffer-timestamp buffer) (get-time)))

    ;; Store chunk
    (setf (gethash idx (chunk-buffer-received buffer)) data)

    ;; Check if complete
    (when (= (hash-table-count (chunk-buffer-received buffer)) total)
      ;; Reassemble in order
      (let ((parts nil))
        (loop for i from 0 below total
              do (push (gethash i (chunk-buffer-received buffer)) parts))
        (decode-snapshot-payload (apply #'concatenate 'string (nreverse parts)))))))
```

#### 3.5 Handling Packet Loss

**Strategies:**
1. **Timeout and skip:** If chunks don't arrive within 1 second, discard partial and wait for next snapshot
2. **Request retransmit:** Client sends `:resend-chunk :seq N :chunk M` (adds complexity)
3. **Rely on next snapshot:** Game state is ephemeral; next full snapshot corrects any gaps

**Recommended:** Strategy 1 (timeout) for simplicity. At 20 TPS, missing one snapshot = 50ms stale data (acceptable).

#### 3.6 Implementation Files

| File | Changes |
|------|---------|
| `src/net.lisp` | Add `send-fragmented-snapshot`, chunk handling in receive loop |
| `src/config.lisp` | Add `*chunk-overhead*`, `*max-chunk-payload*`, `*chunk-timeout*` |

---

### Prong 4: TCP for Snapshots (Fallback)

**Goal:** Guaranteed delivery for large snapshots using TCP stream.

#### 4.1 Hybrid Protocol Design

```
┌─────────────────────────────────────────────────────────────────┐
│                         SERVER                                   │
├─────────────────────────────────────────────────────────────────┤
│  UDP Socket (port 1337)           TCP Socket (port 1338)        │
│  - Receive: intents, auth         - Send: snapshots              │
│  - Send: auth responses           - Reliable, ordered            │
│  - Low latency, best effort       - Handles any size             │
└─────────────────────────────────────────────────────────────────┘
                    │                           │
                    │ UDP                       │ TCP
                    │ (intents)                 │ (snapshots)
                    ▼                           ▼
┌─────────────────────────────────────────────────────────────────┐
│                         CLIENT                                   │
└─────────────────────────────────────────────────────────────────┘
```

#### 4.2 Connection Flow

1. Client connects UDP to port 1337 (existing)
2. Client sends `:type :hello` over UDP
3. Server responds with `:type :hello-ack :tcp-port 1338`
4. Client opens TCP connection to port 1338
5. Client authenticates over UDP (existing flow)
6. Server sends snapshots over TCP stream
7. Client sends intents over UDP (unchanged)

#### 4.3 TCP Stream Format

**Length-prefixed messages:**
```
┌──────────────┬─────────────────────────────────┐
│ Length (4B)  │ Payload (snapshot plist)        │
│ big-endian   │ ASCII encoded                   │
└──────────────┴─────────────────────────────────┘
```

**Server sending:**
```lisp
(defun send-tcp-snapshot (tcp-stream snapshot)
  "Send length-prefixed snapshot over TCP."
  (let* ((payload (encode-net-message snapshot))
         (octets (string-to-octets payload))
         (length (length octets)))
    ;; Write 4-byte length header (big-endian)
    (write-byte (ldb (byte 8 24) length) tcp-stream)
    (write-byte (ldb (byte 8 16) length) tcp-stream)
    (write-byte (ldb (byte 8 8) length) tcp-stream)
    (write-byte (ldb (byte 8 0) length) tcp-stream)
    ;; Write payload
    (write-sequence octets tcp-stream)
    (force-output tcp-stream)))
```

**Client receiving:**
```lisp
(defun receive-tcp-snapshot (tcp-stream)
  "Read length-prefixed snapshot from TCP."
  (let ((length (logior (ash (read-byte tcp-stream) 24)
                        (ash (read-byte tcp-stream) 16)
                        (ash (read-byte tcp-stream) 8)
                        (read-byte tcp-stream))))
    (let ((octets (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence octets tcp-stream)
      (decode-net-message (octets-to-string octets)))))
```

#### 4.4 TCP State per Client

```lisp
;; Add to net-client struct:
tcp-socket          ; usocket stream socket for snapshots
tcp-stream          ; bidirectional stream
tcp-connected       ; nil until TCP handshake complete
```

#### 4.5 Latency Considerations

**UDP intent latency:** ~1-5ms (unchanged)
**TCP snapshot latency:** ~10-50ms additional (TCP handshake, Nagle's algorithm)

**Mitigations:**
1. Disable Nagle's algorithm: `(setf (usocket:socket-option sock :tcp-nodelay) t)`
2. TCP is only for server→client; client→server stays UDP
3. Client-side prediction masks snapshot latency

#### 4.6 Fallback Trigger

Use TCP when:
- Compact + delta still exceeds UDP limit
- Client explicitly requests (poor UDP connectivity)
- Server configured for TCP-only mode (simpler deployment)

#### 4.7 Implementation Files

| File | Changes |
|------|---------|
| `src/net.lisp` | Add TCP listener, per-client TCP socket, `send-tcp-snapshot` |
| `src/config.lisp` | Add `*tcp-snapshot-port*`, `*use-tcp-snapshots*` |
| `src/main.lisp` | Client TCP connection, `receive-tcp-snapshot` in event loop |

---

### Implementation Order

| Phase | Prong | Expected Capacity | Effort |
|-------|-------|-------------------|--------|
| 1 | Compact Serialization | ~1000 players | Medium |
| 2 | Delta Compression | ~2000+ players (idle) | Medium-High |
| 3 | UDP Fragmentation | Any size (UDP) | Low |
| 4 | TCP Fallback | Any size (reliable) | Medium |

**Recommended sequence:**
1. **Prong 1** (Compact): Immediate 3-4x improvement, low risk
2. **Prong 2** (Delta): Major improvement for typical gameplay
3. **Prong 3** (Fragment): Safety net for edge cases
4. **Prong 4** (TCP): Only if fragmentation proves unreliable

### Verification Checklist

After each prong:
- [ ] `make checkparens` - syntax valid
- [ ] `make ci` - compiles and basic handshake works
- [ ] `make test-persistence` - serialization round-trips correctly
- [ ] `make smoke` - visual gameplay works
- [ ] `STRESS_RATE=10 make stress` - measure player capacity
- [ ] Log snapshot sizes at 100, 200, 300, 400, 500 players
- [ ] No `WARNING: Dropping snapshot` at target capacity

### Success Metrics

| Metric | Current | After Prong 1 | After Prong 2 | Target |
|--------|---------|---------------|---------------|--------|
| Bytes/player | 232 | <80 | <20 (idle) | <50 avg |
| Max players (smooth) | 300 | 800 | 2000+ | 2000 |
| Max players (degraded) | 500 | 1200 | 3000+ | - |
| Snapshot drop rate | 100% @ 500 | 0% @ 800 | 0% @ 2000 | 0% |
