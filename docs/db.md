# Database Architecture (db.lisp and db-*.lisp modules)

**Module structure:** `db.lisp` is a thin glue file. The database code is split into:
- `db-storage.lisp` — storage abstraction, backend selection, Redis/memory implementations
- `db-players.lisp` — player save/load, dirty flags, logout, session management
- `db-accounts.lisp` — account creation/verification, password hashing
- `db-admin.lisp` — admin tooling, migrate-all, metrics

See `docs/db-storage.md`, `docs/db-players.md`, `docs/db-accounts.md`, `docs/db-admin.md` for per-file details.

## Overview

This document specifies the persistence layer for the MMORPG. The architecture uses Redis as the sole database, leveraging its persistence modes (RDB + AOF) for durability. Postgres cold storage is deferred to a future phase when scale demands it.

**Philosophy:** Simple, durable, minimal maintenance. One database system until we outgrow it.

## Design Principles

1. **Server-authoritative**: All durable state lives on the server. Clients send intents, not state.
2. **Untrusted clients**: Never persist data based on client claims. Validate everything.
3. **Explicit durability**: Every piece of state is explicitly classified as durable or ephemeral.
4. **Versioned serialization**: All records include a version number for migration support.
5. **Crash-safe**: System recovers gracefully from server crashes, client crashes, and network partitions.
6. **Storage-agnostic**: Application code never knows what database it's talking to. All persistence goes through an abstract interface.

## Storage Abstraction Layer

The game server and client MUST NOT know where data comes from or where it goes. All persistence operations go through a generic interface that hides the underlying storage implementation.

### Why Abstract?

- **Swap backends without code changes**: Move from Redis to Postgres, add caching layers, etc.
- **Testing**: Use in-memory storage for unit tests, no external dependencies.
- **Future flexibility**: Add Postgres cold storage alongside Redis without touching game logic.
- **Single responsibility**: Game code handles gameplay, storage code handles persistence.

### Interface Definition

```lisp
;; Abstract storage protocol - game code only sees this

;; Core operations
(defgeneric storage-load (storage key))
(defgeneric storage-save (storage key data))
(defgeneric storage-delete (storage key))
(defgeneric storage-exists-p (storage key))
(defgeneric storage-connect (storage))
(defgeneric storage-disconnect (storage))
(defgeneric storage-keys (storage pattern))

;; Session ownership (Phase 3)
(defgeneric storage-setnx-with-ttl (storage key value ttl-seconds)
  (:documentation "Set KEY only if not exists, with TTL. Returns T if set."))
(defgeneric storage-refresh-ttl (storage key ttl-seconds)
  (:documentation "Refresh TTL on KEY. Returns T if key exists."))
(defgeneric storage-load-raw (storage key)
  (:documentation "Get raw string value (not deserialized). For size checks before parsing."))

;; Sorted sets for leaderboards (Phase 4)
(defgeneric storage-zadd (storage key score member))
(defgeneric storage-zincrby (storage key increment member))
(defgeneric storage-zrevrange (storage key start stop &key withscores))
(defgeneric storage-zrank (storage key member))
(defgeneric storage-zrevrank (storage key member))
(defgeneric storage-zscore (storage key member))

;; Sets for online tracking (Phase 4)
(defgeneric storage-sadd (storage key member))
(defgeneric storage-srem (storage key member))
(defgeneric storage-scard (storage key))
(defgeneric storage-smembers (storage key))

;; Hashes for quick lookups (Phase 4)
(defgeneric storage-hget (storage key field))
(defgeneric storage-hset (storage key field value))

;; Lua script execution (Phase 5)
(defgeneric storage-script-load (storage script-name script-body))
(defgeneric storage-eval-script (storage script-name keys args))

;; Validation support (Phase 6)
(defgeneric storage-incr (storage key)
  (:documentation "Increment counter at KEY. Returns new value."))
(defgeneric storage-save-with-ttl (storage key data ttl-seconds)
  (:documentation "Save DATA at KEY with TTL expiration."))
```

### Concrete Implementations

```lisp
;; Redis implementation
;; Note: All methods internally wrap redis calls in (redis:with-connection ...)
;; Connection params (host, port) stored in class slots, omitted here for brevity.
(defclass redis-storage ()
  ((host :initarg :host :accessor redis-storage-host)
   (port :initarg :port :accessor redis-storage-port)))

(defmethod storage-load ((storage redis-storage) key)
  (redis:with-connection (:host (redis-storage-host storage)
                          :port (redis-storage-port storage))
    (let ((raw (red:get key)))
      (when raw
        (let ((*read-eval* nil))
          (read-from-string raw))))))

(defmethod storage-save ((storage redis-storage) key data)
  ;; Atomic write-then-rename pattern for crash safety
  (redis:with-connection (:host (redis-storage-host storage)
                          :port (redis-storage-port storage))
    (let ((temp-key (format nil "temp:~a:~a" key (get-internal-real-time))))
      (red:set temp-key (prin1-to-string data))
      (red:rename temp-key key)))  ; Atomic, overwrites if exists
  t)

;; In-memory implementation (for testing)
(defclass memory-storage ()
  ((data :initform (make-hash-table :test 'equal))))

(defmethod storage-load ((storage memory-storage) key)
  (gethash key (slot-value storage 'data)))

(defmethod storage-save ((storage memory-storage) key data)
  (setf (gethash key (slot-value storage 'data)) data)
  t)

;; Future: Postgres, file-based, tiered Redis+Postgres, etc.
```

### Usage in Game Code

```lisp
;; Game code is storage-agnostic
(defun save-player (player)
  (let ((key (format nil "player:~a" (player-id player)))
        (data (serialize-player player)))
    (storage-save *storage* key data)))

(defun load-player (player-id)
  (let* ((key (format nil "player:~a" player-id))
         (data (storage-load *storage* key)))
    (when data
      (deserialize-player (migrate-player data)))))

;; At startup, inject the storage backend (Redis is default)
(defparameter *storage* (make-instance 'redis-storage))
;; For CI/tests only (MMORPG_DB_BACKEND=memory):
(defparameter *storage* (make-instance 'memory-storage))
```

### High-Level Convenience Functions

For common patterns, provide typed wrappers:

```lisp
(defun db-load-player (player-id)
  "Load player by ID, running migrations if needed."
  (let ((data (storage-load *storage* (player-key player-id))))
    (when data
      (deserialize-player (migrate-player data)))))

(defun db-save-player (player)
  "Save player to storage."
  (storage-save *storage*
                (player-key (player-id player))
                (serialize-player player)))

(defun db-load-zone-objects (zone-id)
  "Load zone object state."
  (storage-load *storage* (zone-objects-key zone-id)))
```

Game code calls `db-load-player`, never `redis:get` directly.

## Data Classification

### The Heuristic

> If the server maintains authoritative state that **accumulates** or represents **progress**, it must be durable.

Accumulation = player investment = must save.

### Durable State (MUST persist)

These are non-negotiable. Loss of this data after a crash is unacceptable.

| Category | Examples | Write Trigger |
|----------|----------|---------------|
| **Progression** | XP, levels, skill levels, combat stats | On gain (batched) |
| **Health** | Current HP (see note below) | On change (batched) |
| **Inventory** | Items, equipment, stack counts | On change |
| **Currency** | Gold, bank contents | On change |
| **Position** | Zone ID, X/Y coordinates | Checkpoint (periodic) |
| **Quests** | Progress, completion flags | On update |
| **Achievements** | Unlock flags, progress counters | On unlock |
| **Social** | Friends list, block list, guild membership | On change |
| **Settings** | Keybinds, UI preferences (if server-stored) | On change |

**Note on HP:** Current HP MUST be durable to prevent the "logout heal" exploit. If a player is at 10 HP and about to die, they cannot be allowed to disconnect and reconnect at full health. HP is batched with tier-2 writes, but death (HP reaching 0) triggers an immediate tier-1 write.

### Ephemeral State (OK to lose)

These reset on logout/crash. No persistence required.

| Category | Examples | Why Ephemeral |
|----------|----------|---------------|
| **Temporary buffs** | Active buffs, debuffs, cooldowns | Short-lived, can expire on logout |
| **Targeting** | Current attack target, follow target | Session-only |
| **AI state** | NPC aggro, pathfinding | Rebuilds on load |
| **Visual state** | Animation frame, blood effects | Client-rendered |
| **Matchmaking** | Party invites, trade windows | Session-only |

### Classification Enforcement

Every field in player/entity structs MUST be explicitly categorized:

```lisp
;; Example annotation pattern (conceptual)
(defparameter *durable-player-fields*
  '(id x y zone-id hp xp attack-level strength-level defense-level
    hitpoints-level inventory bank quest-progress achievements
    friends-list block-list settings))

(defparameter *ephemeral-player-fields*
  '(current-target attack-cooldown buffs debuffs
    animation-state path-cache))
```

**Rule:** When adding new features, you MUST update one of these lists. Code review must verify this.

## Redis Architecture

### Why Redis Only (For Now)

- Sub-millisecond read/write latency
- Native support for serialized blobs (our Lisp s-expressions work directly)
- Built-in persistence (RDB + AOF)
- Simpler operations than Redis + Postgres
- Can handle many thousands of concurrent users (actual capacity depends on blob size, write rate, hardware)
- Natural key-value model matches our access patterns (player-id -> state)

### Client Library: cl-redis

**Repository:** https://github.com/vseloved/cl-redis

**Why cl-redis:**
- Standard CL Redis client with good adoption (193 GitHub stars)
- Full Redis command support via `RED` package (e.g., `(red:get key)`, `(red:set key value)`)
- **Pipelining support** - critical for batch writes (used by `flush-dirty-players` for ~300x speedup)
- Connection management via `with-connection` macro
- Automatic reconnection with restart conditions

**Dependencies:** usocket, flexi-streams, rutils

**Example usage:**
```lisp
(ql:quickload :cl-redis)

;; Basic operations
(redis:with-connection ()
  (red:set "player:123" "(serialized-data...)")
  (red:get "player:123"))

;; Pipelining for batch writes (important for tier-2 flushes)
(redis:with-connection ()
  (redis:with-pipelining
    (red:set "player:1" "...")
    (red:set "player:2" "...")
    (red:set "player:3" "...")))

;; Atomic increment for ID generation
(redis:with-connection ()
  (red:incr "server:id-counter"))
```

**Notes:**
- Last updated 2021, but Redis protocol is stable
- No built-in connection pooling (manage connections at application level)
- No Unix socket support (use TCP)

### Persistence Configuration

Redis supports two persistence mechanisms. We use **both** for maximum durability:

#### RDB (Redis Database Snapshots)

Point-in-time snapshots of the entire dataset.

```
# redis.conf
save 900 1      # Snapshot if 1+ key changed in 900 seconds
save 300 10     # Snapshot if 10+ keys changed in 300 seconds
save 60 10000   # Snapshot if 10000+ keys changed in 60 seconds

# For game server, recommend more aggressive:
save 300 1      # Snapshot every 5 min if anything changed
save 60 100     # Snapshot every 1 min if 100+ changes
```

**Pros:** Compact, fast recovery, good for backups
**Cons:** Can lose data since last snapshot (up to 5 min)

#### AOF (Append Only File)

Logs every write operation for replay on recovery.

```
# redis.conf
appendonly yes
appendfsync everysec    # Fsync every second (recommended)
# appendfsync always    # Fsync every write (slower, max durability)
```

**Pros:** Maximum durability (lose at most 1 second of data)
**Cons:** Larger files, slower recovery than RDB

#### Combined Strategy (Recommended)

```
# redis.conf for MMORPG
appendonly yes
appendfsync everysec
save 300 1
save 60 1000

# AOF rewrite to prevent unbounded growth
auto-aof-rewrite-percentage 100
auto-aof-rewrite-min-size 64mb
```

**Recovery behavior:** Redis uses AOF if available (more complete), falls back to RDB.

**Worst-case data loss:** 1 second of writes (with `appendfsync everysec`).

### Key Schema

All keys follow a consistent naming pattern:

```
player:{player-id}          -> Player durable state blob (contains :version inside)

zone:{zone-id}:objects      -> Zone object state (respawns, etc.)

server:config               -> Server configuration
server:id-counter           -> Global ID allocation counter
```

**Note:** Schema version and timestamps live inside the blob (`:version`, `:created-at`), not as separate keys. This keeps player data atomic.

### Data Format

All values are Lisp s-expressions (our existing serialization format) with a version header:

```lisp
(:version 1
 :id 12345                ; Player ID (canonical field name is :id, not :player-id)
 :zone-id :overworld
 :x 150.0
 :y 200.0
 :hp 85                ; Current HP (durable to prevent logout-heal exploit)
 :xp 1500
 :attack-level 10
 :strength-level 8
 :defense-level 12
 :hitpoints-level 15   ; Max HP is derived from this level
 :inventory ((:item-id :sword :count 1 :slot 0)
             (:item-id :coins :count 500 :slot 1))
 :bank ((:item-id :coins :count 10000))
 :quest-progress ((:quest-id :tutorial :stage 3 :complete t))
 :achievements (:first-blood :level-10)
 :friends (12346 12347)
 :settings (:music-volume 0.5 :sfx-volume 0.8))
```

## Write Patterns

### Tiered Durability

Not all durable writes are equal. We tier by criticality:

#### Tier 1: Immediate (Before ACK)

**These writes MUST complete before acknowledging to client.**

- Trade completion
- Bank deposit/withdrawal
- Item destruction (drop, sell, consume)
- Purchase transactions
- Level up (to prevent XP rollback past level boundary)
- Player death (HP reaches 0 - prevents logout-to-survive exploit)

Pattern:
```
1. Client sends intent
2. Server validates
3. Server writes to Redis (WAIT for confirmation)
4. Server updates in-memory state
5. Server ACKs to client
```

#### Tier 2: Batched (Periodic Checkpoint)

**These writes happen periodically, batched per-player.**

- XP gains (batch every 30 seconds)
- HP changes from combat (batch every 30 seconds, unless death - see tier-1)
- Position updates (batch every 60 seconds)
- Minor inventory changes (pickup common items)
- Quest progress increments

Pattern:
```
1. Server updates in-memory state immediately
2. Mark player as "dirty"
3. Every 30s, flush all dirty players to Redis
4. Clear dirty flags
```

#### Tier 3: On Logout / Shutdown

**These writes happen when session ends.**

- Final state snapshot
- Clear any ephemeral state
- Update "last seen" timestamp

### Dirty Flag Implementation

```lisp
;; Conceptual implementation
(defstruct player-session
  player          ; The player struct
  dirty-p         ; Needs flush to Redis
  last-flush      ; Timestamp of last Redis write
  tier1-pending)  ; Queue of tier-1 writes awaiting confirmation
```

### Write Coalescing

Multiple changes to the same player within a batch window coalesce into a single write:

```
T+0.0s: Player gains 10 XP (dirty=t)
T+0.5s: Player gains 15 XP (already dirty, no-op)
T+1.0s: Player moves to (100, 200) (already dirty, no-op)
T+30.0s: Batch flush - single write with all accumulated changes
```

## Read Patterns

### On Player Login

```
1. Read player:{player-id} from Redis
2. Check :version field
3. If version < current, run migration chain
4. Deserialize into in-memory player struct
5. Initialize ephemeral fields to defaults
6. Player is now "live" in server memory
```

### During Gameplay

**No Redis reads during normal gameplay.** Server memory is authoritative for live players.

Redis is only read:
- On login (load player)
- On server restart (reload all live sessions from crash)
- For admin tools (player lookup)

### Cache Warming (Optional)

On server start, optionally pre-load frequently accessed data:

```
1. Server starts
2. Load zone:{zone-id}:objects for all zones
3. Load server:config
4. Wait for player logins to load individual players (lazy)
```

## Migration System

### Version Numbers

Every record has a `:version` field. Current schema version is tracked in code:

```lisp
(defparameter *player-schema-version* 4)  ; v4: added deaths field for leaderboards
```

### Migration Chain

Migrations are **append-only** and **chained**:

```lisp
(defparameter *player-migrations*
  '((2 . migrate-player-v1->v2)
    (3 . migrate-player-v2->v3)
    (4 . migrate-player-v3->v4)))

(defun migrate-player (data)
  "Migrate player data to current schema version."
  (let ((version (getf data :version 0)))
    (loop while (< version *player-schema-version*)
          do (let ((migrator (cdr (assoc (1+ version) *player-migrations*))))
               (when migrator
                 (setf data (funcall migrator data)))
               (incf version)))
    (setf (getf data :version) *player-schema-version*)
    data))
```

### Migration Rules

1. **Never delete old migration code.** A player might skip versions (inactive for months).
2. **Migrations must be idempotent.** Running twice produces same result.
3. **Migrations must handle missing fields.** Use defaults for new fields.
4. **Test migrations on production data copies** before deploying.

### Example Migrations

```lisp
;; v1->v2: Add lifetime-xp field
(defun migrate-player-v1->v2 (data)
  "v1->v2: Add lifetime-xp field, defaulting to 0."
  (unless (getf data :lifetime-xp)
    (setf data (plist-put data :lifetime-xp 0)))
  data)

;; v2->v3: Add playtime and created-at fields
(defun migrate-player-v2->v3 (data)
  "v2->v3: Add playtime (seconds played) and created-at (unix timestamp)."
  (unless (getf data :playtime)
    (setf data (plist-put data :playtime 0)))
  (unless (getf data :created-at)
    (setf data (plist-put data :created-at (get-universal-time))))
  data)

;; v3->v4: Add deaths field for leaderboard tracking
(defun migrate-player-v3->v4 (data)
  "v3->v4: Add deaths field for leaderboard tracking."
  (unless (getf data :deaths)
    (setf data (plist-put data :deaths 0)))
  data)
```

These migrations are tested in `persistence-test.lisp` and `unit-test.lisp`.

## Crash-Safe Writes (Atomic Saves)

To prevent data corruption during crashes, all Redis saves use the **write-new-then-rename** pattern:

```lisp
;; Instead of directly overwriting:
(red:set "player:123" new-data)  ; DANGEROUS: crash mid-write corrupts key

;; We write to a temp key first, then atomically rename:
(red:set "temp:player:123:12345" new-data)  ; Crash here = temp key orphaned
(red:rename "temp:player:123:12345" "player:123")  ; Atomic, overwrites safely
```

**Why this works:**
1. If crash during step 1 (writing temp key): Original data in `player:123` is untouched
2. If crash during step 2 (rename): Redis RENAME is atomic - either happens fully or not at all
3. Orphaned temp keys (`temp:*`) are harmless and can be cleaned up periodically

**Impact:** A server crash during save will never corrupt existing player data. At worst, the latest changes (since last successful save) are lost, but you never end up with half-written garbage.

## Crash Recovery

### Server Crash Scenarios

#### Scenario 1: Server crashes, Redis survives

**Recovery:**
1. Server restarts
2. Redis has all data (AOF replay if needed)
3. Players reconnect, load state from Redis
4. **Data loss:** Up to 1 batch window (30s) for tier-2 data, 1s for tier-1

#### Scenario 2: Server + Redis crash (same machine)

**Recovery:**
1. Both restart
2. Redis replays AOF (or loads RDB if AOF corrupt)
3. Server loads from recovered Redis
4. **Data loss:** Same as scenario 1

#### Scenario 3: Redis data corruption

**Recovery:**
1. Stop server
2. Restore Redis from backup (RDB snapshot)
3. Restart Redis, then server
4. **Data loss:** Time since last backup

### Backup Strategy

```bash
# Cron job: Copy RDB snapshot to backup location
0 * * * * cp /var/lib/redis/dump.rdb /backups/redis/dump-$(date +\%Y\%m\%d-\%H).rdb

# Keep 7 days of hourly backups
find /backups/redis -mtime +7 -delete
```

**Recommended:** Also replicate to off-site storage (S3, etc.) daily.

### Graceful Shutdown

On planned shutdown (deploy, maintenance):

```
1. Stop accepting new connections
2. Broadcast "server shutting down" to clients
3. Flush ALL dirty players to Redis (force batch)
4. Wait for Redis to confirm writes
5. Issue Redis BGSAVE for clean snapshot
6. Shutdown server
```

## Client Crash / Disconnect Handling

### Dirty State on Disconnect

When client disconnects unexpectedly:

```
1. Detect disconnect (UDP timeout or TCP close)
2. Keep player state in memory for grace period (30s)
3. If reconnect within grace period, resume session
4. If no reconnect, flush to Redis and evict from memory
```

### Preventing "Crash Exploits"

Players might crash intentionally to avoid consequences. Mitigation:

1. **Death is immediate:** Player death is tier-1 write (immediate persist)
2. **Combat state sticks:** If in combat on disconnect, remain "in combat" on reconnect
3. **Trade rollback:** Incomplete trades cancelled, items returned
4. **Position rollback:** Disconnect in dangerous area? Respawn at safe point

## Password Hashing

Passwords are never stored in plaintext. The system uses PBKDF2-SHA256 with random salts.

### Implementation

```lisp
;; Hash a password for storage
(hash-password "user-password")
;; Returns: "0459ed2a0f2cfd78686d9319d74dc450$3ef4641c098c206a0b559a5fc629d83b..."
;;          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;          16-byte random salt (hex)         PBKDF2-SHA256 hash (hex)

;; Verify a password against stored hash
(verify-password "user-password" stored-hash)
;; Returns: T or NIL
```

### Parameters

| Parameter | Value | Notes |
|-----------|-------|-------|
| `*password-hash-iterations*` | 100,000 | OWASP recommended minimum |
| `*password-salt-bytes*` | 16 | 128-bit random salt per password |
| Hash algorithm | PBKDF2-SHA256 | Via ironclad library |
| Output key length | 32 bytes | 256-bit derived key |

### Storage Format

Passwords are stored as `salt$hash` strings:
```
(:version 2
 :username "player1"
 :password-hash "0459ed2a0f2cfd78686d9319d74dc450$3ef4641c098c206a0b559a5fc629d83b1cbeeb1da11a1a10cb675fc09b4c5686"
 :character-id 12345)
```

### Security Properties

- **Constant-time comparison**: `verify-password` uses `ironclad:constant-time-equal` to prevent timing attacks
- **Unique salts**: Each password gets a cryptographically random salt
- **No plaintext fallback**: Legacy plaintext accounts cannot authenticate (must re-register)

## Network Encryption (Auth Only)

Authentication messages (login/register) can be encrypted to prevent credential sniffing on untrusted networks. Game traffic remains unencrypted for latency.

### Protocol: X25519 + AES-256-GCM

```
1. Server generates static X25519 keypair at startup
2. Client generates ephemeral X25519 keypair per auth request
3. Client computes shared secret via ECDH
4. Client encrypts credentials with AES-256-GCM using SHA256(shared-secret)
5. Client sends: ephemeral-public-key || nonce || ciphertext || tag
6. Server computes same shared secret, decrypts and verifies tag
```

### Configuration

```lisp
;; Enable encryption (server and client must agree)
(setf *auth-encryption-enabled* t)

;; Server prints public key on startup when enabled:
;; "SERVER: Auth encryption enabled. Public key: a1b2c3d4..."

;; Client must have server's public key configured:
(setf *server-auth-public-key* "a1b2c3d4...")
```

### Message Format

**Plaintext auth (default):**
```lisp
(:type :login :username "player1" :password "secret")
```

**Encrypted auth (when enabled):**
```lisp
(:type :login :encrypted-payload "hex-encoded-ciphertext...")
```

The server automatically detects and handles both formats.

### Functions

| Function | Purpose |
|----------|---------|
| `init-server-encryption` | Generate server keypair at startup |
| `get-server-public-key` | Return server's public key (hex) |
| `encrypt-auth-payload` | Client-side credential encryption |
| `decrypt-auth-payload` | Server-side decryption with tag verification |

## Account Management

Player accounts are stored separately from character data, enabling future multi-character support.

### Account Record Format

```lisp
(:version 2
 :username "player1"
 :password-hash "salt$hash"
 :character-id 12345)  ; Links to player record
```

### Functions

| Function | Purpose |
|----------|---------|
| `db-create-account` | Create new account (hashes password) |
| `db-verify-credentials` | Verify login credentials |
| `db-account-exists-p` | Check if username is taken |
| `db-get-character-id` | Get linked character for account |
| `db-set-character-id` | Link character to account |

### Schema Versioning

Account schema version is tracked separately from player schema:
- v1: Plaintext passwords (legacy, no longer supported)
- v2: PBKDF2-SHA256 hashed passwords

## Security Considerations

### Untrusted Client Principle

Never persist based on client claims:

```
BAD:  Client says "I have 1000 gold" -> write to Redis
GOOD: Client says "I want to sell sword" -> server calculates gold -> write
```

### Serialization Safety

Always use `*read-eval* nil` when reading data:

```lisp
(let ((*read-eval* nil))
  (read-from-string redis-value))
```

This prevents code execution from malformed/malicious data.

### Rate Limiting

Limit tier-1 writes per player to prevent abuse:

```
Max tier-1 writes: 10 per second per player
Exceeding limit: Queue excess, warn, potential kick for abuse
```

## Operations & Monitoring

### Key Metrics to Monitor

| Metric | Warning Threshold | Critical Threshold |
|--------|------------------|-------------------|
| Redis memory usage | 70% of max | 90% of max |
| AOF rewrite frequency | >1 per hour | >1 per 10 min |
| Write latency (p99) | >10ms | >100ms |
| Dirty player count | >1000 | >5000 |
| Batch flush duration | >1s | >5s |

### Redis Configuration Checklist

```
# Memory
maxmemory 4gb
maxmemory-policy noeviction  # NEVER evict game data!

# Persistence
appendonly yes
appendfsync everysec
save 300 1
save 60 1000

# Safety
stop-writes-on-bgsave-error yes
rdbcompression yes
```

### Health Checks

```bash
# Basic connectivity
redis-cli ping

# Persistence status
redis-cli info persistence

# Memory status
redis-cli info memory
```

## Future: Postgres Cold Storage (Tentative)

Redis may be sufficient for the long term. This section documents a potential migration path **if and when** scale demands it. Until then, Redis-only is simpler and adequate.

**When to consider Redis + Postgres:**

1. **Scale:** >50k registered players causing Redis memory pressure
2. **Queries:** Need complex queries (leaderboards, analytics, support tools)
3. **Compliance:** Need audit logs, GDPR deletion, data export
4. **Multi-region:** Need geographic data distribution

**Note:** With proper Redis configuration (maxmemory, eviction policies, RDB snapshots), Redis alone can handle significant scale. Only add Postgres when there's a concrete need.

### Migration Path (If Needed)

If Redis alone proves insufficient, the architecture would become:

```
[Live Players] <-> [Redis Hot Cache] <-> [Postgres Cold Storage]
                         |
                    Write-behind
                    (async, batched)
```

**Redis remains the gameplay database.** Postgres is for:
- Long-term storage of inactive players
- Analytics and reporting
- Backup/archive
- Admin tools

### Postgres Schema (Future)

```sql
-- Simple blob storage, not normalized
CREATE TABLE players (
    player_id BIGINT PRIMARY KEY,
    version INTEGER NOT NULL,
    data JSONB NOT NULL,  -- Or TEXT for s-expressions
    updated_at TIMESTAMP DEFAULT NOW(),
    created_at TIMESTAMP DEFAULT NOW()
);

CREATE INDEX idx_players_updated ON players(updated_at);
```

**Note:** Keep data as blob (JSONB or TEXT), not normalized tables. This preserves the simplicity of our serialization model.

## Implementation Checklist

### Phase 1: Redis Integration (Complete)

- [x] Add cl-redis dependency to ASDF system
- [x] Implement storage abstraction layer (storage protocol + redis-storage class)
- [x] Implement key schema and serialization
- [x] Add version field to all serialized data
- [x] Implement dirty flag and batch flush
- [x] Implement tier-1 immediate writes for critical operations
- [x] Add login/logout Redis read/write
- [x] Configure Redis persistence (RDB + AOF)
- [ ] Set up backup cron job (ops task)
- [x] Add basic monitoring (Redis metrics)

### Phase 2: Migration System (Complete)

- [x] Implement migration chain runner (`migrate-player-data`)
- [x] Add migration test framework (`persistence-test.lisp`, `unit-test.lisp`)
- [x] Document migration procedure (`migrations.md`)

### Phase 3: Hardening (Complete)

- [x] Schema validation (`validate-player-plist`)
- [x] Redis latency metrics (ring buffers, p99 tracking)
- [x] Session ownership with TTL (`storage-setnx-with-ttl`)
- [x] Leaderboards (`storage-zadd`, `storage-zrevrange`)
- [x] Online player tracking (`storage-sadd`, `storage-srem`)
- [x] Lua script execution infrastructure
- [x] Trade system with atomic swaps

### Future Work

- [ ] Graceful shutdown flush
- [ ] Write rate limiting
- [ ] Crash recovery testing
- [ ] Load test batch flush under peak

## Appendix: Redis Commands Reference

```bash
# Player operations
SET player:12345 "(serialized-data...)"
GET player:12345
DEL player:12345
EXISTS player:12345

# Atomic operations (for ID generation)
INCR server:id-counter

# Batch operations
MSET player:1 "..." player:2 "..." player:3 "..."
MGET player:1 player:2 player:3

# Persistence
BGSAVE              # Trigger RDB snapshot
BGREWRITEAOF        # Trigger AOF rewrite
LASTSAVE            # Timestamp of last successful save

# Monitoring
INFO persistence
INFO memory
DBSIZE              # Number of keys
```

---

## Addendum: Production Hardening

The sections below address architectural concerns for production MMO features (trades, banks, guilds, leaderboards). These build on the foundation above.

### Atomic Multi-Entity Operations

**Problem:** The write-then-rename pattern is atomic per-key but NOT atomic across multiple keys. Trades, bank transfers, and guild operations touch multiple players simultaneously.

**Risk:** Server crash between individual writes causes:
- Item duplication (player A loses item, crash, player B never receives)
- Item deletion (player A's item removed, crash before player B gets gold)
- Inconsistent state requiring manual GM intervention

**Solution:** Redis Lua scripts for atomic multi-entity operations via `storage-eval-script` (defined in interface above).

**Lua Script Pattern (Commit-Only):**

All validation happens server-side in Lisp. Lua scripts only provide atomic commit of pre-validated results. This keeps complex game logic in Lisp where it's testable, and Lua simple/auditable.

```lua
-- trade_complete.lua: Atomic commit of pre-validated trade
-- KEYS[1] = player:A, KEYS[2] = player:B
-- ARGV[1] = new player A data (already validated, items swapped)
-- ARGV[2] = new player B data (already validated, items swapped)

-- Minimal safety check (players still exist)
if redis.call('EXISTS', KEYS[1]) == 0 then
  return {err = "PLAYER_NOT_FOUND"}
end
if redis.call('EXISTS', KEYS[2]) == 0 then
  return {err = "PLAYER_NOT_FOUND"}
end

-- Atomic commit of both players (all-or-nothing)
redis.call('SET', KEYS[1], ARGV[1])
redis.call('SET', KEYS[2], ARGV[2])

return {ok = "TRADE_COMPLETE"}
```

**Why commit-only?**
- Game logic stays in Lisp (testable, debuggable)
- Lua scripts are simple and auditable
- No Lisp s-expression parsing in Lua
- Server computes exact post-trade state before commit

**Tier-1 Operations Requiring Atomicity:**

| Operation | Keys Affected | Script |
|-----------|---------------|--------|
| Trade complete | 2 players | `trade_complete.lua` |
| Bank deposit | player + bank | `bank_deposit.lua` |
| Bank withdraw | player + bank | `bank_withdraw.lua` |
| Guild invite accept | player + guild | `guild_join.lua` |
| Auction purchase | buyer + seller + listing | `auction_buy.lua` |
| Mail with attachment | sender + recipient + mail | `mail_send.lua` |

**Implementation:** Scripts stored in `data/redis-scripts/`, loaded at server startup, executed via `EVALSHA`.

### Session Ownership & Lease

**Problem:** Two writers racing for same player key can cause lost updates:
- Old session flush overwrites new session changes
- Reconnect before old session cleanup completes
- Multi-process server (future) with stale ownership

**Solution:** Redis-based session lease with TTL.

**Why Redis-based (not in-memory)?**
- Survives server restart (ownership transfers cleanly)
- Enables multi-server deployment (one server per zone cluster/shard, many zones each)
- Prevents "ghost sessions" from crashed servers blocking logins
- TTL auto-cleanup means no manual intervention after crashes

**Key Schema:**
```
session:{player-id}:owner  -> server-id:session-id (TTL: 60s)
```

**Ownership Protocol:**
```
1. On login:
   - SETNX session:{id}:owner with 60s TTL
   - If fails, reject login (already owned)
   - Start heartbeat (refresh TTL every 30s)

2. On save:
   - Check session:{id}:owner matches our session-id
   - If mismatch, abort save (we lost ownership)
   - If match, proceed with save

3. On logout:
   - DEL session:{id}:owner
   - Final save

4. On crash (TTL expires):
   - Redis auto-deletes ownership key after 60s
   - Next login can claim ownership
```

**Lua Script for Safe Save:**
```lua
-- safe_save.lua: Only save if we own the session
-- KEYS[1] = session:{id}:owner, KEYS[2] = player:{id}
-- ARGV[1] = expected-owner, ARGV[2] = player-data, ARGV[3] = timestamp

local owner = redis.call('GET', KEYS[1])
if owner ~= ARGV[1] then
  return {err = "NOT_OWNER", actual = owner}
end

-- We own it, proceed with atomic save
local temp_key = KEYS[2] .. ':temp:' .. ARGV[3]
redis.call('SET', temp_key, ARGV[2])
redis.call('RENAME', temp_key, KEYS[2])
return {ok = "SAVED"}
```

### Lua Script Execution Infrastructure

**Implementation:** Redis has a built-in Lua 5.1 interpreter. No external Lua installation is required.

**Storage Abstraction:**
```lisp
;; Generic interface for script execution
(defgeneric storage-eval-script (storage script-name keys args)
  (:documentation "Execute a Lua script by name with KEYS and ARGS."))

(defgeneric storage-script-load (storage script-name script-body)
  (:documentation "Load a Lua script into Redis, cache SHA for EVALSHA."))

;; Script SHA cache (avoids re-sending script body on each call)
(defparameter *redis-script-shas* (make-hash-table :test 'equal))
```

**Backend Implementations:**

| Backend | Implementation |
|---------|----------------|
| `redis-storage` | Uses `EVALSHA` with cached SHA, falls back to `EVAL` if evicted |
| `memory-storage` | Dispatches to `*memory-script-handlers*` (Lisp emulation) |
| `postgres-storage` (future) | Would use transactions or stored procedures |

**Script Loading at Startup:**
```lisp
;; Called from run-server after init-storage
(defun load-trade-scripts ()
  "Load trade-related Lua scripts into Redis."
  (when (typep *storage* 'redis-storage)
    (storage-script-load *storage* "trade_complete"
                         (uiop:read-file-string "data/redis-scripts/trade_complete.lua"))))
```

**Available Scripts:**

| Script | Purpose | Location |
|--------|---------|----------|
| `session_claim.lua` | Atomic SETNX with TTL for session ownership | `data/redis-scripts/` |
| `safe_save.lua` | Ownership-checked player save | `data/redis-scripts/` |
| `trade_complete.lua` | Atomic two-player item swap | `data/redis-scripts/` |

**Memory Storage Emulation:**

For testing without Redis, memory storage emulates scripts via handler functions:

```lisp
(defparameter *memory-script-handlers* (make-hash-table :test 'equal))

;; Register handler for trade_complete
(setf (gethash "trade_complete" *memory-script-handlers*)
      (lambda (storage keys args)
        ;; Emulate atomic swap in memory (inherently atomic in single-threaded Lisp)
        (setf (gethash (first keys) (memory-storage-data storage)) (first args))
        (setf (gethash (second keys) (memory-storage-data storage)) (second args))
        "OK"))
```

This design keeps the storage abstraction intact - game code calls `storage-eval-script` regardless of backend.

### Persistence Monitoring & Metrics

**Problem:** No visibility into Redis latency, AOF rewrite impact, or batch flush performance.

**Solution:** Application-level metrics collection.

#### Connection Pooling Analysis

**Current State:** cl-redis uses per-operation connections via `redis:with-connection`. No built-in connection pooling (there's an unmerged PR from years ago, but the library is maintained - commits as recent as 2024).

**Is this a problem?** No, for current scale targets (~500 players). Here's why:

| Event | Redis Ops | Frequency |
|-------|-----------|-----------|
| Login | 1 load | Once per player |
| Logout | 1 save | Once per player |
| Batch flush | 1 pipelined connection for ALL dirty players | Every 30s |
| Tier-1 save (death, level-up, trade) | 1 save | Rare (few/second) |
| During gameplay | **Zero** | In-memory is authoritative |

**Key insight:** Redis is NOT hit every tick. The architecture minimizes Redis operations by:
1. Keeping authoritative state in memory during gameplay
2. Batching dirty player saves every 30s with pipelining (1 connection for N players)
3. Only reading on login, writing on logout/checkpoint

**Rough math for 500 players:**
- 500 logins = 500 connections (one-time, spread over time)
- Every 30s = 1 connection (pipelined batch, even if all 500 dirty)
- Tier-1 saves = ~10/second at peak

This is trivial load. Connection pooling helps at thousands of ops/second.

**When to revisit:**
- 2,000+ concurrent players
- Profiling shows connection overhead as bottleneck
- Redis on remote host (network latency compounds)

**If needed later:**
1. Wrap `with-connection` in a simple application-level pool (~20 lines)
2. Switch to a pooling-capable library
3. Horizontal scaling (more server processes) may solve it first

**Decision:** Keep current approach. Add pooling only if metrics show it's needed.

**Metrics to Track:**

| Metric | Collection Point | Alert Threshold |
|--------|------------------|-----------------|
| `redis.save.latency_ms` | Every `storage-save` | p99 > 10ms |
| `redis.load.latency_ms` | Every `storage-load` | p99 > 5ms |
| `redis.batch.latency_ms` | Every `flush-dirty-players` | p99 > 100ms |
| `redis.batch.size` | Every batch flush | > 100 players |
| `redis.connection.errors` | Every failed operation | > 10/min |
| `redis.script.latency_ms` | Every Lua script exec | p99 > 20ms |

**Implementation:**
```lisp
(defstruct redis-metrics
  (save-latencies (make-ring-buffer 1000))    ; Last 1000 save times
  (load-latencies (make-ring-buffer 1000))
  (batch-latencies (make-ring-buffer 100))
  (connection-errors 0)
  (last-reset-time (get-universal-time)))

(defun record-latency (metrics type start-time)
  (let ((elapsed-ms (* 1000.0 (- (get-internal-real-time) start-time)
                       (/ 1.0 internal-time-units-per-second))))
    (ring-buffer-push (slot-value metrics type) elapsed-ms)))

(defun get-p99-latency (metrics type)
  (percentile (ring-buffer-values (slot-value metrics type)) 99))
```

**Logging:**
- Log warning if p99 exceeds threshold
- Log Redis INFO stats every 5 minutes (memory, persistence status)
- Log AOF rewrite start/complete events

### Structured Redis Data (Non-Blob)

**Problem:** Pure blob storage makes queries expensive. Getting top 100 players by XP requires loading ALL player blobs.

**Solution:** Maintain auxiliary Redis structures alongside blobs for query-able data.

**New Key Schema:**
```
# Sorted Sets (for rankings)
leaderboard:xp           -> ZADD player-id score  (score = lifetime-xp)
leaderboard:level        -> ZADD player-id score  (score = combat-level)
leaderboard:deaths       -> ZADD player-id score  (score = total deaths)

# Future leaderboards (add when features implemented):
# leaderboard:kills            -> ZADD player-id score  (pvp-kills, requires PvP system)
# leaderboard:skill:smithing   -> ZADD player-id score
# leaderboard:skill:fishing    -> ZADD player-id score
# leaderboard:skill:mining     -> ZADD player-id score
# etc.

# Sets (for membership queries)
online:players           -> SADD player-id        (removed on logout/disconnect)
guild:{id}:members       -> SADD player-id
zone:{id}:players        -> SADD player-id        (for zone population)

# Note: Redis sets don't support per-member TTL. Online tracking uses:
# - SADD on login, SREM on logout/disconnect
# - Server boot runs cleanup pass to remove stale entries from crashed sessions:
#     for id in SMEMBERS online:players:
#         if EXISTS session:{id}:owner == 0:
#             SREM online:players id
# - Alternatively: use session ownership TTL expiry as source of truth for online count

# Hashes (for quick lookups without loading full blob)
player:{id}:summary      -> HSET name level zone-id guild-id kills deaths
username:lookup          -> HSET username player-id
```

**Leaderboard Categories (Implemented):**

| Category | Score Source | Updated When |
|----------|--------------|--------------|
| `leaderboard:xp` | `player-lifetime-xp` | On XP gain |
| `leaderboard:level` | `player-combat-level` | On level up (tier-1) |
| `leaderboard:deaths` | `player-deaths` | On death (tier-1) |

**Future Leaderboard Categories (Stub):**

| Category | Score Source | Requires |
|----------|--------------|----------|
| `leaderboard:kills` | `player-pvp-kills` | PvP combat system |
| `leaderboard:skill:*` | Skill levels | Skilling system |

**New Durable Fields Required:**
```lisp
;; Add to player struct (types.lisp)
(deaths 0 :type integer)       ; Total times this player has died

;; Migration v3->v4: Add deaths field
(defun migrate-player-v3->v4 (data)
  (unless (getf data :deaths)
    (setf data (plist-put data :deaths 0)))
  data)

;; Future: Add pvp-kills when PvP system implemented
;; (pvp-kills 0 :type integer)  ; Kills of other players (not NPCs)
```

**Consistency Model:**
- **Blob is authoritative**: The player blob (`player:{id}`) is the source of truth
- **Leaderboards are eventually consistent**: Updated best-effort during gameplay
- **Self-healing on login**: `register-player-session` seeds all leaderboards from blob data
- **Real-time updates are best-effort**: Leaderboard zadd calls happen separately from blob saves

This design provides eventual consistency without requiring atomic transactions:
1. **Tier-1 blob saves** (death, level-up, item destruction) use immediate writes with retry
2. **Trades** commit via an atomic Lua script; failures cancel the trade (no retry)
3. **Tier-2 blob saves** (routine state: position, HP) are batched every 30s and best-effort
4. Leaderboard updates happen separately from blob saves (may be lost on crash)
5. On next login, leaderboards are re-seeded from blob → self-healing

**Update Flow:**
```lisp
;; On login: seed leaderboards from authoritative blob
(register-player-session player)
  → (db-update-leaderboard-xp player-id (player-lifetime-xp player))
  → (db-update-leaderboard-level player-id (combat-level ...))
  → (db-update-leaderboard-deaths player-id (player-deaths player))

;; During gameplay: best-effort leaderboard updates
(db-update-leaderboard-deaths player-id deaths)  ; After death
(db-update-leaderboard-xp player-id xp)          ; After XP gain
```

**Why not atomic saves?**
- Simpler implementation without Lua scripts
- Blob is always correct; leaderboards are derived/cached
- Self-healing eliminates need for complex consistency mechanisms
- A crash between blob save and leaderboard update only causes temporary staleness

**Query Functions:**
```lisp
(defun db-get-leaderboard (category &key (top 10) (withscores t))
  "Return top N entries from leaderboard CATEGORY (:xp, :level, :deaths)."
  ...)

(defun db-get-online-count ()
  (storage-scard *storage* "online:players"))

;; Note: username lookup (username:lookup hash) not yet implemented
;; Note: player summary (player:{id}:summary hash) not yet implemented
```

### Deserialization Validation

**Problem:** Malformed data from corrupted saves or malicious tampering can crash server or cause exploits.

**Solution:** Schema validation layer with type checking, bounds checking, and size limits.

**Schema Rules:**
```lisp
(defparameter *player-schema*
  '((:id          :type integer :required t   :min 1)
    (:version     :type integer :required nil :min 1 :max 100)
    (:x           :type number  :required t   :min -1000000.0 :max 1000000.0)
    (:y           :type number  :required t   :min -1000000.0 :max 1000000.0)
    (:hp          :type integer :required t   :min 0 :max 99999)
    (:lifetime-xp :type integer :required nil :min 0 :max 999999999)
    (:playtime    :type number  :required nil :min 0)
    (:created-at  :type integer :required nil :min 0)
    (:deaths      :type integer :required nil :min 0 :max 999999999)
    (:zone-id     :type symbol  :required nil)
    (:stats       :type plist   :required nil)
    (:inventory   :type plist   :required nil)
    (:equipment   :type plist   :required nil)))
```

**Size Limits:**
```lisp
(defparameter *max-player-blob-size* (* 64 1024))  ; 64KB max per player
(defparameter *max-item-stack-size* 999999)        ; Max items per stack
```
Inventory size is fixed by `*inventory-size*` in `config-server.lisp`. Deserialization ignores slots beyond that size.

**Load Pipeline:** See **Phase 6: 4-Outcome Validation System** for the complete load path with:
- Size check on raw string (before parsing)
- Basic sanity check (before migration)
- Full validation (after migration)
- 4-outcome response: `:ok`, `:clamp`, `:quarantine`, `:reject`

**Legacy Note:** The original implementation used reject-only validation. Phase 6 replaces this with 4-outcome validation to avoid bricking legitimate accounts from benign corruption or schema drift while still failing closed on exploit-adjacent data

### Implementation Status

Phases 1-7 have been implemented:

| Phase | Feature | Files | Status |
|-------|---------|-------|--------|
| 1 | Schema Validation | `save.lisp`, `db.lisp` | **Complete** |
| 2 | Redis Metrics | `db.lisp` | **Complete** |
| 3 | Session Ownership (Redis TTL) | `db.lisp`, `data/redis-scripts/` | **Complete** |
| 4 | Structured Data (Leaderboards) | `db.lisp`, `migrations.lisp` | **Complete** |
| 5 | Trade System + Atomic Ops | `trade.lisp`, `intent.lisp` | **Complete** |
| 6 | 4-Outcome Validation | `save.lisp`, `db.lisp` | **Complete** |
| 7 | Index Consistency Model | `docs/db.md` | **Complete** (doc alignment)

**Notes:**
- Trade system uses coins as regular inventory items (no separate currency)
- PvP kills leaderboard deferred until PvP combat system exists
- Skill leaderboards deferred until skilling system exists

### Trade System Specification

**User Interaction:**
- Right-click another player -> Context menu shows "Trade with [playername]"
- Opens trade window for both players
- Both players add items/gold, confirm
- Trade completes atomically or fails entirely

**Trade Flow:**
```
1. Player A right-clicks Player B -> sends trade request intent
2. Server validates: both players exist, in same zone, within range (10 tiles)
3. Server sends trade window open to both clients
4. Players add/remove items (including coins) from offer
5. Both players click "Confirm"
6. Server validates trade and computes post-trade inventories:
   - Verify both players still have offered items
   - Compute new inventory states with items swapped
   - Execute atomic Lua script to commit both players simultaneously
7. Server sends trade complete/fail to both clients
```

**Currency:** Coins are an existing inventory item dropped by NPCs. Trading uses coins as a regular tradeable item - no separate gold/currency system needed.

**Atomic Trade Lua Script:**

Trade state is validated entirely server-side (in Lisp). The Lua script provides atomic commit of the already-validated result - it does not perform trade validation itself.

```lua
-- trade_complete.lua
-- KEYS[1] = player:A, KEYS[2] = player:B
-- ARGV[1] = playerA-data-with-items-removed-gold-adjusted (pre-validated)
-- ARGV[2] = playerB-data-with-items-removed-gold-adjusted (pre-validated)

-- Server has already validated: items exist, counts correct, inventories have space
-- This script just does the atomic multi-key write

local existsA = redis.call('EXISTS', KEYS[1])
local existsB = redis.call('EXISTS', KEYS[2])

if existsA == 0 or existsB == 0 then
  return {err = "PLAYER_NOT_FOUND"}
end

-- Atomic write of both players (all-or-nothing)
redis.call('SET', KEYS[1], ARGV[1])
redis.call('SET', KEYS[2], ARGV[2])

return {ok = "TRADE_COMPLETE"}
```

**Idempotency consideration:** If intents can replay (network retry), consider adding a trade nonce to prevent double-apply. Current implementation relies on trade session state being cleared on completion.

**Trade State (Ephemeral):**
```lisp
(defstruct trade-session
  player-a-id           ; Initiator
  player-b-id           ; Target
  player-a-offer        ; List of (slot-index . count) offered by A
  player-b-offer        ; List of (slot-index . count) offered by B
  player-a-confirmed-p  ; A clicked confirm
  player-b-confirmed-p  ; B clicked confirm
  created-at            ; For timeout (60s inactivity = cancel)
  last-activity         ; Last action timestamp
  state)                ; :pending, :open, :confirming, :complete, :cancelled

;; Trade sessions are ephemeral - stored in memory, not Redis
;; If server crashes mid-trade, trade is cancelled (no items lost)
(defparameter *active-trades* (make-hash-table :test 'eql))
```

**Trade Validation Rules:**
- Both players must be online
- Both players must be in same zone
- Distance between players < 10 tiles
- Neither player already in a trade
- Items must exist in player's inventory
- No trading equipped items (must unequip first)
- Trade auto-cancels after 60s of inactivity

**Tradeable Items:** ALL inventory items are tradeable (coins, arrows, goblin ears, whatever). No item blacklist. KISS.

**Files to Create/Modify:**
- `src/trade.lisp` (new) - Trade system implementation
- `src/ui.lisp` - Trade window UI, context menu
- `src/input.lisp` - Right-click context menu handling
- `src/intent.lisp` - Trade intents (request, offer, confirm, cancel)
- `data/redis-scripts/trade_complete.lua` - Atomic trade script
- `tests/trade-test.lisp` (new) - Trade system tests

---

## Phase 6: 4-Outcome Validation System (COMPLETE)

**Status: COMPLETE**

The current 2-outcome validator (valid/reject) will brick legitimate players when:
- Schema drift from partial deploys
- Content changes (zones/items removed/renamed)
- Benign corruption from migrations that missed fields
- Inventory size changes

With trades, leaderboards, and session ownership now implemented, rejecting a player has cascading effects on social systems. A more nuanced approach is needed.

### The Problem with Reject-Only

Previous behavior (pre-Phase 6):
```lisp
(multiple-value-bind (valid-p errors)
    (validate-player-plist-deep raw-data :log-errors t)
  (unless valid-p
    (return-from db-load-player-validated (values nil nil))))  ; BRICK
```

This causes:
- Permanent account lockout for benign issues
- Orphaned leaderboard entries
- Orphaned online player set entries
- Session ownership keys that must TTL expire
- Support burden requiring manual Redis blob fixes

### Solution: 4-Outcome Validation

Replace binary valid/invalid with four outcomes:

| Outcome | Meaning | Action |
|---------|---------|--------|
| `:ok` | Data valid | Deserialize, login normally |
| `:clamp` | Minor issues, safely fixable | Fix data, tier-1 save, then login |
| `:quarantine` | Suspicious, needs admin review | Deny login, store blob for inspection |
| `:reject` | Exploit-adjacent or malformed | Deny login, store blob for forensics |

### New Validator Signature

**Current:**
```lisp
(validate-player-plist-deep plist) → (values valid-p errors)
```

**4-way validator:**
```lisp
(validate-player-plist-4way plist) → (values action issues fixed-plist)
;; action: :ok, :clamp, :quarantine, :reject
;; issues: list of warning/error strings
;; fixed-plist: corrected data for :clamp, nil for :quarantine/:reject
```

### Field Classification Policy

#### Clamp-Only (Safe Coercions)

These corrections cannot benefit the player and preserve intent:

| Field | Condition | Clamp To |
|-------|-----------|----------|
| `:hp` | < 0 | 0 |
| `:hp` | > max-hp | max-hp |
| `:x`, `:y` | Out of world bounds | Spawn point |
| `:playtime` | < 0 | 0 |
| `:deaths` | < 0 | 0 |
| `:created-at` | Missing | Current time |
| `:version` | Any | Set to current schema version in fixed-plist |

#### Reject (Exploit-Adjacent)

Fixing these could give advantage or hide real bugs:

| Field | Condition | Reason |
|-------|-----------|--------|
| `:id` | Missing/invalid | Can't identify player |
| `:x`, `:y`, `:hp` | Wrong type | Malformed data |
| `:lifetime-xp` | < 0 | Exploit indicator |
| Inventory slot | Negative count | Exploit indicator |
| Inventory slot | Count > max stack | Possible dupe |
| Inventory slot | Non-symbol item-id | Malformed data |
| Stats skill | Negative XP | Exploit indicator |
| Structure | Not a list or bad :version | Corruption (pre-4way sanity check) |

#### Quarantine (Recoverable but Suspicious)

Weird enough to warrant admin inspection:

| Field | Condition | Reason |
|-------|-----------|--------|
| `:zone-id` | Not a symbol | Zone data corrupt |
| `:zone-id` | Unknown (when `*known-zone-ids*` set) | Zone removed/renamed |
| Inventory | Unknown item-id (when `*game-data-loaded-p*`) | Item deprecated |
| Equipment | Unknown item-id (when `*game-data-loaded-p*`) | Item deprecated |

### Storage Layer Fix: Size Check Bug

**Current bug:** Size check happens AFTER `read-from-string`:
```lisp
(let ((raw-data (storage-load *storage* key)))  ; Already parsed!
  (validate-blob-size (prin1-to-string raw-data)))  ; Re-serialize (WRONG)
```

**Fix:** Add `storage-load-raw` to check size BEFORE parsing:
```lisp
(defgeneric storage-load-raw (storage key)
  (:documentation "Load raw string value for KEY. Returns string or NIL.
   Does not parse - returns the exact bytes stored in Redis.
   Use storage-load for parsed plist, storage-load-raw for size checks."))

(defmethod storage-load-raw ((storage redis-storage) key)
  (redis:with-connection (:host (redis-storage-host storage)
                          :port (redis-storage-port storage))
    (red:get key)))

(defmethod storage-load-raw ((storage memory-storage) key)
  (let ((data (gethash key (memory-storage-data storage))))
    (when data (prin1-to-string data))))
```

**Note:** `storage-load-raw` complements (not replaces) `storage-load`. Use raw for size validation before parsing.

**Correct load path:**
```
raw-string → size-check → parse → basic-sanity → migrate → validate-4way → action
```

**Why validate AFTER migration?** Validating before migration causes false rejects when old versions lack fields the migration would add. The flow is:

1. **Size check** - Reject oversized blobs before any parsing (prevents allocation attacks)
2. **Parse** - `read-from-string` with `*read-eval* nil`
3. **Basic sanity** - Pre-migration check: Is it a plist? Has `:id` integer? If `:version` present, is it integer?
4. **Migrate** - Run migration chain to current schema version (missing `:version` treated as 0)
5. **Validate-4way** - Full validation against current schema (now all fields present)
6. **Action** - `:ok`, `:clamp`, `:quarantine`, or `:reject`

### Forensic Storage

Store corrupt blobs for admin inspection:

**New Redis keys:**
```
corrupt:{player-id}:{timestamp}  → {:raw "..." :report (...) :timestamp N}  (TTL: 7 days)
metrics:validation:ok            → counter (INCR on each outcome)
metrics:validation:clamp         → counter
metrics:validation:quarantine    → counter
metrics:validation:reject        → counter
```

**Retention policy:** Corrupt blobs expire after 7 days (604800 seconds) to prevent unbounded growth. A malicious actor flooding with bad data cannot fill Redis with `corrupt:*` keys.

**Functions:**
```lisp
(defparameter *corrupt-blob-ttl-seconds* 604800
  "TTL for corrupt blob forensic storage (7 days).")

(defun store-corrupt-blob (player-id raw-string report)
  "Store corrupt player data for forensic inspection with TTL."
  (let ((key (format nil "corrupt:~a:~a" player-id (get-universal-time)))
        (data (list :raw raw-string :report report :timestamp (get-universal-time))))
    (storage-save-with-ttl *storage* key data *corrupt-blob-ttl-seconds*)))

(defun increment-validation-metric (action)
  "Increment validation outcome counter."
  (storage-incr *storage* (format nil "metrics:validation:~a"
                                   (string-downcase (symbol-name action)))))
```

**Note:** Requires new `storage-save-with-ttl` generic (Redis: `SETEX`, memory: store with expiry timestamp).

### Quarantine Infrastructure

**Minimal safe identity** (can't play, but recoverable):
```lisp
(defun make-quarantined-player (player-id)
  "Create minimal player for quarantine state."
  (make-player 0.0 0.0 :id player-id :zone-id :quarantine))
```

**Client behavior:** Show "Account needs repair - contact support" message. Do not spawn into world.

### Refactored Load Path

**Session ownership ordering:** The caller must `claim-session-ownership` BEFORE calling `db-load-player-validated`. This ensures that if the `:clamp` branch needs to save corrected data, the ownership-safe save path will succeed. Claiming after load creates a race where clamp saves fail.

```lisp
(defun db-load-player-validated (player-id)
  "Load player with 4-outcome validation.
   Returns (values player zone-id action).
   PRECONDITION: Caller has already claimed session ownership for player-id."
  (let* ((key (player-key player-id))
         (raw-string (storage-load-raw *storage* key)))
    (unless raw-string
      (return-from db-load-player-validated (values nil nil :not-found)))

    ;; 1. Size check BEFORE parsing
    (when (> (length raw-string) *max-player-blob-size*)
      (store-corrupt-blob player-id raw-string '("Blob size exceeds limit"))
      (increment-validation-metric :reject)
      (return-from db-load-player-validated (values nil nil :reject)))

    ;; 2. Parse with error handling (read-from-string returns 2 values; we want the first)
    (let ((raw-data (handler-case
                        (let ((*read-eval* nil))
                          (multiple-value-bind (parsed-plist end-pos)
                              (read-from-string raw-string)
                            (declare (ignore end-pos))
                            parsed-plist))
                      (error (e)
                        (store-corrupt-blob player-id raw-string
                                            (list (format nil "Parse error: ~a" e)))
                        (increment-validation-metric :reject)
                        (return-from db-load-player-validated (values nil nil :reject))))))

      ;; 3. Basic sanity (pre-migration) - can we migrate at all?
      ;; - Must be a list (listp check; full plist structure validated later in validate-4way)
      ;; - Must have :id (integer)
      ;; - :version missing is OK (treated as 0 by migration)
      ;; - :version present must be integer
      (let ((version-val (getf raw-data :version)))
        (unless (and (listp raw-data)
                     (integerp (getf raw-data :id))
                     (or (null version-val) (integerp version-val)))
          (store-corrupt-blob player-id raw-string '("Malformed structure: not a valid player data"))
          (increment-validation-metric :reject)
          (return-from db-load-player-validated (values nil nil :reject))))

      ;; 4. Migrate to current schema (adds missing fields)
      (let ((migrated-data (migrate-player-data raw-data)))

        ;; 5. Full 4-way validation (against current schema, after migration)
        (multiple-value-bind (action issues fixed-plist)
            (validate-player-plist-4way migrated-data)
          (increment-validation-metric action)

          (case action
            (:ok
             (values (deserialize-player migrated-data *inventory-size*
                                         (length *equipment-slot-ids*))
                     (getf migrated-data :zone-id) :ok))

            (:clamp
             (log-verbose "Player ~a clamped: ~{~a~^, ~}" player-id issues)
             ;; GUARANTEE: validate-player-plist-4way returns fixed-plist in current schema
             ;; format with :version set to *player-schema-version*. No re-migration needed.
             ;; Tier-1 save of corrected data using ownership-safe path
             (let ((player (deserialize-player fixed-plist *inventory-size*
                                               (length *equipment-slot-ids*))))
               (db-save-player-immediate player)
               (values player (getf fixed-plist :zone-id) :clamp)))

            (:quarantine
             (store-corrupt-blob player-id raw-string issues)
             (warn "Player ~a quarantined: ~{~a~^, ~}" player-id issues)
             (values (make-quarantined-player player-id) :quarantine :quarantine))

            (:reject
             (store-corrupt-blob player-id raw-string issues)
             (warn "Player ~a REJECTED: ~{~a~^, ~}" player-id issues)
             (values nil nil :reject))))))))
```

**Note on `:clamp` save:** The corrected data is saved through `db-save-player-immediate`, which uses the same ownership-safe save path as normal persistence. This is not a special-cased unsafe write.

**Validator contract for `:clamp`:** When `validate-player-plist-4way` returns `:clamp`, the `fixed-plist` is guaranteed to be:
- In current schema format (all required fields present)
- With `:version` set to `*player-schema-version*`
- Ready for immediate save without re-migration

### Implementation Phases

| Phase | Task | Files |
|-------|------|-------|
| 6.1 | Add `storage-load-raw`, `storage-incr`, `storage-save-with-ttl` | `db.lisp` |
| 6.2 | Write `validate-player-plist-4way` | `save.lisp` |
| 6.3 | Add forensic storage functions | `db.lisp` |
| 6.4 | Refactor `db-load-player-validated` | `db.lisp` |
| 6.5 | Add quarantine player handling | `types.lisp`, `net.lisp` |
| 6.6 | Write comprehensive tests | `tests/persistence-test.lisp` |
| 6.7 | Update documentation | `docs/db.md`, `docs/save.md` |

### Validation Tests (current)

```lisp
;; Schema validation tests
(test-validation-valid-data-passes)
(test-validation-missing-required-fields)
(test-validation-wrong-types)
(test-validation-out-of-bounds)
(test-validation-oversized-blob)
(test-validation-nested-stats)
(test-validation-nested-inventory)
(test-validation-sparse-inventory)

;; 4-way clamp tests
(test-4way-clamp-hp-below-zero)
(test-4way-clamp-hp-above-max)
(test-4way-clamp-position-out-of-bounds)
(test-4way-clamp-missing-created-at)
(test-4way-clamp-negative-deaths)

;; 4-way reject tests
(test-4way-reject-missing-id)
(test-4way-reject-negative-lifetime-xp)
(test-4way-reject-negative-item-count)
(test-4way-reject-excessive-item-count)
(test-4way-reject-wrong-type-x)
(test-4way-reject-negative-skill-xp)
(test-4way-reject-inventory-not-list)
(test-4way-reject-slots-not-list)
(test-4way-reject-stats-not-list)
(test-4way-reject-stat-entry-not-list)

;; 4-way quarantine tests
(test-4way-quarantine-invalid-zone-type)
(test-4way-quarantine-unknown-zone)
(test-4way-quarantine-unknown-item)
(test-4way-quarantine-unknown-equipment-item)
(test-4way-validation-skips-zone-check-when-not-loaded)

;; Load/forensics/metrics tests
(test-4way-load-valid-player)
(test-4way-load-clamp-hp)
(test-4way-load-reject-bad-type)
(test-4way-load-not-found)
(test-4way-storage-incr)
(test-4way-storage-save-with-ttl)
(test-4way-forensic-storage)
(test-4way-validation-metrics)
```

### Open Questions

1. **Quarantine UI:** Do we have a "needs repair" screen? If not, quarantine degrades to reject+logging.
2. **Oldest migration:** What's the floor? Currently v1, but could set v2 as minimum.
3. **Unique items:** Do we have unique item IDs that can't be duplicated?
4. **Content churn:** How often will zones/items be removed? Affects quarantine policy.
