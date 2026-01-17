# Database Architecture

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
(defgeneric storage-load (storage key)
  (:documentation "Load data for KEY. Returns plist or NIL if not found."))

(defgeneric storage-save (storage key data)
  (:documentation "Save DATA under KEY. Returns T on success."))

(defgeneric storage-delete (storage key)
  (:documentation "Delete KEY. Returns T if existed."))

(defgeneric storage-exists-p (storage key)
  (:documentation "Return T if KEY exists."))

(defgeneric storage-flush (storage)
  (:documentation "Force any pending writes to durable storage."))

(defgeneric storage-connect (storage)
  (:documentation "Establish connection to storage backend."))

(defgeneric storage-disconnect (storage)
  (:documentation "Close connection to storage backend."))

(defgeneric storage-keys (storage pattern)
  (:documentation "Return list of keys matching PATTERN (e.g., 'player:*')."))
```

### Concrete Implementations

```lisp
;; Redis implementation
(defclass redis-storage ()
  ((connection :accessor storage-connection)))

(defmethod storage-load ((storage redis-storage) key)
  (let ((raw (redis:get key)))
    (when raw
      (let ((*read-eval* nil))
        (read-from-string raw)))))

(defmethod storage-save ((storage redis-storage) key data)
  ;; Atomic write-then-rename pattern for crash safety
  ;; 1. Write to temp key first
  ;; 2. Atomically RENAME to real key
  ;; If crash during step 1, real key is untouched
  (let ((temp-key (format nil "temp:~a:~a" key (get-internal-real-time))))
    (redis:set temp-key (prin1-to-string data))
    (redis:rename temp-key key))  ; Atomic, overwrites if exists
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
- Sufficient for 10k+ concurrent users on single instance
- Natural key-value model matches our access patterns (player-id -> state)

### Client Library: cl-redis

**Repository:** https://github.com/vseloved/cl-redis

**Why cl-redis:**
- Standard CL Redis client with good adoption (193 GitHub stars)
- Full Redis command support via `RED` package (e.g., `(red:get key)`, `(red:set key value)`)
- **Pipelining support** - critical for batch writes (300x speedup in benchmarks)
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
player:{player-id}          -> Player durable state blob
player:{player-id}:version  -> Schema version number
player:{player-id}:updated  -> Last update timestamp

zone:{zone-id}:objects      -> Zone object state (respawns, etc.)
zone:{zone-id}:version      -> Zone schema version

server:config               -> Server configuration
server:id-counter           -> Global ID allocation counter
```

### Data Format

All values are Lisp s-expressions (our existing serialization format) with a version header:

```lisp
(:version 1
 :player-id 12345
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
(defparameter *player-schema-version* 2)  ; v2: added lifetime-xp
```

### Migration Chain

Migrations are **append-only** and **chained**:

```lisp
(defparameter *player-migrations*
  '((1 . migrate-player-v0->v1)
    (2 . migrate-player-v1->v2)
    (3 . migrate-player-v2->v3)))

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

### Example Migration (v1->v2: lifetime-xp)

```lisp
(defun migrate-player-v1->v2 (data)
  "v1->v2: Add lifetime-xp field, defaulting to 0."
  (unless (getf data :lifetime-xp)
    (setf (getf data :lifetime-xp) 0))
  data)
```

This migration is tested in `persistence-test.lisp` with `test-migration-v1-to-v2`.

## Crash-Safe Writes (Atomic Saves)

To prevent data corruption during crashes, all Redis saves use the **write-new-then-rename** pattern:

```lisp
;; Instead of directly overwriting:
(redis:set "player:123" new-data)  ; DANGEROUS: crash mid-write corrupts key

;; We write to a temp key first, then atomically rename:
(redis:set "temp:player:123:12345" new-data)  ; Crash here = temp key orphaned
(redis:rename "temp:player:123:12345" "player:123")  ; Atomic, overwrites safely
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

## Future: Postgres Cold Storage

When to migrate to Redis + Postgres:

1. **Scale:** >50k registered players (Redis memory pressure)
2. **Queries:** Need complex queries (leaderboards, analytics, support tools)
3. **Compliance:** Need audit logs, GDPR deletion, data export
4. **Multi-region:** Need geographic data distribution

### Migration Path

When ready, the architecture becomes:

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

### Phase 1: Redis Integration

- [ ] Add cl-redis dependency to ASDF system
- [ ] Implement storage abstraction layer (storage protocol + redis-storage class)
- [ ] Implement key schema and serialization
- [ ] Add version field to all serialized data
- [ ] Implement dirty flag and batch flush
- [ ] Implement tier-1 immediate writes for critical operations
- [ ] Add login/logout Redis read/write
- [ ] Configure Redis persistence (RDB + AOF)
- [ ] Set up backup cron job
- [ ] Add basic monitoring

### Phase 2: Migration System

- [ ] Implement migration chain runner
- [ ] Add migration test framework
- [ ] Document migration procedure

### Phase 3: Hardening

- [ ] Implement graceful shutdown flush
- [ ] Add disconnect grace period
- [ ] Implement write rate limiting
- [ ] Add crash recovery testing
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
