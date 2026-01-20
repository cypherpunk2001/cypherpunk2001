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
(defparameter *player-schema-version* 3)  ; v3: added playtime, created-at
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
```

These migrations are tested in `persistence-test.lisp` with `test-migration-v1-to-v2` and `test-migration-v1-to-v3-chain`.

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

---

## Addendum: Production Hardening

The sections below address architectural concerns for production MMO features (trades, banks, guilds, leaderboards). These build on the foundation above.

### Atomic Multi-Entity Operations

**Problem:** The write-then-rename pattern is atomic per-key but NOT atomic across multiple keys. Trades, bank transfers, and guild operations touch multiple players simultaneously.

**Risk:** Server crash between individual writes causes:
- Item duplication (player A loses item, crash, player B never receives)
- Item deletion (player A's item removed, crash before player B gets gold)
- Inconsistent state requiring manual GM intervention

**Solution:** Redis Lua scripts for atomic multi-entity operations.

```lisp
;; New storage generic for atomic multi-key operations
(defgeneric storage-execute-atomic (storage script keys args)
  (:documentation "Execute atomic operation across multiple keys via Lua script."))
```

**Lua Script Pattern:**
```lua
-- trade_complete.lua: Atomic two-player item/gold swap
-- KEYS[1] = player:A, KEYS[2] = player:B
-- ARGV[1] = item-id, ARGV[2] = gold-amount

local playerA = redis.call('GET', KEYS[1])
local playerB = redis.call('GET', KEYS[2])

-- Validation (decoded in Lua or pre-validated by server)
if not playerA or not playerB then
  return {err = "PLAYER_NOT_FOUND"}
end

-- Modify both atomically (Lua scripts are atomic in Redis)
redis.call('SET', KEYS[1], modified_playerA)
redis.call('SET', KEYS[2], modified_playerB)

return {ok = "TRADE_COMPLETE"}
```

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
- Enables multi-server deployment (one server per zone cluster)
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
online:players           -> SADD player-id        (with expiring members)
guild:{id}:members       -> SADD player-id
zone:{id}:players        -> SADD player-id        (for zone population)

# Hashes (for quick lookups without loading full blob)
player:{id}:summary      -> HSET name level zone-id guild-id kills deaths
username:lookup          -> HSET username player-id
```

**Leaderboard Categories (Implemented):**

| Category | Score Source | Updated When |
|----------|--------------|--------------|
| `leaderboard:xp` | `player-lifetime-xp` | On XP gain (batched) |
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
- Sorted sets/hashes updated IN SAME TRANSACTION as blob save
- Use Lua script to atomically update blob + indexes
- On load, blob is authoritative; indexes are derived/cached

**Update Pattern:**
```lua
-- save_with_indexes.lua
-- KEYS[1] = player:{id}, KEYS[2] = leaderboard:xp, KEYS[3] = player:{id}:summary
-- ARGV[1] = player-data, ARGV[2] = lifetime-xp, ARGV[3] = player-id, ...

redis.call('SET', KEYS[1], ARGV[1])
redis.call('ZADD', KEYS[2], ARGV[2], ARGV[3])
redis.call('HSET', KEYS[3], 'name', ARGV[4], 'level', ARGV[5])
return {ok = "SAVED_WITH_INDEXES"}
```

**Query Functions:**
```lisp
(defun db-get-leaderboard (category &key (start 0) (count 100))
  "Return top players for CATEGORY (:xp, :level, :kills)."
  (let ((key (format nil "leaderboard:~a" category)))
    (storage-zrevrange *storage* key start (+ start count -1) :withscores t)))

(defun db-get-online-count ()
  (storage-scard *storage* "online:players"))

(defun db-lookup-player-by-name (username)
  (storage-hget *storage* "username:lookup" (string-downcase username)))
```

### Deserialization Validation

**Problem:** Malformed data from corrupted saves or malicious tampering can crash server or cause exploits.

**Current gaps:**
- No type checking (`:hp "ATTACK!"` accepted)
- No bounds checking (`:hp -999999` accepted)
- No size limits on inventory beyond array bounds
- Missing fields silently default

**Solution:** Schema validation layer before deserialization.

**Validation Rules:**
```lisp
(defparameter *player-schema*
  '((:id        :type integer :required t :min 1)
    (:x         :type float   :required t :min -1000000.0 :max 1000000.0)
    (:y         :type float   :required t :min -1000000.0 :max 1000000.0)
    (:zone-id   :type symbol  :required t)
    (:hp        :type integer :required t :min 0 :max 99999)
    (:lifetime-xp :type integer :required nil :min 0 :default 0)
    (:version   :type integer :required t :min 1)))

(defun validate-player-plist (plist)
  "Validate PLIST against *player-schema*. Returns (values valid-p errors)."
  (let ((errors nil))
    (dolist (rule *player-schema*)
      (let* ((key (first rule))
             (value (getf plist key))
             (props (rest rule))
             (type-spec (getf props :type))
             (required (getf props :required))
             (min-val (getf props :min))
             (max-val (getf props :max)))
        ;; Check required
        (when (and required (null value))
          (push (format nil "Missing required field: ~a" key) errors))
        ;; Check type
        (when (and value (not (typep value type-spec)))
          (push (format nil "Invalid type for ~a: expected ~a, got ~a"
                        key type-spec (type-of value)) errors))
        ;; Check bounds
        (when (and value (numberp value))
          (when (and min-val (< value min-val))
            (push (format nil "~a below minimum: ~a < ~a" key value min-val) errors))
          (when (and max-val (> value max-val))
            (push (format nil "~a above maximum: ~a > ~a" key value max-val) errors)))))
    (values (null errors) (nreverse errors))))
```

**Size Limits:**
```lisp
(defparameter *max-player-blob-size* (* 64 1024))  ; 64KB max per player
(defparameter *max-inventory-slots* 100)           ; Hard cap on slots
(defparameter *max-stack-count* 999999)            ; Max items per stack

(defun validate-blob-size (data-string)
  (when (> (length data-string) *max-player-blob-size*)
    (error 'blob-too-large :size (length data-string) :max *max-player-blob-size*)))
```

**Integration:**
```lisp
(defun db-load-player-validated (player-id)
  "Load player with full validation."
  (let* ((raw (storage-load *storage* (player-key player-id))))
    (when raw
      (validate-blob-size (prin1-to-string raw))
      (let ((migrated (migrate-player-data raw)))
        (multiple-value-bind (valid-p errors) (validate-player-plist migrated)
          (unless valid-p
            (warn "Invalid player data for ~a: ~{~a~^, ~}" player-id errors)
            (return-from db-load-player-validated nil))
          (deserialize-player migrated *inventory-size* (length *equipment-slot-ids*)))))))
```

**Validation Behavior: Reject, Don't Clamp**

Invalid data is **rejected entirely** rather than clamped to valid ranges. Rationale:
- Clamping hides corruption - if HP is -999999, something is very wrong
- Rejecting surfaces bugs immediately rather than masking them
- Players with corrupted data should be flagged for GM investigation
- Production: Consider a "quarantine" table for corrupted player records

If a player cannot load due to validation failure:
1. Log detailed error with all validation failures
2. Return nil (player cannot login)
3. Admin can inspect raw Redis data and manually fix or restore from backup

### Implementation Priority

Phases are implemented **sequentially** in order:

| Phase | Feature | Risk | Value | Status |
|-------|---------|------|-------|--------|
| 1 | Schema Validation | Low | High | Planned |
| 2 | Redis Metrics | Low | Medium | Planned |
| 3 | Session Ownership (Redis TTL) | Medium | High | Planned |
| 4 | Structured Data (Leaderboards: xp, level, deaths) | Medium | Medium | Planned |
| 5 | Trade System + Atomic Ops | Medium | High | Planned |

**Notes:**
- Phase 5 implements trading AND atomic operations together (no deferral)
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
6. Server executes atomic Lua script:
   - Validate both inventories have items being traded
   - Swap items atomically (coins are just another inventory item)
7. Server sends trade complete/fail to both clients
```

**Currency:** Coins are an existing inventory item dropped by NPCs. Trading uses coins as a regular tradeable item - no separate gold/currency system needed.

**Atomic Trade Lua Script:**
```lua
-- trade_complete.lua
-- KEYS[1] = player:A, KEYS[2] = player:B
-- ARGV[1] = playerA-data-with-items-removed-gold-adjusted
-- ARGV[2] = playerB-data-with-items-removed-gold-adjusted

-- Both player states are pre-computed by server with items/gold swapped
-- This script just does the atomic write of both

local existsA = redis.call('EXISTS', KEYS[1])
local existsB = redis.call('EXISTS', KEYS[2])

if existsA == 0 or existsB == 0 then
  return {err = "PLAYER_NOT_FOUND"}
end

-- Atomic write of both players
redis.call('SET', KEYS[1], ARGV[1])
redis.call('SET', KEYS[2], ARGV[2])

return {ok = "TRADE_COMPLETE"}
```

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
