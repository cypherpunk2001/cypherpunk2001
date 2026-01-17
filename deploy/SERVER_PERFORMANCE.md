# Server Performance Analysis & Optimization Strategy

## Current Architecture (Single-Zone Server)

The current `run-server` implementation uses a single-threaded event loop that:
1. Receives UDP messages from all clients (blocking, tight loop)
2. Applies client intents to player state
3. Runs fixed-tick simulation (all players + NPCs)
4. Serializes game state once
5. Sends snapshots to all clients

## Performance Characteristics

### What Works Well
- **Snapshot optimization**: State is serialized once and sent to all clients (efficient)
- **Fixed timestep**: Simulation is deterministic and frame-rate independent
- **UDP datagram**: Low overhead, no connection state, suitable for real-time games
- **Client-side smooth**: User reports smooth gameplay with hundreds of NPCs on client

### Current Bottlenecks for 10k Users (500/zone = 20 zones)

#### 1. Single Zone Limitation (CRITICAL)
**Problem**: Current server runs ONE zone only. For 10k users @ 500/zone, need 20 concurrent zones.
**Impact**: Cannot scale beyond ~500 concurrent users per server process.
**Solution**: Zone sharding (see recommendations below)

#### 2. Blocking I/O
**Problem**: `receive-net-message` blocks until message arrives (line 320 in net.lisp)
**Impact**: If no messages arrive, server wastes CPU cycles spinning in tight loop
**Solution**: Use `:timeout` parameter or separate I/O thread

#### 3. Serial Client Processing
**Problem**: `apply-client-intents` processes all clients sequentially (line 343)
**Impact**: O(n) with number of clients, but intent copying is cheap
**Current**: Not a bottleneck until 1000+ clients per zone

#### 4. Simulation Complexity
**Problem**: `server-step` → `update-sim` runs all entity updates serially
**Impact**: O(players * npcs) for combat checks, O(npcs) for AI
**Current**: User reports client smooth with hundreds of NPCs, suggesting this is acceptable

#### 5. Snapshot Serialization
**Problem**: `serialize-game-state` creates large plist representation every frame
**Impact**: Grows with number of players/NPCs/items in zone
**Current**: Done once per frame, shared across all clients (good!)

## Scaling Strategies

### Strategy 1: Horizontal Scaling (RECOMMENDED for 10k users)
**Run multiple server processes, one per zone**

Advantages:
- Simple: No threading complexity, no shared state
- Robust: Process crash affects one zone only
- Scales linearly: Add more servers for more zones
- Works with Common Lisp: No GIL concerns

Implementation:
```bash
# Zone 1
make server PORT=1337 ZONE=zone-1

# Zone 2
make server PORT=1338 ZONE=zone-2

# ... etc
```

Cross-zone communication:
- Players changing zones → disconnect from old server, connect to new server
- Zone chat → server-to-server UDP messages
- Global events → shared Redis/database

### Strategy 2: Multi-Threading (for single-zone optimization)
**Use threads for I/O and parallel subsystems**

Safe threading opportunities:
1. **I/O thread**: Separate thread for UDP receive/send
2. **Per-entity parallelism**: NPCs in separate spatial regions
3. **Snapshot send**: Parallel `send-net-message` calls

Unsafe/complex:
- Parallel simulation requires fine-grained locking
- Mutable game state makes synchronization tricky
- SBCL native threads exist but GC can cause pauses

Example (I/O thread):
```lisp
(let ((intent-queue (sb-concurrency:make-mailbox)))
  ;; I/O thread
  (sb-thread:make-thread
    (lambda ()
      (loop (sb-concurrency:send-message intent-queue
              (receive-net-message socket buffer)))))

  ;; Simulation thread
  (loop
    (let ((intents (drain-mailbox intent-queue)))
      (apply-intents intents)
      (server-step game nil dt accumulator)
      (send-snapshots clients game socket))))
```

### Strategy 3: Zone Sharding Within Process
**Run multiple zones in one process with thread pool**

Moderate complexity, requires:
- Thread-safe intent queues per zone
- Separate game state per zone
- Snapshot serialization per zone
- Coordinated shutdown

Not recommended as first approach - horizontal scaling is simpler.

## Immediate Optimizations (Low-Hanging Fruit)

### 1. Non-Blocking UDP Receive
**Current**: Tight loop with blocking receive
**Fix**: Add timeout to `usocket:wait-for-input`

```lisp
(when (usocket:wait-for-input socket :timeout 0.001 :ready-only t)
  (receive-net-message socket recv-buffer))
```

Already done! (line 50 in net.lisp)

### 2. Intent Batching (Already Optimal)
Current code already batches: receives all intents in inner loop before simulation.

### 3. Snapshot Sharing (Already Optimal)
Current code already shares: serializes once, sends to all clients.

### 4. Reduce Allocation in Hot Path
**Opportunity**: `combat-event->plist` and `serialize-game-state` allocate heavily
**Fix**: Reuse buffers, object pools (requires profiling first)

### 5. Early Exit for Empty Client List
```lisp
(when (null clients)
  (sleep *sim-tick-seconds*)
  (continue)) ; Skip simulation if no players
```

## Recommendations

### For 500 players/zone (single zone)
**Current architecture is likely sufficient**
- Client is already smooth with hundreds of entities
- Server simulation is similar complexity
- Bottleneck would be network bandwidth, not CPU

Test by running:
```bash
# Simulate 500 clients (requires load testing tool)
./tools/load-test --clients 500 --host 127.0.0.1 --port 1337
```

### For 10k players (20 zones)
**Use horizontal scaling with zone sharding**
1. Run 20 server processes, one per zone
2. Load balancer assigns players to zones based on world position
3. Cross-zone communication via server-to-server messages
4. Shared persistence layer (database) for save/load

### For 100k players (200 zones)
**Add threading within servers**
1. I/O thread for network (receive/send)
2. Per-zone thread pool for parallel simulation
3. Dedicated serialization threads
4. Monitoring and auto-scaling

## Profiling Recommendations

Before optimizing, profile to find actual bottlenecks:

```lisp
#+sbcl
(require :sb-sprof)

(sb-sprof:with-profiling (:max-samples 10000
                           :report :graph
                           :loop t)
  (run-server :max-seconds 60.0))
```

Focus on:
- Time spent in `server-step` vs I/O
- Allocation rate in `serialize-game-state`
- CPU usage per client

## Conclusion

**For immediate deployment (10k users):**
- Horizontal scaling with 20 server processes
- Each server handles one zone (500 players)
- Simple, robust, scales linearly

**For optimization (later):**
- Profile first to identify real bottlenecks
- Add I/O threading if network becomes bottleneck
- Consider spatial partitioning for NPC AI
- Optimize serialization if that's the hot path

**Do NOT optimize prematurely:**
- Current architecture works for single zone
- Client is already smooth with hundreds of entities
- Threading adds complexity without clear need
- Measure first, optimize second
