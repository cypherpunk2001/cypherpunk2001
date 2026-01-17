# server.lisp

Purpose
- Provide an in-process headless server loop that runs the authoritative simulation.

Why we do it this way
- Separating client input from server simulation now makes the eventual network split straightforward.
- Headless simulation lets us test server behavior without rendering or audio.

What it does
- Builds authoritative simulation state without client-only subsystems.
- Spawns a primary player plus a `players` array for multi-client simulation.
- Runs fixed-tick simulation steps and reports zone transitions.

Key functions
- `spawn-player-at-world`: spawn a player on a valid open tile near the world spawn center.
- `make-sim-state`: build world/player/players/NPCs/entities/id-source/combat-events without client subsystems; logs counts in verbose mode.
- `make-server-game`: construct a headless game struct (audio/ui/render/assets/camera/editor are nil; net role is `:server`).
- `apply-client-intent`: copy the client intent payload into the server intent.
- `server-step`: apply client intent and run fixed-tick simulation steps.
- `run-headless`: convenience loop for headless simulation (no rendering), with fatal error context logging.

Walkthrough: server tick
1) Client input writes into `client-intent`.
2) `server-step` copies the client intent into the authoritative intent on the player.
3) Fixed-tick simulation steps run (`update-sim`), validating targets and updating state.
4) Zone transitions are returned to the caller for client-side sync.

Design note
- The server loop owns authoritative state; the client only sends intent.

---

## Multi-Threading Model

### Configuration

Worker threads enable parallel snapshot sending for high client counts:

```bash
# Single-threaded (default)
make server

# Parallel snapshot sends
MMORPG_WORKER_THREADS=4 make server

# Use all CPU cores
MMORPG_WORKER_THREADS=$(nproc) make server
```

### Thread Responsibilities

| Thread | Work Performed |
|--------|----------------|
| **Main Thread** | Game simulation, message handling, all game logic |
| **Worker Threads** | UDP socket writes for snapshot delivery only |

The game loop remains single-threaded. Worker threads are short-lived, spawned per-frame only for the snapshot send phase when `worker-threads > 1` and running on SBCL.

### Server Tick Flow

```
1. Receive client messages     [Main thread]
2. Process auth/intents        [Main thread]
3. Run game simulation         [Main thread]
4. Serialize snapshot (once)   [Main thread]
5. Send snapshots              [Worker threads if configured]
6. Cleanup disconnected        [Main thread]
```

### Thread-Safe Shared State

All shared hash tables are protected by mutexes (SBCL only):

| Mutex | Protects | Location |
|-------|----------|----------|
| `*session-lock*` | `*active-sessions*` - authenticated users | net.lisp |
| `*player-sessions-lock*` | `*player-sessions*` - dirty flag system | db.lisp |
| `*auth-nonce-lock*` | `*auth-nonce-cache*` - replay attack prevention | net.lisp |
| `*auth-rate-limits-lock*` | `*auth-rate-limits*` - brute force prevention | net.lisp |
| `*zone-objects-lock*` | Zone object respawn/pickup | progression.lisp |
| `socket-lock` (per-frame) | UDP socket during parallel sends | net.lisp |

### Wrapper Macros

Each mutex has a corresponding macro that gracefully degrades on non-SBCL implementations:

```lisp
(with-session-lock ...)           ;; Session management
(with-player-sessions-lock ...)   ;; Dirty flag / batch flush
(with-auth-nonce-lock ...)        ;; Replay detection
(with-auth-rate-limits-lock ...)  ;; Rate limiting
(with-zone-objects-lock ...)      ;; Zone object state
```

On non-SBCL Lisps, these expand to `(progn ...)` (no locking).

### What's Protected

- **Session registration/logout** - Prevents double-login race conditions
- **Dirty flag marking** - Prevents lost updates during batch flush
- **Batch save collection** - Minimizes lock hold time (collect under lock, IO outside)
- **Nonce cache access** - Prevents replay attack cache corruption
- **Rate limit tracking** - Ensures accurate brute force attempt counting
- **Zone object pickup/respawn** - Prevents item duplication

### Known Limitations

1. **Admin commands** - Server globals (`*server-game*`, `*server-clients*`) are not mutex-protected. SLIME REPL admin commands should not be run during active gameplay with multiple worker threads.

2. **Socket lock per-frame** - Creates new mutex every frame for parallel sends. Not a correctness issue, but slight overhead. Future optimization: create once at startup.

3. **Non-SBCL** - Threading features only work on SBCL. Other Lisp implementations run single-threaded.

### Single vs Multi-Threaded Parity

With all race conditions fixed, multi-threaded mode (`MMORPG_WORKER_THREADS > 1`) provides identical gameplay behavior to single-threaded mode. The only difference is network I/O parallelization during snapshot delivery.
