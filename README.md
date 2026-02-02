# mmorpg

## Requirements
- SBCL + Quicklisp
- Emacs + SLIME (or any Common Lisp REPL)
- raylib + raygui
- claw-raylib (prebuild branch recommended)
- Valkey/Redis (for persistence)

## Setup
- Follow the claw-raylib build instructions; on the prebuild branch, skip steps 1-2 and start at step 3.
- Register the repo with Quicklisp once per session.
- Start Valkey: `sudo systemctl start valkey`
- For Redis configuration, see [deploy/README.md](deploy/README.md)

## Run (Client/Server UDP)
Server must start first.

### Basic Usage
```shell
make server
```

```shell
make client
```

### Local/Standalone Mode
For zone editing and single-player testing (no server required):
```shell
make local
```
This runs the game in standalone mode with full editor access via ESC menu.

### Verbose Modes
```shell
MMORPG_VERBOSE=1 make server          # Network events, state changes
MMORPG_VERBOSE_COORDS=1 make server   # Entity positions per frame (noisy)
```

### Performance Tuning
```shell
MMORPG_WORKER_THREADS=4 make server      # Parallel snapshot sending
MMORPG_WORKER_THREADS=$(nproc) make server   # Use all CPU cores
MMORPG_SNAPSHOT_RATE=20 make server          # Snapshot rate in Hz (default 20)
MMORPG_BINARY_SNAPSHOTS=1 make server        # Binary snapshots (client/server must match)
MMORPG_NPC_POOL=1 make server                # Enable NPC object pooling (prewarms 256)
MMORPG_GC_SCHEDULING=1 make server           # Periodic safe-point GC (default 60s)
MMORPG_PROFILE=1 MMORPG_VERBOSE_GC=1 make server  # Timing hooks + per-frame GC stats
```

### Build Environment
```shell
MMORPG_ENV=dev make server    # Development (default): safety checks, debug info
MMORPG_ENV=prod make server   # Production: max speed, minimal safety
make server-prod              # Shortcut for production build
```

The `MMORPG_ENV` variable controls SBCL optimization policy:
- `dev` (default): `(speed 2) (safety 2) (debug 2)` - Good for development/debugging
- `prod`: `(speed 3) (safety 1) (debug 0)` - Maximum performance for deployment

### SLIME (two Emacs sessions)

Server REPL:
```lisp
(ql:register-local-projects)
(ql:quickload :mmorpg)
(mmorpg:run-server :host "127.0.0.1" :port 1337)
```

Client REPL:
```lisp
(ql:register-local-projects)
(ql:quickload :mmorpg)
(mmorpg:run-client :host "127.0.0.1" :port 1337)
```

Local/Standalone REPL (zone editing):
```lisp
(ql:register-local-projects)
(ql:quickload :mmorpg)
(mmorpg:run-local)
```

## Storage Backends

**Redis (default)** - Data persists to disk, survives restarts. Requires Valkey running.

**Memory** - RAM only, lost on shutdown. For CI/testing:
```shell
MMORPG_DB_BACKEND=memory make server
```

## Tests
```shell
make tests              # Run ALL tests including smoke (recommended)
```

**Critical test order**: The first three tests MUST run in this exact order:
1. `make checkparens` - Catches syntax errors (fastest)
2. `make ci` - Catches compile errors
3. `make smoke` - Catches runtime errors early

Individual test targets (run by `make tests` in this order):
```shell
make checkparens        # 1st - Syntax error testing (balanced parens)
make ci                 # 2nd - Compile-time error testing, UDP handshake testing
make smoke              # 3rd - Full client/server smoke test with window (2s default)
make test-unit          # All unit tests (game logic, persistence, security, trade)
make checkdocs          # Verify docs exist for each src file
```

**Note:** All test types (unit, persistence, security, trade) run via `make test-unit`. Tests are split into modular domain files under `tests/unit/`, loaded by the aggregator `tests/unit-test.lisp`.

Test env overrides:
- `MMORPG_NET_TEST_PORT` - UDP port (default 1337)
- `MMORPG_NET_TEST_SECONDS` - UDP server duration in CI

## Source Layout

Large source files are split into domain modules (~1000 LOC each). The glue file re-exports shared helpers; the split files contain the actual logic.

| Domain | Glue file | Split modules |
|--------|-----------|---------------|
| Networking | `net.lisp` | `net-protocol`, `net-auth`, `net-snapshot`, `net-server`, `net-client` |
| Rendering | `rendering.lisp` | `rendering-core`, `rendering-tiles`, `rendering-entities`, `rendering-ui` |
| Database | `db.lisp` | `db-storage`, `db-players`, `db-accounts`, `db-admin` |
| Save/Load | `save.lisp` | `save-serialize`, `save-deserialize`, `save-delta`, `save-edge-strips`, `save-validate` |
| Movement | `movement.lisp` | `movement-core`, `movement-collision`, `movement-transition`, `movement-preview` |
| Editor | `editor.lisp` | `editor-core`, `editor-tools`, `editor-io` |

## Code Quality Standards

**See [CLAUDE.md](CLAUDE.md) for detailed criteria and examples.**

## Deployment

See [deploy/README.md](deploy/README.md) for:
- Redis/Valkey configuration
- Automated backup setup
- Restore procedures
