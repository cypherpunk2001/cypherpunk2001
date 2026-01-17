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

### Verbose Modes
```shell
MMORPG_VERBOSE=1 make server          # Network events, state changes
MMORPG_VERBOSE_COORDS=1 make server   # Entity positions per frame (noisy)
```

### Performance Tuning
```shell
MMORPG_WORKER_THREADS=4 make server      # Parallel snapshot sending
MMORPG_WORKER_THREADS=$(nproc) make server   # Use all CPU cores
```

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

## Storage Backends

**Redis (default)** - Data persists to disk, survives restarts. Requires Valkey running.

**Memory** - RAM only, lost on shutdown. For CI/testing:
```shell
MMORPG_DB_BACKEND=memory make server
```

## Tests
```shell
make checkparens        # Balanced parentheses in .lisp files
make ci                 # Cold compile + UDP handshake (no GPU)
make test-persistence   # Data integrity tests
make checkdocs          # Verify docs exist for each src file
make smoke              # Full client/server with window (2s)
```

Test env overrides:
- `MMORPG_NET_TEST_PORT` - UDP port (default 1337)
- `MMORPG_NET_TEST_SECONDS` - UDP server duration in CI

## Deployment

See [deploy/README.md](deploy/README.md) for:
- Redis/Valkey configuration
- Automated backup setup
- Restore procedures
