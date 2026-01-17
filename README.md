# mmorpg

## Requirements
- SBCL + Quicklisp
- Emacs + SLIME (or any Common Lisp REPL)
- raylib + raygui
- claw-raylib (prebuild branch recommended)
- redis (valkey)

## Redis notes

Redis defaults:
  - ✅ appendfsync everysec - already set
  - ✅ auto-aof-rewrite-percentage 100 - already set
  - ✅ auto-aof-rewrite-min-size 64mb - already set
  - ✅ dir /var/lib/valkey/ - data directory configured
  - ✅ stop-writes-on-bgsave-error yes - safety enabled
Missing:
  - ❌ appendonly no - Needs to be yes (critical for durability)
  - ❌ No save directives - Needs RDB snapshots for backups

Enable missing:

```bash
sudo sed -i 's/^appendonly no$/appendonly yes/' /etc/valkey/valkey.conf
echo -e '\n# RDB snapshot settings for MMORPG persistence\nsave 300 1\nsave 60 1000' | sudo tee -a /etc/valkey/valkey.conf
sudo systemctl restart valkey
systemctl status valkey
```

✅ AOF Enabled:

appendonly yes

✅ RDB Snapshots Configured:

save 300 1

save 60 1000

✅ Valkey Started Successfully with Persistence:

The key lines in the status show AOF is working:
Creating AOF base file appendonly.aof.1.base.rdb on server start
Creating AOF incr file appendonly.aof.1.incr.aof on server start
Ready to accept connections tcp

Your Valkey is now configured for maximum durability with:
- AOF: Logs every write operation (max 1 second data loss)
- RDB: Snapshots every 5 minutes (if anything changed) or every 1 minute (if 1000+ changes)


**Redis is now the default storage backend.**

`make server` uses Redis by default - develop close to production!

**Storage Backends**

Redis Storage (:redis) - **DEFAULT**

What it is:
- Data persisted to disk via Valkey (Redis-compatible)
- Data survives server restarts, crashes, code reloads
- Requires Valkey running on port 6379

This is the default because:
- ✅ Dev close to production: Catch persistence bugs early
- ✅ Test real durability: Migrations, crash recovery, tier-1 writes
- ✅ Long-running dev sessions: Build up a character over multiple runs
- ✅ No surprises: What works locally works in production

Memory Storage (:memory) - for CI/testing only

What it is:
- Everything stored in a Lisp hash table in RAM
- Data completely lost when server stops
- No external dependencies (doesn't need Valkey running)

Use cases:
- ✅ CI/automated tests: `make ci` and `make smoke` use memory backend
- ✅ Clean slate testing: Explicit opt-in when you need fresh state
- ✅ Working offline: When Valkey isn't available

To use memory storage explicitly:
```shell
MMORPG_DB_BACKEND=memory make server
```

**Practical Example (with Redis default)**

```shell
# Run 1: Create character, gain XP, reach level 5
make server   # Uses Redis by default
# Stop server (Ctrl+C)

# Run 2: Character still level 5 with all XP/items intact!
make server
```

**Bottom line**: Redis is default. Use `MMORPG_DB_BACKEND=memory` only when you explicitly need a clean slate or don't have Valkey running.


## Setup
- Follow the claw-raylib build instructions; on the prebuild branch, skip steps 1-2 and start at step 3.
- Register the repo with Quicklisp once per session.

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
Enable diagnostic logging for debugging:

```shell
# General verbose mode (network events, state changes)
MMORPG_VERBOSE=1 make server
MMORPG_VERBOSE=1 make client

# Verbose coordinates mode (entity positions per frame, very noisy)
MMORPG_VERBOSE_COORDS=1 make server
MMORPG_VERBOSE_COORDS=1 make client

# Both verbose modes
MMORPG_VERBOSE=1 MMORPG_VERBOSE_COORDS=1 make server
```

### Performance Tuning
```shell
# Use multiple worker threads for parallel snapshot sending
MMORPG_WORKER_THREADS=4 make server

# Use all CPU cores
MMORPG_WORKER_THREADS=$(nproc) make server
```

SLIME (two Emacs sessions, one per process):

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

## Tests
- `make checkparens` Checks all `.lisp` files in `data/` and `src/` for balanced parentheses and general sexp structure.
- `make ci` runs a cold compile check (no window, no GPU needed) and performs a UDP handshake smoke test.
- `make smoke` starts a UDP server + client, opens the client window briefly, and exits automatically.
  It runs from `src/` so existing `../assets` paths resolve correctly.
  The smoke target is wrapped in a Linux `timeout` to kill hung runs.
  Defaults: 2 seconds runtime, 5 seconds timeout.
- `make test-persistence` Data integrity unit tests (serialization, migrations, invariants)
- `make checkdocs` Checks that every `src/foo.lisp` has a matching `docs/foo.md`, errors if any are missing, otherwise prints a friendly reminder when all pass.

Test env overrides:
- `MMORPG_NET_TEST_PORT` override UDP port (default 1337).
- `MMORPG_NET_TEST_SECONDS` override UDP server duration in CI.
