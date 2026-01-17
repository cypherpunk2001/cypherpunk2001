# mmorpg

## Requirements
- SBCL + Quicklisp
- Emacs + SLIME (or any Common Lisp REPL)
- raylib + raygui
- claw-raylib (prebuild branch recommended)
- redis (valkey)

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
- `make checkdocs` Checks that every `src/foo.lisp` has a matching `docs/foo.md`, errors if any are missing, otherwise prints a friendly reminder when all pass.

Test env overrides:
- `MMORPG_NET_TEST_PORT` override UDP port (default 1337).
- `MMORPG_NET_TEST_SECONDS` override UDP server duration in CI.
