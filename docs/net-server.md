# net-server.lisp

Purpose
- Server UDP loop entry point: initializes storage, binds socket, runs fixed-tick simulation, and broadcasts snapshots to connected clients.

Key responsibilities
- `run-server`: main server entry point that orchestrates all server-side systems.
- Storage backend initialization from environment variables (`MMORPG_DB_BACKEND`, `MMORPG_REDIS_HOST`, `MMORPG_REDIS_PORT`).
- Persistent Redis connection for main loop (reduces connection churn for batch flush + ownership refresh).
- Profiling and GC scheduling initialization from environment variables.
- Auth worker thread lifecycle (start on boot, stop on shutdown).
- Non-blocking UDP receive loop with per-tick message cap (`*max-messages-per-tick*`).
- Message dispatch: `:hello`, `:register`, `:login`, `:logout`, `:intent`.
- Auth request queuing with backpressure (rate limiting + queue depth limit).
- Fixed-tick simulation via `server-step` with accumulator.
- Periodic batch flush of dirty players (tier-2 writes every ~30s).
- Periodic session ownership refresh with lost-session cleanup.
- Client timeout detection and cleanup.
- Snapshot broadcasting (full or delta) at configurable rate, decoupled from sim rate.
- Private state sending (inventory/equipment/stats) to owning clients.
- Strategic GC scheduling at safe points (after snapshots).
- Graceful shutdown: stop auth workers, flush dirty players, close socket.

Load order
- Loaded fourth among net files: `net-protocol` -> `net-auth` -> `net-snapshot` -> `net-server` -> `net-client` -> `net`.
- Depends on all preceding net files, plus `db`, `server`, `combat`, and `zone` modules.
