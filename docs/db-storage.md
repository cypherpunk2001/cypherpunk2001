# db-storage.lisp

Purpose
- Storage abstraction layer implementing generic protocol, Redis and memory backends, key schema, metrics tracking, and initialization.

Key responsibilities
- Define the storage protocol via generic functions (`storage-save`, `storage-load`, `storage-keys`, etc.).
- Implement Redis backend with connection management and error handling.
- Implement in-memory backend for testing and development without Redis.
- Redis metrics tracking via ring buffers for latency percentiles (p50, p99).
- Key schema helpers for consistent Redis key naming (`server-id-counter-key`, player keys, etc.).
- Storage initialization and backend selection based on environment variables.
- `storage-save-with-ttl` for time-limited entries (forensic data, rate limiting).
- `storage-incr` for atomic counter increments (validation metrics).
- Metrics logging and warning when p99 latency exceeds thresholds.

Load order
- Loaded first among db files: `db-storage` -> `db-players` -> `db-accounts` -> `db-admin` -> `db`.
- All other db files depend on storage protocol and backends defined here.
