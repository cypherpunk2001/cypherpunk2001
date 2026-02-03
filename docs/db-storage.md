# db-storage.lisp

Purpose
- Storage abstraction layer implementing generic protocol, Redis and memory backends, key schema, metrics tracking, and initialization.

Status
- **Implemented.** This doc reflects `src/db-storage.lisp` as of 2026-02-03.

Key responsibilities
- Define the storage protocol via generic functions (`storage-save`, `storage-load`, `storage-keys`, etc.).
- Implement Redis backend with connection management, metrics, and error handling.
- Implement in-memory backend for testing/dev without Redis.
- Key schema helpers for consistent Redis key naming.
- Session ownership TTL helpers (`storage-save-with-ttl`, `storage-setnx-with-ttl`, `storage-refresh-ttl`).
- **Batch TTL refresh** (`storage-refresh-ttl-batch`) for ownership refresh efficiency.
- Metrics tracking via ring buffers for latency percentiles (p50, p99).
- Storage initialization and backend selection based on environment variables.

Backend notes
- **Redis backend** uses explicit connect/close per operation in the storage layer; the net-server main loop now wraps itself in a persistent Redis connection to avoid per-tick churn.
- **Memory backend** maintains a separate TTL hash table (`*memory-storage-ttls*`) and cleans expired keys on access.

Load order
- Loaded first among db files: `db-storage` -> `db-players` -> `db-accounts` -> `db-admin` -> `db`.
- All other db files depend on storage protocol and backends defined here.
