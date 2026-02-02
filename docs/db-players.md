# db-players.lisp

Purpose
- Player persistence operations including save/load, dirty flag management, session tracking, leaderboards, and forensic storage for corrupt data.

Key responsibilities
- Save and load player data via the storage abstraction (`db-save-player`, `db-load-player`).
- Tier-1 immediate saves (`db-save-player-immediate`) for critical events (death, level-up).
- Dirty flag system: `mark-player-dirty`, `flush-dirty-players` for batched tier-2 writes.
- Session management: `register-player-session`, `db-player-logout` for tier-3 logout saves.
- ID counter persistence (`db-save-id-counter`, `db-load-id-counter`).
- Forensic storage of corrupt blobs with TTL for admin inspection.
- Validation metric counters (ok, clamp, quarantine, reject).
- Online player tracking and leaderboard queries.
- Player ownership verification for security checks.

Load order
- Loaded second among db files: `db-storage` -> `db-players` -> `db-accounts` -> `db-admin` -> `db`.
- Depends on `db-storage` for storage protocol; depended on by `db-accounts` and `db-admin`.
