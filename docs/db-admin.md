# db-admin.lisp

Purpose
- Administrative tooling for database operations including bulk player migration and graceful shutdown flush.

Key responsibilities
- `migrate-all-players`: Eager bulk migration of all stored player data to current schema version with dry-run support.
- Progress reporting during migration with verbose output of per-player version changes.
- Error handling and counting (migrated, skipped, errored) for migration runs.
- Graceful shutdown: flush all dirty players before server exit.
- Admin-level database inspection and maintenance commands.

Load order
- Loaded last among db files: `db-storage` -> `db-players` -> `db-accounts` -> `db-admin` -> `db`.
- Depends on `db-storage` for storage access, `db-players` for player operations, and `migrations.lisp` for `migrate-player-data`.
