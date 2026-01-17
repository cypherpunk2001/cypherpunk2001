# migrations.lisp

Purpose
- Define schema versions and migration functions for player data
- Provide lazy migration on player load (default behavior)
- Provide eager migration admin command for pre-deploy maintenance

Why we do it this way
- **Lazy migration**: Spreads migration cost across individual player logins
- **Eager migration option**: Allows pre-emptive migration before major deploys
- **Append-only migrations**: Old migrations never deleted; players can skip versions
- **Separated from storage**: Pure data transforms, no storage dependencies

Key concepts

**Schema Version**
```lisp
(defparameter *player-schema-version* 2
  "Current player schema version. Increment when changing player format.")
```

**Migration Registry**
```lisp
(defparameter *player-migrations*
  '((2 . migrate-player-v1->v2))
  "Alist of (version . migration-function) for player data.")
```

Each migration function takes a plist and returns the updated plist.

Key functions
- `migrate-player-data`: Chain migrations from data's version to current (called on player load)
- `migrate-all-players`: Admin command to eagerly migrate all players in storage (in db.lisp)

Walkthrough: lazy migration (default)
1. Player logs in
2. `db-load-player` loads raw plist from storage
3. `migrate-player-data` checks version, runs chain of migrations
4. Player struct created from migrated data
5. On next save, data is stored with current schema version

Walkthrough: eager migration (admin command)
1. Before deploy, admin runs `(migrate-all-players :dry-run t)` to preview
2. Admin runs `(migrate-all-players)` to migrate all players
3. Each player record is loaded, migrated, and saved
4. Deploy proceeds with all players at current schema version

Usage example
```lisp
;; Check what would be migrated (dry run)
(migrate-all-players :dry-run t :verbose t)
;; Output:
;;   Found 150 player records to check
;;   player:1: v1 -> v2
;;   player:2: v2 (current, skipped)
;;   ...
;;   Migration complete: 50 migrated, 100 skipped, 0 errors
;;   (dry-run mode - no changes saved)

;; Actually run migrations
(migrate-all-players :verbose t)
```

Writing a new migration

1. **Increment the schema version**
```lisp
(defparameter *player-schema-version* 3)  ; was 2
```

2. **Write the migration function**
```lisp
(defun migrate-player-v2->v3 (data)
  "v2->v3: Add new-field, defaulting to some-value."
  (unless (getf data :new-field)
    (setf (getf data :new-field) some-default-value))
  data)
```

3. **Register the migration**
```lisp
(defparameter *player-migrations*
  '((2 . migrate-player-v1->v2)
    (3 . migrate-player-v2->v3))  ; added
  "Alist of (version . migration-function)")
```

4. **Update serialization** (in save.lisp)
- Add field to `serialize-player`
- Add field to `deserialize-player`
- Add field to `apply-player-plist`

5. **Update struct** (in types.lisp)
- Add field to player struct
- Add initialization in `make-player`

Migration rules
- **Never delete old migrations**: A player at v1 may need to migrate through v2, v3, v4...
- **Migrations must be pure**: Take plist, return plist, no side effects
- **Always provide defaults**: Missing fields should get sensible defaults
- **Test on copy of production data**: Before deploying, test migrations work correctly
- **Migrations are append-only**: Never modify existing migration functions

Current migrations
| Version | Migration | Description |
|---------|-----------|-------------|
| 2 | migrate-player-v1->v2 | Add `lifetime-xp` field (default 0) |
| 3 | migrate-player-v2->v3 | Add `playtime` (default 0) and `created-at` (default current time) |

Relationship to other files
- **types.lisp**: Player struct definition (add new fields here)
- **save.lisp**: Serialization format (add to serialize/deserialize here)
- **db.lisp**: Storage abstraction, `migrate-all-players` admin command

Design note
- Migrations live in separate file for clarity
- Core migration logic has no storage dependencies (can be tested in isolation)
- `migrate-all-players` is in db.lisp since it needs storage access
- Both lazy (per-login) and eager (admin command) strategies supported
