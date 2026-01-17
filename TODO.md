### Building and Testing
REMINDER:
**CRITICAL: Before claiming any task is complete, ALL tests must pass:**
```bash
make checkparens        # Verify balanced parentheses in all .lisp files
make ci                 # Cold compile + UDP handshake test (no GPU needed)
make smoke              # Full client/server smoke test with window (2s default)
make test-persistence   # Data integrity tests (serialization, migrations, invariants)
make checkdocs          # Verify docs/foo.md exists for each src/foo.lisp```
**Never skip tests.** If you implement a feature but don't run all test targets, the work is incomplete.
**Write tests for:**
- ✅ **Data corruption risk**: Serialization, migrations, database writes
- ✅ **Invariants**: Things that MUST always be true (HP ≤ max, gold ≥ 0)
- ✅ **Backend equivalence**: Storage abstraction must work identically across backends
- ✅ **Player-facing bugs**: Anything that loses progress, duplicates items, corrupts saves
**Skip tests for:**
- ❌ **Visual bugs**: Rendering, animations, UI layout (manual testing fine)
- ❌ **Input handling**: Mouse clicks, keyboard (already tested via smoke test)
- ❌ **Gameplay feel**: AI behavior, movement smoothness, combat balance
- ❌ **Helper functions**: Utils that don't touch persistent state
**Rule of thumb**: If a bug loses player progress or corrupts their save, write a test. If it's just annoying or ugly, manual testing is fine.
## Documentation
Every `src/foo.lisp` must have a matching `docs/foo.md`. Run `make checkdocs` to verify.
Key design docs:
- `docs/db.md` - Persistence architecture, storage abstraction, write tiers
- `docs/migrations.md` - Schema versioning, migration functions, admin commands
- `docs/save.md` - Serialization format, durable vs ephemeral classification
- `docs/net.md` - UDP protocol, message format, snapshot streaming
- `docs/SERVER_PERFORMANCE.md` - Scaling strategies, bottleneck analysis
- `docs/movement.md` - Physics, collision, zone transitions
**REMINDER: If you update a feature, update the doc.**
## Important Reminders
- **ALL TESTS MUST PASS**: Before claiming work complete, run ALL test targets in order: `make checkparens && make ci && make test-persistence && make checkdocs && make smoke`. No exceptions.
- **Never commit with unbalanced parens**: Run `make checkparens` before committing
- **CI must pass**: `make ci` runs cold compile + UDP handshake test
- **Data integrity tests must pass**: `make test-persistence` ensures no save corruption
- **Docs must be complete**: `make checkdocs` verifies every src file has matching documentation
- **Smoke test must work**: `make smoke` tests actual client/server with graphics
- **Storage abstraction is mandatory**: Never call Redis/database directly from game logic
- **Classify all new state**: Every field must be marked durable or ephemeral
- **Use correct persistence tier**: Tier-1 for critical, tier-2 for routine, tier-3 for logout
- **Write migrations for durable fields**: New persistent fields require schema version bump + migration function
- **Server is authoritative**: Clients send intents, not state
- **Test both backends**: Memory for dev, Redis for persistence testing
### When to Write Tests (Decision Criteria)
**Write tests for:**
- ✅ **Data corruption risk**: Serialization, migrations, database writes
- ✅ **Invariants**: Things that MUST always be true (HP ≤ max, gold ≥ 0)
- ✅ **Backend equivalence**: Storage abstraction must work identically across backends
- ✅ **Player-facing bugs**: Anything that loses progress, duplicates items, corrupts saves
**Skip tests for:**
- ❌ **Visual bugs**: Rendering, animations, UI layout (manual testing fine)
- ❌ **Input handling**: Mouse clicks, keyboard (already tested via smoke test)
- ❌ **Gameplay feel**: AI behavior, movement smoothness, combat balance
- ❌ **Helper functions**: Utils that don't touch persistent state
**Rule of thumb**: If a bug loses player progress or corrupts their save, write a test. If it's just annoying or ugly, manual testing is fine.

---

## Current Tasks / TODO

 Make persistence boring and safe (still simple)

### What We Already Have ✓
- **Versioned save schema + migrations**: `*save-format-version*` and `*player-schema-version*` in save.lisp
- **Migration system**: `migrate-player-v1->v2`, `*player-migrations*` list, automatic migration on load
- **Storage abstraction**: db.lisp provides backend-agnostic API (memory/redis via env var)
- **Three-tier persistence**: Tier-1 immediate (critical), Tier-2 batched (30s), Tier-3 logout

### What We Need ❌

#### Phase 1: Atomic Saves (Safety) - COMPLETE ✓
- [x] Implement write-new-then-rename pattern in db.lisp
- [x] Replace direct Redis SET with temp key + atomic RENAME
- **Impact**: Crash during save won't corrupt existing data

#### Phase 2: Backup Snapshots (Recovery) - COMPLETE ✓
- [x] Systemd timer for hourly Redis backups (deploy/mmorpg-backup.*)
- [x] Timestamped backups in /var/mmorpg/db/backups/
- [x] Automatic rotation (keeps 7 days of hourly backups by default)
- [x] DevOps instructions in README.md
- **Impact**: Can restore from backup if player data corrupted

#### Phase 3: Admin Commands (Testing) - IN PROGRESS
- [x] Implement Tier A commands (see [docs/admin.md](docs/admin.md) for full spec)
- [ ] Add admin action logging (future enhancement)
- **Impact**: Easy debugging/testing without manual Redis commands
- **Status**: All Tier A commands implemented and exported from `mmorpg` package

## Future Tasks / Roadmap

### Admin Commands - Tier B & C

**Tier B** (requires new infrastructure):
- Ban system (ban table, login check)
- IP tracking (store IP on connect, history)
- Login history (timestamps)
- Chat logs (message storage)

**Tier C** (nice to have):
- Admin spawn NPCs
- Weather control
- Maintenance mode
- Backup restoration by timestamp

See [docs/admin.md](docs/admin.md) for full spec.

### Networking Polish

Networking polish that unlocks "MMO feel"
After ownership, the next noticeable improvement is:
Interpolation for remote entities (if not already)
Basic rate limiting + sanity checks (intent frequency, movement bounds)
Optional later: prediction for local player, but only after everything's stable

----------------------------------
- [ ] Test unauthenticated connection intent handling (acceptance criteria #5: verify server ignores intents from unauthenticated clients)

------------------------------

- I'm wondering about player connectivity, and general use of things like "retry" in the codebase. Sometimes in my life as a webdev certain ops were more professional if they used a simple retry mechanism. I know we use UDP already which is pretty forgiving, but if there is any sections of the codebase that might benefit from a simple retry once in a while, let's add that because it seems like it is a cheap way to make the game more professional.
