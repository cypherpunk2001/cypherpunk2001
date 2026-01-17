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
- `docs/save.md` - Serialization format, durable vs ephemeral classification
- `docs/net.md` - UDP protocol, message format, snapshot streaming
- `docs/SERVER_PERFORMANCE.md` - Scaling strategies, bottleneck analysis
- `docs/movement.md` - Physics, collision, zone transitions

**REMINDER: If you update a feature, update the doc.**

## Important Reminders

- **ALL TESTS MUST PASS**: Before claiming work complete, run ALL test targets in order: `make checkparens && make ci && make smoke && make test-persistence && make checkdocs`. No exceptions.
- **Never commit with unbalanced parens**: Run `make checkparens` before committing
- **CI must pass**: `make ci` runs cold compile + UDP handshake test
- **Data integrity tests must pass**: `make test-persistence` ensures no save corruption
- **Docs must be complete**: `make checkdocs` verifies every src file has matching documentation
- **Smoke test must work**: `make smoke` tests actual client/server with graphics
- **Storage abstraction is mandatory**: Never call Redis/database directly from game logic
- **Classify all new state**: Every field must be marked durable or ephemeral
- **Use correct persistence tier**: Tier-1 for critical, tier-2 for routine, tier-3 for logout
- **Server is authoritative**: Clients send intents, not state
- **Test both backends**: Memory for dev, Redis for persistence testing

---

## Current Tasks / TODO


8. **Schema Migration Tests** ✅
   - [x] Old saves (v1) load correctly after migration to v2
   - [x] Migration applies defaults for new fields (lifetime-xp → 0)
   - [x] lifetime-xp survives serialization roundtrip
   - [x] lifetime-xp increments on XP award
   - [ ] Multiple version jumps work (v1 → v3 skipping v2) - future when v3 added

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

### Integration

- [x] Create `src/tests/persistence-test.lisp` with comprehensive tests (25 tests implemented)
- [x] Add `make test-persistence` target to Makefile
- [x] Add to CI: `make checkparens && make ci && make test-persistence && make checkdocs && make smoke`
- [x] **Update CLAUDE.md** with testing requirements:
  - How to run tests (make test-persistence)
  - When to write new tests (decision criteria above)
  - Requirement: All tests must pass before claiming task complete
- [x] Create `docs/tests.md` documentation
- [x] Redis backend tests (equivalence with memory backend)
- [x] Zone transition persistence tests
- [x] Tier-1 immediate save tests (death, level-up)
- [x] Currency invariant tests (coins never negative)


## Future Tasks / Roadmap

Networking polish that unlocks “MMO feel”
After ownership, the next noticeable improvement is:
Interpolation for remote entities (if not already)
Basic rate limiting + sanity checks (intent frequency, movement bounds)
Optional later: prediction for local player, but only after everything’s stable

----------------------------------

Make persistence boring and safe (still simple)

### What We Already Have ✓
- **Versioned save schema + migrations**: `*save-format-version*` and `*player-schema-version*` in save.lisp
- **Migration system**: `migrate-player-v1->v2`, `*player-migrations*` list, automatic migration on load
- **Storage abstraction**: db.lisp provides backend-agnostic API (memory/redis via env var)
- **Three-tier persistence**: Tier-1 immediate (critical), Tier-2 batched (30s), Tier-3 logout

### What We Need ❌

#### Phase 1: Atomic Saves (Safety) - HIGH PRIORITY
- [ ] Implement write-new-then-rename pattern in db.lisp
- [ ] Replace direct Redis SET with temp key + atomic RENAME
- **Impact**: Crash during save won't corrupt existing data

#### Phase 2: Backup Snapshots (Recovery) - MEDIUM PRIORITY
- [ ] Add periodic backup function (every 5 minutes)
- [ ] Copy player blobs to timestamped keys: `backup:<timestamp>:<id>`
- **Impact**: Can restore from backup if player data corrupted

#### Phase 3: Admin Commands (Testing) - LOW PRIORITY
- [ ] Add REPL helpers: `admin-wipe-character`, `admin-grant-item`, `admin-print-save`
- **Impact**: Easy debugging/testing without manual Redis commands

**Recommendation**: Start with Phase 1 only. Atomic saves = biggest safety win for minimal code.

----------------------------------




----------------------------------
- [ ] Test unauthenticated connection intent handling (acceptance criteria #5: verify server ignores intents from unauthenticated clients)
