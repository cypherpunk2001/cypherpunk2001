## Current Tasks / TODO

Tests (Strategic, Data-Integrity Focused)

**Goal**: Small, focused test suite (~10 tests) for data integrity only. Skip rendering, input, AI - focus on "can't corrupt player progress."

### Test Suite Plan (Option A)

**File**: `src/tests/persistence-test.lisp`
**Run**: `make test-persistence` (added to CI after `make ci`)
**Effort**: 1-2 hours | **Value**: Catches 80% of corruption bugs

#### Tests Implemented ✅

1. **Persistence Round-Trip Tests**
   - [x] Serialize then deserialize = identical data (HP, XP, position, inventory)
   - [x] Ephemeral fields NOT saved to DB (attack-timer, click-marker-timer, hit-timer)
   - [x] Durable fields ARE saved (HP, stats, inventory, equipment, position)
   - [x] Inventory survives serialization
   - [x] Equipment survives serialization

2. **Schema Migration Tests** (Future)
   - [ ] Old saves (v1) load correctly after migration to v2
   - [ ] Migration applies defaults for new fields
   - [ ] Multiple version jumps work (v1 → v3 skipping v2)

3. **Invariant Tests**
   - [x] HP never exceeds max HP
   - [x] Inventory count never exceeds max slots
   - [ ] Gold never goes negative (future: when gold system added)
   - [ ] Zone transitions preserve player state (future)

4. **Storage Abstraction Tests**
   - [x] Memory backend save/load/delete works correctly
   - [ ] Redis backend equivalence (future: when Redis stabilizes)

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

- [x] Create `src/tests/persistence-test.lisp` with ~10 tests (9 tests implemented)
- [x] Add `make test-persistence` target to Makefile
- [x] Add to CI: `make checkparens && make ci && make test-persistence && make checkdocs && make smoke`
- [x] **Update CLAUDE.md** with testing requirements:
  - How to run tests (make test-persistence)
  - When to write new tests (decision criteria above)
  - Requirement: All tests must pass before claiming task complete
- [x] Create `docs/tests.md` documentation


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
