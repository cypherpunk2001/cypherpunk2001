### Code Quality Standards (MANDATORY FOR ALL CHANGES)

**Check EVERY change before claiming complete:**
1. **Tests?** Persistent state → write test | Visual only → skip
2. **Retry?** Tier-1 saves → 5 retries | DB reads → 3 retries | Auth messages → 3 retries | UI/snapshots → no retry
3. **Logging?** Critical fails → `warn` | State changes → `log-verbose` | Hot loops → no logs
4. **Scope?** Config/server state → globals | Computation → locals | Game state → structs (never `*global-players*`)
5. **Data-driven?** Behavior in data files, not hard-coded | Generic (works for NPCs too), not player-only

**If you skip any of these, explain why in commit message.**

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

- [ ] Test unauthenticated connection intent handling (acceptance criteria #5: verify server ignores intents from unauthenticated clients)

## Future Tasks / Roadmap

### Retry Logic Implementation - COMPLETE ✓

Comprehensive retry mechanisms implemented across all critical operations to improve reliability and prevent data loss during transient failures:

**Retry Utilities (utils.lisp)**
- `with-retry-exponential` - Exponential backoff for database operations (100ms → 500ms)
- `with-retry-linear` - Linear delay for network operations (50ms fixed)
- `exponential-backoff-delay` - Helper for calculating backoff timing

**Priority 1 - CRITICAL (Data Loss Risk)**
- ✅ Death saves (combat.lisp) - 5 retries, prevents logout-to-survive exploit
- ✅ Level-up saves (progression.lisp) - 5 retries, prevents XP rollback
- ✅ Login/auth database operations (net.lisp) - 3 retries for verify-credentials, get-character-id, load-player, set-character-id
- ✅ Registration database operations (net.lisp) - 3 retries for create-account, set-character-id

**Priority 2 - HIGH (UX Impact)**
- ✅ Auth response UDP messages (net.lisp) - 3 retries with 50ms delay for all auth-ok/auth-fail messages
- ✅ Server startup ID counter load (db.lisp) - 5 retries to prevent ID collisions on restart

**Priority 3 - MEDIUM (Graceful Degradation)**
- ✅ Zone transitions (movement.lisp) - 2 retries for zone file loading

**Impact:**
- Prevents data corruption during Redis connection hiccups
- Eliminates logout-to-survive exploits
- Reduces login failures from transient database issues
- Improves player experience during network packet loss
- Protects critical tier-1 saves (death, level-up) from failure

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
- Interpolation for remote entities (if not already)
- Basic rate limiting + sanity checks (intent frequency, movement bounds)
- Optional later: prediction for local player, but only after everything's stable
