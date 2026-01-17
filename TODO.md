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
make test-security      # Security tests (input validation, exploit prevention)
make checkdocs          # Verify docs/foo.md exists for each src/foo.lisp```
**Never skip tests.** If you implement a feature but don't run all test targets, the work is incomplete.
### When to Write Tests (Decision Criteria)
**Write tests for:**
- ✅ **Data corruption risk**: Serialization, migrations, database writes
- ✅ **Invariants**: Things that MUST always be true (HP ≤ max, gold ≥ 0)
- ✅ **Backend equivalence**: Storage abstraction must work identically across backends
- ✅ **Player-facing bugs**: Anything that loses progress, duplicates items, corrupts saves
- ✅ **Security bugs**: Auth/session edge cases, privilege escalation (owning/controlling another entity), replay/forged intent packets, rate-limit abuse, and “double-login” / possession bypasses
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
- **ALL TESTS MUST PASS**: Before claiming work complete, run ALL test targets in order: `make checkparens && make ci && make test-persistence && make test-security && make checkdocs && make smoke`. No exceptions.
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

---

## Current Tasks / TODO

### CRITICAL (Second Analysis Pass)

(none - ID counter fixed)

### HIGH (Second Analysis Pass)

(none - all fixed)

### MEDIUM

(none - 30s data loss window documented in code as intentional tradeoff)

### LOW (defensive for future multi-threading)

- **Dirty Flag Race Condition** - `db.lisp:447-452` - `mark-player-dirty` not atomic. Would matter in multi-threaded scenario, but server is single-threaded.

- **Zone Object Respawn Race** - `progression.lisp:566-582` - Zone objects modified in-place. Would matter in multi-threaded scenario, but server is single-threaded.

- **Nonce Cache Race** - `net.lisp:228-236` - Hash table cleanup not thread-safe. Server is single-threaded so low risk.

## Recently Completed

- **Zone Transition Tier-1 Save** - Changed zone transitions from Tier-2 (batched) to Tier-1 (immediate) saves with retry logic. Prevents position loss on crash after zone change.

- **Pickup Target Range Validation** - Added `pickup-tile-in-range-p` check to `sync-player-pickup-target`. Rejects pickup requests for tiles beyond `*max-target-distance-tiles*` (15 tiles).

- **ID Counter Persistence Race** - Added retry logic and save-before-increment to prevent ID collisions on restart.

- **Session Double-Login Race Condition** - Added `session-try-register` with mutex-protected atomic check-and-set. Prevents concurrent logins.

- **Logout Race Condition** - Reordered: clear auth first, then save player, then unregister session. Prevents state changes after logout.

- **Tier-1 Saves More Aggressive** - Increased retries from 5 to 10, max delay from 500ms to 2s for death/level-up saves.

- **Auth Rate Limiting** - Added per-IP rate limiting: 5 failed attempts triggers 5-minute lockout.

- **Auth Encryption Config** - Added `*auth-require-encryption*` server flag to reject plaintext auth in production.

- **Replay Attack Protection** - Added timestamp validation (60s window) and nonce tracking to encrypted auth.

- **Combat Target Range Validation** - Added `*max-target-distance-tiles*` (15 tiles) check to `sync-player-attack-target` and `sync-player-follow-target`.

- **Equipment Swap Tier-1 Save** - Changed equip/unequip from Tier-2 (batched) to Tier-1 (immediate) saves to prevent item loss on crash.

- **Player Unstuck Feature** - ESC menu "Unstuck" button. Server validates player is truly stuck (can't move in any cardinal direction), then teleports to random position in zone. Prevents exploit as free teleport.

## Future Tasks / Roadmap



---

### Admin Commands - Tier B & C
**Tier B** (requires new infrastructure):
**Tier C** (nice to have):
See [docs/admin.md](docs/admin.md) for full spec.

---

### More security unit tests - TODO
Not Tested (features don't exist yet):
- Economy bugs (no gold/currency system)
- Trade duplication (no trade system)
- Shop exploits (no vendors)
