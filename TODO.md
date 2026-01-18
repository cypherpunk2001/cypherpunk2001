### Code Quality Standards (MANDATORY FOR ALL CHANGES)

**Check EVERY change before claiming complete:**
1. **Tests?** Persistent state → write test | Visual only → skip
2. **Retry?** Tier-1 saves → 5 retries | DB reads → 3 retries | Auth messages → 3 retries | UI/snapshots → no retry
3. **Logging?** Critical fails → `warn` | State changes → `log-verbose` | Hot loops → no logs
4. **Scope?** Config/server state → globals | Computation → locals | Game state → structs (never `*global-players*`)
5. **Data-driven?** Behavior in data files, not hard-coded | Generic (works for NPCs too), not player-only
6. **Plist mutation?** ALL mutable keys initialized at creation | Use structs for complex state | See rule below

**If you skip any of these, explain why in commit message.**

### Plist `setf getf` Pitfall (CRITICAL - Common Source of Silent Bugs)

`setf getf` **silently fails** if the key doesn't exist - NO error, NO warning, value unchanged.

```lisp
;; SILENT FAILURE - key missing
(setf (getf object :respawn) 5.0)  ;; Does nothing if :respawn not in plist!
```

**Prevention Rules:**
1. **Initialize ALL mutable keys** when creating plists: `(list :id id :count nil :respawn 0.0 :dirty nil)`
2. **Use structs** for complex mutable state (structs always work with setf)
3. **Add assertions** during debugging: `(assert (member :key plist))`
4. **Document required keys** for plist-based features

**Full doc:** `docs/PLIST_SETF_GETF_PITFALL.md`

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

### Snapshot Size Optimization (4-Prong Approach) - Status

See `docs/net.md` for full spec.

| Prong | Status | Notes |
|-------|--------|-------|
| **Prong 1: Compact Serialization** | ✅ DONE | Vectors instead of plists, ~64 bytes/player |
| **Prong 2: Delta Compression** | ✅ FIXED | Client partitioning: full to new, delta to synced. New players added via delta. |
| **Prong 3: UDP Fragmentation** | ✅ DONE | Split large snapshots into numbered chunks |
| **Prong 4: zlib Compression** | ⏸️ DEFERRED | Not needed yet |

**Stress Test Results (2026-01-17):**
- Prong 1 + Prong 3 only (no delta): **419 clients stable**, players visible and moving
- Previous Prong 2 bug: players disappeared at ~50 clients (fixed)

**Next:** Stress test Prong 2 fix to verify stability at high client counts.

## Future Tasks / Roadmap

**Potential optimizations to investigate:**

- Entity removal tracking in delta (`:removed-ids` for logged-out players)

- Entity culling in snapshots (skip distant players)
- Batch intent processing



---
- Not wanted at this time: Interest management (only send entities within player's view)
- Spatial partitioning for collision/AI (only check nearby entities)
-- in my opinion, i am wondering if we utilize the concept of "zone's", as we have zones and there is a brief 'Loading...' between them as we travel the world map, theoretically we can ignore all events that occur within a zone that we are currently in, this might help us get a better gameplay and support more live players. We might be able to optimize here but i also want to see the real game, if 500 players are in a crowd town square on screen on a saturday morning i want to see them all at the same time then just like old runescape classic.

-- Also we have not explored multi threading usage, yes we've dev'ed it, but currently we focus on optimize single threaded as much as possible before going multi

### We'd like to support 8,000 players, 2,000 per zone simultaneously. This would blow old school runescape out of the water theoretically if it is possible. They used to support around 2,000 per server. If we had one server and supported 2,000 per zone that would be unreal and truly epic.

---

We might want to try 30hz while aiming to keep messages under 1200 bytes?

---

player logged in but cannot walk when running in server multi threaded mode,
                                                   when binding #:STEP3
WARNING:
   Failed to serialize/send snapshot (frame 3849): The value
                                                     0
                                                   is not of type
                                                     (OR (FLOAT (0.0))
                                                         (RATIONAL (0)))

                                                   when binding #:STEP3
^CSERVER: shutdown requested (interrupt).
SERVER: ok

---

if server is killed unexpectedly, client crashes instead of returning to the login screen
CLIENT FAILED: Condition USOCKET:CONNECTION-REFUSED-ERROR was signalled.

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

---

In general, my finding is that tests are pretty cheap for us to make, and offer us a great ability to try to prevent regressions. So going forward from here, now that our codebase is generally performant and behaving in a way that we like, I would like you to try and go through the entire codebase wide and get as much test coverage as possible with a goal towards simply "preventing regressions" or "catching breaking/changing behavior that we previously intentionally had here" to help us streamline codegen going forwards.
