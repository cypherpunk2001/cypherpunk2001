### Code Quality Standards (MANDATORY FOR ALL CHANGES)
*CRITICAL* - Any logging we develop that helps us solve problems
should always belong and stay in verbose mode to help us again in the future.

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
make tests              # Run ALL tests including smoke (recommended)
```

Individual test targets (run by `make tests`):
```bash
make checkparens        # Verify balanced parentheses in all .lisp files
make ci                 # Cold compile + UDP handshake test (no GPU needed)
make test-unit          # Unit tests (68 tests: pure functions, game logic)
make test-persistence   # Data integrity tests (37 tests: serialization, migrations)
make test-security      # Security tests (23 tests: auth, input validation)
make checkdocs          # Verify docs/foo.md exists for each src/foo.lisp
make smoke              # Full client/server smoke test with window (2s default)
```
**Never skip tests.** If you implement a feature but don't run all test targets, the work is incomplete.
### When to Write Tests (Decision Criteria)

**POLICY: If a unit test CAN be written, it SHOULD be written.**

We aim for 99% test coverage. Tests prevent regressions and catch bugs early.

**Write tests for:**
- ✅ **All pure functions**: Functions that take input and return output
- ✅ **Game logic**: Combat calculations, XP/leveling, inventory operations
- ✅ **Data corruption risk**: Serialization, migrations, database writes
- ✅ **Invariants**: Things that MUST always be true (HP ≤ max, gold ≥ 0)
- ✅ **Backend equivalence**: Storage abstraction must work identically across backends
- ✅ **Player-facing bugs**: Anything that loses progress, duplicates items, corrupts saves
- ✅ **Security bugs**: Auth/session edge cases, privilege escalation, replay/forged packets

**Skip tests ONLY for:**
- ❌ **Rendering code**: Functions that call raylib directly
- ❌ **Audio code**: Functions that play sounds
- ❌ **Interactive input**: Real-time mouse/keyboard handling
- ❌ **Network I/O**: UDP send/receive (tested via smoke test)

**Rule of thumb**: If a function has deterministic output for given input, write a test.
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
- **ALL TESTS MUST PASS**: Before claiming work complete, run `make tests`. No exceptions.
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
adding ping hud
investigating if 81a730d origin/master Add advanced client toggles caused tile seam glitches

DONE - we made server hz and client FPS knobs more obvious in config and removed hardcodes there

THINKING - about windowed mode resize options
im thinking client-settings.lisp should be in data/ or something which hopefully is more compatible than ~/.mmorpg
on OS like windows. then offer common window sizes in the same scale that we are developing with actively 1280x720 variants that scale clean all the way up to 1080p, 1440p, and 4k.

---


---

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
