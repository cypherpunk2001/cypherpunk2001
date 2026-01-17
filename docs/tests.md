# Tests

Documentation for the MMORPG test suites.

## Test Suites Overview

| Suite | File | Purpose |
|-------|------|---------|
| **Persistence** | `tests/persistence-test.lisp` | Data integrity (serialization, migrations, invariants) |
| **Security** | `scripts/test-security.lisp` | Input validation, exploit prevention |

---

# Persistence Tests

Documentation for `tests/persistence-test.lisp`.

Data integrity test suite for the MMORPG project.

## Purpose

Strategic, focused testing that prevents player data corruption. Tests cover serialization, migrations, invariants, and storage backends - the critical paths where bugs lose player progress.

## Philosophy

**What we test:**
- ✅ Data corruption risk (serialization, migrations, database writes)
- ✅ Invariants (HP ≤ max, gold ≥ 0, counts within limits)
- ✅ Backend equivalence (memory/Redis behave identically)
- ✅ Player-facing bugs (anything that loses progress or items)

**What we don't test:**
- ❌ Visual bugs (rendering, animations, UI layout)
- ❌ Input handling (mouse clicks, keyboard)
- ❌ Gameplay feel (AI behavior, movement smoothness)
- ❌ Helper functions (utils that don't touch persistence)

**Rule of thumb:** If a bug loses player progress or corrupts their save → write a test. If it's just annoying or ugly → manual testing is fine.

## Running Tests

```bash
make test-persistence    # Run all data integrity tests
```

Should be run as part of the CI pipeline:
```bash
make checkparens && make ci && make test-persistence && make checkdocs && make smoke
```

## Test Coverage (29 tests)

### 1. Persistence Round-Trip Tests
- **test-player-roundtrip**: Serialize then deserialize = identical durable data
- **test-ephemeral-not-persisted**: Ephemeral fields NOT saved to DB
- **test-durable-persisted**: Durable fields ARE saved to DB
- **test-inventory-roundtrip**: Inventory survives serialization
- **test-equipment-roundtrip**: Equipment survives serialization
- **test-stats-roundtrip**: Stats (XP, levels) survive serialization

### 2. XP/Progression Invariant Tests
- **test-xp-never-decreases-on-award**: XP can only increase from awards
- **test-level-increases-with-xp**: Level increases when XP threshold reached
- **test-xp-level-boundary**: Exact XP boundary correctly triggers level-up

### 3. Inventory/Equipment Invariant Tests
- **test-hp-never-exceeds-max**: HP ≤ max HP always
- **test-inventory-count-limits**: Inventory slots never exceed max
- **test-inventory-overflow-returns-leftover**: Adding beyond capacity returns leftover
- **test-inventory-stack-limits**: Stackable items respect max stack size
- **test-equipment-modifiers-applied**: Equipping applies stat modifiers
- **test-equipment-modifiers-removed-on-unequip**: Unequipping removes modifiers

### 4. Storage Backend Tests
- **test-memory-backend-save-load**: Memory backend save/load works
- **test-storage-delete**: Storage delete removes data correctly
- **test-redis-backend-save-load**: Redis backend save/load works
- **test-redis-backend-delete**: Redis delete removes data correctly
- **test-redis-memory-equivalence**: Redis and memory backends behave identically

### 5. Zone Transition Tests
- **test-zone-id-roundtrip**: Zone ID survives serialization
- **test-zone-id-in-db-save**: Zone ID included in DB saves with session

### 6. Tier-1 Immediate Save Tests
- **test-death-triggers-immediate-save**: Player death (HP=0) triggers tier-1 save
- **test-level-up-triggers-immediate-save**: Level-up triggers tier-1 save

### 7. Currency Invariant Tests
- **test-coins-never-negative**: Coins (gold) count can never go negative

### 8. Schema Migration Tests
- **test-migration-v1-to-v2**: v1 player data migrates correctly to v2 (adds lifetime-xp)
- **test-migration-applies-defaults**: Migration applies defaults for missing fields
- **test-lifetime-xp-roundtrip**: lifetime-xp survives serialization roundtrip
- **test-lifetime-xp-incremented**: lifetime-xp increases when XP is awarded

## Test Structure

Each test follows the pattern:
1. Set up test data (create player, inventory, etc.)
2. Execute operation (serialize, save, modify)
3. Assert expected outcome
4. Return T on success, signal error on failure

Helper functions:
- `make-test-player` - Create player with known values
- `count-inventory-item` - Count items in inventory
- `assert-equal`, `assert-true`, `assert-nil`, `assert-<=`, etc. - Assertion helpers

## Adding New Tests

When implementing features that touch player data, add tests if ANY of these apply:

1. **Adding persistent fields to player/NPC/world structs**
   - Write round-trip serialization test
   - Verify field classified as durable or ephemeral

2. **Implementing schema migrations**
   - Write migration test with old-version plist
   - Verify defaults applied correctly

3. **Implementing economy/progression systems**
   - Write invariant tests (currency ≥ 0, XP never decreases)
   - Test edge cases (overflow, underflow, max values)

4. **Adding inventory/equipment/trade systems**
   - Write tests for duplication exploits
   - Test item loss scenarios
   - Verify count limits enforced

5. **Modifying zone transition or respawn logic**
   - Test player state preserved across transitions
   - Verify no item/progress loss on death/respawn

## Implementation Details

- **File**: `tests/persistence-test.lisp`
- **Package**: `mmorpg` (tests run in main package for access to internals)
- **Test runner**: `scripts/test-persistence.lisp`
- **Framework**: Custom lightweight test harness (no external dependencies)
- **Exit codes**: 0 on success, 1 on failure (CI-friendly)

## Maintenance

- Keep tests fast (< 5 seconds total)
- Each test should be independent (no shared state)
- Update tests when schema changes
- Remove obsolete tests when features removed
- Document why a test exists (prevents premature deletion)

## Future Expansion

As the codebase grows, consider adding:
- Schema migration chain tests (v1→v2→v3)
- Invariant tests for new game systems (crafting, trading, etc.)
- Corruption scenario tests (partial saves, crashes mid-write)
- Trade system duplication exploit tests

Remember: Tests should prevent bugs, not create busywork. Only add tests that catch real data corruption risks.

## Notes

**Bug fix discovered during test development:** The death-triggers-immediate-save test revealed a bug in `combatant-apply-hit` where the tier-1 save condition `(and (= new-hp 0) (> hp 0))` was logically impossible (if new-hp=0, then hp≤0). Fixed by capturing `old-hp` before damage calculation and checking `(> old-hp 0)` instead.

---

# Security Tests

Documentation for `scripts/test-security.lisp`.

Security test suite that verifies the server properly validates and sanitizes client input.

## Purpose

Tests that verify the server is resilient against malicious or malformed client input. Covers authentication enforcement, input validation, type safety, and exploit prevention.

## Philosophy

**What we test:**
- Server ignores unauthenticated clients
- Type confusion attacks are rejected (string IDs, malformed data)
- Speed/position exploits are prevented
- Chat message length is enforced server-side
- Double-login attempts are rejected
- Extreme/edge-case values don't crash the server

**What we don't test:**
- DDoS resilience (infrastructure concern)
- Encryption (not implemented yet)
- Rate limiting (future work)

## Running Tests

```bash
make test-security    # Run all security tests
```

Should be run as part of the CI pipeline:
```bash
make checkparens && make ci && make test-persistence && make test-security && make checkdocs && make smoke
```

## Test Coverage (7 tests)

### 1. Authentication Tests
- **test-unauthenticated-intent-rejected**: Server ignores intents from clients that haven't completed auth flow

### 2. Input Validation Tests
- **test-speed-hack-prevented**: Server uses own speed constant, ignores client's move magnitude
- **test-chat-message-length-enforced**: Oversized chat messages are truncated server-side
- **test-malformed-intent-ids-handled**: String/list IDs rejected without crashing (type confusion)
- **test-empty-message-fields-handled**: Nil/missing fields handled gracefully

### 3. Session Security Tests
- **test-double-login-prevented**: Second login attempt for already-logged-in account is rejected

### 4. Edge Case Tests
- **test-extreme-coordinates-handled**: Huge/tiny coordinate values don't crash the server

## Test Structure

Each test:
1. Spawns a server thread on a unique port
2. Creates UDP socket(s) to simulate client(s)
3. Sends malicious/malformed messages
4. Verifies server handles them correctly (rejects, sanitizes, or ignores)
5. Cleans up (closes sockets, joins thread)

Key helpers:
- `with-test-server` - Spawns server, yields socket, cleans up
- `authenticate-client` - Completes auth flow for a socket
- `receive-with-timeout` - Non-blocking receive with deadline

## Security Fixes Discovered

**Malformed ID crash (TYPE-ERROR):** The `test-malformed-intent-ids-handled` test revealed that sending string values for `:requested-attack-target-id` caused a crash in `sync-player-attack-target` when comparing string to number. Fixed by adding `%int-or` validation in `apply-intent-plist`.

## Adding New Security Tests

When adding tests, consider:
1. What malicious input could a client send?
2. What would happen if we trusted that input?
3. Does the server validate/sanitize before use?

Good candidates for new tests:
- Rate limiting (when implemented)
- Trade/economy exploits
- Inventory manipulation attempts
- Admin command injection

## Implementation Details

- **File**: `scripts/test-security.lisp`
- **Package**: `mmorpg` (tests run in main package)
- **Ports**: Each test uses a unique port (1337-1343) to avoid conflicts
- **Exit codes**: 0 on success, 1 on failure (CI-friendly)
