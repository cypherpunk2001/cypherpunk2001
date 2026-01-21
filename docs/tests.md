# Tests

Documentation for the MMORPG test suite.

## Test Suite Overview

All tests are consolidated into a single file: `tests/unit-test.lisp`

| Suite | Section | Purpose |
|-------|---------|---------|
| **Unit** | Unit Tests | Pure functions, game logic, utilities |
| **Persistence** | Persistence Tests | Data integrity (serialization, migrations, invariants, storage) |
| **Security** | Security Tests | Input validation, exploit prevention |
| **Trade** | Trade Tests | Atomic trade execution, ownership checks, backend parity |

## Running Tests

```bash
make test-unit           # Run all tests (unit, persistence, security, trade)
make tests               # Run full test suite (checkparens, ci, smoke, test-unit, checkdocs)
```

The full test order:
```bash
make checkparens && make ci && make smoke && make test-unit && make checkdocs
```

**Critical test order**: The first three tests MUST run in this exact order:
1. `make checkparens` - Verify balanced parentheses (fastest, catches syntax errors)
2. `make ci` - Cold compile + UDP handshake (catches compile errors)
3. `make smoke` - Full client/server smoke test (catches runtime errors early)

---

# Persistence Tests

Data integrity test suite covering serialization, migrations, invariants, and storage backends.

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

## Test Coverage (98 tests)

### 1. Round-Trip and Durable/Ephemeral Coverage
- Player serialization round-trips, durable fields persisted, ephemeral excluded.

### 2. XP/Progression Invariants
- XP monotonicity, level thresholds, and HP clamping.

### 3. Inventory/Equipment Invariants
- Stack limits, overflow behavior, equipment modifiers applied/removed.

### 4. Storage Backends
- Memory/Redis save/load/delete equivalence and storage abstraction behavior.

### 5. Zone and Tier-1 Save Coverage
- Zone-id persistence, tier-1 save triggers (death, level-up, item consume).

### 6. Leaderboard and Death Tracking
- Death increments and leaderboard updates.

### 7. Migrations
- v1→v4 chain, defaults applied, new durable fields round-trip.

### 8. Compact Serialization
- Compact vector round-trips, quantization, enum encoding.

### 9. Validation and 4-Outcome Flow
- Schema validation rejects malformed data and 4-way clamp/quarantine/reject behavior.

### 10. Storage Failure Semantics and Ownership
- Storage errors signal correctly, dirty flags preserved, ownership reclaim/loss logic.

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

- **File**: `tests/unit-test.lisp` (Persistence Tests section)
- **Package**: `mmorpg` (tests run in main package for access to internals)
- **Test runner**: `scripts/test-unit.lisp`
- **Framework**: Custom lightweight test harness (no external dependencies)
- **Exit codes**: 0 on success, 1 on failure (CI-friendly)

## Maintenance

- Keep tests fast (< 5 seconds total)
- Each test should be independent (no shared state)
- Update tests when schema changes
- Remove obsolete tests when features removed
- Document why a test exists (prevents premature deletion)

## Notes

**Bug fix discovered during test development:** The death-triggers-immediate-save test revealed a bug in `combatant-apply-hit` where the tier-1 save condition `(and (= new-hp 0) (> hp 0))` was logically impossible (if new-hp=0, then hp≤0). Fixed by capturing `old-hp` before damage calculation and checking `(> old-hp 0)` instead.

---

# Unit Tests

Purpose: validate pure functions, game logic, and utilities that do not touch persistence.

Current coverage: 173 tests spanning utils, combat math, progression helpers, movement math, AI helpers, data parsing, zone helpers, intent helpers, and serialization helpers.

---

# Trade Tests

Purpose: validate trade session validation and atomic swap logic across backends.

Current coverage: 14 tests covering trade validation rules, Redis Lua script behavior, and memory-backend parity.

---

# Security Tests

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
- Encryption (optional, not covered by the security suite)

## Test Coverage (24 tests)

### 1. Authentication Tests
- **test-unauthenticated-intent-rejected**: Server ignores intents from clients that haven't completed auth flow

### 2. Input Validation Tests
- **test-speed-hack-prevented**: Server uses own speed constant, ignores client's move magnitude
- **test-chat-message-length-enforced**: Oversized chat messages are truncated server-side
- **test-malformed-intent-ids-handled**: String/list IDs rejected without crashing (type confusion)
- **test-empty-message-fields-handled**: Nil/missing fields handled gracefully

### 3. Session Security Tests
- **test-double-login-prevented**: Second login attempt for already-logged-in account is rejected

### 4. Rate Limiting and Concurrency
- **test-auth-rate-limiting**: Excessive auth failures are rate-limited
- **test-concurrent-rate-limit-recording**: Auth rate limit counters are thread-safe

### 5. Edge Case Tests
- **test-extreme-coordinates-handled**: Huge/tiny coordinate values don't crash the server

## Test Structure

Each test:
1. Spawns a server thread on a unique port
2. Creates UDP socket(s) to simulate client(s)
3. Sends malicious/malformed messages
4. Verifies server handles them correctly (rejects, sanitizes, or ignores)
5. Cleans up (closes sockets, joins thread)

Key helpers:
- `with-test-server` - Spawns server, yields socket, cleans up (security tests only)
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
- Auth encryption (when enabled)
- Trade/economy exploits
- Inventory manipulation attempts
- Admin command injection

## Implementation Details

- **File**: `tests/unit-test.lisp` (Security Tests section)
- **Package**: `mmorpg` (tests run in main package)
- **Test macro**: `define-security-test` - Custom macro for security test boilerplate
- **Ports**: Tests spin up temporary servers on configurable ports
- **Exit codes**: 0 on success, 1 on failure (CI-friendly)

---

# File Organization

All tests are now consolidated into a single file for simplicity:

```
tests/
└── unit-test.lisp          # All tests (unit, persistence, security, trade)

scripts/
└── test-unit.lisp          # Test runner script
```

To add a new test:
1. Open `tests/unit-test.lisp`
2. Find the appropriate section (Unit, Persistence, Security, or Trade)
3. Add your test function following the existing patterns
4. Add the test name to the `run-*-tests-internal` function in that section
5. Run `make test-unit` to verify

---

# Test Philosophy

**Write tests for:**
- ✅ Pure functions (take input, return output, no side effects)
- ✅ Game logic (combat, XP, movement, AI decisions)
- ✅ Data integrity (serialization, migrations, invariants)
- ✅ Security (input validation, exploit prevention)
- ✅ Edge cases (boundaries, empty inputs, max values)

**Skip tests for:**
- ❌ Rendering code (requires GPU)
- ❌ Audio code (requires audio device)
- ❌ Real-time input (tested via smoke test)

**Rule of thumb:** If a function has logic that could break, write a test. The only exception is code that requires hardware (GPU, audio, input devices).
