# Tests

Documentation for the MMORPG test suite.

## Test Suite Overview

Tests are organized as modular domain files under `tests/unit/`, loaded by the aggregator `tests/unit-test.lisp`.

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

- **Files**: `tests/unit/persistence-core-tests.lisp`, `tests/unit/persistence-network-tests.lisp`, `tests/unit/persistence-hardening-tests.lisp`
- **Package**: `mmorpg` (tests run in main package for access to internals)
- **Test runner**: `scripts/test-unit.lisp` (loads aggregator which loads all domain files)
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

- **Files**: `tests/unit/security-input-tests.lisp`, `tests/unit/security-gamestate-tests.lisp`
- **Package**: `mmorpg` (tests run in main package)
- **Test macro**: `define-security-test` - Custom macro for security test boilerplate
- **Ports**: Tests spin up temporary servers on configurable ports
- **Exit codes**: 0 on success, 1 on failure (CI-friendly)

---

# File Organization

Tests are split into modular domain files under `tests/unit/`, loaded by the aggregator:

```
tests/
├── unit-test.lisp              # Aggregator: loads all domain files, builds master test list, runs suites
└── unit/
    ├── 00-test-helpers.lisp    # Shared test helpers (ensure-test-game-data, make-test-world)
    ├── ai-tests.lisp           # AI behavior tests
    ├── auth-tests.lisp         # Auth throughput and rate-limiting tests
    ├── chat-tests.lisp         # Chat message tests
    ├── combat-targeting-tests.lisp  # Combat targeting fix + render cache tests
    ├── combat-tests.lisp       # Combat math tests
    ├── data-tests.lisp         # Data parsing and archetype tests
    ├── db-tests.lisp           # Redis metrics tests
    ├── intent-tests.lisp       # Intent system tests
    ├── migration-tests.lisp    # Schema migration tests
    ├── movement-tests.lisp     # Movement and collision tests
    ├── net-tests.lisp          # Networking and binary snapshot tests
    ├── persistence-core-tests.lisp      # Data integrity, round-trip, storage backend tests
    ├── persistence-hardening-tests.lisp # Validation, 4-way outcomes, ownership, storage failures
    ├── persistence-network-tests.lisp   # Compact serialization, zone-filtered deltas
    ├── profiling-tests.lisp    # Profiling and GC tests
    ├── progression-tests.lisp  # XP, inventory, equipment tests
    ├── save-tests.lisp         # Serialization round-trip tests
    ├── security-gamestate-tests.lisp  # Game authority, ownership, thread-safety tests
    ├── security-input-tests.lisp      # Input validation and protocol security tests
    ├── spatial-tests.lisp      # Spatial grid, zone-players cache, vector pool tests
    ├── trade-tests.lisp        # Trade session and swap tests
    ├── types-tests.lisp        # Type system tests
    ├── utils-tests.lisp        # Utility function tests
    ├── world-graph-tests.lisp  # World graph and pathfinding tests
    ├── zone-cache-tests.lisp   # Zone LRU cache, arming, edge-strip serialization tests
    ├── zone-continuity-tests.lisp # Overstep, seam translation, urgent preload tests
    └── zone-tests.lisp         # Zone loading and manipulation tests

scripts/
└── test-unit.lisp              # Test runner script
```

Each domain file:
- Starts with `(in-package #:mmorpg)`
- Defines test functions
- Defines a `*tests-<domain>*` list of test function symbols (for unit test domains)
- Persistence, security, and trade suites have their own internal `run-*-tests-internal` runners

To add a new test:
1. Find the appropriate domain file in `tests/unit/`
2. Add your test function
3. Add the test name to the domain's `*tests-<domain>*` list (or the runner function for persistence/security/trade)
4. Run `make test-unit` to verify

---

# File Size Guideline

**Rule of thumb:** Around ~1000 lines, a test file should be split to avoid files exceeding ~2000 lines. Split at natural domain boundaries (e.g., `persistence-tests.lisp` → `persistence-core-tests.lisp` + `persistence-network-tests.lisp` + `persistence-hardening-tests.lisp`). If no descriptive name is obvious, `foo-tests-a.lisp` and `foo-tests-b.lisp` is acceptable, but prefer descriptive names when possible.

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
