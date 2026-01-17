# tests/persistence-test.lisp

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

## Test Coverage

### 1. Persistence Round-Trip Tests
- **test-player-roundtrip**: Serialize then deserialize = identical durable data
- **test-ephemeral-not-persisted**: Ephemeral fields NOT saved to DB
- **test-durable-persisted**: Durable fields ARE saved to DB
- **test-inventory-roundtrip**: Inventory survives serialization
- **test-equipment-roundtrip**: Equipment survives serialization

### 2. Invariant Tests
- **test-hp-never-exceeds-max**: HP ≤ max HP always
- **test-inventory-count-limits**: Inventory slots never exceed max

### 3. Storage Backend Tests
- **test-memory-backend-save-load**: Memory backend save/load works
- **test-storage-delete**: Storage delete removes data correctly

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

- **File**: `src/tests/persistence-test.lisp`
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
- Redis backend equivalence tests (once Redis stabilizes)
- Invariant tests for new game systems (crafting, trading, etc.)
- Corruption scenario tests (partial saves, crashes mid-write)

Remember: Tests should prevent bugs, not create busywork. Only add tests that catch real data corruption risks.
