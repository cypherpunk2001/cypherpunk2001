# PLAN: Modularize unit-test file into multiple test files

## Decision
Proceed. The current ~10k line `tests/unit-test.lisp` is difficult to navigate and review. Splitting into multiple descriptive files (target <= ~1000 lines each) improves maintainability without changing how tests are invoked (`make test-unit`).

## Goals
- Keep `make test-unit` working unchanged.
- Keep all tests in a predictable, deterministic load order.
- Split tests into descriptive files by domain.
- Preserve existing test functions and semantics.

## Non-goals
- Change test framework or assertion style.
- Rename or rewrite large test bodies unless necessary for load order.

## Proposed layout
- Keep `tests/unit-test.lisp` as the **aggregator + runner**.
- Add `tests/unit/` directory with domain-specific files, for example:
  - `tests/unit/00-test-helpers.lisp`
  - `tests/unit/utils-tests.lisp`
  - `tests/unit/combat-tests.lisp`
  - `tests/unit/movement-tests.lisp`
  - `tests/unit/progression-tests.lisp`
  - `tests/unit/ai-tests.lisp`
  - `tests/unit/data-tests.lisp`
  - `tests/unit/net-tests.lisp`
  - `tests/unit/db-tests.lisp`
  - `tests/unit/zone-tests.lisp`
  - `tests/unit/seamless-zone-tests.lisp`

## Plan

1) **Create `tests/unit/` and move test bodies**
   - Move test function bodies from `tests/unit-test.lisp` into the relevant domain files.
   - Each file begins with `(in-package #:mmorpg)`.
   - Keep helper/test builder functions in `00-test-helpers.lisp`.

2) **Aggregator: load all unit test files**
   - Update `tests/unit-test.lisp` to load all files under `tests/unit/` in a deterministic order.
   - Recommended: sort by filename so `00-` helpers load first.
   - Keep `run-unit-tests` and the test suite wrappers in `tests/unit-test.lisp`.

3) **Aggregator: modular test lists**
   - Replace the single giant list with per-domain lists defined in the new files:
     - Example: each file defines a `*tests-<domain>*` list.
   - In `tests/unit-test.lisp`, build the master list by `append` in a fixed order.

4) **Update test runner script (if needed)**
   - `scripts/test-unit.lisp` can stay unchanged if `tests/unit-test.lisp` loads the new files.
   - If not, add a directory load step before calling `run-unit-tests`.

5) **Verify**
   - Run `make test-unit` and then full `make tests` to ensure no regressions.

6) **Documentation updates (final step)**
   - Update `CLAUDE.md` to describe the new modular test layout and how `tests/unit-test.lisp` loads `tests/unit/*.lisp`.
   - Update `README.md` with the same test structure summary.
   - Update `docs/tests.md` to document the modular layout and file naming convention.

## Acceptance criteria
- `make test-unit` passes.
- No test function lost or renamed.
- Test file size targets met (most files <= ~1000 lines).
- Docs accurately describe the new test structure.

