# Code Review (CLAUDE.md Criteria)
Date: 2026-01-22
Scope: src/*.lisp (core gameplay, net, save, movement, progression, combat)

Findings (ordered by severity)

1) Medium — Tier-1 retry policy mismatches (death/level-up/equip/zone transition)
- CLAUDE.md specifies Tier-1 saves use `with-retry-exponential` with 5 retries and 100–500ms backoff.
- Current Tier-1 save calls use 10 retries and/or different backoff settings.
  - Death save: `src/combat.lisp:32-56` (`:max-retries 10`, `:max-delay 2000`).
  - Level-up save: `src/progression.lisp:238-250` (`:max-retries 10`, `:max-delay 2000`).
  - Equip/unequip saves: `src/progression.lisp:555-590` (`:max-retries 10`).
  - Zone transition save: `src/movement.lisp:1149-1155` (`:initial-delay 50`).
Impact: Deviates from documented resilience policy; could lead to longer stalls or inconsistent save behavior during critical events.

2) Low/Medium — Hot-loop allocation from `players-in-zone` despite cached zone-players
- `players-in-zone` allocates a new list and vector each call (`src/movement.lisp:42-49`).
- `update-sim` calls it per zone every tick (`src/main.lisp:472-490`).
- A zone-players cache exists in `zone-state` for O(zone-players) operations (see `src/types.lisp`).
Impact: Extra per-tick consing in the hot loop; avoidable by using `zone-state-zone-players` or reusing buffers.

3) Low — Public snapshot includes target IDs likely not needed by other clients
- Network-only serialization includes `:attack-target-id` and `:follow-target-id` (`src/save.lisp:580-602`) and compact snapshot includes them as indices 17/18 (`src/save.lisp:971-1034`).
- Client rendering/input code does not appear to use other players’ target IDs.
Impact: Unnecessary bandwidth in 60Hz snapshots and potential leakage of private intent state; should be private or omitted if unused.

4) Low — Intent sanitization gap for pickup/drop IDs
- `apply-intent-plist` claims all values are sanitized, but `:requested-pickup-target-id` and `:requested-drop-item-id` are taken verbatim (`src/net.lisp:1111-1146`).
- This allows arbitrary types from untrusted clients into intent state/logs.
Impact: Hardening gap; could be used for type confusion or log injection. Consider validating to expected symbol/int types.

Open Questions / Assumptions
- Are `attack-target-id` / `follow-target-id` used for any client-side UI/FX that isn’t visible in the current code search? If not, they can be removed from public snapshots.
- Is the Tier-1 retry guidance in CLAUDE.md meant to be strict, or is the “10 retries over ~10s” policy intentional for death/level-up/equip? The current code contradicts the documented standard.

Change Summary
- Added `CODEX_REVIEW.md` with findings against CLAUDE.md criteria.

--------------------------------------

Claude Findings

### 1. Consider Converting Legacy setf getf to plist-put

**Location:** `src/progression.lisp:656,719,725,815`

While these are safe because objects are pre-initialized, converting to `plist-put` would be more defensive:

```lisp
;; Current (safe but relies on initialization)
(setf (getf object :count) new-count)

;; Recommended (explicitly safe)
(setf object (plist-put object :count new-count))
```

**Priority:** LOW - Current code is correct due to proper initialization.

### 2. Document Raylib Dependency in utils.lisp

**Location:** `src/utils.lisp:207-215` (`screen-to-world`)

The function uses raylib types. Consider moving to a client-specific utils file or adding a comment clarifying it's client-only.

**Priority:** LOW - Function is only called from client code.

### 3. Audit defvar vs defparameter Consistency

**Location:** `src/utils.lisp:29-40`

Profile tracking uses `defvar`, which is correct for persistent state. However, the comment could clarify why these shouldn't reset on reload.

**Priority:** VERY LOW - Code is correct, just documentation.
