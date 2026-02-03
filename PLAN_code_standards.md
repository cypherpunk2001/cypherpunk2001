# PLAN: Code Standards Compliance (AGENTS.md)

**Date:** 2026-02-03
**Related:** FINDINGS_code_standards.md

---

## Objectives

Bring the codebase into full, explicit compliance with the Code Style Rules, Code Quality Checklist, and SBCL Performance Directives in AGENTS.md. This plan eliminates the three concrete gaps from findings and adds unambiguous guardrails so future work stays compliant by default.

**Gaps to close (from findings):**
1) Missing tests for batch TTL refresh paths
2) Missing type declarations in hot loops
3) Per‑tick allocations in snapshot event serialization

**Already compliant (no changes required, but explicitly preserved):**
- File size guideline (no files >1500 LOC)
- Storage abstraction (no direct Redis calls in gameplay)
- `*read-eval* nil` for untrusted input
- Retry logic requirements satisfied
- Logging gated by `*verbose*` flags
- Data‑driven design intact
- Global optimize policy in `src/config.lisp`
- SIMD deferred

---

## Phase 1 — Batch TTL Refresh Tests (Coverage Gap)

**Goal:** Add unit tests for `storage-refresh-ttl-batch` and the batch path in `refresh-all-session-ownerships`.

**Where to add tests**
- Add new test functions in `tests/unit/persistence-hardening-tests.lisp` (co-locate with existing storage failure helpers).
- Register new tests in `tests/unit/persistence-core-tests.lisp` under `run-persistence-tests-internal`.

**Tests to add (exact behaviors):**

1) **Batch refresh (memory storage, basic)**
- Create keys with non-expired TTLs in `*memory-storage-ttls*`.
- Call `storage-refresh-ttl-batch` with a TTL > current expiration.
- Assert count equals number of valid keys and TTLs updated.

2) **Batch refresh (empty list)**
- Pass NIL keys list.
- Assert return count is 0.

3) **Batch refresh (partial/mixed)**
- One valid key, one expired key, one missing key.
- Assert count only includes valid non-expired keys.

4) **Batch refresh error path**
- Extend `failing-storage` with a `storage-refresh-ttl-batch` method that errors.
- Assert error is caught and returns 0 (no throw).

5) **refresh-all-session-ownerships uses batch refresh**
- Register two sessions, call `refresh-all-session-ownerships`.
- Assert both session keys have refreshed TTLs (i.e., expiration increased).

**Acceptance criteria:**
- All tests pass in `make test-unit`.
- New tests are listed in `run-persistence-tests-internal`.

---

## Phase 2 — Hot Loop Type Declarations (Performance Hygiene)

**Goal:** Add missing type declarations for hot loops executed every tick/frame. Keep optimize policy consistent with AGENTS.md (no global safety 0; local optimize only where already proven hot).

**Target functions (confirmed missing or partial decls):**

1) `update-zone-transition` — `src/movement-transition.lisp`
- Add arg declarations: `(type game game)` `(type single-float dt)`
- Add local declarations: `world`, `players`, `transition-count`, `tile-size`, `half-w`, `half-h`, bounds, and per-loop counters as `fixnum` / `single-float`.

2) `update-npc-behavior` — `src/ai.lisp`
- Add `(type (or null player) player)`
- Preserve existing `(type npc npc)` and `(type world world)`

3) `update-npc-intent` — `src/ai.lisp`
- Add `(type (or null player) player)`
- Ensure all local numeric vars are `single-float`

4) `update-npc-movement` — `src/ai.lisp`
- Add `(type (or null zone-state) zone-state)`
- Add explicit local types for speed, dx/dy, positions

5) `update-npc-respawns` — `src/combat.lisp`
- Add `(type (simple-array t (*)) npcs)` `(type single-float dt)` `(type (or null zone-state) zone-state)`
- Declare `npc-grid` and timers as `single-float`

6) `update-npc-attack` — `src/combat.lisp`
- Add `(type player player)` and `(type combat-event-queue event-queue)`
- Ensure local distances and timers are `single-float`

**Rule:** Add declarations in `let*` or `loop` blocks for computed locals, not just arguments.

**Acceptance criteria:**
- `make ci` shows no new type warnings.
- Hot loops now have explicit type declarations for all numeric locals.

---

## Phase 3 — Remove Per‑Tick Allocations in Event Serialization

**Goal:** Eliminate per‑tick list + plist allocations in snapshot event serialization (server hot path), while preserving wire format.

**Chosen approach (non‑optional):**
- Reuse **both** plist objects and list cons cells.
- Keep `:events` wire format as a list (no protocol change).

**Implementation steps:**

1) Add a reusable event plist pool and cons buffer
- Store buffers as **server‑local lexicals** in `server-main-loop` (avoid new globals).
- Preallocate N entries (e.g., 64). If more events arrive, grow buffers once and reuse.

2) Add `combat-event->plist-into` helper in `src/net-protocol.lisp`
- Input: `event`, `plist`
- Mutate existing plist keys `:type` and `:text` (safe: plist is a local variable).
- Keep `combat-event->plist` as a wrapper that allocates for non‑pooled uses.

3) Add `events-to-plists-list` helper in `src/net-server.lisp`
- Inputs: event list, plist buffer, cons buffer
- Output: list head + count
- Procedure:
  - Fill plists in buffer via `combat-event->plist-into`
  - Set cons car to plist, wire cdrs for used length, terminate with NIL

4) Replace `mapcar` with pooled list
- In snapshot send path, replace:
  - `(mapcar #'combat-event->plist events)`
  - with `(events-to-plists-list events plist-buffer cons-buffer)`

**Acceptance criteria:**
- Snapshot send path no longer allocates per tick (steady state).
- Wire format remains identical (`:events` is still a list).
- No behavior change in client event display.

---

## Phase 4 — Standards Guardrails (No Ambiguity)

These are enforcement rules for all future work under this plan:

1) **File size split rule:** If any file exceeds ~1500 LOC or becomes multi‑domain, split it and add matching docs entry.
2) **No CLOS in hot loops:** Inner tick loops must remain direct calls to structs/arrays.
3) **No new globals unless config/server state:** Buffers and scratch storage must be local or struct‑owned.
4) **No unsafe `(setf (getf ...))` on non‑places:** Use locals or return updated plist.
5) **Untrusted input:** Keep `*read-eval* nil` for all network/storage reads.

---

## Verification (Required Order)

1) `make checkparens`
2) `make ci`
3) `make smoke`
4) `make test-unit`
5) `make checkdocs`

**Pass condition:** All targets succeed with no new warnings.

---

## Non‑Goals (Explicitly Out of Scope)

- SIMD optimization (deferred per AGENTS.md)
- Full protocol rewrite or binary‑only enforcement
- Large refactors unrelated to the three gaps

---

## Deliverables Checklist

- [ ] TTL batch refresh tests added and registered
- [ ] Hot loop type declarations added in listed functions
- [ ] Event serialization no longer allocates per tick (steady state)
- [ ] All tests pass in required order
