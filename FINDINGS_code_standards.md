Code Standards Deep Dive — Findings
==================================

Scope
-----
Review against AGENTS.md:
- **Code Style Rules**
- **Code Quality Checklist (MANDATORY)**
- **SBCL Performance Directives (MMORPG / 60Hz)**

Summary
-------
Overall compliance is **good** with several areas fully aligned (global optimize
policy, untrusted-client read safety, module splitting). The main gaps are
**performance hygiene in hot loops** (type declarations and per‑tick allocations)
and **missing tests** for newer batch‑TTL paths.

---

Code Style Rules — Findings
---------------------------

1) File Size Guideline
- No source files exceed ~1500 LOC. Largest files are:
  - `src/net-auth.lisp` (~1280 LOC)
  - `src/main.lisp` (~1241 LOC)
  - `src/db-storage.lisp` (~1186 LOC)
  - `src/movement-transition.lisp` (~1172 LOC)
  - `src/rendering-tiles.lisp` (~1013 LOC)
  - `src/types.lisp` (~1002 LOC)
- Guideline says split **only** when >1500 or natural boundary exists.
  Current sizes are acceptable; no mandatory split triggered.

2) Modular & Reusable by Default
- Net stack already split as recommended: `net-protocol`, `net-auth`,
  `net-snapshot`, `net-server`, `net-client`, `net`.
- Storage abstraction is respected: direct Redis usage is confined to
  `src/db-storage.lisp` and `src/db-accounts.lisp` (no game‑logic modules
  call Redis directly).
- Data-driven layout is present: zones and world graph in `data/` and
  `data/world-graph.lisp`.

3) Plist Mutation (`setf getf`) Pitfall
- Quick scan shows `setf (getf ...)` only on **local variables** (e.g.,
  `overrides`, `snapshot`, `deserialized`) in:
  - `src/save-edge-strips.lisp`
  - `src/save-delta.lisp`
  - `src/data.lisp`
- **No unsafe `(setf (getf (foo) ...) ...)` patterns found** in these files.

4) Security: Untrusted Client Principle
- `*read-eval* nil` is consistently used during deserialization for
  network, auth, snapshots, and storage loads:
  - `src/net-protocol.lisp`, `src/net-auth.lisp`, `src/net-snapshot.lisp`
  - `src/db-storage.lisp`, `src/db-players.lisp`, `src/save.lisp`,
    `src/world-graph.lisp`
- Complies with rule: **untrusted input never evaluated**.

---

Code Quality Checklist (MANDATORY) — Findings
---------------------------------------------

1) Tests Written?
- Broad coverage exists under `tests/unit/`.
- **Gap:** no tests found for `storage-refresh-ttl-batch` or the batch
  TTL refresh path in `refresh-all-session-ownerships`.
  - Recommendation: add unit tests for batch refresh count and error path.

2) Retry Logic Added?
- Tier‑1 immediate saves and auth DB reads use `with-retry-exponential`.
  Examples:
  - `src/combat.lisp`, `src/progression.lisp`, `src/net-auth.lisp`
- Auth UDP messages use `with-retry-linear` in `src/net-protocol.lisp`.
- Zone loading uses `with-retry-exponential` in `src/movement-transition.lisp`.
- **No missing retry requirements found** for critical paths listed in AGENTS.md.

3) Logging Added?
- Critical failures use `warn` in storage and auth error paths.
- Verbose logs are gated by `*verbose*` or specific flags (zone transitions).
- **Note:** logging exists inside per‑player loops but is gated; this matches
  “no logging in hot loops unless explicitly enabled.”

4) Variable Scope Correct?
- Globals for config/server state are consistently in `config*.lisp` and
  storage/session tables.
- No clear evidence of mutable gameplay state stored as globals.

5) Data‑Driven Design Consistent?
- Zones/world graph stored in `data/` and loaded via `zone.lisp` / `world-graph.lisp`.
- No direct DB calls from gameplay modules (storage abstraction preserved).

---

SBCL Performance Directives — Findings
--------------------------------------

1) Global Optimization Policy
- **Compliant.** Environment‑driven optimize policy is centralized in
  `src/config.lisp` (dev vs prod), with explicit guidance against global
  `(safety 0)`.

2) Local Hot‑Path Optimize Decls
- **Compliant in hot helpers:**
  - `src/net-protocol.lisp` low‑level byte ops (typed, `safety 0`)
  - `src/rendering-tiles.lisp` chunk key packer (typed, `safety 0`)
  - `src/spatial.lisp` grid helpers (typed, `safety 0`)
- **Gap:** some hot loops lack explicit type declarations.
  - Example: `update-zone-transition` (`src/movement-transition.lisp:886+`)
    has no argument/loop variable declarations despite per‑tick execution.
  - Recommendation: add `declare` types for hot loop vars/accumulators
    (player, bounds, dx/dy, counters).

3) Allocation‑Free Hot Loops
- **Partial compliance.** Many loops use vectors/structs, but some per‑tick
  allocations still occur:
  - `src/net-server.lisp`: `event-plists (mapcar ...)` each snapshot tick
    allocates a fresh list.
  - Text protocol still uses `prin1` → string → octets per send.
- Recommendation: keep text protocol for dev but default to binary snapshots
  in prod; consider pooled lists for event serialization if it becomes hot.

4) Networking/Serialization Buffers
- **Compliant for snapshots** when binary mode enabled: reusable buffer
  `*binary-send-buffer*` in `src/net-protocol.lisp`.
- **Not fully compliant** for text messages and intents (string allocation
  per send). Acceptable for dev/testing; less ideal for peak load.

5) Data Layout (structs/arrays)
- **Good usage** of structs and vectors for players/NPCs and grid data.
- Some list usage remains (e.g., event lists, certain caches), but not
  demonstrably in the hottest loops.

6) GC Predictability
- **Compliant:** strategic GC scheduling is present in `src/net-server.lisp`.

7) SIMD / Auto‑vectorization
- **Compliant with “defer SIMD.”** No SIMD kernels present, which matches
  the current directive.

---

Recommendations (Ordered)
-------------------------
1) **Add missing tests** for batch TTL refresh (`storage-refresh-ttl-batch` and
   `refresh-all-session-ownerships` batch path).
2) **Type‑annotate hot loops** (start with `update-zone-transition` and any other
   per‑tick loops without declarations).
3) **Reduce per‑tick allocations** in snapshot event serialization or enable
   binary snapshots by default in production.

