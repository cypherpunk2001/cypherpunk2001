# PLAN: Seamless Zone Loading

**Source:** `findings_loading_zones.md` (consolidated analysis)
**Scope:** Phase 1 only (Options B + D + F). Phase 2/3 deferred to future plans.

---

## Goal

Eliminate visible "loading..." screens for cached zone transitions, and reduce transition frequency at zone boundaries by 60-80%. After Phase 1, a player running through previously-visited zones should experience near-instant (<50ms) transitions with no loading overlay. The 4-zone intersection circle problem should be fully resolved.

---

## Phase 1 Steps

### Step 1: Add transition cooldown to server

**What:** After `transition-zone` executes, record a timestamp on the player. `update-zone-transition` skips any player whose cooldown hasn't expired.

**Files:**
- `src/types.lisp` — Add `zone-transition-cooldown` field to player struct (single-float, default 0.0). This is ephemeral (not saved to DB).
- `src/movement.lisp:1298` (`update-zone-transition`) — At the top of the per-player loop, check `(> (player-zone-transition-cooldown player) 0.0)` and skip if true. Decrement by dt each tick (or set to absolute time and compare with game clock).
- `src/movement.lisp:1127` (`transition-zone`) — After successful transition, set `(setf (player-zone-transition-cooldown player) *zone-transition-cooldown-seconds*)`.
- `src/config.lisp` — Add `*zone-transition-cooldown-seconds*` (default 1.5).

**Tests:**
- Unit test: player with cooldown > 0 is skipped by transition check.
- Unit test: cooldown decrements correctly per tick.
- Unit test: cooldown is set after transition-zone completes.

**No migration needed** — ephemeral field, not persisted.

---

### Step 2: Add asymmetric hysteresis band

**What:** Instead of transitioning when the player reaches the exact zone edge, pull the trigger point *inward* so the transition fires when the player is still N tiles inside the current zone. The player never leaves valid collision space — the handoff happens early, while they're still within the current zone's wall-map. Use asymmetric thresholds: a larger inward margin to enter the new zone (harder to trigger), and a smaller margin to return (easier to cancel/re-trigger the old zone).

**Spatial model (critical — all code must follow this):**
```
  Zone interior          Hysteresis band       Edge (wall-map boundary)
  |                      |<-- in threshold -->|
  |    player runs  -->  |   [trigger fires]  |  (player is HERE, still inside zone)
  |                      |                    |
  |              |<-out->|                    |
  |              [cancel threshold]           |
```
There are two lines: the **arm line** (in-threshold, further from edge) and the **commit line** (the zone edge itself). The **cancel line** (out-threshold, between arm and interior) determines when a pending transition is cancelled.

```
  Zone interior       Cancel line    Arm line         Edge
  |                   |              |                |
  |                   |<- out-thres->|<-- in-thres -->|
  |                   |              |                |
  |  player runs -->  |         [arm]|       [commit] |
  |          <-- retreats past cancel = cancel        |
```

- **Arm line** (in-threshold, default 4 tiles inward from edge): When the player crosses this line moving toward the edge, `zone-transition-pending` is set. No transition yet.
- **Commit line** (the zone edge itself): When a player with a pending transition reaches the zone edge (existing `y <= min-y` check), `transition-zone` fires. The player never crosses the boundary — handoff happens at the edge and `find-open-position-with-map` places them in the target zone.
- **Cancel line** (out-threshold, default 2 tiles inward from edge, i.e. further from edge than arm line): If the player retreats past this line, pending is cleared. Because out-threshold < in-threshold, there's a dead band between cancel and arm that prevents rapid re-arming.

The player **never leaves valid collision space**. The arm/cancel lines exist within the zone interior; the commit line is the existing zone boundary.

**Files:**
- `src/config.lisp` — Add two parameters:
  - `*zone-hysteresis-in*` (default 4.0 tiles, in pixels = `4.0 * tile-size`) — inward distance from edge where pending is armed.
  - `*zone-hysteresis-out*` (default 2.0 tiles) — inward distance from edge where pending is cancelled (must be > in-threshold, i.e. further from the edge).
- `src/types.lisp` — Add `zone-transition-pending` field to player struct (keyword or nil, e.g. `:east`). Tracks which edge the player is approaching. Ephemeral.
- `src/movement.lisp:799` (`world-exit-edge-with-bounds`) — Add a second detection mode for the arm line. For the north edge: arm when `y <= (+ min-y hysteresis-in-px)` (N tiles inside the zone). Similarly for other edges. The existing edge check (`y <= min-y`) remains unchanged as the commit line.
- `src/movement.lisp:1298` (`update-zone-transition`) — Two-phase state machine:
  - **No pending + player crosses arm line** → set `zone-transition-pending` to that edge. Do NOT transition.
  - **Pending + player reaches commit line (zone edge)** → call `transition-zone`, clear pending.
  - **Pending + player retreats past cancel line** → clear pending (player reversed). Cancel line is at `(+ min-y hysteresis-out-px)` for north edge — further from edge than the arm line, creating a dead band.
  - **Pending + edge changes** → clear pending (player changed direction).
  - **Pending + intent drops below `*zone-direction-threshold*`** → clear pending.

**Implementation note:** The hysteresis values are in *tiles* but the edge detection works in *pixels*. Convert at the point of comparison: `(* *zone-hysteresis-in* (float *tile-size*))`. Assert at config load time that `*zone-hysteresis-out*` > `*zone-hysteresis-in*` (cancel line is further from edge than arm line).

**Corner / 4-way intersection handling:** `world-exit-edge-with-bounds` can detect two edges simultaneously at corners (e.g., player at northeast corner has both north and east candidates). The existing code picks the edge with the highest movement-weight (strongest intent component). The `zone-transition-pending` field locks to a single edge once armed. Resolution rules:
1. If no pending edge → pick edge with highest directional weight (existing behavior). Arm as pending.
2. If pending edge is set → only that edge is evaluated for commit. Other edges are ignored until pending is cleared.
3. Pending is cleared when: (a) player retreats past the cancel line, (b) player's intent toward the pending edge drops below `*zone-direction-threshold*`, or (c) the transition commits.
This prevents corner thrash — once the player is armed toward an edge, they must either commit at the zone edge or fully retreat before a different edge can be considered.

**Tests:**
- Unit test: player in zone interior (outside arm line) → no pending, no transition.
- Unit test: player crosses arm line toward edge → pending is set, no transition yet.
- Unit test: player with pending reaches zone edge → transition fires, pending cleared.
- Unit test: player crosses arm line then retreats past cancel line → pending cleared, no transition.
- Unit test: player crosses arm line, retreats into dead band (between cancel and arm) → pending still set (not yet cancelled).
- Unit test: asymmetry — cancel line is further from edge than arm line.
- Unit test: at corner with two edges, pending locks to dominant edge; other edge ignored.
- Unit test: pending edge cleared when intent drops below direction threshold.

---

### Step 3: Add directional gating

**What:** Only trigger transitions when the player's movement intent has a significant component toward the new zone. This prevents tangential movement along a boundary from firing transitions.

**Files:**
- `src/config.lisp` — Add `*zone-direction-threshold*` (default 0.3, range 0.0-1.0). This is the minimum normalized dot product between movement intent and edge normal required to trigger.
- `src/movement.lisp:799` (`world-exit-edge-with-bounds`) — Already partially does this (checks `dx > 0.0` for east edge, etc.), but the check is binary. Strengthen it: compute the dot product of `(player-dx, player-dy)` with the edge normal, normalize by movement magnitude, and reject if below threshold. For example, for the east edge: `(/ dx (max 0.001 (sqrt (+ (* dx dx) (* dy dy)))))` must be >= `*zone-direction-threshold*`.

**Zero-vector fallback:** If movement magnitude is below a minimum epsilon (e.g., `(< (+ (abs dx) (abs dy)) 0.001)`), treat it as "no transition" — return nil immediately. A stationary or near-stationary player should never trigger a zone transition. This avoids division-by-near-zero instability in the dot product normalization.

**Tests:**
- Unit test: player moving due east at east edge → transition allowed.
- Unit test: player moving northeast at east edge (45°) → allowed (dot ~0.707 > 0.3).
- Unit test: player moving mostly north along east edge (10° off north) → rejected (dot ~0.17 < 0.3).
- Unit test: player with zero movement vector at edge → no transition (not a division error).

---

### Step 4: Client-side zone cache (LRU)

**What:** The client currently reloads zones from disk on every transition (`apply-game-state` calls `load-zone` on every zone-id change). Add an LRU cache so previously-visited zones are served from memory.

**Files:**
- `src/types.lisp` — Add a `zone-cache` struct:
  ```lisp
  (defstruct zone-cache
    (entries (make-hash-table :test 'eq :size 16) :type hash-table)  ; zone-id → zone
    (order nil :type list)    ; LRU order, most recent first
    (capacity 9 :type fixnum)) ; current + 8 neighbors
  ```
- `src/config.lisp` — Add `*client-zone-cache-capacity*` (default 9). **Sizing rationale:** Each zone struct holds a 64x64 tile grid with chunked layers (8x8 chunks), a collision hash table, and object/spawn lists. Estimated memory per zone: ~50-100KB (tile data dominates — 4096 tiles * ~16 bytes per tile per layer * 2-3 layers). At capacity 9, worst-case memory is ~900KB — negligible on any modern system. The capacity is a config parameter so it can be tuned up (e.g., 16-25 for larger worlds) or down (for memory-constrained clients) without code changes.
- `src/zone.lisp` — Add `zone-cache-lookup` and `zone-cache-insert` functions:
  - `zone-cache-lookup (cache zone-id)` → zone struct or nil. Moves zone-id to front of LRU order.
  - `zone-cache-insert (cache zone-id zone)` → stores zone, evicts LRU entry if at capacity.
- `src/save.lisp:1685` (`apply-game-state`) — Before calling `(load-zone path)`, check the cache:
  ```lisp
  (let ((cached (and *client-zone-cache* (zone-cache-lookup *client-zone-cache* zone-id))))
    (if cached
        (progn
          (apply-zone-to-world world cached)
          (when *client-zone-change-hook*
            (funcall *client-zone-change-hook* (zone-id cached)))
          (setf zone-loaded t))
        ;; existing load-zone path
        (let ((zone (load-zone path)))
          (when zone
            (zone-cache-insert *client-zone-cache* zone-id zone)
            (apply-zone-to-world world zone)
            ...))))
  ```
- `src/main.lisp` — Initialize `*client-zone-cache*` at client startup (in `run-client` or equivalent init).

**Tests:**
- Unit test: insert + lookup returns the zone.
- Unit test: LRU eviction at capacity — oldest entry is evicted.
- Unit test: lookup moves entry to front of LRU.
- Unit test: cache miss returns nil.

---

### Step 5: Client-side adjacent zone preloading

**What:** When the client enters a new zone, preload all adjacent zones (from world-graph edges) into the cache. This ensures the next transition is a cache hit.

**Files:**
- `src/zone.lisp` or `src/net.lisp` — Add `preload-adjacent-zones (world-graph zone-id cache)`:
  1. Look up edges for `zone-id` in the world-graph.
  2. For each neighboring zone-id, if not already in cache, call `load-zone` and insert.
  3. Also preload diagonal neighbors (zones that share a corner but not an edge) by looking up neighbors-of-neighbors. **Filter by spatial adjacency only** — skip any edge that represents a teleport or non-spatial link (e.g., dungeon entrances). Only preload zones whose edges are cardinal (`:north`, `:south`, `:east`, `:west`) with `:offset` of `:preserve-x` or `:preserve-y`. This avoids wasting I/O on teleport destinations the player is unlikely to visit.
- `src/main.lisp:422` (`handle-zone-transition`) — After zone transition completes, call `preload-adjacent-zones`. This runs after the current zone is loaded, so it doesn't block the transition itself.

**Performance note:** Preloading 4-8 zones means 4-8 file reads. Each is 100-200ms. To avoid frame hitches, this should NOT happen in the render thread's hot path. Two options:
- (a) Spread across frames: preload one zone per frame over 4-8 frames.
- (b) Accept a one-time hit since it only happens on zone entry (not per-tick).

Option (a) is cleaner. Add a `preload-queue` (list of zone-ids to load) and process one per frame in the client update loop.

**Files (for frame-spread approach):**
- `src/types.lisp` — Add `preload-queue` field to the game or world struct (list of `(zone-id . path)` pairs).
- `src/main.lisp` (client update loop) — Each frame, if `preload-queue` is non-empty, pop one entry, call `load-zone`, insert into cache.

**Tests:**
- Unit test: after preloading, all adjacent zone-ids are in cache.
- Unit test: diagonal neighbors are included.
- Unit test: already-cached zones are not re-loaded.

---

### Step 6: Fast-path transition (skip loading overlay)

**What:** When a zone transition hits the client cache, skip the "loading..." overlay. Only show it on cache misses (cold transitions).

**Files:**
- `src/save.lisp:1685` (`apply-game-state`) — Return an additional value indicating whether the zone was loaded from cache or disk. Change return to `(values zone-id zone-loaded cache-hit-p)`.
- `src/net.lisp` (`apply-snapshot`) — Pass `cache-hit-p` through to `handle-zone-transition`.
- `src/main.lisp:422` (`handle-zone-transition`) — Accept a `cache-hit-p` parameter. If cache hit:
  - Skip `(ui-trigger-loading ui)`.
  - Still reset interpolation/prediction buffers (these must always reset — stale positions from the old zone are invalid).
  - Still call `sync-client-zone-npcs`.

**Tests:**
- Unit test: `handle-zone-transition` with `cache-hit-p t` does NOT call `ui-trigger-loading`. Extract the loading-overlay decision into a pure predicate `zone-transition-show-loading-p (cache-hit-p)` that returns nil on cache hit, t on miss. Unit test the predicate directly — no UI/rendering dependency.
- Unit test: `handle-zone-transition` with `cache-hit-p nil` DOES call `ui-trigger-loading`.
- Visual validation (manual, not CI): confirm cached transition shows no overlay, cold transition shows overlay.

---

### Step 7: Downgrade zone-transition DB save from Tier-1 to Tier-2

**What:** The current Tier-1 immediate save in `transition-zone` (100-500ms with retry) blocks the server tick. Zone transitions are not as critical as death/trade/level-up — the player's position is already covered by the Tier-2 dirty-flag batch system. Downgrade to `mark-player-dirty`.

**Files:**
- `src/movement.lisp:1247` (`transition-zone`) — Replace:
  ```lisp
  (with-retry-exponential (saved (lambda () (db-save-player-immediate player)) ...)
    saved)
  ```
  With:
  ```lisp
  (mark-player-dirty (player-id player))
  ```
  The dirty-flag system will flush within the next 30-second checkpoint.

**Risk assessment:** If the server crashes between a zone transition and the next checkpoint, the player reverts to their pre-transition zone. This is acceptable — they just re-enter the zone on reconnect. Compare with death (gold/XP loss) or trade (item duplication) where Tier-1 is genuinely critical.

**Tests:**
- Unit test: after transition-zone, player is marked dirty (not immediately saved).
- Existing persistence tests should still pass.

---

### Step 8: Add evaluation metrics

**What:** Instrument the transition system to measure the metrics from the findings doc, so we can validate Phase 1 improvements.

**Files:**
- `src/config.lisp` — Add `*verbose-zone-transitions*` flag (default nil, enabled by environment variable `MMORPG_VERBOSE_ZONES=1`).
- `src/movement.lisp` (`update-zone-transition`) — When verbose, log:
  - Transition count per tick.
  - Time since last transition for this player (thrash detection).
  - Whether cooldown suppressed a transition.
  - Whether hysteresis prevented a transition.
  - Whether directional gating rejected a transition.
- `src/main.lisp` (`handle-zone-transition`) — When verbose, log:
  - Cache hit vs miss.
  - Wall-clock time of transition (from snapshot receipt to gameplay resume).
  - Whether loading overlay was shown.

**Tests:** No unit tests needed for logging — validated by manual inspection with `MMORPG_VERBOSE_ZONES=1`.

---

### Step 9: Run all tests and validate

**What:** Run the full test suite to ensure nothing is broken.

```bash
make tests
```

All existing tests must pass. New tests from Steps 1-7 must pass.

---

## File Change Summary

| File | Changes |
|------|---------|
| `src/types.lisp` | Add `zone-transition-cooldown`, `zone-transition-pending` to player. Add `zone-cache` struct. Add `preload-queue` to game/world. |
| `src/config.lisp` | Add `*zone-transition-cooldown-seconds*`, `*zone-hysteresis-in*`, `*zone-hysteresis-out*`, `*zone-direction-threshold*`, `*client-zone-cache-capacity*`, `*verbose-zone-transitions*`. |
| `src/movement.lisp` | Modify `world-exit-edge-with-bounds` (hysteresis + directional gating). Modify `update-zone-transition` (cooldown check, hysteresis state machine). Modify `transition-zone` (set cooldown, downgrade save to Tier-2). |
| `src/zone.lisp` | Add `zone-cache-lookup`, `zone-cache-insert`. |
| `src/save.lisp` | Modify `apply-game-state` (check cache before load-zone, return cache-hit-p). |
| `src/main.lisp` | Modify `handle-zone-transition` (skip loading on cache hit, trigger preload). Add per-frame preload processing. Initialize `*client-zone-cache*`. |
| `src/net.lisp` | Pass cache-hit-p from `apply-snapshot` to `handle-zone-transition`. |
| `src/ui.lisp` | No changes (loading overlay logic stays, just called less often). |
| `tests/unit-test.lisp` | Add tests for cooldown, hysteresis, directional gating, zone cache LRU, preloading, fast-path transition, Tier-2 save downgrade. |

---

## Dependency Order

Steps must be implemented in this order due to dependencies:

```
Step 1 (cooldown)           ─── independent
Step 2 (hysteresis)         ─── independent (but builds on same area as Step 1)
Step 3 (directional gating) ─── independent (modifies same function as Step 2)
Step 4 (zone cache)         ─── independent
Step 5 (preloading)         ─── depends on Step 4 (needs cache to insert into)
Step 6 (fast-path)          ─── depends on Step 4 (needs cache-hit detection)
Step 7 (Tier-2 downgrade)   ─── independent
Step 8 (metrics)            ─── depends on Steps 1-3 (logs their decisions)
Step 9 (tests)              ─── depends on all above
```

Steps 1, 2, 3 can be implemented together (all in movement.lisp).
Step 4 can be done in parallel with Steps 1-3.
Steps 5 and 6 follow Step 4.
Step 7 is independent and can be done anytime.
Step 8 follows Steps 1-3.
Step 9 is always last.

---

## What This Plan Does NOT Cover (Future Phases)

- **Phase 2:** Seamless multi-zone tile rendering, crossfade, deferred handoff, interpolation buffer preservation across transitions.
- **Phase 2.5:** Intersection buffer zones (world-graph data changes).
- **Phase 3:** Larger zone dimensions, chunk-based streaming.
- **Option E:** Portal/threshold transitions (skipped per findings).
