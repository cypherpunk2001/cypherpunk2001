# PLAN: Seamless Zone Loading

**Source:** `findings_loading_zones.md` (consolidated analysis)
**Scope:** Full seamless experience — zero loading screens, truthful cross-zone visibility, zone architecture preserved.

---

## Requirements

1. **Zero loading screens** — no "loading..." overlay ever, including first entry to any zone.
2. **Boundary invisibility** — as a player approaches an edge, they see truthful content from the adjacent zone: terrain, players, NPCs, items/objects. No blind boundaries.
3. **Preserve zone architecture** — server authority, per-zone simulation, horizontal scaling (one process per zone).
4. **Continuous gameplay** — the world feels seamless while zones remain the unit of design, content, and tooling.

---

## Spatial Model (all steps reference this)

All distances measured inward from the zone edge. Larger value = further from edge = deeper into zone interior.

```
Zone interior   Cancel(8t)  Arm(6t)  Preload/AOI(10t from edge)     Edge(0t)
|               |           |        |                              |
|               |<-dead 2t->|<---------- arm-to-edge (6t) -------->|
|               |           |        |<-- edge strip (10t) ------->|
|               |           |        |                              |
| player runs ->|      [arm]|        | [see adjacent zone here]    |[commit]
|       <-- retreat past cancel = pending cleared                   |
```

- **Edge-strip region** (`*zone-edge-visibility-tiles*`, default 10): width of the cross-zone visibility band. Server streams adjacent-zone entities in this strip. Client renders adjacent-zone tiles in this strip. Measured from the edge into the adjacent zone.
- **Preload trigger** (`*zone-preload-radius*`, default 10 tiles from edge = same as arm band entry): when the player is this close to an edge, client begins preloading adjacent zone terrain data into cache.
- **Arm line** (`*zone-hysteresis-in*`, default 6 tiles from edge): crossing this toward the edge sets `zone-transition-pending`. No transition yet.
- **Cancel line** (`*zone-hysteresis-out*`, default 8 tiles from edge): retreating past this clears pending. Dead band (2 tiles) between cancel and arm prevents flicker.
- **Commit line** (the zone edge itself, 0 tiles): player with pending reaches edge → `transition-zone` fires.

---

## Phase 1 Steps

### Step 1: Add transition cooldown to server

**What:** After `transition-zone` executes, record a timestamp on the player. `update-zone-transition` skips any player whose cooldown hasn't expired.

**Files:**
- `src/types.lisp` — Add `zone-transition-cooldown` field to player struct (single-float, default 0.0). Ephemeral (not saved to DB).
- `src/movement.lisp:1298` (`update-zone-transition`) — At the top of the per-player loop, check `(> (player-zone-transition-cooldown player) 0.0)` and skip if true. Decrement by dt each tick (or set to absolute time and compare with game clock).
- `src/movement.lisp:1127` (`transition-zone`) — After successful transition, set `(setf (player-zone-transition-cooldown player) *zone-transition-cooldown-seconds*)`.
- `src/config.lisp` — Add `*zone-transition-cooldown-seconds*` (default 1.5).

**Tests:**
- Unit test: player with cooldown > 0 is skipped by transition check.
- Unit test: cooldown decrements correctly per tick.
- Unit test: cooldown is set after transition-zone completes.

**No migration needed** — ephemeral field, not persisted.

---

### Step 2: Add asymmetric hysteresis band (arm/commit/cancel)

**What:** Two-phase arm/commit model. The system arms a pending transition when the player enters the arm band, commits only when they reach the zone edge. If they retreat past the cancel line, pending is cleared.

**Spatial model:** See top-level diagram. Arm at 6 tiles, cancel at 8 tiles, commit at edge (0 tiles).

**Files:**
- `src/config.lisp` — Add two parameters (both measured in tiles inward from edge; larger = further from edge):
  - `*zone-hysteresis-in*` (default 6.0) — arm line distance.
  - `*zone-hysteresis-out*` (default 8.0) — cancel line distance. Must be > `*zone-hysteresis-in*`.
- `src/types.lisp` — Add `zone-transition-pending` field to player struct (keyword or nil, e.g. `:east`). Ephemeral.
- `src/movement.lisp:799` (`world-exit-edge-with-bounds`) — Add detection for the arm line. For the north edge: arm when `y <= (+ min-y hysteresis-in-px)`. The existing edge check (`y <= min-y`) remains unchanged as the commit line.
- `src/movement.lisp:1298` (`update-zone-transition`) — Two-phase state machine:
  - **No pending + player crosses arm line** → set `zone-transition-pending` to that edge. Do NOT transition.
  - **Pending + player reaches commit line (zone edge)** → call `transition-zone`, clear pending.
  - **Pending + player retreats past cancel line** → clear pending. Cancel line for north edge is at `(+ min-y hysteresis-out-px)`.
  - **Pending + edge changes** → clear pending (player changed direction).
  - **Pending + intent drops below `*zone-direction-threshold*`** → clear pending.

**Implementation note:** Convert tiles to pixels at point of comparison: `(* *zone-hysteresis-in* (float *tile-size*))`. Assert at config load that `*zone-hysteresis-out*` > `*zone-hysteresis-in*`.

**Corner / 4-way intersection handling:** `zone-transition-pending` locks to a single edge once armed:
1. No pending → pick edge with highest directional weight. Arm as pending.
2. Pending set → only that edge evaluated for commit. Others ignored.
3. Pending cleared when: (a) retreat past cancel line, (b) intent drops below direction threshold, or (c) transition commits.

**Tests:**
- Unit test: player in zone interior (outside arm line) → no pending, no transition.
- Unit test: player crosses arm line toward edge → pending is set, no transition yet.
- Unit test: player with pending reaches zone edge → transition fires, pending cleared.
- Unit test: player crosses arm line then retreats past cancel line → pending cleared, no transition.
- Unit test: player in dead band (between cancel and arm) → pending still set.
- Unit test: `*zone-hysteresis-out*` (8) > `*zone-hysteresis-in*` (6).
- Unit test: at corner, pending locks to dominant edge; other edge ignored.
- Unit test: pending cleared when intent drops below direction threshold.

---

### Step 3: Add directional gating

**What:** Only trigger arm/transitions when the player's movement intent has a significant component toward the zone edge. Prevents tangential movement along a boundary from arming.

**Files:**
- `src/config.lisp` — Add `*zone-direction-threshold*` (default 0.3, range 0.0-1.0). Minimum normalized dot product between movement intent and edge normal.
- `src/movement.lisp:799` (`world-exit-edge-with-bounds`) — Compute dot product of `(player-dx, player-dy)` with edge normal, normalize by movement magnitude: `(/ dx (max 0.001 (sqrt (+ (* dx dx) (* dy dy)))))` must be >= `*zone-direction-threshold*`.

**Zero-vector fallback:** If movement magnitude is below epsilon (e.g., `(< (+ (abs dx) (abs dy)) 0.001)`), return nil immediately. Stationary players never trigger transitions.

**Tests:**
- Unit test: moving due east at east edge → allowed.
- Unit test: moving northeast (45°) → allowed (dot ~0.707 > 0.3).
- Unit test: moving mostly north along east edge (10° off) → rejected (dot ~0.17 < 0.3).
- Unit test: zero movement vector → no transition, no division error.

---

### Step 4: Client-side zone cache (LRU)

**What:** Add an LRU cache so zone data is served from memory. Eliminates disk I/O from the transition path.

**Files:**
- `src/types.lisp` — Add `zone-cache` struct:
  ```lisp
  (defstruct zone-cache
    (entries (make-hash-table :test 'eq :size 16) :type hash-table)
    (order nil :type list)
    (capacity 9 :type fixnum))
  ```
- `src/config.lisp` — Add `*client-zone-cache-capacity*` (default 9). **Sizing:** ~50-100KB per zone (4096 tiles * ~16 bytes * 2-3 layers + collision + objects). 9 zones ≈ ~900KB. Tunable via config.
- `src/zone.lisp` — Add `zone-cache-lookup (cache zone-id)` → zone or nil (moves to LRU front). Add `zone-cache-insert (cache zone-id zone)` → stores, evicts LRU tail if full.
- `src/save.lisp:1685` (`apply-game-state`) — Check cache before `load-zone`. On miss, load from disk and insert into cache.
- `src/main.lisp` — Initialize `*client-zone-cache*` at client startup.

**Tests:**
- Unit test: insert + lookup returns zone.
- Unit test: LRU eviction at capacity.
- Unit test: lookup promotes to front.
- Unit test: cache miss returns nil.

---

### Step 5: Arm-triggered preloading (zero first-entry loading)

**What:** When the hysteresis arm fires (Step 2), immediately preload the target zone into the client cache. By the time the player reaches the commit line (zone edge), the zone is already cached. This eliminates loading screens even on first entry to a zone.

This replaces the old "preload after transition" design. Preloading now happens *before* the transition, triggered by the arm event.

**Files:**
- `src/config.lisp` — Add `*zone-preload-radius*` (default 10.0 tiles from edge). When the player is within this distance of any edge, queue preloading for that edge's target zone. This is >= `*zone-hysteresis-in*` so preloading starts at or before arming.
- `src/types.lisp` — Add `preload-queue` field to game struct (list of `(zone-id . path)` pairs).
- `src/movement.lisp:1298` (`update-zone-transition`) — When arm fires (pending set), also check if target zone is in client cache. If not, add to `preload-queue`. Additionally, when the player enters the preload radius on *any* edge (even without arming — e.g., walking parallel near an edge), queue that edge's neighbor for preload.
- `src/main.lisp` (client update loop) — Each frame, if `preload-queue` is non-empty, pop one entry, call `load-zone`, insert into cache. One zone per frame to avoid hitches.
- `src/main.lisp` — On initial client startup / first zone load, also queue all adjacent zones immediately (cold start preload).

**Preload scope:** Filter by spatial adjacency only — cardinal edges with `:preserve-x`/`:preserve-y` offset. Skip teleports/dungeon entrances. Include diagonal neighbors (neighbors-of-neighbors) for corner coverage.

**Tests:**
- Unit test: arm event queues target zone for preload.
- Unit test: entering preload radius without arming still queues neighbor.
- Unit test: already-cached zones not re-queued.
- Unit test: diagonal neighbors included in initial preload.
- Unit test: teleport edges filtered out.

---

### Step 6: Remove loading overlay entirely

**What:** The loading overlay is no longer needed. With arm-triggered preloading (Step 5), zones are always cached before the player reaches the commit line. Remove the overlay unconditionally — `ui-trigger-loading` is never called during zone transitions.

**Files:**
- `src/main.lisp:422` (`handle-zone-transition`) — Remove the call to `(ui-trigger-loading ui)` entirely. Keep buffer resets and NPC sync.
- `src/ui.lisp` — `ui-trigger-loading` and `update-ui-loading` remain in codebase (may be used for other purposes like teleports or initial login), but are not called from zone transitions.
- `src/save.lisp:1685` (`apply-game-state`) — No need to return `cache-hit-p`. Zone transition path always uses cache (preloaded by Step 5). If cache miss occurs (edge case: preload failed), load synchronously from disk but still do not show overlay — the load is fast enough (<200ms) to absorb without UX disruption.

**Predicate for testing:** Extract `zone-transition-show-loading-p` that always returns nil. Unit-testable without UI.

**Tests:**
- Unit test: `zone-transition-show-loading-p` always returns nil (no loading overlay ever).
- Unit test: `handle-zone-transition` does NOT call `ui-trigger-loading` under any circumstance.
- Visual validation (manual): walk through multiple zone transitions, including first-visit zones — no overlay.

---

### Step 7: Server-side edge-strip streaming (boundary AOI)

**What:** When a player is within `*zone-edge-visibility-tiles*` of a zone edge, the server includes a spatially-filtered snapshot of the adjacent zone in their snapshot. This makes boundaries truthful — the player sees the real world across the boundary.

**Design principle: type-agnostic edge data.** The edge strip is NOT a separate per-type system (no `:edge-players`, `:edge-npcs`, `:edge-objects`, etc.). Instead, the server builds a mini-snapshot of the adjacent zone using the *same serialization path* it already uses for normal zone snapshots, just spatially filtered to the strip. Any entity type that appears in a normal snapshot automatically appears in the edge strip. When new entity types are added in the future (projectiles, pets, vehicles, etc.), they appear in edge strips for free — no new edge-specific code needed.

**Spatial model:** The edge strip extends `*zone-edge-visibility-tiles*` (default 10) tiles into the adjacent zone from the shared boundary. Only entities within this strip are included. This is NOT full-zone streaming — it's a bounded AOI extension.

**Files:**
- `src/config.lisp` — Add `*zone-edge-visibility-tiles*` (default 10.0). Width of cross-zone entity strip.
- `src/save.lisp` — Add `serialize-edge-strip (zone-state edge strip-width-px)`:
  1. Determine the spatial bounds of the strip within the adjacent zone (e.g., for a `:south` strip of an adjacent zone to the north: `y >= (zone-height - strip-width)` in that zone's local coords).
  2. Call the *existing* per-zone serialization logic but with a spatial bounding box filter. Reuse `serialize-game-state-for-zone` (or extract its core into a shared helper) with an additional `:bounds` parameter that filters entities by position.
  3. Return a plist in the same format as a normal zone snapshot (`:players`, `:npcs`, `:objects`, etc.) — just spatially clipped.
- `src/save.lisp:1280` (`serialize-game-state-for-zone`) — After building the main zone's snapshot, check if the receiving player is within `*zone-edge-visibility-tiles*` of any edge. For each such edge:
  1. Look up adjacent zone-state via world-graph.
  2. Call `serialize-edge-strip` for that adjacent zone-state.
  3. Append the result to the snapshot under a single `:edge-strips` key — a list of `(:edge :north :zone-id :zone-X :strip <mini-snapshot>)` entries.
- `src/save.lisp:1401` (delta variant) — Same logic. Delta edge strips only include entities that changed.

**Snapshot structure (extended):**
```lisp
(:format :compact-v5
 :zone-id :zone-1
 :players #(...)          ; main zone entities (unchanged)
 :npcs #(...)
 :objects (...)
 :edge-strips (           ; NEW — list of adjacent zone strips
   (:edge :north :zone-id :zone-2 :strip (:players #(...) :npcs #(...) :objects (...)))
   (:edge :east  :zone-id :zone-5 :strip (:players #(...) :npcs #(...) :objects (...)))))
```

Each `:strip` is the same format as a normal zone snapshot. Any future entity type added to normal snapshots (projectiles, pets, etc.) automatically appears in edge strips because they share the same serialization path.

**Per-edge check (server, per player per tick):**
```
for each edge in (:north :south :east :west):
  if player is within *zone-edge-visibility-tiles* of that edge:
    adjacent-zone-state = lookup from world-graph
    if adjacent-zone-state exists:
      strip = serialize-edge-strip(adjacent-zone-state, opposite-edge, strip-width)
      append (:edge edge :zone-id adj-id :strip strip) to edge-strips
```

**Performance bounds:**
- Only triggered for players near edges (most players in zone interior — no cost).
- Spatial grid queries are O(cells-in-strip), not O(all-entities).
- Edge-strip data is small: ~10 tiles wide × zone-height × entity-density. Typical: 0-5 extra entities per edge.
- Bandwidth: ~20-80 extra bytes per edge entity per snapshot. Negligible at 20-30 Hz snapshot rate.
- Serialization reuse: no new serialization code per entity type — the existing path handles everything.

**Tests:**
- Unit test: player in zone interior → no edge strips in snapshot.
- Unit test: player near north edge → snapshot includes strip from adjacent zone's south band.
- Unit test: edge-strip entities are spatially filtered — only those within strip width, not entire adjacent zone.
- Unit test: edge strip from non-existent adjacent zone (world boundary) → no strip entry, no error.
- Unit test: edge-strip entity count is bounded (AOI limit respected).
- Unit test: adding a new entity type to normal snapshot automatically appears in edge strip (no edge-specific code).

---

### Step 8: Client-side edge-strip reception and storage

**What:** The client receives `:edge-strips` from snapshots and stores them as a list of mini-snapshots. These are render-only — no interaction, no intent processing, no stat updates. The storage is type-agnostic: whatever entity types the strip contains are stored and rendered generically.

**Files:**
- `src/types.lisp` (game struct) — Add one field:
  ```lisp
  (edge-strips nil :type list)
  ;; List of (:edge <kw> :zone-id <kw> :offset-x <float> :offset-y <float> :strip <deserialized-snapshot>)
  ```
  Each entry contains the edge direction, source zone-id, computed world-space offset for rendering, and the deserialized entity data (same struct types as main zone — player/NPC/object vectors).
- `src/save.lisp:1331` (`deserialize-game-state-compact`) — After processing main zone data, process `:edge-strips`. For each strip entry:
  1. Deserialize `:strip` using the *same* deserialization path as a normal snapshot (reuse existing compact/delta deserializer).
  2. Compute the world-space offset for this edge using `preview-zone-offset` logic.
  3. Store in `game-edge-strips`.
  Replace (not accumulate) each frame — edge strips are ephemeral view state.
- `src/save.lisp:1472` (delta variant) — Same for delta path. Delta edge strips update in-place.

**Client-side zone filtering:** The existing defense-in-depth zone-hash check (save.lisp:1500-1503) must NOT reject edge-strip entities — they intentionally have a different zone-hash. Edge strips are deserialized through a separate code path (keyed by `:edge-strips`) so the main zone-hash check is not involved.

**Tests:**
- Unit test: edge strips deserialized into `game-edge-strips`, not mixed with main entities.
- Unit test: edge strips replaced (not accumulated) each snapshot.
- Unit test: empty `:edge-strips` → empty `game-edge-strips` (no stale data).
- Unit test: strip deserialization reuses normal snapshot format (same entity types present).

---

### Step 9: Client-side multi-zone rendering (edge strip)

**What:** Render adjacent-zone tiles and edge-strip entities in the overlap region near zone boundaries. The rendering is type-agnostic — it iterates the edge-strip's entity vectors using the same draw functions as the main zone.

**Terrain rendering:** Already partially implemented via `ensure-preview-zones` (movement.lisp:962) and `draw-zone-preview` (rendering.lisp:1134). This step ensures it's always active when the player is near an edge.

**Entity rendering:** New — for each entry in `game-edge-strips`, render its players, NPCs, objects (and any future entity types) with the stored world-space offset applied.

**Files:**
- `src/movement.lisp:962` (`ensure-preview-zones`) — Ensure this runs whenever the player is within `*zone-edge-visibility-tiles*` of any edge. Tie trigger to `*zone-preload-radius*`.
- `src/rendering.lisp:1243` (preview zone rendering in `draw-world`) — Ensure preview zone tiles rendered for all nearby edges. Already works; verify corners.
- `src/rendering.lisp:113` (`draw-entities-with-spatial-culling`) — After rendering main-zone entities, iterate `game-edge-strips`. For each strip:
  1. Apply `offset-x`/`offset-y` to transform strip entity positions into world space.
  2. Call the same draw functions used for main-zone entities (draw-player, draw-npc, draw-object, etc.).
  3. Apply viewport culling identically.
  Because the strip contains the same entity structs as a normal snapshot, the draw functions work without modification. When new entity types are added (e.g., projectiles), they need a draw function for normal rendering — edge-strip rendering picks it up automatically.

**Coordinate mapping:** Edge-strip entities arrive in their home zone's local coordinates (0-origin). The client applies the same offset used by `draw-zone-preview` / `preview-zone-offset` for the corresponding edge. This offset is computed once per strip during deserialization (Step 8) and stored in the strip entry.

**Tests:**
- Unit test: edge-strip entity world-position computed correctly with zone offset.
- Unit test: edge-strip entity outside viewport is culled.
- Unit test: edge strips from multiple edges rendered simultaneously (corner case).
- Visual validation (manual): walk near zone edge, confirm adjacent-zone NPCs/players visible.

---

### Step 10: Zone-aware culling and unloading

**What:** Ensure only nearby cross-zone data is streamed. Far zones stay unloaded. The cache and preview system clean up zones the player has moved away from.

**Files:**
- `src/config.lisp` — `*zone-edge-visibility-tiles*` (from Step 7) controls the AOI width. No full-zone streaming.
- `src/zone.lisp` (zone cache) — LRU eviction (Step 4) handles unloading. Capacity of 9 means at most current + 8 neighbors are cached. Moving away from a region naturally evicts old zones.
- `src/movement.lisp:962` (`ensure-preview-zones`) — Add cache cleanup: when the player moves away from an edge (beyond preload radius), remove that edge's zone from the preview cache. This frees memory for zones the player isn't near.
- `src/save.lisp` (snapshot application) — Edge strips are replaced each frame, so moving away from an edge naturally clears them (server stops sending strips for that edge).

**Server-side bounds:** Edge-strip streaming (Step 7) is already bounded:
- Only triggered for players near edges.
- Only includes entities within strip width.
- Server never sends full adjacent zone data.

**Tests:**
- Unit test: moving away from edge clears preview cache entry.
- Unit test: edge strips cleared when player moves to zone interior (server sends no strips).
- Unit test: LRU cache evicts oldest zone when capacity exceeded.

---

### Step 11: Downgrade zone-transition DB save from Tier-1 to Tier-2

**What:** The current Tier-1 immediate save in `transition-zone` (100-500ms with retry) blocks the server tick. Zone transitions are not critical enough for Tier-1 — the player's position is covered by Tier-2 dirty-flag batching.

**Files:**
- `src/movement.lisp:1247` (`transition-zone`) — Replace `with-retry-exponential` / `db-save-player-immediate` with `(mark-player-dirty (player-id player))`. Flushes within the next 30-second checkpoint.

**Risk:** Server crash between transition and checkpoint → player reverts to pre-transition zone. Acceptable (they just re-enter). Compare with death/trade where Tier-1 is genuinely critical.

**Tests:**
- Unit test: after transition-zone, player is marked dirty (not immediately saved).
- Existing persistence tests still pass.

---

### Step 12: Add evaluation metrics

**What:** Instrument the system to validate all requirements.

**Files:**
- `src/config.lisp` — Add `*verbose-zone-transitions*` flag (default nil, env `MMORPG_VERBOSE_ZONES=1`).
- `src/movement.lisp` (`update-zone-transition`) — When verbose, log:
  - Transition count per tick.
  - Time since last transition per player (thrash detection).
  - Cooldown suppressions, hysteresis arms/cancels/commits, directional gating rejections.
  - Edge-strip entity counts per edge.
- `src/main.lisp` (`handle-zone-transition`) — When verbose, log:
  - Cache hit vs miss.
  - Wall-clock transition time.
  - Preload queue depth.
- `src/rendering.lisp` — When verbose, log edge entity render counts.

**Validation metrics (from findings doc):**
- Time between transitions: target >60s normal running.
- Transitions per 5 minutes: target <2 visible disruptions (now target 0).
- Thrash rate (5s window): target 0-1 (was 4+).
- Loading overlay shown: **never** (predicate always false).
- Edge-strip truthfulness: adjacent entities visible when near boundary.
- AOI bounds: no full-zone streaming (edge-entity count bounded).

**Tests:** No unit tests for logging — validated manually with `MMORPG_VERBOSE_ZONES=1`.

---

### Step 13: Run all tests and validate

```bash
make tests
```

All existing tests must pass. All new tests from Steps 1-12 must pass.

---

## File Change Summary

| File | Changes |
|------|---------|
| `src/types.lisp` | Add `zone-transition-cooldown`, `zone-transition-pending` to player. Add `zone-cache` struct. Add `preload-queue`, `edge-strips` to game. |
| `src/config.lisp` | Add `*zone-transition-cooldown-seconds*`, `*zone-hysteresis-in*` (6.0), `*zone-hysteresis-out*` (8.0), `*zone-direction-threshold*` (0.3), `*client-zone-cache-capacity*` (9), `*zone-preload-radius*` (10.0), `*zone-edge-visibility-tiles*` (10.0), `*verbose-zone-transitions*`. |
| `src/movement.lisp` | Modify `world-exit-edge-with-bounds` (hysteresis + directional gating). Modify `update-zone-transition` (cooldown, arm/commit/cancel state machine, preload trigger). Modify `transition-zone` (set cooldown, downgrade save). Modify `ensure-preview-zones` (preload-radius trigger, cache cleanup). |
| `src/zone.lisp` | Add `zone-cache-lookup`, `zone-cache-insert`. |
| `src/save.lisp` | Modify `apply-game-state` (cache lookup). Add `serialize-edge-strip`. Modify `serialize-game-state-for-zone` + delta variant (append `:edge-strips`). Modify `deserialize-game-state-compact` + delta variant (process `:edge-strips` using shared deserializer). |
| `src/main.lisp` | Modify `handle-zone-transition` (remove loading overlay, trigger preload). Add per-frame preload processing. Initialize zone cache. |
| `src/net.lisp` | No structural changes (serialization handles edge-strip inclusion). |
| `src/rendering.lisp` | Add edge-entity rendering in `draw-entities-with-spatial-culling`. Verify preview-zone tile rendering covers all edges. |
| `src/ui.lisp` | No changes (loading overlay stays for other uses, just never called from zone transitions). |
| `tests/unit-test.lisp` | Tests for all steps: cooldown, hysteresis, directional gating, LRU cache, preloading, loading predicate, edge-strip spatial filtering/serialization/deserialization/rendering coords, AOI bounds, cache cleanup, type-agnostic strip pass-through. |

---

## Dependency Order

```
Step 1  (cooldown)              ─── independent
Step 2  (hysteresis)            ─── independent
Step 3  (directional gating)    ─── independent (same function as Step 2)
Step 4  (zone cache)            ─── independent
Step 5  (arm-triggered preload) ─── depends on Step 2 (arm event) + Step 4 (cache)
Step 6  (remove overlay)        ─── depends on Step 5 (preload guarantees cache warm)
Step 7  (edge-strip server)     ─── independent (server-side only)
Step 8  (edge-strip client rx)  ─── depends on Step 7 (needs server data)
Step 9  (edge-strip rendering)  ─── depends on Step 8 (needs client data)
Step 10 (culling/unloading)     ─── depends on Steps 4, 9 (cache + preview)
Step 11 (Tier-2 downgrade)      ─── independent
Step 12 (metrics)               ─── depends on Steps 1-3, 7 (logs their decisions)
Step 13 (tests)                 ─── depends on all above
```

**Parallelizable groups:**
- Steps 1, 2, 3 together (all movement.lisp, server-side thrash prevention).
- Step 4 in parallel with Steps 1-3 (client cache, independent).
- Step 7 in parallel with Steps 4-6 (server edge-strip, independent of client cache work).
- Step 11 anytime (independent).

---

## Config Knob Summary

| Parameter | Default | Purpose |
|-----------|---------|---------|
| `*zone-transition-cooldown-seconds*` | 1.5 | Post-transition suppression window |
| `*zone-hysteresis-in*` | 6.0 tiles | Arm line distance from edge |
| `*zone-hysteresis-out*` | 8.0 tiles | Cancel line distance from edge (must be > in) |
| `*zone-direction-threshold*` | 0.3 | Min dot product for directional gating |
| `*client-zone-cache-capacity*` | 9 | LRU cache size (current + 8 neighbors) |
| `*zone-preload-radius*` | 10.0 tiles | Distance from edge to begin preloading |
| `*zone-edge-visibility-tiles*` | 10.0 tiles | Width of cross-zone entity/terrain strip |
| `*verbose-zone-transitions*` | nil | Enable transition diagnostics logging |

---

## What This Plan Does NOT Cover (Future Work)

- **Intersection buffer zones** (Option G from findings): data-only world-graph change, add selectively if playtesting reveals problem spots.
- **Larger zone dimensions** (Option C): 128x128 or 256x256 with chunk streaming. Only if the world design outgrows current tile budget.
- **Portal/threshold transitions** (Option E): skipped per findings (wrong vibe for open-world MMO).
- **Cross-zone interaction**: Edge entities are render-only. Players cannot attack/trade/interact with entities in the adjacent zone strip. This would require cross-zone message routing (future work if needed).
- **Cross-zone projectile continuity**: Projectiles (and any future entity types) that exist in an adjacent zone's edge strip are *visible* automatically via the type-agnostic strip system. However, a projectile that originates in one zone and needs to *continue* into another zone (gameplay continuity, not just visibility) requires cross-zone simulation routing — that's future work.
