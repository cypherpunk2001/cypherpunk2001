# PLAN: Zone Transition Position Continuity Fix

**Date:** 2026-02-01 (v2 — incorporates review findings)
**Status:** Ready for implementation
**Reference:** `zone_findings_consolidated.md` (debug data + analysis)
**Scope:** Eliminate visible teleport jumps when crossing zone boundaries. Player must be able to walk on any walkable tile including edge-adjacent tiles, and zone transitions must be seamless.

---

## Behavioral Spec

This section defines the exact behavioral contract. All implementation must conform to these rules.

### Definitions

- **Boundary ring:** Tiles 0 and 63 (for a 64x64 zone). These are non-walkable collision walls. They are not player-accessible and are not considered "visible tiles" for the walkability requirement.
- **Edge-adjacent tile:** The last walkable tile before the boundary ring. Tiles 1 and 62 (for a 64x64 zone). These are the "edge tiles" referenced in the requirement.
- **Collision bounds:** The min/max world coordinates the player center can reach. For a 64x64 zone with tile-size=64 and collider-half=27.2: `[91.2, 4004.8]` on both axes. The player can stand at exactly these coordinates.
- **Attempted position:** The player's intended position for the current tick, computed as `current-pos + direction * step`, BEFORE collision resolution and bounds clamping. Updated every tick.
- **Actual position:** The player's position after collision resolution and clamping. This is what the player struct stores in `player-x`/`player-y`.

### Crossing Criteria

A zone boundary crossing occurs when ALL of the following are true:
1. The player has a pending transition (arm band was entered, directional gating passed)
2. The player's **attempted position** on the crossing axis exceeds the collision bound:
   - South: `attempted-y > max-y` (strict inequality)
   - North: `attempted-y < min-y` (strict inequality)
   - East: `attempted-x > max-x` (strict inequality)
   - West: `attempted-x < min-x` (strict inequality)
3. The player's movement intent has a component toward the edge (directional gating, existing behavior)
4. The transition cooldown has expired (existing behavior)

**Consequence:** A player standing still on the edge-adjacent tile (attempted = actual = collision bound) does NOT trigger a crossing, because `attempted == max` fails strict inequality. The player must be actively moving past the boundary.

### Overstep Definition

- **Overstep** = distance the attempted position extends past the collision bound, clamped to >= 0.
  - South: `max(0.0, attempted-y - max-y)`
  - North: `max(0.0, min-y - attempted-y)`
  - East: `max(0.0, attempted-x - max-x)`
  - West: `max(0.0, min-x - attempted-x)`
- Overstep is 0 when the player has not crossed the boundary.
- Typical overstep at 60Hz with player speed ~200 wu/s: `200 * 0.0167 ≈ 3.3 wu` (~0.05 tiles).

### Seam Translation Formula (unchanged)

The existing `seam-translate-position` formula is correct when fed the attempted position:
- East: `new-x = dst-min-x + (attempted-x - src-max-x)`, `new-y = attempted-y`
- West: `new-x = dst-max-x + (attempted-x - src-min-x)`, `new-y = attempted-y`
- South: `new-x = attempted-x`, `new-y = dst-min-y + (attempted-y - src-max-y)`
- North: `new-x = attempted-x`, `new-y = dst-max-y + (attempted-y - src-min-y)`

**Invariant:** For same-size zones where `src-bounds == dst-bounds`, seam translation produces a position that is `overstep` world units inside the destination collision bounds. `seam-position-valid-p` must return T.

**Fallback:** The ratio-spawn + overstep fallback path is used ONLY when:
- The seam-translated position is out of bounds (different-size zones with extreme offset), OR
- The seam-translated position is blocked by a wall tile in the destination zone.

### Acceptance Criteria

1. Player can walk to and stand on the edge-adjacent tile (tile 1 or 62) without any transition firing.
2. Walking past the edge-adjacent tile (into the boundary ring) triggers a transition.
3. Seam translation is the primary path (`path=seam` in logs) for same-size zones.
4. Post-transition position is `overstep` wu inside the destination zone's collision bound on the crossing axis (~1-5 wu, not ~28 wu).
5. The preserved axis coordinate is unchanged (X preserved for north/south, Y preserved for east/west).
6. No visible pop, hitch, or camera jump during the transition.
7. Logs show: `in-bounds=T`, `path=seam`, `overstep=~1-5`.

### What Is NOT Changed

- **Preview/minimap edge detection** continues to use the player's actual position via `world-exit-edge` / `world-exit-edge-with-bounds`. No change.
- **ARM band detection** continues to use actual position. No change.
- **Cancel line detection** continues to use actual position. No change.
- **Boundary ring (tiles 0/63)** remains non-walkable. The requirement "any tile you can see" means any tile you can walk on.

---

## Root Cause Summary

Three code-level issues combine to guarantee a visible position snap on every zone transition:

1. **Commit fires while the player is inside the source zone.** `world-exit-edge-with-bounds` uses a `commit-margin` of 32 wu (0.5 tiles), which means the transition commits when the player is 28-32 wu from the wall — not at or past it. (`movement-transition.lisp:199-236`, `:712-717`)

2. **Seam translation assumes the player has already crossed the boundary.** `seam-translate-position` computes `dst-min + (pos - src-max)`, which produces a negative offset when the player is still inside source bounds. The result lands outside destination bounds, failing `seam-position-valid-p`. (`movement-transition.lisp:403-412`, `:414-418`)

3. **Overstep means "distance to edge" not "distance past edge."** `compute-transition-overstep` returns `(- src-max pos)` which is positive when the player hasn't crossed. The fallback uses this to push the player inward from the destination wall, creating the snap. (`movement-transition.lisp:422-435`)

**Net effect:** The primary seam path is dead code. Every transition takes the fallback path, placing the player ~0.45 tiles from the destination wall. The position is mathematically mirrored (distance-from-wall is preserved), but the player visibly jumps from near one wall to near the opposite wall of the next zone.

---

## Design Principles for the Fix

1. **The player owns the edge-adjacent tile.** Standing on tiles 1 or 62 must not trigger a transition. Only attempting to move past the collision bound triggers it.
2. **Seam translation is the primary path.** The fallback (ratio-spawn) is only for blocked tiles or mismatched zone sizes.
3. **Overstep preserves sub-tile position.** If the player's intended movement would carry them 3 wu past the boundary, they appear 3 wu into the destination zone.
4. **Collision stops at the bound, not before it.** The player's collider can rest against the collision boundary (91.2 / 4004.8).
5. **No visual disruption.** Camera, interpolation, and rendering must not produce hitches or pops during the transition.
6. **Preview/minimap logic is untouched.** Only commit detection changes. ARM, cancel, preview, and minimap use actual position as before.

---

## Implementation Plan

### Phase 1: Fix Commit Timing (core fix)

**Goal:** Commit fires only when the player's intended movement crosses the zone boundary, not when they're merely near it.

#### Step 1.1: Track pre-collision attempted position

**File:** `src/types.lisp` — add two ephemeral `single-float` fields to the `player` struct:
- `attempted-x` (default 0.0) — pre-collision intended X
- `attempted-y` (default 0.0) — pre-collision intended Y

These are ephemeral (not serialized, not in snapshots, not in save.lisp).

**File:** `src/movement-transition.lisp` (function `update-player-position`, lines 33-119)

Currently `update-player-position` computes movement inside `attempt-move` / `attempt-move-with-map`, but the pre-collision position is not exposed.

**Change:** Before calling `do-move`, compute the unclamped target position and store it:

```lisp
;; Compute attempted position before collision
;; For direct input movement:
(let ((attempted-x (+ x (* input-dx (* *player-speed* speed-mult dt))))
      (attempted-y (+ y (* input-dy (* *player-speed* speed-mult dt)))))
  (setf (player-attempted-x player) attempted-x
        (player-attempted-y player) attempted-y))
```

**Both movement modes must set attempted position:**
- **Direct input** (`input-dx`/`input-dy` nonzero): `attempted = current + dir * step`
- **Click-to-move** (`intent-target-active`): `attempted = current + normalized-to-target * step`
- **Stationary** (no movement): `attempted = current` (actual position). This ensures stale values never cause spurious commits.

The attempted position is always set BEFORE collision resolution and bounds clamping, every tick.

#### Step 1.2: New helper for crossing detection (commit path only)

**CRITICAL: Do NOT modify `world-exit-edge-with-bounds`.** That function is also called by `world-exit-edge` which drives `world-preview-edge` for minimap/preview rendering (`movement-preview.lisp:10`). Changing it to use attempted positions would break preview logic.

**Instead:** Add a new function `world-crossing-edge` for commit detection only:

**File:** `src/movement-transition.lisp`

```lisp
(defun world-crossing-edge (player min-x max-x min-y max-y)
  "Return the edge the player's attempted position crosses, if any.
   Uses strict inequality: attempted must exceed the collision bound.
   Only for commit detection — preview/minimap use world-exit-edge instead."
  (multiple-value-bind (dx dy) (player-intent-direction player)
    (let ((ax (player-attempted-x player))
          (ay (player-attempted-y player))
          (edge nil)
          (weight 0.0))
      ;; Check each edge: attempted position must be past boundary AND
      ;; intent must point toward the edge (directional gating)
      (when (and (< dy 0.0) (< ay min-y))       ; north crossing
        (let ((w (abs dy)))
          (when (> w weight) (setf edge :north weight w))))
      (when (and (> dy 0.0) (> ay max-y))        ; south crossing
        (let ((w (abs dy)))
          (when (> w weight) (setf edge :south weight w))))
      (when (and (< dx 0.0) (< ax min-x))        ; west crossing
        (let ((w (abs dx)))
          (when (> w weight) (setf edge :west weight w))))
      (when (and (> dx 0.0) (> ax max-x))         ; east crossing
        (let ((w (abs dx)))
          (when (> w weight) (setf edge :east weight w))))
      ;; Directional gating (same as existing)
      (when (and edge (not (edge-direction-passes-p dx dy edge)))
        (setf edge nil))
      edge)))
```

**File:** `src/movement-transition.lisp` (function `update-zone-transition`, ~line 759)

Replace the commit detection call:
```lisp
;; OLD (line 759-761):
(let ((actual-edge (world-exit-edge-with-bounds
                     player min-x max-x min-y max-y
                     commit-margin)))
;; NEW:
(let ((actual-edge (world-crossing-edge
                     player min-x max-x min-y max-y)))
```

The `commit-margin` variable is no longer used by the commit path. It can be removed from the `let*` binding at line 716, or kept as dead code for one release cycle.

#### Step 1.3: Update commit margin config

**File:** `src/config.lisp` (line 120)

Change `*zone-commit-margin-tiles*` from `0.5` to `0.01` (tiny epsilon, not zero — avoids tripping the `> 0.0` validation at line 162).

This parameter is now **unused at runtime.** Commit detection uses `world-crossing-edge` (new). Preview uses `world-exit-edge` which calls `world-exit-edge-with-bounds` with margin 0.0 (default). No code path passes `*zone-commit-margin-tiles*` to anything anymore — `update-zone-transition` computed `commit-margin` from it, but that variable fed `world-exit-edge-with-bounds` which is now replaced by `world-crossing-edge` in the commit path.

**Action:** Mark as explicitly unused/legacy. Remove the `validate-zone-config` check for it (line 162-164) since there's nothing to validate. Keep the parameter itself for one release cycle in case rollback is needed.

```lisp
(defparameter *zone-commit-margin-tiles* 0.01
  "UNUSED — legacy parameter. Commit detection now uses world-crossing-edge
   with attempted position. Retained for rollback safety; will be removed.")
```

Also remove from `validate-zone-config`:
```lisp
;; REMOVE these lines (162-164):
(unless (> *zone-commit-margin-tiles* 0.0)
  (error "Zone config: *zone-commit-margin-tiles* (~a) must be > 0"
         *zone-commit-margin-tiles*))
```

Also simplify the commit-margin calculation in `update-zone-transition` (line 716). The `max(half-w, half-h)` floor is no longer needed since commit detection doesn't use it:

```lisp
;; OLD:
(commit-margin (max (* *zone-commit-margin-tiles* tile-size)
                    (max half-w half-h)))
;; NEW:
(commit-margin (* *zone-commit-margin-tiles* tile-size))
```

---

### Phase 2: Fix Seam Translation

**Goal:** `seam-translate-position` produces in-bounds results when the player's attempted position crosses the boundary.

#### Step 2.1: Pass attempted position to seam translation

**File:** `src/movement-transition.lisp` (function `transition-zone`, ~line 546-550)

Currently:
```lisp
(let* ((px (player-x player))
       (py (player-y player)))
  (multiple-value-bind (trans-x trans-y)
      (seam-translate-position edge px py ...)
```

**Change:** Use attempted position:
```lisp
(let* ((px (player-attempted-x player))
       (py (player-attempted-y player)))
  (multiple-value-bind (trans-x trans-y)
      (seam-translate-position edge px py ...)
```

The attempted position is past the boundary, so the seam formula will produce a positive offset into the destination zone:

For a south crossing with `attempted-y > src-max-y`:
```
dst-min-y + (attempted-y - src-max-y)  →  positive offset from dst-min-y  →  in bounds
```

This makes `seam-position-valid-p` return T, and the primary seam path executes.

#### Step 2.2: No formula change needed in `seam-translate-position`

The formula in `seam-translate-position` (lines 392-412) is already correct for the case where the player has crossed the boundary. The problem was that it was being fed a position that hadn't crossed. With step 2.1, the formula works as designed.

---

### Phase 3: Fix Overstep Semantics

**Goal:** `compute-transition-overstep` returns distance *past* the edge, not distance *to* the edge.

#### Step 3.1: Redefine overstep using attempted position

**File:** `src/movement-transition.lisp` (function `compute-transition-overstep`, lines 422-435)

**Current code:**
```lisp
(let ((px (player-x player))
      (py (player-y player)))
  (max 0.0
       (case edge
         (:north (- py src-min-y))    ; distance FROM north edge (always positive inside zone)
         (:south (- src-max-y py))    ; distance FROM south edge (always positive inside zone)
         (:west  (- px src-min-x))
         (:east  (- src-max-x px))
         (t 0.0))))
```

**New code:**
```lisp
(let ((ax (player-attempted-x player))
      (ay (player-attempted-y player)))
  (max 0.0
       (case edge
         (:north (- src-min-y ay))    ; positive when attempted-y < src-min-y (crossed north)
         (:south (- ay src-max-y))    ; positive when attempted-y > src-max-y (crossed south)
         (:west  (- src-min-x ax))    ; positive when attempted-x < src-min-x (crossed west)
         (:east  (- ax src-max-x))    ; positive when attempted-x > src-max-x (crossed east)
         (t 0.0))))
```

Now overstep is 0 when the player hasn't crossed, and positive (~3 wu at 60Hz) when they have.

#### Step 3.2: Fallback behavior remains correct

The fallback path uses `apply-overstep-to-spawn` to push inward from the destination wall by the overstep amount. With the corrected semantics, a typical transition will have overstep ~3 wu (~0.05 tiles), producing a spawn position 3 wu inward — effectively continuous.

If the player hasn't actually crossed (overstep = 0), the fallback places them at the exact wall boundary. This is safe.

---

### Phase 4: Edge Tile Access (verification only — no changes needed)

**Goal:** Confirm the player's collider can rest against the zone boundary without triggering a transition.

#### Step 4.1: Collision bounds already permit edge tile access

**File:** `src/movement-core.lisp` (function `zone-bounds-zero-origin`, lines 135-143)

Current bounds for 64x64 zone, tile-size=64, collider-half=27.2:
```
min = 64 + 27.2 = 91.2    (start of tile 1 + half-collider)
max = 63*64 - 27.2 = 4004.8  (end of tile 62 - half-collider)
```

The player can stand at exactly 91.2 (tile 1) or 4004.8 (tile 62). The boundary ring (tiles 0 and 63) is non-walkable — it acts as the zone's collision wall.

**No change needed.** The premature commit (Phase 1) was the problem, not the collision bounds.

#### Step 4.2: Clamp does not prevent edge access

The clamp in `update-player-position` (lines 94-95) restricts to `[min, max]` inclusive. The player CAN reach the exact bound.

**No change needed.**

---

### Phase 5: Server Zone Cache Fix

**Goal:** Eliminate synchronous disk loads during transitions.

#### Step 5.1: Cache zones after sync load

**File:** `src/movement-transition.lisp` (function `transition-zone`, ~lines 478-491)

When `transition-zone` falls back to synchronous disk load (cache miss), the loaded zone is not inserted into the LRU cache. This means the next transition to the same zone will miss again.

**Fix:** After the sync load succeeds, insert into the LRU cache:

```lisp
;; After sync fallback load:
(let ((loaded (load-zone target-path)))
  (when (and loaded zone-lru)
    (zone-cache-insert zone-lru target-id loaded))
  loaded)
```

This ensures that bouncing between two zones (as in the debug session) hits cache on the return trip.

#### Step 5.2: Server-side preload (deferred)

The server currently has no preload mechanism equivalent to the client's `ensure-preview-zones`. Adding server-side preload when a player enters the arm band would eliminate sync loads entirely. This is a separate enhancement — the cache fix in 5.1 is sufficient for now.

---

### Phase 6: Diagnostic Improvements

#### Step 6.1: Client verbose zone flag

**File:** `scripts/client.lisp`

Add reading of `MMORPG_VERBOSE_ZONES` environment variable:

```lisp
(when (uiop:getenv "MMORPG_VERBOSE_ZONES")
  (setf mmorpg::*verbose-zone-transitions* t))
```

#### Step 6.2: Enhanced seam translation log

**File:** `src/movement-transition.lisp` (transition-zone logging, ~line 565)

Add to the existing log line:
- `attempted=(ax, ay)` — the pre-collision position used for crossing detection
- `overstep=N` — the corrected overstep value (should be ~1-5 wu)
- `path=seam` — confirm the primary path is being used

Post-fix log should look like:
```
[ZONE] Seam translation: ZONE-1->ZONE-3 edge=SOUTH path=seam
  old=(627.6, 3976.2) attempted=(627.6, 4008.1)
  src-bounds=(91.2, 4004.8, 91.2, 4004.8)
  dst-bounds=(91.2, 4004.8, 91.2, 4004.8)
  -> trans=(627.6, 94.5) in-bounds=T overstep=3.3
```

#### Step 6.3: Reduce cooldown log spam

**File:** `src/movement-transition.lisp` (~line 724)

Change cooldown logging from every-tick to once at cooldown start.

**Exact location:** Inside `transition-zone`, immediately after setting `player-zone-transition-cooldown` (currently ~line 670, after the `(setf (player-zone-transition-cooldown player) *zone-transition-cooldown-seconds*)` call). Add:

```lisp
;; Log cooldown start once (not every tick)
(when *verbose-zone-transitions*
  (log-zone "Zone transition: cooldown started (~,2fs) for player ~a"
            *zone-transition-cooldown-seconds* (player-id player)))
```

**Also remove:** The per-tick cooldown log in `update-zone-transition` (~line 724) that currently fires every tick while cooldown > 0. Delete the `(when *verbose-zone-transitions* (log-zone "Zone transition: cooldown active ..."))` block entirely.

---

### Phase 7: Tests

#### Step 7.1: New tests for corrected behavior

**File:** `tests/unit/zone-continuity-tests.lisp`

Add to `*tests-zone-continuity*`:

1. **`test-commit-requires-boundary-crossing`**
   - Player at edge tile (actual pos = max-y = 4004.8), attempted = 4004.8 (stationary) → `world-crossing-edge` returns NIL.
   - Same player, attempted-y = 4008.0 (past boundary) → returns `:south`.

2. **`test-crossing-edge-does-not-affect-preview`**
   - `world-exit-edge-with-bounds` (used by preview) still returns an edge based on actual position + commit margin. Unaffected by attempted position.

3. **`test-overstep-zero-when-inside-zone`**
   - `compute-transition-overstep` with attempted-y = 3976.0 (inside zone, south edge at 4004.8) → returns 0.0.

4. **`test-overstep-positive-when-past-edge`**
   - `compute-transition-overstep` with attempted-y = 4008.0 (past south edge at 4004.8) → returns 3.2.

5. **`test-seam-translation-in-bounds-for-same-size-zones`**
   - `seam-translate-position` with attempted-y = 4008.0, south edge, same-size zones → result passes `seam-position-valid-p`.

6. **`test-player-can-stand-on-edge-tile-without-transition`**
   - Player at exactly max-y with no movement intent. `world-crossing-edge` returns NIL. No transition fires.

7. **`test-transition-uses-seam-path-not-fallback`**
   - Integration test: `transition-zone` with player's attempted position past boundary → player ends up at seam-translated position (not fallback), ~3 wu from destination wall.

8. **`test-attempted-position-set-for-stationary-player`**
   - Player with no movement: `attempted-x = player-x`, `attempted-y = player-y`. No stale values.

9. **`test-attempted-position-set-for-click-to-move`**
   - Player with active click target: attempted position computed from target direction * step.

#### Step 7.2: Update existing tests

- **`test-compute-transition-overstep-north`** — Set `player-attempted-y` to a value past the north edge (e.g., 5.0, with north bound at 10.0). Expected overstep = 5.0.

- **`test-compute-transition-overstep-all-edges`** — Same: set attempted positions past each edge. Update expected values to match "distance past edge" semantics.

- **`test-seam-translation-used-in-transition-zone`** — Set `player-attempted-x` to 562.0 (same as actual, already past boundary). Should still pass.

- **`test-seam-translation-blocked-uses-fallback`** — Set `player-attempted-x` to 562.0. Should still pass (blocked by wall, fallback expected).

---

### Phase 8: Validation

#### Step 8.1: Run all tests
```bash
make tests
```
All must pass, including updated zone-continuity tests.

#### Step 8.2: Manual validation (required)

Run server + client with verbose zones:
```bash
MMORPG_VERBOSE=1 MMORPG_VERBOSE_ZONES=1 make server
MMORPG_VERBOSE=1 MMORPG_VERBOSE_ZONES=1 make client
```

1. Log in, walk to edge tile — confirm no transition fires
2. Walk past edge tile — confirm transition fires with `path=seam`, small overstep
3. Walk back — confirm return transition also uses seam path
4. Verify no visible pop, hitch, or camera jump
5. Check logs confirm: `in-bounds=T`, `path=seam`, `overstep=~1-5`

Only mark complete after user confirms the transition feels seamless.

---

## File Change Summary

| File | Change | Phase |
|------|--------|-------|
| `src/types.lisp` | Add `attempted-x`, `attempted-y` to player struct | 1.1 |
| `src/movement-transition.lisp` | Store attempted position in `update-player-position` (both movement modes + stationary) | 1.1 |
| `src/movement-transition.lisp` | Add `world-crossing-edge` (new function, commit detection only) | 1.2 |
| `src/movement-transition.lisp` | Replace `world-exit-edge-with-bounds` call with `world-crossing-edge` in commit path | 1.2 |
| `src/config.lisp` | `*zone-commit-margin-tiles*` → 0.01, mark unused, remove validation | 1.3 |
| `src/movement-transition.lisp` | Simplify commit-margin calc (remove collision-half floor) | 1.3 |
| `src/movement-transition.lisp` | `transition-zone`: pass attempted pos to seam translation | 2.1 |
| `src/movement-transition.lisp` | `compute-transition-overstep`: use attempted pos, invert sign | 3.1 |
| `src/movement-transition.lisp` | Insert loaded zone into LRU cache after sync load | 5.1 |
| `scripts/client.lisp` | Read `MMORPG_VERBOSE_ZONES` env var | 6.1 |
| `src/movement-transition.lisp` | Enhanced seam log with attempted pos + overstep | 6.2 |
| `src/movement-transition.lisp` | Reduce cooldown log spam (once not every tick) | 6.3 |
| `tests/unit/zone-continuity-tests.lisp` | 9 new tests + update 4 existing overstep tests | 7.1, 7.2 |

**Untouched files:** `world-exit-edge-with-bounds`, `world-exit-edge`, `movement-preview.lisp`, `seam-translate-position` formula, `zone-bounds-zero-origin`.
**Modified but not functionally changed:** `validate-zone-config` (remove dead `*zone-commit-margin-tiles*` check).

---

## Risk Assessment

| Risk | Mitigation |
|------|------------|
| Stale attempted position causes spurious commit | Attempted pos set every tick for ALL movement modes including stationary (attempted = actual when no movement). Test 7.1.8 verifies. |
| `world-crossing-edge` diverges from preview logic | Intentional separation. Preview uses actual pos via `world-exit-edge-with-bounds`. Commit uses attempted pos via `world-crossing-edge`. Test 7.1.2 verifies they don't interfere. |
| `*zone-commit-margin-tiles*` now unused | Explicitly marked as legacy/unused. Validation check removed. Parameter retained for one release cycle for rollback safety. No code reads it. |
| Attempted position never set (nil/0) on first tick | Default to 0.0 in struct. `update-player-position` runs before `update-zone-transition` in the tick loop, so attempted is always fresh. If somehow stale, 0.0 is far from any edge (zones start at 91.2), so no spurious commit. |
| Diagonal movement across corner | `world-crossing-edge` picks the dominant axis via weight (same as existing `world-exit-edge-with-bounds`). |
| NPC transitions | NPCs don't use attempted-position. They're repositioned relative to the player's final spawn. No change needed. |
| Existing tests break on overstep semantics | Phase 7.2 updates all affected tests. `apply-overstep-to-spawn` tests are unaffected (they test the application function, not the computation). |
| Client prediction mismatch | Client runs the same `update-player-position` which stores attempted pos. Server authority resolves any discrepancy via snapshots. |

---

## What This Plan Does NOT Cover

- **Seamless loading / cross-zone rendering:** This plan fixes position continuity only. Visual seams (seeing into the next zone before transitioning) require the broader seamless loading system.
- **Parallel zone loading:** The sync load fallback is fixed by caching (Phase 5), not by making loads async.
- **Multi-zone server architecture:** One server process per zone. This plan doesn't change that.
- **Camera interpolation during transition:** The soft-reset system (ADDENDUM 4 in existing code) already handles this. Phase 1-3 should make transitions small enough (~3 wu) that soft-reset preserves buffers.
- **Boundary ring walkability:** Tiles 0/63 remain non-walkable collision walls by design.
