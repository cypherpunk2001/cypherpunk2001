# Plan: Fix NPC Movement Regression

**Goal:** Restore NPC idle/wander behavior so NPCs move around their home radius even when no player is nearby (but a player is in the zone). Also fix aggro/perception so NPCs can detect players within their perception range, not just 3x3 cells.

**Related analysis:** `npc-regression-analysis.md`

---

## Problem Summary

The Phase 2 spatial-grid optimization in `closest-player` restricted NPC AI to only run when a player is within 3x3 grid cells. Combined with AI functions being gated on a non-nil player, NPCs freeze completely when no player is nearby.

**Simulation scope:** NPCs only update in occupied zones (server sim iterates `*occupied-zones-cache*`). After this fix, NPCs will wander when a player is anywhere in the zone, even if far away. NPCs in unoccupied zones remain frozen (acceptable).

---

## Changes Required

### 1. Expand `closest-player` to use perception range (`src/ai.lisp`)

**In `closest-player` (lines 4-43):**

Current spatial-grid path only scans 3x3 cells. This limits aggro/perception to ~1 tile regardless of NPC's actual perception range.

**Option A (zone-wide fallback):** When neighbor query returns 0 results, fall back to linear scan of `zone-players`:
```lisp
(if (and grid (npc-grid-cell-x npc) (npc-grid-cell-y npc) game)
    (let ((count (spatial-grid-query-neighbors-into ...)))
      (if (> count 0)
          ;; existing neighbor scan
          ...
          ;; Fallback: no neighbors, scan zone-players array
          (when zone-players
            (loop :for player :across zone-players ...))))
    ;; existing fallback
    ...)
```

**Option B (radius-based query):** Add `spatial-grid-query-radius-into` that takes a cell radius parameter. Compute radius from `npc-perception-range-sq` / cell-size. More performant but more complex.

**Recommendation:** Option A is simpler and sufficient. The fallback only runs when no players are in 3x3 cells, and zone-players is typically small.

### 2. Remove zone-players length guard in `simulate-zone-npcs` (`src/main.lisp`)

**In `simulate-zone-npcs` (lines 385-386):**

Current guard:
```lisp
(when (and zone-npcs (> (length zone-npcs) 0)
           zone-players (> (length zone-players) 0))
```

**Problem:** Even after nil-player fix, an empty/stale cache stops NPC updates. But `*occupied-zones-cache*` already proves a player is in the zone.

**Fix:** Remove the zone-players guard entirely. The nil-player handling will treat empty cache as "run with nil player" (idle/wander):
```lisp
(when (and zone-npcs (> (length zone-npcs) 0))
  (loop :for npc :across zone-npcs
        :for target-player = (closest-player zone-players npc zone-state game)
        :do (progn
              (update-npc-behavior npc target-player world)
              (update-npc-intent npc target-player world dt)
              (update-npc-movement npc world dt zone-state)
              (when target-player
                (update-npc-attack npc target-player world dt event-queue))
              (incf count))))
```

This eliminates the fragile dependency on the cache being perfectly populated.

### 3. Handle nil player safely in `update-npc-intent` (`src/ai.lisp`)

**In `update-npc-intent` (line 191):**

Current guard skips entire function when player is nil:
```lisp
(when (and (npc-alive npc) player)
  ...)
```

**CRITICAL:** The `:flee` state accesses `player-x`/`player-y` and will crash if player is nil. Must gate flee on player.

Change to:
```lisp
(when (npc-alive npc)
  (let* ((intent (npc-intent npc))
         (state (cond
                  ;; Flee requires a player to flee FROM - gate on player
                  ((and player (npc-should-flee-p npc)) :flee)
                  ;; No player nearby = idle/wander
                  ((null player) :idle)
                  ;; Otherwise use behavior state
                  (t (npc-behavior-state npc))))
         ...)
```

The existing `t` case (lines 240-244) already handles idle/wander via `npc-wander-direction`. This will now run when player is nil.

### 4. Update `update-npc-behavior` to handle nil player (`src/ai.lisp`)

**In `update-npc-behavior` (lines 143-183):**

Already handles nil player at line 156-157:
```lisp
((not player)
 (setf new-state :idle))
```

No change needed - already correct.

### 5. Zone-state cache rebuild strategy (`src/main.lisp`)

**Decision:** Rely on existing rebuild logic in `update-sim`, but ensure it runs before any guard that could skip NPCs.

**In `update-sim` (around line 534-541):**

Current logic rebuilds cache when empty and players exist:
```lisp
(when (and (= (length zone-players) 0)
           (> (zone-state-player-count zone-state) 0))
  ;; rebuild cache + ensure-player-in-grid
  ...)
```

**Verify:** This rebuild must happen BEFORE `simulate-zone-npcs` is called. Check the ordering in `update-sim`.

**Alternative (if ordering is wrong):** Move cache rebuild to the start of the zone iteration loop, before calling `simulate-zone-npcs`.

### 6. Verify player-zone-id is set on login (`src/net.lisp`)

**In `add-player-to-game`:**

Verify that `player-zone-id` is set before zone-state-player-count is queried. This is what makes the count non-zero for the player's zone.

`add-player-to-game` already inserts the player into the grid + cache when zone-state exists. The signature is `(ensure-player-in-grid player zone-state)`. No additional call needed here if zone-state exists at login time.

### 7. Add deterministic unit test for nil-player path (`tests/unit-test.lisp`)

Add a test that calls `update-npc-behavior` and `update-npc-intent` with `player = nil` and asserts:
- No error/crash
- Intent dx/dy are set (wander direction computed)
- Behavior state is `:idle`

**Avoid randomness:** Before calling, set:
- `(setf (npc-wander-x npc) (+ (npc-x npc) 10.0))` - known offset
- `(setf (npc-wander-y npc) (npc-y npc))`
- `(setf (npc-wander-timer npc) 5.0)` - positive timer so it uses existing target

This ensures `npc-wander-direction` returns a deterministic direction (normalized (10, 0) → (1.0, 0.0)) instead of picking a random target.

---

## Files Modified

| File | Change |
|------|--------|
| `src/ai.lisp` | Add zone-wide fallback in `closest-player`; handle nil player safely in `update-npc-intent` |
| `src/main.lisp` | Remove zone-players length guard in `simulate-zone-npcs`; verify cache rebuild ordering |
| `tests/unit-test.lisp` | Add deterministic nil-player AI test |

---

## Testing

1. `make tests` - all must pass (including new nil-player test)
2. Login to zone with NPCs - they should be wandering immediately
3. Walk away from NPCs - they should continue wandering
4. NPCs should aggro when player enters perception range (not just 3x3 cells)
5. Zone transition and return - NPCs should still be wandering
6. Two clients in same zone - both should see same NPC movement
7. NPCs should still chase and attack when player gets close
8. Verify no crash when NPC tries to flee but player is nil

---

## Risk Assessment

**Low risk:** Changes are isolated to NPC AI loop. The fix restores intended behavior (NPCs wander when idle) that was inadvertently disabled by the spatial-grid optimization.

**Performance note:** The zone-wide fallback in `closest-player` only runs when 3x3 neighbor query returns empty. For typical gameplay (player near some NPCs), the fast path is still used. Idle/wander behavior is lightweight.

**Safety note:** The nil-player :flee crash is a real risk if we forget to gate it. The unit test will catch this.
