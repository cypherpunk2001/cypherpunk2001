# Plan: Fix Client Prediction Breaking on Zone Transitions

**Issue:** Player gets "stuck" after crossing from zone-1 to zone-2 with client prediction enabled.
**Root Cause:** Prediction state is never reset on zone change, leaving stale coordinates from the old zone.

---

## Problem Analysis

### What Happens

1. Player is in zone-1 at position `(800, 300)`
2. Player crosses edge into zone-2
3. Server spawns player in zone-2 at `(50, 500)` (new zone's spawn point)
4. **But prediction state still holds `predicted-x=800, predicted-y=300`**
5. Next movement: `apply-local-prediction` tries to move from stale position
6. Server snapshot arrives with `(50, 500)` — huge error detected
7. `reconcile-prediction` snaps player to server position
8. **But prediction state is still misaligned**
9. Next frame: same problem repeats → player appears "stuck"

### Why Unstuck Doesn't Help

Unstuck teleports to a new position, but prediction state remains stale. Same snap-back loop.

---

## Code Facts

**Prediction state created once at startup** (`src/main.lisp:42-43`):
```lisp
:prediction-state (when *client-prediction-enabled*
                    (make-prediction-state player))
```

**Zone transition does NOT reset prediction** (`src/movement.lisp:1127-1296`):
- Updates player position ✓
- Clears intent targets ✓
- Resets animation states ✓
- Resets prediction state ✗ **MISSING**

**Reconciliation detects huge error** (`src/net.lisp:2165-2176`):
- Server says `(50, 500)`
- Prediction says `(800, 300)`
- Error > 5px → snaps to server
- But doesn't fix underlying state alignment

---

## Recommended Fix

### Option A: Reset Prediction on Zone Change (Recommended)

When player's zone-id changes, reset prediction state to match server position.

**Where:** Add reset logic in one of:
- `apply-game-state` / `apply-player-plists` when zone-id differs from current
- `transition-zone` (server-side, but client also calls similar logic)
- A dedicated hook in client snapshot processing

**What to reset:**
```lisp
(setf (prediction-state-predicted-x pred) (player-x player)
      (prediction-state-predicted-y pred) (player-y player)
      (prediction-state-input-count pred) 0        ; Clear input history
      (prediction-state-input-head pred) 0
      (prediction-state-last-acked-sequence pred) current-seq)  ; Accept server as truth
```

### Option B: Skip Prediction for N Frames After Zone Change

Temporarily disable prediction after zone transition to let client resync.

**How:**
- Set a `zone-transition-cooldown` timer (e.g., 0.5s)
- While active, skip `apply-local-prediction` and just use server positions
- After cooldown, resume prediction with fresh state

### Option C: Detect Large Error as Zone Change Signal

Enhance `reconcile-prediction` to detect "impossible" errors (e.g., >100px) as zone-change signals:
- If error exceeds zone-change threshold, fully reset prediction state
- Essentially auto-detecting the discontinuity

---

## Recommended Approach

**Option A is cleanest.** It explicitly handles the case and keeps prediction responsive.

### Implementation Steps

1. **Detect zone change on client:**
   - In snapshot processing (`apply-game-state` or similar)
   - Compare incoming `zone-id` with `(player-zone-id (game-player game))`

2. **Reset prediction state if zone differs:**
   - Set `predicted-x/y` to server's player position
   - Clear input ring buffer (set count=0, head=0)
   - Update `last-acked-sequence` to current snapshot sequence

3. **Test:**
   - Walk from zone-1 to zone-2 with prediction ON
   - Verify smooth transition, no stuck state
   - Walk back to zone-1, verify bidirectional works

---

## Files to Modify

| File | Change |
|------|--------|
| `src/net.lisp` | Add zone-change detection in snapshot processing, call reset function |
| `src/net.lisp` | Add `reset-prediction-for-zone-change` helper function |

---

## Testing Checklist

1. [ ] `make tests` passes
2. [ ] Zone-1 → Zone-2 transition works with prediction ON
3. [ ] Zone-2 → Zone-1 transition works (bidirectional)
4. [ ] Rapid zone crossings don't cause issues
5. [ ] Prediction still works normally within a zone
6. [ ] No misprediction spam in logs after zone change

---

## Notes

- This is a classic networked game bug: client-side predictive state doesn't handle discontinuous position jumps (teleports, zone changes)
- The fix is standard practice: reset prediction state on any "authoritative position override" event
- Similar logic may be needed for other teleport scenarios (admin teleport, death respawn, etc.)
