# Plan: Enable Client Prediction by Default

**Goal:** Fix local player hitching by enabling client-side prediction by default, and remove the misleading ESC menu checkbox.

---

## Why This Fixes the Hitch

With 20Hz snapshots and prediction OFF, the local player position only updates every 50ms (server snapshot rate). The camera follows the local player, so the ground appears to hitch.

With prediction ON, the client applies movement locally at 60fps, then reconciles with server snapshots. This restores smooth movement feel while keeping Phase 3 bandwidth savings.

---

## Changes Required

### 1. Enable prediction by default

**File:** `src/config-client.lisp` line 119

NOTE: This is already set to `t` in the current tree; keep it as-is and update
docs accordingly.

```lisp
;; Before
(defparameter *client-prediction-enabled* nil
  "Enable client-side prediction for local player. Toggle via SLIME for testing.")

;; After
(defparameter *client-prediction-enabled* t
  "Enable client-side prediction for local player. Provides smooth 60fps movement
   while server snapshots run at lower rates. Disable via SLIME if debugging.")
```

### 2. Remove prediction UI (checkbox + threshold)

We want prediction always on by default, and **no runtime toggle**. Remove the
prediction checkbox **and** the prediction threshold control + display.

**File:** `src/ui.lisp`

- Remove click handler (lines 560-564):
  ```lisp
  ;; Client prediction toggle
  ((point-in-rect-p mouse-x mouse-y
                    (ui-menu-prediction-x ui) (ui-menu-prediction-y ui)
                    (ui-menu-prediction-size ui) (ui-menu-prediction-size ui))
   (setf *client-prediction-enabled* (not *client-prediction-enabled*)))
  ```

- Remove prediction threshold cycle (lines 584-591) - no longer needed.

- Remove UI struct fields:
  `menu-prediction-size`, `menu-prediction-x`, `menu-prediction-y`, `menu-prediction-label`,
  `menu-threshold-size`, `menu-threshold-x`, `menu-threshold-y`

- Update **both** layout paths:
  - `make-ui` layout constants
  - `update-ui-layout` layout constants
  to close the spacing gap left by prediction/threshold rows.

**File:** `src/rendering.lisp`

- Remove prediction checkbox drawing (lines 2368-2383)
- Remove prediction threshold text display (line ~2423)
- Remove `pred-on` and `pred-box-color` variables from draw function

**File:** `src/types.lisp`

- Remove prediction UI slots from `ui` struct definition (line ~222)

### 3. Update documentation

**File:** `docs/config-client.md` - update to reflect new default
**File:** `docs/ui.md` - remove prediction/threshold UI references

---

## Files Modified

| File | Change |
|------|--------|
| `src/config-client.lisp` | Default `*client-prediction-enabled*` to `t` |
| `src/ui.lisp` | Remove prediction toggle click handler and layout |
| `src/rendering.lisp` | Remove prediction checkbox drawing |
| `src/types.lisp` | Remove prediction UI struct slots |
| `docs/ui.md` | Remove prediction UI references |
| `docs/config-client.md` | Update default value |

---

## Testing

1. `make tests` - all tests must pass
2. Manual test:
   - Start server and client
   - Walk around - should feel smooth (60fps local movement)
   - Verify no misprediction spam in verbose logs
   - ESC menu should no longer show prediction checkbox

---

## Rollback

If issues arise, set `*client-prediction-enabled*` back to `nil` via SLIME or revert config change. The UI removal is cosmetic and doesn't affect functionality.
