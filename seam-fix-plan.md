# Tile Seam Fix Plan - Final Solution

## Analysis Comparison

### Codex Analysis Key Points:
- Render-texture vertical flip (`negative height`) + subpixel Y movement causes Y-only alignment errors
- Camera might snap X but not Y (or vice versa), creating directional seams
- Mixed cached/uncached paths in same frame cause seams at boundaries
- Suggested: snap camera to integers or 0.5 increments matching tile scale

### Claude Analysis Key Points:
- Row-by-row iteration (Y outer, X inner) explains why seams are now horizontal
- Float vs integer math mismatch between fallback and cached paths
- Frame timing during fast movement - camera moves between row draws
- Pre-render separation (Phase B) eliminated mid-frame GL state changes

### Convergent Diagnosis:
**Sub-pixel coordinate alignment** is the root cause. Both analyses agree on:
1. Camera position must be pixel-aligned before rendering
2. Chunk draw positions must be integer-aligned
3. Fallback path must match cached path coordinate precision
4. The render-texture Y-flip is a specific source of Y-axis errors

---

## Fix Phases (Diagnosis-First Approach)

### Phase 1: Diagnostic Instrumentation (FIRST - Prove the Cause)

**Goal:** Prove whether seams correlate with cached/uncached mixing or fractional camera movement.

**1a. Per-frame cached vs fallback counters:**
```lisp
(defvar *frame-cached-draws* 0)
(defvar *frame-fallback-draws* 0)

;; In draw-cached-chunk, when texture found:
(incf *frame-cached-draws*)

;; In draw-cached-chunk, when fallback triggered:
(incf *frame-fallback-draws*)

;; At frame end, log if mixed:
(when (and (> *frame-cached-draws* 0) (> *frame-fallback-draws* 0))
  (format t "~&[SEAM-DIAG] MIXED FRAME cached:~D fallback:~D~%"
          *frame-cached-draws* *frame-fallback-draws*))
```

**1b. Camera fractional position logging:**
```lisp
;; Log when camera has sub-pixel position:
(let ((cam-x (raylib:camera-2d-target-x camera))
      (cam-y (raylib:camera-2d-target-y camera)))
  (when (or (/= cam-x (round cam-x))
            (/= cam-y (round cam-y)))
    (format t "~&[SEAM-DIAG] FRACTIONAL-CAM x:~,3F y:~,3F~%"
            cam-x cam-y)))
```

**1c. Video capture + log timestamps:**
- Run client with diagnostics enabled
- Record screen while moving rapidly
- Correlate seam events in video with log timestamps

**Verification:** Do seams appear ONLY when:
- Mixed cached/fallback frames occur? → Fix Phase 4
- Camera has fractional position? → Fix Phase 2
- Both? → Fix both

---

### Phase 2: Integer Alignment (Camera + Chunk Positions)

**Goal:** Enforce integer alignment for camera and all chunk draw positions.

**2a. Camera pixel snapping:**

**Location:** `src/rendering.lisp` - before any world drawing

```lisp
(defun snap-camera-to-pixels (camera)
  "Round camera target to integer pixels to prevent sub-pixel seams."
  (setf (raylib:camera-2d-target-x camera)
        (fround (raylib:camera-2d-target-x camera)))
  (setf (raylib:camera-2d-target-y camera)
        (fround (raylib:camera-2d-target-y camera))))
```

**Call site:** In `draw-game` or `prepare-game-render-caches`, before any world drawing.

**2b. Integer chunk positions:**

**Location:** `src/rendering.lisp:draw-cached-chunk`, `draw-cached-chunk-with-offset`

```lisp
;; Ensure chunk world positions are exact integers
(world-x (float (round (* chunk-x chunk-pixel-size))))
(world-y (float (round (* chunk-y chunk-pixel-size))))
```

**2c. Fallback path alignment:**

**Location:** `src/rendering.lisp:draw-chunk-tiles-direct`

```lisp
;; Match cached path precision exactly
(chunk-world-x (float (round (+ (* chunk-x chunk-pixel-size) world-offset-x))))
(chunk-world-y (float (round (+ (* chunk-y chunk-pixel-size) world-offset-y))))
```

---

### Phase 3: Force Point Filtering

**Goal:** Explicitly force point filtering on render textures to rule out sampler defaults.

**Location:** `src/config-client.lisp`

**Check:** Ensure `*tile-point-filter*` is `t`.

**Code verification in `render-chunk-to-texture`:**
```lisp
(raylib:set-texture-filter (raylib:render-texture-2d-texture texture)
                           0)  ; 0 = TEXTURE_FILTER_POINT (always)
```

Consider hardcoding `0` instead of checking `*tile-point-filter*` to guarantee point filtering for chunk textures regardless of config.

---

### Phase 4: Eliminate Mixed-Path Frames

**Goal:** Never mix cached and uncached draws in the same frame.

**Option A - Skip uncached chunks:**
```lisp
;; In draw-cached-chunk, if no texture:
(unless texture
  (incf *render-cache-stats-misses*)
  (return-from draw-cached-chunk))  ; Skip entirely, don't fallback
```
Trade-off: May show blank areas briefly, but no seams.

**Option B - Pre-render larger viewport buffer:**
```lisp
;; In prepare-zone-chunks, expand view bounds:
(let ((buffer-pixels (* 2 chunk-pixel-size)))  ; 2 chunks extra
  (prepare-zone-chunks zone tile-dest-size
                       (- view-left buffer-pixels)
                       (+ view-right buffer-pixels)
                       (- view-top buffer-pixels)
                       (+ view-bottom buffer-pixels)
                       editor assets))
```
Trade-off: More pre-render work, but chunks always ready.

**Option C - Movement-predictive pre-render:**
```lisp
;; Pre-render in direction of movement
(when (> player-dx 0) (incf view-right (* 2 chunk-pixel-size)))
(when (< player-dx 0) (decf view-left (* 2 chunk-pixel-size)))
(when (> player-dy 0) (incf view-bottom (* 2 chunk-pixel-size)))
(when (< player-dy 0) (decf view-top (* 2 chunk-pixel-size)))
```

---

## Implementation Order (Codex-Recommended)

```
1. DIAGNOSE FIRST
   └─ Phase 1: Add counters + logging
   └─ Capture video + correlate with logs
   └─ DETERMINE: Is it mixed paths? Fractional camera? Both?

2. FIX BASED ON DIAGNOSIS
   └─ If fractional camera: Phase 2a (camera snap)
   └─ If mixed paths: Phase 4 (eliminate mixing)
   └─ If both: Phase 2 + Phase 4

3. BELT AND SUSPENDERS
   └─ Phase 2b/2c: Integer chunk positions
   └─ Phase 3: Force point filtering

4. VERIFY
   └─ Re-run diagnostics
   └─ Confirm zero seam events in logs
   └─ Visual verification with video
```

---

## Quick-Fix Path (If You Want to Skip Diagnosis)

If confident in the analysis and want to just fix it:

1. **Camera snap** (Phase 2a) - 5 min, likely fixes 90%
2. **Force point filtering** (Phase 3) - 2 min, verify config
3. **Integer chunk positions** (Phase 2b/2c) - 10 min
4. **Test** - If seams persist, then add diagnostics

---

## Code Locations Summary

| File | Function | Phase |
|------|----------|-------|
| `rendering.lisp` | `draw-cached-chunk` | 1a, 2b, 4 |
| `rendering.lisp` | `draw-cached-chunk-with-offset` | 2b |
| `rendering.lisp` | `draw-chunk-tiles-direct` | 2c |
| `rendering.lisp` | `draw-game` / camera setup | 1b, 2a |
| `rendering.lisp` | `prepare-zone-chunks` | 4 (Option B/C) |
| `rendering.lisp` | `render-chunk-to-texture` | 3 |
| `config-client.lisp` | `*tile-point-filter*` | 3 |

---

## Expected Outcome

After full implementation:
- Zero seam tearing during normal gameplay
- Pixel-perfect chunk boundaries
- Consistent rendering between cached and fallback paths
- No sub-pixel bleeding or sampling artifacts
- Diagnostic data to prove the fix worked
