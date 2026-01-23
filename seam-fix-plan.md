#############################################################################################
ABORTED PLAN AFTER PHASE 1 BECAUSE THERE ARE TRADEOFFS AND PHASE 1 WAS THE FIX WE NEEDED.
tradeoff notes for phase 2-5 if we revisit in teh future:
Step 2 (pre‑render buffer): pre‑renders extra chunks around the viewport so the draw pass never falls back to per‑tile. This prevents mixed cached/uncached seams, but costs more CPU per frame and a bit more VRAM.
Step 3 (force point filtering on chunk textures): guarantees no texture blending across chunk edges. It can make chunks look more pixelated even if you prefer smoother filtering.
Step 4 (integer chunk positions): locks chunk draw positions to exact pixels. It’s a belt‑and‑suspenders fix for subpixel seams, but can slightly reduce subpixel smoothness when panning.
Step 5 (diagnostics): just logging to prove what’s happening; no gameplay change.
################################################################################################################################
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

**Note:** All diagnostics gated by `*debug-render-cache*` per CLAUDE.md logging discipline.

**1a. Per-frame cached vs fallback counters:**
```lisp
(defvar *frame-cached-draws* 0)
(defvar *frame-fallback-draws* 0)

;; In draw-cached-chunk, when texture found:
(incf *frame-cached-draws*)

;; In draw-cached-chunk, when fallback triggered:
(incf *frame-fallback-draws*)

;; At frame end (in log-cache-stats or draw-game), log if mixed:
(when *debug-render-cache*
  (when (and (> *frame-cached-draws* 0) (> *frame-fallback-draws* 0))
    (log-verbose "[SEAM-DIAG] MIXED FRAME cached:~D fallback:~D"
                 *frame-cached-draws* *frame-fallback-draws*)))
```

**1b. Camera fractional position logging:**
```lisp
;; Log when camera has sub-pixel screen position (zoom-aware):
(when *debug-render-cache*
  (let* ((cam-x (camera-target-x camera))  ; Use correct accessor for your camera struct
         (cam-y (camera-target-y camera))
         (zoom (camera-zoom camera))
         (screen-x (* cam-x zoom))
         (screen-y (* cam-y zoom)))
    (when (or (/= screen-x (round screen-x))
              (/= screen-y (round screen-y)))
      (log-verbose "[SEAM-DIAG] FRACTIONAL-SCREEN x:~,3F y:~,3F (world ~,3F,~,3F)"
                   screen-x screen-y cam-x cam-y))))
```

**1c. Video capture + log timestamps:**
- Enable `*debug-render-cache*`
- Run client with diagnostics enabled
- Record screen while moving rapidly
- Correlate seam events in video with log timestamps

**Verification:** Do seams appear ONLY when:
- Mixed cached/fallback frames occur? → Fix Phase 4
- Camera has fractional screen position? → Fix Phase 2
- Both? → Fix both

---

### Phase 2: Integer Alignment (Camera + Chunk Positions)

**Goal:** Enforce integer alignment for camera and all chunk draw positions.

**2a. Camera screen-pixel snapping:**

**Important:** Rounding to integer world coords can still produce sub-pixel screen alignment at non-1.0 zoom. Use screen-pixel snapping:

**Location:** `src/rendering.lisp` - before any world drawing

```lisp
(defun snap-camera-to-screen-pixels (camera)
  "Round camera target to screen-pixel alignment at current zoom.
   Formula: cam' = (round (* cam zoom)) / zoom
   This preserves pixel alignment at any zoom level."
  ;; NOTE: Verify camera is mutable raylib camera or use correct accessors
  ;; for your wrapper struct. Direct setf on raylib accessors may be no-op
  ;; if camera is wrapped/cached.
  (let ((zoom (raylib:camera-2d-zoom camera)))
    (when (> zoom 0.0)
      (setf (raylib:camera-2d-target-x camera)
            (/ (fround (* (raylib:camera-2d-target-x camera) zoom)) zoom))
      (setf (raylib:camera-2d-target-y camera)
            (/ (fround (* (raylib:camera-2d-target-y camera) zoom)) zoom)))))
```

**Accessor verification checklist:**
- [ ] Confirm `raylib:camera-2d-target-x` is setf-able on your camera object
- [ ] If camera is wrapped in a game struct, use the wrapper's accessors
- [ ] Test with `(setf ... ) (assert (= ... ))` to verify mutation works

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

**Location:** `src/rendering.lisp:render-chunk-to-texture`

**Current code:**
```lisp
(raylib:set-texture-filter (raylib:render-texture-2d-texture texture)
                           (if *tile-point-filter* 0 1))
```

**Fix:** Use named constant, not magic number:
```lisp
(raylib:set-texture-filter (raylib:render-texture-2d-texture texture)
                           raylib:+texture-filter-point+)  ; Always point filter for chunks
```

**Rationale:** Hardcode point filtering for chunk textures regardless of `*tile-point-filter*` config. Chunk boundaries must never blend. The config can still control other texture filtering if needed.

**Verification:** Check that `raylib:+texture-filter-point+` exists in your claw-raylib bindings. If not, define locally:
```lisp
(defconstant +texture-filter-point+ 0 "Point filtering (nearest neighbor)")
```

---

### Phase 4: Eliminate Mixed-Path Frames

**Goal:** Never mix cached and uncached draws in the same frame.

**Option A - Skip uncached chunks (NOT RECOMMENDED):**
```lisp
;; In draw-cached-chunk, if no texture:
(unless texture
  (incf *render-cache-stats-misses*)
  (return-from draw-cached-chunk))  ; Skip entirely, don't fallback
```
**WARNING:** This causes black holes where chunks are missing. Only use if you implement a placeholder fill (solid color rectangle) or are certain pre-render always succeeds.

**Option B - Pre-render larger viewport buffer (RECOMMENDED):**
```lisp
;; In prepare-game-render-caches, expand view bounds before calling prepare-zone-chunks:
(let* ((chunk-pixel-size (zone-render-cache-chunk-pixel-size cache))
       (buffer-pixels (* 2 chunk-pixel-size)))  ; 2 chunks extra in each direction
  ;; Expand bounds
  (let ((buffered-left (- view-left buffer-pixels))
        (buffered-right (+ view-right buffer-pixels))
        (buffered-top (- view-top buffer-pixels))
        (buffered-bottom (+ view-bottom buffer-pixels)))
    ;; IMPORTANT: Re-apply Phase C bounds clamping after expansion
    ;; to avoid negative/OOB chunk creation
    (prepare-zone-chunks zone cache
                         (max 0.0 buffered-left)    ; Clamp to zone bounds
                         buffered-right              ; Will be clamped inside prepare-zone-chunks
                         (max 0.0 buffered-top)
                         buffered-bottom
                         tile-dest-size editor assets)))
```
**Trade-off:** More pre-render work per frame, but chunks always ready. The Phase C clamping inside `prepare-zone-chunks` handles upper bounds.

**Option C - Movement-predictive pre-render:**
```lisp
;; Pre-render extra chunks in direction of movement only
(let ((player-dx (player-dx player))
      (player-dy (player-dy player))
      (buffer (* 2 chunk-pixel-size)))
  (let ((pred-left (if (< player-dx 0) (- view-left buffer) view-left))
        (pred-right (if (> player-dx 0) (+ view-right buffer) view-right))
        (pred-top (if (< player-dy 0) (- view-top buffer) view-top))
        (pred-bottom (if (> player-dy 0) (+ view-bottom buffer) view-bottom)))
    ;; IMPORTANT: Re-apply Phase C bounds clamping
    (prepare-zone-chunks zone cache
                         (max 0.0 pred-left)
                         pred-right
                         (max 0.0 pred-top)
                         pred-bottom
                         tile-dest-size editor assets)))
```
**Trade-off:** Less pre-render than Option B, but only helps if movement is sustained in one direction.

**Recommendation:** Start with Option B (simple, robust). If performance is a concern, switch to Option C.

---

## Implementation Order (Codex-Recommended)

```
1. DIAGNOSE FIRST
   └─ Phase 1: Add counters + logging (gated by *debug-render-cache*)
   └─ Capture video + correlate with logs
   └─ DETERMINE: Is it mixed paths? Fractional camera? Both?

2. FIX BASED ON DIAGNOSIS
   └─ If fractional camera: Phase 2a (screen-pixel camera snap)
   └─ If mixed paths: Phase 4 Option B (pre-render buffer)
   └─ If both: Phase 2 + Phase 4

3. BELT AND SUSPENDERS
   └─ Phase 2b/2c: Integer chunk positions
   └─ Phase 3: Force point filtering (use named constant)

4. VERIFY
   └─ Re-run diagnostics with *debug-render-cache* enabled
   └─ Confirm zero MIXED FRAME / FRACTIONAL-SCREEN logs
   └─ Visual verification with video
```

---

## Quick-Fix Path (If You Want to Skip Diagnosis)

If confident in the analysis and want to just fix it:

1. **Screen-pixel camera snap** (Phase 2a) - likely fixes 90%
2. **Pre-render buffer** (Phase 4 Option B) - eliminates mixed paths
3. **Force point filtering** (Phase 3) - use `raylib:+texture-filter-point+`
4. **Integer chunk positions** (Phase 2b/2c) - belt and suspenders
5. **Test** - If seams persist, enable diagnostics

---

## Code Locations Summary

| File | Function | Phase |
|------|----------|-------|
| `rendering.lisp` | `draw-cached-chunk` | 1a, 2b, 4 |
| `rendering.lisp` | `draw-cached-chunk-with-offset` | 2b |
| `rendering.lisp` | `draw-chunk-tiles-direct` | 2c |
| `rendering.lisp` | `draw-game` / camera setup | 1b, 2a |
| `rendering.lisp` | `prepare-game-render-caches` | 4 (Option B/C) |
| `rendering.lisp` | `prepare-zone-chunks` | bounds clamping |
| `rendering.lisp` | `render-chunk-to-texture` | 3 |
| `config-client.lisp` | `*debug-render-cache*` | 1 (logging gate) |

---

## Implementation Notes

### Logging Discipline (per CLAUDE.md)
- All diagnostic output gated by `*debug-render-cache*`
- Use `log-verbose` not raw `format t`
- Keep diagnostics useful for future debugging sessions

### Bounds Clamping (per Phase C)
- Any view bounds expansion (Phase 4) must respect zone boundaries
- `prepare-zone-chunks` already clamps internally, but pre-clamp negatives
- Never create chunks at negative or out-of-bounds coordinates

### Camera Accessor Verification
- Before implementing Phase 2a, verify `setf` works on your camera object
- If using a wrapper struct, update the wrapper's accessors
- Test mutation with assert to catch silent failures

---

## Expected Outcome

After full implementation:
- Zero seam tearing during normal gameplay
- Pixel-perfect chunk boundaries at any zoom level
- Consistent rendering between cached and fallback paths
- No sub-pixel bleeding or sampling artifacts
- Diagnostic data to prove the fix worked
- No regression on Phase C bounds clamping
