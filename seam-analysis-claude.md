# Tile Seam Tearing Analysis

## Observation

After the chunk cache fixes, tile seam tearing has almost completely disappeared:
- **Before:** Vertical tears, quite noisy
- **After:** Horizontal only, smaller, more rare, mostly when running across the map

The direction shift confirms the issue is not the tile images themselves.

---

## Most Likely Cause of Improvement: Phase B (Pre-render Separation)

**Before:** `get-or-render-chunk` was called during the draw pass, which could:
- Allocate FBOs mid-frame (GL state changes during drawing)
- Cause timing inconsistencies where adjacent chunks were at different render states

**After:** All FBO allocation happens in `prepare-game-render-caches` *before* `begin-drawing`. The draw pass now only blits pre-existing textures - no GL state churn.

---

## Why Vertical → Horizontal?

The draw loop iterates row-by-row (Y outer, X inner):

```lisp
(loop :for cy :from start-chunk-y :to end-chunk-y
      :do (loop :for cx :from start-chunk-x :to end-chunk-x ...))
```

If there's any sub-pixel drift or timing issue, it would now accumulate across rows (horizontal seams) rather than being caused by mid-frame FBO allocation (which could have affected columns randomly).

---

## Possible Remaining Causes

### 1. Floating Point Camera Position
Camera might be at sub-pixel coordinates, causing slight misalignment between chunk rows when transformed through the camera matrix.

### 2. Per-tile Fallback Path Mismatch
The fallback uses integer math while the cached path uses floats. If fallback ever triggers during movement, coordinates could differ slightly:

```lisp
;; Fallback (integer-ish):
(chunk-world-x (+ (* chunk-x chunk-pixel-size) world-offset-x))
:for dest-x = (+ chunk-world-x (* local-x tile-dest-size))

;; Cached path (floats):
(world-x (* chunk-x chunk-pixel-size))
(dest-rect (raylib:make-rectangle :x (float world-x) ...))
```

### 3. Frame Timing During Fast Movement
Fast movement during row iteration means camera has moved slightly by the time next row draws, causing row-to-row offset.

### 4. Texture Filtering
Render textures have filtering applied via `*tile-point-filter*`. If using bilinear (nil), chunk edges could blend slightly differently than expected.

---

## Potential Fixes to Investigate

1. **Snap camera to integer pixels** before rendering
2. **Check `*tile-point-filter*`** - ensure point filtering (0) for pixel-perfect alignment
3. **Log per-tile fallback triggers** to see if they correlate with tears
4. **Ensure chunk positions are pixel-aligned** in both cached and fallback paths
5. **Round tile-dest-size calculations** to exact integers

---

## Related Code Locations

- `src/rendering.lisp:draw-cached-chunk` - cached chunk drawing
- `src/rendering.lisp:draw-chunk-tiles-direct` - per-tile fallback
- `src/rendering.lisp:draw-world-cached` - main draw loop (row-by-row iteration)
- `src/config-client.lisp:*tile-point-filter*` - texture filtering setting
