# Seam Tearing Analysis (Chunk Cache)
Date: 2026-01-23

## Observed behavior
- With chunk cache enabled: vertical seams mostly gone; rare, smaller horizontal seams appear during movement.
- With chunk cache disabled: vertical seams return and are more obvious.

## Likely causes for improvement
1) **Chunk pre-rendering reduces per-tile jitter.**
   - Rendering an entire chunk into a single texture eliminates per-tile draw rounding variance.
   - This explains why the seam direction changed and overall frequency dropped.

2) **Point filtering applied to render textures.**
   - Cached chunks likely use the same filtering path, reducing sampling artifacts between adjacent tiles.

3) **Stable layer composition.**
   - The chunk cache draws entire layers as a single bitmap. That avoids tile-by-tile precision errors, which often present as vertical seams.

## Why new horizontal seams might appear
A) **Render-texture vertical flip + subpixel Y movement.**
   - Cached chunks use a flipped source rect (negative height). If camera Y is fractional, the flip can introduce tiny alignment errors only on Y, showing as horizontal seams.

B) **Chunk origin and camera rounding mismatch.**
   - Chunk draws are world-positioned by chunk-pixel-size, but camera movement may be fractional. If the camera snaps X but not Y (or vice versa), seams can become directional.

C) **Fallback path mixing.**
   - If some chunks are missing and fall back to per-tile draw in the same frame, seams can appear at the boundary between cached and uncached regions. This is more noticeable while moving.

## Recommended verification steps
1) **Log cache fallback usage per frame.**
   - Confirm that cached and uncached tiles are not mixing along a seam line.
2) **Clamp camera target to integer pixels.**
   - Temporarily force integer camera coordinates and test if seams disappear.
3) **Force integer chunk draw positions.**
   - Ensure chunk world positions and dest rects are integers (or round the camera transform) to avoid half-pixel sampling.
4) **Check render texture filtering.**
   - Explicitly set point filtering on render textures if needed.

## Suggested fixes (if verification confirms)
- Snap camera X/Y to integers (or to 0.5 increments matching tile scale) before drawing cached chunks.
- Ensure cached chunk draw positions are integer-aligned in world space.
- Avoid mixed cached/uncached paths in a single frame: if a visible chunk is missing, either pre-render earlier or temporarily disable draw of that chunk until ready.

## Conclusion
The chunk cache is very likely the reason vertical seams largely vanished. The remaining horizontal seams are consistent with subpixel Y alignment or mixed cached/uncached draw paths. The fixes above should isolate and eliminate the remaining artifacts.
