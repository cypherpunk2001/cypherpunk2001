# Unified Chunk Cache Fix Plan (Revised)

**Date:** 2026-01-23 (Revised)
**Scope:** Chunk render cache (Phase 1-2 from findings_plan.md), preview zones, movement-driven artifacts
**Status:** Analysis revised with debug session data (2026-01-23)

---

## Executive Summary

The chunk cache has bugs causing black screen flashes and tile glitches. Debug session data (see Appendix) reveals the actual issues:

1. **Cache lookup failure during movement** - Same chunks created 20-27× each (DATA: 1030 CREATE events in 10s session)
2. **Mid-frame FBO creation** - Render target switches during draw loop cause black flash (DATA: FBO logs interleaved with CREATE)
3. **Per-zone cache isolation** - Each zone (main + up to 8 previews) has separate cache; cache-size:90 = sum across zones
4. **Layer iteration asymmetry** - WALLS layer has ~2× more duplicates than FLOOR (DATA: 26× vs 11× avg)

**Key insight from data:** Steady state shows 100% cache hits. The bug only manifests during movement/zone transitions. This suggests cache entries are being lost or keys are changing between frames.

**Revised fix strategy:**
1. Diagnose WHY cache lookups fail (key instability? cache clearing?)
2. Fix the root cause before refactoring to pre-render step
3. Clamp bounds to prevent out-of-bounds chunk generation

---

## Corrected Math: Visible Chunk Counts

### Actual Configuration Values
```lisp
*tile-size*         = 16 pixels
*tile-scale*        = 4.0
*render-chunk-size* = 16 tiles
```

### Derived Values
```
tile-dest-size    = 16 × 4.0 = 64 pixels
chunk-pixel-size  = 16 × 64  = 1024 pixels
```

### How to Calculate Visible Chunks (Apply to Your Config)

**Formula:**
```
tile-dest-size   = *tile-size* × *tile-scale*
chunk-pixel-size = *render-chunk-size* × tile-dest-size
viewport-world-w = window-width / zoom
viewport-world-h = window-height / zoom
chunks-x         = ceiling(viewport-world-w / chunk-pixel-size)
chunks-y         = ceiling(viewport-world-h / chunk-pixel-size)
visible-chunks   = chunks-x × chunks-y × layer-count
```

**Before deciding cache is too small, calculate with your actual values:**
- Check `*tile-size*` and `*tile-scale*` in `config.lisp`
- Count layers in your zone file (grep for `:ID :` in zone data)
- Apply the formula above

### Example: Current Config (1280×720 window)

| Zoom | Viewport (world px) | Chunks per layer | With 3 layers |
|------|---------------------|------------------|---------------|
| 1.0  | 1280×720           | 2×1 = 2          | 6             |
| 0.7  | 1829×1029          | 2×2 = 4          | 12            |
| 0.5  | 2560×1440          | 3×2 = 6          | 18            |

**Conclusion:** With current config (16px tiles, 4.0 scale, 16-tile chunks → 1024px chunks), even at maximum zoom-out with 3 layers, only ~18 chunks are visible. The default cache of 64 is **more than sufficient**. Cache capacity is NOT the root cause.

### Zone Dimensions (zone-1.lisp)
```lisp
:WIDTH 64 :HEIGHT 64  ; tiles
; Zone extent in world pixels: 64 × 64 = 4096×4096 pixels
; Zone extent in chunks: 64/16 = 4×4 = 16 total chunks per layer
```

---

## Revised Bug Analysis (Updated with Debug Data)

### Bug #1: Cache Lookup Failure During Movement (CRITICAL - NEW FROM DATA)

**Evidence from debug session:**
- Same chunk coordinates created 20-27× each (e.g., `(2,-2) layer:WALLS` created 27×)
- Total 1030 CREATE events in ~10 second session (expected ~100 for cold start + movement)
- Steady state shows 100% hit rate - cache WORKS when stationary
- Failure only occurs during movement/zone transitions

**Possible causes (to investigate):**
1. Cache key instability - key changes between frames for same logical chunk
2. Preview zone cache thrashing - zones unloaded/reloaded clear their caches
3. Hash table corruption - unlikely given steady state works
4. Multiple code paths requesting same chunk with different cache instances

**Investigation needed:** Add logging to trace exact key values and cache instance identity.

### Bug #2: Mid-Frame FBO Creation (CRITICAL - CONFIRMED BY DATA)

**Location:** `rendering.lisp:438-468` in `render-chunk-to-texture`

**Problem:** FBOs are created on-demand inside the draw loop.

**Evidence from debug session:** FBO creation logs interleaved with CREATE events:
```
INFO: TEXTURE: [ID 109] Texture loaded successfully (1024x1024 ...)
INFO: FBO: [ID 70] Framebuffer object created successfully
[CACHE] CREATE chunk (X,Y) layer:WALLS
```

Each CREATE event triggers GPU allocation mid-frame. With 1030 CREATE events, this causes severe render target switching.

**Fix required:** Move ALL chunk creation to a pre-render step BEFORE `begin-drawing`.

### ~~Bug #3: Layer Iteration Asymmetry~~ (EXPLAINED BY A2 DATA)

**Evidence from debug session:**
- WALLS layer chunks: 21-27 duplicates each
- FLOOR layer chunks: 9-11 duplicates each
- Ratio: ~3× more WALLS duplicates

**Root cause (from A2):** WALLS layer uses 3 separate tilesets (OVERWORLD, CAVE, OBJECTS). Cache key is `(layer-id . tileset-id)`, so each tileset creates separate cache entries. The ~3× ratio is expected behavior, not a bug.

**Status:** No fix needed. Cache key design is correct.

### Bug #4: Out-of-Bounds Chunk Generation (MEDIUM - CONFIRMED BY DATA)

**Location:** `rendering.lisp:551-554` in `draw-world-cached`

**Evidence from debug session:**
- 218 CREATE events with negative X coordinates (e.g., `(-3,2)`, `(-2,-2)`)
- These are preview zone chunks, but negative coords appear in main zone iteration too

**Problem:** Chunk bounds calculated without clamping to zone extent.

**Fix required:** Clamp chunk ranges to zone bounds, skip out-of-bounds chunks entirely.

### Bug #5: Preview Zone Cache Thrashing (HIGH - INFERRED FROM DATA)

**Evidence from debug session:**
- cache-size:90 with per-zone limit of 64 → multiple zone caches active
- Negative coordinate duplicates suggest preview zones being reloaded
- Transitions cause burst of 30+ CREATE events

**Hypothesis:** Preview zones are unloaded when player moves away from edge, then reloaded when player returns. Each reload creates all chunks fresh.

**Fix required:** Either persist preview zone caches longer, or pre-warm them.

### Bug #6: Zero Evictions Despite High Activity (INVESTIGATE)

**Evidence from debug session:**
- 0 EVICT events
- cache-size reached 90 (but this is sum across zones)
- Per-zone caches may each be under 64

**Assessment:** Likely NOT a bug - per-zone caches staying under limit. However, verify eviction logic works by forcing a single zone to exceed 64 chunks.

### ~~Bug #7: Cache Capacity Too Small~~ (DISPROVEN)

With corrected math and per-zone caches, capacity is sufficient.

### ~~Bug #8: Frame Counter Issue~~ (NOT A BUG)

Per-zone frame counters are correct design.

---

## Symptoms Analysis (Updated with Data)

### Water/Collision Tiles "Freaking Out" - PARTIALLY EXPLAINED

**Data correlation:** WALLS layer (which includes water/collision) has 2× more duplicate CREATE events than FLOOR layer.

**Likely cause:** The layer iteration logic may be processing WALLS layer multiple times per frame (once as "normal" layer, once as "collision" layer). Each iteration triggers cache lookup with potentially different context.

**Remaining question:** Why does duplicate iteration cause visual glitches? Possibly the re-rendered chunk has slightly different content due to timing.

### Tiles Appearing/Disappearing - EXPLAINED

**Data correlation:** 1030 CREATE events = constant chunk re-creation. If a chunk is being created mid-frame while also being drawn, the texture may be in an incomplete state.

**Root cause:** Cache lookup failure (Bug #1) combined with mid-frame creation (Bug #2).

---

## Fix Plan (Revised Based on Data)

### Phase A: Instrumentation ✓ COMPLETED

Basic instrumentation added. Debug session captured actionable data.

### Phase A2: Diagnostic Enhancement (NEW - DO NEXT)

**Goal:** Determine root cause of cache lookup failure and duplicate CREATEs.

**A2.1: Add zone context to all cache logs (MUST-DO)**

Current logs don't include zone-id, so duplicates might be different zones using same chunk coordinates.

```lisp
;; Update CREATE log to include full context:
;; Note: zoom may not be available at cache call site; pass it down or use tile-dest-size as proxy
(when *debug-render-cache*
  (format t "~&[CACHE] CREATE zone:~A chunk:(~D,~D) layer:~A tileset:~A tile-dest:~D chunk-px:~D~%"
          (zone-id zone)
          chunk-x chunk-y
          (zone-layer-id layer)
          (zone-layer-tileset-id layer)
          tile-dest-size      ; already available at call site
          chunk-pixel-size))  ; derived from tile-dest-size × *render-chunk-size*

;; Update per-frame stats to show per-zone breakdown:
(format t "~&[CACHE] STATS zone:~A visible:~D cache-size:~D | ...~%"
        (zone-id zone)
        visible-count
        (hash-table-count (zone-render-cache-chunks cache))
        ...)
```

**A2.2: Log cache clears (MUST-DO)**

If cache clearing happens during movement/edge previews, it explains "duplicate CREATEs with zero evicts."

**Caller context values (use consistently):**
| Value | When Used |
|-------|-----------|
| `"zone-change"` | Player transitions to different zone |
| `"toggle-filter"` | User toggles tile point filter setting |
| `"toggle-cache"` | User toggles render cache on/off |
| `"editor-reload"` | Editor reloads zone data |
| `"manual"` | Explicit REPL/debug call |

```lisp
;; Add to clear-zone-render-cache:
(when *debug-render-cache*
  (format t "~&[CACHE] CLEAR zone:~A entries:~D caller:~A~%"
          zone-id
          (hash-table-count chunks)
          caller))  ; one of the values above

;; Add to clear-all-zone-render-caches:
(when *debug-render-cache*
  (format t "~&[CACHE] CLEAR-ALL zones:~D caller:~A~%"
          (hash-table-count *zone-render-caches*)
          caller))

;; Add to clear-other-zone-render-caches:
(when *debug-render-cache*
  (format t "~&[CACHE] CLEAR-OTHERS keeping:~A caller:~A~%"
          keep-zone-id
          caller))
```

**A2.3: One-time runtime config log (MUST-DO)**

Visible count of 48-75 seems high if chunks are 1024px. Need to confirm actual values.

```lisp
;; Log once at startup or first cache use:
(format t "~&[CACHE] CONFIG tile-dest-size:~D chunk-pixel-size:~D chunk-tiles:~D max-chunks:~D~%"
        tile-dest-size
        chunk-pixel-size
        *render-chunk-size*
        *render-cache-max-chunks*)
```

If `tile-dest-size` is 16 (scale 1.0) instead of 64 (scale 4.0), earlier math was wrong and cache may be under pressure.

**Questions to answer:**
1. Are duplicate CREATEs from same zone or different zones (main vs preview)?
2. Is any cache clear function being called during movement?
3. What are the actual runtime chunk dimensions?

### ~~Phase A3: Layer Iteration Audit~~ (REMOVED)

**Status:** REMOVED - A2 data answered this question.

**Original goal:** Determine why WALLS has 2× more duplicates than FLOOR.

**Answer from A2:** WALLS layer uses 3 separate tilesets (OVERWORLD, CAVE, OBJECTS). Cache key includes `(layer-id . tileset-id)`, so these are correctly 3 separate cache entries. The ~3× ratio vs FLOOR is expected behavior, not a bug.

### Phase X1: Fix Zone-Change Clear Behavior

**Goal:** Stop clearing destination zone cache on zone transitions.

**Evidence from A2:** `CLEAR-ALL zones:4 caller:zone-change` clears ALL zones including destination, forcing complete rebuild.

**Fix:** Change `clear-all-zone-render-caches` to `clear-other-zone-render-caches` in the zone-change hook.

```lisp
;; BEFORE (current behavior - clears everything):
(clear-all-zone-render-caches "zone-change")

;; AFTER (keep destination warm):
(clear-other-zone-render-caches destination-zone-id "zone-change")
```

**Prerequisites (if not already done in A2):**
1. Ensure `clear-other-zone-render-caches` accepts `(keep-zone-id &optional caller)` parameter
2. Ensure `clear-all-zone-render-caches` accepts `(&optional caller)` parameter
3. If these functions don't accept caller, add the parameter (A2.2 should have done this)

**Location:** Find the zone-change hook in `rendering.lisp` that calls the clear function. This is likely `*client-zone-change-hook*` or a function called from zone transition logic.

### Phase X2: Zone-ID Normalization Verification

**Goal:** Confirm zone-id is stable and cache lookups use consistent keys.

**Evidence from A2:** Same cache key `(ZONE-4, (-2,-2), WALLS, OVERWORLD)` created twice WITHOUT any CLEAR between (lines 1884→2231). Cache lookup is failing.

**Possible causes:**
1. Zone-id type mismatch (symbol vs string vs keyword)
2. Multiple cache instances for same zone-id
3. Hash table test function mismatch (eql vs equal)

**X2.1: Add zone-id diagnostic logging**

```lisp
;; In get-or-create-zone-render-cache, AFTER retrieving/creating the cache:
(defun get-or-create-zone-render-cache (zone-id tile-dest-size)
  (let* ((normalized-id (normalize-zone-id zone-id))
         (cache (or (gethash normalized-id *zone-render-caches*)
                    (setf (gethash normalized-id *zone-render-caches*)
                          (make-zone-render-cache ...)))))
    ;; Log AFTER cache is available:
    (when *debug-render-cache*
      (format t "~&[CACHE] LOOKUP zone-id:~S normalized:~S type:~A cache-id:~D~%"
              zone-id
              normalized-id
              (type-of zone-id)
              (sxhash cache)))
    cache))
```

**X2.2: Add cache instance identity to CREATE logs**

```lisp
;; In get-or-render-chunk (or ensure-chunk-cached):
(when *debug-render-cache*
  (format t "~&[CACHE] CREATE zone:~S cache-id:~D chunk:(~D,~D) layer:~A tileset:~A~%"
          (zone-id zone)
          (sxhash cache)  ; detect if different cache instances
          chunk-x chunk-y
          (zone-layer-id layer)
          (zone-layer-tileset-id layer)))
```

**X2.3: Verify hash table test function**

Check that `*zone-render-caches*` uses appropriate test:
```lisp
;; If zone-ids can be strings or symbols:
(defvar *zone-render-caches* (make-hash-table :test 'equal))

;; If zone-ids are always keywords (preferred):
(defvar *zone-render-caches* (make-hash-table :test 'eql))
```

**X2.4: Normalize zone-id at cache boundary**

Add a normalization helper (used in X2.1's updated `get-or-create-zone-render-cache`):

```lisp
(defun normalize-zone-id (zone-id)
  "Ensure zone-id is always a keyword for consistent hash table lookup."
  (etypecase zone-id
    (keyword zone-id)
    (symbol (intern (symbol-name zone-id) :keyword))
    (string (intern zone-id :keyword))))
```

This normalization is already integrated into the X2.1 example above.

**Validation:** After X2 changes, re-run A2 diagnostics. If cache-id values are consistent and no duplicate CREATEs without CLEAR, the lookup bug is fixed.

### Phase B: Pre-Render Step (Core Fix for Black Flash)

**Goal:** Move ALL chunk creation outside the draw pass.

**IMPORTANT:** This phase should only be done AFTER Phases X1 and X2 fix the cache lookup failure. If we move creation to pre-render but cache lookup still fails, we'll just do the same 1030 FBO allocations earlier in the frame.

**Prerequisite:** Bug #1 (cache lookup failure) must be understood and fixed first. X2 validation must pass.

**Principle:**
```
Frame Timeline (CORRECT):
┌─────────────────────────────────────────────────────────────────┐
│ 1. PRE-RENDER PHASE (before raylib:begin-drawing)               │
│    - Calculate visible chunk bounds (clamped to zone)           │
│    - For each needed chunk: create texture if missing           │
│    - All render target switches happen HERE                     │
├─────────────────────────────────────────────────────────────────┤
│ 2. DRAW PHASE (inside raylib:begin-drawing / begin-mode-2d)     │
│    - Iterate chunks, blit cached textures ONLY                  │
│    - NEVER call load-render-texture or begin-texture-mode       │
│    - If chunk not ready, use per-tile fallback (existing code)  │
└─────────────────────────────────────────────────────────────────┘
```

**Step B.1: Add pre-render function**

```lisp
(defun prepare-zone-chunks (zone cache view-left view-right view-top view-bottom
                            tile-dest-size editor assets)
  "Pre-render all visible chunks for ZONE. Call BEFORE begin-drawing.
   Clamps chunk bounds to zone extents to avoid out-of-bounds generation."
  (let* ((chunk-pixel-size (zone-render-cache-chunk-pixel-size cache))
         (zone-width-chunks (ceiling (zone-width zone) *render-chunk-size*))
         (zone-height-chunks (ceiling (zone-height zone) *render-chunk-size*))
         ;; Clamp to zone bounds
         (start-x (max 0 (floor view-left chunk-pixel-size)))
         (end-x (min (1- zone-width-chunks) (ceiling view-right chunk-pixel-size)))
         (start-y (max 0 (floor view-top chunk-pixel-size)))
         (end-y (min (1- zone-height-chunks) (ceiling view-bottom chunk-pixel-size))))
    (when (and (<= start-x end-x) (<= start-y end-y))
      (loop :for layer :across (zone-layers zone)
            :do (loop :for cy :from start-y :to end-y
                      :do (loop :for cx :from start-x :to end-x
                                :do (ensure-chunk-cached cache zone layer cx cy
                                                          tile-dest-size editor assets)))))))

(defun ensure-chunk-cached (cache zone layer chunk-x chunk-y tile-dest-size editor assets)
  "Ensure a chunk is cached. Creates texture if needed. Safe to call outside draw loop."
  (let* ((key (chunk-cache-key layer chunk-x chunk-y))
         (chunks (zone-render-cache-chunks cache))
         (entry (gethash key chunks)))
    ;; Evict if at capacity (existing logic)
    (when (and (null entry)
               (>= (hash-table-count chunks) *render-cache-max-chunks*))
      (evict-lru-chunk cache))
    ;; Create entry if missing
    (unless entry
      (setf entry (%make-render-chunk-cache
                   :chunk-x chunk-x
                   :chunk-y chunk-y
                   :layer-key (cons (zone-layer-id layer) (zone-layer-tileset-id layer))
                   :dirty t
                   :last-access (zone-render-cache-frame-counter cache)))
      (setf (gethash key chunks) entry)
      (log-cache-event "CREATE chunk (~D,~D) layer ~A" chunk-x chunk-y (zone-layer-id layer))
      (incf *render-cache-stats-created*))
    ;; Render if dirty or no texture
    (when (or (render-chunk-cache-dirty entry)
              (null (render-chunk-cache-texture entry)))
      (when (render-chunk-cache-texture entry)
        (raylib:unload-render-texture (render-chunk-cache-texture entry)))
      (setf (render-chunk-cache-texture entry)
            (render-chunk-to-texture zone layer chunk-x chunk-y tile-dest-size editor assets))
      (setf (render-chunk-cache-dirty entry) nil))
    ;; Update LRU
    (setf (render-chunk-cache-last-access entry) (zone-render-cache-frame-counter cache))))
```

**Step B.2: Modify draw-world-cached to be draw-only**

```lisp
(defun draw-world-cached-fast (zone cache chunk-pixel-size)
  "Draw all cached chunks. MUST call prepare-zone-chunks first.
   Does NOT create textures - uses per-tile fallback for missing chunks."
  (let* ((zone-layers (zone-layers zone))
         (zone-width-chunks (ceiling (zone-width zone) *render-chunk-size*))
         (zone-height-chunks (ceiling (zone-height zone) *render-chunk-size*)))
    ;; Increment frame counter once per frame per zone
    (incf (zone-render-cache-frame-counter cache))
    ;; Draw all chunks in zone (only valid chunks exist due to clamping in prepare)
    (labels ((draw-layer-fast (layer)
               (loop :for cy :from 0 :below zone-height-chunks
                     :do (loop :for cx :from 0 :below zone-width-chunks
                               :do (draw-chunk-or-fallback cache zone layer cx cy
                                                            chunk-pixel-size)))))
      ;; Layer order: normal -> collision -> object
      (loop :for layer :across zone-layers
            :when (and (not (zone-layer-collision-p layer))
                       (not (eql (zone-layer-id layer) *editor-object-layer-id*)))
            :do (draw-layer-fast layer))
      (loop :for layer :across zone-layers
            :when (zone-layer-collision-p layer)
            :do (draw-layer-fast layer))
      (loop :for layer :across zone-layers
            :when (and (not (zone-layer-collision-p layer))
                       (eql (zone-layer-id layer) *editor-object-layer-id*))
            :do (draw-layer-fast layer)))))

(defun draw-chunk-or-fallback (cache zone layer chunk-x chunk-y chunk-pixel-size)
  "Draw cached chunk texture, or fall back to per-tile if not ready."
  (let* ((key (chunk-cache-key layer chunk-x chunk-y))
         (entry (gethash key (zone-render-cache-chunks cache))))
    (if (and entry (render-chunk-cache-texture entry))
        ;; Draw cached texture
        (let* ((texture (render-chunk-cache-texture entry))
               (world-x (* chunk-x chunk-pixel-size))
               (world-y (* chunk-y chunk-pixel-size))
               (tex (raylib:render-texture-2d-texture texture))
               (tex-w (float (raylib:texture-2d-width tex)))
               (tex-h (float (raylib:texture-2d-height tex))))
          (incf *render-cache-stats-hits*)
          (setf (render-chunk-cache-last-access entry) (zone-render-cache-frame-counter cache))
          (raylib:draw-texture-pro tex
                                   (raylib:make-rectangle :x 0.0 :y tex-h
                                                          :width tex-w :height (- tex-h))
                                   (raylib:make-rectangle :x (float world-x) :y (float world-y)
                                                          :width (float chunk-pixel-size)
                                                          :height (float chunk-pixel-size))
                                   (raylib:make-vector2 :x 0.0 :y 0.0)
                                   0.0
                                   raylib:+white+))
        ;; Fallback: per-tile rendering (use existing helpers)
        (progn
          (incf *render-cache-stats-misses*)
          (draw-chunk-tiles-fallback zone layer chunk-x chunk-y chunk-pixel-size)))))
```

**Step B.3: Fallback function using existing helpers**

```lisp
(defun draw-chunk-tiles-fallback (zone layer chunk-x chunk-y chunk-pixel-size)
  "Draw chunk tiles directly when cache not ready. Uses existing helpers."
  (let* ((chunk-tiles *render-chunk-size*)
         (chunk-size (zone-chunk-size zone))
         (tile-dest-size (/ chunk-pixel-size chunk-tiles))
         (tile-size-f (float *tile-size*)))
    (multiple-value-bind (layer-tileset layer-columns)
        (layer-tileset-context layer nil (get-current-assets))  ; Need assets reference
      (when layer-tileset
        (let ((tile-source (raylib:make-rectangle :x 0.0 :y 0.0 :width 0.0 :height 0.0))
              (tile-dest (raylib:make-rectangle :x 0.0 :y 0.0 :width 0.0 :height 0.0))
              (origin (raylib:make-vector2 :x 0.0 :y 0.0)))
          (loop :for local-y :from 0 :below chunk-tiles
                :for tile-y = (+ (* chunk-y chunk-tiles) local-y)
                :for dest-y = (+ (* chunk-y chunk-pixel-size) (* local-y tile-dest-size))
                :do (loop :for local-x :from 0 :below chunk-tiles
                          :for tile-x = (+ (* chunk-x chunk-tiles) local-x)
                          :for dest-x = (+ (* chunk-x chunk-pixel-size) (* local-x tile-dest-size))
                          :for tile-index = (zone-layer-tile-at layer chunk-size tile-x tile-y)
                          :when (and tile-index (> tile-index 0))
                          :do (set-tile-source-rect tile-source tile-index tile-size-f layer-columns)
                              (set-rectangle tile-dest dest-x dest-y tile-dest-size tile-dest-size)
                              (raylib:draw-texture-pro layer-tileset
                                                       tile-source
                                                       tile-dest
                                                       origin
                                                       0.0
                                                       raylib:+white+))))))))
```

**Step B.4: Update draw-world call site**

In `draw-world` function, restructure to:

```lisp
;; BEFORE the main draw block:
(when (and *render-cache-enabled* zone)
  (let* ((zone-id (zone-id zone))
         (cache (get-or-create-zone-render-cache zone-id tile-dest-size)))
    ;; Pre-render all visible chunks (FBO creation happens here)
    (prepare-zone-chunks zone cache view-left view-right view-top view-bottom
                         tile-dest-size editor assets)
    ;; Also pre-render preview zone chunks if needed
    (when ex-west (prepare-preview-zone :west ...))
    (when ex-east (prepare-preview-zone :east ...))
    ;; ... etc for all edges/corners
    ))

;; INSIDE begin-mode-2d (existing location):
(when (and *render-cache-enabled* zone)
  (draw-world-cached-fast zone cache chunk-pixel-size))  ; Only blits, never creates
```

### Phase C: Zone Bounds Clamping (Fix for Out-of-Bounds) - HIGH PRIORITY

**Goal:** Never generate chunks outside zone extents. Negative coords are a **guaranteed waste + FBO burst**.

**Data evidence:** 218 CREATE events with negative X coordinates in debug session.

**Implementation: Clamp & skip BEFORE get-or-render-chunk**

```lisp
;; Add bounds check helper:
(defun chunk-in-zone-bounds-p (zone chunk-x chunk-y)
  "Return T if chunk coords are within zone extents."
  (and (>= chunk-x 0)
       (>= chunk-y 0)
       (< chunk-x (ceiling (zone-width zone) *render-chunk-size*))
       (< chunk-y (ceiling (zone-height zone) *render-chunk-size*))))

;; In draw-world-cached, BEFORE calling get-or-render-chunk:
(when (chunk-in-zone-bounds-p zone chunk-x chunk-y)
  (get-or-render-chunk ...))
;; Skip entirely if out of bounds - no cache lookup, no FBO creation
```

**Apply to BOTH main zone AND preview zones.** Preview zones also need bounds clamping against their own extents.

**Step C.1: Clamp chunk ranges in iteration**

In `draw-world-cached` or equivalent, clamp the iteration range:

```lisp
(defun prepare-zone-chunks (zone cache view-left view-right view-top view-bottom
                            tile-dest-size editor assets)
  (let* ((chunk-pixel-size (zone-render-cache-chunk-pixel-size cache))
         ;; Zone bounds in chunk coords
         (zone-width-chunks (ceiling (zone-width zone) *render-chunk-size*))
         (zone-height-chunks (ceiling (zone-height zone) *render-chunk-size*))
         ;; Raw chunk range from view bounds
         (raw-start-x (floor view-left chunk-pixel-size))
         (raw-end-x (ceiling view-right chunk-pixel-size))
         (raw-start-y (floor view-top chunk-pixel-size))
         (raw-end-y (ceiling view-bottom chunk-pixel-size))
         ;; CLAMP to zone bounds - prevents negative and overflow coords
         (start-x (max 0 raw-start-x))
         (end-x (min (1- zone-width-chunks) raw-end-x))
         (start-y (max 0 raw-start-y))
         (end-y (min (1- zone-height-chunks) raw-end-y)))
    ;; SKIP if entirely out of bounds (no overlap with zone)
    (when (and (<= start-x end-x) (<= start-y end-y))
      ;; Only iterate over valid in-bounds chunks
      (loop :for layer :across (zone-layers zone)
            :do (loop :for cy :from start-y :to end-y
                      :do (loop :for cx :from start-x :to end-x
                                :do (ensure-chunk-cached cache zone layer cx cy
                                                          tile-dest-size editor assets)))))))
```

**Step C.2: Add bounds check helper**

```lisp
(defun chunk-in-zone-bounds-p (zone chunk-x chunk-y)
  "Return T if chunk coords are within zone extents."
  (let ((max-cx (ceiling (zone-width zone) *render-chunk-size*))
        (max-cy (ceiling (zone-height zone) *render-chunk-size*)))
    (and (>= chunk-x 0) (< chunk-x max-cx)
         (>= chunk-y 0) (< chunk-y max-cy))))
```

**Step C.3: Guard in get-or-render-chunk (defense in depth)**

Add early return if somehow an out-of-bounds chunk is requested:

```lisp
(defun get-or-render-chunk (cache zone layer chunk-x chunk-y tile-dest-size editor assets)
  ;; Skip out-of-bounds chunks entirely - no FBO creation
  (unless (chunk-in-zone-bounds-p zone chunk-x chunk-y)
    (return-from get-or-render-chunk nil))
  ;; ... existing logic ...
  )
```

**Step C.4: Guard in draw-chunk-or-fallback**

```lisp
(defun draw-chunk-or-fallback (cache zone layer chunk-x chunk-y chunk-pixel-size)
  ;; Skip out-of-bounds chunks - nothing to draw
  (unless (chunk-in-zone-bounds-p zone chunk-x chunk-y)
    (return-from draw-chunk-or-fallback))
  ;; ... existing logic ...
  )
```

**Why this matters:** At max zoom-out near a corner, `view-left` can be -1280 (negative) and raw-start-x would be -2. Without clamping, we'd create an FBO for chunk (-2, -1) containing zero tiles - wasted GPU allocation that contributes to the black flash.

### Phase D: Preview Zone Handling

**Goal:** Apply same pre-render + bounds clamping to preview zones.

```lisp
(defun prepare-preview-zone-chunks (preview-zone edge view-bounds tile-dest-size editor assets)
  "Pre-render visible chunks for a preview zone."
  (let* ((zone-id (zone-id preview-zone))
         (cache (get-or-create-zone-render-cache zone-id tile-dest-size)))
    (multiple-value-bind (offset-x offset-y)
        (preview-zone-offset preview-zone tile-dest-size edge)
      (let ((adj-left (- (car view-bounds) offset-x))
            (adj-right (- (cadr view-bounds) offset-x))
            (adj-top (- (caddr view-bounds) offset-y))
            (adj-bottom (- (cadddr view-bounds) offset-y)))
        (prepare-zone-chunks preview-zone cache adj-left adj-right adj-top adj-bottom
                             tile-dest-size editor assets)))))
```

---

## Removed Phases

### ~~Global Frame Counter Fix~~ (Removed)

Per-zone frame counters are correct. Each zone cache is independent; preview zone counters don't affect main zone eviction. No fix needed.

### ~~Cache Size Increase~~ (Removed)

With corrected math, 64 chunks is sufficient for 3+ viewports at max zoom-out. If issues persist after Phase B, revisit with instrumentation data.

---

## Configuration Parameters

### Keep Existing
```lisp
(defparameter *render-cache-max-chunks* 64
  "Maximum cached chunk textures per zone. 64 is sufficient for zoom 0.5 with 3 layers.")

(defparameter *render-cache-enabled* t
  "Enable/disable chunk render caching.")

(defparameter *render-chunk-size* 16
  "Tiles per chunk side.")
```

### Add New
```lisp
(defparameter *debug-render-cache* nil
  "When T, log cache statistics and chunk creation/eviction events.")
```

---

## Implementation Order (Revised Based on A2 Data + Codex Review)

| Phase | Description | Risk | Effort | Status |
|-------|-------------|------|--------|--------|
| A | Basic instrumentation | Low | 1h | ✓ DONE |
| A2 | Diagnostic enhancement (zone context, clear logs, config) | Low | 0.5h | ✓ DONE |
| C | Zone bounds clamping (skip negative/OOB chunks) | Low | 0.5h | **NEXT - TOP PRIORITY** |
| X1 | Fix zone-change clear behavior (clear-others, not clear-all) | Low | 0.25h | **HIGH PRIORITY** |
| X2 | Zone-id normalization + cache identity verification | Low | 0.5h | Pending |
| B | Pre-render step + draw-only refactor | High | 3h | **BLOCKED until X2 verified** |
| D | Preview zone handling | Medium | 1h | Pending |
| - | Testing and validation | Medium | 2h | Pending |

**Order:** A ✓ → A2 ✓ → C → X1 → X2 (verify) → B → D → Testing

**Rationale (updated with A2 findings + Codex review):**

1. **C first (bounds clamping):** Negative coords confirmed in A2 data (297 ZONE-4 creates with negative coords). Guaranteed to cut FBO bursts dramatically. Quick win.

2. **X1 (fix clear behavior):** A2 showed `CLEAR-ALL zones:4 caller:zone-change` clears destination zone unnecessarily. Change to `clear-other-zone-render-caches(destination-zone-id)` to keep destination warm. See "Phase X1" section for implementation.

3. **X2 (zone-id normalization + cache identity):** A2 showed same cache key created twice WITHOUT clear between (lines 1884→2231). Must verify:
   - Zone-id type consistency (keyword vs symbol vs string)
   - Cache instance identity (no duplicate caches for same zone)
   - Hash table test function compatibility
   See "Phase X2" section for detailed steps (X2.1-X2.4).

4. **B blocked until X2:** Pre-render refactor is high-effort. Only worth doing once cache lookup is proven reliable. X2 validation must pass first.

**Removed phases:**
- A3 (layer iteration audit): Multi-tileset layers explained by A2 data - WALLS uses 3 tilesets (OVERWORLD/CAVE/OBJECTS). Not a bug, just inflates counts. Cache key `(layer-id . tileset-id)` is correct.

---

## Acceptance Criteria (Updated)

### Primary (Must Fix)
- [ ] No black flash when moving
- [ ] CREATE events < 200 for typical movement session (was 1030)
- [ ] Same chunk coords created ≤ 3× max (was 27×)
- [ ] Water/collision tiles stable during movement
- [ ] Maximum zoom-out renders correctly

### Secondary (Should Fix)
- [ ] No FBO creation during draw phase (only pre-render)
- [ ] WALLS and FLOOR duplicate ratios similar (was 2:1)
- [ ] No out-of-bounds chunk creation (negative coords only for preview zones)

### Validation
- [ ] Cache disabled mode works unchanged (baseline)
- [ ] `make tests` passes
- [ ] `make smoke` passes

---

## Files to Modify

| File | Changes |
|------|---------|
| `src/rendering.lisp` | Add pre-render phase, draw-only mode, bounds clamping, instrumentation |
| `src/config-client.lisp` | Add `*debug-render-cache*` parameter |

---

## Interpretation of Data Anomalies

### Anomaly 1: Duplicate CREATEs + Zero Evicts

**Data:** Same chunk coords created 20-27× each, but EVICT=0.

**Interpretation:** Since eviction isn't happening, the duplicates must be from:
- **Cache clears** (not currently logged), OR
- **Distinct zone caches** (main vs preview zones) but logged without zone context

**Resolution:** Phase A2 adds zone-id to logs and logs cache clears. This will prove which.

### Anomaly 2: Negative Chunk Coordinates

**Data:** 218 CREATE events with negative X (e.g., `(-3,1)`).

**Interpretation:** **Definite bug.** Causes needless chunk creation and FBO bursts. Chunks at negative coords are outside zone bounds and produce empty textures.

**Resolution:** Phase C implements bounds clamping before any cache creation.

### Anomaly 3: Cache Size (90) Exceeds Max (64) With No Evicts

**Data:** `cache-size:90` but `*render-cache-max-chunks*=64` and `EVICT=0`.

**Interpretation:** The `cache-size` stat is **global across all zones**, while max is **per-zone**. This is expected behavior, not a bug:
- Main zone: ~48 chunks
- Preview zone 1: ~21 chunks
- Preview zone 2: ~21 chunks
- Total: 90 (each zone under 64 individually)

**Resolution:** Phase A2 adds per-zone cache size logging to confirm.

### Anomaly 4: Visible Count 48-75 Seems High

**Data:** `visible:75` with supposedly 1024px chunks should only show ~6-18 chunks.

**Interpretation:** Either:
- `tile-dest-size` is smaller than assumed (scale 1.0 instead of 4.0), OR
- Multiple zones/layers counted together, OR
- Chunk size configured differently at runtime

**Resolution:** Phase A2 adds one-time config log to confirm actual values.

---

## Open Questions (Updated Based on Data)

### Answered by Data

1. ~~**Water tile glitch:**~~ → WALLS layer has 2× more duplicates. Likely layer iteration bug.

2. ~~**Exact cause of black flash:**~~ → 1030 mid-frame FBO allocations.

### Still Open (Will Be Answered by Phase X2)

3. ~~**Are duplicate CREATEs from same zone or different zones?**~~
   - **ANSWERED by A2:** Same zone (ZONE-4), same key, no clear between. Lookup bug confirmed.

4. ~~**Is any cache clear function called during movement?**~~
   - **ANSWERED by A2:** Yes, `CLEAR-ALL` on zone-change clears destination unnecessarily.

5. ~~**What are actual runtime chunk dimensions?**~~
   - **ANSWERED by A2:** tile-dest:64, chunk-px:1024, chunk-tiles:16, max:64. As expected.

6. ~~**Why does WALLS have 2× more duplicates than FLOOR?**~~
   - **ANSWERED by A2:** WALLS uses 3 tilesets (OVERWORLD/CAVE/OBJECTS). 3× entries, not a bug.

7. **Why does cache lookup fail for identical keys?**
   - Phase X2 zone-id normalization and cache identity logging will answer.

8. **Remaining tile seam tearing:**
   - May be separate precision issue unrelated to caching.

---

---

## Priority Fixes Summary (Shortlist)

Based on data analysis, these are the highest-impact fixes in order:

| Priority | Fix | Why | Phase |
|----------|-----|-----|-------|
| 1 | **Clamp/skip out-of-bounds chunks** | Eliminates 218+ wasted FBO allocations per session. Hard bug with guaranteed ROI. | C |
| 2 | **Add zone-id to cache logs + log cache clears** | Required to diagnose duplicate CREATEs. Without this, can't fix root cause. | A2 |
| 3 | **Move chunk creation out of draw pass** | Eliminates black flash by separating FBO allocation from screen rendering. | B |

**Note:** Fix #2 is diagnostic, not a code fix. But it's required before we can properly fix the duplicate CREATE issue. Once A2 data is collected, we may discover the fix is simple (e.g., cache clear being called unnecessarily).

---

**Signed:** Claude Opus 4.5 (2026-01-23, Revised)

---

## Appendix: Debug Session Data (2026-01-23)

### Test Protocol
- User walked around zone 1
- Transitioned to another zone
- Returned to zone 1
- Session duration: ~10-15 seconds
- Client configuration: `*debug-render-cache* t`

### Raw Statistics

| Metric | Value |
|--------|-------|
| Total CREATE events | 1030 |
| Total EVICT events | 0 |
| Max observed cache-size | 90 |
| Max observed visible | 75 |
| Config *render-cache-max-chunks* | 64 |

### Duplicate Chunk Creation Counts

Output of `grep "[CACHE] CREATE" | sort | uniq -c | sort -rn`:

```
     27 [CACHE] CREATE chunk (2,-2) layer:WALLS
     26 [CACHE] CREATE chunk (2,-1) layer:WALLS
     26 [CACHE] CREATE chunk (2,0) layer:WALLS
     26 [CACHE] CREATE chunk (1,-1) layer:WALLS
     26 [CACHE] CREATE chunk (-1,-1) layer:WALLS
     26 [CACHE] CREATE chunk (1,0) layer:WALLS
     26 [CACHE] CREATE chunk (-1,0) layer:WALLS
     26 [CACHE] CREATE chunk (0,-1) layer:WALLS
     26 [CACHE] CREATE chunk (0,0) layer:WALLS
     21 [CACHE] CREATE chunk (1,-2) layer:WALLS
     21 [CACHE] CREATE chunk (-1,-2) layer:WALLS
     21 [CACHE] CREATE chunk (0,-2) layer:WALLS
     13 [CACHE] CREATE chunk (-2,-1) layer:WALLS
     13 [CACHE] CREATE chunk (-2,0) layer:WALLS
     12 [CACHE] CREATE chunk (1,1) layer:WALLS
     11 [CACHE] CREATE chunk (3,-2) layer:WALLS
     11 [CACHE] CREATE chunk (2,4) layer:WALLS
     11 [CACHE] CREATE chunk (2,3) layer:WALLS
     11 [CACHE] CREATE chunk (2,2) layer:WALLS
     11 [CACHE] CREATE chunk (2,1) layer:WALLS
     11 [CACHE] CREATE chunk (-1,-2) layer:FLOOR
     10 [CACHE] CREATE chunk (2,-1) layer:FLOOR
     10 [CACHE] CREATE chunk (2,0) layer:FLOOR
     10 [CACHE] CREATE chunk (-1,1) layer:WALLS
     10 [CACHE] CREATE chunk (0,1) layer:WALLS
      9 [CACHE] CREATE chunk (3,-2) layer:FLOOR
      9 [CACHE] CREATE chunk (3,-1) layer:WALLS
      9 [CACHE] CREATE chunk (3,0) layer:WALLS
      9 [CACHE] CREATE chunk (2,-2) layer:FLOOR
      9 [CACHE] CREATE chunk (-2,-2) layer:FLOOR
```

### Negative Coordinate Chunk Count

Total CREATE events with negative X coordinate: 218 (grep `CREATE chunk \(-`)

Examples:
```
[CACHE] CREATE chunk (-3,1) layer:FLOOR
[CACHE] CREATE chunk (-2,1) layer:FLOOR
[CACHE] CREATE chunk (-1,1) layer:FLOOR
[CACHE] CREATE chunk (-3,2) layer:WALLS
[CACHE] CREATE chunk (-2,-2) layer:FLOOR
[CACHE] CREATE chunk (-1,-2) layer:WALLS
```

### Steady State Samples (Stationary Player)

```
[CACHE] visible:48 cache-size:48 | new:0 rerender:0 evict:0 | hits:48 misses:0
[CACHE] visible:48 cache-size:48 | new:0 rerender:0 evict:0 | hits:48 misses:0
[CACHE] visible:48 cache-size:48 | new:0 rerender:0 evict:0 | hits:48 misses:0
```

Observation: 100% hit rate, zero creates, zero evictions when stationary.

### Transition Burst Samples

**Initial zone load (first frame):**
```
[CACHE] visible:75 cache-size:75 | new:75 rerender:0 evict:0 | hits:0 misses:75
```

**Movement near zone edge (lines 1110-1170):**
```
[CACHE] visible:48 cache-size:48 | new:0 rerender:0 evict:0 | hits:48 misses:0
INFO: FBO: [ID 49] Framebuffer object created successfully
[CACHE] CREATE chunk (-3,1) layer:FLOOR
INFO: FBO: [ID 50] Framebuffer object created successfully
[CACHE] CREATE chunk (-2,1) layer:FLOOR
INFO: FBO: [ID 51] Framebuffer object created successfully
[CACHE] CREATE chunk (-1,1) layer:FLOOR
... (32 consecutive FBO creations)
```

**Duplicate creation in rapid succession (lines 1883 vs 1931):**
```
Line 1883: [CACHE] CREATE chunk (-2,-2) layer:FLOOR
Line 1931: [CACHE] CREATE chunk (-2,-2) layer:FLOOR
```
Same chunk coordinates, 48 lines apart in the same transition burst.

### FBO Creation Timing

All FBO creations are interleaved with `INFO: TEXTURE:` and `INFO: FBO:` raylib messages, confirming GPU allocation happens during the draw phase.

Pattern:
```
INFO: TEXTURE: [ID N] Texture loaded successfully (1024x1024 | R8G8B8A8 | 1 mipmaps)
INFO: TEXTURE: [ID M] Depth renderbuffer loaded successfully (32 bits)
INFO: FBO: [ID M] Framebuffer object created successfully
[CACHE] CREATE chunk (X,Y) layer:LAYER
```

### Observations (Data-Driven)

1. Same chunk coordinates appear in CREATE log multiple times (up to 27×)
2. Cache size exceeds configured max (90 > 64) without eviction events
3. Negative chunk coordinates are created (preview zones)
4. Stationary state shows 100% cache hits
5. Movement/transitions show burst of CREATE events with FBO allocation
6. WALLS layer has more duplicate creates than FLOOR layer

### Hypotheses (To Be Validated)

- **H1:** Cache key lookup fails, causing repeated creation of same chunks
- **H2:** Per-zone caches don't share the 64-chunk limit
- **H3:** Preview zones create chunks for same coords as main zone
- **H4:** Eviction check condition not triggering

### Data Location

Full debug output: `/tmp/claude/-home-telecommuter-repos-mmorpg/tasks/b8f4146.output` (555.9KB)

---

## Appendix Part 2: A2 Debug Session Data (2026-01-23)

### Test Protocol
- Same as Part 1: walk zone 1, cross to another zone, return
- A2 diagnostics enabled: zone context, cache clear logging, config log

### Key Findings

#### Finding 1: CLEAR-ALL on Zone Change (Root Cause Candidate)

```
[CACHE] CLEAR-ALL zones:4 caller:zone-change
[CACHE] CLEAR zone:ZONE-4 entries:64 caller:zone-change
[CACHE] CLEAR zone:ZONE-3 entries:64 caller:zone-change
[CACHE] CLEAR zone:ZONE-2 entries:56 caller:zone-change
[CACHE] CLEAR zone:ZONE-1 entries:64 caller:zone-change
```

**Observation:** Zone change triggers `clear-all-zone-render-caches`, which clears ALL zones including the destination zone. This forces complete cache rebuild on every zone transition.

#### Finding 2: Multiple Tilesets per Layer Name

Same chunk coords created with DIFFERENT tilesets:
```
Line 1884: zone:ZONE-4 chunk:(-2,-2) layer:WALLS tileset:OVERWORLD
Line 1932: zone:ZONE-4 chunk:(-2,-2) layer:WALLS tileset:CAVE
Line 1980: zone:ZONE-4 chunk:(-2,-2) layer:WALLS tileset:OBJECTS
Line 2231: zone:ZONE-4 chunk:(-2,-2) layer:WALLS tileset:OVERWORLD (duplicate!)
```

ZONE-4 WALLS layer tileset distribution:
```
     48 tileset:CAVE
    128 tileset:OBJECTS
     48 tileset:OVERWORLD
```

**Observation:** There are 3 different tilesets for "WALLS" layer. Cache key includes tileset, so these are 3 separate cache entries. This explains the ~3× ratio of WALLS vs FLOOR duplicates.

#### Finding 3: Duplicate Creates WITHOUT Clear Between

```
[CACHE] CREATE zone:ZONE-4 chunk:(-2,-2) layer:WALLS tileset:OVERWORLD  (line 1884)
[CACHE] CREATE zone:ZONE-4 chunk:(-2,-2) layer:WALLS tileset:CAVE       (line 1932)
[CACHE] CREATE zone:ZONE-4 chunk:(-2,-2) layer:WALLS tileset:OBJECTS    (line 1980)
[CACHE] CREATE zone:ZONE-4 chunk:(-2,-2) layer:WALLS tileset:OVERWORLD  (line 2231) ← DUPLICATE!
```

**No CLEAR event between lines 1884 and 2231.** The exact same cache key `(ZONE-4, (-2,-2), WALLS, OVERWORLD)` is created twice without any cache clearing.

**Conclusion:** Cache lookup is genuinely failing. Same key, same zone, no clear, but created twice.

#### Finding 4: Config Confirmed

```
[CACHE] CONFIG tile-dest:64 chunk-px:1024 chunk-tiles:16 max-chunks:64
```

Config is as expected: tile-dest 64px, chunk 1024px, 16 tiles/chunk, 64 max per zone.

#### Finding 5: Per-Zone Entry Counts (Steady State)

```
[CACHE]   zone:ZONE-1 entries:60
[CACHE]   zone:ZONE-2 entries:40
```

Per-zone counts are stable during steady state. Each zone is under 64 limit individually.

### Raw Statistics

| Metric | Value |
|--------|-------|
| Total CREATE events | 644 |
| Total CLEAR events | 8 (1 zone-change + 1 shutdown, each clearing multiple zones) |
| ZONE-4 CREATE events | 297 |
| ZONE-1 CREATE events | 129 |
| Max duplicates (same key) | 14× |

### Updated Hypotheses

| Hypothesis | Status |
|------------|--------|
| H1: Cache key lookup fails | **CONFIRMED** - same key created twice without clear |
| H2: Per-zone caches independent | **CONFIRMED** - each zone under 64, global sum > 64 |
| H3: Preview zones duplicate coords | **CONFIRMED** - ZONE-4 is preview zone with negative coords |
| H4: Eviction not triggering | **EXPECTED** - per-zone counts under limit |
| **NEW H5: Multiple tilesets per layer** | **CONFIRMED** - 3 tilesets for WALLS, explains 3× ratio |
| **NEW H6: CLEAR-ALL too aggressive** | **LIKELY** - clears destination zone cache unnecessarily |

### Data Location

Full A2 debug output: `/tmp/claude/-home-telecommuter-repos-mmorpg/tasks/bbed7d0.output`
