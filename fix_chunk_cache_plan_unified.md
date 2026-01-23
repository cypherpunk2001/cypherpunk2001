# Unified Chunk Cache Fix Plan (Revised)

**Date:** 2026-01-23 (Revised)
**Scope:** Chunk render cache (Phase 1-2 from findings_plan.md), preview zones, movement-driven artifacts
**Status:** Analysis revised with corrected math, ready for implementation

---

## Executive Summary

The chunk cache has bugs causing black screen flashes and tile glitches. After recalculating with actual config values, the "cache too small" theory is **disproven** - the real issues are:

1. **Mid-frame FBO creation** (render target switches during draw loop)
2. **Out-of-bounds chunk generation** (empty textures created for chunks outside zone)
3. **Preview zone burst allocations** (sudden demand when entering edge thresholds)

**Fix strategy:** Move chunk creation out of the draw pass entirely, clamp chunk ranges to zone bounds, and provide instrumentation to diagnose remaining issues.

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

## Revised Bug Analysis

### Bug #1: Mid-Frame FBO Creation (CRITICAL - Root Cause of Black Flash)

**Location:** `rendering.lisp:438-468` in `render-chunk-to-texture`

**Problem:** FBOs are created on-demand inside the draw loop:

```lisp
;; Called from draw-world-cached → get-or-render-chunk → render-chunk-to-texture
(let ((texture (raylib:load-render-texture tex-size tex-size)))  ; GPU allocation
  (raylib:begin-texture-mode texture)   ; Switch render target
  ;; ... render tiles ...
  (raylib:end-texture-mode))            ; Switch back to screen
```

This happens while already drawing to the screen (inside `begin-drawing` / `begin-mode-2d`). The render target switch causes the black flash.

**Evidence:** The raylib log shows bursts of texture/FBO creation during movement:
```
INFO: TEXTURE: [ID 109] Texture loaded successfully (1024x1024 ...)
INFO: FBO: [ID 70] Framebuffer object created successfully
```

**Fix required:** Move ALL chunk creation to a pre-render step BEFORE `begin-drawing`.

### Bug #2: Out-of-Bounds Chunk Generation (HIGH)

**Location:** `rendering.lisp:551-554` in `draw-world-cached`

**Problem:** Chunk bounds are calculated from view bounds without clamping:

```lisp
(let ((start-chunk-x (floor view-left chunk-pixel-size))   ; Can be negative!
      (end-chunk-x (ceiling view-right chunk-pixel-size))  ; Can exceed zone
      ...
```

When the camera is near zone edges:
- `view-left` can be negative → `start-chunk-x` = -1
- `view-right` can exceed zone width → `end-chunk-x` beyond zone bounds

For each out-of-bounds chunk:
1. `render-chunk-to-texture` creates an FBO (GPU allocation)
2. All tile lookups return 0 (out of zone)
3. Result: Empty texture created and cached

At corners with preview zones, this can create many unnecessary empty textures.

**Fix required:** Clamp chunk ranges to zone bounds, skip out-of-bounds chunks entirely.

### Bug #3: Preview Zone Burst Allocation (MEDIUM)

**Location:** `rendering.lisp:795-810` (draw-preview-for-edge calls)

**Problem:** When the view first exceeds a zone edge:
- All visible preview zone chunks are rendered at once
- Each preview zone is a separate zone with its own cache
- Sudden demand → burst of FBO creations

**Fix required:** Gradual warmup or throttled creation for preview zones.

### ~~Bug #4: Cache Capacity Too Small~~ (DISPROVEN)

With corrected math, 64 chunks can hold 3+ full viewports at max zoom-out. This is NOT the issue.

### ~~Bug #5: Frame Counter Incremented Multiple Times~~ (NOT A BUG)

Each zone cache has its own frame counter. Preview zone increments don't affect main zone eviction. The per-zone counter design is actually correct.

### Bug #6: Entire Screen Black at Max Zoom-Out (CRITICAL)

**Revised analysis:** This is NOT caused by cache overflow. With only 18 chunks needed, 64 is plenty. The black screen at zoom 0.5 must be caused by:

1. **More aggressive chunk boundary crossing** at low zoom (larger viewport = more chunks enter/leave)
2. **More preview zones visible** at low zoom (up to 8 edges+corners = 8 separate caches)
3. **Cumulative FBO creation** from main zone + all preview zones

Each zone (main + up to 8 previews) may create chunks simultaneously, causing a large burst of render target switches.

---

## Symptoms Not Yet Explained

The user reported:
- Water/collision river tiles "freaking out" during movement
- Tiles appearing/disappearing

This requires investigation. Possible causes:
1. Layer draw order issue (collision layer evicted/re-rendered differently)
2. Tileset ID instability in cache keys
3. Race condition in eviction during layer iteration

**Recommendation:** Add instrumentation first (Phase A) to capture which chunks are being evicted/re-rendered.

---

## Fix Plan (Revised)

### Phase A: Instrumentation (Do First)

**Goal:** Capture cache dynamics to diagnose remaining issues.

**Add to `src/rendering.lisp`:**

```lisp
(defparameter *debug-render-cache* nil
  "When T, log cache stats and chunk creation/eviction events.")

(defvar *render-cache-stats-created* 0)
(defvar *render-cache-stats-evicted* 0)
(defvar *render-cache-stats-hits* 0)
(defvar *render-cache-stats-misses* 0)

(defun reset-render-cache-stats ()
  "Call at start of each frame."
  (setf *render-cache-stats-created* 0
        *render-cache-stats-evicted* 0
        *render-cache-stats-hits* 0
        *render-cache-stats-misses* 0))

(defun log-cache-event (event-type &rest args)
  "Log cache events when *debug-render-cache* enabled."
  (when *debug-render-cache*
    (apply #'format t (concatenate 'string "~&[CACHE] " event-type "~%") args)))
```

**Instrument existing functions:**
- In `get-or-render-chunk`: log hits/misses, chunk coords, layer-key
- In `evict-lru-chunk`: log evicted chunk coords
- In `render-chunk-to-texture`: log chunk creation

### Phase B: Pre-Render Step (Core Fix for Black Flash)

**Goal:** Move ALL chunk creation outside the draw pass.

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

### Phase C: Zone Bounds Clamping (Fix for Out-of-Bounds)

**Goal:** Never generate chunks outside zone extents. When zoomed out near edges, negative or beyond-zone chunk coords can generate empty textures and still trigger FBO allocation.

**Step C.1: Clamp chunk ranges in prepare-zone-chunks**

Already included in Phase B's `prepare-zone-chunks`, but explicitly:

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

## Implementation Order

| Phase | Description | Risk | Effort |
|-------|-------------|------|--------|
| A | Instrumentation + debug logging | Low | 1h |
| B | Pre-render step + draw-only refactor | High | 3h |
| C | Zone bounds clamping | Low | 0.5h |
| D | Preview zone handling | Medium | 1h |
| - | Testing and validation | Medium | 2h |

**Total estimated: ~7.5 hours**

**Order:** A → C → B → D → Testing

(Instrumentation first to validate fixes, bounds clamping is quick win, then core refactor)

---

## Acceptance Criteria

- [ ] No black flash when moving
- [ ] No burst of `load-render-texture` logs during draw phase (only during pre-render)
- [ ] Water/collision tiles stable during movement
- [ ] Maximum zoom-out renders correctly
- [ ] No FBO creation for out-of-bounds chunks (validate with logs)
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

## Open Questions (For Investigation)

1. **Water tile glitch:** Is this a layer-specific issue? Instrumentation will reveal if collision layer chunks are being evicted/re-rendered differently.

2. **Exact cause of zoom 0.5 black screen:** With corrected math, cache overflow is ruled out. Need instrumentation to capture what's happening (likely cumulative preview zone allocation).

3. **Remaining tile seam tearing:** The user noted horizontal seams are now rare but still present. This may be a separate rendering precision issue unrelated to caching.

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
