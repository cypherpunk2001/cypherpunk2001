# Rendering Optimization Plan: Zoom-Out Performance

**Date:** 2026-01-22
**Based on:** codex_findings.md
**Goal:** Optimize rendering for players who frequently play zoomed out

---

## Executive Summary

Players traveling the world often play at max zoom-out (0.5), showing 4x the visible area. While culling is mathematically correct, performance suffers due to:
- 2500+ individual tile draw calls per frame (no batching)
- No chunk-level render caching (static tiles re-drawn every frame)
- Linear entity iteration over `(game-entities game)` array

This plan addresses all suggested improvements from codex_findings.md with implementation-ready details aligned to actual codebase structures.

---

## Current Code Structures

### Tile Rendering (`rendering.lisp:344-484`)
```lisp
(defun draw-world (world render assets camera player npcs ui editor)
  ;; Camera position via editor-camera-target, not camera-x accessor
  (let* ((x (if (and editor (editor-active editor))
                (editor-camera-x editor)
                (player-x player)))
         (y (if (and editor (editor-active editor))
                (editor-camera-y editor)
                (player-y player)))
         (zoom (camera-zoom camera))
         ...)
    ;; Per-layer tileset resolution
    (labels ((draw-layer (layer)
               (multiple-value-bind (layer-tileset layer-columns)
                   (layer-tileset-context layer editor assets)
                 ...)))
      ;; Layer draw order: normal → collision → object
      (draw-layers (lambda (layer) ...))
      (draw-layers #'zone-layer-collision-p)
      (draw-layers (lambda (layer) (eql (zone-layer-id layer) *editor-object-layer-id*))))))
```

### Entity Rendering (`rendering.lisp:1621-1623`)
```lisp
(loop :for entity :across entities  ; (game-entities game) = mixed players + NPCs
      :when (entity-in-viewport-p entity camera-x camera-y zoom margin-x margin-y)
      :do (draw-entity entity assets render))
```

### Zone Tile Access (`zone.lisp:157-168`)
```lisp
(defun zone-layer-tile-at (layer chunk-size tx ty)
  ;; Signature: (layer chunk-size tx ty) - NOT (layer world-x world-y)
  ...)
```

### Preview Zones (`rendering.lisp:381-415`)
Preview zones render at map edges/corners via `draw-preview-for-edge` and `draw-preview-for-corner`.

---

## Implementation Plan

### Phase 1: Chunk-Based Render Texture Caching (HIGH PRIORITY)

**Problem:** Each tile requires a separate `raylib:draw-texture-pro` call.

**Solution:** Pre-render static tile chunks into render textures, respecting per-layer tilesets.

#### 1.1 Add Render Cache Structures

**File:** `src/types.lisp`

```lisp
(defstruct render-chunk-cache
  "Cached render texture for a chunk of tiles."
  (texture nil)           ; raylib render-texture-2d (from raylib:load-render-texture)
  (chunk-x 0 :type fixnum)
  (chunk-y 0 :type fixnum)
  (layer-key nil)         ; (layer-id . tileset-id) for cache key
  (dirty t :type boolean))

(defstruct zone-render-cache
  "Per-zone render cache state."
  (chunks (make-hash-table :test 'equal)) ; key: (layer-key chunk-x chunk-y)
  (chunk-pixel-size 0)    ; *render-chunk-size* * tile-dest-size
  (zone-id nil))          ; track which zone this cache belongs to
```

#### 1.2 Add Cache Configuration

**File:** `src/config-client.lisp`

```lisp
(defparameter *render-chunk-size* 16
  "Tiles per render chunk (16x16 = 256 tiles per texture).
   Balance: larger = fewer textures but more VRAM per texture.")

(defparameter *render-cache-max-chunks* 64
  "Maximum cached chunk textures before LRU eviction.")

(defparameter *render-cache-enabled* t
  "Enable/disable chunk render caching. Set NIL to use original per-tile rendering.")
```

#### 1.3 Implement Layer-Aware Chunk Rendering

**File:** `src/rendering.lisp`

```lisp
(defun render-chunk-to-texture (zone layer chunk-x chunk-y tile-dest-size editor assets)
  "Render a chunk of tiles from LAYER to a render texture.
   Respects per-layer tileset via layer-tileset-context."
  (let* ((chunk-tiles *render-chunk-size*)
         (tex-size (ceiling (* chunk-tiles tile-dest-size)))
         (texture (raylib:load-render-texture tex-size tex-size))
         (chunk-size (zone-chunk-size zone)))
    (multiple-value-bind (layer-tileset layer-columns)
        (layer-tileset-context layer editor assets)
      (when layer-tileset
        (raylib:begin-texture-mode texture)
        (raylib:clear-background (raylib:make-color :r 0 :g 0 :b 0 :a 0))
        ;; Render all tiles in chunk
        (loop :for local-y :from 0 :below chunk-tiles
              :for tile-y = (+ (* chunk-y chunk-tiles) local-y)
              :do (loop :for local-x :from 0 :below chunk-tiles
                        :for tile-x = (+ (* chunk-x chunk-tiles) local-x)
                        :for tile-index = (zone-layer-tile-at layer chunk-size tile-x tile-y)
                        :when (and tile-index (> tile-index 0))
                        :do (draw-tile-at-local layer-tileset layer-columns tile-index
                                                (* local-x tile-dest-size)
                                                (* local-y tile-dest-size)
                                                tile-dest-size)))
        (raylib:end-texture-mode)))
    texture))

(defun draw-tile-at-local (tileset columns tile-index dest-x dest-y tile-size)
  "Draw a single tile to current render target at local coordinates."
  (let* ((col (mod tile-index columns))
         (row (floor tile-index columns))
         (tile-size-f (float tile-size))
         (src-x (* col tile-size-f))
         (src-y (* row tile-size-f)))
    (raylib:draw-texture-pro tileset
                             (raylib:make-rectangle :x src-x :y src-y
                                                    :width tile-size-f :height tile-size-f)
                             (raylib:make-rectangle :x (float dest-x) :y (float dest-y)
                                                    :width tile-size-f :height tile-size-f)
                             (raylib:make-vector2 :x 0.0 :y 0.0)
                             0.0
                             raylib:+white+)))
```

#### 1.4 Cache Key Structure

Layer caching must distinguish layers by both ID and tileset:

```lisp
(defun chunk-cache-key (layer chunk-x chunk-y)
  "Generate cache key for a layer chunk."
  (list (cons (zone-layer-id layer) (zone-layer-tileset-id layer))
        chunk-x chunk-y))
```

#### 1.5 Resource Lifecycle: Texture Cleanup

**Critical:** Render textures must be explicitly unloaded to prevent VRAM leaks.

```lisp
(defun unload-chunk-texture (cache-entry)
  "Unload the render texture from a cache entry."
  (when (render-chunk-cache-texture cache-entry)
    (raylib:unload-render-texture (render-chunk-cache-texture cache-entry))
    (setf (render-chunk-cache-texture cache-entry) nil)))

(defun evict-lru-chunk (cache)
  "Evict least-recently-used chunk when cache is full."
  (let ((lru-key nil)
        (lru-time most-positive-fixnum))
    ;; Find LRU entry (would need last-access timestamp in struct)
    (maphash (lambda (key entry)
               (declare (ignore key))
               (when (< (render-chunk-cache-last-access entry) lru-time)
                 (setf lru-key key
                       lru-time (render-chunk-cache-last-access entry))))
             (zone-render-cache-chunks cache))
    (when lru-key
      (let ((entry (gethash lru-key (zone-render-cache-chunks cache))))
        (unload-chunk-texture entry)
        (remhash lru-key (zone-render-cache-chunks cache))))))

(defun clear-zone-render-cache (cache)
  "Unload all cached textures and clear the cache. Call on zone transition."
  (maphash (lambda (key entry)
             (declare (ignore key))
             (unload-chunk-texture entry))
           (zone-render-cache-chunks cache))
  (clrhash (zone-render-cache-chunks cache))
  (setf (zone-render-cache-zone-id cache) nil))
```

#### 1.6 Cache Invalidation Rules

**When to invalidate:**
- **Zone transition:** Clear entire cache (`clear-zone-render-cache`)
- **Editor tile edit:** Invalidate specific chunk (`invalidate-tile-at`)
- **Layer add/remove:** Clear cache for affected layer

**NOT invalidation triggers:**
- Object pickup/drop (objects drawn separately via `draw-zone-objects`)
- Entity movement (entities drawn separately)
- Camera movement (cached chunks are world-space, not screen-space)

```lisp
(defun invalidate-tile-at (cache layer world-tx world-ty)
  "Mark the chunk containing this tile as dirty for re-render."
  (let* ((chunk-x (floor world-tx *render-chunk-size*))
         (chunk-y (floor world-ty *render-chunk-size*))
         (key (chunk-cache-key layer chunk-x chunk-y))
         (entry (gethash key (zone-render-cache-chunks cache))))
    (when entry
      (setf (render-chunk-cache-dirty entry) t))))
```

#### 1.7 Integrate into draw-world

**File:** `src/rendering.lisp`

Modify `draw-world` to use cached chunks when `*render-cache-enabled*`:

```lisp
(defun draw-world (world render assets camera player npcs ui editor)
  ;; ... existing setup ...
  (if (and *render-cache-enabled* zone)
      (draw-world-cached zone render assets camera player editor
                         tile-dest-size zoom
                         view-left view-right view-top view-bottom)
      ;; Fallback to original per-tile rendering
      (draw-world-uncached ...)))

(defun draw-world-cached (zone render assets camera player editor
                          tile-dest-size zoom
                          view-left view-right view-top view-bottom)
  "Draw world using cached chunk textures."
  (let* ((cache (get-or-create-zone-render-cache zone))
         (chunk-pixel-size (* *render-chunk-size* tile-dest-size))
         (start-chunk-x (floor view-left chunk-pixel-size))
         (end-chunk-x (ceiling view-right chunk-pixel-size))
         (start-chunk-y (floor view-top chunk-pixel-size))
         (end-chunk-y (ceiling view-bottom chunk-pixel-size)))
    ;; Draw each layer in correct order with cached chunks
    (labels ((draw-layer-cached (layer)
               (loop :for cy :from start-chunk-y :to end-chunk-y
                     :do (loop :for cx :from start-chunk-x :to end-chunk-x
                               :do (draw-cached-chunk cache layer cx cy
                                                      chunk-pixel-size tile-dest-size
                                                      editor assets zoom)))))
      ;; Layer order: normal → collision → object (same as original)
      (loop :for layer :across (zone-layers zone)
            :when (and (not (zone-layer-collision-p layer))
                       (not (eql (zone-layer-id layer) *editor-object-layer-id*)))
            :do (draw-layer-cached layer))
      (loop :for layer :across (zone-layers zone)
            :when (zone-layer-collision-p layer)
            :do (draw-layer-cached layer))
      (loop :for layer :across (zone-layers zone)
            :when (eql (zone-layer-id layer) *editor-object-layer-id*)
            :do (draw-layer-cached layer)))))
```

---

### Phase 2: Preview Zone Caching (MEDIUM PRIORITY)

**Problem:** Preview zones at map edges are also rendered per-tile.

**Solution:** Extend chunk caching to preview zones.

#### 2.1 Separate Cache per Preview Zone

Preview zones are different zones, so they get their own cache:

```lisp
(defun get-preview-zone-cache (preview-zone)
  "Get or create render cache for a preview zone."
  (let ((zone-id (zone-id preview-zone)))
    (or (gethash zone-id *preview-zone-caches*)
        (setf (gethash zone-id *preview-zone-caches*)
              (make-zone-render-cache :zone-id zone-id)))))
```

#### 2.2 Modify draw-zone-preview

Apply same cached rendering to preview zones, with offset applied:

```lisp
(defun draw-zone-preview-cached (preview-zone render assets editor
                                  view-left view-right view-top view-bottom
                                  tile-dest-size offset-x offset-y zoom)
  "Draw preview zone using cached chunks with world offset."
  (let ((cache (get-preview-zone-cache preview-zone)))
    ;; Adjust view bounds by offset before chunk calculation
    (let ((adj-left (- view-left offset-x))
          (adj-right (- view-right offset-x))
          (adj-top (- view-top offset-y))
          (adj-bottom (- view-bottom offset-y)))
      ;; Draw chunks with offset applied to dest coordinates
      ...)))
```

---

### Phase 3: Entity Render Optimization (MEDIUM PRIORITY)

**Problem:** Every frame iterates all entities in `(game-entities game)` even if most are off-screen.

**Solution:** Use existing spatial grid infrastructure for render culling.

#### 3.1 Add Rect Query to Spatial Grid

**File:** `src/spatial.lisp`

```lisp
(defun spatial-grid-query-rect (grid left top right bottom)
  "Return list of entity IDs within the rectangle bounds."
  (when grid
    (let ((cell-size (spatial-grid-cell-size grid))
          (results nil))
      (loop :for cell-y :from (floor top cell-size) :to (floor bottom cell-size)
            :do (loop :for cell-x :from (floor left cell-size) :to (floor right cell-size)
                      :for cell = (gethash (spatial-cell-key cell-x cell-y)
                                           (spatial-grid-cells grid))
                      :when cell
                      :do (dolist (id cell)
                            (pushnew id results))))
      results)))
```

#### 3.2 Separate Render Queries for Players and NPCs

Since `game-entities` mixes players and NPCs, and spatial grids are per-type:

```lisp
(defun draw-entities-with-spatial-culling (game zone-state camera-x camera-y zoom
                                            margin-x margin-y assets render)
  "Draw entities using spatial grid for viewport culling."
  (let* ((half-view-w (/ *window-width* (* 2.0 zoom)))
         (half-view-h (/ *window-height* (* 2.0 zoom)))
         (view-left (- camera-x half-view-w margin-x))
         (view-right (+ camera-x half-view-w margin-x))
         (view-top (- camera-y half-view-h margin-y))
         (view-bottom (+ camera-y half-view-h margin-y))
         (players (game-players game))
         (npcs (if zone-state (zone-state-npcs zone-state) (game-npcs game))))
    ;; Draw players (no spatial grid currently, but array is small)
    (loop :for player :across players
          :when (entity-in-viewport-p player camera-x camera-y zoom margin-x margin-y)
          :do (draw-entity player assets render))
    ;; Draw NPCs using spatial grid if available
    (let ((npc-grid (and zone-state (zone-state-npc-grid zone-state))))
      (if npc-grid
          ;; Use spatial query for NPCs
          (let ((visible-ids (spatial-grid-query-rect npc-grid
                                                       view-left view-top
                                                       view-right view-bottom)))
            (dolist (npc-id visible-ids)
              (let ((npc (find-npc-by-id-fast zone-state npc-id)))
                (when npc
                  (draw-entity npc assets render)))))
          ;; Fallback: iterate all NPCs
          (loop :for npc :across npcs
                :when (entity-in-viewport-p npc camera-x camera-y zoom margin-x margin-y)
                :do (draw-entity npc assets render))))))
```

#### 3.3 Update draw-game to Use Spatial Culling

**File:** `src/rendering.lisp` (modify `draw-game`, line ~1621)

```lisp
;; Replace:
(loop :for entity :across entities
      :when (entity-in-viewport-p entity camera-x camera-y zoom margin-x margin-y)
      :do (draw-entity entity assets render))

;; With:
(let ((zone-state (get-zone-state (or (player-zone-id player) *starting-zone-id*))))
  (draw-entities-with-spatial-culling game zone-state camera-x camera-y zoom
                                       margin-x margin-y assets render))
```

---

### Phase 4: Optional Distance Pre-Filter (LOW PRIORITY)

**File:** `src/config-client.lisp`

```lisp
(defparameter *entity-render-max-distance* nil
  "Maximum distance from player to render entities. NIL = unlimited.
   Set to ~3000.0 for very large zones with many NPCs.")
```

Add check in entity draw loops:

```lisp
(defun entity-in-render-distance-p (entity player)
  "Return T if entity is within render distance, or if distance check disabled."
  (or (null *entity-render-max-distance*)
      (let ((dx (- (combatant-x entity) (player-x player)))
            (dy (- (combatant-y entity) (player-y player))))
        (<= (+ (* dx dx) (* dy dy))
            (* *entity-render-max-distance* *entity-render-max-distance*)))))
```

---

### Phase 5: Dynamic Window Resize Support (LOW PRIORITY)

**Problem:** Window dimensions hardcoded as `*window-width*` and `*window-height*`.

#### 5.1 Add Dynamic Dimension Functions

**File:** `src/utils.lisp` (placed here to avoid compile-order dependencies with ui.lisp/input.lisp)

```lisp
(defun current-screen-width ()
  "Return current screen width, dynamic if resize enabled."
  (if *window-resize-enabled*
      (raylib:get-screen-width)
      *window-width*))

(defun current-screen-height ()
  "Return current screen height, dynamic if resize enabled."
  (if *window-resize-enabled*
      (raylib:get-screen-height)
      *window-height*))
```

**File:** `src/config-client.lisp`

```lisp
(defconstant +flag-window-resizable+ 4
  "Raylib config flag to enable window resizing.")

(defparameter *window-resize-enabled* nil
  "When T, creates resizable window and uses dynamic screen dimensions.")
```

**Files:** `src/main.lisp` and `src/net.lisp` (before raylib:with-window)

```lisp
(when *window-resize-enabled*
  (raylib:set-config-flags +flag-window-resizable+))
```

#### 5.2 Replace Hardcoded References

Update all culling and UI code to use `(current-screen-width)` / `(current-screen-height)`:
- `entity-in-viewport-p` (line 30-31)
- `draw-world` (line 367-368)
- `draw-zone-objects` (line 547-548)

#### 5.3 Handle Resize Events

**File:** `src/main.lisp`

```lisp
(defun handle-window-resize (game)
  "Check for window resize and update game components accordingly.
   Only active when *window-resize-enabled* is T."
  (when (and *window-resize-enabled* (raylib:is-window-resized))
    (log-verbose "Window resized to ~dx~d"
                 (raylib:get-screen-width) (raylib:get-screen-height))
    ;; Update UI layout (menu panel, buttons, minimap position)
    (update-ui-for-window-resize (game-ui game))
    ;; Update camera offset for new screen center
    (update-camera-for-window-resize (game-camera game))))
```

**Note:** Render caches are NOT cleared on resize. Chunk caches are world-space and
independent of screen dimensions, so clearing them would cause unnecessary hitches.
Only camera offset and UI layout need updating.

---

## Implementation Order

| Phase | Priority | Effort | Impact | Dependencies |
|-------|----------|--------|--------|--------------|
| **1. Chunk Render Caching** | HIGH | 3-4 days | 90% draw call reduction | None |
| **2. Preview Zone Caching** | MEDIUM | 1 day | Completes Phase 1 | Phase 1 |
| **3. Entity Spatial Culling** | MEDIUM | 1-2 days | Scales with NPC count | Existing spatial.lisp |
| **4. Distance Pre-Filter** | LOW | 0.5 day | Config option | None |
| **5. Window Resizing** | LOW | 2-3 days | Future-proofing | UI refactor |

**Recommended order:** Phase 1 → Phase 2 → Phase 3 → Phase 4 → Phase 5

---

## Testing Checklist

### Phase 1 Tests
- [ ] Chunk textures render correctly for all layer types
- [ ] Per-layer tilesets resolve correctly in cached chunks
- [ ] Layer draw order preserved (normal → collision → object)
- [ ] Dirty chunks re-render on editor tile edit
- [ ] Zone transition clears cache and unloads textures
- [ ] No visual artifacts at chunk boundaries
- [ ] VRAM doesn't grow unbounded (LRU eviction works)
- [ ] `*render-cache-enabled* nil` falls back to original rendering

### Phase 2 Tests
- [ ] Preview zones at edges render with caching
- [ ] Preview zone caches are separate from main zone cache
- [ ] Edge/corner preview offsets applied correctly

### Phase 3 Tests
- [ ] `spatial-grid-query-rect` returns correct NPC IDs
- [ ] No NPCs missing at viewport edges (margin respected)
- [ ] Fallback works when zone-state or grid is nil
- [ ] Players still render (no spatial grid for players)

### Phase 5 Tests
- [ ] `*window-resize-enabled* t` uses dynamic dimensions
- [ ] Resize event clears render cache
- [ ] Camera offset updates on resize

---

## Risk Mitigation

| Risk | Mitigation |
|------|------------|
| VRAM exhaustion | LRU eviction, configurable `*render-cache-max-chunks*` |
| Texture leaks | Explicit `unload-render-texture` on eviction/zone-change |
| Chunk seam artifacts | Careful texture coordinate handling, test at zoom levels |
| Layer tileset bugs | Test with multiple tilesets, editor custom tilesets |
| Breaking existing behavior | Feature flag `*render-cache-enabled*` |

---

## File Change Summary

| File | Changes |
|------|---------|
| `src/types.lisp` | Add `render-chunk-cache`, `zone-render-cache` structs |
| `src/config-client.lisp` | Add cache config params, `*window-resize-enabled*` |
| `src/rendering.lisp` | Add cached draw functions, modify `draw-world`, `draw-game` |
| `src/spatial.lisp` | Add `spatial-grid-query-rect` |
| `src/zone.lisp` | Cache lifecycle helpers |
| `tests/unit-test.lisp` | Tests for spatial rect query, cache invalidation |

---

**Signed:** Claude Opus 4.5 (2026-01-22)
