;; rendering-tiles.lisp — Map/tileset rendering, chunk caching, preview zone rendering
(in-package #:mmorpg)

;;;; ========================================================================
;;;; RENDER CHUNK CACHE (Phase 1 - Zoom-Out Performance Optimization)
;;;; Pre-render static tile chunks into render textures to reduce draw calls.
;;;; ========================================================================

;;; Global zone render cache storage (zone-id -> zone-render-cache)
;;; Phase X2: Changed from :test 'eq to :test 'eql for safer symbol comparison.
;;; Keywords are always eql to themselves; eq can fail for non-interned symbols.
(defparameter *zone-render-caches* (make-hash-table :test 'eql :size 64)
  "Hash table mapping zone-id to zone-render-cache structs.")

;;; Render cache instrumentation (Phase A - Diagnostics)
(defvar *render-cache-stats-created* 0
  "New chunks created this frame (FBO allocations).")
(defvar *render-cache-stats-rerendered* 0
  "Dirty chunks re-rendered this frame (existing FBO updated).")
(defvar *render-cache-stats-evicted* 0
  "Chunks evicted this frame.")
(defvar *render-cache-stats-hits* 0
  "Cache hits this frame (existing texture reused).")
(defvar *render-cache-stats-misses* 0
  "Cache misses this frame (needed render or creation).")
(defvar *render-cache-stats-visible* 0
  "Visible chunks requested this frame.")

(defun reset-render-cache-stats ()
  "Reset per-frame cache statistics. Call at start of each frame."
  (setf *render-cache-stats-created* 0
        *render-cache-stats-rerendered* 0
        *render-cache-stats-evicted* 0
        *render-cache-stats-hits* 0
        *render-cache-stats-misses* 0
        *render-cache-stats-visible* 0))

(defun get-total-cache-size ()
  "Return total number of cached chunks across all zone caches."
  (let ((total 0))
    (maphash (lambda (_zone-id cache)
               (declare (ignore _zone-id))
               (incf total (hash-table-count (zone-render-cache-chunks cache))))
             *zone-render-caches*)
    total))

(defun log-render-cache-stats ()
  "Log cache stats summary when *debug-render-cache* enabled."
  (when *debug-render-cache*
    ;; Main stats line
    (format t "~&[CACHE] visible:~D cache-size:~D | new:~D rerender:~D evict:~D | hits:~D misses:~D~%"
            *render-cache-stats-visible*
            (get-total-cache-size)
            *render-cache-stats-created*
            *render-cache-stats-rerendered*
            *render-cache-stats-evicted*
            *render-cache-stats-hits*
            *render-cache-stats-misses*)
    ;; A2: Per-zone breakdown (only when multiple zones or creates/evicts happening)
    (when (or (> *render-cache-stats-created* 0)
              (> *render-cache-stats-evicted* 0)
              (> (hash-table-count *zone-render-caches*) 1))
      (maphash (lambda (zone-id cache)
                 (format t "~&[CACHE]   zone:~A entries:~D~%"
                         zone-id
                         (hash-table-count (zone-render-cache-chunks cache))))
               *zone-render-caches*))))

(defun draw-cache-debug-overlay ()
  "Draw cache stats HUD overlay when *debug-render-cache* enabled.
   Displays at bottom-left of screen."
  (when *debug-render-cache*
    (let* ((text (format nil "Cache: ~D/~D vis:~D new:~D re:~D ev:~D"
                         (get-total-cache-size)
                         *render-cache-max-chunks*
                         *render-cache-stats-visible*
                         *render-cache-stats-created*
                         *render-cache-stats-rerendered*
                         *render-cache-stats-evicted*))
           (text-size 16)
           (padding 6)
           (width (+ (* 8 (length text)) (* padding 2)))
           (height (+ text-size (* padding 2)))
           (x 6)
           (y (- (current-screen-height) height 6))
           (bg-color (raylib:make-color :r 0 :g 0 :b 0 :a 180)))
      (raylib:draw-rectangle x y width height bg-color)
      (raylib:draw-text text (+ x padding) (+ y padding) text-size raylib:+yellow+))))

;;;; ========================================================================
;;;; Chunk Cache Key Packing (Task 4.2)
;;;; Avoid per-lookup list/cons allocation by packing keys into fixnums.
;;;; Uses nested eq hash tables: layer-id -> tileset-id -> numeric-id.
;;;; ========================================================================

(defparameter *layer-key-ids* (make-hash-table :test 'eq :size 64)
  "Nested map: layer-id -> (tileset-id -> numeric-id).
   Outer and inner tables use eq on keywords - no consing on lookup.
   Reset when zone render caches are cleared.")

(defvar *next-layer-key-id* 0
  "Next numeric ID to assign to a new layer-key combination.")

(defun reset-layer-key-ids ()
  "Reset the layer-key-id mapping. Called when clearing all render caches."
  (clrhash *layer-key-ids*)
  (setf *next-layer-key-id* 0))

(defun get-layer-key-id (layer-id tileset-id)
  "Get or create numeric ID for a (layer-id, tileset-id) combination.
   No per-call allocation - uses nested eq hash tables on keywords.
   Inner tables are created lazily (once per unique layer-id)."
  (let ((inner (gethash layer-id *layer-key-ids*)))
    (unless inner
      ;; First time seeing this layer-id - create inner table (one-time alloc)
      (setf inner (make-hash-table :test 'eq :size 8))
      (setf (gethash layer-id *layer-key-ids*) inner))
    (or (gethash tileset-id inner)
        (setf (gethash tileset-id inner) (incf *next-layer-key-id*)))))

(declaim (inline pack-chunk-cache-key))
(defun pack-chunk-cache-key (layer-key-id chunk-x chunk-y)
  "Pack chunk cache key into a single fixnum (Task 4.2).
   Format: 10 bits layer-key-id | 22 bits chunk-x | 22 bits chunk-y
   Supports up to 1024 layer/tileset combos, +-2M chunk coordinates."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum layer-key-id chunk-x chunk-y))
  ;; Add offset to make chunk coords positive (signed -> unsigned)
  (let ((cx (the fixnum (+ chunk-x #x200000)))  ; Add 2^21 to handle negatives
        (cy (the fixnum (+ chunk-y #x200000))))
    (declare (type fixnum cx cy))
    (the fixnum (logior (the fixnum (ash (logand layer-key-id #x3FF) 44))   ; Top 10 bits
                        (the fixnum (ash (logand cx #x3FFFFF) 22))           ; Middle 22 bits
                        (the fixnum (logand cy #x3FFFFF))))))                ; Bottom 22 bits

(defun chunk-cache-key (layer chunk-x chunk-y)
  "Generate a unique cache key for a layer chunk.
   Returns a packed fixnum instead of list (Task 4.2).
   Key includes both layer-id and tileset-id to handle per-layer tilesets."
  (let ((layer-key-id (get-layer-key-id (zone-layer-id layer)
                                        (zone-layer-tileset-id layer))))
    (pack-chunk-cache-key layer-key-id chunk-x chunk-y)))

;;; Phase C: Zone bounds helpers for chunk clamping
(defun zone-max-chunk-x (zone)
  "Return the maximum valid chunk X coordinate for ZONE (exclusive)."
  (ceiling (zone-width zone) *render-chunk-size*))

(defun zone-max-chunk-y (zone)
  "Return the maximum valid chunk Y coordinate for ZONE (exclusive)."
  (ceiling (zone-height zone) *render-chunk-size*))

(defun chunk-in-zone-bounds-p (zone chunk-x chunk-y)
  "Return T if chunk coords are within zone extents."
  (and (>= chunk-x 0)
       (>= chunk-y 0)
       (< chunk-x (zone-max-chunk-x zone))
       (< chunk-y (zone-max-chunk-y zone))))

;; A2.3: One-time config log flag
(defvar *render-cache-config-logged* nil
  "T once we've logged the cache config. Reset on cache clear-all.")

;;; X2.4: Zone-id normalization for consistent hash table lookup
(defun normalize-zone-id (zone-id)
  "Ensure zone-id is always a keyword for consistent hash table lookup.
   Handles symbols from any package and strings."
  (etypecase zone-id
    (keyword zone-id)
    (symbol (intern (symbol-name zone-id) :keyword))
    (string (intern zone-id :keyword))))

(defun get-or-create-zone-render-cache (zone-id tile-dest-size)
  "Get or create a render cache for ZONE-ID.
   Zone-id is normalized to keyword for consistent lookup."
  (let* ((normalized-id (normalize-zone-id zone-id))
         (existing (gethash normalized-id *zone-render-caches*))
         (cache (or existing
                    (let ((new-cache (%make-zone-render-cache
                                      :zone-id normalized-id
                                      :chunk-pixel-size (* *render-chunk-size* tile-dest-size))))
                      ;; A2.3: Log config once on first cache creation
                      (when (and *debug-render-cache* (not *render-cache-config-logged*))
                        (format t "~&[CACHE] CONFIG tile-dest:~D chunk-px:~D chunk-tiles:~D max-chunks:~D~%"
                                (truncate tile-dest-size)
                                (truncate (* *render-chunk-size* tile-dest-size))
                                *render-chunk-size*
                                *render-cache-max-chunks*)
                        (setf *render-cache-config-logged* t))
                      (setf (gethash normalized-id *zone-render-caches*) new-cache)))))
    ;; X2.1: Log EVERY lookup for diagnostic analysis (detect zone-id instability)
    (when *debug-render-cache*
      (format t "~&[CACHE] LOOKUP zone-id:~S normalized:~S type:~A cache-id:~D hit:~A~%"
              zone-id
              normalized-id
              (type-of zone-id)
              (sxhash cache)
              (if existing "Y" "N")))
    cache))

(defun unload-chunk-texture (cache-entry)
  "Unload the render texture from a cache entry to free VRAM."
  (when (and cache-entry (render-chunk-cache-texture cache-entry))
    (raylib:unload-render-texture (render-chunk-cache-texture cache-entry))
    (setf (render-chunk-cache-texture cache-entry) nil)))

(defun evict-lru-chunk (cache)
  "Evict the least-recently-used chunk when cache exceeds max size."
  (let ((chunks (zone-render-cache-chunks cache))
        (lru-key nil)
        (lru-time most-positive-fixnum))
    ;; Find LRU entry
    (maphash (lambda (key entry)
               (when (< (render-chunk-cache-last-access entry) lru-time)
                 (setf lru-key key
                       lru-time (render-chunk-cache-last-access entry))))
             chunks)
    (when lru-key
      (let ((entry (gethash lru-key chunks)))
        ;; Instrumentation: log eviction with zone context (A2.1)
        (incf *render-cache-stats-evicted*)
        (when *debug-render-cache*
          (format t "~&[CACHE] EVICT zone:~A chunk:(~D,~D) layer-key:~A age:~D~%"
                  (zone-render-cache-zone-id cache)
                  (render-chunk-cache-chunk-x entry)
                  (render-chunk-cache-chunk-y entry)
                  (render-chunk-cache-layer-key entry)
                  (- (zone-render-cache-frame-counter cache) lru-time)))
        (unload-chunk-texture entry)
        (remhash lru-key chunks)))))

(defun clear-zone-render-cache (zone-id &optional (caller "unknown"))
  "Unload all cached textures for ZONE-ID and remove from global cache.
   Call on zone transition to prevent stale textures and VRAM leaks.
   CALLER is a string identifying the call site for diagnostic logging."
  ;; X2.4: Normalize zone-id for consistent lookup
  (let* ((normalized-id (normalize-zone-id zone-id))
         (cache (gethash normalized-id *zone-render-caches*)))
    (when cache
      ;; A2.2: Log cache clear with caller context
      (when *debug-render-cache*
        (format t "~&[CACHE] CLEAR zone:~A entries:~D caller:~A~%"
                normalized-id
                (hash-table-count (zone-render-cache-chunks cache))
                caller))
      (maphash (lambda (_key entry)
                 (declare (ignore _key))
                 (unload-chunk-texture entry))
               (zone-render-cache-chunks cache))
      (clrhash (zone-render-cache-chunks cache))
      (remhash normalized-id *zone-render-caches*))))

(defun clear-all-zone-render-caches (&optional (caller "unknown"))
  "Unload all cached textures across all zones. Call on shutdown or filter toggle.
   Collects keys first to avoid mutating hash table during iteration.
   CALLER is a string identifying the call site for diagnostic logging."
  ;; A2.2: Log clear-all with caller context
  (when *debug-render-cache*
    (format t "~&[CACHE] CLEAR-ALL zones:~D caller:~A~%"
            (hash-table-count *zone-render-caches*)
            caller))
  (let ((zone-ids nil))
    ;; Collect all zone-ids first
    (maphash (lambda (zone-id _cache)
               (declare (ignore _cache))
               (push zone-id zone-ids))
             *zone-render-caches*)
    ;; Now safely clear each zone's cache (pass caller for individual logging)
    (dolist (zone-id zone-ids)
      (clear-zone-render-cache zone-id caller)))
  ;; Final clear in case any were missed
  (clrhash *zone-render-caches*)
  ;; Task 4.2: Reset layer-key-id mapping when clearing all caches
  (reset-layer-key-ids)
  ;; Reset config logged flag so next session logs config again
  (setf *render-cache-config-logged* nil))

(defun clear-other-zone-render-caches (keep-zone-id &optional (caller "unknown"))
  "Clear all zone render caches except KEEP-ZONE-ID.
   Call on zone transition to free preview zone caches while keeping current zone warm.
   CALLER is a string identifying the call site for diagnostic logging."
  ;; X2.4: Normalize keep-zone-id for consistent comparison
  (let ((normalized-keep (normalize-zone-id keep-zone-id)))
    ;; A2.2: Log clear-others with caller context
    (when *debug-render-cache*
      (format t "~&[CACHE] CLEAR-OTHERS keeping:~A caller:~A~%"
              normalized-keep
              caller))
    (let ((zone-ids nil))
      (maphash (lambda (zone-id _cache)
                 (declare (ignore _cache))
                 (unless (eql zone-id normalized-keep)
                   (push zone-id zone-ids)))
               *zone-render-caches*)
      (dolist (zone-id zone-ids)
        (clear-zone-render-cache zone-id caller)))))

;; Register zone change hook to clear stale render caches on zone transition.
;; This keeps game logic (movement.lisp) decoupled from rendering.
;; Phase X1: Changed from clear-all to clear-others to keep destination zone warm.
;; This prevents forced cache rebuild of the zone we're entering.
(defun on-zone-change (new-zone-id)
  "Clear render caches for zones OTHER than NEW-ZONE-ID on zone transition.
   Keeps destination zone cache warm to avoid FBO burst on entry."
  (clear-other-zone-render-caches new-zone-id "zone-change"))

(setf *client-zone-change-hook* #'on-zone-change)

(defun toggle-render-cache-enabled ()
  "Toggle *render-cache-enabled* and clear caches for consistency."
  (setf *render-cache-enabled* (not *render-cache-enabled*))
  (clear-all-zone-render-caches "toggle-cache")
  *render-cache-enabled*)

(defun toggle-tile-point-filter ()
  "Toggle *tile-point-filter* and clear caches so new filter applies to all chunks."
  (setf *tile-point-filter* (not *tile-point-filter*))
  (clear-all-zone-render-caches "toggle-filter")
  *tile-point-filter*)

(defun invalidate-chunk-at-tile (zone-id layer world-tx world-ty)
  "Mark the chunk containing tile (WORLD-TX, WORLD-TY) as dirty for re-render.
   Call when editor modifies a tile."
  ;; X2.4: Normalize zone-id for consistent lookup
  (let* ((normalized-id (normalize-zone-id zone-id))
         (cache (gethash normalized-id *zone-render-caches*)))
    (when cache
      (let* ((chunk-x (floor world-tx *render-chunk-size*))
             (chunk-y (floor world-ty *render-chunk-size*))
             (key (chunk-cache-key layer chunk-x chunk-y))
             (entry (gethash key (zone-render-cache-chunks cache))))
        (when entry
          (setf (render-chunk-cache-dirty entry) t))))))

(defun render-chunk-to-texture (zone layer chunk-x chunk-y tile-dest-size editor assets)
  "Render a chunk of tiles from LAYER to a render texture.
   Returns a raylib render-texture-2d. Caller is responsible for cleanup."
  (let* ((chunk-tiles *render-chunk-size*)
         (tex-size (ceiling (* chunk-tiles tile-dest-size)))
         (texture (raylib:load-render-texture tex-size tex-size))
         (chunk-size (zone-chunk-size zone))
         (tile-src-size (float *tile-size*)))  ; Atlas source tile size (16)
    ;; Apply texture filter to the render texture's underlying texture
    ;; (0=POINT for pixel-perfect, 1=BILINEAR for smooth)
    (raylib:set-texture-filter (raylib:render-texture-2d-texture texture)
                               (if *tile-point-filter* 0 1))
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
                        :do (draw-tile-to-render-texture layer-tileset layer-columns tile-index
                                                          (* local-x tile-dest-size)
                                                          (* local-y tile-dest-size)
                                                          tile-src-size    ; Source: atlas size
                                                          tile-dest-size))) ; Dest: scaled size
        (raylib:end-texture-mode)))
    texture))

(defun draw-tile-to-render-texture (tileset columns tile-index dest-x dest-y src-tile-size dest-tile-size)
  "Draw a single tile to the current render target at local coordinates.
   SRC-TILE-SIZE: atlas tile size (e.g., 16). DEST-TILE-SIZE: scaled size (e.g., 64)."
  (let* ((cols (max 1 columns))
         (col (mod tile-index cols))
         (row (floor tile-index cols))
         (src-size-f (float src-tile-size))
         (dest-size-f (float dest-tile-size))
         (src-x (* col src-size-f))
         (src-y (* row src-size-f))
         (src-rect (raylib:make-rectangle :x src-x :y src-y
                                          :width src-size-f :height src-size-f))
         (dest-rect (raylib:make-rectangle :x (float dest-x) :y (float dest-y)
                                           :width dest-size-f :height dest-size-f))
         (origin (raylib:make-vector2 :x 0.0 :y 0.0)))
    (raylib:draw-texture-pro tileset src-rect dest-rect origin 0.0 raylib:+white+)))

(defun draw-chunk-tiles-direct (zone layer chunk-x chunk-y tile-dest-size
                                world-offset-x world-offset-y editor assets)
  "Phase B fallback: Draw chunk tiles directly to screen without caching.
   Used when a chunk isn't cached yet to avoid blank tiles for a frame.
   WORLD-OFFSET-X/Y: additional offset for preview zones (0,0 for main zone)."
  (let* ((chunk-tiles *render-chunk-size*)
         (chunk-pixel-size (* chunk-tiles tile-dest-size))
         (chunk-world-x (+ (* chunk-x chunk-pixel-size) world-offset-x))
         (chunk-world-y (+ (* chunk-y chunk-pixel-size) world-offset-y))
         (chunk-size (zone-chunk-size zone))
         (tile-src-size (float *tile-size*)))
    (multiple-value-bind (layer-tileset layer-columns)
        (layer-tileset-context layer editor assets)
      (when layer-tileset
        (loop :for local-y :from 0 :below chunk-tiles
              :for tile-y = (+ (* chunk-y chunk-tiles) local-y)
              :for dest-y = (+ chunk-world-y (* local-y tile-dest-size))
              :do (loop :for local-x :from 0 :below chunk-tiles
                        :for tile-x = (+ (* chunk-x chunk-tiles) local-x)
                        :for dest-x = (+ chunk-world-x (* local-x tile-dest-size))
                        :for tile-index = (zone-layer-tile-at layer chunk-size tile-x tile-y)
                        :when (and tile-index (> tile-index 0))
                        :do (draw-tile-to-render-texture layer-tileset layer-columns tile-index
                                                         dest-x dest-y
                                                         tile-src-size tile-dest-size)))))))

(defun get-or-render-chunk (cache zone layer chunk-x chunk-y tile-dest-size editor assets)
  "Get a cached chunk texture, rendering it if needed."
  ;; Phase C: Defense-in-depth bounds check - skip out-of-bounds chunks entirely
  (unless (chunk-in-zone-bounds-p zone chunk-x chunk-y)
    (return-from get-or-render-chunk nil))
  ;; Instrumentation: count visible chunks
  (incf *render-cache-stats-visible*)
  (let* ((key (chunk-cache-key layer chunk-x chunk-y))
         (chunks (zone-render-cache-chunks cache))
         (entry (gethash key chunks))
         (frame (zone-render-cache-frame-counter cache))
         (was-cached (and entry
                          (not (render-chunk-cache-dirty entry))
                          (render-chunk-cache-texture entry)))
         (is-new-entry (null entry))
         (is-dirty (and entry (render-chunk-cache-dirty entry))))
    ;; Evict if at capacity and need new entry
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
                   :last-access frame))
      (setf (gethash key chunks) entry))
    ;; Render if dirty or missing texture
    (when (or (render-chunk-cache-dirty entry)
              (null (render-chunk-cache-texture entry)))
      (when (render-chunk-cache-texture entry)
        (raylib:unload-render-texture (render-chunk-cache-texture entry)))
      (setf (render-chunk-cache-texture entry)
            (render-chunk-to-texture zone layer chunk-x chunk-y tile-dest-size editor assets))
      (setf (render-chunk-cache-dirty entry) nil)
      ;; Instrumentation: distinguish new creation vs dirty re-render
      ;; A2.1: Include zone context for diagnostic analysis
      ;; X2.2: Include cache-id (sxhash) to detect multiple cache instances
      (if is-new-entry
          (progn
            (incf *render-cache-stats-created*)
            (when *debug-render-cache*
              (format t "~&[CACHE] CREATE zone:~A cache-id:~D chunk:(~D,~D) layer:~A tileset:~A~%"
                      (zone-id zone)
                      (sxhash cache)
                      chunk-x chunk-y
                      (zone-layer-id layer)
                      (zone-layer-tileset-id layer))))
          (when is-dirty
            (incf *render-cache-stats-rerendered*)
            (when *debug-render-cache*
              (format t "~&[CACHE] RERENDER zone:~A cache-id:~D chunk:(~D,~D) layer:~A tileset:~A~%"
                      (zone-id zone)
                      (sxhash cache)
                      chunk-x chunk-y
                      (zone-layer-id layer)
                      (zone-layer-tileset-id layer))))))
    ;; Instrumentation: track hits vs misses
    (if was-cached
        (incf *render-cache-stats-hits*)
        (incf *render-cache-stats-misses*))
    ;; Update LRU access time
    (setf (render-chunk-cache-last-access entry) frame)
    (render-chunk-cache-texture entry)))

;;; Phase B: Draw-only chunk retrieval (no FBO creation during draw pass)
(defun get-cached-chunk-texture (cache layer chunk-x chunk-y)
  "Get an existing cached chunk texture WITHOUT creating it.
   Returns the render-texture if cached and ready, NIL otherwise.
   Used during draw pass to ensure no FBO allocation mid-frame."
  (let* ((key (chunk-cache-key layer chunk-x chunk-y))
         (chunks (zone-render-cache-chunks cache))
         (entry (gethash key chunks)))
    (when (and entry
               (not (render-chunk-cache-dirty entry))
               (render-chunk-cache-texture entry))
      ;; Update LRU and return texture
      (setf (render-chunk-cache-last-access entry)
            (zone-render-cache-frame-counter cache))
      (incf *render-cache-stats-hits*)
      (render-chunk-cache-texture entry))))

(defun draw-cached-chunk (cache zone layer chunk-x chunk-y
                          chunk-pixel-size tile-dest-size
                          editor assets zoom)
  "Draw a single cached chunk at its world position.
   Phase B: Only uses existing cached textures - never creates FBOs during draw.
   Falls back to per-tile drawing if chunk not cached (avoids blank frames)."
  (declare (ignore zoom)) ; Camera mode handles zoom
  (let ((texture (get-cached-chunk-texture cache layer chunk-x chunk-y)))
    (if texture
        ;; Draw cached texture
        (let* ((world-x (* chunk-x chunk-pixel-size))
               (world-y (* chunk-y chunk-pixel-size))
               ;; Render textures are flipped vertically in raylib
               (tex (raylib:render-texture-2d-texture texture))
               (tex-width (float (raylib:texture-2d-width tex)))
               (tex-height (float (raylib:texture-2d-height tex)))
               ;; Source rect: flip Y by using negative height
               (src-rect (raylib:make-rectangle :x 0.0 :y tex-height
                                                :width tex-width :height (- tex-height)))
               ;; Dest rect: world position, scaled by zoom (camera handles transform)
               (dest-rect (raylib:make-rectangle :x (float world-x) :y (float world-y)
                                                 :width (float chunk-pixel-size)
                                                 :height (float chunk-pixel-size)))
               (origin (raylib:make-vector2 :x 0.0 :y 0.0)))
          (raylib:draw-texture-pro tex src-rect dest-rect origin 0.0 raylib:+white+))
        ;; Fallback: chunk not cached - draw tiles directly (no blank frames)
        (progn
          (incf *render-cache-stats-misses*)
          (draw-chunk-tiles-direct zone layer chunk-x chunk-y tile-dest-size
                                   0 0 editor assets)))))

(defun draw-world-cached (zone cache tile-dest-size zoom
                          view-left view-right view-top view-bottom
                          editor assets)
  "Draw all visible zone layers using cached chunk textures."
  (let* ((zone-layers (zone-layers zone))
         (chunk-pixel-size (zone-render-cache-chunk-pixel-size cache))
         ;; Phase C: Zone bounds for clamping
         (max-chunk-x (zone-max-chunk-x zone))
         (max-chunk-y (zone-max-chunk-y zone)))
    ;; Increment frame counter for LRU tracking
    (incf (zone-render-cache-frame-counter cache))
    ;; Calculate visible chunk bounds, CLAMPED to zone extents
    (let ((start-chunk-x (max 0 (floor view-left chunk-pixel-size)))
          (end-chunk-x (min (1- max-chunk-x) (ceiling view-right chunk-pixel-size)))
          (start-chunk-y (max 0 (floor view-top chunk-pixel-size)))
          (end-chunk-y (min (1- max-chunk-y) (ceiling view-bottom chunk-pixel-size))))
      ;; Skip if entirely out of bounds (no overlap with zone)
      (when (and (<= start-chunk-x end-chunk-x) (<= start-chunk-y end-chunk-y))
        ;; Draw each layer in correct order with cached chunks
        (labels ((draw-layer-cached (layer)
                   (loop :for cy :from start-chunk-y :to end-chunk-y
                         :do (loop :for cx :from start-chunk-x :to end-chunk-x
                                   :do (draw-cached-chunk cache zone layer cx cy
                                                          chunk-pixel-size tile-dest-size
                                                          editor assets zoom)))))
          ;; Layer order: normal (non-collision, non-object) -> collision -> object
          (loop :for layer :across zone-layers
                :when (and (not (zone-layer-collision-p layer))
                           (not (eql (zone-layer-id layer) *editor-object-layer-id*)))
                :do (draw-layer-cached layer))
          (loop :for layer :across zone-layers
                :when (zone-layer-collision-p layer)
                :do (draw-layer-cached layer))
          (loop :for layer :across zone-layers
                :when (and (not (zone-layer-collision-p layer))
                           (eql (zone-layer-id layer) *editor-object-layer-id*))
                :do (draw-layer-cached layer)))))))

(defun draw-cached-chunk-with-offset (cache zone layer chunk-x chunk-y
                                      chunk-pixel-size tile-dest-size
                                      offset-x offset-y
                                      editor assets zoom)
  "Draw a single cached chunk at its world position plus offset.
   Used for preview zones rendered at map edges.
   Phase B: Only uses existing cached textures - never creates FBOs during draw.
   Falls back to per-tile drawing if chunk not cached (avoids blank frames)."
  (declare (ignore zoom)) ; Camera mode handles zoom
  (let ((texture (get-cached-chunk-texture cache layer chunk-x chunk-y)))
    (if texture
        ;; Draw cached texture with offset
        (let* ((world-x (+ (* chunk-x chunk-pixel-size) offset-x))
               (world-y (+ (* chunk-y chunk-pixel-size) offset-y))
               ;; Render textures are flipped vertically in raylib
               (tex (raylib:render-texture-2d-texture texture))
               (tex-width (float (raylib:texture-2d-width tex)))
               (tex-height (float (raylib:texture-2d-height tex)))
               ;; Source rect: flip Y by using negative height
               (src-rect (raylib:make-rectangle :x 0.0 :y tex-height
                                                :width tex-width :height (- tex-height)))
               ;; Dest rect: world position with offset, camera handles transform
               (dest-rect (raylib:make-rectangle :x (float world-x) :y (float world-y)
                                                 :width (float chunk-pixel-size)
                                                 :height (float chunk-pixel-size)))
               (origin (raylib:make-vector2 :x 0.0 :y 0.0)))
          (raylib:draw-texture-pro tex src-rect dest-rect origin 0.0 raylib:+white+))
        ;; Fallback: chunk not cached - draw tiles directly with offset (no blank frames)
        (progn
          (incf *render-cache-stats-misses*)
          (draw-chunk-tiles-direct zone layer chunk-x chunk-y tile-dest-size
                                   offset-x offset-y editor assets)))))

(defun draw-zone-preview-cached (zone tile-dest-size
                                 view-left view-right view-top view-bottom
                                 offset-x offset-y
                                 editor assets zoom)
  "Draw preview zone using cached chunk textures with world offset."
  (let* ((zone-id (zone-id zone))
         (cache (get-or-create-zone-render-cache zone-id tile-dest-size))
         (zone-layers (zone-layers zone))
         (chunk-pixel-size (zone-render-cache-chunk-pixel-size cache))
         ;; Phase C: Zone bounds for clamping
         (max-chunk-x (zone-max-chunk-x zone))
         (max-chunk-y (zone-max-chunk-y zone))
         ;; Adjust view bounds by offset to get preview-local coordinates
         (preview-left (- view-left offset-x))
         (preview-right (- view-right offset-x))
         (preview-top (- view-top offset-y))
         (preview-bottom (- view-bottom offset-y)))
    ;; Increment frame counter for LRU tracking
    (incf (zone-render-cache-frame-counter cache))
    ;; Calculate visible chunk bounds in preview zone's coordinate space, CLAMPED
    (let ((start-chunk-x (max 0 (floor preview-left chunk-pixel-size)))
          (end-chunk-x (min (1- max-chunk-x) (ceiling preview-right chunk-pixel-size)))
          (start-chunk-y (max 0 (floor preview-top chunk-pixel-size)))
          (end-chunk-y (min (1- max-chunk-y) (ceiling preview-bottom chunk-pixel-size))))
      ;; Skip if entirely out of bounds (no overlap with zone)
      (when (and (<= start-chunk-x end-chunk-x) (<= start-chunk-y end-chunk-y))
        ;; Draw each layer in correct order with cached chunks (with offset)
        (labels ((draw-layer-cached (layer)
                   (loop :for cy :from start-chunk-y :to end-chunk-y
                         :do (loop :for cx :from start-chunk-x :to end-chunk-x
                                   :do (draw-cached-chunk-with-offset
                                        cache zone layer cx cy
                                        chunk-pixel-size tile-dest-size
                                        offset-x offset-y
                                        editor assets zoom)))))
          ;; Layer order: normal (non-collision, non-object) -> collision -> object
          (loop :for layer :across zone-layers
                :when (and (not (zone-layer-collision-p layer))
                           (not (eql (zone-layer-id layer) *editor-object-layer-id*)))
                :do (draw-layer-cached layer))
          (loop :for layer :across zone-layers
                :when (zone-layer-collision-p layer)
                :do (draw-layer-cached layer))
          (loop :for layer :across zone-layers
                :when (and (not (zone-layer-collision-p layer))
                           (eql (zone-layer-id layer) *editor-object-layer-id*))
                :do (draw-layer-cached layer)))))))

;;;; ========================================================================
;;;; Phase B: Pre-Render Chunk Preparation
;;;; Call BEFORE raylib:begin-drawing to move FBO allocation outside draw pass.
;;;; ========================================================================

(defun prepare-zone-chunks (zone tile-dest-size
                            view-left view-right view-top view-bottom
                            editor assets)
  "Pre-render all visible chunks for ZONE. Call BEFORE begin-drawing.
   This ensures FBO allocation happens outside the draw pass, eliminating
   black flash artifacts from mid-frame render target switches."
  (when (and *render-cache-enabled* zone)
    (let* ((zone-id (zone-id zone))
           (cache (get-or-create-zone-render-cache zone-id tile-dest-size))
           (zone-layers (zone-layers zone))
           (chunk-pixel-size (zone-render-cache-chunk-pixel-size cache))
           (max-chunk-x (zone-max-chunk-x zone))
           (max-chunk-y (zone-max-chunk-y zone))
           ;; Calculate visible chunk bounds, CLAMPED to zone extents
           (start-chunk-x (max 0 (floor view-left chunk-pixel-size)))
           (end-chunk-x (min (1- max-chunk-x) (ceiling view-right chunk-pixel-size)))
           (start-chunk-y (max 0 (floor view-top chunk-pixel-size)))
           (end-chunk-y (min (1- max-chunk-y) (ceiling view-bottom chunk-pixel-size))))
      ;; Skip if entirely out of bounds
      (when (and (<= start-chunk-x end-chunk-x) (<= start-chunk-y end-chunk-y))
        ;; Pre-create chunks for all layers (same order as draw)
        (flet ((prepare-layer (layer)
                 (loop :for cy :from start-chunk-y :to end-chunk-y
                       :do (loop :for cx :from start-chunk-x :to end-chunk-x
                                 :do (get-or-render-chunk cache zone layer cx cy
                                                          tile-dest-size editor assets)))))
          (loop :for layer :across zone-layers
                :when (and (not (zone-layer-collision-p layer))
                           (not (eql (zone-layer-id layer) *editor-object-layer-id*)))
                :do (prepare-layer layer))
          (loop :for layer :across zone-layers
                :when (zone-layer-collision-p layer)
                :do (prepare-layer layer))
          (loop :for layer :across zone-layers
                :when (and (not (zone-layer-collision-p layer))
                           (eql (zone-layer-id layer) *editor-object-layer-id*))
                :do (prepare-layer layer)))))))

(defun prepare-preview-zone-chunks (preview-zone tile-dest-size
                                    view-left view-right view-top view-bottom
                                    offset-x offset-y
                                    editor assets)
  "Pre-render visible chunks for a preview zone with offset. Call BEFORE begin-drawing."
  (when (and *render-cache-enabled* preview-zone)
    (let* ((zone-id (zone-id preview-zone))
           (cache (get-or-create-zone-render-cache zone-id tile-dest-size))
           (zone-layers (zone-layers preview-zone))
           (chunk-pixel-size (zone-render-cache-chunk-pixel-size cache))
           (max-chunk-x (zone-max-chunk-x preview-zone))
           (max-chunk-y (zone-max-chunk-y preview-zone))
           ;; Adjust view bounds by offset to get preview-local coordinates
           (preview-left (- view-left offset-x))
           (preview-right (- view-right offset-x))
           (preview-top (- view-top offset-y))
           (preview-bottom (- view-bottom offset-y))
           ;; Calculate visible chunk bounds, CLAMPED
           (start-chunk-x (max 0 (floor preview-left chunk-pixel-size)))
           (end-chunk-x (min (1- max-chunk-x) (ceiling preview-right chunk-pixel-size)))
           (start-chunk-y (max 0 (floor preview-top chunk-pixel-size)))
           (end-chunk-y (min (1- max-chunk-y) (ceiling preview-bottom chunk-pixel-size))))
      ;; Skip if entirely out of bounds
      (when (and (<= start-chunk-x end-chunk-x) (<= start-chunk-y end-chunk-y))
        (flet ((prepare-layer (layer)
                 (loop :for cy :from start-chunk-y :to end-chunk-y
                       :do (loop :for cx :from start-chunk-x :to end-chunk-x
                                 :do (get-or-render-chunk cache preview-zone layer cx cy
                                                          tile-dest-size editor assets)))))
          (loop :for layer :across zone-layers
                :when (and (not (zone-layer-collision-p layer))
                           (not (eql (zone-layer-id layer) *editor-object-layer-id*)))
                :do (prepare-layer layer))
          (loop :for layer :across zone-layers
                :when (zone-layer-collision-p layer)
                :do (prepare-layer layer))
          (loop :for layer :across zone-layers
                :when (and (not (zone-layer-collision-p layer))
                           (eql (zone-layer-id layer) *editor-object-layer-id*))
                :do (prepare-layer layer)))))))

;;;; ========================================================================
;;;; END RENDER CHUNK CACHE
;;;; ========================================================================

(defun preview-zone-offset (zone tile-dest-size edge)
  ;; Return the world offset to align ZONE along EDGE.
  (let* ((span-x (* (zone-width zone) tile-dest-size))
         (span-y (* (zone-height zone) tile-dest-size)))
    (ecase edge
      (:north (values 0.0 (- span-y)))
      (:south (values 0.0 span-y))
      (:east (values span-x 0.0))
      (:west (values (- span-x) 0.0)))))

(defun preview-zone-corner-offset (zone tile-dest-size edge-a edge-b)
  ;; Return the world offset to align ZONE along EDGE-A + EDGE-B.
  (multiple-value-bind (offset-x-a offset-y-a)
      (preview-zone-offset zone tile-dest-size edge-a)
    (multiple-value-bind (offset-x-b offset-y-b)
        (preview-zone-offset zone tile-dest-size edge-b)
      (values (+ offset-x-a offset-x-b)
              (+ offset-y-a offset-y-b)))))

(defun draw-zone-preview (zone render assets editor
                          view-left view-right view-top view-bottom
                          tile-dest-size tile-size-f
                          offset-x offset-y
                          &optional (zoom 1.0))
  ;; Draw ZONE layers offset into world space.
  ;; Uses cached chunks when *render-cache-enabled*, otherwise per-tile rendering.
  (if *render-cache-enabled*
      ;; Cached rendering path for preview zones
      (draw-zone-preview-cached zone tile-dest-size
                                view-left view-right view-top view-bottom
                                offset-x offset-y
                                editor assets zoom)
      ;; Original per-tile rendering path (fallback)
      (progn
        ;; Apply tile filter based on user setting (0=POINT, 1=BILINEAR)
        (raylib:set-texture-filter (assets-tileset assets)
                                   (if *tile-point-filter* 0 1))
        (let* ((zone-layers (zone-layers zone))
               (chunk-size (zone-chunk-size zone))
               (tile-source (render-tile-source render))
               (tile-dest (render-tile-dest render))
               (origin (render-origin render))
               (preview-left (- view-left offset-x))
               (preview-right (- view-right offset-x))
               (preview-top (- view-top offset-y))
               (preview-bottom (- view-bottom offset-y))
               (max-col (max 0 (1- (zone-width zone))))
               (max-row (max 0 (1- (zone-height zone))))
               (start-col (max 0 (floor preview-left tile-dest-size)))
               (end-col (min max-col (ceiling preview-right tile-dest-size)))
               (start-row (max 0 (floor preview-top tile-dest-size)))
               (end-row (min max-row (ceiling preview-bottom tile-dest-size))))
          (when (and zone-layers (<= start-col end-col) (<= start-row end-row))
            (labels ((draw-layer (layer)
                       (multiple-value-bind (layer-tileset layer-columns)
                           (layer-tileset-context layer editor assets)
                         (when (and layer-tileset layer-columns)
                           (loop :for row :from start-row :to end-row
                                 :for dest-y :from (+ offset-y (* start-row tile-dest-size))
                                   :by tile-dest-size
                                 :do (loop :for col :from start-col :to end-col
                                           :for dest-x :from (+ offset-x (* start-col tile-dest-size))
                                             :by tile-dest-size
                                           :for layer-index = (zone-layer-tile-at layer
                                                                                  chunk-size
                                                                                  col row)
                                           :do (set-rectangle tile-dest dest-x dest-y
                                                              tile-dest-size tile-dest-size)
                                               (when (not (zerop layer-index))
                                                 (set-tile-source-rect tile-source
                                                                       layer-index
                                                                       tile-size-f
                                                                       layer-columns)
                                                 (raylib:draw-texture-pro layer-tileset
                                                                          tile-source
                                                                          tile-dest
                                                                          origin
                                                                          0.0
                                                                          raylib:+white+)))))))
                     (draw-layers (predicate)
                       (loop :for layer :across zone-layers
                             :when (funcall predicate layer)
                               :do (draw-layer layer))))
              (draw-layers (lambda (layer)
                             (and (not (zone-layer-collision-p layer))
                                  (not (eql (zone-layer-id layer) *editor-object-layer-id*)))))
              (draw-layers #'zone-layer-collision-p)
              (draw-layers (lambda (layer)
                             (and (not (zone-layer-collision-p layer))
                                  (eql (zone-layer-id layer) *editor-object-layer-id*))))))))))

(defun draw-world (world render assets camera player npcs ui editor)
  ;; Render floor, map layers, and debug overlays.
  ;; Apply tile filter based on user setting (0=POINT, 1=BILINEAR)
  (raylib:set-texture-filter (assets-tileset assets)
                             (if *tile-point-filter* 0 1))
  (let* ((tile-dest-size (world-tile-dest-size world))
         (tile-size-f (world-tile-size-f world))
         (floor-index (world-floor-index world))
         (tileset (assets-tileset assets))
         (tile-source (render-tile-source render))
         (tile-dest (render-tile-dest render))
         (origin (render-origin render))
         (wall-map (world-wall-map world))
         (zone (world-zone world))
         (zone-layers (and zone (zone-layers zone)))
         (chunk-size (and zone (zone-chunk-size zone)))
         (x (if (and editor (editor-active editor))
                (editor-camera-x editor)
                (player-x player)))
         (y (if (and editor (editor-active editor))
                (editor-camera-y editor)
                (player-y player)))
         (zoom (camera-zoom camera))
         (half-view-width (/ (current-screen-width) (* 2.0 zoom)))
         (half-view-height (/ (current-screen-height) (* 2.0 zoom)))
         (view-left (- x half-view-width))
         (view-right (+ x half-view-width))
         (view-top (- y half-view-height))
         (view-bottom (+ y half-view-height))
         (start-col (floor view-left tile-dest-size))
         (end-col (ceiling view-right tile-dest-size))
         (start-row (floor view-top tile-dest-size))
         (end-row (ceiling view-bottom tile-dest-size))
         (ex-west (view-exceeds-edge-p world view-left view-right view-top view-bottom :west))
         (ex-east (view-exceeds-edge-p world view-left view-right view-top view-bottom :east))
         (ex-north (view-exceeds-edge-p world view-left view-right view-top view-bottom :north))
         (ex-south (view-exceeds-edge-p world view-left view-right view-top view-bottom :south)))
    (labels ((draw-preview-for-edge (edge)
               (let ((preview-zone (world-preview-zone-for-edge world edge)))
                 (when preview-zone
                   (multiple-value-bind (offset-x offset-y)
                       (preview-zone-offset preview-zone tile-dest-size edge)
                     (draw-zone-preview preview-zone render assets editor
                                        view-left view-right view-top view-bottom
                                        tile-dest-size tile-size-f
                                        offset-x offset-y zoom)))))
             (draw-preview-for-corner (edge-a edge-b)
               (let ((preview-zone (world-preview-zone-for-corner world edge-a edge-b)))
                 (when preview-zone
                   (multiple-value-bind (offset-x offset-y)
                       (preview-zone-corner-offset preview-zone tile-dest-size edge-a edge-b)
                     (draw-zone-preview preview-zone render assets editor
                                        view-left view-right view-top view-bottom
                                        tile-dest-size tile-size-f
                                        offset-x offset-y zoom)))))
      )
      (when ex-west
        (draw-preview-for-edge :west))
      (when ex-east
        (draw-preview-for-edge :east))
      (when ex-north
        (draw-preview-for-edge :north))
      (when ex-south
        (draw-preview-for-edge :south))
      (when (and ex-west ex-north)
        (draw-preview-for-corner :west :north))
      (when (and ex-east ex-north)
        (draw-preview-for-corner :east :north))
      (when (and ex-west ex-south)
        (draw-preview-for-corner :west :south))
      (when (and ex-east ex-south)
        (draw-preview-for-corner :east :south)))
    (when (not (zerop floor-index))
      (loop :for row :from start-row :to end-row
            :for dest-y :from (* start-row tile-dest-size) :by tile-dest-size
            :do (loop :for col :from start-col :to end-col
                      :for dest-x :from (* start-col tile-dest-size) :by tile-dest-size
                      :for tile-index = (floor-tile-at col row floor-index)
                      :do (set-rectangle tile-dest dest-x dest-y
                                         tile-dest-size tile-dest-size)
                          (when (not (zerop tile-index))
                            (set-tile-source-rect tile-source tile-index tile-size-f)
                            (raylib:draw-texture-pro tileset
                                                     tile-source
                                                     tile-dest
                                                     origin
                                                     0.0
                                                     raylib:+white+)))))
    (when zone-layers
      ;; Use cached rendering if enabled, otherwise fall back to per-tile rendering
      (if (and *render-cache-enabled* zone)
          ;; Cached rendering path: pre-rendered chunk textures
          (let* ((zone-id (zone-id zone))
                 (cache (get-or-create-zone-render-cache zone-id tile-dest-size)))
            (draw-world-cached zone cache tile-dest-size zoom
                               view-left view-right view-top view-bottom
                               editor assets))
          ;; Original per-tile rendering path (fallback)
          (labels ((draw-layer (layer)
                     (multiple-value-bind (layer-tileset layer-columns)
                         (layer-tileset-context layer editor assets)
                       (when (and layer-tileset layer-columns)
                         (loop :for row :from start-row :to end-row
                               :for dest-y :from (* start-row tile-dest-size) :by tile-dest-size
                               :do (loop :for col :from start-col :to end-col
                                         :for dest-x :from (* start-col tile-dest-size) :by tile-dest-size
                                         :for layer-index = (zone-layer-tile-at layer
                                                                                chunk-size
                                                                                col row)
                                         :do (set-rectangle tile-dest dest-x dest-y
                                                            tile-dest-size tile-dest-size)
                                             (when (not (zerop layer-index))
                                               (set-tile-source-rect tile-source
                                                                     layer-index
                                                                     tile-size-f
                                                                     layer-columns)
                                               (raylib:draw-texture-pro layer-tileset
                                                                        tile-source
                                                                        tile-dest
                                                                        origin
                                                                        0.0
                                                                        raylib:+white+)))))))
                   (draw-layers (predicate)
                     (loop :for layer :across zone-layers
                           :when (funcall predicate layer)
                             :do (draw-layer layer))))
            (draw-layers (lambda (layer)
                           (and (not (zone-layer-collision-p layer))
                                (not (eql (zone-layer-id layer) *editor-object-layer-id*)))))
            (draw-layers #'zone-layer-collision-p)
            (draw-layers (lambda (layer)
                           (and (not (zone-layer-collision-p layer))
                                (eql (zone-layer-id layer) *editor-object-layer-id*)))))))
    (unless zone
      (loop :for row :from start-row :to end-row
            :for dest-y :from (* start-row tile-dest-size) :by tile-dest-size
            :do (loop :for col :from start-col :to end-col
                      :for dest-x :from (* start-col tile-dest-size) :by tile-dest-size
                      :for wall-index = (wall-tile-at wall-map col row)
                      :do (set-rectangle tile-dest dest-x dest-y
                                         tile-dest-size tile-dest-size)
                          (when (not (zerop wall-index))
                            (set-tile-source-rect tile-source wall-index tile-size-f)
                            (raylib:draw-texture-pro tileset
                                                     tile-source
                                                     tile-dest
                                                     origin
                                                     0.0
                                                     raylib:+white+)))))
    (when *debug-collision-overlay*
      (let ((tile-px (round tile-dest-size)))
        (loop :for row :from start-row :to end-row
              :for dest-y :from (* start-row tile-dest-size) :by tile-dest-size
              :do (loop :for col :from start-col :to end-col
                        :for dest-x :from (* start-col tile-dest-size) :by tile-dest-size
                        :for ix = (round dest-x)
                        :for iy = (round dest-y)
                        :do (when (wall-blocked-p wall-map col row)
                              (raylib:draw-rectangle ix iy tile-px tile-px
                                                     (ui-debug-collision-color ui)))
                            (when (not (zerop (wall-tile-at wall-map col row)))
                              (raylib:draw-rectangle ix iy tile-px tile-px
                                                     (ui-debug-wall-color ui)))
                            (raylib:draw-rectangle-lines ix iy tile-px tile-px
                                                         (ui-debug-grid-color ui)))))
      (let ((ix (round (- x (world-collision-half-width world))))
            (iy (round (- y (world-collision-half-height world))))
            (iw (round (* 2.0 (world-collision-half-width world))))
            (ih (round (* 2.0 (world-collision-half-height world)))))
        (raylib:draw-rectangle-lines ix iy iw ih (ui-debug-collider-color ui)))
      (loop :for npc :across npcs
            :when (combatant-alive-p npc)
            :do (multiple-value-bind (half-w half-h)
                    (combatant-collision-half npc world)
                  (multiple-value-bind (nx ny)
                      (combatant-position npc)
                    (let ((ix (round (- nx half-w)))
                          (iy (round (- ny half-h)))
                          (iw (round (* 2.0 half-w)))
                          (ih (round (* 2.0 half-h))))
                      (raylib:draw-rectangle-lines ix iy iw ih
                                                   (ui-debug-collider-color ui))))))
      (when (player-attacking player)
        (multiple-value-bind (ax ay ahw ahh)
            (attack-hitbox player world)
          (let ((ix (round (- ax ahw)))
                (iy (round (- ay ahh)))
                (iw (round (* 2.0 ahw)))
                (ih (round (* 2.0 ahh))))
            (raylib:draw-rectangle-lines ix iy iw ih
                                         (ui-debug-collision-color ui))))
      ))))
