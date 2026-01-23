;; NOTE: If you change behavior here, update docs/rendering.md :)
(in-package #:mmorpg)

(defun floor-tile-at (_x _y main-index)
  ;; Return the base floor tile index.
  main-index)

(defun wall-tile-at (wall-map tx ty)
  ;; Return the wall tile index for rendering or 0 if empty.
  (let ((variant-count (length *wall-tile-indices*)))
    (if (and (wall-occupied-p wall-map tx ty)
             (> variant-count 0))
        (aref *wall-tile-indices*
              (mod (u32-hash tx ty *wall-seed*) variant-count))
        0)))

(defun set-rectangle (rect x y width height)
  ;; Mutate a Raylib rectangle with new bounds and return it.
  (setf (raylib:rectangle-x rect) x
        (raylib:rectangle-y rect) y
        (raylib:rectangle-width rect) width
        (raylib:rectangle-height rect) height)
  rect)

(defun entity-in-viewport-p (entity camera-x camera-y zoom margin-x margin-y)
  "Return true if ENTITY is within the viewport bounds plus margin.
   CAMERA-X, CAMERA-Y: center of the camera view in world coordinates.
   ZOOM: camera zoom factor.
   MARGIN-X, MARGIN-Y: padding to add to viewport bounds (sprite half-sizes)."
  (let* ((half-view-width (/ *window-width* (* 2.0 zoom)))
         (half-view-height (/ *window-height* (* 2.0 zoom)))
         (view-left (- camera-x half-view-width margin-x))
         (view-right (+ camera-x half-view-width margin-x))
         (view-top (- camera-y half-view-height margin-y))
         (view-bottom (+ camera-y half-view-height margin-y)))
    (multiple-value-bind (ex ey) (combatant-position entity)
      (and (>= ex view-left)
           (<= ex view-right)
           (>= ey view-top)
           (<= ey view-bottom)))))

(defun set-tile-source-rect (rect tile-index tile-size-f &optional (columns *tileset-columns*))
  ;; Set the atlas source rectangle for a given tile index.
  (let* ((cols (max 1 columns))
         (col (mod tile-index cols))
         (row (floor tile-index cols)))
    (set-rectangle rect
                   (* col tile-size-f)
                   (* row tile-size-f)
                   tile-size-f
                   tile-size-f)))

(defun image-border-color-key (image)
  ;; Choose a color key from the image border when the top-left is transparent.
  (let ((width (raylib:image-width image))
        (height (raylib:image-height image)))
    (when (and (> width 0) (> height 0))
      (let ((corner (raylib:get-image-color image 0 0)))
        (if (> (raylib:color-a corner) 0)
            corner
            (let ((counts (make-hash-table :test 'eql))
                  (best nil)
                  (best-count 0))
              (labels ((tally (x y)
                         (let ((color (raylib:get-image-color image x y)))
                           (when (> (raylib:color-a color) 0)
                             (let* ((key (raylib:color-to-int color))
                                    (count (1+ (gethash key counts 0))))
                               (setf (gethash key counts) count)
                               (when (> count best-count)
                                 (setf best-count count
                                       best color)))))))
                (dotimes (x width)
                  (tally x 0)
                  (tally x (1- height)))
                (dotimes (y height)
                  (tally 0 y)
                  (tally (1- width) y))
                best)))))))

(defun load-texture-with-color-key (path)
  ;; Load a texture and replace a detected border color with transparency.
  (handler-case
      (let ((image (raylib:load-image path)))
        (when image
          (let* ((key (image-border-color-key image))
                 (transparent (raylib:make-color :r 0 :g 0 :b 0 :a 0)))
            (when key
              (raylib:image-color-replace image key transparent))
            (let ((texture (raylib:load-texture-from-image image)))
              (raylib:unload-image image)
              texture))))
    (error (e)
      (warn "Failed to load texture image ~a: ~a" path e)
      (log-verbose "Texture image load error for ~a: ~a" path e)
      nil)))

(defun load-texture-required (path label)
  ;; Load a required texture or signal a fatal error.
  (let ((texture nil)
        (err nil))
    (handler-case
        (setf texture (raylib:load-texture path))
      (error (e)
        (setf err e)))
    (if texture
        (progn
          (log-verbose "Loaded texture (~a): ~a" label path)
          texture)
        (progn
          (warn "Failed to load required texture (~a) from ~a: ~a" label path err)
          (log-verbose "Required texture load error (~a): ~a" label err)
          (error "Required texture load failed for ~a" path)))))

(defun make-render ()
  ;; Allocate reusable rectangles and origin vector for rendering.
  (%make-render :origin (raylib:make-vector2 :x 0.0 :y 0.0)
                :tile-source (raylib:make-rectangle)
                :tile-dest (raylib:make-rectangle)
                :player-source (raylib:make-rectangle)
                :player-dest (raylib:make-rectangle)
                :npc-source (raylib:make-rectangle)
                :npc-dest (raylib:make-rectangle)))

(defun load-assets (world)
  ;; Load textures and compute sprite sizing for rendering.
  (ensure-game-data)
  (let* ((scaled-width (* *sprite-frame-width* *sprite-scale*))
         (scaled-height (* *sprite-frame-height* *sprite-scale*))
         (half-sprite-width (/ scaled-width 2.0))
         (half-sprite-height (/ scaled-height 2.0))
         (player-set (get-animation-set *player-animation-set-id* :player))
         (blood-set (get-animation-set :blood))
         (npc-ids (npc-animation-set-ids))
         (npc-animations (make-hash-table :test 'eq))
         (object-textures (make-hash-table :test 'eq))
         (item-textures (make-hash-table :test 'eq))
         (tileset (load-texture-required *tileset-path* "tileset"))
         (_ (raylib:set-texture-filter tileset 0))  ; POINT filter - no interpolation
         (tileset-columns (max 1 (truncate (/ (raylib:texture-width tileset)
                                              (max 1 *tile-size*)))))
         (down-idle (load-texture-required (animation-path player-set :down-idle)
                                           "player-down-idle"))
         (down-walk (load-texture-required (animation-path player-set :down-walk)
                                           "player-down-walk"))
         (down-attack (load-texture-required (animation-path player-set :down-attack)
                                             "player-down-attack"))
         (up-idle (load-texture-required (animation-path player-set :up-idle)
                                         "player-up-idle"))
         (up-walk (load-texture-required (animation-path player-set :up-walk)
                                         "player-up-walk"))
         (up-attack (load-texture-required (animation-path player-set :up-attack)
                                           "player-up-attack"))
         (side-idle (load-texture-required (animation-path player-set :side-idle)
                                           "player-side-idle"))
         (side-walk (load-texture-required (animation-path player-set :side-walk)
                                           "player-side-walk"))
         (side-attack (load-texture-required (animation-path player-set :side-attack)
                                             "player-side-attack"))
         (blood-down (load-texture-required (animation-path blood-set :down)
                                            "blood-down"))
         (blood-up (load-texture-required (animation-path blood-set :up)
                                          "blood-up"))
         (blood-side (load-texture-required (animation-path blood-set :side)
                                            "blood-side")))
    (dolist (id npc-ids)
      (let ((set (get-animation-set id :npc)))
        (setf (gethash id npc-animations)
              (%make-npc-textures
               :down-idle (load-texture-required (animation-path set :down-idle)
                                                 (format nil "npc-~a-down-idle" id))
               :up-idle (load-texture-required (animation-path set :up-idle)
                                               (format nil "npc-~a-up-idle" id))
               :side-idle (load-texture-required (animation-path set :side-idle)
                                                 (format nil "npc-~a-side-idle" id))))))
    (loop :for id :across (object-archetype-ids)
          :for archetype = (find-object-archetype id)
          :for sprite = (and archetype (object-archetype-sprite archetype))
          :when sprite
            :do (setf (gethash id object-textures)
                      (load-texture-with-color-key sprite)))
    (loop :for id :across (item-archetype-ids)
          :for item = (find-item-archetype id)
          :for sprite = (and item (item-archetype-sprite item))
          :when sprite
            :do (setf (gethash id item-textures)
                      (load-texture-with-color-key sprite)))
    (setf *tileset-columns* tileset-columns)
    (log-verbose "Assets ready: npcs=~d objects=~d items=~d"
                 (hash-table-count npc-animations)
                 (hash-table-count object-textures)
                 (hash-table-count item-textures))
    (%make-assets :tileset tileset
                  :down-idle down-idle
                  :down-walk down-walk
                  :down-attack down-attack
                  :up-idle up-idle
                  :up-walk up-walk
                  :up-attack up-attack
                  :side-idle side-idle
                  :side-walk side-walk
                  :side-attack side-attack
                  :npc-animations npc-animations
                  :object-textures object-textures
                  :item-textures item-textures
                  :blood-down blood-down
                  :blood-up blood-up
                  :blood-side blood-side
                  :scaled-width scaled-width
                  :scaled-height scaled-height
                  :half-sprite-width half-sprite-width
                  :half-sprite-height half-sprite-height)))

(defun unload-assets (assets)
  ;; Unload textures stored in the assets struct.
  (raylib:unload-texture (assets-tileset assets))
  (raylib:unload-texture (assets-down-idle assets))
  (raylib:unload-texture (assets-down-walk assets))
  (raylib:unload-texture (assets-down-attack assets))
  (raylib:unload-texture (assets-up-idle assets))
  (raylib:unload-texture (assets-up-walk assets))
  (raylib:unload-texture (assets-up-attack assets))
  (raylib:unload-texture (assets-side-idle assets))
  (raylib:unload-texture (assets-side-walk assets))
  (raylib:unload-texture (assets-side-attack assets))
  (let ((npc-animations (assets-npc-animations assets)))
    (when npc-animations
      (maphash (lambda (_id textures)
                 (declare (ignore _id))
                 (raylib:unload-texture (npc-textures-down-idle textures))
                 (raylib:unload-texture (npc-textures-up-idle textures))
                 (raylib:unload-texture (npc-textures-side-idle textures)))
               npc-animations)))
  (let ((object-textures (assets-object-textures assets)))
    (when object-textures
      (maphash (lambda (_id texture)
                 (declare (ignore _id))
                 (raylib:unload-texture texture))
               object-textures)))
  (let ((item-textures (assets-item-textures assets)))
    (when item-textures
      (maphash (lambda (_id texture)
                 (declare (ignore _id))
                 (raylib:unload-texture texture))
               item-textures)))
  (raylib:unload-texture (assets-blood-down assets))
  (raylib:unload-texture (assets-blood-up assets))
  (raylib:unload-texture (assets-blood-side assets))
  ;; Clear all render chunk caches to free VRAM
  (clear-all-zone-render-caches))

(defun layer-tileset-context (layer editor assets)
  ;; Resolve the tileset texture and columns for a layer.
  (let ((tileset-id (zone-layer-tileset-id layer)))
    (cond
      ((and tileset-id editor)
       (let ((entry (editor-tileset-by-id editor tileset-id)))
         (when entry
           (editor-load-tileset-texture entry)
           (values (editor-tileset-texture entry)
                   (editor-tileset-columns entry)))))
      (t
       (values (assets-tileset assets) *tileset-columns*)))))

;;;; ========================================================================
;;;; RENDER CHUNK CACHE (Phase 1 - Zoom-Out Performance Optimization)
;;;; Pre-render static tile chunks into render textures to reduce draw calls.
;;;; ========================================================================

;;; Global zone render cache storage (zone-id -> zone-render-cache)
(defparameter *zone-render-caches* (make-hash-table :test 'eq)
  "Hash table mapping zone-id to zone-render-cache structs.")

(defun chunk-cache-key (layer chunk-x chunk-y)
  "Generate a unique cache key for a layer chunk.
   Key includes both layer-id and tileset-id to handle per-layer tilesets."
  (list (cons (zone-layer-id layer) (zone-layer-tileset-id layer))
        chunk-x chunk-y))

(defun get-or-create-zone-render-cache (zone-id tile-dest-size)
  "Get or create a render cache for ZONE-ID."
  (or (gethash zone-id *zone-render-caches*)
      (setf (gethash zone-id *zone-render-caches*)
            (%make-zone-render-cache
             :zone-id zone-id
             :chunk-pixel-size (* *render-chunk-size* tile-dest-size)))))

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
        (unload-chunk-texture entry)
        (remhash lru-key chunks)))))

(defun clear-zone-render-cache (zone-id)
  "Unload all cached textures for ZONE-ID and remove from global cache.
   Call on zone transition to prevent stale textures and VRAM leaks."
  (let ((cache (gethash zone-id *zone-render-caches*)))
    (when cache
      (maphash (lambda (_key entry)
                 (declare (ignore _key))
                 (unload-chunk-texture entry))
               (zone-render-cache-chunks cache))
      (clrhash (zone-render-cache-chunks cache))
      (remhash zone-id *zone-render-caches*))))

(defun clear-all-zone-render-caches ()
  "Unload all cached textures across all zones. Call on shutdown or filter toggle.
   Collects keys first to avoid mutating hash table during iteration."
  (let ((zone-ids nil))
    ;; Collect all zone-ids first
    (maphash (lambda (zone-id _cache)
               (declare (ignore _cache))
               (push zone-id zone-ids))
             *zone-render-caches*)
    ;; Now safely clear each zone's cache
    (dolist (zone-id zone-ids)
      (clear-zone-render-cache zone-id)))
  ;; Final clear in case any were missed
  (clrhash *zone-render-caches*))

(defun clear-other-zone-render-caches (keep-zone-id)
  "Clear all zone render caches except KEEP-ZONE-ID.
   Call on zone transition to free preview zone caches while keeping current zone warm."
  (let ((zone-ids nil))
    (maphash (lambda (zone-id _cache)
               (declare (ignore _cache))
               (unless (eql zone-id keep-zone-id)
                 (push zone-id zone-ids)))
             *zone-render-caches*)
    (dolist (zone-id zone-ids)
      (clear-zone-render-cache zone-id))))

;; Register zone change hook to clear stale render caches on zone transition.
;; This keeps game logic (movement.lisp) decoupled from rendering.
;; Uses clear-all (strict Option A) to ensure editor changes aren't stale.
(defun on-zone-change (new-zone-id)
  "Clear all render caches on zone transition. Cache rebuilds on first draw."
  (declare (ignore new-zone-id))
  (clear-all-zone-render-caches))

(setf *client-zone-change-hook* #'on-zone-change)

(defun toggle-render-cache-enabled ()
  "Toggle *render-cache-enabled* and clear caches for consistency."
  (setf *render-cache-enabled* (not *render-cache-enabled*))
  (clear-all-zone-render-caches)
  *render-cache-enabled*)

(defun toggle-tile-point-filter ()
  "Toggle *tile-point-filter* and clear caches so new filter applies to all chunks."
  (setf *tile-point-filter* (not *tile-point-filter*))
  (clear-all-zone-render-caches)
  *tile-point-filter*)

(defun invalidate-chunk-at-tile (zone-id layer world-tx world-ty)
  "Mark the chunk containing tile (WORLD-TX, WORLD-TY) as dirty for re-render.
   Call when editor modifies a tile."
  (let ((cache (gethash zone-id *zone-render-caches*)))
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

(defun get-or-render-chunk (cache zone layer chunk-x chunk-y tile-dest-size editor assets)
  "Get a cached chunk texture, rendering it if needed."
  (let* ((key (chunk-cache-key layer chunk-x chunk-y))
         (chunks (zone-render-cache-chunks cache))
         (entry (gethash key chunks))
         (frame (zone-render-cache-frame-counter cache)))
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
      (setf (render-chunk-cache-dirty entry) nil))
    ;; Update LRU access time
    (setf (render-chunk-cache-last-access entry) frame)
    (render-chunk-cache-texture entry)))

(defun draw-cached-chunk (cache zone layer chunk-x chunk-y
                          chunk-pixel-size tile-dest-size
                          editor assets zoom)
  "Draw a single cached chunk at its world position."
  (let ((texture (get-or-render-chunk cache zone layer chunk-x chunk-y
                                      tile-dest-size editor assets)))
    (when texture
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
        (declare (ignore zoom)) ; Camera mode handles zoom
        (raylib:draw-texture-pro tex src-rect dest-rect origin 0.0 raylib:+white+)))))

(defun draw-world-cached (zone cache tile-dest-size zoom
                          view-left view-right view-top view-bottom
                          editor assets)
  "Draw all visible zone layers using cached chunk textures."
  (let* ((zone-layers (zone-layers zone))
         (chunk-pixel-size (zone-render-cache-chunk-pixel-size cache)))
    ;; Increment frame counter for LRU tracking
    (incf (zone-render-cache-frame-counter cache))
    ;; Calculate visible chunk bounds
    (let ((start-chunk-x (floor view-left chunk-pixel-size))
          (end-chunk-x (ceiling view-right chunk-pixel-size))
          (start-chunk-y (floor view-top chunk-pixel-size))
          (end-chunk-y (ceiling view-bottom chunk-pixel-size)))
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
              :do (draw-layer-cached layer))))))

(defun draw-cached-chunk-with-offset (cache zone layer chunk-x chunk-y
                                      chunk-pixel-size tile-dest-size
                                      offset-x offset-y
                                      editor assets zoom)
  "Draw a single cached chunk at its world position plus offset.
   Used for preview zones rendered at map edges."
  (let ((texture (get-or-render-chunk cache zone layer chunk-x chunk-y
                                      tile-dest-size editor assets)))
    (when texture
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
        (declare (ignore zoom)) ; Camera mode handles zoom
        (raylib:draw-texture-pro tex src-rect dest-rect origin 0.0 raylib:+white+)))))

(defun draw-zone-preview-cached (zone tile-dest-size
                                 view-left view-right view-top view-bottom
                                 offset-x offset-y
                                 editor assets zoom)
  "Draw preview zone using cached chunk textures with world offset."
  (let* ((zone-id (zone-id zone))
         (cache (get-or-create-zone-render-cache zone-id tile-dest-size))
         (zone-layers (zone-layers zone))
         (chunk-pixel-size (zone-render-cache-chunk-pixel-size cache))
         ;; Adjust view bounds by offset to get preview-local coordinates
         (preview-left (- view-left offset-x))
         (preview-right (- view-right offset-x))
         (preview-top (- view-top offset-y))
         (preview-bottom (- view-bottom offset-y)))
    ;; Increment frame counter for LRU tracking
    (incf (zone-render-cache-frame-counter cache))
    ;; Calculate visible chunk bounds in preview zone's coordinate space
    (let ((start-chunk-x (floor preview-left chunk-pixel-size))
          (end-chunk-x (ceiling preview-right chunk-pixel-size))
          (start-chunk-y (floor preview-top chunk-pixel-size))
          (end-chunk-y (ceiling preview-bottom chunk-pixel-size)))
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
              :do (draw-layer-cached layer))))))

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
         (half-view-width (/ *window-width* (* 2.0 zoom)))
         (half-view-height (/ *window-height* (* 2.0 zoom)))
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

(defun object-texture-for (assets object-id)
  ;; Return the texture for OBJECT-ID, if loaded.
  (let ((textures (and assets (assets-object-textures assets))))
    (and textures object-id (gethash object-id textures))))

(defun item-texture-for (assets item-id)
  ;; Return the texture for ITEM-ID, if loaded.
  (let ((textures (and assets (assets-item-textures assets))))
    (and textures item-id (gethash item-id textures))))

(defun draw-zone-objects (world render assets camera player editor)
  ;; Draw placed zone objects in world space.
  (let* ((zone (world-zone world))
         (objects (and zone (zone-objects zone))))
    (when objects
      (multiple-value-bind (camera-x camera-y)
          (editor-camera-target editor player)
        (let* ((tile-size (world-tile-dest-size world))
               (zoom (camera-zoom camera))
               (half-view-width (/ *window-width* (* 2.0 zoom)))
               (half-view-height (/ *window-height* (* 2.0 zoom)))
               (view-left (- camera-x half-view-width))
               (view-right (+ camera-x half-view-width))
               (view-top (- camera-y half-view-height))
               (view-bottom (+ camera-y half-view-height))
               (tile-source (render-tile-source render))
               (tile-dest (render-tile-dest render))
               (origin (render-origin render)))
          (dolist (object objects)
            (let ((tx (getf object :x))
                  (ty (getf object :y))
                  (object-id (getf object :id))
                  (count (getf object :count nil))
                  (respawn (getf object :respawn nil)))
              ;; DEBUG: Log respawn state occasionally
              (when (and respawn (> respawn 0.0) (< (random 100) 1))
                (log-verbose "RENDER-OBJ: id=~a respawn=~a (should be hidden)" object-id respawn))
              (when (and (numberp tx) (numberp ty)
                         (or (null count) (> count 0))
                         (or (null respawn) (<= respawn 0.0)))
                (let* ((x (* tx tile-size))
                       (y (* ty tile-size))
                       (x2 (+ x tile-size))
                       (y2 (+ y tile-size)))
                  (when (and (< x view-right)
                             (> x2 view-left)
                             (< y view-bottom)
                             (> y2 view-top))
                    ;; Try object texture first, fall back to item texture for dropped items
                    (let ((texture (or (object-texture-for assets object-id)
                                       (item-texture-for assets object-id))))
                      (when texture
                        (let* ((src-w (float (raylib:texture-width texture) 1.0))
                               (src-h (float (raylib:texture-height texture) 1.0))
                               (dest-size (float tile-size 1.0))
                               (dest-x (float x 1.0))
                               (dest-y (float y 1.0)))
                          (set-rectangle tile-source 0.0 0.0 src-w src-h)
                          (set-rectangle tile-dest dest-x dest-y dest-size dest-size)
                          (raylib:draw-texture-pro texture
                                                   tile-source
                                                   tile-dest
                                                   origin
                                                   0.0
                                                   raylib:+white+))))))))))))))

(defun draw-click-marker (player world)
  ;; Draw a fading click marker at the last target position.
  (let* ((timer (player-click-marker-timer player))
         (kind (player-click-marker-kind player)))
    (when (and kind (> timer 0.0))
      (let* ((duration (max 0.01 *click-marker-duration*))
             (alpha (clamp (/ timer duration) 0.0 1.0))
             (size (* (world-tile-dest-size world) *click-marker-size-scale*))
             (half (round (/ size 2.0)))
             (x (round (player-click-marker-x player)))
             (y (round (player-click-marker-y player)))
             (base-color (if (eq kind :attack)
                             *click-marker-attack-color*
                             *click-marker-walk-color*))
             (color (raylib:fade base-color alpha))
             (thickness (max 1 (truncate *click-marker-thickness*)))
             (half-thick (floor thickness 2))
             (x1 (- x half))
             (y1 (- y half))
             (x2 (+ x half))
             (y2 (+ y half))
             (x3 x1)
             (y3 (+ y half))
             (x4 x2)
             (y4 (- y half)))
        (if (<= thickness 1)
            (progn
              (raylib:draw-line x1 y1 x2 y2 color)
              (raylib:draw-line x3 y3 x4 y4 color))
            (loop :for offset :from (- half-thick) :to half-thick
                  :do (raylib:draw-line (+ x1 offset)
                                        (- y1 offset)
                                        (+ x2 offset)
                                        (- y2 offset)
                                        color)
                      (raylib:draw-line (+ x3 offset)
                                        (+ y3 offset)
                                        (+ x4 offset)
                                        (+ y4 offset)
                                        color)))))))
(defun npc-textures-for (npc assets)
  ;; Select the NPC texture set based on archetype.
  (let* ((archetype (npc-archetype npc))
         (set-id (if archetype
                     (npc-archetype-animation-set-id archetype)
                     :npc))
         (npc-animations (assets-npc-animations assets)))
    (or (gethash set-id npc-animations)
        (gethash :npc npc-animations))))

(defun npc-texture-for (npc assets direction)
  ;; Select the NPC idle sprite sheet for DIRECTION.
  (let ((textures (npc-textures-for npc assets)))
    (ecase direction
      (:down (npc-textures-down-idle textures))
      (:up (npc-textures-up-idle textures))
      (:side (npc-textures-side-idle textures)))))

(defun blood-texture-for (assets direction)
  ;; Select the blood effect sprite sheet for DIRECTION.
  (ecase direction
    (:down (assets-blood-down assets))
    (:up (assets-blood-up assets))
    (:side (assets-blood-side assets))))

(defun draw-hit-effect (x y facing facing-sign frame-index assets source dest origin)
  ;; Render a blood effect frame at the given world position.
  (let* ((texture (blood-texture-for assets facing))
         (flip (and (eq facing :side) (> facing-sign 0.0)))
         (src-x (* frame-index *sprite-frame-width*))
         (src-x (if flip
                    (+ src-x *sprite-frame-width*)
                    src-x))
         (src-width (if flip
                        (- *sprite-frame-width*)
                        *sprite-frame-width*))
         (half-width (assets-half-sprite-width assets))
         (half-height (assets-half-sprite-height assets)))
    (set-rectangle source
                   src-x 0.0
                   src-width *sprite-frame-height*)
    (set-rectangle dest
                   (- x half-width)
                   (- y half-height)
                   (assets-scaled-width assets)
                   (assets-scaled-height assets))
    (raylib:draw-texture-pro texture
                             source
                             dest
                             origin
                             0.0
                             raylib:+white+)))

(defun draw-health-bar (x y current max assets)
  ;; Draw a simple health bar above the given world position.
  (let* ((bar-width (assets-scaled-width assets))
         (bar-height *health-bar-height*)
         (offset *health-bar-offset*)
         (half-width (/ bar-width 2.0))
         (ratio (if (> max 0)
                    (clamp (/ current (float max 1.0)) 0.0 1.0)
                    0.0))
         (fill-width (round (* bar-width ratio)))
         (bar-x (round (- x half-width)))
         (bar-y (round (- y (assets-half-sprite-height assets) offset bar-height))))
    (raylib:draw-rectangle bar-x bar-y (round bar-width) bar-height
                           *health-bar-back-color*)
    (raylib:draw-rectangle bar-x bar-y fill-width bar-height
                           *health-bar-fill-color*)
    (raylib:draw-rectangle-lines bar-x bar-y (round bar-width) bar-height
                                 *health-bar-border-color*)))

(defun draw-npc (npc assets render)
  ;; Render the NPC sprite at its world position.
  (let ((alive (combatant-alive-p npc)))
    (when alive
      (let* ((direction (npc-facing npc))
             (texture (npc-texture-for npc assets direction))
             (src-x (* (npc-frame-index npc) *sprite-frame-width*))
             (half-width (assets-half-sprite-width assets))
             (half-height (assets-half-sprite-height assets)))
        (set-rectangle (render-npc-source render)
                       src-x 0.0
                       *sprite-frame-width* *sprite-frame-height*)
        (set-rectangle (render-npc-dest render)
                       (- (npc-x npc) half-width)
                       (- (npc-y npc) half-height)
                       (assets-scaled-width assets)
                       (assets-scaled-height assets))
        (raylib:draw-texture-pro texture
                                 (render-npc-source render)
                                 (render-npc-dest render)
                                 (render-origin render)
                                 0.0
                                 raylib:+white+)
        (multiple-value-bind (hp max-hp)
            (combatant-health npc)
          (draw-health-bar (npc-x npc) (npc-y npc) hp max-hp assets)
          (when *debug-npc-logs*
            (let* ((archetype (npc-archetype npc))
                   (flee-at (if archetype
                                (npc-archetype-flee-at-hits archetype)
                                0))
                   (text (format nil "~a hp=~d/~d flee<=~d prov=~a flee?~a"
                                 (npc-behavior-state npc)
                                 hp
                                 max-hp
                                 flee-at
                                 (if (npc-provoked npc) "Y" "N")
                                 (if (npc-should-flee-p npc) "Y" "N")))
                   (text-x (round (- (npc-x npc)
                                     (assets-half-sprite-width assets))))
                   (text-y (round (- (npc-y npc)
                                     (assets-half-sprite-height assets)
                                     *health-bar-offset*
                                     *health-bar-height*
                                     *debug-npc-text-offset*))))
              (raylib:draw-text text text-x text-y *debug-npc-text-size*
                                *debug-npc-text-color*))))))
    (when (npc-hit-active npc)
      (draw-hit-effect (npc-x npc)
                       (npc-y npc)
                       (npc-hit-facing npc)
                       (npc-hit-facing-sign npc)
                       (npc-hit-frame npc)
                       assets
                       (render-npc-source render)
                       (render-npc-dest render)
                       (render-origin render)))))

(defun player-texture-for (assets direction state)
  ;; Select the sprite sheet texture for DIRECTION and STATE.
  (ecase direction
    (:down (ecase state
             (:walk (assets-down-walk assets))
             (:idle (assets-down-idle assets))
             (:attack (assets-down-attack assets))))
    (:up (ecase state
           (:walk (assets-up-walk assets))
           (:idle (assets-up-idle assets))
           (:attack (assets-up-attack assets))))
    (:side (ecase state
            (:walk (assets-side-walk assets))
            (:idle (assets-side-idle assets))
            (:attack (assets-side-attack assets))))))

(defun draw-player (player assets render)
  ;; Render the player sprite at its world position.
  (let* ((direction (player-facing player))
         (state (player-anim-state player))
         (dx (player-dx player))
         (flip (and (eq direction :side) (> dx 0.0)))
         (texture (player-texture-for assets direction state))
         (src-x (* (player-frame-index player) *sprite-frame-width*))
         (src-x (if flip
                    (+ src-x *sprite-frame-width*)
                    src-x))
         (src-width (if flip
                        (- *sprite-frame-width*)
                        *sprite-frame-width*))
         (half-width (assets-half-sprite-width assets))
         (half-height (assets-half-sprite-height assets)))
    (set-rectangle (render-player-source render)
                   src-x 0.0
                   src-width *sprite-frame-height*)
    (set-rectangle (render-player-dest render)
                   (- (player-x player) half-width)
                   (- (player-y player) half-height)
                   (assets-scaled-width assets)
                   (assets-scaled-height assets))
    (raylib:draw-texture-pro texture
                             (render-player-source render)
                             (render-player-dest render)
                             (render-origin render)
                             0.0
                             raylib:+white+)
    (multiple-value-bind (hp max-hp)
        (combatant-health player)
      (draw-health-bar (player-x player) (player-y player) hp max-hp assets))
    (when (player-hit-active player)
      (draw-hit-effect (player-x player)
                       (player-y player)
                       (player-hit-facing player)
                       (player-hit-facing-sign player)
                       (player-hit-frame player)
                       assets
                       (render-player-source render)
                       (render-player-dest render)
                       (render-origin render)))))

(defmethod draw-entity ((entity npc) assets render)
  (draw-npc entity assets render))

(defmethod draw-entity ((entity player) assets render)
  (draw-player entity assets render))

(defun draw-combat-log (ui start-x start-y)
  ;; Draw combat log lines when debug overlay is enabled.
  (let* ((buffer (ui-combat-log-buffer ui))
         (count (ui-combat-log-count ui))
         (cap (length buffer))
         (text-size (ui-combat-log-text-size ui))
         (line-height (+ text-size (ui-combat-log-line-gap ui))))
    (when (and (> cap 0) (> count 0))
      (let ((start (mod (- (ui-combat-log-index ui) count) cap)))
        (loop :for i :from 0 :below count
              :for index = (mod (+ start i) cap)
              :for text = (aref buffer index)
              :when (and text (not (string= text "")))
                :do (raylib:draw-text text
                                      start-x
                                      (+ start-y (* i line-height))
                                      text-size
                                      *debug-npc-text-color*))))))

(defun draw-hud-log (ui start-x start-y)
  ;; Draw HUD feedback lines in screen space.
  (let* ((buffer (ui-hud-log-buffer ui))
         (times (ui-hud-log-times ui))
         (count (ui-hud-log-count ui))
         (cap (length buffer))
         (text-size (ui-hud-log-text-size ui))
         (line-height (+ text-size (ui-hud-log-line-gap ui))))
    (when (and (> cap 0) (> count 0))
      (let ((start (mod (- (ui-hud-log-index ui) count) cap)))
        (let ((drawn 0))
          (loop :for i :from 0 :below count
                :for index = (mod (+ start i) cap)
                :for text = (aref buffer index)
                :for timer = (if times (aref times index) 0.0)
                :when (and text (not (string= text ""))
                           (> timer 0.0))
                  :do (let* ((fade (max 0.01 *hud-log-fade-seconds*))
                             (alpha (if (> timer fade)
                                        1.0
                                        (clamp (/ timer fade) 0.0 1.0)))
                             (color (raylib:fade raylib:+white+ alpha)))
                        (raylib:draw-text text
                                          start-x
                                          (+ start-y (* drawn line-height))
                                          text-size
                                          color)
                        (incf drawn))))))))

(defun draw-chat-input (ui start-x start-y)
  ;; Draw the active chat input line.
  (let* ((prompt (ui-chat-prompt ui))
         (buffer (ui-chat-buffer ui))
         (text (if (and buffer (> (length buffer) 0))
                   (format nil "~a ~a" prompt buffer)
                   (format nil "~a " prompt)))
         (text-size (ui-hud-log-text-size ui))
         (padding 6)
         (width (+ (* 9 (length text)) (* padding 2)))
         (height (+ text-size 8))
         (bg-color (ui-hud-bg-color ui)))
    (raylib:draw-rectangle (- start-x padding)
                           (- start-y 4)
                           width
                           height
                           bg-color)
    (raylib:draw-text text
                      start-x
                      start-y
                      text-size
                      (ui-menu-text-color ui))))

(defun draw-hud (player ui world)
  ;; Draw stamina HUD, zone label, and player stats.
  (let* ((labels (ui-stamina-labels ui))
         (max-index (1- (length labels)))
         (run-seconds (max 0 (min (truncate (player-run-stamina player))
                                  max-index)))
         (run-text (aref labels run-seconds))
         (zone-label (or (world-zone-label world) "NONE"))
         (zone-width (+ 60 (* 10 (length zone-label))))
         (hud-x 10)
         (hud-y 10)
         (stats-x hud-x)
         (stats-y 38))
    (raylib:draw-rectangle 6 6 110 24 (ui-hud-bg-color ui))
    (raylib:draw-text run-text hud-x hud-y 20 raylib:+white+)
    (raylib:draw-rectangle 122 6 zone-width 24 (ui-hud-bg-color ui))
    (raylib:draw-text "Zone" 128 hud-y 20 raylib:+white+)
    (raylib:draw-text zone-label 176 hud-y 20 raylib:+white+)
    ;; FPS and Ping display (top-right corner)
    (let* ((fps (raylib:get-fps))
           (ping-ms (ui-ping-rtt-ms ui))
           (stats-text (if ping-ms
                           (format nil "~dms | ~d FPS" ping-ms fps)
                           (format nil "~d FPS" fps)))
           (stats-width (+ 20 (* 10 (length stats-text))))
           (stats-x (- *window-width* stats-width 6)))
      (raylib:draw-rectangle stats-x 6 stats-width 24 (ui-hud-bg-color ui))
      (raylib:draw-text stats-text (+ stats-x 10) hud-y 20 raylib:+white+))
    (let ((hover-name (ui-hover-npc-name ui)))
      (when hover-name
        (let* ((text-size 20)
               (padding 10)
               (width (+ (* 10 (length hover-name)) (* padding 2)))
               (x (truncate (/ (- *window-width* width) 2)))
               (y 6))
          (raylib:draw-rectangle x y width 24 (ui-hud-bg-color ui))
          (raylib:draw-text hover-name
                            (+ x padding)
                            hud-y
                            text-size
                            raylib:+white+))))
    (ensure-player-hud-stats player)
    (let* ((lines (player-hud-stats-lines player))
           (count (player-hud-stats-count player))
           (text-size (ui-hud-stats-text-size ui))
           (line-height (+ text-size (ui-hud-stats-line-gap ui)))
           (log-start-y (+ stats-y (* count line-height) 6))
           (chat-active (ui-chat-active ui))
           (log-offset (if chat-active
                           (+ (ui-hud-log-text-size ui)
                              (ui-hud-log-line-gap ui)
                              8)
                           0)))
      (when lines
        (loop :for i :from 0 :below count
              :for text = (aref lines i)
              :do (raylib:draw-text text
                                    stats-x
                                    (+ stats-y (* i line-height))
                                    text-size
                                    raylib:+white+))
        (when chat-active
          (draw-chat-input ui stats-x log-start-y))
        (if *debug-collision-overlay*
            (draw-combat-log ui
                             stats-x
                             (+ log-start-y log-offset))
            (draw-hud-log ui
                          stats-x
                          (+ log-start-y log-offset)))))))

(defun draw-inventory (player ui render assets)
  ;; Draw the inventory overlay when enabled.
  (when (and player (ui-inventory-open ui))
    (let* ((inventory (player-inventory player))
           (slots (and inventory (inventory-slots inventory)))
           (slot-count (if slots (length slots) 0))
           (title-size 24)
           (count-size 12)
           (panel-color (ui-menu-panel-color ui))
           (text-color (ui-menu-text-color ui))
           (slot-color (raylib:fade (ui-menu-panel-color ui) 0.7))
           (slot-border (ui-menu-text-color ui))
           ;; Drag state
           (dragging (ui-drag-active ui))
           (drag-source-slot (and dragging (ui-drag-slot-index ui)))
           (drag-item-id (and dragging (ui-drag-item-id ui)))
           (mouse-x (raylib:get-mouse-x))
           (mouse-y (raylib:get-mouse-y))
           ;; Colors for drag feedback
           (dimmed-slot-color (raylib:fade slot-color 0.3))
           (highlight-color (raylib:make-color :r 255 :g 255 :b 100 :a 100)))
      (multiple-value-bind (grid-x grid-y slot-size gap columns rows
                            panel-x panel-y panel-width panel-height header-height padding)
          (inventory-grid-layout ui slot-count)
        (declare (ignore rows header-height))
        (raylib:draw-rectangle panel-x panel-y panel-width panel-height panel-color)
        (raylib:draw-rectangle-lines panel-x panel-y panel-width panel-height text-color)
        (raylib:draw-text "Inventory"
                          (+ panel-x padding)
                          (+ panel-y padding)
                          title-size
                          text-color)
        (let ((source (render-tile-source render))
              (dest (render-tile-dest render))
              (origin (render-origin render))
              (hover-slot nil))
          ;; Determine which slot mouse is hovering over (for drag target highlight)
          (when dragging
            (dotimes (index slot-count)
              (let* ((col (mod index columns))
                     (row (floor index columns))
                     (slot-x (+ grid-x (* col (+ slot-size gap))))
                     (slot-y (+ grid-y (* row (+ slot-size gap)))))
                (when (and (>= mouse-x slot-x) (< mouse-x (+ slot-x slot-size))
                           (>= mouse-y slot-y) (< mouse-y (+ slot-y slot-size)))
                  (setf hover-slot index)))))
          ;; Draw slots
          (dotimes (index slot-count)
            (let* ((col (mod index columns))
                   (row (floor index columns))
                   (slot-x (+ grid-x (* col (+ slot-size gap))))
                   (slot-y (+ grid-y (* row (+ slot-size gap))))
                   ;; Dim source slot, highlight hover slot
                   (is-source (and dragging (eql index drag-source-slot)))
                   (is-hover-target (and dragging hover-slot (eql index hover-slot)
                                         (not is-source)))
                   (current-slot-color (if is-source dimmed-slot-color slot-color)))
              (raylib:draw-rectangle slot-x slot-y slot-size slot-size current-slot-color)
              ;; Draw highlight for hover target
              (when is-hover-target
                (raylib:draw-rectangle slot-x slot-y slot-size slot-size highlight-color))
              (raylib:draw-rectangle-lines slot-x slot-y slot-size slot-size slot-border)
              ;; Draw item (skip rendering item in source slot if dragging)
              (when (and slots (< index (length slots)) (not is-source))
                (let* ((slot (aref slots index))
                       (item-id (inventory-slot-item-id slot))
                       (count (inventory-slot-count slot)))
                  (when (and item-id (> count 0))
                    (let* ((texture (item-texture-for assets item-id))
                           (dest-x (float slot-x 1.0))
                           (dest-y (float slot-y 1.0))
                           (dest-size (float slot-size 1.0)))
                      (if texture
                          (let ((src-w (float (raylib:texture-width texture) 1.0))
                                (src-h (float (raylib:texture-height texture) 1.0)))
                            (set-rectangle source 0.0 0.0 src-w src-h)
                            (set-rectangle dest dest-x dest-y dest-size dest-size)
                            (raylib:draw-texture-pro texture
                                                     source
                                                     dest
                                                     origin
                                                     0.0
                                                     raylib:+white+))
                          (let* ((item (find-item-archetype item-id))
                                 (label (or (and item (item-archetype-name item))
                                            (string-capitalize (string item-id)))))
                            (raylib:draw-text label
                                              (+ slot-x 4)
                                              (+ slot-y 4)
                                              10
                                              text-color))))
                    (let ((count-text (format nil "~d" count)))
                      (raylib:draw-text count-text
                                        (+ slot-x (- slot-size 18))
                                        (+ slot-y (- slot-size 16))
                                        count-size
                                        text-color)))))))
          ;; Draw dragged item at cursor position (last, so it's on top)
          (when (and dragging drag-item-id)
            (let* ((texture (item-texture-for assets drag-item-id))
                   (half-size (floor slot-size 2))
                   (drag-dest-x (float (- mouse-x half-size) 1.0))
                   (drag-dest-y (float (- mouse-y half-size) 1.0))
                   (drag-dest-size (float slot-size 1.0)))
              (if texture
                  (let ((src-w (float (raylib:texture-width texture) 1.0))
                        (src-h (float (raylib:texture-height texture) 1.0)))
                    (set-rectangle source 0.0 0.0 src-w src-h)
                    (set-rectangle dest drag-dest-x drag-dest-y drag-dest-size drag-dest-size)
                    (raylib:draw-texture-pro texture
                                             source
                                             dest
                                             origin
                                             0.0
                                             (raylib:fade raylib:+white+ 0.8)))
                  (let* ((item (find-item-archetype drag-item-id))
                         (label (or (and item (item-archetype-name item))
                                    (string-capitalize (string drag-item-id)))))
                    (raylib:draw-text label
                                      (truncate drag-dest-x)
                                      (truncate drag-dest-y)
                                      14
                                      text-color))))))))))

(defun minimap-world-to-screen (ui world player world-x world-y)
  ;; Convert world coordinates into minimap screen space.
  (multiple-value-bind (view-min-x view-min-y span-x span-y)
      (minimap-view-bounds world player)
    (let* ((rx (if (> span-x 0.0)
                   (/ (- world-x view-min-x) span-x)
                   0.0))
           (ry (if (> span-y 0.0)
                   (/ (- world-y view-min-y) span-y)
                   0.0)))
      (if (and (<= 0.0 rx) (<= rx 1.0)
               (<= 0.0 ry) (<= ry 1.0))
          (values (+ (ui-minimap-x ui) (* rx (ui-minimap-width ui)))
                  (+ (ui-minimap-y ui) (* ry (ui-minimap-height ui))))
          (values nil nil)))))

(defun draw-minimap (world player npcs ui)
  ;; Render the minimap overlay and markers.
  (let* ((map-x (ui-minimap-x ui))
         (map-y (ui-minimap-y ui))
         (map-width (ui-minimap-width ui))
         (map-height (ui-minimap-height ui))
         (point-size (ui-minimap-point-size ui))
         (half (truncate (/ point-size 2))))
    (raylib:draw-rectangle map-x map-y map-width map-height
                           (ui-minimap-bg-color ui))
    (raylib:draw-rectangle-lines map-x map-y map-width map-height
                                 (ui-minimap-border-color ui))
    (let* ((collisions (world-minimap-collisions world))
           (collision-size (max 1 (truncate (/ point-size 2))))
           (collision-half (truncate (/ collision-size 2))))
      (when (and collisions (> (length collisions) 0))
        (loop :for i :from 0 :below (length collisions) :by 2
              :for wx = (aref collisions i)
              :for wy = (aref collisions (1+ i))
              :do (multiple-value-bind (sx sy)
                      (minimap-world-to-screen ui world player wx wy)
                    (when (and sx sy)
                      (raylib:draw-rectangle (- (truncate sx) collision-half)
                                             (- (truncate sy) collision-half)
                                             collision-size
                                             collision-size
                                             (ui-minimap-collision-color ui)))))))
    (multiple-value-bind (px py)
        (minimap-world-to-screen ui world player (player-x player) (player-y player))
      (when (and px py)
        (raylib:draw-rectangle (- (truncate px) half)
                               (- (truncate py) half)
                               point-size
                               point-size
                               (ui-minimap-player-color ui))))
    ;; Draw NPCs within minimap view radius (squared distance check avoids sqrt)
    (let* ((player-x (player-x player))
           (player-y (player-y player))
           (radius-sq (* *minimap-npc-view-radius* *minimap-npc-view-radius*)))
      (loop :for npc :across npcs
            :when (npc-alive npc)
            :do (let* ((dx (- (npc-x npc) player-x))
                       (dy (- (npc-y npc) player-y))
                       (dist-sq (+ (* dx dx) (* dy dy))))
                  (when (<= dist-sq radius-sq)
                    (multiple-value-bind (nx ny)
                        (minimap-world-to-screen ui world player (npc-x npc) (npc-y npc))
                      (when (and nx ny)
                        (raylib:draw-rectangle (- (truncate nx) half)
                                               (- (truncate ny) half)
                                               point-size
                                               point-size
                                               (ui-minimap-npc-color ui))))))))
    (let* ((preview (world-minimap-spawns world))
           (preview-size (max 2 (truncate (/ point-size 2))))
           (preview-half (truncate (/ preview-size 2)))
           (preview-edge (world-preview-edge world player))
           (preview-exit (and preview-edge (world-edge-exit world preview-edge))))
      (when preview-exit
        (dolist (entry preview)
          (destructuring-bind (edge px py) entry
            (when (eq edge preview-edge)
              (multiple-value-bind (sx sy)
                  (minimap-world-to-screen ui world player px py)
                (when (and sx sy)
                  (raylib:draw-rectangle (- (truncate sx) preview-half)
                                         (- (truncate sy) preview-half)
                                         preview-size
                                         preview-size
                                         (ui-minimap-npc-color ui)))))))))))

(defun draw-loading-overlay (ui)
  ;; Draw a brief loading label while zones swap.
  (when (> (ui-loading-timer ui) 0.0)
    (let* ((label (ui-loading-label ui))
           (font-size 22)
           (padding 16)
           (text-width (truncate (* (length label) (* font-size 0.6))))
           (box-width (+ text-width (* padding 2)))
           (box-height (+ font-size (* padding 2)))
           (box-x (truncate (- (/ *window-width* 2) (/ box-width 2))))
           (box-y (truncate (- (/ *window-height* 2) (/ box-height 2))))
           (text-x (+ box-x padding))
           (text-y (+ box-y padding)))
      (raylib:draw-rectangle box-x box-y box-width box-height
                             (ui-menu-panel-color ui))
      (raylib:draw-text label text-x text-y font-size
                        (ui-menu-text-color ui)))))

(defun draw-editor-tileset-preview (editor render)
  ;; Draw the active tileset sheet in screen space for tile picking.
  (when (and editor (editor-active editor))
    (multiple-value-bind (x y w h scale tileset)
        (editor-tileset-preview-layout editor)
      (when (and x tileset)
        (let* ((texture (editor-tileset-texture tileset))
               (source (render-tile-source render))
               (dest (render-tile-dest render))
               (origin (render-origin render)))
          (raylib:draw-rectangle (round x) (round y) (round w) (round h)
                                 *editor-tileset-preview-bg-color*)
          (set-rectangle source 0.0 0.0
                         (float (raylib:texture-width texture) 1.0)
                         (float (raylib:texture-height texture) 1.0))
          (set-rectangle dest x y w h)
          (raylib:draw-texture-pro texture source dest origin 0.0 raylib:+white+)
          (raylib:draw-rectangle-lines (round x) (round y) (round w) (round h)
                                       *editor-tileset-preview-border-color*)
          (let* ((tile-size (* (max 1.0 (float *tile-size* 1.0)) scale))
                 (columns (max 1 (editor-tileset-columns tileset)))
                 (rows (max 1 (editor-tileset-rows tileset)))
                 (tile-index (editor-selected-tile editor))
                 (tx (mod tile-index columns))
                 (ty (floor tile-index columns))
                 (sel-w (min (max 1 (editor-selection-width editor))
                             (max 1 (- columns tx))))
                 (sel-h (min (max 1 (editor-selection-height editor))
                             (max 1 (- rows ty)))))
            (when (and (> tile-size 0.0)
                       (<= 0 tx) (< tx columns)
                       (<= 0 ty) (< ty rows))
              (let ((sel-x (+ x (* tx tile-size)))
                    (sel-y (+ y (* ty tile-size)))
                    (sel-w-px (* tile-size sel-w))
                    (sel-h-px (* tile-size sel-h)))
                (raylib:draw-rectangle-lines (round sel-x) (round sel-y)
                                             (round sel-w-px) (round sel-h-px)
                                             *editor-tileset-preview-highlight-color*)))))))))

(defun draw-menu (ui audio editor)
  ;; Render the pause menu and hover states.
  (let* ((mouse-x (raylib:get-mouse-x))
         (mouse-y (raylib:get-mouse-y))
         (hover-logout (point-in-rect-p mouse-x mouse-y
                                        (ui-menu-logout-x ui)
                                        (ui-menu-logout-y ui)
                                        (ui-menu-logout-width ui)
                                        (ui-menu-logout-height ui)))
         (hover-unstuck (point-in-rect-p mouse-x mouse-y
                                         (ui-menu-unstuck-x ui)
                                         (ui-menu-unstuck-y ui)
                                         (ui-menu-unstuck-width ui)
                                         (ui-menu-unstuck-height ui)))
         (hover-prev (point-in-rect-p mouse-x mouse-y
                                      (ui-menu-prev-x ui)
                                      (ui-menu-nav-y ui)
                                      (ui-menu-nav-button-width ui)
                                      (ui-menu-nav-button-height ui)))
         (hover-next (point-in-rect-p mouse-x mouse-y
                                      (ui-menu-next-x ui)
                                      (ui-menu-nav-y ui)
                                      (ui-menu-nav-button-width ui)
                                      (ui-menu-nav-button-height ui)))
         (hover-vol-down (point-in-rect-p mouse-x mouse-y
                                          (ui-menu-volume-down-x ui)
                                          (ui-menu-volume-y ui)
                                          (ui-menu-volume-button-width ui)
                                          (ui-menu-volume-button-height ui)))
         (hover-vol-up (point-in-rect-p mouse-x mouse-y
                                        (ui-menu-volume-up-x ui)
                                        (ui-menu-volume-y ui)
                                        (ui-menu-volume-button-width ui)
                                        (ui-menu-volume-button-height ui)))
         (logout-color (if hover-logout
                           (ui-menu-button-hover-color ui)
                           (ui-menu-button-color ui)))
         (unstuck-color (if hover-unstuck
                            (ui-menu-button-hover-color ui)
                            (ui-menu-button-color ui)))
         (prev-color (if hover-prev
                         (ui-menu-button-hover-color ui)
                         (ui-menu-button-color ui)))
         (next-color (if hover-next
                         (ui-menu-button-hover-color ui)
                         (ui-menu-button-color ui)))
         (vol-down-color (if hover-vol-down
                             (ui-menu-button-hover-color ui)
                             (ui-menu-button-color ui)))
         (vol-up-color (if hover-vol-up
                           (ui-menu-button-hover-color ui)
                           (ui-menu-button-color ui)))
         (title-x (+ (ui-menu-panel-x ui) (ui-menu-padding ui)))
         (title-y (+ (ui-menu-panel-y ui) (ui-menu-padding ui)))
         (hint-y (+ (ui-menu-panel-y ui) (ui-menu-padding ui) 44))
         (track-title-y (- (ui-menu-nav-y ui) 28))
         (volume-label-y (- (ui-menu-volume-y ui) 26))
         (volume-bars-text (aref (audio-volume-bars audio)
                                 (audio-volume-level audio)))
         (debug-on *debug-collision-overlay*)
         (hover-debug (point-in-rect-p mouse-x mouse-y
                                       (ui-menu-debug-x ui)
                                       (ui-menu-debug-y ui)
                                       (ui-menu-debug-size ui)
                                       (ui-menu-debug-size ui)))
         (editor-on (and editor (editor-active editor)))
         (hover-editor (point-in-rect-p mouse-x mouse-y
                                        (ui-menu-editor-x ui)
                                        (ui-menu-editor-y ui)
                                        (ui-menu-editor-size ui)
                                        (ui-menu-editor-size ui)))
         (fs-on (raylib:is-window-fullscreen))
         (hover-fs (point-in-rect-p mouse-x mouse-y
                                    (ui-menu-fullscreen-x ui)
                                    (ui-menu-fullscreen-y ui)
                                    (ui-menu-fullscreen-size ui)
                                    (ui-menu-fullscreen-size ui)))
         ;; Client-side options
         (pred-on *client-prediction-enabled*)
         (hover-pred (point-in-rect-p mouse-x mouse-y
                                      (ui-menu-prediction-x ui)
                                      (ui-menu-prediction-y ui)
                                      (ui-menu-prediction-size ui)
                                      (ui-menu-prediction-size ui)))
         (tile-on *tile-point-filter*)
         (hover-tile (point-in-rect-p mouse-x mouse-y
                                      (ui-menu-tile-filter-x ui)
                                      (ui-menu-tile-filter-y ui)
                                      (ui-menu-tile-filter-size ui)
                                      (ui-menu-tile-filter-size ui)))
         (cache-on *render-cache-enabled*)
         (hover-cache (point-in-rect-p mouse-x mouse-y
                                       (ui-menu-render-cache-x ui)
                                       (ui-menu-render-cache-y ui)
                                       (ui-menu-render-cache-size ui)
                                       (ui-menu-render-cache-size ui)))
         (hover-interp (point-in-rect-p mouse-x mouse-y
                                        (ui-menu-interp-x ui)
                                        (ui-menu-interp-y ui)
                                        200
                                        (ui-menu-interp-size ui)))
         (hover-threshold (point-in-rect-p mouse-x mouse-y
                                           (ui-menu-threshold-x ui)
                                           (ui-menu-threshold-y ui)
                                           200
                                           (ui-menu-threshold-size ui)))
         (debug-box-color (cond
                            (hover-debug (ui-menu-button-hover-color ui))
                            (debug-on (ui-menu-button-color ui))
                            (t (ui-menu-panel-color ui))))
         (editor-box-color (cond
                             (hover-editor (ui-menu-button-hover-color ui))
                             (editor-on (ui-menu-button-color ui))
                             (t (ui-menu-panel-color ui))))
         (fs-box-color (cond
                         (hover-fs (ui-menu-button-hover-color ui))
                         (fs-on (ui-menu-button-color ui))
                         (t (ui-menu-panel-color ui))))
         (pred-box-color (cond
                           (hover-pred (ui-menu-button-hover-color ui))
                           (pred-on (ui-menu-button-color ui))
                           (t (ui-menu-panel-color ui))))
         (tile-box-color (cond
                           (hover-tile (ui-menu-button-hover-color ui))
                           (tile-on (ui-menu-button-color ui))
                           (t (ui-menu-panel-color ui))))
         (cache-box-color (cond
                            (hover-cache (ui-menu-button-hover-color ui))
                            (cache-on (ui-menu-button-color ui))
                            (t (ui-menu-panel-color ui))))
         (interp-text-color (if hover-interp
                                (ui-menu-button-hover-color ui)
                                (ui-menu-text-color ui)))
         (threshold-text-color (if hover-threshold
                                   (ui-menu-button-hover-color ui)
                                   (ui-menu-text-color ui))))
    (raylib:draw-rectangle 0 0 *window-width* *window-height*
                           (ui-menu-overlay-color ui))
    (raylib:draw-rectangle (ui-menu-panel-x ui) (ui-menu-panel-y ui)
                           (ui-menu-panel-width ui) (ui-menu-panel-height ui)
                           (ui-menu-panel-color ui))
    (raylib:draw-text (ui-menu-title ui)
                      title-x
                      title-y
                      (ui-menu-title-size ui)
                      (ui-menu-text-color ui))
    (raylib:draw-text (ui-menu-hint ui)
                      title-x
                      hint-y
                      (ui-menu-hint-size ui)
                      (ui-menu-text-color ui))
    (raylib:draw-text (ui-menu-track-title ui)
                      title-x
                      track-title-y
                      (ui-menu-track-size ui)
                      (ui-menu-text-color ui))
    (raylib:draw-rectangle (ui-menu-prev-x ui) (ui-menu-nav-y ui)
                           (ui-menu-nav-button-width ui)
                           (ui-menu-nav-button-height ui)
                           prev-color)
    (raylib:draw-text (ui-menu-prev-label ui)
                      (+ (ui-menu-prev-x ui) 18)
                      (+ (ui-menu-nav-y ui) 12)
                      (ui-menu-nav-text-size ui)
                      (ui-menu-text-color ui))
    (raylib:draw-rectangle (ui-menu-next-x ui) (ui-menu-nav-y ui)
                           (ui-menu-nav-button-width ui)
                           (ui-menu-nav-button-height ui)
                           next-color)
    (raylib:draw-text (ui-menu-next-label ui)
                      (+ (ui-menu-next-x ui) 18)
                      (+ (ui-menu-nav-y ui) 12)
                      (ui-menu-nav-text-size ui)
                      (ui-menu-text-color ui))
    (raylib:draw-text (audio-current-track-label audio)
                      (ui-menu-track-text-x ui)
                      (ui-menu-track-text-y ui)
                      (ui-menu-track-size ui)
                      (ui-menu-text-color ui))
    (raylib:draw-text "Volume"
                      (ui-menu-track-text-x ui)
                      volume-label-y
                      (ui-menu-volume-text-size ui)
                      (ui-menu-text-color ui))
    (raylib:draw-rectangle (ui-menu-volume-down-x ui)
                           (ui-menu-volume-y ui)
                           (ui-menu-volume-button-width ui)
                           (ui-menu-volume-button-height ui)
                           vol-down-color)
    (raylib:draw-text (ui-menu-vol-down-label ui)
                      (+ (ui-menu-volume-down-x ui) 14)
                      (+ (ui-menu-volume-y ui) 10)
                      (ui-menu-volume-text-size ui)
                      (ui-menu-text-color ui))
    (raylib:draw-rectangle (ui-menu-volume-up-x ui)
                           (ui-menu-volume-y ui)
                           (ui-menu-volume-button-width ui)
                           (ui-menu-volume-button-height ui)
                           vol-up-color)
    (raylib:draw-text (ui-menu-vol-up-label ui)
                      (+ (ui-menu-volume-up-x ui) 14)
                      (+ (ui-menu-volume-y ui) 10)
                      (ui-menu-volume-text-size ui)
                      (ui-menu-text-color ui))
    (raylib:draw-text volume-bars-text
                      (ui-menu-volume-bars-x ui)
                      (+ (ui-menu-volume-y ui) 10)
                      (ui-menu-volume-text-size ui)
                      (ui-menu-text-color ui))
    (raylib:draw-rectangle (ui-menu-debug-x ui)
                           (ui-menu-debug-y ui)
                           (ui-menu-debug-size ui)
                           (ui-menu-debug-size ui)
                           debug-box-color)
    (raylib:draw-rectangle-lines (ui-menu-debug-x ui)
                                 (ui-menu-debug-y ui)
                                 (ui-menu-debug-size ui)
                                 (ui-menu-debug-size ui)
                                 (ui-menu-text-color ui))
    (raylib:draw-text (ui-menu-debug-label ui)
                      (+ (ui-menu-debug-x ui) 28)
                      (- (ui-menu-debug-y ui) 2)
                      (ui-menu-volume-text-size ui)
                      (ui-menu-text-color ui))
    (raylib:draw-rectangle (ui-menu-editor-x ui)
                           (ui-menu-editor-y ui)
                           (ui-menu-editor-size ui)
                           (ui-menu-editor-size ui)
                           editor-box-color)
    (raylib:draw-rectangle-lines (ui-menu-editor-x ui)
                                 (ui-menu-editor-y ui)
                                 (ui-menu-editor-size ui)
                                 (ui-menu-editor-size ui)
                                 (ui-menu-text-color ui))
    (raylib:draw-text (ui-menu-editor-label ui)
                      (+ (ui-menu-editor-x ui) 28)
                      (- (ui-menu-editor-y ui) 2)
                      (ui-menu-volume-text-size ui)
                      (ui-menu-text-color ui))
    (raylib:draw-rectangle (ui-menu-fullscreen-x ui)
                           (ui-menu-fullscreen-y ui)
                           (ui-menu-fullscreen-size ui)
                           (ui-menu-fullscreen-size ui)
                           fs-box-color)
    (raylib:draw-rectangle-lines (ui-menu-fullscreen-x ui)
                                 (ui-menu-fullscreen-y ui)
                                 (ui-menu-fullscreen-size ui)
                                 (ui-menu-fullscreen-size ui)
                                 (ui-menu-text-color ui))
    (raylib:draw-text (ui-menu-fullscreen-label ui)
                      (+ (ui-menu-fullscreen-x ui) 28)
                      (- (ui-menu-fullscreen-y ui) 2)
                      (ui-menu-volume-text-size ui)
                      (ui-menu-text-color ui))
    ;; Client prediction toggle
    (raylib:draw-rectangle (ui-menu-prediction-x ui)
                           (ui-menu-prediction-y ui)
                           (ui-menu-prediction-size ui)
                           (ui-menu-prediction-size ui)
                           pred-box-color)
    (raylib:draw-rectangle-lines (ui-menu-prediction-x ui)
                                 (ui-menu-prediction-y ui)
                                 (ui-menu-prediction-size ui)
                                 (ui-menu-prediction-size ui)
                                 (ui-menu-text-color ui))
    (raylib:draw-text (ui-menu-prediction-label ui)
                      (+ (ui-menu-prediction-x ui) 28)
                      (- (ui-menu-prediction-y ui) 2)
                      (ui-menu-volume-text-size ui)
                      (ui-menu-text-color ui))
    ;; Tile filter toggle
    (raylib:draw-rectangle (ui-menu-tile-filter-x ui)
                           (ui-menu-tile-filter-y ui)
                           (ui-menu-tile-filter-size ui)
                           (ui-menu-tile-filter-size ui)
                           tile-box-color)
    (raylib:draw-rectangle-lines (ui-menu-tile-filter-x ui)
                                 (ui-menu-tile-filter-y ui)
                                 (ui-menu-tile-filter-size ui)
                                 (ui-menu-tile-filter-size ui)
                                 (ui-menu-text-color ui))
    (raylib:draw-text (ui-menu-tile-filter-label ui)
                      (+ (ui-menu-tile-filter-x ui) 28)
                      (- (ui-menu-tile-filter-y ui) 2)
                      (ui-menu-volume-text-size ui)
                      (ui-menu-text-color ui))
    ;; Render cache toggle
    (raylib:draw-rectangle (ui-menu-render-cache-x ui)
                           (ui-menu-render-cache-y ui)
                           (ui-menu-render-cache-size ui)
                           (ui-menu-render-cache-size ui)
                           cache-box-color)
    (raylib:draw-rectangle-lines (ui-menu-render-cache-x ui)
                                 (ui-menu-render-cache-y ui)
                                 (ui-menu-render-cache-size ui)
                                 (ui-menu-render-cache-size ui)
                                 (ui-menu-text-color ui))
    (raylib:draw-text (ui-menu-render-cache-label ui)
                      (+ (ui-menu-render-cache-x ui) 28)
                      (- (ui-menu-render-cache-y ui) 2)
                      (ui-menu-volume-text-size ui)
                      (ui-menu-text-color ui))
    ;; Interpolation delay (click to cycle)
    (raylib:draw-text (format nil "Interp Delay: ~,2fs (click)" *interpolation-delay-seconds*)
                      (ui-menu-interp-x ui)
                      (- (ui-menu-interp-y ui) 2)
                      (ui-menu-volume-text-size ui)
                      interp-text-color)
    ;; Prediction threshold (click to cycle)
    (raylib:draw-text (format nil "Predict Threshold: ~,1fpx (click)" *prediction-error-threshold*)
                      (ui-menu-threshold-x ui)
                      (- (ui-menu-threshold-y ui) 2)
                      (ui-menu-volume-text-size ui)
                      threshold-text-color)
    ;; Unstuck button (above logout)
    (raylib:draw-rectangle (ui-menu-unstuck-x ui) (ui-menu-unstuck-y ui)
                           (ui-menu-unstuck-width ui)
                           (ui-menu-unstuck-height ui)
                           unstuck-color)
    (raylib:draw-text (ui-menu-unstuck-label ui)
                      (+ (ui-menu-unstuck-x ui) 24)
                      (+ (ui-menu-unstuck-y ui) 16)
                      (ui-menu-button-text-size ui)
                      (ui-menu-text-color ui))
    ;; Logout button
    (raylib:draw-rectangle (ui-menu-logout-x ui) (ui-menu-logout-y ui)
                           (ui-menu-logout-width ui)
                           (ui-menu-logout-height ui)
                           logout-color)
    (raylib:draw-text (ui-menu-logout-label ui)
                      (+ (ui-menu-logout-x ui) 24)
                      (+ (ui-menu-logout-y ui) 16)
                      (ui-menu-button-text-size ui)
                      (ui-menu-text-color ui))))

(defun draw-context-menu (ui)
  ;; Render the right-click context menu.
  (when (ui-context-open ui)
    (let* ((x (ui-context-x ui))
           (y (ui-context-y ui))
           (width (ui-context-width ui))
           (option-height (ui-context-option-height ui))
           (padding (ui-context-padding ui))
           (text-size (ui-context-text-size ui))
           (count (context-menu-option-count ui))
           (height (* count option-height))
           (text-color (ui-menu-text-color ui))
           (panel-color (ui-menu-panel-color ui))
           (mouse-x (raylib:get-mouse-x))
           (mouse-y (raylib:get-mouse-y))
           (hover-index (when (point-in-rect-p mouse-x mouse-y x y width height)
                          (floor (/ (- mouse-y y) option-height))))
           (hover-color (raylib:fade (ui-menu-button-hover-color ui) 0.6)))
      (raylib:draw-rectangle x y width height panel-color)
      (raylib:draw-rectangle-lines x y width height text-color)
      (let ((option-index 0)
            (actions (context-menu-actions ui)))
        (labels ((label-for (action)
                   (ecase action
                     (:walk (ui-context-walk-label ui))
                     (:attack (ui-context-attack-label ui))
                     (:follow (ui-context-follow-label ui))
                     (:pickup (ui-context-pickup-label ui))
                     (:examine (ui-context-examine-label ui))
                     (:drop (ui-context-drop-label ui))))
                 (draw-option (action)
                   (let ((option-y (+ y (* option-height option-index))))
                     (when (and hover-index (= hover-index option-index))
                       (raylib:draw-rectangle x option-y width option-height hover-color))
                     (raylib:draw-text (label-for action)
                                       (+ x padding)
                                       (+ option-y padding)
                                       text-size
                                       text-color))
                   (incf option-index)))
          (dolist (action actions)
            (draw-option action)))))))

(defun draw-game (game)
  ;; Render a full frame: world, entities, HUD, and menu.
  (with-timing (:draw-game)
    (let* ((player (game-player game))
         (npcs (game-npcs game))
         (entities (game-entities game))
         (world (game-world game))
         (audio (game-audio game))
         (ui (game-ui game))
         (render (game-render game))
         (assets (game-assets game))
         (camera (game-camera game))
         (editor (game-editor game)))
    (raylib:with-drawing
      (raylib:clear-background raylib:+black+)
      (multiple-value-bind (camera-x camera-y)
          (editor-camera-target editor player)
        (let* ((zoom (camera-zoom camera))
               (margin-x (assets-half-sprite-width assets))
               (margin-y (assets-half-sprite-height assets))
               (camera-2d (raylib:make-camera-2d
                           :target (raylib:make-vector2 :x camera-x :y camera-y)
                           :offset (camera-offset camera)
                           :rotation 0.0
                           :zoom zoom)))
          (raylib:with-mode-2d camera-2d
            (draw-world world render assets camera player npcs ui editor)
            (draw-zone-objects world render assets camera player editor)
            (loop :for entity :across entities
                  :when (entity-in-viewport-p entity camera-x camera-y zoom margin-x margin-y)
                  :do (draw-entity entity assets render))
            (draw-click-marker player world)
            (draw-editor-world-overlay editor world camera))))
      (draw-hud player ui world)
      (draw-minimap world player npcs ui)
      (draw-inventory player ui render assets)
      (draw-context-menu ui)
      (draw-loading-overlay ui)
      (draw-editor-ui-overlay editor ui)
      (draw-editor-tileset-preview editor render)
      (when (ui-menu-open ui)
        (draw-menu ui audio editor))))))
