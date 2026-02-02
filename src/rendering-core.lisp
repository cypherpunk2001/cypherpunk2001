;; NOTE: If you change behavior here, update docs/rendering.md :)
;; rendering-core.lisp — Core draw helpers, rectangles, textures, color utils, shared rendering state
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
  (clear-all-zone-render-caches "shutdown"))

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

(defun object-texture-for (assets object-id)
  ;; Return the texture for OBJECT-ID, if loaded.
  (let ((textures (and assets (assets-object-textures assets))))
    (and textures object-id (gethash object-id textures))))

(defun item-texture-for (assets item-id)
  ;; Return the texture for ITEM-ID, if loaded.
  (let ((textures (and assets (assets-item-textures assets))))
    (and textures item-id (gethash item-id textures))))
