(in-package #:mmorpg)

(defun floor-tile-at (x y main-index variant-indices)
  ;; Choose a floor tile index with clustered variant noise.
  (let* ((cluster-size (max 1 *floor-cluster-size*))
         (variant-count (length variant-indices))
         (variant-mod (max 1 *floor-variant-mod*))
         (cx (floor x cluster-size))
         (cy (floor y cluster-size))
         (h (u32-hash cx cy *floor-seed*))
         (h2 (u32-hash (+ cx 17) (+ cy 31) (+ *floor-seed* 7331))))
    (if (and (> variant-count 0)
             (zerop (mod h variant-mod)))
        (aref variant-indices (mod h2 variant-count))
        main-index)))

(defun landmark-tile-at (x y)
  ;; Choose an optional landmark tile index with sparse hashing.
  (let* ((variant-count (length *landmark-indices*))
         (variant-mod (max 1 *landmark-mod*))
         (h (u32-hash x y *landmark-seed*))
         (h2 (u32-hash (+ x 19) (+ y 47) (+ *landmark-seed* 101))))
    (if (and (> variant-count 0)
             (zerop (mod h variant-mod)))
        (aref *landmark-indices* (mod h2 variant-count))
        0)))

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

(defun set-tile-source-rect (rect tile-index tile-size-f)
  ;; Set the atlas source rectangle for a given tile index.
  (let* ((col (mod tile-index *tileset-columns*))
         (row (floor tile-index *tileset-columns*)))
    (set-rectangle rect
                   (* col tile-size-f)
                   (* row tile-size-f)
                   tile-size-f
                   tile-size-f)))

(defun set-map-tile-source-rect (rect tileset tile-index)
  ;; Set the atlas source rectangle for a TMX tileset tile index.
  (let* ((columns (map-tileset-columns tileset))
         (tile-size (float (map-tileset-tilewidth tileset) 1.0))
         (col (mod tile-index columns))
         (row (floor tile-index columns)))
    (set-rectangle rect
                   (* col tile-size)
                   (* row tile-size)
                   tile-size
                   tile-size)))

(defun draw-map-layer (layer tilesets tile-source tile-dest origin tile-dest-size
                             start-col end-col start-row end-row)
  ;; Draw a TMX layer using chunked culling.
  (loop :for chunk :across (map-layer-chunks layer)
        :for chunk-left = (map-chunk-x chunk)
        :for chunk-top = (map-chunk-y chunk)
        :for chunk-right = (+ chunk-left (map-chunk-width chunk) -1)
        :for chunk-bottom = (+ chunk-top (map-chunk-height chunk) -1)
        :when (and (<= chunk-left end-col)
                   (>= chunk-right start-col)
                   (<= chunk-top end-row)
                   (>= chunk-bottom start-row))
        :do (let* ((tiles (map-chunk-tiles chunk))
                   (width (map-chunk-width chunk)))
              (loop :for idx :across (map-chunk-nonzero-indices chunk)
                    :for local-x = (mod idx width)
                    :for local-y = (floor idx width)
                    :for tx = (+ chunk-left local-x)
                    :for ty = (+ chunk-top local-y)
                    :when (and (<= start-col tx end-col)
                               (<= start-row ty end-row))
                    :do (let* ((gid (aref tiles idx)))
                          (when (not (zerop gid))
                            (let ((tileset (map-tileset-for-gid tilesets gid)))
                              (when tileset
                                (let ((tile-index (map-tile-index tileset gid)))
                                  (set-map-tile-source-rect tile-source tileset tile-index)
                                  (set-rectangle tile-dest
                                                 (* tx tile-dest-size)
                                                 (* ty tile-dest-size)
                                                 tile-dest-size
                                                 tile-dest-size)
                                  (raylib:draw-texture-pro (map-tileset-texture tileset)
                                                           tile-source
                                                           tile-dest
                                                           origin
                                                           0.0
                                                           raylib:+white+))))))))))

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
         (map (world-map world))
         (map-tilesets (when map (load-map-tilesets map)))
         (npc-ids (npc-animation-set-ids))
         (npc-animations (make-hash-table :test 'eq))
         (tileset (raylib:load-texture *tileset-path*))
         (down-idle (raylib:load-texture (animation-path player-set :down-idle)))
         (down-walk (raylib:load-texture (animation-path player-set :down-walk)))
         (down-attack (raylib:load-texture (animation-path player-set :down-attack)))
         (up-idle (raylib:load-texture (animation-path player-set :up-idle)))
         (up-walk (raylib:load-texture (animation-path player-set :up-walk)))
         (up-attack (raylib:load-texture (animation-path player-set :up-attack)))
         (side-idle (raylib:load-texture (animation-path player-set :side-idle)))
         (side-walk (raylib:load-texture (animation-path player-set :side-walk)))
         (side-attack (raylib:load-texture (animation-path player-set :side-attack)))
         (blood-down (raylib:load-texture (animation-path blood-set :down)))
         (blood-up (raylib:load-texture (animation-path blood-set :up)))
         (blood-side (raylib:load-texture (animation-path blood-set :side))))
    (dolist (id npc-ids)
      (let ((set (get-animation-set id :npc)))
        (setf (gethash id npc-animations)
              (%make-npc-textures
               :down-idle (raylib:load-texture (animation-path set :down-idle))
               :up-idle (raylib:load-texture (animation-path set :up-idle))
               :side-idle (raylib:load-texture (animation-path set :side-idle))))))
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
                  :blood-down blood-down
                  :blood-up blood-up
                  :blood-side blood-side
                  :map-tilesets map-tilesets
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
  (let ((map-tilesets (assets-map-tilesets assets)))
    (when map-tilesets
      (loop :for tileset :across map-tilesets
            :do (let ((texture (map-tileset-texture tileset)))
                  (when texture
                    (raylib:unload-texture texture))))))
  (raylib:unload-texture (assets-blood-down assets))
  (raylib:unload-texture (assets-blood-up assets))
  (raylib:unload-texture (assets-blood-side assets)))

(defun draw-world (world render assets camera player npcs ui)
  ;; Render floor, map layers, and debug overlays.
  (let* ((tile-dest-size (world-tile-dest-size world))
         (tile-size-f (world-tile-size-f world))
         (floor-index (world-floor-index world))
         (tileset (assets-tileset assets))
         (tile-source (render-tile-source render))
         (tile-dest (render-tile-dest render))
         (origin (render-origin render))
         (map (world-map world))
         (map-tilesets (assets-map-tilesets assets))
         (wall-map (world-wall-map world))
         (x (player-x player))
         (y (player-y player))
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
         (end-row (ceiling view-bottom tile-dest-size)))
    (if map
        (progn
          (when *map-decoration-enabled*
            (loop :for row :from start-row :to end-row
                  :for dest-y :from (* start-row tile-dest-size) :by tile-dest-size
                  :do (loop :for col :from start-col :to end-col
                            :for dest-x :from (* start-col tile-dest-size) :by tile-dest-size
                            :for tile-index = (floor-tile-at col row
                                                             floor-index
                                                             *floor-variant-indices*)
                            :do (set-rectangle tile-dest dest-x dest-y
                                               tile-dest-size tile-dest-size)
                                (when (not (zerop tile-index))
                                  (set-tile-source-rect tile-source tile-index tile-size-f)
                                  (raylib:draw-texture-pro tileset
                                                           tile-source
                                                           tile-dest
                                                           origin
                                                           0.0
                                                           raylib:+white+))
                                (let ((landmark-index (landmark-tile-at col row)))
                                  (when (not (zerop landmark-index))
                                    (set-tile-source-rect tile-source landmark-index tile-size-f)
                                    (raylib:draw-texture-pro tileset
                                                             tile-source
                                                             tile-dest
                                                             origin
                                                             0.0
                                                             raylib:+white+)))))))
          (loop :for layer :across (map-data-layers map)
                :do (draw-map-layer layer map-tilesets tile-source tile-dest origin
                                    tile-dest-size start-col end-col start-row end-row)))
        (loop :for row :from start-row :to end-row
              :for dest-y :from (* start-row tile-dest-size) :by tile-dest-size
              :do (loop :for col :from start-col :to end-col
                        :for dest-x :from (* start-col tile-dest-size) :by tile-dest-size
                        :for tile-index = (floor-tile-at col row
                                                         floor-index
                                                         *floor-variant-indices*)
                        :do (set-rectangle tile-dest dest-x dest-y
                                           tile-dest-size tile-dest-size)
                            (when (not (zerop tile-index))
                              (set-tile-source-rect tile-source tile-index tile-size-f)
                              (raylib:draw-texture-pro tileset
                                                       tile-source
                                                       tile-dest
                                                       origin
                                                       0.0
                                                       raylib:+white+))
                            (let ((landmark-index (landmark-tile-at col row)))
                              (when (not (zerop landmark-index))
                                (set-tile-source-rect tile-source landmark-index tile-size-f)
                                (raylib:draw-texture-pro tileset
                                                         tile-source
                                                         tile-dest
                                                         origin
                                                         0.0
                                                         raylib:+white+)))
                            (let ((wall-index (wall-tile-at wall-map col row)))
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
                            :do (cond
                                  (map
                                  (cond
                                    ((map-tile-outside-bounds-p map col row)
                                     (raylib:draw-rectangle ix iy tile-px tile-px
                                                            (ui-debug-wall-color ui)))
                                    ((world-blocked-tile-p world col row)
                                     (raylib:draw-rectangle ix iy tile-px tile-px
                                                            (ui-debug-collision-color ui)))))
                                  (t
                                   (when (wall-blocked-p wall-map col row)
                                     (raylib:draw-rectangle ix iy tile-px tile-px
                                                            (ui-debug-collision-color ui)))
                               (when (not (zerop (wall-tile-at wall-map col row)))
                                 (raylib:draw-rectangle ix iy tile-px tile-px
                                                        (ui-debug-wall-color ui)))))
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
                                         (ui-debug-collision-color ui)))))))
  )

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
          (draw-health-bar (npc-x npc) (npc-y npc) hp max-hp assets))))
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

(defun draw-hud (player ui)
  ;; Draw stamina HUD using precomputed labels.
  (let* ((labels (ui-stamina-labels ui))
         (max-index (1- (length labels)))
         (run-seconds (max 0 (min (truncate (player-run-stamina player))
                                  max-index)))
         (run-text (aref labels run-seconds)))
    (raylib:draw-rectangle 6 6 110 24 (ui-hud-bg-color ui))
    (raylib:draw-text run-text 10 10 20 raylib:+white+)))

(defun draw-menu (ui audio)
  ;; Render the pause menu and hover states.
  (let* ((mouse-x (raylib:get-mouse-x))
         (mouse-y (raylib:get-mouse-y))
         (hover-quit (point-in-rect-p mouse-x mouse-y
                                      (ui-menu-button-x ui)
                                      (ui-menu-button-y ui)
                                      (ui-menu-button-width ui)
                                      (ui-menu-button-height ui)))
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
         (quit-color (if hover-quit
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
         (fs-on (raylib:is-window-fullscreen))
         (hover-fs (point-in-rect-p mouse-x mouse-y
                                    (ui-menu-fullscreen-x ui)
                                    (ui-menu-fullscreen-y ui)
                                    (ui-menu-fullscreen-size ui)
                                    (ui-menu-fullscreen-size ui)))
         (debug-box-color (cond
                            (hover-debug (ui-menu-button-hover-color ui))
                            (debug-on (ui-menu-button-color ui))
                            (t (ui-menu-panel-color ui))))
         (fs-box-color (cond
                         (hover-fs (ui-menu-button-hover-color ui))
                         (fs-on (ui-menu-button-color ui))
                         (t (ui-menu-panel-color ui)))))
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
    (raylib:draw-rectangle (ui-menu-button-x ui) (ui-menu-button-y ui)
                           (ui-menu-button-width ui)
                           (ui-menu-button-height ui)
                           quit-color)
    (raylib:draw-text (ui-menu-button-label ui)
                      (+ (ui-menu-button-x ui) 24)
                      (+ (ui-menu-button-y ui) 16)
                      (ui-menu-button-text-size ui)
                      (ui-menu-text-color ui))))

(defun draw-game (game)
  ;; Render a full frame: world, entities, HUD, and menu.
  (let* ((player (game-player game))
         (npcs (game-npcs game))
         (entities (game-entities game))
         (world (game-world game))
         (audio (game-audio game))
         (ui (game-ui game))
         (render (game-render game))
         (assets (game-assets game))
         (camera (game-camera game)))
    (raylib:with-drawing
      (raylib:clear-background raylib:+black+)
      (let ((camera-2d (raylib:make-camera-2d
                        :target (raylib:make-vector2 :x (player-x player)
                                                     :y (player-y player))
                        :offset (camera-offset camera)
                        :rotation 0.0
                        :zoom (camera-zoom camera))))
        (raylib:with-mode-2d camera-2d
          (draw-world world render assets camera player npcs ui)
          (loop :for entity :across entities
                :do (draw-entity entity assets render))))
      (draw-hud player ui)
      (when (ui-menu-open ui)
        (draw-menu ui audio)))))
