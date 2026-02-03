;; rendering-entities.lisp — Players/NPCs/objects rendering, culling, name plates, health bars, animations
(in-package #:mmorpg)

;;; Viewport and Distance Check Functions
;;; Type-specific versions avoid CLOS dispatch in hot render loops (Task 1.5)

(defun entity-in-viewport-p (entity camera-x camera-y zoom margin-x margin-y)
  "Return true if ENTITY is within the viewport bounds plus margin.
   CAMERA-X, CAMERA-Y: center of the camera view in world coordinates.
   ZOOM: camera zoom factor.
   MARGIN-X, MARGIN-Y: padding to add to viewport bounds (sprite half-sizes).
   Note: Uses generic combatant-position - prefer type-specific versions in hot loops."
  (let* ((half-view-width (/ (current-screen-width) (* 2.0 zoom)))
         (half-view-height (/ (current-screen-height) (* 2.0 zoom)))
         (view-left (- camera-x half-view-width margin-x))
         (view-right (+ camera-x half-view-width margin-x))
         (view-top (- camera-y half-view-height margin-y))
         (view-bottom (+ camera-y half-view-height margin-y)))
    (multiple-value-bind (ex ey) (combatant-position entity)
      (and (>= ex view-left)
           (<= ex view-right)
           (>= ey view-top)
           (<= ey view-bottom)))))

(defun npc-in-viewport-p (npc camera-x camera-y zoom margin-x margin-y)
  "Return true if NPC is within the viewport bounds plus margin.
   Non-generic version for hot render loops (Task 1.5 CLOS removal)."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type npc npc)
           (type single-float camera-x camera-y zoom margin-x margin-y))
  (let* ((half-view-width (/ (current-screen-width) (* 2.0 zoom)))
         (half-view-height (/ (current-screen-height) (* 2.0 zoom)))
         (view-left (- camera-x half-view-width margin-x))
         (view-right (+ camera-x half-view-width margin-x))
         (view-top (- camera-y half-view-height margin-y))
         (view-bottom (+ camera-y half-view-height margin-y))
         (ex (npc-x npc))
         (ey (npc-y npc)))
    (declare (type single-float half-view-width half-view-height
                   view-left view-right view-top view-bottom ex ey))
    (and (>= ex view-left)
         (<= ex view-right)
         (>= ey view-top)
         (<= ey view-bottom))))

(defun player-in-viewport-p (player camera-x camera-y zoom margin-x margin-y)
  "Return true if PLAYER is within the viewport bounds plus margin.
   Non-generic version for hot render loops (Task 1.5 CLOS removal)."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type player player)
           (type single-float camera-x camera-y zoom margin-x margin-y))
  (let* ((half-view-width (/ (current-screen-width) (* 2.0 zoom)))
         (half-view-height (/ (current-screen-height) (* 2.0 zoom)))
         (view-left (- camera-x half-view-width margin-x))
         (view-right (+ camera-x half-view-width margin-x))
         (view-top (- camera-y half-view-height margin-y))
         (view-bottom (+ camera-y half-view-height margin-y))
         (ex (player-x player))
         (ey (player-y player)))
    (declare (type single-float half-view-width half-view-height
                   view-left view-right view-top view-bottom ex ey))
    (and (>= ex view-left)
         (<= ex view-right)
         (>= ey view-top)
         (<= ey view-bottom))))

(defun entity-in-render-distance-p (entity player)
  "Return T if entity is within render distance, or if distance check disabled.
   Uses *entity-render-max-distance* (nil = unlimited, default).
   Note: Uses generic combatant-position - prefer type-specific versions in hot loops."
  (or (null *entity-render-max-distance*)
      (null player)
      (multiple-value-bind (ex ey) (combatant-position entity)
        (let ((dx (- ex (player-x player)))
              (dy (- ey (player-y player))))
          (<= (+ (* dx dx) (* dy dy))
              (* *entity-render-max-distance* *entity-render-max-distance*))))))

(defun npc-in-render-distance-p (npc player)
  "Return T if NPC is within render distance, or if distance check disabled.
   Non-generic version for hot render loops (Task 1.5 CLOS removal)."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type npc npc))
  (or (null *entity-render-max-distance*)
      (null player)
      (let ((dx (- (npc-x npc) (player-x player)))
            (dy (- (npc-y npc) (player-y player))))
        (declare (type single-float dx dy))
        (<= (+ (* dx dx) (* dy dy))
            (* *entity-render-max-distance* *entity-render-max-distance*)))))

(defun draw-entities-with-spatial-culling (game player camera-x camera-y zoom
                                            margin-x margin-y assets render)
  "Draw entities using spatial grid for NPC viewport culling.
   NPCs are drawn first, then players (so players render on top).
   NPCs use spatial-grid-query-rect when grid is available.
   Optional distance filter via *entity-render-max-distance*."
  (let* ((half-view-w (/ (current-screen-width) (* 2.0 zoom)))
         (half-view-h (/ (current-screen-height) (* 2.0 zoom)))
         (view-left (- camera-x half-view-w margin-x))
         (view-right (+ camera-x half-view-w margin-x))
         (view-top (- camera-y half-view-h margin-y))
         (view-bottom (+ camera-y half-view-h margin-y))
         (players (game-players game))
         (zone-id (and player (player-zone-id player)))
         (zone-state (and zone-id (get-zone-state zone-id)))
         (npcs (if zone-state (zone-state-npcs zone-state) (game-npcs game))))
    ;; Draw NPCs first (so players render on top, preserving prior draw order)
    ;; Draw NPCs - use direct draw-npc call and type-specific viewport checks
    ;; to avoid CLOS dispatch (Task 1.5)
    (let ((npc-grid (and zone-state (zone-state-npc-grid zone-state))))
      (if npc-grid
          ;; Use spatial query for NPCs - only iterate cells in viewport
          ;; Still apply npc-in-viewport-p since cell boundaries may include
          ;; NPCs outside the actual viewport
          ;; Phase 2: allocation-free query using scratch vector
          (let ((count (spatial-grid-query-rect-into npc-grid
                                                      view-left view-top
                                                      view-right view-bottom
                                                      *spatial-scratch-vector*)))
            (declare (type fixnum count))
            (loop :for i fixnum :from 0 :below count
                  :for npc-id fixnum = (aref *spatial-scratch-vector* i)
                  :for npc = (find-npc-by-id-fast zone-state npc-id)
                  :when (and npc
                             (npc-in-viewport-p npc camera-x camera-y zoom margin-x margin-y)
                             (npc-in-render-distance-p npc player))
                  :do (draw-npc npc assets render)))
          ;; Fallback: iterate all NPCs with viewport check
          (when npcs
            (loop :for npc :across npcs
                  :when (and (npc-in-viewport-p npc camera-x camera-y zoom margin-x margin-y)
                             (npc-in-render-distance-p npc player))
                  :do (draw-npc npc assets render)))))
    ;; Draw players last (so they render on top of NPCs)
    ;; Direct draw-player call and type-specific viewport check to avoid CLOS dispatch (Task 1.5)
    ;; Note: distance filter not applied to players (always render other players in view)
    (when players
      (loop :for p :across players
            :when (player-in-viewport-p p camera-x camera-y zoom margin-x margin-y)
            :do (draw-player p assets render)))
    ;; Step 9: Draw edge-strip entities from adjacent zones
    (let ((edge-strips (game-edge-strips game)))
      (when edge-strips
        (let* ((world (game-world game))
               (tds (if world (world-tile-dest-size world) 64.0)))
          (draw-edge-strip-entities edge-strips view-left view-right view-top view-bottom
                                    assets render tds))))))

;;;; ========================================================================
;;;; Edge-Strip Entity Rendering (Step 9)
;;;; Renders entities from adjacent zones that are within the edge strip.
;;;; Uses the same draw functions as main-zone entities.
;;;; ========================================================================

;;;; --- Edge-Strip Cull Helpers (per entity type) ---
;;;; These are referenced by *edge-entity-specs* in save.lisp.
;;;; Signature: (entity view-left view-right view-top view-bottom tile-dest-size) -> bool

(defun edge-cull-player (entity vl vr vt vb tile-dest-size)
  "Viewport cull check for edge-strip player (pixel coords)."
  (declare (ignore tile-dest-size))
  (and entity
       (let ((x (player-x entity)) (y (player-y entity)))
         (and (>= x vl) (<= x vr) (>= y vt) (<= y vb)))))

(defun edge-cull-npc (entity vl vr vt vb tile-dest-size)
  "Viewport cull check for edge-strip NPC (pixel coords, must be alive)."
  (declare (ignore tile-dest-size))
  (and entity
       (npc-alive entity)
       (let ((x (npc-x entity)) (y (npc-y entity)))
         (and (>= x vl) (<= x vr) (>= y vt) (<= y vb)))))

(defun edge-cull-object (entity vl vr vt vb tile-dest-size)
  "Viewport cull check for edge-strip zone-object (tile coords)."
  (let* ((tx (zone-object-x entity))
         (ty (zone-object-y entity))
         (count (zone-object-count entity))
         (respawn (zone-object-respawn entity)))
    (and (numberp tx) (numberp ty)
         (> count 0) (<= respawn 0.0)
         (let* ((x (* tx tile-dest-size))
                (y (* ty tile-dest-size))
                (x2 (+ x tile-dest-size))
                (y2 (+ y tile-dest-size)))
           (and (< x vr) (> x2 vl)
                (< y vb) (> y2 vt))))))

;;;; --- Edge-Strip Draw Helpers (per entity type) ---
;;;; Signature: (entity assets render tile-dest-size) -> nil

(defun edge-draw-player-one (entity assets render tile-dest-size)
  "Draw one edge-strip player."
  (declare (ignore tile-dest-size))
  (draw-player entity assets render))

(defun edge-draw-npc-one (entity assets render tile-dest-size)
  "Draw one edge-strip NPC."
  (declare (ignore tile-dest-size))
  (draw-npc entity assets render))

(defun edge-draw-object-one (entity assets render tile-dest-size)
  "Draw one edge-strip zone-object with texture lookup."
  (let ((tile-source (render-tile-source render))
        (tile-dest (render-tile-dest render))
        (origin (render-origin render))
        (tx (zone-object-x entity))
        (ty (zone-object-y entity)))
    (let ((texture (or (object-texture-for assets (zone-object-id entity))
                       (item-texture-for assets (zone-object-id entity)))))
      (when texture
        (let* ((src-w (float (raylib:texture-width texture) 1.0))
               (src-h (float (raylib:texture-height texture) 1.0))
               (dest-size (float tile-dest-size 1.0))
               (dest-x (float (* tx tile-dest-size) 1.0))
               (dest-y (float (* ty tile-dest-size) 1.0)))
          (set-rectangle tile-source 0.0 0.0 src-w src-h)
          (set-rectangle tile-dest dest-x dest-y dest-size dest-size)
          (raylib:draw-texture-pro texture tile-source tile-dest
                                   origin 0.0 :raywhite))))))

;;;; --- Registry-Driven Edge-Strip Renderer ---

(defun draw-edge-strip-entities (edge-strips view-left view-right view-top view-bottom
                                  assets render tile-dest-size)
  "Render entities from edge strips using *edge-entity-specs* registry.
   Iterates all registered entity types so new types are drawn automatically.
   Each strip contains real entity structs with world-space coordinates
   offset-applied by deserialize-edge-strips (Step 8)."
  (let ((total-drawn 0))
    (declare (type fixnum total-drawn))
    (dolist (strip edge-strips)
      (dolist (spec *edge-entity-specs*)
        (let* ((key (getf spec :key))
               (entities (getf strip key))
               (cull-fn (getf spec :cull-one))
               (draw-fn (getf spec :draw-one))
               (coll-type (getf spec :collection-type)))
          (when entities
            (if (eq coll-type :array)
                (when (> (length entities) 0)
                  (loop :for entity :across entities
                        :when (funcall cull-fn entity view-left view-right
                                       view-top view-bottom tile-dest-size)
                        :do (funcall draw-fn entity assets render tile-dest-size)
                            (incf total-drawn)))
                (dolist (entity entities)
                  (when (funcall cull-fn entity view-left view-right
                                 view-top view-bottom tile-dest-size)
                    (funcall draw-fn entity assets render tile-dest-size)
                    (incf total-drawn))))))))
    (when (> total-drawn 0)
      (log-zone "Edge-strip render: ~d entities drawn" total-drawn))))

(defun draw-zone-objects (world render assets camera player editor)
  ;; Draw placed zone objects in world space.
  (let* ((zone (world-zone world))
         (objects (and zone (zone-objects zone))))
    (when objects
      (multiple-value-bind (camera-x camera-y)
          (editor-camera-target editor player camera)
        (let* ((tile-size (world-tile-dest-size world))
               (zoom (camera-zoom camera))
               (half-view-width (/ (current-screen-width) (* 2.0 zoom)))
               (half-view-height (/ (current-screen-height) (* 2.0 zoom)))
               (view-left (- camera-x half-view-width))
               (view-right (+ camera-x half-view-width))
               (view-top (- camera-y half-view-height))
               (view-bottom (+ camera-y half-view-height))
               (tile-source (render-tile-source render))
               (tile-dest (render-tile-dest render))
               (origin (render-origin render)))
          ;; Task 5.5: Use zone-object struct accessors for O(1) field access
          (dolist (object objects)
            (let ((tx (zone-object-x object))
                  (ty (zone-object-y object))
                  (object-id (zone-object-id object))
                  (count (zone-object-count object))
                  (respawn (zone-object-respawn object)))
              ;; DEBUG: Log respawn state occasionally
              (when (and respawn (> respawn 0.0) (< (random 100) 1))
                (log-verbose "RENDER-OBJ: id=~a respawn=~a (should be hidden)" object-id respawn))
              (when (and (numberp tx) (numberp ty)
                         (> count 0)
                         (<= respawn 0.0))
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
  "Render the NPC sprite at its world position.
   Uses direct struct accessors instead of CLOS (Task 1.5)."
  (let ((alive (npc-alive npc)))
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
        (let ((hp (npc-hits-left npc))
              (max-hp (npc-max-hp npc)))
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
    (let ((hp (player-hp player))
          (max-hp (player-max-hp player)))
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
