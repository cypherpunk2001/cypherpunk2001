(in-package :mmorpg)

;;; save-edge-strips.lisp - Edge strip serialization/deserialization (zone boundary data)
;;;
;;; Depends on save-serialize.lisp for: serialize-player-compact, serialize-npc-compact,
;;;   serialize-object, reset-player-vector-pool, encode-zone-id, *player-compact-vector-size*
;;; Depends on save-deserialize.lisp for: deserialize-player-compact, deserialize-npc-compact

;;;; ========================================================================
;;;; Edge-Strip Serialization (Step 7) — Server-side boundary AOI streaming.
;;;; Each strip is a spatially-filtered mini-snapshot of an adjacent zone.
;;;; ========================================================================

(defun opposite-edge (edge)
  "Return the opposite cardinal direction of EDGE."
  (case edge
    (:north :south)
    (:south :north)
    (:east :west)
    (:west :east)
    (t nil)))

(defun entity-in-edge-strip-p (x y edge zone strip-width-px
                               &optional (tile-dest-size (float (* *tile-size* *tile-scale*) 1.0)))
  "Return T if position (X,Y) is within the edge strip of ZONE.
   The strip extends STRIP-WIDTH-PX from the shared boundary into the zone.
   EDGE is the shared edge from the perspective of the adjacent (querying) zone —
   so the strip is at the OPPOSITE side of this zone.
   TILE-DEST-SIZE is the rendered tile size in pixels (default: *tile-size* * *tile-scale*)."
  (let* ((zone-w (* (zone-width zone) tile-dest-size))
         (zone-h (* (zone-height zone) tile-dest-size)))
    (case edge
      ;; Adjacent zone is to the north → strip is at south edge of adj zone
      (:north (>= y (- zone-h strip-width-px)))
      ;; Adjacent zone is to the south → strip is at north edge of adj zone
      (:south (<= y strip-width-px))
      ;; Adjacent zone is to the west → strip is at east edge of adj zone
      (:west (>= x (- zone-w strip-width-px)))
      ;; Adjacent zone is to the east → strip is at west edge of adj zone
      (:east (<= x strip-width-px))
      (t nil))))

;;;; ========================================================================
;;;; Entity-Type Registry for Type-Agnostic Edge-Strip Pipeline
;;;; To add a new entity type to edge strips (and full-zone snapshots),
;;;; add a spec to *edge-entity-specs* and define the required helper functions.
;;;; All pipeline functions (serialize, deserialize, draw) iterate this registry.
;;;; ========================================================================

;;; --- Source helpers: extract entity collection from zone-state ---

(defun edge-source-players (zone-state)
  "Get player array from zone-state for serialization."
  (when zone-state (zone-state-zone-players zone-state)))

(defun edge-source-npcs (zone-state)
  "Get NPC array from zone-state for serialization."
  (when zone-state (zone-state-npcs zone-state)))

(defun edge-source-objects (zone-state)
  "Get zone objects list from zone-state for serialization."
  (let ((zone (when zone-state (zone-state-zone zone-state))))
    (when zone (zone-objects zone))))

;;; --- Validity helpers: should this entity be included in serialization? ---

(defun edge-valid-player-p (entity)
  "Player is valid for serialization if non-nil."
  (not (null entity)))

(defun edge-valid-npc-p (entity)
  "NPC is valid for serialization if non-nil and alive."
  (and entity (npc-alive entity)))

(defun edge-valid-object-p (entity)
  "Objects are always valid for serialization."
  (declare (ignore entity))
  t)

;;; --- Pixel-position helpers: get pixel coords for spatial filtering ---

(defun edge-player-pixel-pos (entity tile-dest-size)
  "Return pixel position of player (already in pixel coords)."
  (declare (ignore tile-dest-size))
  (values (player-x entity) (player-y entity)))

(defun edge-npc-pixel-pos (entity tile-dest-size)
  "Return pixel position of NPC (already in pixel coords)."
  (declare (ignore tile-dest-size))
  (values (npc-x entity) (npc-y entity)))

(defun edge-object-pixel-pos (entity tile-dest-size)
  "Return pixel position of zone-object (convert from tile coords)."
  (values (* (zone-object-x entity) tile-dest-size)
          (* (zone-object-y entity) tile-dest-size)))

;;; --- Serialize helpers: uniform interface for per-entity serialization ---

(defun edge-serialize-player (entity zone-id use-pool)
  "Serialize one player for snapshot/edge-strip."
  (declare (ignore zone-id))
  (serialize-player-compact entity :use-pool use-pool))

(defun edge-serialize-npc (entity zone-id use-pool)
  "Serialize one NPC for snapshot/edge-strip."
  (declare (ignore use-pool))
  (serialize-npc-compact entity zone-id))

(defun edge-serialize-object (entity zone-id use-pool)
  "Serialize one zone-object for snapshot/edge-strip."
  (declare (ignore zone-id use-pool))
  (serialize-object entity))

;;; --- Offset helpers: apply world-space offset for edge-strip rendering ---

(defun edge-offset-player (entity offset-x offset-y tile-dest-size)
  "Apply pixel offset to player position for edge-strip rendering."
  (declare (ignore tile-dest-size))
  (incf (player-x entity) offset-x)
  (incf (player-y entity) offset-y))

(defun edge-offset-npc (entity offset-x offset-y tile-dest-size)
  "Apply pixel offset to NPC position for edge-strip rendering."
  (declare (ignore tile-dest-size))
  (incf (npc-x entity) offset-x)
  (incf (npc-y entity) offset-y))

(defun edge-offset-object (entity offset-x offset-y tile-dest-size)
  "Apply tile offset to zone-object position for edge-strip rendering."
  (incf (zone-object-x entity) (round (/ offset-x tile-dest-size)))
  (incf (zone-object-y entity) (round (/ offset-y tile-dest-size))))

;;; --- The Registry ---

(defparameter *edge-entity-specs*
  '((:key :players
     :source edge-source-players
     :valid-p edge-valid-player-p
     :pixel-pos edge-player-pixel-pos
     :serialize-one edge-serialize-player
     :deser-compact deserialize-player-compact
     :ctor make-edge-strip-player
     :apply-offset edge-offset-player
     :cull-one edge-cull-player
     :draw-one edge-draw-player-one
     :collection-type :array)
    (:key :npcs
     :source edge-source-npcs
     :valid-p edge-valid-npc-p
     :pixel-pos edge-npc-pixel-pos
     :serialize-one edge-serialize-npc
     :deser-compact deserialize-npc-compact
     :ctor make-edge-strip-npc
     :apply-offset edge-offset-npc
     :cull-one edge-cull-npc
     :draw-one edge-draw-npc-one
     :collection-type :array)
    (:key :objects
     :source edge-source-objects
     :valid-p edge-valid-object-p
     :pixel-pos edge-object-pixel-pos
     :serialize-one edge-serialize-object
     :deser-compact identity
     :ctor make-edge-strip-object
     :apply-offset edge-offset-object
     :cull-one edge-cull-object
     :draw-one edge-draw-object-one
     :collection-type :list))
  "Registry of entity types for the edge-strip pipeline.
   To add a new entity type: add a spec here and define the required functions.
   Pipeline functions (serialize, deserialize, draw) all iterate this registry.")

;;;; ========================================================================
;;;; Unified Serialization Path (serialize-zone-state-filtered)
;;;; ========================================================================

(defun serialize-edge-strip (adj-zone-state edge strip-width-px adj-zone-id
                             tile-dest-size &key use-pool)
  "Serialize entities from ADJ-ZONE-STATE within the edge strip.
   Delegates to serialize-zone-state-filtered (registry-driven)."
  (serialize-zone-state-filtered adj-zone-state adj-zone-id tile-dest-size
                                 :edge edge :strip-width-px strip-width-px
                                 :use-pool use-pool))

(defun serialize-zone-state-filtered (zone-state zone-id tile-dest-size
                                      &key edge strip-width-px use-pool
                                           entity-overrides)
  "Serialize all registered entity types from ZONE-STATE into snapshot format.
   Iterates *edge-entity-specs* so new entity types are included automatically.
   When EDGE and STRIP-WIDTH-PX are provided, only entities within the
   edge strip are included (spatial filter). Otherwise all entities are serialized.
   ENTITY-OVERRIDES is an optional plist of (:key collection) to override sources
   (used by serialize-game-state-for-zone for player fallback)."
  (let ((filter-p (and edge strip-width-px))
        (zone (when zone-state (zone-state-zone zone-state))))
    (loop :for spec :in *edge-entity-specs*
          :nconc
          (let* ((key (getf spec :key))
                 (source-fn (getf spec :source))
                 (valid-fn (getf spec :valid-p))
                 (pos-fn (getf spec :pixel-pos))
                 (ser-fn (getf spec :serialize-one))
                 (coll-type (getf spec :collection-type))
                 ;; Use override if provided, otherwise source from zone-state
                 (entities (or (getf entity-overrides key)
                               (funcall source-fn zone-state)))
                 (serialized nil))
            ;; Iterate entities and serialize (with optional spatial filter)
            (when entities
              (flet ((process-entity (entity)
                       (when (and (funcall valid-fn entity)
                                  (or (not filter-p)
                                      (multiple-value-bind (px py)
                                          (funcall pos-fn entity tile-dest-size)
                                        (entity-in-edge-strip-p
                                         px py edge zone strip-width-px tile-dest-size))))
                         (push (funcall ser-fn entity zone-id use-pool) serialized))))
                (if (eq coll-type :array)
                    (loop :for entity :across entities :do (process-entity entity))
                    (dolist (entity entities) (process-entity entity)))))
            ;; Return (key value) pair for nconc
            (list key (if (eq coll-type :array)
                          (coerce (nreverse serialized) 'vector)
                          (nreverse serialized)))))))

(defun serialize-edge-strips-for-zone (game zone-id)
  "Build edge strips for ZONE-ID by checking all adjacent zones in the world graph.
   Returns a list of (:edge <kw> :zone-id <kw> :strip <plist>) or nil if none."
  (let* ((world (game-world game))
         (graph (and world (world-world-graph world)))
         (strips nil))
    (when graph
      (let* ((tile-dest-size (float (world-tile-dest-size world)))
             (strip-width-px (* *zone-edge-visibility-tiles* tile-dest-size)))
        (dolist (exit-spec (world-graph-exits graph zone-id))
          ;; Only spatial exits (preserve-x/y) get edge strips; skip teleports
          (when (spatial-exit-p exit-spec)
          (let* ((edge (getf exit-spec :edge))
                 (adj-zone-id (getf exit-spec :to))
                 (adj-zone-state (and adj-zone-id (get-zone-state adj-zone-id))))
            (when adj-zone-state
              (let ((strip (serialize-edge-strip adj-zone-state edge strip-width-px adj-zone-id
                                                 tile-dest-size)))
                (let ((np (length (getf strip :players)))
                      (nn (length (getf strip :npcs)))
                      (no (length (getf strip :objects))))
                  (when (or (> np 0) (> nn 0) (> no 0))
                    (log-zone "Edge strip ~a->~a: ~d players, ~d npcs, ~d objects"
                                 edge adj-zone-id np nn no)))
                (push (list :edge edge :zone-id adj-zone-id :strip strip) strips))))))))
    (nreverse strips)))

(defun serialize-game-state-for-zone (game zone-id zone-state &key use-pool)
  "Serialize game state filtered to a specific zone.
   Delegates to serialize-zone-state-filtered (registry-driven) so all entity
   types are handled by the same code path as edge strips.
   Uses cached zone-players array when available (Task 4.1) for O(zone-players)
   instead of O(total-players). Falls back to filtering all players when cache
   is not populated."
  (with-timing (:serialize-zone)
    ;; Reset vector pool for this serialization pass (Task 4.2)
    (when use-pool
      (reset-player-vector-pool))
    ;; Build entity-overrides for fallback when zone-state is incomplete
    (let* ((zone-players (when zone-state (zone-state-zone-players zone-state)))
           (overrides nil))
      ;; Player fallback: when zone-players cache is not populated, filter game-players
      (when (or (null zone-players) (zerop (length zone-players)))
        (let ((fallback (make-array 8 :adjustable t :fill-pointer 0)))
          (when (game-players game)
            (loop :for player :across (game-players game)
                  :for pz = (or (player-zone-id player) *starting-zone-id*)
                  :when (and player (eq pz zone-id))
                  :do (vector-push-extend player fallback)))
          (setf (getf overrides :players) fallback)))
      ;; NPC/object fallback when zone-state is nil
      (unless zone-state
        (setf (getf overrides :npcs) (game-npcs game))
        (let ((zone (world-zone (game-world game))))
          (when zone
            (setf (getf overrides :objects) (zone-objects zone)))))
      ;; Delegate to unified registry-driven serialization
      (let* ((tile-dest-size (float (world-tile-dest-size (game-world game)) 1.0))
             (payload (serialize-zone-state-filtered zone-state zone-id tile-dest-size
                                                     :use-pool use-pool
                                                     :entity-overrides overrides))
             (snapshot (list* :format :compact-v5 :zone-id zone-id payload)))
        ;; Step 7: Append edge strips for all loaded adjacent zones
        (let ((edge-strips (serialize-edge-strips-for-zone game zone-id)))
          (when edge-strips
            (setf (getf snapshot :edge-strips) edge-strips)))
        snapshot))))

;;;; ========================================================================
;;;; Edge-Strip Entity Constructors (Step 8)
;;;; Create minimal player/npc structs from deserialized plists for rendering.
;;;; Only visual fields are set — these entities are render-only.
;;;; ========================================================================

(defun make-edge-strip-player (plist)
  "Create a minimal player struct from compact PLIST for edge-strip rendering."
  (let ((p (%make-player)))
    (setf (player-id p) (getf plist :id 0)
          (player-x p) (float (getf plist :x 0.0) 1.0)
          (player-y p) (float (getf plist :y 0.0) 1.0)
          (player-dx p) (float (getf plist :dx 0.0) 1.0)
          (player-dy p) (float (getf plist :dy 0.0) 1.0)
          (player-hp p) (getf plist :hp 1)
          (player-anim-state p) (getf plist :anim-state :idle)
          (player-facing p) (getf plist :facing :down)
          (player-facing-sign p) (float (getf plist :facing-sign 1.0) 1.0)
          (player-frame-index p) (getf plist :frame-index 0)
          (player-frame-timer p) (float (getf plist :frame-timer 0.0) 1.0)
          (player-attacking p) (getf plist :attacking nil)
          (player-attack-hit p) (getf plist :attack-hit nil)
          (player-hit-active p) (getf plist :hit-active nil)
          (player-attack-timer p) (float (getf plist :attack-timer 0.0) 1.0)
          (player-hit-timer p) (float (getf plist :hit-timer 0.0) 1.0)
          (player-hit-frame p) (getf plist :hit-frame 0)
          (player-hit-facing p) (getf plist :hit-facing :down)
          (player-hit-facing-sign p) (float (getf plist :hit-facing-sign 1.0) 1.0)
          (player-running p) (getf plist :running nil))
    p))

(defun make-edge-strip-npc (plist)
  "Create a minimal NPC struct from compact PLIST for edge-strip rendering."
  (let ((n (%make-npc))
        (hits (getf plist :hits-left 1)))
    (setf (npc-id n) (getf plist :id 0)
          (npc-x n) (float (getf plist :x 0.0) 1.0)
          (npc-y n) (float (getf plist :y 0.0) 1.0)
          (npc-anim-state n) (getf plist :anim-state :idle)
          (npc-facing n) (getf plist :facing :down)
          (npc-frame-index n) (getf plist :frame-index 0)
          (npc-frame-timer n) (float (getf plist :frame-timer 0.0) 1.0)
          (npc-hits-left n) hits
          (npc-alive n) (getf plist :alive t)
          (npc-hit-active n) (getf plist :hit-active nil)
          (npc-hit-timer n) (float (getf plist :hit-timer 0.0) 1.0)
          (npc-hit-frame n) (getf plist :hit-frame 0)
          (npc-hit-facing n) (getf plist :hit-facing :down)
          (npc-hit-facing-sign n) (float (getf plist :hit-facing-sign 1.0) 1.0))
    ;; Set stats for health bar rendering (npc-max-hp requires stats)
    ;; Use hits-left as the hitpoints level for a reasonable health bar
    (setf (npc-stats n) (make-npc-stats nil))
    ;; Try to set archetype from plist if available
    (let ((archetype-name (getf plist :archetype)))
      (when archetype-name
        (let ((archetype (find-npc-archetype archetype-name)))
          (when archetype
            (setf (npc-archetype n) archetype
                  (npc-stats n) (make-npc-stats archetype))))))
    n))

(defun make-edge-strip-object (plist)
  "Create a zone-object struct from serialized PLIST for edge-strip rendering."
  (%make-zone-object :id (getf plist :id)
                     :x (getf plist :x 0)
                     :y (getf plist :y 0)
                     :count (getf plist :count 1)
                     :respawn (float (getf plist :respawn 0.0) 1.0)
                     :respawnable (getf plist :respawnable t)))

(defun compute-edge-strip-offset (edge world)
  "Compute world-space offset for rendering entities from an adjacent zone at EDGE.
   Uses the current zone's dimensions to determine where the adjacent zone starts."
  (let* ((zone (and world (world-zone world)))
         (tile-size (world-tile-dest-size world))
         (span-x (if zone (* (zone-width zone) tile-size) 0.0))
         (span-y (if zone (* (zone-height zone) tile-size) 0.0)))
    (case edge
      (:north (values 0.0 (- span-y)))
      (:south (values 0.0 span-y))
      (:east  (values span-x 0.0))
      (:west  (values (- span-x) 0.0))
      (t (values 0.0 0.0)))))

(defun deserialize-edge-strips (state game)
  "Process :edge-strips from snapshot STATE and store in game-edge-strips.
   Registry-driven: iterates *edge-entity-specs* so new entity types are
   deserialized automatically. Step 8: Replaces (not accumulates) each frame."
  (let ((raw-strips (getf state :edge-strips))
        (world (game-world game))
        (result nil))
    (when raw-strips
      (let ((tile-dest-size (if world (float (world-tile-dest-size world) 1.0) 64.0)))
        (dolist (strip-entry raw-strips)
          (let* ((edge (getf strip-entry :edge))
                 (strip-zone-id (getf strip-entry :zone-id))
                 (strip-data (getf strip-entry :strip)))
            ;; Compute world-space offset for this edge
            (multiple-value-bind (offset-x offset-y)
                (compute-edge-strip-offset edge world)
              ;; Build deserialized strip by iterating entity-type registry
              (let ((deserialized (list :edge edge :zone-id strip-zone-id
                                       :offset-x offset-x :offset-y offset-y)))
                (dolist (spec *edge-entity-specs*)
                  (let* ((key (getf spec :key))
                         (raw (getf strip-data key))
                         (deser-fn (getf spec :deser-compact))
                         (ctor-fn (getf spec :ctor))
                         (offset-fn (getf spec :apply-offset))
                         (coll-type (getf spec :collection-type)))
                    (let ((entities
                            (cond
                              ;; Array collection: compact vectors
                              ((and (eq coll-type :array) raw
                                    (> (length raw) 0))
                               (coerce
                                (loop :for vec :across raw
                                      :for plist = (funcall deser-fn vec)
                                      :when plist
                                      :collect (let ((e (funcall ctor-fn plist)))
                                                 (funcall offset-fn e offset-x offset-y
                                                          tile-dest-size)
                                                 e))
                                'vector))
                              ;; List collection: plists directly
                              ((and (eq coll-type :list) raw)
                               (loop :for item :in raw
                                     :for plist = (funcall deser-fn item)
                                     :when plist
                                     :collect (let ((e (funcall ctor-fn plist)))
                                                (funcall offset-fn e offset-x offset-y
                                                         tile-dest-size)
                                                e)))
                              (t nil))))
                      ;; Store under the entity key (use empty vector for array types)
                      (setf (getf deserialized key)
                            (or entities
                                (if (eq coll-type :array) #() nil))))))
                (push deserialized result)))))))
    ;; Replace edge strips wholesale
    (setf (game-edge-strips game) (nreverse result))))
