;; NOTE: If you change behavior here, update docs/movement.md :)
(in-package #:mmorpg)

;;;; ========================================================================
;;;; Zone State Management
;;;; Tracks loaded zones with their NPCs and collision data.
;;;; Enables multiple zones to be active simultaneously.
;;;; ========================================================================

(defparameter *zone-states* (make-hash-table :test 'eq :size 64)
  "Cache of zone-id -> zone-state for all loaded zones.
   Zones stay loaded once any player enters them.")

;;;; ========================================================================
;;;; Zone Tracking Cache (Task 4.1)
;;;; Cached occupied zone-ids to avoid per-tick list allocation.
;;;; Call refresh-occupied-zones-cache once per tick, then iterate
;;;; *occupied-zones-cache* instead of calling occupied-zone-ids.
;;;; ========================================================================

(defparameter *occupied-zones-cache*
  (make-array 64 :element-type t :fill-pointer 0 :adjustable nil)
  "Reusable vector of zone-ids that have at least one player.
   Populated by refresh-occupied-zones-cache, avoids per-tick allocation.")

(defparameter *occupied-zones-seen*
  (make-hash-table :test 'eq :size 64)
  "Dedup helper for refresh-occupied-zones-cache. Reused each tick.")

(defun get-zone-state (zone-id)
  "Get the zone-state for ZONE-ID, or NIL if not loaded."
  (gethash zone-id *zone-states*))

(defun get-or-create-zone-state (zone-id zone-path &key npcs)
  "Get existing zone-state or create a new one by loading the zone.
   NPCS is an optional NPC vector to associate with the zone.
   Initializes spatial grids for proximity queries."
  (or (gethash zone-id *zone-states*)
      (let ((zone (when zone-path (load-zone zone-path))))
        (when zone
          ;; Phase 2 perf: Use array-backed spatial grids with zone dimensions
          (let* ((tile-dest-size (* (float *tile-size* 1.0) *tile-scale*))
                 (zone-w (or (zone-width zone) 64))
                 (zone-h (or (zone-height zone) 64))
                 (state (make-zone-state
                         :zone-id zone-id
                         :zone zone
                         :npcs (or npcs (vector))
                         :wall-map (derive-wall-map-from-zone zone)
                         :objects (zone-objects zone)
                         ;; Initialize array-backed spatial grids for O(1) queries
                         :player-grid (make-spatial-grid-for-zone zone-w zone-h tile-dest-size)
                         :npc-grid (make-spatial-grid-for-zone zone-w zone-h tile-dest-size))))
            (setf (gethash zone-id *zone-states*) state)
            (log-verbose "Created zone-state for ~a with spatial grids" zone-id)
            state)))))

(defun zone-state-player-count (zone-id players)
  "Count how many players are in ZONE-ID.
   Uses tight loop instead of count-if to avoid lambda allocation (Task 4.1)."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (let ((count 0))
    (declare (type fixnum count))
    (loop :for p :across players
          :when (and p (eq (player-zone-id p) zone-id))
            :do (incf count))
    count))

(defun players-in-zone (zone-id players)
  "Return vector of players in ZONE-ID. Returns nil if zone-id is nil."
  (when zone-id
    (let ((result nil))
      (loop :for p :across players
            :when (eq (player-zone-id p) zone-id)
              :do (push p result))
      (coerce (nreverse result) 'vector))))

(defun occupied-zone-ids (players)
  "Return list of zone-ids that have at least one player.
   DEPRECATED: Use refresh-occupied-zones-cache + *occupied-zones-cache* instead
   to avoid per-tick list allocation (Task 4.1)."
  (let ((ids nil))
    (loop for p across players
          for zid = (player-zone-id p)
          when (and zid (not (member zid ids)))
            do (push zid ids))
    ids))

(defun refresh-occupied-zones-cache (players)
  "Rebuild *occupied-zones-cache* from actual player presence.
   Call once per tick. Only includes zones with players.
   Avoids per-tick list allocation by reusing the cache vector (Task 4.1)."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (setf (fill-pointer *occupied-zones-cache*) 0)
  (clrhash *occupied-zones-seen*)
  ;; Iterate players to find actually-occupied zones
  (loop :for player :across players
        :when player
          :do (let ((zone-id (player-zone-id player)))
                (when (and zone-id (not (gethash zone-id *occupied-zones-seen*)))
                  (setf (gethash zone-id *occupied-zones-seen*) t)
                  (when (< (fill-pointer *occupied-zones-cache*)
                           (array-dimension *occupied-zones-cache* 0))
                    (vector-push zone-id *occupied-zones-cache*)))))
  *occupied-zones-cache*)

(defun release-zone-npcs (zone-state)
  "Release all NPCs in ZONE-STATE back to the pool (Task 4.4).
   Only effective when *use-npc-pool* is enabled."
  (when (and *use-npc-pool* zone-state)
    (let ((npcs (zone-state-npcs zone-state)))
      (when npcs
        (loop :for npc :across npcs
              :when npc
              :do (release-npc npc))))))

(defun clear-zone-states ()
  "Clear all cached zone states. Used for testing or server restart.
   Task 4.4: Releases pooled NPCs before clearing."
  ;; Release NPCs back to pool before clearing
  (when *use-npc-pool*
    (maphash (lambda (_zone-id zone-state)
               (declare (ignore _zone-id))
               (release-zone-npcs zone-state))
             *zone-states*))
  (clrhash *zone-states*))

(defun get-zone-wall-map (zone-id)
  "Get the wall-map for ZONE-ID from zone-state cache.
   Returns NIL if zone not loaded."
  (let ((state (get-zone-state zone-id)))
    (when state
      (zone-state-wall-map state))))

(defun get-zone-collision-bounds (zone-id tile-dest-size collision-half-w collision-half-h)
  "Calculate movement bounds for ZONE-ID using its wall-map dimensions.
   Uses zero-origin bounds since per-zone wall-maps are zero-indexed.
   Returns (values min-x max-x min-y max-y) or NIL if zone not loaded."
  (let ((wall-map (get-zone-wall-map zone-id)))
    (when wall-map
      (let* ((width (array-dimension wall-map 1))
             (height (array-dimension wall-map 0)))
        (zone-bounds-zero-origin tile-dest-size width height
                                 collision-half-w collision-half-h)))))

(defun world-search-radius (world)
  ;; Return a max search radius in tiles for open spawn placement.
  (max (world-wall-map-width world) (world-wall-map-height world)))

(defun tile-center-position (tile-size tx ty)
  ;; Return the world position for the center of tile TX/TY.
  (values (+ (* (+ tx 0.5) tile-size))
          (+ (* (+ ty 0.5) tile-size))))

(defun edge-zone-offset (edge span-x span-y)
  ;; Return world offset to the neighboring zone along EDGE.
  (ecase edge
    (:north (values 0.0 (- span-y)))
    (:south (values 0.0 span-y))
    (:east (values span-x 0.0))
    (:west (values (- span-x) 0.0))))

(defun zone-bounds-from-dimensions (tile-dest-size width height collision-half-width collision-half-height)
  ;; Return wall bounds for a zone with WIDTH/HEIGHT in tiles.
  ;; Bounds include the boundary ring (tile 0 and tile width-1) so all visible tiles
  ;; are walkable.
  (let* ((wall-min-x (+ (* *wall-origin-x* tile-dest-size)
                        collision-half-width))
         (wall-max-x (- (* (+ *wall-origin-x* width)
                           tile-dest-size)
                        collision-half-width))
         (wall-min-y (+ (* *wall-origin-y* tile-dest-size)
                        collision-half-height))
         (wall-max-y (- (* (+ *wall-origin-y* height)
                           tile-dest-size)
                        collision-half-height)))
    (values wall-min-x wall-max-x wall-min-y wall-max-y)))

(defun build-adjacent-minimap-spawns (world &optional player)
  ;; Build cached spawn positions for adjacent zones on the minimap.
  (let* ((graph (world-world-graph world))
         (zone (world-zone world))
         (zone-id (and zone (zone-id zone)))
         (tile-size (world-tile-dest-size world))
         (min-x (world-wall-min-x world))
         (max-x (world-wall-max-x world))
         (min-y (world-wall-min-y world))
         (max-y (world-wall-max-y world))
         (span-x (max 1.0 (- max-x min-x)))
         (span-y (max 1.0 (- max-y min-y)))
         (spawns nil))
    (when (and graph zone-id)
      (dolist (exit (world-graph-exits graph zone-id))
        (let* ((edge (getf exit :edge))
               (target-id (getf exit :to))
               (target-path (and edge target-id
                                 (world-graph-zone-path graph target-id))))
          (when (and edge target-path (probe-file target-path))
            (multiple-value-bind (offset-x offset-y)
                (edge-zone-offset edge span-x span-y)
              (let* ((data (read-zone-data target-path))
                     (plist (zone-data-plist data))
                     (zone-spawns (getf plist :spawns nil)))
                (when (and zone-spawns (not (null zone-spawns)))
                  (dolist (spawn zone-spawns)
                    (let* ((tx (getf spawn :x))
                           (ty (getf spawn :y))
                           (count (max 1 (getf spawn :count 1))))
                      (when (and (numberp tx) (numberp ty))
                        (multiple-value-bind (cx cy)
                            (tile-center-position tile-size tx ty)
                          (loop :repeat count
                                :do (push (list edge
                                                (+ cx offset-x)
                                                (+ cy offset-y))
                                          spawns)))))))))))))
    (nreverse spawns)))

(defun build-minimap-collisions (world)
  ;; Build cached collision marker positions for the minimap.
  (let* ((wall-map (world-wall-map world)))
    (if (not wall-map)
        (make-array 0)
        (let* ((height (array-dimension wall-map 0))
               (width (array-dimension wall-map 1))
               (zone (world-zone world))
               (zone-id (and zone (zone-id zone)))
               (graph (world-world-graph world))
               (exits (and graph zone-id (world-graph-exits graph zone-id)))
               (tile-size (world-tile-dest-size world))
               (origin-x *wall-origin-x*)
               (origin-y *wall-origin-y*)
               (count 0)
               (perimeter-count 0))
          (when (and (> width 0) (> height 0))
            (labels ((edge-exit-p (edge)
                       (and exits
                            (find edge exits
                                  :key (lambda (exit) (getf exit :edge))
                                  :test #'eq)))
                     (count-perimeter (tx ty)
                       (when (zerop (aref wall-map ty tx))
                         (incf perimeter-count))))
              (let* ((show-north (not (edge-exit-p :north)))
                     (show-south (not (edge-exit-p :south)))
                     (show-west (not (edge-exit-p :west)))
                     (show-east (not (edge-exit-p :east)))
                     (show-top (if (= height 1)
                                   (or show-north show-south)
                                   show-north))
                     (show-bottom (and (> height 1) show-south))
                     (show-left (if (= width 1)
                                    (or show-west show-east)
                                    show-west))
                     (show-right (and (> width 1) show-east)))
                (when show-top
                  (loop :for tx :from 0 :below width
                        :do (count-perimeter tx 0)))
                (when show-bottom
                  (loop :for tx :from 0 :below width
                        :do (count-perimeter tx (1- height))))
                (when show-left
                  (let* ((start-ty (if show-top 1 0))
                         (end-ty (if show-bottom (- height 2) (1- height))))
                    (when (<= start-ty end-ty)
                      (loop :for ty :from start-ty :to end-ty
                            :do (count-perimeter 0 ty)))))
                (when show-right
                  (let* ((start-ty (if show-top 1 0))
                         (end-ty (if show-bottom (- height 2) (1- height)))
                         (tx (1- width)))
                    (when (<= start-ty end-ty)
                      (loop :for ty :from start-ty :to end-ty
                            :do (count-perimeter tx ty))))))))
          (loop :for ty :from 0 :below height
                :do (loop :for tx :from 0 :below width
                          :when (not (zerop (aref wall-map ty tx)))
                            :do (incf count)))
          (let ((points (make-array (* 2 (+ count perimeter-count))))
                (index 0))
            (when (and (> width 0) (> height 0))
              (labels ((edge-exit-p (edge)
                         (and exits
                              (find edge exits
                                    :key (lambda (exit) (getf exit :edge))
                                    :test #'eq)))
                       (store-perimeter (tx ty)
                         (when (zerop (aref wall-map ty tx))
                           (multiple-value-bind (cx cy)
                               (tile-center-position tile-size
                                                     (+ tx origin-x)
                                                     (+ ty origin-y))
                             (setf (aref points index) cx
                                   (aref points (1+ index)) cy)
                             (incf index 2)))))
                (let* ((show-north (not (edge-exit-p :north)))
                       (show-south (not (edge-exit-p :south)))
                       (show-west (not (edge-exit-p :west)))
                       (show-east (not (edge-exit-p :east)))
                       (show-top (if (= height 1)
                                     (or show-north show-south)
                                     show-north))
                       (show-bottom (and (> height 1) show-south))
                       (show-left (if (= width 1)
                                      (or show-west show-east)
                                      show-west))
                       (show-right (and (> width 1) show-east)))
                  (when show-top
                    (loop :for tx :from 0 :below width
                          :do (store-perimeter tx 0)))
                  (when show-bottom
                    (loop :for tx :from 0 :below width
                          :do (store-perimeter tx (1- height))))
                  (when show-left
                    (let* ((start-ty (if show-top 1 0))
                           (end-ty (if show-bottom (- height 2) (1- height))))
                      (when (<= start-ty end-ty)
                        (loop :for ty :from start-ty :to end-ty
                              :do (store-perimeter 0 ty)))))
                  (when show-right
                    (let* ((start-ty (if show-top 1 0))
                           (end-ty (if show-bottom (- height 2) (1- height)))
                           (tx (1- width)))
                      (when (<= start-ty end-ty)
                        (loop :for ty :from start-ty :to end-ty
                              :do (store-perimeter tx ty))))))))
            (loop :for ty :from 0 :below height
                  :do (loop :for tx :from 0 :below width
                            :when (not (zerop (aref wall-map ty tx)))
                              :do (multiple-value-bind (cx cy)
                                      (tile-center-position tile-size
                                                            (+ tx origin-x)
                                                            (+ ty origin-y))
                                    (setf (aref points index) cx
                                          (aref points (1+ index)) cy)
                                    (incf index 2))))
            points)))))

(defun npc-collision-half (world)
  ;; Return NPC collider half sizes in world pixels.
  (let ((half (* (/ (world-tile-dest-size world) 2.0) *npc-collision-scale*)))
    (values half half)))

(defun edge-opposite (edge)
  ;; Return the opposite world edge.
  (ecase edge
    (:north :south)
    (:south :north)
    (:east :west)
    (:west :east)))

(defun edge-preserve-axis (edge offset)
  ;; Return which axis to preserve when transitioning.
  (cond
    ((eq offset :preserve-x) :x)
    ((eq offset :preserve-y) :y)
    ((eq offset :none) nil)
    ((member edge '(:north :south)) :x)
    (t :y)))

(defun edge-offset-ratio (min-value max-value value)
  ;; Convert VALUE into a 0..1 ratio inside MIN/MAX.
  (let ((range (- max-value min-value)))
    (if (<= range 0.0)
        0.5
        (clamp (/ (- value min-value) range) 0.0 1.0))))

(defun edge-preserve-position (min-value max-value ratio)
  ;; Return a position inside MIN/MAX based on RATIO.
  (+ min-value (* (clamp ratio 0.0 1.0) (- max-value min-value))))

(defun spatial-exit-p (exit-spec)
  "Return T if EXIT-SPEC is a spatial edge (not a teleport).
   Spatial exits have :offset :preserve-x or :preserve-y."
  (let ((offset (getf exit-spec :offset)))
    (or (eq offset :preserve-x)
        (eq offset :preserve-y))))

(defun world-edge-exit-for-zone (world zone-id edge)
  ;; Return the exit spec for EDGE in ZONE-ID.
  (let ((graph (world-world-graph world)))
    (when (and edge zone-id graph)
      (find edge (world-graph-exits graph zone-id)
            :key (lambda (exit) (getf exit :edge))
            :test #'eq))))

(defun world-edge-exit (world edge)
  ;; Return the exit spec for EDGE in the current zone.
  (let* ((zone (world-zone world))
         (zone-id (and zone (zone-id zone)))
         (graph (world-world-graph world)))
    (when (and edge zone-id graph)
      (find edge (world-graph-exits graph zone-id)
            :key (lambda (exit) (getf exit :edge))
            :test #'eq))))

;;;; ========================================================================
;;;; Diagonal Zone Click Path (Bug 4)
;;;; Multi-hop zone-click-path for diagonal cross-zone click-to-move.
;;;; ========================================================================

(defun get-zone-bounds-from-any-cache (zone-id world tile-size half-w half-h
                                       &optional zone-lru-cache)
  "Get collision bounds for ZONE-ID from the best available source.
   Priority order:
     1. Zone-state wall-map (exact, current zone on client / all on server)
     2. Zone-bounds index on world-graph (always available after startup)
     3. Preview cache zone struct (loaded when camera nears edge/corner)
     4. LRU cache zone struct (preloaded or recently visited)
   Returns (values min-x max-x min-y max-y) or NIL if no source has dimensions."
  ;; 1. Prefer zone-state wall-map (exact collision bounds)
  (let ((wall-map (get-zone-wall-map zone-id)))
    (if wall-map
        (let ((width (array-dimension wall-map 1))
              (height (array-dimension wall-map 0)))
          (zone-bounds-zero-origin tile-size width height half-w half-h))
        ;; 2. Zone-bounds index (pre-computed at startup, always available)
        (let ((graph (and world (world-world-graph world))))
          (multiple-value-bind (idx-min-x idx-max-x idx-min-y idx-max-y)
              (and graph (world-graph-zone-bounds graph zone-id))
            (if idx-min-x
                (values idx-min-x idx-max-x idx-min-y idx-max-y)
                ;; 3/4. Fallback to preview cache or LRU cache
                (let ((zone (or (let ((pc (and world (world-zone-preview-cache world))))
                                  (and pc (gethash zone-id pc)))
                                (and zone-lru-cache
                                     (zone-cache-lookup zone-lru-cache zone-id)))))
                  (when (and zone (zone-width zone) (zone-height zone))
                    (zone-bounds-zero-origin tile-size
                                             (zone-width zone) (zone-height zone)
                                             half-w half-h)))))))))

(defun clear-zone-click-path (game)
  "Clear the multi-hop zone click path state."
  (setf (game-zone-click-path game) nil
        (game-zone-click-edges game) nil
        (game-zone-click-hop-targets game) nil
        (game-zone-click-final-x game) 0.0
        (game-zone-click-final-y game) 0.0
        (game-zone-click-retry-p game) nil))

(defun compute-diagonal-click-path (world player raw-x raw-y)
  "Check if a click at RAW-X/RAW-Y requires diagonal (2-hop) zone traversal.
   Returns (values first-edge second-edge intermediate-zone-id final-zone-id)
   or NIL if the click is not diagonal or no route exists.
   Only considers spatial exits (not teleports)."
  (declare (ignore player))
  (let* ((zone (world-zone world))
         (zone-id (and zone (zone-id zone)))
         (graph (world-world-graph world))
         (tile-size (world-tile-dest-size world))
         (half-w (world-collision-half-width world))
         (half-h (world-collision-half-height world)))
    (when (and zone-id graph tile-size)
      (multiple-value-bind (min-x max-x min-y max-y)
          (if (get-zone-wall-map zone-id)
              (get-zone-collision-bounds zone-id tile-size half-w half-h)
              (values (world-wall-min-x world) (world-wall-max-x world)
                      (world-wall-min-y world) (world-wall-max-y world)))
        (when (and min-x max-x min-y max-y)
          ;; Determine which edges the click exceeds
          (let ((x-edge (cond ((< raw-x min-x) :west)
                              ((> raw-x max-x) :east)
                              (t nil)))
                (y-edge (cond ((< raw-y min-y) :north)
                              ((> raw-y max-y) :south)
                              (t nil))))
            ;; Must exceed on BOTH axes for diagonal
            (when (and x-edge y-edge)
              ;; Try both orderings: x-first and y-first
              (labels ((try-route (edge-1 edge-2)
                         (let ((exit-1 (world-edge-exit-for-zone world zone-id edge-1)))
                           (when (and exit-1 (spatial-exit-p exit-1))
                             (let* ((inter-id (getf exit-1 :to))
                                    (exit-2 (world-edge-exit-for-zone world inter-id edge-2)))
                               (when (and exit-2 (spatial-exit-p exit-2))
                                 (values edge-1 edge-2 inter-id (getf exit-2 :to))))))))
                ;; Prefer x-first, fall back to y-first
                (multiple-value-bind (e1 e2 inter final)
                    (try-route x-edge y-edge)
                  (if inter
                      (values e1 e2 inter final)
                      (try-route y-edge x-edge)))))))))))

(defun translate-click-to-final-zone (world raw-x raw-y src-zone-id inter-zone-id
                                      final-zone-id edge-1 edge-2
                                      &optional zone-lru-cache)
  "Translate click coordinates RAW-X/RAW-Y from SRC-ZONE through INTER-ZONE
   to FINAL-ZONE via two seam translations (EDGE-1 then EDGE-2).
   Falls back to preview cache / LRU cache for zone dimensions when zone-state
   is unavailable (typical on the client for adjacent zones).
   Returns (values final-x final-y) clamped to final zone bounds, or NIL."
  (let* ((tile-size (world-tile-dest-size world))
         (half-w (world-collision-half-width world))
         (half-h (world-collision-half-height world)))
    (when (and tile-size half-w half-h)
      (multiple-value-bind (src-min-x src-max-x src-min-y src-max-y)
          (get-zone-bounds-from-any-cache src-zone-id world tile-size half-w half-h
                                          zone-lru-cache)
        (multiple-value-bind (int-min-x int-max-x int-min-y int-max-y)
            (get-zone-bounds-from-any-cache inter-zone-id world tile-size half-w half-h
                                            zone-lru-cache)
          (multiple-value-bind (fin-min-x fin-max-x fin-min-y fin-max-y)
              (get-zone-bounds-from-any-cache final-zone-id world tile-size half-w half-h
                                              zone-lru-cache)
            ;; All three zones must have bounds
            (when (and src-min-x src-max-x src-min-y src-max-y
                       int-min-x int-max-x int-min-y int-max-y
                       fin-min-x fin-max-x fin-min-y fin-max-y)
              ;; First seam: src -> intermediate
              (multiple-value-bind (mid-x mid-y)
                  (seam-translate-position edge-1 raw-x raw-y
                                           src-min-x src-max-x src-min-y src-max-y
                                           int-min-x int-max-x int-min-y int-max-y)
                ;; Second seam: intermediate -> final
                (multiple-value-bind (final-x final-y)
                    (seam-translate-position edge-2 mid-x mid-y
                                             int-min-x int-max-x int-min-y int-max-y
                                             fin-min-x fin-max-x fin-min-y fin-max-y)
                  (values (clamp final-x fin-min-x fin-max-x)
                          (clamp final-y fin-min-y fin-max-y)))))))))))

;;;; ========================================================================
;;;; Minimap Multi-Hop Path Resolution (Bug 4 Part 3)
;;;; BFS-based multi-hop zone pathing for minimap clicks.
;;;; ========================================================================

(defun resolve-click-destination-zone (world raw-x raw-y)
  "Walk raw click coordinates through zone graph to find the destination zone.
   Starting from the current zone, iteratively translate through whichever edge
   the coords exceed until they land inside a zone (or max hops reached).
   Returns (values dest-zone-id walked-zone-list walked-edge-list) or NIL."
  (let* ((zone (world-zone world))
         (zone-id (and zone (zone-id zone)))
         (graph (world-world-graph world))
         (tile-size (world-tile-dest-size world))
         (half-w (world-collision-half-width world))
         (half-h (world-collision-half-height world))
         (max-hops *minimap-resolve-max-hops*))
    (when (and zone-id graph tile-size half-w half-h)
      (let ((cur-zone zone-id)
            (cur-x (float raw-x 1.0))
            (cur-y (float raw-y 1.0))
            (walked-zones nil)
            (walked-edges nil))
        (block walk-loop
          (loop :for hop :from 0 :below max-hops :do
            (multiple-value-bind (min-x max-x min-y max-y)
                (get-zone-bounds-from-any-cache cur-zone world tile-size half-w half-h)
              (unless (and min-x max-x min-y max-y)
                (return-from walk-loop nil))
              ;; Check if coords are inside current zone bounds
              (when (and (>= cur-x min-x) (<= cur-x max-x)
                         (>= cur-y min-y) (<= cur-y max-y))
                (return-from walk-loop
                  (values cur-zone (nreverse walked-zones) (nreverse walked-edges))))
              ;; Determine dominant overshoot edge
              (let* ((overshoot-west  (if (< cur-x min-x) (- min-x cur-x) 0.0))
                     (overshoot-east  (if (> cur-x max-x) (- cur-x max-x) 0.0))
                     (overshoot-north (if (< cur-y min-y) (- min-y cur-y) 0.0))
                     (overshoot-south (if (> cur-y max-y) (- cur-y max-y) 0.0))
                     (max-overshoot (max overshoot-west overshoot-east
                                         overshoot-north overshoot-south))
                     (edge (cond ((and (> max-overshoot 0.0) (= max-overshoot overshoot-east))  :east)
                                 ((and (> max-overshoot 0.0) (= max-overshoot overshoot-west))  :west)
                                 ((and (> max-overshoot 0.0) (= max-overshoot overshoot-south)) :south)
                                 ((and (> max-overshoot 0.0) (= max-overshoot overshoot-north)) :north)
                                 (t nil))))
                (unless edge (return-from walk-loop nil))
                ;; Find exit for this edge
                (let ((exit-spec (find edge (world-graph-exits graph cur-zone)
                                       :key (lambda (e) (getf e :edge)) :test #'eq)))
                  (unless (and exit-spec (spatial-exit-p exit-spec))
                    (return-from walk-loop nil))
                  (let ((next-zone (getf exit-spec :to)))
                    (unless next-zone (return-from walk-loop nil))
                    ;; Translate coordinates through seam
                    (multiple-value-bind (next-min-x next-max-x next-min-y next-max-y)
                        (get-zone-bounds-from-any-cache next-zone world tile-size half-w half-h)
                      (unless (and next-min-x next-max-x next-min-y next-max-y)
                        (return-from walk-loop nil))
                      (multiple-value-bind (tx ty)
                          (seam-translate-position edge cur-x cur-y
                                                   min-x max-x min-y max-y
                                                   next-min-x next-max-x next-min-y next-max-y)
                        (push cur-zone walked-zones)
                        (push edge walked-edges)
                        (setf cur-zone next-zone
                              cur-x tx
                              cur-y ty))))))))
          ;; Max hops reached — click too far for configured cap
          (log-verbose "resolve-click: max hops (~d) reached from ~a, click at (~,1f,~,1f) dropped"
                       max-hops zone-id raw-x raw-y)
          nil)))))

(defun translate-click-along-path (world raw-x raw-y from-id zone-path
                                   &optional zone-lru-cache)
  "Chain seam translations along ZONE-PATH (list of zone-ids from FROM-ID).
   Returns (values final-x final-y hop-targets) where hop-targets is a list
   of (x . y) cons — the translated coordinates BEFORE entering each hop's
   destination zone (i.e. the raw walk target for that hop, in the source
   zone's coordinate space).  Returns NIL if any hop fails."
  (let* ((tile-size (world-tile-dest-size world))
         (half-w (world-collision-half-width world))
         (half-h (world-collision-half-height world))
         (graph (world-world-graph world)))
    (when (and tile-size half-w half-h graph zone-path)
      (let ((cur-x (float raw-x 1.0))
            (cur-y (float raw-y 1.0))
            (prev-zone from-id)
            (hop-targets nil))
        (dolist (next-zone zone-path)
          (let ((edge (world-graph-edge-between graph prev-zone next-zone)))
            (unless edge (return-from translate-click-along-path nil))
            (multiple-value-bind (prev-min-x prev-max-x prev-min-y prev-max-y)
                (get-zone-bounds-from-any-cache prev-zone world tile-size half-w half-h
                                                zone-lru-cache)
              (multiple-value-bind (next-min-x next-max-x next-min-y next-max-y)
                  (get-zone-bounds-from-any-cache next-zone world tile-size half-w half-h
                                                  zone-lru-cache)
                (unless (and prev-min-x prev-max-x prev-min-y prev-max-y
                             next-min-x next-max-x next-min-y next-max-y)
                  (return-from translate-click-along-path nil))
                ;; Record cur-x/cur-y as the walk target for this hop.
                ;; This is where the click lands in the current (source) zone's
                ;; coordinate space — the player should walk toward this point
                ;; to trigger the zone crossing.
                (push (cons cur-x cur-y) hop-targets)
                (multiple-value-bind (tx ty)
                    (seam-translate-position edge cur-x cur-y
                                             prev-min-x prev-max-x prev-min-y prev-max-y
                                             next-min-x next-max-x next-min-y next-max-y)
                  (setf cur-x tx cur-y ty)))))
          (setf prev-zone next-zone))
        ;; Clamp final result to destination zone bounds
        (let ((dest-zone (car (last zone-path))))
          (multiple-value-bind (fin-min-x fin-max-x fin-min-y fin-max-y)
              (get-zone-bounds-from-any-cache dest-zone world tile-size half-w half-h
                                              zone-lru-cache)
            (values (if (and fin-min-x fin-max-x)
                        (clamp cur-x fin-min-x fin-max-x)
                        cur-x)
                    (if (and fin-min-y fin-max-y)
                        (clamp cur-y fin-min-y fin-max-y)
                        cur-y)
                    (nreverse hop-targets))))))))

(defun zone-path-edge-list (graph from-id zone-path)
  "Compute per-hop edge directions for ZONE-PATH starting from FROM-ID.
   Returns a list of edge keywords (:north/:south/:east/:west) parallel to
   ZONE-PATH, or NIL if any hop lacks a direct spatial connection."
  (when (and graph zone-path)
    (let ((edges nil)
          (prev from-id))
      (dolist (next-zone zone-path)
        (let ((edge (world-graph-edge-between graph prev next-zone)))
          (unless edge (return-from zone-path-edge-list nil))
          (push edge edges))
        (setf prev next-zone))
      (nreverse edges))))

(defun opposite-edge (edge)
  "Return the opposite cardinal edge direction."
  (case edge
    (:north :south)
    (:south :north)
    (:east  :west)
    (:west  :east)
    (t nil)))

(defun reverse-translate-along-path (world dest-x dest-y from-id zone-path
                                     &optional zone-lru-cache)
  "Reverse-translate DEST-X/DEST-Y from the last zone in ZONE-PATH back to
   FROM-ID's local coordinate space.  ZONE-PATH is ordered from FROM-ID to
   destination (excluding FROM-ID, same format as translate-click-along-path).
   Returns (values from-x from-y) or NIL if any hop fails.
   Used by the retry path to convert destination-zone coords into the current
   zone's local space before feeding into translate-click-along-path."
  (let* ((tile-size (world-tile-dest-size world))
         (half-w (world-collision-half-width world))
         (half-h (world-collision-half-height world))
         (graph (world-world-graph world)))
    (when (and tile-size half-w half-h graph zone-path)
      (let ((cur-x (float dest-x 1.0))
            (cur-y (float dest-y 1.0))
            ;; Build the full chain: (from-id zone-1 zone-2 ... dest)
            (chain (cons from-id zone-path)))
        ;; Walk backward through consecutive pairs.
        ;; chain = (A B C D). Pairs forward: A->B, B->C, C->D.
        ;; Reverse order: C->D, B->C, A->B (i.e. translate D-space -> A-space).
        (let ((pairs nil))
          (loop :for (a b) :on chain
                :while b
                :do (push (cons a b) pairs))
          ;; pairs is now ((C . D) (B . C) (A . B)) — reverse hop order
          (dolist (pair pairs)
            (let* ((src-zone (car pair))   ; the zone we're translating INTO
                   (dst-zone (cdr pair))   ; the zone we're translating FROM
                   (fwd-edge (world-graph-edge-between graph src-zone dst-zone)))
              (unless fwd-edge (return-from reverse-translate-along-path nil))
              (let ((rev-edge (opposite-edge fwd-edge)))
                (unless rev-edge (return-from reverse-translate-along-path nil))
                (multiple-value-bind (dst-min-x dst-max-x dst-min-y dst-max-y)
                    (get-zone-bounds-from-any-cache dst-zone world tile-size half-w half-h
                                                    zone-lru-cache)
                  (multiple-value-bind (src-min-x src-max-x src-min-y src-max-y)
                      (get-zone-bounds-from-any-cache src-zone world tile-size half-w half-h
                                                      zone-lru-cache)
                    (unless (and dst-min-x dst-max-x dst-min-y dst-max-y
                                 src-min-x src-max-x src-min-y src-max-y)
                      (return-from reverse-translate-along-path nil))
                    ;; Reverse: seam-translate with opposite edge, swapping src/dst
                    (multiple-value-bind (tx ty)
                        (seam-translate-position rev-edge cur-x cur-y
                                                 dst-min-x dst-max-x dst-min-y dst-max-y
                                                 src-min-x src-max-x src-min-y src-max-y)
                      (setf cur-x tx cur-y ty))))))))
        (values cur-x cur-y)))))

(defun compute-minimap-click-path (world player raw-x raw-y)
  "Compute a multi-hop zone path for any cross-zone click.
   Returns (values zone-path edge-list hop-targets final-x final-y) where:
     zone-path  — ordered list of zones to traverse (excluding current, including dest)
     edge-list  — per-hop edge directions (parallel to zone-path)
     hop-targets — per-hop walk targets as (x . y) cons (parallel to zone-path);
                   each target is in the SOURCE zone's coordinate space for that hop,
                   precomputed from the original raw click via seam translation
     final-x/y — translated click coordinates in the destination zone
   Returns NIL if click is inside current zone or no path found.

   Path selection: uses BFS (world-graph-find-path) for shortest-path guarantee.
   Coordinate translation: translates the original raw click along the BFS path
   via seam-translate-position at each hop, collecting per-hop intermediate targets.
   The continuation reads these precomputed targets instead of translating the
   player's runtime position (which can be anywhere in the zone, not at the edge)."
  (let* ((zone (world-zone world))
         (current-zone-id (and zone (zone-id zone)))
         (graph (world-world-graph world)))
    (when (and current-zone-id graph player)
      ;; Find destination zone by walking raw click through zone bounds.
      (multiple-value-bind (dest-zone-id walked-zones walked-edges)
          (resolve-click-destination-zone world raw-x raw-y)
        (declare (ignore walked-zones walked-edges))
        (when (and dest-zone-id (not (eq dest-zone-id current-zone-id)))
          ;; BFS shortest path from current zone to destination.
          (let ((zone-path (world-graph-find-path graph current-zone-id dest-zone-id)))
            (when zone-path
              ;; Compute per-hop edge directions
              (let ((edge-list (zone-path-edge-list graph current-zone-id zone-path)))
                (when edge-list
                  ;; Translate the original raw click along the BFS path,
                  ;; collecting per-hop walk targets for the continuation.
                  (multiple-value-bind (final-x final-y hop-targets)
                      (translate-click-along-path world raw-x raw-y
                                                  current-zone-id zone-path)
                    (when (and final-x final-y hop-targets)
                      (values zone-path edge-list hop-targets
                              final-x final-y))))))))))))

