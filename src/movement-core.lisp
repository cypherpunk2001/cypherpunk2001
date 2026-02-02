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

(defun zone-bounds-zero-origin (tile-dest-size width height collision-half-w collision-half-h)
  "Calculate movement bounds for a zone with zero-origin wall-map.
   Unlike zone-bounds-from-dimensions, this ignores *wall-origin-x/y* globals
   since per-zone wall-maps are always zero-indexed."
  (let* ((wall-min-x (+ (* 1 tile-dest-size) collision-half-w))
         (wall-max-x (- (* (1- width) tile-dest-size) collision-half-w))
         (wall-min-y (+ (* 1 tile-dest-size) collision-half-h))
         (wall-max-y (- (* (1- height) tile-dest-size) collision-half-h)))
    (values wall-min-x wall-max-x wall-min-y wall-max-y)))

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
  (let* ((wall-min-x (+ (* (+ *wall-origin-x* 1) tile-dest-size)
                        collision-half-width))
         (wall-max-x (- (* (+ *wall-origin-x* (1- width))
                           tile-dest-size)
                        collision-half-width))
         (wall-min-y (+ (* (+ *wall-origin-y* 1) tile-dest-size)
                        collision-half-height))
         (wall-max-y (- (* (+ *wall-origin-y* (1- height))
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
