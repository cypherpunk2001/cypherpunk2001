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

(defun derive-wall-map-from-zone (zone)
  "Create a wall map from zone collision data.
   Returns a 2D array where 1 = blocked, 0 = passable."
  (when zone
    (let* ((width (zone-width zone))
           (height (zone-height zone))
           (map (make-array (list height width) :initial-element 0))
           (collision-tiles (zone-collision-tiles zone)))
      (when collision-tiles
        (maphash (lambda (key _val)
                   (declare (ignore _val))
                   ;; Keys are packed integers from tile-key, not cons cells
                   (let ((tx (tile-key-x key))
                         (ty (tile-key-y key)))
                     (when (and (>= tx 0) (< tx width)
                                (>= ty 0) (< ty height))
                       (setf (aref map ty tx) 1))))
                 collision-tiles))
      map)))

(defun build-wall-map ()
  ;; Create a test wall map array with a solid border.
  (let* ((width *wall-map-width*)
         (height *wall-map-height*)
         (map (make-array (list height width) :initial-element 0)))
    (labels ((set-wall (x y)
               (when (and (<= 0 x) (< x width)
                          (<= 0 y) (< y height))
                 (setf (aref map y x) 1))))
      (loop :for x :from 0 :below width
            :do (set-wall x 0)
                (set-wall x (1- height)))
      (loop :for y :from 0 :below height
            :do (set-wall 0 y)
                (set-wall (1- width) y)))
    map))

(defun wall-occupied-p (wall-map tx ty)
  ;; Check whether a tile inside the wall map is nonzero.
  (when wall-map
    (let* ((local-x (- tx *wall-origin-x*))
           (local-y (- ty *wall-origin-y*))
           (width (array-dimension wall-map 1))
           (height (array-dimension wall-map 0)))
      (and (<= 0 local-x)
           (< local-x width)
           (<= 0 local-y)
           (< local-y height)
           (not (zerop (aref wall-map local-y local-x)))))))

(defun wall-blocked-p (wall-map tx ty)
  ;; Treat walls and out-of-bounds tiles as blocked for collision.
  ;; Uses global *wall-origin-x/y* for coordinate translation.
  (if (not wall-map)
      nil
      (let* ((local-x (- tx *wall-origin-x*))
             (local-y (- ty *wall-origin-y*))
             (width (array-dimension wall-map 1))
             (height (array-dimension wall-map 0)))
        (if (or (< local-x 0)
                (>= local-x width)
                (< local-y 0)
                (>= local-y height))
            t
            (not (zerop (aref wall-map local-y local-x)))))))

(defun wall-blocked-p-zero-origin (wall-map tx ty)
  "Test if tile (TX, TY) is blocked in WALL-MAP assuming zero origin.
   Used for zone-state wall-maps which are stored without offset."
  (if (not wall-map)
      nil
      (let* ((width (array-dimension wall-map 1))
             (height (array-dimension wall-map 0)))
        (if (or (< tx 0)
                (>= tx width)
                (< ty 0)
                (>= ty height))
            t
            (not (zerop (aref wall-map ty tx)))))))

(defun world-blocked-tile-p (world tx ty)
  ;; Return true when a tile coordinate blocks movement.
  (wall-blocked-p (world-wall-map world) tx ty))

(defun set-world-blocked-tile (world tx ty value)
  ;; Update the world wall map at TX/TY for editor changes.
  (let* ((wall-map (world-wall-map world))
         (local-x (- tx *wall-origin-x*))
         (local-y (- ty *wall-origin-y*)))
    (when (and wall-map
               (<= 0 local-x)
               (<= 0 local-y)
               (< local-x (array-dimension wall-map 1))
               (< local-y (array-dimension wall-map 0)))
      (let* ((next (if (and value (not (zerop value))) 1 0))
             (prev (aref wall-map local-y local-x)))
        (when (/= prev next)
          (setf (aref wall-map local-y local-x) next)
          (setf (world-minimap-collisions world)
                (build-minimap-collisions world)))))))

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

(defun position-blocked-p (world x y half-w half-h)
  ;; Return true when a collider centered at X/Y is blocked.
  (blocked-at-p world x y half-w half-h (world-tile-dest-size world)))

(defun find-open-tile (world start-tx start-ty half-w half-h &optional max-radius)
  ;; Return the nearest open tile around START-TX/START-TY.
  (let ((max-radius (max 0 (or max-radius 0)))
        (tile-size (world-tile-dest-size world)))
    (loop :for radius :from 0 :to max-radius
          :do (loop :for dy :from (- radius) :to radius
                    :do (loop :for dx :from (- radius) :to radius
                              :for tx = (+ start-tx dx)
                              :for ty = (+ start-ty dy)
                              :do (multiple-value-bind (cx cy)
                                      (tile-center-position tile-size tx ty)
                                    (when (not (position-blocked-p world cx cy half-w half-h))
                                      (return-from find-open-tile
                                        (values tx ty)))))))
  (values start-tx start-ty)))

(defun world-open-position-for (world x y half-w half-h)
  ;; Return the nearest open tile center for a collider near X/Y.
  (let* ((tile-size (world-tile-dest-size world))
         (tx (floor x tile-size))
         (ty (floor y tile-size))
         (radius (world-search-radius world)))
    (multiple-value-bind (open-tx open-ty)
        (find-open-tile world tx ty half-w half-h radius)
      (tile-center-position tile-size open-tx open-ty))))

(defun world-open-position (world x y)
  ;; Return the nearest open tile center for the player collider.
  (world-open-position-for world x y
                           (world-collision-half-width world)
                           (world-collision-half-height world)))

;;; Zone-state spawn helpers (Phase 1 - per-zone collision)

(defun find-open-position-with-map (wall-map x y half-w half-h tile-size)
  "Find nearest open position using WALL-MAP (zero-origin).
   Mirrors find-open-tile spiral search pattern."
  (if (not (blocked-at-p-with-map wall-map x y half-w half-h tile-size))
      (values x y)
      ;; Spiral search for open tile - use map dimensions as max radius
      (let ((max-radius (if wall-map
                            (max (array-dimension wall-map 0)
                                 (array-dimension wall-map 1))
                            20)))
        (loop :for radius :from 1 :to max-radius
              :do (loop :for dy :from (- radius) :to radius
                        :do (loop :for dx :from (- radius) :to radius
                                  :for tx = (+ (floor x tile-size) dx)
                                  :for ty = (+ (floor y tile-size) dy)
                                  :for cx = (+ (* tx tile-size) (/ tile-size 2.0))
                                  :for cy = (+ (* ty tile-size) (/ tile-size 2.0))
                                  :when (not (blocked-at-p-with-map wall-map cx cy half-w half-h tile-size))
                                  :do (return-from find-open-position-with-map (values cx cy))))
              :finally (return (values x y))))))  ; Fallback to original position

(defun zone-state-spawn-position (zone-state)
  "Return valid spawn (x, y) using ZONE-STATE's wall-map.
   Used for spawning players in a specific zone without relying on global world collision.
   Uses zero-origin bounds since zone wall-maps are zero-indexed."
  (let* ((wall-map (zone-state-wall-map zone-state))
         (tile-dest-size (* (float *tile-size* 1.0) *tile-scale*))
         ;; Match make-world formula for player collision
         (collision-half (* (/ tile-dest-size 2.0) *player-collision-scale*))
         (width (if wall-map (array-dimension wall-map 1) 64))
         (height (if wall-map (array-dimension wall-map 0) 64)))
    ;; Calculate zone bounds using zero-origin (per-zone wall-maps are zero-indexed)
    (multiple-value-bind (min-x max-x min-y max-y)
        (zone-bounds-zero-origin tile-dest-size width height collision-half collision-half)
      ;; Start from center of zone
      (let ((center-x (/ (+ min-x max-x) 2.0))
            (center-y (/ (+ min-y max-y) 2.0)))
        ;; Find open position from center
        (find-open-position-with-map wall-map center-x center-y
                                     collision-half collision-half tile-dest-size)))))

(defun ensure-npcs-open-spawn (npcs world)
  ;; Move NPCs to open tiles and reset their home positions.
  (loop :for npc :across npcs
        :do (multiple-value-bind (half-w half-h)
                (npc-collision-half world)
              (multiple-value-bind (nx ny)
                  (world-open-position-for world (npc-x npc) (npc-y npc)
                                           half-w half-h)
                (setf (npc-x npc) nx
                      (npc-y npc) ny
                      (npc-home-x npc) nx
                      (npc-home-y npc) ny
                      (npc-wander-x npc) nx
                      (npc-wander-y npc) ny)))))

(defun blocked-at-p (world x y half-w half-h tile-size)
  "Test collider bounds against blocked tiles in the world map."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type world world)
           (type single-float x y half-w half-h tile-size))
  (let* ((left (the single-float (- x half-w)))
         (right (the single-float (+ x half-w)))
         (top (the single-float (- y half-h)))
         (bottom (the single-float (+ y half-h)))
         (right-edge (the single-float (- right *collision-edge-epsilon*)))
         (bottom-edge (the single-float (- bottom *collision-edge-epsilon*)))
         (tx1 (the fixnum (floor left tile-size)))
         (tx2 (the fixnum (floor right-edge tile-size)))
         (ty1 (the fixnum (floor top tile-size)))
         (ty2 (the fixnum (floor bottom-edge tile-size))))
    (declare (type single-float left right top bottom right-edge bottom-edge)
             (type fixnum tx1 tx2 ty1 ty2))
    (loop :for ty fixnum :from ty1 :to ty2
          :thereis (loop :for tx fixnum :from tx1 :to tx2
                         :thereis (world-blocked-tile-p world tx ty)))))

(defun blocked-at-p-with-map (wall-map x y half-w half-h tile-size)
  "Test collider bounds against blocked tiles using WALL-MAP (zero-origin).
   Factored from blocked-at-p to support per-zone collision checking."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type (or null array) wall-map)
           (type single-float x y half-w half-h tile-size))
  (let* ((left (the single-float (- x half-w)))
         (right (the single-float (+ x half-w)))
         (top (the single-float (- y half-h)))
         (bottom (the single-float (+ y half-h)))
         (right-edge (the single-float (- right *collision-edge-epsilon*)))
         (bottom-edge (the single-float (- bottom *collision-edge-epsilon*)))
         (tx1 (the fixnum (floor left tile-size)))
         (tx2 (the fixnum (floor right-edge tile-size)))
         (ty1 (the fixnum (floor top tile-size)))
         (ty2 (the fixnum (floor bottom-edge tile-size))))
    (declare (type single-float left right top bottom right-edge bottom-edge)
             (type fixnum tx1 tx2 ty1 ty2))
    (loop :for ty fixnum :from ty1 :to ty2
          :thereis (loop :for tx fixnum :from tx1 :to tx2
                         :thereis (wall-blocked-p-zero-origin wall-map tx ty)))))

(defun attempt-move (world x y dx dy step half-w half-h tile-size)
  "Resolve movement per axis and cancel movement when blocked."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type world world)
           (type single-float x y dx dy step half-w half-h tile-size))
  (let ((nx x)
        (ny y)
        (out-dx 0.0)
        (out-dy 0.0))
    (declare (type single-float nx ny out-dx out-dy))
    (when (not (zerop dx))
      (let ((try-x (the single-float (+ x (* dx step)))))
        (declare (type single-float try-x))
        (if (blocked-at-p world try-x y half-w half-h tile-size)
            (setf out-dx 0.0)
            (setf nx try-x
                  out-dx dx))))
    (when (not (zerop dy))
      (let ((try-y (the single-float (+ ny (* dy step)))))
        (declare (type single-float try-y))
        (if (blocked-at-p world nx try-y half-w half-h tile-size)
            (setf out-dy 0.0)
            (setf ny try-y
                  out-dy dy))))
    (values nx ny out-dx out-dy)))

(defun attempt-move-with-map (wall-map x y dx dy step half-w half-h tile-size)
  "Resolve movement per axis using WALL-MAP for collision.
   Used for per-zone collision checking where global world map isn't appropriate."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type (or null array) wall-map)
           (type single-float x y dx dy step half-w half-h tile-size))
  (let ((nx x)
        (ny y)
        (out-dx 0.0)
        (out-dy 0.0))
    (declare (type single-float nx ny out-dx out-dy))
    (when (not (zerop dx))
      (let ((try-x (the single-float (+ x (* dx step)))))
        (declare (type single-float try-x))
        (if (blocked-at-p-with-map wall-map try-x y half-w half-h tile-size)
            (setf out-dx 0.0)
            (setf nx try-x
                  out-dx dx))))
    (when (not (zerop dy))
      (let ((try-y (the single-float (+ ny (* dy step)))))
        (declare (type single-float try-y))
        (if (blocked-at-p-with-map wall-map nx try-y half-w half-h tile-size)
            (setf out-dy 0.0)
            (setf ny try-y
                  out-dy dy))))
    (values nx ny out-dx out-dy)))

(defun npc-collision-half (world)
  ;; Return NPC collider half sizes in world pixels.
  (let ((half (* (/ (world-tile-dest-size world) 2.0) *npc-collision-scale*)))
    (values half half)))

(defun update-running-state (player dt moving toggle-run)
  "Update stamina and return the current speed multiplier."
  (declare (type player player)
           (type single-float dt))
  (let ((old-stamina (player-run-stamina player)))
    (declare (type single-float old-stamina))
    (when toggle-run
    (if (> (player-run-stamina player) 0.0)
        (setf (player-running player) (not (player-running player)))
        (setf (player-running player) nil)))
    (if (and (player-running player) moving (> (player-run-stamina player) 0.0))
        (progn
          (decf (player-run-stamina player) dt)
          (when (<= (player-run-stamina player) 0.0)
            (setf (player-run-stamina player) 0.0
                  (player-running player) nil)))
        (when (< (player-run-stamina player) *run-stamina-max*)
          (incf (player-run-stamina player) dt)
          (when (>= (player-run-stamina player) *run-stamina-max*)
            (setf (player-run-stamina player) *run-stamina-max*))))
    (when (/= old-stamina (player-run-stamina player))
      (setf (player-snapshot-dirty player) t))
    (if (and (player-running player) (> (player-run-stamina player) 0.0))
        *run-speed-mult*
        1.0)))

(defun update-player-position (player intent world speed-mult dt)
  "Move the player with collision and target logic.
   Uses per-zone collision when available, falls back to global world collision."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type player player)
           (type intent intent)
           (type world world)
           (type single-float speed-mult dt))
  (let* ((x (player-x player))
         (y (player-y player))
         (input-dx (intent-move-dx intent))
         (input-dy (intent-move-dy intent))
         (dx 0.0)
         (dy 0.0)
         ;; Per-zone collision support
         (player-zone (player-zone-id player))
         (zone-wall-map (when player-zone (get-zone-wall-map player-zone)))
         (half-w (world-collision-half-width world))
         (half-h (world-collision-half-height world))
         (tile-size (world-tile-dest-size world)))
    (declare (type single-float x y input-dx input-dy dx dy half-w half-h tile-size))
    ;; Define move helper that uses per-zone or global collision
    (flet ((do-move (from-x from-y dir-x dir-y step)
             (if zone-wall-map
                 (attempt-move-with-map zone-wall-map from-x from-y dir-x dir-y step
                                        half-w half-h tile-size)
                 (attempt-move world from-x from-y dir-x dir-y step
                               half-w half-h tile-size))))
      (cond
        ((or (not (zerop input-dx))
             (not (zerop input-dy)))
         (clear-intent-target intent)
         (multiple-value-setq (x y dx dy)
           (do-move x y input-dx input-dy (* *player-speed* speed-mult dt))))
        ((intent-target-active intent)
         (let* ((target-x (intent-target-x intent))
                (target-y (intent-target-y intent))
                (to-x (- target-x x))
                (to-y (- target-y y))
                (dist (sqrt (+ (* to-x to-x) (* to-y to-y)))))
           (if (<= dist *target-epsilon*)
               (setf (intent-target-active intent) nil
                     dx 0.0
                     dy 0.0)
               (let* ((dir-x (/ to-x dist))
                      (dir-y (/ to-y dist))
                      (step (min (* *player-speed* speed-mult dt) dist)))
                 (multiple-value-setq (x y dx dy)
                   (do-move x y dir-x dir-y step))
                 (when (or (<= dist step)
                           (and (zerop dx) (zerop dy)))
                   (setf (intent-target-active intent) nil))))))
        (t
         (setf dx 0.0
               dy 0.0))))
    ;; Clamp to zone bounds if available, otherwise global world bounds
    (multiple-value-bind (min-x max-x min-y max-y)
        (if zone-wall-map
            (get-zone-collision-bounds player-zone tile-size half-w half-h)
            (values (world-wall-min-x world) (world-wall-max-x world)
                    (world-wall-min-y world) (world-wall-max-y world)))
      (setf x (clamp x min-x max-x)
            y (clamp y min-y max-y)))
    (let ((old-x (player-x player))
          (old-y (player-y player)))
      (setf (player-x player) x
            (player-y player) y
            (player-dx player) dx
            (player-dy player) dy)
      ;; Tier-2 write: position changes should be marked dirty for batched saves
      ;; Also mark snapshot-dirty for delta compression (see docs/net.md Prong 2)
      (when (or (/= old-x x) (/= old-y y))
        (setf (player-snapshot-dirty player) t)
        (mark-player-dirty (player-id player))
        ;; Update spatial grid if cell changed
        (let ((zone-state (get-zone-state player-zone)))
          (when zone-state
            (let ((grid (zone-state-player-grid zone-state)))
              (when grid
                (multiple-value-bind (new-cx new-cy changed)
                    (spatial-grid-move grid (player-id player)
                                       (player-grid-cell-x player)
                                       (player-grid-cell-y player)
                                       x y)
                  (when changed
                    (setf (player-grid-cell-x player) new-cx
                          (player-grid-cell-y player) new-cy)))))))))))

(defun player-intent-direction (player)
  ;; Return the intended movement direction for edge transitions.
  (let* ((intent (player-intent player))
         (dx (intent-move-dx intent))
         (dy (intent-move-dy intent)))
    (cond
      ((or (not (zerop dx)) (not (zerop dy)))
       (values dx dy))
      ((intent-target-active intent)
       (normalize-vector (- (intent-target-x intent) (player-x player))
                         (- (intent-target-y intent) (player-y player))))
      (t
       (values 0.0 0.0)))))

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

;;;; ========================================================================
;;;; Directional Gating (Step 3) — Only arm/transition when movement intent
;;;; has a significant component toward the zone edge.
;;;; ========================================================================

(defun edge-direction-passes-p (dx dy edge)
  "Return T if movement direction (DX, DY) has sufficient component toward EDGE.
   Uses *zone-direction-threshold* as minimum normalized dot product.
   Returns nil for zero movement vectors (stationary players never trigger)."
  (let ((mag (sqrt (+ (* dx dx) (* dy dy)))))
    (when (< mag 0.001)
      (return-from edge-direction-passes-p nil))
    (let ((dot (case edge
                 (:north (/ (- dy) mag))
                 (:south (/ dy mag))
                 (:west  (/ (- dx) mag))
                 (:east  (/ dx mag))
                 (t 0.0))))
      (>= dot *zone-direction-threshold*))))

(defun edge-direction-dot (dx dy edge)
  "Return normalized dot product of (DX, DY) with EDGE's inward normal.
   Returns 0.0 for zero-length vectors. Used to rank edges by alignment strength."
  (let ((mag (sqrt (+ (* dx dx) (* dy dy)))))
    (if (< mag 0.001)
        0.0
        (case edge
          (:north (/ (- dy) mag))
          (:south (/ dy mag))
          (:west  (/ (- dx) mag))
          (:east  (/ dx mag))
          (t 0.0)))))

(defun spatial-exit-p (exit-spec)
  "Return T if EXIT-SPEC is a spatial edge (not a teleport).
   Spatial exits have :offset :preserve-x or :preserve-y."
  (let ((offset (getf exit-spec :offset)))
    (or (eq offset :preserve-x)
        (eq offset :preserve-y))))

;;;; ========================================================================
;;;; Arm-Band Detection (Step 2) — Determine if player is within hysteresis
;;;; arm, cancel, or commit zones relative to zone bounds.
;;;; ========================================================================

(defun player-distance-to-edge (player edge min-x max-x min-y max-y)
  "Return the player's distance inward from EDGE in pixels.
   Larger value = further from edge = deeper in zone interior."
  (let ((x (player-x player))
        (y (player-y player)))
    (case edge
      (:north (- y min-y))
      (:south (- max-y y))
      (:west  (- x min-x))
      (:east  (- max-x x))
      (t 0.0))))

(defun player-in-arm-band-p (player edge min-x max-x min-y max-y tile-dest-size)
  "Return T if PLAYER is within the arm band for EDGE (between arm line and zone edge).
   Arm line = *zone-hysteresis-in* tiles from edge."
  (let ((dist (player-distance-to-edge player edge min-x max-x min-y max-y))
        (arm-px (* *zone-hysteresis-in* tile-dest-size)))
    (<= dist arm-px)))

(defun player-past-cancel-line-p (player edge min-x max-x min-y max-y tile-dest-size)
  "Return T if PLAYER has retreated past the cancel line for EDGE.
   Cancel line = *zone-hysteresis-out* tiles from edge (deeper into interior than arm)."
  (let ((dist (player-distance-to-edge player edge min-x max-x min-y max-y))
        (cancel-px (* *zone-hysteresis-out* tile-dest-size)))
    (> dist cancel-px)))

(defun world-exit-edge-with-bounds (player min-x max-x min-y max-y
                                    &optional (commit-margin 0.0))
  "Return the edge the player is pushing against using specified bounds.
   COMMIT-MARGIN is added to relax the boundary check so the player doesn't
   need to push through collision to trigger commit. Applies directional gating (Step 3)."
  (multiple-value-bind (dx dy)
      (player-intent-direction player)
    (let ((edge nil)
          (weight 0.0)
          (x (player-x player))
          (y (player-y player)))
      (when (and (< dy 0.0) (<= y (+ min-y commit-margin)))
        (let ((w (abs dy)))
          (when (> w weight)
            (setf edge :north
                  weight w))))
      (when (and (> dy 0.0) (>= y (- max-y commit-margin)))
        (let ((w (abs dy)))
          (when (> w weight)
            (setf edge :south
                  weight w))))
      (when (and (< dx 0.0) (<= x (+ min-x commit-margin)))
        (let ((w (abs dx)))
          (when (> w weight)
            (setf edge :west
                  weight w))))
      (when (and (> dx 0.0) (>= x (- max-x commit-margin)))
        (let ((w (abs dx)))
          (when (> w weight)
            (setf edge :east
                  weight w))))
      ;; Step 3: Directional gating — reject if movement doesn't point toward edge
      (when (and edge (not (edge-direction-passes-p dx dy edge)))
        (when *verbose-zone-transitions*
          (log-zone "Zone transition: directional gating rejected edge ~a (dx=~,2f dy=~,2f)"
                    edge dx dy))
        (setf edge nil))
      edge)))

(defun world-exit-edge (world player)
  ;; Return the edge the player is pushing against, if any.
  ;; Uses per-zone bounds when available, falls back to global world bounds.
  (let* ((player-zone (player-zone-id player))
         (tile-size (world-tile-dest-size world))
         (half-w (world-collision-half-width world))
         (half-h (world-collision-half-height world)))
    (multiple-value-bind (min-x max-x min-y max-y)
        (if (and player-zone (get-zone-wall-map player-zone))
            (get-zone-collision-bounds player-zone tile-size half-w half-h)
            (values (world-wall-min-x world) (world-wall-max-x world)
                    (world-wall-min-y world) (world-wall-max-y world)))
      (world-exit-edge-with-bounds player min-x max-x min-y max-y))))

(defun world-preview-edge (world player)
  ;; Return the edge to preview minimap spawns for.
  (let ((edge (world-exit-edge world player)))
    (if edge
        edge
        (let* ((zone (world-zone world))
               (zone-id (and zone (zone-id zone)))
               (graph (world-world-graph world))
               (threshold (* (world-tile-dest-size world)
                             (max 0.0 *minimap-preview-edge-tiles*)))
               (x (player-x player))
               (y (player-y player))
               (min-x (world-wall-min-x world))
               (max-x (world-wall-max-x world))
               (min-y (world-wall-min-y world))
               (max-y (world-wall-max-y world)))
          (when (and graph zone-id (> threshold 0.0))
            (let ((best-edge nil)
                  (best-distance nil))
              (dolist (exit (world-graph-exits graph zone-id))
                (let* ((edge (getf exit :edge))
                       (edge-distance (case edge
                                        (:west (max 0.0 (- x min-x)))
                                        (:east (max 0.0 (- max-x x)))
                                        (:north (max 0.0 (- y min-y)))
                                        (:south (max 0.0 (- max-y y)))
                                        (t nil))))
                  (when (and edge-distance
                             (<= edge-distance threshold))
                    (when (or (null best-distance)
                              (< edge-distance best-distance))
                      (setf best-distance edge-distance
                            best-edge edge)))))
              best-edge))))))

(defun camera-view-center (player editor)
  ;; Return the current camera focus point.
  (if (and editor (editor-active editor))
      (values (editor-camera-x editor) (editor-camera-y editor))
      (values (player-x player) (player-y player))))

(defun camera-view-bounds (camera player editor)
  ;; Return view bounds in world coordinates for the current camera focus.
  (let* ((zoom (camera-zoom camera))
         (half-view-width (/ (current-screen-width) (* 2.0 zoom)))
         (half-view-height (/ (current-screen-height) (* 2.0 zoom))))
    (multiple-value-bind (x y) (camera-view-center player editor)
      (values (- x half-view-width) (+ x half-view-width)
              (- y half-view-height) (+ y half-view-height)))))

(defun view-exceeds-edge-p (world view-left view-right view-top view-bottom edge)
  ;; Return true when the camera view extends beyond EDGE.
  (case edge
    (:west (< view-left (world-wall-min-x world)))
    (:east (> view-right (world-wall-max-x world)))
    (:north (< view-top (world-wall-min-y world)))
    (:south (> view-bottom (world-wall-max-y world)))
    (t nil)))

(defun world-edge-exit-for-zone (world zone-id edge)
  ;; Return the exit spec for EDGE in ZONE-ID.
  (let ((graph (world-world-graph world)))
    (when (and edge zone-id graph)
      (find edge (world-graph-exits graph zone-id)
            :key (lambda (exit) (getf exit :edge))
            :test #'eq))))

(defun world-diagonal-zone-id (world edge-a edge-b)
  ;; Return the diagonal zone id for EDGE-A + EDGE-B if the graph connects it.
  ;; Only follows spatial exits (not teleports/dungeon portals).
  (let* ((zone (world-zone world))
         (zone-id (and zone (zone-id zone))))
    (when zone-id
      (labels ((next-zone (from edge)
                 (let ((exit (world-edge-exit-for-zone world from edge)))
                   (and exit (spatial-exit-p exit) (getf exit :to)))))
        (let ((first (next-zone zone-id edge-a)))
          (or (and first (next-zone first edge-b))
              (let ((second (next-zone zone-id edge-b)))
                (and second (next-zone second edge-a)))))))))

(defun world-preview-zone-for-edge (world edge)
  ;; Return cached preview zone data for EDGE if available.
  (let* ((cache (world-zone-preview-cache world))
         (exit (and cache (world-edge-exit world edge)))
         (target-id (and exit (getf exit :to))))
    (and target-id (gethash target-id cache))))

(defun world-preview-zone-for-corner (world edge-a edge-b)
  ;; Return cached preview zone data for EDGE-A + EDGE-B if available.
  (let* ((cache (world-zone-preview-cache world))
         (target-id (and cache (world-diagonal-zone-id world edge-a edge-b))))
    (and target-id (gethash target-id cache))))

(defun ensure-preview-zone-for-edge (world edge &optional zone-lru-cache)
  "Load adjacent zone data for preview rendering on EDGE.
   Step 10: Consults LRU zone cache before loading from disk."
  (let* ((graph (world-world-graph world))
         (cache (world-zone-preview-cache world))
         (exit (and graph cache (world-edge-exit world edge)))
         (target-id (and exit (spatial-exit-p exit) (getf exit :to))))
    (when (and graph cache target-id (not (gethash target-id cache)))
      ;; Step 10: Check LRU cache first
      (let ((cached-zone (and zone-lru-cache (zone-cache-lookup zone-lru-cache target-id))))
        (if cached-zone
            (progn
              (when *verbose-zone-transitions*
                (log-zone "Preview cache HIT (LRU) for ~a on edge" target-id))
              (setf (gethash target-id cache) cached-zone))
            ;; Load from disk and insert into both caches
            (let ((path (world-graph-zone-path graph target-id)))
              (when (and path (probe-file path))
                (log-zone "Preview cache MISS for ~a — loading from disk" target-id)
                (let ((zone (load-zone path)))
                  (when zone
                    (setf (gethash target-id cache) zone)
                    ;; Also insert into LRU cache
                    (when zone-lru-cache
                      (zone-cache-insert zone-lru-cache target-id zone)))))))))))

(defun ensure-preview-zone-for-corner (world edge-a edge-b &optional zone-lru-cache)
  "Load diagonal zone data for preview rendering on EDGE-A + EDGE-B.
   Step 10: Consults LRU zone cache before loading from disk."
  (let* ((cache (world-zone-preview-cache world))
         (target-id (and cache (world-diagonal-zone-id world edge-a edge-b))))
    (when (and cache target-id (not (gethash target-id cache)))
      ;; Step 10: Check LRU cache first
      (let ((cached-zone (and zone-lru-cache (zone-cache-lookup zone-lru-cache target-id))))
        (if cached-zone
            (progn
              (when *verbose-zone-transitions*
                (log-zone "Preview corner cache HIT (LRU) for ~a+~a -> ~a" edge-a edge-b target-id))
              (setf (gethash target-id cache) cached-zone))
            (let* ((graph (world-world-graph world))
                   (path (and graph (world-graph-zone-path graph target-id))))
              (when (and path (probe-file path))
                (log-zone "Preview corner cache MISS for ~a+~a -> ~a — loading from disk" edge-a edge-b target-id)
                (let ((zone (load-zone path)))
                  (when zone
                    (setf (gethash target-id cache) zone)
                    (when zone-lru-cache
                      (zone-cache-insert zone-lru-cache target-id zone)))))))))))


(defun cleanup-stale-preview-zones (world ex-west ex-east ex-north ex-south)
  "Remove preview cache entries for edges no longer visible.
   Step 10: The underlying zone data stays in the LRU cache (may be evicted later)."
  (let ((cache (world-zone-preview-cache world)))
    (when cache
      (let ((graph (world-world-graph world))
            (zone (world-zone world)))
        (when (and graph zone)
          (let ((zone-id (zone-id zone)))
            ;; Clean cardinal preview entries
            (dolist (exit-spec (world-graph-exits graph zone-id))
              (when (spatial-exit-p exit-spec)
                (let* ((edge (getf exit-spec :edge))
                       (target-id (getf exit-spec :to))
                       (visible (case edge
                                  (:west ex-west)
                                  (:east ex-east)
                                  (:north ex-north)
                                  (:south ex-south))))
                  (when (and (not visible) target-id (gethash target-id cache))
                    (remhash target-id cache)))))
            ;; Clean diagonal preview entries -- remove when either edge is no longer visible
            (dolist (pair '((:west :north) (:east :north) (:west :south) (:east :south)))
              (let ((diag-id (world-diagonal-zone-id world (first pair) (second pair))))
                (when (and diag-id (gethash diag-id cache))
                  (let ((vis-a (case (first pair) (:west ex-west) (:east ex-east)))
                        (vis-b (case (second pair) (:north ex-north) (:south ex-south))))
                    (unless (and vis-a vis-b)
                      (remhash diag-id cache))))))))))))


(defun ensure-preview-zones (world player camera editor &optional zone-lru-cache)
  "Load adjacent zone data when the camera view reaches a world edge.
   Step 10: Passes LRU cache to avoid duplicate loads."
  (multiple-value-bind (view-left view-right view-top view-bottom)
      (camera-view-bounds camera player editor)
    (let* ((ex-west (view-exceeds-edge-p world view-left view-right view-top view-bottom :west))
           (ex-east (view-exceeds-edge-p world view-left view-right view-top view-bottom :east))
           (ex-north (view-exceeds-edge-p world view-left view-right view-top view-bottom :north))
           (ex-south (view-exceeds-edge-p world view-left view-right view-top view-bottom :south)))
      (when ex-west
        (ensure-preview-zone-for-edge world :west zone-lru-cache))
      (when ex-east
        (ensure-preview-zone-for-edge world :east zone-lru-cache))
      (when ex-north
        (ensure-preview-zone-for-edge world :north zone-lru-cache))
      (when ex-south
        (ensure-preview-zone-for-edge world :south zone-lru-cache))
      (when (and ex-west ex-north)
        (ensure-preview-zone-for-corner world :west :north zone-lru-cache))
      (when (and ex-east ex-north)
        (ensure-preview-zone-for-corner world :east :north zone-lru-cache))
      (when (and ex-west ex-south)
        (ensure-preview-zone-for-corner world :west :south zone-lru-cache))
      (when (and ex-east ex-south)
        (ensure-preview-zone-for-corner world :east :south zone-lru-cache))
      ;; Step 10: Remove stale preview entries
      (cleanup-stale-preview-zones world ex-west ex-east ex-north ex-south))))

(defun world-edge-exit (world edge)
  ;; Return the exit spec for EDGE in the current zone.
  (let* ((zone (world-zone world))
         (zone-id (and zone (zone-id zone)))
         (graph (world-world-graph world)))
    (when (and edge zone-id graph)
      (find edge (world-graph-exits graph zone-id)
            :key (lambda (exit) (getf exit :edge))
            :test #'eq))))

(defun edge-spawn-position-bounds (min-x max-x min-y max-y spawn-edge preserve-axis ratio)
  ;; Return spawn coordinates for a target edge and optional offset.
  (let* ((spawn-x (case spawn-edge
                    (:west min-x)
                    (:east max-x)
                    (t nil)))
         (spawn-y (case spawn-edge
                    (:north min-y)
                    (:south max-y)
                    (t nil))))
    (when (eq preserve-axis :x)
      (setf spawn-x (edge-preserve-position min-x max-x ratio)))
    (when (eq preserve-axis :y)
      (setf spawn-y (edge-preserve-position min-y max-y ratio)))
    (when (null spawn-x)
      (setf spawn-x (edge-preserve-position min-x max-x 0.5)))
    (when (null spawn-y)
      (setf spawn-y (edge-preserve-position min-y max-y 0.5)))
    (values spawn-x spawn-y)))

(defun edge-spawn-position (world spawn-edge preserve-axis ratio)
  ;; Return spawn coordinates for a target edge and optional offset.
  (edge-spawn-position-bounds (world-wall-min-x world)
                              (world-wall-max-x world)
                              (world-wall-min-y world)
                              (world-wall-max-y world)
                              spawn-edge preserve-axis ratio))

(defun npc-transition-range-sq (npc world)
  ;; Return squared perception range for transition decisions.
  (let* ((archetype (npc-archetype npc))
         (tiles (if archetype
                    (npc-archetype-perception-tiles archetype)
                    0.0))
         (range (* tiles (world-tile-dest-size world))))
    (* range range)))

(defun npc-transition-candidate-p (npc player world)
  ;; Return true when an NPC should follow across a zone edge.
  (when (npc-alive npc)
    (let* ((dx (- (player-x player) (npc-x npc)))
           (dy (- (player-y player) (npc-y npc)))
           (dist-sq (+ (* dx dx) (* dy dy)))
           (range-sq (npc-transition-range-sq npc world)))
      (and (> range-sq 0.0)
           (<= dist-sq range-sq)
           (or (npc-provoked npc)
               (member (npc-behavior-state npc)
                       '(:aggressive :retaliate :flee)))))))

(defun collect-transition-npcs (npcs player world)
  ;; Collect NPCs that should carry across zone transitions.
  (let ((entries nil)
        (px (player-x player))
        (py (player-y player)))
    (loop :for npc :across npcs
          :when (npc-transition-candidate-p npc player world)
          :do (push (list npc (- (npc-x npc) px) (- (npc-y npc) py)) entries))
    (nreverse entries)))

(defun build-carry-npc-table (entries)
  ;; Build a lookup table for NPCs carried across a zone transition.
  (let ((table (make-hash-table :test 'eq :size 32)))
    (dolist (entry entries)
      (setf (gethash (first entry) table) t))
    table))

(defun cache-zone-npcs (zone-id npcs carried-table)
  "Cache NPCs for ZONE-ID in zone-state, excluding those in CARRIED-TABLE.
   Also populates the zone's NPC spatial grid."
  (when zone-id
    (let ((zone-state (get-zone-state zone-id)))
      (when zone-state
        (if (or (null carried-table) (null npcs))
            (progn
              (setf (zone-state-npcs zone-state) npcs)
              (populate-npc-grid zone-state npcs))
            (let ((kept 0))
              (loop :for npc :across npcs
                    :unless (gethash npc carried-table)
                      :do (incf kept))
              (let ((stored (make-array kept))
                    (index 0))
                (loop :for npc :across npcs
                      :unless (gethash npc carried-table)
                        :do (setf (aref stored index) npc)
                            (incf index))
                (setf (zone-state-npcs zone-state) stored)
                (populate-npc-grid zone-state stored))))))))

(defun cached-zone-npcs (zone-id)
  ;; Return cached NPCs for ZONE-ID from zone-state, if any.
  (when zone-id
    (let ((zone-state (get-zone-state zone-id)))
      (and zone-state (zone-state-npcs zone-state)))))

(defun reposition-transition-npcs (entries player world)
  ;; Reposition carried NPCs around the player's new spawn.
  (let ((moved nil))
    (dolist (entry entries)
      (destructuring-bind (npc dx dy) entry
        (let ((target-x (+ (player-x player) dx))
              (target-y (+ (player-y player) dy)))
          (multiple-value-bind (half-w half-h)
              (npc-collision-half world)
            (multiple-value-bind (nx ny)
                (world-open-position-for world target-x target-y half-w half-h)
              (setf (npc-x npc) nx
                    (npc-y npc) ny
                    (npc-home-x npc) nx
                    (npc-home-y npc) ny
                    (npc-wander-x npc) nx
                    (npc-wander-y npc) ny)
              (reset-frame-intent (npc-intent npc))
              (push npc moved))))))
    (nreverse moved)))

(defun merge-npc-vectors (base extras)
  ;; Append EXTRAS to BASE and return a new NPC array.
  (if (null extras)
      base
      (let* ((base-count (length base))
             (extra-count (length extras))
             (result (make-array (+ base-count extra-count))))
        (replace result base)
        (loop :for npc :in extras
              :for i :from base-count
              :do (setf (aref result i) npc))
        result)))

;;; Seam translation for continuous zone transitions (PLAN_zone_transition_continuity)

(defun seam-translate-position (edge player-x player-y
                                src-min-x src-max-x src-min-y src-max-y
                                dst-min-x dst-max-x dst-min-y dst-max-y)
  "Translate player position across a zone seam using collision bounds.
   EDGE is the direction the player crossed (east/west/north/south).
   SRC-MIN/MAX and DST-MIN/MAX are collision bounds for source and destination zones.
   Maps the player's overstep distance from the source edge to the destination opposite edge.
   Returns (values new-x new-y)."
  (declare (type single-float player-x player-y
                 src-min-x src-max-x src-min-y src-max-y
                 dst-min-x dst-max-x dst-min-y dst-max-y))
  (case edge
    ;; East: player past src-max-x → appears at dst-min-x + overstep
    (:east  (values (+ dst-min-x (- player-x src-max-x)) player-y))
    ;; West: player past src-min-x → appears at dst-max-x - overstep
    (:west  (values (+ dst-max-x (- player-x src-min-x)) player-y))
    ;; North: player past src-min-y → appears at dst-max-y + overstep
    (:north (values player-x (+ dst-max-y (- player-y src-min-y))))
    ;; South: player past src-max-y → appears at dst-min-y + overstep
    (:south (values player-x (+ dst-min-y (- player-y src-max-y))))
    (t      (values player-x player-y))))

(defun seam-position-valid-p (x y min-x max-x min-y max-y)
  "Check if a seam-translated position is within destination zone bounds."
  (declare (type single-float x y min-x max-x min-y max-y))
  (and (>= x min-x) (<= x max-x)
       (>= y min-y) (<= y max-y)))

;;; ADDENDUM 1: Overstep preservation for continuous zone transitions

(defun compute-transition-overstep (edge player src-min-x src-max-x src-min-y src-max-y)
  "Compute the distance between the player and the source zone edge being crossed.
   Returns a non-negative float: how far the player is from the exact edge boundary.
   EDGE is the direction the player is transitioning toward."
  (declare (type single-float src-min-x src-max-x src-min-y src-max-y))
  (let ((px (player-x player))
        (py (player-y player)))
    (max 0.0
         (case edge
           (:north (- py src-min-y))    ; distance from north (top) edge
           (:south (- src-max-y py))    ; distance from south (bottom) edge
           (:west  (- px src-min-x))    ; distance from west (left) edge
           (:east  (- src-max-x px))    ; distance from east (right) edge
           (t 0.0)))))

(defun apply-overstep-to-spawn (spawn-edge overstep base-x base-y)
  "Apply overstep offset to base spawn position for continuous world-space transition.
   SPAWN-EDGE is the edge in the destination zone where the player spawns.
   OVERSTEP is subtracted from the spawn edge position, pushing the player
   inward from the destination edge by the same amount they were from the source edge."
  (declare (type single-float overstep base-x base-y))
  (case spawn-edge
    (:north (values base-x (+ base-y overstep)))   ; push down from north edge
    (:south (values base-x (- base-y overstep)))   ; push up from south edge
    (:west  (values (+ base-x overstep) base-y))   ; push right from west edge
    (:east  (values (- base-x overstep) base-y))   ; push left from east edge
    (t (values base-x base-y))))

(defun transition-zone (game player exit edge)
  ;; Apply a zone transition using EXIT metadata for the given PLAYER.
  ;; Updates player's zone-id and position. Also updates zone-state cache.
  ;; Uses player's current zone-id (not world-zone) for per-zone bounds.
  (let* ((world (game-world game))
         (intent (player-intent player))
         ;; Use player's zone-id for source zone, not world-zone (server may have stale world-zone)
         (current-zone-id (or (player-zone-id player) *starting-zone-id*))
         (current-npcs (game-npcs game))
         (graph (world-world-graph world))
         (target-id (getf exit :to))
         (target-path (and graph (world-graph-zone-path graph target-id)))
         (had-target (intent-target-active intent))
         (target-offset-x (when had-target
                            (- (intent-target-x intent) (player-x player))))
         (target-offset-y (when had-target
                            (- (intent-target-y intent) (player-y player))))
         (carry (collect-transition-npcs current-npcs player world))
         (carry-table (and carry (build-carry-npc-table carry))))
    (when (and target-path (probe-file target-path))
      ;; Cache current zone's NPCs in zone-state (excluding carried NPCs)
      (cache-zone-npcs current-zone-id current-npcs carry-table)
      ;; Check LRU zone cache first to avoid synchronous disk load on main thread.
      ;; Preloading (Step 5) should have warmed the cache, but best-effort fallback.
      (let* ((zone-lru (game-zone-cache game))
             (cached-zone (and zone-lru (zone-cache-lookup zone-lru target-id)))
             (zone (or cached-zone
                       (progn
                         (when *verbose-zone-transitions*
                           (log-zone "Zone transition: cache MISS for ~a — synchronous disk load" target-id))
                         (with-retry-exponential (loaded (lambda () (load-zone target-path))
                                                   :max-retries 2
                                                   :initial-delay 100
                                                   :max-delay 200
                                                   :on-final-fail (lambda (e)
                                                                    (warn "Zone transition failed: could not load zone ~a after retries: ~a"
                                                                          target-id e)))
                           loaded))))
             (spawn-edge (or (getf exit :spawn-edge)
                             (getf exit :to-edge)
                             (edge-opposite edge)))
             (offset (getf exit :offset))
             (preserve-axis (edge-preserve-axis edge offset))
             ;; Source zone bounds (used for ratio AND overstep computation)
             (tile-size-for-ratio (world-tile-dest-size world))
             (half-w-for-ratio (world-collision-half-width world))
             (half-h-for-ratio (world-collision-half-height world)))
        (when zone
          ;; Compute source zone collision bounds (for ratio and overstep)
          (multiple-value-bind (src-min-x src-max-x src-min-y src-max-y)
              (if (get-zone-wall-map current-zone-id)
                  (get-zone-collision-bounds current-zone-id
                                              tile-size-for-ratio
                                              half-w-for-ratio
                                              half-h-for-ratio)
                  (values (world-wall-min-x world) (world-wall-max-x world)
                          (world-wall-min-y world) (world-wall-max-y world)))
            (let ((ratio (if preserve-axis
                             (if (eq preserve-axis :x)
                                 (edge-offset-ratio src-min-x src-max-x (player-x player))
                                 (edge-offset-ratio src-min-y src-max-y (player-y player)))
                             0.5)))
          (log-zone "Zone transition: ~a -> ~a edge=~a spawn-edge=~a"
                    current-zone-id
                    (zone-id zone)
                    edge spawn-edge)
          ;; Only set *zone-path* and apply-zone-to-world for client/local mode
          ;; Server uses per-zone collision from zone-state instead
          (let* ((is-server (eq (game-net-role game) :server))
                 (target-zone-id (zone-id zone))
                 ;; Ensure zone-state exists for the target zone
                 (target-zone-state (get-or-create-zone-state target-zone-id target-path))
                 (target-wall-map (when target-zone-state (zone-state-wall-map target-zone-state)))
                 (tile-size (world-tile-dest-size world))
                 (half-w (world-collision-half-width world))
                 (half-h (world-collision-half-height world)))
            (unless is-server
              (setf *zone-path* target-path)
              (apply-zone-to-world world zone)
              ;; Call client hook to clear render caches (set by rendering.lisp)
              (when *client-zone-change-hook*
                (funcall *client-zone-change-hook* target-zone-id)))
            (setf (world-zone-label world) (zone-label zone))
            ;; Calculate spawn position using appropriate bounds
            (multiple-value-bind (new-min-x new-max-x new-min-y new-max-y)
                (if (and is-server target-wall-map)
                    (get-zone-collision-bounds target-zone-id tile-size half-w half-h)
                    (values (world-wall-min-x world) (world-wall-max-x world)
                            (world-wall-min-y world) (world-wall-max-y world)))
              ;; PLAN_zone_transition_continuity: Seam translation as primary path
              ;; Uses collision bounds (not pixel spans) so translated positions
              ;; land within walkable area without multi-tile clamping.
              (let* ((px (player-x player))
                     (py (player-y player)))
                ;; Step 1: Try seam translation (primary path)
                (multiple-value-bind (trans-x trans-y)
                    (seam-translate-position edge px py
                                            src-min-x src-max-x src-min-y src-max-y
                                            new-min-x new-max-x new-min-y new-max-y)
                  ;; Step 2: Check if translated position is in bounds and not blocked
                  (let* ((in-bounds (seam-position-valid-p trans-x trans-y
                                                          new-min-x new-max-x
                                                          new-min-y new-max-y))
                         (blocked (and in-bounds
                                       (if (and is-server target-wall-map)
                                           (blocked-at-p-with-map target-wall-map
                                                                  trans-x trans-y
                                                                  half-w half-h tile-size)
                                           nil)))
                         (seam-valid (and in-bounds (not blocked))))
                    (when *verbose-zone-transitions*
                      (log-zone "Seam translation: ~a->~a edge=~a path=~a old=(~,1f,~,1f) src-bounds=(~,1f,~,1f,~,1f,~,1f) dst-bounds=(~,1f,~,1f,~,1f,~,1f) -> trans=(~,1f,~,1f) in-bounds=~a blocked=~a overstep=not-applied"
                                current-zone-id (zone-id zone) edge
                                (if seam-valid "seam" "fallback")
                                px py
                                src-min-x src-max-x src-min-y src-max-y
                                new-min-x new-max-x new-min-y new-max-y
                                trans-x trans-y in-bounds blocked))
                    ;; Step 3: Use seam result if valid, otherwise fall back to ratio-spawn
                    (multiple-value-bind (raw-x raw-y fallback-reason)
                        (if seam-valid
                            (values trans-x trans-y nil)
                            ;; Fallback: ratio-based spawn + overstep (old path)
                            (let* ((reason (if (not in-bounds) "out-of-bounds" "blocked"))
                                   (overstep (compute-transition-overstep
                                              edge player
                                              src-min-x src-max-x src-min-y src-max-y)))
                              (multiple-value-bind (base-x base-y)
                                  (edge-spawn-position-bounds new-min-x new-max-x new-min-y new-max-y
                                                              spawn-edge preserve-axis ratio)
                                (multiple-value-bind (fb-x fb-y)
                                    (apply-overstep-to-spawn spawn-edge overstep base-x base-y)
                                  (when *verbose-zone-transitions*
                                    (log-zone "Seam fallback: reason=~a overstep=~,2f spawn-edge=~a base=(~,1f,~,1f) result=(~,1f,~,1f)"
                                              reason overstep spawn-edge base-x base-y fb-x fb-y))
                                  (values fb-x fb-y reason)))))
                (multiple-value-bind (spawn-x spawn-y)
                    (if (and is-server target-wall-map)
                        (find-open-position-with-map target-wall-map raw-x raw-y
                                                     half-w half-h tile-size)
                        (world-open-position-for world raw-x raw-y half-w half-h))
                  (when *verbose-zone-transitions*
                    (log-zone "Seam final: path=~a raw=(~,1f,~,1f) spawn=(~,1f,~,1f)~@[ fallback-reason=~a~]"
                              (if fallback-reason "fallback" "seam")
                              raw-x raw-y spawn-x spawn-y fallback-reason))
                  ;; Remove player from old zone's spatial grid and zone-players cache
                  (let ((old-zone-state (get-zone-state current-zone-id)))
                    (when old-zone-state
                      ;; Remove from zone-players cache (Task 4.1)
                      (remove-player-from-zone-cache player old-zone-state)
                      ;; Remove from spatial grid
                      (let ((old-grid (zone-state-player-grid old-zone-state)))
                        (when (and old-grid
                                   (player-grid-cell-x player)
                                   (player-grid-cell-y player))
                          (spatial-grid-remove old-grid (player-id player)
                                               (player-grid-cell-x player)
                                               (player-grid-cell-y player))))))
                  (setf (player-x player) spawn-x
                        (player-y player) spawn-y
                        (player-dx player) 0.0
                        (player-dy player) 0.0
                        (player-zone-id player) target-zone-id
                        (player-snapshot-dirty player) t)
                  ;; Insert player into new zone's spatial grid and zone-players cache
                  (when target-zone-state
                    ;; Add to zone-players cache (Task 4.1)
                    (add-player-to-zone-cache player target-zone-state)
                    ;; Add to spatial grid
                    (let ((new-grid (zone-state-player-grid target-zone-state)))
                      (when new-grid
                        (multiple-value-bind (cx cy)
                            (position-to-cell spawn-x spawn-y (spatial-grid-cell-size new-grid))
                          (spatial-grid-insert new-grid (player-id player) spawn-x spawn-y)
                          (setf (player-grid-cell-x player) cx
                                (player-grid-cell-y player) cy)))))
                  ;; Step 11: Downgrade to Tier-2 (dirty flag) — flushes within 30s checkpoint.
                  ;; Zone transitions are not critical enough for Tier-1 blocking save.
                  ;; Crash between transition and checkpoint → player reverts to pre-transition zone (acceptable).
                  (mark-player-dirty (player-id player))))))))) ; close mvb-spawn, mvb-raw, let*-seam, mvb-trans, let*-src, mvb-new-min
          ;; Step 1: Set transition cooldown
          (setf (player-zone-transition-cooldown player)
                *zone-transition-cooldown-seconds*)
          ;; Step 2: Clear pending transition
          (setf (player-zone-transition-pending player) nil)
          (reset-frame-intent intent)
          (when had-target
            (set-intent-target intent
                               (+ (player-x player) target-offset-x)
                               (+ (player-y player) target-offset-y)))
          (setf (player-attacking player) nil
                (player-attack-hit player) nil
                    (player-attack-timer player) 0.0)
          (setf (world-minimap-spawns world)
                (build-adjacent-minimap-spawns world player))
          (let* ((players (game-players game))
                 (target-zone-id (and zone (zone-id zone)))
                 (cached (cached-zone-npcs target-zone-id))
                 (npcs (or cached
                           (make-npcs player world
                                      :id-source (game-npc-id-source game))))
                 (carried (reposition-transition-npcs carry player world)))
            ;; Update session zone-id for persistence
            (update-player-session-zone (player-id player) target-zone-id)
            (ensure-npcs-open-spawn npcs world)
            (let ((merged (merge-npc-vectors npcs carried)))
              (setf (game-npcs game) merged
                    (game-entities game) (make-entities players merged))
              ;; Update zone-state cache with NPCs for zone-filtered snapshots
              (when target-zone-id
                ;; Phase 2 perf: Use array-backed spatial grids
                (let* ((tile-dest-size (* (float *tile-size* 1.0) *tile-scale*))
                       (zone-w (or (zone-width zone) 64))
                       (zone-h (or (zone-height zone) 64))
                       (target-state (or (get-zone-state target-zone-id)
                                         (make-zone-state
                                          :zone-id target-zone-id
                                          :zone zone
                                          :wall-map (zone-wall-map zone)
                                          :objects (zone-objects zone)
                                          :player-grid (make-spatial-grid-for-zone zone-w zone-h tile-dest-size)
                                          :npc-grid (make-spatial-grid-for-zone zone-w zone-h tile-dest-size)))))
                  (setf (zone-state-npcs target-state) merged)
                  ;; Populate NPC spatial grid for proximity queries
                  (populate-npc-grid target-state merged)
                  (setf (gethash target-zone-id *zone-states*) target-state)))))
          t)))))))

(defun update-zone-transition (game &optional (dt *sim-tick-seconds*))
  "Handle edge-based zone transitions with cooldown, hysteresis, and directional gating.
   Each player is checked against their own zone's exits using per-zone collision bounds.
   Two-phase arm/commit/cancel model:
   - Arm: player crosses arm line toward edge → set pending (no transition yet)
   - Commit: player with pending reaches zone edge → transition fires
   - Cancel: player retreats past cancel line → pending cleared
   Returns count of players that transitioned this frame."
  ;; Config invariant: cancel line must be further from edge than arm line
  (assert (> *zone-hysteresis-out* *zone-hysteresis-in*)
          (*zone-hysteresis-out* *zone-hysteresis-in*)
          "Zone config invariant violated: *zone-hysteresis-out* (~a) must be > *zone-hysteresis-in* (~a)")
  (let* ((world (game-world game))
         (players (game-players game))
         (transition-count 0))
    (when (and world players (> (length players) 0))
      (loop :for player :across players
            :when player
            :do (block per-player
                  (let* ((player-zone-id (or (player-zone-id player) *starting-zone-id*))
                         (zone-path (zone-path-for-id world player-zone-id))
                         (_zone-state (when zone-path
                                        (get-or-create-zone-state player-zone-id zone-path)))
                         (tile-size (world-tile-dest-size world))
                         (half-w (world-collision-half-width world))
                         (half-h (world-collision-half-height world))
                         ;; ADDENDUM 2: Derive commit margin from collision size
                         ;; so the player doesn't need to push through collision.
                         ;; Use max(collision-half-w, collision-half-h) as the base,
                         ;; scaled by *zone-commit-margin-tiles* as a multiplier.
                         (commit-margin (max (* *zone-commit-margin-tiles* tile-size)
                                             (max half-w half-h))))
                    (declare (ignore _zone-state))
                    ;; Step 1: Cooldown — decrement and skip if active
                    (when (> (player-zone-transition-cooldown player) 0.0)
                      (decf (player-zone-transition-cooldown player) dt)
                      (when (> (player-zone-transition-cooldown player) 0.0)
                        (when *verbose-zone-transitions*
                          (log-zone "Zone transition: cooldown active (~,2fs remaining) for player ~a"
                                   (player-zone-transition-cooldown player) (player-id player)))
                        (return-from per-player)))
                    ;; Get per-zone bounds
                    (multiple-value-bind (min-x max-x min-y max-y)
                        (if (and player-zone-id (get-zone-wall-map player-zone-id))
                            (get-zone-collision-bounds player-zone-id tile-size half-w half-h)
                            (values (world-wall-min-x world) (world-wall-max-x world)
                                    (world-wall-min-y world) (world-wall-max-y world)))
                      (when (and min-x max-x min-y max-y)
                        (let ((pending (player-zone-transition-pending player)))
                          (cond
                            ;; === PENDING SET: evaluate commit/cancel ===
                            (pending
                             (let ((exit (world-edge-exit-for-zone world player-zone-id pending)))
                               (cond
                                 ;; Cancel: retreated past cancel line
                                 ((player-past-cancel-line-p player pending min-x max-x min-y max-y tile-size)
                                  (when *verbose-zone-transitions*
                                    (log-zone "Zone transition: cancel for edge ~a (player ~a retreated past cancel line)"
                                              pending (player-id player)))
                                  (setf (player-zone-transition-pending player) nil))
                                 ;; Cancel: no exit for this edge
                                 ((null exit)
                                  (setf (player-zone-transition-pending player) nil))
                                 ;; Cancel: intent dropped below direction threshold
                                 ((multiple-value-bind (dx dy) (player-intent-direction player)
                                    (not (edge-direction-passes-p dx dy pending)))
                                  (when *verbose-zone-transitions*
                                    (log-zone "Zone transition: cancel for edge ~a (intent dropped)" pending))
                                  (setf (player-zone-transition-pending player) nil))
                                 ;; Commit or edge-change: check actual edge at boundary
                                 ;; commit-margin relaxes the boundary so collision doesn't
                                 ;; prevent reaching the exact edge position
                                 (t
                                  (let ((actual-edge (world-exit-edge-with-bounds
                                                      player min-x max-x min-y max-y
                                                      commit-margin)))
                                    (cond
                                      ;; Commit: at the pending edge
                                      ((eq actual-edge pending)
                                       (let ((now (/ (float (get-internal-real-time) 1.0)
                                                     (float internal-time-units-per-second 1.0))))
                                         (let* ((last (player-zone-transition-last-time player))
                                                (time-since (if (> last 0.0) (- now last) -1.0)))
                                           (log-zone "Zone transition: COMMIT edge=~a player=~a commit-margin=~,1f time-since-last=~,2fs"
                                                     pending (player-id player) commit-margin time-since))
                                         (setf (player-zone-transition-last-time player) now))
                                       (transition-zone game player exit pending)
                                       (incf transition-count))
                                      ;; Edge changed: player now at a different edge → clear pending
                                      (actual-edge
                                       (when *verbose-zone-transitions*
                                         (log-zone "Zone transition: cancel ~a (edge changed to ~a)"
                                                   pending actual-edge))
                                       (setf (player-zone-transition-pending player) nil))
                                      ;; No edge detected: still in arm band, waiting
                                      (t nil)))))))
                            ;; === NO PENDING: check for arm ===
                            (t
                             ;; Pick the edge with strongest directional alignment
                             ;; (dominant direction), not first match from dolist.
                             (let ((graph (world-world-graph world)))
                               (when graph
                                 (multiple-value-bind (dx dy) (player-intent-direction player)
                                   (let ((best-edge nil)
                                         (best-dot 0.0))
                                     (declare (type single-float best-dot))
                                     (dolist (exit-spec (world-graph-exits graph player-zone-id))
                                       (let ((edge (getf exit-spec :edge)))
                                         (when (and edge
                                                    (player-in-arm-band-p player edge
                                                                          min-x max-x min-y max-y tile-size)
                                                    (edge-direction-passes-p dx dy edge))
                                           ;; Rank by dot product with edge normal
                                           (let ((dot (edge-direction-dot dx dy edge)))
                                             (when (> dot best-dot)
                                               (setf best-edge edge
                                                     best-dot dot))))))
                                     (when best-edge
                                       (when *verbose-zone-transitions*
                                         (log-zone "Zone transition: ARM edge ~a for player ~a (dot=~,2f)"
                                                   best-edge (player-id player) best-dot))
                                       (setf (player-zone-transition-pending player) best-edge)))))))))))))))
    transition-count))

(defun log-player-position (player world)
  ;; Emit verbose position and tile diagnostics for debugging.
  (let* ((x (player-x player))
         (y (player-y player))
         (tile-dest-size (world-tile-dest-size world))
         (tile-x (floor x tile-dest-size))
         (tile-y (floor y tile-dest-size))
         (feet-x x)
         (feet-y (+ y (world-collision-half-height world)))
         (feet-tile-x (floor feet-x tile-dest-size))
         (feet-tile-y (floor feet-y tile-dest-size)))
    (format t "~&pos=~,2f,~,2f center=~,2f,~,2f tile=~d,~d feet=~,2f,~,2f tile-feet=~d,~d~%"
            x y
            x y
            tile-x tile-y
            feet-x feet-y
            feet-tile-x feet-tile-y)
    (finish-output)))

;;;; Player Unstuck System

(defun player-is-stuck-p (player world)
  ;; Return T if player cannot move in any cardinal direction.
  ;; Tests small movements in all 4 directions against collision.
  (let* ((px (player-x player))
         (py (player-y player))
         (test-dist 2.0)
         (half-w (world-collision-half-width world))
         (half-h (world-collision-half-height world)))
    (and (position-blocked-p world (+ px test-dist) py half-w half-h)
         (position-blocked-p world (- px test-dist) py half-w half-h)
         (position-blocked-p world px (+ py test-dist) half-w half-h)
         (position-blocked-p world px (- py test-dist) half-w half-h))))

(defun player-is-stuck-p-for-zone (player zone-id world)
  "Return T if player cannot move in any cardinal direction.
   Uses zone-state wall-map for collision, falling back to world if zone not loaded."
  (let* ((px (player-x player))
         (py (player-y player))
         (test-dist 2.0)
         (half-w (world-collision-half-width world))
         (half-h (world-collision-half-height world))
         (tile-size (world-tile-dest-size world))
         (wall-map (get-zone-wall-map zone-id)))
    (if wall-map
        ;; Use zone-specific wall-map
        (and (blocked-at-p-with-map wall-map (+ px test-dist) py half-w half-h tile-size)
             (blocked-at-p-with-map wall-map (- px test-dist) py half-w half-h tile-size)
             (blocked-at-p-with-map wall-map px (+ py test-dist) half-w half-h tile-size)
             (blocked-at-p-with-map wall-map px (- py test-dist) half-w half-h tile-size))
        ;; Fallback to global world collision
        (player-is-stuck-p player world))))

(defun get-zone-safe-spawn (world)
  ;; Return a random position within the zone's playable bounds.
  ;; If the random spot is blocked, world-open-position will find the nearest open tile.
  (let* ((min-x (world-wall-min-x world))
         (max-x (world-wall-max-x world))
         (min-y (world-wall-min-y world))
         (max-y (world-wall-max-y world))
         (rand-x (+ min-x (random (max 1.0 (- max-x min-x)))))
         (rand-y (+ min-y (random (max 1.0 (- max-y min-y))))))
    (values rand-x rand-y)))

(defun get-zone-safe-spawn-for-zone (zone-id world)
  "Return a random position within ZONE-ID's playable bounds.
   Falls back to global world bounds if zone not loaded."
  (let* ((tile-size (world-tile-dest-size world))
         (half-w (world-collision-half-width world))
         (half-h (world-collision-half-height world)))
    (multiple-value-bind (min-x max-x min-y max-y)
        (get-zone-collision-bounds zone-id tile-size half-w half-h)
      (if min-x
          ;; Use zone-specific bounds
          (let ((rand-x (+ min-x (random (max 1.0 (- max-x min-x)))))
                (rand-y (+ min-y (random (max 1.0 (- max-y min-y))))))
            (values rand-x rand-y))
          ;; Fallback to global world bounds
          (get-zone-safe-spawn world)))))

(defun process-player-unstuck (player intent world zone-id &optional event-queue)
  ;; Handle client unstuck request (server authority).
  ;; Uses player's zone-id for per-zone collision checking.
  ;; NOTE: Player's own zone-id takes precedence over the passed zone-id parameter
  ;; to ensure multi-zone correctness when global world-zone differs.
  (when (intent-requested-unstuck intent)
    (let* ((player-zone (or (player-zone-id player) zone-id *starting-zone-id*))
           (wall-map (get-zone-wall-map player-zone))
           (tile-size (world-tile-dest-size world))
           (half-w (world-collision-half-width world))
           (half-h (world-collision-half-height world)))
      (if (player-is-stuck-p-for-zone player player-zone world)
          ;; Player is stuck - teleport to random position in their zone
          (multiple-value-bind (safe-x safe-y)
              (get-zone-safe-spawn-for-zone player-zone world)
            (multiple-value-bind (final-x final-y)
                (if wall-map
                    (find-open-position-with-map wall-map safe-x safe-y
                                                 half-w half-h tile-size)
                    (world-open-position world safe-x safe-y))
              (setf (player-x player) final-x
                    (player-y player) final-y
                    (player-dx player) 0.0
                    (player-dy player) 0.0
                    (player-snapshot-dirty player) t
                    ;; Force full resync so client gets complete snapshot after teleport
                    ;; This prevents frozen sprites from stale interpolation buffers
                    (player-force-full-resync player) t)
              (mark-player-dirty (player-id player))
              (log-verbose "Player ~a unstuck in zone ~a, teleported to (~,1f, ~,1f)"
                           (player-id player) player-zone final-x final-y)
              (when event-queue
                (emit-hud-message-event event-queue "Teleported to safe location."))))
          ;; Player not stuck - deny free teleport
          (progn
            (log-verbose "Player ~a unstuck request denied - not stuck"
                         (player-id player))
            (when event-queue
              (emit-hud-message-event event-queue "You don't appear to be stuck.")))))
    (clear-requested-unstuck intent)))

(defun make-world ()
  ;; Build world state and derived collision/render constants.
  (let* ((zone (load-zone *zone-path*))
         (graph (load-world-graph))
         (tile-size-f (float *tile-size* 1.0))
         (tile-dest-size (* tile-size-f *tile-scale*))
         (floor-index *floor-tile-index*)
         (wall-map (if zone
                       (zone-wall-map zone)
                       (build-wall-map)))
         (wall-map-width (array-dimension wall-map 1))
         (wall-map-height (array-dimension wall-map 0))
         (collision-half-width (* (/ tile-dest-size 2.0) *player-collision-scale*))
         (collision-half-height (* (/ tile-dest-size 2.0) *player-collision-scale*))
         (wall-min-x (+ (* (+ *wall-origin-x* 1) tile-dest-size)
                        collision-half-width))
         (wall-max-x (- (* (+ *wall-origin-x* (1- wall-map-width))
                           tile-dest-size)
                        collision-half-width))
         (wall-min-y (+ (* (+ *wall-origin-y* 1) tile-dest-size)
                        collision-half-height))
        (wall-max-y (- (* (+ *wall-origin-y* (1- wall-map-height))
                           tile-dest-size)
                        collision-half-height)))
    (let ((world (%make-world :tile-size-f tile-size-f
                              :tile-dest-size tile-dest-size
                              :floor-index floor-index
                              :zone zone
                              :zone-label (zone-label zone)
                              :world-graph graph
                              :zone-preview-cache (make-hash-table :test 'eq :size 64)
                              :minimap-spawns nil
                              :minimap-collisions nil
                              :wall-map wall-map
                              :wall-map-width wall-map-width
                              :wall-map-height wall-map-height
                              :collision-half-width collision-half-width
                              :collision-half-height collision-half-height
                              :wall-min-x wall-min-x
                              :wall-max-x wall-max-x
                              :wall-min-y wall-min-y
                              :wall-max-y wall-max-y)))
      (log-verbose "World ready: zone=~a walls=~dx~d tile=~,2f dest=~,2f"
                   (zone-label zone)
                   wall-map-width
                   wall-map-height
                   tile-size-f
                   tile-dest-size)
      (setf (world-minimap-spawns world)
            (build-adjacent-minimap-spawns world))
      (setf (world-minimap-collisions world)
            (build-minimap-collisions world))
      ;; Register initial zone in zone-state cache for zone-filtered snapshots
      (when zone
        (let ((initial-zone-id (zone-id zone)))
          (when initial-zone-id
            ;; Phase 2 perf: Use array-backed spatial grids
            (let* ((tile-dest-size-f (* (float *tile-size* 1.0) *tile-scale*))
                   (zone-w (or (zone-width zone) 64))
                   (zone-h (or (zone-height zone) 64)))
              (setf (gethash initial-zone-id *zone-states*)
                    (make-zone-state
                     :zone-id initial-zone-id
                     :zone zone
                     :wall-map wall-map
                     :objects (zone-objects zone)
                     :npcs (vector)
                     ;; Initialize array-backed spatial grids for O(1) queries
                     :player-grid (make-spatial-grid-for-zone zone-w zone-h tile-dest-size-f)
                     :npc-grid (make-spatial-grid-for-zone zone-w zone-h tile-dest-size-f)))))))  ; NPCs populated later
      world)))

(defun apply-zone-to-world (world zone)
  ;; Replace the world's zone and rebuild wall-map-derived bounds.
  ;; NOTE: Render cache clearing is handled by *client-zone-change-hook*
  ;; which is called from client-only code paths, not here.
  (let* ((tile-dest-size (world-tile-dest-size world))
         (wall-map (if zone
                       (zone-wall-map zone)
                       (build-wall-map)))
         (wall-map-width (array-dimension wall-map 1))
         (wall-map-height (array-dimension wall-map 0))
         (collision-half-width (world-collision-half-width world))
         (collision-half-height (world-collision-half-height world))
         (wall-min-x (+ (* (+ *wall-origin-x* 1) tile-dest-size)
                        collision-half-width))
         (wall-max-x (- (* (+ *wall-origin-x* (1- wall-map-width))
                           tile-dest-size)
                        collision-half-width))
         (wall-min-y (+ (* (+ *wall-origin-y* 1) tile-dest-size)
                        collision-half-height))
         (wall-max-y (- (* (+ *wall-origin-y* (1- wall-map-height))
                           tile-dest-size)
                        collision-half-height)))
    (setf (world-zone world) zone
          (world-zone-label world) (zone-label zone)
          (world-wall-map world) wall-map
          (world-wall-map-width world) wall-map-width
          (world-wall-map-height world) wall-map-height
          (world-wall-min-x world) wall-min-x
          (world-wall-max-x world) wall-max-x
          (world-wall-min-y world) wall-min-y
          (world-wall-max-y world) wall-max-y)
    (unless (world-zone-preview-cache world)
      (setf (world-zone-preview-cache world) (make-hash-table :test 'eq :size 64)))
    (let ((graph (world-world-graph world)))
      (when graph
        (setf (world-graph-zone-paths graph)
              (build-zone-paths (resolve-zone-path *zone-root*)))))
    (setf (world-minimap-spawns world)
          (build-adjacent-minimap-spawns world))
    (setf (world-minimap-collisions world)
          (build-minimap-collisions world))
    world))
