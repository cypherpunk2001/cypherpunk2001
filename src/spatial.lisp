;;;; spatial.lisp - Spatial grid for efficient proximity queries
;;;;
;;;; Provides O(1) cell-based spatial indexing for entities. Each zone has its own
;;;; grid for players and NPCs. Entities track their current cell for O(1) updates.
;;;;
;;;; Phase 2 Performance: Array-backed grids for zones with known dimensions.
;;;; Uses direct 2D array access (O(1)) instead of hash-table with cons keys.
;;;; Scratch vectors provided for allocation-free queries.

(in-package #:mmorpg)

;;; Configuration

(defparameter *spatial-cell-size* 128.0
  "Size of spatial grid cells in world pixels. Should be >= typical entity interaction range.
   128px = 4 tiles at 32px/tile, covers melee range + margin.")

;;; Scratch Vectors for Allocation-Free Queries
;;; These are thread-local in spirit - single-threaded game loop reuses them.

(defparameter *spatial-scratch-vector*
  (make-array 512 :element-type 'fixnum :fill-pointer 0 :adjustable nil)
  "Primary scratch vector for spatial query results. Reused per query.")

(defparameter *spatial-scratch-vector-2*
  (make-array 512 :element-type 'fixnum :fill-pointer 0 :adjustable nil)
  "Secondary scratch vector for nested queries (e.g., during iteration).")

;;; Spatial Grid Structure
;;; Supports both hash-based (fallback/tests) and array-backed (zones) modes.

(defstruct (spatial-grid (:constructor %make-spatial-grid))
  "Spatial grid for efficient proximity queries.
   When CELLS-ARRAY is non-nil, uses array-backed O(1) access.
   Otherwise falls back to hash-based access."
  ;; Array-backed storage (preferred for zones with known dimensions)
  (cells-array nil :type (or null (simple-array t (* *))))
  (width 0 :type fixnum)   ; Grid width in cells
  (height 0 :type fixnum)  ; Grid height in cells
  ;; Hash-based fallback (for tests/unbounded grids)
  (cells nil :type (or null hash-table))
  ;; Common
  (cell-size *spatial-cell-size* :type single-float))

(defun make-spatial-grid (&optional (cell-size *spatial-cell-size*))
  "Create a new hash-based spatial grid with the given cell size.
   Use make-spatial-grid-for-zone for array-backed grids."
  (%make-spatial-grid :cells (make-hash-table :test 'eql :size 256)
                      :cell-size (float cell-size 1.0)))

(defun make-spatial-grid-for-zone (zone-width-tiles zone-height-tiles tile-size
                                   &optional (cell-size *spatial-cell-size*))
  "Create array-backed spatial grid sized for zone dimensions.
   ZONE-WIDTH-TILES and ZONE-HEIGHT-TILES are zone dimensions in tiles.
   TILE-SIZE is pixels per tile. CELL-SIZE is spatial grid cell size in pixels."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type fixnum zone-width-tiles zone-height-tiles)
           (type single-float tile-size cell-size))
  (let* ((zone-px-width (* zone-width-tiles tile-size))
         (zone-px-height (* zone-height-tiles tile-size))
         (grid-w (max 1 (the fixnum (ceiling zone-px-width cell-size))))
         (grid-h (max 1 (the fixnum (ceiling zone-px-height cell-size)))))
    (declare (type single-float zone-px-width zone-px-height)
             (type fixnum grid-w grid-h))
    (%make-spatial-grid
     :cells-array (make-array (list grid-w grid-h) :initial-element nil)
     :width grid-w
     :height grid-h
     :cells nil  ; No hash table needed
     :cell-size (float cell-size 1.0))))

;;; Position/Cell Conversion

(defun position-to-cell (x y cell-size)
  "Convert world position to cell coordinates. Returns (values cell-x cell-y)."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type single-float x y cell-size))
  (values (the fixnum (floor x cell-size)) (the fixnum (floor y cell-size))))

(declaim (inline cell-in-bounds-p))
(defun cell-in-bounds-p (grid cx cy)
  "Return T if cell (CX, CY) is within array-backed grid bounds."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type spatial-grid grid)
           (type fixnum cx cy))
  (and (>= cx 0) (< cx (spatial-grid-width grid))
       (>= cy 0) (< cy (spatial-grid-height grid))))

;;; Core Grid Operations - Array-Backed

(declaim (inline grid-cell-ref))
(defun grid-cell-ref (grid cx cy)
  "Get cell contents at (CX, CY) for array-backed grid. Returns list or nil."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type spatial-grid grid)
           (type fixnum cx cy))
  (aref (spatial-grid-cells-array grid) cx cy))

(declaim (inline set-grid-cell-ref))
(defun set-grid-cell-ref (grid cx cy value)
  "Set cell contents at (CX, CY) for array-backed grid."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type spatial-grid grid)
           (type fixnum cx cy))
  (setf (aref (spatial-grid-cells-array grid) cx cy) value))

(defsetf grid-cell-ref set-grid-cell-ref)

;;; Core Grid Operations - Unified Interface

(defun spatial-grid-insert (grid id x y)
  "Insert entity ID at position (X, Y). Returns (values cx cy)."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type (or null spatial-grid) grid)
           (type fixnum id)
           (type single-float x y))
  (when grid
    (let ((cell-size (spatial-grid-cell-size grid)))
      (declare (type single-float cell-size))
      (multiple-value-bind (cx cy) (position-to-cell x y cell-size)
        (declare (type fixnum cx cy))
        (if (spatial-grid-cells-array grid)
            ;; Array-backed: direct access
            (when (cell-in-bounds-p grid cx cy)
              (push id (grid-cell-ref grid cx cy)))
            ;; Hash-based fallback
            (let ((cells (spatial-grid-cells grid)))
              (when cells
                (push id (gethash (pack-cell-key cx cy) cells)))))
        (values cx cy)))))

(defun spatial-grid-remove (grid id cx cy)
  "Remove entity ID from cell (CX, CY). Returns T if found and removed."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type (or null spatial-grid) grid)
           (type fixnum id)
           (type (or null fixnum) cx cy))
  (when (and grid cx cy)
    (if (spatial-grid-cells-array grid)
        ;; Array-backed: direct access
        (when (cell-in-bounds-p grid cx cy)
          (let* ((cell-list (grid-cell-ref grid cx cy))
                 (new-list (delete id cell-list :count 1)))
            (set-grid-cell-ref grid cx cy new-list)
            t))
        ;; Hash-based fallback
        (let ((cells (spatial-grid-cells grid)))
          (when cells
            (let* ((key (pack-cell-key cx cy))
                   (cell-list (gethash key cells)))
              (when cell-list
                (let ((new-list (delete id cell-list :count 1)))
                  (if new-list
                      (setf (gethash key cells) new-list)
                      (remhash key cells))
                  t))))))))

(defun spatial-grid-move (grid id old-cx old-cy new-x new-y)
  "Move entity from old cell to new position. Returns (values new-cx new-cy changed-p).
   If OLD-CX/OLD-CY are nil, performs insert only."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type (or null spatial-grid) grid)
           (type fixnum id)
           (type (or null fixnum) old-cx old-cy)
           (type single-float new-x new-y))
  (when grid
    (let ((cell-size (spatial-grid-cell-size grid)))
      (declare (type single-float cell-size))
      (multiple-value-bind (new-cx new-cy) (position-to-cell new-x new-y cell-size)
        (declare (type fixnum new-cx new-cy))
        (let ((changed (or (null old-cx)
                           (null old-cy)
                           (/= old-cx new-cx)
                           (/= old-cy new-cy))))
          (when changed
            ;; Remove from old cell if present
            (when (and old-cx old-cy)
              (spatial-grid-remove grid id old-cx old-cy))
            ;; Insert into new cell
            (if (spatial-grid-cells-array grid)
                (when (cell-in-bounds-p grid new-cx new-cy)
                  (push id (grid-cell-ref grid new-cx new-cy)))
                (let ((cells (spatial-grid-cells grid)))
                  (when cells
                    (push id (gethash (pack-cell-key new-cx new-cy) cells))))))
          (values new-cx new-cy changed))))))

(defun spatial-grid-clear (grid)
  "Remove all entities from the grid."
  (when grid
    (if (spatial-grid-cells-array grid)
        ;; Array-backed: clear all cells
        (let ((arr (spatial-grid-cells-array grid))
              (w (spatial-grid-width grid))
              (h (spatial-grid-height grid)))
          (dotimes (cx w)
            (dotimes (cy h)
              (setf (aref arr cx cy) nil))))
        ;; Hash-based
        (let ((cells (spatial-grid-cells grid)))
          (when cells
            (clrhash cells))))))

;;; Hash Key Packing (for fallback hash-based grids)

(declaim (inline pack-cell-key))
(defun pack-cell-key (cx cy)
  "Pack cell coordinates into a single fixnum key. Avoids cons allocation."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type fixnum cx cy))
  ;; Use 20 bits for each coordinate, supporting grids up to ~1M cells per axis
  (the fixnum (logior (the fixnum (ash (logand cx #xFFFFF) 20))
                      (the fixnum (logand cy #xFFFFF)))))

;;; Query Operations - Into Scratch Vector (No Allocation)
;;; These functions write results into a pre-allocated vector, avoiding per-query allocation.
;;; Work with both array-backed and hash-based grids.

(defun spatial-grid-query-neighbors-into (grid cx cy result-vector)
  "Query entities in cell (CX, CY) and its 8 neighbors into RESULT-VECTOR.
   Returns the fill-pointer (count of results). Does not allocate.
   RESULT-VECTOR must be a vector with fill-pointer."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type spatial-grid grid)
           (type fixnum cx cy)
           (type (vector fixnum) result-vector))
  (setf (fill-pointer result-vector) 0)
  (if (spatial-grid-cells-array grid)
      ;; Array-backed: direct O(1) access
      (let ((arr (spatial-grid-cells-array grid))
            (w (spatial-grid-width grid))
            (h (spatial-grid-height grid)))
        (declare (type fixnum w h))
        (loop :for dx fixnum :from -1 :to 1
              :for nx fixnum = (+ cx dx)
              :when (and (>= nx 0) (< nx w))
              :do (loop :for dy fixnum :from -1 :to 1
                        :for ny fixnum = (+ cy dy)
                        :when (and (>= ny 0) (< ny h))
                        :do (let ((cell-ids (aref arr nx ny)))
                              (dolist (id cell-ids)
                                (when (< (fill-pointer result-vector)
                                         (array-dimension result-vector 0))
                                  (vector-push id result-vector)))))))
      ;; Hash-based fallback
      (let ((cells (spatial-grid-cells grid)))
        (when cells
          (loop :for dx fixnum :from -1 :to 1
                :do (loop :for dy fixnum :from -1 :to 1
                          :for key = (pack-cell-key (the fixnum (+ cx dx))
                                                     (the fixnum (+ cy dy)))
                          :for cell-ids = (gethash key cells)
                          :when cell-ids
                          :do (dolist (id cell-ids)
                                (when (< (fill-pointer result-vector)
                                         (array-dimension result-vector 0))
                                  (vector-push id result-vector))))))))
  (fill-pointer result-vector))

(defun spatial-grid-query-rect-into (grid left top right bottom result-vector)
  "Query entities within rectangle into RESULT-VECTOR. Does not allocate.
   Returns the fill-pointer (count of results)."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type spatial-grid grid)
           (type single-float left top right bottom)
           (type (vector fixnum) result-vector))
  (setf (fill-pointer result-vector) 0)
  (let ((cell-size (spatial-grid-cell-size grid)))
    (declare (type single-float cell-size))
    (if (spatial-grid-cells-array grid)
        ;; Array-backed: direct O(1) access
        (let* ((arr (spatial-grid-cells-array grid))
               (w (spatial-grid-width grid))
               (h (spatial-grid-height grid))
               (start-cx (max 0 (the fixnum (floor left cell-size))))
               (end-cx (min (1- w) (the fixnum (floor right cell-size))))
               (start-cy (max 0 (the fixnum (floor top cell-size))))
               (end-cy (min (1- h) (the fixnum (floor bottom cell-size)))))
          (declare (type fixnum w h start-cx end-cx start-cy end-cy))
          (loop :for cy fixnum :from start-cy :to end-cy
                :do (loop :for cx fixnum :from start-cx :to end-cx
                          :do (let ((cell-ids (aref arr cx cy)))
                                (dolist (id cell-ids)
                                  (when (< (fill-pointer result-vector)
                                           (array-dimension result-vector 0))
                                    (vector-push id result-vector)))))))
        ;; Hash-based fallback
        (let* ((cells (spatial-grid-cells grid))
               (start-cx (the fixnum (floor left cell-size)))
               (end-cx (the fixnum (floor right cell-size)))
               (start-cy (the fixnum (floor top cell-size)))
               (end-cy (the fixnum (floor bottom cell-size))))
          (declare (type fixnum start-cx end-cx start-cy end-cy))
          (when cells
            (loop :for cy fixnum :from start-cy :to end-cy
                  :do (loop :for cx fixnum :from start-cx :to end-cx
                            :for key = (pack-cell-key cx cy)
                            :for cell-ids = (gethash key cells)
                            :when cell-ids
                            :do (dolist (id cell-ids)
                                  (when (< (fill-pointer result-vector)
                                           (array-dimension result-vector 0))
                                    (vector-push id result-vector)))))))))
  (fill-pointer result-vector))

;;; Query Operations - List-Returning (Backward Compatible)

(defun spatial-grid-get-cell (grid cx cy)
  "Get list of entity IDs in cell (CX, CY)."
  (when grid
    (if (spatial-grid-cells-array grid)
        (when (cell-in-bounds-p grid cx cy)
          (grid-cell-ref grid cx cy))
        (let ((cells (spatial-grid-cells grid)))
          (when cells
            (gethash (pack-cell-key cx cy) cells))))))

(defun spatial-grid-query-neighbors (grid cx cy)
  "Get all entity IDs in cell (CX, CY) and its 8 neighbors (9 cells total).
   Returns a fresh list of IDs."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type (or null spatial-grid) grid)
           (type fixnum cx cy))
  (when grid
    (if (spatial-grid-cells-array grid)
        ;; Array-backed: use scratch vector then convert to list
        (progn
          (spatial-grid-query-neighbors-into grid cx cy *spatial-scratch-vector*)
          (loop :for i fixnum :from 0 :below (fill-pointer *spatial-scratch-vector*)
                :collect (aref *spatial-scratch-vector* i)))
        ;; Hash-based fallback
        (let ((cells (spatial-grid-cells grid))
              (result nil))
          (when cells
            (loop :for dx fixnum :from -1 :to 1
                  :do (loop :for dy fixnum :from -1 :to 1
                            :for key = (pack-cell-key (the fixnum (+ cx dx))
                                                       (the fixnum (+ cy dy)))
                            :for cell-ids = (gethash key cells)
                            :when cell-ids
                            :do (setf result (nconc (copy-list cell-ids) result)))))
          result))))

(defun spatial-grid-query-radius (grid cx cy radius)
  "Get all entity IDs within RADIUS cells of (CX, CY).
   Radius 0 = center cell only, 1 = 3x3, 2 = 5x5, etc.
   Returns a fresh list of IDs."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type (or null spatial-grid) grid)
           (type fixnum cx cy radius))
  (when grid
    (if (spatial-grid-cells-array grid)
        ;; Array-backed
        (let ((arr (spatial-grid-cells-array grid))
              (w (spatial-grid-width grid))
              (h (spatial-grid-height grid))
              (result nil))
          (loop :for dx fixnum :from (- radius) :to radius
                :for nx fixnum = (+ cx dx)
                :when (and (>= nx 0) (< nx w))
                :do (loop :for dy fixnum :from (- radius) :to radius
                          :for ny fixnum = (+ cy dy)
                          :when (and (>= ny 0) (< ny h))
                          :do (let ((cell-ids (aref arr nx ny)))
                                (dolist (id cell-ids)
                                  (push id result)))))
          result)
        ;; Hash-based fallback
        (let ((cells (spatial-grid-cells grid))
              (result nil))
          (when cells
            (loop :for dx fixnum :from (- radius) :to radius
                  :do (loop :for dy fixnum :from (- radius) :to radius
                            :for key = (pack-cell-key (the fixnum (+ cx dx))
                                                       (the fixnum (+ cy dy)))
                            :for cell-ids = (gethash key cells)
                            :when cell-ids
                            :do (setf result (nconc (copy-list cell-ids) result)))))
          result))))

(defun spatial-grid-query-position (grid x y)
  "Get all entity IDs in the cell containing position (X, Y) and neighbors.
   Convenience wrapper around spatial-grid-query-neighbors."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type (or null spatial-grid) grid)
           (type single-float x y))
  (when grid
    (multiple-value-bind (cx cy) (position-to-cell x y (spatial-grid-cell-size grid))
      (declare (type fixnum cx cy))
      (spatial-grid-query-neighbors grid cx cy))))

(defun spatial-grid-query-rect (grid left top right bottom)
  "Return list of entity IDs within the world-coordinate rectangle.
   Queries all cells overlapping the rectangle bounds.
   Used for viewport culling during rendering."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type (or null spatial-grid) grid)
           (type single-float left top right bottom))
  (when grid
    (if (spatial-grid-cells-array grid)
        ;; Array-backed: use scratch vector then convert
        (progn
          (spatial-grid-query-rect-into grid left top right bottom *spatial-scratch-vector*)
          (loop :for i fixnum :from 0 :below (fill-pointer *spatial-scratch-vector*)
                :collect (aref *spatial-scratch-vector* i)))
        ;; Hash-based fallback
        (let* ((cell-size (spatial-grid-cell-size grid))
               (cells (spatial-grid-cells grid))
               (start-cx (the fixnum (floor left cell-size)))
               (end-cx (the fixnum (floor right cell-size)))
               (start-cy (the fixnum (floor top cell-size)))
               (end-cy (the fixnum (floor bottom cell-size)))
               (result nil))
          (declare (type single-float cell-size)
                   (type fixnum start-cx end-cx start-cy end-cy))
          (when cells
            (loop :for cy fixnum :from start-cy :to end-cy
                  :do (loop :for cx fixnum :from start-cx :to end-cx
                            :for key = (pack-cell-key cx cy)
                            :for cell-ids = (gethash key cells)
                            :when cell-ids
                            :do (setf result (nconc (copy-list cell-ids) result)))))
          result))))

;;; Entity Cell Tracking Helpers

(defun entity-cell-changed-p (old-cx old-cy new-x new-y cell-size)
  "Return T if position (NEW-X, NEW-Y) is in a different cell than (OLD-CX, OLD-CY)."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type (or null fixnum) old-cx old-cy)
           (type single-float new-x new-y cell-size))
  (multiple-value-bind (new-cx new-cy) (position-to-cell new-x new-y cell-size)
    (declare (type fixnum new-cx new-cy))
    (or (null old-cx)
        (null old-cy)
        (/= old-cx new-cx)
        (/= old-cy new-cy))))

(defun update-entity-grid-cell (grid entity-id old-cx old-cy new-x new-y
                                 set-cell-x-fn set-cell-y-fn)
  "Update entity's grid cell if position changed. Calls SET-CELL-X-FN and SET-CELL-Y-FN
   with the new cell coordinates. Returns T if cell changed."
  (when grid
    (multiple-value-bind (new-cx new-cy changed)
        (spatial-grid-move grid entity-id old-cx old-cy new-x new-y)
      (when changed
        (funcall set-cell-x-fn new-cx)
        (funcall set-cell-y-fn new-cy))
      changed)))

;;; Bulk Population

(defun populate-npc-grid (zone-state npcs)
  "Insert all NPCs into the zone-state's NPC spatial grid.
   Updates each NPC's grid-cell-x and grid-cell-y.
   Also rebuilds the NPC index map for O(1) ID lookup."
  (when (and zone-state npcs)
    ;; Rebuild NPC index map for O(1) lookup
    (rebuild-npc-index-map zone-state)
    (let ((grid (zone-state-npc-grid zone-state)))
      (when grid
        (spatial-grid-clear grid)
        (loop :for npc :across npcs
              :do (multiple-value-bind (cx cy)
                      (position-to-cell (npc-x npc) (npc-y npc)
                                        (spatial-grid-cell-size grid))
                    (spatial-grid-insert grid (npc-id npc) (npc-x npc) (npc-y npc))
                    (setf (npc-grid-cell-x npc) cx
                          (npc-grid-cell-y npc) cy)))))))

(defun populate-player-grid (zone-state players)
  "Insert all players in a zone into the zone-state's player spatial grid.
   Updates each player's grid-cell-x and grid-cell-y."
  (when (and zone-state players)
    (let ((grid (zone-state-player-grid zone-state)))
      (when grid
        (spatial-grid-clear grid)
        (let ((zone-id (zone-state-zone-id zone-state)))
          (loop :for player :across players
                :when (eql (player-zone-id player) zone-id)
                :do (multiple-value-bind (cx cy)
                        (position-to-cell (player-x player) (player-y player)
                                          (spatial-grid-cell-size grid))
                      (spatial-grid-insert grid (player-id player) (player-x player) (player-y player))
                      (setf (player-grid-cell-x player) cx
                            (player-grid-cell-y player) cy))))))))

(defun ensure-player-in-grid (player zone-state)
  "Ensure a player is in the zone's spatial grid and zone-players cache.
   Only inserts if grid-cell is nil (player wasn't already tracked).
   Called to handle players who were added before zone-state existed."
  (when (and player zone-state
             (null (player-grid-cell-x player)))
    ;; Player not yet in grid - add to both grid and cache
    ;; This only runs once per player (when they first need tracking)
    (add-player-to-zone-cache player zone-state)
    (let ((grid (zone-state-player-grid zone-state)))
      (when grid
        (multiple-value-bind (cx cy)
            (position-to-cell (player-x player) (player-y player)
                              (spatial-grid-cell-size grid))
          (spatial-grid-insert grid (player-id player) (player-x player) (player-y player))
          (setf (player-grid-cell-x player) cx
                (player-grid-cell-y player) cy))))))

;;; Debug/Stats

(defun spatial-grid-stats (grid)
  "Return stats about the grid: (values cell-count total-entities)."
  (if grid
      (if (spatial-grid-cells-array grid)
          ;; Array-backed
          (let ((arr (spatial-grid-cells-array grid))
                (w (spatial-grid-width grid))
                (h (spatial-grid-height grid))
                (cell-count 0)
                (total 0))
            (dotimes (cx w)
              (dotimes (cy h)
                (let ((cell (aref arr cx cy)))
                  (when cell
                    (incf cell-count)
                    (incf total (length cell))))))
            (values cell-count total))
          ;; Hash-based
          (let ((cells (spatial-grid-cells grid))
                (total 0))
            (when cells
              (maphash (lambda (k v)
                         (declare (ignore k))
                         (incf total (length v)))
                       cells))
            (values (if cells (hash-table-count cells) 0) total)))
      (values 0 0)))
