;;;; spatial.lisp - Spatial grid for efficient proximity queries
;;;;
;;;; Provides O(1) cell-based spatial indexing for entities. Each zone has its own
;;;; grid for players and NPCs. Entities track their current cell for O(1) updates.

(in-package #:mmorpg)

;;; Configuration

(defparameter *spatial-cell-size* 128.0
  "Size of spatial grid cells in world pixels. Should be >= typical entity interaction range.
   128px = 4 tiles at 32px/tile, covers melee range + margin.")

;;; Spatial Grid Structure

(defstruct (spatial-grid (:constructor %make-spatial-grid))
  "Hash-based spatial grid for efficient proximity queries.
   Keys are (cell-x . cell-y) cons cells, values are lists of entity IDs."
  (cells (make-hash-table :test 'equal))  ; (cx . cy) -> (id1 id2 ...)
  (cell-size *spatial-cell-size*))

(defun make-spatial-grid (&optional (cell-size *spatial-cell-size*))
  "Create a new spatial grid with the given cell size."
  (%make-spatial-grid :cells (make-hash-table :test 'equal)
                      :cell-size cell-size))

;;; Position/Cell Conversion

(defun position-to-cell (x y cell-size)
  "Convert world position to cell coordinates. Returns (values cell-x cell-y)."
  (values (floor x cell-size) (floor y cell-size)))

(defun make-cell-key (cx cy)
  "Create a hash key for cell coordinates."
  (cons cx cy))

;;; Core Grid Operations

(defun spatial-grid-insert (grid id x y)
  "Insert entity ID at position (X, Y). Returns the cell key."
  (when grid
    (let ((cell-size (spatial-grid-cell-size grid)))
      (multiple-value-bind (cx cy) (position-to-cell x y cell-size)
        (let ((key (make-cell-key cx cy)))
          (push id (gethash key (spatial-grid-cells grid)))
          key)))))

(defun spatial-grid-remove (grid id cx cy)
  "Remove entity ID from cell (CX, CY). Returns T if found and removed."
  (when (and grid cx cy)
    (let* ((key (make-cell-key cx cy))
           (cells (spatial-grid-cells grid))
           (cell-list (gethash key cells)))
      (when cell-list
        (let ((new-list (delete id cell-list :count 1)))
          (if new-list
              (setf (gethash key cells) new-list)
              (remhash key cells))
          t)))))

(defun spatial-grid-move (grid id old-cx old-cy new-x new-y)
  "Move entity from old cell to new position. Returns (values new-cx new-cy changed-p).
   If OLD-CX/OLD-CY are nil, performs insert only."
  (when grid
    (let ((cell-size (spatial-grid-cell-size grid)))
      (multiple-value-bind (new-cx new-cy) (position-to-cell new-x new-y cell-size)
        (let ((changed (or (null old-cx)
                           (null old-cy)
                           (/= old-cx new-cx)
                           (/= old-cy new-cy))))
          (when changed
            ;; Remove from old cell if present
            (when (and old-cx old-cy)
              (spatial-grid-remove grid id old-cx old-cy))
            ;; Insert into new cell
            (let ((key (make-cell-key new-cx new-cy)))
              (push id (gethash key (spatial-grid-cells grid)))))
          (values new-cx new-cy changed))))))

(defun spatial-grid-clear (grid)
  "Remove all entities from the grid."
  (when grid
    (clrhash (spatial-grid-cells grid))))

;;; Query Operations

(defun spatial-grid-get-cell (grid cx cy)
  "Get list of entity IDs in cell (CX, CY)."
  (when grid
    (gethash (make-cell-key cx cy) (spatial-grid-cells grid))))

(defun spatial-grid-query-neighbors (grid cx cy)
  "Get all entity IDs in cell (CX, CY) and its 8 neighbors (9 cells total).
   Returns a fresh list of IDs."
  (when grid
    (let ((cells (spatial-grid-cells grid))
          (result nil))
      (loop :for dx :from -1 :to 1
            :do (loop :for dy :from -1 :to 1
                      :for key = (make-cell-key (+ cx dx) (+ cy dy))
                      :for cell-ids = (gethash key cells)
                      :when cell-ids
                      :do (setf result (nconc (copy-list cell-ids) result))))
      result)))

(defun spatial-grid-query-radius (grid cx cy radius)
  "Get all entity IDs within RADIUS cells of (CX, CY).
   Radius 0 = center cell only, 1 = 3x3, 2 = 5x5, etc.
   Returns a fresh list of IDs."
  (when grid
    (let ((cells (spatial-grid-cells grid))
          (result nil))
      (loop :for dx :from (- radius) :to radius
            :do (loop :for dy :from (- radius) :to radius
                      :for key = (make-cell-key (+ cx dx) (+ cy dy))
                      :for cell-ids = (gethash key cells)
                      :when cell-ids
                      :do (setf result (nconc (copy-list cell-ids) result))))
      result)))

(defun spatial-grid-query-position (grid x y)
  "Get all entity IDs in the cell containing position (X, Y) and neighbors.
   Convenience wrapper around spatial-grid-query-neighbors."
  (when grid
    (multiple-value-bind (cx cy) (position-to-cell x y (spatial-grid-cell-size grid))
      (spatial-grid-query-neighbors grid cx cy))))

;;; Entity Cell Tracking Helpers

(defun entity-cell-changed-p (old-cx old-cy new-x new-y cell-size)
  "Return T if position (NEW-X, NEW-Y) is in a different cell than (OLD-CX, OLD-CY)."
  (multiple-value-bind (new-cx new-cy) (position-to-cell new-x new-y cell-size)
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
      (let ((cells (spatial-grid-cells grid))
            (total 0))
        (maphash (lambda (k v)
                   (declare (ignore k))
                   (incf total (length v)))
                 cells)
        (values (hash-table-count cells) total))
      (values 0 0)))
