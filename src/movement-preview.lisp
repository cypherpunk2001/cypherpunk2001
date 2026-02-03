(in-package #:mmorpg)

;;;; ========================================================================
;;;; Preview Zones & Camera
;;;; Camera edge checks, neighbor zone display logic, preview zone loading.
;;;; ========================================================================

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

(defun camera-view-center (player editor &optional camera)
  "Return the current camera focus point.
   When camera is provided and has leash state, uses leash target.
   Otherwise falls back to editor camera or player position."
  (cond
    ((and editor (editor-active editor))
     (values (editor-camera-x editor) (editor-camera-y editor)))
    ((and camera
          *camera-leash-enabled*
          (camera-leash-x camera)
          (camera-leash-y camera))
     (values (camera-leash-x camera) (camera-leash-y camera)))
    (t
     (values (player-x player) (player-y player)))))

(defun camera-view-bounds (camera player editor)
  "Return view bounds in world coordinates for the current camera focus.
   Uses camera leash when available via camera-view-center."
  (let* ((zoom (camera-zoom camera))
         (half-view-width (/ (current-screen-width) (* 2.0 zoom)))
         (half-view-height (/ (current-screen-height) (* 2.0 zoom))))
    (multiple-value-bind (x y) (camera-view-center player editor camera)
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
