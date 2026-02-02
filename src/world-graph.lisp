;; NOTE: If you change behavior here, update docs/world-graph.md :)
(in-package #:mmorpg)

(defstruct (world-graph (:constructor %make-world-graph))
  ;; World graph data for zone transitions.
  edges-by-zone zone-paths
  ;; Phase 4 (Bug 4): Pre-computed collision bounds for every zone.
  ;; Hash table: zone-id -> #(min-x max-x min-y max-y) as single-float.
  ;; Built at startup from zone file headers; always available on client.
  (zone-bounds-index nil :type (or null hash-table)))

(defun resolve-world-graph-path (path)
  ;; Resolve PATH relative to the system root if needed.
  (if (and path (not (pathnamep path)))
      (merge-pathnames path (asdf:system-source-directory :mmorpg))
      path))

(defun read-world-graph-data (path)
  ;; Read a world graph data form without evaluation.
  ;; Returns world graph data on success, NIL on failure (non-fatal).
  (let* ((full-path (resolve-world-graph-path path)))
    (if (and full-path (probe-file full-path))
        (handler-case
            (with-open-file (in full-path :direction :input)
              (with-standard-io-syntax
                (let ((*read-eval* nil))
                  (read in nil nil))))
          (error (e)
            (warn "Failed to read world graph data from ~a: ~a" full-path e)
            (log-verbose "World graph read error: ~a" e)
            nil))
        (progn
          (log-verbose "World graph not found at ~a; using empty graph"
                       (or full-path "<unset>"))
          nil))))

(defun world-graph-data-plist (data)
  ;; Normalize world graph data to a plist.
  (cond
    ((and (listp data) (keywordp (first data))) data)
    ((and (listp data) (eq (first data) :world-graph)) (second data))
    (t nil)))

(defun collect-zone-files (root)
  ;; Collect zone files under ROOT.
  (let ((files nil))
    (let ((root (and root (uiop:ensure-directory-pathname root))))
      (when (and root (probe-file root))
        (dolist (path (uiop:directory-files root))
          (when (string-equal (pathname-type path) "lisp")
            (push (namestring path) files)))))
    (if files
        (coerce (sort files #'string<) 'vector)
        (make-array 0))))

(defun zone-id-from-file (path)
  ;; Read the zone ID from PATH without loading the full zone.
  (let* ((data (read-zone-data path))
         (plist (zone-data-plist data)))
    (getf plist :id)))

(defun build-zone-paths (root)
  ;; Build a lookup from zone ID to zone file path.
  (let ((table (make-hash-table :test 'eq))
        (root (and root (uiop:ensure-directory-pathname root))))
    (dolist (path (coerce (collect-zone-files root) 'list))
      (let ((id (zone-id-from-file path)))
        (when id
          (setf (gethash id table) path))))
    table))

(defun normalize-world-graph-edges (edges)
  ;; Build an edge table keyed by :from zone ID.
  (let ((table (make-hash-table :test 'eq)))
    (dolist (edge edges)
      (let ((from (getf edge :from)))
        (when from
          (push edge (gethash from table)))))
    (maphash (lambda (key value)
               (setf (gethash key table) (nreverse value)))
             table)
    table))

(defun load-world-graph (&optional (path *world-graph-path*) (zone-root *zone-root*))
  ;; Load the world graph and index zone paths.
  ;; Also populates *known-zone-ids* for validation (Phase 6).
  (log-verbose "Loading world graph from ~a" (or path "<default>"))
  (let* ((data (read-world-graph-data path))
         (plist (world-graph-data-plist data))
         (edges (getf plist :edges nil))
         (edges-by-zone (normalize-world-graph-edges edges))
         (zone-paths (build-zone-paths (resolve-zone-path zone-root))))
    ;; Phase 6: Populate *known-zone-ids* for validation quarantine checks
    (let ((zone-ids (make-hash-table :test 'eq)))
      (maphash (lambda (id path)
                 (declare (ignore path))
                 (setf (gethash id zone-ids) t))
               zone-paths)
      (setf *known-zone-ids* zone-ids))
    (log-verbose "World graph ready: edges=~d zones=~d"
                 (hash-table-count edges-by-zone)
                 (hash-table-count zone-paths))
    (%make-world-graph :edges-by-zone edges-by-zone
                       :zone-paths zone-paths)))

(defun world-graph-exits (graph zone-id)
  ;; Return the exits for ZONE-ID.
  (when (and graph zone-id)
    (gethash zone-id (world-graph-edges-by-zone graph))))

(defun world-graph-zone-path (graph zone-id)
  ;; Return the zone file path for ZONE-ID.
  (when (and graph zone-id)
    (gethash zone-id (world-graph-zone-paths graph))))

(defun zone-bounds-with-origin (tile-dest-size width height origin-x origin-y
                                collision-half-w collision-half-h)
  "Calculate movement bounds for a zone with given tile dimensions and origin.
   ORIGIN-X/ORIGIN-Y are tile offsets (default 0 for zero-origin zones).
   Returns (values min-x max-x min-y max-y) as single-float."
  (let* ((ox (float (or origin-x 0) 1.0))
         (oy (float (or origin-y 0) 1.0))
         (wall-min-x (+ (* ox tile-dest-size) collision-half-w))
         (wall-max-x (- (* (+ ox (float width 1.0)) tile-dest-size) collision-half-w))
         (wall-min-y (+ (* oy tile-dest-size) collision-half-h))
         (wall-max-y (- (* (+ oy (float height 1.0)) tile-dest-size) collision-half-h)))
    (values wall-min-x wall-max-x wall-min-y wall-max-y)))

(defun build-zone-bounds-index (zone-paths tile-dest-size collision-half-w collision-half-h)
  "Build a zone-bounds index from ZONE-PATHS (zone-id -> file-path hash table).
   Reads :width, :height, and optional :origin-x/:origin-y from each zone file
   header and computes collision bounds.  Origin defaults to (0,0) when absent.
   Returns hash table: zone-id -> #(min-x max-x min-y max-y) as single-float.
   Runs once at startup; not on hot path."
  (let ((index (make-hash-table :test 'eq :size (hash-table-count zone-paths))))
    (maphash
     (lambda (zone-id path)
       (handler-case
           (let* ((data (read-zone-data path))
                  (plist (zone-data-plist data))
                  (width (getf plist :width))
                  (height (getf plist :height))
                  (origin-x (getf plist :origin-x 0))
                  (origin-y (getf plist :origin-y 0)))
             (when (and width height (> width 0) (> height 0))
               (multiple-value-bind (min-x max-x min-y max-y)
                   (zone-bounds-with-origin tile-dest-size width height
                                            origin-x origin-y
                                            collision-half-w collision-half-h)
                 (setf (gethash zone-id index)
                       (make-array 4 :element-type 'single-float
                                     :initial-contents
                                     (list min-x max-x min-y max-y))))))
         (error (e)
           (warn "Failed to read zone bounds for ~a: ~a" zone-id e))))
     zone-paths)
    (log-verbose "Zone bounds index: ~d zones indexed" (hash-table-count index))
    ;; Validate zone bounds uniformity — minimap multi-hop pathing assumes
    ;; uniform bounds across spatially connected zones.
    (let ((ref-bounds nil)
          (mismatches nil))
      (maphash (lambda (zone-id bounds)
                 (if (null ref-bounds)
                     (setf ref-bounds bounds)
                     (unless (and (= (aref bounds 0) (aref ref-bounds 0))
                                  (= (aref bounds 1) (aref ref-bounds 1))
                                  (= (aref bounds 2) (aref ref-bounds 2))
                                  (= (aref bounds 3) (aref ref-bounds 3)))
                       (push zone-id mismatches))))
               index)
      (when mismatches
        (warn "Zone bounds non-uniformity detected: ~d zone(s) differ from reference bounds ~a. ~
               Minimap multi-hop pathing may land on wrong tiles. Non-uniform zones: ~{~a~^, ~}"
              (length mismatches) ref-bounds mismatches)))
    index))

(defun world-graph-zone-bounds (graph zone-id)
  "Look up pre-computed collision bounds for ZONE-ID from the zone-bounds index.
   Returns (values min-x max-x min-y max-y) or NIL if not indexed."
  (let ((index (and graph (world-graph-zone-bounds-index graph))))
    (when index
      (let ((bounds (gethash zone-id index)))
        (when bounds
          (values (aref bounds 0) (aref bounds 1)
                  (aref bounds 2) (aref bounds 3)))))))

(defun world-graph-find-path (graph from-id to-id)
  "BFS shortest path from FROM-ID to TO-ID over spatial edges.
   Returns ordered list of zone-ids (excluding FROM-ID, including TO-ID),
   or NIL if unreachable or same zone."
  (when (or (null graph) (null from-id) (null to-id) (eq from-id to-id))
    (return-from world-graph-find-path nil))
  (let ((visited (make-hash-table :test 'eq :size 64))
        (parent (make-hash-table :test 'eq :size 64))
        (queue (list from-id))
        (max-hops *bfs-max-hops*))
    (setf (gethash from-id visited) t)
    (loop :for hop :from 0 :below max-hops
          :while queue
          :do (let ((next-queue nil))
                (dolist (current queue)
                  (dolist (exit (world-graph-exits graph current))
                    (when (spatial-exit-p exit)
                      (let ((neighbor (getf exit :to)))
                        (when (and neighbor (not (gethash neighbor visited)))
                          (setf (gethash neighbor visited) t)
                          (setf (gethash neighbor parent) current)
                          (when (eq neighbor to-id)
                            ;; Reconstruct path from to-id back to from-id
                            (let ((path nil)
                                  (node to-id))
                              (loop :while (and node (not (eq node from-id)))
                                    :do (push node path)
                                        (setf node (gethash node parent)))
                              (return-from world-graph-find-path path)))
                          (push neighbor next-queue))))))
                (setf queue (nreverse next-queue))))
    ;; If we get here, BFS exhausted without finding to-id
    (when queue
      (log-verbose "world-graph-find-path: BFS cap (~d) reached searching ~a -> ~a"
                   max-hops from-id to-id))
    nil))

(defun world-graph-edge-between (graph from-id to-id)
  "Return the edge direction (:north/:south/:east/:west) from FROM-ID to TO-ID,
   or NIL if not directly connected by a spatial exit."
  (when (and graph from-id to-id)
    (dolist (exit (world-graph-exits graph from-id))
      (when (and (spatial-exit-p exit)
                 (eq (getf exit :to) to-id))
        (return (getf exit :edge))))))

(defun zone-path-for-id (world zone-id)
  "Return file path for ZONE-ID from WORLD's graph, or nil."
  (let ((graph (world-world-graph world)))
    (when graph
      (world-graph-zone-path graph zone-id))))

(defun zone-path-for-id-exists-p (world zone-id)
  "Return T if ZONE-ID has a valid path in WORLD's graph."
  (not (null (zone-path-for-id world zone-id))))
