;; NOTE: If you change behavior here, update docs/world-graph.md :)
(in-package #:mmorpg)

(defstruct (world-graph (:constructor %make-world-graph))
  ;; World graph data for zone transitions.
  edges-by-zone zone-paths)

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
  (log-verbose "Loading world graph from ~a" (or path "<default>"))
  (let* ((data (read-world-graph-data path))
         (plist (world-graph-data-plist data))
         (edges (getf plist :edges nil))
         (edges-by-zone (normalize-world-graph-edges edges))
         (zone-paths (build-zone-paths (resolve-zone-path zone-root))))
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
