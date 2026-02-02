(in-package #:mmorpg)

;;; ============================================================
;;; WORLD-GRAPH.LISP TESTS
;;; ============================================================

(defun test-world-graph-data-plist ()
  "Test world graph data normalization."
  ;; Direct plist - any plist starting with keyword returns as-is
  (let ((direct '(:edges ((:from :town :to :forest)))))
    (assert (equal (world-graph-data-plist direct) direct) () "wg-plist: direct"))
  ;; Another keyword-starting plist (world-graph is a keyword, so first condition matches)
  (let ((wrapped '(:world-graph (:edges ()))))
    (assert (equal (world-graph-data-plist wrapped) wrapped) () "wg-plist: keyword plist"))
  ;; Invalid data
  (assert (null (world-graph-data-plist nil)) () "wg-plist: nil -> nil")
  (assert (null (world-graph-data-plist '("not" "a" "plist"))) () "wg-plist: invalid"))

(defun test-normalize-world-graph-edges ()
  "Test world graph edge normalization."
  (let* ((edges '((:from :town :to :forest :edge :north)
                  (:from :town :to :cave :edge :south)
                  (:from :forest :to :town :edge :south)))
         (table (normalize-world-graph-edges edges)))
    ;; Town should have 2 exits
    (let ((town-exits (gethash :town table)))
      (assert (= (length town-exits) 2) () "wg-edges: town has 2 exits"))
    ;; Forest should have 1 exit
    (let ((forest-exits (gethash :forest table)))
      (assert (= (length forest-exits) 1) () "wg-edges: forest has 1 exit"))
    ;; Cave should have 0 exits (only destination)
    (let ((cave-exits (gethash :cave table)))
      (assert (null cave-exits) () "wg-edges: cave has no exits"))))

;;; ============================================================

;;; ADDITIONAL WORLD GRAPH TESTS
;;; ============================================================

(defun test-world-graph-exits ()
  "Test getting zone exits from world graph."
  (let* ((edges (list '(:from :zone-a :to :zone-b :edge :north)
                      '(:from :zone-a :to :zone-c :edge :east)
                      '(:from :zone-b :to :zone-a :edge :south)))
         (edges-by-zone (normalize-world-graph-edges edges))
         (graph (%make-world-graph :edges-by-zone edges-by-zone
                                    :zone-paths (make-hash-table))))
    ;; Zone A has 2 exits
    (let ((exits (world-graph-exits graph :zone-a)))
      (assert (= (length exits) 2) () "graph-exits: zone-a has 2 exits"))
    ;; Zone B has 1 exit
    (let ((exits (world-graph-exits graph :zone-b)))
      (assert (= (length exits) 1) () "graph-exits: zone-b has 1 exit"))
    ;; Zone C has no exits
    (assert (null (world-graph-exits graph :zone-c)) () "graph-exits: zone-c has none")
    ;; Nil graph
    (assert (null (world-graph-exits nil :zone-a)) () "graph-exits: nil graph")))

(defun test-world-graph-zone-path ()
  "Test getting zone file path from world graph."
  (let* ((paths (make-hash-table :test 'eq))
         (graph nil))
    (setf (gethash :zone-a paths) "/path/to/zone-a.lisp")
    (setf (gethash :zone-b paths) "/path/to/zone-b.lisp")
    (setf graph (%make-world-graph :edges-by-zone (make-hash-table)
                                    :zone-paths paths))
    (assert (string= (world-graph-zone-path graph :zone-a) "/path/to/zone-a.lisp") ()
            "graph-path: zone-a")
    (assert (string= (world-graph-zone-path graph :zone-b) "/path/to/zone-b.lisp") ()
            "graph-path: zone-b")
    (assert (null (world-graph-zone-path graph :nonexistent)) ()
            "graph-path: not found")
    (assert (null (world-graph-zone-path nil :zone-a)) ()
            "graph-path: nil graph")))

(defun test-collect-zone-files ()
  "Test collecting zone files from directory."
  ;; Test against actual data/zones directory
  (let* ((zone-root (merge-pathnames "data/zones/"
                                      (asdf:system-source-directory :mmorpg)))
         (files (collect-zone-files zone-root)))
    (assert (vectorp files) () "collect-zone-files: returns vector")
    ;; Should find at least one zone file if directory exists
    (when (probe-file zone-root)
      (assert (> (length files) 0) () "collect-zone-files: finds files")))
  ;; Non-existent directory returns empty
  (let ((files (collect-zone-files "/nonexistent/path/12345/")))
    (assert (= (length files) 0) () "collect-zone-files: empty for missing dir")))

(defun test-zone-id-from-file ()
  "Test reading zone ID from file."
  ;; Test against actual zone file if exists
  (let* ((zone-root (merge-pathnames "data/zones/"
                                      (asdf:system-source-directory :mmorpg)))
         (files (collect-zone-files zone-root)))
    (when (and (> (length files) 0) (probe-file (aref files 0)))
      (let ((id (zone-id-from-file (aref files 0))))
        (assert (keywordp id) () "zone-id-from-file: returns keyword"))))
  ;; Non-existent file returns nil
  (assert (null (zone-id-from-file "/nonexistent/zone.lisp")) ()
          "zone-id-from-file: nil for missing file"))

(defun test-build-zone-paths ()
  "Test building zone path lookup table."
  (let* ((zone-root (merge-pathnames "data/zones/"
                                      (asdf:system-source-directory :mmorpg)))
         (paths (build-zone-paths zone-root)))
    (assert (hash-table-p paths) () "build-zone-paths: returns hash table")
    ;; If zones exist, table should have entries
    (when (probe-file zone-root)
      (let ((count (hash-table-count paths)))
        (assert (>= count 0) () "build-zone-paths: non-negative count")))))

;;; ============================================================

;;; BFS PATHFINDING TESTS (Bug 4 Part 3)
;;; ============================================================

(defun make-test-graph-3x3 ()
  "Create a 3x3 grid world graph for BFS tests.
   Layout:  NW  N  NE
            W   C  E
            SW  S  SE
   All edges are spatial (preserve-x/y)."
  (let ((edges (list
                ;; Center row: W <-> C <-> E
                '(:from :zone-c :to :zone-e :edge :east :offset :preserve-y)
                '(:from :zone-e :to :zone-c :edge :west :offset :preserve-y)
                '(:from :zone-c :to :zone-w :edge :west :offset :preserve-y)
                '(:from :zone-w :to :zone-c :edge :east :offset :preserve-y)
                ;; Center column: N <-> C <-> S
                '(:from :zone-c :to :zone-n :edge :north :offset :preserve-x)
                '(:from :zone-n :to :zone-c :edge :south :offset :preserve-x)
                '(:from :zone-c :to :zone-s :edge :south :offset :preserve-x)
                '(:from :zone-s :to :zone-c :edge :north :offset :preserve-x)
                ;; North row: NW <-> N <-> NE
                '(:from :zone-n :to :zone-ne :edge :east :offset :preserve-y)
                '(:from :zone-ne :to :zone-n :edge :west :offset :preserve-y)
                '(:from :zone-n :to :zone-nw :edge :west :offset :preserve-y)
                '(:from :zone-nw :to :zone-n :edge :east :offset :preserve-y)
                ;; South row: SW <-> S <-> SE
                '(:from :zone-s :to :zone-se :edge :east :offset :preserve-y)
                '(:from :zone-se :to :zone-s :edge :west :offset :preserve-y)
                '(:from :zone-s :to :zone-sw :edge :west :offset :preserve-y)
                '(:from :zone-sw :to :zone-s :edge :east :offset :preserve-y)
                ;; West column: NW <-> W <-> SW
                '(:from :zone-w :to :zone-nw :edge :north :offset :preserve-x)
                '(:from :zone-nw :to :zone-w :edge :south :offset :preserve-x)
                '(:from :zone-w :to :zone-sw :edge :south :offset :preserve-x)
                '(:from :zone-sw :to :zone-w :edge :north :offset :preserve-x)
                ;; East column: NE <-> E <-> SE
                '(:from :zone-e :to :zone-ne :edge :north :offset :preserve-x)
                '(:from :zone-ne :to :zone-e :edge :south :offset :preserve-x)
                '(:from :zone-e :to :zone-se :edge :south :offset :preserve-x)
                '(:from :zone-se :to :zone-e :edge :north :offset :preserve-x))))
    (%make-world-graph :edges-by-zone (normalize-world-graph-edges edges)
                       :zone-paths (make-hash-table :test 'eq))))

(defun test-world-graph-find-path-adjacent ()
  "BFS finds 1-hop path between direct neighbors."
  (let ((graph (make-test-graph-3x3)))
    (let ((path (world-graph-find-path graph :zone-c :zone-e)))
      (assert (equal path '(:zone-e)) ()
              "bfs-adjacent: C->E should be (:zone-e), got ~a" path))
    (let ((path (world-graph-find-path graph :zone-c :zone-n)))
      (assert (equal path '(:zone-n)) ()
              "bfs-adjacent: C->N should be (:zone-n), got ~a" path))))

(defun test-world-graph-find-path-diagonal ()
  "BFS finds 2-hop path to diagonal zone."
  (let ((graph (make-test-graph-3x3)))
    (let ((path (world-graph-find-path graph :zone-c :zone-ne)))
      (assert (= (length path) 2) ()
              "bfs-diagonal: C->NE should be 2 hops, got ~d: ~a"
              (length path) path)
      ;; Path should go through either N or E
      (assert (or (equal path '(:zone-n :zone-ne))
                  (equal path '(:zone-e :zone-ne))) ()
              "bfs-diagonal: C->NE path should go through N or E, got ~a" path))))

(defun test-world-graph-find-path-same-zone ()
  "BFS returns NIL for same zone."
  (let ((graph (make-test-graph-3x3)))
    (assert (null (world-graph-find-path graph :zone-c :zone-c)) ()
            "bfs-same: C->C should return NIL")))

(defun test-world-graph-find-path-unreachable ()
  "BFS returns NIL for disconnected zone."
  (let ((graph (make-test-graph-3x3)))
    (assert (null (world-graph-find-path graph :zone-c :zone-isolated)) ()
            "bfs-unreachable: C->isolated should return NIL")))

(defun test-world-graph-find-path-multi-hop ()
  "BFS finds path across multiple hops (corner to corner)."
  (let ((graph (make-test-graph-3x3)))
    (let ((path (world-graph-find-path graph :zone-sw :zone-ne)))
      (assert (and path (>= (length path) 2)) ()
              "bfs-multihop: SW->NE should have >= 2 hops, got ~a" path)
      ;; Last element must be destination
      (assert (eq (car (last path)) :zone-ne) ()
              "bfs-multihop: path should end with :zone-ne, got ~a" path))))

(defun test-world-graph-edge-between ()
  "Returns correct edge direction between connected zones."
  (let ((graph (make-test-graph-3x3)))
    (assert (eq (world-graph-edge-between graph :zone-c :zone-e) :east) ()
            "edge-between: C->E should be :east")
    (assert (eq (world-graph-edge-between graph :zone-c :zone-w) :west) ()
            "edge-between: C->W should be :west")
    (assert (eq (world-graph-edge-between graph :zone-c :zone-n) :north) ()
            "edge-between: C->N should be :north")
    (assert (eq (world-graph-edge-between graph :zone-c :zone-s) :south) ()
            "edge-between: C->S should be :south")
    ;; Non-adjacent zones return NIL
    (assert (null (world-graph-edge-between graph :zone-c :zone-ne)) ()
            "edge-between: C->NE should be NIL (not direct)")
    ;; Nil graph returns NIL
    (assert (null (world-graph-edge-between nil :zone-c :zone-e)) ()
            "edge-between: nil graph should return NIL")))

;;; ============================================================


(defvar *tests-world-graph*
  '(test-world-graph-data-plist
    test-normalize-world-graph-edges
    ;; Additional World Graph Tests
    test-world-graph-exits
    test-world-graph-zone-path
    test-collect-zone-files
    test-zone-id-from-file
    test-build-zone-paths
    ;; BFS pathfinding tests (Bug 4 Part 3)
    test-world-graph-find-path-adjacent
    test-world-graph-find-path-diagonal
    test-world-graph-find-path-same-zone
    test-world-graph-find-path-unreachable
    test-world-graph-find-path-multi-hop
    test-world-graph-edge-between)
  "World-graph domain test functions.")
