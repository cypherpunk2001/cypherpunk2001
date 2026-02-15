(in-package #:mmorpg)

;;; ============================================================
;;; ZONE.LISP TESTS
;;; ============================================================

(defun test-zone-chunk-key ()
  "Test zone chunk key packing."
  (let ((key1 (zone-chunk-key 0 0))
        (key2 (zone-chunk-key 1 0))
        (key3 (zone-chunk-key 0 1)))
    (assert (integerp key1) () "chunk-key: returns integer")
    (assert (/= key1 key2) () "chunk-key: different x")
    (assert (/= key1 key3) () "chunk-key: different y")
    (assert (/= key2 key3) () "chunk-key: all unique")))

(defun test-tile-key-roundtrip ()
  "Test tile key packing and unpacking."
  (dolist (coords '((0 0) (10 20) (100 200) (1000 2000)))
    (let* ((x (first coords))
           (y (second coords))
           (key (tile-key x y))
           (back-x (tile-key-x key))
           (back-y (tile-key-y key)))
      (assert (= x back-x) () (format nil "tile-key roundtrip x: ~d" x))
      (assert (= y back-y) () (format nil "tile-key roundtrip y: ~d" y)))))

(defun test-zone-tile-in-bounds-p ()
  "Test zone bounds checking."
  ;; Use %make-zone directly since make-zone may not exist
  (let ((zone (%make-zone :id :test :width 10 :height 10)))
    (assert (zone-tile-in-bounds-p zone 0 0) () "zone-bounds: origin")
    (assert (zone-tile-in-bounds-p zone 5 5) () "zone-bounds: center")
    (assert (zone-tile-in-bounds-p zone 9 9) () "zone-bounds: max valid")
    (assert (not (zone-tile-in-bounds-p zone 10 5)) () "zone-bounds: x out")
    (assert (not (zone-tile-in-bounds-p zone 5 10)) () "zone-bounds: y out")
    (assert (not (zone-tile-in-bounds-p zone -1 5)) () "zone-bounds: negative x")))

;;; ============================================================

;;; NEW ZONE TESTS (Priority 2)
;;; ============================================================

(defun test-zone-label ()
  "Test zone label generation."
  (let ((zone (%make-zone :id :test-zone :width 10 :height 10)))
    (let ((label (zone-label zone)))
      (assert (stringp label) () "zone-label: returns string")
      (assert (string= label "TEST-ZONE") () "zone-label: uppercase")))
  ;; Nil zone
  (let ((label (zone-label nil)))
    (assert (string= label "NONE") () "zone-label: nil -> NONE")))

(defun test-zone-data-plist ()
  "Test zone data plist normalization."
  ;; Direct plist
  (let ((result (zone-data-plist '(:id :test :width 10))))
    (assert (listp result) () "zone-plist: direct returns list")
    (assert (eq (getf result :id) :test) () "zone-plist: preserves id"))
  ;; Nil data
  (assert (null (zone-data-plist nil)) () "zone-plist: nil -> nil")
  ;; Invalid data
  (assert (null (zone-data-plist '("not" "a" "plist"))) () "zone-plist: invalid -> nil"))

(defun test-make-empty-zone ()
  "Test empty zone creation."
  (let ((zone (make-empty-zone :empty-test 20 15)))
    (assert (eq (zone-id zone) :empty-test) () "empty-zone: id")
    (assert (= (zone-width zone) 20) () "empty-zone: width")
    (assert (= (zone-height zone) 15) () "empty-zone: height")
    (assert (= (length (zone-layers zone)) 0) () "empty-zone: no layers")
    (assert (null (zone-objects zone)) () "empty-zone: no objects")))

(defun test-build-tiles-from-fill ()
  "Test tile vector building from fill value."
  ;; All same value
  (let ((tiles (build-tiles-from-fill 4 5 nil)))
    (assert (= (length tiles) 16) () "fill-tiles: 4x4 = 16")
    (assert (every (lambda (tile) (= tile 5)) tiles) () "fill-tiles: all 5"))
  ;; With overrides
  (let ((tiles (build-tiles-from-fill 4 0 '((0 0 1) (1 1 2)))))
    (assert (= (aref tiles 0) 1) () "fill-tiles: override at 0,0")
    (assert (= (aref tiles 5) 2) () "fill-tiles: override at 1,1")
    (assert (= (aref tiles 2) 0) () "fill-tiles: non-override is fill")))

(defun test-zone-layer-tile-at ()
  "Test getting tile at coordinates from layer."
  ;; Create a simple layer with one chunk
  (let* ((chunk (%make-zone-chunk :x 0 :y 0
                                   :tiles (make-array 16 :initial-contents
                                                      '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))))
         (chunks (make-hash-table :test 'eql))
         (layer nil))
    (setf (gethash (zone-chunk-key 0 0) chunks) chunk)
    (setf layer (%make-zone-layer :id :ground :chunks chunks))
    ;; Test tile retrieval (chunk-size 4)
    (assert (= (zone-layer-tile-at layer 4 0 0) 1) () "layer-tile: 0,0 = 1")
    (assert (= (zone-layer-tile-at layer 4 1 0) 2) () "layer-tile: 1,0 = 2")
    (assert (= (zone-layer-tile-at layer 4 0 1) 5) () "layer-tile: 0,1 = 5")
    ;; Out of chunk returns 0
    (assert (= (zone-layer-tile-at layer 4 10 10) 0) () "layer-tile: out of chunk = 0")))

;;; ============================================================

;;; ADDITIONAL ZONE TESTS
;;; ============================================================

(defun test-zone-chunk-from-spec ()
  "Test zone chunk creation from spec."
  ;; With explicit tiles
  (let* ((tiles (make-list 16 :initial-element 1))
         (spec (list :x 2 :y 3 :tiles tiles))
         (chunk (zone-chunk-from-spec spec 4)))
    (assert (= (zone-chunk-x chunk) 2) () "chunk-from-spec: x")
    (assert (= (zone-chunk-y chunk) 3) () "chunk-from-spec: y")
    (assert (= (length (zone-chunk-tiles chunk)) 16) () "chunk-from-spec: tiles"))
  ;; With fill value
  (let* ((spec '(:x 0 :y 0 :fill 5))
         (chunk (zone-chunk-from-spec spec 4)))
    (assert (every (lambda (tile) (= tile 5)) (zone-chunk-tiles chunk)) ()
            "chunk-from-spec: fill value")))

(defun test-zone-layer-from-spec ()
  "Test zone layer creation from spec."
  (let* ((chunk-spec '(:x 0 :y 0 :fill 1))
         (spec (list :id :ground
                     :tileset :grass
                     :collision nil
                     :chunks (list chunk-spec)))
         (layer (zone-layer-from-spec spec 4)))
    (assert (eq (zone-layer-id layer) :ground) () "layer-from-spec: id")
    (assert (eq (zone-layer-tileset-id layer) :grass) () "layer-from-spec: tileset")
    (assert (hash-table-p (zone-layer-chunks layer)) () "layer-from-spec: chunks hash")))

(defun test-build-zone-collision-tiles ()
  "Test building collision tile hash from layers."
  ;; Create a collision layer with some blocked tiles
  (let* ((chunk (%make-zone-chunk :x 0 :y 0
                                   :tiles (make-array 16 :initial-contents
                                                      '(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
         (chunks (make-hash-table :test 'eql))
         (layer nil))
    (setf (gethash (zone-chunk-key 0 0) chunks) chunk)
    (setf layer (%make-zone-layer :id :collision :collision-p t :chunks chunks))
    (let ((blocked (build-zone-collision-tiles (list layer) 4)))
      (assert (hash-table-p blocked) () "collision-tiles: returns hash")
      ;; Tile at 1,0 should be blocked (non-zero in tiles array)
      (assert (gethash (tile-key 1 0) blocked) () "collision-tiles: 1,0 blocked")
      ;; Tile at 0,0 should not be blocked (zero)
      (assert (not (gethash (tile-key 0 0) blocked)) () "collision-tiles: 0,0 not blocked"))))

(defun test-zone-wall-map ()
  "Test converting collision tiles to wall map array."
  (let* ((zone (make-empty-zone :test 10 10))
         (collision (zone-collision-tiles zone)))
    ;; Mark some tiles as blocked
    (setf (gethash (tile-key 2 3) collision) t)
    (setf (gethash (tile-key 5 5) collision) t)
    (let ((wall-map (zone-wall-map zone)))
      (assert (arrayp wall-map) () "wall-map: returns array")
      (assert (= (array-dimension wall-map 0) 10) () "wall-map: height")
      (assert (= (array-dimension wall-map 1) 10) () "wall-map: width")
      (assert (= (aref wall-map 3 2) 1) () "wall-map: 2,3 blocked")
      (assert (= (aref wall-map 5 5) 1) () "wall-map: 5,5 blocked")
      (assert (= (aref wall-map 0 0) 0) () "wall-map: 0,0 not blocked"))))

(defun test-zone-layer-by-id ()
  "Test finding layer by ID."
  (let* ((layer1 (%make-zone-layer :id :ground :chunks (make-hash-table)))
         (layer2 (%make-zone-layer :id :collision :collision-p t :chunks (make-hash-table)))
         (zone (%make-zone :id :test :width 10 :height 10
                           :layers (vector layer1 layer2)
                           :collision-tiles (make-hash-table))))
    (assert (eq (zone-layer-by-id zone :ground) layer1) () "layer-by-id: ground")
    (assert (eq (zone-layer-by-id zone :collision) layer2) () "layer-by-id: collision")
    (assert (null (zone-layer-by-id zone :nonexistent)) () "layer-by-id: not found")))

(defun test-zone-to-plist ()
  "Test zone serialization to plist.
   Task 5.5: Updated to use zone-object structs."
  (let* ((zone (make-empty-zone :test-zone 20 15 :chunk-size 8)))
    ;; Add an object (using zone-object struct)
    (push (%make-zone-object :id :coins :x 5 :y 5 :count 10 :base-count 10
                             :respawn 0.0 :respawnable t :snapshot-dirty nil)
          (zone-objects zone))
    (let ((plist (zone-to-plist zone)))
      (assert (listp plist) () "zone-to-plist: returns list")
      (assert (eq (getf plist :id) :test-zone) () "zone-to-plist: id")
      (assert (= (getf plist :width) 20) () "zone-to-plist: width")
      (assert (= (getf plist :height) 15) () "zone-to-plist: height")
      (assert (= (getf plist :chunk-size) 8) () "zone-to-plist: chunk-size")
      (assert (listp (getf plist :objects)) () "zone-to-plist: objects list"))))

(defun test-zone-slice ()
  "Test extracting a subregion of a zone."
  (let* ((zone (make-empty-zone :big-zone 100 100))
         (sliced (zone-slice zone 10 10 20 15)))
    (assert (= (zone-width sliced) 20) () "zone-slice: width")
    (assert (= (zone-height sliced) 15) () "zone-slice: height")
    (assert (eq (zone-id sliced) :big-zone) () "zone-slice: preserves id")))

(defun test-zone-resize ()
  "Test resizing a zone."
  (let* ((zone (make-empty-zone :resizable 50 50))
         (resized (zone-resize zone 30 25)))
    (assert (= (zone-width resized) 30) () "zone-resize: new width")
    (assert (= (zone-height resized) 25) () "zone-resize: new height")))

;;; ============================================================

;;; FINAL ZONE TESTS
;;; ============================================================

(defun test-load-write-zone-roundtrip ()
  "Test zone save and load roundtrip.
   Task 5.5: Updated to use zone-object structs."
  (let* ((temp-path (merge-pathnames "test-zone-roundtrip.lisp"
                                      (uiop:temporary-directory)))
         (zone (make-empty-zone :test-roundtrip 25 20 :chunk-size 8)))
    ;; Add some content (zone objects now use structs)
    (push (%make-zone-object :id :coins :x 5 :y 5 :count 10 :base-count 10
                             :respawn 0.0 :respawnable t :snapshot-dirty nil)
          (zone-objects zone))
    (push '(:id :spawn :x 10 :y 10) (zone-spawns zone))
    ;; Write
    (unwind-protect
        (progn
          (write-zone zone temp-path)
          (assert (probe-file temp-path) () "roundtrip: file written")
          ;; Load
          (let ((loaded (load-zone temp-path)))
            (assert loaded () "roundtrip: zone loaded")
            (assert (eq (zone-id loaded) :test-roundtrip) () "roundtrip: id preserved")
            (assert (= (zone-width loaded) 25) () "roundtrip: width preserved")
            (assert (= (zone-height loaded) 20) () "roundtrip: height preserved")
            (assert (= (zone-chunk-size loaded) 8) () "roundtrip: chunk-size preserved")))
      ;; Cleanup
      (when (probe-file temp-path)
        (delete-file temp-path)))))

;;; ============================================================

(defun test-zone-add-object ()
  "Test zone-add-object adds a zone-object struct and replaces duplicates."
  (let ((zone (%make-zone :id :test :width 10 :height 10)))
    ;; Add first object
    (let ((obj (%make-zone-object :id :coins :x 3 :y 4 :count 5 :base-count 5
                                  :respawn 0.0 :respawnable t :snapshot-dirty nil)))
      (zone-add-object zone obj)
      (assert (= (length (zone-objects zone)) 1) () "add-object: one object")
      (assert (eq (zone-object-id (first (zone-objects zone))) :coins) ()
              "add-object: correct id")
      (assert (= (zone-object-x (first (zone-objects zone))) 3) ()
              "add-object: correct x"))
    ;; Add second object at different tile
    (let ((obj2 (%make-zone-object :id :bones :x 5 :y 6 :count 1 :base-count 1
                                   :respawn 0.0 :respawnable t :snapshot-dirty nil)))
      (zone-add-object zone obj2)
      (assert (= (length (zone-objects zone)) 2) () "add-object: two objects"))
    ;; Replace object at same tile
    (let ((obj3 (%make-zone-object :id :arrows :x 3 :y 4 :count 10 :base-count 10
                                   :respawn 0.0 :respawnable t :snapshot-dirty nil)))
      (zone-add-object zone obj3)
      (assert (= (length (zone-objects zone)) 2) () "add-object: replaced, still two")
      ;; The one at 3,4 should now be :arrows
      (let ((found (find-if (lambda (o) (and (= (zone-object-x o) 3)
                                             (= (zone-object-y o) 4)))
                            (zone-objects zone))))
        (assert (eq (zone-object-id found) :arrows) () "add-object: replaced id")))))

(defun test-zone-remove-object-at ()
  "Test zone-remove-object-at removes object by tile coordinates."
  (let ((zone (%make-zone :id :test :width 10 :height 10)))
    (zone-add-object zone (%make-zone-object :id :coins :x 3 :y 4 :count 1 :base-count 1
                                             :respawn 0.0 :respawnable t :snapshot-dirty nil))
    (zone-add-object zone (%make-zone-object :id :bones :x 5 :y 6 :count 1 :base-count 1
                                             :respawn 0.0 :respawnable t :snapshot-dirty nil))
    (assert (= (length (zone-objects zone)) 2) () "remove-object: starts with 2")
    ;; Remove one
    (zone-remove-object-at zone 3 4)
    (assert (= (length (zone-objects zone)) 1) () "remove-object: now 1")
    (assert (eq (zone-object-id (first (zone-objects zone))) :bones) ()
            "remove-object: correct one remains")
    ;; Remove non-existent (no-op)
    (zone-remove-object-at zone 99 99)
    (assert (= (length (zone-objects zone)) 1) () "remove-object: no-op on missing")))

;;; ============================================================


(defvar *tests-zone*
  '(test-zone-chunk-key
    test-tile-key-roundtrip
    test-zone-tile-in-bounds-p
    test-zone-label
    test-zone-data-plist
    test-make-empty-zone
    test-build-tiles-from-fill
    test-zone-layer-tile-at
    ;; Additional Zone Tests
    test-zone-chunk-from-spec
    test-zone-layer-from-spec
    test-build-zone-collision-tiles
    test-zone-wall-map
    test-zone-layer-by-id
    test-zone-to-plist
    test-zone-slice
    test-zone-resize
    ;; Final Zone Tests
    test-load-write-zone-roundtrip
    ;; Object management tests
    test-zone-add-object
    test-zone-remove-object-at)
  "Zone domain test functions.")
