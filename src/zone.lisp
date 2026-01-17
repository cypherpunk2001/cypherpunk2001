;; NOTE: If you change behavior here, update docs/zone.md :)
(in-package #:mmorpg)

(defstruct (zone-chunk (:constructor %make-zone-chunk))
  ;; Chunk of tile data in chunk coordinates.
  x y tiles)

(defstruct (zone-layer (:constructor %make-zone-layer))
  ;; Layer of chunked tiles with an optional collision flag.
  id tileset-id collision-p chunks)

(defstruct (zone (:constructor %make-zone))
  ;; Zone metadata with layered chunked tiles.
  id chunk-size width height layers collision-tiles objects spawns)

(defun zone-label (zone)
  ;; Return a display label for ZONE.
  (let ((id (and zone (zone-id zone))))
    (if id
        (string-upcase (string id))
        "NONE")))

(defun zone-chunk-key (x y)
  ;; Pack chunk coordinates into a single integer key.
  (logior (ash (logand x #xffffffff) 32)
          (logand y #xffffffff)))

(defun tile-key (tx ty)
  ;; Pack tile coordinates into a single integer key.
  (logior (ash (logand tx #xffffffff) 32)
          (logand ty #xffffffff)))

(defun tile-key-x (key)
  ;; Extract X from a packed tile key.
  (ldb (byte 32 32) key))

(defun tile-key-y (key)
  ;; Extract Y from a packed tile key.
  (ldb (byte 32 0) key))

(defun read-zone-data (path)
  ;; Read a single zone data form without evaluation.
  ;; Returns zone data on success, NIL on failure (non-fatal).
  (let* ((full-path (if (and path (not (pathnamep path)))
                        (merge-pathnames path (asdf:system-source-directory :mmorpg))
                        path)))
    (when (and full-path (probe-file full-path))
      (handler-case
          (with-open-file (in full-path :direction :input)
            (with-standard-io-syntax
              (let ((*read-eval* nil))
                (read in nil nil))))
        (error (e)
          (warn "Failed to read zone data from ~a: ~a" full-path e)
          (log-verbose "Zone data read error: ~a" e)
          nil)))))

(defun zone-data-plist (data)
  ;; Normalize zone data to a plist.
  (cond
    ((and (listp data) (keywordp (first data))) data)
    ((and (listp data) (eq (first data) :zone)) (second data))
    (t nil)))

(defun ensure-tile-vector (tiles)
  ;; Normalize tile data into a vector.
  (cond
    ((null tiles) nil)
    ((vectorp tiles) tiles)
    ((listp tiles) (coerce tiles 'vector))
    (t (error "Zone tiles must be a list or vector: ~s" tiles))))

(defun build-tiles-from-fill (chunk-size fill overrides)
  ;; Build a tile vector from a fill value plus override coordinates.
  (let* ((count (* chunk-size chunk-size))
         (tiles (make-array count :initial-element fill)))
    (dolist (entry overrides)
      (destructuring-bind (x y value) entry
        (when (or (< x 0) (>= x chunk-size)
                  (< y 0) (>= y chunk-size))
          (error "Zone override out of bounds: ~s" entry))
        (setf (aref tiles (+ x (* y chunk-size))) value)))
    tiles))

(defun zone-chunk-from-spec (spec chunk-size)
  ;; Build a zone chunk from a plist spec.
  (let* ((x (getf spec :x))
         (y (getf spec :y))
         (tiles (ensure-tile-vector (getf spec :tiles))))
    (unless (and (numberp x) (numberp y))
      (error "Zone chunk missing :x or :y: ~s" spec))
    (unless tiles
      (let ((fill (getf spec :fill 0))
            (overrides (getf spec :overrides nil)))
        (setf tiles (build-tiles-from-fill chunk-size fill overrides))))
    (let ((expected (* chunk-size chunk-size)))
      (when (/= (length tiles) expected)
        (error "Zone chunk ~s,~s has ~d tiles (expected ~d)" x y (length tiles) expected)))
    (%make-zone-chunk :x x :y y :tiles tiles)))

(defun zone-layer-from-spec (spec chunk-size)
  ;; Build a zone layer from a plist spec.
  (let* ((id (getf spec :id))
         (tileset-id (getf spec :tileset nil))
         (collision-p (getf spec :collision nil))
         (chunks (make-hash-table :test 'eql))
         (chunk-specs (getf spec :chunks nil)))
    (dolist (chunk-spec chunk-specs)
      (let* ((chunk (zone-chunk-from-spec chunk-spec chunk-size))
             (key (zone-chunk-key (zone-chunk-x chunk) (zone-chunk-y chunk))))
        (setf (gethash key chunks) chunk)))
    (%make-zone-layer :id id
                      :tileset-id tileset-id
                      :collision-p collision-p
                      :chunks chunks)))

(defun build-zone-collision-tiles (layers chunk-size)
  ;; Build a hash of blocked tiles from collision layers.
  (let ((blocked (make-hash-table :test 'eql)))
    (dolist (layer layers)
      (when (zone-layer-collision-p layer)
        (maphash
         (lambda (_key chunk)
           (declare (ignore _key))
           (let* ((tiles (zone-chunk-tiles chunk))
                  (base-x (* (zone-chunk-x chunk) chunk-size))
                  (base-y (* (zone-chunk-y chunk) chunk-size))
                  (limit (length tiles)))
             (loop :for idx :from 0 :below limit
                   :for tile = (aref tiles idx)
                   :when (not (zerop tile))
                     :do (let ((local-x (mod idx chunk-size))
                               (local-y (floor idx chunk-size)))
                           (setf (gethash (tile-key (+ base-x local-x)
                                                    (+ base-y local-y))
                                          blocked)
                                 t)))))
         (zone-layer-chunks layer))))
    blocked))

(defun zone-wall-map (zone)
  ;; Convert collision tiles into a wall map array.
  (let* ((width (zone-width zone))
         (height (zone-height zone))
         (wall-map (make-array (list height width) :initial-element 0)))
    (maphash
     (lambda (key _value)
       (declare (ignore _value))
       (let ((tx (tile-key-x key))
             (ty (tile-key-y key)))
         (when (and (<= 0 tx) (< tx width)
                    (<= 0 ty) (< ty height))
           (setf (aref wall-map ty tx) 1))))
     (zone-collision-tiles zone))
    wall-map))

(defun zone-layer-tile-at (layer chunk-size tx ty)
  ;; Return the tile index for a layer at tile coordinate TX/TY.
  (let* ((cx (floor tx chunk-size))
         (cy (floor ty chunk-size))
         (key (zone-chunk-key cx cy))
         (chunk (gethash key (zone-layer-chunks layer))))
    (if chunk
        (let* ((local-x (- tx (* cx chunk-size)))
               (local-y (- ty (* cy chunk-size)))
               (idx (+ local-x (* local-y chunk-size))))
          (aref (zone-chunk-tiles chunk) idx))
        0)))

(defun zone-tile-in-bounds-p (zone tx ty)
  ;; Return true when TX/TY lies within the zone dimensions.
  (and (<= 0 tx)
       (< tx (zone-width zone))
       (<= 0 ty)
       (< ty (zone-height zone))))

(defun zone-layer-by-id (zone id &optional tileset-id)
  ;; Find a layer by ID (and optional TILESET-ID) or return nil.
  (when zone
    (loop :for layer :across (zone-layers zone)
          :when (and (eql (zone-layer-id layer) id)
                     (if tileset-id
                         (eql (zone-layer-tileset-id layer) tileset-id)
                         t))
            :do (return layer))))

(defun append-zone-layer (zone layer)
  ;; Append LAYER to the zone's layer vector.
  (let* ((layers (zone-layers zone))
         (count (length layers))
         (next (make-array (1+ count))))
    (replace next layers)
    (setf (aref next count) layer
          (zone-layers zone) next)
    layer))

(defun ensure-zone-layer (zone id &key (collision-p nil) tileset-id)
  ;; Find or create a layer with ID, ensuring collision flag if provided.
  (let ((layer (zone-layer-by-id zone id tileset-id)))
    (cond
      (layer
       layer)
      (tileset-id
       (let ((fallback (loop :for layer :across (zone-layers zone)
                             :when (and (eql (zone-layer-id layer) id)
                                        (null (zone-layer-tileset-id layer)))
                               :do (return layer))))
         (if fallback
             (progn
               (setf (zone-layer-tileset-id fallback) tileset-id)
               fallback)
             (append-zone-layer
              zone
              (%make-zone-layer :id id
                                :tileset-id tileset-id
                                :collision-p collision-p
                                :chunks (make-hash-table :test 'eql))))))
      (t
       (append-zone-layer
        zone
        (%make-zone-layer :id id
                          :tileset-id tileset-id
                          :collision-p collision-p
                          :chunks (make-hash-table :test 'eql)))))))

(defun zone-layer-ensure-chunk (layer chunk-size cx cy)
  ;; Find or create a chunk in LAYER at chunk coordinates CX/CY.
  (let* ((key (zone-chunk-key cx cy))
         (chunks (zone-layer-chunks layer))
         (chunk (gethash key chunks)))
    (unless chunk
      (let ((tiles (make-array (* chunk-size chunk-size) :initial-element 0)))
        (setf chunk (%make-zone-chunk :x cx :y cy :tiles tiles)
              (gethash key chunks) chunk)))
    chunk))

(defun zone-layer-set-tile (layer chunk-size tx ty value)
  ;; Set a tile in LAYER at TX/TY to VALUE.
  (when (and (<= 0 tx) (<= 0 ty))
    (let* ((cx (floor tx chunk-size))
           (cy (floor ty chunk-size))
           (chunk (zone-layer-ensure-chunk layer chunk-size cx cy))
           (local-x (- tx (* cx chunk-size)))
           (local-y (- ty (* cy chunk-size)))
           (idx (+ local-x (* local-y chunk-size))))
      (setf (aref (zone-chunk-tiles chunk) idx) value))))

(defun zone-set-collision-tile (zone tx ty value)
  ;; Update the collision hash for a tile at TX/TY.
  (let ((key (tile-key tx ty))
        (tiles (zone-collision-tiles zone)))
    (if (and value (not (zerop value)))
        (setf (gethash key tiles) t)
        (remhash key tiles))))

(defun zone-remove-object-at (zone tx ty)
  ;; Remove any object at TX/TY from the zone.
  (setf (zone-objects zone)
        (remove-if (lambda (obj)
                     (and (eql (getf obj :x) tx)
                          (eql (getf obj :y) ty)))
                   (zone-objects zone))))

(defun zone-add-object (zone object)
  ;; Add OBJECT to the zone, replacing any at the same tile.
  (zone-remove-object-at zone (getf object :x) (getf object :y))
  (push object (zone-objects zone)))

(defun zone-remove-spawn-at (zone tx ty)
  ;; Remove any spawn at TX/TY from the zone.
  (setf (zone-spawns zone)
        (remove-if (lambda (spawn)
                     (and (eql (getf spawn :x) tx)
                          (eql (getf spawn :y) ty)))
                   (zone-spawns zone))))

(defun zone-add-spawn (zone spawn)
  ;; Add SPAWN to the zone, replacing any at the same tile.
  (zone-remove-spawn-at zone (getf spawn :x) (getf spawn :y))
  (push spawn (zone-spawns zone)))

(defun zone-chunk-spec (chunk chunk-size)
  ;; Serialize a chunk into a plist spec.
  (let* ((tiles (zone-chunk-tiles chunk))
         (count (length tiles))
         (fill (when (> count 0) (aref tiles 0)))
         (uniform (and fill
                       (loop :for idx :from 1 :below count
                             :always (= (aref tiles idx) fill)))))
    (list :x (zone-chunk-x chunk)
          :y (zone-chunk-y chunk)
          :tiles (if uniform
                     nil
                     (coerce tiles 'list))
          :fill (if uniform fill nil))))

(defun zone-layer-spec (layer chunk-size)
  ;; Serialize a layer into a plist spec.
  (let ((chunk-specs nil)
        (tileset-id (zone-layer-tileset-id layer)))
    (maphash (lambda (_key chunk)
               (declare (ignore _key))
               (push (zone-chunk-spec chunk chunk-size) chunk-specs))
             (zone-layer-chunks layer))
    (if tileset-id
        (list :id (zone-layer-id layer)
              :tileset tileset-id
              :collision (zone-layer-collision-p layer)
              :chunks (nreverse chunk-specs))
        (list :id (zone-layer-id layer)
              :collision (zone-layer-collision-p layer)
              :chunks (nreverse chunk-specs)))))

(defun zone-to-plist (zone)
  ;; Serialize a zone into a plist suitable for writing.
  (let* ((chunk-size (zone-chunk-size zone))
         (layers (loop :for layer :across (zone-layers zone)
                       :collect (zone-layer-spec layer chunk-size)))
         (objects (loop :for obj :in (zone-objects zone)
                        :for id = (getf obj :id)
                        :for tx = (getf obj :x)
                        :for ty = (getf obj :y)
                        :for count = (getf obj :count nil)
                        :when (and id (numberp tx) (numberp ty))
                          :collect (if (and count (numberp count) (> count 0))
                                       (list :id id :x tx :y ty :count count)
                                       (list :id id :x tx :y ty)))))
    (list :id (zone-id zone)
          :chunk-size chunk-size
          :width (zone-width zone)
          :height (zone-height zone)
          :layers layers
          :objects objects
          :spawns (zone-spawns zone))))

(defun zone-slice (zone min-x min-y width height)
  ;; Return a new zone containing the tiles inside the given region.
  (let* ((chunk-size (zone-chunk-size zone))
         (max-x (+ min-x (1- width)))
         (max-y (+ min-y (1- height)))
         (new-layers (make-array (length (zone-layers zone))))
         (objects nil)
         (spawns nil))
    (loop :for idx :from 0
          :for layer :across (zone-layers zone)
          :do (let ((new-layer (%make-zone-layer
                                :id (zone-layer-id layer)
                                :tileset-id (zone-layer-tileset-id layer)
                                :collision-p (zone-layer-collision-p layer)
                                :chunks (make-hash-table :test 'eql))))
                (loop :for ty :from min-y :to max-y
                      :for local-y :from 0
                      :do (loop :for tx :from min-x :to max-x
                                :for local-x :from 0
                                :for tile = (zone-layer-tile-at layer chunk-size tx ty)
                                :when (not (zerop tile))
                                  :do (zone-layer-set-tile new-layer chunk-size
                                                           local-x local-y tile)))
                (setf (aref new-layers idx) new-layer)))
    (dolist (obj (zone-objects zone))
      (let ((tx (getf obj :x))
            (ty (getf obj :y))
            (count (getf obj :count nil)))
        (when (and (<= min-x tx) (<= tx max-x)
                   (<= min-y ty) (<= ty max-y))
          (push (if (and count (numberp count) (> count 0))
                    (list :id (getf obj :id)
                          :x (- tx min-x)
                          :y (- ty min-y)
                          :count count)
                    (list :id (getf obj :id)
                          :x (- tx min-x)
                          :y (- ty min-y)))
                objects))))
    (dolist (spawn (zone-spawns zone))
      (let ((tx (getf spawn :x))
            (ty (getf spawn :y)))
        (when (and (<= min-x tx) (<= tx max-x)
                   (<= min-y ty) (<= ty max-y))
          (push (list :id (getf spawn :id)
                      :x (- tx min-x)
                      :y (- ty min-y))
                spawns))))
    (%make-zone :id (zone-id zone)
                :chunk-size chunk-size
                :width width
                :height height
                :layers new-layers
                :collision-tiles (build-zone-collision-tiles (coerce new-layers 'list)
                                                             chunk-size)
                :objects (nreverse objects)
                :spawns (nreverse spawns))))

(defun make-empty-zone (id width height &key (chunk-size *zone-default-chunk-size*))
  ;; Create a blank zone with no layers or placed content.
  (%make-zone :id id
              :chunk-size chunk-size
              :width width
              :height height
              :layers (make-array 0)
              :collision-tiles (make-hash-table :test 'eql)
              :objects nil
              :spawns nil))

(defun zone-resize (zone width height)
  ;; Return ZONE resized to WIDTH/HEIGHT tiles, preserving existing content.
  (zone-slice zone 0 0 width height))

(defun resolve-zone-path (path)
  ;; Resolve PATH relative to the system root if needed.
  (cond
    ((null path) nil)
    ((and (pathnamep path) (uiop:absolute-pathname-p path)) path)
    (t (merge-pathnames path (asdf:system-source-directory :mmorpg)))))

(defun write-zone (zone path)
  ;; Write ZONE data to PATH, ensuring the destination directory exists.
  ;; Returns T on success, NIL on failure (non-fatal).
  (let* ((full-path (resolve-zone-path path)))
    (when full-path
      (handler-case
          (progn
            (ensure-directories-exist full-path)
            (with-open-file (out full-path
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
              (with-standard-io-syntax
                (let ((*print-pretty* t)
                      (*print-right-margin* 100))
                  (pprint (zone-to-plist zone) out))))
            (log-verbose "Zone saved: ~a" full-path)
            t)
        (error (e)
          (warn "Failed to write zone to ~a: ~a" full-path e)
          (log-verbose "Zone write error: ~a" e)
          nil)))))

(defun load-zone (path)
  ;; Load a zone from PATH, returning a zone struct or nil.
  (let* ((data (read-zone-data path))
         (plist (zone-data-plist data)))
    (when plist
      (handler-case
          (let* ((id (getf plist :id))
                 (chunk-size (or (getf plist :chunk-size) 32))
                 (width (getf plist :width))
                 (height (getf plist :height))
                 (layer-specs (getf plist :layers nil))
                 (layers (mapcar (lambda (spec)
                                   (zone-layer-from-spec spec chunk-size))
                                 layer-specs))
                 (objects (getf plist :objects nil))
                 (spawns (getf plist :spawns nil))
                 (collision-tiles (build-zone-collision-tiles layers chunk-size)))
            (unless (and (numberp width) (numberp height))
              (error "Zone requires :width and :height: ~s" plist))
            (log-verbose "Zone loaded: ~a id=~a size=~dx~d layers=~d"
                         (or path "<memory>")
                         id
                         width
                         height
                         (length layers))
            (%make-zone :id id
                        :chunk-size chunk-size
                        :width width
                        :height height
                        :layers (coerce layers 'vector)
                        :collision-tiles collision-tiles
                        :objects objects
                        :spawns spawns))
        (error (e)
          (warn "Failed to load zone from ~a: ~a" path e)
          (log-verbose "Zone load error: ~a" e)
          nil)))))
