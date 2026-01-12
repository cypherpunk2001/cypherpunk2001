;; NOTE: If you change behavior here, update docs/map.md :)
(in-package #:mmorpg)

(defparameter +tmx-flip-horizontal+ #x80000000)
(defparameter +tmx-flip-vertical+ #x40000000)
(defparameter +tmx-flip-diagonal+ #x20000000)
(defparameter +tmx-gid-mask+ #x1fffffff)

(defstruct (map-tileset (:constructor %make-map-tileset))
  ;; Tileset metadata for TMX rendering.
  firstgid name tilewidth tileheight columns image-source texture)

(defstruct (map-chunk (:constructor %make-map-chunk))
  ;; Chunk of tiles in tile coordinates.
  x y width height tiles nonzero-indices)

(defstruct (map-layer (:constructor %make-map-layer))
  ;; Tile layer with chunked data.
  name width height chunks)

(defstruct (map-data (:constructor %make-map-data))
  ;; Loaded TMX map data and derived bounds.
  tilewidth tileheight width height
  layers tilesets
  min-x min-y max-x max-y
  collision-tiles)

(defun tmx-clean-gid (gid)
  ;; Strip TMX flip flags from a gid.
  (logand gid +tmx-gid-mask+))

(defun tile-key (tx ty)
  ;; Pack tile coordinates into a single integer key.
  (logior (ash (logand tx #xffffffff) 32)
          (logand ty #xffffffff)))

(defun read-file-string (path)
  ;; Read a file into a string.
  (with-open-file (in path :direction :input)
    (let* ((size (file-length in))
           (buffer (make-string size)))
      (read-sequence buffer in)
      buffer)))

(defun tag-attribute (tag name)
  ;; Extract attribute NAME from a tag string.
  (let* ((needle (format nil "~a=\"" name))
         (start (search needle tag)))
    (when start
      (let* ((value-start (+ start (length needle)))
             (value-end (position #\" tag :start value-start)))
        (when value-end
          (subseq tag value-start value-end))))))

(defun parse-int (text)
  ;; Parse TEXT into an integer, returning 0 when missing.
  (if text
      (parse-integer text)
      0))

(defun parse-csv-ints (text)
  ;; Parse CSV integer data into a vector.
  (let* ((len (length text))
         (values (make-array 0 :adjustable t :fill-pointer 0))
         (sign 1)
         (acc 0)
         (in-number nil))
    (labels ((emit ()
               (when in-number
                 (vector-push-extend (* sign acc) values)
                 (setf acc 0
                       sign 1
                       in-number nil))))
      (loop :for idx :from 0 :below len
            :for ch = (char text idx)
            :do (cond
                  ((and (char= ch #\-) (not in-number))
                   (setf sign -1
                         in-number t))
                  ((and (char>= ch #\0) (char<= ch #\9))
                   (setf acc (+ (* acc 10) (- (char-code ch) (char-code #\0)))
                         in-number t))
                  (t
                   (emit))))
      (emit))
    (coerce values 'vector)))

(defun chunk-nonzero-indices (tiles)
  ;; Precompute a vector of indices for nonzero tiles.
  (let ((indices (make-array 0 :adjustable t :fill-pointer 0)))
    (loop :for i :from 0 :below (length tiles)
          :for gid = (aref tiles i)
          :when (not (zerop gid))
          :do (vector-push-extend i indices))
    (coerce indices 'vector)))

(defun parse-tilesets (text base-path)
  ;; Parse tilesets from TMX data.
  (let ((tilesets (make-array 0 :adjustable t :fill-pointer 0))
        (pos 0))
    (loop :for start = (search "<tileset" text :start2 pos)
          :while start
          :do (let* ((tag-end (position #\> text :start start))
                     (tag (subseq text start (1+ tag-end)))
                     (close (search "</tileset>" text :start2 tag-end))
                     (block (subseq text start close))
                     (firstgid (parse-int (tag-attribute tag "firstgid")))
                     (name (tag-attribute tag "name"))
                     (tilewidth (parse-int (tag-attribute tag "tilewidth")))
                     (tileheight (parse-int (tag-attribute tag "tileheight")))
                     (columns (parse-int (tag-attribute tag "columns")))
                     (image-tag-start (search "<image" block))
                     (image-tag-end (and image-tag-start
                                         (position #\> block :start image-tag-start)))
                     (image-tag (and image-tag-start image-tag-end
                                     (subseq block image-tag-start (1+ image-tag-end))))
                     (image-source (tag-attribute image-tag "source"))
                     (image-path (when image-source
                                   (merge-pathnames image-source base-path))))
                (vector-push-extend
                 (%make-map-tileset :firstgid firstgid
                                    :name name
                                    :tilewidth tilewidth
                                    :tileheight tileheight
                                    :columns columns
                                    :image-source (when image-path (namestring image-path)))
                 tilesets)
                (setf pos (+ close (length "</tileset>")))))
    (coerce tilesets 'vector)))

(defun parse-chunk (chunk-tag chunk-body)
  ;; Parse a <chunk> tag with CSV data.
  (let* ((x (parse-int (tag-attribute chunk-tag "x")))
         (y (parse-int (tag-attribute chunk-tag "y")))
         (width (parse-int (tag-attribute chunk-tag "width")))
         (height (parse-int (tag-attribute chunk-tag "height")))
         (tiles-raw (parse-csv-ints chunk-body))
         (tiles (make-array (length tiles-raw))))
    (loop :for i :from 0 :below (length tiles-raw)
          :do (setf (aref tiles i) (tmx-clean-gid (aref tiles-raw i))))
    (%make-map-chunk :x x
                     :y y
                     :width width
                     :height height
                     :tiles tiles
                     :nonzero-indices (chunk-nonzero-indices tiles))))

(defun parse-layer (layer-text)
  ;; Parse a TMX layer into chunks.
  (let* ((tag-end (position #\> layer-text))
         (tag (subseq layer-text 0 (1+ tag-end)))
         (name (tag-attribute tag "name"))
         (width (parse-int (tag-attribute tag "width")))
         (height (parse-int (tag-attribute tag "height")))
         (data-start (search "<data" layer-text))
         (data-tag-end (and data-start (position #\> layer-text :start data-start)))
         (data-end (and data-tag-end (search "</data>" layer-text :start2 data-tag-end)))
         (data-body (and data-tag-end data-end
                         (subseq layer-text (1+ data-tag-end) data-end)))
         (chunks (make-array 0 :adjustable t :fill-pointer 0)))
    (when data-body
      (let ((chunk-pos 0)
            (chunk-found nil))
        (loop :for chunk-start = (search "<chunk" data-body :start2 chunk-pos)
              :while chunk-start
              :do (let* ((chunk-tag-end (position #\> data-body :start chunk-start))
                         (chunk-tag (subseq data-body chunk-start (1+ chunk-tag-end)))
                         (chunk-close (search "</chunk>" data-body :start2 chunk-tag-end))
                         (chunk-body (subseq data-body (1+ chunk-tag-end) chunk-close)))
                    (vector-push-extend (parse-chunk chunk-tag chunk-body) chunks)
                    (setf chunk-pos (+ chunk-close (length "</chunk>"))
                          chunk-found t)))
        (unless chunk-found
          (let* ((tiles-raw (parse-csv-ints data-body))
                 (tiles (make-array (length tiles-raw))))
            (loop :for i :from 0 :below (length tiles-raw)
                  :do (setf (aref tiles i) (tmx-clean-gid (aref tiles-raw i))))
            (vector-push-extend
             (%make-map-chunk :x 0 :y 0
                              :width width
                              :height height
                              :tiles tiles
                              :nonzero-indices (chunk-nonzero-indices tiles))
             chunks)))))
    (%make-map-layer :name name
                     :width width
                     :height height
                     :chunks (coerce chunks 'vector))))

(defun parse-layers (text)
  ;; Parse all TMX layers from the document.
  (let ((layers (make-array 0 :adjustable t :fill-pointer 0))
        (pos 0))
    (loop :for start = (search "<layer" text :start2 pos)
          :while start
          :do (let* ((close (search "</layer>" text :start2 start))
                     (block (subseq text start close)))
                (vector-push-extend (parse-layer block) layers)
                (setf pos (+ close (length "</layer>")))))
    (coerce layers 'vector)))

(defun map-bounds (layers)
  ;; Compute map bounds in tile coordinates.
  (let ((min-x nil)
        (min-y nil)
        (max-x nil)
        (max-y nil))
    (loop :for layer :across layers
          :do (loop :for chunk :across (map-layer-chunks layer)
                    :for left = (map-chunk-x chunk)
                    :for top = (map-chunk-y chunk)
                    :for right = (+ left (map-chunk-width chunk) -1)
                    :for bottom = (+ top (map-chunk-height chunk) -1)
                    :do (setf min-x (if min-x (min min-x left) left)
                              min-y (if min-y (min min-y top) top)
                              max-x (if max-x (max max-x right) right)
                              max-y (if max-y (max max-y bottom) bottom))))
    (values (or min-x 0) (or min-y 0) (or max-x 0) (or max-y 0))))

(defun map-tile-outside-bounds-p (map tx ty)
  ;; Return true when tile coordinates fall outside the map bounds.
  (and map
       (or (< tx (map-data-min-x map))
           (> tx (map-data-max-x map))
           (< ty (map-data-min-y map))
           (> ty (map-data-max-y map)))))

(defun load-tmx-map (path collision-layers)
  ;; Load a TMX map from PATH and return map-data.
  (let* ((full-path (if (and path (not (pathnamep path)))
                        (merge-pathnames path (asdf:system-source-directory :mmorpg))
                        path))
         (namestring (when full-path (namestring full-path))))
    (when (and namestring (probe-file namestring))
      (let* ((text (read-file-string namestring))
             (map-tag-start (search "<map" text))
             (map-tag-end (and map-tag-start (position #\> text :start map-tag-start)))
             (map-tag (and map-tag-start map-tag-end
                           (subseq text map-tag-start (1+ map-tag-end))))
             (width (parse-int (tag-attribute map-tag "width")))
             (height (parse-int (tag-attribute map-tag "height")))
             (tilewidth (parse-int (tag-attribute map-tag "tilewidth")))
             (tileheight (parse-int (tag-attribute map-tag "tileheight")))
             (base-path (pathname-directory full-path))
             (tilesets (parse-tilesets text (make-pathname :directory base-path)))
             (layers (parse-layers text))
             (sorted-tilesets (sort tilesets #'< :key #'map-tileset-firstgid)))
        (multiple-value-bind (min-x min-y max-x max-y)
            (map-bounds layers)
          (let ((collision-tiles (build-collision-tiles layers collision-layers)))
            (%make-map-data :tilewidth tilewidth
                            :tileheight tileheight
                            :width width
                            :height height
                            :layers layers
                            :tilesets sorted-tilesets
                            :min-x min-x
                            :min-y min-y
                            :max-x max-x
                            :max-y max-y
                            :collision-tiles collision-tiles)))))))

(defun build-collision-tiles (layers collision-layers)
  ;; Build a hash of blocked tiles for collision layers.
  (when collision-layers
    (let ((blocked (make-hash-table :test 'eql)))
      (loop :for layer :across layers
            :when (find (map-layer-name layer) collision-layers
                        :test #'string-equal)
            :do (loop :for chunk :across (map-layer-chunks layer)
                      :for tiles = (map-chunk-tiles chunk)
                      :for width = (map-chunk-width chunk)
                      :do (loop :for idx :across (map-chunk-nonzero-indices chunk)
                                :for local-x = (mod idx width)
                                :for local-y = (floor idx width)
                                :for tx = (+ (map-chunk-x chunk) local-x)
                                :for ty = (+ (map-chunk-y chunk) local-y)
                                :do (setf (gethash (tile-key tx ty) blocked) t))))
      blocked)))

(defun map-tileset-for-gid (tilesets gid)
  ;; Find the tileset containing GID.
  (let ((result nil))
    (loop :for tileset :across tilesets
          :do (when (<= (map-tileset-firstgid tileset) gid)
                (setf result tileset)))
    result))

(defun map-tile-index (tileset gid)
  ;; Compute the local tile index for GID in TILESET.
  (- gid (map-tileset-firstgid tileset)))

(defun load-map-tilesets (map)
  ;; Load textures for TMX tilesets.
  (let* ((tilesets (map-data-tilesets map))
         (count (length tilesets))
         (loaded (make-array count)))
    (loop :for i :from 0 :below count
          :for tileset = (aref tilesets i)
          :for texture = (raylib:load-texture (map-tileset-image-source tileset))
          :do (setf (aref loaded i)
                    (%make-map-tileset :firstgid (map-tileset-firstgid tileset)
                                       :name (map-tileset-name tileset)
                                       :tilewidth (map-tileset-tilewidth tileset)
                                       :tileheight (map-tileset-tileheight tileset)
                                       :columns (map-tileset-columns tileset)
                                       :image-source (map-tileset-image-source tileset)
                                       :texture texture)))
    loaded))

(defun collision-tile-p (world tx ty)
  ;; Return true when a tile coordinate is blocked.
  (let ((blocked (world-collision-tiles world)))
    (and blocked (gethash (tile-key tx ty) blocked))))
