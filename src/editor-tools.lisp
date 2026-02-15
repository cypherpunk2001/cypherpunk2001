;; NOTE: If you change behavior here, update docs/editor.md :)
;; Editor tools: brush/paint, hotkeys, tileset picker, selection, mode switching
(in-package #:mmorpg)

(defun editor-adjust-selection (editor delta)
  ;; Move the selected tile index by DELTA.
  (let* ((count (max 1 (editor-tile-count editor)))
         (next (mod (+ (editor-selected-tile editor) delta) count)))
    (setf (editor-selected-tile editor) next
          (editor-selection-width editor) 1
          (editor-selection-height editor) 1
          (editor-selection-anchor editor) next)
    (update-editor-labels editor)))

(defun editor-adjust-tileset (editor game assets delta)
  ;; Move the selected tileset by DELTA.
  (let ((tilesets (editor-tileset-catalog editor)))
    (when (and tilesets (> (length tilesets) 0))
      (editor-set-active-tileset editor game assets
                                 (+ (editor-tileset-index editor) delta)))))

(defun editor-adjust-spawn (editor delta)
  ;; Move the selected spawn index by DELTA.
  (let ((spawns (editor-spawn-catalog editor)))
    (when (and spawns (> (length spawns) 0))
      (setf (editor-spawn-index editor)
            (mod (+ (editor-spawn-index editor) delta) (length spawns)))
      (update-editor-labels editor))))

(defun editor-adjust-object (editor delta)
  ;; Move the selected object index by DELTA.
  (let ((objects (editor-object-catalog editor)))
    (when (and objects (> (length objects) 0))
      (setf (editor-object-index editor)
            (mod (+ (editor-object-index editor) delta) (length objects)))
      (update-editor-labels editor))))

(defun editor-tileset-preview-layout (editor)
  ;; Return preview panel layout for the active tileset.
  (let* ((tileset (editor-current-tileset editor))
         (texture (and tileset (editor-tileset-texture tileset))))
    (when (and texture (member (editor-mode editor) '(:tile :collision :object)))
      (let* ((tex-w (raylib:texture-width texture))
             (tex-h (raylib:texture-height texture))
             (padding (float *editor-tileset-preview-padding* 1.0))
             (max-w (max 0.0 (- (min (float *editor-tileset-preview-max-width* 1.0)
                                     (float (current-screen-width) 1.0))
                                (* 2.0 padding))))
             (max-h (max 0.0 (- (min (float *editor-tileset-preview-max-height* 1.0)
                                     (float (current-screen-height) 1.0))
                                (* 2.0 padding))))
             (tex-w-f (float tex-w 1.0))
             (tex-h-f (float tex-h 1.0)))
        (when (and (> tex-w 0) (> tex-h 0) (> max-w 0.0) (> max-h 0.0))
          (let* ((scale (min 1.0 (min (/ max-w tex-w-f) (/ max-h tex-h-f))))
                 (dest-w (* tex-w-f scale))
                 (dest-h (* tex-h-f scale))
                 (dest-x (- (float (current-screen-width) 1.0) padding dest-w))
                 (dest-y padding))
            (values dest-x dest-y dest-w dest-h scale tileset)))))))

(defun editor-mouse-over-tileset-preview-p (editor)
  ;; Return true when the mouse is over the tileset preview panel.
  (multiple-value-bind (x y w h _scale _tileset)
      (editor-tileset-preview-layout editor)
    (declare (ignore _scale _tileset))
    (and x
         (point-in-rect-p (virtual-mouse-x)
                          (virtual-mouse-y)
                          x y w h))))

(defun editor-update-tileset-selection (editor tileset tx ty shift-held)
  ;; Update the selection in the tileset preview.
  (let* ((columns (max 1 (editor-tileset-columns tileset)))
         (rows (max 1 (editor-tileset-rows tileset)))
         (tile-index (+ tx (* ty columns))))
    (if shift-held
        (let* ((anchor (or (editor-selection-anchor editor) tile-index))
               (ax (mod anchor columns))
               (ay (floor anchor columns))
               (min-x (min ax tx))
               (max-x (max ax tx))
               (min-y (min ay ty))
               (max-y (max ay ty))
               (width (max 1 (1+ (- max-x min-x))))
               (height (max 1 (1+ (- max-y min-y)))))
          (setf (editor-selection-anchor editor) anchor
                (editor-selected-tile editor) (+ min-x (* min-y columns))
                (editor-selection-width editor) (min width (- columns min-x))
                (editor-selection-height editor) (min height (- rows min-y))))
        (setf (editor-selected-tile editor) tile-index
              (editor-selection-width editor) 1
              (editor-selection-height editor) 1
              (editor-selection-anchor editor) tile-index))
    (editor-clamp-selection editor tileset)))

(defun editor-handle-tileset-picker (editor)
  ;; Update tile selection from the tileset preview panel.
  (when (and (member (editor-mode editor) '(:tile :collision :object))
             (raylib:is-mouse-button-pressed +mouse-left+))
    (multiple-value-bind (x y w h scale tileset)
        (editor-tileset-preview-layout editor)
      (when (and x tileset
                 (point-in-rect-p (virtual-mouse-x)
                                  (virtual-mouse-y)
                                  x y w h))
        (let* ((tile-size (* (max 1.0 (float *tile-size* 1.0)) scale))
               (local-x (- (virtual-mouse-x) x))
               (local-y (- (virtual-mouse-y) y))
               (tx (floor local-x tile-size))
               (ty (floor local-y tile-size))
               (columns (max 1 (editor-tileset-columns tileset)))
               (rows (max 1 (editor-tileset-rows tileset))))
          (when (and (> tile-size 0.0)
                     (<= 0 tx) (< tx columns)
                     (<= 0 ty) (< ty rows))
            (editor-update-tileset-selection editor tileset tx ty
                                             (editor-shift-held-p))
            (update-editor-labels editor)
            t))))))

(defun editor-mouse-tile (editor world camera)
  ;; Return the tile coordinate under the mouse pointer.
  (multiple-value-bind (wx wy)
      (screen-to-world (virtual-mouse-x)
                       (virtual-mouse-y)
                       (editor-camera-x editor)
                       (editor-camera-y editor)
                       (camera-offset camera)
                       (camera-zoom camera))
    (let ((tile-size (world-tile-dest-size world)))
      (values (floor wx tile-size)
              (floor wy tile-size)
              wx wy))))

(defun editor-clear-layer-tiles (zone base-id tileset-id chunk-size tx ty)
  ;; Clear tiles at TX/TY from layers sharing BASE-ID but a different tileset.
  (let ((layers (zone-layers zone)))
    (when layers
      (loop :for layer :across layers
            :when (and (eql (zone-layer-id layer) base-id)
                       (not (eql (zone-layer-tileset-id layer) tileset-id)))
              :do (zone-layer-set-tile layer chunk-size tx ty 0)))))

(defun editor-update-camera (editor world dt)
  ;; Update editor camera position using WASD/arrow keys.
  (multiple-value-bind (dx dy)
      (read-input-direction)
    (let ((step (* (editor-move-speed editor) dt))
          (nx (editor-camera-x editor))
          (ny (editor-camera-y editor)))
      (when (or (not (zerop dx)) (not (zerop dy)))
        (setf nx (+ nx (* dx step))
              ny (+ ny (* dy step))))
      (setf nx (clamp nx (world-wall-min-x world) (world-wall-max-x world))
            ny (clamp ny (world-wall-min-y world) (world-wall-max-y world))
            (editor-camera-x editor) nx
            (editor-camera-y editor) ny))))

(defun editor-update-mode (editor)
  ;; Switch editor modes based on numeric keys.
  (when (raylib:is-key-pressed +key-one+)
    (setf (editor-mode editor) :tile)
    (update-editor-labels editor))
  (when (raylib:is-key-pressed +key-two+)
    (setf (editor-mode editor) :collision)
    (update-editor-labels editor))
  (when (raylib:is-key-pressed +key-three+)
    (setf (editor-mode editor) :object)
    (update-editor-labels editor))
  (when (raylib:is-key-pressed +key-four+)
    (setf (editor-mode editor) :spawn)
    (update-editor-labels editor)))

(defun editor-selection-origin (editor tileset)
  ;; Return selection origin (tile coords) and column count.
  (let* ((columns (max 1 (if tileset (editor-tileset-columns tileset) 1)))
         (tile-index (editor-selected-tile editor)))
    (values (mod tile-index columns)
            (floor tile-index columns)
            columns)))

(defun editor-apply-paint (editor world camera)
  ;; Paint tiles, layers, or spawns under the mouse.
  (let* ((zone (world-zone world))
         (left-down (raylib:is-mouse-button-down +mouse-left+))
         (right-down (raylib:is-mouse-button-down +mouse-right+)))
    (when (and (member (editor-mode editor) '(:tile :collision :object))
               (editor-mouse-over-tileset-preview-p editor))
      (return-from editor-apply-paint))
    (when (and zone (or left-down right-down))
      (multiple-value-bind (tx ty)
          (editor-mouse-tile editor world camera)
        (when (zone-tile-in-bounds-p zone tx ty)
          (ecase (editor-mode editor)
            (:tile
             (let* ((tileset-id (editor-current-tileset-id editor))
                    (tileset (editor-current-tileset editor))
                    (layer (ensure-zone-layer zone (editor-tile-layer-id editor)
                                              :tileset-id tileset-id))
                    (brush-w (max 1 (editor-selection-width editor)))
                    (brush-h (max 1 (editor-selection-height editor)))
                    (chunk-size (zone-chunk-size zone))
                    (painted nil))
               (multiple-value-bind (origin-x origin-y columns)
                   (editor-selection-origin editor tileset)
                 (loop :for dy :from 0 :below brush-h
                       :for world-y = (+ ty dy)
                       :do (loop :for dx :from 0 :below brush-w
                                 :for world-x = (+ tx dx)
                                 :do (when (zone-tile-in-bounds-p zone world-x world-y)
                                       (let ((value (if right-down
                                                        0
                                                        (+ (+ origin-x dx)
                                                           (* (+ origin-y dy) columns)))))
                                         (when tileset-id
                                           (editor-clear-layer-tiles zone
                                                                     (editor-tile-layer-id editor)
                                                                     tileset-id
                                                                     chunk-size
                                                                     world-x world-y))
                                         (zone-layer-set-tile layer chunk-size world-x world-y value)
                                         ;; Invalidate render cache for this chunk
                                         (invalidate-chunk-at-tile (zone-id zone) layer world-x world-y)
                                         (setf painted t))))))
               (when painted
                 (setf (editor-dirty editor) t))))
            (:collision
             (let* ((tileset-id (editor-current-tileset-id editor))
                    (tileset (editor-current-tileset editor))
                    (layer (ensure-zone-layer zone (editor-collision-layer-id editor)
                                              :collision-p t
                                              :tileset-id tileset-id))
                    (brush-w (max 1 (editor-selection-width editor)))
                    (brush-h (max 1 (editor-selection-height editor)))
                    (chunk-size (zone-chunk-size zone))
                    (painted nil))
               (multiple-value-bind (origin-x origin-y columns)
                   (editor-selection-origin editor tileset)
                 (loop :for dy :from 0 :below brush-h
                       :for world-y = (+ ty dy)
                       :do (loop :for dx :from 0 :below brush-w
                                 :for world-x = (+ tx dx)
                                 :do (when (zone-tile-in-bounds-p zone world-x world-y)
                                       (let ((value (if right-down
                                                        0
                                                        (+ (+ origin-x dx)
                                                           (* (+ origin-y dy) columns)))))
                                         (when tileset-id
                                           (editor-clear-layer-tiles zone
                                                                     (editor-collision-layer-id editor)
                                                                     tileset-id
                                                                     chunk-size
                                                                     world-x world-y))
                                         (zone-layer-set-tile layer chunk-size world-x world-y value)
                                         (zone-set-collision-tile zone world-x world-y value)
                                         (set-world-blocked-tile world world-x world-y value)
                                         ;; Invalidate render cache for this chunk
                                         (invalidate-chunk-at-tile (zone-id zone) layer world-x world-y)
                                         (setf painted t))))))
               (when painted
                 (setf (editor-dirty editor) t))))
            (:object
             (if right-down
                 (progn
                   (zone-remove-object-at zone tx ty)
                   (setf (editor-dirty editor) t))
                 (let ((object-id (editor-current-object-id editor)))
                   (when object-id
                     (zone-add-object zone (%make-zone-object
                                            :id object-id
                                            :x tx
                                            :y ty
                                            :count 1
                                            :base-count 1
                                            :respawn 0.0
                                            :respawnable t
                                            :snapshot-dirty nil))
                     (setf (editor-dirty editor) t)))))
            (:spawn
             (if right-down
                 (progn
                   (zone-remove-spawn-at zone tx ty)
                   (setf (editor-dirty editor) t))
                 (let ((spawn-id (editor-current-spawn-id editor)))
                   (when spawn-id
                     (zone-add-spawn zone (list :id spawn-id
                                                :x tx
                                                :y ty))
                     (setf (editor-dirty editor) t)))))))))))
