;; NOTE: If you change behavior here, update docs/editor.md :)
(in-package #:mmorpg)

(defstruct (editor-object (:constructor %make-editor-object))
  ;; Object metadata for editor palette entries.
  id label path texture width height)

(defun strip-extension (name)
  ;; Return NAME without a trailing extension.
  (let ((dot (position #\. name :from-end t)))
    (if dot
        (subseq name 0 dot)
        name)))

(defun resolve-editor-root (root)
  ;; Resolve ROOT relative to the current defaults.
  (let* ((path (pathname root))
         (dir (if (pathname-name path)
                  (make-pathname :directory (append (or (pathname-directory path) '(:relative))
                                                    (list (pathname-name path)))
                                 :name nil
                                 :type nil
                                 :defaults path)
                  path)))
    (merge-pathnames (uiop:ensure-directory-pathname dir)
                     *default-pathname-defaults*)))

(defun resolve-zone-root (root)
  ;; Resolve the zone ROOT relative to the system source directory.
  (resolve-zone-path (uiop:ensure-directory-pathname root)))

(defun zone-root-prefix (root)
  ;; Return ROOT as a normalized string without a trailing slash.
  (string-right-trim "/"
                     (namestring (uiop:ensure-directory-pathname root))))

(defun collect-zone-files (root)
  ;; Collect zone files under ROOT.
  (let ((files nil))
    (when (probe-file root)
      (dolist (path (uiop:directory-files root))
        (when (string-equal (pathname-type path) "lisp")
          (push (namestring path) files))))
    (if files
        (coerce (sort files #'string<) 'vector)
        (make-array 0))))

(defun collect-png-files (root)
  ;; Recursively collect PNG files under ROOT.
  (let ((root (uiop:ensure-directory-pathname root)))
    (labels ((walk (dir)
               (let ((pngs nil))
                 (dolist (file (uiop:directory-files dir))
                   (when (string-equal (pathname-type file) "png")
                     (push file pngs)))
                 (dolist (sub (uiop:subdirectories dir))
                   (setf pngs (nconc pngs (walk sub))))
                 pngs)))
      (walk root))))

(defun editor-object-from-path (path root)
  ;; Build an editor object entry from PATH.
  (let* ((relative (relative-path-from-root path root))
         (base (strip-extension relative))
         (id (sanitize-identifier base))
         (label base)
         (texture (raylib:load-texture (namestring path)))
         (width (raylib:texture-width texture))
         (height (raylib:texture-height texture)))
    (%make-editor-object :id id
                         :label label
                         :path relative
                         :texture texture
                         :width width
                         :height height)))

(defun load-editor-objects ()
  ;; Load object palette entries from the configured assets root.
  (if (not *editor-object-root*)
      (make-array 0)
      (let* ((root (resolve-editor-root *editor-object-root*))
             (paths (when (probe-file root)
                      (collect-png-files root)))
             (objects (when paths
                        (mapcar (lambda (path)
                                  (editor-object-from-path path root))
                                (nreverse paths)))))
        (if objects
            (coerce objects 'vector)
            (make-array 0)))))

(defun build-editor-object-table (objects)
  ;; Build a lookup table for editor objects keyed by ID.
  (let ((table (make-hash-table :test 'eq)))
    (when objects
      (loop :for obj :across objects
            :do (setf (gethash (editor-object-id obj) table) obj)))
    table))

(defun load-editor-spawns ()
  ;; Build the spawn palette from loaded NPC archetypes.
  (let ((ids (npc-archetype-ids)))
    (if (> (length ids) 0)
        ids
        (vector *npc-default-archetype-id*))))

(defun editor-refresh-zone-files (editor)
  ;; Refresh the zone file list and active index.
  (let* ((root (editor-zone-root editor))
         (files (collect-zone-files root))
         (current (and *zone-path*
                       (namestring (resolve-zone-path *zone-path*)))))
    (when (and current (probe-file current)
               (not (find current files :test #'string=)))
      (setf files (coerce (append (coerce files 'list)
                                  (list current))
                          'vector)))
    (setf (editor-zone-files editor) files
          (editor-zone-index editor)
          (or (and current (position current files :test #'string=)) 0))))

(defun editor-current-zone-path (editor)
  ;; Return the active zone path for the editor, if any.
  (let ((files (editor-zone-files editor)))
    (when (and files (> (length files) 0))
      (aref files (mod (editor-zone-index editor) (length files))))))

(defun editor-zone-id-from-path (path)
  ;; Build a zone ID from PATH.
  (sanitize-identifier (strip-extension (basename path))))

(defun editor-next-zone-path (editor)
  ;; Return a new zone file path under the editor zone root.
  (let* ((root (zone-root-prefix (editor-zone-root editor)))
         (index 1)
         (path nil))
    (loop
      (setf path (format nil "~a/zone-~d.lisp" root index))
      (unless (probe-file path)
        (return path))
      (incf index))))

(defun editor-reset-game-for-zone (editor game)
  ;; Reset player/NPC positions after a zone change.
  (let* ((world (game-world game))
         (player (game-player game))
         (intent (player-intent player)))
    (multiple-value-bind (center-x center-y)
        (world-spawn-center world)
      (multiple-value-bind (spawn-x spawn-y)
          (world-open-position world center-x center-y)
        (setf (player-x player) spawn-x
              (player-y player) spawn-y
              (player-dx player) 0.0
              (player-dy player) 0.0)
        (reset-frame-intent intent)
        (clear-intent-target intent)
        (setf (player-attacking player) nil
              (player-attack-hit player) nil
              (player-attack-timer player) 0.0))
    (when (editor-active editor)
      (setf (editor-camera-x editor) (player-x player)
            (editor-camera-y editor) (player-y player)))
    (let ((npcs (make-npcs player world)))
      (ensure-npcs-open-spawn npcs world)
      (setf (game-npcs game) npcs
            (game-entities game) (make-entities player npcs))))))

(defun editor-activate-zone (editor game zone path status)
  ;; Activate ZONE/PATH and refresh editor state.
  (when path
    (setf *zone-path* path
          (editor-export-path editor) path))
  (apply-zone-to-world (game-world game) zone)
  (editor-refresh-zone-files editor)
  (update-editor-zone-label editor zone)
  (setf (editor-dirty editor) nil)
  (editor-reset-game-for-zone editor game)
  (when status
    (editor-status editor status)))

(defun editor-load-zone (editor game path)
  ;; Load a zone from PATH and activate it.
  (let ((zone (and path (load-zone path))))
    (if zone
        (editor-activate-zone editor game zone path
                              (format nil "Zone loaded: ~a" (basename path)))
        (editor-status editor "Zone load failed"))))

(defun editor-create-zone (editor game)
  ;; Create a new blank zone and switch to it.
  (let* ((root (editor-zone-root editor)))
    (when root
      (ensure-directories-exist (uiop:ensure-directory-pathname root))
      (let* ((path (editor-next-zone-path editor))
             (id (editor-zone-id-from-path path))
             (width (max 1 *zone-default-width*))
             (height (max 1 *zone-default-height*))
             (chunk-size (max 1 *zone-default-chunk-size*))
             (zone (make-empty-zone id width height :chunk-size chunk-size)))
        (write-zone zone path)
        (editor-activate-zone editor game zone path
                              (format nil "Zone created: ~a" (basename path)))))))

(defun editor-delete-zone (editor game)
  ;; Delete the current zone file and switch to another.
  (let* ((files (editor-zone-files editor))
         (count (if files (length files) 0))
         (index (editor-zone-index editor))
         (path (editor-current-zone-path editor)))
    (when path
      (if (> count 1)
          (let ((next-path (aref files (mod (1+ index) count))))
            (ignore-errors (delete-file path))
            (editor-load-zone editor game next-path)
            (editor-status editor (format nil "Zone deleted: ~a" (basename path))))
          (let* ((id (editor-zone-id-from-path path))
                 (width (max 1 *zone-default-width*))
                 (height (max 1 *zone-default-height*))
                 (chunk-size (max 1 *zone-default-chunk-size*))
                 (zone (make-empty-zone id width height :chunk-size chunk-size)))
            (write-zone zone path)
            (editor-activate-zone editor game zone path
                                  "Zone reset (last zone)"))))))

(defun editor-rename-zone (editor game)
  ;; Rename the current zone file using the next available name.
  (let* ((path (editor-current-zone-path editor))
         (zone (and path (world-zone (game-world game)))))
    (when (and path zone)
      (let* ((new-path (editor-next-zone-path editor))
             (new-id (editor-zone-id-from-path new-path)))
        (setf (zone-id zone) new-id)
        (write-zone zone new-path)
        (ignore-errors (delete-file path))
        (editor-activate-zone editor game zone new-path
                              (format nil "Zone renamed: ~a" (basename new-path)))))))

(defun editor-resize-zone (editor game delta)
  ;; Resize the current zone by DELTA chunks.
  (let* ((world (game-world game))
         (zone (world-zone world)))
    (when zone
      (let* ((chunk-size (max 1 (zone-chunk-size zone)))
             (step (* delta chunk-size))
             (width (max chunk-size (+ (zone-width zone) step)))
             (height (max chunk-size (+ (zone-height zone) step)))
             (resized (zone-resize zone width height)))
        (apply-zone-to-world world resized)
        (setf (editor-dirty editor) t)
        (update-editor-zone-label editor resized)
        (editor-status editor (format nil "Zone resized: ~dx~d" width height))))))

(defun editor-cycle-zone (editor game delta)
  ;; Switch to the next/previous zone in the list.
  (let* ((files (editor-zone-files editor))
         (count (if files (length files) 0)))
    (when (> count 0)
      (setf (editor-zone-index editor)
            (mod (+ (editor-zone-index editor) delta) count))
      (editor-load-zone editor game (editor-current-zone-path editor)))))

(defun editor-handle-zone-actions (editor game)
  ;; Respond to zone lifecycle hotkeys while the editor is active.
  (when (raylib:is-key-pressed +key-f6+)
    (editor-create-zone editor game))
  (when (raylib:is-key-pressed +key-f7+)
    (editor-delete-zone editor game))
  (when (raylib:is-key-pressed +key-f8+)
    (editor-cycle-zone editor game -1))
  (when (raylib:is-key-pressed +key-f9+)
    (editor-cycle-zone editor game 1))
  (when (raylib:is-key-pressed +key-f10+)
    (editor-resize-zone editor game -1))
  (when (raylib:is-key-pressed +key-f11+)
    (editor-resize-zone editor game 1))
  (when (raylib:is-key-pressed +key-f12+)
    (editor-rename-zone editor game)))

(defun unload-editor-objects (editor)
  ;; Unload object textures used by the editor.
  (let ((objects (editor-object-catalog editor)))
    (when objects
      (loop :for obj :across objects
            :do (when (editor-object-texture obj)
                  (raylib:unload-texture (editor-object-texture obj)))))))

(defun tileset-tile-count (tileset)
  ;; Calculate the total number of tiles in the tileset texture.
  (let* ((columns (max 1 *tileset-columns*))
         (rows (max 1 (truncate (/ (raylib:texture-height tileset)
                                   (max 1 *tile-size*))))))
    (* columns rows)))

(defun editor-current-object (editor)
  ;; Return the currently selected editor object, if any.
  (let ((objects (editor-object-catalog editor)))
    (when (and objects (> (length objects) 0))
      (let ((index (mod (editor-object-index editor) (length objects))))
        (aref objects index)))))

(defun editor-current-spawn-id (editor)
  ;; Return the currently selected spawn archetype ID.
  (let ((spawns (editor-spawn-catalog editor)))
    (when (and spawns (> (length spawns) 0))
      (let ((index (mod (editor-spawn-index editor) (length spawns))))
        (aref spawns index)))))

(defun editor-object-by-id (editor id)
  ;; Return the editor object matching ID.
  (when (and editor id)
    (let ((key (cond
                 ((keywordp id) id)
                 ((symbolp id) (intern (symbol-name id) :keyword))
                 ((stringp id) (sanitize-identifier id))
                 (t id))))
      (gethash key (editor-object-table editor)))))

(defun editor-mode-label-text (mode)
  ;; Convert editor mode into a display label.
  (ecase mode
    (:tile "Mode: Tile")
    (:collision "Mode: Collision")
    (:object "Mode: Object")
    (:spawn "Mode: Spawn")))

(defun update-editor-labels (editor)
  ;; Refresh cached editor labels after selection changes.
  (setf (editor-mode-label editor)
        (editor-mode-label-text (editor-mode editor)))
  (setf (editor-tile-label editor)
        (format nil "Tile: ~d/~d"
                (editor-selected-tile editor)
                (max 1 (editor-tile-count editor))))
  (let ((mode (editor-mode editor)))
    (setf (editor-object-label-text editor)
          (cond
            ((eq mode :spawn)
             (let* ((spawn-id (editor-current-spawn-id editor))
                    (archetype (and spawn-id (find-npc-archetype spawn-id)))
                    (label (or (and archetype (npc-archetype-name archetype))
                               spawn-id)))
               (if label
                   (format nil "Spawn: ~a" label)
                   "Spawn: none")))
            (t
             (let ((obj (editor-current-object editor)))
               (if obj
                   (format nil "Object: ~a" (editor-object-label obj))
                   "Object: none")))))))

(defun update-editor-zone-label (editor zone)
  ;; Refresh the cached zone label for the editor overlay.
  (let* ((files (editor-zone-files editor))
         (count (if files (length files) 0))
         (path (editor-current-zone-path editor))
         (name (if path (basename path) "none")))
    (setf (editor-zone-label editor)
          (if zone
              (format nil "Zone: ~a (~d/~d) ~dx~d"
                      name
                      (if (> count 0) (1+ (mod (editor-zone-index editor) count)) 0)
                      count
                      (zone-width zone)
                      (zone-height zone))
              (format nil "Zone: ~a (~d/~d)"
                      name
                      (if (> count 0) (1+ (mod (editor-zone-index editor) count)) 0)
                      count)))))

(defun make-editor (world assets player)
  ;; Build editor state with a tile palette and object catalog.
  (let* ((tile-count (tileset-tile-count (assets-tileset assets)))
         (objects (load-editor-objects))
         (object-table (build-editor-object-table objects))
         (zone-root (resolve-zone-root *zone-root*))
         (spawns (load-editor-spawns))
         (editor (%make-editor :active nil
                               :mode :tile
                               :camera-x (player-x player)
                               :camera-y (player-y player)
                               :move-speed *editor-move-speed*
                               :selected-tile *floor-tile-index*
                               :tile-count tile-count
                               :tile-layer-id *editor-tile-layer-id*
                               :collision-layer-id *editor-collision-layer-id*
                               :object-catalog objects
                               :object-table object-table
                               :object-index 0
                               :zone-root zone-root
                               :zone-files (make-array 0)
                               :zone-index 0
                               :zone-label nil
                               :spawn-catalog spawns
                               :spawn-index 0
                               :selection-start-x nil
                               :selection-start-y nil
                               :selection-end-x nil
                               :selection-end-y nil
                               :status-label nil
                               :status-timer 0.0
                               :export-path nil
                               :dirty nil)))
    (editor-refresh-zone-files editor)
    (setf (editor-export-path editor)
          (or (editor-current-zone-path editor)
              *zone-path*
              *editor-export-path*))
    (update-editor-labels editor)
    (update-editor-zone-label editor (world-zone world))
    editor))

(defun toggle-editor-mode (editor player)
  ;; Toggle editor state and reset camera to the player.
  (setf (editor-active editor) (not (editor-active editor)))
  (when (editor-active editor)
    (setf (editor-camera-x editor) (player-x player)
          (editor-camera-y editor) (player-y player)
          (editor-status-label editor) nil
          (editor-status-timer editor) 0.0))
  (update-editor-labels editor))

(defun editor-camera-target (editor player)
  ;; Return the active camera target.
  (if (editor-active editor)
      (values (editor-camera-x editor)
              (editor-camera-y editor))
      (values (player-x player)
              (player-y player))))

(defun editor-status (editor label)
  ;; Set a transient status label for the editor overlay.
  (setf (editor-status-label editor) label
        (editor-status-timer editor) 2.0))

(defun update-editor-status (editor dt)
  ;; Update and clear any transient editor status label.
  (when (> (editor-status-timer editor) 0.0)
    (decf (editor-status-timer editor) dt)
    (when (<= (editor-status-timer editor) 0.0)
      (setf (editor-status-label editor) nil))))

(defun editor-adjust-selection (editor delta)
  ;; Move the selected tile index by DELTA.
  (let* ((count (max 1 (editor-tile-count editor)))
         (next (mod (+ (editor-selected-tile editor) delta) count)))
    (setf (editor-selected-tile editor) next)
    (update-editor-labels editor)))

(defun editor-adjust-object (editor delta)
  ;; Move the selected object index by DELTA.
  (let ((objects (editor-object-catalog editor)))
    (when (and objects (> (length objects) 0))
      (setf (editor-object-index editor)
            (mod (+ (editor-object-index editor) delta) (length objects)))
      (update-editor-labels editor))))

(defun editor-adjust-spawn (editor delta)
  ;; Move the selected spawn index by DELTA.
  (let ((spawns (editor-spawn-catalog editor)))
    (when (and spawns (> (length spawns) 0))
      (setf (editor-spawn-index editor)
            (mod (+ (editor-spawn-index editor) delta) (length spawns)))
      (update-editor-labels editor))))

(defun editor-mouse-tile (editor world camera)
  ;; Return the tile coordinate under the mouse pointer.
  (multiple-value-bind (wx wy)
      (screen-to-world (raylib:get-mouse-x)
                       (raylib:get-mouse-y)
                       (editor-camera-x editor)
                       (editor-camera-y editor)
                       (camera-offset camera)
                       (camera-zoom camera))
    (let ((tile-size (world-tile-dest-size world)))
      (values (floor wx tile-size)
              (floor wy tile-size)
              wx wy))))

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

(defun editor-update-selection (editor world camera)
  ;; Capture selection region bounds via hotkeys.
  (let ((zone (world-zone world)))
    (when (raylib:is-key-pressed +key-b+)
      (multiple-value-bind (tx ty)
          (editor-mouse-tile editor world camera)
        (when zone
          (setf tx (clamp tx 0 (max 0 (1- (zone-width zone))))
                ty (clamp ty 0 (max 0 (1- (zone-height zone))))))
        (setf (editor-selection-start-x editor) tx
              (editor-selection-start-y editor) ty)
        (editor-status editor "Selection start set")))
    (when (raylib:is-key-pressed +key-n+)
      (multiple-value-bind (tx ty)
          (editor-mouse-tile editor world camera)
        (when zone
          (setf tx (clamp tx 0 (max 0 (1- (zone-width zone))))
                ty (clamp ty 0 (max 0 (1- (zone-height zone))))))
        (setf (editor-selection-end-x editor) tx
              (editor-selection-end-y editor) ty)
        (editor-status editor "Selection end set"))))
  (when (raylib:is-key-pressed +key-c+)
    (setf (editor-selection-start-x editor) nil
          (editor-selection-start-y editor) nil
          (editor-selection-end-x editor) nil
          (editor-selection-end-y editor) nil)
    (editor-status editor "Selection cleared")))

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

(defun editor-apply-paint (editor world camera)
  ;; Paint tiles or objects under the mouse.
  (let* ((zone (world-zone world))
         (left-down (raylib:is-mouse-button-down +mouse-left+))
         (right-down (raylib:is-mouse-button-down +mouse-right+)))
    (when (and zone (or left-down right-down))
      (multiple-value-bind (tx ty)
          (editor-mouse-tile editor world camera)
        (when (zone-tile-in-bounds-p zone tx ty)
          (ecase (editor-mode editor)
            (:tile
             (let* ((layer (ensure-zone-layer zone (editor-tile-layer-id editor)))
                    (value (if right-down 0 (editor-selected-tile editor))))
               (zone-layer-set-tile layer (zone-chunk-size zone) tx ty value)
               (setf (editor-dirty editor) t)))
            (:collision
             (let* ((layer (ensure-zone-layer zone (editor-collision-layer-id editor)
                                              :collision-p t))
                    (value (if right-down 0 1)))
               (zone-layer-set-tile layer (zone-chunk-size zone) tx ty value)
               (zone-set-collision-tile zone tx ty value)
               (set-world-blocked-tile world tx ty value)
               (setf (editor-dirty editor) t)))
            (:object
             (if right-down
                 (progn
                   (zone-remove-object-at zone tx ty)
                   (setf (editor-dirty editor) t))
                 (let ((obj (editor-current-object editor)))
                   (when obj
                     (zone-add-object zone (list :id (editor-object-id obj)
                                                 :x tx
                                                 :y ty))
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

(defun editor-export-zone (editor world)
  ;; Export the current zone (or selection) to disk.
  (let* ((zone (world-zone world))
         (path (editor-export-path editor)))
    (when (and zone path)
      (let* ((sx (editor-selection-start-x editor))
             (sy (editor-selection-start-y editor))
             (ex (editor-selection-end-x editor))
             (ey (editor-selection-end-y editor))
             (slice (if (and sx sy ex ey)
                        (let* ((min-x (min sx ex))
                               (min-y (min sy ey))
                               (width (1+ (abs (- ex sx))))
                               (height (1+ (abs (- ey sy)))))
                          (zone-slice zone min-x min-y width height))
                        zone)))
        (write-zone slice path)
        (setf (editor-dirty editor) nil)
        (editor-status editor "Zone exported")))))

(defun update-editor (editor game dt)
  ;; Update editor input, camera, and painting when active.
  (let* ((world (game-world game))
         (camera (game-camera game))
         (ui (game-ui game)))
    (when (editor-active editor)
      (update-editor-status editor dt)
      (unless (ui-menu-open ui)
        (editor-update-mode editor)
        (editor-update-selection editor world camera)
        (editor-handle-zone-actions editor game)
        (when (raylib:is-key-pressed +key-q+)
          (editor-adjust-selection editor -1))
        (when (raylib:is-key-pressed +key-e+)
          (editor-adjust-selection editor 1))
        (when (raylib:is-key-pressed +key-z+)
          (if (eq (editor-mode editor) :spawn)
              (editor-adjust-spawn editor -1)
              (editor-adjust-object editor -1)))
        (when (raylib:is-key-pressed +key-x+)
          (if (eq (editor-mode editor) :spawn)
              (editor-adjust-spawn editor 1)
              (editor-adjust-object editor 1)))
        (when (raylib:is-key-pressed +key-f5+)
          (editor-export-zone editor world))
        (editor-update-camera editor world dt)
        (editor-apply-paint editor world camera)))))

(defun draw-editor-selection (editor world)
  ;; Draw selection bounds in world space.
  (let ((sx (editor-selection-start-x editor))
        (sy (editor-selection-start-y editor))
        (ex (editor-selection-end-x editor))
        (ey (editor-selection-end-y editor)))
    (when (and sx sy ex ey)
      (let* ((tile-size (world-tile-dest-size world))
             (min-x (min sx ex))
             (min-y (min sy ey))
             (width (+ 1 (abs (- ex sx))))
             (height (+ 1 (abs (- ey sy))))
             (x (round (* min-x tile-size)))
             (y (round (* min-y tile-size)))
             (w (round (* width tile-size)))
             (h (round (* height tile-size))))
        (raylib:draw-rectangle-lines x y w h *editor-selection-color*)))))

(defun draw-editor-spawns (editor world)
  ;; Draw spawn markers in world space while editing.
  (let ((zone (world-zone world)))
    (when zone
      (let* ((tile-size (world-tile-dest-size world))
             (size (round tile-size))
             (dot (max 2 (round (* size 0.2)))))
        (dolist (spawn (zone-spawns zone))
          (let ((tx (getf spawn :x))
                (ty (getf spawn :y)))
            (when (and (numberp tx) (numberp ty))
              (let* ((x (round (* tx tile-size)))
                     (y (round (* ty tile-size)))
                     (cx (+ x (round (/ (- size dot) 2))))
                     (cy (+ y (round (/ (- size dot) 2)))))
                (raylib:draw-rectangle-lines x y size size *editor-spawn-color*)
                (raylib:draw-rectangle cx cy dot dot *editor-spawn-color*)))))))))

(defun draw-editor-cursor (editor world camera)
  ;; Draw the tile highlight under the mouse cursor.
  (multiple-value-bind (tx ty _wx _wy)
      (editor-mouse-tile editor world camera)
    (declare (ignore _wx _wy))
    (when (and (world-zone world)
               (zone-tile-in-bounds-p (world-zone world) tx ty))
      (let* ((tile-size (world-tile-dest-size world))
             (x (round (* tx tile-size)))
             (y (round (* ty tile-size)))
             (size (round tile-size)))
        (raylib:draw-rectangle-lines x y size size
                                     *editor-cursor-color*)))))

(defun draw-editor-world-overlay (editor world camera)
  ;; Draw editor overlays in world space.
  (when (editor-active editor)
    (draw-editor-selection editor world)
    (draw-editor-spawns editor world)
    (draw-editor-cursor editor world camera)))

(defun draw-editor-ui-overlay (editor ui)
  ;; Draw editor UI hints in screen space.
  (when (editor-active editor)
    (let ((x 16)
          (y 40)
          (line 18))
      (when (editor-zone-label editor)
        (raylib:draw-text (editor-zone-label editor) x y 16 (ui-menu-text-color ui))
        (incf y line))
      (raylib:draw-text (editor-mode-label editor) x y 16 (ui-menu-text-color ui))
      (incf y line)
      (raylib:draw-text (editor-tile-label editor) x y 16 (ui-menu-text-color ui))
      (incf y line)
      (raylib:draw-text (editor-object-label-text editor) x y 16 (ui-menu-text-color ui))
      (incf y line)
      (raylib:draw-text "1/2/3/4 mode | Q/E tile | Z/X object/spawn | F5 export" x y 16
                        (ui-menu-text-color ui))
      (incf y line)
      (raylib:draw-text "F6 new | F7 delete | F8/F9 cycle | F10/F11 resize | F12 rename"
                        x y 16 (ui-menu-text-color ui))
      (incf y line)
      (raylib:draw-text "B start | N end | C clear selection | LMB paint | RMB erase" x y 16
                        (ui-menu-text-color ui))
      (when (editor-status-label editor)
        (incf y line)
        (raylib:draw-text (editor-status-label editor) x y 16
                          (ui-menu-text-color ui))))))
