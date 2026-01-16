;; NOTE: If you change behavior here, update docs/editor.md :)
(in-package #:mmorpg)

(defstruct (editor-tileset (:constructor %make-editor-tileset))
  ;; Tileset metadata for editor tile selection.
  id label path texture columns rows tile-count)

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

(defun normalize-zone-path (path)
  ;; Return a resolved namestring for PATH, or nil.
  (when path
    (namestring (resolve-zone-path path))))

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

(defun normalize-tileset-path (path)
  ;; Return PATH as a normalized namestring if it exists.
  (when path
    (let ((probe (probe-file path)))
      (cond
        (probe (namestring probe))
        ((pathnamep path) (namestring path))
        (t path)))))

(defun editor-tileset-from-path (path root)
  ;; Build a tileset entry from PATH with an optional ROOT for labels.
  (let* ((path (if (pathnamep path) path (pathname path)))
         (relative (if root
                       (relative-path-from-root path root)
                       (basename path)))
         (base (strip-extension relative))
         (id (sanitize-identifier base))
         (label base)
         (full-path (or (normalize-tileset-path path) (namestring path))))
    (%make-editor-tileset :id id
                          :label label
                          :path full-path
                          :texture nil
                          :columns 0
                          :rows 0
                          :tile-count 0)))

(defun collect-editor-tileset-paths ()
  ;; Collect tileset sheet paths from explicit paths or a root directory.
  (cond
    ((and *editor-tileset-paths*
          (> (length (coerce *editor-tileset-paths* 'list)) 0))
     (remove nil
             (mapcar #'normalize-tileset-path
                     (coerce *editor-tileset-paths* 'list))))
    (*editor-tileset-root*
     (let* ((root (resolve-editor-root *editor-tileset-root*))
            (paths (when (probe-file root)
                     (collect-png-files root))))
       (when paths
         (mapcar #'normalize-tileset-path
                 (sort paths #'string< :key #'namestring)))))
    (t
     (let ((fallback (normalize-tileset-path *tileset-path*)))
       (when fallback
         (list fallback))))))

(defun load-editor-tilesets ()
  ;; Build a tileset catalog from configured paths or root.
  (let* ((paths (collect-editor-tileset-paths))
         (root (when (and (null *editor-tileset-paths*) *editor-tileset-root*)
                 (resolve-editor-root *editor-tileset-root*))))
    (if (and paths (> (length paths) 0))
        (coerce (mapcar (lambda (path)
                          (editor-tileset-from-path path root))
                        paths)
                'vector)
        (make-array 0))))

(defun editor-tileset-index-for-path (tilesets path)
  ;; Return the index of PATH inside TILESETS, or 0 when missing.
  (let ((normalized (normalize-tileset-path path)))
    (if (and normalized tilesets (> (length tilesets) 0))
        (or (position normalized tilesets
                      :test #'string=
                      :key #'editor-tileset-path)
            0)
        0)))

(defun editor-ensure-tileset-metrics (tileset)
  ;; Refresh tileset grid metrics from its texture.
  (let ((texture (editor-tileset-texture tileset)))
    (when texture
      (let* ((tile-size (max 1 *tile-size*))
             (columns (max 1 (truncate (/ (raylib:texture-width texture) tile-size))))
             (rows (max 1 (truncate (/ (raylib:texture-height texture) tile-size)))))
        (setf (editor-tileset-columns tileset) columns
              (editor-tileset-rows tileset) rows
              (editor-tileset-tile-count tileset) (* columns rows))))
  tileset))

(defun editor-load-tileset-texture (tileset)
  ;; Ensure TILESET has a loaded texture and computed metrics.
  (unless (editor-tileset-texture tileset)
    (setf (editor-tileset-texture tileset)
          (raylib:load-texture (editor-tileset-path tileset))))
  (editor-ensure-tileset-metrics tileset))

(defun editor-current-tileset (editor)
  ;; Return the active tileset entry for the editor.
  (let ((tilesets (editor-tileset-catalog editor)))
    (when (and tilesets (> (length tilesets) 0))
      (aref tilesets (mod (editor-tileset-index editor) (length tilesets))))))

(defun editor-current-tileset-id (editor)
  ;; Return the active tileset ID, if any.
  (let ((tileset (editor-current-tileset editor)))
    (when tileset
      (editor-tileset-id tileset))))

(defun editor-tileset-by-id (editor tileset-id)
  ;; Return the tileset entry matching TILESET-ID.
  (let ((tilesets (editor-tileset-catalog editor)))
    (when (and tilesets tileset-id)
      (loop :for tileset :across tilesets
            :when (eql (editor-tileset-id tileset) tileset-id)
              :do (return tileset)))))

(defun editor-default-tileset-id (editor)
  ;; Return the tileset ID for the configured default tileset path.
  (let ((tilesets (editor-tileset-catalog editor)))
    (when (and tilesets (> (length tilesets) 0))
      (let* ((index (editor-tileset-index-for-path tilesets *tileset-path*))
             (tileset (aref tilesets (mod index (length tilesets)))))
        (editor-tileset-id tileset)))))


(defun load-editor-spawns ()
  ;; Build the spawn palette from loaded NPC archetypes.
  (let ((ids (npc-archetype-ids)))
    (if (> (length ids) 0)
        ids
        (vector *npc-default-archetype-id*))))

(defun load-editor-objects ()
  ;; Build the object palette from loaded object archetypes.
  (object-archetype-ids))

(defun editor-refresh-zone-files (editor)
  ;; Refresh the zone file list and active index.
  (let* ((root (editor-zone-root editor))
         (files (coerce (collect-zone-files root) 'list))
         (current (normalize-zone-path *zone-path*))
         (extras (editor-zone-history editor)))
    (dolist (extra extras)
      (when (and extra (probe-file extra)
                 (not (find extra files :test #'string=)))
        (push extra files)))
    (when (and current (probe-file current)
               (not (find current files :test #'string=)))
      (push current files))
    (let ((vector-files (if files
                            (coerce (sort files #'string<) 'vector)
                            (make-array 0))))
      (setf (editor-zone-files editor) vector-files
            (editor-zone-index editor)
            (or (and current (position current vector-files :test #'string=)) 0)))))

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

(defun zone-path-under-root-p (path root)
  ;; Return true when PATH is under ROOT.
  (let* ((root-str (namestring (uiop:ensure-directory-pathname root)))
         (full (normalize-zone-path path)))
    (and full
         (<= (length root-str) (length full))
         (string= root-str full :end2 (length root-str)))))

(defun editor-track-zone (editor path)
  ;; Track PATH when it lives outside the editor zone root.
  (let ((full (normalize-zone-path path)))
    (when (and full (probe-file full)
               (not (zone-path-under-root-p full (editor-zone-root editor))))
      (pushnew full (editor-zone-history editor) :test #'string=))))

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
      (setf (world-minimap-spawns world)
            (build-adjacent-minimap-spawns world player))
    (when (editor-active editor)
      (setf (editor-camera-x editor) (player-x player)
            (editor-camera-y editor) (player-y player)))
    (let ((npcs (make-npcs player world
                           :id-source (game-id-source game))))
      (ensure-npcs-open-spawn npcs world)
      (setf (game-npcs game) npcs
            (game-entities game) (make-entities player npcs))))))

(defun editor-activate-zone (editor game zone path status)
  ;; Activate ZONE/PATH and refresh editor state.
  (let* ((full-path (normalize-zone-path path))
         (outside-root (and full-path
                            (not (zone-path-under-root-p full-path (editor-zone-root editor)))))
         (status (if (and status outside-root)
                     (format nil "~a (outside zone-root, pinned)" status)
                     status)))
    (when path
      (setf *zone-path* path
            (editor-export-path editor) path))
    (apply-zone-to-world (game-world game) zone)
    (editor-assign-default-layer-tilesets editor zone)
    (editor-track-zone editor path)
    (editor-refresh-zone-files editor)
    (update-editor-zone-label editor zone)
    (setf (editor-dirty editor) nil)
    (editor-reset-game-for-zone editor game)
    (cond
      (status (editor-status editor status))
      (outside-root (editor-status editor "Zone outside zone-root, pinned")))))

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
                              (format nil "Zone created: ~a" path))))))

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
    (editor-cycle-zone editor game 1)))

(defun unload-editor-tilesets (editor assets)
  ;; Unload tileset textures used by the editor.
  (let ((tilesets (editor-tileset-catalog editor))
        (active (and assets (assets-tileset assets))))
    (when tilesets
      (loop :for tileset :across tilesets
            :for texture = (editor-tileset-texture tileset)
            :do (when (and texture (not (eq texture active)))
                  (raylib:unload-texture texture))))))

(defun tileset-tile-count (tileset)
  ;; Calculate the total number of tiles in the tileset texture.
  (let* ((tile-size (max 1 *tile-size*))
         (columns (max 1 (truncate (/ (raylib:texture-width tileset) tile-size))))
         (rows (max 1 (truncate (/ (raylib:texture-height tileset) tile-size)))))
    (* columns rows)))

(defun editor-current-spawn-id (editor)
  ;; Return the currently selected spawn archetype ID.
  (let ((spawns (editor-spawn-catalog editor)))
    (when (and spawns (> (length spawns) 0))
      (let ((index (mod (editor-spawn-index editor) (length spawns))))
        (aref spawns index)))))

(defun editor-current-object-id (editor)
  ;; Return the currently selected object archetype ID.
  (let ((objects (editor-object-catalog editor)))
    (when (and objects (> (length objects) 0))
      (let ((index (mod (editor-object-index editor) (length objects))))
        (aref objects index)))))

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
  (let* ((tilesets (editor-tileset-catalog editor))
         (count (if tilesets (length tilesets) 0))
         (index (if (> count 0) (1+ (mod (editor-tileset-index editor) count)) 0))
         (tileset (editor-current-tileset editor))
         (label (if tileset (editor-tileset-label tileset) "none")))
    (setf (editor-tileset-label-text editor)
          (if (> count 0)
              (format nil "Sheet: ~a (~d/~d)" label index count)
              "Sheet: none")))
  (let ((sel-w (max 1 (editor-selection-width editor)))
        (sel-h (max 1 (editor-selection-height editor))))
    (setf (editor-tile-label editor)
          (if (and (= sel-w 1) (= sel-h 1))
              (format nil "Tile: ~d/~d"
                      (editor-selected-tile editor)
                      (max 1 (editor-tile-count editor)))
              (format nil "Tile: ~d/~d (~dx~d)"
                      (editor-selected-tile editor)
                      (max 1 (editor-tile-count editor))
                      sel-w sel-h))))
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
            ((eq mode :tile)
             (format nil "Layer: Base (~a)" (editor-tile-layer-id editor)))
            ((eq mode :collision)
             (format nil "Layer: Collision (~a)" (editor-collision-layer-id editor)))
            ((eq mode :object)
             (let* ((object-id (editor-current-object-id editor))
                    (archetype (and object-id (find-object-archetype object-id)))
                    (label (or (and archetype (object-archetype-name archetype))
                               object-id)))
               (if label
                   (format nil "Object: ~a" label)
                   "Object: none")))
            (t "Layer: none"))))
  )

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

(defun editor-sync-zone (editor world)
  ;; Sync editor state after an external zone change.
  (when editor
    (editor-track-zone editor *zone-path*)
    (editor-refresh-zone-files editor)
    (update-editor-zone-label editor (world-zone world))
    (editor-assign-default-layer-tilesets editor (world-zone world))
    (setf (editor-export-path editor)
          (or (editor-current-zone-path editor)
              *zone-path*
              (editor-export-path editor)))
    (setf (editor-dirty editor) nil)))

(defun editor-assign-default-layer-tilesets (editor zone)
  ;; Ensure non-collision layers have a default tileset ID.
  (let ((tileset-id (editor-default-tileset-id editor)))
    (when (and zone tileset-id)
      (loop :for layer :across (zone-layers zone)
            :unless (zone-layer-collision-p layer)
              :when (null (zone-layer-tileset-id layer))
                :do (setf (zone-layer-tileset-id layer) tileset-id)))))

(defun make-editor (world assets player)
  ;; Build editor state with tile palette and layer selections.
  (let* ((tilesets (load-editor-tilesets))
         (tileset-index (editor-tileset-index-for-path tilesets *tileset-path*))
         (tile-count (tileset-tile-count (assets-tileset assets)))
         (zone-root (resolve-zone-root *zone-root*))
         (spawns (load-editor-spawns))
         (objects (load-editor-objects))
         (editor (%make-editor :active nil
                               :mode :tile
                               :camera-x (player-x player)
                               :camera-y (player-y player)
                               :move-speed *editor-move-speed*
                               :selected-tile *floor-tile-index*
                               :selection-width 1
                               :selection-height 1
                               :selection-anchor *floor-tile-index*
                               :tile-count (max 1 tile-count)
                               :tileset-catalog tilesets
                               :tileset-index tileset-index
                               :tileset-label-text nil
                               :tile-layer-id *editor-tile-layer-id*
                               :collision-layer-id *editor-collision-layer-id*
                               :object-layer-id *editor-object-layer-id*
                               :zone-root zone-root
                               :zone-files (make-array 0)
                               :zone-index 0
                               :zone-label nil
                               :zone-history nil
                               :spawn-catalog spawns
                               :spawn-index 0
                               :object-catalog objects
                               :object-index 0
                               :status-label nil
                               :status-timer 0.0
                               :export-path nil
                               :dirty nil)))
    (editor-set-active-tileset editor nil assets tileset-index nil)
    (editor-clamp-floor-index editor world)
    (editor-refresh-zone-files editor)
    (editor-assign-default-layer-tilesets editor (world-zone world))
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

(defun editor-shift-held-p ()
  ;; Return true when either shift key is held.
  (or (raylib:is-key-down +key-left-shift+)
      (raylib:is-key-down +key-right-shift+)))

(defun editor-adjust-selection (editor delta)
  ;; Move the selected tile index by DELTA.
  (let* ((count (max 1 (editor-tile-count editor)))
         (next (mod (+ (editor-selected-tile editor) delta) count)))
    (setf (editor-selected-tile editor) next
          (editor-selection-width editor) 1
          (editor-selection-height editor) 1
          (editor-selection-anchor editor) next)
    (update-editor-labels editor)))

(defun editor-clamp-selection (editor tileset)
  ;; Clamp selection bounds to the current tileset dimensions.
  (when tileset
    (let* ((count (max 1 (editor-tileset-tile-count tileset)))
           (columns (max 1 (editor-tileset-columns tileset)))
           (rows (max 1 (editor-tileset-rows tileset)))
           (tile-index (mod (editor-selected-tile editor) count)))
      (setf (editor-selected-tile editor) tile-index)
      (let* ((tx (mod tile-index columns))
             (ty (floor tile-index columns))
             (sel-w (max 1 (editor-selection-width editor)))
             (sel-h (max 1 (editor-selection-height editor)))
             (max-w (max 1 (- columns tx)))
             (max-h (max 1 (- rows ty))))
        (setf (editor-selection-width editor) (min sel-w max-w)
              (editor-selection-height editor) (min sel-h max-h)))
      (let ((anchor (editor-selection-anchor editor)))
        (when (or (null anchor) (< anchor 0) (>= anchor count))
          (setf (editor-selection-anchor editor) tile-index))))))

(defun editor-clamp-floor-index (editor world)
  ;; Clamp the world's floor index to the active tileset size.
  (let ((tileset (editor-current-tileset editor)))
    (when (and world tileset)
      (let* ((count (editor-tileset-tile-count tileset))
             (floor (world-floor-index world)))
        (setf (world-floor-index world)
              (if (and (numberp floor) (> count 0))
                  (mod floor count)
                  0))))))

(defun editor-set-active-tileset (editor game assets index &optional (show-status t))
  ;; Select the tileset at INDEX and sync it to rendering and labels.
  (let ((tilesets (editor-tileset-catalog editor)))
    (when (and tilesets (> (length tilesets) 0))
      (setf (editor-tileset-index editor)
            (mod index (length tilesets)))
      (let* ((tileset (editor-current-tileset editor))
             (active-path (normalize-tileset-path *tileset-path*)))
        (when tileset
          (when (and assets (assets-tileset assets)
                     (null (editor-tileset-texture tileset))
                     active-path
                     (string= active-path (editor-tileset-path tileset)))
            (setf (editor-tileset-texture tileset) (assets-tileset assets)))
          (editor-load-tileset-texture tileset)
          (setf (editor-tile-count editor)
                (max 1 (editor-tileset-tile-count tileset)))
          (setf (editor-selected-tile editor)
                (mod (editor-selected-tile editor) (editor-tile-count editor)))
          (editor-clamp-selection editor tileset)
          (editor-clamp-floor-index editor (and game (game-world game)))
          (update-editor-labels editor)
          (when show-status
            (editor-status editor (format nil "Tileset: ~a"
                                          (editor-tileset-label tileset)))))))))

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
                                     (float *window-width* 1.0))
                                (* 2.0 padding))))
             (max-h (max 0.0 (- (min (float *editor-tileset-preview-max-height* 1.0)
                                     (float *window-height* 1.0))
                                (* 2.0 padding))))
             (tex-w-f (float tex-w 1.0))
             (tex-h-f (float tex-h 1.0)))
        (when (and (> tex-w 0) (> tex-h 0) (> max-w 0.0) (> max-h 0.0))
          (let* ((scale (min 1.0 (min (/ max-w tex-w-f) (/ max-h tex-h-f))))
                 (dest-w (* tex-w-f scale))
                 (dest-h (* tex-h-f scale))
                 (dest-x (- (float *window-width* 1.0) padding dest-w))
                 (dest-y padding))
            (values dest-x dest-y dest-w dest-h scale tileset)))))))

(defun editor-mouse-over-tileset-preview-p (editor)
  ;; Return true when the mouse is over the tileset preview panel.
  (multiple-value-bind (x y w h _scale _tileset)
      (editor-tileset-preview-layout editor)
    (declare (ignore _scale _tileset))
    (and x
         (point-in-rect-p (raylib:get-mouse-x)
                          (raylib:get-mouse-y)
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
                 (point-in-rect-p (raylib:get-mouse-x)
                                  (raylib:get-mouse-y)
                                  x y w h))
        (let* ((tile-size (* (max 1.0 (float *tile-size* 1.0)) scale))
               (local-x (- (raylib:get-mouse-x) x))
               (local-y (- (raylib:get-mouse-y) y))
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
                     (zone-add-object zone (list :id object-id
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
  ;; Export the current zone to disk.
  (let* ((zone (world-zone world))
         (path (editor-export-path editor)))
    (when (and zone path)
      (write-zone zone path)
      (setf (editor-dirty editor) nil)
      (editor-status editor "Zone exported"))))

(defun update-editor (editor game dt)
  ;; Update editor input, camera, and painting when active.
  (let* ((world (game-world game))
         (camera (game-camera game))
         (ui (game-ui game))
         (assets (game-assets game)))
    (when (editor-active editor)
      (update-editor-status editor dt)
      (unless (ui-menu-open ui)
        (editor-update-mode editor)
        (editor-handle-zone-actions editor game)
        (when (raylib:is-key-pressed +key-q+)
          (cond
            ((eq (editor-mode editor) :spawn)
             (editor-adjust-spawn editor -1))
            ((eq (editor-mode editor) :object)
             (editor-adjust-object editor -1))
            ((member (editor-mode editor) '(:tile :collision))
             (editor-adjust-tileset editor game assets -1))))
        (when (raylib:is-key-pressed +key-e+)
          (cond
            ((eq (editor-mode editor) :spawn)
             (editor-adjust-spawn editor 1))
            ((eq (editor-mode editor) :object)
             (editor-adjust-object editor 1))
            ((member (editor-mode editor) '(:tile :collision))
             (editor-adjust-tileset editor game assets 1))))
        (editor-handle-tileset-picker editor)
        (when (raylib:is-key-pressed +key-f5+)
          (editor-export-zone editor world))
        (editor-update-camera editor world dt)
        (editor-apply-paint editor world camera)))))

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
             (brush-w (if (member (editor-mode editor) '(:tile :collision :object))
                          (max 1 (editor-selection-width editor))
                          1))
             (brush-h (if (member (editor-mode editor) '(:tile :collision :object))
                          (max 1 (editor-selection-height editor))
                          1))
             (x (round (* tx tile-size)))
             (y (round (* ty tile-size)))
             (width (round (* tile-size brush-w)))
             (height (round (* tile-size brush-h))))
        (raylib:draw-rectangle-lines x y width height
                                     *editor-cursor-color*)))))

(defun draw-editor-world-overlay (editor world camera)
  ;; Draw editor overlays in world space.
  (when (editor-active editor)
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
      (raylib:draw-text (editor-tileset-label-text editor) x y 16 (ui-menu-text-color ui))
      (incf y line)
      (raylib:draw-text (editor-tile-label editor) x y 16 (ui-menu-text-color ui))
      (incf y line)
      (raylib:draw-text (editor-object-label-text editor) x y 16 (ui-menu-text-color ui))
      (incf y line)
      (raylib:draw-text "1 tiles | 2 collision | 3 objects | 4 npcs | Q/E sheet (1/2) | Q/E object (3) | Q/E spawn (4) | click tile | shift+click block | F5 export" x y 16
                        (ui-menu-text-color ui))
      (incf y line)
      (raylib:draw-text "F6 new | F7 delete | F8/F9 cycle"
                        x y 16 (ui-menu-text-color ui))
      (incf y line)
      (raylib:draw-text "LMB paint | RMB erase" x y 16 (ui-menu-text-color ui))
      (when (editor-status-label editor)
        (incf y line)
        (raylib:draw-text (editor-status-label editor) x y 16
                          (ui-menu-text-color ui))))))
