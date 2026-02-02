;; NOTE: If you change behavior here, update docs/editor.md :)
;; Editor I/O: loading/saving map data, zone lifecycle, file I/O
(in-package #:mmorpg)

(defun editor-reset-game-for-zone (editor game)
  ;; Reset player/NPC positions after a zone change.
  (let* ((world (game-world game))
         (player (game-player game))
         (players (game-players game))
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
                           :id-source (game-npc-id-source game))))
      (ensure-npcs-open-spawn npcs world)
      (setf (game-npcs game) npcs
            (game-entities game) (make-entities players npcs))))))

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
    ;; Call client hook to clear render caches (set by rendering.lisp)
    (when *client-zone-change-hook*
      (funcall *client-zone-change-hook* (and zone (zone-id zone))))
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
        (progn
          (log-verbose "Editor loaded zone: ~a" path)
          (editor-activate-zone editor game zone path
                                (format nil "Zone loaded: ~a" (basename path))))
        (progn
          (log-verbose "Editor zone load failed: ~a" path)
          (editor-status editor "Zone load failed")))))

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
        (unless (write-zone zone path)
          (log-verbose "Editor zone write failed: ~a" path))
        (log-verbose "Editor created zone: ~a" path)
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
            (handler-case
                (delete-file path)
              (error (e)
                (warn "Failed to delete zone ~a: ~a" path e)
                (log-verbose "Zone delete error for ~a: ~a" path e)))
            (editor-load-zone editor game next-path)
            (editor-status editor (format nil "Zone deleted: ~a" (basename path))))
          (let* ((id (editor-zone-id-from-path path))
                 (width (max 1 *zone-default-width*))
                 (height (max 1 *zone-default-height*))
                 (chunk-size (max 1 *zone-default-chunk-size*))
                 (zone (make-empty-zone id width height :chunk-size chunk-size)))
            (write-zone zone path)
            (log-verbose "Editor reset last zone: ~a" path)
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

(defun editor-export-zone (editor world)
  ;; Export the current zone to disk.
  (let* ((zone (world-zone world))
         (path (editor-export-path editor)))
    (when (and zone path)
      (write-zone zone path)
      (setf (editor-dirty editor) nil)
      (editor-status editor "Zone exported"))))
