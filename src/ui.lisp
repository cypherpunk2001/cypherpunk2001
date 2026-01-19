;; NOTE: If you change behavior here, update docs/ui.md :)
(in-package #:mmorpg)

(defun make-stamina-labels ()
  ;; Precompute stamina HUD strings to avoid per-frame consing.
  (let* ((max (truncate *run-stamina-max*))
         (labels (make-array (1+ max))))
    (loop :for i :from 0 :to max
          :do (setf (aref labels i) (format nil "Stamina: ~2d" i)))
    labels))

(defun make-ui ()
  ;; Build UI layout constants and colors for the menu and HUD.
  (let* ((menu-open nil)
         (exit-requested nil)
         (loading-label "Loading...")
         (loading-timer 0.0)
         (menu-padding 32)
         (menu-panel-width (truncate (* *window-width* 0.92)))
         (menu-panel-height (truncate (* *window-height* 0.92)))
         (menu-panel-x (truncate (/ (- *window-width* menu-panel-width) 2)))
         (menu-panel-y (truncate (/ (- *window-height* menu-panel-height) 2)))
         (menu-title "Escape Menu")
         (menu-hint "Press Esc to close")
         (menu-track-title "Music")
         (menu-save-label "Save")
         (menu-load-label "Load")
         (menu-prev-label "Prev")
         (menu-next-label "Next")
         (menu-vol-down-label "Vol -")
         (menu-vol-up-label "Vol +")
         (menu-title-size 34)
         (menu-hint-size 18)
         (menu-track-size 20)
         (menu-button-text-size 22)
         (menu-nav-text-size 18)
         (menu-volume-text-size 18)
         ;; Bottom buttons (Unstuck above Logout)
         (menu-action-gap 12)
         (menu-logout-label "Logout")
         (menu-logout-width 260)
         (menu-logout-height 56)
         (menu-logout-x (truncate (/ (- *window-width* menu-logout-width) 2)))
         (menu-logout-y (- (+ menu-panel-y menu-panel-height)
                           menu-padding
                           menu-logout-height))
         (menu-unstuck-label "Unstuck")
         (menu-unstuck-width 260)
         (menu-unstuck-height 56)
         (menu-unstuck-x (truncate (/ (- *window-width* menu-unstuck-width) 2)))
         (menu-unstuck-y (- menu-logout-y menu-action-gap menu-unstuck-height))
         (menu-nav-button-width 140)
         (menu-nav-button-height 44)
         (menu-nav-gap 16)
         (menu-nav-y (+ menu-panel-y 140))
         (menu-prev-x (+ menu-panel-x menu-padding))
         (menu-next-x (+ menu-prev-x menu-nav-button-width menu-nav-gap))
         (menu-track-text-x (+ menu-panel-x menu-padding))
         (menu-track-text-y (+ menu-nav-y menu-nav-button-height 22))
         (menu-volume-button-width 110)
         (menu-volume-button-height 40)
         (menu-volume-gap 12)
         (menu-volume-y (+ menu-track-text-y 40))
         (menu-volume-down-x (+ menu-panel-x menu-padding))
         (menu-volume-up-x (+ menu-volume-down-x
                              menu-volume-button-width
                              menu-volume-gap))
         (menu-volume-bars-x (+ menu-volume-up-x
                                menu-volume-button-width
                                menu-volume-gap))
         (menu-toggle-gap 18)
         (menu-debug-size 18)
         (menu-debug-x (+ menu-panel-x menu-padding))
         (menu-debug-y (+ menu-volume-y menu-volume-button-height 24))
         (menu-debug-label "Debug Collision Overlay")
         (menu-editor-size 18)
         (menu-editor-x menu-debug-x)
         (menu-editor-y (+ menu-debug-y menu-debug-size menu-toggle-gap))
         (menu-editor-label "Editor Mode")
         (menu-fullscreen-size 18)
         (menu-fullscreen-x menu-debug-x)
         (menu-fullscreen-y (+ menu-editor-y menu-editor-size menu-toggle-gap))
         (menu-fullscreen-label "Fullscreen | Windowed")
         (menu-save-x (+ menu-panel-x menu-padding))
         (menu-save-y (+ menu-fullscreen-y menu-fullscreen-size menu-action-gap))
         (menu-load-x (+ menu-save-x menu-nav-button-width menu-nav-gap))
         (menu-load-y menu-save-y)
         (hud-bg-color (raylib:make-color :r 0 :g 0 :b 0 :a 160))
         (menu-overlay-color (raylib:make-color :r 0 :g 0 :b 0 :a 110))
         (menu-panel-color (raylib:make-color :r 18 :g 18 :b 18 :a 200))
         (menu-text-color (raylib:make-color :r 235 :g 235 :b 235 :a 255))
         (menu-button-color (raylib:make-color :r 170 :g 60 :b 60 :a 220))
         (menu-button-hover-color (raylib:make-color :r 210 :g 80 :b 80 :a 240))
         (inventory-open nil)
         (chat-active nil)
         (chat-buffer "")
         (chat-prompt "Say:")
         (chat-max-length *chat-max-length*)
         (hover-npc-name nil)
         (context-open nil)
         (context-x 0)
         (context-y 0)
         (context-world-x 0.0)
         (context-world-y 0.0)
         (context-target-id 0)
         (context-target-type nil)
         (context-object-id nil)
         (context-slot-index nil)
         (context-item-id nil)
         (context-has-walk t)
         (context-has-attack nil)
         (context-has-follow nil)
         (context-has-pickup nil)
         (context-has-examine nil)
         (context-has-drop nil)
         (context-width 160)
         (context-option-height 28)
         (context-padding 6)
         (context-text-size 18)
         (context-walk-label "Walk here")
         (context-attack-label "Attack")
         (context-follow-label "Follow")
         (context-pickup-label "Pick up")
         (context-examine-label "Examine")
         (context-drop-label "Drop")
         (minimap-width *minimap-width*)
         (minimap-height *minimap-height*)
         (minimap-point-size *minimap-point-size*)
         (minimap-x (- *window-width* *minimap-padding* *minimap-width*))
         (minimap-y *minimap-padding*)
         (minimap-bg-color *minimap-bg-color*)
         (minimap-border-color *minimap-border-color*)
         (minimap-player-color *minimap-player-color*)
         (minimap-npc-color *minimap-npc-color*)
         (minimap-collision-color *minimap-collision-color*)
         (debug-grid-color (raylib:make-color :r 255 :g 255 :b 255 :a 40))
         (debug-wall-color (raylib:make-color :r 80 :g 160 :b 255 :a 90))
         (debug-collision-color (raylib:make-color :r 255 :g 0 :b 0 :a 90))
         (debug-collider-color (raylib:make-color :r 0 :g 255 :b 0 :a 180))
         (stamina-labels (make-stamina-labels))
         (hud-stats-text-size 16)
         (hud-stats-line-gap 4)
         (hud-log-text-size 14)
         (hud-log-line-gap 2)
         (hud-log-lines 4)
         (hud-log-buffer (make-array (max 1 hud-log-lines)
                                      :initial-element ""))
         (hud-log-times (make-array (max 1 hud-log-lines)
                                    :initial-element 0.0))
         (hud-log-index 0)
         (hud-log-count 0)
         (combat-log-text-size 14)
         (combat-log-line-gap 2)
         (combat-log-lines 10)
         (combat-log-buffer (make-array (max 1 combat-log-lines)
                                         :initial-element ""))
         (combat-log-index 0)
         (combat-log-count 0))
    (%make-ui :menu-open menu-open
              :exit-requested exit-requested
              :loading-label loading-label
              :loading-timer loading-timer
              :login-active t
              :auth-complete nil
              :username-buffer ""
              :password-buffer ""
              :auth-error-message nil
              :server-selector-index 0
              :server-status :connecting
              :server-last-heard 0.0
              :server-next-ping 0.0
              :menu-padding menu-padding
              :menu-panel-width menu-panel-width
              :menu-panel-height menu-panel-height
              :menu-panel-x menu-panel-x
              :menu-panel-y menu-panel-y
              :menu-title menu-title
              :menu-hint menu-hint
              :menu-track-title menu-track-title
              :menu-save-label menu-save-label
              :menu-load-label menu-load-label
              :menu-prev-label menu-prev-label
              :menu-next-label menu-next-label
              :menu-vol-down-label menu-vol-down-label
              :menu-vol-up-label menu-vol-up-label
              :menu-title-size menu-title-size
              :menu-hint-size menu-hint-size
              :menu-track-size menu-track-size
              :menu-button-text-size menu-button-text-size
              :menu-nav-text-size menu-nav-text-size
              :menu-volume-text-size menu-volume-text-size
              :menu-logout-label menu-logout-label
              :menu-logout-x menu-logout-x
              :menu-logout-y menu-logout-y
              :menu-logout-width menu-logout-width
              :menu-logout-height menu-logout-height
              :menu-unstuck-label menu-unstuck-label
              :menu-unstuck-x menu-unstuck-x
              :menu-unstuck-y menu-unstuck-y
              :menu-unstuck-width menu-unstuck-width
              :menu-unstuck-height menu-unstuck-height
              :menu-save-x menu-save-x
              :menu-save-y menu-save-y
              :menu-load-x menu-load-x
              :menu-load-y menu-load-y
              :menu-nav-button-width menu-nav-button-width
              :menu-nav-button-height menu-nav-button-height
              :menu-nav-gap menu-nav-gap
              :menu-nav-y menu-nav-y
              :menu-prev-x menu-prev-x
              :menu-next-x menu-next-x
              :menu-track-text-x menu-track-text-x
              :menu-track-text-y menu-track-text-y
              :menu-volume-button-width menu-volume-button-width
              :menu-volume-button-height menu-volume-button-height
              :menu-volume-gap menu-volume-gap
              :menu-volume-y menu-volume-y
              :menu-volume-down-x menu-volume-down-x
              :menu-volume-up-x menu-volume-up-x
              :menu-volume-bars-x menu-volume-bars-x
              :menu-toggle-gap menu-toggle-gap
              :menu-debug-size menu-debug-size
              :menu-debug-x menu-debug-x
              :menu-debug-y menu-debug-y
              :menu-debug-label menu-debug-label
              :menu-editor-size menu-editor-size
              :menu-editor-x menu-editor-x
              :menu-editor-y menu-editor-y
              :menu-editor-label menu-editor-label
              :menu-fullscreen-size menu-fullscreen-size
              :menu-fullscreen-x menu-fullscreen-x
              :menu-fullscreen-y menu-fullscreen-y
              :menu-fullscreen-label menu-fullscreen-label
              :hud-bg-color hud-bg-color
              :menu-overlay-color menu-overlay-color
              :menu-panel-color menu-panel-color
              :menu-text-color menu-text-color
              :menu-button-color menu-button-color
              :menu-button-hover-color menu-button-hover-color
              :inventory-open inventory-open
              :chat-active chat-active
              :chat-buffer chat-buffer
              :chat-prompt chat-prompt
              :chat-max-length chat-max-length
              :hover-npc-name hover-npc-name
              :context-open context-open
              :context-x context-x
              :context-y context-y
              :context-world-x context-world-x
              :context-world-y context-world-y
              :context-target-id context-target-id
              :context-target-type context-target-type
              :context-object-id context-object-id
              :context-slot-index context-slot-index
              :context-item-id context-item-id
              :context-has-walk context-has-walk
              :context-has-attack context-has-attack
              :context-has-follow context-has-follow
              :context-has-pickup context-has-pickup
              :context-has-examine context-has-examine
              :context-has-drop context-has-drop
              :context-width context-width
              :context-option-height context-option-height
              :context-padding context-padding
              :context-text-size context-text-size
              :context-walk-label context-walk-label
              :context-attack-label context-attack-label
              :context-follow-label context-follow-label
              :context-pickup-label context-pickup-label
              :context-examine-label context-examine-label
              :context-drop-label context-drop-label
              :minimap-x minimap-x
              :minimap-y minimap-y
              :minimap-width minimap-width
              :minimap-height minimap-height
              :minimap-point-size minimap-point-size
              :minimap-bg-color minimap-bg-color
              :minimap-border-color minimap-border-color
              :minimap-player-color minimap-player-color
              :minimap-npc-color minimap-npc-color
              :minimap-collision-color minimap-collision-color
              :debug-grid-color debug-grid-color
              :debug-wall-color debug-wall-color
              :debug-collision-color debug-collision-color
              :debug-collider-color debug-collider-color
              :stamina-labels stamina-labels
              :hud-stats-text-size hud-stats-text-size
              :hud-stats-line-gap hud-stats-line-gap
              :hud-log-text-size hud-log-text-size
              :hud-log-line-gap hud-log-line-gap
              :hud-log-lines hud-log-lines
              :hud-log-index hud-log-index
              :hud-log-count hud-log-count
              :hud-log-buffer hud-log-buffer
              :hud-log-times hud-log-times
              :combat-log-text-size combat-log-text-size
              :combat-log-line-gap combat-log-line-gap
              :combat-log-lines combat-log-lines
              :combat-log-index combat-log-index
              :combat-log-count combat-log-count
              :combat-log-buffer combat-log-buffer)))

(defun ui-push-combat-log (ui text)
  ;; Append TEXT to the UI combat log ring buffer.
  (when (and ui text)
    (let* ((buffer (ui-combat-log-buffer ui))
           (cap (length buffer)))
      (when (> cap 0)
        (let ((index (ui-combat-log-index ui)))
          (setf (aref buffer index) text
                (ui-combat-log-index ui) (mod (1+ index) cap)
                (ui-combat-log-count ui) (min cap (1+ (ui-combat-log-count ui))))))))
  ui)

(defun ui-push-hud-log (ui text)
  ;; Append TEXT to the HUD feedback ring buffer.
  (when (and ui text (not (string= text "")))
    (let* ((buffer (ui-hud-log-buffer ui))
           (times (ui-hud-log-times ui))
           (cap (length buffer)))
      (when (> cap 0)
        (let* ((index (ui-hud-log-index ui))
               (prev-index (mod (- index 1) cap))
               (prev (if (> (ui-hud-log-count ui) 0)
                         (aref buffer prev-index)
                         nil)))
          (if (and prev (string= prev text))
              (setf (aref times prev-index) *hud-log-line-seconds*)
              (progn
                (setf (aref buffer index) text
                      (aref times index) *hud-log-line-seconds*
                      (ui-hud-log-index ui) (mod (1+ index) cap)
                      (ui-hud-log-count ui) (min cap (1+ (ui-hud-log-count ui))))))))))
  ui)

(defun update-ui-hud-log (ui dt)
  ;; Tick down HUD log message timers.
  (let* ((times (and ui (ui-hud-log-times ui)))
         (cap (and times (length times))))
    (when (and cap (> cap 0))
      (dotimes (i cap)
        (let ((timer (aref times i)))
          (when (> timer 0.0)
            (setf (aref times i) (max 0.0 (- timer dt))))))))
  ui)

(defun handle-menu-click (ui audio mouse-x mouse-y)
  ;; Process menu clicks for quit, logout, unstuck, music, volume, and toggles.
  (cond
    ((point-in-rect-p mouse-x mouse-y
                      (ui-menu-logout-x ui) (ui-menu-logout-y ui)
                      (ui-menu-logout-width ui) (ui-menu-logout-height ui))
     :logout)
    ((point-in-rect-p mouse-x mouse-y
                      (ui-menu-unstuck-x ui) (ui-menu-unstuck-y ui)
                      (ui-menu-unstuck-width ui) (ui-menu-unstuck-height ui))
     :unstuck)
    ((and (> (audio-soundtrack-count audio) 0)
          (point-in-rect-p mouse-x mouse-y
                           (ui-menu-prev-x ui) (ui-menu-nav-y ui)
                           (ui-menu-nav-button-width ui)
                           (ui-menu-nav-button-height ui)))
     (audio-advance-track audio -1))
    ((and (> (audio-soundtrack-count audio) 0)
          (point-in-rect-p mouse-x mouse-y
                           (ui-menu-next-x ui) (ui-menu-nav-y ui)
                           (ui-menu-nav-button-width ui)
                           (ui-menu-nav-button-height ui)))
     (audio-advance-track audio 1))
    ((point-in-rect-p mouse-x mouse-y
                      (ui-menu-volume-down-x ui) (ui-menu-volume-y ui)
                      (ui-menu-volume-button-width ui)
                      (ui-menu-volume-button-height ui))
     (audio-adjust-volume audio -1))
    ((point-in-rect-p mouse-x mouse-y
                      (ui-menu-volume-up-x ui) (ui-menu-volume-y ui)
                      (ui-menu-volume-button-width ui)
                      (ui-menu-volume-button-height ui))
     (audio-adjust-volume audio 1))
    ((point-in-rect-p mouse-x mouse-y
                      (ui-menu-debug-x ui) (ui-menu-debug-y ui)
                      (ui-menu-debug-size ui) (ui-menu-debug-size ui))
     (let ((enabled (not *debug-collision-overlay*)))
       (setf *debug-collision-overlay* enabled
             *debug-npc-logs* enabled)))
    ((point-in-rect-p mouse-x mouse-y
                      (ui-menu-editor-x ui) (ui-menu-editor-y ui)
                      (ui-menu-editor-size ui) (ui-menu-editor-size ui))
     :toggle-editor)
    ((point-in-rect-p mouse-x mouse-y
                      (ui-menu-fullscreen-x ui) (ui-menu-fullscreen-y ui)
                      (ui-menu-fullscreen-size ui)
                      (ui-menu-fullscreen-size ui))
     (raylib:toggle-fullscreen))))

(defun update-ui-input (ui audio mouse-clicked)
  ;; Handle UI toggle input and click interactions.
  (let ((action nil))
    (when (and (not (ui-chat-active ui))
               (raylib:is-key-pressed +key-escape+))
      (setf (ui-menu-open ui) (not (ui-menu-open ui))))
    (when (and (ui-menu-open ui)
               mouse-clicked
               (not (ui-chat-active ui)))
      (setf action (handle-menu-click ui audio
                                      (raylib:get-mouse-x)
                                      (raylib:get-mouse-y))))
    action))

(defun open-context-menu (ui screen-x screen-y world-x world-y
                             &key target-id target-type object-id slot-index item-id)
  ;; Open a context menu anchored at the screen coordinates.
  (let ((id (or target-id 0))
        (target-type (or target-type :world)))
    (setf (ui-context-open ui) t
          (ui-context-x ui) (truncate screen-x)
          (ui-context-y ui) (truncate screen-y)
          (ui-context-world-x ui) world-x
          (ui-context-world-y ui) world-y
          (ui-context-target-id ui) id
          (ui-context-target-type ui) target-type
          (ui-context-object-id ui) object-id
          (ui-context-slot-index ui) slot-index
          (ui-context-item-id ui) item-id
          (ui-context-has-walk ui) (not (eq target-type :inventory))
          (ui-context-has-attack ui) (eq target-type :npc)
          (ui-context-has-follow ui) (eq target-type :npc)
          (ui-context-has-pickup ui) (eq target-type :object)
          (ui-context-has-examine ui) (or (eq target-type :npc)
                                          (eq target-type :object)
                                          (eq target-type :inventory))
          (ui-context-has-drop ui) (eq target-type :inventory)))
  ui)

(defun close-context-menu (ui)
  ;; Close any open context menu.
  (setf (ui-context-open ui) nil
        (ui-context-target-id ui) 0
        (ui-context-target-type ui) nil
        (ui-context-object-id ui) nil
        (ui-context-slot-index ui) nil
        (ui-context-item-id ui) nil
        (ui-context-has-walk ui) t
        (ui-context-has-attack ui) nil
        (ui-context-has-follow ui) nil
        (ui-context-has-pickup ui) nil
        (ui-context-has-examine ui) nil
        (ui-context-has-drop ui) nil)
  ui)

(defun context-menu-actions (ui)
  ;; Return the ordered list of context menu actions.
  (let ((actions nil))
    (when (ui-context-has-walk ui)
      (push :walk actions))
    (when (ui-context-has-attack ui)
      (push :attack actions))
    (when (ui-context-has-follow ui)
      (push :follow actions))
    (when (ui-context-has-pickup ui)
      (push :pickup actions))
    (when (ui-context-has-examine ui)
      (push :examine actions))
    (when (ui-context-has-drop ui)
      (push :drop actions))
    (nreverse actions)))

(defun context-menu-option-count (ui)
  ;; Return the number of context menu options.
  (length (context-menu-actions ui)))

(defun context-menu-action-for-index (ui index)
  ;; Map a menu INDEX to an action keyword, if valid.
  (let ((actions (context-menu-actions ui)))
    (when (and actions (<= 0 index) (< index (length actions)))
      (nth index actions))))

(defun handle-context-menu-click (ui mouse-x mouse-y)
  ;; Handle a click against the context menu; returns an action, :close, or nil.
  (when (ui-context-open ui)
    (let* ((x (ui-context-x ui))
           (y (ui-context-y ui))
           (width (ui-context-width ui))
           (option-height (ui-context-option-height ui))
           (count (context-menu-option-count ui))
           (height (* count option-height)))
      (if (point-in-rect-p mouse-x mouse-y x y width height)
          (let* ((index (floor (/ (- mouse-y y) option-height))))
            (context-menu-action-for-index ui index))
          :close))))

(defun inventory-grid-layout (ui &optional (slot-count *inventory-size*)
                                        (columns *inventory-grid-columns*))
  ;; Return layout values for the inventory grid.
  (let* ((columns (max 1 columns))
         (rows (max 1 (ceiling slot-count columns)))
         (panel-x (ui-menu-panel-x ui))
         (panel-y (ui-menu-panel-y ui))
         (panel-width (ui-menu-panel-width ui))
         (panel-height (ui-menu-panel-height ui))
         (padding (ui-menu-padding ui))
         (header-height 36)
         (gap (max 0 *inventory-slot-gap*))
         (grid-width (- panel-width (* 2 padding)))
         (grid-height (- panel-height (* 2 padding) header-height))
         (slot-size (max 16 (floor (min (/ (- grid-width (* gap (1- columns))) columns)
                                          (/ (- grid-height (* gap (1- rows))) rows)))))
         (grid-x (+ panel-x padding))
         (grid-y (+ panel-y padding header-height)))
    (values grid-x grid-y slot-size gap columns rows
            panel-x panel-y panel-width panel-height header-height padding)))

(defun inventory-slot-at-screen (ui screen-x screen-y
                                    &optional (slot-count *inventory-size*)
                                    (columns *inventory-grid-columns*))
  ;; Return the inventory slot index at SCREEN-X/Y, or nil.
  (multiple-value-bind (grid-x grid-y slot-size gap columns rows)
      (inventory-grid-layout ui slot-count columns)
    (let* ((spacing (+ slot-size gap))
           (local-x (- screen-x grid-x))
           (local-y (- screen-y grid-y)))
      (when (and (>= local-x 0) (>= local-y 0))
        (let* ((col (floor (/ local-x spacing)))
               (row (floor (/ local-y spacing)))
               (offset-x (- local-x (* col spacing)))
               (offset-y (- local-y (* row spacing)))
               (index (+ (* row columns) col)))
          (when (and (< col columns)
                     (< row rows)
                     (< offset-x slot-size)
                     (< offset-y slot-size)
                     (< index slot-count))
            index))))))

;;;; Inventory Drag-and-Drop

(defun ui-start-inventory-drag (ui slot-index item-id x y)
  ;; Start dragging an inventory item from SLOT-INDEX.
  (setf (ui-drag-active ui) t
        (ui-drag-slot-index ui) slot-index
        (ui-drag-item-id ui) item-id
        (ui-drag-start-x ui) x
        (ui-drag-start-y ui) y))

(defun ui-end-inventory-drag (ui)
  ;; End the current drag operation and return the source slot index.
  (let ((slot-index (ui-drag-slot-index ui)))
    (setf (ui-drag-active ui) nil
          (ui-drag-slot-index ui) nil
          (ui-drag-item-id ui) nil
          (ui-drag-start-x ui) nil
          (ui-drag-start-y ui) nil)
    slot-index))

(defun ui-cancel-inventory-drag (ui)
  ;; Cancel the drag operation without performing any action.
  (ui-end-inventory-drag ui)
  nil)

(defun ui-trigger-loading (ui &optional (seconds *zone-loading-seconds*))
  ;; Ensure the loading overlay stays visible for at least SECONDS.
  (when (and seconds (> seconds 0.0))
    (setf (ui-loading-timer ui)
          (max (ui-loading-timer ui) seconds))))

(defun update-ui-loading (ui dt)
  ;; Advance the loading overlay timer.
  (when (> (ui-loading-timer ui) 0.0)
    (setf (ui-loading-timer ui)
          (max 0.0 (- (ui-loading-timer ui) dt)))))

;;;; Login Screen

(defparameter *login-max-username-length* 20
  "Maximum characters for username input.")

(defparameter *login-max-password-length* 30
  "Maximum characters for password input.")

(defun update-login-input (ui)
  "Handle text input for username and password fields."
  (when (ui-login-active ui)
    (let ((key (raylib:get-char-pressed)))
      (loop :while (not (zerop key))
            :do (let ((char (code-char key)))
                  ;; Add to username buffer (simple, no field focus for MVP)
                  (when (and (graphic-char-p char)
                            (< (length (ui-username-buffer ui)) *login-max-username-length*))
                    (setf (ui-username-buffer ui)
                          (concatenate 'string (ui-username-buffer ui) (string char)))))
                (setf key (raylib:get-char-pressed))))

    ;; Handle backspace
    (when (raylib:is-key-pressed +key-backspace+)
      (let ((username (ui-username-buffer ui)))
        (when (plusp (length username))
          (setf (ui-username-buffer ui)
                (subseq username 0 (1- (length username)))))))))

(defun draw-login-screen (ui)
  "Draw the login/register screen."
  (when (ui-login-active ui)
    (let* ((screen-width *window-width*)
           (screen-height *window-height*)
           (panel-width 500)
           (panel-height 550)
           (panel-x (truncate (/ (- screen-width panel-width) 2)))
           (panel-y (truncate (/ (- screen-height panel-height) 2)))
           (title-text "MMORPG Login")
           (title-size 40)
           (label-size 20)
           (input-size 18)
           (error-size 16)
           (button-width 200)
           (button-height 50)
           (input-width 400)
           (input-height 45)
           (padding 30)
           (bg-color (raylib:make-color :r 20 :g 20 :b 30 :a 255))
           (panel-color (raylib:make-color :r 40 :g 40 :b 50 :a 255))
           (text-color (raylib:make-color :r 220 :g 220 :b 220 :a 255))
           (input-bg-color (raylib:make-color :r 30 :g 30 :b 40 :a 255))
           (button-color (raylib:make-color :r 60 :g 120 :b 180 :a 255))
           (button-hover-color (raylib:make-color :r 80 :g 150 :b 220 :a 255))
           (register-button-color (raylib:make-color :r 80 :g 140 :b 80 :a 255))
           (register-button-hover-color (raylib:make-color :r 100 :g 180 :b 100 :a 255))
           (error-color (raylib:make-color :r 220 :g 60 :b 60 :a 255)))

      ;; Background
      (raylib:clear-background bg-color)

      ;; Panel
      (raylib:draw-rectangle panel-x panel-y panel-width panel-height panel-color)

      ;; Title
      (let* ((title-width (raylib:measure-text title-text title-size))
             (title-x (truncate (/ (- screen-width title-width) 2)))
             (title-y (+ panel-y padding)))
        (raylib:draw-text title-text title-x title-y title-size text-color))

      ;; Server status indicator
      (let* ((status (ui-server-status ui))
             (status-y (+ panel-y padding 50))
             (status-text (case status
                           (:online "Server Online")
                           (:offline "Server Offline")
                           (t "Connecting...")))
             (status-color (case status
                            (:online (raylib:make-color :r 60 :g 200 :b 60 :a 255))
                            (:offline (raylib:make-color :r 200 :g 60 :b 60 :a 255))
                            (t (raylib:make-color :r 200 :g 200 :b 60 :a 255))))
             (status-text-width (raylib:measure-text status-text 16))
             (status-x (truncate (/ (- screen-width (+ status-text-width 20)) 2))))
        (raylib:draw-circle (+ status-x 6) (+ status-y 8) 6.0 status-color)
        (raylib:draw-text status-text (+ status-x 20) status-y 16 status-color))

      ;; Username label
      (let ((username-label-y (+ panel-y padding 80)))
        (raylib:draw-text "Username:" (+ panel-x padding) username-label-y label-size text-color)

        ;; Username input box
        (let ((input-x (+ panel-x (truncate (/ (- panel-width input-width) 2))))
              (input-y (+ username-label-y 30)))
          (raylib:draw-rectangle input-x input-y input-width input-height input-bg-color)
          (raylib:draw-rectangle-lines input-x input-y input-width input-height text-color)
          (raylib:draw-text (ui-username-buffer ui) (+ input-x 10) (+ input-y 12) input-size text-color)))

      ;; Password label and hint (no real password input for MVP - just text saying to use username)
      (let ((password-label-y (+ panel-y padding 200)))
        (raylib:draw-text "Password:" (+ panel-x padding) password-label-y label-size text-color)
        (raylib:draw-text "(For MVP: will be same as username)"
                         (+ panel-x padding)
                         (+ password-label-y 25)
                         14
                         (raylib:make-color :r 150 :g 150 :b 150 :a 255)))

      ;; Error message (if any)
      (when (ui-auth-error-message ui)
        (let* ((error-y (+ panel-y padding 280))
               (error-text (ui-auth-error-message ui))
               (error-width (raylib:measure-text error-text error-size))
               (error-x (truncate (/ (- screen-width error-width) 2))))
          (raylib:draw-text error-text error-x error-y error-size error-color)))

      ;; Login button
      (let* ((button-y (+ panel-y 330))
             (login-button-x (truncate (/ (- screen-width button-width) 2)))
             (mouse-x (raylib:get-mouse-x))
             (mouse-y (raylib:get-mouse-y))
             (login-hover (and (>= mouse-x login-button-x)
                              (<= mouse-x (+ login-button-x button-width))
                              (>= mouse-y button-y)
                              (<= mouse-y (+ button-y button-height))))
             (login-color (if login-hover button-hover-color button-color)))

        (raylib:draw-rectangle login-button-x button-y button-width button-height login-color)
        (let* ((text "Login")
               (text-width (raylib:measure-text text 24))
               (text-x (+ login-button-x (truncate (/ (- button-width text-width) 2))))
               (text-y (+ button-y 12)))
          (raylib:draw-text text text-x text-y 24 text-color))

        ;; Return :login if clicked
        (when (and login-hover (raylib:is-mouse-button-pressed +mouse-left+))
          (return-from draw-login-screen :login)))

      ;; Register button
      (let* ((button-y (+ panel-y 400))
             (register-button-x (truncate (/ (- screen-width button-width) 2)))
             (mouse-x (raylib:get-mouse-x))
             (mouse-y (raylib:get-mouse-y))
             (register-hover (and (>= mouse-x register-button-x)
                                 (<= mouse-x (+ register-button-x button-width))
                                 (>= mouse-y button-y)
                                 (<= mouse-y (+ button-y button-height))))
             (register-color (if register-hover register-button-hover-color register-button-color)))

        (raylib:draw-rectangle register-button-x button-y button-width button-height register-color)
        (let* ((text "Register")
               (text-width (raylib:measure-text text 24))
               (text-x (+ register-button-x (truncate (/ (- button-width text-width) 2))))
               (text-y (+ button-y 12)))
          (raylib:draw-text text text-x text-y 24 text-color))

        ;; Return :register if clicked
        (when (and register-hover (raylib:is-mouse-button-pressed +mouse-left+))
          (return-from draw-login-screen :register)))

      ;; Quit button
      (let* ((button-y (+ panel-y 470))
             (quit-button-x (truncate (/ (- screen-width button-width) 2)))
             (mouse-x (raylib:get-mouse-x))
             (mouse-y (raylib:get-mouse-y))
             (quit-hover (and (>= mouse-x quit-button-x)
                             (<= mouse-x (+ quit-button-x button-width))
                             (>= mouse-y button-y)
                             (<= mouse-y (+ button-y button-height))))
             (quit-color (if quit-hover
                            (raylib:make-color :r 210 :g 80 :b 80 :a 240)
                            (raylib:make-color :r 170 :g 60 :b 60 :a 220))))

        (raylib:draw-rectangle quit-button-x button-y button-width button-height quit-color)
        (let* ((text "Quit")
               (text-width (raylib:measure-text text 24))
               (text-x (+ quit-button-x (truncate (/ (- button-width text-width) 2))))
               (text-y (+ button-y 12)))
          (raylib:draw-text text text-x text-y 24 text-color))

        ;; Quit application if clicked
        (when (and quit-hover (raylib:is-mouse-button-pressed +mouse-left+))
          (setf (ui-exit-requested ui) t)))

      ;; Server selector (localhost:1337 for now)
      (let ((server-y (+ panel-y panel-height (- padding) 5)))
        (raylib:draw-text "Server: localhost:1337"
                         (+ panel-x padding)
                         server-y
                         14
                         (raylib:make-color :r 150 :g 150 :b 150 :a 255))
        ;; F11 hint
        (raylib:draw-text "[F11 Fullscreen]"
                         (+ panel-x padding)
                         (+ server-y 18)
                         12
                         (raylib:make-color :r 120 :g 120 :b 120 :a 255))))

    ;; Return nil if no button clicked
    nil))
