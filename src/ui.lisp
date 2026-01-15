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
         (menu-button-label "Quit")
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
         (menu-button-width 260)
         (menu-button-height 56)
         (menu-button-x (truncate (/ (- *window-width* menu-button-width) 2)))
         (menu-button-y (- (+ menu-panel-y menu-panel-height)
                           menu-padding
                           menu-button-height))
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
         (hud-bg-color (raylib:make-color :r 0 :g 0 :b 0 :a 160))
         (menu-overlay-color (raylib:make-color :r 0 :g 0 :b 0 :a 110))
         (menu-panel-color (raylib:make-color :r 18 :g 18 :b 18 :a 200))
         (menu-text-color (raylib:make-color :r 235 :g 235 :b 235 :a 255))
         (menu-button-color (raylib:make-color :r 170 :g 60 :b 60 :a 220))
         (menu-button-hover-color (raylib:make-color :r 210 :g 80 :b 80 :a 240))
         (context-open nil)
         (context-x 0)
         (context-y 0)
         (context-world-x 0.0)
         (context-world-y 0.0)
         (context-target-id 0)
         (context-has-attack nil)
         (context-has-follow nil)
         (context-width 160)
         (context-option-height 28)
         (context-padding 6)
         (context-text-size 18)
         (context-walk-label "Walk here")
         (context-attack-label "Attack")
         (context-follow-label "Follow")
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
              :menu-padding menu-padding
              :menu-panel-width menu-panel-width
              :menu-panel-height menu-panel-height
              :menu-panel-x menu-panel-x
              :menu-panel-y menu-panel-y
              :menu-title menu-title
              :menu-hint menu-hint
              :menu-track-title menu-track-title
              :menu-button-label menu-button-label
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
              :menu-button-width menu-button-width
              :menu-button-height menu-button-height
              :menu-button-x menu-button-x
              :menu-button-y menu-button-y
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
              :context-open context-open
              :context-x context-x
              :context-y context-y
              :context-world-x context-world-x
              :context-world-y context-world-y
              :context-target-id context-target-id
              :context-has-attack context-has-attack
              :context-has-follow context-has-follow
              :context-width context-width
              :context-option-height context-option-height
              :context-padding context-padding
              :context-text-size context-text-size
              :context-walk-label context-walk-label
              :context-attack-label context-attack-label
              :context-follow-label context-follow-label
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

(defun handle-menu-click (ui audio mouse-x mouse-y)
  ;; Process menu clicks for quit, music, volume, and toggles.
  (cond
    ((point-in-rect-p mouse-x mouse-y
                      (ui-menu-button-x ui) (ui-menu-button-y ui)
                      (ui-menu-button-width ui) (ui-menu-button-height ui))
     (setf (ui-exit-requested ui) t))
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
  (when (raylib:is-key-pressed +key-escape+)
    (setf (ui-menu-open ui) (not (ui-menu-open ui))))
  (when (and (ui-menu-open ui) mouse-clicked)
    (setf action (handle-menu-click ui audio
                                    (raylib:get-mouse-x)
                                    (raylib:get-mouse-y))))
    action))

(defun open-context-menu (ui screen-x screen-y world-x world-y &key target-id)
  ;; Open a context menu anchored at the screen coordinates.
  (let ((id (or target-id 0)))
    (setf (ui-context-open ui) t
          (ui-context-x ui) (truncate screen-x)
          (ui-context-y ui) (truncate screen-y)
          (ui-context-world-x ui) world-x
          (ui-context-world-y ui) world-y
          (ui-context-target-id ui) id
          (ui-context-has-attack ui) (> id 0)
          (ui-context-has-follow ui) (> id 0)))
  ui)

(defun close-context-menu (ui)
  ;; Close any open context menu.
  (setf (ui-context-open ui) nil
        (ui-context-target-id ui) 0
        (ui-context-has-attack ui) nil
        (ui-context-has-follow ui) nil)
  ui)

(defun context-menu-option-count (ui)
  ;; Return the number of context menu options.
  (+ 1
     (if (ui-context-has-attack ui) 1 0)
     (if (ui-context-has-follow ui) 1 0)))

(defun handle-context-menu-click (ui mouse-x mouse-y)
  ;; Handle a click against the context menu; returns :walk, :attack, :close, or nil.
  (when (ui-context-open ui)
    (let* ((x (ui-context-x ui))
           (y (ui-context-y ui))
           (width (ui-context-width ui))
           (option-height (ui-context-option-height ui))
           (count (context-menu-option-count ui))
           (height (* count option-height)))
      (if (point-in-rect-p mouse-x mouse-y x y width height)
          (let* ((index (floor (/ (- mouse-y y) option-height))))
            (cond
              ((= index 0) :walk)
              ((and (= index 1) (ui-context-has-attack ui)) :attack)
              ((and (= index 2) (ui-context-has-follow ui)) :follow)
              (t nil)))
          :close))))

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
