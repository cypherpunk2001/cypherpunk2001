;; NOTE: If you change behavior here, update docs/main.md :)
(in-package #:mmorpg)

(defun make-game ()
  ;; Assemble game state and log setup if verbose is enabled.
  (load-game-data)
  (let* ((world (make-world))
         (spawn-x nil)
         (spawn-y nil))
    (multiple-value-bind (center-x center-y)
        (world-spawn-center world)
      (multiple-value-setq (spawn-x spawn-y)
        (world-open-position world center-x center-y)))
    (let* ((player (make-player spawn-x spawn-y))
           (npcs (make-npcs player world))
           (entities (make-entities player npcs))
           (audio (make-audio))
           (ui (make-ui))
           (render (make-render))
           (assets (load-assets world))
           (camera (make-camera))
           (editor (make-editor world assets player)))
      (ensure-npcs-open-spawn npcs world)
      (when *verbose-logs*
        (format t "~&Verbose logs on. tile-size=~,2f collider-half=~,2f,~,2f wall=[~,2f..~,2f, ~,2f..~,2f]~%"
                (world-tile-dest-size world)
                (world-collision-half-width world)
                (world-collision-half-height world)
                (world-wall-min-x world)
                (world-wall-max-x world)
                (world-wall-min-y world)
                (world-wall-max-y world))
        (finish-output))
      (%make-game :world world
                  :player player
                  :npcs npcs
                  :entities entities
                  :audio audio
                  :ui ui
                  :render render
                  :assets assets
                  :camera camera
                  :editor editor))))

(defun shutdown-game (game)
  ;; Release game resources before exiting.
  (shutdown-audio (game-audio game))
  (unload-editor-objects (game-editor game))
  (unload-assets (game-assets game)))

(defun update-game (game dt)
  ;; Run one frame of input, audio, movement, and animation updates.
  (let* ((player (game-player game))
         (npcs (game-npcs game))
         (entities (game-entities game))
         (world (game-world game))
         (audio (game-audio game))
         (ui (game-ui game))
         (camera (game-camera game))
         (editor (game-editor game))
         (player-intent (player-intent player))
         (mouse-clicked (raylib:is-mouse-button-pressed +mouse-left+))
         (mouse-down (raylib:is-mouse-button-down +mouse-left+)))
    (update-audio audio)
    (update-camera-zoom camera)
    (update-ui-loading ui dt)
    (let ((menu-action (update-ui-input ui audio mouse-clicked)))
      (when (eq menu-action :toggle-editor)
        (toggle-editor-mode editor player)
        (setf (ui-menu-open ui) nil)))
    (reset-frame-intent player-intent)
    (loop :for npc :across npcs
          :do (reset-frame-intent (npc-intent npc)))
    (unless (or (ui-menu-open ui)
                (editor-active editor))
      (update-target-from-mouse player player-intent camera dt mouse-clicked mouse-down))
    (unless (editor-active editor)
      (update-input-direction player player-intent mouse-clicked))
    (unless (or (ui-menu-open ui)
                (editor-active editor))
      (update-input-actions player-intent (not mouse-clicked)))
    (unless (editor-active editor)
      (let* ((moving (or (not (zerop (intent-move-dx player-intent)))
                         (not (zerop (intent-move-dy player-intent)))
                         (intent-target-active player-intent)))
             (speed-mult (update-running-state player dt moving
                                               (intent-run-toggle player-intent))))
        (update-player-position player player-intent world speed-mult dt))
      (when (update-zone-transition game)
        (setf npcs (game-npcs game)
              entities (game-entities game))
        (ui-trigger-loading ui)
        (editor-sync-zone editor world))
      (when (and (not (ui-menu-open ui))
                 (intent-attack player-intent))
        (start-player-attack player player-intent)))
    (when *verbose-logs*
      (log-player-position player world))
    (update-editor editor game dt)
    (unless (editor-active editor)
      (loop :for entity :across entities
            :do (update-entity-animation entity dt))
      (loop :for npc :across npcs
            :do (apply-melee-hit player npc world))
      (loop :for npc :across npcs
            :do (update-npc-behavior npc player world)
                (update-npc-intent npc player world dt)
                (update-npc-movement npc world dt)
                (update-npc-attack npc player world dt))
      (loop :for entity :across entities
            :do (combatant-update-hit-effect entity dt)))))

(defun run (&key (max-seconds 0.0) (max-frames 0))
  ;; Entry point that initializes game state and drives the main loop.
  (raylib:with-window ("Hello MMO" (*window-width* *window-height*))
    (raylib:set-target-fps 60)
    (raylib:set-exit-key 0)
    (raylib:init-audio-device)
    (let ((game (make-game)))
      (unwind-protect
           (loop :with elapsed = 0.0
                 :with frames = 0
                 :until (or (raylib:window-should-close)
                            (ui-exit-requested (game-ui game))
                            (and (> max-seconds 0.0)
                                 (>= elapsed max-seconds))
                            (and (> max-frames 0)
                                 (>= frames max-frames)))
                 :do (let ((dt (raylib:get-frame-time)))
                       (incf elapsed dt)
                       (incf frames)
                       (update-game game dt)
                       (draw-game game)))
        (shutdown-game game)
        (raylib:close-audio-device)))))
