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
    (let* ((id-source (make-id-source))
           (player (make-player spawn-x spawn-y
                                :id (allocate-entity-id id-source)))
           (npcs (make-npcs player world :id-source id-source))
           (entities (make-entities player npcs))
           (audio (make-audio))
           (ui (make-ui))
           (render (make-render))
           (assets (load-assets world))
           (camera (make-camera))
           (editor (make-editor world assets player)))
      (when *editor-start-enabled*
        (toggle-editor-mode editor player))
      (setf (world-minimap-spawns world)
            (build-adjacent-minimap-spawns world player))
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
                  :id-source id-source
                  :audio audio
                  :ui ui
                  :render render
                  :assets assets
                  :camera camera
                  :editor editor))))

(defun shutdown-game (game)
  ;; Release game resources before exiting.
  (shutdown-audio (game-audio game))
  (unload-editor-tilesets (game-editor game) (game-assets game))
  (unload-assets (game-assets game)))

(defun update-client-input (game dt)
  ;; Read raylib input and update UI/audio state; writes player intent.
  (let* ((player (game-player game))
         (world (game-world game))
         (npcs (game-npcs game))
         (audio (game-audio game))
         (ui (game-ui game))
         (camera (game-camera game))
         (editor (game-editor game))
         (player-intent (player-intent player))
         (mouse-clicked (raylib:is-mouse-button-pressed +mouse-left+))
         (mouse-down (raylib:is-mouse-button-down +mouse-left+))
         (mouse-right-clicked (raylib:is-mouse-button-pressed +mouse-right+))
         (mouse-x (raylib:get-mouse-x))
         (mouse-y (raylib:get-mouse-y)))
    (update-audio audio)
    (update-camera-zoom camera)
    (update-ui-loading ui dt)
    (update-click-marker player dt)
    (ensure-preview-zones world player camera editor)
    (let ((menu-action (update-ui-input ui audio mouse-clicked)))
      (when (eq menu-action :toggle-editor)
        (toggle-editor-mode editor player)
        (setf (ui-menu-open ui) nil)))
    (when (or (ui-menu-open ui)
              (editor-active editor))
      (close-context-menu ui))
    (reset-frame-intent player-intent)
    (let ((click-consumed nil))
      (when (and (ui-context-open ui) mouse-clicked)
        (let ((action (handle-context-menu-click ui mouse-x mouse-y)))
          (when action
            (let ((context-x (ui-context-world-x ui))
                  (context-y (ui-context-world-y ui))
                  (context-id (ui-context-target-id ui)))
              (close-context-menu ui)
              (unless (eq action :close)
                (setf click-consumed t)
                (cond
                  ((eq action :walk)
                   (set-player-walk-target player player-intent
                                           context-x
                                           context-y
                                           t))
                  ((eq action :attack)
                   (let ((npc (find-npc-by-id npcs context-id)))
                     (when npc
                       (set-player-attack-target player player-intent npc t))))
                  ((eq action :follow)
                   (let ((npc (find-npc-by-id npcs context-id)))
                     (when npc
                       (set-player-follow-target player player-intent npc t))))))))))
      (when (and (not click-consumed)
                 (not (ui-menu-open ui))
                 (not (editor-active editor))
                 mouse-right-clicked)
        (multiple-value-bind (npc world-x world-y)
            (find-npc-at-screen npcs world player camera mouse-x mouse-y)
          (open-context-menu ui mouse-x mouse-y world-x world-y
                             :target-id (and npc (npc-id npc))))
        (setf click-consumed t))
      (unless (or (ui-menu-open ui)
                  (editor-active editor))
        (let ((minimap-handled (and (not click-consumed)
                                    (update-target-from-minimap player player-intent ui world
                                                                dt mouse-clicked mouse-down))))
          (unless (or click-consumed minimap-handled)
            (when mouse-clicked
              (multiple-value-bind (npc world-x world-y)
                  (find-npc-at-screen npcs world player camera mouse-x mouse-y)
                (if npc
                    (set-player-attack-target player player-intent npc t)
                    (set-player-walk-target player player-intent world-x world-y t))))
            (unless mouse-clicked
              (update-target-from-mouse player player-intent camera dt
                                        mouse-clicked mouse-down))))))
    (unless (editor-active editor)
      (update-input-direction player player-intent mouse-clicked))
    (unless (or (ui-menu-open ui)
                (editor-active editor))
      (update-input-actions player-intent (not mouse-clicked))
      (update-training-mode player))))

(defun reset-npc-frame-intents (npcs)
  ;; Clear per-tick intent signals for NPCs.
  (loop :for npc :across npcs
        :do (reset-frame-intent (npc-intent npc))))

(defun handle-zone-transition (game)
  ;; Sync client-facing state after a zone change.
  (let ((ui (game-ui game))
        (editor (game-editor game))
        (world (game-world game)))
    (ui-trigger-loading ui)
    (editor-sync-zone editor world)))

(defun update-sim (game dt &optional (allow-player-control t))
  ;; Run one fixed-tick simulation step. Returns true on zone transition.
  (let* ((player (game-player game))
         (npcs (game-npcs game))
         (entities (game-entities game))
         (world (game-world game))
         (ui (game-ui game))
         (player-intent (player-intent player)))
    (reset-npc-frame-intents npcs)
    (when allow-player-control
      (sync-player-follow-target player player-intent npcs)
      (sync-player-attack-target player player-intent npcs)
      (let* ((moving (or (not (zerop (intent-move-dx player-intent)))
                         (not (zerop (intent-move-dy player-intent)))
                         (intent-target-active player-intent)))
             (speed-mult (update-running-state player dt moving
                                               (intent-run-toggle player-intent))))
        (update-player-position player player-intent world speed-mult dt)))
    (let ((transitioned (and allow-player-control
                             (update-zone-transition game))))
      (when transitioned
        (setf npcs (game-npcs game)
              entities (game-entities game))
        (setf (player-attack-target-id player) 0
              (player-follow-target-id player) 0)
        (reset-npc-frame-intents npcs))
      (update-npc-respawns npcs dt)
      (when allow-player-control
        (update-player-attack-intent player npcs world))
      (when (and allow-player-control
                 (intent-attack player-intent))
        (start-player-attack player player-intent))
      (when *verbose-logs*
        (log-player-position player world))
      (loop :for entity :across entities
            :do (update-entity-animation entity dt))
      (loop :for npc :across npcs
            :do (apply-melee-hit player npc world ui))
      (loop :for npc :across npcs
            :do (update-npc-behavior npc player world)
                (update-npc-intent npc player world dt)
                (update-npc-movement npc world dt)
                (update-npc-attack npc player world dt ui))
      (loop :for entity :across entities
            :do (combatant-update-hit-effect entity dt))
      (consume-intent-actions player-intent)
      transitioned)))

(defun update-game (game dt accumulator)
  ;; Run one frame of input/audio and fixed-tick simulation updates.
  (update-client-input game dt)
  (let* ((ui (game-ui game))
         (editor (game-editor game))
         (allow-player-control (and (not (ui-menu-open ui))
                                    (not (editor-active editor))))
         (step *sim-tick-seconds*)
         (max-steps (max 1 *sim-max-steps-per-frame*)))
    (if (editor-active editor)
        (setf accumulator 0.0)
        (when (> step 0.0)
          (incf accumulator dt)
          (let ((max-accum (* step max-steps)))
            (when (> accumulator max-accum)
              (setf accumulator max-accum)))
          (loop :with steps = 0
                :while (and (>= accumulator step)
                            (< steps max-steps))
                :do (when (update-sim game step allow-player-control)
                      (handle-zone-transition game))
                    (decf accumulator step)
                    (incf steps)))))
  (update-editor (game-editor game) game dt)
  accumulator)

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
                 :with sim-accumulator = 0.0
                 :until (or (raylib:window-should-close)
                            (ui-exit-requested (game-ui game))
                            (and (> max-seconds 0.0)
                                 (>= elapsed max-seconds))
                            (and (> max-frames 0)
                                 (>= frames max-frames)))
                 :do (let ((dt (raylib:get-frame-time)))
                       (incf elapsed dt)
                       (incf frames)
                       (setf sim-accumulator
                             (update-game game dt sim-accumulator))
                       (draw-game game)))
        (shutdown-game game)
        (raylib:close-audio-device)))))
