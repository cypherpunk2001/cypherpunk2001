(in-package #:mmorpg)

(defun make-game ()
  ;; Assemble game state and log setup if verbose is enabled.
  (let* ((world (make-world))
         (player (make-player (/ *window-width* 2.0)
                              (/ *window-height* 2.0)))
         (npc (make-npc (+ (player-x player) (world-tile-dest-size world))
                        (player-y player)))
         (audio (make-audio))
         (ui (make-ui))
         (render (make-render))
         (assets (load-assets))
         (camera (make-camera)))
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
                :npc npc
                :audio audio
                :ui ui
                :render render
                :assets assets
                :camera camera)))

(defun shutdown-game (game)
  ;; Release game resources before exiting.
  (shutdown-audio (game-audio game))
  (unload-assets (game-assets game)))

(defun update-game (game dt)
  ;; Run one frame of input, audio, movement, and animation updates.
  (let* ((player (game-player game))
         (npc (game-npc game))
         (world (game-world game))
         (audio (game-audio game))
         (ui (game-ui game))
         (camera (game-camera game))
         (mouse-clicked (raylib:is-mouse-button-pressed +mouse-left+))
         (mouse-down (raylib:is-mouse-button-down +mouse-left+)))
    (update-audio audio)
    (update-camera-zoom camera)
    (update-ui-input ui audio mouse-clicked)
    (unless (ui-menu-open ui)
      (update-target-from-mouse player camera dt mouse-clicked mouse-down))
    (multiple-value-bind (input-dx input-dy)
        (update-input-direction player mouse-clicked)
      (let* ((moving (or (not (zerop input-dx))
                         (not (zerop input-dy))
                         (player-target-active player)))
             (speed-mult (update-running-state player dt moving (not mouse-clicked))))
        (update-player-position player world input-dx input-dy speed-mult dt)))
    (when (and (not (ui-menu-open ui))
               (raylib:is-key-pressed +key-space+))
      (start-player-attack player))
    (when *verbose-logs*
      (log-player-position player world))
    (update-player-animation player dt)
    (apply-melee-hit player npc world)
    (update-npc-behavior npc player world)
    (update-npc-movement npc player world dt)
    (update-npc-attack npc player world dt)
    (update-npc-animation npc dt)
    (combatant-update-hit-effect player dt)
    (combatant-update-hit-effect npc dt)))

(defun run ()
  ;; Entry point that initializes game state and drives the main loop.
  (raylib:with-window ("Hello MMO" (*window-width* *window-height*))
    (raylib:set-target-fps 60)
    (raylib:set-exit-key 0)
    (raylib:init-audio-device)
    (let ((game (make-game)))
      (unwind-protect
           (loop :until (or (raylib:window-should-close)
                            (ui-exit-requested (game-ui game)))
                 :do (let ((dt (raylib:get-frame-time)))
                       (update-game game dt)
                       (draw-game game)))
        (shutdown-game game)
        (raylib:close-audio-device)))))
