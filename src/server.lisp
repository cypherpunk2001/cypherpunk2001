;; NOTE: If you change behavior here, update docs/server.md :)
(in-package #:mmorpg)

(defun spawn-player-at-world (world id-source)
  ;; Spawn a player at the world spawn center on a valid open tile.
  (multiple-value-bind (center-x center-y)
      (world-spawn-center world)
    (multiple-value-bind (spawn-x spawn-y)
        (world-open-position world center-x center-y)
      (make-player spawn-x spawn-y
                   :id (when id-source
                         (allocate-entity-id id-source))))))

(defun make-sim-state ()
  ;; Build authoritative simulation state without client-only subsystems.
  (load-game-data)
  (let* ((world (make-world))
         (id-source (make-id-source))
         (player (spawn-player-at-world world id-source))
         (players (make-array 1 :initial-element player))
         (npcs (make-npcs player world :id-source id-source))
         (entities (make-entities players npcs))
         (combat-events (make-combat-event-queue)))
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
    (values world player players npcs entities id-source combat-events)))

(defun make-server-game ()
  ;; Build a headless game state for server-only simulation.
  (multiple-value-bind (world player players npcs entities id-source combat-events)
      (make-sim-state)
    (%make-game :world world
                :player player
                :players players
                :npcs npcs
                :entities entities
                :id-source id-source
                :audio nil
                :ui nil
                :render nil
                :assets nil
                :camera nil
                :editor nil
                :combat-events combat-events
                :client-intent nil
                :net-role :server
                :net-requests nil
                :net-player-id nil)))

(defun apply-client-intent (server-intent client-intent)
  ;; Copy the client intent payload into the server intent for this frame.
  (when (and server-intent client-intent)
    (setf (intent-move-dx server-intent) (intent-move-dx client-intent)
          (intent-move-dy server-intent) (intent-move-dy client-intent)
          (intent-face-dx server-intent) (intent-face-dx client-intent)
          (intent-face-dy server-intent) (intent-face-dy client-intent)
          (intent-target-x server-intent) (intent-target-x client-intent)
          (intent-target-y server-intent) (intent-target-y client-intent)
          (intent-target-active server-intent) (intent-target-active client-intent)
          (intent-attack server-intent) (intent-attack client-intent)
          (intent-run-toggle server-intent) (intent-run-toggle client-intent)
          (intent-requested-attack-target-id server-intent)
          (intent-requested-attack-target-id client-intent)
          (intent-requested-follow-target-id server-intent)
          (intent-requested-follow-target-id client-intent)
          (intent-requested-pickup-target-id server-intent)
          (intent-requested-pickup-target-id client-intent)
          (intent-requested-pickup-tx server-intent)
          (intent-requested-pickup-tx client-intent)
          (intent-requested-pickup-ty server-intent)
          (intent-requested-pickup-ty client-intent)
          (intent-requested-chat-message server-intent)
          (intent-requested-chat-message client-intent)))
  server-intent)

(defun server-step (game client-intent dt accumulator &key (allow-player-control t))
  ;; Apply client intent and run the fixed-tick server simulation.
  (let* ((player (game-player game))
         (server-intent (and player (player-intent player)))
         (chat-message (and client-intent
                            (intent-requested-chat-message client-intent))))
    (apply-client-intent server-intent client-intent)
    (when chat-message
      (clear-requested-chat-message client-intent))
    (let* ((step *sim-tick-seconds*)
           (max-steps (max 1 *sim-max-steps-per-frame*))
           (transition-count 0))
      (when (> step 0.0)
        (incf accumulator dt)
        (let ((max-accum (* step max-steps)))
          (when (> accumulator max-accum)
            (setf accumulator max-accum)))
        (loop :with steps = 0
              :while (and (>= accumulator step)
                          (< steps max-steps))
              :do (when (update-sim game step allow-player-control)
                    (incf transition-count))
                  (decf accumulator step)
                  (incf steps)))
      (values accumulator transition-count))))

(defun run-headless (&key (max-seconds 0.0) (max-frames 0))
  ;; Run a headless server loop without rendering.
  (let ((game (make-server-game)))
    (loop :with elapsed = 0.0
          :with frames = 0
          :with accumulator = 0.0
          :until (or (and (> max-seconds 0.0)
                          (>= elapsed max-seconds))
                     (and (> max-frames 0)
                          (>= frames max-frames)))
          :do (let ((dt *sim-tick-seconds*))
                (incf elapsed dt)
                (incf frames)
                (multiple-value-bind (new-acc transitions)
                    (server-step game nil dt accumulator)
                  (setf accumulator new-acc)
                  (dotimes (_ transitions)
                    (declare (ignore _))))))
    game))
