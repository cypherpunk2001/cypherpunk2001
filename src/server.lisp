;; NOTE: If you change behavior here, update docs/server.md :)
(in-package #:mmorpg)

(defun spawn-player-at-world (world id-source)
  ;; Spawn a player at a valid open tile in the starting zone.
  ;; Uses zone-state collision for correct per-zone spawn coordinates (Phase 1).
  (let* ((zone-path (zone-path-for-id world *starting-zone-id*))
         (zone-state (when zone-path
                       (get-or-create-zone-state *starting-zone-id* zone-path))))
    (if zone-state
        ;; Use zone-state for per-zone spawn position
        (multiple-value-bind (spawn-x spawn-y)
            (zone-state-spawn-position zone-state)
          (make-player spawn-x spawn-y
                       :id (when id-source
                             (allocate-entity-id id-source))
                       :zone-id *starting-zone-id*))
        ;; Fallback to global world collision if zone-state unavailable
        (progn
          (warn "spawn-player-at-world: zone-state unavailable for ~a, using global collision"
                *starting-zone-id*)
          (multiple-value-bind (center-x center-y)
              (world-spawn-center world)
            (multiple-value-bind (spawn-x spawn-y)
                (world-open-position world center-x center-y)
              (make-player spawn-x spawn-y
                           :id (when id-source
                                 (allocate-entity-id id-source))
                           :zone-id *starting-zone-id*)))))))

(defun make-sim-state (&key (server-mode nil))
  ;; Build authoritative simulation state without client-only subsystems.
  ;; If SERVER-MODE is T, starts with no players (auth system adds players).
  ;; If SERVER-MODE is NIL (client/local), creates one initial player.
  (load-game-data)
  (let* ((world (make-world))
         ;; Player ID source: persistent, loads from storage in server mode
         (id-source (if (and server-mode (boundp '*storage*) *storage*)
                        ;; Server mode with storage: load counter from DB
                        (let ((saved-counter (db-load-id-counter)))
                          (log-verbose "Loaded ID counter from storage: ~d" saved-counter)
                          (make-id-source saved-counter t))  ; persistent=t
                        ;; Client mode or no storage: start from 1
                        (make-id-source)))
         ;; NPC ID source: local, starts at 1000000 to avoid player ID conflicts
         (npc-id-source (make-id-source 1000000 nil))  ; persistent=nil
         (player (if server-mode
                     nil
                     (spawn-player-at-world world id-source)))
         (players (if server-mode
                      (make-array 0)
                      (make-array 1 :initial-element player)))
         (npcs (make-npcs player world :id-source npc-id-source))
         (entities (make-entities players npcs))
         (combat-events (make-combat-event-queue)))
    (if server-mode
        ;; Server: don't build minimap spawns until players connect
        (setf (world-minimap-spawns world) nil)
        ;; Client: build minimap spawns for the initial player
        (setf (world-minimap-spawns world)
              (build-adjacent-minimap-spawns world player)))
    (ensure-npcs-open-spawn npcs world)
    ;; Update initial zone-state with created NPCs for zone-filtered snapshots
    (let* ((zone (world-zone world))
           (zone-id (and zone (zone-id zone)))
           (zone-state (and zone-id (get-zone-state zone-id))))
      (when zone-state
        (setf (zone-state-npcs zone-state) npcs)
        ;; Populate NPC spatial grid for proximity queries
        (populate-npc-grid zone-state npcs)))
    (if player
        (log-verbose "Sim state initialized: player-id=~d npcs=~d zone=~a"
                     (player-id player)
                     (length npcs)
                     (zone-label (world-zone world)))
        (log-verbose "Sim state initialized (server): npcs=~d zone=~a"
                     (length npcs)
                     (zone-label (world-zone world))))
    (when (or *verbose-logs* *verbose-coordinates*)
      (format t "~&[VERBOSE COORDS] tile-size=~,2f collider-half=~,2f,~,2f wall=[~,2f..~,2f, ~,2f..~,2f]~%"
              (world-tile-dest-size world)
              (world-collision-half-width world)
              (world-collision-half-height world)
              (world-wall-min-x world)
              (world-wall-max-x world)
              (world-wall-min-y world)
              (world-wall-max-y world))
      (finish-output))
    (values world player players npcs entities id-source npc-id-source combat-events)))

(defun make-server-game ()
  ;; Build a headless game state for server-only simulation.
  (multiple-value-bind (world player players npcs entities id-source npc-id-source combat-events)
      (make-sim-state :server-mode t)
    (%make-game :world world
                :player player
                :players players
                :npcs npcs
                :entities entities
                :id-source id-source
                :npc-id-source npc-id-source
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
                :net-player-id nil
                ;; Zone cache and preload queue for server-side transition smoothness
                :zone-cache (make-zone-lru-cache)
                :preload-queue nil
                :edge-strips nil)))

(defun apply-client-intent (server-intent client-intent)
  ;; Copy the client intent payload into the server intent for this frame.
  (when (and server-intent client-intent)
    (when (intent-requested-drop-item-id client-intent)
      (log-verbose "APPLY-CLIENT-INTENT: drop item=~a count=~a"
                   (intent-requested-drop-item-id client-intent)
                   (intent-requested-drop-count client-intent)))
    (setf (intent-move-dx server-intent) (intent-move-dx client-intent)
          (intent-move-dy server-intent) (intent-move-dy client-intent)
          (intent-face-dx server-intent) (intent-face-dx client-intent)
          (intent-face-dy server-intent) (intent-face-dy client-intent)
          (intent-target-x server-intent) (intent-target-x client-intent)
          (intent-target-y server-intent) (intent-target-y client-intent)
          (intent-target-raw-x server-intent) (intent-target-raw-x client-intent)
          (intent-target-raw-y server-intent) (intent-target-raw-y client-intent)
          (intent-target-clamped-p server-intent) (intent-target-clamped-p client-intent)
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
          (intent-requested-drop-item-id server-intent)
          (intent-requested-drop-item-id client-intent)
          (intent-requested-drop-count server-intent)
          (intent-requested-drop-count client-intent)
          (intent-requested-drop-slot-index server-intent)
          (intent-requested-drop-slot-index client-intent)
          (intent-requested-swap-slot-a server-intent)
          (intent-requested-swap-slot-a client-intent)
          (intent-requested-swap-slot-b server-intent)
          (intent-requested-swap-slot-b client-intent)
          (intent-requested-chat-message server-intent)
          (intent-requested-chat-message client-intent)
          (intent-requested-unstuck server-intent)
          (intent-requested-unstuck client-intent)))
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
  (with-fatal-error-log ("Headless server runtime")
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
      game)))
