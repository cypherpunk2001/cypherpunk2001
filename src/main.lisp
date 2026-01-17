;; NOTE: If you change behavior here, update docs/main.md :)
(in-package #:mmorpg)

(defun make-game ()
  ;; Assemble game state and log setup if verbose is enabled.
  (multiple-value-bind (world player players npcs entities id-source combat-events)
      (make-sim-state)
    (let* ((audio (make-audio))
           (ui (make-ui))
           (render (make-render))
           (assets (load-assets world))
           (camera (make-camera))
           (editor (make-editor world assets player))
           (client-intent (make-intent :target-x (player-x player)
                                       :target-y (player-y player))))
      (when *editor-start-enabled*
        (toggle-editor-mode editor player))
      (let ((game (%make-game :world world
                              :player player
                              :players players
                              :npcs npcs
                              :entities entities
                              :id-source id-source
                              :audio audio
                              :ui ui
                              :render render
                              :assets assets
                              :camera camera
                              :editor editor
                              :combat-events combat-events
                              :client-intent client-intent
                              :net-role :local
                              :net-requests nil
                              :net-player-id (player-id player))))
        (log-verbose "Client game initialized: player-id=~d npcs=~d zone=~a"
                     (player-id player)
                     (length npcs)
                     (zone-label (world-zone world)))
        game))))

(defun shutdown-game (game)
  ;; Release game resources before exiting.
  (log-verbose "Shutting down game resources")
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
         (event-queue (game-combat-events game))
         (client-intent (game-client-intent game))
         (mouse-clicked (raylib:is-mouse-button-pressed +mouse-left+))
         (mouse-down (raylib:is-mouse-button-down +mouse-left+))
         (mouse-right-clicked (raylib:is-mouse-button-pressed +mouse-right+))
         (mouse-x (raylib:get-mouse-x))
         (mouse-y (raylib:get-mouse-y)))
    (update-audio audio)
    (update-camera-zoom camera)
    (update-ui-loading ui dt)
    (update-ui-hud-log ui dt)
    (update-click-marker player dt)
    (ensure-preview-zones world player camera editor)
    (let ((menu-action (update-ui-input ui audio mouse-clicked))
          (net-role (game-net-role game)))
      (when menu-action
        (case menu-action
          (:toggle-editor
           (log-verbose "Toggle editor requested")
           (if (eq net-role :client)
               (emit-hud-message-event event-queue
                                       "Editor disabled in client mode.")
               (progn
                 (toggle-editor-mode editor player)
                 (setf (ui-menu-open ui) nil))))
          (:save-game
           (log-verbose "Save requested (net-role=~a)" net-role)
           (if (eq net-role :client)
               (queue-net-request game (list :type :save))
               (when (save-game game *save-filepath*)
                 (emit-hud-message-event event-queue "Game saved."))))
          (:load-game
           (log-verbose "Load requested (net-role=~a)" net-role)
           (if (eq net-role :client)
               (queue-net-request game (list :type :load))
               (let ((zone-id (load-game game *save-filepath*)))
                 (if zone-id
                     (progn
                       (let ((server-intent (and player (player-intent player))))
                         (when server-intent
                           (reset-frame-intent server-intent)
                           (clear-intent-target server-intent)))
                       (when client-intent
                         (reset-frame-intent client-intent)
                         (clear-intent-target client-intent))
                       (clear-player-auto-walk player)
                       (setf (player-attacking player) nil
                             (player-attack-hit player) nil
                             (player-hit-active player) nil)
                       (mark-player-hud-stats-dirty player)
                       (mark-player-inventory-dirty player)
                       (setf (world-minimap-spawns world)
                             (build-adjacent-minimap-spawns world player))
                       (setf (world-minimap-collisions world)
                             (build-minimap-collisions world))
                       (emit-hud-message-event event-queue
                                               (format nil "Game loaded (~a)." zone-id)))
                     (emit-hud-message-event event-queue "Load failed."))))))))
    (let ((chat-opened nil))
      (when (and (not (ui-chat-active ui))
                 (not (ui-menu-open ui))
                 (not (editor-active editor))
                 (not (ui-inventory-open ui))
                 (raylib:is-key-pressed +key-t+))
        (open-chat-input ui)
        (setf chat-opened t)
        (close-context-menu ui))
      (let ((chat-message (update-chat-input ui :skip-input chat-opened)))
        (when chat-message
          (request-chat-message client-intent chat-message))))
    (when (and (not (ui-menu-open ui))
               (not (editor-active editor))
               (not (ui-chat-active ui))
               (raylib:is-key-pressed +key-i+))
      (setf (ui-inventory-open ui) (not (ui-inventory-open ui)))
      (close-context-menu ui))
    (when (or (ui-menu-open ui)
              (editor-active editor)
              (ui-chat-active ui))
      (setf (ui-inventory-open ui) nil))
    (let* ((menu-blocked (or (ui-menu-open ui)
                             (editor-active editor)
                             (ui-chat-active ui)))
           (inventory-open (ui-inventory-open ui))
           (input-blocked (or menu-blocked inventory-open)))
      (when menu-blocked
        (close-context-menu ui))
      (when (and inventory-open
                 (ui-context-open ui)
                 (not (eq (ui-context-target-type ui) :inventory)))
        (close-context-menu ui))
      (reset-frame-intent client-intent)
      (let ((click-consumed nil))
        (when (and (ui-context-open ui) mouse-clicked)
          (let ((action (handle-context-menu-click ui mouse-x mouse-y)))
            (when action
              (let ((context-x (ui-context-world-x ui))
                    (context-y (ui-context-world-y ui))
                    (context-id (ui-context-target-id ui))
                    (context-type (ui-context-target-type ui))
                    (context-object-id (ui-context-object-id ui))
                    (slot-index (ui-context-slot-index ui))
                    (item-id (ui-context-item-id ui)))
                (close-context-menu ui)
                (unless (eq action :close)
                  (setf click-consumed t)
                  (cond
                    ((eq action :walk)
                     (set-player-walk-target player client-intent
                                             context-x
                                             context-y
                                             t))
                    ((eq action :attack)
                     (let ((npc (find-npc-by-id npcs context-id)))
                       (when npc
                         (set-player-attack-target player client-intent npc t))))
                    ((eq action :follow)
                     (let ((npc (find-npc-by-id npcs context-id)))
                       (when npc
                         (set-player-follow-target player client-intent npc t))))
                    ((eq action :pickup)
                     (let* ((tile-size (world-tile-dest-size world))
                            (tx (floor context-x tile-size))
                            (ty (floor context-y tile-size)))
                       (when context-object-id
                         (set-player-pickup-target player client-intent world
                                                   context-object-id
                                                   tx
                                                   ty
                                                   t))))
                    ((eq action :examine)
                     (cond
                       ((eq context-type :npc)
                        (let ((npc (find-npc-by-id npcs context-id)))
                          (when npc
                            (emit-hud-message-event event-queue (npc-examine-description npc)))))
                       ((eq context-type :object)
                        (let ((object (and context-object-id
                                           (list :id context-object-id))))
                          (emit-hud-message-event event-queue (object-examine-description object))))
                       ((eq context-type :inventory)
                        (when item-id
                          (emit-hud-message-event event-queue (item-examine-description item-id))))))
                    ((eq action :drop)
                     (let* ((inventory (player-inventory player))
                            (slots (and inventory (inventory-slots inventory)))
                            (slot (and slot-index slots
                                       (< slot-index (length slots))
                                       (aref slots slot-index)))
                            (count (and slot (inventory-slot-count slot))))
                        (when (and item-id count (> count 0))
                         (let ((dropped (drop-inventory-item player world item-id count)))
                           (when (and dropped (> dropped 0))
                             (emit-hud-message-event event-queue
                                              (format nil "Dropped ~a x~d."
                                                      (item-display-name item-id)
                                                      dropped)))))))))))))
        (when (and (not click-consumed)
                   inventory-open
                   mouse-right-clicked)
          (let* ((inventory (player-inventory player))
                 (slots (and inventory (inventory-slots inventory)))
                 (slot-count (if slots (length slots) 0))
                 (slot-index (and (> slot-count 0)
                                  (inventory-slot-at-screen ui mouse-x mouse-y
                                                            slot-count))))
            (when (and slot-index slots (< slot-index (length slots)))
              (let* ((slot (aref slots slot-index))
                     (slot-item (inventory-slot-item-id slot))
                     (item-count (inventory-slot-count slot)))
                (when (and slot-item (> item-count 0))
                  (open-context-menu ui mouse-x mouse-y
                                     (player-x player)
                                     (player-y player)
                                     :target-type :inventory
                                     :slot-index slot-index
                                     :item-id slot-item)
                  (setf click-consumed t))))))
        (when (and (not click-consumed)
                   (not input-blocked)
                   mouse-right-clicked)
          (multiple-value-bind (object object-world-x object-world-y)
              (find-object-at-screen world player camera mouse-x mouse-y)
            (declare (ignore object-world-x object-world-y))
            (if object
                (multiple-value-bind (cx cy)
                    (tile-center-position (world-tile-dest-size world)
                                          (getf object :x)
                                          (getf object :y))
                  (open-context-menu ui mouse-x mouse-y cx cy
                                     :target-type :object
                                     :object-id (getf object :id)))
                (multiple-value-bind (npc world-x world-y)
                    (find-npc-at-screen npcs world player camera mouse-x mouse-y)
                  (open-context-menu ui mouse-x mouse-y world-x world-y
                                     :target-id (and npc (npc-id npc))
                                     :target-type (if npc :npc :world)))))
          (setf click-consumed t))
        (unless input-blocked
          (let ((minimap-handled (and (not click-consumed)
                                      (update-target-from-minimap player client-intent ui world
                                                                  dt mouse-clicked mouse-down))))
            (unless (or click-consumed minimap-handled)
              (let ((mouse-npc nil)
                    (mouse-object nil))
                (when (or mouse-clicked mouse-down)
                  (multiple-value-bind (world-x world-y)
                      (screen-to-world mouse-x mouse-y
                                       (player-x player)
                                       (player-y player)
                                       (camera-offset camera)
                                       (camera-zoom camera))
                    (setf mouse-npc (find-npc-at-world npcs world world-x world-y)
                          mouse-object (find-object-at-world world world-x world-y))
                    (when mouse-clicked
                      (cond
                        (mouse-npc
                         (set-player-attack-target player client-intent mouse-npc t))
                        (mouse-object
                         (set-player-pickup-target player client-intent world
                                                   (getf mouse-object :id)
                                                   (getf mouse-object :x)
                                                   (getf mouse-object :y)
                                                   t))
                        (t
                         (update-target-from-mouse player client-intent camera dt
                                                   mouse-clicked mouse-down))))))
                (when (and mouse-down (not mouse-clicked)
                           (not mouse-npc) (not mouse-object))
                  (update-target-from-mouse player client-intent camera dt
                                            mouse-clicked mouse-down)))))))
      (unless (or (editor-active editor)
                  (ui-inventory-open ui)
                  (ui-chat-active ui))
        (update-input-direction player client-intent mouse-clicked))
      (unless (or (ui-menu-open ui)
                  (editor-active editor)
                  (ui-inventory-open ui)
                  (ui-chat-active ui))
        (update-input-actions client-intent (not mouse-clicked))
        (update-training-mode player))
      (if input-blocked
          (setf (ui-hover-npc-name ui) nil)
          (update-ui-hovered-npc ui npcs world player camera)))))

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
         (players (game-players game))
         (npcs (game-npcs game))
         (entities (game-entities game))
         (world (game-world game))
         (event-queue (game-combat-events game)))
    (reset-npc-frame-intents npcs)
    (when allow-player-control
      (loop :for current-player :across players
            :for current-intent = (player-intent current-player)
            :do (sync-player-follow-target current-player current-intent npcs)
                (sync-player-attack-target current-player current-intent npcs)
                (sync-player-pickup-target current-player current-intent world)
                (let* ((moving (or (not (zerop (intent-move-dx current-intent)))
                                   (not (zerop (intent-move-dy current-intent)))
                                   (intent-target-active current-intent)))
                       (speed-mult (update-running-state current-player dt moving
                                                         (intent-run-toggle current-intent))))
                  (update-player-position current-player current-intent world speed-mult dt)
                  (update-player-pickup-target current-player world))))
    (update-object-respawns world dt)
    (let ((transitioned (and allow-player-control
                             (update-zone-transition game))))
      (when transitioned
        (setf npcs (game-npcs game)
              entities (game-entities game))
        (loop :for current-player :across players
              :do (setf (player-attack-target-id current-player) 0
                        (player-follow-target-id current-player) 0))
        (reset-npc-frame-intents npcs))
      (update-npc-respawns npcs dt)
      (when allow-player-control
        (loop :for current-player :across players
              :do (update-player-attack-intent current-player npcs world)))
      (loop :for current-player :across players
            :for current-intent = (player-intent current-player)
            :do (process-chat-intent current-player current-intent world event-queue)
                (when (and allow-player-control
                           (intent-attack current-intent))
                  (start-player-attack current-player current-intent)))
      (when (or *verbose-logs* *verbose-coordinates*)
        (log-player-position player world))
      (loop :for entity :across entities
            :do (update-entity-animation entity dt))
      (loop :for current-player :across players
            :do (loop :for npc :across npcs
                      :do (apply-melee-hit current-player npc world event-queue)))
      (loop :for npc :across npcs
            :for target-player = (closest-player players npc)
            :do (when target-player
                  (update-npc-behavior npc target-player world)
                  (update-npc-intent npc target-player world dt)
                  (update-npc-movement npc world dt)
                  (update-npc-attack npc target-player world dt event-queue)))
      (loop :for entity :across entities
            :do (combatant-update-hit-effect entity dt))
      (loop :for current-player :across players
            :do (consume-intent-actions (player-intent current-player)))
      transitioned)))

(defun process-combat-events (game)
  ;; Process combat events from simulation and write to UI (client-side rendering).
  (let* ((event-queue (game-combat-events game))
         (ui (game-ui game))
         (events (pop-combat-events event-queue)))
    (dolist (event events)
      (let ((type (combat-event-type event))
            (text (combat-event-text event)))
        (cond
          ((eq type :combat-log)
           (ui-push-combat-log ui text))
          ((eq type :hud-message)
           (ui-push-hud-log ui text)))))))
