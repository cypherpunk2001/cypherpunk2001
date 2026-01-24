;; NOTE: If you change behavior here, update docs/main.md :)
(in-package #:mmorpg)

(defun make-game ()
  ;; Assemble game state and log setup if verbose is enabled.
  (multiple-value-bind (world player players npcs entities id-source npc-id-source combat-events)
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
                              :npc-id-source npc-id-source
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
                              :net-player-id (player-id player)
                              ;; Interpolation state (for smooth remote entity movement)
                              :interpolation-buffer (make-interpolation-buffer)
                              :interpolation-delay *interpolation-delay-seconds*
                              :client-time 0.0
                              :last-snapshot-time 0.0
                              ;; Prediction state (optional, controlled by *client-prediction-enabled*)
                              :prediction-state (when *client-prediction-enabled*
                                                  (make-prediction-state player)))))
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

(defun handle-window-resize (game)
  "Check for window resize and update game components accordingly.
   Only active when *window-resize-enabled* is T."
  (when (and *window-resize-enabled*
             (raylib:is-window-resized))
    (log-verbose "Window resized to ~dx~d"
                 (raylib:get-screen-width)
                 (raylib:get-screen-height))
    (update-ui-for-window-resize (game-ui game))
    (update-camera-for-window-resize (game-camera game))))

(defun npc-array-for-player-zone (game player)
  "Return the correct NPC array for PLAYER's current zone.
   Uses zone-state NPCs when available, falls back to game-npcs."
  (let* ((zone-id (or (player-zone-id player) *starting-zone-id*))
         (zone-state (and zone-id (get-zone-state zone-id))))
    (or (and zone-state (zone-state-npcs zone-state))
        (game-npcs game))))

(defun update-client-input (game dt)
  ;; Read raylib input and update UI/audio state; writes player intent.
  (let* ((player (game-player game))
         (world (game-world game))
         (npcs (npc-array-for-player-zone game player))
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
    ;; F11 toggles fullscreen globally (works in menus, gameplay, etc)
    (when (raylib:is-key-pressed +key-f11+)
      (raylib:toggle-fullscreen))
    (update-audio audio)
    (update-camera-zoom camera)
    (update-ui-loading ui dt)
    (update-ui-hud-log ui dt)
    (update-click-marker player dt npcs)
    (ensure-preview-zones world player camera editor)
    ;; Clear per-frame intent at start before processing new input
    (reset-frame-intent client-intent)
    ;; Track if menu was open at frame start (for click consumption)
    (let ((menu-was-open (ui-menu-open ui)))
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
            (:logout
             (log-verbose "Logout requested")
             (when (eq net-role :client)
               (queue-net-request game (list :type :logout))
               (setf (ui-menu-open ui) nil)
               ;; Return to login screen
               (setf (ui-auth-complete ui) nil)
               (setf (ui-login-active ui) t)
               (setf (ui-auth-error-message ui) nil)))
            (:unstuck
             (log-verbose "Unstuck requested")
             (request-unstuck client-intent)
             ;; Immediate client-side feedback (server validates and may override)
             (emit-hud-message-event event-queue "Unstuck requested...")
             (setf (ui-menu-open ui) nil)))))
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
             (input-blocked (or menu-blocked menu-was-open inventory-open)))
      (when menu-blocked
        (close-context-menu ui))
      (when (and inventory-open
                 (ui-context-open ui)
                 (not (eq (ui-context-target-type ui) :inventory)))
        (close-context-menu ui))
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
                     (log-verbose "CONTEXT-PICKUP: action=~a obj-id=~a world-x=~a world-y=~a"
                                  action context-object-id context-x context-y)
                     (let* ((tile-size (world-tile-dest-size world))
                            (tx (floor context-x tile-size))
                            (ty (floor context-y tile-size)))
                       (log-verbose "CONTEXT-PICKUP: tile=~d,~d tile-size=~a" tx ty tile-size)
                       (if context-object-id
                           (set-player-pickup-target player client-intent world
                                                     context-object-id
                                                     tx
                                                     ty
                                                     t)
                           (log-verbose "CONTEXT-PICKUP: SKIPPED - context-object-id is nil!"))))
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
                        (log-verbose "DROP-ACTION: item-id=~a slot-index=~a count=~a"
                                     item-id slot-index count)
                        (when (and item-id count (> count 0))
                          ;; Request drop via intent (server will process authoritatively)
                          (log-verbose "DROP-ACTION: requesting drop item=~a count=~a slot=~a" item-id count slot-index)
                          (request-drop-item client-intent item-id count slot-index)
                          (log-verbose "DROP-ACTION: AFTER request, intent drop-id=~a drop-count=~a"
                                       (intent-requested-drop-item-id client-intent)
                                       (intent-requested-drop-count client-intent)))))))))))
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
        ;; Inventory drag-and-drop: start drag on left-click
        (when (and (not click-consumed)
                   inventory-open
                   mouse-clicked
                   (not (ui-drag-active ui)))
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
                  (ui-start-inventory-drag ui slot-index slot-item mouse-x mouse-y)
                  (log-verbose "DRAG-START: slot=~a item=~a" slot-index slot-item)
                  (setf click-consumed t))))))
        ;; Inventory drag-and-drop: complete swap on mouse release
        (when (and inventory-open
                   (ui-drag-active ui)
                   (not mouse-down))
          (let* ((source-slot (ui-drag-slot-index ui))
                 (inventory (player-inventory player))
                 (slots (and inventory (inventory-slots inventory)))
                 (slot-count (if slots (length slots) 0))
                 (dest-slot (and (> slot-count 0)
                                 (inventory-slot-at-screen ui mouse-x mouse-y
                                                           slot-count))))
            (log-verbose "DRAG-END: source=~a dest=~a" source-slot dest-slot)
            (ui-end-inventory-drag ui)
            (when (and dest-slot source-slot
                       (/= dest-slot source-slot)
                       (< dest-slot slot-count))
              ;; Request swap via intent (server will process authoritatively)
              (request-inventory-swap client-intent source-slot dest-slot)
              (log-verbose "DRAG-SWAP: requesting swap ~a <-> ~a" source-slot dest-slot))))
        ;; Cancel drag if inventory closes or escape pressed
        (when (and (ui-drag-active ui)
                   (or (not inventory-open)
                       (raylib:is-key-pressed +key-escape+)))
          (ui-cancel-inventory-drag ui)
          (log-verbose "DRAG-CANCEL: inventory closed or escape pressed"))
        (when (and (not click-consumed)
                   (not input-blocked)
                   mouse-right-clicked)
          (multiple-value-bind (object object-world-x object-world-y)
              (find-object-at-screen world player camera mouse-x mouse-y)
            (declare (ignore object-world-x object-world-y))
            ;; Task 5.5: Use zone-object struct accessors for O(1) field access
            (if object
                (multiple-value-bind (cx cy)
                    (tile-center-position (world-tile-dest-size world)
                                          (zone-object-x object)
                                          (zone-object-y object))
                  (open-context-menu ui mouse-x mouse-y cx cy
                                     :target-type :object
                                     :object-id (zone-object-id object)))
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
                         ;; Task 5.5: Use zone-object struct accessors
                         (set-player-pickup-target player client-intent world
                                                   (zone-object-id mouse-object)
                                                   (zone-object-x mouse-object)
                                                   (zone-object-y mouse-object)
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
            (update-ui-hovered-npc ui npcs world player camera))
        ;; Update editor when active (handles camera, painting, mode switching)
        (update-editor editor game dt)))))

(defun reset-npc-frame-intents (npcs)
  ;; Clear per-tick intent signals for NPCs.
  (loop :for npc :across npcs
        :do (reset-frame-intent (npc-intent npc))))

(defun simulate-zone-npcs (zone-npcs zone-players world dt event-queue &optional zone-state game)
  "Run NPC AI for NPCs in a specific zone with that zone's players.
   ZONE-NPCS: vector of NPCs in the zone
   ZONE-PLAYERS: vector of players in the same zone
   ZONE-STATE: optional zone-state for per-zone collision and spatial grid
   GAME: optional game struct for O(1) player lookup via spatial grid
   Returns number of NPCs updated."
  (let ((count 0))
    (when (and zone-npcs (> (length zone-npcs) 0)
               zone-players (> (length zone-players) 0))
      (loop :for npc :across zone-npcs
            ;; Use spatial grid for O(1) cell lookup when available
            :for target-player = (closest-player zone-players npc zone-state game)
            :do (when target-player
                  (update-npc-behavior npc target-player world)
                  (update-npc-intent npc target-player world dt)
                  (update-npc-movement npc world dt zone-state)
                  (update-npc-attack npc target-player world dt event-queue)
                  (incf count))))
    count))

(defun handle-zone-transition (game)
  ;; Sync client-facing state after a zone change.
  (let* ((ui (game-ui game))
         (editor (game-editor game))
         (world (game-world game))
         (zone (and world (world-zone world)))
         (zone-id (and zone (zone-id zone)))
         (buffer (game-interpolation-buffer game))
         (pred (game-prediction-state game))
         (player (game-player game)))
    (ui-trigger-loading ui)
    (editor-sync-zone editor world)
    ;; Sync player zone-id to world zone - compact snapshots don't set this,
    ;; and prediction uses player-zone-id for collision map lookup
    (when (and player zone-id)
      (setf (player-zone-id player) zone-id))
    ;; Clear interpolation buffer - stale positions are invalid after zone change
    (when buffer
      (setf (interpolation-buffer-count buffer) 0
            (interpolation-buffer-head buffer) 0))
    ;; Reset prediction state - stale predicted position causes "stuck" movement
    ;; after zone transition because old coordinates don't match new zone
    ;; NOTE: Don't reset last-acked-sequence (leave unchanged to avoid snap spike)
    (when (and pred player)
      (setf (prediction-state-predicted-x pred) (player-x player)
            (prediction-state-predicted-y pred) (player-y player)
            (prediction-state-input-count pred) 0
            (prediction-state-input-head pred) 0))))

(defun update-sim (game dt &optional (allow-player-control t))
  "Run one fixed-tick simulation step. Returns true on zone transition.
   Task 1.3/1.4: Type and optimize declarations for hot tick function."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type game game)
           (type single-float dt))
  (reset-gc-stats)  ; Start GC tracking for this tick (Task 6.2)
  (with-timing (:update-sim)
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
            :for player-npcs = (npc-array-for-player-zone game current-player)
            :do (sync-player-follow-target current-player current-intent player-npcs world)
                (sync-player-attack-target current-player current-intent player-npcs world)
                (sync-player-pickup-target current-player current-intent world)
                (when (intent-requested-drop-item-id current-intent)
                  (log-verbose "UPDATE-SIM: processing drop for player=~a (obj ~a) item=~a count=~a"
                               (player-id current-player)
                               current-player
                               (intent-requested-drop-item-id current-intent)
                               (intent-requested-drop-count current-intent))
                  (process-player-drop-request current-player current-intent world)
                  (clear-requested-drop-item current-intent))
                ;; Process inventory swap requests
                (when (intent-requested-swap-slot-a current-intent)
                  (process-player-inventory-swap current-player current-intent))
                (process-player-unstuck current-player current-intent world
                                        (and (world-zone world)
                                             (zone-id (world-zone world)))
                                        event-queue)
                (let* ((moving (or (not (zerop (intent-move-dx current-intent)))
                                   (not (zerop (intent-move-dy current-intent)))
                                   (intent-target-active current-intent)))
                       (speed-mult (update-running-state current-player dt moving
                                                         (intent-run-toggle current-intent))))
                  (update-player-position current-player current-intent world speed-mult dt)
                  (update-player-pickup-target current-player world))))
    ;; Task 4.1: Refresh occupied zones cache once per tick
    ;; This populates *occupied-zones-cache* without allocating a new list
    (refresh-occupied-zones-cache players)
    ;; Object and NPC respawns - run for all occupied zones
    ;; Task 4.1: zone-ids is *occupied-zones-cache* (vector) - use length check
    (let ((zone-ids *occupied-zones-cache*))
      (if (> (length zone-ids) 0)
          ;; Multi-zone: update respawns for all occupied zone-states
          (loop :for zone-id :across zone-ids :do
            (let ((zone-state (get-zone-state zone-id)))
              (when zone-state
                ;; Object respawns for this zone
                (let ((zone-objects (zone-state-objects zone-state)))
                  (when zone-objects
                    (update-zone-objects-respawns zone-objects dt)))
                ;; NPC respawns for this zone (pass zone-state for grid update)
                (update-npc-respawns (zone-state-npcs zone-state) dt zone-state))))
          ;; Fallback: use world's zone
          (progn
            (update-object-respawns world dt)
            (update-npc-respawns npcs dt))))
    (let ((transitioned (and allow-player-control
                             (update-zone-transition game))))
      (when transitioned
        (setf npcs (game-npcs game)
              entities (game-entities game))
        (loop :for current-player :across players
              :do (setf (player-attack-target-id current-player) 0
                        (player-follow-target-id current-player) 0))
        (reset-npc-frame-intents npcs))
      (when allow-player-control
        (loop :for current-player :across players
              :for player-npcs = (npc-array-for-player-zone game current-player)
              :do (update-player-attack-intent current-player player-npcs world)))
      (loop :for current-player :across players
            :for current-intent = (player-intent current-player)
            :do (process-chat-intent current-player current-intent world event-queue)
                (when (and allow-player-control
                           (intent-attack current-intent))
                  (start-player-attack current-player current-intent)))
      (when (and player (or *verbose-logs* *verbose-coordinates*))
        (log-player-position player world))
      ;; Update animations and hit effects - split by type to avoid CLOS dispatch (Task 1.5)
      ;; Players and NPCs processed separately for direct function calls
      (loop :for player :across players
            :do (update-player-animation player dt)
                (update-player-hit-effect player dt))
      (loop :for npc :across npcs
            :do (update-npc-animation npc dt)
                (update-npc-hit-effect npc dt))
      ;; Per-zone melee combat and NPC simulation
      ;; For each occupied zone, run combat and NPC AI with that zone's players
      ;; Task 4.1: Reuse *occupied-zones-cache* populated earlier in this tick
      (let ((zone-ids *occupied-zones-cache*))
        (if (> (length zone-ids) 0)
            ;; Multi-zone mode: simulate each zone separately with per-zone collision
            (loop :for zone-id :across zone-ids :do
              (let* ((zone-state (get-zone-state zone-id))
                     (zone-npcs (if zone-state
                                    (zone-state-npcs zone-state)
                                    npcs))  ; fallback to game-npcs
                     ;; Task 4.1: Avoid allocating fallback path - zone-state should
                     ;; always exist for occupied zones (created by register-player-session)
                     (zone-players (if zone-state
                                       (let ((cache (zone-state-zone-players zone-state)))
                                         ;; Rebuild cache once if stale/empty but players exist
                                         (when (and cache
                                                    (zerop (length cache))
                                                    (> (zone-state-player-count zone-id players) 0))
                                           (rebuild-zone-players-cache zone-state game))
                                         (zone-state-zone-players zone-state))
                                       ;; Non-allocating fallback: warn and skip this zone
                                       (progn
                                         (warn "update-sim: zone-state nil for occupied zone ~a" zone-id)
                                         #()))))
                ;; Ensure all players in this zone are in the spatial grid
                ;; (handles players who joined before zone-state existed)
                (when (and zone-state zone-players)
                  (loop :for player :across zone-players
                        :do (ensure-player-in-grid player zone-state)))
                ;; Melee combat for this zone (using spatial grid for O(P×k) vs O(P×N))
                (when (and zone-npcs zone-players)
                  (apply-melee-hits-spatial zone-players zone-state world event-queue))
                ;; NPC AI for this zone (pass zone-state for per-zone collision and spatial grid)
                (simulate-zone-npcs zone-npcs zone-players world dt event-queue zone-state game)))
            ;; Fallback: no zone-ids means use legacy behavior (local mode, nil zone-ids)
            (progn
              (loop :for current-player :across players
                    :do (loop :for npc :across npcs
                              :do (apply-melee-hit current-player npc world event-queue)))
              (loop :for npc :across npcs
                    :for target-player = (closest-player players npc)
                    :do (when target-player
                          (update-npc-behavior npc target-player world)
                          (update-npc-intent npc target-player world dt)
                          (update-npc-movement npc world dt)
                          (update-npc-attack npc target-player world dt event-queue))))))
      (loop :for current-player :across players
            :do (consume-intent-actions (player-intent current-player)))
      ;; Increment playtime for all connected players (server-side tracking)
      (loop :for current-player :across players
            :do (incf (player-playtime current-player) dt))
      (log-gc-delta)  ; Log allocation for this tick (Task 6.2)
      transitioned))))

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

(defun run-local (&key (max-seconds 0.0) (max-frames 0))
  "Run the game in local/standalone mode with full editor access.
   Unlike run-client, this does not connect to a server and allows zone editing."
  (with-fatal-error-log ("Local game runtime")
    (log-verbose "Local game starting")
    ;; Set window flags before init (must be called before with-window)
    (when *window-resize-enabled*
      (raylib:set-config-flags +flag-window-resizable+))
    (raylib:with-window ("Hello MMO (Local)" (*window-width* *window-height*))
      (raylib:set-target-fps *client-target-fps*)
      (raylib:set-exit-key 0)
      (raylib:init-audio-device)
      (let ((game (make-game)))
        (unwind-protect
             (let ((ui (game-ui game))
                   (accumulator 0.0))
               (loop :with elapsed = 0.0
                     :with frames = 0
                     :until (or (raylib:window-should-close)
                                (ui-exit-requested ui)
                                (and (> max-seconds 0.0)
                                     (>= elapsed max-seconds))
                                (and (> max-frames 0)
                                     (>= frames max-frames)))
                     :do (let ((dt (raylib:get-frame-time)))
                           (incf elapsed dt)
                           (incf frames)
                           ;; Window resize handling (when enabled)
                           (handle-window-resize game)
                           ;; Input
                           (update-client-input game dt)
                           ;; Sync client intent to player (server does this via apply-client-intents)
                           (apply-client-intent (player-intent (game-player game))
                                                (game-client-intent game))
                           ;; Fixed timestep simulation
                           (incf accumulator dt)
                           (loop :with steps = 0
                                 :while (and (>= accumulator *sim-tick-seconds*)
                                             (< steps *sim-max-steps-per-frame*))
                                 :do (update-sim game *sim-tick-seconds*)
                                     (decf accumulator *sim-tick-seconds*)
                                     (incf steps))
                           ;; Process events and draw
                           (process-combat-events game)
                           (draw-game game))))
          (shutdown-game game)
          (raylib:close-audio-device)))))
  (log-verbose "Local game finished"))
