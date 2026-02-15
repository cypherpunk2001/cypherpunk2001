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
                                                  (make-prediction-state player))
                              ;; Seamless zone loading (Step 4/5)
                              :zone-cache (make-zone-lru-cache)
                              :preload-queue nil
                              :edge-strips nil)))
        (log-verbose "Client game initialized: player-id=~d npcs=~d zone=~a"
                     (player-id player)
                     (length npcs)
                     (zone-label (world-zone world)))
        ;; Step 5: Warm cache with adjacent zones on initial load
        (cold-start-preload-adjacent game)
        game))))

(defun shutdown-game (game)
  ;; Release game resources before exiting.
  (log-verbose "Shutting down game resources")
  (shutdown-audio (game-audio game))
  (unload-editor-tilesets (game-editor game) (game-assets game))
  (unload-virtual-target (game-render game))
  (unload-assets (game-assets game)))

(defun handle-window-resize (game)
  "Check for window resize/fullscreen toggle and update game components.
   Handles F11 fullscreen toggle and refreshes present metrics for virtual pipeline."
  ;; F11 toggles fullscreen (centralized here for all modes)
  (when (raylib:is-key-pressed +key-f11+)
    (raylib:toggle-fullscreen))
  ;; Refresh present metrics whenever display dimensions might have changed.
  ;; Compare scale AND offsets — display can change offsets without changing scale.
  (let ((render (game-render game)))
    (when render
      (let ((dw (display-screen-width))
            (dh (display-screen-height)))
        (multiple-value-bind (new-scale new-ox new-oy)
            (compute-present-scale dw dh *virtual-width* *virtual-height*)
          (when (or (/= new-scale (render-present-scale render))
                    (/= new-ox (render-present-offset-x render))
                    (/= new-oy (render-present-offset-y render)))
            (log-verbose "Display resized to ~dx~d (scale ~d)" dw dh new-scale)
            (refresh-present-metrics render)
            (update-camera-for-window-resize (game-camera game))))))))

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
         (mouse-x (virtual-mouse-x))
         (mouse-y (virtual-mouse-y)))
    ;; F11 fullscreen toggle handled by handle-window-resize
    (update-audio audio)
    (update-camera-zoom camera)
    (update-camera-leash camera player world editor)
    (update-ui-loading ui dt)
    (update-ui-hud-log ui dt)
    (update-click-marker player dt npcs)
    (ensure-preview-zones world player camera editor (game-zone-cache game))
    ;; Step 5: Client-side proximity preloading
    (update-client-preloading game)
    ;; ADDENDUM 3: Urgent preload — when player is close to the commit line,
    ;; flush the entire preload queue this frame to guarantee the target zone
    ;; is cached before transition fires. Uses proximity only (no pending flag)
    ;; because pending is server-side state not serialized to network clients.
    (let ((urgent (and player
                      (game-preload-queue game)
                      (let* ((tile-size (world-tile-dest-size world))
                             (urgent-px (* *zone-urgent-preload-tiles* tile-size)))
                        (player-within-urgent-preload-distance-p
                         player world urgent-px)))))
      (if urgent
          (let ((before-depth (length (game-preload-queue game))))
            (process-preload-queue game :count before-depth)
            (when *verbose-zone-transitions*
              (let ((after-depth (length (game-preload-queue game))))
                (log-zone "Urgent preload flush: queue-before=~d queue-after=~d"
                          before-depth after-depth))))
          (process-preload-queue game)))
    ;; Clear per-frame intent at start before processing new input
    (reset-frame-intent client-intent)
    ;; Track if menu was open at frame start (for click consumption)
    (let ((menu-was-open (ui-menu-open ui)))
      (let ((menu-action (update-ui-input ui audio mouse-clicked))
            (net-role (game-net-role game)))
        (when menu-action
          (case menu-action
            (:toggle-fullscreen
             (raylib:toggle-fullscreen)
             (refresh-present-metrics (game-render game))
             (update-camera-for-window-resize (game-camera game))
             (setf (ui-menu-open ui) nil))
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
                                             t world)
                     (maybe-set-diagonal-path game))
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
        (let ((minimap-handled nil))
          (unless input-blocked
            (setf minimap-handled (and (not click-consumed)
                                       (update-target-from-minimap player client-intent ui world
                                                                   dt mouse-clicked mouse-down)))
            (unless (or click-consumed minimap-handled)
              (let ((mouse-npc nil)
                    (mouse-object nil))
                (when (or mouse-clicked mouse-down)
                  ;; Use camera focus (leash or editor) for screen-to-world conversion
                  (multiple-value-bind (cam-x cam-y)
                      (editor-camera-target editor player camera)
                    (multiple-value-bind (world-x world-y)
                        (screen-to-world mouse-x mouse-y
                                         cam-x cam-y
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
                                                     mouse-clicked mouse-down world editor)))))))
                (when (and mouse-down (not mouse-clicked)
                           (not mouse-npc) (not mouse-object))
                  (update-target-from-mouse player client-intent camera dt
                                            mouse-clicked mouse-down world editor))))))
        ;; Bug 4: After any click sets a walk target, check for cross-zone path
        ;; Both floor and minimap clicks use bounds-based destination resolution
        (when mouse-clicked
          (maybe-set-diagonal-path game)))
      (unless (or (editor-active editor)
                  (ui-inventory-open ui)
                  (ui-chat-active ui))
        (update-input-direction player client-intent mouse-clicked)
        ;; Bug 4: Keyboard movement cancels diagonal zone-click path
        (when (or (not (zerop (intent-move-dx client-intent)))
                  (not (zerop (intent-move-dy client-intent))))
          (clear-zone-click-path game)))
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
   ZONE-PLAYERS: vector of players in the same zone (may be empty/nil)
   ZONE-STATE: optional zone-state for per-zone collision and spatial grid
   GAME: optional game struct for O(1) player lookup via spatial grid
   Returns number of NPCs updated.
   NPCs run idle/wander behavior even without a nearby player."
  (let ((count 0))
    (when (and zone-npcs (> (length zone-npcs) 0))
      (loop :for npc :across zone-npcs
            ;; Use spatial grid for O(1) cell lookup when available
            ;; Returns nil if no player nearby - NPC will idle/wander
            :for target-player = (closest-player zone-players npc zone-state game)
            :do (progn
                  (update-npc-behavior npc target-player world)
                  (update-npc-intent npc target-player world dt)
                  (update-npc-movement npc world dt zone-state)
                  ;; Only attack when player is nearby
                  (when target-player
                    (update-npc-attack npc target-player world dt event-queue))
                  (incf count))))
    count))

(defun sync-client-zone-npcs (game)
  "Sync zone-state NPCs with game-npcs after teleport/resync/zone-change.
   Phase 2: Ensures rendering uses the same NPC array that snapshots update.
   Without this, zone-state-npcs can become stale and cause frozen sprites.
   Called from handle-zone-transition (zone changes) and apply-snapshot (resyncs/teleports)."
  (let* ((player (game-player game))
         (npcs (game-npcs game))
         ;; Resolve zone-id from player or world
         (zone-id (or (and player (player-zone-id player))
                      (let ((world (game-world game)))
                        (and world (world-zone world)
                             (zone-id (world-zone world))))))
         (zone-state (and zone-id (get-zone-state zone-id))))
    (when (and zone-state npcs)
      ;; Update zone-state to use the current game-npcs array
      (setf (zone-state-npcs zone-state) npcs)
      ;; Rebuild NPC spatial grid and index map for rendering/culling
      (populate-npc-grid zone-state npcs)
      (log-verbose "Client NPC zone-state synced (zone-id=~a npcs=~d)"
                   zone-id (length npcs)))))

;;;; ========================================================================
;;;; Client-Side Proximity Preloading (Step 5)
;;;; Monitors player distance to zone edges and queues adjacent zones for
;;;; preloading into the LRU cache. One zone loaded per frame to avoid hitches.
;;;; ========================================================================

(defun player-near-edge-p (player world edge)
  "Return T if PLAYER is within *zone-preload-radius* tiles of EDGE.
   Client-only function — uses local player position."
  (let* ((zone (world-zone world))
         (tile-size (world-tile-dest-size world))
         (radius-px (* *zone-preload-radius* tile-size))
         (half-w (world-collision-half-width world))
         (half-h (world-collision-half-height world)))
    (when zone
      (multiple-value-bind (min-x max-x min-y max-y)
          (let ((zone-id (zone-id zone)))
            (if (and zone-id (get-zone-wall-map zone-id))
                (get-zone-collision-bounds zone-id tile-size half-w half-h)
                (values (world-wall-min-x world) (world-wall-max-x world)
                        (world-wall-min-y world) (world-wall-max-y world))))
        (when (and min-x max-x min-y max-y)
          (let ((dist (player-distance-to-edge player edge min-x max-x min-y max-y)))
            (<= dist radius-px)))))))

(defun player-within-urgent-preload-distance-p (player world urgent-px)
  "Return T if PLAYER is within URGENT-PX pixels of any zone edge.
   ADDENDUM 3: Used to detect when the preload queue should be flushed entirely."
  (declare (type single-float urgent-px))
  (let* ((zone (world-zone world))
         (tile-size (world-tile-dest-size world))
         (half-w (world-collision-half-width world))
         (half-h (world-collision-half-height world)))
    (when zone
      (multiple-value-bind (min-x max-x min-y max-y)
          (let ((zone-id (zone-id zone)))
            (if (and zone-id (get-zone-wall-map zone-id))
                (get-zone-collision-bounds zone-id tile-size half-w half-h)
                (values (world-wall-min-x world) (world-wall-max-x world)
                        (world-wall-min-y world) (world-wall-max-y world))))
        (when (and min-x max-x min-y max-y)
          (let ((px (player-x player))
                (py (player-y player)))
            (or (<= (- px min-x) urgent-px)
                (<= (- max-x px) urgent-px)
                (<= (- py min-y) urgent-px)
                (<= (- max-y py) urgent-px))))))))

(defun player-near-edge-for-zone-p (player world zone-id edge)
  "Return T if PLAYER is within *zone-preload-radius* tiles of EDGE in ZONE-ID.
   Server/local helper that uses per-zone bounds."
  (let* ((tile-size (world-tile-dest-size world))
         (radius-px (* *zone-preload-radius* tile-size))
         (half-w (world-collision-half-width world))
         (half-h (world-collision-half-height world)))
    (when zone-id
      (multiple-value-bind (min-x max-x min-y max-y)
          (if (get-zone-wall-map zone-id)
              (get-zone-collision-bounds zone-id tile-size half-w half-h)
              (values (world-wall-min-x world) (world-wall-max-x world)
                      (world-wall-min-y world) (world-wall-max-y world)))
        (when (and min-x max-x min-y max-y)
          (let ((dist (player-distance-to-edge player edge min-x max-x min-y max-y)))
            (<= dist radius-px)))))))

(defun player-within-urgent-preload-distance-for-zone-p (player world zone-id urgent-px)
  "Return T if PLAYER is within URGENT-PX of any zone edge for ZONE-ID."
  (declare (type single-float urgent-px))
  (let* ((tile-size (world-tile-dest-size world))
         (half-w (world-collision-half-width world))
         (half-h (world-collision-half-height world)))
    (when zone-id
      (multiple-value-bind (min-x max-x min-y max-y)
          (if (get-zone-wall-map zone-id)
              (get-zone-collision-bounds zone-id tile-size half-w half-h)
              (values (world-wall-min-x world) (world-wall-max-x world)
                      (world-wall-min-y world) (world-wall-max-y world)))
        (when (and min-x max-x min-y max-y)
          (let ((px (player-x player))
                (py (player-y player)))
            (or (<= (- px min-x) urgent-px)
                (<= (- max-x px) urgent-px)
                (<= (- py min-y) urgent-px)
                (<= (- max-y py) urgent-px))))))))

(defun update-server-preloading (game)
  "Queue adjacent zones for preloading on the server.
   Uses all active players and their current zone-id."
  (let* ((world (game-world game))
         (zone-lru (game-zone-cache game))
         (graph (and world (world-world-graph world)))
         (players (game-players game)))
    (when (and world zone-lru graph players (> (length players) 0))
      (loop :for player :across players
            :when player
            :do (let ((zone-id (player-zone-id player)))
                  (when zone-id
                    (dolist (edge '(:north :south :east :west))
                      (when (player-near-edge-for-zone-p player world zone-id edge)
                        (let ((exit (find edge (world-graph-exits graph zone-id)
                                          :key (lambda (e) (getf e :edge)) :test #'eq)))
                          (when (and exit (spatial-exit-p exit))
                            (let ((target-id (getf exit :to)))
                              (queue-zone-preload game zone-lru graph target-id
                                                  (format nil "server near ~a edge" edge))
                              ;; Diagonal neighbors via perpendicular exits
                              (when target-id
                                (let ((perp-edges (case edge
                                                    ((:north :south) '(:east :west))
                                                    ((:east :west) '(:north :south)))))
                                  (dolist (perp perp-edges)
                                    (let ((diag-exit (find perp (world-graph-exits graph target-id)
                                                           :key (lambda (e) (getf e :edge)) :test #'eq)))
                                      (when (and diag-exit (spatial-exit-p diag-exit))
                                        (queue-zone-preload game zone-lru graph (getf diag-exit :to)
                                                            (format nil "server diagonal ~a->~a" edge perp))))))))))))))))))

(defun queue-zone-preload (game zone-lru graph target-id reason)
  "Queue TARGET-ID for preloading if not already cached or queued."
  (when (and target-id
             (not (zone-cache-contains-p zone-lru target-id))
             (not (assoc target-id (game-preload-queue game))))
    (let ((path (world-graph-zone-path graph target-id)))
      (when path
        (push (cons target-id path) (game-preload-queue game))
        (log-zone "Zone preload: queued ~a (~a)" target-id reason)))))

(defun cold-start-preload-adjacent (game)
  "Queue all spatially adjacent zones (cardinal + diagonal) for preloading.
   Called once on initial zone load to warm the cache. Step 5."
  (let* ((world (game-world game))
         (zone-lru (game-zone-cache game))
         (graph (and world (world-world-graph world)))
         (zone (and world (world-zone world)))
         (zone-id (and zone (zone-id zone))))
    (when (and zone-lru graph zone-id)
      ;; Queue cardinal neighbors
      (dolist (exit-spec (world-graph-exits graph zone-id))
        (when (spatial-exit-p exit-spec)
          (let ((target-id (getf exit-spec :to))
                (edge (getf exit-spec :edge)))
            (queue-zone-preload game zone-lru graph target-id
                                (format nil "cold-start ~a" edge))
            ;; Queue diagonal neighbors (neighbor's perpendicular exits)
            (when target-id
              (let ((perp-edges (case edge
                                  ((:north :south) '(:east :west))
                                  ((:east :west) '(:north :south)))))
                (dolist (perp perp-edges)
                  (let ((diag-exit (find perp (world-graph-exits graph target-id)
                                         :key (lambda (e) (getf e :edge)) :test #'eq)))
                    (when (and diag-exit (spatial-exit-p diag-exit))
                      (queue-zone-preload game zone-lru graph (getf diag-exit :to)
                                          (format nil "cold-start diagonal ~a->~a" edge perp)))))))))))))

(defun update-client-preloading (game)
  "Check proximity to zone edges and queue adjacent zones for preloading.
   Step 5: Client-only, independent of server arm/commit state machine.
   Includes diagonal neighbors and filters non-spatial exits (teleports)."
  (let* ((player (game-player game))
         (world (game-world game))
         (zone-lru (game-zone-cache game))
         (graph (and world (world-world-graph world)))
         (zone (and world (world-zone world)))
         (zone-id (and zone (zone-id zone))))
    (when (and player world zone-lru graph zone-id)
      ;; Check each cardinal edge
      (dolist (edge '(:north :south :east :west))
        (when (player-near-edge-p player world edge)
          (let ((exit (find edge (world-graph-exits graph zone-id)
                            :key (lambda (e) (getf e :edge)) :test #'eq)))
            (when (and exit (spatial-exit-p exit))
              (let ((target-id (getf exit :to)))
                (queue-zone-preload game zone-lru graph target-id
                                    (format nil "player near ~a edge" edge))
                ;; Diagonal neighbors: preload this neighbor's perpendicular exits
                (when target-id
                  (let ((perp-edges (case edge
                                      ((:north :south) '(:east :west))
                                      ((:east :west) '(:north :south)))))
                    (dolist (perp perp-edges)
                      (let ((diag-exit (find perp (world-graph-exits graph target-id)
                                             :key (lambda (e) (getf e :edge)) :test #'eq)))
                        (when (and diag-exit (spatial-exit-p diag-exit))
                          (queue-zone-preload game zone-lru graph (getf diag-exit :to)
                                              (format nil "diagonal via ~a->~a" edge perp)))))))))))))))

(defun process-preload-queue (game &key (count 1))
  "Pop up to COUNT entries from the preload queue and load them into the LRU cache.
   Default: 1 zone per frame to avoid hitches. ADDENDUM 3: urgent mode passes
   a higher count when the player is near the commit line."
  (declare (type fixnum count))
  (let ((zone-lru (game-zone-cache game)))
    (when zone-lru
      (loop :repeat count
            :while (game-preload-queue game)
            :do (let* ((entry (pop (game-preload-queue game)))
                       (zone-id (car entry))
                       (path (cdr entry)))
                  ;; Skip if already cached (may have been loaded by ensure-preview-zones)
                  (unless (zone-cache-contains-p zone-lru zone-id)
                    (when (and path (probe-file path))
                      (let ((zone (load-zone path)))
                        (when zone
                          (zone-cache-insert zone-lru zone-id zone)
                          (log-zone "Zone preload: loaded ~a into cache" zone-id))))))))))

(defun zone-transition-show-loading-p ()
  "Return T if zone transitions should show a loading overlay.
   Always returns nil for locomotion transitions — preloading (Step 5) ensures zones
   are cached before the player reaches the commit line."
  nil)

(defun maybe-set-diagonal-path (game)
  "After a click sets a walk target, detect cross-zone clicks and populate zone-click-path.
   Uses bounds-based destination resolution for all click types (floor and minimap).
   Floor clicks have camera-limited raw coordinates (typically 1-2 hop), minimap clicks
   can reach further, but both use the same resolve/translate code path."
  (let* ((ci (game-client-intent game))
         (world (game-world game))
         (player (game-player game)))
    ;; Always clear old path first — new click overrides any in-progress path
    (clear-zone-click-path game)
    (when (and ci world player
               (intent-target-clamped-p ci)
               (intent-target-active ci))
      (let ((raw-x (intent-target-raw-x ci))
            (raw-y (intent-target-raw-y ci)))
        (when (and raw-x raw-y)
          (multiple-value-bind (zone-path edge-list hop-targets final-x final-y)
              (compute-minimap-click-path world player raw-x raw-y)
            (when (and zone-path edge-list hop-targets final-x final-y)
              (setf (game-zone-click-path game) zone-path
                    (game-zone-click-edges game) edge-list
                    (game-zone-click-hop-targets game) hop-targets
                    (game-zone-click-final-x game) final-x
                    (game-zone-click-final-y game) final-y)
              (log-verbose "Click path: ~a -> ~{~a~^ -> ~} final=(~,1f,~,1f)"
                           (player-zone-id player) zone-path
                           final-x final-y))))))))

(defun handle-zone-transition (game &key (old-x 0.0f0) (old-y 0.0f0) old-zone-id old-world-bounds)
  "Sync client-facing state after a zone change.
   Step 6: No loading overlay for locomotion transitions.
   Step 12: Per-transition diagnostics (cache hit/miss, preload queue depth).
   ADDENDUM 4: Soft reset — only clear interpolation/prediction buffers
   when position delta exceeds *soft-reset-threshold-sq*. Small deltas
   (typical of seamless walk-through transitions) preserve buffer continuity.
   Phase 1 Bug 1 fix: Rebase cross-zone click targets on BOTH player-intent
   and game-client-intent so the player walks to the intended destination
   in the new zone. Non-crossing targets are cleared."
  (let* ((t0 (when *verbose-zone-transitions* (get-internal-real-time)))
         (editor (game-editor game))
         (world (game-world game))
         (zone (and world (world-zone world)))
         (zone-id (and zone (zone-id zone)))
         (buffer (game-interpolation-buffer game))
         (pred (game-prediction-state game))
         (player (game-player game))
         ;; Step 12: Capture pre-transition state for diagnostics
         (diag-from-zone (and *verbose-zone-transitions* player (player-zone-id player)))
         (diag-cache-hits (when *verbose-zone-transitions*
                            (let ((zl (game-zone-cache game)))
                              (if zl (zone-cache-hits zl) 0))))
         (diag-cache-misses (when *verbose-zone-transitions*
                              (let ((zl (game-zone-cache game)))
                                (if zl (zone-cache-misses zl) 0)))))
    ;; Step 6: Only show loading overlay if predicate says so (always nil for locomotion)
    (when (zone-transition-show-loading-p)
      (ui-trigger-loading (game-ui game)))
    (editor-sync-zone editor world)
    ;; Sync player zone-id to world zone - compact snapshots don't set this,
    ;; and prediction uses player-zone-id for collision map lookup
    (when (and player zone-id)
      (setf (player-zone-id player) zone-id))
    ;; ADDENDUM 4: Soft reset — check if position delta is large enough to warrant
    ;; clearing buffers. For seamless walk-through transitions, overstep preservation
    ;; keeps delta small and buffers can be preserved for smooth continuity.
    (let ((large-delta-p (and player
                              (> (position-distance-sq
                                  old-x old-y
                                  (player-x player) (player-y player))
                                 *soft-reset-threshold-sq*))))
      (when (and buffer large-delta-p)
        ;; Clear interpolation buffer - stale positions are invalid after large jump
        (setf (interpolation-buffer-count buffer) 0
              (interpolation-buffer-head buffer) 0))
      (when (and pred player large-delta-p)
        ;; Reset prediction state - stale predicted position causes "stuck" movement
        ;; NOTE: Don't reset last-acked-sequence (leave unchanged to avoid snap spike)
        (setf (prediction-state-predicted-x pred) (player-x player)
              (prediction-state-predicted-y pred) (player-y player)
              (prediction-state-input-count pred) 0
              (prediction-state-input-head pred) 0))
      ;; Always update prediction position to new coords (even for small delta)
      ;; so prediction doesn't drift. Input buffers are preserved for continuity.
      (when (and pred player (not large-delta-p))
        (setf (prediction-state-predicted-x pred) (player-x player)
              (prediction-state-predicted-y pred) (player-y player))))
    ;; Clear stale NPC targets — old zone's NPCs are no longer valid.
    (when player
      (setf (player-attack-target-id player) 0
            (player-follow-target-id player) 0))
    ;; Rebase or clear click-to-move targets on BOTH player-intent and
    ;; game-client-intent. The server rebases via transition-zone, but that
    ;; only runs server-side. The client must do its own rebasing here.
    ;; game-client-intent is the source of truth (it's what the client sends
    ;; to the server every frame and what run-local applies via apply-client-intent).
    ;; Bug 5/6: Skip rebase when a multi-hop path is active -- the continuation
    ;; below will set the correct next-hop target. The rebase block can clear
    ;; intents (line "unless rebased") which would deactivate intent-target-active
    ;; before the continuation runs.
    (unless (game-zone-click-path game)
    (let* ((ci (game-client-intent game))
           (p-intent (and player (player-intent player)))
           (had-target (and ci (intent-target-active ci)))
           (target-clamped (and had-target (intent-target-clamped-p ci)))
           (target-raw-x (and had-target (intent-target-raw-x ci)))
           (target-raw-y (and had-target (intent-target-raw-y ci)))
           (target-x (and had-target
                          (if target-clamped
                              target-raw-x
                              (intent-target-x ci))))
           (target-y (and had-target
                          (if target-clamped
                              target-raw-y
                              (intent-target-y ci))))
           (new-zone-id zone-id)
           (wg (and world (world-world-graph world)))
           ;; Find the exit edge from old zone to new zone
           (exit-edge (when (and wg old-zone-id new-zone-id)
                        (let ((exits (world-graph-exits wg old-zone-id)))
                          (loop :for ex :in exits
                                :when (eq (getf ex :to) new-zone-id)
                                :return (getf ex :edge)))))
           ;; Get collision bounds for old and new zones.
           ;; Client often lacks zone-state, so fall back to world bounds:
           ;;   - Source (old zone): use old-world-bounds captured BEFORE apply-game-state
           ;;   - Dest (new zone): use current world bounds (now reflect new zone)
           (tile-size (and world (world-tile-dest-size world)))
           (half-w (and world (world-collision-half-width world)))
           (half-h (and world (world-collision-half-height world))))
      (multiple-value-bind (zone-src-min-x zone-src-max-x zone-src-min-y zone-src-max-y)
          (if (and old-zone-id tile-size half-w half-h)
              (get-zone-collision-bounds old-zone-id tile-size half-w half-h)
              (values nil nil nil nil))
        ;; Source bounds: prefer per-zone, fall back to old world bounds
        (let* ((src-min-x (or zone-src-min-x (and old-world-bounds (first old-world-bounds))))
               (src-max-x (or zone-src-max-x (and old-world-bounds (second old-world-bounds))))
               (src-min-y (or zone-src-min-y (and old-world-bounds (third old-world-bounds))))
               (src-max-y (or zone-src-max-y (and old-world-bounds (fourth old-world-bounds)))))
          (multiple-value-bind (zone-dst-min-x zone-dst-max-x zone-dst-min-y zone-dst-max-y)
              (if (and new-zone-id tile-size half-w half-h)
                  (get-zone-collision-bounds new-zone-id tile-size half-w half-h)
                  (values nil nil nil nil))
            ;; Dest bounds: prefer per-zone, fall back to current world bounds (new zone)
            (let* ((dst-min-x (or zone-dst-min-x (and world (world-wall-min-x world))))
                   (dst-max-x (or zone-dst-max-x (and world (world-wall-max-x world))))
                   (dst-min-y (or zone-dst-min-y (and world (world-wall-min-y world))))
                   (dst-max-y (or zone-dst-max-y (and world (world-wall-max-y world)))))
          (let* ((crossing-p (and had-target exit-edge
                                  src-min-x src-max-x src-min-y src-max-y
                                  (or target-clamped
                                      (< target-x src-min-x) (> target-x src-max-x)
                                      (< target-y src-min-y) (> target-y src-max-y))))
                 (rebased nil))
            (when (and crossing-p
                       dst-min-x dst-max-x dst-min-y dst-max-y)
              ;; Rebase the target across the seam
              (multiple-value-bind (tx ty)
                  (seam-translate-position exit-edge target-x target-y
                                           src-min-x src-max-x src-min-y src-max-y
                                           dst-min-x dst-max-x dst-min-y dst-max-y)
                (let ((rx (clamp tx dst-min-x dst-max-x))
                      (ry (clamp ty dst-min-y dst-max-y)))
                  (setf rebased t)
                  ;; Set rebased target on both intents
                  (when ci
                    (set-intent-target ci rx ry)
                    (setf (intent-target-raw-x ci) rx
                          (intent-target-raw-y ci) ry
                          (intent-target-clamped-p ci) nil))
                  (when p-intent
                    (set-intent-target p-intent rx ry)
                    (setf (intent-target-raw-x p-intent) rx
                          (intent-target-raw-y p-intent) ry
                          (intent-target-clamped-p p-intent) nil)))))
            ;; If not crossing or rebasing failed, clear both intents
            (unless rebased
              (when ci (clear-intent-target ci))
              (when p-intent (clear-intent-target p-intent)))
            (when *verbose-zone-transitions*
              (log-zone "Target rebase: old=~a new=~a edge=~a src=(~a ~a ~a ~a) dst=(~a ~a ~a ~a) rebased=~a"
                        old-zone-id new-zone-id exit-edge
                        src-min-x src-max-x src-min-y src-max-y
                        dst-min-x dst-max-x dst-min-y dst-max-y
                        rebased))))))))) ; end unless game-zone-click-path
    ;; Bug 4: Multi-hop zone-click continuation
    ;; After a hop completes, if we have a zone-click-path, advance through it.
    ;; The path, edges, and hop-targets are parallel lists.  When we arrive at
    ;; a zone in the path, pop everything up to and including it.
    ;; The continuation uses PRECOMPUTED hop targets (derived from the original
    ;; raw click) rather than the player's runtime position, so the walk target
    ;; is always correct regardless of where the player stands in the zone.
    (when (and (game-zone-click-path game) zone-id)
      (let ((remaining (game-zone-click-path game))
            (remaining-edges (game-zone-click-edges game))
            (remaining-targets (game-zone-click-hop-targets game))
            (ci (game-client-intent game))
            (p-intent (and player (player-intent player))))
        (let ((pos (position zone-id remaining)))
          (cond
            (pos
             ;; Arrived at a zone in our path — advance past it
             (setf (game-zone-click-path game) (nthcdr (1+ pos) remaining))
             (setf (game-zone-click-edges game) (nthcdr (1+ pos) remaining-edges))
             (setf (game-zone-click-hop-targets game) (nthcdr (1+ pos) remaining-targets))
             (let ((still-remaining (game-zone-click-path game))
                   (still-targets (game-zone-click-hop-targets game)))
               (if (null still-remaining)
                   ;; Final destination reached — set final target, clear path
                   (let ((fx (game-zone-click-final-x game))
                         (fy (game-zone-click-final-y game)))
                     (when ci
                       (set-intent-target ci fx fy)
                       (setf (intent-target-raw-x ci) fx
                             (intent-target-raw-y ci) fy
                             (intent-target-clamped-p ci) nil))
                     (when p-intent
                       (set-intent-target p-intent fx fy)
                       (setf (intent-target-raw-x p-intent) fx
                             (intent-target-raw-y p-intent) fy
                             (intent-target-clamped-p p-intent) nil))
                     (log-verbose "Multi-hop path: arrived at final zone ~a, target=(~,1f,~,1f)"
                                  zone-id fx fy)
                     (clear-zone-click-path game))
                   ;; More hops remain — use the precomputed walk target for
                   ;; this hop.  The target was derived from the original raw
                   ;; click coordinates translated through each seam, so it
                   ;; points toward the correct edge regardless of where the
                   ;; player currently stands.
                   (let ((hop-target (first still-targets)))
                     (if hop-target
                         (let ((target-x (car hop-target))
                               (target-y (cdr hop-target)))
                           ;; Bug 5/6: Pass nil for world to skip clamping -- hop targets
                           ;; are intentionally past the zone edge to trigger crossings.
                           ;; Pass nil for mark-p to suppress click marker on continuation hops.
                           (when (and ci player)
                             (set-player-walk-target player ci target-x target-y nil nil))
                           (when p-intent
                             (set-player-walk-target player p-intent target-x target-y nil nil))
                           (log-verbose "Multi-hop path: zone ~a, hop target=(~,1f,~,1f) (~d hops left)"
                                        zone-id target-x target-y (length still-remaining)))
                         ;; No target stored — clear path (fallback)
                         (progn
                           (log-verbose "Multi-hop path: no hop target from ~a, clearing path"
                                        zone-id)
                           (clear-zone-click-path game)))))))
            (t
             ;; Bug 5/6: Unexpected zone -- try to recompute path from here to final destination
             (let ((final-dest (car (last remaining))))
               (if (and final-dest
                        (not (game-zone-click-retry-p game)))  ; one retry only
                   (let* ((path-world (game-world game))
                          (graph (and path-world (world-world-graph path-world)))
                          (new-path (and graph (world-graph-find-path graph zone-id final-dest))))
                     (if new-path
                         (let ((new-edges (and graph (zone-path-edge-list graph zone-id new-path))))
                           (if new-edges
                               ;; Reverse-translate final-dest coords from destination-zone
                               ;; local space back to current zone's local space, then
                               ;; forward-translate along the new path.
                               (multiple-value-bind (local-x local-y)
                                   (reverse-translate-along-path path-world
                                     (game-zone-click-final-x game)
                                     (game-zone-click-final-y game)
                                     zone-id new-path)
                                 (if (and local-x local-y)
                               (multiple-value-bind (fx fy new-targets)
                                   (translate-click-along-path path-world
                                     local-x local-y
                                     zone-id new-path)
                                 (if (and fx fy new-targets)
                                     (progn
                                       (setf (game-zone-click-path game) new-path
                                             (game-zone-click-edges game) new-edges
                                             (game-zone-click-hop-targets game) new-targets
                                             (game-zone-click-final-x game) fx
                                             (game-zone-click-final-y game) fy
                                             (game-zone-click-retry-p game) t)
                                       ;; Set first hop target
                                       (let ((hop-target (first new-targets)))
                                         (when hop-target
                                           (set-player-walk-target player ci
                                             (car hop-target) (cdr hop-target) nil nil)
                                           (when p-intent
                                             (set-player-walk-target player p-intent
                                               (car hop-target) (cdr hop-target) nil nil))))
                                       (log-verbose "Multi-hop: recomputed path from ~a: ~a"
                                                    zone-id new-path))
                                     (clear-zone-click-path game)))
                                 (clear-zone-click-path game))) ; reverse-translate failed
                               (clear-zone-click-path game)))
                         (clear-zone-click-path game)))
                   (progn
                     (log-verbose "Multi-hop path: unexpected zone ~a (expected one of ~a), clearing"
                                  zone-id remaining)
                     (clear-zone-click-path game)))))))))
    ;; Clear stale edge-strips from the old zone. Between zone change and
    ;; next snapshot, old edge-strips would render as ghost sprites at wrong
    ;; positions. New edge-strips arrive with the next server snapshot.
    (setf (game-edge-strips game) nil)
    ;; Phase 2: Sync zone-state NPCs with game-npcs so rendering matches
    (sync-client-zone-npcs game)
    ;; Step 5: Queue all adjacent zones for preloading after zone change
    (cold-start-preload-adjacent game)
    ;; Step 12: Post-transition log with wall-clock time
    (when (and *verbose-zone-transitions* t0)
      (let* ((t1 (get-internal-real-time))
             (elapsed-ms (* (/ (float (- t1 t0) 1.0)
                               (float internal-time-units-per-second 1.0))
                            1000.0))
             (zone-lru (game-zone-cache game))
             (cache-size (if zone-lru (hash-table-count (zone-cache-entries zone-lru)) 0))
             (cache-cap (if zone-lru (zone-cache-capacity zone-lru) 0))
             (queue-depth (length (game-preload-queue game)))
             (delta-sq (if player
                           (position-distance-sq old-x old-y
                                                 (player-x player) (player-y player))
                           0.0)))
        (log-zone "Zone transition: ~a -> ~a wall=~,2fms cache=~d/~d hits=~d misses=~d preload-queue=~d delta=~,1f soft-reset=~a threshold=~,1f"
                  diag-from-zone zone-id elapsed-ms
                  cache-size cache-cap diag-cache-hits diag-cache-misses
                  queue-depth (sqrt delta-sq)
                  (if (> delta-sq *soft-reset-threshold-sq*) "yes" "no")
                  (sqrt *soft-reset-threshold-sq*))))))

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
    ;; Server-side preloading: warm adjacent zones to avoid sync load stalls.
    (when (eq (game-net-role game) :server)
      (update-server-preloading game)
      (let ((queue (game-preload-queue game)))
        (when (and queue (> (length queue) 0))
          (let* ((tile-size (world-tile-dest-size world))
                 (urgent-px (* *zone-urgent-preload-tiles* tile-size))
                 (urgent (loop :for p :across players
                               :thereis (and p
                                             (player-within-urgent-preload-distance-for-zone-p
                                              p world (player-zone-id p) urgent-px)))))
            (if urgent
                (process-preload-queue game :count (length queue))
                (process-preload-queue game)))))
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
    ;; Zone transition state machine: server/local only.
    ;; Clients receive zone changes via snapshot zone-id (net.lisp apply-snapshot).
    ;; Running this on client would cause synchronous load-zone hitching.
    (let ((transitioned (if (and allow-player-control
                                (not (eq (game-net-role game) :client)))
                           (update-zone-transition game)
                           0)))
      (declare (type fixnum transitioned))
      (when (plusp transitioned)
        (setf npcs (game-npcs game)
              entities (game-entities game))
        (loop :for current-player :across players
              :do (setf (player-attack-target-id current-player) 0
                        (player-follow-target-id current-player) 0))
        (reset-npc-frame-intents npcs))
      ;; Step 12: Per-tick transition count, preload queue depth, and cache occupancy
      (let* ((zone-lru (game-zone-cache game))
             (cache-size (if zone-lru (hash-table-count (zone-cache-entries zone-lru)) 0))
             (cache-cap (if zone-lru (zone-cache-capacity zone-lru) 0))
             (cache-hits (if zone-lru (zone-cache-hits zone-lru) 0))
             (cache-misses (if zone-lru (zone-cache-misses zone-lru) 0)))
        (log-zone "Zone tick: transitions=~d preload-queue=~d cache=~d/~d hits=~d misses=~d"
                     transitioned
                     (length (game-preload-queue game))
                     cache-size cache-cap cache-hits cache-misses))
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
                    :do (progn
                          (update-npc-behavior npc target-player world)
                          (update-npc-intent npc target-player world dt)
                          (update-npc-movement npc world dt)
                          (when target-player
                            (update-npc-attack npc target-player world dt event-queue)))))))
      (loop :for current-player :across players
            :do (consume-intent-actions (player-intent current-player)))
      ;; Increment playtime for all connected players (server-side tracking)
      (loop :for current-player :across players
            :do (incf (player-playtime current-player) dt))
      (log-gc-delta)  ; Log allocation for this tick (Task 6.2)
      transitioned)))))

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
    (validate-zone-config)
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
