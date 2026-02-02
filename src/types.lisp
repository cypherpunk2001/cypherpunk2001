;; NOTE: If you change behavior here, update docs/types.md :)
(in-package #:mmorpg)

(defstruct (player (:constructor %make-player))
  ;; Player state used by update/draw loops.
  ;; Type declarations for SBCL optimization (Phase 1 perf work)
  (id 0 :type fixnum)
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (dx 0.0 :type single-float)
  (dy 0.0 :type single-float)
  (zone-id nil :type (or null keyword))
  (intent nil :type (or null intent))
  (stats nil :type (or null stat-block))
  (inventory nil :type (or null inventory))
  (equipment nil :type (or null equipment))
  (attack-target-id 0 :type fixnum)
  (follow-target-id 0 :type fixnum)
  (pickup-target-id nil :type (or null symbol))
  (pickup-target-tx 0 :type fixnum)
  (pickup-target-ty 0 :type fixnum)
  (pickup-target-active nil :type boolean)
  (click-marker-x 0.0 :type single-float)
  (click-marker-y 0.0 :type single-float)
  (click-marker-timer 0.0 :type single-float)
  (click-marker-kind nil :type (or null keyword))
  (click-marker-target-id 0 :type fixnum)
  (anim-state :idle :type keyword)
  (facing :down :type keyword)
  (facing-sign 1.0 :type single-float)
  (class nil :type (or null character-class))
  (hp 0 :type fixnum)
  (lifetime-xp 0 :type fixnum)
  (playtime 0.0 :type single-float)
  (created-at 0 :type fixnum)
  ;; Deaths counter (Phase 4 - leaderboards)
  (deaths 0 :type fixnum)
  (frame-index 0 :type fixnum)
  (frame-timer 0.0 :type single-float)
  (attacking nil :type boolean)
  (attack-timer 0.0 :type single-float)
  (attack-hit nil :type boolean)
  (hit-active nil :type boolean)
  (hit-timer 0.0 :type single-float)
  (hit-frame 0 :type fixnum)
  (hit-facing :down :type keyword)
  (hit-facing-sign 1.0 :type single-float)
  (running nil :type boolean)
  (run-stamina 0.0 :type single-float)
  (auto-right nil :type boolean)
  (auto-left nil :type boolean)
  (auto-down nil :type boolean)
  (auto-up nil :type boolean)
  (mouse-hold-timer 0.0 :type single-float)
  (inventory-lines nil :type (or null simple-vector))
  (inventory-count 0 :type fixnum)
  (inventory-dirty t :type boolean)
  (hud-stats-lines nil :type (or null simple-vector))
  (hud-stats-count 0 :type fixnum)
  (hud-stats-dirty t :type boolean)
  (last-sequence 0 :type fixnum)
  ;; Network delta compression (see docs/net.md 4-Prong Approach)
  (snapshot-dirty t :type boolean)
  ;; Force full resync on next snapshot (set after unstuck teleport)
  (force-full-resync nil :type boolean)
  ;; Spatial grid cell tracking (nil if not in a grid)
  (grid-cell-x nil :type (or null fixnum))
  (grid-cell-y nil :type (or null fixnum))
  ;; Seamless zone transition state (ephemeral, not persisted)
  (zone-transition-cooldown 0.0 :type single-float)   ; Seconds remaining before next transition allowed
  (zone-transition-pending nil :type (or null keyword)) ; Edge keyword (:north etc) or nil
  (zone-transition-last-time 0.0 :type single-float)   ; Wall-clock time of last commit (for thrash metrics)
  ;; Pre-collision attempted position (ephemeral, not persisted/serialized).
  ;; Set every tick in update-player-position BEFORE collision resolution.
  ;; Used by world-crossing-edge for commit detection and by seam translation.
  (attempted-x 0.0 :type single-float)
  (attempted-y 0.0 :type single-float))

(defstruct (npc (:constructor %make-npc))
  ;; NPC state used by update/draw loops.
  ;; Type declarations for SBCL optimization (Phase 1 perf work)
  (id 0 :type fixnum)
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (intent nil :type (or null intent))
  (stats nil :type (or null stat-block))
  (anim-state :idle :type keyword)
  (facing :down :type keyword)
  (archetype nil :type (or null npc-archetype))
  (behavior-state :idle :type keyword)
  (provoked nil :type boolean)
  (home-x 0.0 :type single-float)
  (home-y 0.0 :type single-float)
  (wander-x 0.0 :type single-float)
  (wander-y 0.0 :type single-float)
  (wander-timer 0.0 :type single-float)
  (attack-timer 0.0 :type single-float)
  (frame-index 0 :type fixnum)
  (frame-timer 0.0 :type single-float)
  (hits-left 0 :type fixnum)
  (alive t :type boolean)
  (respawn-timer 0.0 :type single-float)
  (hit-active nil :type boolean)
  (hit-timer 0.0 :type single-float)
  (hit-frame 0 :type fixnum)
  (hit-facing :down :type keyword)
  (hit-facing-sign 1.0 :type single-float)
  ;; Network delta compression (see docs/net.md 4-Prong Approach)
  (snapshot-dirty t :type boolean)
  ;; Spatial grid cell tracking (nil if not in a grid)
  (grid-cell-x nil :type (or null fixnum))
  (grid-cell-y nil :type (or null fixnum)))

(defstruct zone-state
  "State for a single zone: zone data, NPCs, and derived collision data.
   Used by *zone-states* cache to support multiple simultaneous zones.
   Type declarations for SBCL optimization (Phase 1 perf work)."
  (zone-id nil :type (or null keyword))
  (zone nil :type (or null zone))           ; Zone struct (tiles, collision, objects, spawns)
  (npcs (vector) :type vector)              ; Vector of NPCs in this zone
  (wall-map nil :type (or null array))      ; 2D array for collision detection
  (objects nil :type list)                  ; Respawning objects list
  ;; Spatial grids for proximity queries (per-zone, not global)
  (player-grid nil :type (or null spatial-grid))  ; Spatial grid for players in this zone
  (npc-grid nil :type (or null spatial-grid))     ; Spatial grid for NPCs in this zone
  ;; NPC index map for O(1) lookup by ID (npc-id -> array index)
  (npc-index-map nil :type (or null hash-table))
  ;; Cached player array for O(zone-players) serialization (Task 4.1)
  ;; Maintained by add/remove-player-zone-cache and zone transitions
  (zone-players (make-array 0 :adjustable t :fill-pointer 0) :type vector))

(defstruct (skill (:constructor make-skill (&key (level 1) (xp 0))))
  ;; Skill state tracking level and xp.
  ;; Type declarations for SBCL optimization (Phase 1 perf work)
  (level 1 :type fixnum)
  (xp 0 :type fixnum))

(defstruct (stat-modifiers (:constructor make-stat-modifiers
                              (&key (attack 0) (strength 0) (defense 0) (hitpoints 0))))
  ;; Additive stat bonuses from equipment or buffs.
  ;; Type declarations for SBCL optimization (Phase 1 perf work)
  (attack 0 :type fixnum)
  (strength 0 :type fixnum)
  (defense 0 :type fixnum)
  (hitpoints 0 :type fixnum))

(defstruct (stat-block (:constructor make-stat-block))
  ;; Combat stats and training mode for a combatant.
  ;; Skills are typed via skill struct; training-mode is keyword or nil
  attack strength defense hitpoints
  (training-mode nil :type (or null keyword))
  modifiers)

(defstruct (inventory-slot (:constructor make-inventory-slot
                              (&key item-id (count 0))))
  ;; Single inventory slot holding stackable items.
  ;; Type declarations for SBCL optimization (Phase 1 perf work)
  (item-id nil :type (or null keyword))
  (count 0 :type fixnum))

(defstruct (inventory (:constructor %make-inventory))
  ;; Inventory slots for a player.
  slots)

(defstruct (equipment (:constructor %make-equipment))
  ;; Equipped item IDs aligned to *equipment-slot-ids*.
  items)

(defparameter *player-hud-lines* 6) ;; Number of cached HUD lines for player stats.

(defstruct (id-source (:constructor make-id-source (&optional (next-id 1) (persistent nil))))
  ;; Monotonic IDs for entities inside a simulation.
  ;; If PERSISTENT is T, the counter is saved to storage on each allocation.
  next-id
  persistent)

(defstruct (world (:constructor %make-world))
  "World state including tiles, collision, and derived bounds.
   Type declarations for SBCL optimization (Phase 1 perf work)."
  (tile-size-f 0.0 :type single-float)
  (tile-dest-size 0.0 :type single-float)
  (floor-index 0 :type fixnum)
  (zone nil :type (or null zone))
  (zone-label nil :type (or null string))
  (world-graph nil :type (or null world-graph))
  (zone-preview-cache nil :type (or null hash-table))
  (minimap-spawns nil :type list)
  (minimap-collisions nil :type (or null vector))
  (minimap-dirty nil :type boolean)
  (wall-map nil :type (or null array))
  (wall-map-width 0 :type fixnum)
  (wall-map-height 0 :type fixnum)
  (collision-half-width 0.0 :type single-float)
  (collision-half-height 0.0 :type single-float)
  (wall-min-x 0.0 :type single-float)
  (wall-max-x 0.0 :type single-float)
  (wall-min-y 0.0 :type single-float)
  (wall-max-y 0.0 :type single-float))

(defstruct (audio (:constructor %make-audio))
  ;; Audio state for music playback and UI labels.
  soundtrack-count soundtrack-music soundtrack-names soundtrack-labels
  soundtrack-index current-music current-track-label
  volume-steps volume-level volume-bars music-volume)

(defstruct (ui (:constructor %make-ui))
  ;; UI state for menu layout, colors, and HUD labels.
  menu-open exit-requested
  loading-label loading-timer
  ;; Login screen state
  login-active auth-complete username-buffer password-buffer
  auth-error-message server-selector-index
  ;; Server connection status (:online :offline :connecting)
  server-status server-last-heard server-next-ping
  ;; Ping RTT tracking (for HUD display)
  ping-send-time ping-rtt-ms
  menu-padding menu-panel-width menu-panel-height menu-panel-x menu-panel-y
  menu-title menu-hint menu-track-title menu-prev-label menu-next-label
  menu-vol-down-label menu-vol-up-label
  menu-title-size menu-hint-size menu-track-size menu-button-text-size
  menu-nav-text-size menu-volume-text-size
  menu-logout-label menu-logout-x menu-logout-y menu-logout-width menu-logout-height
  menu-unstuck-label menu-unstuck-x menu-unstuck-y menu-unstuck-width menu-unstuck-height
  menu-save-label menu-load-label
  menu-save-x menu-save-y
  menu-load-x menu-load-y
  menu-nav-button-width menu-nav-button-height menu-nav-gap menu-nav-y menu-prev-x menu-next-x
  menu-track-text-x menu-track-text-y
  menu-volume-button-width menu-volume-button-height menu-volume-gap menu-volume-y
  menu-volume-down-x menu-volume-up-x menu-volume-bars-x
  menu-toggle-gap menu-debug-size menu-debug-x menu-debug-y menu-debug-label
  menu-editor-size menu-editor-x menu-editor-y menu-editor-label
  menu-fullscreen-size menu-fullscreen-x menu-fullscreen-y menu-fullscreen-label
  hud-bg-color menu-overlay-color menu-panel-color menu-text-color
  menu-button-color menu-button-hover-color
  inventory-open
  chat-active chat-buffer chat-prompt chat-max-length
  hover-npc-name
  context-open context-x context-y context-world-x context-world-y
  context-target-id context-target-type context-object-id context-slot-index context-item-id
  context-has-walk context-has-attack context-has-follow context-has-pickup
  context-has-examine context-has-drop
  context-width context-option-height context-padding context-text-size
  context-walk-label context-attack-label context-follow-label
  context-pickup-label context-examine-label context-drop-label
  ;; Inventory drag-and-drop state
  drag-active drag-slot-index drag-item-id drag-start-x drag-start-y
  minimap-x minimap-y minimap-width minimap-height minimap-point-size
  minimap-bg-color minimap-border-color minimap-player-color minimap-npc-color
  minimap-collision-color
  debug-grid-color debug-wall-color debug-collision-color debug-collider-color
  stamina-labels
  hud-stats-text-size hud-stats-line-gap
  hud-log-text-size hud-log-line-gap hud-log-lines hud-log-buffer hud-log-times
  hud-log-index hud-log-count
  combat-log-text-size combat-log-line-gap
  combat-log-lines combat-log-index combat-log-count combat-log-buffer)

(defstruct (render (:constructor %make-render))
  ;; Reusable render rectangles and vectors to avoid consing.
  origin tile-source tile-dest player-source player-dest npc-source npc-dest)

(defstruct (npc-textures (:constructor %make-npc-textures))
  ;; Loaded NPC textures for idle directions.
  down-idle up-idle side-idle)

(defstruct (assets (:constructor %make-assets))
  ;; Loaded textures and sprite sizing data.
  tileset
  down-idle down-walk down-attack
  up-idle up-walk up-attack
  side-idle side-walk side-attack
  npc-animations object-textures item-textures
  blood-down blood-up blood-side
  scaled-width scaled-height half-sprite-width half-sprite-height)

(defstruct (camera (:constructor %make-camera))
  ;; Camera state used by 2D mode.
  offset zoom)

(defstruct (editor (:constructor %make-editor))
  ;; Editor state for in-game map editing.
  active mode
  camera-x camera-y move-speed
  selected-tile selection-width selection-height selection-anchor tile-count
  tileset-catalog tileset-index tileset-label-text
  tile-layer-id collision-layer-id object-layer-id
  zone-root zone-files zone-index zone-label zone-history
  spawn-catalog spawn-index
  object-catalog object-index
  mode-label tile-label object-label-text status-label status-timer
  export-path dirty)

(defstruct (combat-event (:constructor make-combat-event (&key type text)))
  ;; Event emitted by simulation for UI/logging (decouples server from client).
  type  ; :combat-log, :hud-message
  text)

;;; Render Chunk Cache (Phase 1 - Zoom-Out Performance Optimization)
;;; Pre-render static tile chunks into render textures to reduce draw calls.

(defstruct (render-chunk-cache (:constructor %make-render-chunk-cache))
  "Cached render texture for a chunk of tiles."
  (texture nil)              ; raylib render-texture-2d (from raylib:load-render-texture)
  (chunk-x 0 :type fixnum)   ; Chunk X coordinate in chunk-space
  (chunk-y 0 :type fixnum)   ; Chunk Y coordinate in chunk-space
  (layer-key nil)            ; (layer-id . tileset-id) for cache key
  (dirty t :type boolean)    ; T if chunk needs re-rendering
  (last-access 0 :type fixnum)) ; Frame counter for LRU eviction

(defstruct (zone-render-cache (:constructor %make-zone-render-cache))
  "Per-zone render cache state."
  ;; Task 4.2: Changed from :test 'equal to :test 'eql for packed fixnum keys
  (chunks (make-hash-table :test 'eql :size 1024)) ; key: packed-fixnum -> render-chunk-cache
  (chunk-pixel-size 0)       ; *render-chunk-size* * tile-dest-size (computed once)
  (zone-id nil)              ; Track which zone this cache belongs to
  (frame-counter 0 :type fixnum)) ; Frame counter for LRU tracking

;;;; ========================================================================
;;;; Event Ring Buffer (Task 4.3)
;;;; Fixed-size ring buffer to avoid per-event list allocation.
;;;; ========================================================================

(defconstant +event-ring-size+ 256
  "Maximum events in ring buffer. Oldest events dropped if exceeded.")

(defstruct (combat-event-queue
            (:constructor %make-combat-event-queue)
            (:constructor make-combat-event-queue
                (&aux (buffer (make-array +event-ring-size+ :initial-element nil)))))
  "Ring buffer of combat events for the UI to process.
   Task 4.3: Replaces list-based queue to avoid nconc allocation."
  (buffer nil :type (or null simple-vector))  ; Fixed-size array of events
  (head 0 :type fixnum)   ; Next write position
  (tail 0 :type fixnum)   ; Next read position
  (count 0 :type fixnum)) ; Number of valid events

(defstruct (interpolation-snapshot (:constructor %make-interpolation-snapshot))
  ;; Cached entity positions at a specific timestamp for interpolation.
  ;; Type declarations for SBCL optimization (Phase 1 perf work)
  (timestamp 0.0 :type single-float)
  (entity-positions nil :type (or null hash-table)))  ; Hash table: entity-id -> (x y)

(defstruct (interpolation-buffer (:constructor %make-interpolation-buffer))
  ;; Ring buffer of recent snapshots for smooth entity interpolation.
  ;; Type declarations for SBCL optimization (Phase 1 perf work)
  (snapshots nil :type (or null simple-vector))    ; Simple-vector of interpolation-snapshot
  (head 0 :type fixnum)           ; Write position (circular)
  (count 0 :type fixnum)          ; Number of valid snapshots
  (capacity 4 :type fixnum)       ; Buffer size
  ;; Pool of position tables matching ring buffer capacity (one per snapshot slot).
  ;; This avoids aliasing - each snapshot has its own dedicated position table.
  (position-pool nil :type (or null simple-vector)))  ; Simple-vector of hash tables (length = capacity)

(defstruct (prediction-input (:constructor %make-prediction-input))
  ;; A single input with sequence number for prediction reconciliation.
  ;; Type declarations for SBCL optimization (Phase 1 perf work)
  (sequence 0 :type fixnum)
  (timestamp 0.0 :type single-float)
  (move-dx 0.0 :type single-float)
  (move-dy 0.0 :type single-float)
  (target-x 0.0 :type single-float)
  (target-y 0.0 :type single-float)
  (target-active nil :type boolean))

(defstruct (prediction-state (:constructor %make-prediction-state))
  ;; Client-side prediction state for reconciliation.
  ;; Type declarations for SBCL optimization (Phase 1 perf work)
  (inputs nil :type (or null simple-vector)) ; Ring buffer of recent inputs
  (input-head 0 :type fixnum)            ; Write position
  (input-count 0 :type fixnum)           ; Number of valid inputs
  (input-capacity 64 :type fixnum)       ; Buffer size
  (input-sequence 0 :type fixnum)        ; Monotonic counter for outgoing inputs
  (last-acked-sequence 0 :type fixnum)   ; Last sequence server acknowledged
  (predicted-x 0.0 :type single-float)   ; Client's predicted position
  (predicted-y 0.0 :type single-float)
  (misprediction-count 0 :type fixnum))  ; Debug counter

;;;; ========================================================================
;;;; Object Pooling Infrastructure (Task 4.4)
;;;; Generic pool for reusing allocated objects to avoid GC pressure.
;;;; ========================================================================

(defstruct (object-pool
            (:constructor %make-object-pool (constructor &key (initial-size 64))))
  "Pool of reusable objects to avoid per-use allocation."
  (free-list nil :type list)       ; Stack of available objects
  (constructor nil :type function) ; Function to create new objects
  (allocated 0 :type fixnum)       ; Total objects ever allocated
  (peak 0 :type fixnum))           ; Peak concurrent usage

(defun pool-acquire (pool)
  "Acquire an object from POOL, creating new if necessary.
   Returns the object (caller should reset it before use)."
  (if (object-pool-free-list pool)
      (pop (object-pool-free-list pool))
      (progn
        (incf (object-pool-allocated pool))
        (setf (object-pool-peak pool)
              (max (object-pool-peak pool) (object-pool-allocated pool)))
        (funcall (object-pool-constructor pool)))))

(defun pool-release (pool object)
  "Return OBJECT to POOL for reuse."
  (push object (object-pool-free-list pool)))

(defun pool-prewarm (pool count)
  "Pre-allocate COUNT objects into POOL."
  (dotimes (i count)
    (pool-release pool (funcall (object-pool-constructor pool)))))

;;;; ========================================================================
;;;; Zone Cache (LRU) — Seamless Zone Loading Step 4
;;;; Client-side cache of loaded zone data to eliminate disk I/O.
;;;; ========================================================================

(defstruct zone-cache
  "LRU cache of zone-id -> zone struct for client-side zone preloading."
  (entries (make-hash-table :test 'eq :size 16) :type hash-table)
  (order nil :type list)   ; LRU order: most recently used at front
  (capacity 9 :type fixnum)
  (hits 0 :type fixnum)    ; Step 12: cache hit counter for diagnostics
  (misses 0 :type fixnum)) ; Step 12: cache miss counter for diagnostics

(defstruct (game (:constructor %make-game))
  ;; Aggregate of game subsystems for update/draw.
  world player players npcs entities id-source npc-id-source audio ui render assets camera editor
  combat-events client-intent net-role net-requests net-player-id
  ;; Player index map for O(1) lookup by ID (player-id -> array index)
  player-index-map
  ;; Interpolation state (client-only, for smooth remote entity movement)
  interpolation-buffer interpolation-delay client-time last-snapshot-time
  ;; Prediction state (client-only, optional via *client-prediction-enabled*)
  prediction-state
  ;; Seamless zone loading state (client-only)
  (zone-cache nil)           ; zone-cache struct (LRU)
  (preload-queue nil)        ; List of (zone-id . path) pairs to preload
  (edge-strips nil))

(defun queue-net-request (game request)
  ;; Queue a network request for the client to send.
  (when (and game request)
    (push request (game-net-requests game)))
  request)

(defun drain-net-requests (game)
  ;; Return queued net requests in FIFO order and clear the queue.
  (let ((requests (nreverse (game-net-requests game))))
    (setf (game-net-requests game) nil)
    requests))

(defun allocate-entity-id (id-source)
  ;; Return the next entity id and advance the counter.
  ;; Persists counter to storage BEFORE incrementing to prevent ID collisions
  ;; if save fails and server restarts.
  (let ((next (id-source-next-id id-source)))
    ;; Save the NEXT value first to ensure persistence before allocation
    (if (and (id-source-persistent id-source)
             (boundp '*storage*)
             *storage*)
        ;; Use retry to handle transient failures - ID allocation is critical
        ;; Phase 1: Only advance ID if save succeeds (prevents ID collision on restart)
        (let ((save-succeeded
                (with-retry-exponential (saved (lambda () (db-save-id-counter (1+ next)))
                                          :max-retries 5
                                          :initial-delay 50
                                          :max-delay 200
                                          :on-final-fail (lambda (e)
                                                           (warn "CRITICAL: ID counter save failed: ~a - allocation blocked"
                                                                 e)))
                  (when saved t))))
          (if save-succeeded
              (progn
                (setf (id-source-next-id id-source) (1+ next))
                next)
              ;; Save failed after all retries - signal error to abort login/registration
              ;; (returning nil would cause callers to create ID 0 players)
              (error "ID allocation failed: persistence unavailable after retries")))
        ;; Non-persistent mode (testing) - just increment
        (progn
          (setf (id-source-next-id id-source) (1+ next))
          next))))

(defun world-spawn-center (world)
  ;; Return a spawn center inside the collision bounds.
  (let ((x (/ (+ (world-wall-min-x world) (world-wall-max-x world)) 2.0))
        (y (/ (+ (world-wall-min-y world) (world-wall-max-y world)) 2.0)))
    (values x y)))

(defun make-player-stats (&key (attack *player-base-attack*)
                               (strength *player-base-strength*)
                               (defense *player-base-defense*)
                               (hitpoints *player-base-hitpoints*)
                               (training-mode *player-training-mode*))
  ;; Build the player's base stats with empty modifiers.
  (make-stat-block :attack (make-skill-from-level attack)
                   :strength (make-skill-from-level strength)
                   :defense (make-skill-from-level defense)
                   :hitpoints (make-skill-from-level hitpoints)
                   :training-mode training-mode
                   :modifiers (make-stat-modifiers)))

(defun skill-xp-for-level (level)
  ;; Return the minimum XP required to reach LEVEL.
  (let* ((level (max 1 level))
         (per-level (max 1 *stat-xp-per-level*))
         (base (1- level)))
    (* per-level base base)))

(defun make-skill-from-level (level)
  ;; Create a skill with XP seeded to match LEVEL.
  (make-skill :level level :xp (skill-xp-for-level level)))

(defun make-npc-stats (archetype)
  ;; Build NPC stats from the archetype or safe defaults.
  (let ((attack (or (and archetype (npc-archetype-attack-level archetype)) 1))
        (strength (or (and archetype (npc-archetype-strength-level archetype)) 1))
        (defense (or (and archetype (npc-archetype-defense-level archetype)) 1))
        (hitpoints (or (and archetype (npc-archetype-hitpoints-level archetype))
                       (and archetype (npc-archetype-max-hits archetype))
                       *npc-max-hits*)))
    (make-stat-block :attack (make-skill-from-level attack)
                     :strength (make-skill-from-level strength)
                     :defense (make-skill-from-level defense)
                     :hitpoints (make-skill-from-level hitpoints)
                     :training-mode nil
                     :modifiers (make-stat-modifiers))))

(defun stat-block-base-level (stats stat-id)
  ;; Return the base (unmodified) level for STAT-ID.
  (ecase stat-id
    (:attack (skill-level (stat-block-attack stats)))
    (:strength (skill-level (stat-block-strength stats)))
    (:defense (skill-level (stat-block-defense stats)))
    (:hitpoints (skill-level (stat-block-hitpoints stats)))))

(defun make-inventory (&optional (size *inventory-size*))
  ;; Build an inventory with SIZE empty slots.
  (let ((slots (make-array (max 0 size))))
    (dotimes (i (length slots))
      (setf (aref slots i) (make-inventory-slot)))
    (%make-inventory :slots slots)))

(defun make-equipment (&optional (slot-ids *equipment-slot-ids*))
  ;; Build an equipment container aligned to SLOT-IDS.
  (let ((items (make-array (length slot-ids) :initial-element nil)))
    (%make-equipment :items items)))

(defun make-player (start-x start-y &key (class *wizard-class*) id zone-id)
  ;; Construct a player state struct at the given start position.
  (let* ((intent (make-intent :target-x start-x :target-y start-y))
         (stats (make-player-stats))
         (max-hp (stat-block-base-level stats :hitpoints))
         (hud-lines (make-array (max 1 *player-hud-lines*)
                                :initial-element ""))
         (inventory-lines (make-array (max 1 *inventory-size*)
                                      :initial-element "")))
    (%make-player :id (or id 0)
                  :x start-x
                  :y start-y
                  :dx 0.0
                  :dy 0.0
                  :zone-id zone-id
                  :intent intent
                  :stats stats
                  :inventory (make-inventory)
                  :equipment (make-equipment)
                  :attack-target-id 0
                  :follow-target-id 0
                  :pickup-target-id nil
                  :pickup-target-tx 0
                  :pickup-target-ty 0
                  :pickup-target-active nil
                  :click-marker-x 0.0
                  :click-marker-y 0.0
                  :click-marker-timer 0.0
                  :click-marker-kind nil
                  :click-marker-target-id 0
                  :anim-state :idle
                  :facing :down
                  :facing-sign 1.0
                  :class class
                  :hp max-hp
                  :lifetime-xp 0
                  :playtime 0.0
                  :created-at (get-universal-time)
                  :frame-index 0
                  :frame-timer 0.0
                  :attacking nil
                  :attack-timer 0.0
                  :attack-hit nil
                  :hit-active nil
                  :hit-timer 0.0
                  :hit-frame 0
                  :hit-facing :down
                  :hit-facing-sign 1.0
                  :running nil
                  :run-stamina *run-stamina-max*
                  :auto-right nil
                  :auto-left nil
                  :auto-down nil
                  :auto-up nil
                  :mouse-hold-timer 0.0
                  :inventory-lines inventory-lines
                  :inventory-count 0
                  :inventory-dirty t
                  :hud-stats-lines hud-lines
                  :hud-stats-count 0
                  :hud-stats-dirty t
                  :last-sequence 0
                  :snapshot-dirty t)))

;;;; ========================================================================
;;;; NPC Pooling (Task 4.4)
;;;; Reuse NPC structs to avoid allocation during zone transitions and respawns.
;;;; ========================================================================

(defparameter *npc-pool* nil
  "Global NPC object pool. Initialized lazily on first use.")

(defparameter *use-npc-pool* nil
  "When T, acquire-npc uses pool instead of allocating fresh NPCs.
   Toggle via MMORPG_NPC_POOL=1 environment variable.")

(defun get-npc-pool ()
  "Get or create the global NPC pool."
  (unless *npc-pool*
    (setf *npc-pool*
          (%make-object-pool (lambda () (%make-npc :intent (make-intent)))
                             :initial-size 256)))
  *npc-pool*)

(defun reset-npc (npc x y archetype id)
  "Reset NPC state for reuse from pool. Returns NPC."
  (let* ((sx (float x 1.0f0))
         (sy (float y 1.0f0))
         (arch (or archetype (default-npc-archetype)))
         (stats (make-npc-stats arch))
         (max-hp (stat-block-base-level stats :hitpoints)))
    ;; Reset all fields to fresh state
    (setf (npc-id npc) (or id 0)
          (npc-x npc) sx
          (npc-y npc) sy
          (npc-stats npc) stats
          (npc-anim-state npc) :idle
          (npc-facing npc) :down
          (npc-archetype npc) arch
          (npc-behavior-state npc) :idle
          (npc-provoked npc) nil
          (npc-home-x npc) sx
          (npc-home-y npc) sy
          (npc-wander-x npc) sx
          (npc-wander-y npc) sy
          (npc-wander-timer npc) 0.0
          (npc-attack-timer npc) 0.0
          (npc-frame-index npc) 0
          (npc-frame-timer npc) 0.0
          (npc-hits-left npc) max-hp
          (npc-alive npc) t
          (npc-respawn-timer npc) 0.0
          (npc-hit-active npc) nil
          (npc-hit-timer npc) 0.0
          (npc-hit-frame npc) 0
          (npc-hit-facing npc) :down
          (npc-hit-facing-sign npc) 1.0
          (npc-snapshot-dirty npc) t
          (npc-grid-cell-x npc) nil
          (npc-grid-cell-y npc) nil)
    ;; Reset intent
    (let ((intent (npc-intent npc)))
      (when intent
        (reset-frame-intent intent)))
    npc))

(defun acquire-npc (x y &key archetype id)
  "Acquire an NPC from pool or create new. Reset for use at (X, Y)."
  (if *use-npc-pool*
      (let ((npc (pool-acquire (get-npc-pool))))
        (reset-npc npc x y archetype id))
      ;; Fallback to direct allocation
      (make-npc-direct x y :archetype archetype :id id)))

(defun release-npc (npc)
  "Return NPC to pool for reuse."
  (when (and *use-npc-pool* npc)
    (pool-release (get-npc-pool) npc)))

(defun prewarm-npc-pool (&optional (count 256))
  "Pre-allocate NPCs into the pool for faster zone loading."
  (when *use-npc-pool*
    (pool-prewarm (get-npc-pool) count)))

(defun make-npc-direct (start-x start-y &key archetype id)
  "Construct an NPC state struct directly (no pool). Internal use."
  (let ((sx (float start-x 1.0f0))
        (sy (float start-y 1.0f0))
        (archetype (or archetype (default-npc-archetype)))
        (intent (make-intent)))
    (let* ((stats (make-npc-stats archetype))
           (max-hp (stat-block-base-level stats :hitpoints)))
      (%make-npc :id (or id 0)
               :x sx
               :y sy
               :intent intent
               :stats stats
               :anim-state :idle
               :facing :down
               :archetype archetype
               :behavior-state :idle
               :provoked nil
               :home-x sx
               :home-y sy
               :wander-x sx
               :wander-y sy
               :wander-timer 0.0
               :attack-timer 0.0
               :frame-index 0
               :frame-timer 0.0
               :hits-left max-hp
               :alive t
               :respawn-timer 0.0
               :hit-active nil
               :hit-timer 0.0
               :hit-frame 0
               :hit-facing :down
               :hit-facing-sign 1.0
               :snapshot-dirty t))))

(defun make-npc (start-x start-y &key archetype id)
  "Construct an NPC state struct at the given start position.
   Uses pool if *use-npc-pool* is enabled."
  (if *use-npc-pool*
      (acquire-npc start-x start-y :archetype archetype :id id)
      (make-npc-direct start-x start-y :archetype archetype :id id)))

(defun make-npcs (player world &key id-source)
  ;; Construct NPCs from zone spawn data (or none if no spawns are defined).
  (let* ((zone (world-zone world))
         (spawns (and zone (zone-spawns zone))))
    (if (and spawns (not (null spawns)))
        (let* ((tile-size (world-tile-dest-size world))
               (total (loop :for spawn :in spawns
                            :for count = (getf spawn :count 1)
                            :sum (max 1 count)))
               (npcs (make-array total)))
          (loop :with index = 0
                :for spawn :in spawns
                :for count = (getf spawn :count 1)
                :for spawn-count = (max 1 count)
                :do (let* ((tx (getf spawn :x))
                           (ty (getf spawn :y))
                           (id (or (getf spawn :id) *npc-default-archetype-id*))
                           (archetype (find-npc-archetype id)))
                      (multiple-value-bind (x y)
                          (tile-center-position tile-size (or tx 0) (or ty 0))
                        (loop :repeat spawn-count
                              :do (setf (aref npcs index)
                                        (make-npc x y
                                                  :archetype archetype
                                                  :id (when id-source
                                                        (allocate-entity-id id-source))))
                                  (incf index)))))
          npcs)
        (make-array 0))))

(defun make-entities (players npcs)
  ;; Build a stable entity array containing NPCs followed by players.
  (let* ((npc-count (length npcs))
         (player-count (if players (length players) 0))
         (entities (make-array (+ npc-count player-count))))
    (loop :for i :from 0 :below npc-count
          :do (setf (aref entities i) (aref npcs i)))
    (loop :for i :from 0 :below player-count
          :do (setf (aref entities (+ npc-count i)) (aref players i)))
    entities))

(defun find-player-by-id (players id)
  ;; Return the player with ID, if present. O(n) linear scan fallback.
  (when (and players (> id 0))
    (loop :for player :across players
          :when (= (player-id player) id)
            :do (return player))))

(defun rebuild-player-index-map (game)
  "Rebuild the player-index-map from the current players array. O(n).
   Called when the players array structure changes (add/remove/rebuild)."
  (let ((players (game-players game))
        (map (or (game-player-index-map game)
                 (make-hash-table :test 'eql :size 512))))
    (clrhash map)
    (when players
      (loop :for i :from 0 :below (length players)
            :for player = (aref players i)
            :when player
            :do (setf (gethash (player-id player) map) i)))
    (setf (game-player-index-map game) map)
    map))

(defun find-player-by-id-fast (game id)
  "O(1) player lookup using the game's index map. Falls back to linear scan if no map."
  (let ((players (game-players game))
        (map (game-player-index-map game)))
    (when (and players (> id 0))
      (if map
          (let ((index (gethash id map)))
            (when (and index (< index (length players)))
              (aref players index)))
          ;; Fallback to linear scan if no map exists
          (find-player-by-id players id)))))

(defun rebuild-npc-index-map (zone-state)
  "Rebuild the npc-index-map from the current NPCs array. O(n).
   Called when the NPCs array structure changes."
  (when zone-state
    (let ((npcs (zone-state-npcs zone-state))
          (map (or (zone-state-npc-index-map zone-state)
                   (make-hash-table :test 'eql :size 256))))
      (clrhash map)
      (when npcs
        (loop :for i :from 0 :below (length npcs)
              :for npc = (aref npcs i)
              :when npc
              :do (setf (gethash (npc-id npc) map) i)))
      (setf (zone-state-npc-index-map zone-state) map)
      map)))

(defun find-npc-by-id-fast (zone-state id)
  "O(1) NPC lookup using the zone-state's index map. Falls back to linear scan if no map."
  (when zone-state
    (let ((npcs (zone-state-npcs zone-state))
          (map (zone-state-npc-index-map zone-state)))
      (when (and npcs (> id 0))
        (if map
            (let ((index (gethash id map)))
              (when (and index (< index (length npcs)))
                (aref npcs index)))
            ;; Fallback to linear scan if no map exists
            (find-npc-by-id npcs id))))))

;;; Zone-players cache management (Task 4.1)
;;; Maintains per-zone player arrays for O(zone-players) serialization instead of O(total-players).

(defun add-player-to-zone-cache (player zone-state)
  "Add PLAYER to ZONE-STATE's zone-players cache.
   Used when a player joins or transitions into a zone."
  (when (and player zone-state)
    (let ((cache (zone-state-zone-players zone-state)))
      ;; Avoid duplicates - check if player already in cache
      (unless (find player cache :test #'eq)
        (vector-push-extend player cache))
      cache)))

(defun remove-player-from-zone-cache (player zone-state)
  "Remove PLAYER from ZONE-STATE's zone-players cache.
   Used when a player leaves or transitions out of a zone."
  (when (and player zone-state)
    (let ((cache (zone-state-zone-players zone-state)))
      (when cache
        ;; Find and remove the player, shifting remaining elements
        (let ((pos (position player cache :test #'eq)))
          (when pos
            ;; Shift elements down and decrement fill pointer
            (loop :for i :from pos :below (1- (length cache))
                  :do (setf (aref cache i) (aref cache (1+ i))))
            (when (> (fill-pointer cache) 0)
              (decf (fill-pointer cache)))))))))

(defun rebuild-zone-players-cache (zone-state game)
  "Rebuild ZONE-STATE's zone-players cache from scratch using all players in GAME.
   Called when zone-state is first created or when cache becomes stale."
  (when (and zone-state game)
    (let* ((zone-id (zone-state-zone-id zone-state))
           (all-players (game-players game))
           (cache (zone-state-zone-players zone-state)))
      ;; Clear existing cache
      (setf (fill-pointer cache) 0)
      ;; Populate from all players
      (when all-players
        (loop :for player :across all-players
              :when (and player
                         (eql (or (player-zone-id player) *starting-zone-id*)
                              zone-id))
              :do (vector-push-extend player cache)))
      cache)))

(defgeneric entity-id (entity)
  (:documentation "Return the stable id for ENTITY."))

(defmethod entity-id ((entity player))
  (player-id entity))

(defmethod entity-id ((entity npc))
  (npc-id entity))

(defgeneric combatant-stats (combatant)
  (:documentation "Return the stat-block for COMBATANT."))

(defmethod combatant-stats ((combatant player))
  (player-stats combatant))

(defmethod combatant-stats ((combatant npc))
  (npc-stats combatant))

(defgeneric combatant-position (combatant)
  (:documentation "Return combatant center position as two values."))

(defgeneric combatant-alive-p (combatant)
  (:documentation "Return true when the combatant is alive/active."))

(defgeneric combatant-collision-half (combatant world)
  (:documentation "Return combatant collider half sizes in world pixels."))

(defgeneric combatant-apply-hit (combatant &optional amount)
  (:documentation "Apply a hit to the combatant (AMOUNT defaults to 1)."))

(defgeneric combatant-health (combatant)
  (:documentation "Return current and max health as two values."))

(defgeneric combatant-trigger-hit-effect (combatant)
  (:documentation "Start a hit effect animation on the combatant."))

(defgeneric combatant-update-hit-effect (combatant dt)
  (:documentation "Advance hit effect timing for the combatant."))

(defgeneric update-entity-animation (entity dt)
  (:documentation "Advance animation state for ENTITY."))

(defgeneric draw-entity (entity assets render)
  (:documentation "Render ENTITY using ASSETS and RENDER helpers."))

(defun push-combat-event (queue event)
  "Add EVENT to the combat event ring buffer.
   Task 4.3: O(1) insertion with no allocation."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (when (and queue event)
    (let* ((buffer (combat-event-queue-buffer queue))
           (size (length buffer))
           (head (combat-event-queue-head queue)))
      (declare (type fixnum size head))
      ;; Write event at head position
      (setf (aref buffer head) event)
      ;; Advance head (circular)
      (setf (combat-event-queue-head queue) (mod (1+ head) size))
      ;; Update count (capped at size)
      (when (< (combat-event-queue-count queue) size)
        (incf (combat-event-queue-count queue)))
      ;; If buffer full, advance tail (drop oldest)
      (when (= (combat-event-queue-head queue) (combat-event-queue-tail queue))
        (setf (combat-event-queue-tail queue)
              (mod (1+ (combat-event-queue-tail queue)) size))))))

(defun emit-combat-log-event (queue text)
  "Emit a combat log event to the queue."
  (when (and queue text)
    (push-combat-event queue (make-combat-event :type :combat-log :text text))))

(defun emit-hud-message-event (queue text)
  "Emit a HUD message event to the queue."
  (when (and queue text)
    (push-combat-event queue (make-combat-event :type :hud-message :text text))))

(defun pop-combat-events (queue)
  "Return all events as a list and clear the queue.
   Task 4.3: Collects events from ring buffer, then resets."
  (when queue
    (let* ((count (combat-event-queue-count queue))
           (buffer (combat-event-queue-buffer queue))
           (size (length buffer))
           (tail (combat-event-queue-tail queue))
           (events nil))
      (declare (type fixnum count size tail))
      ;; Collect events from tail to head
      (dotimes (i count)
        (let ((idx (mod (+ tail i) size)))
          (push (aref buffer idx) events)
          ;; Clear slot for GC
          (setf (aref buffer idx) nil)))
      ;; Reset queue state
      (setf (combat-event-queue-head queue) 0
            (combat-event-queue-tail queue) 0
            (combat-event-queue-count queue) 0)
      ;; Return events in insertion order (nreverse since we pushed)
      (nreverse events))))
