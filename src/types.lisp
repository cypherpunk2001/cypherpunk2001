;; NOTE: If you change behavior here, update docs/types.md :)
(in-package #:mmorpg)

(defstruct (player (:constructor %make-player))
  ;; Player state used by update/draw loops.
  id x y dx dy intent stats inventory
  anim-state facing
  facing-sign class hp
  frame-index frame-timer
  attacking attack-timer attack-hit
  hit-active hit-timer hit-frame hit-facing hit-facing-sign
  running run-stamina
  auto-right auto-left auto-down auto-up
  mouse-hold-timer
  hud-stats-lines hud-stats-count hud-stats-dirty)

(defstruct (npc (:constructor %make-npc))
  ;; NPC state used by update/draw loops.
  id x y intent stats
  anim-state facing
  archetype behavior-state provoked
  home-x home-y
  wander-x wander-y wander-timer
  attack-timer
  frame-index frame-timer
  hits-left alive
  hit-active hit-timer hit-frame hit-facing hit-facing-sign)

(defstruct (skill (:constructor make-skill (&key (level 1) (xp 0))))
  ;; Skill state tracking level and xp.
  level xp)

(defstruct (stat-modifiers (:constructor make-stat-modifiers
                              (&key (attack 0) (strength 0) (defense 0) (hitpoints 0))))
  ;; Additive stat bonuses from equipment or buffs.
  attack strength defense hitpoints)

(defstruct (stat-block (:constructor make-stat-block))
  ;; Combat stats and training mode for a combatant.
  attack strength defense hitpoints training-mode modifiers)

(defstruct (inventory-slot (:constructor make-inventory-slot
                              (&key item-id (count 0))))
  ;; Single inventory slot holding stackable items.
  item-id count)

(defstruct (inventory (:constructor %make-inventory))
  ;; Inventory slots for a player.
  slots)

(defparameter *player-hud-lines* 6) ;; Number of cached HUD lines for player stats.

(defstruct (id-source (:constructor make-id-source (&optional (next-id 1))))
  ;; Monotonic IDs for entities inside a simulation.
  next-id)

(defstruct (world (:constructor %make-world))
  ;; World state including tiles, collision, and derived bounds.
  tile-size-f tile-dest-size floor-index zone zone-label world-graph zone-npc-cache
  minimap-spawns minimap-collisions
  wall-map wall-map-width wall-map-height
  collision-half-width collision-half-height
  wall-min-x wall-max-x wall-min-y wall-max-y)

(defstruct (audio (:constructor %make-audio))
  ;; Audio state for music playback and UI labels.
  soundtrack-count soundtrack-music soundtrack-names soundtrack-labels
  soundtrack-index current-music current-track-label
  volume-steps volume-level volume-bars music-volume)

(defstruct (ui (:constructor %make-ui))
  ;; UI state for menu layout, colors, and HUD labels.
  menu-open exit-requested
  loading-label loading-timer
  menu-padding menu-panel-width menu-panel-height menu-panel-x menu-panel-y
  menu-title menu-hint menu-track-title menu-button-label menu-prev-label menu-next-label
  menu-vol-down-label menu-vol-up-label
  menu-title-size menu-hint-size menu-track-size menu-button-text-size
  menu-nav-text-size menu-volume-text-size
  menu-button-width menu-button-height menu-button-x menu-button-y
  menu-nav-button-width menu-nav-button-height menu-nav-gap menu-nav-y menu-prev-x menu-next-x
  menu-track-text-x menu-track-text-y
  menu-volume-button-width menu-volume-button-height menu-volume-gap menu-volume-y
  menu-volume-down-x menu-volume-up-x menu-volume-bars-x
  menu-toggle-gap menu-debug-size menu-debug-x menu-debug-y menu-debug-label
  menu-editor-size menu-editor-x menu-editor-y menu-editor-label
  menu-fullscreen-size menu-fullscreen-x menu-fullscreen-y menu-fullscreen-label
  hud-bg-color menu-overlay-color menu-panel-color menu-text-color
  menu-button-color menu-button-hover-color
  minimap-x minimap-y minimap-width minimap-height minimap-point-size
  minimap-bg-color minimap-border-color minimap-player-color minimap-npc-color
  minimap-collision-color
  debug-grid-color debug-wall-color debug-collision-color debug-collider-color
  stamina-labels
  hud-stats-text-size hud-stats-line-gap
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
  npc-animations
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
  mode-label tile-label object-label-text status-label status-timer
  export-path dirty)

(defstruct (game (:constructor %make-game))
  ;; Aggregate of game subsystems for update/draw.
  world player npcs entities id-source audio ui render assets camera editor)

(defun allocate-entity-id (id-source)
  ;; Return the next entity id and advance the counter.
  (let ((next (id-source-next-id id-source)))
    (setf (id-source-next-id id-source) (1+ next))
    next))

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

(defun make-player (start-x start-y &key (class *wizard-class*) id)
  ;; Construct a player state struct at the given start position.
  (let* ((intent (make-intent :target-x start-x :target-y start-y))
         (stats (make-player-stats))
         (max-hp (stat-block-base-level stats :hitpoints))
         (hud-lines (make-array (max 1 *player-hud-lines*)
                                :initial-element "")))
    (%make-player :id (or id 0)
                  :x start-x
                  :y start-y
                  :dx 0.0
                  :dy 0.0
                  :intent intent
                  :stats stats
                  :inventory (make-inventory)
                  :anim-state :idle
                  :facing :down
                  :facing-sign 1.0
                  :class class
                  :hp max-hp
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
                  :hud-stats-lines hud-lines
                  :hud-stats-count 0
                  :hud-stats-dirty t)))

(defun make-npc (start-x start-y &key archetype id)
  ;; Construct an NPC state struct at the given start position.
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
               :hit-active nil
               :hit-timer 0.0
               :hit-frame 0
               :hit-facing :down
               :hit-facing-sign 1.0))))

(defun make-npcs (player world &key (count *npc-count*) id-source)
  ;; Construct a fixed NPC pool placed in a simple grid near the zone center.
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
        (let* ((npc-count (max 0 count))
               (npcs (make-array npc-count))
               (tile-size (world-tile-dest-size world))
               (npc-half (* (/ tile-size 2.0) *npc-collision-scale*))
               (gap (max (* *npc-spawn-gap-tiles* tile-size)
                         (+ (world-collision-half-width world) npc-half)))
               (cols (max 1 *npc-spawn-columns*))
               (spawn-ids *npc-spawn-ids*)
               (spawn-count (if spawn-ids (length spawn-ids) 0)))
          (multiple-value-bind (anchor-x anchor-y)
              (world-spawn-center world)
            (loop :for i :from 0 :below npc-count
                  :for col = (mod i cols)
                  :for row = (floor i cols)
                  :for x = (+ anchor-x (* (1+ col) gap))
                  :for y = (+ anchor-y (* row gap))
                  :for archetype = (if (> spawn-count 0)
                                       (find-npc-archetype (nth (mod i spawn-count) spawn-ids))
                                       nil)
                  :do (setf (aref npcs i)
                            (make-npc x y
                                      :archetype archetype
                                      :id (when id-source
                                            (allocate-entity-id id-source))))))
          npcs))))

(defun make-entities (player npcs)
  ;; Build a stable entity array containing NPCs followed by the player.
  (let* ((npc-count (length npcs))
         (entities (make-array (1+ npc-count))))
    (loop :for i :from 0 :below npc-count
          :do (setf (aref entities i) (aref npcs i)))
    (setf (aref entities npc-count) player)
    entities))

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
