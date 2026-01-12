(in-package #:mmorpg)

(defstruct (player (:constructor %make-player))
  ;; Player state used by update/draw loops.
  x y dx dy
  anim-state facing
  facing-sign class hp
  frame-index frame-timer
  attacking attack-timer attack-hit
  hit-active hit-timer hit-frame hit-facing hit-facing-sign
  target-x target-y target-active
  running run-stamina
  auto-right auto-left auto-down auto-up
  mouse-hold-timer)

(defstruct (npc (:constructor %make-npc))
  ;; NPC state used by update/draw loops.
  x y
  anim-state facing
  archetype behavior-state provoked
  home-x home-y
  wander-x wander-y wander-timer
  attack-timer
  frame-index frame-timer
  hits-left alive
  hit-active hit-timer hit-frame hit-facing hit-facing-sign)

(defstruct (world (:constructor %make-world))
  ;; World state including tiles, collision, and derived bounds.
  tile-size-f tile-dest-size floor-index
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
  menu-fullscreen-size menu-fullscreen-x menu-fullscreen-y menu-fullscreen-label
  hud-bg-color menu-overlay-color menu-panel-color menu-text-color
  menu-button-color menu-button-hover-color
  debug-grid-color debug-wall-color debug-collision-color debug-collider-color
  stamina-labels)

(defstruct (render (:constructor %make-render))
  ;; Reusable render rectangles and vectors to avoid consing.
  origin tile-source tile-dest player-source player-dest npc-source npc-dest)

(defstruct (assets (:constructor %make-assets))
  ;; Loaded textures and sprite sizing data.
  tileset
  down-idle down-walk down-attack
  up-idle up-walk up-attack
  side-idle side-walk side-attack
  npc-down-idle npc-up-idle npc-side-idle
  blood-down blood-up blood-side
  scaled-width scaled-height half-sprite-width half-sprite-height)

(defstruct (camera (:constructor %make-camera))
  ;; Camera state used by 2D mode.
  offset zoom)

(defstruct (game (:constructor %make-game))
  ;; Aggregate of game subsystems for update/draw.
  world player npc audio ui render assets camera)

(defun make-player (start-x start-y &optional (class *wizard-class*))
  ;; Construct a player state struct at the given start position.
  (%make-player :x start-x
                :y start-y
                :dx 0.0
                :dy 0.0
                :anim-state :idle
                :facing :down
                :facing-sign 1.0
                :class class
                :hp (if class
                        (character-class-max-hp class)
                        1)
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
                :target-x start-x
                :target-y start-y
                :target-active nil
                :running nil
                :run-stamina *run-stamina-max*
                :auto-right nil
                :auto-left nil
                :auto-down nil
                :auto-up nil
                :mouse-hold-timer 0.0))

(defun make-npc (start-x start-y &optional (archetype *rat-archetype*))
  ;; Construct an NPC state struct at the given start position.
  (let ((sx (float start-x 1.0f0))
        (sy (float start-y 1.0f0)))
    (%make-npc :x sx
               :y sy
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
               :hits-left (if archetype
                              (npc-archetype-max-hits archetype)
                              *npc-max-hits*)
               :alive t
               :hit-active nil
               :hit-timer 0.0
               :hit-frame 0
               :hit-facing :down
               :hit-facing-sign 1.0)))

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
