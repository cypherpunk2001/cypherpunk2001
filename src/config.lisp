;; NOTE: If you change behavior here, update docs/config.md :)
(in-package #:mmorpg)

;;;; ========================================================================
;;;; IMMEDIATE EFFECT VIA SLIME
;;;; These parameters are read every frame/tick. Changing them via SLIME
;;;; (setf *verbose* t) takes effect immediately without restart.
;;;; ========================================================================

;;; Debug Flags - Toggle debugging output and overlays
(defparameter *verbose* nil) ;; General verbose mode: logs network events, state changes, and diagnostic info.
(defparameter *verbose-coordinates* nil) ;; Logs entity positions and collider info per frame (very noisy).
(defparameter *verbose-logs* nil) ;; DEPRECATED: Use *verbose-coordinates* instead.
(defparameter *debug-collision-overlay* nil) ;; Draws debug grid and collision overlays.
(defparameter *debug-npc-logs* nil) ;; Logs NPC AI/combat events and enables AI debug text overlay.

;;; Feature Flags - Toggle gameplay features
(defparameter *auto-walk-enabled* t) ;; When true, WASD toggles auto-walk direction.
(defparameter *editor-start-enabled* nil) ;; When true, editor mode starts enabled.

;;; Interpolation & Prediction - Read during client loop
(defparameter *interpolation-delay-seconds* 0.1) ;; Render delay for interpolation. Higher = smoother, more perceived lag.
(defparameter *client-prediction-enabled* nil) ;; Enable client-side prediction for local player. Toggle via SLIME for testing.
(defparameter *prediction-error-threshold* 5.0) ;; Max prediction error in pixels before correction.

;;; Simulation Timing - Affects game loop behavior
(defparameter *sim-tick-seconds* (/ 1.0 60.0)) ;; Fixed simulation tick length in seconds.
(defparameter *sim-max-steps-per-frame* 5) ;; Max sim ticks per frame to avoid spiral of death.

;;; Player Movement - Read every movement update
(defparameter *player-speed* 222.0) ;; Base movement speed in pixels per second.
(defparameter *run-speed-mult* 2.0) ;; Movement speed multiplier while running.
(defparameter *run-stamina-max* 10.0) ;; Seconds of run stamina when full.
(defparameter *target-epsilon* 6.0) ;; Stop distance for click-to-move.

;;; Camera - Read every frame for view calculations
(defparameter *camera-zoom-default* 1.0) ;; Default camera zoom level.
(defparameter *camera-zoom-min* 0.5) ;; Minimum zoom level.
(defparameter *camera-zoom-max* 3.0) ;; Maximum zoom level.
(defparameter *camera-zoom-step* 0.1) ;; Zoom step per mouse wheel tick.

;;; Combat & XP - Read during combat calculations
(defparameter *stat-xp-per-level* 100) ;; XP needed for each quadratic level step.
(defparameter *stat-max-level* 99) ;; Maximum attainable level per stat.
(defparameter *xp-per-damage* 4) ;; XP awarded per point of damage dealt.
(defparameter *combat-hitpoints-xp-multiplier* 0.33) ;; HP XP multiplier applied to focused combat XP.
(defparameter *attack-hitbox-scale* 1.0) ;; Attack hitbox size relative to one tile.

;;; NPC Behavior - Read every AI/movement tick
(defparameter *npc-max-hits* 3) ;; Hits required to defeat the NPC.
(defparameter *npc-walk-speed* 120.0) ;; Base NPC movement speed in pixels per second.
(defparameter *npc-flee-speed-mult* 1.4) ;; Speed multiplier while fleeing.
(defparameter *npc-attack-range-tiles* 0.9) ;; NPC melee range in tiles.
(defparameter *npc-attack-cooldown* 0.9) ;; Seconds between NPC attacks.
(defparameter *npc-attack-damage* 1) ;; Damage per NPC hit.
(defparameter *npc-home-radius-tiles* 2.0) ;; Roam radius around spawn in tiles.
(defparameter *npc-wander-interval* 1.1) ;; Seconds between wander target changes.
(defparameter *npc-wander-arrive-distance* 6.0) ;; Pixels to consider wander target reached.
(defparameter *npc-respawn-seconds* 5.0) ;; Default respawn cooldown in seconds.
(defparameter *npc-default-archetype-id* :rat) ;; Default NPC archetype ID to spawn.
(defparameter *npc-default-loot-table-id* nil) ;; Fallback loot table when NPC archetypes omit one.
(defparameter *npc-collision-scale* 2.0) ;; Collision box size relative to one tile.

;;; Collision - Read every movement calculation
(defparameter *player-collision-scale* 2.0) ;; Collision box size relative to one tile.
(defparameter *collision-edge-epsilon* 0.01) ;; Avoid counting exact edge contact as a blocked tile.

;;; Click Marker - Read when drawing/updating markers
(defparameter *click-marker-duration* 0.6) ;; Seconds a click marker stays visible.
(defparameter *click-marker-size-scale* 0.35) ;; Marker size as a fraction of a tile.
(defparameter *click-marker-thickness* 5) ;; Marker line thickness in pixels.
(defparameter *click-marker-walk-color* (raylib:make-color :r 240 :g 210 :b 60 :a 240))
;; Marker color for walk targets.
(defparameter *click-marker-attack-color* (raylib:make-color :r 230 :g 70 :b 70 :a 240))
;; Marker color for attack targets.

;;; HUD & UI Timing - Read during UI rendering
(defparameter *hud-log-line-seconds* 30.0) ;; Seconds a HUD log line stays visible.
(defparameter *hud-log-fade-seconds* 0.4) ;; Seconds to fade out HUD log lines.
(defparameter *mouse-hold-repeat-seconds* 0.25) ;; Repeat rate for mouse-held updates.
(defparameter *chat-max-length* 180) ;; Maximum characters in a chat message.
(defparameter *zone-loading-seconds* 0.35) ;; Seconds to show the zone loading overlay after transitions.

;;; Minimap - Read when drawing minimap
(defparameter *minimap-width* 220) ;; Minimap width in pixels.
(defparameter *minimap-height* 220) ;; Minimap height in pixels.
(defparameter *minimap-padding* 12) ;; Padding from screen edges for minimap placement.
(defparameter *minimap-point-size* 4) ;; Size of player/NPC markers on the minimap.
(defparameter *minimap-preview-edge-tiles* 1.5) ;; Tiles from an exit edge to show adjacent zone spawn previews.

;;; Animation Timing - Read every animation frame
(defparameter *idle-frame-count* 4) ;; Frames in each idle animation row.
(defparameter *walk-frame-count* 6) ;; Frames in each walk animation row.
(defparameter *attack-frame-count* 4) ;; Frames in each attack animation row.
(defparameter *idle-frame-time* 0.25) ;; Seconds per idle frame.
(defparameter *walk-frame-time* 0.12) ;; Seconds per walk frame.
(defparameter *attack-frame-time* 0.1) ;; Seconds per attack frame.
(defparameter *blood-frame-count* 4) ;; Frames in each blood animation row.
(defparameter *blood-frame-time* 0.08) ;; Seconds per blood frame.

;;; Health Bar - Read when drawing health bars
(defparameter *health-bar-height* 6) ;; Height of the health bar in pixels.
(defparameter *health-bar-offset* 10) ;; Vertical offset above the sprite center.
(defparameter *health-bar-back-color* (raylib:make-color :r 8 :g 8 :b 8 :a 200)) ;; Health bar background color.
(defparameter *health-bar-fill-color* (raylib:make-color :r 70 :g 200 :b 80 :a 220)) ;; Health bar fill color.
(defparameter *health-bar-border-color* (raylib:make-color :r 220 :g 220 :b 220 :a 220)) ;; Health bar outline color.

;;; Debug Text - Read when drawing debug overlays
(defparameter *debug-npc-text-size* 12) ;; Debug text size for NPC AI overlay.
(defparameter *debug-npc-text-offset* 18) ;; Extra vertical offset for NPC debug text.
(defparameter *debug-npc-text-color* (raylib:make-color :r 255 :g 240 :b 160 :a 230)) ;; NPC AI debug text color.

;;; Minimap Colors - Read when drawing minimap
(defparameter *minimap-bg-color* (raylib:make-color :r 8 :g 12 :b 18 :a 190)) ;; Minimap background color.
(defparameter *minimap-border-color* (raylib:make-color :r 220 :g 220 :b 220 :a 200)) ;; Minimap border color.
(defparameter *minimap-player-color* (raylib:make-color :r 80 :g 220 :b 255 :a 220)) ;; Minimap player marker color.
(defparameter *minimap-npc-color* (raylib:make-color :r 255 :g 120 :b 80 :a 200)) ;; Minimap NPC marker color.
(defparameter *minimap-collision-color* (raylib:make-color :r 180 :g 180 :b 180 :a 140)) ;; Minimap collision marker color.

;;; Editor Visual Params - Read when drawing editor UI
(defparameter *editor-move-speed* 360.0) ;; Movement speed for editor camera.
(defparameter *editor-cursor-color* (raylib:make-color :r 80 :g 220 :b 255 :a 200)) ;; Editor cursor highlight color.
(defparameter *editor-spawn-color* (raylib:make-color :r 255 :g 140 :b 60 :a 210)) ;; Editor spawn marker color.
(defparameter *editor-tileset-preview-padding* 12) ;; Padding for the tileset preview panel.
(defparameter *editor-tileset-preview-max-width* 480) ;; Max width for the tileset preview panel.
(defparameter *editor-tileset-preview-max-height* 360) ;; Max height for the tileset preview panel.
(defparameter *editor-tileset-preview-bg-color* (raylib:make-color :r 10 :g 12 :b 18 :a 220)) ;; Tileset preview background color.
(defparameter *editor-tileset-preview-border-color* (raylib:make-color :r 180 :g 180 :b 180 :a 200)) ;; Tileset preview border color.
(defparameter *editor-tileset-preview-highlight-color* (raylib:make-color :r 255 :g 220 :b 120 :a 220)) ;; Tileset preview selection color.

;;; Music Volume - Read during audio updates
(defparameter *music-volume-steps* 10) ;; Number of volume steps for music controls.
(defparameter *music-default-volume-level* 1) ;; Default music volume step (0 mutes).

;;;; ========================================================================
;;;; RESTART REQUIRED
;;;; These parameters are only read at initialization. Changing them via
;;;; SLIME requires restarting the client/server to take effect.
;;;; ========================================================================

;;; Window Size - Read once at raylib:init-window
;;; Why: Raylib creates the window once at startup. Changing these affects
;;; UI layout calculations immediately, but NOT the actual window dimensions.
(defparameter *window-width* 1280) ;; Window width in pixels.
(defparameter *window-height* 720) ;; Window height in pixels.

;;; Network - Buffer allocated once at connection start
;;; Why: UDP receive buffer created once per socket. Changing this doesn't
;;; resize existing buffers.
(defparameter *net-default-host* "127.0.0.1") ;; Default UDP host for client/server.
(defparameter *net-default-port* 1337) ;; Default UDP port for client/server.
(defparameter *net-buffer-size* usocket:+max-datagram-packet-size+)
;; Max UDP payload size for snapshot messages.

;;; Auth Encryption - X25519 + ChaCha20-Poly1305
;;; Why: Client must know server's public key to encrypt credentials.
;;; Server generates this at startup and should share it with clients.
;;; For dev/local testing, leave NIL to disable encryption.
(defparameter *auth-encryption-enabled* nil) ;; Enable auth payload encryption. Requires server public key.
(defparameter *server-auth-public-key* nil) ;; Server's X25519 public key as hex string. Set by server or config.

;;; Inventory & Equipment - Array sizes set at player creation
;;; Why: Inventory/equipment arrays allocated with fixed size when player
;;; struct is created. Changing these won't resize existing players' storage.
(defparameter *inventory-size* 20) ;; Player inventory slots.
(defparameter *inventory-grid-columns* 5) ;; Inventory grid columns.
(defparameter *inventory-slot-gap* 8) ;; Inventory slot gap in pixels.
(defparameter *equipment-slot-ids* #(:head :body :legs :weapon :offhand :accessory))
;; Equipment slot order used by the equipment vector.

;;; New Player Stats - Read only when spawning new players
;;; Why: These set initial values for newly created players. Existing players
;;; retain their current stats.
(defparameter *player-base-attack* 1) ;; Base attack level for new players.
(defparameter *player-base-strength* 1) ;; Base strength level for new players.
(defparameter *player-base-defense* 1) ;; Base defense level for new players.
(defparameter *player-base-hitpoints* 10) ;; Base hitpoints level for new players.
(defparameter *player-training-mode* :balanced) ;; Training focus (:attack/:strength/:defense/:hitpoints/:balanced).

;;; Asset Paths - Textures/audio loaded once at startup
;;; Why: Assets are loaded into GPU/audio memory once. Changing paths doesn't
;;; reload already-loaded resources. Requires restart to load different files.
;; FIXME: Asset paths are currently hardcoded. Future: move to data-driven asset manifest system.
(defparameter *player-sprite-dir* "../assets/1 Characters/3") ;; Directory that holds player sprite sheets.
(defparameter *npc-sprite-dir* "../assets/3 Dungeon Enemies/1") ;; Directory that holds NPC sprite sheets.
(defparameter *blood-sprite-dir* "../assets/1 Characters/Other") ;; Directory that holds blood effect sprites.
(defparameter *tileset-path* "../assets/Zelda-like/Overworld.png") ;; Atlas image used for floor tiles.
(defparameter *soundtrack-dir* "../assets/6 Soundtrack") ;; Directory that holds soundtrack files.
(defparameter *soundtrack-tracks* ;; Vector of soundtrack file paths.
  (vector
   (format nil "~a/Juhani Junkala [Retro Game Music Pack] Title Screen.wav" *soundtrack-dir*)
   (format nil "~a/Juhani Junkala [Retro Game Music Pack] Level 1.wav" *soundtrack-dir*)
   (format nil "~a/Juhani Junkala [Retro Game Music Pack] Level 2.wav" *soundtrack-dir*)
   (format nil "~a/Juhani Junkala [Retro Game Music Pack] Level 3.wav" *soundtrack-dir*)
   (format nil "~a/Juhani Junkala [Retro Game Music Pack] Ending.wav" *soundtrack-dir*)))
(defparameter *soundtrack-display-names* ;; Vector of display names for the soundtrack.
  (vector
   "Title Screen"
   "Level 1"
   "Level 2"
   "Level 3"
   "Ending"))

;;; Sprite Frame Sizes - Used when parsing sprite sheets at load time
;;; Why: Frame dimensions define how sprites are sliced from sheets. Changing
;;; these after loading won't re-slice the textures.
(defparameter *sprite-frame-width* 32.0) ;; Width of a single sprite frame in pixels.
(defparameter *sprite-frame-height* 32.0) ;; Height of a single sprite frame in pixels.
(defparameter *sprite-scale* 4.0) ;; Scale factor applied when drawing sprites.
(defparameter *player-animation-set-id* :player) ;; Animation set ID used for the player sprite set.

;;; Tileset/Map Layout - Parsed once when loading zones
;;; Why: Zone files and tileset atlases are parsed using these values.
;;; Changing them doesn't re-parse already-loaded zones.
(defparameter *tile-size* 16) ;; Source tile size in the atlas, in pixels.
(defparameter *tile-scale* 4.0) ;; Scale factor for drawing tiles to the screen.
(defparameter *tileset-columns* 40) ;; Number of columns in the atlas grid.
(defparameter *floor-tile-index* 0) ;; Which atlas tile index to use for the floor fill (0 disables fill).
(defparameter *wall-map-width* 40) ;; Width of the test wall map in tiles.
(defparameter *wall-map-height* 24) ;; Height of the test wall map in tiles.
(defparameter *wall-origin-x* 0) ;; World tile X where the wall map starts.
(defparameter *wall-origin-y* 0) ;; World tile Y where the wall map starts.
(defparameter *wall-tile-indices* #(107)) ;; Wall tile variants.
(defparameter *wall-seed* 2468) ;; Seed for wall tile variation.

;;; Zone/World Paths - Loaded once at startup
;;; Why: World graph and initial zone loaded once. Changing paths requires
;;; reload or zone transition to pick up new zones.
(defparameter *zone-path* nil) ;; Zone data path relative to repo (nil uses wall map).
(defparameter *zone-root* "data/zones") ;; Directory that holds zone files for the editor.
(defparameter *zone-default-width* 64) ;; Default zone width in tiles for new zones.
(defparameter *zone-default-height* 64) ;; Default zone height in tiles for new zones.
(defparameter *zone-default-chunk-size* 8) ;; Default chunk size in tiles for new zones.
(defparameter *world-graph-path* "data/world-graph.lisp") ;; World graph data path relative to repo.
(defparameter *save-filepath*
  (merge-pathnames "data/savegame.lisp"
                   (asdf:system-source-directory :mmorpg)))
;; Default save file path used by the ESC menu Save/Load.

;;; Editor Config - Layer IDs parsed at zone load
;;; Why: These identify which layers to paint on. Changing them after
;;; loading a zone won't affect the current editing target.
(defparameter *editor-tileset-paths* nil) ;; Optional list of tileset sheets to use in the editor.
(defparameter *editor-tileset-root* "../assets/Zelda-like") ;; Directory that holds editor tileset sheets.
(defparameter *editor-export-path* "data/zones/editor-zone.lisp") ;; Default export path for editor zones.
(defparameter *editor-tile-layer-id* :floor) ;; Zone layer ID used for tile painting.
(defparameter *editor-collision-layer-id* :walls) ;; Zone layer ID used for collision painting.
(defparameter *editor-object-layer-id* :objects) ;; Zone layer ID used for object painting.

;;;; ========================================================================
;;;; STATIC DATA (Evaluated at load time, effectively constants)
;;;; These are evaluated once when the file is loaded. They cannot be
;;;; meaningfully changed at runtime.
;;;; ========================================================================

;;; CLOS Class Definitions
(defclass character-class ()
  ;; Static player class data (CLOS keeps class metadata extensible).
  ((name :initarg :name :reader character-class-name)
   (max-hp :initarg :max-hp :reader character-class-max-hp)))

(defparameter *wizard-class*
  (make-instance 'character-class :name "Wizard" :max-hp 10)) ;; Default player class.

(defclass npc-archetype ()
  ;; Static NPC archetype data (durability, temperament, perception).
  ((name :initarg :name :reader npc-archetype-name)
   (description :initarg :description :initform nil :reader npc-archetype-description)
   (max-hits :initarg :max-hits :reader npc-archetype-max-hits)
   (attack-level :initarg :attack-level :initform 1 :reader npc-archetype-attack-level)
   (strength-level :initarg :strength-level :initform 1 :reader npc-archetype-strength-level)
   (defense-level :initarg :defense-level :initform 1 :reader npc-archetype-defense-level)
   (hitpoints-level :initarg :hitpoints-level :initform *npc-max-hits*
                    :reader npc-archetype-hitpoints-level)
   (combat-xp :initarg :combat-xp :initform 0 :reader npc-archetype-combat-xp)
   (loot-table-id :initarg :loot-table-id :initform nil :reader npc-archetype-loot-table-id)
   (move-speed :initarg :move-speed :initform *npc-walk-speed* :reader npc-archetype-move-speed)
   (attack-range-tiles :initarg :attack-range-tiles :initform *npc-attack-range-tiles*
                       :reader npc-archetype-attack-range-tiles)
   (attack-cooldown :initarg :attack-cooldown :initform *npc-attack-cooldown*
                    :reader npc-archetype-attack-cooldown)
   (attack-damage :initarg :attack-damage :initform *npc-attack-damage*
                  :reader npc-archetype-attack-damage)
   (home-radius-tiles :initarg :home-radius-tiles :initform *npc-home-radius-tiles*
                      :reader npc-archetype-home-radius-tiles)
   (wander-interval :initarg :wander-interval :initform *npc-wander-interval*
                    :reader npc-archetype-wander-interval)
   (respawn-seconds :initarg :respawn-seconds :initform *npc-respawn-seconds*
                    :reader npc-archetype-respawn-seconds)
   (flee-speed-mult :initarg :flee-speed-mult :initform *npc-flee-speed-mult*
                    :reader npc-archetype-flee-speed-mult)
   (animation-set-id :initarg :animation-set-id :initform :npc
                     :reader npc-archetype-animation-set-id)
   (aggro-mode :initarg :aggro-mode :initform :never :reader npc-archetype-aggro-mode)
   (retaliate :initarg :retaliate :initform nil :reader npc-archetype-retaliate)
   (flee-at-hits :initarg :flee-at-hits :initform 0 :reader npc-archetype-flee-at-hits)
   (perception-tiles :initarg :perception-tiles :initform 0.0 :reader npc-archetype-perception-tiles)))

;;; Raylib Key Codes - Evaluated once at compile/load time
;;; Why: cffi:foreign-enum-value returns integers that never change.
;;; These are effectively constants after file load.
(defparameter +key-right+ (cffi:foreign-enum-value 'raylib:keyboard-key :right)) ;; Raylib keycode for the Right Arrow key.
(defparameter +key-left+ (cffi:foreign-enum-value 'raylib:keyboard-key :left)) ;; Raylib keycode for the Left Arrow key.
(defparameter +key-down+ (cffi:foreign-enum-value 'raylib:keyboard-key :down)) ;; Raylib keycode for the Down Arrow key.
(defparameter +key-up+ (cffi:foreign-enum-value 'raylib:keyboard-key :up)) ;; Raylib keycode for the Up Arrow key.
(defparameter +key-escape+ (cffi:foreign-enum-value 'raylib:keyboard-key :escape)) ;; Raylib keycode for the Escape key.
(defparameter +key-d+ (cffi:foreign-enum-value 'raylib:keyboard-key :d)) ;; Raylib keycode for the D key.
(defparameter +key-a+ (cffi:foreign-enum-value 'raylib:keyboard-key :a)) ;; Raylib keycode for the A key.
(defparameter +key-s+ (cffi:foreign-enum-value 'raylib:keyboard-key :s)) ;; Raylib keycode for the S key.
(defparameter +key-w+ (cffi:foreign-enum-value 'raylib:keyboard-key :w)) ;; Raylib keycode for the W key.
(defparameter +key-tab+ (cffi:foreign-enum-value 'raylib:keyboard-key :tab)) ;; Raylib keycode for the Tab key.
(defparameter +key-space+ (cffi:foreign-enum-value 'raylib:keyboard-key :space)) ;; Raylib keycode for the Space key.
(defparameter +key-q+ (cffi:foreign-enum-value 'raylib:keyboard-key :q)) ;; Raylib keycode for the Q key.
(defparameter +key-e+ (cffi:foreign-enum-value 'raylib:keyboard-key :e)) ;; Raylib keycode for the E key.
(defparameter +key-t+ (cffi:foreign-enum-value 'raylib:keyboard-key :t)) ;; Raylib keycode for the T key.
(defparameter +key-z+ (cffi:foreign-enum-value 'raylib:keyboard-key :z)) ;; Raylib keycode for the Z key.
(defparameter +key-i+ (cffi:foreign-enum-value 'raylib:keyboard-key :i)) ;; Raylib keycode for the I key.
(defparameter +key-x+ (cffi:foreign-enum-value 'raylib:keyboard-key :x)) ;; Raylib keycode for the X key.
(defparameter +key-one+ (cffi:foreign-enum-value 'raylib:keyboard-key :one)) ;; Raylib keycode for the 1 key.
(defparameter +key-two+ (cffi:foreign-enum-value 'raylib:keyboard-key :two)) ;; Raylib keycode for the 2 key.
(defparameter +key-three+ (cffi:foreign-enum-value 'raylib:keyboard-key :three)) ;; Raylib keycode for the 3 key.
(defparameter +key-four+ (cffi:foreign-enum-value 'raylib:keyboard-key :four)) ;; Raylib keycode for the 4 key.
(defparameter +key-f5+ (cffi:foreign-enum-value 'raylib:keyboard-key :f5)) ;; Raylib keycode for the F5 key.
(defparameter +key-f6+ (cffi:foreign-enum-value 'raylib:keyboard-key :f6)) ;; Raylib keycode for the F6 key.
(defparameter +key-f7+ (cffi:foreign-enum-value 'raylib:keyboard-key :f7)) ;; Raylib keycode for the F7 key.
(defparameter +key-f8+ (cffi:foreign-enum-value 'raylib:keyboard-key :f8)) ;; Raylib keycode for the F8 key.
(defparameter +key-f9+ (cffi:foreign-enum-value 'raylib:keyboard-key :f9)) ;; Raylib keycode for the F9 key.
(defparameter +key-f10+ (cffi:foreign-enum-value 'raylib:keyboard-key :f10)) ;; Raylib keycode for the F10 key.
(defparameter +key-f11+ (cffi:foreign-enum-value 'raylib:keyboard-key :f11)) ;; Raylib keycode for the F11 key.
(defparameter +key-f12+ (cffi:foreign-enum-value 'raylib:keyboard-key :f12)) ;; Raylib keycode for the F12 key.
(defparameter +key-left-shift+ (cffi:foreign-enum-value 'raylib:keyboard-key :left-shift)) ;; Raylib keycode for the Left Shift key.
(defparameter +key-right-shift+ (cffi:foreign-enum-value 'raylib:keyboard-key :right-shift)) ;; Raylib keycode for the Right Shift key.
(defparameter +key-enter+ (cffi:foreign-enum-value 'raylib:keyboard-key :enter)) ;; Raylib keycode for the Enter key.
(defparameter +key-backspace+ (cffi:foreign-enum-value 'raylib:keyboard-key :backspace)) ;; Raylib keycode for the Backspace key.
(defparameter +mouse-left+ (cffi:foreign-enum-value 'raylib:mouse-button :left)) ;; Raylib mouse button code for left click.
(defparameter +mouse-right+ (cffi:foreign-enum-value 'raylib:mouse-button :right)) ;; Raylib mouse button code for right click.
(defparameter +mouse-middle+ (cffi:foreign-enum-value 'raylib:mouse-button :middle)) ;; Raylib mouse button code for middle click.
