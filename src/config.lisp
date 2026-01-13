;; NOTE: If you change behavior here, update docs/config.md :)
(in-package #:mmorpg)

(defparameter *verbose-logs* nil) ;; When true, logs player position and collider info per frame.
(defparameter *debug-collision-overlay* nil) ;; Draws debug grid and collision overlays.
(defparameter *debug-npc-logs* nil) ;; Logs NPC AI/combat events and enables AI debug text overlay.

(defparameter *window-width* 1280) ;; Window width in pixels.
(defparameter *window-height* 720) ;; Window height in pixels.
(defparameter *player-speed* 222.0) ;; Base movement speed in pixels per second.
(defparameter *auto-walk-enabled* t) ;; When true, WASD toggles auto-walk direction.
(defparameter *camera-zoom-default* 1.0) ;; Default camera zoom level.
(defparameter *camera-zoom-min* 0.5) ;; Minimum zoom level.
(defparameter *camera-zoom-max* 3.0) ;; Maximum zoom level.
(defparameter *camera-zoom-step* 0.1) ;; Zoom step per mouse wheel tick.
(defparameter *run-speed-mult* 2.0) ;; Movement speed multiplier while running.
(defparameter *run-stamina-max* 10.0) ;; Seconds of run stamina when full.
(defparameter *mouse-hold-repeat-seconds* 0.25) ;; Repeat rate for mouse-held updates.
(defparameter *editor-move-speed* 360.0) ;; Movement speed for editor camera.

(defparameter *player-sprite-dir* "../assets/1 Characters/3") ;; Directory that holds player sprite sheets.
(defparameter *npc-sprite-dir* "../assets/3 Dungeon Enemies/1") ;; Directory that holds NPC sprite sheets.
(defparameter *sprite-frame-width* 32.0) ;; Width of a single sprite frame in pixels.
(defparameter *sprite-frame-height* 32.0) ;; Height of a single sprite frame in pixels.
(defparameter *sprite-scale* 4.0) ;; Scale factor applied when drawing sprites.
(defparameter *player-animation-set-id* :player) ;; Animation set ID used for the player sprite set.

(defparameter *tileset-path* "../assets/2 Dungeon Tileset/1 Tiles/Tileset.png") ;; Atlas image used for floor tiles.
(defparameter *zone-path* nil) ;; Zone data path relative to repo (nil uses wall map).
(defparameter *editor-object-root* "../assets/2 Dungeon Tileset/2 Objects") ;; Root directory for editor object palette.
(defparameter *editor-export-path* "data/zones/editor-zone.lisp") ;; Default export path for editor zones.
(defparameter *editor-tile-layer-id* :floor) ;; Zone layer ID used for tile painting.
(defparameter *editor-collision-layer-id* :walls) ;; Zone layer ID used for collision painting.
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
(defparameter *tile-size* 16) ;; Source tile size in the atlas, in pixels.
(defparameter *tile-scale* 4.0) ;; Scale factor for drawing tiles to the screen.
(defparameter *tileset-columns* 19) ;; Number of columns in the atlas grid.
(defparameter *floor-tile-index* 40) ;; Which atlas tile index to use for the floor fill.
(defparameter *wall-map-width* 40) ;; Width of the test wall map in tiles.
(defparameter *wall-map-height* 24) ;; Height of the test wall map in tiles.
(defparameter *wall-origin-x* 0) ;; World tile X where the wall map starts.
(defparameter *wall-origin-y* 0) ;; World tile Y where the wall map starts.
(defparameter *wall-tile-indices* #(107)) ;; Wall tile variants.
(defparameter *wall-seed* 2468) ;; Seed for wall tile variation.
(defparameter *player-collision-scale* 2.0) ;; Collision box size relative to one tile.
(defparameter *target-epsilon* 6.0) ;; Stop distance for click-to-move.
(defparameter *npc-collision-scale* 2.0) ;; Collision box size relative to one tile.
(defparameter *collision-edge-epsilon* 0.01) ;; Avoid counting exact edge contact as a blocked tile.
(defparameter *npc-max-hits* 3) ;; Hits required to defeat the NPC.
(defparameter *npc-walk-speed* 120.0) ;; Base NPC movement speed in pixels per second.
(defparameter *npc-flee-speed-mult* 1.4) ;; Speed multiplier while fleeing.
(defparameter *npc-attack-range-tiles* 0.9) ;; NPC melee range in tiles.
(defparameter *npc-attack-cooldown* 0.9) ;; Seconds between NPC attacks.
(defparameter *npc-attack-damage* 1) ;; Damage per NPC hit.
(defparameter *npc-home-radius-tiles* 2.0) ;; Roam radius around spawn in tiles.
(defparameter *npc-wander-interval* 1.1) ;; Seconds between wander target changes.
(defparameter *npc-wander-arrive-distance* 6.0) ;; Pixels to consider wander target reached.
(defparameter *npc-count* 3) ;; Default number of NPCs spawned in the world.
(defparameter *npc-spawn-columns* 3) ;; Column count when placing NPCs in a grid.
(defparameter *npc-spawn-gap-tiles* 2.0) ;; Spacing between NPC spawns in tiles.
(defparameter *npc-default-archetype-id* :rat) ;; Default NPC archetype ID to spawn.
(defparameter *npc-spawn-ids* nil) ;; Optional list of archetype IDs to cycle when spawning.
(defparameter *attack-hitbox-scale* 1.0) ;; Attack hitbox size relative to one tile.
(defparameter *blood-sprite-dir* "../assets/1 Characters/Other") ;; Directory that holds blood effect sprites.
(defparameter *blood-frame-count* 4) ;; Frames in each blood animation row.
(defparameter *blood-frame-time* 0.08) ;; Seconds per blood frame.
(defparameter *health-bar-height* 6) ;; Height of the health bar in pixels.
(defparameter *health-bar-offset* 10) ;; Vertical offset above the sprite center.
(defparameter *health-bar-back-color* (raylib:make-color :r 8 :g 8 :b 8 :a 200)) ;; Health bar background color.
(defparameter *health-bar-fill-color* (raylib:make-color :r 70 :g 200 :b 80 :a 220)) ;; Health bar fill color.
(defparameter *health-bar-border-color* (raylib:make-color :r 220 :g 220 :b 220 :a 220)) ;; Health bar outline color.
(defparameter *debug-npc-text-size* 12) ;; Debug text size for NPC AI overlay.
(defparameter *debug-npc-text-offset* 18) ;; Extra vertical offset for NPC debug text.
(defparameter *debug-npc-text-color* (raylib:make-color :r 255 :g 240 :b 160 :a 230)) ;; NPC AI debug text color.
(defparameter *editor-selection-color* (raylib:make-color :r 255 :g 215 :b 0 :a 200)) ;; Editor selection rectangle color.
(defparameter *editor-cursor-color* (raylib:make-color :r 80 :g 220 :b 255 :a 200)) ;; Editor cursor highlight color.

(defclass character-class ()
  ;; Static player class data (CLOS keeps class metadata extensible).
  ((name :initarg :name :reader character-class-name)
   (max-hp :initarg :max-hp :reader character-class-max-hp)))

(defparameter *wizard-class*
  (make-instance 'character-class :name "Wizard" :max-hp 10)) ;; Default player class.

(defclass npc-archetype ()
  ;; Static NPC archetype data (durability, temperament, perception).
  ((name :initarg :name :reader npc-archetype-name)
   (max-hits :initarg :max-hits :reader npc-archetype-max-hits)
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
   (flee-speed-mult :initarg :flee-speed-mult :initform *npc-flee-speed-mult*
                    :reader npc-archetype-flee-speed-mult)
   (animation-set-id :initarg :animation-set-id :initform :npc
                     :reader npc-archetype-animation-set-id)
   (aggro-mode :initarg :aggro-mode :initform :never :reader npc-archetype-aggro-mode)
   (retaliate :initarg :retaliate :initform nil :reader npc-archetype-retaliate)
   (flee-at-hits :initarg :flee-at-hits :initform 0 :reader npc-archetype-flee-at-hits)
   (perception-tiles :initarg :perception-tiles :initform 0.0 :reader npc-archetype-perception-tiles)))

(defparameter *idle-frame-count* 4) ;; Frames in each idle animation row.
(defparameter *walk-frame-count* 6) ;; Frames in each walk animation row.
(defparameter *attack-frame-count* 4) ;; Frames in each attack animation row.
(defparameter *idle-frame-time* 0.25) ;; Seconds per idle frame.
(defparameter *walk-frame-time* 0.12) ;; Seconds per walk frame.
(defparameter *attack-frame-time* 0.1) ;; Seconds per attack frame.

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
(defparameter +key-z+ (cffi:foreign-enum-value 'raylib:keyboard-key :z)) ;; Raylib keycode for the Z key.
(defparameter +key-x+ (cffi:foreign-enum-value 'raylib:keyboard-key :x)) ;; Raylib keycode for the X key.
(defparameter +key-c+ (cffi:foreign-enum-value 'raylib:keyboard-key :c)) ;; Raylib keycode for the C key.
(defparameter +key-b+ (cffi:foreign-enum-value 'raylib:keyboard-key :b)) ;; Raylib keycode for the B key.
(defparameter +key-n+ (cffi:foreign-enum-value 'raylib:keyboard-key :n)) ;; Raylib keycode for the N key.
(defparameter +key-one+ (cffi:foreign-enum-value 'raylib:keyboard-key :one)) ;; Raylib keycode for the 1 key.
(defparameter +key-two+ (cffi:foreign-enum-value 'raylib:keyboard-key :two)) ;; Raylib keycode for the 2 key.
(defparameter +key-three+ (cffi:foreign-enum-value 'raylib:keyboard-key :three)) ;; Raylib keycode for the 3 key.
(defparameter +key-f5+ (cffi:foreign-enum-value 'raylib:keyboard-key :f5)) ;; Raylib keycode for the F5 key.
(defparameter +key-left-shift+ (cffi:foreign-enum-value 'raylib:keyboard-key :left-shift)) ;; Raylib keycode for the Left Shift key.
(defparameter +key-right-shift+ (cffi:foreign-enum-value 'raylib:keyboard-key :right-shift)) ;; Raylib keycode for the Right Shift key.
(defparameter +mouse-left+ (cffi:foreign-enum-value 'raylib:mouse-button :left)) ;; Raylib mouse button code for left click.
(defparameter +mouse-right+ (cffi:foreign-enum-value 'raylib:mouse-button :right)) ;; Raylib mouse button code for right click.
(defparameter +mouse-middle+ (cffi:foreign-enum-value 'raylib:mouse-button :middle)) ;; Raylib mouse button code for middle click.
