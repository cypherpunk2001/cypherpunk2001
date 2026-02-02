;; NOTE: If you change behavior here, update docs/config.md :)
(in-package #:mmorpg)

;;;; ========================================================================
;;;; ENVIRONMENT-DRIVEN OPTIMIZATION POLICY
;;;; Set MMORPG_ENV=prod for production builds, dev (default) for development.
;;;; This MUST come before any other definitions to affect all compilation.
;;;; ========================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *mmorpg-environment*
    (or (uiop:getenv "MMORPG_ENV") "dev")
    "Build environment: 'dev' for development (safe, debuggable) or 'prod' for production (fast).")

  (if (string= *mmorpg-environment* "prod")
      ;; Production: maximize speed, maintain basic safety
      ;; Never use global (safety 0) - only localize in proven hot functions
      (declaim (optimize (speed 3) (safety 1) (debug 0) (compilation-speed 0)))
      ;; Development: balance speed with debugging, maximize correctness
      (declaim (optimize (speed 2) (safety 2) (debug 2) (compilation-speed 2)))))

;;;; ========================================================================
;;;; SHARED OPTIONS - Restart Required
;;;; Used by both client and server. Restart required for changes.
;;;; ========================================================================

;;; Zone/World Paths - Loaded once at startup
(defparameter *zone-path* nil
  "Zone data path relative to repo (nil uses wall map).")
(defparameter *zone-root* "data/zones"
  "Directory that holds zone files for the editor.")
(defparameter *zone-default-width* 64
  "Default zone width in tiles for new zones.")
(defparameter *zone-default-height* 64
  "Default zone height in tiles for new zones.")
(defparameter *zone-default-chunk-size* 8
  "Default chunk size in tiles for new zones.")
(defparameter *world-graph-path* "data/world-graph.lisp"
  "World graph data path relative to repo.")
(defparameter *known-zone-ids* nil
  "Set of known zone IDs from world-graph. Used by validation to quarantine unknown zones.
   Populated by load-world-graph, nil means zone validation is skipped.")
(defparameter *starting-zone-id* :zone-1
  "Zone where new players spawn. Must exist in world-graph.")
(defparameter *save-filepath*
  (merge-pathnames "data/savegame.lisp"
                   (asdf:system-source-directory :mmorpg))
  "Default save file path used by the ESC menu Save/Load.")

;;; Tileset/Map Layout - Parsed once when loading zones
(defparameter *tile-size* 16
  "Source tile size in the atlas, in pixels.")
(defparameter *tile-scale* 4.0
  "Scale factor for drawing tiles to the screen.")
(defparameter *tileset-columns* 40
  "Number of columns in the atlas grid.")
(defparameter *floor-tile-index* 0
  "Which atlas tile index to use for the floor fill (0 disables fill).")
(defparameter *wall-map-width* 40
  "Width of the test wall map in tiles.")
(defparameter *wall-map-height* 24
  "Height of the test wall map in tiles.")
(defparameter *wall-origin-x* 0
  "World tile X where the wall map starts.")
(defparameter *wall-origin-y* 0
  "World tile Y where the wall map starts.")
(defparameter *wall-tile-indices* #(107)
  "Wall tile variants.")
(defparameter *wall-seed* 2468
  "Seed for wall tile variation.")

;;;; ========================================================================
;;;; SHARED OPTIONS - Immediate (SLIME tunable)
;;;; Used by both client and server. Changes take effect immediately.
;;;; Server is authoritative; client uses for prediction/display.
;;;; ========================================================================

;;; Debug Flags - Toggle debugging output
(defparameter *verbose* nil
  "General verbose mode: logs network events, state changes, and diagnostic info.")
(defparameter *verbose-coordinates* nil
  "Logs entity positions and collider info per frame (very noisy).")
(defparameter *verbose-logs* nil
  "DEPRECATED: Use *verbose-coordinates* instead.")

;;; Player Movement - Server authoritative, client uses for prediction
(defparameter *player-speed* 222.0
  "Base movement speed in pixels per second.")
(defparameter *run-speed-mult* 2.0
  "Movement speed multiplier while running.")
(defparameter *run-stamina-max* 10.0
  "Seconds of run stamina when full.")
(defparameter *target-epsilon* 6.0
  "Stop distance for click-to-move.")

;;; Collision - Used by both for movement calculations
(defparameter *player-collision-scale* 2.0
  "Collision box size relative to one tile.")
(defparameter *collision-edge-epsilon* 0.01
  "Avoid counting exact edge contact as a blocked tile.")

;;;; ========================================================================
;;;; SEAMLESS ZONE TRANSITION CONFIG
;;;; Parameters controlling zone transition smoothness, preloading, and
;;;; cross-zone visibility. See PLAN_loading_zones.md for spatial model.
;;;; ========================================================================

;;; Transition Cooldown (Step 1)
(defparameter *zone-transition-cooldown-seconds* 0.5
  "Post-transition suppression window in seconds. Prevents rapid re-transitions.")

;;; Hysteresis Band (Step 2) — distances in tiles inward from edge
(defparameter *zone-hysteresis-in* 2.0
  "Arm line distance from zone edge in tiles. Crossing toward edge sets pending.")
(defparameter *zone-hysteresis-out* 3.0
  "Cancel line distance from zone edge in tiles. Retreating past this clears pending.
   Must be > *zone-hysteresis-in*.")

;;; Commit Margin (UNUSED — legacy parameter)
(defparameter *zone-commit-margin-tiles* 0.01
  "UNUSED — legacy parameter. Commit detection now uses world-crossing-edge
   with attempted position (strict inequality on collision bounds).
   Retained for rollback safety; will be removed.")

;;; Directional Gating (Step 3)
(defparameter *zone-direction-threshold* 0.3
  "Min normalized dot product between movement intent and edge normal.
   Range 0.0-1.0. Lower = more permissive. 0.3 allows ~72° cone.")

;;; Client Zone Cache (Step 4)
(defparameter *client-zone-cache-capacity* 9
  "LRU cache size for loaded zone data. 9 = current + 8 neighbors (~900KB).")

;;; Proximity Preloading (Step 5)
(defparameter *zone-preload-radius* 10.0
  "Distance from edge in tiles to begin preloading adjacent zone. Must be >= *zone-hysteresis-in*.")

;;; Cross-Zone Visibility (Step 7)
(defparameter *zone-edge-visibility-tiles* 10.0
  "Width of cross-zone entity/terrain strip in tiles from edge into adjacent zone.")

;;; Urgent Preload (ADDENDUM 3)
(defparameter *zone-urgent-preload-tiles* 2.0
  "When player is within this many tiles of the commit line, pop ALL remaining
   preload entries per frame (instead of 1) to guarantee the target zone is cached.")

;;; Evaluation Metrics (Step 12)
(defparameter *verbose-zone-transitions* nil
  "Enable zone transition diagnostics (cooldown, hysteresis, directional gating).
   Set via MMORPG_VERBOSE_ZONES=1 environment variable.")

;;; Runtime config guard — called at server/client startup
(defun validate-zone-config ()
  "Assert zone config invariants at startup. Signals error on violation."
  (unless (> *zone-hysteresis-out* *zone-hysteresis-in*)
    (error "Zone config: *zone-hysteresis-out* (~a) must be > *zone-hysteresis-in* (~a)"
           *zone-hysteresis-out* *zone-hysteresis-in*))
  (unless (>= *zone-preload-radius* *zone-hysteresis-in*)
    (error "Zone config: *zone-preload-radius* (~a) must be >= *zone-hysteresis-in* (~a)"
           *zone-preload-radius* *zone-hysteresis-in*)))

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
  (make-instance 'character-class :name "Wizard" :max-hp 10)
  "Default player class.")

(defclass npc-archetype ()
  ;; Static NPC archetype data (durability, temperament, perception).
  ((name :initarg :name :reader npc-archetype-name)
   (description :initarg :description :initform nil :reader npc-archetype-description)
   (max-hits :initarg :max-hits :reader npc-archetype-max-hits)
   (attack-level :initarg :attack-level :initform 1 :reader npc-archetype-attack-level)
   (strength-level :initarg :strength-level :initform 1 :reader npc-archetype-strength-level)
   (defense-level :initarg :defense-level :initform 1 :reader npc-archetype-defense-level)
   (hitpoints-level :initarg :hitpoints-level :initform 3 ; default from *npc-max-hits*
                    :reader npc-archetype-hitpoints-level)
   (combat-xp :initarg :combat-xp :initform 0 :reader npc-archetype-combat-xp)
   (loot-table-id :initarg :loot-table-id :initform nil :reader npc-archetype-loot-table-id)
   (move-speed :initarg :move-speed :initform 120.0 :reader npc-archetype-move-speed)
   (attack-range-tiles :initarg :attack-range-tiles :initform 0.9
                       :reader npc-archetype-attack-range-tiles)
   (attack-cooldown :initarg :attack-cooldown :initform 0.9
                    :reader npc-archetype-attack-cooldown)
   (attack-damage :initarg :attack-damage :initform 1
                  :reader npc-archetype-attack-damage)
   (home-radius-tiles :initarg :home-radius-tiles :initform 2.0
                      :reader npc-archetype-home-radius-tiles)
   (wander-interval :initarg :wander-interval :initform 1.1
                    :reader npc-archetype-wander-interval)
   (respawn-seconds :initarg :respawn-seconds :initform 5.0
                    :reader npc-archetype-respawn-seconds)
   (flee-speed-mult :initarg :flee-speed-mult :initform 1.4
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
(defparameter +key-right+ (cffi:foreign-enum-value 'raylib:keyboard-key :right)
  "Raylib keycode for the Right Arrow key.")
(defparameter +key-left+ (cffi:foreign-enum-value 'raylib:keyboard-key :left)
  "Raylib keycode for the Left Arrow key.")
(defparameter +key-down+ (cffi:foreign-enum-value 'raylib:keyboard-key :down)
  "Raylib keycode for the Down Arrow key.")
(defparameter +key-up+ (cffi:foreign-enum-value 'raylib:keyboard-key :up)
  "Raylib keycode for the Up Arrow key.")
(defparameter +key-escape+ (cffi:foreign-enum-value 'raylib:keyboard-key :escape)
  "Raylib keycode for the Escape key.")
(defparameter +key-d+ (cffi:foreign-enum-value 'raylib:keyboard-key :d)
  "Raylib keycode for the D key.")
(defparameter +key-a+ (cffi:foreign-enum-value 'raylib:keyboard-key :a)
  "Raylib keycode for the A key.")
(defparameter +key-s+ (cffi:foreign-enum-value 'raylib:keyboard-key :s)
  "Raylib keycode for the S key.")
(defparameter +key-w+ (cffi:foreign-enum-value 'raylib:keyboard-key :w)
  "Raylib keycode for the W key.")
(defparameter +key-tab+ (cffi:foreign-enum-value 'raylib:keyboard-key :tab)
  "Raylib keycode for the Tab key.")
(defparameter +key-space+ (cffi:foreign-enum-value 'raylib:keyboard-key :space)
  "Raylib keycode for the Space key.")
(defparameter +key-q+ (cffi:foreign-enum-value 'raylib:keyboard-key :q)
  "Raylib keycode for the Q key.")
(defparameter +key-e+ (cffi:foreign-enum-value 'raylib:keyboard-key :e)
  "Raylib keycode for the E key.")
(defparameter +key-t+ (cffi:foreign-enum-value 'raylib:keyboard-key :t)
  "Raylib keycode for the T key.")
(defparameter +key-z+ (cffi:foreign-enum-value 'raylib:keyboard-key :z)
  "Raylib keycode for the Z key.")
(defparameter +key-i+ (cffi:foreign-enum-value 'raylib:keyboard-key :i)
  "Raylib keycode for the I key.")
(defparameter +key-x+ (cffi:foreign-enum-value 'raylib:keyboard-key :x)
  "Raylib keycode for the X key.")
(defparameter +key-one+ (cffi:foreign-enum-value 'raylib:keyboard-key :one)
  "Raylib keycode for the 1 key.")
(defparameter +key-two+ (cffi:foreign-enum-value 'raylib:keyboard-key :two)
  "Raylib keycode for the 2 key.")
(defparameter +key-three+ (cffi:foreign-enum-value 'raylib:keyboard-key :three)
  "Raylib keycode for the 3 key.")
(defparameter +key-four+ (cffi:foreign-enum-value 'raylib:keyboard-key :four)
  "Raylib keycode for the 4 key.")
(defparameter +key-f5+ (cffi:foreign-enum-value 'raylib:keyboard-key :f5)
  "Raylib keycode for the F5 key.")
(defparameter +key-f6+ (cffi:foreign-enum-value 'raylib:keyboard-key :f6)
  "Raylib keycode for the F6 key.")
(defparameter +key-f7+ (cffi:foreign-enum-value 'raylib:keyboard-key :f7)
  "Raylib keycode for the F7 key.")
(defparameter +key-f8+ (cffi:foreign-enum-value 'raylib:keyboard-key :f8)
  "Raylib keycode for the F8 key.")
(defparameter +key-f9+ (cffi:foreign-enum-value 'raylib:keyboard-key :f9)
  "Raylib keycode for the F9 key.")
(defparameter +key-f10+ (cffi:foreign-enum-value 'raylib:keyboard-key :f10)
  "Raylib keycode for the F10 key.")
(defparameter +key-f11+ (cffi:foreign-enum-value 'raylib:keyboard-key :f11)
  "Raylib keycode for the F11 key.")
(defparameter +key-f12+ (cffi:foreign-enum-value 'raylib:keyboard-key :f12)
  "Raylib keycode for the F12 key.")
(defparameter +key-left-shift+ (cffi:foreign-enum-value 'raylib:keyboard-key :left-shift)
  "Raylib keycode for the Left Shift key.")
(defparameter +key-right-shift+ (cffi:foreign-enum-value 'raylib:keyboard-key :right-shift)
  "Raylib keycode for the Right Shift key.")
(defparameter +key-enter+ (cffi:foreign-enum-value 'raylib:keyboard-key :enter)
  "Raylib keycode for the Enter key.")
(defparameter +key-backspace+ (cffi:foreign-enum-value 'raylib:keyboard-key :backspace)
  "Raylib keycode for the Backspace key.")
(defparameter +mouse-left+ (cffi:foreign-enum-value 'raylib:mouse-button :left)
  "Raylib mouse button code for left click.")
(defparameter +mouse-right+ (cffi:foreign-enum-value 'raylib:mouse-button :right)
  "Raylib mouse button code for right click.")
(defparameter +mouse-middle+ (cffi:foreign-enum-value 'raylib:mouse-button :middle)
  "Raylib mouse button code for middle click.")

;;;; ========================================================================
;;;; COMPACT SERIALIZATION - Network Snapshot Optimization
;;;; Used to minimize snapshot size for scalability (see docs/net.md 4-Prong)
;;;; ========================================================================

;;; Animation State Codes - Map keywords to small integers
;;; NOTE: Game uses :walk/:attack (not :walking/:attacking) - see utils.lisp:player-state
(defparameter *anim-state-to-code*
  '((:idle . 0) (:walk . 1) (:attack . 2))
  "Animation state keyword to integer code mapping for compact serialization.")

(defparameter *code-to-anim-state*
  '((0 . :idle) (1 . :walk) (2 . :attack))
  "Integer code to animation state keyword mapping for deserialization.")

;;; Facing Direction Codes - Map keywords to small integers
;;; NOTE: Game uses :side (not :left/:right) with facing-sign for left/right flip
(defparameter *facing-to-code*
  '((:up . 0) (:down . 1) (:side . 2))
  "Facing direction keyword to integer code mapping for compact serialization.")

(defparameter *code-to-facing*
  '((0 . :up) (1 . :down) (2 . :side))
  "Integer code to facing direction keyword mapping for deserialization.")

;;; Quantization Parameters
(defparameter *coord-scale* 10
  "Scale factor for coordinate quantization. 10 = 0.1 pixel precision.")

(defparameter *timer-scale* 100
  "Scale factor for timer quantization. 100 = 0.01 second precision.")
