;; NOTE: If you change behavior here, update docs/config-client.md :)
(in-package #:mmorpg)

;;;; ========================================================================
;;;; CLIENT OPTIONS - Restart Required
;;;; Client must be restarted for these changes to take effect.
;;;; These are read once at client initialization (window, assets).
;;;; ========================================================================

;;; Window Size - Read once at raylib:init-window
(defparameter *window-width* 1280
  "Window width in pixels. Requires restart to change.")
(defparameter *window-height* 720
  "Window height in pixels. Requires restart to change.")

;;; Window Resize - Enable dynamic screen dimensions
;;; Raylib FLAG_WINDOW_RESIZABLE = 4 (from raylib.h)
(defconstant +flag-window-resizable+ 4
  "Raylib config flag to enable window resizing.")

(defparameter *window-resize-enabled* nil
  "When T, creates resizable window and uses dynamic screen dimensions.
   When NIL (default), uses fixed *window-width*/*window-height*.")

;;; Frame Rate Target - Read once at startup, but CAN be changed at runtime
;;; NOTE: This is CLIENT-SIDE ONLY. Does not affect server tick rate or other players.
;;; Server tick rate is controlled by *sim-tick-seconds* in config-server.lisp.
;;; Common values: 30 (battery saver), 60 (standard), 0 (unlimited/vsync)
(defparameter *client-target-fps* 60
  "Target frames per second for client rendering. 0 = unlimited (vsync).
   This is purely visual - does not affect server simulation or network rate.
   Lower values save CPU/GPU but feel less smooth.")

;;; Asset Paths - Textures/audio loaded once at startup
;; FIXME: Asset paths are currently hardcoded. Future: move to data-driven asset manifest system.
(defparameter *player-sprite-dir* "../assets/1 Characters/3"
  "Directory that holds player sprite sheets.")
(defparameter *npc-sprite-dir* "../assets/3 Dungeon Enemies/1"
  "Directory that holds NPC sprite sheets.")
(defparameter *blood-sprite-dir* "../assets/1 Characters/Other"
  "Directory that holds blood effect sprites.")
(defparameter *tileset-path* "../assets/Zelda-like/Overworld.png"
  "Atlas image used for floor tiles.")
(defparameter *soundtrack-dir* "../assets/6 Soundtrack"
  "Directory that holds soundtrack files.")
(defparameter *soundtrack-tracks*
  (vector
   (format nil "~a/Juhani Junkala [Retro Game Music Pack] Title Screen.wav" *soundtrack-dir*)
   (format nil "~a/Juhani Junkala [Retro Game Music Pack] Level 1.wav" *soundtrack-dir*)
   (format nil "~a/Juhani Junkala [Retro Game Music Pack] Level 2.wav" *soundtrack-dir*)
   (format nil "~a/Juhani Junkala [Retro Game Music Pack] Level 3.wav" *soundtrack-dir*)
   (format nil "~a/Juhani Junkala [Retro Game Music Pack] Ending.wav" *soundtrack-dir*))
  "Vector of soundtrack file paths.")
(defparameter *soundtrack-display-names*
  (vector
   "Title Screen"
   "Level 1"
   "Level 2"
   "Level 3"
   "Ending")
  "Vector of display names for the soundtrack.")

;;; Sprite Frame Sizes - Used when parsing sprite sheets at load time
(defparameter *sprite-frame-width* 32.0
  "Width of a single sprite frame in pixels.")
(defparameter *sprite-frame-height* 32.0
  "Height of a single sprite frame in pixels.")
(defparameter *sprite-scale* 4.0
  "Scale factor applied when drawing sprites.")
(defparameter *player-animation-set-id* :player
  "Animation set ID used for the player sprite set.")

;;; Inventory UI Layout - Read when building inventory UI
(defparameter *inventory-grid-columns* 5
  "Inventory grid columns.")
(defparameter *inventory-slot-gap* 8
  "Inventory slot gap in pixels.")

;;; Editor Setup - Paths parsed at zone load
(defparameter *editor-tileset-paths* nil
  "Optional list of tileset sheets to use in the editor.")
(defparameter *editor-tileset-root* "../assets/Zelda-like"
  "Directory that holds editor tileset sheets.")
(defparameter *editor-export-path* "data/zones/editor-zone.lisp"
  "Default export path for editor zones.")
(defparameter *editor-tile-layer-id* :floor
  "Zone layer ID used for tile painting.")
(defparameter *editor-collision-layer-id* :walls
  "Zone layer ID used for collision painting.")
(defparameter *editor-object-layer-id* :objects
  "Zone layer ID used for object painting.")

;;; Render Cache - Chunk size (restart required to change)
(defparameter *render-chunk-size* 16
  "Tiles per render chunk side (16x16 = 256 tiles per texture).
   Requires restart to change. Balance: larger = fewer textures but more VRAM per texture.")

;;;; ========================================================================
;;;; CLIENT OPTIONS - Immediate (SLIME tunable)
;;;; Changes take effect immediately. Modify via SLIME: (setf *var* value)
;;;; These are read every frame during client rendering/input.
;;;; ========================================================================

;;; Debug Flags - Toggle debugging output and overlays
(defparameter *debug-collision-overlay* nil
  "Draws debug grid and collision overlays.")
(defparameter *debug-npc-logs* nil
  "Logs NPC AI/combat events and enables AI debug text overlay.")

;;; Feature Flags - Toggle gameplay features
(defparameter *auto-walk-enabled* t
  "When true, WASD toggles auto-walk direction.")
(defparameter *editor-start-enabled* nil
  "When true, editor mode starts enabled.")

;;; Interpolation & Prediction - Read during client loop
(defparameter *interpolation-delay-seconds* 0.1
  "Render delay for interpolation. Higher = smoother, more perceived lag.")
(defparameter *client-prediction-enabled* nil
  "Enable client-side prediction for local player. Toggle via SLIME for testing.")
(defparameter *prediction-error-threshold* 5.0
  "Max prediction error in pixels before correction.")

;;; Rendering - Read during draw
;;; NOTE: Use toggle-tile-point-filter or toggle-render-cache-enabled (ESC menu or SLIME)
;;; to change these values - they clear render caches. Direct setf leaves stale caches.
(defparameter *tile-point-filter* t
  "Use point (nearest-neighbor) filtering for tiles. Reduces seam artifacts but looks pixelated.
   Use toggle-tile-point-filter to change - it clears render caches for new filter to apply.")

;;; Render Chunk Cache (Phase 1 - Zoom-Out Performance Optimization)
;;; Pre-render static tile chunks to reduce draw calls at high zoom-out levels.
(defparameter *render-cache-enabled* t
  "Enable/disable chunk render caching. Set NIL to use original per-tile rendering.
   Use toggle-render-cache-enabled (ESC menu) to change - it clears render caches.")

(defparameter *render-cache-max-chunks* 64
  "Maximum cached chunk textures before LRU eviction. Prevents unbounded VRAM growth.")

(defparameter *entity-render-max-distance* nil
  "Maximum distance from player to render NPCs. NIL = unlimited (default).
   Set to ~3000.0 for very large zones with many NPCs to reduce draw calls.
   Distance is measured in world pixels from player center to NPC center.
   Note: Only affects NPCs; other players always render if in viewport.")

;;; Camera - Read every frame for view calculations
(defparameter *camera-zoom-default* 1.0
  "Default camera zoom level.")
(defparameter *camera-zoom-min* 0.5
  "Minimum zoom level.")
(defparameter *camera-zoom-max* 3.0
  "Maximum zoom level.")
(defparameter *camera-zoom-step* 0.1
  "Zoom step per mouse wheel tick.")

;;; Click Marker - Read when drawing/updating markers
(defparameter *click-marker-duration* 0.6
  "Seconds a click marker stays visible.")
(defparameter *click-marker-size-scale* 0.35
  "Marker size as a fraction of a tile.")
(defparameter *click-marker-thickness* 5
  "Marker line thickness in pixels.")
(defparameter *click-marker-walk-color* (raylib:make-color :r 240 :g 210 :b 60 :a 240)
  "Marker color for walk targets.")
(defparameter *click-marker-attack-color* (raylib:make-color :r 230 :g 70 :b 70 :a 240)
  "Marker color for attack targets.")

;;; HUD & UI Timing - Read during UI rendering
(defparameter *hud-log-line-seconds* 30.0
  "Seconds a HUD log line stays visible.")
(defparameter *hud-log-fade-seconds* 0.4
  "Seconds to fade out HUD log lines.")
(defparameter *mouse-hold-repeat-seconds* 0.25
  "Repeat rate for mouse-held updates.")
(defparameter *chat-max-length* 180
  "Maximum characters in a chat message.")
(defparameter *zone-loading-seconds* 0.35
  "Seconds to show the zone loading overlay after transitions.")

;;; Minimap - Read when drawing minimap
(defparameter *minimap-width* 220
  "Minimap width in pixels.")
(defparameter *minimap-height* 220
  "Minimap height in pixels.")
(defparameter *minimap-padding* 12
  "Padding from screen edges for minimap placement.")
(defparameter *minimap-point-size* 4
  "Size of player/NPC markers on the minimap.")
(defparameter *minimap-preview-edge-tiles* 1.5
  "Tiles from an exit edge to show adjacent zone spawn previews.")
(defparameter *minimap-npc-view-radius* 2000.0
  "Maximum distance in world pixels to show NPCs on minimap. NPCs beyond this are culled.")

;;; Animation Timing - Read every animation frame
(defparameter *idle-frame-count* 4
  "Frames in each idle animation row.")
(defparameter *walk-frame-count* 6
  "Frames in each walk animation row.")
(defparameter *attack-frame-count* 4
  "Frames in each attack animation row.")
(defparameter *idle-frame-time* 0.25
  "Seconds per idle frame.")
(defparameter *walk-frame-time* 0.12
  "Seconds per walk frame.")
(defparameter *attack-frame-time* 0.1
  "Seconds per attack frame.")
(defparameter *blood-frame-count* 4
  "Frames in each blood animation row.")
(defparameter *blood-frame-time* 0.08
  "Seconds per blood frame.")

;;; Health Bar - Read when drawing health bars
(defparameter *health-bar-height* 6
  "Height of the health bar in pixels.")
(defparameter *health-bar-offset* 10
  "Vertical offset above the sprite center.")
(defparameter *health-bar-back-color* (raylib:make-color :r 8 :g 8 :b 8 :a 200)
  "Health bar background color.")
(defparameter *health-bar-fill-color* (raylib:make-color :r 70 :g 200 :b 80 :a 220)
  "Health bar fill color.")
(defparameter *health-bar-border-color* (raylib:make-color :r 220 :g 220 :b 220 :a 220)
  "Health bar outline color.")

;;; Debug Text - Read when drawing debug overlays
(defparameter *debug-npc-text-size* 12
  "Debug text size for NPC AI overlay.")
(defparameter *debug-npc-text-offset* 18
  "Extra vertical offset for NPC debug text.")
(defparameter *debug-npc-text-color* (raylib:make-color :r 255 :g 240 :b 160 :a 230)
  "NPC AI debug text color.")

;;; Minimap Colors - Read when drawing minimap
(defparameter *minimap-bg-color* (raylib:make-color :r 8 :g 12 :b 18 :a 190)
  "Minimap background color.")
(defparameter *minimap-border-color* (raylib:make-color :r 220 :g 220 :b 220 :a 200)
  "Minimap border color.")
(defparameter *minimap-player-color* (raylib:make-color :r 80 :g 220 :b 255 :a 220)
  "Minimap player marker color.")
(defparameter *minimap-npc-color* (raylib:make-color :r 255 :g 120 :b 80 :a 200)
  "Minimap NPC marker color.")
(defparameter *minimap-collision-color* (raylib:make-color :r 180 :g 180 :b 180 :a 140)
  "Minimap collision marker color.")

;;; Editor Visual Params - Read when drawing editor UI
(defparameter *editor-move-speed* 360.0
  "Movement speed for editor camera.")
(defparameter *editor-cursor-color* (raylib:make-color :r 80 :g 220 :b 255 :a 200)
  "Editor cursor highlight color.")
(defparameter *editor-spawn-color* (raylib:make-color :r 255 :g 140 :b 60 :a 210)
  "Editor spawn marker color.")
(defparameter *editor-tileset-preview-padding* 12
  "Padding for the tileset preview panel.")
(defparameter *editor-tileset-preview-max-width* 480
  "Max width for the tileset preview panel.")
(defparameter *editor-tileset-preview-max-height* 360
  "Max height for the tileset preview panel.")
(defparameter *editor-tileset-preview-bg-color* (raylib:make-color :r 10 :g 12 :b 18 :a 220)
  "Tileset preview background color.")
(defparameter *editor-tileset-preview-border-color* (raylib:make-color :r 180 :g 180 :b 180 :a 200)
  "Tileset preview border color.")
(defparameter *editor-tileset-preview-highlight-color* (raylib:make-color :r 255 :g 220 :b 120 :a 220)
  "Tileset preview selection color.")

;;; Music Volume - Read during audio updates
(defparameter *music-volume-steps* 10
  "Number of volume steps for music controls.")
(defparameter *music-default-volume-level* 1
  "Default music volume step (0 mutes).")

;;; Client Hooks - Called by game logic for client-only side effects
;;; These allow rendering/audio systems to register callbacks without
;;; creating dependencies from game logic to presentation code.
(defparameter *client-zone-change-hook* nil
  "Function called on zone transition with new zone-id.
   Set by rendering.lisp to clear stale render caches.")
