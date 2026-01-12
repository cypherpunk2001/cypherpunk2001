(in-package #:mmorpg)

(defparameter *verbose-logs* nil) ;; When true, logs player position and collider info per frame.
(defparameter *debug-collision-overlay* nil) ;; Draws debug grid and collision overlays.

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

(defparameter *player-sprite-dir* "../assets/1 Characters/3") ;; Directory that holds player sprite sheets.
(defparameter *sprite-frame-width* 32.0) ;; Width of a single sprite frame in pixels.
(defparameter *sprite-frame-height* 32.0) ;; Height of a single sprite frame in pixels.
(defparameter *sprite-scale* 4.0) ;; Scale factor applied when drawing sprites.

(defparameter *tileset-path* "../assets/2 Dungeon Tileset/1 Tiles/Tileset.png") ;; Atlas image used for floor tiles.
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
(defparameter *floor-variant-indices* #(41 42)) ;; Occasional variants (0 can be used for empty).
(defparameter *floor-variant-mod* 10) ;; 1 in N chance to use a variant instead of main.
(defparameter *floor-cluster-size* 3) ;; Size of clustered variant blocks, in tiles.
(defparameter *floor-seed* 1337) ;; Seed for deterministic floor variation.
(defparameter *landmark-indices* #(41 42)) ;; Sparse decorative overlays.
(defparameter *landmark-mod* 80) ;; 1 in N tiles become a landmark.
(defparameter *landmark-seed* 7331) ;; Seed for deterministic landmark placement.
(defparameter *wall-map-width* 40) ;; Width of the test wall map in tiles.
(defparameter *wall-map-height* 24) ;; Height of the test wall map in tiles.
(defparameter *wall-origin-x* 0) ;; World tile X where the wall map starts.
(defparameter *wall-origin-y* 0) ;; World tile Y where the wall map starts.
(defparameter *wall-tile-indices* #(107)) ;; Wall tile variants.
(defparameter *wall-seed* 2468) ;; Seed for wall tile variation.
(defparameter *player-collision-scale* 2.0) ;; Collision box size relative to one tile.
(defparameter *target-epsilon* 6.0) ;; Stop distance for click-to-move.

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
(defparameter +key-left-shift+ (cffi:foreign-enum-value 'raylib:keyboard-key :left-shift)) ;; Raylib keycode for the Left Shift key.
(defparameter +key-right-shift+ (cffi:foreign-enum-value 'raylib:keyboard-key :right-shift)) ;; Raylib keycode for the Right Shift key.
(defparameter +mouse-left+ (cffi:foreign-enum-value 'raylib:mouse-button :left)) ;; Raylib mouse button code for left click.
(defparameter +mouse-middle+ (cffi:foreign-enum-value 'raylib:mouse-button :middle)) ;; Raylib mouse button code for middle click.

(defun clamp (value min-value max-value)
  ;; Clamp VALUE between MIN-VALUE and MAX-VALUE for bounds checks.
  (max min-value (min value max-value)))

(defun normalize-direction (dx dy)
  ;; Normalize diagonal movement to unit length; keep axis-aligned values as-is.
  (if (and (not (zerop dx)) (not (zerop dy)))
      (let ((len (sqrt (+ (* dx dx) (* dy dy)))))
        (values (/ dx len) (/ dy len)))
      (values dx dy)))

(defun read-input-direction ()
  ;; Read WASD/arrow keys and return a normalized movement vector.
  (let ((dx 0.0)
        (dy 0.0))
    (when (or (raylib:is-key-down +key-right+)
              (raylib:is-key-down +key-d+))
      (incf dx 1.0))
    (when (or (raylib:is-key-down +key-left+)
              (raylib:is-key-down +key-a+))
      (decf dx 1.0))
    (when (or (raylib:is-key-down +key-down+)
              (raylib:is-key-down +key-s+))
      (incf dy 1.0))
    (when (or (raylib:is-key-down +key-up+)
              (raylib:is-key-down +key-w+))
      (decf dy 1.0))
    (normalize-direction dx dy)))

(defun screen-to-world (screen-x screen-y target-x target-y camera-offset camera-zoom)
  ;; Convert screen coordinates into world space using camera offset and zoom.
  (let* ((zoom (if (zerop camera-zoom) 1.0 camera-zoom))
         (sx (float screen-x 1.0))
         (sy (float screen-y 1.0)))
    (values (+ (/ (- sx (raylib:vector2-x camera-offset)) zoom)
               target-x)
            (+ (/ (- sy (raylib:vector2-y camera-offset)) zoom)
               target-y))))

(defun point-in-rect-p (x y rx ry rw rh)
  ;; Return true when point (x,y) lies inside the given rectangle bounds.
  (and (>= x rx)
       (< x (+ rx rw))
       (>= y ry)
       (< y (+ ry rh))))

(defun basename (path)
  ;; Return the filename portion of PATH for display labels.
  (let* ((path-str (string path))
         (slash (position #\/ path-str :from-end t))
         (backslash (position #\\ path-str :from-end t))
         (cut (max (or slash -1) (or backslash -1))))
    (if (>= cut 0)
        (subseq path-str (1+ cut))
        path-str)))

(defun sprite-path (filename)
  ;; Build a sprite sheet path under *player-sprite-dir*.
  (format nil "~a/~a" *player-sprite-dir* filename))

(defun player-direction (dx dy)
  ;; Pick a facing direction keyword from movement delta.
  (cond ((> (abs dx) (abs dy)) :side)
        ((< dy 0.0) :up)
        (t :down)))

(defun player-state (dx dy)
  ;; Return :idle or :walk based on movement delta.
  (if (and (zerop dx) (zerop dy)) :idle :walk))

(defun player-animation-params (state)
  ;; Return frame count and base frame time for STATE.
  (ecase state
    (:idle (values *idle-frame-count* *idle-frame-time*))
    (:walk (values *walk-frame-count* *walk-frame-time*))
    (:attack (values *attack-frame-count* *attack-frame-time*))))

(defun u32-hash (x y &optional (seed 1337))
  ;; Generate a deterministic 32-bit hash for tile variation.
  (logand #xffffffff
          (+ (* x 73856093)
             (* y 19349663)
             (* seed 83492791))))

(defun floor-tile-at (x y main-index variant-indices)
  ;; Choose a floor tile index with clustered variant noise.
  (let* ((cluster-size (max 1 *floor-cluster-size*))
         (variant-count (length variant-indices))
         (variant-mod (max 1 *floor-variant-mod*))
         (cx (floor x cluster-size))
         (cy (floor y cluster-size))
         (h (u32-hash cx cy *floor-seed*))
         (h2 (u32-hash (+ cx 17) (+ cy 31) (+ *floor-seed* 7331))))
    (if (and (> variant-count 0)
             (zerop (mod h variant-mod)))
        (aref variant-indices (mod h2 variant-count))
        main-index)))

(defun landmark-tile-at (x y)
  ;; Choose an optional landmark tile index with sparse hashing.
  (let* ((variant-count (length *landmark-indices*))
         (variant-mod (max 1 *landmark-mod*))
         (h (u32-hash x y *landmark-seed*))
         (h2 (u32-hash (+ x 19) (+ y 47) (+ *landmark-seed* 101))))
    (if (and (> variant-count 0)
             (zerop (mod h variant-mod)))
        (aref *landmark-indices* (mod h2 variant-count))
        0)))

(defun build-wall-map ()
  ;; Create a test wall map array with a solid border.
  (let* ((width *wall-map-width*)
         (height *wall-map-height*)
         (map (make-array (list height width) :initial-element 0)))
    (labels ((set-wall (x y)
               (when (and (<= 0 x) (< x width)
                          (<= 0 y) (< y height))
                 (setf (aref map y x) 1))))
      (loop :for x :from 0 :below width
            :do (set-wall x 0)
                (set-wall x (1- height)))
      (loop :for y :from 0 :below height
            :do (set-wall 0 y)
                (set-wall (1- width) y)))
    map))

(defun wall-occupied-p (wall-map tx ty)
  ;; Check whether a tile inside the wall map is nonzero.
  (let* ((local-x (- tx *wall-origin-x*))
         (local-y (- ty *wall-origin-y*))
         (width (array-dimension wall-map 1))
         (height (array-dimension wall-map 0)))
    (and (<= 0 local-x)
         (< local-x width)
         (<= 0 local-y)
         (< local-y height)
         (not (zerop (aref wall-map local-y local-x))))))

(defun wall-blocked-p (wall-map tx ty)
  ;; Treat walls and out-of-bounds tiles as blocked for collision.
  (let* ((local-x (- tx *wall-origin-x*))
         (local-y (- ty *wall-origin-y*))
         (width (array-dimension wall-map 1))
         (height (array-dimension wall-map 0)))
    (if (or (< local-x 0)
            (>= local-x width)
            (< local-y 0)
            (>= local-y height))
        t
        (not (zerop (aref wall-map local-y local-x))))))

(defun wall-tile-at (wall-map tx ty)
  ;; Return the wall tile index for rendering or 0 if empty.
  (let ((variant-count (length *wall-tile-indices*)))
    (if (and (wall-occupied-p wall-map tx ty)
             (> variant-count 0))
        (aref *wall-tile-indices*
              (mod (u32-hash tx ty *wall-seed*) variant-count))
        0)))

(defun blocked-at-p (wall-map x y half-w half-h tile-size)
  ;; Test collider bounds against blocked tiles in the wall map.
  (let* ((left (- x half-w))
         (right (+ x half-w))
         (top (- y half-h))
         (bottom (+ y half-h))
         (tx1 (floor left tile-size))
         (tx2 (floor right tile-size))
         (ty1 (floor top tile-size))
         (ty2 (floor bottom tile-size)))
    (loop :for ty :from ty1 :to ty2
          :thereis (loop :for tx :from tx1 :to tx2
                         :thereis (wall-blocked-p wall-map tx ty)))))

(defun attempt-move (wall-map x y dx dy step half-w half-h tile-size)
  ;; Resolve movement per axis and cancel movement when blocked.
  (let ((nx x)
        (ny y)
        (out-dx 0.0)
        (out-dy 0.0))
    (when (not (zerop dx))
      (let ((try-x (+ x (* dx step))))
        (if (blocked-at-p wall-map try-x y half-w half-h tile-size)
            (setf out-dx 0.0)
            (setf nx try-x
                  out-dx dx))))
    (when (not (zerop dy))
      (let ((try-y (+ ny (* dy step))))
        (if (blocked-at-p wall-map nx try-y half-w half-h tile-size)
            (setf out-dy 0.0)
            (setf ny try-y
                  out-dy dy))))
    (values nx ny out-dx out-dy)))

(defun set-rectangle (rect x y width height)
  ;; Mutate a Raylib rectangle with new bounds and return it.
  (setf (raylib:rectangle-x rect) x
        (raylib:rectangle-y rect) y
        (raylib:rectangle-width rect) width
        (raylib:rectangle-height rect) height)
  rect)

(defun set-tile-source-rect (rect tile-index tile-size-f)
  ;; Set the atlas source rectangle for a given tile index.
  (let* ((col (mod tile-index *tileset-columns*))
         (row (floor tile-index *tileset-columns*)))
    (set-rectangle rect
                   (* col tile-size-f)
                   (* row tile-size-f)
                   tile-size-f
                   tile-size-f)))

(defstruct (player (:constructor %make-player))
  ;; Player state used by update/draw loops.
  x y dx dy
  anim-state facing
  frame-index frame-timer
  attacking attack-timer
  target-x target-y target-active
  running run-stamina
  auto-right auto-left auto-down auto-up
  mouse-hold-timer)

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
  origin tile-source tile-dest player-source player-dest)

(defstruct (assets (:constructor %make-assets))
  ;; Loaded textures and sprite sizing data.
  tileset
  down-idle down-walk down-attack
  up-idle up-walk up-attack
  side-idle side-walk side-attack
  scaled-width scaled-height half-sprite-width half-sprite-height)

(defstruct (camera (:constructor %make-camera))
  ;; Camera state used by 2D mode.
  offset zoom)

(defstruct (game (:constructor %make-game))
  ;; Aggregate of game subsystems for update/draw.
  world player audio ui render assets camera)

(defun make-stamina-labels ()
  ;; Precompute stamina HUD strings to avoid per-frame consing.
  (let* ((max (truncate *run-stamina-max*))
         (labels (make-array (1+ max))))
    (loop :for i :from 0 :to max
          :do (setf (aref labels i) (format nil "Stamina: ~2d" i)))
    labels))

(defun make-player (start-x start-y)
  ;; Construct a player state struct at the given start position.
  (%make-player :x start-x
                :y start-y
                :dx 0.0
                :dy 0.0
                :anim-state :idle
                :facing :down
                :frame-index 0
                :frame-timer 0.0
                :attacking nil
                :attack-timer 0.0
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

(defun make-world ()
  ;; Build world state and derived collision/render constants.
  (let* ((tile-size-f (float *tile-size* 1.0))
         (tile-dest-size (* tile-size-f *tile-scale*))
         (floor-index *floor-tile-index*)
         (wall-map (build-wall-map))
         (wall-map-width (array-dimension wall-map 1))
         (wall-map-height (array-dimension wall-map 0))
         (collision-half-width (* (/ tile-dest-size 2.0) *player-collision-scale*))
         (collision-half-height (* (/ tile-dest-size 2.0) *player-collision-scale*))
         (wall-min-x (+ (* (+ *wall-origin-x* 1) tile-dest-size)
                        collision-half-width))
         (wall-max-x (- (* (+ *wall-origin-x* (1- wall-map-width))
                           tile-dest-size)
                        collision-half-width))
         (wall-min-y (+ (* (+ *wall-origin-y* 1) tile-dest-size)
                        collision-half-height))
         (wall-max-y (- (* (+ *wall-origin-y* (1- wall-map-height))
                           tile-dest-size)
                        collision-half-height)))
    (%make-world :tile-size-f tile-size-f
                 :tile-dest-size tile-dest-size
                 :floor-index floor-index
                 :wall-map wall-map
                 :wall-map-width wall-map-width
                 :wall-map-height wall-map-height
                 :collision-half-width collision-half-width
                 :collision-half-height collision-half-height
                 :wall-min-x wall-min-x
                 :wall-max-x wall-max-x
                 :wall-min-y wall-min-y
                 :wall-max-y wall-max-y)))

(defun make-render ()
  ;; Allocate reusable rectangles and origin vector for rendering.
  (%make-render :origin (raylib:make-vector2 :x 0.0 :y 0.0)
                :tile-source (raylib:make-rectangle)
                :tile-dest (raylib:make-rectangle)
                :player-source (raylib:make-rectangle)
                :player-dest (raylib:make-rectangle)))

(defun load-assets ()
  ;; Load textures and compute sprite sizing for rendering.
  (let* ((scaled-width (* *sprite-frame-width* *sprite-scale*))
         (scaled-height (* *sprite-frame-height* *sprite-scale*))
         (half-sprite-width (/ scaled-width 2.0))
         (half-sprite-height (/ scaled-height 2.0))
         (tileset (raylib:load-texture *tileset-path*))
         (down-idle (raylib:load-texture (sprite-path "D_Idle.png")))
         (down-walk (raylib:load-texture (sprite-path "D_Walk.png")))
         (down-attack (raylib:load-texture (sprite-path "D_Attack.png")))
         (up-idle (raylib:load-texture (sprite-path "U_Idle.png")))
         (up-walk (raylib:load-texture (sprite-path "U_Walk.png")))
         (up-attack (raylib:load-texture (sprite-path "U_Attack.png")))
         (side-idle (raylib:load-texture (sprite-path "S_Idle.png")))
         (side-walk (raylib:load-texture (sprite-path "S_Walk.png")))
         (side-attack (raylib:load-texture (sprite-path "S_Attack.png"))))
    (%make-assets :tileset tileset
                  :down-idle down-idle
                  :down-walk down-walk
                  :down-attack down-attack
                  :up-idle up-idle
                  :up-walk up-walk
                  :up-attack up-attack
                  :side-idle side-idle
                  :side-walk side-walk
                  :side-attack side-attack
                  :scaled-width scaled-width
                  :scaled-height scaled-height
                  :half-sprite-width half-sprite-width
                  :half-sprite-height half-sprite-height)))

(defun unload-assets (assets)
  ;; Unload textures stored in the assets struct.
  (raylib:unload-texture (assets-tileset assets))
  (raylib:unload-texture (assets-down-idle assets))
  (raylib:unload-texture (assets-down-walk assets))
  (raylib:unload-texture (assets-down-attack assets))
  (raylib:unload-texture (assets-up-idle assets))
  (raylib:unload-texture (assets-up-walk assets))
  (raylib:unload-texture (assets-up-attack assets))
  (raylib:unload-texture (assets-side-idle assets))
  (raylib:unload-texture (assets-side-walk assets))
  (raylib:unload-texture (assets-side-attack assets)))

(defun build-volume-bars (volume-steps)
  ;; Create prebuilt volume bar strings for the menu UI.
  (let ((bars (make-array (1+ volume-steps))))
    (loop :for i :from 0 :to volume-steps
          :do (let ((s (make-string (+ volume-steps 2)
                                    :initial-element #\-)))
                (setf (aref s 0) #\[)
                (setf (aref s (1+ volume-steps)) #\])
                (dotimes (j i)
                  (setf (aref s (1+ j)) #\|))
                (setf (aref bars i) s)))
    bars))

(defun make-audio ()
  ;; Load music streams and initialize audio state.
  (let* ((soundtrack-count (length *soundtrack-tracks*))
         (soundtrack-music (make-array soundtrack-count))
         (soundtrack-names (make-array soundtrack-count))
         (soundtrack-labels (make-array soundtrack-count))
         (soundtrack-index 0)
         (menu-no-music-label "No music loaded")
         (current-track-label menu-no-music-label)
         (volume-steps 10)
         (volume-level volume-steps)
         (volume-bars (build-volume-bars volume-steps))
         (music-volume (/ volume-level (float volume-steps 1.0)))
         (current-music nil))
    (loop :for index :from 0 :below soundtrack-count
          :for path = (aref *soundtrack-tracks* index)
          :for display = (if (and (< index (length *soundtrack-display-names*))
                                  (aref *soundtrack-display-names* index))
                             (aref *soundtrack-display-names* index)
                             (basename path))
          :for label = (format nil "Now Playing: ~a" display)
          :do (setf (aref soundtrack-names index) display
                    (aref soundtrack-labels index) label
                    (aref soundtrack-music index)
                    (raylib:load-music-stream path)))
    (when (> soundtrack-count 0)
      (setf current-music (aref soundtrack-music 0)
            current-track-label (aref soundtrack-labels 0))
      (raylib:play-music-stream current-music)
      (raylib:set-music-volume current-music music-volume))
    (%make-audio :soundtrack-count soundtrack-count
                 :soundtrack-music soundtrack-music
                 :soundtrack-names soundtrack-names
                 :soundtrack-labels soundtrack-labels
                 :soundtrack-index soundtrack-index
                 :current-music current-music
                 :current-track-label current-track-label
                 :volume-steps volume-steps
                 :volume-level volume-level
                 :volume-bars volume-bars
                 :music-volume music-volume)))

(defun shutdown-audio (audio)
  ;; Unload music streams stored in audio state.
  (let ((count (audio-soundtrack-count audio))
        (music (audio-soundtrack-music audio)))
    (loop :for index :from 0 :below count
          :do (raylib:unload-music-stream (aref music index)))))

(defun audio-advance-track (audio step)
  ;; Switch to the next or previous track and restart playback.
  (let ((count (audio-soundtrack-count audio)))
    (when (> count 0)
      (let ((old-music (audio-current-music audio)))
        (setf (audio-soundtrack-index audio)
              (mod (+ (audio-soundtrack-index audio) step) count)
              (audio-current-music audio)
              (aref (audio-soundtrack-music audio)
                    (audio-soundtrack-index audio))
              (audio-current-track-label audio)
              (aref (audio-soundtrack-labels audio)
                    (audio-soundtrack-index audio)))
        (when old-music
          (raylib:stop-music-stream old-music))
        (raylib:play-music-stream (audio-current-music audio))
        (raylib:set-music-volume (audio-current-music audio)
                                 (audio-music-volume audio)))))
  audio)

(defun audio-adjust-volume (audio delta)
  ;; Adjust volume level and apply it to current music.
  (let* ((steps (audio-volume-steps audio))
         (new-level (clamp (+ (audio-volume-level audio) delta) 0 steps))
         (music-volume (/ new-level (float steps 1.0))))
    (setf (audio-volume-level audio) new-level
          (audio-music-volume audio) music-volume)
    (let ((current (audio-current-music audio)))
      (when current
        (raylib:set-music-volume current music-volume))))
  audio)

(defun update-audio (audio)
  ;; Update streaming music and auto-advance near track end.
  (let ((current (audio-current-music audio)))
    (when current
      (raylib:update-music-stream current)
      (let* ((track-length (raylib:get-music-time-length current))
             (track-played (raylib:get-music-time-played current)))
        (when (and (> track-length 0.0)
                   (>= track-played (- track-length 0.05)))
          (audio-advance-track audio 1)))))
  audio)

(defun make-ui ()
  ;; Build UI layout constants and colors for the menu and HUD.
  (let* ((menu-open nil)
         (exit-requested nil)
         (menu-padding 32)
         (menu-panel-width (truncate (* *window-width* 0.92)))
         (menu-panel-height (truncate (* *window-height* 0.92)))
         (menu-panel-x (truncate (/ (- *window-width* menu-panel-width) 2)))
         (menu-panel-y (truncate (/ (- *window-height* menu-panel-height) 2)))
         (menu-title "Escape Menu")
         (menu-hint "Press Esc to close")
         (menu-track-title "Music")
         (menu-button-label "Quit")
         (menu-prev-label "Prev")
         (menu-next-label "Next")
         (menu-vol-down-label "Vol -")
         (menu-vol-up-label "Vol +")
         (menu-title-size 34)
         (menu-hint-size 18)
         (menu-track-size 20)
         (menu-button-text-size 22)
         (menu-nav-text-size 18)
         (menu-volume-text-size 18)
         (menu-button-width 260)
         (menu-button-height 56)
         (menu-button-x (truncate (/ (- *window-width* menu-button-width) 2)))
         (menu-button-y (- (+ menu-panel-y menu-panel-height)
                           menu-padding
                           menu-button-height))
         (menu-nav-button-width 140)
         (menu-nav-button-height 44)
         (menu-nav-gap 16)
         (menu-nav-y (+ menu-panel-y 140))
         (menu-prev-x (+ menu-panel-x menu-padding))
         (menu-next-x (+ menu-prev-x menu-nav-button-width menu-nav-gap))
         (menu-track-text-x (+ menu-panel-x menu-padding))
         (menu-track-text-y (+ menu-nav-y menu-nav-button-height 22))
         (menu-volume-button-width 110)
         (menu-volume-button-height 40)
         (menu-volume-gap 12)
         (menu-volume-y (+ menu-track-text-y 40))
         (menu-volume-down-x (+ menu-panel-x menu-padding))
         (menu-volume-up-x (+ menu-volume-down-x
                              menu-volume-button-width
                              menu-volume-gap))
         (menu-volume-bars-x (+ menu-volume-up-x
                                menu-volume-button-width
                                menu-volume-gap))
         (menu-toggle-gap 18)
         (menu-debug-size 18)
         (menu-debug-x (+ menu-panel-x menu-padding))
         (menu-debug-y (+ menu-volume-y menu-volume-button-height 24))
         (menu-debug-label "Debug Collision Overlay")
         (menu-fullscreen-size 18)
         (menu-fullscreen-x menu-debug-x)
         (menu-fullscreen-y (+ menu-debug-y menu-debug-size menu-toggle-gap))
         (menu-fullscreen-label "Fullscreen | Windowed")
         (hud-bg-color (raylib:make-color :r 0 :g 0 :b 0 :a 160))
         (menu-overlay-color (raylib:make-color :r 0 :g 0 :b 0 :a 110))
         (menu-panel-color (raylib:make-color :r 18 :g 18 :b 18 :a 200))
         (menu-text-color (raylib:make-color :r 235 :g 235 :b 235 :a 255))
         (menu-button-color (raylib:make-color :r 170 :g 60 :b 60 :a 220))
         (menu-button-hover-color (raylib:make-color :r 210 :g 80 :b 80 :a 240))
         (debug-grid-color (raylib:make-color :r 255 :g 255 :b 255 :a 40))
         (debug-wall-color (raylib:make-color :r 80 :g 160 :b 255 :a 90))
         (debug-collision-color (raylib:make-color :r 255 :g 0 :b 0 :a 90))
         (debug-collider-color (raylib:make-color :r 0 :g 255 :b 0 :a 180))
         (stamina-labels (make-stamina-labels)))
    (%make-ui :menu-open menu-open
              :exit-requested exit-requested
              :menu-padding menu-padding
              :menu-panel-width menu-panel-width
              :menu-panel-height menu-panel-height
              :menu-panel-x menu-panel-x
              :menu-panel-y menu-panel-y
              :menu-title menu-title
              :menu-hint menu-hint
              :menu-track-title menu-track-title
              :menu-button-label menu-button-label
              :menu-prev-label menu-prev-label
              :menu-next-label menu-next-label
              :menu-vol-down-label menu-vol-down-label
              :menu-vol-up-label menu-vol-up-label
              :menu-title-size menu-title-size
              :menu-hint-size menu-hint-size
              :menu-track-size menu-track-size
              :menu-button-text-size menu-button-text-size
              :menu-nav-text-size menu-nav-text-size
              :menu-volume-text-size menu-volume-text-size
              :menu-button-width menu-button-width
              :menu-button-height menu-button-height
              :menu-button-x menu-button-x
              :menu-button-y menu-button-y
              :menu-nav-button-width menu-nav-button-width
              :menu-nav-button-height menu-nav-button-height
              :menu-nav-gap menu-nav-gap
              :menu-nav-y menu-nav-y
              :menu-prev-x menu-prev-x
              :menu-next-x menu-next-x
              :menu-track-text-x menu-track-text-x
              :menu-track-text-y menu-track-text-y
              :menu-volume-button-width menu-volume-button-width
              :menu-volume-button-height menu-volume-button-height
              :menu-volume-gap menu-volume-gap
              :menu-volume-y menu-volume-y
              :menu-volume-down-x menu-volume-down-x
              :menu-volume-up-x menu-volume-up-x
              :menu-volume-bars-x menu-volume-bars-x
              :menu-toggle-gap menu-toggle-gap
              :menu-debug-size menu-debug-size
              :menu-debug-x menu-debug-x
              :menu-debug-y menu-debug-y
              :menu-debug-label menu-debug-label
              :menu-fullscreen-size menu-fullscreen-size
              :menu-fullscreen-x menu-fullscreen-x
              :menu-fullscreen-y menu-fullscreen-y
              :menu-fullscreen-label menu-fullscreen-label
              :hud-bg-color hud-bg-color
              :menu-overlay-color menu-overlay-color
              :menu-panel-color menu-panel-color
              :menu-text-color menu-text-color
              :menu-button-color menu-button-color
              :menu-button-hover-color menu-button-hover-color
              :debug-grid-color debug-grid-color
              :debug-wall-color debug-wall-color
              :debug-collision-color debug-collision-color
              :debug-collider-color debug-collider-color
              :stamina-labels stamina-labels)))

(defun make-camera ()
  ;; Initialize camera offset and zoom settings.
  (%make-camera :offset (raylib:make-vector2 :x (/ *window-width* 2.0)
                                             :y (/ *window-height* 2.0))
                :zoom *camera-zoom-default*))

(defun make-game ()
  ;; Assemble game state and log setup if verbose is enabled.
  (let* ((world (make-world))
         (player (make-player (/ *window-width* 2.0)
                              (/ *window-height* 2.0)))
         (audio (make-audio))
         (ui (make-ui))
         (render (make-render))
         (assets (load-assets))
         (camera (make-camera)))
    (when *verbose-logs*
      (format t "~&Verbose logs on. tile-size=~,2f collider-half=~,2f,~,2f wall=[~,2f..~,2f, ~,2f..~,2f]~%"
              (world-tile-dest-size world)
              (world-collision-half-width world)
              (world-collision-half-height world)
              (world-wall-min-x world)
              (world-wall-max-x world)
              (world-wall-min-y world)
              (world-wall-max-y world))
      (finish-output))
    (%make-game :world world
                :player player
                :audio audio
                :ui ui
                :render render
                :assets assets
                :camera camera)))

(defun shutdown-game (game)
  ;; Release game resources before exiting.
  (shutdown-audio (game-audio game))
  (unload-assets (game-assets game)))

(defun update-camera-zoom (camera)
  ;; Adjust zoom with the mouse wheel and reset on middle click.
  (let ((wheel (raylib:get-mouse-wheel-move)))
    (when (not (zerop wheel))
      (setf (camera-zoom camera)
            (clamp (+ (camera-zoom camera) (* wheel *camera-zoom-step*))
                   *camera-zoom-min*
                   *camera-zoom-max*))))
  (when (raylib:is-mouse-button-pressed +mouse-middle+)
    (setf (camera-zoom camera) *camera-zoom-default*)))

(defun clear-player-auto-walk (player)
  ;; Clear auto-walk toggles on the player.
  (setf (player-auto-right player) nil
        (player-auto-left player) nil
        (player-auto-down player) nil
        (player-auto-up player) nil))

(defun set-player-target (player target-x target-y)
  ;; Set click-to-move target and activate it.
  (setf (player-target-x player) target-x
        (player-target-y player) target-y
        (player-target-active player) t
        (player-mouse-hold-timer player) 0.0))

(defun start-player-attack (player)
  ;; Start an attack animation if one is not already active.
  (unless (player-attacking player)
    (setf (player-attacking player) t
          (player-attack-timer player) 0.0)))

(defun update-target-from-mouse (player camera dt mouse-clicked mouse-down)
  ;; Handle click/hold to update the player target position.
  (when mouse-clicked
    (clear-player-auto-walk player)
    (multiple-value-bind (target-x target-y)
        (screen-to-world (raylib:get-mouse-x)
                         (raylib:get-mouse-y)
                         (player-x player)
                         (player-y player)
                         (camera-offset camera)
                         (camera-zoom camera))
      (set-player-target player target-x target-y)))
  (when (and mouse-down (not mouse-clicked))
    (incf (player-mouse-hold-timer player) dt)
    (when (>= (player-mouse-hold-timer player) *mouse-hold-repeat-seconds*)
      (setf (player-mouse-hold-timer player) 0.0)
      (clear-player-auto-walk player)
      (multiple-value-bind (target-x target-y)
          (screen-to-world (raylib:get-mouse-x)
                           (raylib:get-mouse-y)
                           (player-x player)
                           (player-y player)
                           (camera-offset camera)
                           (camera-zoom camera))
        (set-player-target player target-x target-y))))
  (unless mouse-down
    (setf (player-mouse-hold-timer player) 0.0)))

(defun update-input-direction (player mouse-clicked)
  ;; Compute input dx/dy and handle auto-walk toggles.
  (let ((input-dx 0.0)
        (input-dy 0.0))
    (unless mouse-clicked
      (let* ((shift-held (or (raylib:is-key-down +key-left-shift+)
                             (raylib:is-key-down +key-right-shift+)))
             (pressed-right (or (raylib:is-key-pressed +key-right+)
                                (raylib:is-key-pressed +key-d+)))
             (pressed-left (or (raylib:is-key-pressed +key-left+)
                               (raylib:is-key-pressed +key-a+)))
             (pressed-down (or (raylib:is-key-pressed +key-down+)
                               (raylib:is-key-pressed +key-s+)))
             (pressed-up (or (raylib:is-key-pressed +key-up+)
                             (raylib:is-key-pressed +key-w+))))
        (when (or pressed-right pressed-left pressed-down pressed-up)
          (if shift-held
              (setf *auto-walk-enabled* t)
              (progn
                (setf *auto-walk-enabled* nil)
                (clear-player-auto-walk player))))
        (if *auto-walk-enabled*
            (let ((key-pressed nil))
              (when pressed-right
                (setf key-pressed t
                      (player-auto-right player) (not (player-auto-right player)))
                (when (player-auto-right player)
                  (setf (player-auto-left player) nil)))
              (when pressed-left
                (setf key-pressed t
                      (player-auto-left player) (not (player-auto-left player)))
                (when (player-auto-left player)
                  (setf (player-auto-right player) nil)))
              (when pressed-down
                (setf key-pressed t
                      (player-auto-down player) (not (player-auto-down player)))
                (when (player-auto-down player)
                  (setf (player-auto-up player) nil)))
              (when pressed-up
                (setf key-pressed t
                      (player-auto-up player) (not (player-auto-up player)))
                (when (player-auto-up player)
                  (setf (player-auto-down player) nil)))
              (when key-pressed
                (setf (player-target-active player) nil))
              (setf input-dx (+ (if (player-auto-right player) 1.0 0.0)
                                (if (player-auto-left player) -1.0 0.0))
                    input-dy (+ (if (player-auto-down player) 1.0 0.0)
                                (if (player-auto-up player) -1.0 0.0)))
              (multiple-value-setq (input-dx input-dy)
                (normalize-direction input-dx input-dy)))
            (multiple-value-setq (input-dx input-dy)
              (read-input-direction)))))
    (values input-dx input-dy)))

(defun update-running-state (player dt moving allow-toggle)
  ;; Update stamina and return the current speed multiplier.
  (when allow-toggle
    (when (raylib:is-key-pressed +key-tab+)
      (if (> (player-run-stamina player) 0.0)
          (setf (player-running player) (not (player-running player)))
          (setf (player-running player) nil))))
  (if (and (player-running player) moving (> (player-run-stamina player) 0.0))
      (progn
        (decf (player-run-stamina player) dt)
        (when (<= (player-run-stamina player) 0.0)
          (setf (player-run-stamina player) 0.0
                (player-running player) nil)))
      (when (< (player-run-stamina player) *run-stamina-max*)
        (incf (player-run-stamina player) dt)
        (when (>= (player-run-stamina player) *run-stamina-max*)
          (setf (player-run-stamina player) *run-stamina-max*))))
  (if (and (player-running player) (> (player-run-stamina player) 0.0))
      *run-speed-mult*
      1.0))

(defun update-player-position (player world input-dx input-dy speed-mult dt)
  ;; Move the player with collision and target logic.
  (let ((x (player-x player))
        (y (player-y player))
        (dx 0.0)
        (dy 0.0))
    (cond
      ((or (not (zerop input-dx))
           (not (zerop input-dy)))
       (setf (player-target-active player) nil)
       (multiple-value-setq (x y dx dy)
         (attempt-move (world-wall-map world) x y input-dx input-dy
                       (* *player-speed* speed-mult dt)
                       (world-collision-half-width world)
                       (world-collision-half-height world)
                       (world-tile-dest-size world))))
      ((player-target-active player)
       (let* ((target-x (player-target-x player))
              (target-y (player-target-y player))
              (to-x (- target-x x))
              (to-y (- target-y y))
              (dist (sqrt (+ (* to-x to-x) (* to-y to-y)))))
         (if (<= dist *target-epsilon*)
             (setf (player-target-active player) nil
                   dx 0.0
                   dy 0.0)
             (let* ((dir-x (/ to-x dist))
                    (dir-y (/ to-y dist))
                    (step (min (* *player-speed* speed-mult dt) dist)))
               (multiple-value-setq (x y dx dy)
                 (attempt-move (world-wall-map world) x y dir-x dir-y step
                               (world-collision-half-width world)
                               (world-collision-half-height world)
                               (world-tile-dest-size world)))
               (when (or (<= dist step)
                         (and (zerop dx) (zerop dy)))
                 (setf (player-target-active player) nil))))))
      (t
       (setf dx 0.0
             dy 0.0)))
    (setf x (clamp x (world-wall-min-x world) (world-wall-max-x world))
          y (clamp y (world-wall-min-y world) (world-wall-max-y world)))
    (setf (player-x player) x
          (player-y player) y
          (player-dx player) dx
          (player-dy player) dy)))

(defun log-player-position (player world)
  ;; Emit verbose position and tile diagnostics for debugging.
  (let* ((x (player-x player))
         (y (player-y player))
         (tile-dest-size (world-tile-dest-size world))
         (tile-x (floor x tile-dest-size))
         (tile-y (floor y tile-dest-size))
         (feet-x x)
         (feet-y (+ y (world-collision-half-height world)))
         (feet-tile-x (floor feet-x tile-dest-size))
         (feet-tile-y (floor feet-y tile-dest-size)))
    (format t "~&pos=~,2f,~,2f center=~,2f,~,2f tile=~d,~d feet=~,2f,~,2f tile-feet=~d,~d~%"
            x y
            x y
            tile-x tile-y
            feet-x feet-y
            feet-tile-x feet-tile-y)
    (finish-output)))

(defun update-player-animation (player dt)
  ;; Advance animation timers and set facing/state.
  (let* ((dx (player-dx player))
         (dy (player-dy player))
         (moving (or (not (zerop dx)) (not (zerop dy))))
         (attacking (player-attacking player))
         (state (if attacking
                    :attack
                    (player-state dx dy)))
         (direction (if attacking
                        (if moving
                            (player-direction dx dy)
                            (player-facing player))
                        (player-direction dx dy))))
    (multiple-value-bind (frame-count base-frame-time)
        (player-animation-params state)
      (let* ((run-anim-mult (if (and (eq state :walk)
                                     (player-running player)
                                     (> (player-run-stamina player) 0.0))
                                *run-speed-mult*
                                1.0))
             (frame-time (/ base-frame-time run-anim-mult))
             (frame-index (player-frame-index player))
             (frame-timer (player-frame-timer player)))
        (unless (and (eq state (player-anim-state player))
                     (eq direction (player-facing player)))
          (setf (player-anim-state player) state
                (player-facing player) direction
                frame-index 0
                frame-timer 0.0)
          (when attacking
            (setf (player-attack-timer player) 0.0)))
        (if (eq state :attack)
            (let* ((attack-timer (+ (player-attack-timer player) dt))
                   (duration (* frame-time frame-count))
                   (clamped (min attack-timer duration))
                   (attack-frame (min (truncate (/ clamped frame-time))
                                      (1- frame-count))))
              (setf (player-attack-timer player) clamped
                    frame-index attack-frame
                    frame-timer (- clamped (* attack-frame frame-time)))
              (when (>= attack-timer duration)
                (setf (player-attacking player) nil
                      (player-attack-timer player) 0.0)))
            (progn
              (incf frame-timer dt)
              (loop :while (>= frame-timer frame-time)
                    :do (decf frame-timer frame-time)
                        (setf frame-index
                              (mod (1+ frame-index) frame-count)))))
        (setf (player-frame-index player) frame-index
              (player-frame-timer player) frame-timer)))))

(defun handle-menu-click (ui audio mouse-x mouse-y)
  ;; Process menu clicks for quit, music, volume, and toggles.
  (cond
    ((point-in-rect-p mouse-x mouse-y
                      (ui-menu-button-x ui) (ui-menu-button-y ui)
                      (ui-menu-button-width ui) (ui-menu-button-height ui))
     (setf (ui-exit-requested ui) t))
    ((and (> (audio-soundtrack-count audio) 0)
          (point-in-rect-p mouse-x mouse-y
                           (ui-menu-prev-x ui) (ui-menu-nav-y ui)
                           (ui-menu-nav-button-width ui)
                           (ui-menu-nav-button-height ui)))
     (audio-advance-track audio -1))
    ((and (> (audio-soundtrack-count audio) 0)
          (point-in-rect-p mouse-x mouse-y
                           (ui-menu-next-x ui) (ui-menu-nav-y ui)
                           (ui-menu-nav-button-width ui)
                           (ui-menu-nav-button-height ui)))
     (audio-advance-track audio 1))
    ((point-in-rect-p mouse-x mouse-y
                      (ui-menu-volume-down-x ui) (ui-menu-volume-y ui)
                      (ui-menu-volume-button-width ui)
                      (ui-menu-volume-button-height ui))
     (audio-adjust-volume audio -1))
    ((point-in-rect-p mouse-x mouse-y
                      (ui-menu-volume-up-x ui) (ui-menu-volume-y ui)
                      (ui-menu-volume-button-width ui)
                      (ui-menu-volume-button-height ui))
     (audio-adjust-volume audio 1))
    ((point-in-rect-p mouse-x mouse-y
                      (ui-menu-debug-x ui) (ui-menu-debug-y ui)
                      (ui-menu-debug-size ui) (ui-menu-debug-size ui))
     (setf *debug-collision-overlay* (not *debug-collision-overlay*)))
    ((point-in-rect-p mouse-x mouse-y
                      (ui-menu-fullscreen-x ui) (ui-menu-fullscreen-y ui)
                      (ui-menu-fullscreen-size ui)
                      (ui-menu-fullscreen-size ui))
     (raylib:toggle-fullscreen))))

(defun update-game (game dt)
  ;; Run one frame of input, audio, movement, and animation updates.
  (let* ((player (game-player game))
         (world (game-world game))
         (audio (game-audio game))
         (ui (game-ui game))
         (camera (game-camera game))
         (mouse-clicked (raylib:is-mouse-button-pressed +mouse-left+))
         (mouse-down (raylib:is-mouse-button-down +mouse-left+)))
    (update-audio audio)
    (update-camera-zoom camera)
    (when (raylib:is-key-pressed +key-escape+)
      (setf (ui-menu-open ui) (not (ui-menu-open ui))))
    (when (and (ui-menu-open ui) mouse-clicked)
      (handle-menu-click ui audio
                         (raylib:get-mouse-x)
                         (raylib:get-mouse-y)))
    (unless (ui-menu-open ui)
      (update-target-from-mouse player camera dt mouse-clicked mouse-down))
    (multiple-value-bind (input-dx input-dy)
        (update-input-direction player mouse-clicked)
      (let* ((moving (or (not (zerop input-dx))
                         (not (zerop input-dy))
                         (player-target-active player)))
             (speed-mult (update-running-state player dt moving (not mouse-clicked))))
        (update-player-position player world input-dx input-dy speed-mult dt)))
    (when (and (not (ui-menu-open ui))
               (raylib:is-key-pressed +key-space+))
      (start-player-attack player))
    (when *verbose-logs*
      (log-player-position player world))
    (update-player-animation player dt)))

(defun draw-world (world render assets camera player ui)
  ;; Render floor, landmarks, walls, and debug overlays.
  (let* ((tile-dest-size (world-tile-dest-size world))
         (tile-size-f (world-tile-size-f world))
         (floor-index (world-floor-index world))
         (tileset (assets-tileset assets))
         (tile-source (render-tile-source render))
         (tile-dest (render-tile-dest render))
         (origin (render-origin render))
         (wall-map (world-wall-map world))
         (x (player-x player))
         (y (player-y player))
         (zoom (camera-zoom camera))
         (half-view-width (/ *window-width* (* 2.0 zoom)))
         (half-view-height (/ *window-height* (* 2.0 zoom)))
         (view-left (- x half-view-width))
         (view-right (+ x half-view-width))
         (view-top (- y half-view-height))
         (view-bottom (+ y half-view-height))
         (start-col (floor view-left tile-dest-size))
         (end-col (ceiling view-right tile-dest-size))
         (start-row (floor view-top tile-dest-size))
         (end-row (ceiling view-bottom tile-dest-size)))
    (loop :for row :from start-row :to end-row
          :for dest-y :from (* start-row tile-dest-size) :by tile-dest-size
          :do (loop :for col :from start-col :to end-col
                    :for dest-x :from (* start-col tile-dest-size) :by tile-dest-size
                    :for tile-index = (floor-tile-at col row
                                                     floor-index
                                                     *floor-variant-indices*)
                    :do (set-rectangle tile-dest dest-x dest-y
                                       tile-dest-size tile-dest-size)
                        (when (not (zerop tile-index))
                          (set-tile-source-rect tile-source tile-index tile-size-f)
                          (raylib:draw-texture-pro tileset
                                                   tile-source
                                                   tile-dest
                                                   origin
                                                   0.0
                                                   raylib:+white+))
                        (let ((landmark-index (landmark-tile-at col row)))
                          (when (not (zerop landmark-index))
                            (set-tile-source-rect tile-source landmark-index tile-size-f)
                            (raylib:draw-texture-pro tileset
                                                     tile-source
                                                     tile-dest
                                                     origin
                                                     0.0
                                                     raylib:+white+)))
                        (let ((wall-index (wall-tile-at wall-map col row)))
                          (when (not (zerop wall-index))
                            (set-tile-source-rect tile-source wall-index tile-size-f)
                            (raylib:draw-texture-pro tileset
                                                     tile-source
                                                     tile-dest
                                                     origin
                                                     0.0
                                                     raylib:+white+)))))
    (when *debug-collision-overlay*
      (let ((tile-px (round tile-dest-size)))
        (loop :for row :from start-row :to end-row
              :for dest-y :from (* start-row tile-dest-size) :by tile-dest-size
              :do (loop :for col :from start-col :to end-col
                        :for dest-x :from (* start-col tile-dest-size) :by tile-dest-size
                        :for ix = (round dest-x)
                        :for iy = (round dest-y)
                        :do (when (wall-blocked-p wall-map col row)
                              (raylib:draw-rectangle ix iy tile-px tile-px
                                                     (ui-debug-collision-color ui)))
                            (when (not (zerop (wall-tile-at wall-map col row)))
                              (raylib:draw-rectangle ix iy tile-px tile-px
                                                     (ui-debug-wall-color ui)))
                            (raylib:draw-rectangle-lines ix iy tile-px tile-px
                                                         (ui-debug-grid-color ui)))))
      (let ((ix (round (- x (world-collision-half-width world))))
            (iy (round (- y (world-collision-half-height world))))
            (iw (round (* 2.0 (world-collision-half-width world))))
            (ih (round (* 2.0 (world-collision-half-height world)))))
        (raylib:draw-rectangle-lines ix iy iw ih (ui-debug-collider-color ui))))))

(defun player-texture-for (assets direction state)
  ;; Select the sprite sheet texture for DIRECTION and STATE.
  (ecase direction
    (:down (ecase state
             (:walk (assets-down-walk assets))
             (:idle (assets-down-idle assets))
             (:attack (assets-down-attack assets))))
    (:up (ecase state
           (:walk (assets-up-walk assets))
           (:idle (assets-up-idle assets))
           (:attack (assets-up-attack assets))))
    (:side (ecase state
            (:walk (assets-side-walk assets))
            (:idle (assets-side-idle assets))
            (:attack (assets-side-attack assets))))))

(defun draw-player (player assets render)
  ;; Render the player sprite at its world position.
  (let* ((direction (player-facing player))
         (state (player-anim-state player))
         (dx (player-dx player))
         (flip (and (eq direction :side) (> dx 0.0)))
         (texture (player-texture-for assets direction state))
         (src-x (* (player-frame-index player) *sprite-frame-width*))
         (src-x (if flip
                    (+ src-x *sprite-frame-width*)
                    src-x))
         (src-width (if flip
                        (- *sprite-frame-width*)
                        *sprite-frame-width*))
         (half-width (assets-half-sprite-width assets))
         (half-height (assets-half-sprite-height assets)))
    (set-rectangle (render-player-source render)
                   src-x 0.0
                   src-width *sprite-frame-height*)
    (set-rectangle (render-player-dest render)
                   (- (player-x player) half-width)
                   (- (player-y player) half-height)
                   (assets-scaled-width assets)
                   (assets-scaled-height assets))
    (raylib:draw-texture-pro texture
                             (render-player-source render)
                             (render-player-dest render)
                             (render-origin render)
                             0.0
                             raylib:+white+)))

(defun draw-hud (player ui)
  ;; Draw stamina HUD using precomputed labels.
  (let* ((labels (ui-stamina-labels ui))
         (max-index (1- (length labels)))
         (run-seconds (max 0 (min (truncate (player-run-stamina player))
                                  max-index)))
         (run-text (aref labels run-seconds)))
    (raylib:draw-rectangle 6 6 110 24 (ui-hud-bg-color ui))
    (raylib:draw-text run-text 10 10 20 raylib:+white+)))

(defun draw-menu (ui audio)
  ;; Render the pause menu and hover states.
  (let* ((mouse-x (raylib:get-mouse-x))
         (mouse-y (raylib:get-mouse-y))
         (hover-quit (point-in-rect-p mouse-x mouse-y
                                      (ui-menu-button-x ui)
                                      (ui-menu-button-y ui)
                                      (ui-menu-button-width ui)
                                      (ui-menu-button-height ui)))
         (hover-prev (point-in-rect-p mouse-x mouse-y
                                      (ui-menu-prev-x ui)
                                      (ui-menu-nav-y ui)
                                      (ui-menu-nav-button-width ui)
                                      (ui-menu-nav-button-height ui)))
         (hover-next (point-in-rect-p mouse-x mouse-y
                                      (ui-menu-next-x ui)
                                      (ui-menu-nav-y ui)
                                      (ui-menu-nav-button-width ui)
                                      (ui-menu-nav-button-height ui)))
         (hover-vol-down (point-in-rect-p mouse-x mouse-y
                                          (ui-menu-volume-down-x ui)
                                          (ui-menu-volume-y ui)
                                          (ui-menu-volume-button-width ui)
                                          (ui-menu-volume-button-height ui)))
         (hover-vol-up (point-in-rect-p mouse-x mouse-y
                                        (ui-menu-volume-up-x ui)
                                        (ui-menu-volume-y ui)
                                        (ui-menu-volume-button-width ui)
                                        (ui-menu-volume-button-height ui)))
         (quit-color (if hover-quit
                         (ui-menu-button-hover-color ui)
                         (ui-menu-button-color ui)))
         (prev-color (if hover-prev
                         (ui-menu-button-hover-color ui)
                         (ui-menu-button-color ui)))
         (next-color (if hover-next
                         (ui-menu-button-hover-color ui)
                         (ui-menu-button-color ui)))
         (vol-down-color (if hover-vol-down
                             (ui-menu-button-hover-color ui)
                             (ui-menu-button-color ui)))
         (vol-up-color (if hover-vol-up
                           (ui-menu-button-hover-color ui)
                           (ui-menu-button-color ui)))
         (title-x (+ (ui-menu-panel-x ui) (ui-menu-padding ui)))
         (title-y (+ (ui-menu-panel-y ui) (ui-menu-padding ui)))
         (hint-y (+ (ui-menu-panel-y ui) (ui-menu-padding ui) 44))
         (track-title-y (- (ui-menu-nav-y ui) 28))
         (volume-label-y (- (ui-menu-volume-y ui) 26))
         (volume-bars-text (aref (audio-volume-bars audio)
                                 (audio-volume-level audio)))
         (debug-on *debug-collision-overlay*)
         (hover-debug (point-in-rect-p mouse-x mouse-y
                                       (ui-menu-debug-x ui)
                                       (ui-menu-debug-y ui)
                                       (ui-menu-debug-size ui)
                                       (ui-menu-debug-size ui)))
         (fs-on (raylib:is-window-fullscreen))
         (hover-fs (point-in-rect-p mouse-x mouse-y
                                    (ui-menu-fullscreen-x ui)
                                    (ui-menu-fullscreen-y ui)
                                    (ui-menu-fullscreen-size ui)
                                    (ui-menu-fullscreen-size ui)))
         (debug-box-color (cond
                            (hover-debug (ui-menu-button-hover-color ui))
                            (debug-on (ui-menu-button-color ui))
                            (t (ui-menu-panel-color ui))))
         (fs-box-color (cond
                         (hover-fs (ui-menu-button-hover-color ui))
                         (fs-on (ui-menu-button-color ui))
                         (t (ui-menu-panel-color ui)))))
    (raylib:draw-rectangle 0 0 *window-width* *window-height*
                           (ui-menu-overlay-color ui))
    (raylib:draw-rectangle (ui-menu-panel-x ui) (ui-menu-panel-y ui)
                           (ui-menu-panel-width ui) (ui-menu-panel-height ui)
                           (ui-menu-panel-color ui))
    (raylib:draw-text (ui-menu-title ui)
                      title-x
                      title-y
                      (ui-menu-title-size ui)
                      (ui-menu-text-color ui))
    (raylib:draw-text (ui-menu-hint ui)
                      title-x
                      hint-y
                      (ui-menu-hint-size ui)
                      (ui-menu-text-color ui))
    (raylib:draw-text (ui-menu-track-title ui)
                      title-x
                      track-title-y
                      (ui-menu-track-size ui)
                      (ui-menu-text-color ui))
    (raylib:draw-rectangle (ui-menu-prev-x ui) (ui-menu-nav-y ui)
                           (ui-menu-nav-button-width ui)
                           (ui-menu-nav-button-height ui)
                           prev-color)
    (raylib:draw-text (ui-menu-prev-label ui)
                      (+ (ui-menu-prev-x ui) 18)
                      (+ (ui-menu-nav-y ui) 12)
                      (ui-menu-nav-text-size ui)
                      (ui-menu-text-color ui))
    (raylib:draw-rectangle (ui-menu-next-x ui) (ui-menu-nav-y ui)
                           (ui-menu-nav-button-width ui)
                           (ui-menu-nav-button-height ui)
                           next-color)
    (raylib:draw-text (ui-menu-next-label ui)
                      (+ (ui-menu-next-x ui) 18)
                      (+ (ui-menu-nav-y ui) 12)
                      (ui-menu-nav-text-size ui)
                      (ui-menu-text-color ui))
    (raylib:draw-text (audio-current-track-label audio)
                      (ui-menu-track-text-x ui)
                      (ui-menu-track-text-y ui)
                      (ui-menu-track-size ui)
                      (ui-menu-text-color ui))
    (raylib:draw-text "Volume"
                      (ui-menu-track-text-x ui)
                      volume-label-y
                      (ui-menu-volume-text-size ui)
                      (ui-menu-text-color ui))
    (raylib:draw-rectangle (ui-menu-volume-down-x ui)
                           (ui-menu-volume-y ui)
                           (ui-menu-volume-button-width ui)
                           (ui-menu-volume-button-height ui)
                           vol-down-color)
    (raylib:draw-text (ui-menu-vol-down-label ui)
                      (+ (ui-menu-volume-down-x ui) 14)
                      (+ (ui-menu-volume-y ui) 10)
                      (ui-menu-volume-text-size ui)
                      (ui-menu-text-color ui))
    (raylib:draw-rectangle (ui-menu-volume-up-x ui)
                           (ui-menu-volume-y ui)
                           (ui-menu-volume-button-width ui)
                           (ui-menu-volume-button-height ui)
                           vol-up-color)
    (raylib:draw-text (ui-menu-vol-up-label ui)
                      (+ (ui-menu-volume-up-x ui) 14)
                      (+ (ui-menu-volume-y ui) 10)
                      (ui-menu-volume-text-size ui)
                      (ui-menu-text-color ui))
    (raylib:draw-text volume-bars-text
                      (ui-menu-volume-bars-x ui)
                      (+ (ui-menu-volume-y ui) 10)
                      (ui-menu-volume-text-size ui)
                      (ui-menu-text-color ui))
    (raylib:draw-rectangle (ui-menu-debug-x ui)
                           (ui-menu-debug-y ui)
                           (ui-menu-debug-size ui)
                           (ui-menu-debug-size ui)
                           debug-box-color)
    (raylib:draw-rectangle-lines (ui-menu-debug-x ui)
                                 (ui-menu-debug-y ui)
                                 (ui-menu-debug-size ui)
                                 (ui-menu-debug-size ui)
                                 (ui-menu-text-color ui))
    (raylib:draw-text (ui-menu-debug-label ui)
                      (+ (ui-menu-debug-x ui) 28)
                      (- (ui-menu-debug-y ui) 2)
                      (ui-menu-volume-text-size ui)
                      (ui-menu-text-color ui))
    (raylib:draw-rectangle (ui-menu-fullscreen-x ui)
                           (ui-menu-fullscreen-y ui)
                           (ui-menu-fullscreen-size ui)
                           (ui-menu-fullscreen-size ui)
                           fs-box-color)
    (raylib:draw-rectangle-lines (ui-menu-fullscreen-x ui)
                                 (ui-menu-fullscreen-y ui)
                                 (ui-menu-fullscreen-size ui)
                                 (ui-menu-fullscreen-size ui)
                                 (ui-menu-text-color ui))
    (raylib:draw-text (ui-menu-fullscreen-label ui)
                      (+ (ui-menu-fullscreen-x ui) 28)
                      (- (ui-menu-fullscreen-y ui) 2)
                      (ui-menu-volume-text-size ui)
                      (ui-menu-text-color ui))
    (raylib:draw-rectangle (ui-menu-button-x ui) (ui-menu-button-y ui)
                           (ui-menu-button-width ui)
                           (ui-menu-button-height ui)
                           quit-color)
    (raylib:draw-text (ui-menu-button-label ui)
                      (+ (ui-menu-button-x ui) 24)
                      (+ (ui-menu-button-y ui) 16)
                      (ui-menu-button-text-size ui)
                      (ui-menu-text-color ui))))

(defun draw-game (game)
  ;; Render a full frame: world, player, HUD, and menu.
  (let* ((player (game-player game))
         (world (game-world game))
         (audio (game-audio game))
         (ui (game-ui game))
         (render (game-render game))
         (assets (game-assets game))
         (camera (game-camera game)))
    (raylib:with-drawing
      (raylib:clear-background raylib:+black+)
      (let ((camera-2d (raylib:make-camera-2d
                        :target (raylib:make-vector2 :x (player-x player)
                                                     :y (player-y player))
                        :offset (camera-offset camera)
                        :rotation 0.0
                        :zoom (camera-zoom camera))))
        (raylib:with-mode-2d camera-2d
          (draw-world world render assets camera player ui)
          (draw-player player assets render)))
      (draw-hud player ui)
      (when (ui-menu-open ui)
        (draw-menu ui audio)))))

(defun run ()
  ;; Entry point that initializes game state and drives the main loop.
  (raylib:with-window ("Hello MMO" (*window-width* *window-height*))
    (raylib:set-target-fps 60)
    (raylib:set-exit-key 0)
    (raylib:init-audio-device)
    (let ((game (make-game)))
      (unwind-protect
           (loop :until (or (raylib:window-should-close)
                            (ui-exit-requested (game-ui game)))
                 :do (let ((dt (raylib:get-frame-time)))
                       (update-game game dt)
                       (draw-game game)))
        (shutdown-game game)
        (raylib:close-audio-device)))))
