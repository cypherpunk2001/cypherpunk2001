;; NOTE: If you change behavior here, update docs/ai.md :)
(in-package #:mmorpg)

(defun closest-player (players npc &optional zone-state game)
  "Return the closest alive player to NPC, if any.
   When ZONE-STATE and GAME are provided, uses spatial grid for O(1) cell lookup
   instead of O(n) linear scan of all players.

   NOTE: This is intentionally neighbor-cell biased for performance. When players
   exist in the 3x3 neighboring cells, returns the closest among those neighbors
   (not the globally closest in the zone). Zone-wide fallback only triggers when
   no players are found in neighbor cells or all grid IDs are stale. This means
   aggro is driven by local proximity. If true perception-range awareness is
   needed (global closest), consider a radius-based spatial query."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type npc npc))
  (let ((best nil)
        (best-dist nil)
        (grid (and zone-state (zone-state-player-grid zone-state))))
    ;; Use spatial grid query if available
    (if (and grid (npc-grid-cell-x npc) (npc-grid-cell-y npc) game)
        ;; O(1) cell lookup + local scan (9 cells) - Phase 2: allocation-free query
        (let ((count (spatial-grid-query-neighbors-into
                      grid
                      (npc-grid-cell-x npc)
                      (npc-grid-cell-y npc)
                      *spatial-scratch-vector*)))
          (declare (type fixnum count))
          ;; Scan neighbor cells if any IDs found
          (when (> count 0)
            (loop :for i fixnum :from 0 :below count
                  :for id fixnum = (aref *spatial-scratch-vector* i)
                  :for player = (find-player-by-id-fast game id)
                  ;; Players are always considered alive (Task 1.5: avoid CLOS dispatch)
                  :when player
                  :do (let* ((dx (- (player-x player) (npc-x npc)))
                             (dy (- (player-y player) (npc-y npc)))
                             (dist (+ (* dx dx) (* dy dy))))
                        (when (or (null best-dist) (< dist best-dist))
                          (setf best player
                                best-dist dist)))))
          ;; Fall back to zone-wide scan if no player found (empty cells or stale IDs)
          (when (and (null best) players)
            (loop :for player :across players
                  :do (let* ((dx (- (player-x player) (npc-x npc)))
                             (dy (- (player-y player) (npc-y npc)))
                             (dist (+ (* dx dx) (* dy dy))))
                        (when (or (null best-dist) (< dist best-dist))
                          (setf best player
                                best-dist dist))))))
        ;; No grid available: O(n) linear scan of all players
        (when players
          (loop :for player :across players
                ;; Players are always considered alive (Task 1.5: avoid CLOS dispatch)
                :do (let* ((dx (- (player-x player) (npc-x npc)))
                           (dy (- (player-y player) (npc-y npc)))
                           (dist (+ (* dx dx) (* dy dy))))
                      (when (or (null best-dist) (< dist best-dist))
                        (setf best player
                              best-dist dist))))))
    best))

(defun npc-home-radius (npc world)
  ;; Return NPC home radius in world pixels.
  (let* ((archetype (npc-archetype npc))
         (tiles (if archetype
                    (npc-archetype-home-radius-tiles archetype)
                    *npc-home-radius-tiles*)))
    (* tiles (world-tile-dest-size world))))

(defun npc-move-speed (npc)
  ;; Return NPC movement speed in pixels per second.
  (let ((archetype (npc-archetype npc)))
    (if archetype
        (npc-archetype-move-speed archetype)
        *npc-walk-speed*)))

(defun npc-flee-speed-mult (npc)
  ;; Return NPC flee speed multiplier.
  (let ((archetype (npc-archetype npc)))
    (if archetype
        (npc-archetype-flee-speed-mult archetype)
        *npc-flee-speed-mult*)))

(defun npc-wander-interval (npc)
  ;; Return NPC wander target interval in seconds.
  (let ((archetype (npc-archetype npc)))
    (if archetype
        (npc-archetype-wander-interval archetype)
        *npc-wander-interval*)))

(defun npc-pick-wander-target (npc world)
  ;; Pick a new wander target around the NPC home position.
  (let* ((radius (float (npc-home-radius npc world) 1.0f0))
         (angle (* 2.0f0 (float pi 1.0f0) (random 1.0f0)))
         (r (* (random 1.0f0) radius)))
    (if (<= radius 0.0f0)
        (setf (npc-wander-x npc) (float (npc-home-x npc) 1.0f0)
              (npc-wander-y npc) (float (npc-home-y npc) 1.0f0))
        (setf (npc-wander-x npc)
              (float (+ (npc-home-x npc) (* r (cos angle))) 1.0f0)
              (npc-wander-y npc)
              (float (+ (npc-home-y npc) (* r (sin angle))) 1.0f0)))
    (setf (npc-wander-timer npc) (float (npc-wander-interval npc) 1.0f0))))

(defun npc-wander-direction (npc world dt)
  ;; Return a normalized wander direction and update target timer.
  (let* ((timer (- (npc-wander-timer npc) dt))
         (arrive-dist *npc-wander-arrive-distance*)
         (arrive-sq (* arrive-dist arrive-dist)))
    (setf (npc-wander-timer npc) timer)
    (let* ((tx (npc-wander-x npc))
           (ty (npc-wander-y npc))
           (dx (- tx (npc-x npc)))
           (dy (- ty (npc-y npc))))
      (when (or (<= timer 0.0)
                (<= (+ (* dx dx) (* dy dy)) arrive-sq))
        (npc-pick-wander-target npc world)
        (setf tx (npc-wander-x npc)
              ty (npc-wander-y npc)
              dx (- tx (npc-x npc))
              dy (- ty (npc-y npc))))
      (normalize-vector dx dy))))

(defun npc-perception-range-sq (npc world)
  "Return squared perception range in world pixels."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type npc npc)
           (type world world))
  (let* ((archetype (npc-archetype npc))
         (tiles (if archetype
                    (float (npc-archetype-perception-tiles archetype) 1.0f0)
                    0.0f0))
         (range (* tiles (world-tile-dest-size world))))
    (declare (type single-float tiles range))
    (* range range)))

(defun npc-in-perception-range-p (npc player world)
  "Return true when the player is within the NPC perception radius."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type npc npc)
           (type world world))
  (when player
    (let* ((dx (- (player-x player) (npc-x npc)))
           (dy (- (player-y player) (npc-y npc)))
           (dist-sq (+ (* dx dx) (* dy dy)))
           (range-sq (npc-perception-range-sq npc world)))
      (declare (type single-float dx dy dist-sq range-sq))
      (and (> range-sq 0.0f0)
           (<= dist-sq range-sq)))))

(defun npc-should-flee-p (npc)
  ;; Return true when the NPC is low enough to flee.
  (let* ((archetype (npc-archetype npc))
         (flee-at (when archetype
                    (npc-archetype-flee-at-hits archetype))))
    (and flee-at
         (> flee-at 0)
         (<= (npc-hits-left npc) flee-at))))

(defun update-npc-behavior (npc player world)
  "Update NPC behavior state based on archetype rules and player range."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type npc npc)
           (type world world))
  (let* ((archetype (npc-archetype npc))
         (old-state (npc-behavior-state npc))
         (new-state nil)
         (provoked nil)
         (in-range nil))
    (cond
      ((not (npc-alive npc))
       (setf new-state :dead))
      ((not player)
       (setf new-state :idle))
      ((not archetype)
       (setf new-state :idle))
      (t
       (let ((aggro-mode (npc-archetype-aggro-mode archetype)))
         (setf provoked (npc-provoked npc)
               in-range (or provoked
                            (npc-in-perception-range-p npc player world)))
         (setf new-state
               (cond
                 ((npc-should-flee-p npc) :flee)
                 ((and provoked in-range) :retaliate)
                 ((and in-range (eq aggro-mode :always)) :aggressive)
                 ((and in-range (eq aggro-mode :provoked) provoked) :aggressive)
                 (t :idle))))))
    (setf (npc-behavior-state npc) new-state)
    ;; Mark snapshot-dirty when behavior state changes (delta compression)
    (when (not (eq old-state new-state))
      (setf (npc-snapshot-dirty npc) t))
    (when (and *debug-npc-logs* (not (eq old-state new-state)))
      (let* ((flee-at (if archetype
                          (npc-archetype-flee-at-hits archetype)
                          0))
             (name (if archetype (npc-archetype-name archetype) "NPC")))
        (format t "~&NPC-STATE ~a ~a->~a hits-left=~d flee-at=~d provoked=~a in-range=~a~%"
                name old-state new-state (npc-hits-left npc) flee-at provoked in-range)
        (finish-output)))))

(defun update-npc-intent (npc player world dt)
  "Populate the NPC intent based on behavior and proximity.
   When PLAYER is nil, NPC runs idle/wander behavior."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type npc npc)
           (type world world)
           (type single-float dt))
  (when (npc-alive npc)
    (let* ((intent (npc-intent npc))
           (state (cond
                    ;; Flee requires a player to flee FROM - gate on player
                    ((and player (npc-should-flee-p npc)) :flee)
                    ;; No player nearby = idle/wander
                    ((null player) :idle)
                    ;; Otherwise use behavior state
                    (t (npc-behavior-state npc))))
           (attack-range (npc-attack-range npc world))
           (attack-range-sq (* attack-range attack-range))
           (dx 0.0f0)
           (dy 0.0f0)
           (face-dx 0.0f0)
           (face-dy 0.0f0)
           (attack-request nil))
      (declare (type single-float attack-range attack-range-sq dx dy face-dx face-dy))
      (let* ((home-radius (npc-home-radius npc world))
             (home-dx (- (npc-home-x npc) (npc-x npc)))
             (home-dy (- (npc-home-y npc) (npc-y npc)))
             (home-dist-sq (+ (* home-dx home-dx) (* home-dy home-dy)))
             (home-radius-sq (* home-radius home-radius)))
        (if (and (not (eq state :flee))
                 (> home-radius 0.0)
                 (> home-dist-sq home-radius-sq))
            (progn
              (setf face-dx home-dx
                    face-dy home-dy)
              (multiple-value-setq (dx dy)
                (normalize-vector home-dx home-dy)))
            (case state
              (:flee
               (let ((vx (- (npc-x npc) (player-x player)))
                     (vy (- (npc-y npc) (player-y player))))
                 (setf face-dx vx
                       face-dy vy)
                 (if (and (zerop vx) (zerop vy))
                     (setf dx 1.0
                           dy 0.0)
                     (multiple-value-setq (dx dy)
                       (normalize-vector vx vy)))))
              ((:aggressive :retaliate)
               (let* ((vx (- (player-x player) (npc-x npc)))
                      (vy (- (player-y player) (npc-y npc)))
                      (dist-sq (+ (* vx vx) (* vy vy))))
                 (setf face-dx vx
                       face-dy vy)
                 (if (<= dist-sq attack-range-sq)
                     (setf dx 0.0
                           dy 0.0
                           attack-request t)
                     (multiple-value-setq (dx dy)
                       (normalize-vector vx vy)))))
              (t
               (multiple-value-setq (dx dy)
                 (npc-wander-direction npc world dt))
               (setf face-dx dx
                     face-dy dy)))))
      (set-intent-move intent dx dy)
      (set-intent-face intent face-dx face-dy)
      (when attack-request
        (setf (intent-attack intent) t)))))

(defun update-npc-movement (npc world dt &optional zone-state)
  "Move NPC based on intent and keep it near its home radius.
   When ZONE-STATE is provided, use per-zone collision bounds and wall-map."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type npc npc)
           (type world world)
           (type single-float dt))
  (when (npc-alive npc)
    (let* ((intent (npc-intent npc))
           (state (npc-behavior-state npc))
           (speed (float (npc-move-speed npc) 1.0f0))
           (flee-mult (float (npc-flee-speed-mult npc) 1.0f0))
           (dx (intent-move-dx intent))
           (dy (intent-move-dy intent))
           (face-dx (intent-face-dx intent))
           (face-dy (intent-face-dy intent)))
      (declare (type single-float speed flee-mult dx dy face-dx face-dy))
      (when (eq state :flee)
        (setf speed (* speed (max 1.0 flee-mult))))
      (when (or (not (zerop face-dx))
                (not (zerop face-dy)))
        (setf (npc-facing npc) (player-direction face-dx face-dy)))
      (when (or (not (zerop dx))
                (not (zerop dy)))
        (multiple-value-bind (half-w half-h)
            (npc-collision-half world)
          ;; Use per-zone collision if zone-state is available, otherwise fallback to world
          (let* ((tile-size (world-tile-dest-size world))
                 (zone-wall-map (and zone-state (zone-state-wall-map zone-state)))
                 (zone (and zone-state (zone-state-zone zone-state))))
            (if zone-wall-map
                ;; Per-zone collision with zone-state wall-map
                (multiple-value-bind (min-x max-x min-y max-y)
                    (zone-bounds-zero-origin tile-size
                                              (zone-width zone)
                                              (zone-height zone)
                                              half-w half-h)
                  (multiple-value-bind (nx ny out-dx out-dy)
                      (attempt-move-with-map zone-wall-map
                                              (npc-x npc) (npc-y npc)
                                              dx dy (* speed dt)
                                              half-w half-h tile-size)
                    (declare (ignore out-dx out-dy))
                    (let ((old-x (npc-x npc))
                          (old-y (npc-y npc)))
                      (setf (npc-x npc) (float (clamp nx min-x max-x) 1.0f0)
                            (npc-y npc) (float (clamp ny min-y max-y) 1.0f0))
                      (when (or (/= old-x (npc-x npc)) (/= old-y (npc-y npc)))
                        (setf (npc-snapshot-dirty npc) t)
                        ;; Update spatial grid if cell changed
                        (let ((grid (zone-state-npc-grid zone-state)))
                          (when grid
                            (multiple-value-bind (new-cx new-cy changed)
                                (spatial-grid-move grid (npc-id npc)
                                                   (npc-grid-cell-x npc)
                                                   (npc-grid-cell-y npc)
                                                   (npc-x npc) (npc-y npc))
                              (when changed
                                (setf (npc-grid-cell-x npc) new-cx
                                      (npc-grid-cell-y npc) new-cy)))))))))
                ;; Fallback to global world collision
                (multiple-value-bind (nx ny out-dx out-dy)
                    (attempt-move world
                                  (npc-x npc) (npc-y npc)
                                  dx dy (* speed dt)
                                  half-w half-h tile-size)
                  (declare (ignore out-dx out-dy))
                  (let ((old-x (npc-x npc))
                        (old-y (npc-y npc)))
                    (setf (npc-x npc) (float (clamp nx (world-wall-min-x world)
                                                    (world-wall-max-x world))
                                             1.0f0)
                          (npc-y npc) (float (clamp ny (world-wall-min-y world)
                                                    (world-wall-max-y world))
                                             1.0f0))
                    (when (or (/= old-x (npc-x npc)) (/= old-y (npc-y npc)))
                      (setf (npc-snapshot-dirty npc) t)))))))))))
