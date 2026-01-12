(in-package #:mmorpg)

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
  ;; Return squared perception range in world pixels.
  (let* ((archetype (npc-archetype npc))
         (tiles (if archetype
                    (npc-archetype-perception-tiles archetype)
                    0.0))
         (range (* tiles (world-tile-dest-size world))))
    (* range range)))

(defun npc-in-perception-range-p (npc player world)
  ;; Return true when the player is within the NPC perception radius.
  (let* ((dx (- (player-x player) (npc-x npc)))
         (dy (- (player-y player) (npc-y npc)))
         (dist-sq (+ (* dx dx) (* dy dy)))
         (range-sq (npc-perception-range-sq npc world)))
    (and (> range-sq 0.0)
         (<= dist-sq range-sq))))

(defun update-npc-behavior (npc player world)
  ;; Update NPC behavior state based on archetype rules and player range.
  (let ((archetype (npc-archetype npc)))
    (cond
      ((not (npc-alive npc))
       (setf (npc-behavior-state npc) :dead))
      ((not archetype)
       (setf (npc-behavior-state npc) :idle))
      (t
       (let* ((provoked (npc-provoked npc))
              (aggro-mode (npc-archetype-aggro-mode archetype))
              (retaliate (npc-archetype-retaliate archetype))
              (flee-at (npc-archetype-flee-at-hits archetype))
              (in-range (npc-in-perception-range-p npc player world)))
         (cond
           ((and (> flee-at 0)
                 (<= (npc-hits-left npc) flee-at))
            (setf (npc-behavior-state npc) :flee))
           ((and provoked retaliate in-range)
            (setf (npc-behavior-state npc) :retaliate))
           ((and in-range (eq aggro-mode :always))
            (setf (npc-behavior-state npc) :aggressive))
           ((and in-range (eq aggro-mode :provoked) provoked)
            (setf (npc-behavior-state npc) :aggressive))
           (t
            (setf (npc-behavior-state npc) :idle))))))))

(defun update-npc-intent (npc player world dt)
  ;; Populate the NPC intent based on behavior and proximity.
  (when (npc-alive npc)
    (let* ((intent (npc-intent npc))
           (state (npc-behavior-state npc))
           (attack-range (npc-attack-range npc world))
           (attack-range-sq (* attack-range attack-range))
           (dx 0.0)
           (dy 0.0)
           (face-dx 0.0)
           (face-dy 0.0)
           (attack-request nil))
      (let* ((home-radius (npc-home-radius npc world))
             (home-dx (- (npc-home-x npc) (npc-x npc)))
             (home-dy (- (npc-home-y npc) (npc-y npc)))
             (home-dist-sq (+ (* home-dx home-dx) (* home-dy home-dy)))
             (home-radius-sq (* home-radius home-radius)))
        (if (and (> home-radius 0.0)
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

(defun update-npc-movement (npc world dt)
  ;; Move NPC based on intent and keep it near its home radius.
  (when (npc-alive npc)
    (let* ((intent (npc-intent npc))
           (state (npc-behavior-state npc))
           (speed (npc-move-speed npc))
           (flee-mult (npc-flee-speed-mult npc))
           (dx (intent-move-dx intent))
           (dy (intent-move-dy intent))
           (face-dx (intent-face-dx intent))
           (face-dy (intent-face-dy intent)))
      (when (eq state :flee)
        (setf speed (* speed (max 1.0 flee-mult))))
      (when (or (not (zerop face-dx))
                (not (zerop face-dy)))
        (setf (npc-facing npc) (player-direction face-dx face-dy)))
      (when (or (not (zerop dx))
                (not (zerop dy)))
        (multiple-value-bind (half-w half-h)
            (npc-collision-half world)
          (multiple-value-bind (nx ny out-dx out-dy)
              (attempt-move world
                            (npc-x npc)
                            (npc-y npc)
                            dx dy (* speed dt)
                            half-w half-h
                            (world-tile-dest-size world))
            (declare (ignore out-dx out-dy))
            (setf (npc-x npc) (float (clamp nx (world-wall-min-x world)
                                            (world-wall-max-x world))
                                     1.0f0)
                  (npc-y npc) (float (clamp ny (world-wall-min-y world)
                                            (world-wall-max-y world))
                                     1.0f0))))))))
