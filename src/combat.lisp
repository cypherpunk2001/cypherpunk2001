;; NOTE: If you change behavior here, update docs/combat.md :)
(in-package #:mmorpg)

(defmethod combatant-position ((combatant player))
  (values (player-x combatant) (player-y combatant)))

(defmethod combatant-position ((combatant npc))
  (values (npc-x combatant) (npc-y combatant)))

(defmethod combatant-alive-p ((combatant player))
  t)

(defmethod combatant-alive-p ((combatant npc))
  (npc-alive combatant))

(defmethod combatant-collision-half ((combatant player) world)
  (values (world-collision-half-width world)
          (world-collision-half-height world)))

(defmethod combatant-collision-half ((combatant npc) world)
  (declare (ignore combatant))
  (npc-collision-half world))

(defmethod combatant-health ((combatant player))
  (values (player-hp combatant)
          (combatant-max-hp combatant)))

(defmethod combatant-health ((combatant npc))
  (values (npc-hits-left combatant)
          (combatant-max-hp combatant)))

(defmethod combatant-apply-hit ((combatant player) &optional amount)
  (let* ((damage (if amount amount 1))
         (hp (- (player-hp combatant) damage)))
    (setf (player-hp combatant) (max 0 hp))))

(defmethod combatant-apply-hit ((combatant npc) &optional amount)
  (let* ((damage (if amount amount 1))
         (max-hits (combatant-max-hp combatant))
         (before (npc-hits-left combatant)))
    (when (and max-hits (> (npc-hits-left combatant) max-hits))
      (setf (npc-hits-left combatant) max-hits))
    (decf (npc-hits-left combatant) damage)
    (setf (npc-provoked combatant) t)
    (let ((killed nil))
      (when (<= (npc-hits-left combatant) 0)
        (setf (npc-hits-left combatant) 0
              (npc-alive combatant) nil
              killed t))
      (when *debug-npc-logs*
        (let* ((archetype (npc-archetype combatant))
               (name (if archetype (npc-archetype-name archetype) "NPC"))
               (flee-at (if archetype
                            (npc-archetype-flee-at-hits archetype)
                            0)))
          (format t "~&NPC-HIT ~a hits-left=~d->~d flee-at=~d alive=~a state=~a~%"
                  name
                  before
                  (npc-hits-left combatant)
                  flee-at
                  (npc-alive combatant)
                  (npc-behavior-state combatant))
          (finish-output)))
      killed)))

(defmethod combatant-trigger-hit-effect ((combatant player))
  (setf (player-hit-active combatant) t
        (player-hit-timer combatant) 0.0
        (player-hit-frame combatant) 0
        (player-hit-facing combatant) (player-facing combatant)
        (player-hit-facing-sign combatant) (player-facing-sign combatant)))

(defmethod combatant-trigger-hit-effect ((combatant npc))
  (setf (npc-hit-active combatant) t
        (npc-hit-timer combatant) 0.0
        (npc-hit-frame combatant) 0
        (npc-hit-facing combatant) (npc-facing combatant)
        (npc-hit-facing-sign combatant) 1.0))

(defmethod combatant-update-hit-effect ((combatant player) dt)
  (when (player-hit-active combatant)
    (let* ((frame-count *blood-frame-count*)
           (frame-time *blood-frame-time*)
           (timer (+ (player-hit-timer combatant) dt))
           (duration (* frame-count frame-time))
           (frame (min (truncate (/ timer frame-time))
                       (1- frame-count))))
      (setf (player-hit-timer combatant) timer
            (player-hit-frame combatant) frame)
      (when (>= timer duration)
        (setf (player-hit-active combatant) nil
              (player-hit-timer combatant) 0.0
              (player-hit-frame combatant) 0)))))

(defmethod combatant-update-hit-effect ((combatant npc) dt)
  (when (npc-hit-active combatant)
    (let* ((frame-count *blood-frame-count*)
           (frame-time *blood-frame-time*)
           (timer (+ (npc-hit-timer combatant) dt))
           (duration (* frame-count frame-time))
           (frame (min (truncate (/ timer frame-time))
                       (1- frame-count))))
      (setf (npc-hit-timer combatant) timer
            (npc-hit-frame combatant) frame)
      (when (>= timer duration)
        (setf (npc-hit-active combatant) nil
              (npc-hit-timer combatant) 0.0
              (npc-hit-frame combatant) 0)))))

(defun combatant-display-name (combatant)
  ;; Return a short display name for combat logs.
  (typecase combatant
    (player "Player")
    (npc (let ((archetype (npc-archetype combatant)))
           (if archetype
               (npc-archetype-name archetype)
               "NPC")))
    (t "Entity")))

(defun format-combat-log (attacker defender hit chance roll attack-level defense-level
                          &key damage xp-text killed)
  ;; Build a combat log line for a hit or miss.
  (let ((chance-pct (round (* chance 100.0)))
        (roll-pct (round (* roll 100.0))))
    (if hit
        (format nil "~a -> ~a: hit ~d (c~d r~d atk~d def~d)~@[ ~a~]~@[ KILL~]"
                attacker defender damage chance-pct roll-pct
                attack-level defense-level xp-text killed)
        (format nil "~a -> ~a: miss (c~d r~d atk~d def~d)~@[ ~a~]"
                attacker defender chance-pct roll-pct
                attack-level defense-level xp-text))))

(defun emit-combat-log (ui text)
  ;; Emit combat log lines to the UI buffer and stdout in debug mode.
  (when (and ui *debug-collision-overlay* text)
    (ui-push-combat-log ui text)
    (format t "~&COMBAT ~a~%" text)
    (finish-output)))

(defun push-combat-log (ui attacker defender hit chance roll attack-level defense-level
                           &key damage xp-text killed)
  ;; Build and emit a combat log line when debug is enabled.
  (emit-combat-log ui
                   (format-combat-log
                    (combatant-display-name attacker)
                    (combatant-display-name defender)
                    hit chance roll
                    attack-level defense-level
                    :damage damage
                    :xp-text xp-text
                    :killed killed)))

(defun aabb-overlap-p (ax ay ahw ahh bx by bhw bhh)
  ;; Return true when two axis-aligned boxes overlap (center + half sizes).
  (and (<= (abs (- ax bx)) (+ ahw bhw))
       (<= (abs (- ay by)) (+ ahh bhh))))

(defun attack-hitbox (player world)
  ;; Return attack hitbox center and half sizes for the current facing.
  (let* ((tile-size (world-tile-dest-size world))
         (half (* (/ tile-size 2.0) *attack-hitbox-scale*))
         (offset (+ (world-collision-half-width world) half))
         (x (player-x player))
         (y (player-y player))
         (direction (player-facing player))
         (side-sign (player-facing-sign player)))
    (case direction
      (:up (values x (- y offset) half half))
      (:down (values x (+ y offset) half half))
      (:side (values (+ x (* side-sign offset)) y half half))
      (t (values x y half half)))))

(defun npc-attack-range (npc world)
  ;; Return NPC melee range in world pixels (center-to-center).
  (let* ((archetype (npc-archetype npc))
         (tiles (if archetype
                    (npc-archetype-attack-range-tiles archetype)
                    *npc-attack-range-tiles*))
         (base (* tiles (world-tile-dest-size world)))
         (player-half (max (world-collision-half-width world)
                           (world-collision-half-height world))))
    (multiple-value-bind (npc-half-w npc-half-h)
        (npc-collision-half world)
      (+ base player-half (max npc-half-w npc-half-h)))))

(defun npc-attack-cooldown (npc)
  ;; Return NPC melee cooldown in seconds.
  (let ((archetype (npc-archetype npc)))
    (if archetype
        (npc-archetype-attack-cooldown archetype)
        *npc-attack-cooldown*)))

(defun npc-attack-damage (npc)
  ;; Return NPC melee damage per hit.
  (let ((archetype (npc-archetype npc)))
    (if archetype
        (npc-archetype-attack-damage archetype)
        *npc-attack-damage*)))

(defun intent-attack-direction (player intent)
  ;; Choose an attack direction from intent input or target.
  (let ((dx (intent-move-dx intent))
        (dy (intent-move-dy intent)))
    (cond
      ((or (not (zerop dx)) (not (zerop dy)))
       (values (player-direction dx dy) dx))
      ((intent-target-active intent)
       (let* ((tx (intent-target-x intent))
              (ty (intent-target-y intent))
              (tdx (- tx (player-x player)))
              (tdy (- ty (player-y player))))
         (if (or (not (zerop tdx)) (not (zerop tdy)))
             (values (player-direction tdx tdy) tdx)
             (values nil 0.0))))
      (t (values nil 0.0)))))

(defun start-player-attack (player intent)
  ;; Start an attack animation if one is not already active.
  (unless (player-attacking player)
    (multiple-value-bind (direction side-dx)
        (intent-attack-direction player intent)
      (when direction
        (setf (player-facing player) direction)
        (when (eq direction :side)
          (setf (player-facing-sign player)
                (if (> side-dx 0.0) 1.0 -1.0)))))
    (setf (player-attacking player) t
          (player-attack-timer player) 0.0
          (player-attack-hit player) nil)))

(defun apply-melee-hit (player target world ui)
  ;; Apply melee damage once per attack if the hitbox overlaps the target.
  (when (and (player-attacking player)
             (not (player-attack-hit player))
             (combatant-alive-p target))
    (multiple-value-bind (ax ay ahw ahh)
        (attack-hitbox player world)
      (multiple-value-bind (thw thh)
          (combatant-collision-half target world)
        (multiple-value-bind (tx ty)
            (combatant-position target)
          (when (aabb-overlap-p ax ay ahw ahh
                                tx ty thw thh)
            (setf (player-attack-hit player) t)
            (multiple-value-bind (hit chance roll)
                (roll-melee-hit player target)
              (let ((attack-level (combatant-attack-level player))
                    (defense-level (combatant-defense-level target)))
                (if hit
                    (let* ((damage (roll-melee-damage player))
                           (killed (combatant-apply-hit target damage))
                           (xp-text nil))
                      (combatant-trigger-hit-effect target)
                      (multiple-value-bind (attack-xp strength-xp defense-xp hitpoints-xp)
                          (award-combat-xp player (* damage *xp-per-damage*))
                        (setf xp-text (format-xp-awards attack-xp strength-xp
                                                       defense-xp hitpoints-xp)))
                      (push-combat-log ui player target t chance roll
                                       attack-level defense-level
                                       :damage damage
                                       :xp-text xp-text
                                       :killed killed)
                      (when killed
                        (let ((kill-xp (npc-kill-xp target)))
                          (when (> kill-xp 0)
                            (multiple-value-bind (k-attack k-strength k-defense k-hp)
                                (award-combat-xp player kill-xp)
                              (let ((kill-text (format-xp-awards k-attack k-strength
                                                                 k-defense k-hp)))
                                (when kill-text
                                  (emit-combat-log ui
                                                   (format nil "Kill XP: ~a"
                                                           kill-text)))))))
                        (award-npc-loot player target)))
                    (push-combat-log ui player target nil chance roll
                                     attack-level defense-level
                                     :xp-text "XP 0"))))))))))

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
    (when (and (eq direction :side) (not (zerop dx)))
      (setf (player-facing-sign player) (if (> dx 0.0) 1.0 -1.0)))
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

(defun update-npc-attack (npc player world dt ui)
  ;; Handle NPC melee attacks and cooldowns.
  (when (npc-alive npc)
    (let* ((intent (npc-intent npc))
           (timer (max 0.0 (- (npc-attack-timer npc) dt)))
           (state (npc-behavior-state npc))
           (attack-range (npc-attack-range npc world))
           (attack-range-sq (* attack-range attack-range)))
      (setf (npc-attack-timer npc) timer)
      (when (and (intent-attack intent)
                 (<= timer 0.0)
                 (or (eq state :aggressive)
                     (eq state :retaliate)))
        (let* ((dx (- (player-x player) (npc-x npc)))
               (dy (- (player-y player) (npc-y npc)))
               (dist-sq (+ (* dx dx) (* dy dy))))
          (when (<= dist-sq attack-range-sq)
            (multiple-value-bind (hit chance roll)
                (roll-melee-hit npc player)
              (let ((attack-level (combatant-attack-level npc))
                    (defense-level (combatant-defense-level player)))
                (if hit
                    (let ((damage (roll-melee-damage npc (npc-attack-damage npc))))
                      (combatant-apply-hit player damage)
                      (combatant-trigger-hit-effect player)
                      (push-combat-log ui npc player t chance roll
                                       attack-level defense-level
                                       :damage damage))
                    (push-combat-log ui npc player nil chance roll
                                     attack-level defense-level
                                     :xp-text "XP 0")))))
            (setf (npc-attack-timer npc) (npc-attack-cooldown npc)))))))

(defun update-npc-animation (npc dt)
  ;; Advance idle animation frames for the NPC.
  (when (npc-alive npc)
    (let* ((frame-count *idle-frame-count*)
           (frame-time *idle-frame-time*)
           (frame-index (npc-frame-index npc))
           (frame-timer (npc-frame-timer npc)))
      (incf frame-timer dt)
      (loop :while (>= frame-timer frame-time)
            :do (decf frame-timer frame-time)
                (setf frame-index
                      (mod (1+ frame-index) frame-count)))
      (setf (npc-frame-index npc) frame-index
            (npc-frame-timer npc) frame-timer))))

(defmethod update-entity-animation ((entity player) dt)
  (update-player-animation entity dt))

(defmethod update-entity-animation ((entity npc) dt)
  (update-npc-animation entity dt))
