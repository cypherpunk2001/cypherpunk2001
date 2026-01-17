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
         (old-hp (player-hp combatant))
         (hp (- old-hp damage))
         (new-hp (max 0 hp)))
    (setf (player-hp combatant) new-hp)
    ;; Tier-1 write: player death (HP reaches 0) must be saved immediately
    ;; to prevent logout-to-survive exploit
    ;; Use aggressive retry with exponential backoff (10 retries over ~10s)
    (when (and (= new-hp 0) (> old-hp 0))
      (with-retry-exponential (saved (lambda () (db-save-player-immediate combatant))
                                :max-retries 10
                                :initial-delay 100
                                :max-delay 2000
                                :on-final-fail (lambda (e)
                                                 (warn "CRITICAL: Death save FAILED for player ~d after 10 retries: ~a - using dirty flag fallback"
                                                       (player-id combatant) e)
                                                 ;; Fallback to dirty flag - will save within 30s if server survives
                                                 (mark-player-dirty (player-id combatant))))))
    ;; Tier-2 write: HP changes should be marked dirty for batched saves
    (when (/= hp new-hp)
      (mark-player-dirty (player-id combatant)))))

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
      (when killed
        (let ((respawn (npc-respawn-seconds combatant)))
          (when (and respawn (> respawn 0.0))
            (setf (npc-respawn-timer combatant) respawn))))
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

(defun emit-combat-log (event-queue text)
  ;; Emit combat log events to the queue and stdout in debug mode (server-side).
  (when (and event-queue text)
    (emit-combat-log-event event-queue text)
    (when *debug-collision-overlay*
      (format t "~&COMBAT ~a~%" text)
      (finish-output))))

(defun emit-hud-message (event-queue text)
  ;; Emit a HUD feedback event to the queue (server-side).
  (when (and event-queue text)
    (emit-hud-message-event event-queue text)))

(defun emit-level-up-messages (event-queue level-ups)
  ;; Emit HUD message events for stat level-ups (server-side).
  (dolist (entry level-ups)
    (let* ((stat (car entry))
           (level (cdr entry))
           (label (case stat
                    (:attack "Attack")
                    (:strength "Strength")
                    (:defense "Defense")
                    (:hitpoints "Hitpoints")
                    (t "Skill"))))
      (emit-hud-message event-queue
                        (format nil "Congratulations! ~a level ~d."
                                label level)))))

(defun push-combat-log (event-queue attacker defender hit chance roll attack-level defense-level
                           &key damage xp-text killed)
  ;; Build and emit a combat log event (server-side).
  (emit-combat-log event-queue
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

(defun find-npc-by-id (npcs id)
  ;; Return the NPC with ID, if present.
  (when (> id 0)
    (loop :for npc :across npcs
          :when (= (npc-id npc) id)
            :do (return npc))))

(defun npc-respawn-seconds (npc)
  ;; Return the respawn cooldown for NPC.
  (let ((archetype (npc-archetype npc)))
    (if archetype
        (npc-archetype-respawn-seconds archetype)
        *npc-respawn-seconds*)))

(defun respawn-npc (npc)
  ;; Reset NPC state after a respawn cooldown.
  (let* ((archetype (npc-archetype npc))
         (stats (make-npc-stats archetype))
         (max-hp (stat-block-base-level stats :hitpoints))
         (x (npc-home-x npc))
         (y (npc-home-y npc))
         (intent (npc-intent npc)))
    (reset-frame-intent intent)
    (clear-intent-target intent)
    (setf (npc-x npc) x
          (npc-y npc) y
          (npc-stats npc) stats
          (npc-anim-state npc) :idle
          (npc-facing npc) :down
          (npc-behavior-state npc) :idle
          (npc-provoked npc) nil
          (npc-wander-x npc) x
          (npc-wander-y npc) y
          (npc-wander-timer npc) 0.0
          (npc-attack-timer npc) 0.0
          (npc-frame-index npc) 0
          (npc-frame-timer npc) 0.0
          (npc-hits-left npc) max-hp
          (npc-alive npc) t
          (npc-respawn-timer npc) 0.0
          (npc-hit-active npc) nil
          (npc-hit-timer npc) 0.0
          (npc-hit-frame npc) 0
          (npc-hit-facing npc) :down
          (npc-hit-facing-sign npc) 1.0))
  (when *debug-npc-logs*
    (let* ((archetype (npc-archetype npc))
           (name (if archetype (npc-archetype-name archetype) "NPC")))
      (format t "~&NPC-RESPAWN ~a at ~,1f ~,1f~%" name (npc-x npc) (npc-y npc))
      (finish-output))))

(defun update-npc-respawns (npcs dt)
  ;; Tick respawn timers and restore NPCs when timers expire.
  (loop :for npc :across npcs
        :when (and (not (npc-alive npc))
                   (> (npc-respawn-timer npc) 0.0))
          :do (let ((timer (max 0.0 (- (npc-respawn-timer npc) dt))))
                (setf (npc-respawn-timer npc) timer)
                (when (<= timer 0.0)
                  (respawn-npc npc)))))

(defun player-attack-target (player npcs)
  ;; Return the active NPC target for PLAYER, if any.
  (let* ((id (player-attack-target-id player))
         (npc (and (> id 0) (find-npc-by-id npcs id))))
    (when (and npc (combatant-alive-p npc))
      npc)))

(defun player-follow-target (player npcs)
  ;; Return the active NPC follow target for PLAYER, if any.
  (let* ((id (player-follow-target-id player))
         (npc (and (> id 0) (find-npc-by-id npcs id))))
    (when (and npc (combatant-alive-p npc))
      npc)))

(defun target-in-range-p (player npc world)
  "Check if NPC is within targeting range of PLAYER."
  (let* ((px (player-x player))
         (py (player-y player))
         (nx (npc-x npc))
         (ny (npc-y npc))
         (dx (- nx px))
         (dy (- ny py))
         (dist-sq (+ (* dx dx) (* dy dy)))
         (tile-size (world-tile-dest-size world))
         (max-dist (* *max-target-distance-tiles* tile-size))
         (max-dist-sq (* max-dist max-dist)))
    (<= dist-sq max-dist-sq)))

(defun sync-player-attack-target (player intent npcs world)
  ;; Validate requested attack target and set authoritative state (server authority).
  ;; Checks range to prevent targeting distant NPCs.
  (let* ((requested-id (intent-requested-attack-target-id intent))
         (current-id (player-attack-target-id player)))
    ;; Process clear request (requested-id = 0 means client wants to cancel)
    (when (and requested-id (= requested-id 0) (> current-id 0))
      (setf (player-attack-target-id player) 0)
      (clear-intent-target intent))
    ;; Process new attack target request
    (when (and requested-id (> requested-id 0) (not (= requested-id current-id)))
      (let ((npc (find-npc-by-id npcs requested-id)))
        (if (and npc (combatant-alive-p npc) (target-in-range-p player npc world))
            (progn
              ;; Valid target in range: set authoritative state and clear conflicting targets
              (setf (player-attack-target-id player) requested-id
                    (player-follow-target-id player) 0
                    (player-pickup-target-id player) nil
                    (player-pickup-target-active player) nil)
              (clear-player-auto-walk player))
            ;; Invalid target or out of range: reject request
            (clear-requested-attack-target intent))))
    ;; Sync current authoritative target to intent position
    (let ((target (player-attack-target player npcs)))
      (if target
          (set-intent-target intent (npc-x target) (npc-y target))
          (when (> (player-attack-target-id player) 0)
            (setf (player-attack-target-id player) 0)
            (clear-intent-target intent))))))

(defun sync-player-follow-target (player intent npcs world)
  ;; Validate requested follow target and set authoritative state (server authority).
  ;; Checks range to prevent targeting distant NPCs.
  (let* ((requested-id (intent-requested-follow-target-id intent))
         (current-id (player-follow-target-id player)))
    ;; Process clear request (requested-id = 0 means client wants to cancel)
    (when (and requested-id (= requested-id 0) (> current-id 0))
      (setf (player-follow-target-id player) 0)
      (clear-intent-target intent))
    ;; Process new follow target request
    (when (and requested-id (> requested-id 0) (not (= requested-id current-id)))
      (let ((npc (find-npc-by-id npcs requested-id)))
        (if (and npc (combatant-alive-p npc) (target-in-range-p player npc world))
            (progn
              ;; Valid target in range: set authoritative state and clear conflicting targets
              (setf (player-follow-target-id player) requested-id
                    (player-attack-target-id player) 0
                    (player-pickup-target-id player) nil
                    (player-pickup-target-active player) nil)
              (clear-player-auto-walk player))
            ;; Invalid target: reject request
            (clear-requested-follow-target intent))))
    ;; Sync current authoritative target to intent position
    (let ((target (player-follow-target player npcs)))
      (if target
          (set-intent-target intent (npc-x target) (npc-y target))
          (when (> (player-follow-target-id player) 0)
            (setf (player-follow-target-id player) 0)
            (clear-intent-target intent))))))

(defun pickup-tile-in-range-p (player tx ty world)
  "Check if player is standing on tile TX,TY (same tile = can pickup)."
  (let* ((tile-size (world-tile-dest-size world))
         (px (player-x player))
         (py (player-y player))
         (player-tx (floor px tile-size))
         (player-ty (floor py tile-size)))
    (and (= player-tx tx) (= player-ty ty))))

(defun sync-player-pickup-target (player intent world)
  ;; Validate requested pickup target and set authoritative state (server authority).
  ;; Checks range to prevent targeting distant pickup objects.
  (let* ((requested-id (intent-requested-pickup-target-id intent))
         (requested-tx (intent-requested-pickup-tx intent))
         (requested-ty (intent-requested-pickup-ty intent))
         (current-id (player-pickup-target-id player)))
    ;; Process clear request (requested-id = nil means client wants to cancel)
    (when (and (not requested-id) current-id)
      (setf (player-pickup-target-id player) nil
            (player-pickup-target-active player) nil))
    ;; Process new pickup target request
    (when (and requested-id requested-tx requested-ty
               (not (and (eql requested-id current-id)
                         (eql requested-tx (player-pickup-target-tx player))
                         (eql requested-ty (player-pickup-target-ty player)))))
      ;; Validate range before accepting pickup target
      (if (pickup-tile-in-range-p player requested-tx requested-ty world)
          ;; Valid target in range: set authoritative state
          (progn
            (setf (player-pickup-target-id player) requested-id
                  (player-pickup-target-tx player) requested-tx
                  (player-pickup-target-ty player) requested-ty
                  (player-pickup-target-active player) t
                  (player-attack-target-id player) 0
                  (player-follow-target-id player) 0)
            (clear-player-auto-walk player))
          ;; Out of range: reject request (log for debugging)
          (log-verbose "Pickup target rejected: tile (~d,~d) out of range for player ~a"
                       requested-tx requested-ty (player-id player))))))

(defun player-attack-target-in-range-p (player target world)
  ;; Return true when TARGET is inside the player's melee hitbox.
  (multiple-value-bind (ax ay ahw ahh)
      (attack-hitbox player world)
    (multiple-value-bind (thw thh)
        (combatant-collision-half target world)
      (multiple-value-bind (tx ty)
          (combatant-position target)
        (aabb-overlap-p ax ay ahw ahh
                        tx ty thw thh)))))

(defun update-player-attack-intent (player npcs world)
  ;; Request attacks when the active attack target is in range.
  (let* ((intent (player-intent player))
         (target (player-attack-target player npcs)))
    (when (and target (player-attack-target-in-range-p player target world))
      (request-intent-attack intent))))

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

(defun apply-melee-hit (player target world event-queue)
  ;; Apply melee damage once per attack if the hitbox overlaps the target (server-side).
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
              (let ((damage nil)
                    (killed nil)
                    (xp-text nil))
                (if hit
                    (progn
                      (setf damage (roll-melee-damage player)
                            killed (combatant-apply-hit target damage))
                      (combatant-trigger-hit-effect target)
                      (let ((old-combat (combat-level (player-stats player))))
                        (multiple-value-bind (attack-xp strength-xp defense-xp hitpoints-xp level-ups)
                            (award-combat-xp player (* damage *xp-per-damage*))
                          (setf xp-text (format-xp-awards attack-xp strength-xp
                                                          defense-xp hitpoints-xp))
                          (emit-level-up-messages event-queue level-ups)
                          (let ((new-combat (combat-level (player-stats player))))
                            (when (> new-combat old-combat)
                              (emit-hud-message event-queue
                                                (format nil
                                                        "Congratulations! Combat level ~d."
                                                        new-combat))))))
                      (push-combat-log event-queue player target t chance roll
                                       (combatant-attack-level player)
                                       (combatant-defense-level target)
                                       :damage damage
                                       :xp-text xp-text
                                       :killed killed)
                      (when killed
                        (let ((kill-xp (npc-kill-xp target)))
                          (when (> kill-xp 0)
                            (let ((old-combat (combat-level (player-stats player))))
                              (multiple-value-bind (k-attack k-strength k-defense k-hp level-ups)
                                  (award-combat-xp player kill-xp)
                                (let ((kill-text (format-xp-awards k-attack k-strength
                                                                   k-defense k-hp)))
                                  (when kill-text
                                    (emit-combat-log event-queue
                                                     (format nil "Kill XP: ~a"
                                                             kill-text))))
                                (emit-level-up-messages event-queue level-ups)
                                (let ((new-combat (combat-level (player-stats player))))
                                  (when (> new-combat old-combat)
                                    (emit-hud-message event-queue
                                                      (format nil
                                                              "Congratulations! Combat level ~d."
                                                              new-combat))))))))
                          (award-npc-loot player target)))))
                    (push-combat-log event-queue player target nil chance roll
                                     (combatant-attack-level player)
                                     (combatant-defense-level target)
                                     :xp-text "XP 0"))))))))

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

(defun update-npc-attack (npc player world dt event-queue)
  ;; Handle NPC melee attacks and cooldowns (server-side).
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
              (if hit
                  (let ((damage (roll-melee-damage npc (npc-attack-damage npc))))
                    (combatant-apply-hit player damage)
                    (combatant-trigger-hit-effect player)
                    (emit-hud-message event-queue "You are under attack!")
                    (push-combat-log event-queue npc player t chance roll
                                     (combatant-attack-level npc)
                                     (combatant-defense-level player)
                                     :damage damage))
                  (push-combat-log event-queue npc player nil chance roll
                                   (combatant-attack-level npc)
                                   (combatant-defense-level player)
                                   :xp-text "XP 0"))))
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
