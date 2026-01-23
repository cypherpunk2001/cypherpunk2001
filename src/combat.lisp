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
    ;; Mark snapshot-dirty for delta compression (see docs/net.md Prong 2)
    (setf (player-snapshot-dirty combatant) t)
    ;; Tier-1 write: player death (HP reaches 0) must be saved immediately
    ;; to prevent logout-to-survive exploit
    ;; Use exponential backoff per Tier-1 policy (5 retries, 100-500ms)
    (when (and (= new-hp 0) (> old-hp 0))
      ;; Phase 5: Increment player deaths counter before leaderboard update
      (incf (player-deaths combatant))
      ;; Update deaths leaderboard with new total (Phase 5: uses zadd, not zincrby)
      (db-update-leaderboard-deaths (player-id combatant) (player-deaths combatant))
      (with-retry-exponential (saved (lambda () (db-save-player-immediate combatant))
                                :max-retries 5
                                :initial-delay 100
                                :max-delay 500
                                :on-final-fail (lambda (e)
                                                 (warn "CRITICAL: Death save FAILED for player ~d after 5 retries: ~a - using dirty flag fallback"
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
    ;; Mark snapshot-dirty for delta compression (see docs/net.md Prong 2)
    (setf (npc-snapshot-dirty combatant) t)
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

;;; Non-generic hit effect functions (Task 1.5: CLOS dispatch removal)
;;; These avoid CLOS dispatch overhead in hot loops by being called directly
;;; when the entity type is already known.

(defun update-player-hit-effect (player dt)
  "Update hit effect animation for PLAYER. Non-generic version for hot loops."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type player player)
           (type single-float dt))
  (when (player-hit-active player)
    (let* ((frame-count *blood-frame-count*)
           (frame-time *blood-frame-time*)
           (timer (+ (player-hit-timer player) dt))
           (duration (* frame-count frame-time))
           (frame (min (the fixnum (truncate (/ timer frame-time)))
                       (the fixnum (1- frame-count)))))
      (declare (type fixnum frame-count frame)
               (type single-float frame-time timer duration))
      (setf (player-hit-timer player) timer
            (player-hit-frame player) frame)
      (when (>= timer duration)
        (setf (player-hit-active player) nil
              (player-hit-timer player) 0.0
              (player-hit-frame player) 0)))))

(defun update-npc-hit-effect (npc dt)
  "Update hit effect animation for NPC. Non-generic version for hot loops."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type npc npc)
           (type single-float dt))
  (when (npc-hit-active npc)
    (let* ((frame-count *blood-frame-count*)
           (frame-time *blood-frame-time*)
           (timer (+ (npc-hit-timer npc) dt))
           (duration (* frame-count frame-time))
           (frame (min (the fixnum (truncate (/ timer frame-time)))
                       (the fixnum (1- frame-count)))))
      (declare (type fixnum frame-count frame)
               (type single-float frame-time timer duration))
      (setf (npc-hit-timer npc) timer
            (npc-hit-frame npc) frame)
      (when (>= timer duration)
        (setf (npc-hit-active npc) nil
              (npc-hit-timer npc) 0.0
              (npc-hit-frame npc) 0)))))

;;; Type-specific apply-hit and trigger-hit-effect (Task 1.5: CLOS removal)
;;; These avoid CLOS dispatch in hot combat loops.

(defun npc-trigger-hit-effect (npc)
  "Trigger hit effect animation on NPC. Direct accessor for hot paths."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type npc npc))
  (setf (npc-hit-active npc) t
        (npc-hit-timer npc) 0.0
        (npc-hit-frame npc) 0
        (npc-hit-facing npc) (npc-facing npc)
        (npc-hit-facing-sign npc) 1.0))

(defun player-trigger-hit-effect (player)
  "Trigger hit effect animation on PLAYER. Direct accessor for hot paths."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type player player))
  (setf (player-hit-active player) t
        (player-hit-timer player) 0.0
        (player-hit-frame player) 0
        (player-hit-facing player) (player-facing player)
        (player-hit-facing-sign player) (player-facing-sign player)))

(defun npc-apply-hit (npc &optional amount)
  "Apply damage to NPC and return T if killed. Direct accessor for hot paths."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type npc npc))
  (let* ((damage (if amount amount 1))
         (max-hits (npc-max-hp npc))
         (before (npc-hits-left npc)))
    (when (and max-hits (> (npc-hits-left npc) max-hits))
      (setf (npc-hits-left npc) max-hits))
    (decf (npc-hits-left npc) damage)
    (setf (npc-provoked npc) t)
    (setf (npc-snapshot-dirty npc) t)
    (let ((killed nil))
      (when (<= (npc-hits-left npc) 0)
        (setf (npc-hits-left npc) 0
              (npc-alive npc) nil
              killed t))
      (when killed
        (let ((respawn (npc-respawn-seconds npc)))
          (when (and respawn (> respawn 0.0))
            (setf (npc-respawn-timer npc) respawn))))
      (when *debug-npc-logs*
        (let* ((archetype (npc-archetype npc))
               (name (if archetype (npc-archetype-name archetype) "NPC"))
               (flee-at (if archetype (npc-archetype-flee-at-hits archetype) 0)))
          (format t "~&NPC-HIT ~a hits-left=~d->~d flee-at=~d alive=~a state=~a~%"
                  name before (npc-hits-left npc) flee-at (npc-alive npc)
                  (npc-behavior-state npc))
          (finish-output)))
      killed)))

(defun player-apply-hit (player &optional amount)
  "Apply damage to PLAYER. Direct accessor for hot paths."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type player player))
  (let* ((damage (if amount amount 1))
         (old-hp (player-hp player))
         (hp (- old-hp damage))
         (new-hp (max 0 hp)))
    (setf (player-hp player) new-hp)
    (setf (player-snapshot-dirty player) t)
    ;; Tier-1 write: player death (HP reaches 0) must be saved immediately
    (when (and (= new-hp 0) (> old-hp 0))
      (incf (player-deaths player))
      (db-update-leaderboard-deaths (player-id player) (player-deaths player))
      (with-retry-exponential (saved (lambda () (db-save-player-immediate player))
                                :max-retries 5
                                :initial-delay 100
                                :max-delay 500
                                :on-final-fail (lambda (e)
                                                 (warn "CRITICAL: Death save FAILED for player ~d after 5 retries: ~a - using dirty flag fallback"
                                                       (player-id player) e)
                                                 (mark-player-dirty (player-id player))))))
    ;; Tier-2 write: HP changes should be marked dirty for batched saves
    (when (/= hp new-hp)
      (mark-player-dirty (player-id player)))))

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
  "Return true when two axis-aligned boxes overlap (center + half sizes)."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type real ax ay ahw ahh bx by bhw bhh))
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

(defun update-npc-respawns (npcs dt &optional zone-state)
  "Tick respawn timers and restore NPCs when timers expire.
   When ZONE-STATE is provided, updates the NPC's grid cell on respawn."
  (let ((npc-grid (and zone-state (zone-state-npc-grid zone-state))))
    (loop :for npc :across npcs
          :when (and (not (npc-alive npc))
                     (> (npc-respawn-timer npc) 0.0))
          :do (let ((timer (max 0.0 (- (npc-respawn-timer npc) dt))))
                (setf (npc-respawn-timer npc) timer)
                (when (<= timer 0.0)
                  ;; Remove from old grid cell before respawn moves NPC
                  (when (and npc-grid
                             (npc-grid-cell-x npc)
                             (npc-grid-cell-y npc))
                    (spatial-grid-remove npc-grid (npc-id npc)
                                         (npc-grid-cell-x npc)
                                         (npc-grid-cell-y npc)))
                  (respawn-npc npc)
                  ;; Insert into new grid cell at home position
                  (when npc-grid
                    (multiple-value-bind (cx cy)
                        (position-to-cell (npc-x npc) (npc-y npc)
                                          (spatial-grid-cell-size npc-grid))
                      (spatial-grid-insert npc-grid (npc-id npc)
                                           (npc-x npc) (npc-y npc))
                      (setf (npc-grid-cell-x npc) cx
                            (npc-grid-cell-y npc) cy))))))))

(defun player-attack-target (player npcs)
  "Return the active NPC target for PLAYER, if any.
   Uses direct npc-alive instead of CLOS (Task 1.5)."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (let* ((id (player-attack-target-id player))
         (npc (and (> id 0) (find-npc-by-id npcs id))))
    (when (and npc (npc-alive npc))
      npc)))

(defun player-follow-target (player npcs)
  "Return the active NPC follow target for PLAYER, if any.
   Uses direct npc-alive instead of CLOS (Task 1.5)."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (let* ((id (player-follow-target-id player))
         (npc (and (> id 0) (find-npc-by-id npcs id))))
    (when (and npc (npc-alive npc))
      npc)))

(defun target-in-range-p (player npc world)
  "Check if NPC is within targeting range of PLAYER."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type player player)
           (type npc npc)
           (type world world))
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
    (declare (type single-float px py nx ny dx dy dist-sq tile-size max-dist max-dist-sq))
    (<= dist-sq max-dist-sq)))

(defun sync-player-attack-target (player intent npcs world)
  ;; Validate requested attack target and set authoritative state (server authority).
  ;; No range check - player will walk to target and auto-attack when in melee range.
  (declare (ignore world))
  (let* ((requested-id (intent-requested-attack-target-id intent))
         (current-id (player-attack-target-id player)))
    ;; Process clear request (requested-id = 0 means client wants to cancel)
    (when (and requested-id (= requested-id 0) (> current-id 0))
      (setf (player-attack-target-id player) 0)
      (clear-intent-target intent))
    ;; Process new attack target request
    (when (and requested-id (> requested-id 0) (not (= requested-id current-id)))
      (let ((npc (find-npc-by-id npcs requested-id)))
        (if (and npc (npc-alive npc))
            (progn
              ;; Valid target: set authoritative state and clear conflicting targets
              (setf (player-attack-target-id player) requested-id
                    (player-follow-target-id player) 0
                    (player-pickup-target-id player) nil
                    (player-pickup-target-active player) nil)
              (clear-player-auto-walk player))
            ;; Invalid target (dead or not found): reject request
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
  ;; No range check - player will walk to target.
  (declare (ignore world))
  (let* ((requested-id (intent-requested-follow-target-id intent))
         (current-id (player-follow-target-id player)))
    ;; Process clear request (requested-id = 0 means client wants to cancel)
    (when (and requested-id (= requested-id 0) (> current-id 0))
      (setf (player-follow-target-id player) 0)
      (clear-intent-target intent))
    ;; Process new follow target request
    (when (and requested-id (> requested-id 0) (not (= requested-id current-id)))
      (let ((npc (find-npc-by-id npcs requested-id)))
        (if (and npc (npc-alive npc))  ; Task 1.5: direct accessor, no CLOS
            (progn
              ;; Valid target: set authoritative state and clear conflicting targets
              (setf (player-follow-target-id player) requested-id
                    (player-attack-target-id player) 0
                    (player-pickup-target-id player) nil
                    (player-pickup-target-active player) nil)
              (clear-player-auto-walk player))
            ;; Invalid target (dead or not found): reject request
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
  ;; Accepts target regardless of range - player will walk to it.
  ;; Actual pickup happens in update-player-pickup-target when player reaches the tile.
  (declare (ignore world))
  (let* ((requested-id (intent-requested-pickup-target-id intent))
         (requested-tx (intent-requested-pickup-tx intent))
         (requested-ty (intent-requested-pickup-ty intent))
         (current-id (player-pickup-target-id player)))
    (when requested-id
      (log-verbose "SYNC-PICKUP: req-id=~a req-tx=~a req-ty=~a cur-id=~a"
                   requested-id requested-tx requested-ty current-id))
    ;; Process clear request (requested-id = nil means client wants to cancel)
    (when (and (not requested-id) current-id)
      (setf (player-pickup-target-id player) nil
            (player-pickup-target-active player) nil))
    ;; Process new pickup target request - accept and let player walk there
    (when (and requested-id requested-tx requested-ty
               (not (and (eql requested-id current-id)
                         (eql requested-tx (player-pickup-target-tx player))
                         (eql requested-ty (player-pickup-target-ty player)))))
      (log-verbose "SYNC-PICKUP: Setting target for player ~a" (player-id player))
      (setf (player-pickup-target-id player) requested-id
            (player-pickup-target-tx player) requested-tx
            (player-pickup-target-ty player) requested-ty
            (player-pickup-target-active player) t
            (player-attack-target-id player) 0
            (player-follow-target-id player) 0))))

(defun player-attack-target-in-range-p (player target world)
  "Return true when TARGET (NPC) is inside the player's melee hitbox.
   Uses direct struct accessors instead of CLOS dispatch (Task 1.5)."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type player player)
           (type npc target)
           (type world world))
  (multiple-value-bind (ax ay ahw ahh)
      (attack-hitbox player world)
    (let ((thw (world-collision-half-width world))
          (thh (world-collision-half-height world))
          (tx (npc-x target))
          (ty (npc-y target)))
      (declare (type single-float ax ay ahw ahh thw thh tx ty))
      (aabb-overlap-p ax ay ahw ahh tx ty thw thh))))

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
  "Apply melee damage once per attack if the hitbox overlaps the target (server-side).
   Uses direct NPC accessors instead of CLOS dispatch (Task 1.5)."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type player player)
           (type npc target)
           (type world world))
  (when (and (player-attacking player)
             (not (player-attack-hit player))
             (npc-alive target))
    (multiple-value-bind (ax ay ahw ahh)
        (attack-hitbox player world)
      (let ((thw (world-collision-half-width world))
            (thh (world-collision-half-height world))
            (tx (npc-x target))
            (ty (npc-y target)))
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
                            killed (npc-apply-hit target damage))
                      (npc-trigger-hit-effect target)
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
                                       (player-attack-level player)
                                       (npc-defense-level target)
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
                                     (player-attack-level player)
                                     (npc-defense-level target)
                                     :xp-text "XP 0")))))))

(defun update-player-animation (player dt)
  "Advance animation timers and set facing/state."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type player player)
           (type single-float dt))
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
    (declare (type single-float dx dy))
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
  "Handle NPC melee attacks and cooldowns (server-side)."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type npc npc)
           (type world world)
           (type single-float dt))
  (when (npc-alive npc)
    (let* ((intent (npc-intent npc))
           (timer (max 0.0f0 (- (npc-attack-timer npc) dt)))
           (state (npc-behavior-state npc))
           (attack-range (npc-attack-range npc world))
           (attack-range-sq (* attack-range attack-range)))
      (declare (type single-float timer attack-range attack-range-sq))
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
                    (player-apply-hit player damage)
                    (player-trigger-hit-effect player)
                    (emit-hud-message event-queue "You are under attack!")
                    (push-combat-log event-queue npc player t chance roll
                                     (npc-attack-level npc)
                                     (player-defense-level player)
                                     :damage damage))
                  (push-combat-log event-queue npc player nil chance roll
                                   (npc-attack-level npc)
                                   (player-defense-level player)
                                   :xp-text "XP 0"))))
          (setf (npc-attack-timer npc) (npc-attack-cooldown npc)))))))

(defun update-npc-animation (npc dt)
  "Advance idle animation frames for the NPC."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type npc npc)
           (type single-float dt))
  (when (npc-alive npc)
    (let* ((frame-count *idle-frame-count*)
           (frame-time *idle-frame-time*)
           (frame-index (npc-frame-index npc))
           (frame-timer (npc-frame-timer npc)))
      (declare (type fixnum frame-count frame-index)
               (type single-float frame-time frame-timer))
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

;;; Spatial Grid Optimized Melee Combat

(defun apply-melee-hits-spatial (players zone-state world event-queue)
  "Apply melee hits from attacking players to nearby NPCs using spatial grid.
   O(P×k) where k = NPCs in each player's 3x3 cell region, vs O(P×N) brute force.
   Uses O(1) NPC lookup via zone-state's npc-index-map."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type world world))
  (when (and players zone-state (> (length players) 0))
    (let ((npc-grid (zone-state-npc-grid zone-state))
          (zone-npcs (zone-state-npcs zone-state)))
      (if (and npc-grid zone-npcs)
          ;; Spatial grid path: query nearby NPCs for each attacking player
          (loop :for player :across players
                :when (and (player-attacking player)
                           (not (player-attack-hit player))
                           (player-grid-cell-x player)
                           (player-grid-cell-y player))
                ;; Phase 2: allocation-free query using scratch vector
                :do (let ((count (spatial-grid-query-neighbors-into
                                  npc-grid
                                  (player-grid-cell-x player)
                                  (player-grid-cell-y player)
                                  *spatial-scratch-vector-2*)))  ; Use secondary scratch (nested in player loop)
                      (declare (type fixnum count))
                      (loop :for i fixnum :from 0 :below count
                            :for npc-id fixnum = (aref *spatial-scratch-vector-2* i)
                            ;; O(1) NPC lookup via index map
                            :for npc = (find-npc-by-id-fast zone-state npc-id)
                            :when npc
                            :do (apply-melee-hit player npc world event-queue))))
          ;; Fallback: brute force if no spatial grid
          (loop :for player :across players
                :do (loop :for npc :across zone-npcs
                          :do (apply-melee-hit player npc world event-queue)))))))
