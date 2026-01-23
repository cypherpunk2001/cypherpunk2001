;; NOTE: If you change behavior here, update docs/input.md :)
(in-package #:mmorpg)

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

(defun open-chat-input (ui)
  ;; Enter chat input mode and clear the buffer.
  (setf (ui-chat-active ui) t
        (ui-chat-buffer ui) "")
  ui)

(defun close-chat-input (ui)
  ;; Exit chat input mode and clear the buffer.
  (setf (ui-chat-active ui) nil
        (ui-chat-buffer ui) "")
  ui)

(defun append-chat-char (ui char)
  ;; Append CHAR to the chat buffer if under the limit.
  (let* ((buffer (ui-chat-buffer ui))
         (max-len (ui-chat-max-length ui)))
    (when (< (length buffer) max-len)
      (setf (ui-chat-buffer ui)
            (concatenate 'string buffer (string char))))))

(defun backspace-chat-buffer (ui)
  ;; Remove the last character in the chat buffer.
  (let ((buffer (ui-chat-buffer ui)))
    (when (> (length buffer) 0)
      (setf (ui-chat-buffer ui)
            (subseq buffer 0 (1- (length buffer)))))))

(defun update-chat-input (ui &key (skip-input nil))
  ;; Handle chat typing. Returns a submitted message or nil.
  (when (ui-chat-active ui)
    (cond
      ((raylib:is-key-pressed +key-escape+)
       (close-chat-input ui)
       nil)
      ((raylib:is-key-pressed +key-enter+)
       (let* ((buffer (ui-chat-buffer ui))
              (trimmed (string-trim '(#\Space #\Tab) buffer)))
         (close-chat-input ui)
         (when (> (length trimmed) 0)
           trimmed)))
      (t
       (when (raylib:is-key-pressed +key-backspace+)
         (backspace-chat-buffer ui))
       (loop :for code = (raylib:get-char-pressed)
             :while (> code 0)
             :do (when (and (not skip-input)
                            (>= code 32)
                            (<= code 126))
                   (append-chat-char ui (code-char code))))
       nil))))

(defun clear-player-auto-walk (player)
  ;; Clear auto-walk toggles on the player.
  (setf (player-auto-right player) nil
        (player-auto-left player) nil
        (player-auto-down player) nil
        (player-auto-up player) nil))

(defun clear-player-attack-target (player)
  ;; Clear any active attack target and its marker.
  (setf (player-attack-target-id player) 0
        (player-click-marker-target-id player) 0
        (player-click-marker-kind player) nil))

(defun clear-player-follow-target (player)
  ;; Clear any active follow target and marker tracking.
  (setf (player-follow-target-id player) 0
        (player-click-marker-target-id player) 0
        (player-click-marker-kind player) nil))

(defun clear-player-pickup-target (player)
  ;; Clear any active pickup target and marker tracking.
  (setf (player-pickup-target-active player) nil
        (player-pickup-target-id player) nil
        (player-click-marker-target-id player) 0
        (player-click-marker-kind player) nil))

(defun trigger-click-marker (player world-x world-y kind &optional (target-id 0))
  "Set a click marker at the given world position.
   If TARGET-ID is non-zero, marker follows that NPC until it dies or target is cleared."
  (setf (player-click-marker-x player) world-x
        (player-click-marker-y player) world-y
        (player-click-marker-kind player) kind
        (player-click-marker-timer player) *click-marker-duration*
        (player-click-marker-target-id player) target-id))

(defun update-click-marker (player dt &optional npcs)
  "Update click marker timer and position (if tracking a target NPC).
   When tracking (target-id > 0), marker follows the NPC and persists until it dies."
  (let ((target-id (player-click-marker-target-id player)))
    (if (> target-id 0)
        ;; Tracking mode: update position from NPC, keep marker alive
        (when npcs
          (let ((npc (find-npc-by-id npcs target-id)))
            (if (and npc (combatant-alive-p npc))
                ;; Target alive: follow it and keep marker visible
                (setf (player-click-marker-x player) (npc-x npc)
                      (player-click-marker-y player) (npc-y npc)
                      (player-click-marker-timer player) 1.0)
                ;; Target dead or gone: clear tracking and marker
                (setf (player-click-marker-target-id player) 0
                      (player-click-marker-kind player) nil
                      (player-click-marker-timer player) 0.0))))
        ;; Non-tracking mode: normal timer decay
        (let ((timer (player-click-marker-timer player)))
          (when (> timer 0.0)
            (setf timer (max 0.0 (- timer dt)))
            (setf (player-click-marker-timer player) timer)
            (when (<= timer 0.0)
              (setf (player-click-marker-kind player) nil)))))))

(defun set-player-walk-target (player intent world-x world-y &optional (mark-p t))
  ;; Request a walk target via intent (server validates and clears conflicting targets).
  ;; Note: We do NOT clear pickup target here - if player has an active pickup,
  ;; clicking on ground shouldn't cancel it. Let them finish the pickup first.
  (set-intent-target intent world-x world-y)
  (clear-requested-attack-target intent)
  (clear-requested-follow-target intent)
  ;; Only clear pickup if no active pickup target (don't interrupt walk-to-pickup)
  (unless (intent-requested-pickup-target-id intent)
    (clear-requested-pickup-target intent))
  ;; Clear any NPC-tracking marker (walk overrides attack target)
  (setf (player-click-marker-target-id player) 0)
  (when mark-p
    (trigger-click-marker player world-x world-y :walk)))

(defun set-player-attack-target (player intent npc &optional (mark-p t))
  ;; Request an NPC attack target via intent (server validates and sets authoritative target).
  ;; Marker tracks NPC position until death.
  (when npc
    (request-attack-target intent (npc-id npc))
    (clear-requested-follow-target intent)
    (clear-requested-pickup-target intent)
    (set-intent-target intent (npc-x npc) (npc-y npc))
    (when mark-p
      (trigger-click-marker player (npc-x npc) (npc-y npc) :attack (npc-id npc)))))

(defun set-player-follow-target (player intent npc &optional (mark-p t))
  ;; Request an NPC follow target via intent (server validates and sets authoritative target).
  ;; Follow uses a walk marker (yellow), not attack marker, and clears attack marker tracking.
  (when npc
    (request-follow-target intent (npc-id npc))
    (clear-requested-attack-target intent)
    (clear-requested-pickup-target intent)
    (set-intent-target intent (npc-x npc) (npc-y npc))
    ;; Clear attack marker tracking (follow overrides attack)
    (setf (player-click-marker-target-id player) 0)
    (when mark-p
      (trigger-click-marker player (npc-x npc) (npc-y npc) :walk))))

(defun set-player-pickup-target (player intent world object-id tx ty &optional (mark-p t))
  ;; Request a pickup target via intent (server validates and sets authoritative target).
  (log-verbose "CLIENT-PICKUP: setting target id=~a tx=~d ty=~d" object-id tx ty)
  (request-pickup-target intent object-id tx ty)
  (log-verbose "CLIENT-PICKUP: intent fields now id=~a tx=~a ty=~a"
               (intent-requested-pickup-target-id intent)
               (intent-requested-pickup-tx intent)
               (intent-requested-pickup-ty intent))
  (clear-requested-attack-target intent)
  (clear-requested-follow-target intent)
  (multiple-value-bind (world-x world-y)
      (tile-center-position (world-tile-dest-size world) tx ty)
    (set-intent-target intent world-x world-y)
    (when mark-p
      (trigger-click-marker player world-x world-y :walk))))

(defun npc-hit-test-p (npc world world-x world-y)
  ;; Return true when WORLD-X/WORLD-Y overlap the NPC collider.
  (multiple-value-bind (half-w half-h)
      (combatant-collision-half npc world)
    (and (<= (abs (- world-x (npc-x npc))) half-w)
         (<= (abs (- world-y (npc-y npc))) half-h))))

(defun find-npc-at-world (npcs world world-x world-y)
  ;; Return the closest NPC under the world coordinates, if any.
  (let ((best nil)
        (best-dist nil))
    (loop :for npc :across npcs
          :when (and (combatant-alive-p npc)
                     (npc-hit-test-p npc world world-x world-y))
            :do (let* ((dx (- world-x (npc-x npc)))
                       (dy (- world-y (npc-y npc)))
                       (dist (+ (* dx dx) (* dy dy))))
                  (when (or (null best-dist) (< dist best-dist))
                    (setf best npc
                          best-dist dist))))
    best))

(defun find-npc-by-id (npcs id)
  ;; Return the NPC with ID, if present.
  (when (> id 0)
    (loop :for npc :across npcs
          :when (= (npc-id npc) id)
            :do (return npc))))

(defun find-npc-at-screen (npcs world player camera screen-x screen-y)
  ;; Return NPC under the cursor and the world coordinates.
  (multiple-value-bind (world-x world-y)
      (screen-to-world screen-x screen-y
                       (player-x player)
                       (player-y player)
                       (camera-offset camera)
                       (camera-zoom camera))
    (values (find-npc-at-world npcs world world-x world-y)
            world-x
            world-y)))

(defun find-object-at-world (world world-x world-y)
  ;; Return the object at WORLD-X/WORLD-Y, if any.
  (let* ((zone (world-zone world))
         (objects (and zone (zone-objects zone))))
    (when objects
      (let* ((tile-size (world-tile-dest-size world))
             (tx (floor world-x tile-size))
             (ty (floor world-y tile-size)))
        ;; Task 5.5: Use zone-object struct accessors for O(1) field access
        (loop :for object :in objects
              :for count = (and object (zone-object-count object))
              :for respawn = (and object (zone-object-respawn object))
              :for active = (and (or (null count) (> count 0))
                                 (or (null respawn) (<= respawn 0.0)))
              :when (and active
                         (eql (zone-object-x object) tx)
                         (eql (zone-object-y object) ty))
                :do (return object))))))

(defun find-object-at-screen (world player camera screen-x screen-y)
  ;; Return object under the cursor and the world coordinates.
  (multiple-value-bind (world-x world-y)
      (screen-to-world screen-x screen-y
                       (player-x player)
                       (player-y player)
                       (camera-offset camera)
                       (camera-zoom camera))
    (values (find-object-at-world world world-x world-y)
            world-x
            world-y)))

(defun update-ui-hovered-npc (ui npcs world player camera)
  ;; Update the UI hover label for the NPC under the cursor.
  (multiple-value-bind (npc _world-x _world-y)
      (find-npc-at-screen npcs world player camera
                          (raylib:get-mouse-x)
                          (raylib:get-mouse-y))
    (declare (ignore _world-x _world-y))
    (setf (ui-hover-npc-name ui)
          (and npc (combatant-display-name npc)))))

(defun npc-examine-description (npc)
  ;; Return an examine description for NPC.
  (let* ((archetype (and npc (npc-archetype npc)))
         (desc (and archetype (npc-archetype-description archetype)))
         (name (combatant-display-name npc)))
    (or desc
        (format nil "A sketchy ~a that looks ready for trouble." name))))

(defun object-examine-description (object)
  ;; Return an examine description for OBJECT.
  ;; Task 5.5: Use zone-object struct accessor for O(1) field access
  (let* ((object-id (and object (zone-object-id object)))
         (archetype (and object-id (find-object-archetype object-id)))
         (desc (and archetype (object-archetype-description archetype)))
         (name (or (and archetype (object-archetype-name archetype))
                   (and object-id (string-capitalize (string object-id)))
                   "Object")))
    (or desc
        (format nil "A discarded ~a. It might be useful." name))))

(defun item-examine-description (item-id)
  ;; Return an examine description for ITEM-ID.
  (let* ((archetype (and item-id (find-item-archetype item-id)))
         (desc (and archetype (item-archetype-description archetype)))
         (name (or (and archetype (item-archetype-name archetype))
                   (and item-id (string-capitalize (string item-id)))
                   "Item")))
    (or desc
        (format nil "A ~a." name))))

(defun update-target-from-mouse (player intent camera dt mouse-clicked mouse-down)
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
      (set-player-walk-target player intent target-x target-y t)
      (setf (player-mouse-hold-timer player) 0.0)))
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
        (set-player-walk-target player intent target-x target-y nil))))
  (unless mouse-down
    (setf (player-mouse-hold-timer player) 0.0)))

(defun minimap-screen-to-world (ui world player screen-x screen-y)
  ;; Convert minimap screen coordinates into world space.
  (multiple-value-bind (view-min-x view-min-y span-x span-y)
      (minimap-view-bounds world player)
    (let* ((rx (if (> (ui-minimap-width ui) 0)
                   (/ (- screen-x (ui-minimap-x ui))
                      (ui-minimap-width ui))
                   0.0))
           (ry (if (> (ui-minimap-height ui) 0)
                   (/ (- screen-y (ui-minimap-y ui))
                      (ui-minimap-height ui))
                   0.0))
           (clamped-x (clamp rx 0.0 1.0))
           (clamped-y (clamp ry 0.0 1.0)))
      (values (+ view-min-x (* clamped-x span-x))
              (+ view-min-y (* clamped-y span-y))))))

(defun update-target-from-minimap (player intent ui world dt mouse-clicked mouse-down)
  ;; Handle minimap click/hold to update the player target position.
  (let* ((mouse-x (raylib:get-mouse-x))
         (mouse-y (raylib:get-mouse-y))
         (inside (point-in-rect-p mouse-x mouse-y
                                  (ui-minimap-x ui)
                                  (ui-minimap-y ui)
                                  (ui-minimap-width ui)
                                  (ui-minimap-height ui))))
    (when (and inside mouse-clicked)
      (clear-player-auto-walk player)
      (multiple-value-bind (target-x target-y)
          (minimap-screen-to-world ui world player mouse-x mouse-y)
        (set-player-walk-target player intent target-x target-y t)
        (setf (player-mouse-hold-timer player) 0.0))
      t)
    (when (and inside mouse-down (not mouse-clicked))
      (incf (player-mouse-hold-timer player) dt)
      (when (>= (player-mouse-hold-timer player) *mouse-hold-repeat-seconds*)
        (setf (player-mouse-hold-timer player) 0.0)
        (clear-player-auto-walk player)
        (multiple-value-bind (target-x target-y)
            (minimap-screen-to-world ui world player mouse-x mouse-y)
          (set-player-walk-target player intent target-x target-y nil)))
      t)
    (unless mouse-down
      (setf (player-mouse-hold-timer player) 0.0))
    (and inside (or mouse-clicked mouse-down))))

(defun update-input-direction (player intent mouse-clicked)
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
                (clear-intent-target intent))
              (setf input-dx (+ (if (player-auto-right player) 1.0 0.0)
                                (if (player-auto-left player) -1.0 0.0))
                    input-dy (+ (if (player-auto-down player) 1.0 0.0)
                                (if (player-auto-up player) -1.0 0.0)))
              (multiple-value-setq (input-dx input-dy)
                (normalize-direction input-dx input-dy)))
            (multiple-value-setq (input-dx input-dy)
              (read-input-direction)))))
    (set-intent-move intent input-dx input-dy)
    (when (or (not (zerop input-dx)) (not (zerop input-dy)))
      (clear-requested-attack-target intent)
      (clear-requested-follow-target intent)
      (clear-requested-pickup-target intent))
    (values input-dx input-dy)))

(defun update-input-actions (intent allow-run-toggle)
  ;; Translate direct input into intent actions.
  (when (and allow-run-toggle
             (raylib:is-key-pressed +key-tab+))
    (request-intent-run-toggle intent))
  (when (raylib:is-key-pressed +key-space+)
    (request-intent-attack intent)))

(defun update-training-mode (player)
  ;; Update training mode selection from hotkeys.
  (let* ((stats (player-stats player))
         (before (and stats (stat-block-training-mode stats))))
    (when stats
      (cond
        ((raylib:is-key-pressed +key-one+)
         (set-training-mode stats :attack))
        ((raylib:is-key-pressed +key-two+)
         (set-training-mode stats :strength))
        ((raylib:is-key-pressed +key-three+)
         (set-training-mode stats :defense))
        ((raylib:is-key-pressed +key-z+)
         (set-training-mode stats :balanced))))
    (when (and stats (not (eq before (stat-block-training-mode stats))))
      (mark-player-hud-stats-dirty player))))

(defun make-camera ()
  ;; Initialize camera offset and zoom settings.
  (%make-camera :offset (raylib:make-vector2 :x (/ *window-width* 2.0)
                                             :y (/ *window-height* 2.0))
                :zoom *camera-zoom-default*))

(defun update-camera-for-window-resize (camera)
  "Update camera offset to center on new screen dimensions.
   Called when the window is resized and *window-resize-enabled* is T."
  (when camera
    (let ((offset (camera-offset camera)))
      (setf (raylib:vector2-x offset) (/ (current-screen-width) 2.0)
            (raylib:vector2-y offset) (/ (current-screen-height) 2.0))))
  camera)
