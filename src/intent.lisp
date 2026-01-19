;; NOTE: If you change behavior here, update docs/intent.md :)
(in-package #:mmorpg)

(defstruct (intent (:constructor %make-intent))
  ;; Desired actions for an entity, filled by input/AI.
  move-dx move-dy
  face-dx face-dy
  target-x target-y target-active
  attack
  run-toggle
  ;; Client-requested targets (server validates and sets authoritative state)
  requested-attack-target-id
  requested-follow-target-id
  requested-pickup-target-id
  requested-pickup-tx
  requested-pickup-ty
  requested-drop-item-id
  requested-drop-count
  requested-drop-slot-index
  requested-swap-slot-a
  requested-swap-slot-b
  requested-chat-message
  requested-unstuck)

(defun make-intent (&key (target-x 0.0) (target-y 0.0))
  ;; Create a reusable intent with optional target coordinates.
  (%make-intent :move-dx 0.0
                :move-dy 0.0
                :face-dx 0.0
                :face-dy 0.0
                :target-x target-x
                :target-y target-y
                :target-active nil
                :attack nil
                :run-toggle nil
                :requested-attack-target-id 0
                :requested-follow-target-id 0
                :requested-pickup-target-id nil
                :requested-pickup-tx nil
                :requested-pickup-ty nil
                :requested-drop-item-id nil
                :requested-drop-count 0
                :requested-drop-slot-index nil
                :requested-swap-slot-a nil
                :requested-swap-slot-b nil
                :requested-chat-message nil
                :requested-unstuck nil))

(defun reset-frame-intent (intent)
  ;; Clear per-frame intent signals without touching persistent targets.
  (setf (intent-move-dx intent) 0.0
        (intent-move-dy intent) 0.0
        (intent-face-dx intent) 0.0
        (intent-face-dy intent) 0.0
        (intent-attack intent) nil
        (intent-run-toggle intent) nil
        (intent-requested-unstuck intent) nil))

(defun consume-intent-actions (intent)
  ;; Clear one-shot actions after a simulation tick.
  (setf (intent-attack intent) nil
        (intent-run-toggle intent) nil))

(defun set-intent-face (intent dx dy)
  ;; Update facing intent when input supplies a non-zero direction.
  (when (or (not (zerop dx)) (not (zerop dy)))
    (setf (intent-face-dx intent) dx
          (intent-face-dy intent) dy)))

(defun set-intent-move (intent dx dy)
  ;; Set movement direction and update facing as needed.
  (setf (intent-move-dx intent) dx
        (intent-move-dy intent) dy)
  (set-intent-face intent dx dy))

(defun set-intent-target (intent target-x target-y)
  ;; Set a target and mark it active.
  (setf (intent-target-x intent) target-x
        (intent-target-y intent) target-y
        (intent-target-active intent) t))

(defun clear-intent-target (intent)
  ;; Cancel any active target.
  (setf (intent-target-active intent) nil))

(defun request-intent-attack (intent)
  ;; Request an attack this frame.
  (setf (intent-attack intent) t))

(defun request-intent-run-toggle (intent)
  ;; Request a run toggle this frame.
  (setf (intent-run-toggle intent) t))

(defun request-attack-target (intent target-id)
  ;; Request an attack target by entity ID (client sends, server validates).
  (setf (intent-requested-attack-target-id intent) (or target-id 0)))

(defun request-follow-target (intent target-id)
  ;; Request a follow target by entity ID (client sends, server validates).
  (setf (intent-requested-follow-target-id intent) (or target-id 0)))

(defun request-pickup-target (intent object-id tx ty)
  ;; Request a pickup target by object ID and tile coordinates.
  (setf (intent-requested-pickup-target-id intent) object-id
        (intent-requested-pickup-tx intent) tx
        (intent-requested-pickup-ty intent) ty))

(defun clear-requested-attack-target (intent)
  ;; Clear the requested attack target.
  (setf (intent-requested-attack-target-id intent) 0))

(defun clear-requested-follow-target (intent)
  ;; Clear the requested follow target.
  (setf (intent-requested-follow-target-id intent) 0))

(defun clear-requested-pickup-target (intent)
  ;; Clear the requested pickup target.
  (setf (intent-requested-pickup-target-id intent) nil
        (intent-requested-pickup-tx intent) nil
        (intent-requested-pickup-ty intent) nil))

(defun request-drop-item (intent item-id count slot-index)
  ;; Request to drop items from inventory (client sends, server validates).
  (setf (intent-requested-drop-item-id intent) item-id
        (intent-requested-drop-count intent) (or count 0)
        (intent-requested-drop-slot-index intent) slot-index))

(defun clear-requested-drop-item (intent)
  ;; Clear the requested drop item.
  (setf (intent-requested-drop-item-id intent) nil
        (intent-requested-drop-count intent) 0
        (intent-requested-drop-slot-index intent) nil))

(defun request-chat-message (intent message)
  ;; Request a chat message to broadcast.
  (setf (intent-requested-chat-message intent) message))

(defun clear-requested-chat-message (intent)
  ;; Clear the requested chat message.
  (setf (intent-requested-chat-message intent) nil))

(defun request-unstuck (intent)
  ;; Request an unstuck teleport.
  (setf (intent-requested-unstuck intent) t))

(defun clear-requested-unstuck (intent)
  ;; Clear the unstuck request flag.
  (setf (intent-requested-unstuck intent) nil))

(defun request-inventory-swap (intent slot-a slot-b)
  ;; Request to swap two inventory slots (client sends, server validates).
  (setf (intent-requested-swap-slot-a intent) slot-a
        (intent-requested-swap-slot-b intent) slot-b))

(defun clear-requested-inventory-swap (intent)
  ;; Clear the inventory swap request.
  (setf (intent-requested-swap-slot-a intent) nil
        (intent-requested-swap-slot-b intent) nil))
