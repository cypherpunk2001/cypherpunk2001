;; NOTE: If you change behavior here, update docs/chat.md :)
(in-package #:mmorpg)

(defun trim-chat-message (message)
  ;; Trim whitespace from MESSAGE.
  (string-trim '(#\Space #\Tab #\Newline #\Return) message))

(defun process-chat-intent (player intent world event-queue)
  ;; Validate and broadcast chat messages from intent.
  (let ((message (intent-requested-chat-message intent)))
    (when message
      (let ((trimmed (trim-chat-message message)))
        (when (> (length trimmed) 0)
          (let ((zone-label (or (world-zone-label world) "ZONE")))
            (emit-hud-message-event event-queue
                                    (format nil "[~a] ~a: ~a"
                                            zone-label
                                            (combatant-display-name player)
                                            trimmed)))))
      (clear-requested-chat-message intent))))
