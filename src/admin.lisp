;;; NOTE: If you change behavior here, update docs/admin.md :)
(in-package #:mmorpg)

;;; Global server state for admin access
;;; These are set by run-server and accessed by admin commands

(defparameter *server-game* nil
  "Current server game state. Set by run-server.")

(defparameter *server-socket* nil
  "Current server UDP socket. Set by run-server.")

(defparameter *server-clients* nil
  "List of connected net-clients. Set by run-server.")

(defparameter *server-start-time* 0.0
  "Server start timestamp (internal-real-time). Set by run-server.")

(defparameter *server-total-saves* 0
  "Total number of player saves performed. Incremented by persistence layer.")

;;; Helper functions

(defun admin--find-player-by-id (player-id)
  "Find player struct by ID from player sessions (admin helper)."
  (let ((session (gethash player-id *player-sessions*)))
    (when session
      (player-session-player session))))

(defun admin--find-player-by-username (username)
  "Find player struct by username (case-insensitive)."
  (maphash (lambda (player-id session)
             (declare (ignore player-id))
             (let ((player (player-session-player session)))
               (when (and player
                          (string-equal (player-username player) username))
                 (return-from admin--find-player-by-username player))))
           *player-sessions*)
  nil)

(defun admin--find-player (id-or-username)
  "Find player by ID (integer) or username (string) (admin helper)."
  (etypecase id-or-username
    (integer (admin--find-player-by-id id-or-username))
    (string (admin--find-player-by-username id-or-username))))

(defun admin--find-net-client (player-id)
  "Find net-client for given player-id (admin helper)."
  (when *server-clients*
    (find-if (lambda (client)
               (and (net-client-player client)
                    (= (player-id (net-client-player client)) player-id)))
             *server-clients*)))

;;; Tier A: Player Data Inspection

(defun admin-print-save (id-or-username)
  "Pretty-print player's saved data. Returns plist or NIL."
  (let ((player (admin--find-player id-or-username)))
    (if player
        (let ((data (serialize-player player)))
          (format t "~&Player ~a (ID ~d):~%"
                  (player-username player)
                  (player-id player))
          (loop for (key value) on data by #'cddr
                do (format t "  ~a: ~s~%" key value))
          (finish-output)
          data)
        (progn
          (format t "~&Player not found: ~a~%" id-or-username)
          (finish-output)
          nil))))

(defun admin-list-players ()
  "List all online players. Returns list of (id name zone hp level)."
  (let ((result nil))
    (maphash (lambda (player-id session)
               (declare (ignore player-id))
               (let* ((player (player-session-player session))
                      (zone-id (player-session-zone-id session))
                      (hp (player-hp player))
                      (level (player-hitpoints-level player)))
                 (push (list (player-id player)
                             (player-username player)
                             zone-id
                             hp
                             level)
                       result)))
             *player-sessions*)
    (setf result (nreverse result))
    (format t "~&Online players (~d):~%" (length result))
    (dolist (entry result)
      (destructuring-bind (id name zone hp level) entry
        (format t "  ID ~5d | ~20a | ~15a | HP ~3d | Lvl ~2d~%"
                id name zone hp level)))
    (finish-output)
    result))

(defun admin-find-player (partial-name)
  "Search players by partial name match (case-insensitive). Returns list of (id name)."
  (let ((result nil)
        (search-term (string-downcase partial-name)))
    (maphash (lambda (player-id session)
               (declare (ignore player-id))
               (let ((player (player-session-player session)))
                 (when (search search-term (string-downcase (player-username player)))
                   (push (list (player-id player) (player-username player))
                         result))))
             *player-sessions*)
    (setf result (nreverse result))
    (format t "~&Found ~d player(s) matching '~a':~%" (length result) partial-name)
    (dolist (entry result)
      (destructuring-bind (id name) entry
        (format t "  ID ~5d | ~a~%" id name)))
    (finish-output)
    result))

;;; Tier A: Player Modification

(defun admin-grant-item (id-or-username item-id count)
  "Give item to player. Returns T on success, NIL on failure."
  (let ((player (admin--find-player id-or-username)))
    (if player
        (progn
          (add-inventory-item player item-id count)
          (mark-player-dirty (player-id player))
          (format t "~&Granted ~a x~d to player ~a (ID ~d)~%"
                  item-id count (player-username player) (player-id player))
          (finish-output)
          t)
        (progn
          (format t "~&Player not found: ~a~%" id-or-username)
          (finish-output)
          nil))))

(defun admin-remove-item (id-or-username item-id count)
  "Remove item from player. Returns T on success, NIL on failure."
  (let ((player (admin--find-player id-or-username)))
    (if player
        (let ((removed (remove-inventory-item player item-id count)))
          (mark-player-dirty (player-id player))
          (format t "~&Removed ~a x~d from player ~a (ID ~d) (actual: ~d)~%"
                  item-id count (player-username player) (player-id player) removed)
          (finish-output)
          t)
        (progn
          (format t "~&Player not found: ~a~%" id-or-username)
          (finish-output)
          nil))))

(defun admin-clear-inventory (id-or-username)
  "Wipe player inventory. Returns T on success, NIL on failure."
  (let ((player (admin--find-player id-or-username)))
    (if player
        (progn
          (clear-inventory player)
          (mark-player-dirty (player-id player))
          (format t "~&Cleared inventory for player ~a (ID ~d)~%"
                  (player-username player) (player-id player))
          (finish-output)
          t)
        (progn
          (format t "~&Player not found: ~a~%" id-or-username)
          (finish-output)
          nil))))

(defun admin-set-hp (id-or-username new-hp)
  "Set player current HP. Returns T on success, NIL on failure."
  (let ((player (admin--find-player id-or-username)))
    (if player
        (let ((max-hp (calculate-max-hp player)))
          (setf (player-hp player) (clamp new-hp 0 max-hp))
          (mark-player-dirty (player-id player))
          (format t "~&Set HP to ~d for player ~a (ID ~d) (max: ~d)~%"
                  (player-hp player) (player-username player) (player-id player) max-hp)
          (finish-output)
          t)
        (progn
          (format t "~&Player not found: ~a~%" id-or-username)
          (finish-output)
          nil))))

(defun admin-set-xp (id-or-username new-xp)
  "Set player XP and recalculate level. Returns T on success, NIL on failure."
  (let ((player (admin--find-player id-or-username)))
    (if player
        (progn
          (setf (player-xp player) (max 0 new-xp))
          ;; Recalculate level based on XP
          (loop while (>= (player-xp player)
                         (xp-for-level (1+ (player-hitpoints-level player))))
                do (incf (player-hitpoints-level player)))
          (mark-player-dirty (player-id player))
          (format t "~&Set XP to ~d for player ~a (ID ~d) (level: ~d)~%"
                  (player-xp player) (player-username player) (player-id player)
                  (player-hitpoints-level player))
          (finish-output)
          t)
        (progn
          (format t "~&Player not found: ~a~%" id-or-username)
          (finish-output)
          nil))))

(defun admin-set-level (id-or-username new-level)
  "Set player level and adjust XP to match. Returns T on success, NIL on failure."
  (let ((player (admin--find-player id-or-username)))
    (if player
        (progn
          (setf (player-hitpoints-level player) (max 1 new-level))
          ;; Set XP to minimum for this level
          (setf (player-xp player) (xp-for-level new-level))
          (mark-player-dirty (player-id player))
          (format t "~&Set level to ~d for player ~a (ID ~d) (XP: ~d)~%"
                  new-level (player-username player) (player-id player)
                  (player-xp player))
          (finish-output)
          t)
        (progn
          (format t "~&Player not found: ~a~%" id-or-username)
          (finish-output)
          nil))))

(defun admin-set-coins (id-or-username new-coins)
  "Set player coins (gold). Returns T on success, NIL on failure."
  (let ((player (admin--find-player id-or-username)))
    (if player
        (let ((inventory (player-inventory player)))
          (when inventory
            (setf (inventory-coins inventory) (max 0 new-coins))
            (mark-player-dirty (player-id player))
            (format t "~&Set coins to ~d for player ~a (ID ~d)~%"
                    new-coins (player-username player) (player-id player))
            (finish-output)
            t))
        (progn
          (format t "~&Player not found: ~a~%" id-or-username)
          (finish-output)
          nil))))

(defun admin-teleport (id-or-username zone-or-keyword &optional x y)
  "Teleport player to zone/coords or to another player.
   Usage: (admin-teleport id :overworld 500 300)
          (admin-teleport id :to-player other-id)"
  (let ((player (admin--find-player id-or-username)))
    (if player
        (cond
          ;; Teleport to another player
          ((eq zone-or-keyword :to-player)
           (let ((target-player (admin--find-player x)))
             (if target-player
                 (let ((target-session (gethash (player-id target-player) *player-sessions*)))
                   (setf (player-x player) (player-x target-player))
                   (setf (player-y player) (player-y target-player))
                   (update-player-session-zone (player-id player)
                                               (player-session-zone-id target-session))
                   (mark-player-dirty (player-id player))
                   (format t "~&Teleported player ~a to player ~a (zone: ~a, x:~d y:~d)~%"
                           (player-username player) (player-username target-player)
                           (player-session-zone-id target-session)
                           (player-x player) (player-y player))
                   (finish-output)
                   t)
                 (progn
                   (format t "~&Target player not found: ~a~%" x)
                   (finish-output)
                   nil))))
          ;; Teleport to zone/coords
          ((and (keywordp zone-or-keyword) x y)
           (setf (player-x player) (float x 1.0))
           (setf (player-y player) (float y 1.0))
           (update-player-session-zone (player-id player) zone-or-keyword)
           (mark-player-dirty (player-id player))
           (format t "~&Teleported player ~a to zone ~a (x:~d y:~d)~%"
                   (player-username player) zone-or-keyword x y)
           (finish-output)
           t)
          (t
           (format t "~&Invalid teleport syntax. Use: (admin-teleport id :zone x y) or (admin-teleport id :to-player other-id)~%")
           (finish-output)
           nil))
        (progn
          (format t "~&Player not found: ~a~%" id-or-username)
          (finish-output)
          nil))))

;;; Tier A: Character Management

(defun admin-wipe-character (id-or-username)
  "Delete character from database (irreversible). Returns T on success."
  (let ((player (admin--find-player id-or-username)))
    (if player
        (let ((player-id (player-id player))
              (username (player-username player)))
          (format t "~&WARNING: About to delete character ~a (ID ~d) - irreversible!~%"
                  username player-id)
          (format t "Type 'yes' to confirm: ")
          (finish-output)
          (let ((response (read-line)))
            (if (string-equal response "yes")
                (progn
                  (unregister-player-session player-id)
                  (db-delete-player player-id)
                  (format t "~&Character ~a (ID ~d) deleted.~%" username player-id)
                  (finish-output)
                  t)
                (progn
                  (format t "~&Deletion cancelled.~%")
                  (finish-output)
                  nil))))
        (progn
          (format t "~&Player not found: ~a~%" id-or-username)
          (finish-output)
          nil))))

(defun admin-kick (id-or-username reason)
  "Disconnect player from server. Returns T on success."
  (let ((player (admin--find-player id-or-username)))
    (if player
        (let ((client (admin--find-net-client (player-id player))))
          (if client
              (progn
                ;; Send a message to the client before disconnecting
                (when *server-socket*
                  (send-net-message *server-socket*
                                   (list :type :kick :reason reason)
                                   :host (net-client-host client)
                                   :port (net-client-port client)))
                ;; Save and unregister
                (db-player-logout player)
                (format t "~&Kicked player ~a (ID ~d): ~a~%"
                        (player-username player) (player-id player) reason)
                (finish-output)
                t)
              (progn
                (format t "~&Player ~a is not connected (no net-client found)~%"
                        (player-username player))
                (finish-output)
                nil)))
        (progn
          (format t "~&Player not found: ~a~%" id-or-username)
          (finish-output)
          nil))))

(defun admin-reset-position (id-or-username)
  "Move player to their zone spawn point (unstuck). Returns T on success."
  (let ((player (admin--find-player id-or-username)))
    (if player
        (let* ((session (gethash (player-id player) *player-sessions*))
               (zone-id (player-session-zone-id session))
               (zone-data (get-zone-data zone-id)))
          (when zone-data
            (let ((spawn-x (getf zone-data :spawn-x 500.0))
                  (spawn-y (getf zone-data :spawn-y 300.0)))
              (setf (player-x player) spawn-x)
              (setf (player-y player) spawn-y)
              (mark-player-dirty (player-id player))
              (format t "~&Reset position for player ~a to spawn (~d, ~d) in zone ~a~%"
                      (player-username player) spawn-x spawn-y zone-id)
              (finish-output)
              t)))
        (progn
          (format t "~&Player not found: ~a~%" id-or-username)
          (finish-output)
          nil))))

;;; Tier A: Server Operations

(defun admin-broadcast (message)
  "Send message to all connected players. Returns number of recipients."
  (let ((count 0))
    (when *server-socket*
      (dolist (client *server-clients*)
        (when (net-client-authenticated-p client)
          (send-net-message *server-socket*
                           (list :type :broadcast :message message)
                           :host (net-client-host client)
                           :port (net-client-port client))
          (incf count))))
    (format t "~&Broadcast to ~d player(s): ~a~%" count message)
    (finish-output)
    count))

(defun admin-save-all ()
  "Force flush all dirty players to database. Returns number saved."
  (let ((count (flush-dirty-players)))
    (format t "~&Flushed ~d dirty player(s) to database~%" count)
    (finish-output)
    count))

(defun admin-player-count ()
  "Return current online player count."
  (let ((count (hash-table-count *player-sessions*)))
    (format t "~&Online players: ~d~%" count)
    (finish-output)
    count))

(defun admin-server-stats ()
  "Show server statistics. Returns plist."
  (let* ((uptime-seconds (/ (- (get-internal-real-time) *server-start-time*)
                            internal-time-units-per-second))
         (uptime-minutes (floor uptime-seconds 60))
         (uptime-hours (floor uptime-minutes 60))
         (online-count (hash-table-count *player-sessions*))
         (stats (list :uptime-seconds uptime-seconds
                      :uptime-hours uptime-hours
                      :online-players online-count
                      :total-saves *server-total-saves*)))
    (format t "~&Server Statistics:~%")
    (format t "  Uptime: ~d hours (~,1f seconds)~%" uptime-hours uptime-seconds)
    (format t "  Online players: ~d~%" online-count)
    (format t "  Total saves: ~d~%" *server-total-saves*)
    (finish-output)
    stats))
