(in-package :mmorpg)

;;; db.lisp - Storage abstraction layer and Redis persistence
;;;
;;; Implements the database architecture specified in docs/db.md.
;;; Provides storage-agnostic interface for game persistence.
;;;
;;; Key concepts:
;;; - Storage abstraction: game code never knows what DB it uses
;;; - Tiered writes: tier-1 (immediate), tier-2 (batched), tier-3 (logout)
;;; - Versioned serialization: all data includes version for migrations
;;; - Durable vs ephemeral: explicit classification of all state
;;;
;;; Storage backends:
;;; - redis-storage: Production backend using cl-redis
;;; - memory-storage: In-memory backend for testing (no external deps)

;;;; Storage Protocol (Abstract Interface)

(defgeneric storage-load (storage key)
  (:documentation "Load data for KEY. Returns plist or NIL if not found."))

(defgeneric storage-save (storage key data)
  (:documentation "Save DATA under KEY. Returns T on success."))

(defgeneric storage-delete (storage key)
  (:documentation "Delete KEY. Returns T if existed."))

(defgeneric storage-exists-p (storage key)
  (:documentation "Return T if KEY exists."))

(defgeneric storage-flush (storage)
  (:documentation "Force any pending writes to durable storage."))

(defgeneric storage-connect (storage)
  (:documentation "Establish connection to storage backend."))

(defgeneric storage-disconnect (storage)
  (:documentation "Close connection to storage backend."))

;;;; Redis Storage Implementation

(defclass redis-storage ()
  ((host :initarg :host
         :initform "127.0.0.1"
         :accessor redis-storage-host
         :documentation "Redis server hostname")
   (port :initarg :port
         :initform 6379
         :accessor redis-storage-port
         :documentation "Redis server port")
   (connected :initform nil
              :accessor redis-storage-connected
              :documentation "Whether connection is established"))
  (:documentation "Redis-backed storage implementation using cl-redis."))

(defmethod storage-connect ((storage redis-storage))
  "Establish Redis connection. Connection is per-thread in cl-redis."
  ;; cl-redis uses dynamic variables for connection management
  ;; The actual connection happens in with-connection macro
  (setf (redis-storage-connected storage) t)
  (log-verbose "Redis storage configured for ~a:~a"
               (redis-storage-host storage)
               (redis-storage-port storage))
  t)

(defmethod storage-disconnect ((storage redis-storage))
  "Mark Redis storage as disconnected."
  (setf (redis-storage-connected storage) nil)
  (log-verbose "Redis storage disconnected")
  t)

(defmethod storage-load ((storage redis-storage) key)
  "Load data from Redis. Returns plist or NIL if not found."
  (handler-case
      (redis:with-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (let ((raw (red:get key)))
          (when raw
            (let ((*read-eval* nil)) ; Security: disable eval in read
              (read-from-string raw)))))
    (error (e)
      (warn "Redis load error for key ~a: ~a" key e)
      nil)))

(defmethod storage-save ((storage redis-storage) key data)
  "Save data to Redis. Returns T on success."
  (handler-case
      (progn
        (redis:with-connection (:host (redis-storage-host storage)
                                :port (redis-storage-port storage))
          (red:set key (prin1-to-string data)))
        t)
    (error (e)
      (warn "Redis save error for key ~a: ~a" key e)
      nil)))

(defmethod storage-delete ((storage redis-storage) key)
  "Delete key from Redis. Returns T if existed."
  (handler-case
      (redis:with-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (> (red:del key) 0))
    (error (e)
      (warn "Redis delete error for key ~a: ~a" key e)
      nil)))

(defmethod storage-exists-p ((storage redis-storage) key)
  "Check if key exists in Redis."
  (handler-case
      (redis:with-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (= (red:exists key) 1))
    (error (e)
      (warn "Redis exists check error for key ~a: ~a" key e)
      nil)))

(defmethod storage-flush ((storage redis-storage))
  "Trigger Redis BGSAVE for immediate snapshot."
  (handler-case
      (redis:with-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (red:bgsave)
        (log-verbose "Redis BGSAVE triggered")
        t)
    (error (e)
      (warn "Redis flush error: ~a" e)
      nil)))

;;;; Memory Storage Implementation (for testing)

(defclass memory-storage ()
  ((data :initform (make-hash-table :test 'equal)
         :accessor memory-storage-data
         :documentation "Hash table storing all data"))
  (:documentation "In-memory storage for testing. No external dependencies."))

(defmethod storage-connect ((storage memory-storage))
  "No-op for memory storage."
  (log-verbose "Memory storage initialized")
  t)

(defmethod storage-disconnect ((storage memory-storage))
  "No-op for memory storage."
  (log-verbose "Memory storage disconnected")
  t)

(defmethod storage-load ((storage memory-storage) key)
  "Load data from memory hash table."
  (gethash key (memory-storage-data storage)))

(defmethod storage-save ((storage memory-storage) key data)
  "Save data to memory hash table."
  (setf (gethash key (memory-storage-data storage)) data)
  t)

(defmethod storage-delete ((storage memory-storage) key)
  "Delete key from memory hash table."
  (remhash key (memory-storage-data storage)))

(defmethod storage-exists-p ((storage memory-storage) key)
  "Check if key exists in memory hash table."
  (nth-value 1 (gethash key (memory-storage-data storage))))

(defmethod storage-flush ((storage memory-storage))
  "No-op for memory storage (already durable in RAM)."
  t)

;;;; Key Schema Functions

(defun player-key (player-id)
  "Generate Redis key for player data."
  (format nil "player:~a" player-id))

(defun zone-objects-key (zone-id)
  "Generate Redis key for zone object state."
  (format nil "zone:~a:objects" zone-id))

(defun server-config-key ()
  "Generate Redis key for server configuration."
  "server:config")

(defun server-id-counter-key ()
  "Generate Redis key for global ID counter."
  "server:id-counter")

;;;; Global Storage Instance

(defparameter *storage* nil
  "Global storage backend instance. Set at startup to redis-storage or memory-storage.")

(defun init-storage (&key (backend :redis) (host "127.0.0.1") (port 6379))
  "Initialize global storage backend.
   BACKEND can be :redis or :memory.
   For :redis, HOST and PORT specify the Redis server."
  (setf *storage*
        (ecase backend
          (:redis (make-instance 'redis-storage :host host :port port))
          (:memory (make-instance 'memory-storage))))
  (storage-connect *storage*)
  (log-verbose "Storage initialized: ~a" backend)
  *storage*)

(defun shutdown-storage ()
  "Disconnect from storage backend."
  (when *storage*
    (storage-disconnect *storage*)
    (setf *storage* nil)))

;;;; Migration System

(defparameter *player-schema-version* 1
  "Current player schema version. Increment when changing player format.")

(defparameter *player-migrations* nil
  "Alist of (version . migration-function) for player data.
   Each migration function takes a plist and returns updated plist.
   Example: ((2 . migrate-player-v1->v2) (3 . migrate-player-v2->v3))")

(defun migrate-player-data (data)
  "Migrate player data to current schema version.
   Runs migration chain from data's version to *player-schema-version*."
  (let ((version (getf data :version 0)))
    (when (> version *player-schema-version*)
      (warn "Player data version ~d is newer than current version ~d"
            version *player-schema-version*))
    (loop while (< version *player-schema-version*)
          do (let ((migrator (cdr (assoc (1+ version) *player-migrations*))))
               (if migrator
                   (progn
                     (setf data (funcall migrator data))
                     (log-verbose "Migrated player data from v~d to v~d"
                                  version (1+ version)))
                   (warn "No migrator found for version ~d to ~d"
                         version (1+ version)))
               (incf version)))
    (setf (getf data :version) *player-schema-version*)
    data))

;;;; High-Level Convenience Functions

(defun db-save-player (player)
  "Save player to storage using current schema version."
  (when (and *storage* player)
    (let* ((player-id (player-id player))
           (key (player-key player-id))
           (session (gethash player-id *player-sessions*))
           (zone-id (and session (player-session-zone-id session)))
           ;; Use serialize-player from save.lisp (no visuals for DB, include zone-id)
           (data (serialize-player player :include-visuals nil :zone-id zone-id)))
      ;; Add version to serialized data
      (setf (getf data :version) *player-schema-version*)
      (storage-save *storage* key data)
      (log-verbose "Saved player ~a to storage (zone: ~a)" player-id zone-id)
      t)))

(defun db-load-player (player-id)
  "Load player by ID, running migrations if needed.
   Returns player struct or NIL if not found."
  (when *storage*
    (let* ((key (player-key player-id))
           (data (storage-load *storage* key)))
      (when data
        ;; Run migrations
        (setf data (migrate-player-data data))
        ;; Deserialize using save.lisp functions
        (let ((player (deserialize-player data
                                          *inventory-size*
                                          (length *equipment-slot-ids*))))
          (log-verbose "Loaded player ~a from storage (version ~a)"
                       player-id (getf data :version))
          player)))))

(defun db-delete-player (player-id)
  "Delete player from storage."
  (when *storage*
    (let ((key (player-key player-id)))
      (storage-delete *storage* key)
      (log-verbose "Deleted player ~a from storage" player-id))))

(defun db-player-exists-p (player-id)
  "Check if player exists in storage."
  (when *storage*
    (storage-exists-p *storage* (player-key player-id))))

(defun db-save-zone-objects (zone-id objects)
  "Save zone objects to storage."
  (when *storage*
    (let ((key (zone-objects-key zone-id))
          (data (list :version 1
                      :zone-id zone-id
                      :objects (mapcar #'serialize-object objects))))
      (storage-save *storage* key data)
      (log-verbose "Saved zone ~a objects to storage" zone-id))))

(defun db-load-zone-objects (zone-id)
  "Load zone objects from storage. Returns list of objects or NIL."
  (when *storage*
    (let* ((key (zone-objects-key zone-id))
           (data (storage-load *storage* key)))
      (when data
        (mapcar #'deserialize-object (getf data :objects))))))

;;;; Dirty Flag System (for Tier-2 Batched Writes)

(defstruct player-session
  "Tracks persistence state for a connected player."
  (player nil :type (or null player))
  (zone-id nil :type (or null symbol))
  (dirty-p nil :type boolean)
  (last-flush 0.0 :type float)
  (tier1-pending nil :type list))

(defparameter *player-sessions* (make-hash-table :test 'eql)
  "Map of player-id -> player-session for connected players.")

(defparameter *batch-flush-interval* 30.0
  "Seconds between batch flushes for tier-2 writes.")

(defun mark-player-dirty (player-id)
  "Mark player as needing a database flush."
  (let ((session (gethash player-id *player-sessions*)))
    (when session
      (setf (player-session-dirty-p session) t)
      (log-verbose "Marked player ~a as dirty" player-id))))

(defun register-player-session (player &key (zone-id nil))
  "Register a player session when they login."
  (let ((player-id (player-id player)))
    (setf (gethash player-id *player-sessions*)
          (make-player-session :player player
                               :zone-id zone-id
                               :dirty-p nil
                               :last-flush (float (get-internal-real-time) 1.0)
                               :tier1-pending nil))
    (log-verbose "Registered session for player ~a in zone ~a" player-id zone-id)))

(defun update-player-session-zone (player-id zone-id)
  "Update the zone-id for a player session (called on zone transitions)."
  (let ((session (gethash player-id *player-sessions*)))
    (when session
      (setf (player-session-zone-id session) zone-id)
      (log-verbose "Updated session zone for player ~a to ~a" player-id zone-id))))

(defun unregister-player-session (player-id)
  "Unregister a player session when they logout."
  (remhash player-id *player-sessions*)
  (log-verbose "Unregistered session for player ~a" player-id))

(defun flush-dirty-players (&key force)
  "Flush all dirty players to storage (tier-2 batched writes).
   If FORCE is T, flush all players regardless of dirty flag."
  (let ((flushed-count 0)
        (current-time (get-internal-real-time)))
    (maphash
     (lambda (player-id session)
       (let ((player (player-session-player session))
             (dirty (player-session-dirty-p session))
             (last-flush (player-session-last-flush session)))
         (when (and player
                    (or force
                        dirty
                        (> (- current-time last-flush)
                           (* *batch-flush-interval* internal-time-units-per-second))))
           (db-save-player player)
           (setf (player-session-dirty-p session) nil)
           (setf (player-session-last-flush session) (float current-time 1.0))
           (incf flushed-count))))
     *player-sessions*)
    (when (> flushed-count 0)
      (log-verbose "Flushed ~a player(s) to storage" flushed-count))
    flushed-count))

;;;; Tier-1 Immediate Write Operations

(defun db-save-player-immediate (player)
  "Tier-1 immediate write: save player and wait for confirmation.
   Use for critical operations: trade, bank, death, level-up, item destruction."
  (db-save-player player)
  ;; For Redis with AOF, write is durable after return
  ;; Mark as not dirty since we just saved
  (let ((session (gethash (player-id player) *player-sessions*)))
    (when session
      (setf (player-session-dirty-p session) nil)
      (setf (player-session-last-flush session) (float (get-internal-real-time) 1.0))))
  (log-verbose "Tier-1 immediate save for player ~a" (player-id player))
  t)

;;;; Login/Logout Operations

(defun db-login-player (player-id)
  "Load player from storage on login. Returns player or NIL."
  (let ((player (db-load-player player-id)))
    (when player
      (register-player-session player)
      (log-verbose "Player ~a logged in from storage" player-id))
    player))

(defun db-logout-player (player)
  "Save player to storage on logout (tier-3 write)."
  (when player
    (let ((player-id (player-id player)))
      (db-save-player-immediate player)
      (unregister-player-session player-id)
      (log-verbose "Player ~a logged out, saved to storage" player-id))))

;;;; Graceful Shutdown

(defun db-shutdown-flush ()
  "Gracefully flush all data during server shutdown.
   1. Flush all dirty players
   2. Trigger Redis BGSAVE
   3. Close storage connection"
  (log-verbose "Starting graceful shutdown flush")
  (flush-dirty-players :force t)
  (when *storage*
    (storage-flush *storage*))
  (shutdown-storage)
  (log-verbose "Graceful shutdown flush completed"))

;;;; Durable vs Ephemeral State Documentation

;;; As specified in AGENTS.md and docs/db.md:
;;;
;;; DURABLE (must persist):
;;; - Progression: XP, levels, skill levels, combat stats
;;; - Health: Current HP (prevents logout-heal exploit)
;;; - Inventory: Items, equipment, stack counts
;;; - Currency: Gold, bank contents (when implemented)
;;; - Position: Zone ID, X/Y coordinates
;;; - Quests, Achievements, Social data, Settings (when implemented)
;;;
;;; EPHEMERAL (OK to lose):
;;; - Temporary buffs, debuffs, cooldowns
;;; - Current attack/follow target (reset on login)
;;; - AI state, pathfinding cache
;;; - Animation frame, visual effects (client-side)
;;; - Party invites, trade windows (session-only)
;;;
;;; The serialize-player function in save.lisp handles this classification.
;;; Visual fields are excluded from DB saves (:include-visuals nil).
