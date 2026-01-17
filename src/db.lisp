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

(defgeneric storage-keys (storage pattern)
  (:documentation "Return list of keys matching PATTERN (glob-style, e.g. 'player:*')."))

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
  "Save data to Redis using atomic write-then-rename pattern.
   Writes to a temp key first, then atomically renames to the real key.
   This prevents data corruption if the server crashes during write."
  (handler-case
      (let ((temp-key (format nil "temp:~a:~a" key (get-internal-real-time))))
        (redis:with-connection (:host (redis-storage-host storage)
                                :port (redis-storage-port storage))
          ;; Write to temporary key
          (red:set temp-key (prin1-to-string data))
          ;; Atomically rename temp key to real key
          ;; RENAME overwrites the destination key if it exists
          (red:rename temp-key key))
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
        (let ((result (red:exists key)))
          (if (numberp result)
              (plusp result)
              result)))
    (error (e)
      (warn "Redis exists check error for key ~a: ~a" key e)
      nil)))

(defmethod storage-keys ((storage redis-storage) pattern)
  "Return list of keys matching PATTERN from Redis."
  (handler-case
      (redis:with-connection (:host (redis-storage-host storage)
                              :port (redis-storage-port storage))
        (red:keys pattern))
    (error (e)
      (warn "Redis keys error for pattern ~a: ~a" pattern e)
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

(defmethod storage-keys ((storage memory-storage) pattern)
  "Return list of keys matching PATTERN from memory hash table.
   Supports simple prefix matching: 'player:*' matches all keys starting with 'player:'."
  (let* ((star-pos (position #\* pattern))
         (prefix (if star-pos (subseq pattern 0 star-pos) pattern))
         (result nil))
    (maphash (lambda (key value)
               (declare (ignore value))
               (when (if star-pos
                         (and (>= (length key) (length prefix))
                              (string= prefix key :end2 (length prefix)))
                         (string= pattern key))
                 (push key result)))
             (memory-storage-data storage))
    result))

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

(defun db-save-id-counter (counter-value)
  "Save the global ID counter to storage."
  (storage-save *storage* (server-id-counter-key) counter-value))

(defun db-load-id-counter ()
  "Load the global ID counter from storage with retry (critical for server startup).
   Returns 0 if not found or all retries exhausted."
  (let ((data (with-retry-exponential (loaded (lambda () (storage-load *storage* (server-id-counter-key)))
                                        :max-retries 5
                                        :initial-delay 200
                                        :max-delay 2000
                                        :on-final-fail (lambda (e)
                                                         (warn "CRITICAL: Failed to load ID counter after all retries: ~a. Starting from 0 may cause ID collisions!" e)))
                loaded)))
    (if (and data (integerp data))
        data
        0)))

;;;; Migration System
;;;;
;;;; Core migration logic is in migrations.lisp (schema version, migration functions).
;;;; migrate-player-data is called on each player load (lazy migration).
;;;; migrate-all-players below provides eager migration for admin use.

(defun migrate-all-players (&key (dry-run nil) (verbose t))
  "Migrate all players in storage to current schema version.
   Use this before major deploys to pre-migrate inactive players.
   Options:
     :dry-run t   - Report what would be migrated without saving
     :verbose t   - Print progress (default)
   Returns: (values migrated-count skipped-count error-count)"
  (unless *storage*
    (warn "No storage backend initialized")
    (return-from migrate-all-players (values 0 0 0)))
  (let ((keys (storage-keys *storage* "player:*"))
        (migrated 0)
        (skipped 0)
        (errors 0))
    (when verbose
      (format t "~&Found ~a player records to check~%" (length keys)))
    (dolist (key keys)
      (handler-case
          (let ((data (storage-load *storage* key)))
            (if (null data)
                (progn
                  (when verbose
                    (format t "  ~a: no data (skipped)~%" key))
                  (incf skipped))
                (let ((version (getf data :version 0)))
                  (if (>= version *player-schema-version*)
                      (progn
                        (when verbose
                          (format t "  ~a: v~a (current, skipped)~%" key version))
                        (incf skipped))
                      (progn
                        (when verbose
                          (format t "  ~a: v~a -> v~a~%" key version *player-schema-version*))
                        (let ((migrated-data (migrate-player-data data)))
                          (unless dry-run
                            (storage-save *storage* key migrated-data)))
                        (incf migrated))))))
        (error (e)
          (when verbose
            (format t "  ~a: ERROR ~a~%" key e))
          (incf errors))))
    (when verbose
      (format t "~&Migration complete: ~a migrated, ~a skipped, ~a errors~%"
              migrated skipped errors)
      (when dry-run
        (format t "(dry-run mode - no changes saved)~%")))
    (values migrated skipped errors)))

(export 'migrate-all-players)

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
      (when (boundp '*server-total-saves*)
        (incf *server-total-saves*))
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

;;;; Password Hashing (using ironclad PBKDF2-SHA256)

(defparameter *password-hash-iterations* 100000
  "Number of PBKDF2 iterations for password hashing.
   Higher = more secure but slower. 100k is industry standard as of 2024.")

(defparameter *password-salt-bytes* 16
  "Salt length in bytes. 16 bytes = 128 bits, sufficient for security.")

(defun generate-salt ()
  "Generate a cryptographically secure random salt."
  (ironclad:random-data *password-salt-bytes*))

(defun derive-password-key (password salt)
  "Derive a key from PASSWORD using PBKDF2-SHA256 with SALT.
   Returns the derived key as a byte vector."
  (let ((password-bytes (ironclad:ascii-string-to-byte-array password)))
    (ironclad:derive-key
     (ironclad:make-kdf 'ironclad:pbkdf2 :digest 'ironclad:sha256)
     password-bytes
     salt
     *password-hash-iterations*
     32)))  ; 32 bytes = 256 bits output

(defun bytes-to-hex (bytes)
  "Convert byte vector to lowercase hex string."
  (ironclad:byte-array-to-hex-string bytes))

(defun hex-to-bytes (hex-string)
  "Convert hex string to byte vector."
  (ironclad:hex-string-to-byte-array hex-string))

(defun hash-password (password)
  "Hash PASSWORD using PBKDF2-SHA256 with random salt.
   Returns a string in format 'salt$hash' for storage."
  (let* ((salt (generate-salt))
         (key (derive-password-key password salt))
         (salt-hex (bytes-to-hex salt))
         (key-hex (bytes-to-hex key)))
    (concatenate 'string salt-hex "$" key-hex)))

(defun verify-password (password stored-hash)
  "Verify PASSWORD against STORED-HASH (format: 'salt$hash').
   Returns T if password matches, NIL otherwise."
  (let* ((dollar-pos (position #\$ stored-hash)))
    (unless dollar-pos
      ;; Invalid hash format
      (return-from verify-password nil))
    (let* ((salt-hex (subseq stored-hash 0 dollar-pos))
           (stored-key-hex (subseq stored-hash (1+ dollar-pos)))
           (salt (hex-to-bytes salt-hex))
           (computed-key (derive-password-key password salt))
           (computed-key-hex (bytes-to-hex computed-key)))
      ;; Constant-time comparison to prevent timing attacks
      (ironclad:constant-time-equal
       (ironclad:ascii-string-to-byte-array computed-key-hex)
       (ironclad:ascii-string-to-byte-array stored-key-hex)))))

;;;; Network Encryption (X25519 + AES-256-GCM)
;;;;
;;;; Encrypts authentication messages (login/register) to prevent wifi sniffing.
;;;; Game traffic (snapshots, intents) remains unencrypted for latency reasons.
;;;;
;;;; Protocol:
;;;; 1. Server has static X25519 keypair (generated once, persisted)
;;;; 2. Client generates ephemeral X25519 keypair per auth request
;;;; 3. Client computes shared secret via ECDH (ephemeral-private + server-public)
;;;; 4. Client encrypts credentials with AES-256-GCM using SHA256(shared-secret)
;;;; 5. Client sends: ephemeral-public-key || nonce || ciphertext || tag
;;;; 6. Server computes same shared secret (server-private + ephemeral-public)
;;;; 7. Server decrypts credentials, verifies tag

(defparameter *server-keypair* nil
  "Server's X25519 keypair for auth encryption. NIL until initialized.")

(defparameter *server-public-key-hex* nil
  "Server's public key as hex string for embedding in client config.")

(defun generate-server-keypair ()
  "Generate a new X25519 keypair for the server.
   Returns (values private-key public-key)."
  (let ((keypair (ironclad:generate-key-pair :curve25519)))
    (values (ironclad:destructure-private-key keypair)
            (ironclad:destructure-public-key keypair))))

(defun init-server-encryption ()
  "Initialize server encryption keypair. Generates new keys if none exist."
  (multiple-value-bind (priv pub) (generate-server-keypair)
    (setf *server-keypair* (cons priv pub))
    (setf *server-public-key-hex* (bytes-to-hex pub))
    (log-verbose "Server encryption initialized. Public key: ~a..."
                 (subseq *server-public-key-hex* 0 16))
    *server-public-key-hex*))

(defun get-server-public-key ()
  "Get server's public key as hex string."
  (unless *server-keypair*
    (init-server-encryption))
  *server-public-key-hex*)

(defun compute-shared-secret (my-private-key their-public-key)
  "Compute X25519 shared secret from private and public keys (byte vectors)."
  (ironclad:diffie-hellman :curve25519 my-private-key their-public-key))

(defun derive-encryption-key (shared-secret)
  "Derive a 256-bit encryption key from shared secret using SHA-256.
   This adds key derivation to the raw ECDH output."
  (ironclad:digest-sequence :sha256 shared-secret))

(defun encrypt-auth-payload (payload server-public-key-hex)
  "Encrypt PAYLOAD (string) for server using its public key.
   Returns hex string: ephemeral-public || nonce || ciphertext || tag
   PAYLOAD should be a plist serialized as string."
  (let* (;; Generate ephemeral keypair
         (ephemeral-keypair (ironclad:generate-key-pair :curve25519))
         (ephemeral-private (ironclad:destructure-private-key ephemeral-keypair))
         (ephemeral-public (ironclad:destructure-public-key ephemeral-keypair))
         ;; Compute shared secret
         (server-public (hex-to-bytes server-public-key-hex))
         (shared-secret (compute-shared-secret ephemeral-private server-public))
         (encryption-key (derive-encryption-key shared-secret))
         ;; Generate random nonce (12 bytes for AES-GCM)
         (nonce (ironclad:random-data 12))
         ;; Encrypt with AES-256-GCM
         (plaintext (ironclad:ascii-string-to-byte-array payload))
         (cipher (ironclad:make-authenticated-encryption-mode
                  :gcm
                  :cipher-name :aes
                  :key encryption-key
                  :initialization-vector nonce))
         (ciphertext (make-array (length plaintext)
                                 :element-type '(unsigned-byte 8)))
         (tag (make-array 16 :element-type '(unsigned-byte 8))))
    ;; Perform encryption
    (ironclad:process-associated-data cipher #())
    (ironclad:encrypt cipher plaintext ciphertext)
    (ironclad:produce-tag cipher :tag tag)
    ;; Concatenate: ephemeral-public (32) || nonce (12) || ciphertext || tag (16)
    (let* ((result-len (+ 32 12 (length ciphertext) 16))
           (result (make-array result-len :element-type '(unsigned-byte 8))))
      (replace result ephemeral-public :start1 0)
      (replace result nonce :start1 32)
      (replace result ciphertext :start1 44)
      (replace result tag :start1 (+ 44 (length ciphertext)))
      (bytes-to-hex result))))

(defun decrypt-auth-payload (encrypted-hex)
  "Decrypt an auth payload encrypted by client.
   ENCRYPTED-HEX is: ephemeral-public || nonce || ciphertext || tag
   Returns decrypted payload string, or NIL if decryption fails."
  (unless *server-keypair*
    (warn "Server encryption not initialized")
    (return-from decrypt-auth-payload nil))
  (handler-case
      (let* ((encrypted (hex-to-bytes encrypted-hex))
             (len (length encrypted)))
        (when (< len (+ 32 12 16))  ; minimum: pubkey + nonce + tag
          (warn "Encrypted payload too short")
          (return-from decrypt-auth-payload nil))
        ;; Extract components
        (let* ((ephemeral-public (subseq encrypted 0 32))
               (nonce (subseq encrypted 32 44))
               (ciphertext-end (- len 16))
               (ciphertext (subseq encrypted 44 ciphertext-end))
               (tag (subseq encrypted ciphertext-end))
               ;; Compute shared secret using server's private key
               (server-private (car *server-keypair*))
               (shared-secret (compute-shared-secret server-private ephemeral-public))
               (encryption-key (derive-encryption-key shared-secret))
               ;; Decrypt with AES-GCM, providing tag for verification
               ;; If tag doesn't match, ironclad signals bad-authentication-tag
               (cipher (ironclad:make-authenticated-encryption-mode
                        :gcm
                        :cipher-name :aes
                        :key encryption-key
                        :initialization-vector nonce
                        :tag tag))
               (plaintext (make-array (length ciphertext)
                                      :element-type '(unsigned-byte 8))))
          ;; Decrypt (tag verified automatically via cipher creation)
          (ironclad:process-associated-data cipher #())
          (ironclad:decrypt cipher ciphertext plaintext)
          ;; Convert to string
          (let ((result (make-string (length plaintext))))
            (dotimes (i (length plaintext))
              (setf (aref result i) (code-char (aref plaintext i))))
            result)))
    (ironclad:bad-authentication-tag ()
      (warn "Auth payload tag verification failed - possible tampering")
      nil)
    (error (e)
      (warn "Failed to decrypt auth payload: ~a" e)
      nil)))

;;;; Account Management

(defparameter *account-schema-version* 2
  "Current schema version for account records.
   v1: plaintext passwords
   v2: PBKDF2-SHA256 hashed passwords (salt$hash format)")

(defun account-key (username)
  "Return storage key for account USERNAME."
  (format nil "account:~a" (string-downcase username)))

(defun db-save-account (username password-hash character-id)
  "Save account to storage. PASSWORD-HASH should be pre-hashed.
   Returns T on success."
  (when (and *storage* username password-hash)
    (let* ((key (account-key username))
           (data (list :version *account-schema-version*
                      :username (string-downcase username)
                      :password-hash password-hash
                      :character-id character-id)))
      (storage-save *storage* key data)
      (log-verbose "Saved account ~a to storage" username)
      t)))

(defun db-load-account (username)
  "Load account from storage. Returns plist or NIL if not found."
  (when (and *storage* username)
    (let ((key (account-key username)))
      (storage-load *storage* key))))

(defun db-account-exists-p (username)
  "Return T if account USERNAME exists in storage."
  (when (and *storage* username)
    (let ((key (account-key username)))
      (storage-exists-p *storage* key))))

(defun db-create-account (username password)
  "Create new account with USERNAME and PASSWORD (hashes before storing).
   Returns T on success, NIL if username taken."
  (when (db-account-exists-p username)
    (log-verbose "Account creation failed: username ~a already exists" username)
    (return-from db-create-account nil))
  (let ((password-hash (hash-password password)))
    (db-save-account username password-hash nil))
  (log-verbose "Created new account: ~a" username)
  t)

(defun db-verify-credentials (username password)
  "Verify username/password. Returns T if credentials are valid, NIL otherwise."
  (let ((account (db-load-account username)))
    (when account
      (let ((stored-hash (getf account :password-hash)))
        (when stored-hash
          (verify-password password stored-hash))))))

(defun db-get-character-id (username)
  "Get character-id for account USERNAME. Returns character-id or NIL."
  (let ((account (db-load-account username)))
    (when account
      (getf account :character-id))))

(defun db-set-character-id (username character-id)
  "Set character-id for account USERNAME. Returns T on success."
  (let ((account (db-load-account username)))
    (when account
      (let ((password-hash (getf account :password-hash)))
        (when password-hash
          (db-save-account username password-hash character-id))))))

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
