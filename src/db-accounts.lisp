(in-package :mmorpg)

;;; db-accounts.lisp - Account management, password hashing, encryption
;;;
;;; Handles account creation/verification, credential management,
;;; password hashing (PBKDF2-SHA256), and auth encryption (X25519 + AES-256-GCM).
;;; Split from db.lisp.
;;;
;;; Load order: db-storage -> db-players -> db-accounts -> db-admin -> db

;;;; Password Hashing (using ironclad PBKDF2-SHA256)

;;; Password hashing parameters are defined in config-server.lisp:
;;;   *password-hash-iterations*, *password-legacy-iterations*, *password-salt-bytes*



(defun generate-salt ()
  "Generate a cryptographically secure random salt."
  (ironclad:random-data *password-salt-bytes*))

(defun derive-password-key (password salt)
  "Derive a key from PASSWORD using PBKDF2-SHA256 with SALT.
   Uses current *password-hash-iterations*. Returns the derived key as a byte vector."
  (let ((password-bytes (ironclad:ascii-string-to-byte-array password)))
    (ironclad:derive-key
     (ironclad:make-kdf 'ironclad:pbkdf2 :digest 'ironclad:sha256)
     password-bytes
     salt
     *password-hash-iterations*
     32)))  ; 32 bytes = 256 bits output

(defun derive-password-key-with-iterations (password salt iterations)
  "Derive a key from PASSWORD using PBKDF2-SHA256 with SALT and explicit ITERATIONS.
   Step 11: Used for verifying hashes with specific iteration counts."
  (let ((password-bytes (ironclad:ascii-string-to-byte-array password)))
    (ironclad:derive-key
     (ironclad:make-kdf 'ironclad:pbkdf2 :digest 'ironclad:sha256)
     password-bytes
     salt
     iterations
     32)))

(defun bytes-to-hex (bytes)
  "Convert byte vector to lowercase hex string."
  (ironclad:byte-array-to-hex-string bytes))

(defun hex-to-bytes (hex-string)
  "Convert hex string to byte vector."
  (ironclad:hex-string-to-byte-array hex-string))

(defun split-hash-string (stored-hash)
  "Split a stored hash string on '$' delimiters.
   Returns a list of parts. 2 parts = legacy format, 3 parts = new format."
  (let ((parts nil)
        (start 0))
    (loop :for i :from 0 :below (length stored-hash)
          :when (char= (char stored-hash i) #\$)
          :do (push (subseq stored-hash start i) parts)
              (setf start (1+ i)))
    (push (subseq stored-hash start) parts)
    (nreverse parts)))

(defun hash-password (password)
  "Hash PASSWORD using PBKDF2-SHA256 with random salt.
   Step 11: Returns 3-part format 'salt$iterations$hash' for storage.
   The iteration count is embedded so verify-password uses the correct count."
  (let* ((salt (generate-salt))
         (key (derive-password-key password salt))
         (salt-hex (bytes-to-hex salt))
         (key-hex (bytes-to-hex key)))
    (format nil "~a$~d$~a" salt-hex *password-hash-iterations* key-hex)))

(defun verify-password (password stored-hash)
  "Verify PASSWORD against STORED-HASH.
   Step 11: Handles both formats:
   - 3-part 'salt$iterations$hash' (new, uses embedded iteration count)
   - 2-part 'salt$hash' (legacy, assumes *password-legacy-iterations*)
   Returns T if password matches, NIL otherwise."
  (let ((parts (split-hash-string stored-hash)))
    (cond
      ;; New format: salt$iterations$hash
      ((= (length parts) 3)
       (let* ((salt-hex (first parts))
              (iterations (handler-case
                              (parse-integer (second parts) :junk-allowed nil)
                            (error () (return-from verify-password nil))))
              (stored-key-hex (third parts))
              (salt (hex-to-bytes salt-hex))
              (computed-key (derive-password-key-with-iterations password salt iterations))
              (computed-key-hex (bytes-to-hex computed-key)))
         ;; Constant-time comparison to prevent timing attacks
         (ironclad:constant-time-equal
          (ironclad:ascii-string-to-byte-array computed-key-hex)
          (ironclad:ascii-string-to-byte-array stored-key-hex))))
      ;; Legacy format: salt$hash (assumes 100k iterations)
      ((= (length parts) 2)
       (let* ((salt-hex (first parts))
              (stored-key-hex (second parts))
              (salt (hex-to-bytes salt-hex))
              (computed-key (derive-password-key-with-iterations
                             password salt *password-legacy-iterations*))
              (computed-key-hex (bytes-to-hex computed-key)))
         (ironclad:constant-time-equal
          (ironclad:ascii-string-to-byte-array computed-key-hex)
          (ironclad:ascii-string-to-byte-array stored-key-hex))))
      ;; Invalid format
      (t nil))))

(defun legacy-hash-format-p (stored-hash)
  "Return T if STORED-HASH uses the legacy 2-part format (salt$hash)."
  (= (length (split-hash-string stored-hash)) 2))

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

(defparameter *account-schema-version* 3
  "Current schema version for account records.
   v1: plaintext passwords
   v2: PBKDF2-SHA256 hashed passwords (salt$hash format, 100k iterations)
   v3: PBKDF2-SHA256 with embedded iteration count (salt$iterations$hash format, 10k default)")

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

(defun db-delete-account (username)
  "Delete account from storage. Returns T if deleted."
  (when (and *storage* username)
    (let ((key (account-key username)))
      (when (storage-delete *storage* key)
        (log-verbose "Deleted account ~a from storage" username)
        t))))

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
  "Verify username/password. Returns T if credentials are valid, NIL otherwise.
   Step 11: Re-hashes legacy accounts on successful login (transparent migration)."
  (let ((account (db-load-account username)))
    (when account
      (let ((stored-hash (getf account :password-hash)))
        (when (and stored-hash (verify-password password stored-hash))
          ;; Step 11: Transparent re-hash migration for legacy accounts
          (when (legacy-hash-format-p stored-hash)
            (let ((new-hash (hash-password password)))
              (handler-case
                  (db-save-account username new-hash (getf account :character-id))
                (error (e)
                  ;; Re-hash failure is non-critical; login still succeeds
                  (warn "Failed to re-hash legacy account ~a: ~a" username e))))
            (log-verbose "Re-hashed legacy account ~a to new format" username))
          t)))))

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

;;;; Pipelined Auth Operations (Step 10: Reduce Redis round-trips)

(defun db-create-account-pipelined (username password character-id)
  "Create account, hash password, and link character-id in one Redis connection.
   Reduces 4-5 separate connections to 1 connection + 3 Redis commands.
   Returns T on success, :USERNAME-TAKEN if username exists.
   Signals on Redis infrastructure errors (does not swallow them).
   Falls back to sequential calls for non-Redis storage."
  (unless *storage*
    (return-from db-create-account-pipelined nil))
  (if (typep *storage* 'redis-storage)
      ;; Redis: single connection with pipelining
      (let ((key (account-key username)))
        (redis:with-recursive-connection (:host (redis-storage-host *storage*)
                                :port (redis-storage-port *storage*))
          ;; Phase 1: Check existence (must be synchronous - conditional on result)
          (let ((exists (red:exists key)))
            (when (or (eq exists t) (and (numberp exists) (plusp exists)))
              (log-verbose "Account creation failed: username ~a already exists" username)
              (return-from db-create-account-pipelined :username-taken)))
          ;; Phase 2: Hash password (CPU work while connection is idle)
          (let* ((password-hash (hash-password password))
                 (data (list :version *account-schema-version*
                             :username (string-downcase username)
                             :password-hash password-hash
                             :character-id character-id))
                 (temp-key (format nil "temp:~a:~a" key (get-internal-real-time))))
            ;; Phase 3: Save account with pipelining (SET + RENAME atomic)
            (redis:with-pipelining
              (red:set temp-key (prin1-to-string data))
              (red:rename temp-key key))
            (log-verbose "Created new account: ~a (pipelined)" username)
            t)))
      ;; Non-Redis: fall back to sequential calls
      (if (db-create-account username password)
          (progn
            (db-set-character-id username character-id)
            t)
          :username-taken)))

(defun db-verify-and-load-account (username password)
  "Verify credentials and return character-id in one operation.
   Loads the account record once instead of twice (eliminates redundant
   db-load-account call between db-verify-credentials and db-get-character-id).
   Step 11: Also re-hashes legacy accounts on successful login.
   Returns (values character-id T) on success,
   (values nil nil) on bad credentials or not found."
  (let ((account (db-load-account username)))
    (when account
      (let ((stored-hash (getf account :password-hash)))
        (when (and stored-hash (verify-password password stored-hash))
          ;; Step 11: Transparent re-hash migration for legacy accounts
          (when (legacy-hash-format-p stored-hash)
            (let ((new-hash (hash-password password)))
              (handler-case
                  (db-save-account username new-hash (getf account :character-id))
                (error (e)
                  (warn "Failed to re-hash legacy account ~a: ~a" username e))))
            (log-verbose "Re-hashed legacy account ~a to new format" username))
          (values (getf account :character-id) t))))))
