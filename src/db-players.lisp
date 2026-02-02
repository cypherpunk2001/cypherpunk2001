(in-package :mmorpg)

;;; db-players.lisp - Player save/load, dirty flags, sessions, leaderboards
;;;
;;; Player persistence operations, session management, ownership, tier-1/2/3
;;; writes, leaderboards, and online tracking. Split from db.lisp.
;;;
;;; Load order: db-storage -> db-players -> db-accounts -> db-admin -> db

;;;; ========================================================================
;;;; FORENSIC STORAGE (Phase 6 - 4-Outcome Validation System)
;;;; Store corrupt blobs for admin inspection, validation metrics
;;;; ========================================================================

(defparameter *corrupt-blob-ttl-seconds* 604800
  "TTL for corrupt blob forensic storage (7 days = 604800 seconds).")

(defun store-corrupt-blob (player-id raw-string report)
  "Store corrupt player data for forensic inspection with TTL.
   KEY: corrupt:{player-id}:{timestamp}
   DATA: (:raw raw-string :report report-list :timestamp unix-time)
   TTL: 7 days (auto-expires to prevent unbounded growth)"
  (when *storage*
    (let ((key (format nil "corrupt:~a:~a" player-id (get-universal-time)))
          (data (list :raw raw-string
                      :report report
                      :timestamp (get-universal-time))))
      (storage-save-with-ttl *storage* key data *corrupt-blob-ttl-seconds*)
      (log-verbose "Stored corrupt blob for player ~a: ~{~a~^, ~}" player-id report))))

(defun increment-validation-metric (action)
  "Increment validation outcome counter.
   ACTION should be :ok, :clamp, :quarantine, or :reject."
  (when *storage*
    (storage-incr *storage* (format nil "metrics:validation:~a"
                                     (string-downcase (symbol-name action))))))

(defun get-validation-metrics ()
  "Return validation metrics as a plist.
   Returns (:ok N :clamp N :quarantine N :reject N)"
  (when *storage*
    (list :ok (or (storage-load *storage* "metrics:validation:ok") 0)
          :clamp (or (storage-load *storage* "metrics:validation:clamp") 0)
          :quarantine (or (storage-load *storage* "metrics:validation:quarantine") 0)
          :reject (or (storage-load *storage* "metrics:validation:reject") 0))))

(defun db-save-id-counter (counter-value)
  "Save the global ID counter to storage."
  (storage-save *storage* (server-id-counter-key) counter-value))

(defun db-load-id-counter ()
  "Load the global ID counter from storage with retry (critical for server startup).
   Returns 1 if not found or all retries exhausted (ID 0 is reserved/invalid)."
  (let ((data (with-retry-exponential (loaded (lambda () (storage-load *storage* (server-id-counter-key)))
                                        :max-retries 5
                                        :initial-delay 200
                                        :max-delay 2000
                                        :on-final-fail (lambda (e)
                                                         (warn "CRITICAL: Failed to load ID counter after all retries: ~a. Starting from 1 may cause ID collisions!" e)))
                loaded)))
    (if (and data (integerp data) (> data 0))
        data
        1)))

;;;; High-Level Convenience Functions

(defun db-save-player (player)
  "Save player to storage using current schema version.
   Returns T on success. Signals STORAGE-ERROR on failure (Phase 1).
   Callers using with-retry-exponential will automatically retry on error."
  (when (and *storage* player)
    (let* ((player-id (player-id player))
           (key (player-key player-id))
           (session (gethash player-id *player-sessions*))
           (zone-id (and session (player-session-zone-id session)))
           ;; Use serialize-player from save.lisp (no visuals for DB, include zone-id)
           (data (serialize-player player :include-visuals nil :zone-id zone-id)))
      ;; Add version to serialized data
      (setf data (plist-put data :version *player-schema-version*))
      ;; storage-save signals storage-error on failure (Phase 1)
      (storage-save *storage* key data)
      ;; Only update counters and log on success
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
        (let* ((player (deserialize-player data
                                           *inventory-size*
                                           (length *equipment-slot-ids*)))
               (zone-id (getf data :zone-id)))
          (log-verbose "Loaded player ~a from storage (version ~a)"
                       player-id (getf data :version))
          (values player zone-id))))))

(defun db-load-player-validated (player-id)
  "Load player with 4-outcome validation.
   Returns (values player zone-id action).

   PRECONDITION: Caller has already claimed session ownership for player-id.
   This ensures that if the :clamp branch needs to save corrected data,
   the ownership-safe save path will succeed.

   Actions returned:
   - :ok          = Data valid, player loaded normally
   - :clamp       = Minor issues fixed, corrected data saved, player loaded
   - :quarantine  = Suspicious data, quarantine player returned (can't play)
   - :reject      = Exploit-adjacent data, login denied (nil player)
   - :not-found   = No data for player-id (nil player)

   The load pipeline is:
   1. Load raw string (storage-load-raw)
   2. Size check BEFORE parsing (prevents allocation attacks)
   3. Parse with read-from-string (*read-eval* nil)
   4. Basic sanity (is it a plist? has :id? :version OK?)
   5. Migrate to current schema
   6. 4-way validation against current schema
   7. Handle outcome

   See docs/db.md 'Phase 6: 4-Outcome Validation System' for details."
  (unless *storage*
    (return-from db-load-player-validated (values nil nil :not-found)))

  (let* ((key (player-key player-id))
         (raw-string (storage-load-raw *storage* key)))

    ;; No data found
    (unless raw-string
      (return-from db-load-player-validated (values nil nil :not-found)))

    ;; 1. Size check BEFORE parsing (prevents allocation attacks)
    (when (> (length raw-string) *max-player-blob-size*)
      (store-corrupt-blob player-id raw-string
                          (list (format nil "Blob size ~d exceeds max ~d bytes"
                                        (length raw-string) *max-player-blob-size*)))
      (increment-validation-metric :reject)
      (return-from db-load-player-validated (values nil nil :reject)))

    ;; 2. Parse with error handling
    (let ((raw-data
            (handler-case
                (let ((*read-eval* nil))
                  (multiple-value-bind (parsed-plist end-pos)
                      (read-from-string raw-string)
                    (declare (ignore end-pos))
                    parsed-plist))
              (error (e)
                (store-corrupt-blob player-id raw-string
                                    (list (format nil "Parse error: ~a" e)))
                (increment-validation-metric :reject)
                (return-from db-load-player-validated (values nil nil :reject))))))

      ;; 3. Basic sanity (pre-migration) - can we migrate at all?
      ;; - Must be a list
      ;; - Must have :id (integer)
      ;; - :version missing is OK (treated as 0 by migration)
      ;; - :version present must be integer
      (let ((version-val (getf raw-data :version)))
        (unless (and (listp raw-data)
                     (integerp (getf raw-data :id))
                     (or (null version-val) (integerp version-val)))
          (store-corrupt-blob player-id raw-string
                              '("Malformed structure: not a valid player data"))
          (increment-validation-metric :reject)
          (return-from db-load-player-validated (values nil nil :reject))))

      ;; 4. Migrate to current schema (adds missing fields)
      (let ((migrated-data (migrate-player-data raw-data)))

        ;; 5. Full 4-way validation (against current schema, after migration)
        (multiple-value-bind (action issues fixed-plist)
            (validate-player-plist-4way migrated-data)
          (increment-validation-metric action)

          (case action
            (:ok
             (let ((player (deserialize-player migrated-data *inventory-size*
                                               (length *equipment-slot-ids*)))
                   (zone-id (getf migrated-data :zone-id)))
               (log-verbose "Loaded and validated player ~a from storage (version ~a)"
                            player-id (getf migrated-data :version))
               (values player zone-id :ok)))

            (:clamp
             (log-verbose "Player ~a clamped: ~{~a~^, ~}" player-id issues)
             ;; Tier-1 save of corrected data using ownership-safe path
             (let ((player (deserialize-player fixed-plist *inventory-size*
                                               (length *equipment-slot-ids*))))
               (db-save-player-immediate player)
               (values player (getf fixed-plist :zone-id) :clamp)))

            (:quarantine
             (store-corrupt-blob player-id raw-string issues)
             (warn "Player ~a QUARANTINED: ~{~a~^, ~}" player-id issues)
             (values (make-quarantined-player player-id) :quarantine :quarantine))

            (:reject
             (store-corrupt-blob player-id raw-string issues)
             (warn "Player ~a REJECTED: ~{~a~^, ~}" player-id issues)
             (values nil nil :reject))))))))

(defun make-quarantined-player (player-id)
  "Create minimal player for quarantine state.
   The player cannot play but their account is recoverable.
   They are placed in the :quarantine zone (special handling)."
  (let ((player (make-player 0.0 0.0)))
    (setf (player-id player) player-id)
    (setf (player-zone-id player) :quarantine)
    player))

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

;;;; ========================================================================
;;;; LEADERBOARD SYSTEM (Phase 4 - Structured Data)
;;;; See docs/db.md "Structured Redis Data" section
;;;; ========================================================================

(defun db-update-leaderboard-xp (player-id xp)
  "Update player's XP on the XP leaderboard."
  (when *storage*
    (storage-zadd *storage* (leaderboard-key :xp) xp (prin1-to-string player-id))))

(defun db-update-leaderboard-level (player-id level)
  "Update player's level on the level leaderboard."
  (when *storage*
    (storage-zadd *storage* (leaderboard-key :level) level (prin1-to-string player-id))))

(defun db-update-leaderboard-deaths (player-id deaths)
  "Update player's death count on the deaths leaderboard.
   Phase 5: Uses zadd with total count (not zincrby) for correctness after restart."
  (when *storage*
    (storage-zadd *storage* (leaderboard-key :deaths) deaths (prin1-to-string player-id))))

(defun db-get-leaderboard (category &key (top 10) (withscores t))
  "Get top N entries from leaderboard CATEGORY (:xp, :level, :deaths).
   Returns list of (player-id score) pairs if WITHSCORES, else list of player-ids."
  (when *storage*
    (let* ((key (leaderboard-key category))
           (result (storage-zrevrange *storage* key 0 (1- top) :withscores withscores)))
      (if withscores
          ;; Convert string IDs back to numbers
          (mapcar (lambda (entry)
                    (list (parse-integer (first entry) :junk-allowed t)
                          (second entry)))
                  result)
          (mapcar (lambda (id-str)
                    (parse-integer id-str :junk-allowed t))
                  result)))))

(defun db-get-player-rank (player-id category)
  "Get player's rank on CATEGORY leaderboard (0-based descending).
   Returns NIL if player not on leaderboard."
  (when *storage*
    (storage-zrevrank *storage* (leaderboard-key category) (prin1-to-string player-id))))

(defun db-get-player-leaderboard-score (player-id category)
  "Get player's score on CATEGORY leaderboard.
   Returns NIL if player not on leaderboard."
  (when *storage*
    (storage-zscore *storage* (leaderboard-key category) (prin1-to-string player-id))))

;;; Online Player Tracking

(defun db-add-online-player (player-id)
  "Add player to online players set."
  (when *storage*
    (storage-sadd *storage* (online-players-key) (prin1-to-string player-id))))

(defun db-remove-online-player (player-id)
  "Remove player from online players set."
  (when *storage*
    (storage-srem *storage* (online-players-key) (prin1-to-string player-id))))

(defun db-get-online-count ()
  "Get count of online players."
  (if *storage*
      (storage-scard *storage* (online-players-key))
      0))

(defun db-get-online-players ()
  "Get list of online player IDs."
  (when *storage*
    (mapcar (lambda (id-str)
              (parse-integer id-str :junk-allowed t))
            (storage-smembers *storage* (online-players-key)))))

;;;; Dirty Flag System (for Tier-2 Batched Writes)

(defstruct player-session
  "Tracks persistence state for a connected player."
  (player nil :type (or null player))
  (zone-id nil :type (or null symbol))
  (username nil :type (or null string))  ; For reverse lookup on ownership loss
  (dirty-p nil :type boolean)
  (last-flush 0.0 :type float)
  (tier1-pending nil :type list))

(defparameter *player-sessions* (make-hash-table :test 'eql :size 512)
  "Map of player-id -> player-session for connected players.")

#+sbcl
(defvar *player-sessions-lock* (sb-thread:make-mutex :name "player-sessions-lock")
  "Mutex protecting *player-sessions* for thread-safe access.")

(defmacro with-player-sessions-lock (&body body)
  "Execute BODY with *player-sessions-lock* held for thread-safe session operations."
  #+sbcl
  `(sb-thread:with-mutex (*player-sessions-lock*)
     ,@body)
  #-sbcl
  `(progn ,@body))

(defun clear-all-player-sessions ()
  "Clear all player sessions. Called during server shutdown/startup for clean state."
  (with-player-sessions-lock
    (clrhash *player-sessions*)))

(defparameter *batch-flush-interval* 30.0
  "Seconds between batch flushes for tier-2 writes.
   TRADEOFF: Server crash loses up to 30s of routine state (XP, position, HP).
   Critical state (death, level-up, equipment, zone transitions) uses Tier-1
   immediate saves and is not affected. 30s chosen to balance durability vs
   database load. Reduce for more durability, increase for less DB pressure.")

(defun mark-player-dirty (player-id)
  "Mark player as needing a database flush. Thread-safe."
  (with-player-sessions-lock
    (let ((session (gethash player-id *player-sessions*)))
      (when session
        (setf (player-session-dirty-p session) t)
        (log-verbose "Marked player ~a as dirty" player-id)))))

;;; Session Ownership Functions (Phase 3 - Database Hardening)

(defun claim-session-ownership (player-id)
  "Attempt to claim ownership of a player session.
   Returns T if claimed successfully, NIL if session is owned elsewhere (double-login).
   On storage error, returns NIL (safe default - treats as claim failure)."
  (unless *storage*
    (return-from claim-session-ownership t))  ; No storage = always succeed
  ;; Ensure server instance ID exists
  (unless *server-instance-id*
    (setf *server-instance-id* (generate-server-instance-id)))
  ;; Catch storage-error to prevent crashing server on transient Redis failures
  (handler-case
      (let ((owner-key (session-owner-key player-id)))
        (let ((claimed (storage-setnx-with-ttl *storage* owner-key *server-instance-id*
                                               *session-ownership-ttl-seconds*)))
          (if claimed
              (progn
                (log-verbose "Claimed session ownership for player ~a" player-id)
                t)
              ;; Check if we already own it (reconnect case)
              (let ((current-owner (storage-load-raw *storage* owner-key)))
                (if (and current-owner (string= current-owner *server-instance-id*))
                    (progn
                      ;; We already own it, refresh TTL
                      (storage-refresh-ttl *storage* owner-key *session-ownership-ttl-seconds*)
                      (log-verbose "Refreshed existing session ownership for player ~a" player-id)
                      t)
                    (progn
                      (log-verbose "Session ownership claim REJECTED for player ~a (owned by ~a)"
                                   player-id current-owner)
                      nil))))))
    (storage-error (e)
      (warn "Session ownership claim failed for player ~a (storage error): ~a" player-id e)
      nil)))

(defun release-session-ownership (player-id)
  "Release session ownership for a player. Called on logout.
   On storage error, logs warning but continues (cleanup still proceeds locally)."
  (when *storage*
    ;; Catch storage-error to prevent crashing server on transient Redis failures
    (handler-case
        (let ((owner-key (session-owner-key player-id)))
          ;; Only delete if we own it
          (let ((current-owner (storage-load-raw *storage* owner-key)))
            (when (and current-owner *server-instance-id*
                       (string= current-owner *server-instance-id*))
              (storage-delete *storage* owner-key)
              (log-verbose "Released session ownership for player ~a" player-id))))
      (storage-error (e)
        (warn "Release session ownership failed for player ~a (storage error): ~a - local cleanup continues"
              player-id e)))))

(defun verify-session-ownership (player-id &key (signal-on-error nil))
  "Verify we still own the session. Returns T if we own it, NIL otherwise.
   When SIGNAL-ON-ERROR is true, re-signals storage errors so callers can retry."
  (unless *storage*
    (return-from verify-session-ownership t))  ; No storage = always succeed
  (unless *server-instance-id*
    (return-from verify-session-ownership nil))  ; No server ID = don't own anything
  ;; Catch storage-error to keep server loop alive on transient Redis failures
  (handler-case
      (let* ((owner-key (session-owner-key player-id))
             (current-owner (storage-load-raw *storage* owner-key)))
        (and current-owner (string= current-owner *server-instance-id*)))
    (storage-error (e)
      (log-verbose "Ownership check failed for player ~a (transient): ~a" player-id e)
      (if signal-on-error
          (error e)
          nil))))

(defun refresh-session-ownership (player-id)
  "Refresh TTL on session ownership. Called periodically as heartbeat."
  (when *storage*
    (let ((owner-key (session-owner-key player-id)))
      (storage-refresh-ttl *storage* owner-key *session-ownership-ttl-seconds*))))

(defparameter *ownership-refresh-interval* 30.0
  "Seconds between session ownership refreshes (half of TTL).
   Refreshing at half the TTL ensures ownership never expires during normal operation.")

(defun refresh-all-session-ownerships ()
  "Refresh TTL on all active session ownerships. Called periodically.
   Returns list of player-ids that failed to refresh (ownership truly lost).

   Phase 2 improvement: On verification failure, attempts to re-claim ownership
   before reporting as lost. This handles transient Redis hiccups where the key
   expired but no other server took ownership.

   Catches storage-error to keep server loop alive on transient Redis failures."
  (let ((failed-ids nil))
    (with-player-sessions-lock
      (maphash (lambda (player-id session)
                 (declare (ignore session))
                 ;; Verify we still own before refreshing
                 ;; verify-session-ownership already catches storage-error
                 (if (verify-session-ownership player-id)
                     ;; Catch errors during refresh to avoid unwinding loop
                     (handler-case
                         (refresh-session-ownership player-id)
                       (error (e)
                         (log-verbose "Refresh failed for player ~a (transient): ~a" player-id e)
                         ;; Don't mark as failed - will retry next cycle
                         nil))
                     ;; Ownership verification failed - try to re-claim before giving up
                     ;; This handles the case where Redis key expired but no other
                     ;; server has claimed ownership yet (transient outage)
                     (if (claim-session-ownership player-id)
                         (progn
                           (log-verbose "Re-claimed ownership for player ~a after verification failure" player-id)
                           ;; Successfully re-claimed, refresh TTL
                           (handler-case
                               (refresh-session-ownership player-id)
                             (error (e)
                               (log-verbose "Refresh after re-claim failed for ~a: ~a" player-id e))))
                         ;; Re-claim failed - another server owns it now, truly lost
                         (progn
                           (log-verbose "Ownership truly lost for player ~a (another server owns)" player-id)
                           (push player-id failed-ids)))))
               *player-sessions*))
    failed-ids))

(defun register-player-session (player &key (zone-id nil) (username nil))
  "Register a player session when they login. Thread-safe.
   USERNAME is stored for reverse lookup on ownership loss cleanup.
   Returns T on success, NIL if session is owned elsewhere (double-login rejected)."
  (let ((player-id (player-id player)))
    ;; Attempt to claim ownership first
    (unless (claim-session-ownership player-id)
      (warn "Double-login rejected for player ~a - session owned elsewhere" player-id)
      (return-from register-player-session nil))
    ;; Ownership claimed, register local session
    (with-player-sessions-lock
      (setf (gethash player-id *player-sessions*)
            (make-player-session :player player
                                 :zone-id zone-id
                                 :username username
                                 :dirty-p nil
                                 :last-flush (float (get-internal-real-time) 1.0)
                                 :tier1-pending nil)))
    ;; Add to online players set (Phase 4)
    (db-add-online-player player-id)
    ;; Update leaderboards with current player stats (Phase 4)
    (db-update-leaderboard-xp player-id (player-lifetime-xp player))
    (let ((level (combat-level (player-stats player))))
      (db-update-leaderboard-level player-id level))
    ;; Phase 5: Seed deaths leaderboard on login with existing death count
    (db-update-leaderboard-deaths player-id (player-deaths player))
    (log-verbose "Registered session for player ~a in zone ~a" player-id zone-id)
    t))

(defun register-player-session-local (player &key (zone-id nil) (username nil))
  "Register local session state only (no DB calls). For main-thread use.
   Step 5: Split of register-player-session - in-memory part only."
  (let ((player-id (player-id player)))
    (with-player-sessions-lock
      (setf (gethash player-id *player-sessions*)
            (make-player-session :player player
                                 :zone-id zone-id
                                 :username username
                                 :dirty-p nil
                                 :last-flush (float (get-internal-real-time) 1.0)
                                 :tier1-pending nil)))
    (log-verbose "Registered local session for player ~a in zone ~a" player-id zone-id)
    t))

(defun register-player-session-db (player-id player)
  "Register session in DB: ownership, online set, leaderboards. For worker-thread use.
   Step 5: Split of register-player-session - DB part only.
   Returns T on success, NIL if session ownership rejected."
  (unless (claim-session-ownership player-id)
    (warn "Double-login rejected for player ~a - session owned elsewhere" player-id)
    (return-from register-player-session-db nil))
  (db-add-online-player player-id)
  (db-update-leaderboard-xp player-id (player-lifetime-xp player))
  (let ((level (combat-level (player-stats player))))
    (db-update-leaderboard-level player-id level))
  (db-update-leaderboard-deaths player-id (player-deaths player))
  (log-verbose "Registered DB session for player ~a" player-id)
  t)

(defun update-player-session-zone (player-id zone-id)
  "Update the zone-id for a player session (called on zone transitions). Thread-safe."
  (with-player-sessions-lock
    (let ((session (gethash player-id *player-sessions*)))
      (when session
        (setf (player-session-zone-id session) zone-id)
        (log-verbose "Updated session zone for player ~a to ~a" player-id zone-id)))))

(defun unregister-player-session (player-id)
  "Unregister a player session when they logout. Thread-safe.
   Releases session ownership before removing local session."
  ;; Remove from online players set (Phase 4)
  (db-remove-online-player player-id)
  ;; Release ownership first (before final save)
  (release-session-ownership player-id)
  ;; Remove local session
  (with-player-sessions-lock
    (remhash player-id *player-sessions*))
  (log-verbose "Unregistered session for player ~a" player-id))

(defun unregister-player-session-local (player-id)
  "Unregister a player session locally only (no online-set or ownership changes).
   Used when ownership is already lost to another server - we should not touch
   the global online set since the new owner controls that now.
   Thread-safe."
  (with-player-sessions-lock
    (remhash player-id *player-sessions*))
  (log-verbose "Unregistered local session for player ~a (ownership lost)" player-id))

(defun flush-dirty-players (&key force)
  "Flush all dirty players to storage (tier-2 batched writes). Thread-safe.
   Uses pipelined batch save for efficiency (up to 300x faster than sequential).
   If FORCE is T, flush all players regardless of dirty flag.
   Verifies session ownership before saving - players with lost ownership are
   NOT saved (to prevent stale server overwrites) and their sessions are cleaned up."
  (let ((to-flush nil)
        (key-data-pairs nil)
        (lost-player-ids nil)
        (current-time (get-internal-real-time)))
    ;; Collect players that need flushing and build save data (under lock)
    (with-player-sessions-lock
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
             ;; Check ownership BEFORE adding to flush list
             (if (verify-session-ownership player-id)
                 (progn
                   (push (cons player-id session) to-flush)
                   ;; Build save data while under lock to get consistent snapshot
                   (let* ((zone-id (player-session-zone-id session))
                          (key (player-key player-id))
                          (data (serialize-player player :include-visuals nil :zone-id zone-id)))
                     (setf data (plist-put data :version *player-schema-version*))
                     (push (cons key data) key-data-pairs)))
                 ;; Lost ownership - track for cleanup (don't save - would overwrite)
                 (push player-id lost-player-ids)))))
       *player-sessions*))
    ;; Handle lost ownership - DO NOT cleanup here, just skip saves
    ;; Phase 2 fix: Let the ownership refresh loop (refresh-all-session-ownerships)
    ;; handle cleanup so it can attempt re-claim first and do full net-client de-auth.
    ;; If we cleanup here, the session is removed before the refresh loop sees it,
    ;; leaving stale authenticated clients in the server.
    (when lost-player-ids
      (warn "Batch flush: lost ownership for ~d players, skipping save (cleanup deferred to refresh loop): ~a"
            (length lost-player-ids) lost-player-ids))
    ;; Execute batch save outside lock (IO can be slow)
    (when to-flush
      (unless *storage*
        (log-verbose "flush-dirty-players: no storage backend initialized, skipping")
        (return-from flush-dirty-players 0))
      ;; Phase 1: Only clear dirty flags on successful save
      ;; If batch save fails, flags remain set for next flush cycle to retry
      (handler-case
          (let ((saved-count (storage-save-batch *storage* key-data-pairs)))
            ;; Success: Update session state for all flushed players (under lock)
            (with-player-sessions-lock
              (dolist (entry to-flush)
                (let ((session (cdr entry)))
                  (setf (player-session-dirty-p session) nil)
                  (setf (player-session-last-flush session) (float current-time 1.0)))))
            ;; Update stats counter
            (when (boundp '*server-total-saves*)
              (incf *server-total-saves* saved-count))
            (when (> saved-count 0)
              (log-verbose "Batch flushed ~a player(s) to storage (pipelined)" saved-count))
            saved-count)
        (storage-error (e)
          ;; Failure: Keep dirty flags set so next flush cycle retries
          (warn "Batch flush failed, dirty flags preserved for retry: ~a" e)
          0)))
    (length to-flush)))

;;;; Tier-1 Immediate Write Operations

(defun db-save-player-immediate (player)
  "Tier-1 immediate write: save player and wait for confirmation.
   Use for critical operations: trade, bank, death, level-up, item destruction.
   Verifies session ownership before saving to prevent stale server writes.
   Returns T on success. Signals STORAGE-ERROR on failure (Phase 1: enables retry)."
  (let ((player-id (player-id player)))
    ;; Verify we still own this session (prevents stale server writes).
    ;; Signal storage errors so retry logic can handle transient failures.
    (unless (verify-session-ownership player-id :signal-on-error t)
      (warn "REJECTED: Tier-1 save for player ~a - session not owned by this server" player-id)
      (return-from db-save-player-immediate nil))
    ;; Ownership verified, proceed with save
    ;; db-save-player signals storage-error on failure (Phase 1)
    (db-save-player player)
    ;; Only update session state if save succeeded (we get here only on success)
    ;; For Redis with AOF, write is durable after return
    ;; Mark as not dirty since we just saved
    (let ((session (gethash player-id *player-sessions*)))
      (when session
        (setf (player-session-dirty-p session) nil)
        (setf (player-session-last-flush session) (float (get-internal-real-time) 1.0))))
    (log-verbose "Tier-1 immediate save for player ~a" player-id)
    t))

;;;; Login/Logout Operations

(defun db-login-player (player-id)
  "Load player from storage on login. Returns player or NIL."
  (let ((player (db-load-player player-id)))
    (when player
      (register-player-session player)
      (log-verbose "Player ~a logged in from storage" player-id))
    player))

(defun db-logout-player (player)
  "Save player to storage on logout (tier-3 write).
   Ensures session cleanup happens even if save fails (transient Redis errors)."
  (when player
    (let ((player-id (player-id player)))
      ;; Try to save, but don't let save failure block cleanup
      ;; Cleanup must happen to prevent stale sessions
      (handler-case
          (db-save-player-immediate player)
        (storage-error (e)
          (warn "Logout save failed for player ~a (will lose unsaved progress): ~a"
                player-id e))
        (error (e)
          (warn "Logout save error for player ~a: ~a" player-id e)))
      ;; Always unregister session, even if save failed
      (unregister-player-session player-id)
      (log-verbose "Player ~a logged out" player-id))))
