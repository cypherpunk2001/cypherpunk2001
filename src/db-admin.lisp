(in-package :mmorpg)

;;; db-admin.lisp - Admin tooling, migration commands, shutdown
;;;
;;; Administrative operations: migrate-all-players, graceful shutdown flush.
;;; Split from db.lisp.
;;;
;;; Load order: db-storage -> db-players -> db-accounts -> db-admin -> db

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

;;;; Graceful Shutdown

(defun db-shutdown-flush ()
  "Gracefully flush all data during server shutdown.
   1. Flush all dirty players
   2. Clear in-memory session state (for REPL restarts)
   3. Trigger Redis BGSAVE
   4. Close storage connection"
  (log-verbose "Starting graceful shutdown flush")
  (flush-dirty-players :force t)
  ;; Clear in-memory session state to prevent stale sessions on REPL restart
  (clear-all-player-sessions)
  (session-clear-all)  ; Clear *active-sessions* (username -> client)
  (when *storage*
    (storage-flush *storage*))
  (shutdown-storage)
  (log-verbose "Graceful shutdown flush completed"))
