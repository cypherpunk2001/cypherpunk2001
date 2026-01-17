;; NOTE: If you change behavior here, update docs/migrations.md :)
(in-package #:mmorpg)

;;;; Schema Version and Migration Registry
;;;;
;;;; Migrations run lazily on player load (default) or eagerly via admin command.
;;;; Each migration function takes a plist and returns the updated plist.

(defparameter *player-schema-version* 2
  "Current player schema version. Increment when changing player format.")

(defun migrate-player-v1->v2 (data)
  "v1->v2: Add lifetime-xp field, defaulting to 0."
  (unless (getf data :lifetime-xp)
    (setf (getf data :lifetime-xp) 0))
  data)

(defparameter *player-migrations*
  '((2 . migrate-player-v1->v2))
  "Alist of (version . migration-function) for player data.
   Each migration takes a plist and returns the updated plist.
   Migrations are chained: v1->v2, v2->v3, etc.")

;;;; Migration Runner

(defun migrate-player-data (data)
  "Migrate player data to current schema version.
   Runs migration chain from data's version to *player-schema-version*.
   Called lazily on each player load."
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

;;;; Exports
;;;;
;;;; Note: migrate-all-players is defined in db.lisp (needs storage functions)

(export '(*player-schema-version*
          *player-migrations*
          migrate-player-data))
