;; NOTE: If you change behavior here, update docs/migrations.md :)
(in-package #:mmorpg)

;;;; Schema Version and Migration Registry
;;;;
;;;; Migrations run lazily on player load (default) or eagerly via admin command.
;;;; Each migration function takes a plist and returns the updated plist.

(defparameter *player-schema-version* 5
  "Current player schema version. Increment when changing player format.")

(defun migrate-player-v1->v2 (data)
  "v1->v2: Add lifetime-xp field, defaulting to 0."
  (unless (getf data :lifetime-xp)
    (setf data (plist-put data :lifetime-xp 0)))
  data)

(defun migrate-player-v2->v3 (data)
  "v2->v3: Add playtime and created-at fields.
   playtime defaults to 0.0 (seconds played, single-float).
   created-at defaults to current time for existing players."
  (unless (getf data :playtime)
    (setf data (plist-put data :playtime 0.0)))
  (unless (getf data :created-at)
    (setf data (plist-put data :created-at (get-universal-time))))
  data)

(defun migrate-player-v3->v4 (data)
  "v3->v4: Add deaths field for leaderboard tracking.
   deaths defaults to 0 (Phase 4 - Database Hardening)."
  (unless (getf data :deaths)
    (setf data (plist-put data :deaths 0)))
  data)

(defun migrate-player-v4->v5 (data)
  "v4->v5: Rename fantasy item IDs to cyberpunk equivalents in inventory.
   :rat-tail -> :drone-scrap, :goblin-ear -> :punk-tag."
  (let ((inv (getf data :inventory)))
    (when inv
      (let ((slots (getf inv :slots)))
        (when slots
          (dolist (slot slots)
            (let ((item-id (getf slot :item-id)))
              (case item-id
                (:rat-tail (setf (getf slot :item-id) :drone-scrap))
                (:goblin-ear (setf (getf slot :item-id) :punk-tag)))))
          (setf (getf inv :slots) slots)
          (setf data (plist-put data :inventory inv))))))
  data)

(defparameter *player-migrations*
  '((2 . migrate-player-v1->v2)
    (3 . migrate-player-v2->v3)
    (4 . migrate-player-v3->v4)
    (5 . migrate-player-v4->v5))
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
    (setf data (plist-put data :version *player-schema-version*))
    data))

;;;; Exports
;;;;
;;;; Note: migrate-all-players is defined in db.lisp (needs storage functions)

(export '(*player-schema-version*
          *player-migrations*
          migrate-player-data))
