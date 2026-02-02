(in-package :mmorpg)

;;; db.lisp - Glue: storage abstraction layer and Redis persistence
;;;
;;; This file is loaded LAST among db-* files.
;;; All functionality has been split into:
;;; - db-storage.lisp: storage abstraction, backends (Redis/memory), metrics
;;; - db-players.lisp: player save/load, dirty flags, sessions, flush
;;; - db-accounts.lisp: account creation/verification, password hashing, encryption
;;; - db-admin.lisp: admin tooling (migrate-all-players, db-shutdown-flush)
