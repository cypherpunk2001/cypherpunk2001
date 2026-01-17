;; NOTE: If you change behavior here, update docs/package.md :)
(defpackage #:mmorpg
  (:use #:cl)
  (:export #:run-headless
           #:run-server
           #:run-client
           #:run-local
           ;; Admin commands
           #:admin-print-save
           #:admin-list-players
           #:admin-find-player
           #:admin-grant-item
           #:admin-remove-item
           #:admin-clear-inventory
           #:admin-set-hp
           #:admin-set-xp
           #:admin-set-level
           #:admin-set-coins
           #:admin-teleport
           #:admin-wipe-character
           #:admin-kick
           #:admin-reset-position
           #:admin-broadcast
           #:admin-save-all
           #:admin-player-count
           #:admin-server-stats))

(in-package #:mmorpg)
