;; NOTE: If you change behavior here, update docs/package.md :)
(defpackage #:mmorpg
  (:use #:cl)
  (:export #:run-headless
           #:run-server
           #:run-client))

(in-package #:mmorpg)
