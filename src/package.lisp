;; NOTE: If you change behavior here, update docs/package.md :)
(defpackage #:mmorpg
  (:use #:cl)
  (:export #:run
           #:run-headless))

(in-package #:mmorpg)
