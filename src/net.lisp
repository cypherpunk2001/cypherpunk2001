;; NOTE: If you change behavior here, update docs/net.md :)
(in-package #:mmorpg)

;;; net.lisp - Glue: re-exports and shared helpers for net-* modules
;;;
;;; Split files (loaded before this, in order):
;;; - net-protocol.lisp: message encode/decode, binary snapshots, client management
;;; - net-auth.lisp: login/register, auth queues, rate limiting, session management
;;; - net-snapshot.lisp: snapshot send/receive, delta compression, interpolation, prediction
;;; - net-server.lisp: server UDP loop, dispatch, connection tracking
;;; - net-client.lisp: client networking loop, reconnect, client handlers
;;;
;;; This file is loaded LAST among net-* files. Currently it contains no
;;; additional code — all networking logic lives in the split modules above.
