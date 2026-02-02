(in-package #:mmorpg)

;;; ============================================================
;;; CHAT.LISP TESTS
;;; ============================================================

(defun test-trim-chat-message ()
  "Test chat message trimming."
  (assert (string= (trim-chat-message "hello") "hello") () "trim: no whitespace")
  (assert (string= (trim-chat-message "  hello  ") "hello") () "trim: spaces")
  (assert (string= (trim-chat-message "	hello	") "hello") () "trim: tabs")
  (assert (string= (trim-chat-message "
hello
") "hello") () "trim: newlines")
  (assert (string= (trim-chat-message "") "") () "trim: empty string")
  (assert (string= (trim-chat-message "   ") "") () "trim: only whitespace"))

;;; ============================================================


(defvar *tests-chat*
  '(test-trim-chat-message)
  "Chat domain test functions.")
