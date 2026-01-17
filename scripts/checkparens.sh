#!/bin/bash
# Check balanced parentheses in all Lisp files
set -euo pipefail
cd "$(dirname "$0")/.."

emacs --batch data/*.lisp src/*.lisp --eval \
  '(progn
     (dolist (b (buffer-list))
       (with-current-buffer b
         (when (buffer-file-name)
           (message "CHECK-PARENS: %s" (buffer-file-name))
           (check-parens)
           (message "OK: %s" (buffer-file-name)))))
     (message "OK: all files balanced"))'
