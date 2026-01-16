.PHONY: ci smoke start server client checkparens checkdocs
SMOKE_TIMEOUT ?= 5s
MMORPG_SMOKE_SECONDS ?= 2.0
ci:
	sbcl --script scripts/ci.lisp

smoke:
	MMORPG_SMOKE_SECONDS=$(MMORPG_SMOKE_SECONDS) timeout $(SMOKE_TIMEOUT) sbcl --script scripts/smoke.lisp

start:
	sbcl --script scripts/start.lisp

server:
	sbcl --script scripts/server.lisp

client:
	sbcl --script scripts/client.lisp

checkparens:
	emacs --batch data/*.lisp src/*.lisp --eval '(progn (dolist (b (buffer-list)) (with-current-buffer b (when (buffer-file-name) (message "CHECK-PARENS: %s" (buffer-file-name)) (check-parens) (message "OK: %s" (buffer-file-name))))) (message "OK: all files balanced"))'

checkdocs:
	bash scripts/checkdocs.sh
