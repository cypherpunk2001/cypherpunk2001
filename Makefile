.PHONY: ci smoke server client checkparens checkdocs test-persistence
SMOKE_TIMEOUT ?= 5s
MMORPG_SMOKE_SECONDS ?= 2.0

# CI and smoke use memory backend (no Redis required)
ci:
	MMORPG_DB_BACKEND=memory sbcl --script scripts/ci.lisp

test-persistence:
	sbcl --script scripts/test-persistence.lisp

smoke:
	MMORPG_DB_BACKEND=memory MMORPG_SMOKE_SECONDS=$(MMORPG_SMOKE_SECONDS) timeout $(SMOKE_TIMEOUT) sbcl --script scripts/smoke.lisp

# Server and client use Redis by default (dev close to production)
server:
	sbcl --script scripts/server.lisp

client:
	sbcl --script scripts/client.lisp

checkparens:
	emacs --batch data/*.lisp src/*.lisp --eval '(progn (dolist (b (buffer-list)) (with-current-buffer b (when (buffer-file-name) (message "CHECK-PARENS: %s" (buffer-file-name)) (check-parens) (message "OK: %s" (buffer-file-name))))) (message "OK: all files balanced"))'

checkdocs:
	bash scripts/checkdocs.sh
