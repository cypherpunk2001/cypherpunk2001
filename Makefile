.PHONY: ci smoke server client checkparens checkdocs test-persistence
SMOKE_TIMEOUT ?= 5s
MMORPG_SMOKE_SECONDS ?= 2.0

ci:
	MMORPG_DB_BACKEND=memory sbcl --script scripts/ci.lisp

test-persistence:
	sbcl --script scripts/test-persistence.lisp

smoke:
	MMORPG_DB_BACKEND=memory MMORPG_SMOKE_SECONDS=$(MMORPG_SMOKE_SECONDS) timeout $(SMOKE_TIMEOUT) sbcl --script scripts/smoke.lisp

server:
	sbcl --script scripts/server.lisp

client:
	sbcl --script scripts/client.lisp

checkparens:
	./scripts/checkparens.sh

checkdocs:
	./scripts/checkdocs.sh
