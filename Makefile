.PHONY: ci smoke server client local checkparens checkdocs test-unit test-persistence test-security test-trade stress tests
SMOKE_TIMEOUT ?= 5s
MMORPG_SMOKE_SECONDS ?= 2.0
STRESS_CLIENTS ?= 10
STRESS_DURATION ?= 60

tests: checkparens ci test-unit test-persistence test-security test-trade checkdocs smoke
	@echo "All tests passed!"

ci:
	MMORPG_DB_BACKEND=memory sbcl --script scripts/ci.lisp

test-persistence:
	sbcl --script scripts/test-persistence.lisp

test-security:
	MMORPG_DB_BACKEND=memory sbcl --script scripts/test-security.lisp

test-unit:
	MMORPG_DB_BACKEND=memory sbcl --script scripts/test-unit.lisp

test-trade:
	MMORPG_DB_BACKEND=memory sbcl --script scripts/test-trade.lisp

smoke:
	MMORPG_DB_BACKEND=memory MMORPG_SMOKE_SECONDS=$(MMORPG_SMOKE_SECONDS) timeout $(SMOKE_TIMEOUT) sbcl --script scripts/smoke.lisp

server:
	sbcl --script scripts/server.lisp

client:
	sbcl --script scripts/client.lisp

local:
	sbcl --script scripts/local.lisp

checkparens:
	./scripts/checkparens.sh

checkdocs:
	./scripts/checkdocs.sh

stress:
	sbcl --script scripts/stress-test.lisp $(STRESS_CLIENTS) $(STRESS_DURATION)
