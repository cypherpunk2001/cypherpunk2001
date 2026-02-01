.PHONY: ci smoke server client local checkparens checkdocs test-unit stress tests profile server-prod notify
SMOKE_TIMEOUT ?= 5s
MMORPG_SMOKE_SECONDS ?= 2.0
STRESS_CLIENTS ?= 10
STRESS_DURATION ?= 60
# Build environment: dev (default) or prod
MMORPG_ENV ?= dev

tests: checkparens ci smoke test-unit checkdocs
	@echo "All tests passed!"

ci:
	MMORPG_DB_BACKEND=memory sbcl --script scripts/ci.lisp

test-unit:
	MMORPG_DB_BACKEND=memory sbcl --script scripts/test-unit.lisp

smoke:
	MMORPG_DB_BACKEND=memory MMORPG_SMOKE_SECONDS=$(MMORPG_SMOKE_SECONDS) timeout $(SMOKE_TIMEOUT) sbcl --script scripts/smoke.lisp

server:
	MMORPG_ENV=$(MMORPG_ENV) sbcl --script scripts/server.lisp

server-prod:
	MMORPG_ENV=prod sbcl --script scripts/server.lisp

client:
	MMORPG_ENV=$(MMORPG_ENV) sbcl --script scripts/client.lisp

local:
	MMORPG_ENV=$(MMORPG_ENV) sbcl --script scripts/local.lisp

checkparens:
	./scripts/checkparens.sh

checkdocs:
	./scripts/checkdocs.sh

stress:
	sbcl --script scripts/stress-test.lisp $(STRESS_CLIENTS) $(STRESS_DURATION)

profile:
	MMORPG_PROFILE=1 MMORPG_VERBOSE_GC=1 sbcl --script scripts/server.lisp

notify:
	@notify-send -a "MMORPG" "$(MSG)"
