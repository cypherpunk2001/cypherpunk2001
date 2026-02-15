.PHONY: ci smoke server client local checkparens checkdocs test-unit stress tests profile server-prod
SMOKE_TIMEOUT ?= 5s
MMORPG_SMOKE_SECONDS ?= 2.0
STRESS_CLIENTS ?= 10
STRESS_DURATION ?= 60
# Build environment: dev (default) or prod
MMORPG_ENV ?= dev
SBCL_HEAP ?= 4096
SBCL := sbcl --dynamic-space-size $(SBCL_HEAP)

tests: checkparens ci smoke test-unit checkdocs
	@echo "All tests passed!"

ci:
	MMORPG_DB_BACKEND=memory $(SBCL) --script scripts/ci.lisp

test-unit:
	MMORPG_DB_BACKEND=memory $(SBCL) --script scripts/test-unit.lisp

smoke:
	MMORPG_DB_BACKEND=memory MMORPG_SMOKE_SECONDS=$(MMORPG_SMOKE_SECONDS) timeout $(SMOKE_TIMEOUT) $(SBCL) --script scripts/smoke.lisp

server:
	MMORPG_ENV=$(MMORPG_ENV) $(SBCL) --script scripts/server.lisp

server-prod:
	MMORPG_ENV=prod $(SBCL) --script scripts/server.lisp

client:
	MMORPG_ENV=$(MMORPG_ENV) $(SBCL) --script scripts/client.lisp

local:
	MMORPG_ENV=$(MMORPG_ENV) $(SBCL) --script scripts/local.lisp

checkparens:
	./scripts/checkparens.sh

checkdocs:
	./scripts/checkdocs.sh

stress:
	$(SBCL) --script scripts/stress-test.lisp $(STRESS_CLIENTS) $(STRESS_DURATION)

profile:
	MMORPG_PROFILE=1 MMORPG_VERBOSE_GC=1 $(SBCL) --script scripts/server.lisp
