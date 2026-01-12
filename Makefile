.PHONY: ci smoke
SMOKE_TIMEOUT ?= 5s
MMORPG_SMOKE_SECONDS ?= 2.0
ci:
	sbcl --script scripts/ci.lisp

smoke:
	MMORPG_SMOKE_SECONDS=$(MMORPG_SMOKE_SECONDS) timeout $(SMOKE_TIMEOUT) sbcl --script scripts/smoke.lisp
