.PHONY: ci smoke checkparens
SMOKE_TIMEOUT ?= 5s
MMORPG_SMOKE_SECONDS ?= 2.0
ci:
	sbcl --script scripts/ci.lisp

smoke:
	MMORPG_SMOKE_SECONDS=$(MMORPG_SMOKE_SECONDS) timeout $(SMOKE_TIMEOUT) sbcl --script scripts/smoke.lisp

checkparens:
	emacs --batch data/*.lisp src/*.lisp --eval '(progn (dolist (b (buffer-list)) (with-current-buffer b (when (buffer-file-name) (message "CHECK-PARENS: %s" (buffer-file-name)) (check-parens) (message "OK: %s" (buffer-file-name))))) (message "OK: all files balanced"))'

checkdocs:
	bash -c 'for f in src/*.lisp; do b=$(basename "$$f" .lisp); [ -f "docs/$$b.md" ] || { echo "ERROR: missing docs/$$b.md for $$f"; exit 1; }; done; echo "PASSED: OK - Reminder, update the .md doc if you touch the .lisp in some significant way! Thank you! :)"'
