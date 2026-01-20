# TODO

- [ ] Add a signal-on-error option to `verify-session-ownership` and use it from `db-save-player-immediate` so tier-1 saves retry on transient storage errors.
- [ ] Harden 4-way validation to reject malformed inventory slots and malformed equipment structures (`:equipment` / `:items` not lists).
- [ ] Add persistence tests for the new ownership-error retry behavior and the new reject paths, and wire them into the 4-way test runner.
- [ ] Run full test suite: `make checkparens && make ci && make test-persistence && make test-security && make checkdocs && make smoke`.
---

confirm these are now fixed
P1
Retry tier-1 saves when ownership checks error
￼Fix
verify-session-ownership returns NIL on storage errors, so db-save-player-immediate treats transient Redis failures as lost ownership and returns NIL without signaling. Callers (death/level-up/consume paths) wrap db-save-player-immediate in with-retry-exponential, which only retries on errors, so these tier‑1 saves can be silently skipped during transient storage hiccups even though ownership is still valid. Consider distinguishing storage errors from true ownership loss (e.g., signal storage-error or retry the ownership check) so tier‑1 saves either retry or fall back to dirty saves.

￼
/home/telecommuter/repos/mmorpg/src/db.lisp:1622-1636
￼Hide low priority findings
P2
Guard non-plist inventory slots during 4-way validation
￼Fix
validate-player-plist-4way assumes each inventory slot is a plist and calls getf directly. If corrupted data contains a non-list slot (e.g., integer/string), getf will signal a type error, so validation fails via an exception instead of returning :reject/:quarantine, and the login path treats it as a load failure. Add a per-slot listp guard and mark malformed slots as :reject (with a test) to keep the 4-way flow robust.

￼
/home/telecommuter/repos/mmorpg/src/save.lisp:289-294
P2
Reject malformed equipment structures in 4-way validation
￼Fix
validate-player-plist-4way only inspects equipment when :equipment and :items are lists; malformed equipment (string/number or :items non-list) is currently ignored and can pass validation. That leads to deserialize-equipment receiving non-plist data during login, which will error out. Add explicit type checks for :equipment and its :items field (mirroring inventory) and return :reject when malformed.

￼
/home/telecommuter/repos/mmorpg/src/save.lisp:371-375
