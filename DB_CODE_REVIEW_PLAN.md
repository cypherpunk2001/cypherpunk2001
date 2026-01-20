# DB Code Review Remediation Plan

This document provides a detailed implementation plan to address all findings from `DB_CODE_REVIEW_FINDINGS.md`. Findings are grouped by implementation phase to respect dependencies.

## Implementation Order

Dependencies dictate this order:
1. **Phase A** (Foundation): Fix session ownership refresh (#1) - everything depends on working ownership
2. **Phase B** (Validation Hardening): Fix validator crash (#4) - must be safe before wiring into login
3. **Phase C** (Login Integration): Wire Phase 6 validation into login (#2), handle quarantine (#6)
4. **Phase D** (Batch Safety): Fix tier-2 flush ownership check (#3)
5. **Phase E** (Leaderboards): Wire leaderboard updates to gameplay (#5)
6. **Phase F** (Robustness): NOSCRIPT fallback (#7), trade version tagging (#8)
7. **Phase G** (Polish): Memory TTL semantics (#9), documentation (#10)

---

## Phase A: Session Ownership Refresh (Critical #1)

**Finding:** `refresh-session-ownership`/`refresh-all-session-ownerships` are defined but never called. After 60s TTL expiry, tier-1 saves silently fail.

### A.1 Add Heartbeat Call in Server Loop

**File:** `src/server.lisp`

**Location:** Inside `run-server-loop` or the main tick function, after processing intents.

**Implementation:**
```lisp
;; Add to server tick (every tick, or gated by timer)
(defparameter *ownership-refresh-interval* 30.0
  "Seconds between session ownership refreshes (half of TTL).")

(defparameter *last-ownership-refresh* 0.0
  "Timestamp of last ownership refresh.")

;; In server loop, after processing:
(when (> (- current-time *last-ownership-refresh*) *ownership-refresh-interval*)
  (refresh-all-session-ownerships)
  (setf *last-ownership-refresh* current-time))
```

**Rationale:** Refresh at half the TTL (30s) ensures ownership never expires during normal operation. The 60s TTL provides a safety margin.

### A.2 Handle Lost Ownership

**File:** `src/db.lisp`

**Modify:** `refresh-all-session-ownerships` to return list of player-ids that failed refresh.

**File:** `src/server.lisp` (or `src/net.lisp` if cleanup is network-centric)

**Implementation:**

Note: There is no `disconnect-player` function. Cleanup requires:
1. `unregister-player-session` (db.lisp) - removes from `*player-sessions*`, releases ownership
2. `session-unregister` (net.lisp) - removes from `*active-sessions*` by username
3. Removing player from world/zone

```lisp
;; After refresh, handle lost sessions
(let ((lost-sessions (refresh-all-session-ownerships)))
  (dolist (player-id lost-sessions)
    (warn "Session ownership lost for player ~a - forcing cleanup" player-id)
    (let ((session (gethash player-id *player-sessions*)))
      (when session
        (let ((player (player-session-player session))
              (username (player-session-username session)))  ;; NEW FIELD
          ;; Try to reclaim ownership
          (unless (claim-session-ownership player-id)
            ;; Cannot reclaim - must disconnect without saving
            ;; 1. Remove from active sessions (net.lisp)
            (when username
              (session-unregister username))
            ;; 2. Remove from player sessions (db.lisp) - skip ownership release since lost
            (with-player-sessions-lock
              (remhash player-id *player-sessions*))
            ;; 3. Remove player from game world
            (when player
              (remove-player-from-game *server-game* player))))))))
```

**Prerequisites for Phase A.2:**

1. **Extend `player-session` struct** in `src/db.lisp`:
   ```lisp
   (defstruct player-session
     "Tracks persistence state for a connected player."
     (player nil :type (or null player))
     (zone-id nil :type (or null symbol))
     (username nil :type (or null string))  ;; NEW: for reverse lookup
     (dirty-p nil :type boolean)
     (last-flush 0.0 :type float)
     (tier1-pending nil :type list))
   ```

2. **Update `register-player-session`** to accept and store username:
   ```lisp
   (defun register-player-session (player &key zone-id username)
     ;; ... existing code ...
     (setf (player-session-username session) username)
     ;; ...
   ```

3. **Update all `register-player-session` call sites** in `src/net.lisp` to pass `:username`:
   - `handle-auth-result` (around line 721)
   - `handle-login-sync` (around line 1845)
   - `handle-registration-sync` (around line 1669)

4. **Use `remove-player-from-game`** (exists in `src/net.lisp:823`) with `*server-game*`:
   - `*server-game*` already exists and is set in `run-server` (net.lisp:1965)
   - No new global needed - just use `*server-game*` in the cleanup path

### A.3 Tests

**File:** `tests/persistence-test.lisp`

- `test-ownership-refresh-extends-ttl` - Verify refresh prevents expiry
- `test-ownership-lost-triggers-disconnect` - Verify lost ownership handling
- `test-tier1-save-after-refresh` - Verify tier-1 saves work after refresh

---

## Phase B: Validator Crash Fix (High #4)

**Finding:** `validate-player-plist-4way` calls `getf` on `:inventory`/`:stats` without checking they're lists, causing crashes on malformed data.

### B.1 Add Type Guards

**File:** `src/save.lisp`

**Location:** `validate-player-plist-4way`, before accessing nested structures.

**Implementation:**
```lisp
;; Before inventory validation
(let ((inventory (getf plist :inventory)))
  (unless (listp inventory)
    (push (format nil "Field :inventory has wrong type: expected list, got ~a"
                  (type-of inventory)) issues)
    (setf needs-reject t))
  (when (listp inventory)
    ;; Existing inventory slot validation...
    (let ((slots (getf inventory :slots)))
      (unless (listp slots)
        (push (format nil "Field :inventory :slots has wrong type: expected list, got ~a"
                      (type-of slots)) issues)
        (setf needs-reject t))
      (when (listp slots)
        ;; Existing slot iteration...
        ))))

;; Before stats validation
(let ((stats (getf plist :stats)))
  (unless (or (null stats) (listp stats))
    (push (format nil "Field :stats has wrong type: expected list/nil, got ~a"
                  (type-of stats)) issues)
    (setf needs-reject t))
  (when (listp stats)
    ;; Existing stats validation...
    ;; NOTE: Only validate stats that exist in current schema
    (dolist (stat-key '(:attack :strength :defense :hitpoints))
      (let ((stat-data (getf stats stat-key)))
        (when stat-data
          (unless (listp stat-data)
            (push (format nil "Stat ~a has wrong type: expected list, got ~a"
                          stat-key (type-of stat-data)) issues)
            (setf needs-reject t))
          (when (listp stat-data)
            ;; Existing stat-data validation...
            ))))))
```

### B.2 Tests

**File:** `tests/persistence-test.lisp`

- `test-4way-reject-inventory-not-list` - `:inventory "string"` → `:reject`
- `test-4way-reject-stats-not-list` - `:stats 123` → `:reject`
- `test-4way-reject-slots-not-list` - `:inventory (:slots "bad")` → `:reject`
- `test-4way-reject-stat-entry-not-list` - `:stats (:attack "bad")` → `:reject`

---

## Phase C: Login Integration (High #2, Medium #6)

**Finding #2:** Login flows use `db-load-player` instead of `db-load-player-validated`, bypassing Phase 6 validation.

**Finding #6:** Quarantine action not handled - quarantined players would spawn into world.

### C.1 Update Login Handler

**File:** `src/net.lisp`

**Location:** `handle-login-request` and the login path in `process-login-async`

**Current flow:**
```lisp
;; session-try-register happens first (for username)
;; then db-load-player is called
(let ((player (db-load-player character-id)))
  ...)
```

**New flow:**

Note: Auth responses use `(list :type :auth-fail :reason ...)` pattern, not `make-auth-response`. The `session-try-register` (username-based) happens before we have character-id, so we must handle rollback if validation fails.

```lisp
;; IMPORTANT: session-try-register already happened earlier in the login flow
;; (registered username in *active-sessions*). If we fail after this point,
;; we MUST call (session-unregister username) to prevent leaked locks.

;; 1. Get character-id from account (existing code)
(let ((character-id (db-get-character-id username)))
  (unless character-id
    (session-unregister username)  ;; Rollback active session
    (send-net-message-with-retry socket
                                  (list :type :auth-fail :reason :no-character)
                                  :host host :port port :max-retries 3 :delay 50)
    (return-from handle-login ...))

  ;; 2. Claim session ownership FIRST (required by db-load-player-validated)
  (unless (claim-session-ownership character-id)
    (session-unregister username)  ;; Rollback active session
    (send-net-message-with-retry socket
                                  (list :type :auth-fail :reason :ownership-conflict)
                                  :host host :port port :max-retries 3 :delay 50)
    (return-from handle-login ...))

  ;; 3. Load with validation
  (multiple-value-bind (player zone-id action)
      (db-load-player-validated character-id)
    (case action
      ((:ok :clamp)
       ;; Normal login or corrected data - proceed
       (when (eq action :clamp)
         (log-verbose "Player ~a loaded with corrections" character-id))
       (register-player-session player :zone-id zone-id)
       ;; ... rest of success path ...
       (send-net-message-with-retry socket
                                    (list :type :auth-ok :player-id (player-id player))
                                    :host host :port port :max-retries 3 :delay 50))

      (:quarantine
       ;; Account needs admin repair - reject login
       (release-session-ownership character-id)
       (session-unregister username)  ;; Rollback active session
       (send-net-message-with-retry socket
                                    (list :type :auth-fail :reason :account-quarantined)
                                    :host host :port port :max-retries 3 :delay 50))

      (:reject
       ;; Dangerous data - reject login
       (release-session-ownership character-id)
       (session-unregister username)  ;; Rollback active session
       (send-net-message-with-retry socket
                                    (list :type :auth-fail :reason :data-corrupted)
                                    :host host :port port :max-retries 3 :delay 50))

      (:not-found
       ;; No existing character data - should not happen for existing account
       (release-session-ownership character-id)
       (session-unregister username)  ;; Rollback active session
       (send-net-message-with-retry socket
                                    (list :type :auth-fail :reason :load-failed)
                                    :host host :port port :max-retries 3 :delay 50)))))
```

**Critical:** Every failure path after `session-try-register` must call `session-unregister` to prevent `*active-sessions*` from leaking locked usernames.

### C.2 Update Async Login Path

**File:** `src/net.lisp`

**Location:** `process-login-async`

Same pattern as C.1, but ensure thread safety around session ownership claims.

### C.3 Tests

**File:** `tests/security-test.lisp`

- `test-login-validates-player-data` - Verify validation runs on login
- `test-login-rejects-quarantine` - Quarantine action blocks login
- `test-login-rejects-corrupt` - Reject action blocks login
- `test-login-clamp-proceeds` - Clamp action allows login with fix

---

## Phase D: Batch Flush Ownership (High #3)

**Finding:** `flush-dirty-players` saves all dirty players without ownership verification, allowing stale servers to overwrite data.

### D.1 Add Ownership Filter

**File:** `src/db.lisp`

**Location:** `flush-dirty-players`

**Current structure (simplified):**
```lisp
(let ((to-flush nil)
      (key-data-pairs nil))
  ;; Collect (player-id . session) into to-flush
  ;; Collect (key . data) into key-data-pairs in parallel
  (maphash (lambda (player-id session)
             (push (cons player-id session) to-flush)
             (push (cons (player-key player-id) (serialize-player ...)) key-data-pairs))
           *player-sessions*)
  ;; Batch save
  (storage-save-batch *storage* key-data-pairs)
  ;; Update session state using to-flush
  ...)
```

**New structure:**

Must maintain parallel `to-flush` and `key-data-pairs` lists, filtering both by ownership:

```lisp
(let ((to-flush nil)
      (key-data-pairs nil)
      (lost-player-ids nil))
  ;; Collect dirty sessions
  (with-player-sessions-lock
    (maphash
     (lambda (player-id session)
       (when (should-flush-p session force)
         ;; Check ownership BEFORE adding to flush list
         (if (verify-session-ownership player-id)
             (progn
               (push (cons player-id session) to-flush)
               (let* ((player (player-session-player session))
                      (zone-id (player-session-zone-id session))
                      (key (player-key player-id))
                      (data (serialize-player player :include-visuals nil :zone-id zone-id)))
                 (setf data (plist-put data :version *player-schema-version*))
                 (push (cons key data) key-data-pairs)))
             ;; Lost ownership - track for cleanup
             (push player-id lost-player-ids))))
     *player-sessions*))

  ;; Handle lost ownership outside lock
  (when lost-player-ids
    (warn "Batch flush: lost ownership for ~d players: ~a"
          (length lost-player-ids) lost-player-ids)
    (dolist (player-id lost-player-ids)
      ;; Clean up session - cannot save, must disconnect
      ;; Note: unregister-player-session doesn't take :reason arg
      (unregister-player-session player-id)))

  ;; Batch save only owned sessions
  (when key-data-pairs
    (storage-save-batch *storage* key-data-pairs))

  ;; Update session state for flushed players (existing code)
  ...)
```

**Key point:** `to-flush` contains `(player-id . session)` pairs for session state updates. `key-data-pairs` contains `(redis-key . plist-data)` pairs for storage. Both lists must stay aligned and both must be filtered by ownership.

### D.2 Alternative: Ownership-Checked Batch Lua Script

For stronger guarantees, create a Lua script that checks ownership atomically:

**File:** `data/redis-scripts/safe_save_batch.lua`

```lua
-- safe_save_batch.lua
-- Saves multiple players, but only if we own each session
-- KEYS[1..N] = session ownership keys
-- KEYS[N+1..2N] = player data keys
-- ARGV[1] = expected owner
-- ARGV[2..N+1] = player data blobs

local n = #KEYS / 2
local saved = 0
local failed = {}

for i = 1, n do
  local owner_key = KEYS[i]
  local data_key = KEYS[n + i]
  local data = ARGV[i + 1]

  local owner = redis.call('GET', owner_key)
  if owner == ARGV[1] then
    redis.call('SET', data_key, data)
    saved = saved + 1
  else
    table.insert(failed, data_key)
  end
end

return {saved = saved, failed = failed}
```

### D.3 Tests

**File:** `tests/persistence-test.lisp`

- `test-batch-flush-skips-unowned` - Verify unowned sessions not saved
- `test-batch-flush-removes-lost-sessions` - Verify cleanup on lost ownership

---

## Phase E: Leaderboard Wiring (Medium #5)

**Finding:** Leaderboards only update on login. XP/level/death changes during gameplay don't update leaderboards.

### E.1 XP Gain Updates

**File:** `src/progression.lisp`

**Location:** `award-combat-xp`, after XP is added.

```lisp
(defun award-combat-xp (player xp-amount)
  ;; ... existing XP logic ...
  (incf (player-lifetime-xp player) xp-amount)

  ;; Update leaderboard (batched with dirty flag)
  (db-update-leaderboard-xp (player-id player) (player-lifetime-xp player))

  ;; Mark dirty for tier-2 save
  (mark-player-dirty (player-id player)))
```

### E.2 Level-Up Updates

**File:** `src/progression.lisp`

**Location:** After level-up detection.

```lisp
;; On level up (already triggers tier-1 save)
(when leveled-up
  (db-update-leaderboard-level (player-id player) new-level)
  ;; Tier-1 save already happens for level-up
  )
```

### E.3 Death Updates

**File:** `src/combat.lisp`

**Location:** `combatant-apply-hit`, when HP reaches 0.

```lisp
(when (<= (player-hp player) 0)
  ;; Increment deaths counter
  (incf (player-deaths player))

  ;; Update leaderboard
  (db-update-leaderboard-deaths (player-id player) (player-deaths player))

  ;; Tier-1 save (death is critical)
  (db-save-player-immediate player)

  ;; ... existing death handling ...
  )
```

### E.4 Tests

**File:** `tests/persistence-test.lisp`

- `test-xp-gain-updates-leaderboard` - Verify XP leaderboard update
- `test-level-up-updates-leaderboard` - Verify level leaderboard update
- `test-death-updates-leaderboard` - Verify death count and leaderboard

---

## Phase F: Robustness (Medium #7, #8)

### F.1 NOSCRIPT Fallback (Finding #7)

**Finding:** Script cache flush causes permanent failures until server restart.

**File:** `src/db.lisp`

**Location:** `storage-eval-script` for redis-storage

**Current handling:** Uses string-based error detection (not typed Redis exceptions):
```lisp
(handler-case
    (red:evalsha sha ...)
  (error (e)
    (let ((err-str (format nil "~a" e)))
      (if (search "NOSCRIPT" err-str)
          (progn
            (warn "Script ~a evicted from Redis cache - cannot reload at runtime" script-name)
            nil)
          ...))))
```

**New:** Add script body caching and reload on NOSCRIPT:

```lisp
;; New global to cache script bodies for reload
(defparameter *redis-script-bodies* (make-hash-table :test 'equal)
  "Cache of script name -> script body for NOSCRIPT recovery.")

;; Modify storage-script-load to also cache body
(defmethod storage-script-load ((storage redis-storage) script-name script-body)
  ;; ... existing SHA caching ...
  (setf (gethash script-name *redis-script-bodies*) script-body)
  sha)

;; Modify storage-eval-script to reload on NOSCRIPT
(defmethod storage-eval-script ((storage redis-storage) script-name keys args)
  (let ((sha (gethash script-name *redis-script-shas*)))
    (unless sha
      (error "Script ~a not loaded" script-name))
    (handler-case
        (redis:with-connection (:host (redis-storage-host storage)
                                :port (redis-storage-port storage))
          (let ((num-keys (length keys))
                (all-args (append keys args)))
            (apply #'red:evalsha sha num-keys all-args)))
      (error (e)
        (let ((err-str (format nil "~a" e)))
          (if (search "NOSCRIPT" err-str)
              ;; Script evicted - reload and retry
              (let ((script-body (gethash script-name *redis-script-bodies*)))
                (if script-body
                    (progn
                      (log-verbose "Script ~a evicted, reloading..." script-name)
                      (redis:with-connection (:host (redis-storage-host storage)
                                              :port (redis-storage-port storage))
                        (let ((new-sha (red:script-load script-body)))
                          (setf (gethash script-name *redis-script-shas*) new-sha)
                          ;; Retry with new SHA
                          (let ((num-keys (length keys))
                                (all-args (append keys args)))
                            (apply #'red:evalsha new-sha num-keys all-args)))))
                    (progn
                      (warn "Script ~a evicted but body not cached - cannot reload" script-name)
                      nil)))
              ;; Other error
              (progn
                (warn "Redis EVALSHA error for ~a: ~a" script-name e)
                nil)))))))
```

**Also needed:** Update `load-trade-scripts` in `src/trade.lisp` to populate `*redis-script-bodies*`:

```lisp
;; In load-trade-scripts, after reading script body:
(let ((script-body (uiop:read-file-string script-path)))
  ;; Cache body for NOSCRIPT recovery (NEW)
  (setf (gethash "trade_complete" *redis-script-bodies*) script-body)
  ;; Load into Redis
  (storage-script-load *storage* "trade_complete" script-body))
```

Note: `load-trade-scripts` (trade.lisp:450) is currently the only script loader, called from `run-server` (net.lisp:1943). If additional scripts are added later (e.g., session ownership), they should follow the same pattern of caching the body in `*redis-script-bodies*`.

### F.2 Trade Version Tagging (Finding #8)

**Finding:** `execute-trade-atomic` serializes without `:version`, creating versionless blobs.

**File:** `src/trade.lisp`

**Location:** `execute-trade-atomic`

**Context:** `serialize-player` does NOT include `:version` by design - version is added at persistence call sites. This is correct because:
- Network snapshots (`:include-visuals t`) don't need version
- Version tagging happens at save time, not serialize time
- `flush-dirty-players` already adds version via `(plist-put data :version *player-schema-version*)`

**Fix:** Add version tagging at trade call site, same pattern as `flush-dirty-players`:

```lisp
;; Current (missing version):
(let ((player-a-data (prin1-to-string (serialize-player player-a)))
      (player-b-data (prin1-to-string (serialize-player player-b))))
  ...)

;; Fixed (add version at call site):
;; Look up zone-id from *player-sessions* (same pattern as flush-dirty-players)
(let* ((session-a (gethash (player-id player-a) *player-sessions*))
       (session-b (gethash (player-id player-b) *player-sessions*))
       (zone-a (and session-a (player-session-zone-id session-a)))
       (zone-b (and session-b (player-session-zone-id session-b)))
       (data-a (serialize-player player-a :include-visuals nil :zone-id zone-a))
       (data-b (serialize-player player-b :include-visuals nil :zone-id zone-b)))
  ;; Add version before stringifying (same pattern as flush-dirty-players)
  (setf data-a (plist-put data-a :version *player-schema-version*))
  (setf data-b (plist-put data-b :version *player-schema-version*))
  (let ((player-a-str (prin1-to-string data-a))
        (player-b-str (prin1-to-string data-b)))
    ;; ... pass to Lua script ...
    ))
```

**Important:** Do NOT modify `serialize-player` to include version - that would bloat network payloads and break the separation between serialization and persistence.

### F.3 Tests

- `test-noscript-recovery` - Verify script reload on NOSCRIPT error
- `test-trade-includes-version` - Verify serialized trade data has `:version`

---

## Phase G: Polish (Low #9, #10)

### G.1 Memory Backend TTL Consistency (Finding #9)

**Finding:** Only `storage-load-raw` checks TTL in memory backend.

**File:** `src/db.lisp`

**Add TTL check to:**

```lisp
(defmethod storage-load ((storage memory-storage) key)
  ;; Check expiration first
  (when (memory-storage-key-expired-p key)
    (memory-storage-cleanup-key storage key)
    (return-from storage-load nil))
  (gethash key (memory-storage-data storage)))

(defmethod storage-exists-p ((storage memory-storage) key)
  (when (memory-storage-key-expired-p key)
    (memory-storage-cleanup-key storage key)
    (return-from storage-exists-p nil))
  (nth-value 1 (gethash key (memory-storage-data storage))))

(defmethod storage-keys ((storage memory-storage) pattern)
  ;; Filter out expired keys
  (let ((keys (call-next-method)))
    (remove-if #'memory-storage-key-expired-p keys)))
```

### G.2 Documentation Update (Finding #10)

**File:** `docs/save.md`

**Find and update:**
- Change "Schema validation rejects invalid data rather than clamping" to describe 4-way outcomes
- Add section on `:ok`, `:clamp`, `:quarantine`, `:reject` outcomes
- Document which fields are clamped vs rejected
- Reference Phase 6 in `docs/db.md` for full details

### G.3 Tests

- `test-memory-storage-load-respects-ttl` - Verify `storage-load` checks TTL
- `test-memory-storage-exists-respects-ttl` - Verify `storage-exists-p` checks TTL
- `test-memory-storage-keys-filters-expired` - Verify `storage-keys` excludes expired

---

## Test Requirements Summary

| Phase | New Tests Required |
|-------|-------------------|
| A | 3 tests (ownership refresh) |
| B | 4 tests (validator type guards) |
| C | 4 tests (login validation integration) |
| D | 2 tests (batch flush ownership) |
| E | 3 tests (leaderboard updates) |
| F | 2 tests (NOSCRIPT, trade version) |
| G | 3 tests (memory TTL consistency) |
| **Total** | **21 new tests** |

---

## Verification Checklist

After all phases complete:

- [ ] `make tests` passes (all test targets)
- [ ] Session ownership persists beyond 60s in live server
- [ ] Login uses `db-load-player-validated`
- [ ] Quarantine accounts cannot log in
- [ ] Batch flush only saves owned sessions
- [ ] Leaderboards update on XP/level/death
- [ ] NOSCRIPT causes automatic reload
- [ ] Trade blobs include `:version`
- [ ] Memory backend TTL matches Redis behavior
- [ ] `docs/save.md` describes 4-way validation

---

## Risk Assessment

| Phase | Risk | Mitigation |
|-------|------|------------|
| A | Breaking live sessions | Test refresh timing carefully; 30s interval is safe |
| C | Login failures | Feature flag to fall back to old path if needed |
| D | Data loss on ownership conflicts | Log extensively; prefer disconnect over data loss |
| E | Performance impact | Leaderboard updates are already O(log N) in Redis |
| F | Script reload race | Single-threaded Lisp makes this safe |

---

## Estimated Scope

| Phase | Files Modified | Complexity |
|-------|---------------|------------|
| A | 2 (db.lisp, server.lisp) | Medium |
| B | 1 (save.lisp) | Low |
| C | 1 (net.lisp) | Medium |
| D | 1 (db.lisp) | Medium |
| E | 2 (progression.lisp, combat.lisp) | Low |
| F | 2 (db.lisp, trade.lisp or save.lisp) | Medium |
| G | 2 (db.lisp, docs/save.md) | Low |
