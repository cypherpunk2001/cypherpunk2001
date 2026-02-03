# PLAN: Code Standards Compliance Fixes

**Date:** 2026-02-03
**Related:** FINDINGS_code_standards.md

---

## Summary

Address three gaps identified in FINDINGS_code_standards.md:
1. Missing tests for batch TTL refresh operations
2. Missing type declarations in hot loops (per-tick functions)
3. Per-tick allocations in snapshot event serialization

---

## Priority Order

| Priority | Issue | Impact | Effort |
|----------|-------|--------|--------|
| 1 | Missing tests for batch TTL refresh | Test coverage gap for critical path | Low |
| 2 | Type declarations in hot loops | Performance + compiler optimization | Medium |
| 3 | Per-tick event plist allocations | GC pressure at scale | Medium |

---

## Step 1: Add Tests for Batch TTL Refresh

**File:** `tests/unit/persistence-core-tests.lisp` (or new `persistence-ttl-tests.lisp`)

### 1.1: Test `storage-refresh-ttl-batch` with memory-storage

```lisp
(defun test-storage-refresh-ttl-batch-basic ()
  "storage-refresh-ttl-batch refreshes TTL on multiple existing keys."
  (let* ((storage (make-instance 'memory-storage))
         (now (get-universal-time))
         (keys '("key1" "key2" "key3")))
    ;; Setup: create keys with TTL
    (dolist (key keys)
      (storage-save storage key "value")
      (setf (gethash key *memory-storage-ttls*) (+ now 60)))  ; 60s TTL
    ;; Act: batch refresh to 120s
    (let ((count (storage-refresh-ttl-batch storage keys 120)))
      ;; Assert: all 3 refreshed
      (assert (= count 3) () "Expected 3 keys refreshed, got ~a" count)
      ;; Assert: TTLs updated
      (dolist (key keys)
        (let ((new-ttl (gethash key *memory-storage-ttls*)))
          (assert (>= new-ttl (+ now 119))
                  () "Key ~a TTL not refreshed" key))))))
```

### 1.2: Test `storage-refresh-ttl-batch` with empty list

```lisp
(defun test-storage-refresh-ttl-batch-empty ()
  "storage-refresh-ttl-batch returns 0 for empty key list."
  (let ((storage (make-instance 'memory-storage)))
    (let ((count (storage-refresh-ttl-batch storage nil 60)))
      (assert (= count 0) () "Expected 0 for empty list, got ~a" count))))
```

### 1.3: Test `storage-refresh-ttl-batch` with missing/expired keys

```lisp
(defun test-storage-refresh-ttl-batch-partial ()
  "storage-refresh-ttl-batch only counts existing non-expired keys."
  (let* ((storage (make-instance 'memory-storage))
         (now (get-universal-time)))
    ;; Setup: key1 exists with valid TTL, key2 expired, key3 missing
    (storage-save storage "key1" "value")
    (setf (gethash "key1" *memory-storage-ttls*) (+ now 60))
    (storage-save storage "key2" "value")
    (setf (gethash "key2" *memory-storage-ttls*) (- now 10))  ; Expired
    ;; Act
    (let ((count (storage-refresh-ttl-batch storage '("key1" "key2" "key3") 120)))
      ;; Assert: only key1 refreshed
      (assert (= count 1) () "Expected 1 key refreshed, got ~a" count))))
```

### 1.4: Test `refresh-all-session-ownerships` batch path

```lisp
(defun test-refresh-all-session-ownerships-uses-batch ()
  "refresh-all-session-ownerships uses batch TTL refresh for efficiency."
  (let* ((storage (make-instance 'memory-storage))
         (*storage* storage)
         (*player-sessions* (make-hash-table))
         (*player-sessions-lock* (bt:make-lock "test-lock"))
         (player1 (make-test-player :id 1))
         (player2 (make-test-player :id 2)))
    ;; Setup: register sessions
    (register-player-session player1)
    (register-player-session player2)
    ;; Act: refresh all
    (let ((failed (refresh-all-session-ownerships)))
      ;; Assert: no failures, sessions still valid
      (assert (null failed) () "Expected no failures, got ~a" failed)
      (assert (gethash 1 *player-sessions*) () "Player 1 session lost")
      (assert (gethash 2 *player-sessions*) () "Player 2 session lost"))))
```

### 1.5: Test batch refresh error handling

```lisp
(defun test-storage-refresh-ttl-batch-error-returns-zero ()
  "storage-refresh-ttl-batch returns 0 on storage error (doesn't throw)."
  ;; Use failing-storage subclass that throws on operations
  (let ((storage (make-instance 'failing-storage)))
    (setf (slot-value storage 'fail-mode) :always)
    (let ((count (storage-refresh-ttl-batch storage '("key1" "key2") 60)))
      (assert (= count 0) () "Expected 0 on error, got ~a" count))))
```

---

## Step 2: Add Type Declarations to Hot Loops

### Target Functions (Per-Tick Execution)

| Function | File | Line | Notes |
|----------|------|------|-------|
| `update-zone-transition` | movement-transition.lisp | 886 | Main zone crossing loop |
| `update-npc-behavior` | ai.lisp | 161 | NPC AI per-tick |
| `update-npc-intent` | ai.lisp | 203 | NPC intent generation |
| `update-npc-movement` | ai.lisp | 273 | NPC position update |
| `update-player-position` | movement-transition.lisp | 33 | Player movement |
| `update-npc-respawns` | combat.lisp | 388 | Respawn timer ticks |
| `update-npc-attack` | combat.lisp | 780 | NPC attack logic |

### 2.1: Type Declarations for `update-zone-transition`

**File:** `src/movement-transition.lisp` (line 886+)

Add declarations after the function docstring:

```lisp
(defun update-zone-transition (game &optional (dt *sim-tick-seconds*))
  "Handle edge-based zone transitions..."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type game game)
           (type single-float dt))
  ;; ... existing code ...
  (let* ((world (game-world game))
         (players (game-players game))
         (transition-count 0))
    (declare (type (or null world) world)
             (type (or null (simple-array t (*))) players)
             (type fixnum transition-count))
    ;; Inside the loop, after binding player-zone-id, tile-size, etc:
    (let* ((player-zone-id ...)
           (tile-size (world-tile-dest-size world))
           (half-w (world-collision-half-width world))
           (half-h (world-collision-half-height world)))
      (declare (type (or null keyword) player-zone-id)
               (type single-float tile-size half-w half-h))
      ;; ... rest of loop ...
```

Key variables to type:
- `world` → `(or null world)`
- `players` → `(or null (simple-array t (*)))`
- `transition-count` → `fixnum`
- `tile-size`, `half-w`, `half-h` → `single-float`
- `min-x`, `max-x`, `min-y`, `max-y` → `single-float`
- `pending` → `(or null keyword)`
- `dt` → `single-float`

### 2.2: Type Declarations for `update-npc-behavior`

**File:** `src/ai.lisp` (line 161+)

```lisp
(defun update-npc-behavior (npc player world)
  "Update NPC behavior state based on player proximity."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type npc npc)
           (type (or null player) player)
           (type world world))
  ;; ... existing code ...
```

### 2.3: Type Declarations for `update-npc-intent`

**File:** `src/ai.lisp` (line 203+)

```lisp
(defun update-npc-intent (npc player world dt)
  "Generate NPC movement intent based on behavior and target."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type npc npc)
           (type (or null player) player)
           (type world world)
           (type single-float dt))
  ;; ... existing code ...
```

### 2.4: Type Declarations for `update-npc-movement`

**File:** `src/ai.lisp` (line 273+)

```lisp
(defun update-npc-movement (npc world dt &optional zone-state)
  "Apply NPC movement based on intent, with collision detection."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type npc npc)
           (type world world)
           (type single-float dt)
           (type (or null zone-state) zone-state))
  ;; ... existing code ...
```

### 2.5: Pattern for Inner Loop Variables

For loop variables and accumulators inside hot functions, add declarations at the start of the `let*` or `loop` form:

```lisp
;; Example: inside update-zone-transition's player loop
(loop :for player :of-type (or null player) :across players
      :for i :of-type fixnum :from 0
      :when player
      :do ...)
```

Or use `declare` inside the loop body when binding computed values:

```lisp
(multiple-value-bind (min-x max-x min-y max-y)
    (get-zone-collision-bounds ...)
  (declare (type single-float min-x max-x min-y max-y))
  ...)
```

---

## Step 3: Reduce Per-Tick Allocations in Event Serialization

**File:** `src/net-server.lisp` (line 405)

### Current Code (Allocates per Tick)

```lisp
(let* ((events (pop-combat-events (game-combat-events game)))
       (event-plists (mapcar #'combat-event->plist events))  ; <-- ALLOCATES
       (current-seq (incf snapshot-seq)))
  ...)
```

### 3.1: Option A — Pre-allocated Event Plist Vector

Add a reusable event plist buffer to the game struct or as a server-local buffer:

**File:** `src/types.lisp`

```lisp
;; Add to game struct or create server-local buffer
(defvar *event-plist-buffer* (make-array 64 :fill-pointer 0 :adjustable nil))
```

**File:** `src/net-server.lisp`

```lisp
(let* ((events (pop-combat-events (game-combat-events game)))
       (event-plists (events-to-plists-into-buffer events *event-plist-buffer*))
       (current-seq (incf snapshot-seq)))
  ...)

(defun events-to-plists-into-buffer (events buffer)
  "Convert combat events to plists, reusing BUFFER to avoid allocation.
   Returns the buffer with fill-pointer set to event count."
  (setf (fill-pointer buffer) 0)
  (dolist (event events)
    (when (< (fill-pointer buffer) (array-dimension buffer 0))
      (vector-push (combat-event->plist-reuse event) buffer)))
  buffer)
```

### 3.2: Option B — Pool Combat Event Plists

Modify `combat-event->plist` to reuse plist conses from a pool:

```lisp
(defvar *event-plist-pool* (make-array 128 :fill-pointer 0))
(defvar *event-plist-pool-index* 0)

(defun combat-event->plist-pooled (event)
  "Convert EVENT to plist using pooled storage."
  (when event
    (let ((plist (or (and (< *event-plist-pool-index* (length *event-plist-pool*))
                         (aref *event-plist-pool* *event-plist-pool-index*))
                    (list :type nil :text nil))))  ; Allocate if pool exhausted
      (setf (getf plist :type) (combat-event-type event)
            (getf plist :text) (combat-event-text event))
      (incf *event-plist-pool-index*)
      plist)))

(defun reset-event-plist-pool ()
  "Reset pool index at start of each snapshot tick."
  (setf *event-plist-pool-index* 0))
```

### 3.3: Option C — Binary Events (Preferred for Production)

If binary snapshots are enabled (`*binary-snapshots-enabled*`), serialize events directly to binary without intermediate plists:

**File:** `src/net-protocol.lisp`

```lisp
(defun encode-combat-events-binary (events buffer offset)
  "Encode EVENTS directly into BUFFER starting at OFFSET.
   Returns new offset after encoding."
  (let ((count (length events)))
    ;; Write event count (1 byte, max 255)
    (setf (aref buffer offset) (min count 255))
    (incf offset)
    ;; Write each event: type-byte + text-length + text-bytes
    (dolist (event events)
      (when event
        (let* ((type (combat-event-type event))
               (text (combat-event-text event))
               (type-byte (ecase type (:combat-log 1) (:hud-message 2)))
               (text-bytes (sb-ext:string-to-octets text :external-format :utf-8))
               (text-len (length text-bytes)))
          (setf (aref buffer offset) type-byte)
          (incf offset)
          (setf (aref buffer offset) (min text-len 255))
          (incf offset)
          (replace buffer text-bytes :start1 offset)
          (incf offset text-len))))
    offset))
```

### Recommendation

- **For now:** Use Option A (pre-allocated vector) as simplest fix
- **Later:** Enable binary snapshots by default in production (already supported)
- Option B (pooling) adds complexity for marginal gain over Option A

---

## Files Modified

| File | Changes |
|------|---------|
| `tests/unit/persistence-core-tests.lisp` | Add batch TTL refresh tests |
| `src/movement-transition.lisp` | Type declarations in `update-zone-transition` |
| `src/ai.lisp` | Type declarations in NPC update functions |
| `src/combat.lisp` | Type declarations in `update-npc-respawns`, `update-npc-attack` |
| `src/net-server.lisp` | Pre-allocated event plist buffer |
| `src/types.lisp` | (Optional) Event plist buffer slot |

---

## Testing

### Step 1 Verification
```bash
make test-unit  # New TTL tests pass
```

### Step 2 Verification
```bash
make checkparens  # Balanced after adding declares
make ci           # Compiles without type warnings
make tests        # All tests pass
```

### Step 3 Verification
```bash
# Profile before/after to verify reduced allocations
MMORPG_VERBOSE=1 make server &
# Connect multiple clients, observe GC frequency
```

### Manual Verification
1. Run server with `MMORPG_VERBOSE=1`, check no type warnings during compile
2. Run 5+ clients, observe smooth performance (no GC stutters)
3. Run `(room)` before/after snapshot loop to verify allocation reduction

---

## Verification Checklist

- [ ] `make checkparens` — balanced
- [ ] `make tests` — all pass (including new TTL tests)
- [ ] No SBCL type warnings during compilation
- [ ] Profiler shows reduced allocations in snapshot path (Step 3)
- [ ] `update-zone-transition` has type declarations for all hot variables
- [ ] NPC update functions have type declarations
- [ ] Batch TTL refresh has >80% test coverage

---

## Non-Goals (Explicitly Out of Scope)

- **SIMD optimization** — Deferred per CLAUDE.md directive
- **Binary protocol by default** — Already supported, just not default
- **File splitting** — No files exceed 1500 LOC threshold
- **Additional retry logic** — Existing coverage is complete per findings

---

## Future Considerations

If profiling shows additional hot spots:

1. **Spatial grid queries** — Already typed in spatial.lisp
2. **Collision detection** — Consider typed arrays for wall maps
3. **Snapshot serialization** — Binary mode eliminates most allocation

The goal is incremental improvement, not premature optimization.
