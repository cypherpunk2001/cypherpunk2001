# Stress Test Fix Plan (Comprehensive) - REVISED

**Goal:** Fix all architectural leaks causing cross-zone spawns, entity leakage, client zone thrashing, and camera warping.

**Sources:** MAKE_STRESS_FINDINGS_CLAUDE.md, MAKE_STRESS_FINDINGS_CODEX.MD, MAKE_STRESS_FINDINGS_SHARED.md

**Review:** Incorporates critical findings from Codex review (protocol constraints, transition mechanics).

---

## Latest Review Findings (2026-01-21) - ADDRESSED

| Severity | Finding | Resolution | Phase |
|----------|---------|------------|-------|
| **CRITICAL** | Full resync is still unfiltered when zone-state is nil - `serialize-game-state-compact` used as fallback (net.lisp:1241-1244) | Two-tier fallback (primary zone → starting zone → panic). NEVER skip sending. Remove unfiltered fallback paths entirely. | 3.2, 3.3 |
| **HIGH** | Zone states never created in Phase 3 - only fetched with `get-zone-state`. Zones not loaded fall into unfiltered branch. | Always use `get-or-create-zone-state` with zone-path lookup. Use `zone-path-for-id` helpers (previously dead code). | 3.2 |
| **HIGH** | Nil player-zone handling incomplete - serialization clamps but grouping uses raw nil, causing empty snapshots | THREE-LAYER DEFENSE: (1) Clamp at login (source fix), (2) Clamp in grouping (defense-in-depth), (3) Clamp in serialization (last resort) | 2.5 |
| **MEDIUM** | Missing tests for zone-state nil behavior and NPC/object filtering | Added 7 new required test cases covering nil zone-state, NPC filtering, object filtering, and nil zone-id clamping | Testing Plan |
| **LOW** | `zone-path-for-id` helpers added but not exported/used - dead code | Now used in Phase 3.2 for zone lookup with fallback | 3.2 |

### Open Questions - ANSWERED

**Q: Should Phase 3 include zone-state creation + zone-path fallback now?**
**A: YES.** Phase 3.2 now includes two-tier fallback. This closes the resync leak.

**Q: Should nil zone-id clamping be enforced in Phase 2 (login) or broadcast-time?**
**A: BOTH, as defense-in-depth.** Primary fix is at login (Step 2.5). Grouping and serialization provide fallback layers.

---

## Architecture Decision (CONFIRMED)

**Chosen: Option B - Multi-Zone Single Process**
- One server holds all zones in memory
- Zone filtering at every layer (snapshots, rendering, collision)
- Seamless zone transitions without client reconnection
- Infrastructure partially exists (`*zone-states*`, zone-filtered full snapshots)

**Additional context:**
- Multi-threading (worker threads for parallel snapshot sends) is planned for removal
- This simplifies implementation - no thread-safety concerns for zone state
- Single-threaded server loop processes all zones sequentially

**Current state:** Hybrid/broken - server holds multi-zone data but assumes single-zone in many places. This plan fixes that.

---

## Key Design Decisions (From Review)

### Zone-ID Transmission
**Decision:** Keep zone-id at SNAPSHOT level only, not per-entity.

- Compact format (`serialize-player-compact`) does NOT include zone-id
- Server-side filtering is the PRIMARY gatekeeper
- Client trusts that server only sends same-zone entities
- Client-side entity filtering is defense-in-depth, DEFERRED (not critical path)

### Zone Transition Mechanism
**Decision:** Explicit full-resync trigger on zone change.

- When player crosses zone edge, server updates `player-zone-id`
- Server sets `net-client-needs-full-resync` for that client
- Next broadcast sends full zone-filtered snapshot for new zone
- Client honors snapshot's zone-id (legitimate transition)

### Collision Handling
**Decision:** Keep global collision for now, defer per-zone collision.

- `update-zone-transition` continues using global world collision
- Per-zone collision maps are architectural cleanup (later phase)
- This avoids breaking existing transition mechanics

---

## Phase 1: Fix New Player Spawn Zone

**Problem (CODEX):** New players spawn in whatever zone was last loaded globally.

**Files:** `src/server.lisp:4-15`, `src/config.lisp`, `src/data.lisp`, `data/game-data.lisp`

### Step 1.1: Add `*starting-zone-id*` as tunable parameter

In `src/config.lisp`:
```lisp
(defparameter *starting-zone-id* :zone-1
  "Zone where new players spawn. Must exist in world-graph.")
```

In `src/data.lisp` - add to `*tunable-keys*`:
```lisp
(:starting-zone-id . *starting-zone-id*)
```

In `data/game-data.lisp` - add tunable:
```lisp
(:starting-zone-id :zone-1)
```

### Step 1.2: Use starting zone for new player spawn

In `spawn-player-at-world` (server.lisp:4-15):
```lisp
;; OLD: :zone-id (and zone (zone-id zone))
;; NEW: Use configured starting zone
:zone-id *starting-zone-id*
```

### Step 1.3: Add `zone-path-for-id` helper using existing world-graph

**Placement:** Define in `src/world-graph.lisp` and export from package.

This ensures proper load order (world-graph.lisp loads before net.lisp) and visibility.

```lisp
;; In src/world-graph.lisp:
(defun zone-path-for-id (world zone-id)
  "Return file path for ZONE-ID from WORLD's graph, or nil."
  (let ((graph (world-world-graph world)))
    (when graph
      (world-graph-zone-path graph zone-id))))

(defun zone-path-for-id-exists-p (world zone-id)
  "Return T if ZONE-ID has a valid path in WORLD's graph."
  (not (null (zone-path-for-id world zone-id))))

;; In src/package.lisp - add to exports:
(:export #:zone-path-for-id
         #:zone-path-for-id-exists-p
         ...)
```

**Note:** Uses existing `world-graph-zone-path` function. Export ensures net.lisp can call it.

### Step 1.4: Zone-specific spawn position helper (BLOCKER FIX)

**Problem:** Current spawn uses global `world` collision which reflects last loaded zone. New players would spawn at wrong coordinates even with correct zone-id.

**Solution:** Factor existing collision helpers to accept wall-map parameter, then use for zone-state spawn.

#### Step 1.4a: Factor `blocked-at-p` to accept wall-map (movement.lisp)

Add a wall-map variant that existing functions can call:
```lisp
(defun blocked-at-p-with-map (wall-map x y half-w half-h tile-size)
  "Test collider bounds against blocked tiles using WALL-MAP.
   Factored from blocked-at-p to support per-zone collision."
  (let* ((left (- x half-w))
         (right (+ x half-w))
         (top (- y half-h))
         (bottom (+ y half-h))
         (right-edge (- right *collision-edge-epsilon*))
         (bottom-edge (- bottom *collision-edge-epsilon*))
         (tx1 (floor left tile-size))
         (tx2 (floor right-edge tile-size))
         (ty1 (floor top tile-size))
         (ty2 (floor bottom-edge tile-size)))
    (loop :for ty :from ty1 :to ty2
          :thereis (loop :for tx :from tx1 :to tx2
                         :thereis (wall-blocked-p wall-map tx ty)))))

;; Existing blocked-at-p now delegates:
(defun blocked-at-p (world x y half-w half-h tile-size)
  (blocked-at-p-with-map (world-wall-map world) x y half-w half-h tile-size))
```

#### Step 1.4b: Add zone-state spawn position helper (movement.lisp)

Reuse existing `zone-bounds-from-dimensions` for bounds calculation:
```lisp
(defun zone-state-spawn-position (zone-state)
  "Return valid spawn (x, y) using ZONE-STATE's wall-map.
   Reuses existing zone-bounds-from-dimensions and blocked-at-p-with-map."
  (let* ((wall-map (zone-state-wall-map zone-state))
         (tile-dest-size (* (float *tile-size* 1.0) *tile-scale*))
         ;; Match make-world formula: (* (/ tile-dest-size 2.0) *player-collision-scale*)
         (collision-half (* (/ tile-dest-size 2.0) *player-collision-scale*))
         (width (array-dimension wall-map 1))
         (height (array-dimension wall-map 0)))
    ;; Use existing zone-bounds-from-dimensions
    (multiple-value-bind (min-x max-x min-y max-y)
        (zone-bounds-from-dimensions tile-dest-size width height collision-half collision-half)
      ;; Random point in bounds
      (let ((rand-x (+ min-x (random (max 1.0 (- max-x min-x)))))
            (rand-y (+ min-y (random (max 1.0 (- max-y min-y))))))
        ;; Find open position using factored collision helper
        (find-open-position-with-map wall-map rand-x rand-y
                                     collision-half collision-half tile-dest-size)))))

(defun find-open-position-with-map (wall-map x y half-w half-h tile-size)
  "Find nearest open position using WALL-MAP. Mirrors find-open-tile logic."
  (if (not (blocked-at-p-with-map wall-map x y half-w half-h tile-size))
      (values x y)
      ;; Spiral search (same pattern as find-open-tile)
      (loop :for radius :from 1 :to 20
            :do (loop :for dy :from (- radius) :to radius
                      :do (loop :for dx :from (- radius) :to radius
                                :for tx = (+ (floor x tile-size) dx)
                                :for ty = (+ (floor y tile-size) dy)
                                :for cx = (+ (* tx tile-size) (/ tile-size 2))
                                :for cy = (+ (* ty tile-size) (/ tile-size 2))
                                :when (not (blocked-at-p-with-map wall-map cx cy half-w half-h tile-size))
                                :do (return-from find-open-position-with-map (values cx cy))))
            :finally (return (values x y)))))  ; Fallback
```

#### Step 1.4c: Update spawn-player-at-world (server.lisp)
```lisp
(let* ((zone-path (zone-path-for-id world *starting-zone-id*))
       (zone-state (get-or-create-zone-state *starting-zone-id* zone-path)))
  (multiple-value-bind (spawn-x spawn-y)
      (zone-state-spawn-position zone-state)
    (make-player spawn-x spawn-y :zone-id *starting-zone-id* ...)))
```

**References:** movement.lisp:162 (`zone-bounds-from-dimensions`), movement.lisp:336 (`position-blocked-p`), movement.lisp:387 (`blocked-at-p`).

---

## Phase 2: Fix Auth Flow Zone Assignment

**Problem (CODEX):** Login/register captures zone-id from global `world-zone`, not from player data or config.

**Files:** `src/net.lisp:539-719`, `src/net.lisp:800-828`

**Important:** Do NOT call `get-or-create-zone-state` in auth worker thread. Zone-state creation happens on main thread during snapshot broadcast.

### Step 2.1: New players (registration) get starting zone

In `process-register-async` (net.lisp), when creating new player:
```lisp
;; OLD: (zone-id (and zone (zone-id zone)))
;; NEW: Use configured starting zone
(zone-id *starting-zone-id*)
```

The auth-result should carry this zone-id back to main thread.

### Step 2.2: Returning players use their stored zone-id

In `process-login-async` (net.lisp:539+), the existing flow already loads player via `db-load-player-validated` which returns `loaded-player` with its persisted `zone-id`.

```lisp
;; Existing flow already has loaded-player from db-load-player-validated
;; Use loaded player's zone-id, not global world zone
(let ((player-zone (player-zone-id loaded-player)))
  ;; Return this in auth-result
  (make-auth-result ... :zone-id player-zone ...))
```

**Key:** Don't re-load player; use the already-loaded player from `db-load-player-validated`.

### Step 2.3: Session registration uses player's zone-id (main thread)

In `integrate-auth-results` (net.lisp:800+), when processing auth-result on main thread:
```lisp
;; Use zone-id from auth-result (which came from player struct)
(register-player-session player
                         :zone-id (auth-result-zone-id result)
                         :username username)
```

### Step 2.4: Zone-state creation on main thread (snapshot broadcast)

Zone-state creation happens in `broadcast-snapshots-with-delta` (Phase 3), NOT in auth flow:
```lisp
;; In broadcast loop (main thread), ensure zone-state exists
(let* ((zone-id ...)
       (world (game-world game))
       (zone-path (zone-path-for-id world zone-id))
       (zone-state (get-or-create-zone-state zone-id zone-path)))
  ...)
```

This keeps `*zone-states*` access on the main thread only.

### Step 2.5: Clamp nil/unknown zone-id at login (HIGH PRIORITY - CRITICAL FIX)

**Problem:** Returning players with nil or corrupted zone-id create nil zone groups in broadcast. Current implementation has a bug:
- `group-clients-by-zone` gets the zone-id from `(player-zone-id player)`
- If player-zone-id is nil, the group key is nil
- Clients in nil group may receive wrong-zone data or empty snapshots

**Root cause:** The player struct itself has nil zone-id. All downstream clamping (grouping, serialization) is just masking the corruption.

**Solution:** THREE-LAYER DEFENSE (all required):

#### Layer 1: Clamp at login (SOURCE FIX) - MOST IMPORTANT

In `process-login-async` (net.lisp:539+), after loading player:
```lisp
(let ((player-zone (player-zone-id loaded-player)))
  ;; Clamp nil/unknown zone-id to starting zone
  (when (or (null player-zone)
            (not (zone-path-for-id-exists-p world player-zone)))  ; validate zone exists
    (warn "Player ~d has invalid zone-id ~a, clamping to ~a"
          (player-id loaded-player) player-zone *starting-zone-id*)
    (setf (player-zone-id loaded-player) *starting-zone-id*)
    (mark-player-dirty (player-id loaded-player)))  ; Persist the fix
  ;; Continue with auth-result
  (make-auth-result ... :zone-id (player-zone-id loaded-player) ...))
```

**NOTE:** `zone-path-for-id-exists-p` needs `world` parameter. In auth worker thread, we may not have world access. Options:
- Pass world to auth worker (breaks thread isolation)
- Only check for nil at login, defer unknown-zone validation to main thread
- Cache valid zone-ids at startup

**Recommendation:** At login, only check `(null player-zone)`. Main thread broadcast (Step 3.2) handles unknown zones via fallback.

#### Layer 2: Clamp in grouping (DEFENSE-IN-DEPTH)

In `group-clients-by-zone` (net.lisp:1205+):
```lisp
(defun group-clients-by-zone (clients)
  "Group authenticated clients by their player's zone-id.
   Nil zone-ids are clamped to *starting-zone-id* as defense-in-depth."
  (let ((groups (make-hash-table :test 'eq)))
    (dolist (c clients)
      (when (and (net-client-authenticated-p c)
                 (net-client-player c))
        (let* ((player (net-client-player c))
               ;; CRITICAL: Clamp nil to starting zone
               (zone-id (or (player-zone-id player) *starting-zone-id*)))
          (push c (gethash zone-id groups)))))
    groups))
```

**VERIFY THIS IS IMPLEMENTED:** Current code should already have `(or (player-zone-id player) *starting-zone-id*)` - if not, add it!

#### Layer 3: Clamp in serialization (LAST RESORT)

Already implemented in save.lisp:1131 and save.lisp:1238:
```lisp
:for player-zone = (or (player-zone-id player) *starting-zone-id*)
```

**Why all three layers:**
| Layer | When it helps | What it catches |
|-------|--------------|-----------------|
| Login (L1) | On connect | Persisted nil zone-id (data corruption) |
| Grouping (L2) | Every broadcast | Race condition or bug setting nil after login |
| Serialization (L3) | Per-snapshot | Edge cases where player reaches serialization with nil |

**Why login clamping is primary fix:**
- Fixes the data at source (player struct)
- Persists correction via dirty flag (fixes DB permanently)
- Layers 2-3 should never trigger if Layer 1 works
- Makes corrupted data visible via warning log

**References:** net.lisp:539 (`process-login-async`), net.lisp:1205 (`group-clients-by-zone`), save.lisp:1131, save.lisp:1238.

---

## Phase 3: Fix Delta Snapshot Zone Filtering

**Problem (ALL THREE):** `serialize-game-state-delta` collects ALL dirty entities regardless of zone.

**Files:** `src/save.lisp:1187-1221`, `src/net.lisp:1217-1266`

### Step 3.1: Create `serialize-game-state-delta-for-zone`

Add zone-filtered delta serialization (save.lisp). Note: Keep same signature pattern as existing `serialize-game-state-delta`:

```lisp
(defun serialize-game-state-delta-for-zone (game zone-id zone-state seq)
  "Serialize delta snapshot filtered to ZONE-ID.
   Only includes dirty players in that zone and NPCs from zone-state."
  (let* ((players (game-players game))
         (npcs (if zone-state (zone-state-npcs zone-state) nil))
         (zone (if zone-state (zone-state-zone zone-state) nil))
         (objects (and zone (zone-objects zone)))
         (changed-players nil)
         (changed-npcs nil)
         (object-list nil))
    ;; Filter dirty players by zone
    (when players
      (loop :for player :across players
            :when (and player
                       (player-snapshot-dirty player)
                       (eq (player-zone-id player) zone-id))
            :do (push (serialize-player-compact player) changed-players)))
    ;; Collect dirty NPCs from zone-state only
    (when npcs
      (loop :for npc :across npcs
            :when (and npc (npc-snapshot-dirty npc))
            :do (push (serialize-npc-compact npc) changed-npcs)))
    ;; Zone objects
    (when objects
      (dolist (object objects)
        (push (serialize-object object) object-list)))
    ;; Build delta with EXPLICIT zone-id (matches existing delta-v3 format)
    ;; NOTE: Include :baseline-seq nil to keep delta shape stable
    (list :format :delta-v3
          :seq seq
          :baseline-seq nil  ; Keep shape stable with existing delta-v3
          :zone-id zone-id
          :changed-players (coerce (nreverse changed-players) 'vector)
          :changed-npcs (coerce (nreverse changed-npcs) 'vector)
          :objects (nreverse object-list))))
```

### Step 3.2: Update broadcast to use zone-filtered deltas (CRITICAL FIX)

**CRITICAL BUG IN CURRENT IMPLEMENTATION (net.lisp:1238-1248):**
The current code has TWO failure paths that leave clients in limbo:
1. When `zone-path` is nil: sets `needs-full-resync` but sends nothing
2. When `zone-state` is nil: sets `needs-full-resync` but sends nothing

These clients never receive snapshots and loop forever needing resync!

**FIX:** Always ensure zone-state exists via fallback to `*starting-zone-id*`. Never skip sending - if zone lookup fails, fall back to starting zone.

In `broadcast-snapshots-with-delta` (net.lisp:1217-1266):

**IMPORTANT:** Preserve existing behavior:
1. Track `any-sent` flag across all zone groups
2. Call `clear-snapshot-dirty-flags` AFTER all sends complete (once at end)
3. **NEW: Never skip clients** - always fall back to starting zone if zone lookup fails

```lisp
(defun broadcast-snapshots-with-delta (socket clients game current-seq event-plists)
  ;; FIRST: Check for zone changes (from Step 5.2)
  (dolist (client clients) ...)

  ;; THEN: zone-group broadcast
  (let ((zone-groups (group-clients-by-zone clients))
        (any-sent nil)
        (world (game-world game)))
    (maphash
     (lambda (zone-id zone-clients)
       ;; CRITICAL FIX: Always resolve to a valid zone-state
       ;; Step 1: Try requested zone
       ;; Step 2: If that fails, fall back to *starting-zone-id*
       ;; Step 3: If THAT fails (starting zone invalid?), panic and skip
       (let* ((zone-path (zone-path-for-id world zone-id))
              (effective-zone-id zone-id)
              (zone-state nil))
         ;; Try primary zone
         (if zone-path
             (setf zone-state (get-or-create-zone-state zone-id zone-path))
             ;; Primary failed - try fallback to starting zone
             (let ((fallback-path (zone-path-for-id world *starting-zone-id*)))
               (when fallback-path
                 (warn "Zone ~a: unknown zone-id, falling back to ~a"
                       zone-id *starting-zone-id*)
                 (setf effective-zone-id *starting-zone-id*)
                 (setf zone-state (get-or-create-zone-state *starting-zone-id* fallback-path))
                 ;; Force resync since we changed their zone
                 (dolist (c zone-clients)
                   (setf (net-client-needs-full-resync c) t)))))

         ;; CRITICAL: If zone-state is STILL nil, starting zone is broken - panic
         (unless zone-state
           (warn "CRITICAL: Cannot create zone-state for ~a or fallback ~a - skipping ~d clients!"
                 zone-id *starting-zone-id* (length zone-clients))
           (return-from nil))  ; This should never happen in production

         ;; Now we ALWAYS have a valid zone-state - proceed with zone-filtered sends
         (let ((resync-clients nil)
               (delta-clients nil))
           (dolist (c zone-clients)
             (if (client-needs-full-resync-p c current-seq)
                 (push c resync-clients)
                 (push c delta-clients)))

           ;; Full resync for clients that need it - ALWAYS zone-filtered
           (when resync-clients
             (let ((full-state (serialize-game-state-for-zone game effective-zone-id zone-state)))
               (setf full-state (plist-put full-state :seq current-seq))
               (log-verbose "Zone ~a: resync ~d clients (seq ~d, ~d players)"
                            effective-zone-id (length resync-clients) current-seq
                            (length (getf full-state :players)))
               (send-snapshots-parallel socket resync-clients full-state event-plists 1)
               (dolist (c resync-clients)
                 (setf (net-client-needs-full-resync c) nil))
               (setf any-sent t)))

           ;; Zone-filtered delta for synced clients
           (when delta-clients
             (let ((zone-delta (serialize-game-state-delta-for-zone
                                game effective-zone-id zone-state current-seq)))
               (log-verbose "Zone ~a: delta ~d clients" effective-zone-id (length delta-clients))
               (send-snapshots-parallel socket delta-clients zone-delta event-plists 1)
               (setf any-sent t))))))
     zone-groups)

    ;; Clear dirty flags AFTER all sends complete
    (when any-sent
      (clear-snapshot-dirty-flags game))))
```

**Key changes from current code:**
1. Two-tier fallback: primary zone → starting zone → panic
2. NEVER just set `needs-full-resync` and skip sending
3. Always reach `serialize-game-state-for-zone` or `serialize-game-state-delta-for-zone`
4. `zone-path-for-id` helpers are NOW USED (previously dead code)

**Reference:** net.lisp:1264-1266 (existing dirty flag clearing pattern).

### Step 3.3: Remove unfiltered fallback paths entirely

**CRITICAL:** The original leak exists because of fallback to `serialize-game-state-compact`:

```lisp
;; CURRENT BUG (save.lisp:1117-1123, net.lisp:1241-1244):
;; When zone-state is nil, these functions fall back to game-wide data:
(npcs (if zone-state (zone-state-npcs zone-state) (game-npcs game)))  ; LEAK!
(zone (if zone-state (zone-state-zone zone-state) (world-zone world))) ; LEAK!
```

**FIX:** Remove all fallbacks to global game state in zone-filtered functions:

In `serialize-game-state-for-zone` (save.lisp:1117-1123):
```lisp
;; OLD - Falls back to global on nil zone-state:
(npcs (if zone-state (zone-state-npcs zone-state) (game-npcs game)))
(zone (if zone-state (zone-state-zone zone-state) (world-zone world)))

;; NEW - No fallback, zone-state is REQUIRED (caller ensures it):
(npcs (when zone-state (zone-state-npcs zone-state)))
(zone (when zone-state (zone-state-zone zone-state)))
;; If zone-state is nil, we get empty NPCs/objects - which is correct for
;; an unloaded zone (better than leaking wrong-zone data!)
```

**Policy change:** If `zone-state` is nil:
- Return empty NPCs and objects (not wrong-zone data)
- This should never happen in production (Step 3.2 ensures zone-state exists)
- But if it does, empty > leak

---

## Phase 4: Fix Global World Zone Mutation (Partial)

**Problem (CODEX, SHARED):** Zone transitions mutate global `world` and `*zone-path*`, affecting all players.

**Note:** Per-zone collision is DEFERRED. This phase only stops unnecessary global mutation.

**Files:** `src/movement.lisp:862-967`

### Step 4.1: Stop mutating `*zone-path*` on server

In `try-zone-transition` (movement.lisp):
```lisp
;; Only set *zone-path* for client/local mode, not server
(when (and target-path (not (eq (game-net-role game) :server)))
  (setf *zone-path* target-path))
```

### Step 4.2: Keep `apply-zone-to-world` on server (FOR NOW)

**IMPORTANT:** Do NOT conditionalize `apply-zone-to-world` to client-only yet.

The server currently relies on `apply-zone-to-world` for:
- Collision detection during transitions
- Object interaction
- Zone edge detection

```lisp
;; KEEP THIS ON SERVER - required for transitions to work
(apply-zone-to-world world target-zone)
```

Removing this breaks transitions until per-zone collision is implemented.

**DEFERRED:** Conditionalize `apply-zone-to-world` to client-only AFTER per-zone collision maps are implemented.

### Step 4.3: Server still needs zone loaded for collision

Ensure target zone-state exists (for future per-zone collision):
```lisp
;; Always ensure zone-state exists for the target zone
(get-or-create-zone-state target-zone-id target-path)
```

**DEFERRED to later phase:** Per-zone collision map lookup in movement functions.

---

## Phase 5: Fix Zone Transition with Full-Resync Trigger

**Problem (SHARED):** Client zone-switches based on snapshot zone-id without explicit coordination.

**Solution:** Server triggers full-resync when player changes zones. Detection happens in net loop, not movement.lisp.

**Files:** `src/net.lisp`, `src/types.lisp`

### Step 5.1: Add `net-client-zone-id` field to track client's zone

**Update `net-client` defstruct** in net.lisp (around line 79):
```lisp
(defstruct net-client
  ...
  (zone-id nil)            ; Last known zone-id for this client (NEW)
  (needs-full-resync nil)  ; Already exists
  ...)
```

**Update `make-net-client` calls** - wherever net-client is constructed, initialize zone-id to nil:
```lisp
;; Existing calls - add :zone-id nil (or leave default)
(make-net-client :address addr :port port ...)
```

**Reference:** net.lisp (net-client defstruct and make-net-client usages).

### Step 5.2: Detect zone changes in net loop (snapshot broadcast)

In `broadcast-snapshots-with-delta` (net.lisp:1217), at the START of the function, iterate over the `clients` parameter (NOT `*server-clients*` which doesn't exist):

```lisp
(defun broadcast-snapshots-with-delta (socket clients game current-seq event-plists)
  ;; FIRST: Check for zone changes and trigger full resync
  (dolist (client clients)  ; <-- Use 'clients' parameter, not *server-clients*
    (let ((player (net-client-player client)))
      (when player
        (let ((player-zone (player-zone-id player))
              (client-zone (net-client-zone-id client)))
          ;; If zone changed, trigger full resync and update cached zone
          (when (and player-zone (not (eq player-zone client-zone)))
            (log-verbose "Zone change detected: player ~d zone ~a -> ~a"
                         (player-id player) client-zone player-zone)
            (setf (net-client-needs-full-resync client) t)
            (setf (net-client-zone-id client) player-zone))))))
  ;; THEN: existing zone-group broadcast logic
  (let ((zone-groups (group-clients-by-zone clients))
        (any-sent nil))
    ...))
```

**Why this approach:**
- Uses `clients` parameter already passed to function
- No need for new global variable
- Detection happens before zone-group split
- Clean separation: movement handles transitions, net handles client sync

### Step 5.3: Initialize client zone-id on auth

In `integrate-auth-results` (net.lisp), when setting up client after auth:
```lisp
;; Set initial client zone from player
(setf (net-client-zone-id client) (player-zone-id player))
(setf (net-client-needs-full-resync client) t)  ; First snapshot is full
```

### Step 5.4: Update session zone when player transitions

In `transition-zone` (movement.lisp), keep the existing session update:
```lisp
;; Update session tracking to reflect new zone
(update-player-session-zone (player-id player) target-zone-id)
```

No need to find client here - the net loop handles full-resync detection.

### Step 5.5: Client honors snapshot zone-id for transitions

In `apply-game-state` (save.lisp), the existing zone-switching logic is now CORRECT because:
- Server only sends full resync when zone actually changed
- Snapshot zone-id matches player's new zone
- No "ignore mismatch" needed - mismatches shouldn't happen with proper server filtering

```lisp
;; Existing logic is fine:
(when (and apply-zone snapshot-zone-id
           (not (eq snapshot-zone-id current-zone-id)))
  ;; This is a legitimate transition triggered by server
  (load-zone-for-client snapshot-zone-id ...))
```

### Step 5.6: Reset client zone-id on logout/unauth

**Problem:** Stale `net-client-zone-id` can cause false zone-change detections if client reconnects.

**Solution:** Clear zone-id when client is unauthed or removed.

In `remove-client` or `unauth-client` (net.lisp:864+):
```lisp
;; When removing or un-authenticating a client, reset zone tracking
(setf (net-client-zone-id client) nil)
(setf (net-client-player client) nil)
(setf (net-client-needs-full-resync client) nil)
```

In `handle-timeout` / `handle-disconnect` (net.lisp:2246+):
```lisp
;; Same cleanup for disconnected clients
(when client
  (setf (net-client-zone-id client) nil))
```

**Why this matters:**
- Prevents stale zone diffs if client reconnects
- Keeps net-client state consistent with player state
- Avoids spurious full-resync triggers on reconnection

**References:** net.lisp:864 (client cleanup), net.lisp:2246 (timeout handling).

---

## Phase 6: Strengthen Local Player Tracking

**Problem (CLAUDE):** Local player lookup can fail or switch to wrong player.

**Files:** `src/save.lisp:820-877`

### Step 6.1: Never fall back to first player

Remove the unsafe fallback:
```lisp
;; OLD:
;; ((and (or (null local-id) (zerop local-id)) (> (length players) 0))
;;  (setf (game-player game) (aref players 0)))

;; NEW: If no valid local-id, keep current player or nil
;; The auth-ok message sets game-net-player-id, so this case
;; should only happen before authentication completes
((and (or (null local-id) (zerop local-id)) (> (length players) 0))
 (log-verbose "No local player ID set, keeping current game-player"))
```

### Step 6.2: Log when local player not found (debugging)

```lisp
((and (null local-player) local-id (plusp local-id) (> (length players) 0))
 (log-verbose "Client player ID ~d not found in snapshot (~d players in zone)"
              local-id (length players))
 ;; Keep current game-player - don't switch to nil or wrong player
 )
```

---

## Phase 7: Defense in Depth - Client-Side Filtering (IMPLEMENTED)

**Status:** IMPLEMENTED - v4 compact format with zone-id-hash filtering.

**Implementation:**
- Bumped compact format to v4 (`:compact-v4`, `:delta-v4`)
- Added `zone-id-hash` to player compact vectors (index 21) via `encode-zone-id`
- Added `zone-id-hash` to NPC compact vectors (index 14) via `encode-zone-id`
- `encode-zone-id` uses djb2 hash for cross-process stability (not sxhash)
- Delta deserialization filters players and NPCs by zone-id-hash
- Filter uses snapshot's `:zone-id` field (not player-zone-id which may be nil)

---

## Implementation Order (REVISED)

### Critical (Fix the bugs):
1. **Phase 3.1-3.3:** Zone-filter delta snapshots (highest impact)
2. **Phase 5.1-5.6:** Zone transition with full-resync trigger
3. **Phase 2.1-2.5:** Fix auth flow zone assignment (including login zone clamping)

### Important (Complete the fix):
4. **Phase 1.1-1.4:** New player spawn zone config (with factored collision helpers)
5. **Phase 4.1-4.3:** Stop unnecessary global world mutation (partial - keep `apply-zone-to-world`)

### Stability:
6. **Phase 6.1-6.2:** Strengthen local player tracking

### Completed (Previously Deferred):
7. **Phase 7:** Client-side entity filtering - IMPLEMENTED (v4 format with zone-id-hash)
8. **Phase 4 (full):** Per-zone collision maps + conditionalize `apply-zone-to-world` - IMPLEMENTED
9. **Object pickup/unstuck:** Fixed to use player's zone-id and per-zone collision
   - `progression.lisp` - object pickup syncs zone-state-objects
   - `movement.lisp` - unstuck uses per-zone collision via zone-state wall-maps

---

## Documentation Updates (Required)

New behavior requires doc updates per CLAUDE.md guidelines:

### docs/config.md
- Add `*starting-zone-id*` parameter documentation
- Note: Tunable via `data/game-data.lisp`

### docs/movement.md
- Document new `blocked-at-p-with-map` helper (factored collision)
- Document `zone-state-spawn-position` and `find-open-position-with-map`
- Note: Per-zone collision for spawn is minimal, full per-zone collision deferred

### docs/world-graph.md (or relevant zone doc)
- Document `zone-path-for-id` and `zone-path-for-id-exists-p` exports
- Note: Used by net.lisp for zone validation

### docs/net.md
- Document `net-client-zone-id` field lifecycle (init, update, reset)
- Document zone-change detection in broadcast loop
- Note: Zone clamping at login for corrupted data

---

## Testing Plan

### After each phase:
1. `make tests` - All existing tests pass
2. `make server` + `make client` - Manual verification single player
3. `make stress` - Verify multi-client behavior

### Specific verifications:
- [ ] All stress players spawn in zone-1 (not random zones)
- [ ] User's camera stays on their player throughout stress test
- [ ] No cross-zone entities visible in client
- [ ] Zone transitions work correctly for user's player
- [ ] Returning players load into their saved zone
- [ ] Full resync triggers on zone transition
- [ ] Session zone-id updates on transition

### New test cases:
- `test-new-player-spawns-in-starting-zone`
- `test-returning-player-loads-saved-zone`
- `test-delta-snapshot-zone-filtered` (DONE - tests player filtering)
- `test-auth-uses-player-zone-not-global`
- `test-zone-transition-triggers-full-resync`
- `test-session-zone-updates-on-transition`

### CRITICAL: Missing test coverage (from review feedback)

**These tests MUST be added before Phase 3 is complete:**

#### 1. Zone-state nil behavior tests
```lisp
(defun test-delta-for-zone-with-nil-zone-state ()
  "Test that nil zone-state produces empty NPCs/objects, not wrong-zone data."
  ;; Create game with players in zone-1 but pass nil zone-state
  ;; Should return: players in zone-1, EMPTY npcs, EMPTY objects
  ;; NOT: all game NPCs (the original leak)
  ...)

(defun test-full-snapshot-with-nil-zone-state ()
  "Test that serialize-game-state-for-zone with nil zone-state returns empty."
  ;; Same principle - nil zone-state should NOT fall back to global game data
  ...)
```

#### 2. NPC filtering tests
```lisp
(defun test-delta-for-zone-filters-npcs ()
  "Test that NPCs from zone-state are included, not global game NPCs."
  ;; Create game with global NPCs array
  ;; Create zone-state with different NPCs
  ;; Serialize delta for zone
  ;; Should contain ONLY zone-state NPCs, not game NPCs
  ...)

(defun test-full-snapshot-uses-zone-state-npcs ()
  "Test that full snapshot uses zone-state NPCs, not game-npcs."
  ;; Same verification for serialize-game-state-for-zone
  ...)
```

#### 3. Object filtering tests
```lisp
(defun test-delta-for-zone-filters-objects ()
  "Test that objects from zone-state's zone are included."
  ;; Create zone-state with zone containing objects
  ;; Serialize delta
  ;; Should contain zone-state's zone objects
  ...)
```

#### 4. Nil zone-id clamping tests
```lisp
(defun test-group-clients-by-zone-clamps-nil ()
  "Test that nil player-zone-id is clamped to *starting-zone-id* in grouping."
  ;; Create client with player having nil zone-id
  ;; Call group-clients-by-zone
  ;; Should be in *starting-zone-id* group, NOT nil group
  ...)

(defun test-login-clamps-nil-zone-id ()
  "Test that login process clamps nil zone-id and marks dirty."
  ;; Simulate player load with nil zone-id
  ;; Process through login flow
  ;; Player should have *starting-zone-id* and be marked dirty
  ...)
```

**Rationale:** The existing tests (`test-delta-for-zone-filters-players`, `test-delta-for-zone-nil-player-zone`, `test-delta-for-zone-with-npcs`) cover player filtering but don't verify:
- What happens when zone-state is nil (the critical leak path)
- That NPCs come from zone-state, not game-npcs
- That grouping properly clamps nil zone-ids

---

## Risk Assessment (REVISED)

| Phase | Risk | Mitigation |
|-------|------|------------|
| 1 (Spawn zone) | Low | Config parameter, helper function |
| 2 (Auth flow) | Medium | Careful testing of login vs register |
| 3 (Delta filter) | Low | Additive - new function alongside old |
| 4 (Global mutation) | Low | Only removing unnecessary mutation |
| 5 (Zone transition) | Medium | Test transitions thoroughly |
| 6 (Player tracking) | Low | Removes unsafe fallback |
| 7 (Client filter) | N/A | Deferred |

---

## Summary of All Concerns Addressed

| Source | Concern | Phase | Status |
|--------|---------|-------|--------|
| CLAUDE | Delta snapshots not zone-filtered | 3 | Critical |
| CLAUDE | Client arrays polluted | 3 (server-side) | Via server filtering |
| CLAUDE | Rendering no zone filter | 7 | DEFERRED |
| CLAUDE | Local player lookup fails | 6 | Stability |
| CLAUDE | Camera follows wrong player | 6 | Via player tracking |
| CODEX | Global world-zone mutation | 4 | Important |
| CODEX | New spawns inherit wrong zone | 1 | Important |
| CODEX | Auth uses global world zone | 2 | Critical |
| CODEX | Session zone-id persisted wrong | 2, 5 | Critical |
| CODEX | Full snapshot fallback unfiltered | 3.3 | Critical |
| CODEX | Collision uses active world zone | 4 (partial) | DEFERRED (full) |
| SHARED | Delta carries global zone-id | 3 | Critical |
| SHARED | Client zone-switches on apply | 5 | Critical |

---

## Review Findings Addressed

| Finding | Resolution |
|---------|------------|
| **Critical:** No per-entity zone-id in compact format | Keep at snapshot level; server-side filtering is primary. Client-side filtering DEFERRED. |
| **High:** Phase 4 breaks transitions | Only partial Phase 4 - stop unnecessary mutation. Keep global collision. Per-zone collision DEFERRED. |
| **High:** Phase 5 blocks legitimate transitions | Reworked: Full-resync trigger on zone change. Client honors snapshot zone-id. |
| **Medium:** NPCs lack zone-id | NPCs stay in zone-state; no per-NPC zone-id needed. Server filters by zone-state. |
| **Medium:** Hardcoded :zone-1 | Use `*starting-zone-id*` config with `zone-path-for-id` helper. |
| **Medium:** Zone-state for returning players | `get-or-create-zone-state` before snapshot in auth flow. |

---

## Open Questions - ANSWERED

*Decisions made with RuneScape Classic as reference (small-scale classic MMO).*

### Q1: NPC zone ownership - Should NPCs have zone-id field?

**Answer: NO.** NPCs stay in `zone-state`, no per-NPC zone-id field needed.

Rationale:
- RS Classic: NPCs are fixed to their areas, don't roam between zones
- NPCs live in `zone-state-npcs` which is already per-zone
- Server filters NPCs by pulling from correct zone-state
- Simpler, no struct changes needed

### Q2: Zone transition UX - Loading screen or seamless?

**Answer: LOADING SCREEN.**

Rationale:
- RS Classic: Brief black screen when entering new area
- Players expect/accept brief loading on zone change
- Full-resync trigger naturally creates a "loading moment"
- Simpler to implement than seamless blending
- Can show zone name during transition (flavor)

Implementation:
- Client shows loading overlay when zone-id changes in snapshot
- Loading clears when full snapshot applied and zone rendered
- Optional: Display target zone name/label

### Q3: Adjacent zone visibility - See entities from neighboring zones?

**Answer: NO.** Players only see their current zone.

Rationale:
- RS Classic: You only see your current area
- Cross-zone visibility adds complexity (what's the boundary?)
- Clean mental model: one zone = one view
- Server-side filtering becomes simpler (exact zone match only)
- Matches the "zone as discrete region" design

### Q4: Stress test verification - Should stress test verify zone isolation?

**Answer: YES.** Add explicit zone isolation checks.

New stress test verifications:
- All stress-spawned players have zone-id `:zone-1`
- Client snapshot contains ONLY same-zone players
- No cross-zone entities appear in render
- Zone transition triggers full resync (if stress clients transition)

Implementation:
- Add `--verify-isolation` flag to stress test
- Log warnings if any cross-zone data detected
- Fail stress test if isolation violated
