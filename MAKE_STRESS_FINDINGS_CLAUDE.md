# Stress Test Findings - Claude's Analysis

**Date:** 2026-01-21
**Test:** `make stress` with exponential client growth
**Bugs Found:** 2 critical issues affecting zone isolation and client view stability

---

## Bug #1: Players Appearing in All Zones

### Observed Behavior
During stress testing, players spawned by the stress test were visible in ALL zones (zone-1, zone-2, zone-3, zone-4), not just zone-1 where new players should spawn.

### Root Cause: Delta Snapshots Lack Zone Filtering

**Location:** `src/save.lisp:1187-1221` - `serialize-game-state-delta`

The delta snapshot serialization collects ALL dirty players without filtering by zone:

```lisp
(defun serialize-game-state-delta (game seq baseline-seq)
  ...
  ;; Collect dirty players - NO ZONE FILTER
  (when players
    (loop :for player :across players
          :when (and player (player-snapshot-dirty player))  ; <-- Only checks dirty flag
          :do (push (serialize-player-compact player) changed-players)))
```

**Contrast with full snapshots** (`serialize-game-state-for-zone` at save.lisp:1113-1147):
```lisp
;; Serialize ONLY players in this zone
(loop :for player :across players
      :when (and player (eq (player-zone-id player) zone-id))  ; <-- Zone filter
      :do ...)
```

### The Propagation Chain

1. **Server generates delta:** `serialize-game-state-delta` includes ALL dirty players from ALL zones
2. **Server sends to zone groups:** Even though clients are grouped by zone (`group-clients-by-zone` at net.lisp:1205-1215), they receive the same un-filtered delta
3. **Client applies delta:** `apply-game-state-delta` (save.lisp:1244-1310) processes all received players:
   - Updates existing players by ID (OK)
   - **ADDS new players** not in client's array (PROBLEM - cross-zone pollution)
4. **Client renders all:** `draw-game` (rendering.lisp:1592-1593) renders ALL entities in `game-entities` with NO zone filtering

### Evidence

- `net.lisp:1255-1256` has a comment acknowledging this:
  ```lisp
  ;; NOTE: Delta compression currently uses full game state - future optimization
  ;; could filter deltas by zone as well
  ```

- Full resync IS properly zone-filtered (net.lisp:1240-1252 uses `serialize-game-state-for-zone`)
- Delta is NOT zone-filtered

### Impact
- Client's `game-players` array accumulates players from ALL zones
- All players are rendered regardless of their zone-id
- Players from zone-2, zone-3, zone-4 appear in zone-1 client's view

---

## Bug #2: Camera Warping Between Players

### Observed Behavior
The user's personal player camera periodically "warped" to view other players in different zones, then back to their own player.

### Root Cause: Local Player Lookup Failure + Array Pollution

**Location:** `src/save.lisp:820-877` - `apply-player-plists`

The local player identification depends on:
1. `game-net-player-id` - set during auth
2. Finding that ID in the current `game-players` array

When delta pollution adds cross-zone players, the arrays get out of sync:

```lisp
(let* ((local-id (or (game-net-player-id game)
                     (and (game-player game)
                          (player-id (game-player game)))))
       (local-player (and local-id (plusp local-id)
                          (find-player-by-id players local-id))))
  (cond
    ;; Found local player - update game-player if changed
    ((and local-player (not (eq (game-player game) local-player)))
     (setf (game-player game) local-player))  ; <-- Can switch to wrong player object
```

### Potential Failure Modes

1. **Array reordering:** When `players-match-order-p` returns nil, a new array is built. The `local-player` found might be a different struct instance with the same ID.

2. **Fallback to first player:** If local-id is 0 or nil:
   ```lisp
   ((and (or (null local-id) (zerop local-id)) (> (length players) 0))
    (setf (game-player game) (aref players 0)))  ; <-- Takes first player in array
   ```
   With cross-zone pollution, `(aref players 0)` might be a player from another zone.

3. **Delta adding new players:** `apply-game-state-delta` appends new players to the array (save.lisp:1276-1291), potentially changing the local player's position in the array.

### Camera Follow Mechanism

**Location:** `src/editor.lisp:571-577` and `src/rendering.lisp:1582-1585`

```lisp
;; Camera follows game-player
(multiple-value-bind (camera-x camera-y)
    (editor-camera-target editor player)  ; player = game-player
  ...)
```

If `game-player` switches to a different player object (even momentarily), the camera follows that player's coordinates.

---

## Architecture Questions Raised

### 1. Zone Concept Clarity
Is a "zone" meant to be:
- **Isolated shard:** Completely separate simulation, separate server process, no cross-zone visibility
- **Contiguous region:** Same simulation, players filtered by current zone for rendering/snapshots
- **Hybrid:** Players exist in global state but are filtered per-zone for network optimization

Current implementation suggests hybrid, but delta snapshots break the filtering assumption.

### 2. Server Authority Model
The server runs ONE zone per process (net.lisp:2097 comment). But:
- `game-players` array holds players from ALL zones
- Dirty flags are global, not per-zone
- Delta broadcast is global, not zone-scoped

This creates a mismatch: server knows about all zones but doesn't consistently filter by zone.

### 3. Client State Model
Should the client:
- **Only know about its zone:** Never receive cross-zone player data
- **Know about all zones but filter display:** Receive all data, render only current zone
- **Seamlessly handle zone transitions:** Maintain some cross-zone awareness for smooth transitions

Current: Receives all via delta, renders all, no zone filtering at any layer.

---

## Recommended Investigation Areas

1. **Delta snapshot zone filtering:** Should `serialize-game-state-delta` filter by zone-id like `serialize-game-state-for-zone` does?

2. **Client-side zone filtering:** Should rendering filter entities by zone-id before drawing?

3. **Player array management:** Should cross-zone players be removed from client arrays, or never added?

4. **Local player tracking:** Is the current ID-based lookup robust enough, or does it need zone-awareness?

5. **Server architecture:** Should each zone be a completely separate server process with no shared player state?

---

## Code Locations Summary

| Component | File | Lines | Issue |
|-----------|------|-------|-------|
| Delta serialization (no zone filter) | save.lisp | 1187-1221 | Collects ALL dirty players |
| Full serialization (has zone filter) | save.lisp | 1113-1147 | Only same-zone players |
| Delta application (adds new players) | save.lisp | 1244-1310 | Cross-zone pollution |
| Local player lookup | save.lisp | 820-877 | Can switch player |
| Rendering (no zone filter) | rendering.lisp | 1592-1593 | Draws ALL entities |
| Client grouping by zone | net.lisp | 1205-1215 | Groups clients correctly |
| Full resync (zone filtered) | net.lisp | 1240-1252 | Uses zone filter |
| Delta broadcast | net.lisp | 1253-1265 | Uses un-filtered delta |

---

## Test Reproduction

1. Start server: `make server`
2. Start client: `make client` (user logs in as personal player)
3. Start stress: `make stress`
4. Observe: Stress test players appear in ALL zones visible to user's client
5. Observe: User's camera periodically jumps to follow other players
