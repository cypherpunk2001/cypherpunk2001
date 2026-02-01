# PLAN: Fix Zone Transition Ghost Sprite & Choppy Cross-Zone Animation

## Goal
Fix two multiplayer zone-transition bugs:
1. **Ghost sprite**: A frozen, unanimated duplicate of a departing player remains visible to observers in the old zone.
2. **Choppy cross-zone animation**: Players visible via edge strips animate with visible stepping and 50ms position jumps instead of smooth motion.

See `new_zone_findings_claude.md` for the full root-cause analysis.

---

## Step 1: Add `:removed-players` to delta snapshots (fixes Ghost Cause A)

**Problem:** Delta snapshots never signal entity removal. When a player leaves a zone, the old zone's clients keep the stale entity forever (until full resync).

**Where:** `src/save.lisp` — `serialize-game-state-delta-for-zone`, `deserialize-game-state-delta`
**Where:** `src/movement.lisp` — `transition-zone`
**Where:** `src/types.lisp` — `zone-state` struct

### 1a: Track departed players per zone on the server

- Add a `zone-state-departed-players` slot to `zone-state` in `src/types.lisp`:
  a bounded list (or small vector with fill-pointer) of player IDs that left this zone since the last snapshot broadcast.
- In `transition-zone` (`movement.lisp:1475`), after calling `remove-player-from-zone-cache`,
  push the departing player's ID onto `(zone-state-departed-players old-zone-state)`.

### 1b: Serialize the removal list

- In `serialize-game-state-delta-for-zone` (`save.lisp:1779-1832`), after building
  `:changed-players`, read `(zone-state-departed-players zone-state)`.
  If non-empty, add `:removed-player-ids` to the snapshot plist as a simple list of fixnum IDs.
- After serialization, clear the departed list (so removals are only sent once per delta cycle).

### 1c: Deserialize and prune on the client

- In `deserialize-game-state-delta` (`save.lisp:1856-1957`), after applying changed players,
  read `:removed-player-ids` from the delta plist.
- For each ID: find the player in `(game-players game)` by ID and zero out its `player-id`
  (marking the slot as empty/reusable, consistent with existing empty-slot convention).
- Also remove from the interpolation buffer's position hash tables so stale lerp targets
  don't cause a flash on reuse.

### 1d: Tests

- Unit test: serialize a delta for a zone after a player departs — verify `:removed-player-ids`
  contains the departed ID.
- Unit test: deserialize a delta with `:removed-player-ids` — verify the player slot is zeroed.
- Unit test: departed list clears after serialization (not re-sent on next delta).

---

## Step 2: Suppress edge-strip ghost for just-transitioned players (fixes Ghost Cause B)

**Problem:** A player who just transitioned into the new zone spawns at the boundary and
immediately appears in edge strips sent back to the old zone, creating a duplicate ghost.

**Where:** `src/save.lisp` — `serialize-zone-state-filtered` (called by `serialize-edge-strip`)
**Where:** `src/types.lisp` — player struct (existing `zone-transition-cooldown` field)

### 2a: Add `player-transition-source-zone` field

- Add `(transition-source-zone nil)` slot to the player struct in `src/types.lisp`.
  This records which zone the player departed from. Ephemeral — not serialized to DB or network.
- In `transition-zone` (`movement.lisp:1484-1488`), before updating `player-zone-id`,
  set `(player-transition-source-zone player)` to `current-zone-id`.
- Clear it when `zone-transition-cooldown` reaches zero (in the cooldown decrement block,
  `movement.lisp:1588-1594`).

### 2b: Filter edge-strip serialization

- In `serialize-zone-state-filtered` (`save.lisp:1456-1495`), when iterating players for
  edge-strip serialization, add a check: skip any player whose
  `(player-transition-source-zone player)` equals the **requesting zone-id** (the zone
  that will receive this edge strip).
- This requires passing the requesting zone-id into `serialize-zone-state-filtered`. Currently
  the function receives `zone-id` (the adjacent zone being serialized). The requesting zone-id
  is available in `serialize-edge-strips-for-zone` as the `zone-id` parameter — thread it
  through as a new keyword arg `:requesting-zone-id`.

### 2c: Tests

- Unit test: player with `transition-source-zone = :zone-a` is excluded from edge strips
  destined for `:zone-a`, but included in edge strips for other zones.
- Unit test: player with `transition-source-zone = nil` (no recent transition) is included
  normally.
- Unit test: after cooldown expires, `transition-source-zone` is nil and player appears in
  edge strips again.

---

## Step 3: Tick animation on edge-strip entities (fixes Choppy — cheapest win)

**Problem:** Edge-strip player structs have animation fields (`anim-state`, `frame-index`,
`frame-timer`) but they are never updated between snapshots, so animation frames are frozen.

**Where:** `src/main.lisp` — `update-sim` (lines 778-785)

### 3a: Add animation loop for edge strips

- After the existing player/NPC animation loops (`main.lisp:780-785`), add:
  ```lisp
  (let ((edge-strips (game-edge-strips game)))
    (dolist (strip edge-strips)
      (let ((players (getf strip :players)))
        (when players
          (loop :for ep :across players
                :when (and ep (> (player-id ep) 0))
                :do (update-player-animation ep dt))))))
  ```
- This iterates the edge-strip player structs (which are full `player` structs created by
  `make-edge-strip-player`) and ticks their animation forward each frame.
- NPC edge strips: add the equivalent `update-npc-animation` call if edge-strip NPCs exist
  in the strip data.

### 3b: Preserve animation state across snapshot rebuilds

- Currently `deserialize-edge-strips` (`save.lisp:1649-1705`) does wholesale replacement.
  Animation state set by the server snapshot may conflict with client-ticked state.
- On each new snapshot, for each incoming edge-strip player, carry forward the client-side
  `frame-timer` from the previous edge-strip entity with the same `player-id` (if it exists).
  This prevents animation resets every 50ms.
- Implementation: before replacing `game-edge-strips`, build a hash table
  `old-id -> (frame-timer frame-index)` from the current list. After constructing new
  edge-strip players, look up by ID and restore `frame-timer` if the `anim-state` matches.

### 3c: Tests

- Unit test: after calling `update-player-animation` on an edge-strip player struct,
  `frame-timer` advances (confirming the struct is compatible with the animation function).
- This is primarily a visual improvement verified via the manual validation step.

---

## Step 4: Add position interpolation for edge-strip entities (fixes Choppy — smooth motion)

**Problem:** Edge-strip entity positions jump every 50ms (snapshot rate) because there is
no interpolation between snapshots. Same-zone entities use the interpolation buffer for
smooth lerping.

**Where:** `src/net.lisp` — `capture-entity-positions`, `interpolate-remote-entities`
**Where:** `src/save.lisp` — `deserialize-edge-strips`
**Where:** `src/types.lisp` — `game` struct

### 4a: Preserve edge-strip identity across snapshots

- Change `deserialize-edge-strips` to diff-and-update instead of wholesale replace.
- Maintain `(game-edge-strip-index game)`: a hash table mapping `player-id` -> edge-strip
  player struct. This persists across snapshots.
- On each new snapshot:
  - Mark all existing edge-strip entries as "unseen".
  - For each incoming edge-strip player: if ID exists in the index, update its fields
    in-place (position, dx, dy, anim-state, etc.) but preserve client-side interpolation
    state. If new, create and add to index.
  - After processing all strips, remove entries still marked "unseen" (player left strip range).
  - Rebuild the `game-edge-strips` list from the index for rendering.

### 4b: Capture edge-strip positions into the interpolation buffer

- In `capture-entity-positions` (`net.lisp:2129-2185`), after capturing main players and NPCs,
  also iterate `game-edge-strip-index` and store positions in the same hash table using
  a namespaced key to avoid ID collisions with main-zone players (e.g., `(+ player-id 1000000)`
  or a separate hash table).
- Alternative (simpler): use a separate small `edge-strip-interpolation-buffer` on the game
  struct with the same ring-buffer structure. This avoids ID collision entirely.

### 4c: Lerp edge-strip positions in `interpolate-remote-entities`

- After the existing player/NPC lerp loops, add a loop over edge-strip entities that reads
  from the edge-strip interpolation buffer and lerps X/Y.
- Use the same `find-interpolation-bounds` + alpha calculation.

### 4d: Tests

- Unit test: push two snapshots with different edge-strip positions into the buffer.
  Verify `interpolate-remote-entities` produces intermediate positions at alpha=0.5.
- Unit test: edge-strip entity that disappears between snapshots is removed from the index.
- Unit test: edge-strip entity that appears for the first time gets no lerp (snaps to position).

---

## Step 5: Tests (consolidated)

All tests go in `tests/unit-test.lisp`.

### Ghost sprite tests (Steps 1-2)
- `test-delta-snapshot-includes-removed-player-ids`
- `test-delta-deserialization-prunes-removed-players`
- `test-departed-list-clears-after-serialization`
- `test-edge-strip-excludes-recently-transitioned-player`
- `test-edge-strip-includes-player-without-transition-source`
- `test-edge-strip-includes-player-after-cooldown-expires`

### Animation tests (Step 3)
- `test-edge-strip-player-animation-ticks`

### Interpolation tests (Step 4)
- `test-edge-strip-position-interpolation`
- `test-edge-strip-entity-removal-on-disappear`
- `test-edge-strip-entity-snap-on-first-appear`

---

## Step 6: Verbose logging

All new diagnostic logging uses `log-zone` (gated by `*verbose-zone-transitions*`,
enabled with `MMORPG_VERBOSE_ZONES=1`).

- **Departure tracking**: log when a player ID is added to `zone-state-departed-players`.
- **Removal serialization**: log `:removed-player-ids` count in delta snapshot.
- **Client-side pruning**: log when a player slot is zeroed due to removal signal.
- **Edge-strip suppression**: log when a player is skipped from edge strips due to
  `transition-source-zone` matching the requesting zone.
- **Edge-strip identity**: log new/updated/removed edge-strip entities per snapshot cycle
  (count only, not per-entity, to avoid hot-loop spam).

---

## Execution order and dependencies

```
Step 1 (removal list)  ──┐
                          ├── Step 5 (tests) ── Step 6 (logging)
Step 2 (edge-strip filter)┘
Step 3 (animation tick) ──── Step 4 (position interpolation)
```

Steps 1+2 fix the ghost sprite and are independent of Steps 3+4 which fix choppy animation.
Within each pair, the steps are sequential: Step 2 depends on the `transition-source-zone`
field added in Step 2a; Step 4 depends on the identity preservation from Step 4a which
builds on Step 3b's snapshot-diffing concept.

Steps 1+2 can be implemented in parallel with Steps 3+4.

---

## Manual Validation Step (required success step)

1. Run server + client with verbose logging:
   - `MMORPG_VERBOSE_ZONES=1 make server`
   - `MMORPG_VERBOSE_ZONES=1 make client` (two client instances, two players)
2. Player A stays in zone. Player B crosses zone boundary.
3. Verify: **no ghost sprite** visible to player A at the boundary.
4. Verify: **no frozen duplicate** at player B's last position in the old zone.
5. Verify: player B visible across the boundary via edge strip with **smooth animation**
   (comparable to same-zone player quality).
6. Player B crosses back. Verify: no pop, no duplicate, smooth reunion.
7. Repeat for all four directions (east/west/north/south).

---

## Acceptance Criteria

- No ghost/duplicate sprite when a player crosses zones (observed by other players).
- Edge-strip players animate smoothly (no frozen frames between snapshots).
- Edge-strip player movement is interpolated (no 50ms position jumps).
- Delta snapshots include `:removed-player-ids` for departed players.
- Client prunes departed players on receiving removal signal.
- All tests pass (`make tests`).
- Verbose logs provide enough detail to diagnose edge cases.

---

## Key Files Modified

| File | Changes |
|------|---------|
| `src/types.lisp` | Add `zone-state-departed-players` slot; add `player-transition-source-zone` slot; add `game-edge-strip-index` hash table |
| `src/movement.lisp` | Push departed ID in `transition-zone`; set `transition-source-zone`; clear on cooldown expiry |
| `src/save.lisp` | Serialize `:removed-player-ids` in delta; deserialize and prune; filter edge strips by `transition-source-zone`; diff-and-update edge strip deserialization |
| `src/net.lisp` | Capture edge-strip positions in interpolation buffer; lerp edge-strip entities |
| `src/main.lisp` | Tick animation on edge-strip entities in `update-sim` |
| `tests/unit-test.lisp` | ~10 new tests covering removal, edge-strip filtering, animation, interpolation |
