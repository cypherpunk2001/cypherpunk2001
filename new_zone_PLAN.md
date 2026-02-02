# PLAN: Fix Zone Transition Ghost Sprite & Choppy Cross-Zone Animation

## Goal
Fix two multiplayer zone-transition bugs:
1. **Ghost sprite**: A frozen, unanimated duplicate of a departing player remains visible to observers in the old zone.
2. **Choppy cross-zone animation**: Players visible via edge strips animate with visible stepping and 50ms position jumps instead of smooth motion.

See `new_zone_findings_claude.md` for the full root-cause analysis.

---

## Step 1: Add `:removed-player-ids` to delta snapshots (fixes Ghost Cause A)

**Problem:** Delta snapshots never signal entity removal. When a player leaves a zone
(transition, logout, disconnect, timeout), the old zone's clients keep the stale entity
forever (until full resync). The rendering loop (`rendering.lisp:159-162`) draws every
player struct in the `game-players` array that passes viewport culling — it does not check
player-id — so zeroing the ID is insufficient. The entity must be removed from the array.

**Where:** `src/save.lisp` — `serialize-game-state-delta-for-zone`, `deserialize-game-state-delta`
**Where:** `src/movement.lisp` — `transition-zone`
**Where:** `src/net.lisp` — `remove-player-from-game` (existing, used by disconnect/logout)
**Where:** `src/types.lisp` — `zone-state` struct

### 1a: Track departed players per zone on the server

- Add a `zone-state-departed-players` slot to `zone-state` in `src/types.lisp`:
  a bounded list of player IDs that left this zone since the last snapshot broadcast.
- **Zone transition:** In `transition-zone` (`movement.lisp:1475`), after calling
  `remove-player-from-zone-cache`, push the departing player's ID onto
  `(zone-state-departed-players old-zone-state)`.
- **Logout/disconnect:** In `remove-player-from-game` (`net.lisp:1393-1419`), after calling
  `remove-player-from-zone-cache`, push the departing player's ID onto
  `(zone-state-departed-players zone-state)`. This covers all departure paths:
  ownership-loss disconnect (`net.lisp:3172`), client timeout (`check-client-timeouts`),
  and admin kick (`admin.lisp:323`).

### 1b: Serialize the removal list with retention for UDP reliability

- In `serialize-game-state-delta-for-zone` (`save.lisp:1779-1832`), after building
  `:changed-players`, read `(zone-state-departed-players zone-state)`.
  If non-empty, add `:removed-player-ids` to the snapshot plist as a simple list of fixnum IDs.
- **UDP loss mitigation:** Do NOT clear the departed list after a single serialization.
  Instead, retain each ID for `*removal-retention-snapshots*` consecutive delta cycles
  (default: 3, ~150ms at 20Hz). This ensures that if 1-2 deltas are dropped over UDP,
  the client still receives the removal signal. Use a simple list of `(id . remaining-count)`
  pairs; decrement count each cycle and prune when zero.
- **Re-entry guard:** When a player re-enters a zone (added back to `zone-state-zone-players`
  via `add-player-to-zone-cache`), remove their ID from `zone-state-departed-players`
  immediately. This prevents the retained removal from deleting a player who transitions
  back within the retention window.
- Add `*removal-retention-snapshots*` parameter to `src/config-server.lisp` (default 3).

### 1c: Deserialize and remove on the client

- In `deserialize-game-state-delta` (`save.lisp:1856-1957`), **after** applying `:changed-players`,
  read `:removed-player-ids` from the delta plist.
- **Skip any ID that was just added or updated by `:changed-players` in this same delta.**
  Collect the set of IDs from `:changed-players` first, then filter `:removed-player-ids`
  against it. This is the client-side guard against the retention re-entry race: if the
  server's re-entry guard (1b) missed a cycle, the client still won't delete a live player.
- For each remaining ID: call a new helper `remove-player-by-id-from-game` that:
  1. Finds the player in `(game-players game)` via `find-player-by-id-fast`.
  2. Removes it from the array using `remove` (same pattern as `remove-player-from-game`
     at `net.lisp:1410-1414`).
  3. Rebuilds `(game-entities game)` via `make-entities`.
  4. Rebuilds `(game-player-index-map game)` via `rebuild-player-index-map`.
  5. Removes the ID from interpolation buffer position hash tables so stale lerp
     targets don't cause a flash on slot reuse.
- Note: this is the client-side equivalent of the server's `remove-player-from-game`.
  The server version also handles spatial grid and zone-cache cleanup (not needed on client).

### 1d: Tests

- Unit test: serialize a delta for a zone after a player departs — verify `:removed-player-ids`
  contains the departed ID.
- Unit test: deserialize a delta with `:removed-player-ids` — verify the player is removed
  from `game-players` array and `player-index-map` is rebuilt.
- Unit test: departed IDs are retained for 3 delta cycles, then pruned.
- Unit test: departed list populated on logout path (via `remove-player-from-game`).
- Unit test: **re-entry guard** — player departs zone, then re-enters within retention window;
  verify their ID is removed from `zone-state-departed-players` on re-entry and is NOT
  included in subsequent `:removed-player-ids`.
- Unit test: **client-side guard** — delta contains both a player in `:changed-players` and
  in `:removed-player-ids`; verify the player is NOT removed (changed takes precedence).

---

## Step 2: Suppress edge-strip ghost for one tick after transition (fixes Ghost Cause B)

**Problem:** A player who just transitioned into the new zone spawns at the boundary and
immediately appears in edge strips sent back to the old zone, creating a duplicate ghost.

**Why this is still needed after Step 1:** Step 1 removes the stale main-zone entity, but
the edge-strip system independently sends the just-transitioned player back to the old zone
as a new edge-strip entity in the same tick as the transition. For one tick, the old zone's
clients would see the edge-strip ghost before the next snapshot removes the stale main entity.
The suppression window must be minimal (one tick, not the full cooldown) to preserve cross-zone
visibility.

**Where:** `src/save.lisp` — `serialize-zone-state-filtered` (called by `serialize-edge-strip`)
**Where:** `src/types.lisp` — player struct
**Where:** `src/movement.lisp` — `transition-zone`, cooldown decrement

### 2a: Add `player-transition-source-zone` field

- Add `(transition-source-zone nil)` slot to the player struct in `src/types.lisp`.
  This records which zone the player departed from. Ephemeral — not serialized to DB or network.
- In `transition-zone` (`movement.lisp:1484-1488`), before updating `player-zone-id`,
  set `(player-transition-source-zone player)` to `current-zone-id`.
- Clear it after **one server tick** (not the full cooldown). In the per-player tick in
  `update-zone-transition` (`movement.lisp:1588-1594`), clear `transition-source-zone` to nil
  on the first tick after it was set. Use a simple flag: if `transition-source-zone` is non-nil
  and `zone-transition-cooldown` has already been decremented at least once (i.e.,
  `cooldown < *zone-transition-cooldown-seconds*`), clear it. This gives exactly one tick of
  suppression — enough for the removal signal (Step 1) to reach the old zone's clients.

### 2b: Filter edge-strip serialization

- In `serialize-zone-state-filtered` (`save.lisp:1456-1495`), when iterating players for
  edge-strip serialization, add a check: skip any player whose
  `(player-transition-source-zone player)` equals the **requesting zone-id** (the zone
  that will receive this edge strip).
- Thread the requesting zone-id into `serialize-zone-state-filtered` as a new keyword arg
  `:requesting-zone-id`. The value is available in `serialize-edge-strips-for-zone` as its
  `zone-id` parameter.

### 2c: Tests

- Unit test: player with `transition-source-zone = :zone-a` is excluded from edge strips
  destined for `:zone-a`, but included in edge strips for other zones.
- Unit test: player with `transition-source-zone = nil` (no recent transition or cleared
  after one tick) is included normally.

---

## Step 3: Tick animation on edge-strip entities (fixes Choppy — cheapest win)

**Problem:** Edge-strip player structs have animation fields (`anim-state`, `frame-index`,
`frame-timer`) but they are never updated between snapshots, so animation frames are frozen.

**Where:** `src/main.lisp` — `update-sim` (lines 778-785)

### 3a: Add animation and hit-effect loop for edge strips

- After the existing player/NPC animation loops (`main.lisp:780-785`), add a loop over
  edge-strip entities that calls **both** `update-player-animation` and
  `update-player-hit-effect` (mirroring the main-zone player loop). Without hit-effect
  ticking, cross-zone combat flashes (hit highlights, attack animations) would appear frozen.
  ```lisp
  (let ((edge-strips (game-edge-strips game)))
    (dolist (strip edge-strips)
      (let ((players (getf strip :players)))
        (when players
          (loop :for ep :across players
                :when (and ep (> (player-id ep) 0))
                :do (update-player-animation ep dt)
                    (update-player-hit-effect ep dt))))
      (let ((npcs (getf strip :npcs)))
        (when npcs
          (loop :for en :across npcs
                :when en
                :do (update-npc-animation en dt)
                    (update-npc-hit-effect en dt))))))
  ```
- This iterates edge-strip player structs (full `player` structs from `make-edge-strip-player`)
  and edge-strip NPC structs, ticking animation and hit effects each frame for both.

### 3b: Tests

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

### 4a: Preserve edge-strip identity across snapshots (diff-and-update)

- Change `deserialize-edge-strips` to diff-and-update instead of wholesale replace.
  This also subsumes Step 3's animation preservation — updating in-place preserves
  client-ticked `frame-timer`/`frame-index` naturally.
- Maintain per-strip containers that preserve the `(:edge :zone-id :strip)` grouping
  required by rendering. The data structure is:
  `(game-edge-strip-registry game)`: a hash table mapping `(edge . zone-id)` ->
  strip-container. Each strip-container holds:
  - The `:edge` and `:zone-id` metadata (for rebuilding `game-edge-strips`).
  - A hash table mapping `player-id` -> player struct (persists across snapshots).
  - A hash table mapping `npc-id` -> npc struct (persists across snapshots).
  - A "generation" counter to detect stale entries.
- **Scope:** Both players and NPCs are tracked in the registry. The diff-and-update
  logic is identical for both entity types (match by ID, update fields in-place,
  preserve client-ticked animation timers). NPC edge strips use `make-edge-strip-npc`
  (or equivalent constructor) and `update-npc-animation`/`update-npc-hit-effect`.
- On each new snapshot:
  - Increment a global generation counter.
  - For each incoming strip `(:edge E :zone-id Z :strip data)`:
    - Look up or create the strip-container for key `(E . Z)`.
    - Set its generation to current.
    - For each incoming player: if ID exists in the strip's player table, update fields
      in-place (x, y, dx, dy, anim-state, facing, etc.) but preserve client-ticked
      animation timers (`frame-timer`, `frame-index`) when `anim-state` matches.
      If new, create via `make-edge-strip-player` and add to table.
    - For each incoming NPC: same diff-and-update logic against the strip's NPC table.
    - Mark entities (players and NPCs) not in this snapshot's incoming data as stale;
      remove after one missed snapshot (they left strip range).
  - After processing all strips, prune strip-containers with stale generations
    (entire adjacent zone no longer visible).
  - Rebuild `(game-edge-strips game)` list from the registry for rendering
    (same format as before: list of plists with `:players` and `:npcs` vectors).

### 4b: Add a dedicated edge-strip interpolation buffer

- Add `(game-edge-strip-interp-buffer game)` slot to the `game` struct in `src/types.lisp`.
  Same `interpolation-buffer` type, separate instance. This avoids ID collisions with
  main-zone players (edge-strip player IDs may overlap with main-zone IDs since they
  represent players from other zones).
- **Initialization:** Create the buffer in `make-game` (`src/main.lisp`) alongside the
  existing main interpolation buffer. Use the same `make-interpolation-buffer` constructor
  with capacity 4.
- **Snapshot push:** In the client snapshot receive path (`src/net.lisp`), after
  `deserialize-edge-strips` updates the registry (Step 4a), call a new
  `capture-edge-strip-positions` function that:
  1. Iterates all strip-containers in `game-edge-strip-registry`.
  2. For each player/NPC entity, stores `(x y)` keyed by entity ID in a pooled hash table.
  3. Pushes the snapshot into `game-edge-strip-interp-buffer` via `push-interpolation-snapshot`.
  This mirrors how `capture-entity-positions` (`net.lisp:2129-2185`) works for main entities,
  but reads from the registry instead of `game-players`/`game-npcs`.
- NPC positions are stored with negative IDs (same convention as the main buffer).

### 4c: Lerp edge-strip positions in `interpolate-remote-entities`

- After the existing player/NPC lerp loops, add a loop over edge-strip entities:
  - Get the edge-strip interpolation buffer.
  - Find interpolation bounds (same `find-interpolation-bounds` + alpha).
  - For each edge-strip player in the registry, look up positions in the two bracketing
    snapshots and lerp X/Y.
- Entities appearing for the first time (only in snap-b, not snap-a) snap to position
  with no lerp.

### 4d: Tests

- Unit test: push two snapshots with different edge-strip positions into the buffer.
  Verify `interpolate-remote-entities` produces intermediate positions at alpha=0.5.
- Unit test: edge-strip entity that disappears between snapshots is removed from registry.
- Unit test: edge-strip entity that appears for the first time snaps to position (no lerp).
- Unit test: strip-container grouping by `(edge . zone-id)` preserved across updates.

---

## Step 5: Tests (consolidated)

All tests go in `tests/unit-test.lisp`.

### Ghost sprite tests (Steps 1-2)
- `test-delta-snapshot-includes-removed-player-ids`
- `test-delta-deserialization-removes-player-from-array`
- `test-departed-ids-retained-across-multiple-deltas`
- `test-departed-list-populated-on-logout`
- `test-departed-reentry-guard-clears-on-zone-reenter`
- `test-delta-deserialization-changed-trumps-removed`
- `test-edge-strip-excludes-recently-transitioned-player`
- `test-edge-strip-includes-player-after-one-tick`

### Animation tests (Step 3)
- `test-edge-strip-player-animation-ticks`
- `test-edge-strip-player-hit-effect-ticks`
- `test-edge-strip-npc-animation-ticks`

### Interpolation tests (Step 4)
- `test-edge-strip-position-interpolation`
- `test-edge-strip-entity-removal-on-disappear`
- `test-edge-strip-entity-snap-on-first-appear`
- `test-edge-strip-registry-preserves-strip-grouping`
- `test-edge-strip-npc-identity-preserved-across-snapshots`

---

## Step 6: Verbose logging

All new diagnostic logging uses `log-zone` (gated by `*verbose-zone-transitions*`,
enabled with `MMORPG_VERBOSE_ZONES=1`).

- **Departure tracking**: log when a player ID is added to `zone-state-departed-players`
  (zone-id, player-id, reason: "transition" or "disconnect").
- **Removal serialization**: log `:removed-player-ids` count and retention remaining in delta.
- **Client-side removal**: log when a player is removed from `game-players` due to removal
  signal (player-id, zone-id).
- **Edge-strip suppression**: log when a player is skipped from edge strips due to
  `transition-source-zone` matching the requesting zone (one tick only).
- **Edge-strip identity**: log new/updated/removed edge-strip entities per snapshot cycle
  (count only, not per-entity, to avoid hot-loop spam).

---

## Execution order and dependencies

```
Step 1 (removal list)  ──┐
                          ├── Step 5 (tests) ── Step 6 (logging)
Step 2 (one-tick filter) ─┘
Step 3 (animation tick) ──── Step 4 (identity + interpolation)
```

Steps 1+2 fix the ghost sprite and are independent of Steps 3+4 which fix choppy animation.
Step 2 is a narrow one-tick suppression that prevents the edge-strip ghost in the same tick
as the removal signal. Step 4's diff-and-update subsumes Step 3b's animation preservation,
so Step 3 should be implemented first (animation ticking) and Step 4 then replaces the
wholesale rebuild with identity-preserving updates.

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
8. Player B logs out while in adjacent zone. Verify: no stale ghost in player A's view.

---

## Acceptance Criteria

- No ghost/duplicate sprite when a player crosses zones (observed by other players).
- No stale ghost when a player disconnects/logs out from an adjacent zone.
- Edge-strip players animate smoothly (no frozen frames between snapshots).
- Edge-strip player movement is interpolated (no 50ms position jumps).
- Delta snapshots include `:removed-player-ids` for departed players (retained for 3 cycles).
- Client removes departed players from `game-players` array on receiving removal signal.
- Cross-zone edge-strip visibility preserved (suppression limited to one tick).
- All tests pass (`make tests`).
- Verbose logs provide enough detail to diagnose edge cases.

---

## Key Files Modified

| File | Changes |
|------|---------|
| `src/types.lisp` | Add `zone-state-departed-players` slot; add `player-transition-source-zone` slot; add `game-edge-strip-registry` hash table; add `game-edge-strip-interp-buffer` slot |
| `src/movement.lisp` | Push departed ID in `transition-zone`; set/clear `transition-source-zone` (one-tick window) |
| `src/net.lisp` | Push departed ID in `remove-player-from-game` (logout/disconnect); capture edge-strip positions in interpolation buffer; lerp edge-strip entities; client-side `remove-player-by-id-from-game` |
| `src/save.lisp` | Serialize `:removed-player-ids` in delta with retention; deserialize and remove from array; filter edge strips by `transition-source-zone`; diff-and-update edge strip deserialization via registry |
| `src/main.lisp` | Tick animation on edge-strip entities in `update-sim` |
| `src/config-server.lisp` | Add `*removal-retention-snapshots*` (default 3) |
| `tests/unit-test.lisp` | ~16 new tests covering removal, retention, re-entry guard, edge-strip filtering, animation, hit effects, interpolation, registry |
