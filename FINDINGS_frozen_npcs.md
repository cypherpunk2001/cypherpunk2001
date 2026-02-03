# FINDINGS: Frozen NPC Animation Bug

**Date:** 2026-02-03

---

## Symptom

- Player logs in and sees NPCs frozen on their spawn tiles (not animating)
- NPCs still attack/chase when player enters their region (server sim works)
- Another player logs in, sees the SAME NPCs animating correctly
- One client shows frozen, other client shows correct animation

---

## Root Cause: Delta Snapshots Skip Animation-Only Changes

The bug is caused by **animation frame state not being included in delta snapshots** when no other NPC state changed.

### The Dirty Flag Problem

NPCs have a `npc-snapshot-dirty` flag that controls whether they're included in delta snapshots. This flag is ONLY set when:

| Event | File:Line | Sets Dirty |
|-------|-----------|------------|
| Position change (wander/chase) | ai.lisp:193, 321, 349 | Yes |
| HP change (damage) | combat.lisp:70 | Yes |
| Behavior change (aggro) | combat.lisp:220 | Yes |
| Animation frame advance | combat.lisp:836-837 | **NO** |

The `update-npc-animation` function (combat.lisp:819-837) advances `frame-index` and `frame-timer` every tick but **never sets the dirty flag**:

```lisp
(defun update-npc-animation (npc dt)
  "Advance idle animation frames for the NPC."
  ;; ... frame advancement logic ...
  (setf (npc-frame-index npc) frame-index      ; <-- NO dirty flag set!
        (npc-frame-timer npc) frame-timer)))
```

### Delta Snapshot Filtering

In save-delta.lisp:39-41 and 87-90, only dirty NPCs are serialized:

```lisp
(loop :for npc :across npcs
      :when (and npc (npc-snapshot-dirty npc))  ; <-- Filters out idle NPCs
      :do (push (serialize-npc-compact npc npc-zone-id) changed-npcs))
```

An idle NPC that's just animating (not moving or taking damage) will have `npc-snapshot-dirty = nil` and be **excluded from delta snapshots**.

### The Desync Scenario

1. **Server**: NPC idles at spawn, animates frames 0,1,2,3,4,5...
2. **Client A joins**: Receives full snapshot with NPC at frame=5
3. **Client A**: Advances locally to frame=6,7,8,9...
4. **Server sends delta**: NPC not dirty (no position/behavior change), omitted
5. **Client A**: Never receives frame updates, relies on local timer
6. **Client B joins later**: Receives full snapshot with NPC at frame=12
7. **Client B**: Starts at frame=12, advances to 13,14,15...
8. **Result**: Both clients animating at different offsets

### Why "Frozen" Specifically?

The "frozen at frame 0" case likely occurs when:

1. Client joins while NPC is being created/respawned (frame-index=0 per types.lisp:675)
2. Client receives full snapshot before server's first animation tick
3. Delta snapshots omit NPC (not dirty)
4. Client's local `update-npc-animation` should advance, but...

**Possible secondary issue**: If client-side `update-npc-animation` isn't being called for certain NPCs, they stay at frame=0. This could happen if:
- NPC is in a zone not fully loaded
- NPC failed to be added to the game-npcs array
- Some condition prevents the animation loop from running

---

## Code Paths Involved

### Server-Side Serialization
- **Full snapshots**: `serialize-npc-compact` (save-serialize.lisp:362-376) - includes frame-index at position 9
- **Delta snapshots**: `serialize-game-state-delta-for-zone` (save-delta.lisp:55-105) - filters by dirty flag

### Client-Side Deserialization
- **Applying data**: `apply-npc-compact-direct` (save-deserialize.lisp:411-439) - correctly sets frame-index from vec[9]
- **Local animation**: `update-npc-animation` called in main.lisp:1121 for all NPCs

### Dirty Flag Management
- **Set dirty**: ai.lisp (position), combat.lisp (HP/behavior)
- **Clear dirty**: save-delta.lisp:125 (after snapshot sent)
- **Missing**: No dirty flag set for animation changes

---

## Why Players Don't Have This Bug

Players are almost always "dirty" because:
- Player input causes position changes constantly
- Movement always sets `player-snapshot-dirty`
- Players rarely stand perfectly still long enough for this to matter

NPCs can idle for extended periods without any state change except animation.

---

## Evidence Summary

1. **ai.lisp**: `npc-snapshot-dirty` only set on position changes (lines 193, 321, 349)
2. **combat.lisp**: `npc-snapshot-dirty` only set on HP/behavior changes (lines 70, 220)
3. **combat.lisp**: `update-npc-animation` (lines 819-837) advances frames without setting dirty
4. **save-delta.lisp**: Delta serialization filters by `npc-snapshot-dirty` (lines 39-41, 87-90)
5. **types.lisp**: NPC initialized with `frame-index = 0` (line 675)

---

## Recommended Fix

Either:
1. **Always include animation state in deltas** (bandwidth cost: 2 fields per NPC per delta)
2. **Set dirty flag on animation changes** (would include NPC in every delta - high bandwidth)
3. **Drive animation client-side only** (remove server authority over animation frames)
4. **Periodic full snapshots** (force resync every N seconds)

Option 3 is cleanest: animation is purely visual and doesn't affect gameplay. Server only needs to track behavior/position. Client drives its own animation timer.

---

## Update: Client Rendering Uses Stale zone-state-npcs (Likely Primary Cause)

### Evidence (Code-Level)

- Rendering prefers `zone-state-npcs` when a zone-state exists: `src/rendering-entities.lisp:107`.
- Client animation updates run on `game-npcs`: `src/main.lisp:1124`.
- `sync-client-zone-npcs` explicitly warns that stale `zone-state-npcs` causes frozen sprites and updates the pointer + grid: `src/main.lisp:440-457`.
- `apply-snapshot` only calls `sync-client-zone-npcs` on **zone change**, **teleport**, or **server :resync**; normal compact snapshots skip it: `src/net-snapshot.lisp:449-468`.

### Why This Matches the Symptom

- If a zone-state already exists on the client (preload or prior session) and its `zone-state-npcs` array is stale, the renderer will use that stale array even though `game-npcs` is updating and animating correctly.
- One client can appear frozen while another client looks fine because the second client happened to trigger a resync/zone-change path (which runs `sync-client-zone-npcs`) and therefore renders the correct NPC array.

### Consolidated Conclusion (With Original Doc)

- The earlier delta-snapshot/dirty-flag analysis explains **animation phase drift** between clients, but it does **not** explain “frozen” NPCs when client-side animation is running.
- The **primary cause of freezing** is the renderer pulling from a stale `zone-state-npcs` array that is never resynced for normal snapshots.

---

## Updated Directives (Actionable Fixes)

1. **Always reconcile `zone-state-npcs` after snapshots when mismatched.**
   - In `apply-snapshot`, after `apply-game-state`, check `(eq (zone-state-npcs zone-state) (game-npcs game))`.
   - If mismatched, call `sync-client-zone-npcs` to realign pointer and rebuild the grid.
   - This is an O(1) guard with O(N) work only when a mismatch exists.

2. **Render fallback safety.**
   - In `draw-entities-with-spatial-culling`, if `zone-state-npcs` is non-nil but **not** the same array as `game-npcs`, fall back to `game-npcs` instead of rendering stale data.
   - This avoids “frozen” visuals even if a sync is missed.

3. **Regression test coverage.**
   - Add a unit test that creates a zone-state with a stale NPC array, applies a snapshot, and asserts that `zone-state-npcs` is updated to `game-npcs`.
   - Add a test to ensure the renderer uses `game-npcs` when `zone-state-npcs` is stale (or a small helper that selects the NPC array for rendering and unit-test that).

These fixes should eliminate the “frozen NPC” client discrepancy without touching server snapshot formats.
