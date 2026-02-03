# PLAN: Fix Frozen NPC Animation Bug

**Date:** 2026-02-03
**Related:** FINDINGS_frozen_npcs.md

---

## Problem Summary

Some clients render NPCs frozen on their spawn tiles while other clients see the same NPCs animating and moving correctly. The server sim is fine (NPCs still chase/attack), so the issue is **client-side rendering of a stale NPC array**.

**Confirmed root cause (consolidated):**
- Rendering prefers `zone-state-npcs` when a zone-state exists.
- `zone-state-npcs` can become stale because it only syncs on zone change, teleport, or explicit server resync.
- Normal compact snapshots update `game-npcs`, but the renderer keeps using the stale `zone-state-npcs`, so those NPCs appear frozen to that client.

The earlier delta/dirty-flag theory explains **animation phase drift** but does **not** explain “frozen” NPCs when the local animation loop is running. The stale array mismatch is the primary bug to fix.

---

## Chosen Approach: Always Keep zone-state-npcs In Sync

We will make `zone-state-npcs` a strict mirror of `game-npcs` on the client after snapshots, and guard rendering against stale arrays.

**Why this approach:**
- Directly addresses the freeze symptom.
- Zero protocol changes.
- Minimal runtime cost (O(1) mismatch check, O(N) only when resync needed).
- Keeps spatial grid correctness (grid is rebuilt on sync).

---

## Implementation Steps

### Step 1: Sync zone-state-npcs after snapshots when mismatched

**File:** `src/net-snapshot.lisp`

After `apply-game-state` in `apply-snapshot`, add a mismatch check:
- Resolve current `zone-state`.
- If `(and zone-state (not (eq (zone-state-npcs zone-state) (game-npcs game))))`, call `sync-client-zone-npcs`.

This ensures any compact snapshot that replaces/updates `game-npcs` also refreshes the zone-state pointer and spatial grid.

---

### Step 2: Rendering fallback when zone-state-npcs is stale

**File:** `src/rendering-entities.lisp`

In `draw-entities-with-spatial-culling`, when `zone-state` exists:
- Use `zone-state-npcs` **only if** it is `eq` to `game-npcs`.
- Otherwise, render from `game-npcs` and skip the zone-state grid (or rebuild via Step 1 so grid is valid next frame).

This prevents frozen visuals even if a sync is missed.

---

### Step 3: Keep sync-client-zone-npcs as the single source of truth

**File:** `src/main.lisp`

No logic change required beyond relying on `sync-client-zone-npcs`. The function already:
- Updates `zone-state-npcs` to `game-npcs`.
- Rebuilds the NPC spatial grid.

We will call it more consistently (Step 1), and avoid duplicating logic elsewhere.

---

## Files Modified

| File | Changes |
|------|---------|
| `src/net-snapshot.lisp` | Sync `zone-state-npcs` after snapshots when mismatched |
| `src/rendering-entities.lisp` | Render fallback to `game-npcs` when zone-state is stale |
| `tests/unit/zone-continuity-tests.lisp` (or new test file) | Add tests for NPC sync + render selection |

---

## Testing

1. **Unit test:** Build a zone-state with stale `zone-state-npcs`, apply a snapshot, assert `zone-state-npcs` now `eq` `game-npcs`.
2. **Unit test:** Rendering selector returns `game-npcs` when `zone-state-npcs` is stale.
3. **Manual test:**
   - Client A logs in, watch NPCs animate.
   - Client B logs in later, verify both clients see NPCs animating.
   - Force a compact snapshot update (movement/attack) and verify Client A does not freeze.
4. **Manual test:** Zone transitions still render NPCs correctly (grid rebuilt).

---

## Explicit Non-Changes (Not Required for Freeze)

- No change to NPC animation serialization. The freeze bug is not caused by animation frames being omitted in deltas.
- No change to dirty-flag rules. That only affects animation phase drift, not freezing.

If you later want animation phase consistency across clients, that is a separate visual fidelity task, not required for this bug fix.

---

## Verification Checklist

- [ ] `make checkparens`
- [ ] `make tests`
- [ ] Manual: Both clients see animating NPCs concurrently
- [ ] Manual: Zone transition still renders NPCs correctly
