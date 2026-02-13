# Plan: Fix Dead Sprite at Zone Edge

**Date:** 2026-02-03
**Bug:** Ghost sprite appears at zone edge when another player crosses zones

---

## Problem Summary

When player B crosses into a new zone, player A (staying in original zone) sees a "dead sprite" at the crossing point. Root cause: client never updates `player-zone-id` from network snapshots, leaving stale entity state.

---

## Fix Strategy

Two-pronged approach:
1. **Client-side:** Decode and apply zone-id from snapshots
2. **Client-side:** Remove/hide entities that have left the local zone

---

## Step 1: Decode Zone-ID in Compact Vector Deserialization

**File:** `src/save-deserialize.lisp`

### 1a: Add zone-id decoding helper

Add a `decode-zone-id` function (inverse of `encode-zone-id` in save-serialize.lisp):

```lisp
(defun decode-zone-id (encoded)
  "Decode an encoded zone-id back to keyword form.
   Returns nil if encoded is 0 or invalid."
  (when (and encoded (not (zerop encoded)))
    ;; Lookup from zone registry or use hash-based reverse lookup
    (zone-id-from-hash encoded)))
```

Note: May need to maintain a hash->zone-id mapping, or include zone-id as a keyword in the compact format. Check if `encode-zone-id` is reversible.

### 1b: Update `apply-player-compact-direct`

**Location:** Lines 333-383

Add zone-id extraction and update after other field updates:

```lisp
;; After existing field updates, add:
(let ((zone-hash (player-compact-zone-hash vec)))
  (when (and zone-hash (not (zerop zone-hash)))
    ;; Option A: If we have reverse lookup
    (let ((decoded-zone (decode-zone-id zone-hash)))
      (when decoded-zone
        (setf (player-zone-id player) decoded-zone)))
    ;; Option B: If zone-id is sent separately in plist alongside compact vec
    ))
```

### 1c: Update `apply-player-plist`

**Location:** Lines 93-157

Add zone-id field handling:

```lisp
(let ((zone-id (getf plist :zone-id)))
  (when zone-id
    (setf (player-zone-id player) zone-id)))
```

---

## Step 2: Remove Stale Entities When Zone-ID Changes

**File:** `src/save-delta.lisp` or `src/net-snapshot.lisp`

### 2a: Track zone-id changes in delta application

In `deserialize-game-state-delta`, after applying player compact:

```lisp
;; After apply-player-compact-direct
(let ((old-zone (player-zone-id existing))
      (new-zone-hash (player-compact-zone-hash vec)))
  (when (and old-zone new-zone-hash
             (not (= (encode-zone-id old-zone) new-zone-hash)))
    ;; Player has transitioned zones - mark for cleanup if not in our zone
    (let ((local-zone (player-zone-id local-player)))
      (when (and local-zone
                 (not (= new-zone-hash (encode-zone-id local-zone))))
        ;; This player left our zone - remove from local entity list
        (mark-entity-for-removal existing-id)))))
```

### 2b: Alternative - Filter rendering by zone-hash

In rendering code, skip entities whose zone-hash doesn't match local player's zone:

```lisp
;; In draw-other-players or equivalent
(when (and entity-zone-hash local-zone-hash
           (/= entity-zone-hash local-zone-hash))
  ;; Skip rendering - entity is in different zone
  (return-from draw-entity))
```

---

## Step 3: Clear Interpolation Buffer on Zone Transition

**File:** `src/net-snapshot.lisp`

When a remote player's zone-id changes, clear their interpolation history to prevent ghosting:

```lisp
;; In apply-snapshot or delta processing
(when (player-zone-changed-p old-zone new-zone)
  (clear-interpolation-for-entity player-id))
```

---

## Step 4: Server-Side Edge Strip Notification (Optional Enhancement)

**File:** `src/net-server.lisp` or `src/save-edge-strips.lisp`

When player transitions, send explicit "player left zone" notification to remaining players:

```lisp
;; In transition-zone or post-transition hook
(broadcast-to-zone old-zone-id
  (list :type :entity-left :id player-id))
```

Client handles by immediately removing entity from render list.

---

## Implementation Order

1. **Step 1c** - Quick win: Add `:zone-id` handling in `apply-player-plist`
2. **Step 2b** - Add zone-hash filter in rendering (defensive)
3. **Step 1a/1b** - Full zone-id decoding in compact format
4. **Step 3** - Clear interpolation on zone change
5. **Step 4** - Optional server notification (if above doesn't fully fix)

---

## Testing

1. Two players in same zone, one crosses to adjacent zone
   - Verify no ghost sprite at crossing point
   - Verify player renders correctly in new zone from observer's view

2. Player rapidly crossing back and forth
   - No accumulated ghost sprites
   - Smooth visual transition

3. Multiple players crossing simultaneously
   - All transitions clean

4. Edge case: Player disconnects mid-transition
   - No permanent ghost

---

## Files to Modify

| File | Changes |
|------|---------|
| `src/save-deserialize.lisp` | Add zone-id decoding, update `apply-player-compact-direct` and `apply-player-plist` |
| `src/save-delta.lisp` | Add zone transition detection and entity cleanup |
| `src/net-snapshot.lisp` | Clear interpolation on zone change |
| `src/rendering-entities.lisp` | Add zone-hash filter (defensive) |
| `tests/unit/snapshot-tests.lisp` | Add zone transition tests |

---

## Risk Assessment

- **Low risk:** Steps 1c, 2b (additive changes, defensive filters)
- **Medium risk:** Steps 1a/1b (need to verify encode/decode roundtrip)
- **Low risk:** Step 3 (isolated to interpolation system)
- **Medium risk:** Step 4 (new network message type)

Recommend implementing in order listed to get quick fix first, then harden.

---

## Addendum (2026-02-04) — Consolidated Findings (dead_sprite.md + dead_sprite_claude.md)

### Key Corrections
- **Primary root cause is missing removals in delta snapshots**, not just missing zone-id updates.
  - Delta deserialization preserves entities not present in the delta, so a player who leaves the zone is never removed.
  - Edge strips are still rendered from adjacent zones, so the client sees both: a frozen in-zone sprite **and** the correct adjacent-zone sprite.
- **`zone-id-hash` is not reversible.** The compact vector stores a hash, not a full zone-id. A decode helper is not viable unless we build and maintain a hash→zone-id map. This makes the original “decode zone-id” steps unreliable.

### Updated Fix Strategy (Authoritative)
1) **Server sends explicit removals on zone membership change (preferred)**
   - Track last-sent player IDs per zone (e.g., store a set in `zone-state`).
   - On each delta snapshot for a zone, compute `removed-players = last_ids − current_ids`.
   - Include `:removed-players` (vector of IDs) in the delta snapshot.
   - Update the stored last_ids after sending.

2) **Client prunes removed players immediately**
   - In `deserialize-game-state-delta`, if `:removed-players` is present:
     - Remove those IDs from `game-players` array.
     - Rebuild the player index map.
     - Clear any interpolation state for those IDs (if stored).

3) **Fallback option (simple, higher bandwidth)**
   - If membership changed, force a full snapshot for that zone instead of delta.
   - This automatically rebuilds the player array and eliminates ghosts.

### Secondary/Optional Enhancements
- If needed for debugging, add a **player-zone-hash** field to the player struct and update it on compact applies. This is informational only and does not solve the removal problem by itself.
- Rendering filters based on zone-hash can remain as a defensive layer but should **not** be the primary fix.

### Plan Adjustments
- **Supersede Steps 1a/1b** (zone-id decode). These are not required for the core fix and are unreliable without a reversible mapping.
- **Replace Step 2 with explicit removals** (as above).
- Keep Step 3 (interpolation cleanup) but trigger it from removal handling rather than zone-id deltas.

### Tests to Add
- **Delta removal test:** a player leaves a zone → delta includes `:removed-players` → client removes player.
- **Edge-strip duplication test:** after removal, only edge-strip representation remains (no in-zone ghost).
- **Full-resync fallback test** (if chosen): membership change forces resync, no stale players left.

This addendum is the authoritative interpretation; earlier decode-zone-id steps are now deprecated.
