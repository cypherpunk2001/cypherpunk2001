# Dead Sprite at Zone Edge Bug

**Date:** 2026-02-03
**Reporter:** User observed with accounts 'tele' and 'tele2'

---

## Problem Statement

When player tele2 crosses into a new zone, player tele (who stays in the original zone) sees a "dead sprite" at the edge where tele2 crossed. However, tele can still see tele2 moving correctly in the new zone from across the boundary.

---

## Root Cause Analysis

Three interconnected issues in the zone transition and delta compression system:

### Issue 1: Player Zone-ID Not Updated in Delta Deserialization (CRITICAL)

**Location:** `src/save-deserialize.lisp:333-383`

In `apply-player-compact-direct` (called during delta snapshot processing), the `player-zone-id` field is **never updated** from the network snapshot.

```lisp
;; This function updates position, animation, and all other fields
;; but conspicuously omits:
(setf (player-zone-id player) ...)
```

**Code flow:**
1. `save-delta.lisp:169`: `(apply-player-compact-direct existing vec)` processes delta snapshots
2. `save-deserialize.lisp:333-383`: Applies compact vector fields but skips player-zone-id
3. **Result:** Client-side player retains **old zone-id** even though server sent new zone-id in compact vec[19]

**Also affected:** `apply-player-plist` (lines 93-157) similarly does NOT update `player-zone-id` from the plist.

---

### Issue 2: Zone-ID Encoding Exists but Is Never Decoded

**Location:** `src/save-serialize.lisp:333`

Server correctly serializes player zone-id into the compact vector:
```lisp
(aref vec 19) (encode-zone-id (player-zone-id player))
```

However, the deserialization side **never extracts this value**.

In `deserialize-player-compact` (lines 286-331), the returned plist includes `:zone-id-hash` but does not decode the actual zone-id:
```lisp
:zone-id-hash (player-compact-zone-hash vec)  ; Returns only the hash, not the zone-id
```

The `player-compact-zone-hash` function extracts only a hash (last element), not the full zone-id. The zone-id from vec[19] is never reconstructed.

---

### Issue 3: Zone Filtering Uses Stale Client Zone-ID

**Location:** `src/save-delta.lisp:143-163`

The client-side zone filter attempts to prevent rendering out-of-zone entities:
```lisp
(snapshot-zone-id (getf delta :zone-id))
(local-zone-hash (when snapshot-zone-id (encode-zone-id snapshot-zone-id)))

:when (or (null local-zone-hash)
          (zerop entity-zone-hash)
          (= entity-zone-hash local-zone-hash))
```

This creates an asymmetry: players who haven't transitioned yet still have their old player-zone-id on the client, so they receive and process deltas for a player in the NEW zone, updating position to the new zone edge, but player-zone-id remains OLD ZONE.

---

## The Dead Sprite Mechanism

**Exact sequence:**

1. **tele2 transitions to zone-2:** Server calls `transition-zone`, updates `(player-zone-id tele2) = zone-2`

2. **Server broadcasts delta:** `serialize-game-state-delta-for-zone` includes tele2 in zone-2's delta with new zone-id in vec[19]

3. **tele (in zone-1) receives snapshot:** Server includes tele2's last position in zone-1's perspective

4. **tele's client processes delta:** `apply-player-compact-direct` updates tele2's position to (zone-edge-x, zone-edge-y) but **tele2's player-zone-id remains zone-1 on tele's client**

5. **Rendering on tele's client:** Uses stale player-zone-id for collision bounds lookup, renders at edge position -> appears as "frozen sprite at edge"

6. **tele CAN see tele2 in new zone:** Because subsequent snapshots include tele2's position in zone-2, which gets rendered correctly (but the ghost remains)

---

## Why the Sprite Appears "Dead"

The sprite appears dead/frozen because:

1. It's stuck at the zone boundary (position updated but zone-id wasn't)
2. No mechanism removes the stale entity representation
3. Interpolation buffer has old positions that don't smoothly transition
4. `reset-client-sync-state` in `apply-snapshot` (line 391-407) only triggers on explicit zone-change flag, not on player-zone-id mismatch

---

## Affected Code Paths

| File | Function | Issue |
|------|----------|-------|
| `save-delta.lisp` | `deserialize-game-state-delta` | Calls `apply-player-compact-direct` which never sets zone-id |
| `save-deserialize.lisp` | `apply-player-compact-direct` | Missing `(setf (player-zone-id player) ...)` |
| `save-deserialize.lisp` | `apply-player-plist` | Missing `:zone-id` field update |
| `save-deserialize.lisp` | `deserialize-player-compact` | Extracts zone-hash but not actual zone-id from vec[19] |
| `net-snapshot.lisp` | `apply-snapshot` | Zone-change detection only on explicit flag |
| `save-serialize.lisp` | Line 333 | Encodes zone-id into vec[19] but deserialization never uses it |

---

## Summary

This is a multi-layer bug where:
- Server correctly transitions zones and sends the right data (zone-id in vec[19])
- Client receives the data but never applies the player-zone-id update
- Result: ghost sprite at transition point with stale zone-id while real player renders in new zone
