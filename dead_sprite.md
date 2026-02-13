# Dead Sprite on Zone Edge (tele vs tele2)

**Date:** 2026-02-04

## Symptom
- Two clients (`tele` and `tele2`) move normally in the same zone.
- When `tele2` crosses into an adjacent zone, `tele2` looks correct to itself.
- On `tele`, a **static “dead” sprite** remains at the edge where `tele2` crossed, **while** `tele` also sees `tele2` moving correctly in the new zone across the seam.

## Likely Root Cause
**Delta snapshots do not remove players who left the zone.**

- Delta deserialization explicitly **preserves existing entities not included in the delta**.
- When a player leaves the zone, the old zone’s delta snapshot does **not** include that player anymore, so the client **never removes** the old player entry.
- Since client animation for remote players relies on snapshot updates, the stale player entry becomes a frozen sprite.

At the same time, **edge strips** (adjacent-zone previews) are still sent in full, so the client *also* sees `tele2` correctly in the new zone. Result: **duplicate visuals** — frozen in old zone + correct in adjacent zone.

## Evidence (Code)

### 1) Delta deserialization keeps old players
`src/save-delta.lisp:132+`:
- `deserialize-game-state-delta` **preserves existing entities not in the delta**.
- No pruning/removal of players who are missing from `:changed-players`.

### 2) Zone changes do not send removal in old zone
`src/save-delta.lisp:55+`:
- `serialize-game-state-delta-for-zone` only includes players **still in the zone**.
- Players who left are simply omitted; no tombstone is sent.

### 3) Player zone-id not updated in delta path
`src/save-deserialize.lisp:312+`:
- `apply-player-compact-direct` updates position/animation but **does not update player-zone-id**.
- So old players remain tagged with their previous zone ID even after crossing.

### 4) Edge strips render adjacent zone entities
`src/save-delta.lisp:103` + `src/save-edge-strips.lisp:226+`:
- Edge strips are **always sent in full** for adjacent zones.
- `src/rendering-entities.lisp:140+` draws edge-strip entities every frame.

This combination explains the exact symptom: **a stale in-zone player + correct edge-strip player**.

## Why It Appears as a “Dead” Sprite
- The stale player is no longer updated by deltas.
- Client-side animation for remote players is snapshot-driven (no sim tick for non-local players).
- The last frame/position remains frozen at the crossing edge.

## Fix Directions (Options)

**Option A (Preferred): Explicit removal/tombstone in deltas**
- When a player leaves a zone, include a `:removed-players` list in that zone’s delta snapshot.
- Client removes those players from `game-players` array and index map.

**Option B: Prune on each delta apply**
- After applying a delta for zone `Z`, prune any existing players whose stored `zone-id-hash` ≠ `Z`.
- Requires storing `zone-id-hash` in player struct or updating `player-zone-id` in compact updates.

**Option C: Periodic full resync for zones**
- Force a full snapshot on zone membership changes in that zone.
- Clears stale players but increases bandwidth.

## Recommendation
Implement **Option A** (tombstones) or **Option B** (client-side pruning). These are deterministic, low-bandwidth, and solve duplicate sprites immediately.

