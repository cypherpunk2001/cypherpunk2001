# Zone Transition Debug Findings — Raw Data Report

**Date:** 2026-02-01
**Session:** Manual walkthrough, single player (player-id=1, account=tele)
**Route:** ZONE-1 -> ZONE-3 (south edge) -> ZONE-1 (north edge back)
**Flags:** `MMORPG_VERBOSE=1 MMORPG_VERBOSE_ZONES=1 MMORPG_VERBOSE_COORDS=1`

---

## 1. Configuration Constants (from config.lisp)

| Parameter | Value | Unit |
|-----------|-------|------|
| `*tile-size*` | 16 | pixels (source atlas) |
| `*tile-scale*` | 4.0 | multiplier |
| **Effective tile (world units)** | **64.0** | `16 * 4.0` |
| `*zone-transition-cooldown-seconds*` | 1.5 | seconds |
| `*zone-hysteresis-in*` | 6.0 | tiles from edge |
| `*zone-hysteresis-out*` | 8.0 | tiles from edge |
| `*zone-commit-margin-tiles*` | 0.5 | tiles |
| `*zone-direction-threshold*` | 0.3 | dot product |
| `*client-zone-cache-capacity*` | 9 | zones |

### Derived values (from server startup log)

```
tile-size=64.00  collider-half=27.20,27.20  wall=[91.20..4004.80, 91.20..4004.80]
```

- Zone dimensions: 64x64 tiles = 4096x4096 world units
- Wall bounds: 91.20 to 4004.80 (both axes)
- Playable range: 4004.80 - 91.20 = 3913.60 world units
- Commit margin in world units: 0.5 * 64.0 = 32.0

---

## 2. World Graph Edges (ZONE-1 <-> ZONE-3)

```lisp
(:from :zone-1 :edge :south :to :zone-3 :spawn-edge :north :offset :preserve-x)
(:from :zone-3 :edge :north :to :zone-1 :spawn-edge :south :offset :preserve-x)
```

Both zones are 64x64 tiles with identical bounds: `(91.2, 4004.8, 91.2, 4004.8)`.

---

## 3. Transition #1: ZONE-1 -> ZONE-3 (walking south)

### 3a. ARM event
```
[ZONE] Zone transition: ARM edge SOUTH for player 1 (dot=0.98)
```

### 3b. COMMIT event
```
[ZONE] Zone transition: COMMIT edge=SOUTH player=1 commit-margin=32.0 time-since-last=-1.00s
```
- `time-since-last=-1.00s` indicates first transition (no previous)

### 3c. Cache state
```
[ZONE] Zone transition: cache MISS for ZONE-3 — synchronous disk load
```

### 3d. Zone change direction
```
[ZONE] Zone transition: ZONE-1 -> ZONE-3 edge=SOUTH spawn-edge=NORTH
```

### 3e. Seam translation computation
```
[ZONE] Seam translation: ZONE-1->ZONE-3 edge=SOUTH path=fallback
  old=(627.6, 3976.2)
  src-bounds=(91.2, 4004.8, 91.2, 4004.8)
  dst-bounds=(91.2, 4004.8, 91.2, 4004.8)
  -> trans=(627.6, 62.6)
  in-bounds=NIL
  blocked=NIL
  overstep=not-applied
```

**Key values:**
- Old position: `(627.6, 3976.2)` — player was 28.6 world units from south wall (4004.8 - 3976.2 = 28.6)
- Raw translated Y: `62.6` — this is where the math placed the player in the new zone
- `in-bounds=NIL` — the translated position (627.6, 62.6) was deemed out of bounds
- `overstep=not-applied`

### 3f. Fallback applied
```
[ZONE] Seam fallback: reason=out-of-bounds overstep=28.61 spawn-edge=NORTH
  base=(627.6, 91.2)
  result=(627.6, 119.8)
```

- Fallback reason: `out-of-bounds`
- Overstep: `28.61` world units (= 28.6 / 64.0 = ~0.45 tiles)
- Base: `(627.6, 91.2)` — north wall boundary
- Result: `(627.6, 119.8)` — base + overstep = 91.2 + 28.6 = 119.8

### 3g. Final position
```
[ZONE] Seam final: path=fallback raw=(627.6,119.8) spawn=(627.6,119.8) fallback-reason=out-of-bounds
```

- Player placed at `(627.6, 119.8)` in ZONE-3
- This is 119.8 - 91.2 = **28.6 world units** inward from the north wall
- In tiles: 28.6 / 64.0 = **~0.45 tiles** from edge

### 3h. Cooldown
```
[ZONE] Zone transition: cooldown active (1.48s remaining) for player 1
  ... (90 lines of cooldown ticking every ~0.017s) ...
[ZONE] Zone transition: cooldown active (0.00s remaining) for player 1
```
- Cooldown starts at 1.48s (not 1.5s — small processing delay)
- Cooldown ticks logged every frame while player is near edge

### 3i. Client-side
```
[VERBOSE] Client zone transitioned to ZONE-3
```
(No position data logged on client side — `*verbose-zone-transitions*` not set in client script)

---

## 4. Transition #2: ZONE-3 -> ZONE-1 (walking north, back)

### 4a. ARM event
```
[ZONE] Zone transition: ARM edge NORTH for player 1 (dot=0.99)
```

### 4b. Cancelled ARM
```
[ZONE] Zone transition: cancel for edge NORTH (intent dropped)
```
- Player briefly stopped or changed direction

### 4c. Re-ARM
```
[ZONE] Zone transition: ARM edge NORTH for player 1 (dot=1.00)
```

### 4d. COMMIT event
```
[ZONE] Zone transition: COMMIT edge=NORTH player=1 commit-margin=32.0 time-since-last=5.82s
```
- `time-since-last=5.82s` — 5.82 seconds between transitions

### 4e. Cache state
```
[ZONE] Zone transition: cache MISS for ZONE-1 — synchronous disk load
```
- ZONE-1 not cached even though player just came from there

### 4f. Zone change direction
```
[ZONE] Zone transition: ZONE-3 -> ZONE-1 edge=NORTH spawn-edge=SOUTH
```

### 4g. Seam translation computation
```
[ZONE] Seam translation: ZONE-3->ZONE-1 edge=NORTH path=fallback
  old=(585.8, 120.1)
  src-bounds=(91.2, 4004.8, 91.2, 4004.8)
  dst-bounds=(91.2, 4004.8, 91.2, 4004.8)
  -> trans=(585.8, 4033.7)
  in-bounds=NIL
  blocked=NIL
  overstep=not-applied
```

**Key values:**
- Old position: `(585.8, 120.1)` — player was 28.9 world units from north wall (120.1 - 91.2 = 28.9)
- Raw translated Y: `4033.7` — this is where the math placed the player in the new zone
- `in-bounds=NIL` — the translated position (585.8, 4033.7) was deemed out of bounds
- `overstep=not-applied`

### 4h. Fallback applied
```
[ZONE] Seam fallback: reason=out-of-bounds overstep=28.88 spawn-edge=SOUTH
  base=(585.8, 4004.8)
  result=(585.8, 3975.9)
```

- Fallback reason: `out-of-bounds`
- Overstep: `28.88` world units (~0.45 tiles)
- Base: `(585.8, 4004.8)` — south wall boundary
- Result: `(585.8, 3975.9)` — base - overstep = 4004.8 - 28.9 = 3975.9

### 4i. Final position
```
[ZONE] Seam final: path=fallback raw=(585.8,3975.9) spawn=(585.8,3975.9) fallback-reason=out-of-bounds
```

- Player placed at `(585.8, 3975.9)` in ZONE-1
- This is 4004.8 - 3975.9 = **28.9 world units** inward from the south wall
- In tiles: 28.9 / 64.0 = **~0.45 tiles** from edge

### 4j. Cooldown
```
[ZONE] Zone transition: cooldown active (1.48s remaining) for player 1
  ... (90 lines) ...
[ZONE] Zone transition: cooldown active (0.00s remaining) for player 1
```

### 4k. Client-side
```
[VERBOSE] Client zone transitioned to ZONE-1
[VERBOSE] Client NPC zone-state synced (zone-id=ZONE-1 npcs=4)
```

---

## 5. Position Data Summary

### Transition 1 (ZONE-1 south -> ZONE-3 north)

| Metric | Value |
|--------|-------|
| Pre-transition position (ZONE-1) | (627.6, 3976.2) |
| Distance from south wall before | 4004.8 - 3976.2 = **28.6 wu** (~0.45 tiles) |
| Raw translated position | (627.6, 62.6) |
| Raw translated in-bounds? | **NIL** (out of bounds) |
| Fallback path taken? | **YES** |
| Post-transition position (ZONE-3) | (627.6, 119.8) |
| Distance from north wall after | 119.8 - 91.2 = **28.6 wu** (~0.45 tiles) |
| X preserved? | YES (627.6 -> 627.6) |

### Transition 2 (ZONE-3 north -> ZONE-1 south)

| Metric | Value |
|--------|-------|
| Pre-transition position (ZONE-3) | (585.8, 120.1) |
| Distance from north wall before | 120.1 - 91.2 = **28.9 wu** (~0.45 tiles) |
| Raw translated position | (585.8, 4033.7) |
| Raw translated in-bounds? | **NIL** (out of bounds) |
| Fallback path taken? | **YES** |
| Post-transition position (ZONE-1) | (585.8, 3975.9) |
| Distance from south wall after | 4004.8 - 3975.9 = **28.9 wu** (~0.45 tiles) |
| X preserved? | YES (585.8 -> 585.8) |

---

## 6. Zone Cache Behavior

- Transition 1: `cache MISS for ZONE-3 — synchronous disk load`
- Transition 2: `cache MISS for ZONE-1 — synchronous disk load` (even though ZONE-1 was just loaded)
- `*client-zone-cache-capacity*` = 9, but server-side cache missed both times
- Client preloaded neighboring zones after each transition (9 zones loaded each time)

---

## 7. Cooldown Logging Volume

- 90 cooldown lines per transition (1.5s / ~0.017s per tick = ~88 ticks)
- Cooldown was actively checking and logging every tick while player was near the edge
- Total cooldown log lines across both transitions: 180

---

## 8. Client Log Timeline (filtered)

```
[VERBOSE] Sim state initialized: player-id=1 npcs=4 zone=ZONE-1
[VERBOSE] Sending login request for tele (Enter key)
[VERBOSE] Assigned player ID: 1
[VERBOSE] Authentication successful
  ... (zone preloading: ZONE-3, ZONE-123, ZONE-137, ZONE-4, ZONE-2, ZONE-107, ZONE-109, ZONE-108) ...
  ... (FBO unload x9: IDs 1-6, 23-25) ...
[VERBOSE] Client zone transitioned to ZONE-3
  ... (zone preloading: ZONE-1, ZONE-152, ZONE-153, ZONE-151, ZONE-137, ZONE-123, ZONE-2) ...
  ... (FBO unload x16: IDs 7-22, 26-33) ...
[VERBOSE] Client zone transitioned to ZONE-1
[VERBOSE] Client NPC zone-state synced (zone-id=ZONE-1 npcs=4)
  ... (zone preloading: ZONE-4, ZONE-107, ZONE-109, ZONE-3) ...
[VERBOSE] Shutting down game resources
```

- Each zone transition triggers FBO unload of old chunks and texture reload for new chunks
- First transition unloaded 9 FBOs, second unloaded 16 FBOs

---

## 9. Raw Log Files

- Server log: `./mmorpg_server.log` (825KB, ~9395 zone-tick lines, 180 cooldown lines)
- Client log: `./mmorpg_client.log` (860 lines including raylib/texture noise)

---

## 10. Observations for Codex / Follow-up Analysis

These are factual observations, not interpretations:

1. **Both transitions took the `fallback` path** — the primary seam translation produced `in-bounds=NIL` every time.
2. **Raw translated Y values (62.6 and 4033.7) were outside wall bounds (91.2..4004.8)** — the translation formula places the player outside the playable area, triggering fallback.
3. **`overstep=not-applied` in both seam translations** — the overstep correction did not run during the translation step; it only appeared in the fallback step.
4. **Both zones have identical bounds** — `src-bounds` and `dst-bounds` are both `(91.2, 4004.8, 91.2, 4004.8)`.
5. **The commit fires when the player is ~28.6 wu (0.45 tiles) from the wall** — consistent with `*zone-commit-margin-tiles*` = 0.5.
6. **The seam translation math**: for a SOUTH edge exit at Y=3976.2, the formula produces Y=62.6 in the destination zone (4096.0 - 3976.2 - collider-offset?). This value is below the wall minimum (91.2), so `in-bounds=NIL`.
7. **The fallback places the player at wall-boundary + overstep** — e.g., 91.2 + 28.6 = 119.8 for the north wall.
8. **Post-transition position is ~0.45 tiles from the destination wall** — mirroring the pre-transition distance from the source wall.
9. **1.5 second cooldown is enforced** — logs show it ticking down every frame.
10. **Server cache misses ZONE-1 on return trip** — the recently-departed zone was not retained in server-side cache.
11. **Client `*verbose-zone-transitions*` was NOT active** — the client script does not read `MMORPG_VERBOSE_ZONES` env var, so client-side zone position diagnostics were not emitted.
