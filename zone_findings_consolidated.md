# Zone Transition Debug Findings — Consolidated Report

**Date:** 2026-02-01
**Contributors:** Claude Code (live debug session + data collection), Codex (code analysis + log review)
**Status:** Data collection and analysis complete. Awaiting plan phase.

---

## Table of Contents

1. [How the Data Was Collected](#1-how-the-data-was-collected)
2. [Configuration Constants](#2-configuration-constants)
3. [World Graph Edges](#3-world-graph-edges-zone-1--zone-3)
4. [Transition #1 Raw Data: ZONE-1 -> ZONE-3](#4-transition-1-raw-data-zone-1---zone-3-walking-south)
5. [Transition #2 Raw Data: ZONE-3 -> ZONE-1](#5-transition-2-raw-data-zone-3---zone-1-walking-north)
6. [Position Data Summary Tables](#6-position-data-summary-tables)
7. [Zone Cache Behavior](#7-zone-cache-behavior)
8. [Cooldown Behavior](#8-cooldown-behavior)
9. [Client Log Timeline](#9-client-log-timeline)
10. [Code Analysis — Why Seam Translation Fails (Codex)](#10-code-analysis--why-seam-translation-fails-codex)
11. [Perceived Pop vs Logged Pop (Codex)](#11-perceived-pop-vs-logged-pop-codex)
12. [Findings Comparison: Common and Unique](#12-findings-comparison-common-and-unique)
13. [Consolidated Observations](#13-consolidated-observations)
14. [Recommendations](#14-recommendations)
15. [Raw Log Files](#15-raw-log-files)

---

## 1. How the Data Was Collected

### Claude Code — Live Debug Session

A live debugging session was conducted with the user playing the game. No code changes were made.

**Procedure:**
1. Server started with maximum verbose logging: `MMORPG_VERBOSE=1 MMORPG_VERBOSE_ZONES=1 MMORPG_VERBOSE_COORDS=1 make server`
2. Client started with available verbose logging: `MMORPG_VERBOSE=1 MMORPG_VERBOSE_COORDS=1 make client`
3. User logged in as account `tele` (player-id=1)
4. User walked south to the nearest edge tile in ZONE-1, was popped to ZONE-3
5. User walked north back to the nearest edge tile in ZONE-3, was popped back to ZONE-1
6. User closed the client
7. Server and client stdout were captured to `./mmorpg_server.log` and `./mmorpg_client.log`

**Limitation noted:** The client script (`scripts/client.lisp`) does not read the `MMORPG_VERBOSE_ZONES` environment variable, so `*verbose-zone-transitions*` was not set on the client side. Client-side zone position diagnostics were therefore not emitted. Only `*verbose*` and `*verbose-coordinates*` were active on the client.

### Codex — Code + Log Analysis

Codex performed post-hoc analysis of:
- `mmorpg_client.log` and `mmorpg_server.log` (same logs from the live session)
- Source code in `src/movement-transition.lisp` and `src/config.lisp`

Codex traced the code paths to explain *why* the logged behavior occurs, identifying the specific functions and line numbers responsible.

**Goal described by user:** Eliminate visible position pops when crossing zones; player should be able to stand on any visible edge tile without being teleported inside the adjacent zone.

---

## 2. Configuration Constants

From `src/config.lisp` and server startup log.

| Parameter | Value | Unit | Source |
|-----------|-------|------|--------|
| `*tile-size*` | 16 | pixels (source atlas) | config.lisp:51 |
| `*tile-scale*` | 4.0 | multiplier | config.lisp:53 |
| **Effective tile (world units)** | **64.0** | `16 * 4.0` | derived |
| `*zone-transition-cooldown-seconds*` | 1.5 | seconds | config.lisp:109 |
| `*zone-hysteresis-in*` | 6.0 | tiles from edge | config.lisp:113 |
| `*zone-hysteresis-out*` | 8.0 | tiles from edge | config.lisp:115 |
| `*zone-commit-margin-tiles*` | 0.5 | tiles | config.lisp:120 |
| `*zone-direction-threshold*` | 0.3 | dot product | config.lisp:127 |
| `*client-zone-cache-capacity*` | 9 | zones | config.lisp:132 |

### Derived values (from server startup log)

```
tile-size=64.00  collider-half=27.20,27.20  wall=[91.20..4004.80, 91.20..4004.80]
```

| Derived | Value | Calculation |
|---------|-------|-------------|
| Zone dimensions | 64x64 tiles = 4096x4096 world units | |
| Wall bounds | 91.20 to 4004.80 (both axes) | |
| Playable range | 3913.60 world units | 4004.80 - 91.20 |
| Commit margin (world units) | 32.0 | 0.5 * 64.0 |
| Effective commit margin (Codex) | 32.0 | max(0.5 * 64, max(27.2, 27.2)) = max(32, 27.2) = 32 |
| Collider half-width | 27.20 | server log |
| Collider half-height | 27.20 | server log |

---

## 3. World Graph Edges (ZONE-1 <-> ZONE-3)

```lisp
(:from :zone-1 :edge :south :to :zone-3 :spawn-edge :north :offset :preserve-x)
(:from :zone-3 :edge :north :to :zone-1 :spawn-edge :south :offset :preserve-x)
```

Both zones are 64x64 tiles with identical bounds: `(91.2, 4004.8, 91.2, 4004.8)`.

### Full ZONE-1 neighborhood (from world-graph.lisp)

| Direction | Neighbor | Offset |
|-----------|----------|--------|
| North | ZONE-108 | preserve-x |
| South | ZONE-3 | preserve-x |
| East | ZONE-2 | preserve-y |
| West | ZONE-123 | preserve-y |

### Full ZONE-3 neighborhood (from world-graph.lisp)

| Direction | Neighbor | Offset |
|-----------|----------|--------|
| North | ZONE-1 | preserve-x |
| South | ZONE-152 | preserve-x |
| East | ZONE-4 | preserve-y |
| West | ZONE-137 | preserve-y |

---

## 4. Transition #1 Raw Data: ZONE-1 -> ZONE-3 (walking south)

### 4a. ARM event
```
[ZONE] Zone transition: ARM edge SOUTH for player 1 (dot=0.98)
```

### 4b. COMMIT event
```
[ZONE] Zone transition: COMMIT edge=SOUTH player=1 commit-margin=32.0 time-since-last=-1.00s
```
- `commit-margin=32.0` — effective margin in world units
- `time-since-last=-1.00s` — first transition of session (no previous)

### 4c. Cache state
```
[ZONE] Zone transition: cache MISS for ZONE-3 — synchronous disk load
[VERBOSE] Zone loaded: /home/telecommuter/repos/mmorpg/data/zones/zone-3.lisp id=ZONE-3 size=64x64 layers=4
```

### 4d. Zone change direction
```
[ZONE] Zone transition: ZONE-1 -> ZONE-3 edge=SOUTH spawn-edge=NORTH
```

### 4e. Seam translation computation
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
- Old position: `(627.6, 3976.2)` — player was 28.6 wu from south wall (4004.8 - 3976.2)
- Raw translated Y: `62.6` — below wall minimum (91.2), therefore out of bounds
- `in-bounds=NIL` — translated position rejected
- `overstep=not-applied` — overstep correction did not run during translation step

### 4f. Fallback applied
```
[ZONE] Seam fallback: reason=out-of-bounds overstep=28.61 spawn-edge=NORTH
  base=(627.6, 91.2)
  result=(627.6, 119.8)
```

- Fallback reason: `out-of-bounds`
- Overstep: `28.61` world units (~0.45 tiles)
- Base: `(627.6, 91.2)` — north wall boundary
- Result: `(627.6, 119.8)` — base + overstep = 91.2 + 28.6 = 119.8

### 4g. Final position
```
[ZONE] Seam final: path=fallback raw=(627.6,119.8) spawn=(627.6,119.8) fallback-reason=out-of-bounds
```

- Player placed at `(627.6, 119.8)` in ZONE-3
- Distance from north wall: 119.8 - 91.2 = **28.6 wu** (~0.45 tiles) inward

### 4h. Cooldown
```
[ZONE] Zone transition: cooldown active (1.48s remaining) for player 1
  ... (90 lines of cooldown ticking every ~0.017s) ...
[ZONE] Zone transition: cooldown active (0.00s remaining) for player 1
```
- Cooldown starts at 1.48s (not 1.5s — small processing delay)
- 90 cooldown log lines emitted (one per tick while player near edge)

### 4i. Client-side
```
[VERBOSE] Client zone transitioned to ZONE-3
```
(No position data — `*verbose-zone-transitions*` not active on client)

---

## 5. Transition #2 Raw Data: ZONE-3 -> ZONE-1 (walking north)

### 5a. ARM event
```
[ZONE] Zone transition: ARM edge NORTH for player 1 (dot=0.99)
```

### 5b. Cancelled ARM
```
[ZONE] Zone transition: cancel for edge NORTH (intent dropped)
```
- Player briefly stopped or changed direction, cancelling the pending transition

### 5c. Re-ARM
```
[ZONE] Zone transition: ARM edge NORTH for player 1 (dot=1.00)
```

### 5d. COMMIT event
```
[ZONE] Zone transition: COMMIT edge=NORTH player=1 commit-margin=32.0 time-since-last=5.82s
```
- `time-since-last=5.82s` — 5.82 seconds between transitions

### 5e. Cache state
```
[ZONE] Zone transition: cache MISS for ZONE-1 — synchronous disk load
```
- ZONE-1 not cached even though player just came from there 5.82s ago

### 5f. Zone change direction
```
[ZONE] Zone transition: ZONE-3 -> ZONE-1 edge=NORTH spawn-edge=SOUTH
```

### 5g. Seam translation computation
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
- Old position: `(585.8, 120.1)` — player was 28.9 wu from north wall (120.1 - 91.2)
- Raw translated Y: `4033.7` — above wall maximum (4004.8), therefore out of bounds
- `in-bounds=NIL` — translated position rejected
- `overstep=not-applied`

### 5h. Fallback applied
```
[ZONE] Seam fallback: reason=out-of-bounds overstep=28.88 spawn-edge=SOUTH
  base=(585.8, 4004.8)
  result=(585.8, 3975.9)
```

- Fallback reason: `out-of-bounds`
- Overstep: `28.88` world units (~0.45 tiles)
- Base: `(585.8, 4004.8)` — south wall boundary
- Result: `(585.8, 3975.9)` — base - overstep = 4004.8 - 28.9 = 3975.9

### 5i. Final position
```
[ZONE] Seam final: path=fallback raw=(585.8,3975.9) spawn=(585.8,3975.9) fallback-reason=out-of-bounds
```

- Player placed at `(585.8, 3975.9)` in ZONE-1
- Distance from south wall: 4004.8 - 3975.9 = **28.9 wu** (~0.45 tiles) inward

### 5j. Cooldown
```
[ZONE] Zone transition: cooldown active (1.48s remaining) for player 1
  ... (90 lines) ...
[ZONE] Zone transition: cooldown active (0.00s remaining) for player 1
```

### 5k. Client-side
```
[VERBOSE] Client zone transitioned to ZONE-1
[VERBOSE] Client NPC zone-state synced (zone-id=ZONE-1 npcs=4)
```

---

## 6. Position Data Summary Tables

### Transition 1 (ZONE-1 south -> ZONE-3 north)

| Metric | Value |
|--------|-------|
| Pre-transition position (ZONE-1) | (627.6, 3976.2) |
| Distance from south wall before | 4004.8 - 3976.2 = **28.6 wu** (~0.45 tiles) |
| Raw translated position | (627.6, 62.6) |
| Raw translated in-bounds? | **NIL** (out of bounds — 62.6 < 91.2) |
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
| Raw translated in-bounds? | **NIL** (out of bounds — 4033.7 > 4004.8) |
| Fallback path taken? | **YES** |
| Post-transition position (ZONE-1) | (585.8, 3975.9) |
| Distance from south wall after | 4004.8 - 3975.9 = **28.9 wu** (~0.45 tiles) |
| X preserved? | YES (585.8 -> 585.8) |

---

## 7. Zone Cache Behavior

| Transition | Zone Loaded | Cache Result | Load Type |
|------------|-------------|--------------|-----------|
| #1 ZONE-1 -> ZONE-3 | ZONE-3 | MISS | synchronous disk load |
| #2 ZONE-3 -> ZONE-1 | ZONE-1 | MISS | synchronous disk load |

- `*client-zone-cache-capacity*` = 9, but the **server-side** cache missed both times
- ZONE-1 was not retained in server cache after departure, even though the player returned 5.82s later
- Server `preload-queue=0` throughout (from zone tick lines) — preloading did not happen
- Client preloaded neighboring zones after each transition (loaded 8-9 zones each time)

---

## 8. Cooldown Behavior

- 1.5 second cooldown enforced after each transition
- Cooldown starts at ~1.48s (small processing delay from 1.5s configured)
- ~90 cooldown log lines per transition (one per tick, ~0.017s per tick)
- Total cooldown log lines across both transitions: **180**
- Cooldown fires every tick while the player remains near the edge

---

## 9. Client Log Timeline

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
- NPC zone-state resync occurs after returning to ZONE-1 (which has NPCs)

---

## 10. Code Analysis — Why Seam Translation Fails (Codex)

This section is entirely from Codex's code analysis, providing the *why* behind the logged behavior.

### 10a. Commit triggers before the player crosses the seam

Commit detection uses `world-exit-edge-with-bounds` with a **commit margin** that relaxes the edge check inward.

The commit margin is computed as the **max of**:
- `*zone-commit-margin-tiles* * tile-size` (0.5 * 64 = 32.0), and
- `max(half-w, half-h)` (collision half-size = 27.2)

Effective commit margin = `max(32.0, 27.2)` = **32.0 world units**.

**Relevant code:**
- `src/movement-transition.lisp:199-236` — edge detection uses `commit-margin`
- `src/movement-transition.lisp:712-717` — `commit-margin` computed as `max(...)`
- `src/config.lisp:120-129` — `*zone-commit-margin-tiles*` default = 0.5

**Effect:** The player commits while still **28-32 px inside** the source zone. This prevents the player from ever reaching (or standing on) the actual boundary tile.

### 10b. Seam translation assumes the player is already beyond the edge

Seam translation uses the raw player position and expects it to be past the edge:
- For SOUTH exit: `dst-min-y + (player-y - src-max-y)`
- For NORTH exit: `dst-max-y + (player-y - src-min-y)`

**Relevant code:**
- `src/movement-transition.lisp:403-412` — `seam-translate-position`

If the player is still inside the source bounds (which they always are, per 10a), the translated position lands **outside** the destination bounds, causing `seam-position-valid-p` to reject it (`in-bounds=NIL`).

**Log evidence:** Both transitions show `in-bounds=NIL`, so the seam path is never used.

### 10c. "Overstep" means distance TO the edge, not distance PAST it

`compute-transition-overstep` returns the **distance from the player to the edge**, clamped >= 0.

**Relevant code:**
- `src/movement-transition.lisp:422-435` — `compute-transition-overstep`

Even if the player hasn't crossed the seam, `overstep` is positive. When seam translation fails, the fallback **pushes the player inward** from the destination wall boundary by this overstep distance, producing the visible teleport/snap.

---

## 11. Perceived Pop vs Logged Pop (Codex)

Codex noted a discrepancy between the user's reported experience and the logged data:

- **Logged pop magnitude:** ~28-29 world units = ~0.45 tiles
- **User-reported pop magnitude:** "3-4 tiles"

Codex's note: The logs do **not** show jumps of 3-4 tiles. The ~0.45-tile positional snap is what the server computes. The discrepancy may come from:
- Camera interpolation behavior during the transition
- Frame timing (synchronous disk load causing a hitch during which the player's input continues)
- The combined effect of FBO unload/reload (visual disruption amplifying perceived distance)
- Misperception due to the sudden scene change

If larger pops are happening, they are not present in the provided server log data for the position computation itself.

---

## 12. Findings Comparison: Common and Unique

### Findings in common (both Claude Code and Codex independently identified)

| Finding | Claude Code | Codex |
|---------|-------------|-------|
| Both transitions take the `fallback` path (primary seam translation always fails) | Yes | Yes |
| `in-bounds=NIL` on every seam translation | Yes | Yes |
| Raw translated positions land outside wall bounds (62.6 < 91.2, 4033.7 > 4004.8) | Yes | Yes |
| Commit fires ~28-32 wu before the wall | Yes | Yes |
| `overstep=not-applied` during translation step | Yes | Yes |
| Fallback places player at wall-boundary +/- overstep | Yes | Yes |
| Both zones have identical bounds | Yes | Yes |
| Server cache misses both transitions (synchronous disk load) | Yes | Yes |
| X coordinate is preserved across transitions | Yes | Yes |
| Post-transition distance from wall mirrors pre-transition distance | Yes | Yes |

### Unique to Claude Code (live session data)

| Finding | Detail |
|---------|--------|
| ARM/cancel/re-ARM sequence on transition #2 | Player briefly stopped, arm cancelled, then re-armed with dot=1.00 |
| Hysteresis band config values documented | `*zone-hysteresis-in*`=6.0, `*zone-hysteresis-out*`=8.0 |
| Direction threshold config documented | `*zone-direction-threshold*`=0.3 |
| Cooldown logging volume quantified | 90 lines per transition, 180 total |
| Client FBO unload/reload counts | 9 FBOs first transition, 16 FBOs second |
| Client zone preload lists captured | Full list of which neighbor zones were preloaded each time |
| Client `*verbose-zone-transitions*` gap identified | Client script missing `MMORPG_VERBOSE_ZONES` env var reading |
| Collider half-size values from startup log | 27.20 x 27.20 |
| Server startup sequence (instance ID, Redis, etc.) | Full initialization timeline |
| `time-since-last` values | -1.00s (first), 5.82s (second) |

### Unique to Codex (code analysis)

| Finding | Detail |
|---------|--------|
| Exact commit margin calculation traced in code | `max(*zone-commit-margin-tiles* * tile-size, max(half-w, half-h))` at lines 712-717 |
| Seam translation formula identified | SOUTH: `dst-min-y + (player-y - src-max-y)`, NORTH: `dst-max-y + (player-y - src-min-y)` at lines 403-412 |
| `compute-transition-overstep` semantics explained | Returns distance TO edge (not past it), lines 422-435 |
| Root cause chain articulated | Commit-before-crossing + seam-assumes-past-crossing + overstep-means-distance-to = guaranteed fallback |
| Perceived vs logged pop discrepancy flagged | User reports 3-4 tiles but logs show ~0.45 tiles; possible camera/timing/visual factors |
| Edge detection function identified | `world-exit-edge-with-bounds` at lines 199-236 |
| Validation function identified | `seam-position-valid-p` is what rejects the translation |

---

## 13. Consolidated Observations

These are factual observations drawn from both analyses:

1. **The primary seam translation path never succeeds.** Both transitions produced `in-bounds=NIL` and took the fallback path. The seam translation formula assumes the player has already crossed the zone boundary, but the commit margin triggers while the player is still 28-32 wu inside the source zone. This is a structural mismatch.

2. **The commit margin and the seam formula are misaligned.** The commit margin (32.0 wu) causes transition to fire while the player is inside bounds. The seam formula (`dst-min + (pos - src-max)`) then produces a negative offset in the destination zone, landing outside the playable area. The two systems have contradictory assumptions about where the player is at commit time.

3. **"Overstep" does not mean what its name implies.** `compute-transition-overstep` returns the distance FROM the player TO the edge, not the distance the player has gone PAST the edge. Since the player never crosses the edge, this value is always a positive inward distance, and the fallback uses it to push the player inward from the destination wall.

4. **The fallback preserves distance-from-wall symmetry.** Pre-transition distance from source wall (~28.6-28.9 wu) equals post-transition distance from destination wall (~28.6-28.9 wu). The player ends up at a mirrored position, not a random or grossly wrong position.

5. **Server-side zone cache missed both transitions.** Even returning to ZONE-1 after only 5.82 seconds resulted in a synchronous disk load. Server `preload-queue=0` throughout — no preloading occurred.

6. **Client does preload neighbors.** After each transition, the client loaded 7-9 neighboring zones. However, the server did not benefit from this.

7. **1.5s cooldown is enforced and logs every tick.** This is working as designed but generates significant log volume (180 lines for 2 transitions).

8. **The logged positional snap is ~0.45 tiles, not 3-4 tiles.** The server places the player ~28 wu from the destination wall. The user-perceived "3-4 tile" pop may be amplified by camera behavior, synchronous load hitches, or visual disruption from FBO unload/reload.

9. **Client-side zone diagnostics were not captured** because `scripts/client.lisp` does not read `MMORPG_VERBOSE_ZONES`. This is a tooling gap for future debugging.

10. **ARM -> cancel -> re-ARM sequence works correctly.** When the player briefly stopped during transition #2, the pending transition was cancelled and re-armed when movement resumed. The hysteresis system functions as designed.

---

## 14. Recommendations

### From Codex

Codex's core finding: the visible teleport is **not a rendering problem** — it is an intentional position adjustment caused by:
1. **Commit happening before boundary crossing**, and
2. **Fallback spawn using "distance-to-edge" as overstep**

This guarantees a positional snap equal to the distance from the edge at commit time.

If the requirement is "stand on any visible edge tile with no pop," then **the commit rule and the seam translation expectations are currently misaligned** and need to be reconciled.

### From Claude Code

Observations that feed into planning (no specific code recommendations made — data collection only):

- The `fallback` path is the only path that ever executes. The primary seam translation is dead code in practice for same-size zones with commit margins.
- Server-side zone caching needs investigation — cache misses on a zone the player just left 5.82s ago suggests the cache is not retaining departed zones or has a different eviction policy than expected.
- Client verbose zone diagnostics should be enabled via env var (tooling fix for `scripts/client.lisp`).

---

## 15. Raw Log Files

| File | Size | Contents |
|------|------|----------|
| `./mmorpg_server.log` | 825 KB | ~9395 zone-tick lines, 180 cooldown lines, 2 full transition sequences |
| `./mmorpg_client.log` | ~40 KB | 860 lines including raylib/texture init, 2 zone transitions, FBO lifecycle |
| `./zone_findings.md` | Claude Code raw data report |
| `./codex_zone_findings.md` | Codex analysis report |

---

## ADDENDUM FINAL RECOMMENDATIONS

This section consolidates the best path forward based on **all findings above** and the current codebase behavior. It is intentionally **implementation‑aware** (what to change conceptually), but **not a full plan**.

### 1) Align commit timing with seam translation assumptions (core fix)
Right now commit happens **inside** the source zone (commit margin), while seam translation assumes the player has already **crossed** the boundary. This mismatch guarantees fallback and therefore visible snaps.

**Recommended direction:** Make “commit” correspond to **actual seam crossing**, not “near the edge.” This can be achieved by:
- Using the **attempted post‑movement position** (pre‑collision) to detect crossing, or
- Requiring the player’s **collider** to reach/past the edge (distance ≤ 0), with only a tiny epsilon.

If commit triggers only when the player *actually crosses*, the seam translation result will land **in‑bounds**, and the primary path will finally execute.

### 2) Redefine “overstep” to mean **distance past the edge**, not distance to it
The current `compute-transition-overstep` returns how far the player is **inside** the edge. That value is then used to push inward in the target zone, producing a snap.

**Recommended direction:** Compute overstep as **max(0, past‑edge distance)**, ideally from:
- The post‑movement attempted position (pre‑collision), or
- The delta‑intended movement for that frame.

If the player hasn’t crossed, overstep should be **0**, not a positive inward offset.

### 3) Allow standing on the edge tile without forced transition
Your requirement is explicit: “the player should be able to stand on any tile they can see.”

**Recommended direction:** Treat the edge tile as a valid resting position, and only trigger a transition when the player **attempts to step beyond** it. This implies:
- Commit margin should be **near zero**, not 0.5 tiles.
- Collision should not prevent the player from standing on the edge tile.

This keeps zones invisible to the player while still using them internally.

### 4) Make seam translation the default path (fallback only for true blockers)
Once commit timing is fixed, seam translation should succeed almost always for same‑size zones. The fallback should be reserved for cases where:
- The translated point is blocked (actual wall), or
- The destination bounds differ (non‑symmetric zones).

This matches the architectural goal: **continuous world‑space position** across zone seams.

### 5) Keep server cache warm to remove hitch amplification
Although not the root cause of the “teleport,” synchronous zone loads amplify perceived popping by stalling the frame. Logs show server cache misses on both transitions.

**Recommended direction:** Ensure server preloading is **active and observable**, so transitions do not stall. This does not fix position continuity, but it eliminates the hitch that makes the snap feel worse.

### 6) Add targeted logging to verify the fix quickly
Since this bug is subtle, diagnostics should explicitly show:
- Distance to edge at commit
- Past‑edge overstep used for seam translation
- Whether seam path was used or fallback

This makes it obvious when the fix is working (seam path used, overstep near 0 at edge, no fallback).

### 7) Test the exact requirement
Add tests that enforce the requirement directly:
- Player can occupy the edge tile without transition
- Transition fires only when crossing past edge
- Seam translation produces in‑bounds position for same‑size zones
- Overstep is 0 when not past the edge

These tests prevent regressions and lock in the “no teleport” behavior.

---

**Summary:**
The visible teleport is a direct consequence of **commit happening early** and **overstep meaning distance‑to‑edge**. Fix those two semantics and seam translation will work as designed. Everything else (cache, logging, diagnostics) supports making the fix stable and verifiable.
