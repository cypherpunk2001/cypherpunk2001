# Codex Findings — Zone Boundary “Teleport” Pops

## Scope
Analysis based on:
- `mmorpg_client.log`
- `mmorpg_server.log`
- Current code in `src/movement-transition.lisp` + `src/config.lisp`

Goal described by user: eliminate visible position pops when crossing zones; player should be able to stand on any edge tile without being teleported inside the adjacent zone.

## Key Log Evidence (Server)
Only two transitions are logged in the provided server log, both showing the same pattern:

1) ZONE-1 → ZONE-3 (SOUTH)
```
[ZONE] Zone transition: COMMIT edge=SOUTH player=1 commit-margin=32.0
[ZONE] Seam translation: ... old=(627.6,3976.2) src-bounds=(91.2,4004.8,91.2,4004.8) -> trans=(627.6,62.6) in-bounds=NIL ...
[ZONE] Seam fallback: reason=out-of-bounds overstep=28.61 spawn-edge=NORTH base=(627.6,91.2) result=(627.6,119.8)
```

2) ZONE-3 → ZONE-1 (NORTH)
```
[ZONE] Zone transition: COMMIT edge=NORTH player=1 commit-margin=32.0
[ZONE] Seam translation: ... old=(585.8,120.1) src-bounds=(91.2,4004.8,91.2,4004.8) -> trans=(585.8,4033.7) in-bounds=NIL ...
[ZONE] Seam fallback: reason=out-of-bounds overstep=28.88 spawn-edge=SOUTH base=(585.8,4004.8) result=(585.8,3975.9)
```

Observations from the logs:
- Both transitions **commit while the player is still inside the source zone**, not beyond the seam.
- The seam translation path **never succeeds** (`in-bounds=NIL`), so the code always falls back to “ratio spawn + overstep”.
- The fallback applies an “overstep” of ~28–29 px, which becomes the visible snap into the target zone.

## Code Analysis (Why the seam translation fails)

### 1) Commit triggers **before** the player crosses the seam
- Commit detection uses `world-exit-edge-with-bounds` with a **commit margin** that relaxes the edge check inward.
- The commit margin is computed as the **max of**:
  - `*zone-commit-margin-tiles* * tile-size` (0.5 tiles), and
  - `max(half-w, half-h)` (collision half-size).

Relevant code:
- `src/movement-transition.lisp:199-236` — edge detection uses `commit-margin`
- `src/movement-transition.lisp:712-717` — `commit-margin` computed as `max(...)`
- `src/config.lisp:120-129` — `*zone-commit-margin-tiles*` default = 0.5

Effect:
- With tile size 64 and half-width/height 32, the effective commit margin is **32 px**.
- The player **commits while still 28–32 px inside** the source zone.
- This prevents the player from ever reaching (or standing on) the actual boundary tile.

### 2) Seam translation assumes the player is already **beyond** the edge
- Seam translation uses the raw player position and expects it to be past the edge:
  - For SOUTH: `dst-min-y + (player-y - src-max-y)`
  - For NORTH: `dst-max-y + (player-y - src-min-y)`

Relevant code:
- `src/movement-transition.lisp:403-412` — `seam-translate-position`

If the player is still inside the source bounds, the translated position is **outside** the destination bounds, which makes `seam-position-valid-p` reject it.

Log evidence shows this explicitly: `in-bounds=NIL`, so the seam path is never used.

### 3) “Overstep” currently means **distance to the edge**, not distance past it
- `compute-transition-overstep` returns the **distance from the player to the edge**, clamped ≥ 0.
- That means even if the player hasn’t crossed the seam, `overstep` is positive.

Relevant code:
- `src/movement-transition.lisp:422-435` — `compute-transition-overstep`

So when seam translation fails, fallback **pushes the player inward** by the distance they were from the edge, resulting in a visible teleport.

## What the logs imply vs. user observation
- The logs show a pop of ~28–29 px. With a 64 px tile size, that is **~0.45 tiles**.
- The user perceives “3–4 tiles.” The logs do **not** show jumps that large.
  - This discrepancy might come from camera interpolation behavior, frame timing, or misperception.
  - If larger pops are happening, they are not present in the provided log segment.

## Secondary Finding (Not the root of the pop, but visible)
- Both transitions show:
  - `cache MISS for ZONE-* — synchronous disk load`
  - `preload-queue=0`

This indicates **preloading did not happen**, so the server synchronously loads the zone during transition. That can cause a hitch, but it does **not** explain the positional “teleport” (which is already explained by the commit + overstep behavior).

## Bottom Line
The visible teleport is **not** a rendering problem — it is an intentional position adjustment caused by:
1) **Commit happening before boundary crossing**, and
2) **Fallback spawn using “distance-to-edge” as overstep**.

This guarantees a positional snap equal to the distance from the edge at commit time.

If the requirement is “stand on any visible edge tile with no pop,” then the commit rule and the seam translation expectations are currently **misaligned**.

