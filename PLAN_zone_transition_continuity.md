# PLAN: Zone Transition Continuity (No Teleport Pop)

## Goal
Eliminate visible “teleport” jumps (3–4 tiles) when crossing zone boundaries by preserving continuous world‑space position across the seam. This plan is focused on **position continuity** (not the broader seamless loading system).

## Problem Summary
Current transitions compute a spawn edge + axis ratio, which can reposition the player several tiles inside the destination zone. Overstep preservation helps, but if source/destination collision bounds differ, or commit fires before the visual edge, the player still “pops.”

## Requirements
1. **Continuous position:** The player’s world‑space position should be preserved across the boundary whenever possible.
2. **No visible jump:** Crossing should feel like walking across, not teleporting forward.
3. **No clamping:** If the translated position is out-of-bounds or blocked, fall back to ratio-spawn (clamping would hide multi-tile pops).

## Plan

### Step 1: Translate world‑space position across seam
- **Where:** `src/movement.lisp` in `transition-zone`
- **What:** Replace `edge-spawn-position-bounds` + ratio as the primary path.
- **How (implemented):**
  - Use **collision bounds** (not full pixel spans) for translation. Collision bounds
    (`get-zone-collision-bounds`) represent the walkable area inset by the player's
    half-width from the wall edges. Since commit fires at collision bounds, the
    translation must map between them directly.
  - `seam-translate-position` takes src and dst collision bounds and maps the player's
    overstep distance from the source edge to the destination opposite edge:
    - East: `new-x = dst-min-x + (old-x - src-max-x)`, `new-y = old-y`
    - West: `new-x = dst-max-x + (old-x - src-min-x)`, `new-y = old-y`
    - North: `new-x = old-x`, `new-y = dst-max-y + (old-y - src-min-y)`
    - South: `new-x = old-x`, `new-y = dst-min-y + (old-y - src-max-y)`
  - This preserves the overstep distance (pixels past the commit boundary) exactly,
    producing positions within the destination's walkable area without clamping.

### Step 2: Validate and fall back if needed
- **Where:** `transition-zone` after translation
- **What:** If the translated position is out-of-bounds or blocked:
  - **Do not clamp** — clamping would hide multi-tile jumps and produce the same pop
    the plan aims to eliminate. Instead, fall back immediately to ratio-spawn + overstep.
  - Out-of-bounds and blocked are distinguished in logs for diagnostics.
  - `find-open-position-with-map` / `world-open-position-for` resolves any remaining
    collision after either path.

### Step 3: Make ratio‑spawn a fallback only
- **Where:** `transition-zone`
- **What:** Use existing `edge-spawn-position-bounds` (ratio‑based) only if translation fails or if destination collision is invalid.

### Step 4: Tests
- **Unit tests:**
  - Crossing east/west/north/south preserves world‑space continuity using collision bounds.
  - Translation maps overstep distance from source edge to destination edge exactly.
  - Mixed bounds (different-sized zones) preserve overstep distance.
  - If translated position is blocked, fallback path is used (ratio-spawn + overstep + open-position).

## Acceptance Criteria
- Crossing a zone boundary does not pop the player forward by multiple tiles.
- Player position continuity preserved across the seam in normal cases.
- Tests pass (`make test-unit`, then `make tests`).
- Verbose logs provide enough detail to diagnose edge cases without re-instrumenting.

## Verbose Logging Requirements (for all new code in this plan + relevant existing code)
- All transition diagnostics use `log-zone` (gated by `*verbose-zone-transitions*`).
  Enable with `MMORPG_VERBOSE_ZONES=1`.
- When `MMORPG_VERBOSE_ZONES=1` (and optionally `MMORPG_VERBOSE_COORDS=1`), log:
  - **Commit decision**: edge, commit margin, and time since last transition.
  - **Existing transition path**: zone-id → target zone-id, edge, and spawn-edge (via `log-zone`).
  - **Translation result**: old-pos, src/dst bounds, translated pos, in-bounds, blocked, path used.
  - **Fallback reason**: out-of-bounds vs blocked; overstep value; spawn-edge; base and result positions.
  - **Final spawn**: path used (seam/fallback), raw position, resolved spawn, fallback-reason if any.
  - **Overstep**: logged as `overstep=not-applied` on seam path; computed value on fallback path.
  - **Soft reset**: delta, threshold value, and whether buffers were cleared or preserved.
  - **Urgent preload**: when the queue flushes, queue depth before/after.

## Manual Validation Step (required success step)
1) Run server + client with verbose modes enabled so logs are visible:
   - `MMORPG_VERBOSE_ZONES=1 make server`
   - `MMORPG_VERBOSE_ZONES=1 make client` (add `MMORPG_VERBOSE_COORDS=1` if needed)
2) Pause and ask the user to log in and cross multiple zone boundaries.
3) Observe stdout logs and correlate with the user’s reported feel (push, pop, hitch).
4) Only mark the work complete after user confirms the transition is seamless.

---

## SPEC — Zone Transition Continuity (Detailed)

### Coordinate model
- **World space**: Player positions (`player-x`, `player-y`) are in pixels relative to the current zone's origin.
- **Collision bounds**: `get-zone-collision-bounds` returns the walkable area inset from zone
  edges by `tile-size + collision-half-width`. Commit fires when the player reaches these bounds.
- **Tile size**: `tile-dest-size = (world-tile-dest-size world)` (pixels per tile).

### Seam translation (primary path)
`seam-translate-position` maps the player's overstep distance (pixels past the source
collision edge) to the destination's opposite collision edge:
- **East**: `new-x = dst-min-x + (old-x - src-max-x)`, `new-y = old-y`
- **West**: `new-x = dst-max-x + (old-x - src-min-x)`, `new-y = old-y`
- **North**: `new-x = old-x`, `new-y = dst-max-y + (old-y - src-min-y)`
- **South**: `new-x = old-x`, `new-y = dst-min-y + (old-y - src-max-y)`

This preserves the exact overstep distance across the seam, producing positions within
the destination's walkable area without clamping.

### Validity checks
- **Bounds**: `seam-position-valid-p` checks if the translated position is within
  destination collision bounds. Falls back to world bounds if collision bounds unavailable.
- **Blocked**: If translated position is blocked (wall/collision via `blocked-at-p-with-map`),
  fall back immediately — do not clamp (see below).

### Fallback logic (only when translation invalid)
- If translated position is out-of-bounds or blocked, do **not** clamp (clamping hides
  multi-tile pops). Instead fall back immediately to ratio-spawn + overstep.
- Compute edge‑spawn position using existing ratio/offset logic (`edge-spawn-position-bounds`).
- Apply overstep preservation (if present) to spawn edge.
- Run `find-open-position-with-map` / `world-open-position-for` to resolve collisions.
- **Order**: Translation → bounds check → blocked check → fallback spawn → open‑position.

### Overstep interaction
- Overstep is **secondary**. If translation is valid, do **not** apply overstep.
- Overstep only applies in fallback path to minimize positional discontinuity.

### Commit timing
- Commit should occur when player visually reaches the seam.
- Commit margin should only relax "push into wall," not trigger early teleport.

### Example (numeric)
Assume:
- 10×10 tile zone, tile-dest-size = 64 px
- Collision half-width = 16 px
- Collision bounds: min-x = 80, max-x = 560 (for both src and dst)

If crossing east with `old-x = 562`, `old-y = 300`:
- Overstep = 562 − 560 = 2 px
- `new-x = 80 + 2 = 82`, `new-y = 300`
The player appears 2 px inside the destination's walkable area — seamless.

### Testing matrix
1) **Direct translation**: East/West/North/South preserves overstep distance via collision bounds.
2) **Blocked fallback**: Translation lands on wall → fallback used; result inside bounds.
3) **Mixed bounds**: Different-sized zones preserve overstep distance unless blocked.
4) **Edge case**: Player crosses at corner (both axes near edge) → correct seam translation.

### Success criteria (SPEC)
- Max visible position delta at boundary ≤ 0.25 tile in the normal case.
- No forced "push" into wall to trigger transition.
- No multi‑tile pop forward on transition.
