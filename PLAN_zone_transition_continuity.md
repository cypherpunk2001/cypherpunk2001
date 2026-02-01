# PLAN: Zone Transition Continuity (No Teleport Pop)

## Goal
Eliminate visible “teleport” jumps (3–4 tiles) when crossing zone boundaries by preserving continuous world‑space position across the seam. This plan is focused on **position continuity** (not the broader seamless loading system).

## Problem Summary
Current transitions compute a spawn edge + axis ratio, which can reposition the player several tiles inside the destination zone. Overstep preservation helps, but if source/destination collision bounds differ, or commit fires before the visual edge, the player still “pops.”

## Requirements
1. **Continuous position:** The player’s world‑space position should be preserved across the boundary whenever possible.
2. **No visible jump:** Crossing should feel like walking across, not teleporting forward.
3. **Minimal clamping:** Only clamp if the translated position is blocked/out of bounds.

## Plan

### Step 1: Translate world‑space position across seam
- **Where:** `src/movement.lisp` in `transition-zone`
- **What:** Replace `edge-spawn-position-bounds` + ratio as the primary path.
- **How (conceptual):**
  - Determine the zone span in pixels (`zone-width * tile-dest-size`, `zone-height * tile-dest-size`).
  - When crossing:
    - East: `new-x = old-x - span-x`, `new-y = old-y`
    - West: `new-x = old-x + span-x`, `new-y = old-y`
    - North: `new-x = old-x`, `new-y = old-y + span-y`
    - South: `new-x = old-x`, `new-y = old-y - span-y`
  - This keeps continuity across the seam.

### Step 2: Clamp only if needed
- **Where:** `transition-zone` after translation
- **What:** If the translated position is blocked or outside the destination bounds, then:
  - Clamp to nearest valid position (minimal correction).
  - Apply existing overstep or open‑position logic as fallback.

### Step 3: Make ratio‑spawn a fallback only
- **Where:** `transition-zone`
- **What:** Use existing `edge-spawn-position-bounds` (ratio‑based) only if translation fails or if destination collision is invalid.

### Step 4: Tests
- **Unit tests:**
  - Crossing east/west/north/south preserves world‑space continuity.
  - Translation uses zone span exactly (no extra offset).
  - If translated position is blocked, fallback path is used (clamp/open).

## Acceptance Criteria
- Crossing a zone boundary does not pop the player forward by multiple tiles.
- Player position continuity preserved across the seam in normal cases.
- Tests pass (`make test-unit`, then `make tests`).
- Verbose logs provide enough detail to diagnose edge cases without re-instrumenting.

## Verbose Logging Requirements (for all new code in this plan + relevant existing code)
- **Important:** `log-verbose` only prints when `*verbose*` is enabled. Until/unless we add a
  zone-specific logger, **run with BOTH** `MMORPG_VERBOSE=1` and `MMORPG_VERBOSE_ZONES=1`
  to see zone logs.
- When `MMORPG_VERBOSE_ZONES=1` (and optionally `MMORPG_VERBOSE_COORDS=1`), log:
  - **Commit decision**: edge, commit margin, and whether translation or fallback path was used.
  - **Translation result**: old‑pos → translated pos; whether it was in bounds/blocked.
  - **Fallback reason**: out‑of‑bounds vs blocked; final spawn after open‑position.
  - **Overstep**: computed overstep and whether it was applied.
  - **Soft reset**: delta, threshold, and whether buffers were cleared or preserved.
  - **Urgent preload**: when the queue flushes, and queue depth before/after.
  - **Existing transition path (relevant)**: log current zone‑id → target zone‑id, edge, and spawn‑edge.

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
- **World space**: Player positions (`player-x`, `player-y`) are in pixels relative to the current zone’s origin.
- **Zone span**:
  - `span-x = zone-width * tile-dest-size`
  - `span-y = zone-height * tile-dest-size`
- **Tile size**: `tile-dest-size = (world-tile-dest-size world)` (pixels per tile).

### Seam translation (primary path)
When crossing from source zone to destination zone:
- **East**: `new-x = old-x - span-x`, `new-y = old-y`
- **West**: `new-x = old-x + span-x`, `new-y = old-y`
- **North**: `new-x = old-x`, `new-y = old-y + span-y`
- **South**: `new-x = old-x`, `new-y = old-y - span-y`

This preserves continuous world‑space motion across the seam.

### Validity checks
- **Bounds**: Use destination zone collision bounds (`get-zone-collision-bounds`) when available; otherwise fall back to world bounds.
- **Blocked**: If translated position is blocked (wall/collision), apply fallback logic (see below).

### Fallback logic (only when translation invalid)
- Compute edge‑spawn position using existing ratio/offset logic (`edge-spawn-position-bounds`).
- Apply overstep preservation (if present) to spawn edge.
- Run `find-open-position-with-map` / `world-open-position-for` to resolve collisions.
- **Order**: Translation → bounds check → fallback spawn → open‑position.

### Overstep interaction
- Overstep is **secondary**. If translation is valid, do **not** apply overstep.
- Overstep only applies in fallback path to minimize positional discontinuity.

### Commit timing
- Commit should occur when player visually reaches the seam.
- Commit margin should only relax “push into wall,” not trigger early teleport.

### Example (numeric)
Assume:
- Zone width/height = 64 tiles
- tile-dest-size = 64 px
- span-x = span-y = 4096 px

If crossing east with `old-x = 4100`, `old-y = 900`:
- `new-x = 4100 - 4096 = 4`, `new-y = 900`
The player appears 4 px into the destination zone — seamless.

### Testing matrix
1) **Direct translation**: East/West/North/South preserves continuity (no extra offset).
2) **Blocked fallback**: Translation lands on wall → fallback used; result inside bounds.
3) **Mixed bounds**: Different zone collision bounds still preserve continuity unless blocked.
4) **Edge case**: Player crosses at corner (both axes near edge) → correct seam translation.

### Success criteria (SPEC)
- Max visible position delta at boundary ≤ 0.25 tile in the normal case.
- No forced “push” into wall to trigger transition.
- No multi‑tile pop forward on transition.
