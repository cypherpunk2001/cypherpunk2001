# Adjacent Zone Preloading — Design Analysis

**Date:** 2026-02-02
**Status:** Proposal for review

---

## The Idea

Pre-load all zones adjacent to the player's current zone (cardinal + diagonal = up to 8
neighbors) so that any zone crossing is guaranteed to hit a warm cache. When the player
crosses from zone A to zone B, immediately unload zones that are no longer adjacent to B
and begin loading zones newly adjacent to B. The player never waits for a disk read during
a transition.

### Example: Player in Zone 1

From the world map:

```txt
  5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20
 21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36
 37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52
 53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68
 69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84
 85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100
101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116
117 118 119 120 121 122 123  [1]  [2] 124 125 126 127 128 129 130
131 132 133 134 135 136 137  [3]  [4] 138 139 140 141 142 143 144
145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160
```

Zone 1 sits in the center of the starting 2x2 block. Its neighbors from `world-graph.lisp`:

| Direction | Neighbor |
|-----------|----------|
| North     | 108      |
| South     | 3        |
| East      | 2        |
| West      | 123      |
| NE (N→E)  | 109      |
| NW (N→W)  | 107      |
| SE (S→E)  | 4        |
| SW (S→W)  | 137      |

**Preloaded set while in zone 1:** `{1, 2, 3, 4, 107, 108, 109, 123, 137}` = 9 zones.

### After Crossing East to Zone 2

Zone 2's neighbors:

| Direction | Neighbor |
|-----------|----------|
| North     | 109      |
| South     | 4        |
| East      | 124      |
| West      | 1        |
| NE (N→E)  | 110      |
| NW (N→W)  | 108      |
| SE (S→E)  | 138      |
| SW (S→W)  | 3        |

**New preloaded set:** `{1, 2, 3, 4, 108, 109, 110, 124, 138}` = 9 zones.

**Diff:**
- Unload: `{107, 123, 137}` (no longer adjacent to zone 2)
- Load: `{110, 124, 138}` (newly adjacent to zone 2)

The player never waits because zone 2 was already cached from zone 1's adjacency set.

---

## What Already Exists

The codebase already implements most of this idea. Here is what's in place:

### LRU Zone Cache (`src/types.lisp:416`)
- Capacity: 9 (configured via `*client-zone-cache-capacity*`)
- Already sized for 1 current + 8 neighbors
- Hash-table backed, O(1) lookup, LRU eviction

### Cold-Start Preload (`src/main.lisp:575`)
- `cold-start-preload-adjacent` queues all cardinal + diagonal neighbors on initial load
- Walks the world-graph to find diagonal neighbors (cardinal → perpendicular)
- Already does exactly what the proposal describes — on first zone load

### Proximity Preload (`src/main.lisp:603`)
- `update-client-preloading` runs every frame
- When player is within 10 tiles of an edge, queues that edge's target zone
- Also queues diagonal neighbors via perpendicular exits
- Processes 1 zone per frame from the queue (spreads I/O load)

### Urgent Preload (`src/main.lisp:473`)
- When player is within 2 tiles of commit line, flushes entire queue in one frame
- Emergency measure to guarantee cache is warm before commit

### Client Cache Gate (`src/movement-transition.lisp:843`)
- Recently added: client defers commit if target zone not in LRU cache
- Keeps pending set, preloader continues warming cache
- Server always commits immediately (no rendering hitch concern)

### Preview System (`src/movement-preview.lisp:175`)
- Loads adjacent zones for camera rendering (edge strips, minimap)
- Separate from preload queue but uses same LRU cache

---

## Where the Hitch Actually Comes From

Given all the above, the preloading system should prevent cache misses. The remaining
hitch is **not from disk I/O** — it's from **synchronous reconstruction work** that
happens during `transition-zone` even when the zone is already cached:

### `apply-zone-to-world` (client-only, `src/movement.lisp:88`)
Called synchronously during transition. Does:
1. Rebuild wall-map bounds (fast — just math)
2. `build-zone-paths` — **rescans the zone directory on disk** (`resolve-zone-path`)
3. `build-adjacent-minimap-spawns` — iterates all adjacent zones for minimap data
4. `build-minimap-collisions` — rebuilds minimap collision overlay

### Render Cache Clear (`*client-zone-change-hook*`)
- Invalidates all cached chunk render textures
- Next frame must re-render every visible chunk from scratch

### NPC Reconstruction (`src/movement-transition.lisp:735`)
- `make-npcs` allocates and initializes NPC array if not cached
- Spatial grid creation + population

### The Actual Cost Breakdown (estimated)
| Step | Time | Cached? |
|------|------|---------|
| Zone struct from LRU | ~0 ms | Yes |
| `apply-zone-to-world` | 1-5 ms | No — always runs |
| `build-zone-paths` (disk scan) | 2-10 ms | No — always runs |
| `build-minimap-*` | 1-3 ms | No — always runs |
| Render cache invalidation | 0 ms (but next frame is expensive) | N/A |
| NPC spawn/grid setup | 0-2 ms | Partially (NPC cache) |
| **Total** | **~5-20 ms** | |

At 60 FPS, a frame is 16.67 ms. A 10-20 ms spike means a dropped frame or two — visible
as a "hitch" even though no disk I/O occurred.

---

## Critique of the Proposal

### What's Right

1. **The adjacency model is correct.** A 16x16 grid of zones with 4 cardinal exits each
   means every zone has exactly 8 spatial neighbors (minus edges/corners). Preloading all
   8 guarantees any single-step transition hits cache.

2. **The unload-on-transition idea is sound.** LRU eviction already does this implicitly
   (capacity=9 means old zones fall off as new ones are inserted), but explicit unloading
   would be more predictable and free memory sooner.

3. **The cache capacity is already right.** `*client-zone-cache-capacity*` = 9 = 1 current
   + 8 neighbors. This was designed for exactly this adjacency ring.

### What's Already Implemented

Most of the proposal is already in the code:
- Cold-start preload queues all 8 neighbors ✓
- Proximity preload re-queues as player moves ✓
- Urgent preload flushes queue near commit ✓
- Cache gate defers commit if not cached ✓
- LRU eviction naturally unloads distant zones ✓

### What's Missing (the actual gap)

The hitch isn't from cache misses — it's from **per-transition reconstruction work** that
runs synchronously even on cache hits. Preloading more aggressively won't help because the
zone struct is already in memory. The expensive part is what happens after the lookup.

---

## Proposed Improvements (Building on the Existing System)

### Tier 1: Pre-compute reconstruction data (High Impact)

Cache the results of `apply-zone-to-world`-style work alongside the zone struct in the
LRU cache. When a zone is preloaded, also pre-build:
- Wall-map bounds (already just math — cheap)
- Minimap spawns and collisions for that zone
- NPC array and spatial grids

Store these in `zone-state` so `transition-zone` can skip reconstruction entirely. The
transition becomes: swap pointers, update player position, done.

**Key insight:** `build-zone-paths` rescans the disk directory every transition. This is
unnecessary — zone paths are static and already stored in the world-graph. Remove the
rescan and use the graph's `zone-paths` table directly.

### Tier 2: Explicit adjacency-driven preload (Medium Impact)

Replace the proximity-based preload trigger with a simpler model:

**On entering any zone:**
1. Compute the 8-neighbor set from the world-graph
2. Diff against current cache contents
3. Queue all missing neighbors (not 1-per-frame — batch them)
4. Mark eviction candidates (zones in cache but not in neighbor set)

This is cleaner than the current per-frame distance checks and guarantees all neighbors
are queued immediately on transition, not gradually as the player approaches edges.

The current system queues neighbors lazily (only when player is within 10 tiles of an
edge). If a player teleports or spawns in a new zone, there's a window where neighbors
aren't queued yet. Explicit adjacency preload closes this gap.

### Tier 3: Background reconstruction thread (Low Priority)

Move `apply-zone-to-world` work to a background thread. The preloader already loads zone
structs asynchronously (1 per frame). Extend this to also run reconstruction work in the
background, storing results in a "ready" flag on the zone-state. Transition checks the
ready flag before committing.

This is more complex and may not be needed if Tier 1 eliminates the reconstruction cost.

---

## Codex Addendum — Why Crossing Still Feels “Blocked” (Not a Preload Problem)

I don’t think the stubborn “invisible wall / must double‑click / must hold keys” symptoms
are caused by preload. Preload removes disk I/O hitches, but the **block‑and‑delay** feels
like logic that **never commits a transition when the player is on the boundary**. That
points to movement/transition gating, not cache misses.

### Likely Root Causes (Behavioral)

1. **Commit detection relies on “attempted” position past the edge.**  
   If movement is clamped or blocked, the attempted position may never exceed the bound,
   so `world-crossing-edge` never fires. This creates the “push against wall” feeling.

2. **Target clears before crossing (click‑to‑move).**  
   If a clamped click target is “reached” within one step, it gets cleared, removing
   intent direction before the transition can commit. This looks like: player walks to
   the edge, stops, never crosses — unless you re‑click.

3. **Directional gating cancels pending.**  
   The pending edge is cancelled if `player-intent-direction` drops below threshold.
   That can happen when the target is cleared or when the move vector is reset to zero
   even though the player is still pressing into the edge.

4. **Cooldown masks repeated attempts.**  
   If a partial attempt sets pending and then cancels, the cooldown can prevent a new
   commit for up to 1.5s — perceived as “stuck against a wall.”

5. **Collision bounds vs. zone ring mismatch.**  
   If the bounds used for movement/collision and the bounds used for transition commit
   diverge (tile ring vs. collision bounds), the player can be “inside” for movement but
   “outside” for transition (or vice‑versa), causing inconsistent edge behavior.

### Why This Matters For Preload

Preload only guarantees the destination zone is *ready*. It doesn’t guarantee the logic
will decide to cross. So when the player can’t walk across, it’s almost always a **commit
condition or intent state issue**, not a missing zone in cache.

### Practical Fix Direction (Aligned With Current System)

These are the fixes that address the “wall” feeling directly:

- **Ensure attempted position crosses the bound even when collision blocks movement.**  
  For keyboard input, force attempted to “step past” the edge when blocked near the arm
  band. For clamped click targets, preserve raw target and use it to drive attempted.

- **Never clear a clamped crossing target on arrival.**  
  Keep the target active until the transition commits. Clearing it too early drops
  direction and cancels pending.

- **Reduce or conditionalize cooldown.**  
  Only apply cooldown after a successful transition, not after a cancel/failed attempt.

- **Log intent + attempted + pending edge in verbose mode.**  
  This will reveal whether the transition logic is failing because of intent direction,
  attempted position, or cancel line.

### Summary

If preloading is solid (and it mostly is), the remaining “blocked crossing” problem is
almost certainly in **movement → intent → transition** logic, not in loading. Preload
fixes hitches; it doesn’t fix “the player can’t cross.” Fixing attempted position and
target persistence will.
