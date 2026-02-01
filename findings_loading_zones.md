# Zone Loading Analysis & Recommendations

## The Problem

Zone transitions trigger a hard "loading..." screen every time a player crosses a zone boundary. With 64x64 tile zones (1024px at 16px/tile), a running player hits a zone edge roughly every 10 seconds. At the intersection of 4 zones, a player running in a small circle can trigger 4 transitions in a few seconds — each one a jarring interruption.

```
      |
      |
  load|ing...
  ----+----
  load|ing...
      |
      |
```

This is primarily a **UX pacing issue** rather than a pure performance issue. The zone system is architecturally correct for scalability (one server process per zone, spatial partitioning, NPC scoping), but zone boundaries are perceived as hard walls rather than soft seams. The goal is to preserve zone-based scalability while reducing visible load churn and making boundaries feel invisible.

## Current Architecture

**What happens during a zone transition today:**

| Phase | Cost | Where |
|-------|------|-------|
| Edge detection | ~0.5ms | Server tick |
| Zone file I/O + parse | 100-200ms | Server (`load-zone` with retry) |
| Zone-state creation (grids, collision) | ~10ms | Server |
| Tier-1 DB save | 100-500ms | Server (immediate, with retry) |
| Snapshot broadcast | ~0.5ms | Server |
| Network round-trip | Variable | Wire |
| Zone file I/O + parse (client) | 100-200ms | Client |
| UI "loading..." trigger | ~1ms | Client |
| Buffer reset (interpolation, prediction) | ~1ms | Client |
| NPC sync + grid rebuild | ~5ms | Client |

**Total wall-clock: ~300-500ms local, worse over network.**

Key observations:
- Zones are cached in `*zone-states*` on the server, so repeat visits skip the load.
- The client does *not* cache — it reloads from disk every time.
- The Tier-1 DB save (100-500ms worst case) blocks the transition.
- Interpolation and prediction buffers are wiped, causing a visual "snap."
- The "loading..." UI is unconditional — there's no concept of a lightweight transition.

## Options

### Option A: Seamless Zone Streaming (Overlap Zones)

**Idea:** Load a border region of adjacent zones so the player is never aware of the boundary. The client always has the current zone + a strip of neighboring zones loaded. Transitions happen invisibly when the player's canonical position crosses the midpoint.

**How it works:**
1. When a player is within N tiles of a zone edge, the client preloads the adjacent zone (already partially implemented via `ensure-preview-zones` for the minimap).
2. Render tiles from both zones in the overlap region.
3. The server still transitions the player's authoritative zone-id at the boundary, but the client doesn't show "loading..." because the target zone is already in memory.
4. Buffer resets become partial — keep interpolation data for entities that exist in both the old and new snapshots.

**Pros:**
- Completely eliminates the loading screen for normal play.
- The 4-zone intersection problem vanishes — all 4 zones are preloaded.
- Server architecture (one process per zone) is unchanged.

**Cons:**
- Client memory usage increases (~4x worst case at a 4-zone corner).
- Rendering must composite tiles from multiple zone structs in the overlap strip.
- NPC visibility across zone boundaries needs careful handling (show NPCs from adjacent zone in the strip? Or pop them in at the boundary?).
- Server must send snapshot data for entities in adjacent zones' border strips, or accept a pop-in radius.

**Complexity:** Medium-high. The zone preloading infrastructure partially exists. Main work is: client-side multi-zone rendering, smarter buffer resets, and a preload trigger based on player proximity to edges.

### Option B: Hysteresis + Cooldown ("Sticky Zone")

**Idea:** Add a buffer band on both sides of boundaries and a post-transition cooldown so the player must move *deeper* into the adjacent zone before triggering a transition, and rapid oscillation is suppressed.

**How it works:**
1. Define asymmetric thresholds: `transition-threshold-in` (e.g., 8-16 tiles into the new zone before handoff fires) and `transition-threshold-out` (e.g., 4-8 tiles back before re-triggering the original zone). The asymmetry means it's harder to "bounce" than to commit.
2. Player enters band → start preloading target zone in background.
3. Player reaches far side of band → execute transition.
4. Player turns around before reaching far side → cancel preload, no transition.
5. After a transition completes, impose a cooldown (e.g., 1-3 seconds) or minimum travel distance before another transition can fire. Player can move freely during cooldown — it's not a hard block, just a suppression of the handoff trigger.

**Pros:**
- Simple to implement — modify `world-exit-edge-with-bounds` thresholds + add a timestamp/distance check.
- Eliminates the "running in circles at intersection" problem.
- Cooldown alone would cut the worst case from 4 transitions in 3 seconds to 1-2.
- Pairs well with background preloading — the band gives you time to load.
- Hysteresis removes thrashing without increasing zone size.

**Cons:**
- Doesn't eliminate the loading screen, just reduces frequency.
- The band width must be tuned — too wide feels like invisible walls, too narrow doesn't help.
- At a 4-zone intersection, the bands overlap in complex ways.
- Players who genuinely want to cross quickly (running through a zone) still get a loading screen.
- Needs careful UX — the cooldown should be subtle, not feel like a restriction.

**Complexity:** Low. Mostly config changes and a small state machine for "in-band" tracking.

### Option C: Bigger Zones with Sub-Zone Streaming

**Idea:** Make logical zones much larger (e.g., 256x256 or 512x512 tiles) but divide them into chunks that stream in/out. The server still partitions by zone for scalability, but players cross zone boundaries far less often.

**How it works:**
1. Increase zone dimensions 4-8x (256x256 = 16x current area).
2. Only load/render chunks near the player (e.g., 5x5 chunk window = 40x40 tiles visible).
3. Zone transitions still exist but happen every ~80+ seconds of running instead of ~10.
4. Combine with Option A or B for the rare transitions that do happen.

**Pros:**
- Directly attacks the "zones are too small" problem.
- Chunk streaming is a well-understood technique (Minecraft, etc.).
- Fewer transitions = fewer loading screens even without any other fix.
- Server-side zone partitioning still works — just with bigger zones.

**Cons:**
- Requires reworking zone data format and all spatial queries.
- Collision maps, NPC grids, and spatial partitioning all need to work with chunks, not monolithic zone arrays.
- Zone file sizes grow significantly — 256x256 = 65k tiles per layer vs 4k today.
- Doesn't eliminate transitions, just makes them rare.

**Complexity:** High. Touches zone loading, rendering, collision, spatial grids, NPC management, and the data pipeline.

### Option D: Client-Side Zone Caching + Async Preloading

**Idea:** Keep current zone sizes and boundaries, but eliminate the *cost* of transitions by caching loaded zones on the client and preloading neighbors asynchronously.

**How it works:**
1. Client maintains an LRU cache of N most recent zone structs (e.g., 8-16 zones).
2. When a player enters a zone, immediately queue preloading of all adjacent zones (from world-graph edges).
3. Zone transition checks the cache first — if the target zone is cached, skip I/O entirely.
4. The "loading..." screen only appears on a cache miss (first visit to a zone, or cache eviction).
5. Buffer resets become instant (no I/O wait), so the transition can be a brief fade instead of a blocking load.

**Pros:**
- Minimal architectural change — zones, server, world-graph all stay the same.
- The 4-zone intersection problem is solved because all 4 zones are pre-cached after the first visit.
- Memory cost is bounded by cache size.
- Can be combined with Option B (hysteresis) for even smoother feel.

**Cons:**
- First visit to a zone still has a loading screen (cold cache).
- Preloading adds background I/O — need to ensure it doesn't cause frame hitches.
- Doesn't help with the visual "snap" from buffer resets (interpolation/prediction wipe).
- Cache invalidation if zone data changes (editor, server-side object state).

**Complexity:** Low-medium. Main work: LRU cache struct, preload trigger on zone entry, cache lookup in `apply-game-state`.

### Option E: Portal/Threshold Transitions (Design-Level Fix)

**Idea:** Instead of invisible boundaries at zone edges, use explicit transition points (doors, caves, bridges, paths) that the player interacts with intentionally. The loading screen becomes a natural part of the world.

**How it works:**
1. Zone edges are walled off (cliffs, water, dense forest).
2. Transitions happen at specific objects: a cave entrance, a bridge, a gate.
3. Player presses interact → fade to black → load zone → fade in.
4. The loading time is masked by the fade animation.

**Pros:**
- Loading screens feel intentional, not jarring.
- No accidental transitions from running near edges.
- The 4-zone intersection problem is eliminated by design.
- Well-established pattern (Dark Souls, Zelda, classic MMOs like EverQuest).

**Cons:**
- Fundamentally changes world feel — no seamless open world.
- Requires art/design work for every transition point.
- Doesn't scale to a huge open world without feeling like a series of rooms.
- Players expect modern MMOs to be seamless.

**Complexity:** Low (code), high (content/design).

### Option F: Directional Gate Volumes (One-Way Triggers)

**Idea:** Instead of transitioning whenever any part of the player crosses the boundary pixel, require that the player's movement direction is toward the new zone AND they cross a defined gate volume. Running parallel to a boundary or circling an intersection won't trigger transitions.

**How it works:**
1. At each zone edge, define gate volumes — rectangular trigger regions with a required crossing direction.
2. Transition fires only when: (a) player overlaps the gate volume, AND (b) player's movement intent vector has a significant component toward the new zone (e.g., dot product with edge normal > threshold).
3. Running tangent to a boundary or circling a 4-way intersection at an angle won't spam transitions.

**Pros:**
- Specifically targets the 4-zone intersection circle problem.
- No cooldown or hysteresis needed — the direction check alone prevents most accidental triggers.
- Can be implemented as a data-only change if gates are defined in zone data files.
- Works well as a complement to hysteresis.

**Cons:**
- Requires explicit gate placement in zone data (more authoring).
- Edge cases: what if a player approaches at exactly 45 degrees at a corner?
- Doesn't reduce the cost of transitions that *do* fire.

**Complexity:** Low-medium. Gate volumes in zone data + direction check in `world-exit-edge-with-bounds`.

### Option G: Intersection Buffer Zones ("Plaza" / Neutral Hub)

**Idea:** Replace problematic 4-way zone intersections with a small shared buffer zone that loads once and serves as a low-load transition region. Instead of 4 zones meeting at a point, they meet at a small plaza.

**How it works:**
1. At each 4-way intersection in the world graph, insert a small zone (e.g., 16x16 or 32x32 tiles).
2. This buffer zone connects to all 4 surrounding zones.
3. Player crosses from Zone A → Buffer → Zone B. The buffer is small and loads fast.
4. Because the buffer zone is tiny, it can stay permanently cached.
5. The buffer zone can be themed as a crossroads, plaza, bridge, or clearing.

**Pros:**
- Directly solves the worst-case topology (4-way intersection thrashing).
- Buffer zones load once and stay cached — subsequent transitions through them are instant.
- Adds world-building opportunities (crossroads towns, waystations).
- No changes to zone loading code — just world-graph topology.

**Cons:**
- Adds extra zones to the world graph (potentially many for a 10x10 grid).
- Requires content creation for each buffer zone.
- Increases total transition count for straight-line travel (A → Buffer → B instead of A → B).
- Only helps at intersections, not at simple 2-zone edges.

**Complexity:** Low (code — just world-graph changes), medium (content — need to author buffer zones).

### Option H: Client-Side Crossfade & Deferred Handoff

**Idea:** Visually allow the player to move into the next zone immediately on the client, rendering tiles from the adjacent zone, while deferring the actual server-side handoff until the player has committed to the crossing. The transition is masked by a crossfade or seamless overlay.

**How it works:**
1. When the player approaches a zone edge, the client begins rendering tiles from the adjacent zone (already partially supported by `ensure-preview-zones`).
2. The player visually crosses into the new zone with no loading screen — the client composites both zones.
3. The server-side handoff (zone-id change, DB save, NPC swap) happens once the player is N tiles into the new zone.
4. If the player reverses before the handoff threshold, no server transition occurs — the client just stops rendering the adjacent zone.
5. During the deferred window, the server still sends snapshots for the *old* zone. The client shows the player in the new zone visually but reconciles position if the server rejects the transition.

**Pros:**
- Player never sees a loading screen — movement is visually continuous.
- Server handoff is batched and intentional, not triggered on every pixel-crossing.
- Pairs naturally with hysteresis — the deferred window IS the hysteresis band, but with visual continuity.

**Cons:**
- Requires reconciliation logic if the server handoff fails or the player reverses after visual crossing.
- Client must render two zones simultaneously in the overlap region.
- Entity visibility is complex: NPCs/players in the adjacent zone aren't in the current snapshot, so the border region may feel "empty" until handoff completes.
- Most complex option — touches rendering, prediction, and server handoff timing.

**Complexity:** Medium-high. Overlaps significantly with Option A (seamless streaming) but focuses on decoupling the visual transition from the server handoff rather than full multi-zone rendering.

## How to Evaluate Improvements

Whichever options are implemented, measure these metrics to validate:

- **Time between zone transitions**: Target >60 seconds during normal running gameplay.
- **Loads per 5 minutes**: Target <2 visible loading screens for normal roaming.
- **Transition thrash rate**: Count transitions within a 5-second sliding window. Target: 0-1 (currently can spike to 4+).
- **Seamless transition ratio**: Percentage of transitions that show no "loading..." message vs. those that do. Target: >90% after Phase 1.
- **Transition latency (wall-clock)**: Time from edge detection to gameplay resuming. Target: <50ms for cached zones, <300ms for cold.

## Recommendation

**Phase 1 — Immediate relief (Options B + D + F combined):**

Implement hysteresis, client-side caching, and directional gating together. This is the highest impact for the lowest cost:

1. **Transition cooldown**: After any zone transition, suppress further transitions for 1.5 seconds. This alone kills the "4 zones in 3 seconds" problem.
2. **Asymmetric hysteresis band**: Require the player to move 8-16 tiles past the edge before transitioning in; require 4-8 tiles back before re-triggering the original zone. If they reverse within the band, cancel. The asymmetry makes "bouncing" harder than committing.
3. **Directional gating**: Only trigger transitions when the player's movement intent has a significant component toward the new zone. Running parallel to a boundary or circling an intersection tangentially won't fire transitions.
4. **Client zone cache**: LRU cache of 9 zones (current + 8 neighbors). On entering a zone, preload all adjacent zones in the background.
5. **Fast-path transition**: When target zone is cached, skip I/O, skip the "loading..." message, and just do a quick buffer reset with a subtle fade.
6. **Make the Tier-1 DB save async for transitions**: The 100-500ms save blocks the server tick. Move it to a background flush (it's already durable via dirty-flag batching — the immediate save on *every* transition is excessive; reserve Tier-1 immediate for death/trade/level-up).

Expected result: After the first visit to a region, zone transitions become near-instant (<50ms) with no visible loading screen. The cooldown + hysteresis + directional gating together prevent rapid re-triggering even at 4-zone intersections.

**Phase 2 — Seamless streaming (Options A + H, partial):**

Once Phase 1 is stable, add visual overlap and deferred handoff:

1. Render a strip of adjacent zone tiles at zone borders (the preview cache already loads them).
2. Allow the player to visually cross into the next zone before the server handoff fires — the hysteresis band becomes a visual overlap region rather than an invisible buffer.
3. Fade NPCs in/out at zone boundaries instead of popping.
4. Maintain interpolation buffers across transitions for entities that persist (the player themselves, carried NPCs).
5. Add reconciliation logic for the rare case where the server rejects or delays a transition that the client already rendered.

This makes transitions truly invisible for normal gameplay.

**Phase 2.5 — Intersection buffer zones (Option G, selective):**

If playtesting reveals that specific 4-way intersections are still problematic after Phase 2 (e.g., high-traffic crossroads), insert small buffer zones at those intersections. These are data-only world-graph changes and can be added incrementally at problem spots without touching code.

**Phase 3 — Larger zones (Option C, if needed):**

If the world design demands wide-open spaces without any transition feel, increase zone sizes to 128x128 or 256x256 and add chunk-based streaming. This is a bigger refactor and should only be done if Phase 1+2 still feel insufficient after playtesting.

**Skip Option E** unless the game's art direction specifically calls for it. An open-world MMO should feel seamless.

## Summary

| Option | Effort | Impact | Recommended |
|--------|--------|--------|-------------|
| A: Seamless streaming | Medium-high | Eliminates loading entirely | Phase 2 |
| B: Hysteresis + cooldown | Low | Cuts transition frequency 60-80% | Phase 1 |
| C: Bigger zones + chunks | High | Reduces transitions to rare events | Phase 3 (if needed) |
| D: Client cache + preload | Low-medium | Makes transitions instant (warm cache) | Phase 1 |
| E: Portal transitions | Low code, high design | Hides loading behind intention | Skip (wrong vibe) |
| F: Directional gates | Low-medium | Prevents tangential/circle triggers | Phase 1 |
| G: Intersection buffer zones | Low code, medium content | Solves 4-way intersection worst case | Phase 2.5 (selective) |
| H: Crossfade + deferred handoff | Medium-high | Visual continuity during handoff | Phase 2 |

The sweet spot is **B+D+F first** (immediate thrash elimination + cached fast transitions), **then A+H** (visual seamlessness), **then G selectively** at problem intersections. Phase 3 (bigger zones) only if the world design outgrows the current tile budget after all other options are in place.
