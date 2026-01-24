# NPC Movement Regression Analysis

**Symptoms reported:**
1. NPCs don't move when player logs in - frozen on spawn tiles
2. NPCs only start moving when player is very close (right underneath them)
3. Different clients show different NPC movement states for same NPCs
4. After zone transition and return, NPCs are frozen again
5. NPCs can still attack/hit player but don't visually chase

---

## Finding 1: NPC AI is gated on a *nearby* player (3x3 grid only)

**Location:** `src/ai.lisp:14-33` (`closest-player`), `src/main.lisp:385-395` (`simulate-zone-npcs`)

**What's happening:**

The Phase 2 performance optimization changed `closest-player` to only scan the 3x3 grid cells around the NPC via `spatial-grid-query-neighbors-into`. If no players are in those neighboring cells:
- `closest-player` returns `nil`
- `simulate-zone-npcs` skips the entire NPC update when `target-player` is nil
- `update-npc-intent` is gated on non-nil player: `(when (and (npc-alive npc) player) ...)`
- NPC never executes `update-npc-behavior`, `update-npc-intent`, or `update-npc-movement`

**This explains:**
- NPCs frozen until player is *very close* (within spatial grid neighbor range)
- Walking right underneath them "wakes" them up
- Different clients can see different NPC movement depending on which NPCs have a nearby player
- Leaving the zone freezes them again (no players in neighbor cells)
- "Hitting me but not walking up" - when in attack range, NPC attacks in place with `dx/dy = 0`

**Code evidence:**
```lisp
;; closest-player uses spatial-grid neighbors only, no fallback
(spatial-grid-query-neighbors-into grid (npc-grid-cell-x npc) (npc-grid-cell-y npc)
                                   *spatial-scratch-vector*)
;; If results empty, returns nil - NPC AI never runs
```

---

## Finding 2: Zone player cache can suppress AI entirely

**Location:** `src/main.lisp:385-386`, `src/main.lisp:534-541`, `src/spatial.lisp:488-504`

**What's happening:**

- `simulate-zone-npcs` does nothing when `zone-players` is empty
- If zone-state is created after the player joins, the cache can be empty until rebuilt
- Cache rebuild only triggers when `length = 0` AND `zone-state-player-count > 0`
- If the player never makes it into `zone-state-zone-players`, `closest-player` won’t see them

**This explains:**
- "Walking under them didn't trigger movement" in some cases
- Player not in spatial grid = NPCs stay frozen regardless of proximity

---

## Root Cause Summary

| Issue | Location | Symptom |
|-------|----------|---------|
| Spatial grid 3x3 limit | `closest-player` (ai.lisp) | NPCs only move when player very close |
| Zone player cache empty | `simulate-zone-npcs` (main.lisp) | AI skipped if player not in cache |

---

## Likely Regression Source

The spatial-grid "Phase 2 perf" optimization in `closest-player` changed the meaning from "closest in the zone" to "closest in 3x3 neighboring cells." This removed the fallback to a full scan, so NPC AI now only runs in the immediate vicinity of a player.

Combined with the AI gating on a non-nil player, NPCs are completely frozen unless a player is within those neighboring cells.

---

## Suggested Fixes

1. **Expand AI range:** In `closest-player`, either:
   - Increase neighbor search radius beyond 3x3, OR
   - Add fallback to zone-wide scan when neighbor query returns nil, OR
   - Run idle/wander behavior even without a target player

2. **Harden zone-player cache:** Ensure `zone-state-zone-players` is populated whenever a zone has players (e.g., rebuild when empty and players exist, and/or force a rebuild when zone-state is created after login).

3. **Ensure player in grid:** Make sure `ensure-player-in-grid` runs for players in that zone *even when the cache is empty* (otherwise the cache never self-heals).
