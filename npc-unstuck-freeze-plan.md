# Plan: Fix NPC “Frozen Sprite” After Unstuck Teleport (Phase 1 Complete + Phase 2)

**Goal:** Prevent NPC sprites from appearing frozen after a player uses “unstuck,” ensuring the client display stays in sync with server NPC movement.

**Observed symptom**
- After pressing **Unstuck**, player teleports near NPCs.
- Debug collider boxes move (NPC positions update), but NPC sprites appear frozen.

**Hypothesis**
- The **client render state** becomes stale after a teleport (unstuck), likely due to interpolation/prediction buffers spanning the teleport. Even if positions update, animation/render state can appear frozen or out of sync.

---

## Phase 1 (Original Plan) — Status: DONE

### 1) Add a client‑side “teleport resync” detection — DONE
**Where:** `src/net.lisp` (`apply-snapshot` or immediately after it in the client loop)

**What:**
- Detect large player position jumps (teleports) by comparing the latest snapshot position to the previous local player position.
- If jump distance > threshold (e.g., 2–3 tiles), then:
  - Clear interpolation buffer (same as zone transition).
  - Reset prediction state (same as zone transition).
  - Optionally clear any NPC interpolation snapshots to avoid mixed timelines.

**Why:**
- Prevents interpolation from blending pre‑teleport and post‑teleport states (which can freeze sprites or show stale animation).

**Notes:**
- Keep the logic local to the client so no protocol changes are required.

### 2) Force a full resync after an unstuck teleport (server-side safety net) — DONE
**Where:** `src/movement.lisp` (`process-player-unstuck`) + `src/net.lisp`

**What:**
- When the server executes an unstuck teleport, mark the player’s client for **full resync** on the next snapshot:
  - Set `net-client-needs-full-resync` for that player’s client, or
  - Add a lightweight resync event (if you already route events) that the client can act on.

**Why:**
- Ensures the client gets a full snapshot immediately after teleport, not a delta built on stale baseline.

### 3) Ensure NPC render list stays authoritative after resync — DONE
**Where:** `src/rendering.lisp` / `src/save.lisp`

**What:**
- Confirm that the NPC list used for rendering matches the one being updated by snapshot/interpolation.
- If zone‑state NPCs are used on the client, ensure they are refreshed when a full resync occurs (or ensure client render always uses `game-npcs`).

**Why:**
- Prevents a split where one NPC array updates (hitboxes) while another is rendered (sprites).

---

## Phase 1 Implementation Steps — DONE

1. **Client teleport detection** — DONE
   - Add a helper to compute squared distance between old/new player positions.
   - In `apply-snapshot` (or right after it), compare previous player position to the new snapshot.
   - If distance exceeds threshold, call a new helper like `reset-client-sync-state` that:
     - Clears interpolation buffer
     - Resets prediction state
     - Optionally clears NPC interpolation snapshots

2. **Server resync flag on unstuck** — DONE
   - In `process-player-unstuck`, after teleport:
     - Set a flag so the client receives a **full snapshot** next tick.
   - In `broadcast-snapshots-with-delta`, ensure resync flag forces a full snapshot for that client.

3. **NPC render list alignment** — DONE
   - Verify client render path uses the authoritative NPC array updated by snapshots/interpolation.
   - If zone‑state NPCs are used on client, ensure they’re updated on resync.

---

## Phase 1 Tests — DONE (note: tests were planned; execution status unknown)

**Unit tests (if possible):**
- Add a pure helper test for teleport detection (distance threshold).

**Manual verification:**
1. Trigger **Unstuck** near NPCs → NPC sprites move normally.
2. Debug collider boxes and sprites should match positions.
3. After unstuck, NPCs should not appear frozen.

---

## Phase 1 Risk & Notes — DONE

---

## Phase 2 (New) — Fix Client NPC Array Mismatch After Teleport/Resync

**Goal:** Ensure client rendering uses the same NPC array that snapshots update, so sprites, hitboxes, and minimap always match after unstuck/teleport.

**Why Phase 1 didn’t fully fix it**
- Client rendering uses `zone-state-npcs` (via spatial grid) when available.
- Snapshots update `game-npcs`.
- `zone-state-npcs` can remain stale after a teleport/resync, so sprites freeze while hitboxes/minimap move.

### Approach A (Preferred): Re-sync zone-state NPCs on client after resync/teleport
**Where:** `src/net.lisp` (client path), `src/spatial.lisp`

**What to do**
1. Add a small helper that refreshes the client’s zone-state NPCs from `game-npcs`:
   - `(setf (zone-state-npcs zone-state) (game-npcs game))`
   - `(populate-npc-grid zone-state (game-npcs game))`
   - Rebuilds index map + spatial grid so rendering and culling are accurate.
2. Call that helper:
   - After **full resync** (when `net-client-needs-full-resync` triggered).
   - After **same-zone teleport** detection (unstuck).
   - After **zone change** (client-side `apply-zone-to-world` path).

**Pros**
- Keeps spatial culling performance.
- Guarantees sprite/hitbox/minimap alignment.

**Cons**
- Slight CPU cost after resync/teleport (one-time grid rebuild).

### Approach B (Fallback): Client render bypasses zone-state NPCs
**Where:** `src/rendering.lisp`

**What to do**
- If `game-net-role` is `:client`, skip spatial grid path and draw from `game-npcs` directly.

**Pros**
- Fast to implement.
- Guarantees correctness.

**Cons**
- O(N) NPC loop per frame on client (may be OK for now).

---

## Phase 2 Concrete Steps (Approach A) — Detailed Implementation Plan

1. **Add helper in `src/net.lisp` (client-only context)**
   - New function: `sync-client-zone-npcs` (name flexible).
   - Inputs: `game` (optional explicit `zone-id` if needed).
   - Resolve zone-id using:
     - `(player-zone-id (game-player game))` if available, else
     - `(zone-id (world-zone (game-world game)))` if available.
   - Resolve zone-state with `(get-zone-state zone-id)`.
   - If zone-state + npc vector exist:
     - `(setf (zone-state-npcs zone-state) (game-npcs game))`
     - `(populate-npc-grid zone-state (game-npcs game))`
   - Add `log-verbose` guarded by `*verbose*` to confirm sync.

2. **Trigger helper after full resync (client snapshot path)**
   - In `apply-snapshot` (client), after `apply-game-state` returns
     and after `game-npcs` has been updated by snapshot.
   - If full snapshot was applied, call `sync-client-zone-npcs`.
   - Ensure this runs only on client (if `game-net-role` is `:client`).

3. **Trigger helper after same-zone teleport (unstuck)**
   - In `apply-snapshot`, immediately after `reset-client-sync-state`
     is called on teleport detection.
   - This ensures zone-state grid matches the new post-teleport NPC array.

4. **Trigger helper after zone change**
   - In `apply-game-state`, after `apply-zone-to-world` creates new NPCs
     and `game-npcs` is replaced for the new zone.
   - Call `sync-client-zone-npcs` before returning from `apply-game-state`
     when `zone-loaded` is true.

5. **Consistency guard (optional but recommended)**
   - If `zone-state-npcs` length ≠ `game-npcs` length, force resync helper.
   - Useful to catch any future divergence.

6. **Logging**
   - `log-verbose "Client NPC zone-state synced (zone-id=~a npcs=~d)"`
   - Helps confirm when the sync is happening during teleport and resync.

---

## Phase 2 Tests

**Unit tests (if feasible):**
- Test helper logic with a mocked zone-state + npc array.

**Manual verification (required):**
1. Trigger **Unstuck** near NPCs → sprites move normally.
2. Debug collider overlay and minimap dots match sprite positions.
3. Repeat after zone transition and after reconnect.

---

## Phase 2 Cleanup

- If Approach A works, leave spatial grid rendering in place.
- If Approach B is needed temporarily, add TODO note to revert to Approach A once stable.
- Low risk: changes are localized to client sync/reset and a resync flag on server.
- This approach avoids heavy bandwidth (only resyncs when needed) and prevents interpolation artifacts on teleports.
