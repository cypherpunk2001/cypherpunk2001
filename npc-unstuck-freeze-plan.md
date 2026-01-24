# Plan: Fix NPC “Frozen Sprite” After Unstuck Teleport

**Goal:** Prevent NPC sprites from appearing frozen after a player uses “unstuck,” ensuring the client display stays in sync with server NPC movement.

**Observed symptom**
- After pressing **Unstuck**, player teleports near NPCs.
- Debug collider boxes move (NPC positions update), but NPC sprites appear frozen.

**Hypothesis**
- The **client render state** becomes stale after a teleport (unstuck), likely due to interpolation/prediction buffers spanning the teleport. Even if positions update, animation/render state can appear frozen or out of sync.

---

## Approach (recommended)

### 1) Add a client‑side “teleport resync” detection
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

### 2) Force a full resync after an unstuck teleport (server-side safety net)
**Where:** `src/movement.lisp` (`process-player-unstuck`) + `src/net.lisp`

**What:**
- When the server executes an unstuck teleport, mark the player’s client for **full resync** on the next snapshot:
  - Set `net-client-needs-full-resync` for that player’s client, or
  - Add a lightweight resync event (if you already route events) that the client can act on.

**Why:**
- Ensures the client gets a full snapshot immediately after teleport, not a delta built on stale baseline.

### 3) Ensure NPC render list stays authoritative after resync
**Where:** `src/rendering.lisp` / `src/save.lisp`

**What:**
- Confirm that the NPC list used for rendering matches the one being updated by snapshot/interpolation.
- If zone‑state NPCs are used on the client, ensure they are refreshed when a full resync occurs (or ensure client render always uses `game-npcs`).

**Why:**
- Prevents a split where one NPC array updates (hitboxes) while another is rendered (sprites).

---

## Concrete Implementation Steps

1. **Client teleport detection**
   - Add a helper to compute squared distance between old/new player positions.
   - In `apply-snapshot` (or right after it), compare previous player position to the new snapshot.
   - If distance exceeds threshold, call a new helper like `reset-client-sync-state` that:
     - Clears interpolation buffer
     - Resets prediction state
     - Optionally clears NPC interpolation snapshots

2. **Server resync flag on unstuck**
   - In `process-player-unstuck`, after teleport:
     - Set a flag so the client receives a **full snapshot** next tick.
   - In `broadcast-snapshots-with-delta`, ensure resync flag forces a full snapshot for that client.

3. **NPC render list alignment**
   - Verify client render path uses the authoritative NPC array updated by snapshots/interpolation.
   - If zone‑state NPCs are used on client, ensure they’re updated on resync.

---

## Tests

**Unit tests (if possible):**
- Add a pure helper test for teleport detection (distance threshold).

**Manual verification:**
1. Trigger **Unstuck** near NPCs → NPC sprites move normally.
2. Debug collider boxes and sprites should match positions.
3. After unstuck, NPCs should not appear frozen.

---

## Risk & Notes
- Low risk: changes are localized to client sync/reset and a resync flag on server.
- This approach avoids heavy bandwidth (only resyncs when needed) and prevents interpolation artifacts on teleports.
