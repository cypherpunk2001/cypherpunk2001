# Phase 3 Performance: Choppy Movement Analysis (Unified)

**Date:** 2026-01-24
**Commit:** be6bea5 (Phase 3 performance: serialization and networking improvements)
**Issue:** Players report choppy/hitchy movement after Phase 3 changes
**Status:** FIXED — client prediction now enabled by default

---

## Executive Summary

Phase 3 is working as designed for throughput (bandwidth + server CPU), but the
client feel regressed because **snapshot rate dropped to 20 Hz** while **client
prediction was OFF by default** and **local player is not interpolated**.

Result: the local player (and camera/ground) updated only every 50 ms, which
felt like hitching even if interpolation for remote entities was fine.

**Fix applied:** Client prediction is now **enabled by default** (`*client-prediction-enabled* = t`).
The misleading ESC menu checkbox has been removed.

---

## What Changed in Phase 3 (Relevant to Feel)

- **Snapshot rate decoupled** from 60 Hz sim to **20 Hz** by default
  (`*snapshot-rate-hz*`, `*snapshot-interval*`).
- **Binary snapshot encoding** and fragmentation/reassembly optimizations.
- **Interpolation buffer remains small (capacity 4)** and is **remote-only**.
- **Client prediction is disabled by default** (`*client-prediction-enabled*`).

---

## Two Separate Systems (Do Not Confuse)

| System | Purpose | Affects | Default |
|--------|---------|---------|---------|
| **Client Prediction** | Instant feedback for **local** player movement | Local player only | **ON** (was OFF) |
| **Interpolation** | Smooth movement between snapshots | Remote players/NPCs only | ON |

**Key point:** The reported “ground hitching beneath you” is most consistent
with **local player stepping**, not remote interpolation.

---

## Root Cause Analysis

### Local Player (Primary Hitch)

- Local player position is driven by server snapshots.
- With snapshots at **20 Hz**, local position updates in **50 ms steps**.
- The camera follows the local player → visible “ground hitching”.
- Client prediction exists but is **disabled by default**.

**This is expected behavior** given the current defaults.

### Remote Entities (Secondary Risk)

Remote interpolation is generally OK at 20 Hz, but can hitch if:
- the **buffer is small (4 snapshots)** and
- **render-time falls outside buffered bounds**, causing clamping.

This is a possible contributor for other players/NPCs, but it does **not**
explain the local ground hitching.

---

## Code Facts (Current Configuration)

```lisp
;; Server snapshot rate (src/config-server.lisp)
*snapshot-rate-hz*   = 20
*snapshot-interval*  = 0.05

;; Client defaults (src/config-client.lisp)
*interpolation-delay-seconds* = 0.1
*client-prediction-enabled*   = t    ; CHANGED: was nil, now t (smooth local movement)
*prediction-error-threshold*  = 5.0  ; pixels before snap to server

;; Interpolation buffer (src/types.lisp + src/net.lisp)
capacity default = 4 snapshots
```

---

## Options to Restore Smoothness (Without Reverting Phase 3)

### Option A — Enable Client Prediction (Best Feel, Low Cost)
- Turn on prediction for local player.
- Keeps 20 Hz snapshots but restores 60 fps input feel.
- Minimal bandwidth impact.

**Change:** `*client-prediction-enabled*` default to `t`
(`src/config-client.lisp`) or add env toggle.

### Option B — Raise Snapshot Rate (Quick Toggle)
- `MMORPG_SNAPSHOT_RATE=30` or `60`.
- Smoother feel without client-side changes.
- Tradeoff: more bandwidth/CPU.

### Option C — Local Interpolation / Camera Smoothing
- Interpolate the local player (or camera) using the same buffer.
- Adds a small input latency (~`*interpolation-delay-seconds*`).

### Option D — Increase Interpolation Buffer (Remote Smoothness)
- Increase buffer capacity (e.g., 8) and/or delay.
- Helps remote entities when snapshots jitter or drop.
- Does **not** fix local hitch unless local interpolation is added.

### Option E — Extrapolation for Sparse Gaps (Medium Effort)
- Use velocity to predict forward when interpolation buffer underruns.
- Useful for remote entities under packet loss.

---

## Recommended Path (Minimal Risk)

1. **Enable client prediction by default** (local smoothness).
   - This is the cheapest/lowest‑risk way to keep 20 Hz snapshots without
     compromising Phase 3 optimizations.
   - Because prediction state is initialized at client startup, the ESC menu
     checkbox is misleading (it requires a hard client restart to take effect).
     Recommendation: **remove the checkbox** to avoid false toggles.


2. Optionally **raise snapshot rate to 30 Hz** if bandwidth allows. (DEFER)
3. If remote entities still hitch under load, **increase buffer to 8** and (DEFER)
   slightly bump interpolation delay (e.g., 0.12–0.15 s).

---

## Debug/Isolation Checks

- If decode failures appear in logs, temporarily disable binary snapshots:
  `MMORPG_BINARY_SNAPSHOTS=0`.
- Verify that snapshots are arriving at the expected rate (20/30/60 Hz).

---

## Testing Checklist

1. **Local movement feel** with prediction OFF vs ON.
2. **Remote movement feel** at 20 Hz vs 30 Hz.
3. **Packet loss simulation** (if available) to validate interpolation buffer.
4. Confirm no new misprediction spikes (ESC overlay).

---

## Notes on Expected Behavior

~~With Phase 3 defaults (20 Hz snapshots + prediction OFF), local hitching is
expected.~~ **FIXED:** With prediction now ON by default, local movement is smooth
at 60 fps. The client applies movement immediately and reconciles with server
snapshots, preserving Phase 3 bandwidth savings while restoring smooth feel.
