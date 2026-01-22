# Entity Culling vs Zoom (Analysis)
Date: 2026-01-22
Scope: `src/rendering.lisp`, `src/config-client.lisp`, `src/main.lisp`, `src/save.lisp`

## Summary
Zoomed-out play increases the visible world area, and the current culling logic scales with zoom correctly (no correctness bug for zoom itself). The main risk is **performance** (draw loops scale with visible area). Zoom config is `*camera-zoom-min* = 0.5`, `*camera-zoom-default* = 1.0`, `*camera-zoom-max* = 3.0`, so max zoom-out shows ~4x the area (area scales as 1/zoom^2). I verified fullscreen behavior on this build: toggling fullscreen **does not change** `raylib:get-screen-width/height` (stays 1280×720), so culling bounds based on `*window-width*/*window-height*` remain accurate in fullscreen for current settings.

## Findings

### 1) Culling math is zoom-aware (correctness OK)
- **What happens:** View bounds are computed as `half-view = window-size / (2 * zoom)`. When zoom decreases (zoom out), bounds expand proportionally.
- **Where:**
  - `entity-in-viewport-p` uses zoom for bounds (`src/rendering.lisp:25-40`).
  - Tile culling in `draw-world` uses the same zoom-derived bounds (`src/rendering.lisp:366-376`).
  - Object culling in `draw-zone-objects` also uses zoom-derived bounds (`src/rendering.lisp:546-575`).
- **Impact:** No culling errors from zoom; visible area grows correctly.

### 1a) Edge margin prevents pop-in
- **What happens:** Entity culling expands bounds by sprite half-size margins, reducing edge pop-in at any zoom level.
- **Where:** `entity-in-viewport-p` subtracts/adds `margin-x/margin-y` to view bounds.
- **Impact:** Smooth edge behavior when zooming and moving.

### 2) Zoomed-out view increases draw workload (performance risk)
- **What happens:** Visible area scales as `1/zoom^2`. At max zoom-out, you draw many more tiles, objects, and entities per frame.
- **Where:**
  - Tile loops iterate `start-row..end-row` and `start-col..end-col` based solely on view size (`src/rendering.lisp:372-420`).
  - No chunk-level or distance-based LOD culling beyond viewport bounds.
- **Impact:** Likely fine for modest zoom, but max zoom-out can spike CPU and draw calls in dense zones.

### 2a) Server snapshots are zone-wide (zoom does not affect network load)
- **What happens:** The server sends all entities in the player's zone, independent of client zoom.
- **Where:** Zone-filtered snapshots include all zone players/NPCs (`src/save.lisp` serialize-game-state-for-zone).
- **Impact:** Zooming out increases client-side render work but not network payload size. This is good for instant zoom changes.

### 2b) Minimap uses radius culling; main view does not
- **What happens:** Minimap caps NPCs by radius; main viewport uses only rectangle bounds.
- **Impact:** Main view can render many more entities at max zoom-out compared to minimap.

### 3) Fullscreen verified: no culling mismatch at current settings
- **Verification (local):** A raylib toggle test showed `get-screen-width/height` stays `1280×720` before/after fullscreen.
- **Impact:** No fullscreen-driven culling errors in the current setup.
- **Future risk:** If you enable resizable windows or change render size on fullscreen, switch to runtime screen dimensions for culling bounds.

## Answer to the Question
- **Zoom-out itself:** culling behaves correctly; bounds expand with zoom as intended.
- **Main risk:** performance, because we draw more tiles/entities per frame at max zoom-out.
- **Fullscreen:** verified no culling mismatch with current fullscreen toggle; logical render size stays 1280×720.

## Suggested Improvements (best player experience)
- **Performance safeguards for max zoom-out:**
  - Cache static tile layers into render textures (per chunk) and only redraw dirty chunks.
  - Add chunk-level culling so large zoom-out doesn’t iterate every tile per layer.
  - Keep entity draw culling but consider a spatial index for render lists if NPC/player counts grow.
  - Optional: add a distance-based pre-filter for entity rendering in very large zones.
- **Future-proof culling bounds:**
  - If you add window resizing or change fullscreen render size, swap `*window-width*/*window-height*` usage for `raylib:get-screen-width/height` in culling bounds, and update camera offset accordingly.
