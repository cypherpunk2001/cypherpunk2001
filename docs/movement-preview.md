# movement-preview.lisp

Purpose
- Camera edge detection, neighbor zone preview logic, and view bounds computation for seamless zone boundary rendering.

Status
- **Implemented.** This doc reflects `src/movement-preview.lisp` as of 2026-02-03.

Key responsibilities
- `world-preview-edge`: determine which zone boundary edge to preview based on player proximity.
- `camera-view-center`: returns camera focus point (editor camera, camera leash target, or player).
- `camera-view-bounds`: compute viewport bounds in world coordinates using camera leash when enabled.
- Edge proximity calculation with configurable threshold (`*minimap-preview-edge-tiles*`).
- Best‑edge selection when near multiple zone boundaries simultaneously.
- World graph integration to determine valid neighbor zones.
- **Diagonal preview zones:** `world-diagonal-zone-id` and `ensure-preview-zone-for-corner`.
- **LRU cache integration:** preview loads consult the zone cache before disk IO.
- Preview cache cleanup when edges are no longer visible.

Load order
- Loaded third among movement files: `movement-core` -> `movement-collision` -> `movement-preview` -> `movement-transition`.
- Depends on `movement-core` for zone state and world graph access; used by rendering for preview zone display.
