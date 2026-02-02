# movement-preview.lisp

Purpose
- Camera edge detection, neighbor zone preview logic, and view bounds computation for seamless zone boundary rendering.

Key responsibilities
- `world-preview-edge`: determine which zone boundary edge to preview based on player proximity to zone edges.
- `camera-view-center`: return camera focus point (player position or editor camera position).
- `camera-view-bounds`: compute viewport bounds in world coordinates for culling and preview decisions.
- Edge proximity calculation with configurable threshold (`*minimap-preview-edge-tiles*`).
- Best-edge selection when player is near multiple zone boundaries simultaneously.
- World graph integration to check which edges have valid neighbor zones.

Load order
- Loaded third among movement files: `movement-core` -> `movement-collision` -> `movement-preview` -> `movement-transition`.
- Depends on `movement-core` for zone state and world graph access; used by rendering for preview zone display.
