Zone Consolidation Fallback Plan (data-only)
===========================================

Context
-------
We currently have a 1024x1024-tile world split into 256 zones (16x16),
with each zone 64x64 tiles. If seamless transitions remain unstable,
a fallback is to reduce zone count while preserving total tiles.

Current Facts
-------------
- Zones: 256 total (16x16 grid)
- Zone size: 64x64 tiles
- World size: 1024x1024 tiles (same total tiles across all plans)

Goal
----
Reduce transition frequency without exploding per-zone load or risking
unbounded hotspots for 1000 concurrent players.

Recommended Middle-Ground Option
--------------------------------
Option A: 16 zones (4x4 grid)
- Zone size: 256x256 tiles
- Area per zone: 16x current
- Transitions reduced 16x
- Risk: higher per-zone player concentration; snapshot sizes and AI/collision
  costs increase but still reasonable vs 4-zone extreme.

Safer Alternative
-----------------
Option B: 64 zones (8x8 grid)
- Zone size: 128x128 tiles
- Area per zone: 4x current
- Transitions reduced 4x
- Lower per-zone density risk; smoother path if 16 feels too risky.

Why Not 9 or 25 Zones?
----------------------
- 9 zones implies ~341.33x341.33 tiles per zone (not integer);
  would require padding or changing world size.
- 25 zones implies ~204.8x204.8 tiles per zone (same issue).

Decision Table (world size fixed at 1024x1024)
-----------------------------------------------
- 256 zones (16x16): 64x64 per zone (current)
- 64 zones  (8x8): 128x128 per zone
- 16 zones  (4x4): 256x256 per zone
- 4 zones   (2x2): 512x512 per zone (high risk)

Data-Only Work Plan (no engine code changes)
--------------------------------------------
1) Pick target grid size (recommended: 16 zones, fallback: 64 zones).
2) Define new zone ID mapping from old 16x16 grid to new grid cells.
3) Merge old zone tile layers into each new mega-zone (tile data, collisions).
4) Merge objects/spawns into the new zones; update coordinates to mega-zone
   local space.
5) Update data/world-graph.lisp to reflect the new grid and exits.
6) Verify zone dims: :width and :height match the new size.
7) Smoke-test transitions and minimap alignment.

Risks / Performance Notes
-------------------------
- Larger zones increase per-zone player count and snapshot size.
- Collision and NPC AI cost scales with population, not tile count.
- 16-zone option should be manageable for 1000 players if population is
  spread out; worst-case hotspots will need monitoring.

Next Actions (when ready)
-------------------------
- Confirm preferred target grid (16 or 64 zones).
- I can draft a zone-merging script or a detailed manual merge checklist.
