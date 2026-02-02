Camera Leash Experiment Plan
============================

Idea
----
Give the player a "camera leash" so they can move within a small radius
(~8x8 tile circular region) before the camera begins to follow. This
adds a subtle pull effect: the player can roam near center without the
camera constantly shifting.

Goal
----
- Reduce camera jitter for small movements
- Keep camera motion smooth and intentional
- Avoid diagonal awkwardness by using a circular (radius-based) leash

Design Parameters
-----------------
- Leash shape: circle centered on current camera target
- Leash size: radius = ~4 tiles (diameter ~8 tiles)
- Units: pixels, derived from tile size * scale

Behavior
--------
1) Compute player position vs camera target.
2) If player is within leash radius, keep camera target unchanged.
3) If player exceeds leash radius, move camera target toward player
   just enough to keep player on the leash boundary.

Math (2D Clamp)
---------------
- dx = player.x - cam.x
- dy = player.y - cam.y
- dist = sqrt(dx^2 + dy^2)
- If dist <= leash_radius: cam unchanged
- Else: cam = player - (dx, dy) * (leash_radius / dist)

Implementation Sketch
---------------------
- Add configurable values in config:
  - *camera-leash-enabled* (bool)
  - *camera-leash-radius-tiles* (float, default 4.0)
- Modify camera target calculation (rendering.lisp / editor-camera-target):
  - Apply leash logic only when editor not active
  - Convert leash radius to pixels using world-tile-dest-size
- Ensure compatibility with camera snap formula (pixel snapping):
  - Apply leash BEFORE snap rounding

Edge Cases
----------
- Zone transition: leash should update smoothly (no jumps)
- Zoom changes: leash radius stays in world space, not screen space
- Prediction/interpolation: use actual player position, not predicted

Testing
-------
- Walk small circles in center; camera should stay still
- Walk beyond leash; camera follows smoothly
- Diagonal movement should feel consistent
- Verify zone transition camera behavior with leash enabled

Rollback
--------
- Feature flag to disable quickly if it feels worse
