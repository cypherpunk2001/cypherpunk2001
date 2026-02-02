;; NOTE: If you change behavior here, update docs/movement.md :)
(in-package #:mmorpg)

;;; movement.lisp - Glue: world construction and zone application
;;;
;;; This file is loaded LAST among movement-* files and contains:
;;; - make-world: Build world state and derived collision/render constants
;;; - apply-zone-to-world: Replace zone and rebuild wall-map bounds
;;;
;;; Split files (loaded before this, in order):
;;; - movement-core.lisp: zone state, movement helpers, edge utilities
;;; - movement-collision.lisp: collision checks, wall maps, position queries
;;; - movement-preview.lisp: preview zones, camera edge checks
;;; - movement-transition.lisp: zone transitions, hysteresis, NPC carry

(defun make-world ()
  ;; Build world state and derived collision/render constants.
  (let* ((zone (load-zone *zone-path*))
         (graph (load-world-graph))
         (tile-size-f (float *tile-size* 1.0))
         (tile-dest-size (* tile-size-f *tile-scale*))
         (floor-index *floor-tile-index*)
         (wall-map (if zone
                       (zone-wall-map zone)
                       (build-wall-map)))
         (wall-map-width (array-dimension wall-map 1))
         (wall-map-height (array-dimension wall-map 0))
         (collision-half-width (* (/ tile-dest-size 2.0) *player-collision-scale*))
         (collision-half-height (* (/ tile-dest-size 2.0) *player-collision-scale*))
         (wall-min-x (+ (* (+ *wall-origin-x* 1) tile-dest-size)
                        collision-half-width))
         (wall-max-x (- (* (+ *wall-origin-x* (1- wall-map-width))
                           tile-dest-size)
                        collision-half-width))
         (wall-min-y (+ (* (+ *wall-origin-y* 1) tile-dest-size)
                        collision-half-height))
        (wall-max-y (- (* (+ *wall-origin-y* (1- wall-map-height))
                           tile-dest-size)
                        collision-half-height)))
    (let ((world (%make-world :tile-size-f tile-size-f
                              :tile-dest-size tile-dest-size
                              :floor-index floor-index
                              :zone zone
                              :zone-label (zone-label zone)
                              :world-graph graph
                              :zone-preview-cache (make-hash-table :test 'eq :size 64)
                              :minimap-spawns nil
                              :minimap-collisions nil
                              :wall-map wall-map
                              :wall-map-width wall-map-width
                              :wall-map-height wall-map-height
                              :collision-half-width collision-half-width
                              :collision-half-height collision-half-height
                              :wall-min-x wall-min-x
                              :wall-max-x wall-max-x
                              :wall-min-y wall-min-y
                              :wall-max-y wall-max-y)))
      (log-verbose "World ready: zone=~a walls=~dx~d tile=~,2f dest=~,2f"
                   (zone-label zone)
                   wall-map-width
                   wall-map-height
                   tile-size-f
                   tile-dest-size)
      (setf (world-minimap-spawns world)
            (build-adjacent-minimap-spawns world))
      (setf (world-minimap-collisions world)
            (build-minimap-collisions world))
      ;; Register initial zone in zone-state cache for zone-filtered snapshots
      (when zone
        (let ((initial-zone-id (zone-id zone)))
          (when initial-zone-id
            ;; Phase 2 perf: Use array-backed spatial grids
            (let* ((tile-dest-size-f (* (float *tile-size* 1.0) *tile-scale*))
                   (zone-w (or (zone-width zone) 64))
                   (zone-h (or (zone-height zone) 64)))
              (setf (gethash initial-zone-id *zone-states*)
                    (make-zone-state
                     :zone-id initial-zone-id
                     :zone zone
                     :wall-map wall-map
                     :objects (zone-objects zone)
                     :npcs (vector)
                     ;; Initialize array-backed spatial grids for O(1) queries
                     :player-grid (make-spatial-grid-for-zone zone-w zone-h tile-dest-size-f)
                     :npc-grid (make-spatial-grid-for-zone zone-w zone-h tile-dest-size-f)))))))  ; NPCs populated later
      world)))

(defun apply-zone-to-world (world zone)
  ;; Replace the world's zone and rebuild wall-map-derived bounds.
  ;; NOTE: Render cache clearing is handled by *client-zone-change-hook*
  ;; which is called from client-only code paths, not here.
  (let* ((tile-dest-size (world-tile-dest-size world))
         (wall-map (if zone
                       (zone-wall-map zone)
                       (build-wall-map)))
         (wall-map-width (array-dimension wall-map 1))
         (wall-map-height (array-dimension wall-map 0))
         (collision-half-width (world-collision-half-width world))
         (collision-half-height (world-collision-half-height world))
         (wall-min-x (+ (* (+ *wall-origin-x* 1) tile-dest-size)
                        collision-half-width))
         (wall-max-x (- (* (+ *wall-origin-x* (1- wall-map-width))
                           tile-dest-size)
                        collision-half-width))
         (wall-min-y (+ (* (+ *wall-origin-y* 1) tile-dest-size)
                        collision-half-height))
         (wall-max-y (- (* (+ *wall-origin-y* (1- wall-map-height))
                           tile-dest-size)
                        collision-half-height)))
    (setf (world-zone world) zone
          (world-zone-label world) (zone-label zone)
          (world-wall-map world) wall-map
          (world-wall-map-width world) wall-map-width
          (world-wall-map-height world) wall-map-height
          (world-wall-min-x world) wall-min-x
          (world-wall-max-x world) wall-max-x
          (world-wall-min-y world) wall-min-y
          (world-wall-max-y world) wall-max-y)
    (unless (world-zone-preview-cache world)
      (setf (world-zone-preview-cache world) (make-hash-table :test 'eq :size 64)))
    (let ((graph (world-world-graph world)))
      (when graph
        (setf (world-graph-zone-paths graph)
              (build-zone-paths (resolve-zone-path *zone-root*)))))
    (setf (world-minimap-spawns world)
          (build-adjacent-minimap-spawns world))
    (setf (world-minimap-collisions world)
          (build-minimap-collisions world))
    world))
