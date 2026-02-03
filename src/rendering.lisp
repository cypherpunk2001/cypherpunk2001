;; NOTE: If you change behavior here, update docs/rendering.md :)
;; rendering.lisp — Glue: main draw-game function that calls into sub-modules
;; Sub-modules (loaded before this file):
;;   rendering-core.lisp     — core helpers, textures, assets
;;   rendering-tiles.lisp    — chunk cache, tile/map rendering
;;   rendering-entities.lisp — entity rendering, culling, edge strips
;;   rendering-ui.lisp       — HUD, minimap, inventory, menus
(in-package #:mmorpg)

(defun prepare-game-render-caches (game)
  "Phase B: Pre-render all visible chunk textures BEFORE begin-drawing.
   This moves FBO allocation outside the draw pass to eliminate black flash."
  (when *render-cache-enabled*
    (let* ((player (game-player game))
           (world (game-world game))
           (assets (game-assets game))
           (camera (game-camera game))
           (editor (game-editor game))
           (zone (world-zone world))
           (tile-dest-size (world-tile-dest-size world)))
      (when zone
        (multiple-value-bind (camera-x camera-y)
            (editor-camera-target editor player camera)
          (let* ((zoom (camera-zoom camera))
                 (half-view-width (/ (current-screen-width) (* 2.0 zoom)))
                 (half-view-height (/ (current-screen-height) (* 2.0 zoom)))
                 (view-left (- camera-x half-view-width))
                 (view-right (+ camera-x half-view-width))
                 (view-top (- camera-y half-view-height))
                 (view-bottom (+ camera-y half-view-height)))
            ;; Prepare main zone chunks
            (prepare-zone-chunks zone tile-dest-size
                                 view-left view-right view-top view-bottom
                                 editor assets)
            ;; Prepare preview zone chunks (edges and corners)
            (labels ((prepare-edge (edge)
                       (let ((preview-zone (world-preview-zone-for-edge world edge)))
                         (when preview-zone
                           (multiple-value-bind (offset-x offset-y)
                               (preview-zone-offset preview-zone tile-dest-size edge)
                             (prepare-preview-zone-chunks preview-zone tile-dest-size
                                                          view-left view-right view-top view-bottom
                                                          offset-x offset-y
                                                          editor assets)))))
                     (prepare-corner (edge-a edge-b)
                       (let ((preview-zone (world-preview-zone-for-corner world edge-a edge-b)))
                         (when preview-zone
                           (multiple-value-bind (offset-x offset-y)
                               (preview-zone-corner-offset preview-zone tile-dest-size edge-a edge-b)
                             (prepare-preview-zone-chunks preview-zone tile-dest-size
                                                          view-left view-right view-top view-bottom
                                                          offset-x offset-y
                                                          editor assets))))))
              ;; Check which edges/corners need preview zones
              (let ((ex-west (view-exceeds-edge-p world view-left view-right view-top view-bottom :west))
                    (ex-east (view-exceeds-edge-p world view-left view-right view-top view-bottom :east))
                    (ex-north (view-exceeds-edge-p world view-left view-right view-top view-bottom :north))
                    (ex-south (view-exceeds-edge-p world view-left view-right view-top view-bottom :south)))
                (when ex-west (prepare-edge :west))
                (when ex-east (prepare-edge :east))
                (when ex-north (prepare-edge :north))
                (when ex-south (prepare-edge :south))
                (when (and ex-west ex-north) (prepare-corner :west :north))
                (when (and ex-east ex-north) (prepare-corner :east :north))
                (when (and ex-west ex-south) (prepare-corner :west :south))
                (when (and ex-east ex-south) (prepare-corner :east :south))))))))))

(defun draw-game (game)
  ;; Render a full frame: world, entities, HUD, and menu.
  ;; Reset cache stats at start of frame (Phase A instrumentation)
  (reset-render-cache-stats)
  (with-timing (:draw-game)
    (let* ((player (game-player game))
           (npcs (game-npcs game))
           (world (game-world game))
           (audio (game-audio game))
           (ui (game-ui game))
           (render (game-render game))
           (assets (game-assets game))
           (camera (game-camera game))
           (editor (game-editor game)))
      ;; Phase B: Pre-render chunk textures BEFORE begin-drawing
      (prepare-game-render-caches game)
      (raylib:with-drawing
        (raylib:clear-background raylib:+black+)
        (multiple-value-bind (raw-camera-x raw-camera-y)
            (editor-camera-target editor player camera)
          (let* ((zoom (camera-zoom camera))
                 ;; Phase 2a: Screen-pixel snap - ensures camera aligns to screen pixels at any zoom
                 ;; Formula: cam' = round(cam * zoom) / zoom
                 ;; This prevents sub-pixel rendering artifacts (tile seams) during movement
                 (camera-x (if (> zoom 0.0)
                               (/ (fround (* raw-camera-x zoom)) zoom)
                               raw-camera-x))
                 (camera-y (if (> zoom 0.0)
                               (/ (fround (* raw-camera-y zoom)) zoom)
                               raw-camera-y))
                 (margin-x (assets-half-sprite-width assets))
                 (margin-y (assets-half-sprite-height assets))
                 (camera-2d (raylib:make-camera-2d
                             :target (raylib:make-vector2 :x camera-x :y camera-y)
                             :offset (camera-offset camera)
                             :rotation 0.0
                             :zoom zoom)))
            (raylib:with-mode-2d camera-2d
              (draw-world world render assets camera player npcs ui editor)
              (draw-zone-objects world render assets camera player editor)
              ;; Use spatial culling for entity rendering (Phase 3 optimization)
              (draw-entities-with-spatial-culling game player camera-x camera-y zoom
                                                   margin-x margin-y assets render)
              (draw-click-marker player world)
              (draw-editor-world-overlay editor world camera))))
        (draw-hud player ui world)
        (draw-minimap world player npcs ui)
        (draw-inventory player ui render assets)
        (draw-context-menu ui)
        (draw-loading-overlay ui)
        (draw-editor-ui-overlay editor ui)
        (draw-editor-tileset-preview editor render)
        ;; Cache debug overlay (Phase A instrumentation)
        (draw-cache-debug-overlay)
        (when (ui-menu-open ui)
          (draw-menu ui audio editor)))
    ;; Log cache stats at end of frame (Phase A instrumentation)
    (log-render-cache-stats))))
