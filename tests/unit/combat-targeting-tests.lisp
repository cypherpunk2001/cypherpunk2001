(in-package #:mmorpg)

;;; ============================================================
;;; COMBAT TARGETING FIX TESTS (COMBAT_PLAN.md)
;;; Tests for P0-A, P0-B, P1 fixes
;;; ============================================================

(defun test-sync-attack-target-accepts-distant ()
  "Test that sync-player-attack-target accepts distant NPCs (range-gate removed)."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (player (make-player 100.0 100.0 :id 1))
         (intent (make-intent :target-x 100.0 :target-y 100.0))
         ;; NPC at 1000,1000 - far beyond *max-target-distance-tiles* (15 tiles * 32 = 480px)
         (npc-far (make-npc 1000.0 1000.0 :archetype archetype :id 99))
         (npcs (vector npc-far))
         (world (make-test-world :tile-size 32.0 :collision-half 12.0)))
    ;; Request attack on distant NPC
    (request-attack-target intent 99)
    ;; Sync should accept the target (no range-gate)
    (sync-player-attack-target player intent npcs world)
    ;; Verify target was set (not rejected due to range)
    (assert (= (player-attack-target-id player) 99)
            () "sync-attack: should accept distant NPC (range-gate removed)")))

(defun test-sync-follow-target-accepts-distant ()
  "Test that sync-player-follow-target accepts distant NPCs (range-gate removed)."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (player (make-player 100.0 100.0 :id 1))
         (intent (make-intent :target-x 100.0 :target-y 100.0))
         ;; NPC at 1000,1000 - far beyond *max-target-distance-tiles*
         (npc-far (make-npc 1000.0 1000.0 :archetype archetype :id 88))
         (npcs (vector npc-far))
         (world (make-test-world :tile-size 32.0 :collision-half 12.0)))
    ;; Request follow on distant NPC
    (request-follow-target intent 88)
    ;; Sync should accept the target (no range-gate)
    (sync-player-follow-target player intent npcs world)
    ;; Verify target was set (not rejected due to range)
    (assert (= (player-follow-target-id player) 88)
            () "sync-follow: should accept distant NPC (range-gate removed)")))

(defun test-click-marker-tracks-target ()
  "Test that click marker follows NPC position when target-id is set."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (player (make-player 100.0 100.0 :id 1))
         ;; NPC starts at 200,200
         (npc (make-npc 200.0 200.0 :archetype archetype :id 42))
         (npcs (vector npc)))
    ;; Set attack marker tracking NPC 42
    (trigger-click-marker player 200.0 200.0 :attack 42)
    ;; Verify initial marker position
    (assert (= (player-click-marker-x player) 200.0) () "marker: initial x")
    (assert (= (player-click-marker-y player) 200.0) () "marker: initial y")
    (assert (eq (player-click-marker-kind player) :attack) () "marker: attack kind")
    (assert (= (player-click-marker-target-id player) 42) () "marker: target-id set")
    ;; NPC moves to 300,400
    (setf (npc-x npc) 300.0
          (npc-y npc) 400.0)
    ;; Update marker (should follow NPC)
    (update-click-marker player 0.016 npcs)
    ;; Verify marker followed NPC
    (assert (= (player-click-marker-x player) 300.0) () "marker: followed x")
    (assert (= (player-click-marker-y player) 400.0) () "marker: followed y")
    (assert (eq (player-click-marker-kind player) :attack) () "marker: still attack kind")
    (assert (> (player-click-marker-timer player) 0) () "marker: timer kept alive")))

(defun test-click-marker-clears-on-target-death ()
  "Test that click marker clears when tracked NPC dies."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (player (make-player 100.0 100.0 :id 1))
         (npc (make-npc 200.0 200.0 :archetype archetype :id 77))
         (npcs (vector npc)))
    ;; Set attack marker tracking NPC 77
    (trigger-click-marker player 200.0 200.0 :attack 77)
    ;; Verify marker is active
    (assert (eq (player-click-marker-kind player) :attack) () "marker: active before death")
    (assert (= (player-click-marker-target-id player) 77) () "marker: tracking target")
    ;; NPC dies
    (setf (npc-alive npc) nil)
    ;; Update marker (should clear because target is dead)
    (update-click-marker player 0.016 npcs)
    ;; Verify marker was cleared
    (assert (null (player-click-marker-kind player)) () "marker: cleared after death")
    (assert (= (player-click-marker-target-id player) 0) () "marker: target-id cleared")
    (assert (= (player-click-marker-timer player) 0.0) () "marker: timer cleared")))

(defun test-npc-array-for-player-zone ()
  "Test that npc-array-for-player-zone returns zone-state NPCs when available."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         ;; Create game with global NPCs
         (global-npcs (vector (make-npc 100.0 100.0 :archetype archetype :id 1)))
         ;; Create zone-state with different NPCs
         (zone-npcs (vector (make-npc 200.0 200.0 :archetype archetype :id 2)
                            (make-npc 300.0 300.0 :archetype archetype :id 3)))
         (zone-state (make-zone-state :zone-id :test-zone :npcs zone-npcs))
         (player (make-player 100.0 100.0 :id 1 :zone-id :test-zone))
         (game (%make-game :npcs global-npcs :player player :players (vector player))))
    ;; Register zone-state
    (setf (gethash :test-zone *zone-states*) zone-state)
    (unwind-protect
        (let ((result (npc-array-for-player-zone game player)))
          ;; Should return zone-state NPCs, not game-npcs
          (assert (eq result zone-npcs)
                  () "npc-array: should return zone-state-npcs when available")
          (assert (= (length result) 2)
                  () "npc-array: zone-state has 2 NPCs"))
      ;; Cleanup
      (remhash :test-zone *zone-states*))))

(defun test-clear-follow-target-clears-marker ()
  "Test that clear-player-follow-target also clears marker tracking."
  (let ((player (make-player 100.0 100.0 :id 1)))
    ;; Set up marker tracking
    (setf (player-follow-target-id player) 42
          (player-click-marker-target-id player) 42
          (player-click-marker-kind player) :walk)
    ;; Clear follow target
    (clear-player-follow-target player)
    ;; Verify marker was also cleared
    (assert (= (player-follow-target-id player) 0)
            () "clear-follow: follow-target-id cleared")
    (assert (= (player-click-marker-target-id player) 0)
            () "clear-follow: marker target-id cleared")
    (assert (null (player-click-marker-kind player))
            () "clear-follow: marker kind cleared")))

(defun test-clear-pickup-target-clears-marker ()
  "Test that clear-player-pickup-target also clears marker tracking."
  (let ((player (make-player 100.0 100.0 :id 1)))
    ;; Set up marker tracking
    (setf (player-pickup-target-active player) t
          (player-pickup-target-id player) :gold
          (player-click-marker-target-id player) 99
          (player-click-marker-kind player) :walk)
    ;; Clear pickup target
    (clear-player-pickup-target player)
    ;; Verify marker was also cleared
    (assert (null (player-pickup-target-active player))
            () "clear-pickup: pickup-target-active cleared")
    (assert (null (player-pickup-target-id player))
            () "clear-pickup: pickup-target-id cleared")
    (assert (= (player-click-marker-target-id player) 0)
            () "clear-pickup: marker target-id cleared")
    (assert (null (player-click-marker-kind player))
            () "clear-pickup: marker kind cleared")))

;;;; ========================================================================
;;;; Render Chunk Cache Tests (Phase 1 - Zoom-Out Performance)
;;;; ========================================================================

(defun test-chunk-cache-key ()
  "Test that chunk-cache-key generates unique keys for layer/chunk combinations."
  (let* ((layer1 (%make-zone-layer :id :floor :tileset-id :default :chunks (make-hash-table)))
         (layer2 (%make-zone-layer :id :floor :tileset-id :custom :chunks (make-hash-table)))
         (layer3 (%make-zone-layer :id :walls :tileset-id :default :chunks (make-hash-table)))
         (key1a (chunk-cache-key layer1 0 0))
         (key1b (chunk-cache-key layer1 0 0))
         (key1c (chunk-cache-key layer1 1 0))
         (key2 (chunk-cache-key layer2 0 0))
         (key3 (chunk-cache-key layer3 0 0)))
    ;; Same layer, same chunk = same key
    (assert (equal key1a key1b)
            () "chunk-cache-key: same layer + chunk = same key")
    ;; Same layer, different chunk = different key
    (assert (not (equal key1a key1c))
            () "chunk-cache-key: different chunk = different key")
    ;; Same layer id, different tileset = different key
    (assert (not (equal key1a key2))
            () "chunk-cache-key: different tileset = different key")
    ;; Different layer id = different key
    (assert (not (equal key1a key3))
            () "chunk-cache-key: different layer id = different key")))

(defun test-zone-render-cache-create ()
  "Test zone render cache creation and lookup."
  ;; Clear any existing caches
  (clrhash *zone-render-caches*)
  (let* ((zone-id :test-zone)
         (tile-dest-size 64.0)
         (cache (get-or-create-zone-render-cache zone-id tile-dest-size)))
    ;; Verify cache was created
    (assert (not (null cache))
            () "zone-render-cache-create: cache created")
    (assert (eq (zone-render-cache-zone-id cache) zone-id)
            () "zone-render-cache-create: zone-id set")
    (assert (= (zone-render-cache-chunk-pixel-size cache)
               (* *render-chunk-size* tile-dest-size))
            () "zone-render-cache-create: chunk-pixel-size computed")
    ;; Verify second call returns same cache
    (let ((cache2 (get-or-create-zone-render-cache zone-id tile-dest-size)))
      (assert (eq cache cache2)
              () "zone-render-cache-create: returns existing cache"))
    ;; Verify different zone returns different cache
    (let ((cache3 (get-or-create-zone-render-cache :other-zone tile-dest-size)))
      (assert (not (eq cache cache3))
              () "zone-render-cache-create: different zone = different cache"))
    ;; Clean up
    (clrhash *zone-render-caches*)))

(defun test-chunk-cache-invalidation ()
  "Test that invalidate-chunk-at-tile marks chunks dirty."
  ;; Clear any existing caches
  (clrhash *zone-render-caches*)
  (let* ((zone-id :test-zone)
         (tile-dest-size 64.0)
         (cache (get-or-create-zone-render-cache zone-id tile-dest-size))
         (layer (%make-zone-layer :id :floor :tileset-id :default :chunks (make-hash-table)))
         (chunk-x (floor 10 *render-chunk-size*))
         (chunk-y (floor 10 *render-chunk-size*))
         (key (chunk-cache-key layer chunk-x chunk-y)))
    ;; Create a cache entry manually (simulating a rendered chunk)
    (let ((entry (%make-render-chunk-cache
                  :chunk-x chunk-x
                  :chunk-y chunk-y
                  :layer-key (cons (zone-layer-id layer) (zone-layer-tileset-id layer))
                  :dirty nil
                  :last-access 0)))
      (setf (gethash key (zone-render-cache-chunks cache)) entry)
      ;; Verify chunk is not dirty
      (assert (not (render-chunk-cache-dirty entry))
              () "chunk-cache-invalidation: initially not dirty")
      ;; Invalidate the chunk
      (invalidate-chunk-at-tile zone-id layer 10 10)
      ;; Verify chunk is now dirty
      (assert (render-chunk-cache-dirty entry)
              () "chunk-cache-invalidation: marked dirty after invalidate")))
  ;; Clean up
  (clrhash *zone-render-caches*))

(defun test-chunk-cache-lru-eviction ()
  "Test that LRU eviction removes oldest chunks when cache is full."
  ;; Clear any existing caches
  (clrhash *zone-render-caches*)
  ;; Temporarily set max chunks to small value for testing
  (let ((*render-cache-max-chunks* 3))
    (let* ((zone-id :test-zone)
           (tile-dest-size 64.0)
           (cache (get-or-create-zone-render-cache zone-id tile-dest-size))
           (layer (%make-zone-layer :id :floor :tileset-id :default :chunks (make-hash-table)))
           (chunks (zone-render-cache-chunks cache)))
      ;; Add 3 entries with different last-access times
      (loop :for i :from 0 :below 3
            :for key = (chunk-cache-key layer i 0)
            :for entry = (%make-render-chunk-cache
                          :chunk-x i :chunk-y 0
                          :layer-key (cons :floor :default)
                          :dirty nil
                          :last-access i  ; 0, 1, 2
                          :texture nil)   ; No actual texture (testing logic only)
            :do (setf (gethash key chunks) entry))
      ;; Verify we have 3 entries
      (assert (= (hash-table-count chunks) 3)
              () "chunk-cache-lru: initial count = 3")
      ;; Evict LRU (should remove entry with last-access = 0)
      (evict-lru-chunk cache)
      ;; Verify we have 2 entries
      (assert (= (hash-table-count chunks) 2)
              () "chunk-cache-lru: count after evict = 2")
      ;; Verify entry 0 was evicted
      (let ((key0 (chunk-cache-key layer 0 0)))
        (assert (null (gethash key0 chunks))
                () "chunk-cache-lru: oldest entry evicted"))
      ;; Verify entries 1 and 2 remain
      (let ((key1 (chunk-cache-key layer 1 0))
            (key2 (chunk-cache-key layer 2 0)))
        (assert (not (null (gethash key1 chunks)))
                () "chunk-cache-lru: entry 1 remains")
        (assert (not (null (gethash key2 chunks)))
                () "chunk-cache-lru: entry 2 remains"))))
  ;; Clean up
  (clrhash *zone-render-caches*))

(defun test-toggle-render-cache-enabled ()
  "Test that toggle-render-cache-enabled flips the flag and returns new value."
  ;; Ensure cache hash is empty (no raylib calls)
  (clrhash *zone-render-caches*)
  (let ((original *render-cache-enabled*))
    (unwind-protect
        (progn
          ;; Toggle from current state
          (let ((result (toggle-render-cache-enabled)))
            (assert (eq result (not original))
                    () "toggle-render-cache-enabled: returned opposite value")
            (assert (eq *render-cache-enabled* (not original))
                    () "toggle-render-cache-enabled: global is toggled"))
          ;; Toggle back
          (let ((result2 (toggle-render-cache-enabled)))
            (assert (eq result2 original)
                    () "toggle-render-cache-enabled: double toggle restores")
            (assert (eq *render-cache-enabled* original)
                    () "toggle-render-cache-enabled: global restored")))
      ;; Restore original state
      (setf *render-cache-enabled* original))))

(defun test-toggle-tile-point-filter ()
  "Test that toggle-tile-point-filter flips the flag and returns new value."
  ;; Ensure cache hash is empty (no raylib calls)
  (clrhash *zone-render-caches*)
  (let ((original *tile-point-filter*))
    (unwind-protect
        (progn
          ;; Toggle from current state
          (let ((result (toggle-tile-point-filter)))
            (assert (eq result (not original))
                    () "toggle-tile-point-filter: returned opposite value")
            (assert (eq *tile-point-filter* (not original))
                    () "toggle-tile-point-filter: global is toggled"))
          ;; Toggle back
          (let ((result2 (toggle-tile-point-filter)))
            (assert (eq result2 original)
                    () "toggle-tile-point-filter: double toggle restores")
            (assert (eq *tile-point-filter* original)
                    () "toggle-tile-point-filter: global restored")))
      ;; Restore original state
      (setf *tile-point-filter* original))))

(defun test-preview-zone-cache-separation ()
  "Test that preview zones get separate cache entries from main zone."
  ;; Clear any existing caches
  (clrhash *zone-render-caches*)
  (let ((main-zone-id :main-zone)
        (preview-zone-id :preview-zone)
        (tile-dest-size 64.0))
    ;; Create cache for main zone
    (let ((main-cache (get-or-create-zone-render-cache main-zone-id tile-dest-size)))
      (assert (not (null main-cache))
              () "preview-zone-cache: main zone cache created")
      (assert (= (hash-table-count *zone-render-caches*) 1)
              () "preview-zone-cache: 1 cache after main zone"))
    ;; Create cache for preview zone (should be separate)
    (let ((preview-cache (get-or-create-zone-render-cache preview-zone-id tile-dest-size)))
      (assert (not (null preview-cache))
              () "preview-zone-cache: preview zone cache created")
      (assert (= (hash-table-count *zone-render-caches*) 2)
              () "preview-zone-cache: 2 caches after preview zone")
      ;; Verify they are different cache objects
      (let ((main-cache-2 (gethash main-zone-id *zone-render-caches*)))
        (assert (not (eq main-cache-2 preview-cache))
                () "preview-zone-cache: caches are separate objects"))))
  ;; Clean up
  (clrhash *zone-render-caches*))

(defun test-clear-other-zone-render-caches ()
  "Test that clear-other-zone-render-caches keeps only the specified zone."
  ;; Clear any existing caches
  (clrhash *zone-render-caches*)
  (let ((current-zone-id :current)
        (preview1-zone-id :preview1)
        (preview2-zone-id :preview2)
        (tile-dest-size 64.0))
    ;; Create caches for current zone and two preview zones
    (get-or-create-zone-render-cache current-zone-id tile-dest-size)
    (get-or-create-zone-render-cache preview1-zone-id tile-dest-size)
    (get-or-create-zone-render-cache preview2-zone-id tile-dest-size)
    (assert (= (hash-table-count *zone-render-caches*) 3)
            () "clear-other: 3 caches before clear")
    ;; Clear all except current zone
    (clear-other-zone-render-caches current-zone-id)
    ;; Verify only current zone cache remains
    (assert (= (hash-table-count *zone-render-caches*) 1)
            () "clear-other: 1 cache after clear")
    (assert (not (null (gethash current-zone-id *zone-render-caches*)))
            () "clear-other: current zone cache kept")
    (assert (null (gethash preview1-zone-id *zone-render-caches*))
            () "clear-other: preview1 cache cleared")
    (assert (null (gethash preview2-zone-id *zone-render-caches*))
            () "clear-other: preview2 cache cleared"))
  ;; Clean up
  (clrhash *zone-render-caches*))

(defun test-cleanup-stale-preview-zones ()
  "Step 10: cleanup-stale-preview-zones removes preview cache entries for edges
   no longer visible, and preserves entries for edges that ARE still visible."
  (let* ((zone-dir (namestring
                    (merge-pathnames "data/zones/"
                                     (asdf:system-source-directory :mmorpg))))
         (zone-5-path (concatenate 'string zone-dir "zone-5.lisp"))
         (zone-5 (load-zone zone-5-path))
         (graph (load-world-graph))
         (preview-cache (make-hash-table :test 'eq))
         (world (%make-world :zone zone-5
                             :tile-dest-size 32.0
                             :collision-half-width 12.0
                             :collision-half-height 12.0
                             :wall-min-x 12.0 :wall-max-x 628.0
                             :wall-min-y 12.0 :wall-max-y 628.0
                             :world-graph graph
                             :zone-preview-cache preview-cache)))
    ;; Find which spatial exits zone-5 has and pre-populate preview cache
    (let ((exits (world-graph-exits graph :zone-5))
          (populated-ids nil))
      ;; Populate preview cache with a dummy zone for each spatial exit
      (dolist (exit-spec exits)
        (when (spatial-exit-p exit-spec)
          (let ((target-id (getf exit-spec :to)))
            (when target-id
              (setf (gethash target-id preview-cache) zone-5)  ; dummy zone data
              (push (cons (getf exit-spec :edge) target-id) populated-ids)))))
      ;; Verify we have at least one entry to test
      (assert (> (hash-table-count preview-cache) 0) ()
              "cleanup-stale: need at least one spatial exit in zone-5 for test")
      (let ((initial-count (hash-table-count preview-cache)))
        ;; Case 1: All edges visible — nothing removed
        (cleanup-stale-preview-zones world t t t t)
        (assert (= (hash-table-count preview-cache) initial-count) ()
                "cleanup-stale: all-visible should not remove any entries")
        ;; Case 2: No edges visible — all entries removed
        (cleanup-stale-preview-zones world nil nil nil nil)
        (assert (= (hash-table-count preview-cache) 0) ()
                "cleanup-stale: no-visible should remove all entries")
        ;; Case 3: Repopulate and test partial visibility
        (dolist (pair populated-ids)
          (setf (gethash (cdr pair) preview-cache) zone-5))
        ;; Make only west visible — entries for other edges should be removed
        (cleanup-stale-preview-zones world t nil nil nil)
        ;; West-connected zones should remain, others removed
        (dolist (pair populated-ids)
          (if (eq (car pair) :west)
              (assert (gethash (cdr pair) preview-cache) ()
                      "cleanup-stale: west entry should remain when west visible")
              (assert (null (gethash (cdr pair) preview-cache)) ()
                      "cleanup-stale: non-west entry ~a should be removed" (car pair))))))))

(defun test-spatial-grid-query-rect ()
  "Test spatial-grid-query-rect returns entities within world-coordinate bounds."
  (let* ((cell-size 128.0)  ; Default *spatial-cell-size*
         (grid (make-spatial-grid cell-size)))
    ;; Insert entities at various positions
    ;; Entity 1 at (64, 64) - in cell (0, 0)
    (spatial-grid-insert grid 1 64.0 64.0)
    ;; Entity 2 at (200, 64) - in cell (1, 0)
    (spatial-grid-insert grid 2 200.0 64.0)
    ;; Entity 3 at (64, 200) - in cell (0, 1)
    (spatial-grid-insert grid 3 64.0 200.0)
    ;; Entity 4 at (300, 300) - in cell (2, 2)
    (spatial-grid-insert grid 4 300.0 300.0)
    ;; Entity 5 at (500, 500) - in cell (3, 3) - far away
    (spatial-grid-insert grid 5 500.0 500.0)

    ;; Query rect covering cells (0,0) and (1,0) - should find entities 1 and 2
    (let ((result (spatial-grid-query-rect grid 0.0 0.0 256.0 127.0)))
      (assert (= (length result) 2)
              () "query-rect: 2 entities in [0,256]x[0,127], got ~a" (length result))
      (assert (member 1 result) () "query-rect: entity 1 should be in result")
      (assert (member 2 result) () "query-rect: entity 2 should be in result"))

    ;; Query rect covering just cell (2,2) - should find entity 4
    ;; Use 383.0 to avoid including cell 3 which starts at 384
    (let ((result (spatial-grid-query-rect grid 256.0 256.0 383.0 383.0)))
      (assert (= (length result) 1)
              () "query-rect: 1 entity in [256,383]x[256,383], got ~a" (length result))
      (assert (member 4 result) () "query-rect: entity 4 should be in result"))

    ;; Query rect covering all cells (0,0) through (2,2) - should find 1,2,3,4
    ;; Use 383.0 to avoid including cell 3 which starts at 384
    (let ((result (spatial-grid-query-rect grid 0.0 0.0 383.0 383.0)))
      (assert (= (length result) 4)
              () "query-rect: 4 entities in [0,383]x[0,383], got ~a" (length result))
      (assert (not (member 5 result)) () "query-rect: entity 5 should NOT be in result"))

    ;; Query empty rect (no cells) - should return nil
    (let ((result (spatial-grid-query-rect grid 1000.0 1000.0 1100.0 1100.0)))
      (assert (null result)
              () "query-rect: no entities in empty area, got ~a" result))

    ;; Query nil grid - should return nil gracefully
    (let ((result (spatial-grid-query-rect nil 0.0 0.0 100.0 100.0)))
      (assert (null result)
              () "query-rect: nil grid should return nil"))))

(defun test-entity-in-render-distance-p ()
  "Test entity-in-render-distance-p distance filtering."
  (ensure-test-game-data)
  (let ((original-max-distance *entity-render-max-distance*)
        (archetype (gethash :goblin *npc-archetypes*)))
    (unwind-protect
        (let* ((player (make-player 100.0 100.0 :id 1))
               (near-npc (make-npc 150.0 100.0 :archetype archetype :id 2))   ; 50 pixels away
               (far-npc (make-npc 600.0 100.0 :archetype archetype :id 3)))   ; 500 pixels away
          ;; Test with nil (unlimited) - all should pass
          (setf *entity-render-max-distance* nil)
          (assert (entity-in-render-distance-p near-npc player)
                  () "distance: near NPC should pass with nil limit")
          (assert (entity-in-render-distance-p far-npc player)
                  () "distance: far NPC should pass with nil limit")

          ;; Test with distance limit of 200 pixels
          (setf *entity-render-max-distance* 200.0)
          (assert (entity-in-render-distance-p near-npc player)
                  () "distance: near NPC (50px) should pass with 200px limit")
          (assert (not (entity-in-render-distance-p far-npc player))
                  () "distance: far NPC (500px) should fail with 200px limit")

          ;; Test with exact boundary (50px away, 50px limit)
          (setf *entity-render-max-distance* 50.0)
          (assert (entity-in-render-distance-p near-npc player)
                  () "distance: NPC at exactly max distance should pass")

          ;; Test with nil player - should pass (graceful handling)
          (setf *entity-render-max-distance* 100.0)
          (assert (entity-in-render-distance-p near-npc nil)
                  () "distance: nil player should pass"))
      ;; Restore original value
      (setf *entity-render-max-distance* original-max-distance))))



(defvar *tests-combat-targeting*
  '(test-npc-array-for-player-zone
    test-sync-attack-target-accepts-distant
    test-sync-follow-target-accepts-distant
    test-click-marker-tracks-target
    test-click-marker-clears-on-target-death
    test-clear-follow-target-clears-marker
    test-clear-pickup-target-clears-marker
    ;; Render Chunk Cache Tests (Phase 1 - Zoom-Out Performance)
    test-chunk-cache-key
    test-zone-render-cache-create
    test-chunk-cache-invalidation
    test-chunk-cache-lru-eviction
    ;; Render Toggle Tests
    test-toggle-render-cache-enabled
    test-toggle-tile-point-filter
    ;; Preview Zone Cache Tests (Phase 2)
    test-preview-zone-cache-separation
    test-clear-other-zone-render-caches
    ;; Preview Cache Cleanup (Step 10)
    test-cleanup-stale-preview-zones
    ;; Spatial Grid Query Tests (Phase 3)
    test-spatial-grid-query-rect
    ;; Distance Filter Tests (Phase 4)
    test-entity-in-render-distance-p)
  "Combat targeting + render cache domain test functions.")
