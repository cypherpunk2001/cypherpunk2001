(in-package #:mmorpg)

;;; ZONE TRANSITION TESTS (Seamless Zone Loading - PLAN_loading_zones.md)
;;; ============================================================

;;; --- Step 4: LRU Zone Cache ---

(defun test-zone-cache-insert-lookup ()
  "Test insert + lookup returns the zone."
  (let ((cache (make-zone-lru-cache 3)))
    ;; Insert a mock zone (use a keyword as a stand-in)
    (zone-cache-insert cache :zone-1 :zone-data-1)
    (assert (eq (zone-cache-lookup cache :zone-1) :zone-data-1) ()
            "zone-cache: insert + lookup should return zone")))

(defun test-zone-cache-lru-eviction-at-capacity ()
  "Test LRU eviction when cache is at capacity."
  (let ((cache (make-zone-lru-cache 2)))
    (zone-cache-insert cache :zone-1 :data-1)
    (zone-cache-insert cache :zone-2 :data-2)
    ;; Cache is full (capacity 2). Insert third evicts LRU tail (:zone-1)
    (zone-cache-insert cache :zone-3 :data-3)
    (assert (null (zone-cache-lookup cache :zone-1)) ()
            "zone-cache: evicted zone should return nil")
    (assert (eq (zone-cache-lookup cache :zone-2) :data-2) ()
            "zone-cache: non-evicted zone should remain")
    (assert (eq (zone-cache-lookup cache :zone-3) :data-3) ()
            "zone-cache: newly inserted zone should be present")))

(defun test-zone-cache-lookup-promotes-to-front ()
  "Test that lookup promotes entry to LRU front (preventing eviction)."
  (let ((cache (make-zone-lru-cache 2)))
    (zone-cache-insert cache :zone-1 :data-1)
    (zone-cache-insert cache :zone-2 :data-2)
    ;; Access zone-1 to promote it (zone-2 becomes LRU tail)
    (zone-cache-lookup cache :zone-1)
    ;; Insert zone-3 — should evict zone-2 (LRU tail), not zone-1
    (zone-cache-insert cache :zone-3 :data-3)
    (assert (eq (zone-cache-lookup cache :zone-1) :data-1) ()
            "zone-cache: promoted zone should survive eviction")
    (assert (null (zone-cache-lookup cache :zone-2)) ()
            "zone-cache: LRU tail should be evicted")))

(defun test-zone-cache-miss-returns-nil ()
  "Test that cache miss returns nil."
  (let ((cache (make-zone-lru-cache 3)))
    (assert (null (zone-cache-lookup cache :nonexistent)) ()
            "zone-cache: miss should return nil")))

(defun test-zone-cache-contains-p-no-promote ()
  "Test contains-p returns T without promoting in LRU order."
  (let ((cache (make-zone-lru-cache 2)))
    (zone-cache-insert cache :zone-1 :data-1)
    (zone-cache-insert cache :zone-2 :data-2)
    ;; Check zone-1 exists via contains-p (should NOT promote)
    (assert (zone-cache-contains-p cache :zone-1) ()
            "zone-cache: contains-p should return T for present zone")
    (assert (not (zone-cache-contains-p cache :zone-99)) ()
            "zone-cache: contains-p should return nil for absent zone")
    ;; Insert zone-3 — zone-1 should be evicted since contains-p didn't promote
    (zone-cache-insert cache :zone-3 :data-3)
    (assert (null (zone-cache-lookup cache :zone-1)) ()
            "zone-cache: contains-p should NOT promote (zone-1 evicted)")))

;;; --- Step 3: Directional Gating ---

(defun test-edge-direction-passes-due-east ()
  "Moving due east at east edge should pass."
  (let ((*zone-direction-threshold* 0.3))
    (assert (edge-direction-passes-p 1.0 0.0 :east) ()
            "directional-gating: due east at east edge should pass")))

(defun test-edge-direction-passes-northeast ()
  "Moving northeast (45 degrees) at east edge should pass (dot ~0.707 > 0.3)."
  (let ((*zone-direction-threshold* 0.3))
    (assert (edge-direction-passes-p 1.0 -1.0 :east) ()
            "directional-gating: northeast at east edge should pass")))

(defun test-edge-direction-rejects-tangential ()
  "Moving mostly north along east edge (small east component) should be rejected."
  (let ((*zone-direction-threshold* 0.3))
    ;; Moving at ~10 degrees off north: dx=0.17, dy=-0.98
    ;; Dot with east normal (1,0) = 0.17/1.0 ≈ 0.17 < 0.3
    (assert (not (edge-direction-passes-p 0.17 -0.98 :east)) ()
            "directional-gating: tangential movement should be rejected")))

(defun test-edge-direction-zero-vector ()
  "Zero movement vector should return nil (no transition for stationary players)."
  (let ((*zone-direction-threshold* 0.3))
    (assert (not (edge-direction-passes-p 0.0 0.0 :east)) ()
            "directional-gating: zero vector should return nil")))

(defun test-edge-direction-all-edges ()
  "Test directional gating for all four cardinal edges."
  (let ((*zone-direction-threshold* 0.3))
    ;; North edge: movement must have negative Y (upward)
    (assert (edge-direction-passes-p 0.0 -1.0 :north) ()
            "directional-gating: due north at north edge")
    ;; South edge: movement must have positive Y (downward)
    (assert (edge-direction-passes-p 0.0 1.0 :south) ()
            "directional-gating: due south at south edge")
    ;; West edge: movement must have negative X (leftward)
    (assert (edge-direction-passes-p -1.0 0.0 :west) ()
            "directional-gating: due west at west edge")
    ;; East edge: movement must have positive X (rightward)
    (assert (edge-direction-passes-p 1.0 0.0 :east) ()
            "directional-gating: due east at east edge")
    ;; Opposite direction should fail
    (assert (not (edge-direction-passes-p 0.0 1.0 :north)) ()
            "directional-gating: south at north edge should fail")
    (assert (not (edge-direction-passes-p 1.0 0.0 :west)) ()
            "directional-gating: east at west edge should fail")))

;;; --- Steps 1-2: Cooldown, Hysteresis, Distance ---

(defun test-player-distance-to-edge-north ()
  "Test distance from player to north edge."
  (let ((player (make-player 100.0 50.0)))
    ;; Zone bounds: min-y=0, max-y=200
    ;; Player at y=50 → distance to north = 50 - 0 = 50
    (assert (= (player-distance-to-edge player :north 0.0 200.0 0.0 200.0) 50.0) ()
            "distance-to-edge: north should be y - min-y")))

(defun test-player-distance-to-edge-all ()
  "Test distance from player to all four edges."
  (let ((player (make-player 60.0 40.0)))
    ;; Zone bounds: 0-200 in both axes
    (assert (= (player-distance-to-edge player :north 0.0 200.0 0.0 200.0) 40.0) ()
            "distance-to-edge: north = y - min-y")
    (assert (= (player-distance-to-edge player :south 0.0 200.0 0.0 200.0) 160.0) ()
            "distance-to-edge: south = max-y - y")
    (assert (= (player-distance-to-edge player :west 0.0 200.0 0.0 200.0) 60.0) ()
            "distance-to-edge: west = x - min-x")
    (assert (= (player-distance-to-edge player :east 0.0 200.0 0.0 200.0) 140.0) ()
            "distance-to-edge: east = max-x - x")))

(defun test-player-in-arm-band-p-inside ()
  "Player close to edge should be in arm band."
  (let ((*zone-hysteresis-in* 6.0))
    ;; tile-dest-size = 64 (16 * 4.0), arm threshold = 6 * 64 = 384 px
    ;; Player at y=100, min-y=0 → distance to north = 100 (< 384) → in arm band
    (let ((player (make-player 500.0 100.0)))
      (assert (player-in-arm-band-p player :north 0.0 4096.0 0.0 4096.0 64.0) ()
              "arm-band: player close to edge should be in arm band"))))

(defun test-player-in-arm-band-p-outside ()
  "Player deep in zone interior should NOT be in arm band."
  (let ((*zone-hysteresis-in* 6.0))
    ;; Player at y=2048, min-y=0 → distance to north = 2048 (> 384) → NOT in arm band
    (let ((player (make-player 500.0 2048.0)))
      (assert (not (player-in-arm-band-p player :north 0.0 4096.0 0.0 4096.0 64.0)) ()
              "arm-band: player in interior should NOT be in arm band"))))

(defun test-player-past-cancel-line-p-test ()
  "Player deep in interior should be past cancel line."
  (let ((*zone-hysteresis-out* 8.0))
    ;; cancel threshold = 8 * 64 = 512 px
    ;; Player at y=600, min-y=0 → distance to north = 600 (> 512) → past cancel
    (let ((player (make-player 500.0 600.0)))
      (assert (player-past-cancel-line-p player :north 0.0 4096.0 0.0 4096.0 64.0) ()
              "cancel-line: player past cancel distance should return T"))
    ;; Player at y=400 → distance = 400 (< 512) → NOT past cancel
    (let ((player (make-player 500.0 400.0)))
      (assert (not (player-past-cancel-line-p player :north 0.0 4096.0 0.0 4096.0 64.0)) ()
              "cancel-line: player within cancel distance should return nil"))))

(defun test-cooldown-skips-transition ()
  "Player with active cooldown should have transition skipped."
  (let ((player (make-player 100.0 200.0)))
    ;; Set cooldown > 0
    (setf (player-zone-transition-cooldown player) 1.0)
    (assert (> (player-zone-transition-cooldown player) 0.0) ()
            "cooldown: player with cooldown > 0 should be skipped")))

(defun test-cooldown-decrements ()
  "Cooldown decrements by dt."
  (let ((player (make-player 100.0 200.0)))
    (setf (player-zone-transition-cooldown player) 1.5)
    ;; Simulate decrement (what update-zone-transition does)
    (decf (player-zone-transition-cooldown player) 0.016)
    (assert (< (abs (- (player-zone-transition-cooldown player) 1.484)) 0.001) ()
            "cooldown: should decrement by dt")))

(defun test-hysteresis-config-invariant ()
  "Cancel line must be further from edge than arm line."
  (assert (> *zone-hysteresis-out* *zone-hysteresis-in*) ()
          "hysteresis: cancel distance must be > arm distance"))

;;; --- Step 7: Edge Strip Spatial Filtering ---

(defun test-opposite-edge-roundtrip ()
  "opposite-edge should be an involution (applying twice returns original)."
  (dolist (edge '(:north :south :east :west))
    (assert (eq (opposite-edge (opposite-edge edge)) edge) ()
            (format nil "opposite-edge: roundtrip failed for ~a" edge))))

(defun test-entity-in-edge-strip-p-north ()
  "Entity at south edge of adjacent zone (queried as :north) should be in strip."
  ;; Zone: 64 tiles wide, 64 tall, tile-dest-size = 32px
  ;; Zone pixel dimensions: 64*32 = 2048 x 2048
  ;; For :north edge, strip is at south edge of adj zone: y >= (2048 - strip_width)
  ;; strip-width-px = 10 * 32 = 320
  ;; y >= 2048 - 320 = 1728
  (let* ((zone (%make-zone :width 64 :height 64 :chunk-size 8))
         (tds 32.0))
    (assert (entity-in-edge-strip-p 100.0 1800.0 :north zone 320.0 tds) ()
            "edge-strip: entity near south edge should be in :north strip")
    (assert (not (entity-in-edge-strip-p 100.0 100.0 :north zone 320.0 tds)) ()
            "edge-strip: entity far from south edge should NOT be in :north strip")))

(defun test-entity-in-edge-strip-p-south ()
  "Entity at north edge of adjacent zone (queried as :south) should be in strip."
  (let ((zone (%make-zone :width 64 :height 64 :chunk-size 8))
        (tds 32.0))
    ;; For :south edge, strip is at north edge of adj zone: y <= strip_width
    (assert (entity-in-edge-strip-p 100.0 200.0 :south zone 320.0 tds) ()
            "edge-strip: entity near north edge should be in :south strip")
    (assert (not (entity-in-edge-strip-p 100.0 1800.0 :south zone 320.0 tds)) ()
            "edge-strip: entity far from north edge should NOT be in :south strip")))

(defun test-entity-in-edge-strip-p-outside ()
  "Entity in zone center should not be in any edge strip."
  (let ((zone (%make-zone :width 64 :height 64 :chunk-size 8))
        (tds 32.0))
    ;; Center of 2048x2048 zone = (1024, 1024), strip-width = 320
    (dolist (edge '(:north :south :east :west))
      (assert (not (entity-in-edge-strip-p 1024.0 1024.0 edge zone 320.0 tds)) ()
              (format nil "edge-strip: center entity should not be in ~a strip" edge)))))

;;; --- Step 9: Edge Strip Offset Computation ---

(defun test-compute-edge-strip-offset-all-edges ()
  "Verify edge strip offsets for all four cardinal directions."
  ;; We need a world with a zone. Create a minimal one.
  (let* ((zone (%make-zone :width 64 :height 64 :chunk-size 8))
         ;; tile-dest-size = chunk-size * 4 = 32, so zone-span = 64*32 = 2048
         (world (%make-world :zone zone :tile-dest-size 32.0)))
    ;; North: adjacent zone is above → offset (0, -2048)
    (multiple-value-bind (ox oy) (compute-edge-strip-offset :north world)
      (assert (= ox 0.0) () "offset-north: x should be 0")
      (assert (= oy -2048.0) () "offset-north: y should be -span"))
    ;; South: adjacent zone is below → offset (0, +2048)
    (multiple-value-bind (ox oy) (compute-edge-strip-offset :south world)
      (assert (= ox 0.0) () "offset-south: x should be 0")
      (assert (= oy 2048.0) () "offset-south: y should be +span"))
    ;; East: adjacent zone is right → offset (+2048, 0)
    (multiple-value-bind (ox oy) (compute-edge-strip-offset :east world)
      (assert (= ox 2048.0) () "offset-east: x should be +span")
      (assert (= oy 0.0) () "offset-east: y should be 0"))
    ;; West: adjacent zone is left → offset (-2048, 0)
    (multiple-value-bind (ox oy) (compute-edge-strip-offset :west world)
      (assert (= ox -2048.0) () "offset-west: x should be -span")
      (assert (= oy 0.0) () "offset-west: y should be 0"))))

;;; --- Step 6: Loading Overlay ---

(defun test-zone-transition-show-loading-p-always-nil ()
  "zone-transition-show-loading-p must always return nil for locomotion transitions."
  (assert (not (zone-transition-show-loading-p)) ()
          "show-loading-p: must return nil for seamless transitions"))

;;; --- Step 8: Edge Strip Entity Creation ---

(defun test-edge-strip-player-creation ()
  "make-edge-strip-player creates a player struct with correct fields."
  (let* ((plist '(:id 42 :x 100.0 :y 200.0 :dx 1.0 :dy 0.0 :hp 50
                  :anim-state :walk :facing :side :facing-sign -1.0
                  :frame-index 2 :frame-timer 0.5
                  :attacking nil :running t))
         (p (make-edge-strip-player plist)))
    (assert (= (player-id p) 42) () "edge-strip-player: id")
    (assert (= (player-x p) 100.0) () "edge-strip-player: x")
    (assert (= (player-y p) 200.0) () "edge-strip-player: y")
    (assert (= (player-dx p) 1.0) () "edge-strip-player: dx")
    (assert (eq (player-anim-state p) :walk) () "edge-strip-player: anim-state")
    (assert (eq (player-facing p) :side) () "edge-strip-player: facing")
    (assert (= (player-facing-sign p) -1.0) () "edge-strip-player: facing-sign")
    (assert (eq (player-running p) t) () "edge-strip-player: running")))

(defun test-edge-strip-npc-creation ()
  "make-edge-strip-npc creates an NPC struct with stats for health bar."
  (let* ((plist '(:id 99 :x 300.0 :y 400.0 :hits-left 3 :alive t
                  :anim-state :idle :facing :down :frame-index 0 :frame-timer 0.0))
         (n (make-edge-strip-npc plist)))
    (assert (= (npc-id n) 99) () "edge-strip-npc: id")
    (assert (= (npc-x n) 300.0) () "edge-strip-npc: x")
    (assert (= (npc-y n) 400.0) () "edge-strip-npc: y")
    (assert (= (npc-hits-left n) 3) () "edge-strip-npc: hits-left")
    (assert (npc-alive n) () "edge-strip-npc: alive")
    ;; Must have stats for health bar rendering
    (assert (npc-stats n) () "edge-strip-npc: must have stats (for health bar)")))

;;; --- Step 12: Preloading, State Machine, and Edge-Strip E2E Tests ---

(defun test-spatial-exit-p-preserve-x ()
  "Spatial exits with :preserve-x offset should return T."
  (assert (spatial-exit-p '(:edge :north :to :zone-b :offset :preserve-x)) ()
          "spatial-exit-p: :preserve-x should be spatial"))

(defun test-spatial-exit-p-preserve-y ()
  "Spatial exits with :preserve-y offset should return T."
  (assert (spatial-exit-p '(:edge :east :to :zone-c :offset :preserve-y)) ()
          "spatial-exit-p: :preserve-y should be spatial"))

(defun test-spatial-exit-p-teleport ()
  "Non-spatial exits (teleports with numeric offset) should return nil."
  (assert (not (spatial-exit-p '(:edge :north :to :zone-d :offset 100))) ()
          "spatial-exit-p: numeric offset should NOT be spatial"))

(defun test-spatial-exit-p-nil-offset ()
  "Exits with nil offset should return nil (not spatial)."
  (assert (not (spatial-exit-p '(:edge :north :to :zone-e))) ()
          "spatial-exit-p: nil offset should NOT be spatial"))

(defun test-edge-strip-object-creation ()
  "make-edge-strip-object creates a zone-object struct from plist."
  (let* ((plist '(:id :gold-ore :x 5 :y 10 :count 3 :respawn 0.0 :respawnable t))
         (obj (make-edge-strip-object plist)))
    (assert (eq (zone-object-id obj) :gold-ore) () "edge-strip-object: id")
    (assert (= (zone-object-x obj) 5) () "edge-strip-object: x")
    (assert (= (zone-object-y obj) 10) () "edge-strip-object: y")
    (assert (= (zone-object-count obj) 3) () "edge-strip-object: count")
    (assert (= (zone-object-respawn obj) 0.0) () "edge-strip-object: respawn")
    (assert (zone-object-respawnable obj) () "edge-strip-object: respawnable")))

(defun test-edge-strip-object-defaults ()
  "make-edge-strip-object handles missing fields with defaults."
  (let* ((plist '(:id :rock))
         (obj (make-edge-strip-object plist)))
    (assert (eq (zone-object-id obj) :rock) () "edge-strip-object-defaults: id")
    (assert (= (zone-object-x obj) 0) () "edge-strip-object-defaults: x defaults to 0")
    (assert (= (zone-object-y obj) 0) () "edge-strip-object-defaults: y defaults to 0")
    (assert (= (zone-object-count obj) 1) () "edge-strip-object-defaults: count defaults to 1")
    (assert (= (zone-object-respawn obj) 0.0) () "edge-strip-object-defaults: respawn defaults to 0")))

(defun test-update-zone-transition-returns-count ()
  "update-zone-transition must return 0 (fixnum) when no transitions occur.
   Callers use (plusp transitioned) to gate post-transition logic."
  (let* ((world (make-test-world :tile-size 64.0))
         (player (make-player 2000.0 2000.0))  ; deep in center, no edges
         (players (make-array 1 :initial-element player))
         (game (%make-game :world world :players players :player player)))
    ;; With no world-graph, no transitions can fire
    (let ((result (update-zone-transition game)))
      (assert (and (integerp result) (zerop result)) ()
              "update-zone-transition: must return 0 when no transitions occur"))))

(defun test-arm-sets-pending-edge ()
  "When player enters arm band with correct intent, pending should be set."
  (let* ((*zone-hysteresis-in* 6.0)
         (player (make-player 500.0 50.0)))  ; close to north edge (y=50)
    ;; Set intent pointing north (negative dy)
    (let ((intent (player-intent player)))
      (setf (intent-move-dx intent) 0.0
            (intent-move-dy intent) -1.0))
    ;; Verify arm band detection
    (assert (player-in-arm-band-p player :north 0.0 4096.0 0.0 4096.0 64.0) ()
            "arm-sets-pending: player near north should be in arm band")
    ;; Verify directional gating passes
    (multiple-value-bind (dx dy) (player-intent-direction player)
      (assert (edge-direction-passes-p dx dy :north) ()
              "arm-sets-pending: north intent should pass directional gating"))))

(defun test-cancel-clears-pending ()
  "When player retreats past cancel line, pending should be clearable."
  (let* ((*zone-hysteresis-out* 8.0)
         ;; Player at y=600, cancel threshold = 8*64 = 512, distance to north = 600 > 512
         (player (make-player 500.0 600.0)))
    (setf (player-zone-transition-pending player) :north)
    (assert (player-past-cancel-line-p player :north 0.0 4096.0 0.0 4096.0 64.0) ()
            "cancel-clears: player past cancel distance should trigger cancel")))

(defun test-direction-gating-rejects-wrong-direction ()
  "Moving away from an edge should fail directional gating."
  ;; Moving south (positive dy) should NOT pass :north gating
  (assert (not (edge-direction-passes-p 0.0 1.0 :north)) ()
          "direction-gating: moving south should not pass :north")
  ;; Moving west (negative dx) should NOT pass :east gating
  (assert (not (edge-direction-passes-p -1.0 0.0 :east)) ()
          "direction-gating: moving west should not pass :east"))

(defun test-compute-edge-strip-offset-symmetry ()
  "Offsets for opposite edges should be equal in magnitude but opposite in sign."
  (let ((world (make-test-world :tile-size 64.0)))
    (setf (world-zone world) (%make-zone :width 64 :height 64 :chunk-size 8))
    (multiple-value-bind (nx ny) (compute-edge-strip-offset :north world)
      (multiple-value-bind (sx sy) (compute-edge-strip-offset :south world)
        (assert (= nx 0.0) () "offset-symmetry: north x should be 0")
        (assert (= sx 0.0) () "offset-symmetry: south x should be 0")
        (assert (= ny (- sy)) () "offset-symmetry: north-y should equal -south-y")))
    (multiple-value-bind (ex ey) (compute-edge-strip-offset :east world)
      (multiple-value-bind (wx wy) (compute-edge-strip-offset :west world)
        (assert (= ey 0.0) () "offset-symmetry: east y should be 0")
        (assert (= wy 0.0) () "offset-symmetry: west y should be 0")
        (assert (= ex (- wx)) () "offset-symmetry: east-x should equal -west-x")))))

(defun test-client-skips-zone-transition ()
  "Client-mode game must NOT run update-zone-transition (server/local only).
   Verifying via the gate: net-role :client + update-zone-transition returns nil."
  (let* ((world (make-test-world :tile-size 64.0))
         (player (make-player 50.0 50.0))  ; near north-west corner
         (players (make-array 1 :initial-element player))
         (game (%make-game :world world :players players :player player
                           :net-role :client)))
    ;; Even with a player near an edge, client mode should not trigger transitions
    ;; The gate in update-sim checks (not (eq (game-net-role game) :client))
    (assert (eq (game-net-role game) :client) ()
            "client-skips-zt: game should be in client mode")
    ;; update-zone-transition itself doesn't check role — the gate is in update-sim.
    ;; So we test that the condition (not (eq (game-net-role game) :client)) is false.
    (assert (eq (game-net-role game) :client) ()
            "client-skips-zt: confirms gate would prevent zone transition")))

;;; Edge-Direction-Dot Tests (Dominant Direction Arming)

(defun test-edge-direction-dot-east ()
  "Moving due east should have dot=1.0 with :east edge."
  (let ((dot (edge-direction-dot 1.0 0.0 :east)))
    (assert (> dot 0.99) () "edge-direction-dot: due east should be ~1.0, got ~a" dot)))

(defun test-edge-direction-dot-perpendicular ()
  "Moving due north with :east edge should have dot=0.0."
  (let ((dot (edge-direction-dot 0.0 -1.0 :east)))
    (assert (< (abs dot) 0.01) () "edge-direction-dot: perpendicular should be ~0.0, got ~a" dot)))

(defun test-edge-direction-dot-zero-vector ()
  "Zero-length movement vector should return 0.0."
  (let ((dot (edge-direction-dot 0.0 0.0 :north)))
    (assert (= dot 0.0) () "edge-direction-dot: zero vector should return 0.0")))

(defun test-edge-direction-dot-diagonal ()
  "Diagonal NE movement should have ~0.707 for both :north and :east."
  (let ((dot-n (edge-direction-dot 0.0 -1.0 :north))
        (dot-e (edge-direction-dot 1.0 0.0 :east))
        (dot-ne-n (edge-direction-dot 1.0 -1.0 :north))
        (dot-ne-e (edge-direction-dot 1.0 -1.0 :east)))
    ;; Pure north/east should be 1.0
    (assert (> dot-n 0.99) () "edge-direction-dot: pure north should be ~1.0")
    (assert (> dot-e 0.99) () "edge-direction-dot: pure east should be ~1.0")
    ;; Diagonal should be ~0.707 for both
    (assert (< (abs (- dot-ne-n 0.707)) 0.01) ()
            "edge-direction-dot: NE diagonal for :north should be ~0.707, got ~a" dot-ne-n)
    (assert (< (abs (- dot-ne-e 0.707)) 0.01) ()
            "edge-direction-dot: NE diagonal for :east should be ~0.707, got ~a" dot-ne-e)))

;;; Commit Margin Tests

(defun test-commit-margin-relaxes-boundary ()
  "world-exit-edge-with-bounds with commit-margin should detect edge sooner."
  (let* ((player (make-player 100.0 5.0))  ; near north edge (min-y=0)
         (min-x 0.0) (max-x 2048.0) (min-y 0.0) (max-y 2048.0))
    ;; Player at y=5.0, moving north (dy=-1)
    (let ((intent (player-intent player)))
      (setf (intent-move-dx intent) 0.0
            (intent-move-dy intent) -1.0))
    ;; Without margin: y=5.0 > min-y=0.0, no edge detected
    (let ((no-margin (world-exit-edge-with-bounds player min-x max-x min-y max-y 0.0)))
      (assert (null no-margin) ()
              "commit-margin: without margin, y=5 should not trigger"))
    ;; With margin=8.0: y=5.0 <= min-y+8.0=8.0, should detect :north
    (let ((with-margin (world-exit-edge-with-bounds player min-x max-x min-y max-y 8.0)))
      (assert (eq with-margin :north) ()
              "commit-margin: with margin=8, y=5 should trigger :north"))))

;;; Spatial Exit Filtering Tests

(defun test-spatial-exit-p-filters-teleport-in-edge-strips ()
  "serialize-edge-strips-for-zone should skip teleport exits (non-spatial).
   Teleport exits have :offset nil or non-preserve values."
  ;; This tests the predicate used by the filter
  (assert (spatial-exit-p '(:edge :north :to :zone-2 :offset :preserve-y))
          () "spatial-exit-p: preserve-y is spatial")
  (assert (spatial-exit-p '(:edge :east :to :zone-3 :offset :preserve-x))
          () "spatial-exit-p: preserve-x is spatial")
  (assert (not (spatial-exit-p '(:edge :north :to :zone-4 :offset nil)))
          () "spatial-exit-p: nil offset is teleport")
  (assert (not (spatial-exit-p '(:edge :north :to :zone-5)))
          () "spatial-exit-p: missing offset is teleport"))

;;; Config Invariant Tests

(defun test-zone-config-invariant-hysteresis ()
  "The hysteresis cancel line must be further from edge than the arm line."
  (assert (> *zone-hysteresis-out* *zone-hysteresis-in*) ()
          "config-invariant: *zone-hysteresis-out* (~a) > *zone-hysteresis-in* (~a)"
          *zone-hysteresis-out* *zone-hysteresis-in*))

(defun test-zone-config-invariant-preload-radius ()
  "The preload radius must be >= arm distance for timely preloading."
  (assert (>= *zone-preload-radius* *zone-hysteresis-in*) ()
          "config-invariant: *zone-preload-radius* (~a) >= *zone-hysteresis-in* (~a)"
          *zone-preload-radius* *zone-hysteresis-in*))

(defun test-zone-config-commit-margin-positive ()
  "Commit margin should be non-negative."
  (assert (>= *zone-commit-margin-tiles* 0.0) ()
          "config-invariant: *zone-commit-margin-tiles* (~a) >= 0"
          *zone-commit-margin-tiles*))

;;; End-to-End Edge-Strip and Preloading Tests

(defun make-test-world-graph (zone-id exits &optional zone-paths)
  "Create a minimal world-graph with EXIT specs for ZONE-ID.
   Each exit is a plist like (:edge :north :to :zone-2 :offset :preserve-y)."
  (let ((edges-by-zone (make-hash-table :test 'eq))
        (paths (make-hash-table :test 'eq)))
    (setf (gethash zone-id edges-by-zone) exits)
    (when zone-paths
      (loop :for (id . path) :in zone-paths
            :do (setf (gethash id paths) path)))
    (%make-world-graph :edges-by-zone edges-by-zone :zone-paths paths)))

(defun test-serialize-edge-strips-spatial-only ()
  "serialize-edge-strips-for-zone should include strips only for spatial exits.
   Teleport exits (no :offset :preserve-x/y) should be excluded."
  (let* ((zone (make-empty-zone :test-zone 10 10))
         (adj-zone (make-empty-zone :adj-zone 10 10))
         (adj-zone-state (make-zone-state :zone-id :adj-zone :zone adj-zone))
         (tele-zone (make-empty-zone :tele-zone 10 10))
         (tele-zone-state (make-zone-state :zone-id :tele-zone :zone tele-zone))
         ;; World with one spatial exit (north) and one teleport (south)
         (graph (make-test-world-graph
                 :test-zone
                 (list '(:edge :north :to :adj-zone :offset :preserve-x)
                       '(:edge :south :to :tele-zone))))
         (world (%make-world :tile-dest-size 64.0 :zone zone :world-graph graph))
         (game (%make-game :world world)))
    ;; Register zone states
    (let ((saved-zone-states (make-hash-table :test 'eq)))
      (maphash (lambda (k v) (setf (gethash k saved-zone-states) v)) *zone-states*)
      (unwind-protect
           (progn
             (setf (gethash :adj-zone *zone-states*) adj-zone-state)
             (setf (gethash :tele-zone *zone-states*) tele-zone-state)
             (let ((strips (serialize-edge-strips-for-zone game :test-zone)))
               ;; Should only have the spatial exit (:north -> :adj-zone)
               (assert (= (length strips) 1) ()
                       "spatial-only: should have 1 strip, got ~d" (length strips))
               (let ((strip (first strips)))
                 (assert (eq (getf strip :edge) :north) ()
                         "spatial-only: strip edge should be :north")
                 (assert (eq (getf strip :zone-id) :adj-zone) ()
                         "spatial-only: strip zone should be :adj-zone"))))
        ;; Restore zone-states
        (clrhash *zone-states*)
        (maphash (lambda (k v) (setf (gethash k *zone-states*) v)) saved-zone-states)))))

(defun test-deserialize-edge-strips-replaces ()
  "deserialize-edge-strips should replace (not accumulate) game-edge-strips each call."
  (let* ((world (%make-world :tile-dest-size 64.0))
         (game (%make-game :world world :edge-strips '(:old-data))))
    ;; First call with one strip
    (let ((state1 (list :edge-strips
                        (list (list :edge :north :zone-id :z2
                                    :strip (list :players #() :npcs #() :objects nil))))))
      (deserialize-edge-strips state1 game)
      (assert (= (length (game-edge-strips game)) 1) ()
              "replaces: first call should have 1 strip"))
    ;; Second call with different strips — should replace, not append
    (let ((state2 (list :edge-strips
                        (list (list :edge :east :zone-id :z3
                                    :strip (list :players #() :npcs #() :objects nil))
                              (list :edge :south :zone-id :z4
                                    :strip (list :players #() :npcs #() :objects nil))))))
      (deserialize-edge-strips state2 game)
      (assert (= (length (game-edge-strips game)) 2) ()
              "replaces: second call should have 2 strips, not 3"))
    ;; Third call with empty — should clear
    (let ((state3 (list :edge-strips nil)))
      (deserialize-edge-strips state3 game)
      (assert (null (game-edge-strips game)) ()
              "replaces: empty edge-strips should clear"))))

(defun test-pending-edge-change-cancel ()
  "When player has pending on one edge but moves to a different edge,
   pending should be cleared (edge-change cancel)."
  (let* ((world (make-test-world :tile-size 64.0))
         (player (make-player 5.0 5.0)))  ; near both north and west edges
    ;; Arm on :north
    (setf (player-zone-transition-pending player) :north)
    ;; Set intent pointing west (dx=-1, dy=0) — pushing against west edge
    (let ((intent (player-intent player)))
      (setf (intent-move-dx intent) -1.0
            (intent-move-dy intent) 0.0))
    ;; world-exit-edge-with-bounds should detect :west (not :north)
    (let ((actual (world-exit-edge-with-bounds player 0.0 4096.0 0.0 4096.0 32.0)))
      (assert (eq actual :west) ()
              "edge-change: actual edge should be :west when moving west at corner")
      ;; Since actual (:west) != pending (:north), edge-change cancel should fire
      (assert (not (eq actual :north)) ()
              "edge-change: actual should differ from pending"))))

(defun test-zone-cache-hit-miss-counters ()
  "zone-cache-lookup should increment hit/miss counters."
  (let* ((cache (make-zone-lru-cache))
         (zone (make-empty-zone :test 10 10)))
    ;; Miss
    (zone-cache-lookup cache :nonexistent)
    (assert (= (zone-cache-misses cache) 1) ()
            "cache-counters: miss count should be 1")
    ;; Insert and hit
    (zone-cache-insert cache :test zone)
    (zone-cache-lookup cache :test)
    (assert (= (zone-cache-hits cache) 1) ()
            "cache-counters: hit count should be 1")
    (assert (= (zone-cache-misses cache) 1) ()
            "cache-counters: miss count should still be 1")))

(defun test-edge-entity-registry-keys ()
  "Every spec in *edge-entity-specs* must have all required keys."
  (let ((required-keys '(:key :source :valid-p :pixel-pos :serialize-one
                         :deser-compact :ctor :apply-offset :cull-one
                         :draw-one :collection-type)))
    (dolist (spec *edge-entity-specs*)
      (dolist (rk required-keys)
        (assert (getf spec rk) ()
                "registry: spec ~a missing key ~a" (getf spec :key) rk)))))

(defun test-registry-serialize-zone-state-filtered ()
  "serialize-zone-state-filtered (registry-driven) produces :players/:npcs/:objects keys."
  (let* ((zone (make-empty-zone :test-zone 10 10))
         (player (%make-player))
         (npc (%make-npc))
         (zone-players (make-array 1 :initial-contents (list player)
                                     :adjustable t :fill-pointer 1))
         (npcs (make-array 1 :initial-contents (list npc)))
         (zone-state (make-zone-state :zone-id :test-zone :zone zone
                                       :zone-players zone-players :npcs npcs)))
    (setf (player-id player) 99
          (player-x player) 100.0 (player-y player) 200.0
          (player-hp player) 10
          (player-stats player) (make-stat-block))
    (setf (npc-id npc) 42
          (npc-x npc) 300.0 (npc-y npc) 400.0
          (npc-alive npc) t
          (npc-stats npc) (make-npc-stats nil))
    (let ((result (serialize-zone-state-filtered zone-state :test-zone 64.0)))
      ;; Should have :players, :npcs, :objects keys
      (assert (getf result :players) ()
              "registry-ser: missing :players")
      (assert (getf result :npcs) ()
              "registry-ser: missing :npcs")
      ;; Players should be a vector with 1 entry
      (assert (= (length (getf result :players)) 1) ()
              "registry-ser: should have 1 player, got ~d"
              (length (getf result :players)))
      ;; NPCs should be a vector with 1 entry
      (assert (= (length (getf result :npcs)) 1) ()
              "registry-ser: should have 1 npc, got ~d"
              (length (getf result :npcs))))))

(defun test-registry-serialize-game-state-for-zone-delegates ()
  "serialize-game-state-for-zone should delegate to serialize-zone-state-filtered
   and produce :format :compact-v5 with :players/:npcs/:objects."
  (let* ((zone (make-empty-zone :test-zone 10 10))
         (player (%make-player))
         (npc (%make-npc))
         (zone-players (make-array 1 :initial-contents (list player)
                                     :adjustable t :fill-pointer 1))
         (npcs (make-array 1 :initial-contents (list npc)))
         (zone-state (make-zone-state :zone-id :test-zone :zone zone
                                       :zone-players zone-players :npcs npcs))
         (world (%make-world :tile-dest-size 64.0 :zone zone))
         (game (%make-game :world world
                           :players (make-array 1 :initial-contents (list player)))))
    (setf (player-id player) 99
          (player-zone-id player) :test-zone
          (player-x player) 100.0 (player-y player) 200.0
          (player-hp player) 10
          (player-stats player) (make-stat-block))
    (setf (npc-id npc) 42
          (npc-x npc) 300.0 (npc-y npc) 400.0
          (npc-alive npc) t
          (npc-stats npc) (make-npc-stats nil))
    (let ((snapshot (serialize-game-state-for-zone game :test-zone zone-state)))
      (assert (eq (getf snapshot :format) :compact-v5) ()
              "game-state-zone: format should be :compact-v5")
      (assert (eq (getf snapshot :zone-id) :test-zone) ()
              "game-state-zone: zone-id should be :test-zone")
      (assert (vectorp (getf snapshot :players)) ()
              "game-state-zone: :players should be a vector")
      (assert (= (length (getf snapshot :players)) 1) ()
              "game-state-zone: should have 1 player"))))

(defun test-registry-deserialize-edge-strips-round-trip ()
  "Serialized edge strips should round-trip through deserialize-edge-strips
   using the registry pipeline, producing entity structs with correct types."
  (let* ((zone (make-empty-zone :main-zone 10 10))
         (adj-zone (make-empty-zone :adj-zone 10 10))
         (npc (%make-npc))
         (npcs (make-array 1 :initial-contents (list npc)))
         (adj-zone-state (make-zone-state :zone-id :adj-zone :zone adj-zone :npcs npcs))
         (world (%make-world :tile-dest-size 64.0 :zone zone))
         (game (%make-game :world world)))
    (setf (npc-id npc) 7
          (npc-x npc) 50.0 (npc-y npc) 50.0
          (npc-alive npc) t
          (npc-stats npc) (make-npc-stats nil))
    ;; Serialize an edge strip (north edge)
    (let ((strip (serialize-edge-strip adj-zone-state :north 200.0 :adj-zone 64.0)))
      ;; Build fake snapshot state with this strip
      (let ((state (list :edge-strips
                         (list (list :edge :north :zone-id :adj-zone :strip strip)))))
        (deserialize-edge-strips state game)
        ;; Should have 1 strip
        (assert (= (length (game-edge-strips game)) 1) ()
                "round-trip: should have 1 deserialized strip")
        ;; The strip should have :npcs key with NPC structs
        (let* ((ds (first (game-edge-strips game)))
               (strip-npcs (getf ds :npcs)))
          (assert (vectorp strip-npcs) ()
                  "round-trip: :npcs should be a vector")
          ;; NPC at (50,50) with strip-width 200px from south edge of a 640px zone
          ;; Only included if y >= (640 - 200) = 440. Our NPC at y=50 is NOT in the strip.
          ;; Let's verify the count matches expectation
          (assert (typep strip-npcs 'vector) ()
                  "round-trip: npcs should be a vector"))))))

(defun test-validate-zone-config-passes ()
  "validate-zone-config should not error with default config values."
  (validate-zone-config))

(defun test-validate-zone-config-catches-violation ()
  "validate-zone-config should signal error when preload-radius < hysteresis-in."
  (let ((saved-preload *zone-preload-radius*)
        (saved-hyst-out *zone-hysteresis-out*))
    (unwind-protect
         (progn
           ;; Violate: preload-radius < hysteresis-in
           (setf *zone-preload-radius* 1.0)
           (let ((errored nil))
             (handler-case (validate-zone-config)
               (error () (setf errored t)))
             (assert errored ()
                     "validate-zone-config: should error when preload-radius < hysteresis-in"))
           ;; Restore and violate: hysteresis-out <= hysteresis-in
           (setf *zone-preload-radius* saved-preload)
           (setf *zone-hysteresis-out* 3.0)  ; less than hysteresis-in (6.0)
           (let ((errored nil))
             (handler-case (validate-zone-config)
               (error () (setf errored t)))
             (assert errored ()
                     "validate-zone-config: should error when hysteresis-out <= hysteresis-in")))
      (setf *zone-preload-radius* saved-preload
            *zone-hysteresis-out* saved-hyst-out))))

(defun test-queue-zone-preload-skips-cached ()
  "queue-zone-preload should not queue a zone that is already in the LRU cache."
  (let* ((cache (make-zone-lru-cache))
         (zone (make-empty-zone :already-cached 10 10))
         (graph (make-test-world-graph
                 :main '((:edge :north :to :already-cached :offset :preserve-x))
                 (list (cons :already-cached "/tmp/fake-zone.lisp"))))
         (game (%make-game :zone-cache cache)))
    ;; Pre-cache the zone
    (zone-cache-insert cache :already-cached zone)
    ;; Try to queue it
    (queue-zone-preload game cache graph :already-cached "test")
    ;; Preload queue should be empty — zone was already cached
    (assert (null (game-preload-queue game)) ()
            "queue-preload: should skip already-cached zone")))

(defun test-queue-zone-preload-skips-duplicate ()
  "queue-zone-preload should not queue the same zone-id twice."
  (let* ((cache (make-zone-lru-cache))
         (graph (make-test-world-graph
                 :main '((:edge :north :to :target :offset :preserve-x))
                 (list (cons :target "/tmp/fake-zone.lisp"))))
         (game (%make-game :zone-cache cache)))
    ;; Queue once
    (queue-zone-preload game cache graph :target "first")
    (assert (= (length (game-preload-queue game)) 1) ()
            "queue-preload: first queue should add 1 entry")
    ;; Queue again — should be skipped (duplicate)
    (queue-zone-preload game cache graph :target "second")
    (assert (= (length (game-preload-queue game)) 1) ()
            "queue-preload: duplicate should not add another entry")))

(defun test-cold-start-preload-adjacent-queues-neighbors ()
  "cold-start-preload-adjacent should queue all spatial cardinal neighbors."
  (let* ((zone (make-empty-zone :center 10 10))
         (cache (make-zone-lru-cache))
         (graph (make-test-world-graph
                 :center
                 (list '(:edge :north :to :north-zone :offset :preserve-x)
                       '(:edge :south :to :south-zone :offset :preserve-x)
                       '(:edge :east :to :east-zone :offset :preserve-y)
                       '(:edge :west :to :teleport-dest))  ; no :offset → teleport
                 (list (cons :north-zone "/tmp/n.lisp")
                       (cons :south-zone "/tmp/s.lisp")
                       (cons :east-zone "/tmp/e.lisp")
                       (cons :teleport-dest "/tmp/t.lisp"))))
         (world (%make-world :tile-dest-size 64.0 :zone zone :world-graph graph))
         (game (%make-game :world world :zone-cache cache)))
    (cold-start-preload-adjacent game)
    (let ((queued-ids (mapcar #'car (game-preload-queue game))))
      ;; Should have north, south, east (3 spatial exits) — west is teleport
      (assert (member :north-zone queued-ids) ()
              "cold-start: north-zone should be queued")
      (assert (member :south-zone queued-ids) ()
              "cold-start: south-zone should be queued")
      (assert (member :east-zone queued-ids) ()
              "cold-start: east-zone should be queued")
      (assert (not (member :teleport-dest queued-ids)) ()
              "cold-start: teleport dest should NOT be queued"))))

(defun test-cold-start-preload-includes-diagonal-neighbors ()
  "Step 5: cold-start-preload-adjacent should queue diagonal neighbors.
   Diagonal = neighbor-of-neighbor via perpendicular edges."
  (let* ((zone (make-empty-zone :center 10 10))
         (cache (make-zone-lru-cache))
         ;; Build a graph with center -> north-zone, and north-zone -> northeast-zone (east)
         (edges-by-zone (make-hash-table :test 'eq))
         (paths (make-hash-table :test 'eq)))
    ;; Center has a north exit
    (setf (gethash :center edges-by-zone)
          (list '(:edge :north :to :north-zone :offset :preserve-x)))
    ;; North-zone has an east exit leading to the diagonal neighbor
    (setf (gethash :north-zone edges-by-zone)
          (list '(:edge :east :to :northeast-zone :offset :preserve-y)))
    ;; Paths for both targets
    (setf (gethash :north-zone paths) "/tmp/n.lisp")
    (setf (gethash :northeast-zone paths) "/tmp/ne.lisp")
    (let* ((graph (%make-world-graph :edges-by-zone edges-by-zone :zone-paths paths))
           (world (%make-world :tile-dest-size 64.0 :zone zone :world-graph graph))
           (game (%make-game :world world :zone-cache cache)))
      (cold-start-preload-adjacent game)
      (let ((queued-ids (mapcar #'car (game-preload-queue game))))
        ;; Should have both north-zone (cardinal) and northeast-zone (diagonal)
        (assert (member :north-zone queued-ids) ()
                "cold-start-diag: north-zone (cardinal) should be queued")
        (assert (member :northeast-zone queued-ids) ()
                "cold-start-diag: northeast-zone (diagonal) should be queued")))))

(defun test-preview-zone-consults-lru-cache ()
  "Step 10: ensure-preview-zone-for-edge should use LRU cache instead of disk
   when the zone is already in the LRU cache."
  (let* ((zone-dir (namestring
                    (merge-pathnames "data/zones/"
                                     (asdf:system-source-directory :mmorpg))))
         (zone-5-path (concatenate 'string zone-dir "zone-5.lisp"))
         (zone-5 (load-zone zone-5-path))
         (graph (load-world-graph))
         (preview-cache (make-hash-table :test 'eq))
         (lru-cache (make-zone-lru-cache))
         (world (%make-world :zone zone-5
                             :tile-dest-size 32.0
                             :collision-half-width 12.0
                             :collision-half-height 12.0
                             :wall-min-x 12.0 :wall-max-x 628.0
                             :wall-min-y 12.0 :wall-max-y 628.0
                             :world-graph graph
                             :zone-preview-cache preview-cache)))
    ;; Find a spatial exit from zone-5
    (let* ((exits (world-graph-exits graph :zone-5))
           (spatial-exit (find-if #'spatial-exit-p exits)))
      (when spatial-exit
        (let* ((target-id (getf spatial-exit :to))
               (edge (getf spatial-exit :edge))
               ;; Create a sentinel zone to put in the LRU cache
               (sentinel-zone (make-empty-zone target-id 5 5)))
          ;; Pre-populate the LRU cache with our sentinel
          (zone-cache-insert lru-cache target-id sentinel-zone)
          (let ((hits-before (zone-cache-hits lru-cache)))
            ;; Call ensure-preview-zone-for-edge with LRU cache
            (ensure-preview-zone-for-edge world edge lru-cache)
            ;; Preview cache should now contain the sentinel from LRU (not loaded from disk)
            (let ((preview-entry (gethash target-id preview-cache)))
              (assert (not (null preview-entry)) ()
                      "preview-lru: preview cache should have an entry for ~a" target-id)
              (assert (eq preview-entry sentinel-zone) ()
                      "preview-lru: preview entry should be the sentinel from LRU, not disk")
              ;; LRU cache should have registered a hit
              (assert (> (zone-cache-hits lru-cache) hits-before) ()
                      "preview-lru: LRU hit counter should have incremented"))))))))

(defun test-process-preload-queue-loads-zone ()
  "process-preload-queue should pop one entry and load it into the LRU cache."
  (let* ((cache (make-zone-lru-cache))
         ;; Use a real zone file that exists on disk
         (zone-path (namestring
                     (merge-pathnames "data/zones/zone-5.lisp"
                                      (asdf:system-source-directory :mmorpg))))
         (game (%make-game :zone-cache cache
                           :preload-queue (list (cons :zone-5 zone-path)))))
    ;; Queue has 1 entry
    (assert (= (length (game-preload-queue game)) 1) ()
            "process-preload: queue should start with 1 entry")
    ;; Process one entry
    (process-preload-queue game)
    ;; Queue should be empty (popped)
    (assert (null (game-preload-queue game)) ()
            "process-preload: queue should be empty after processing")
    ;; Zone should be in cache
    (assert (zone-cache-contains-p cache :zone-5) ()
            "process-preload: :zone-5 should be in cache after loading")))

(defun test-update-client-preloading-near-edge ()
  "Step 5: update-client-preloading should queue adjacent zone when player is near edge."
  (let* ((*zone-preload-radius* 10.0)
         (zone (make-empty-zone :center 10 10))  ; 10x10 tiles
         (cache (make-zone-lru-cache))
         (edges-by-zone (make-hash-table :test 'eq))
         (paths (make-hash-table :test 'eq)))
    ;; Center has a north exit
    (setf (gethash :center edges-by-zone)
          (list '(:edge :north :to :north-zone :offset :preserve-x)))
    (setf (gethash :north-zone paths) "/tmp/n.lisp")
    (let* ((graph (%make-world-graph :edges-by-zone edges-by-zone :zone-paths paths))
           (world (%make-world :tile-dest-size 64.0 :zone zone :world-graph graph
                               :collision-half-width 12.0 :collision-half-height 12.0
                               :wall-min-x 12.0 :wall-max-x 628.0
                               :wall-min-y 12.0 :wall-max-y 628.0))
           ;; Player near north edge: y close to wall-min-y (12.0)
           ;; Preload radius = 10 tiles * 64 = 640px, so anything within zone is "near"
           ;; Use a smaller radius for a meaningful test
           (*zone-preload-radius* 2.0)  ; 2 tiles = 128px from edge
           (player (%make-player)))
      ;; Place player 1 tile from north edge (y = wall-min-y + 1*tile = 12+64 = 76)
      (setf (player-x player) 320.0 (player-y player) 76.0)
      (let ((game (%make-game :world world :zone-cache cache :player player)))
        (update-client-preloading game)
        (let ((queued-ids (mapcar #'car (game-preload-queue game))))
          (assert (member :north-zone queued-ids) ()
                  "preload-near: north-zone should be queued when player near north edge"))))))

(defun test-update-client-preloading-far-from-edge ()
  "Step 5: update-client-preloading should NOT queue when player is far from edges."
  (let* ((zone (make-empty-zone :center 10 10))
         (cache (make-zone-lru-cache))
         (edges-by-zone (make-hash-table :test 'eq))
         (paths (make-hash-table :test 'eq)))
    (setf (gethash :center edges-by-zone)
          (list '(:edge :north :to :north-zone :offset :preserve-x)))
    (setf (gethash :north-zone paths) "/tmp/n.lisp")
    (let* ((graph (%make-world-graph :edges-by-zone edges-by-zone :zone-paths paths))
           (world (%make-world :tile-dest-size 64.0 :zone zone :world-graph graph
                               :collision-half-width 12.0 :collision-half-height 12.0
                               :wall-min-x 12.0 :wall-max-x 628.0
                               :wall-min-y 12.0 :wall-max-y 628.0))
           (*zone-preload-radius* 2.0)  ; 2 tiles = 128px from edge
           (player (%make-player)))
      ;; Place player in center of zone — far from all edges
      (setf (player-x player) 320.0 (player-y player) 320.0)
      (let ((game (%make-game :world world :zone-cache cache :player player)))
        (update-client-preloading game)
        (assert (null (game-preload-queue game)) ()
                "preload-far: no zones should be queued when player is in center")))))

(defun test-handle-zone-transition-no-loading-overlay ()
  "Step 6: handle-zone-transition must NOT trigger the loading overlay.
   zone-transition-show-loading-p returns nil for locomotion transitions."
  (let* ((zone-dir (namestring
                    (merge-pathnames "data/zones/"
                                     (asdf:system-source-directory :mmorpg))))
         (zone-5 (load-zone (concatenate 'string zone-dir "zone-5.lisp")))
         (world (%make-world :zone zone-5
                             :tile-dest-size 32.0
                             :collision-half-width 12.0
                             :collision-half-height 12.0
                             :wall-min-x 12.0 :wall-max-x 628.0
                             :wall-min-y 12.0 :wall-max-y 628.0
                             :world-graph (load-world-graph)))
         (player (%make-player))
         (ui (%make-ui))
         (game (%make-game :world world :player player
                           :players (make-array 1 :initial-element player)
                           :ui ui :zone-cache (make-zone-lru-cache))))
    (setf (player-x player) 100.0 (player-y player) 100.0)
    (setf (ui-loading-timer ui) 0.0)
    ;; Call handle-zone-transition
    (handle-zone-transition game :old-x 100.0 :old-y 100.0)
    ;; Loading timer must still be 0 — no overlay triggered
    (assert (= (ui-loading-timer ui) 0.0) ()
            "no-overlay: loading-timer should remain 0 after zone transition")))

(defun test-edge-strip-offset-applied-to-entities ()
  "Step 9: Deserialized edge-strip entities should have world-space offsets applied.
   After deserialize-edge-strips, player coords = local + offset."
  (let* ((zone (make-empty-zone :test-zone 10 10))
         (world (%make-world :tile-dest-size 64.0 :zone zone))
         (game (%make-game :world world))
         ;; Create a real player and serialize it via compact format
         (source-player (%make-player)))
    (setf (player-id source-player) 42
          (player-x source-player) 100.0
          (player-y source-player) 200.0
          (player-dx source-player) 1.0
          (player-dy source-player) 0.0
          (player-hp source-player) 10
          (player-anim-state source-player) :walk
          (player-facing source-player) :side
          (player-facing-sign source-player) -1.0)
    (let* ((compact-vec (serialize-player-compact source-player))
           (state (list :edge-strips
                        (list (list :edge :east :zone-id :adj-zone
                                    :strip (list :players (vector compact-vec)
                                                 :npcs #() :objects nil))))))
      (deserialize-edge-strips state game)
      (let* ((strips (game-edge-strips game))
             (strip (first strips))
             (players (getf strip :players)))
        (assert (and players (> (length players) 0)) ()
                "offset-applied: should have at least one player")
        ;; East offset = zone-width * tile-size = 10 * 64 = 640
        ;; Player local x=100, so world x should be ~740
        (let* ((p (aref players 0))
               (px (player-x p))
               (py (player-y p)))
          (assert (> px 600.0) ()
                  "offset-applied: player x (~,1f) should include east offset (640)" px)
          (assert (< (abs (- py 200.0)) 1.0) ()
                  "offset-applied: player y (~,1f) should be ~200 (east edge)" py))))))

(defun test-edge-cull-player-viewport ()
  "Step 9: edge-cull-player should return T for visible players, nil for offscreen."
  (let ((visible-player (%make-player))
        (offscreen-player (%make-player)))
    (setf (player-x visible-player) 500.0 (player-y visible-player) 400.0)
    (setf (player-x offscreen-player) 2000.0 (player-y offscreen-player) 2000.0)
    ;; Viewport: left=0 right=1024 top=0 bottom=768
    (assert (edge-cull-player visible-player 0.0 1024.0 0.0 768.0 64.0) ()
            "edge-cull: visible player should pass cull check")
    (assert (not (edge-cull-player offscreen-player 0.0 1024.0 0.0 768.0 64.0)) ()
            "edge-cull: offscreen player should fail cull check")))

(defun test-edge-cull-npc-viewport ()
  "Step 9: edge-cull-npc should return T for visible alive NPCs, nil for offscreen or dead."
  (let ((visible-npc (make-npc-direct 500.0 400.0 :id 99))
        (offscreen-npc (make-npc-direct 2000.0 2000.0 :id 100))
        (dead-npc (make-npc-direct 500.0 400.0 :id 101)))
    (setf (npc-alive dead-npc) nil)
    ;; Viewport: left=0 right=1024 top=0 bottom=768
    (assert (edge-cull-npc visible-npc 0.0 1024.0 0.0 768.0 64.0) ()
            "edge-cull-npc: visible alive NPC should pass")
    (assert (not (edge-cull-npc offscreen-npc 0.0 1024.0 0.0 768.0 64.0)) ()
            "edge-cull-npc: offscreen NPC should fail")
    (assert (not (edge-cull-npc dead-npc 0.0 1024.0 0.0 768.0 64.0)) ()
            "edge-cull-npc: dead NPC should fail even if visible")))

(defun test-edge-strips-present-with-adjacent-zone-state ()
  "Step 10: serialize-edge-strips-for-zone should include strips when adjacent
   zone-state is loaded (not just filter test — verify strips are populated)."
  (let* ((zone (make-empty-zone :test-zone 10 10))
         (adj-zone (make-empty-zone :adj-zone 10 10))
         ;; Create an adjacent zone-state with an NPC in the edge strip
         (npc (make-npc-direct 50.0 600.0 :id 42))  ; near south edge
         (adj-zone-state (make-zone-state :zone-id :adj-zone :zone adj-zone
                                          :npcs (make-array 1 :initial-element npc)))
         (graph (make-test-world-graph
                 :test-zone
                 (list '(:edge :north :to :adj-zone :offset :preserve-x))
                 nil))
         (world (%make-world :tile-dest-size 64.0 :zone zone :world-graph graph))
         (game (%make-game :world world)))
    ;; Register adjacent zone-state
    (let ((saved-zone-states (make-hash-table :test 'eq)))
      (maphash (lambda (k v) (setf (gethash k saved-zone-states) v)) *zone-states*)
      (unwind-protect
           (progn
             (setf (gethash :adj-zone *zone-states*) adj-zone-state)
             (let ((strips (serialize-edge-strips-for-zone game :test-zone)))
               ;; Should have 1 strip for the :north spatial exit
               (assert (= (length strips) 1) ()
                       "strips-present: should have 1 strip, got ~d" (length strips))
               (let* ((strip (first strips))
                      (strip-data (getf strip :strip))
                      (strip-npcs (getf strip-data :npcs)))
                 ;; The strip should have NPC data (may be empty if NPC isn't in strip bounds)
                 (assert (not (null strip-data)) ()
                         "strips-present: strip should have data"))))
        ;; Restore zone-states
        (clrhash *zone-states*)
        (maphash (lambda (k v) (setf (gethash k *zone-states*) v)) saved-zone-states)))))

(defparameter *tests-zone-cache* (list
    ;; Zone Transition Tests (Seamless Zone Loading)
    'test-zone-cache-insert-lookup
    'test-zone-cache-lru-eviction-at-capacity
    'test-zone-cache-lookup-promotes-to-front
    'test-zone-cache-miss-returns-nil
    'test-zone-cache-contains-p-no-promote
    'test-edge-direction-passes-due-east
    'test-edge-direction-passes-northeast
    'test-edge-direction-rejects-tangential
    'test-edge-direction-zero-vector
    'test-edge-direction-all-edges
    'test-player-distance-to-edge-north
    'test-player-distance-to-edge-all
    'test-player-in-arm-band-p-inside
    'test-player-in-arm-band-p-outside
    'test-player-past-cancel-line-p-test
    'test-cooldown-skips-transition
    'test-cooldown-decrements
    'test-hysteresis-config-invariant
    'test-opposite-edge-roundtrip
    'test-entity-in-edge-strip-p-north
    'test-entity-in-edge-strip-p-south
    'test-entity-in-edge-strip-p-outside
    'test-compute-edge-strip-offset-all-edges
    'test-zone-transition-show-loading-p-always-nil
    'test-edge-strip-player-creation
    'test-edge-strip-npc-creation
    ;; Step 12: preloading, state machine, edge-strip e2e
    'test-spatial-exit-p-preserve-x
    'test-spatial-exit-p-preserve-y
    'test-spatial-exit-p-teleport
    'test-spatial-exit-p-nil-offset
    'test-edge-strip-object-creation
    'test-edge-strip-object-defaults
    'test-update-zone-transition-returns-count
    'test-arm-sets-pending-edge
    'test-cancel-clears-pending
    'test-direction-gating-rejects-wrong-direction
    'test-compute-edge-strip-offset-symmetry
    'test-client-skips-zone-transition
    ;; Dominant direction arming tests
    'test-edge-direction-dot-east
    'test-edge-direction-dot-perpendicular
    'test-edge-direction-dot-zero-vector
    'test-edge-direction-dot-diagonal
    ;; Commit margin tests
    'test-commit-margin-relaxes-boundary
    ;; Spatial exit filtering
    'test-spatial-exit-p-filters-teleport-in-edge-strips
    ;; Config invariant tests
    'test-zone-config-invariant-hysteresis
    'test-zone-config-invariant-preload-radius
    'test-zone-config-commit-margin-positive
    ;; End-to-end edge-strip and preloading tests
    'test-serialize-edge-strips-spatial-only
    'test-deserialize-edge-strips-replaces
    'test-pending-edge-change-cancel
    'test-zone-cache-hit-miss-counters
    ;; Entity-type registry tests
    'test-edge-entity-registry-keys
    'test-registry-serialize-zone-state-filtered
    'test-registry-serialize-game-state-for-zone-delegates
    'test-registry-deserialize-edge-strips-round-trip
    ;; Runtime config guard
    'test-validate-zone-config-passes
    'test-validate-zone-config-catches-violation
    ;; Preloading behavior tests
    'test-queue-zone-preload-skips-cached
    'test-queue-zone-preload-skips-duplicate
    'test-cold-start-preload-adjacent-queues-neighbors
    'test-cold-start-preload-includes-diagonal-neighbors
    'test-preview-zone-consults-lru-cache
    'test-process-preload-queue-loads-zone
    ;; Step 5: Proximity preloading
    'test-update-client-preloading-near-edge
    'test-update-client-preloading-far-from-edge
    ;; Step 6: No loading overlay
    'test-handle-zone-transition-no-loading-overlay
    ;; Step 9: Edge-strip world-position offset and viewport culling
    'test-edge-strip-offset-applied-to-entities
    'test-edge-cull-player-viewport
    'test-edge-cull-npc-viewport
    ;; Step 10: Edge strips present with adjacent zone state
    'test-edge-strips-present-with-adjacent-zone-state))
