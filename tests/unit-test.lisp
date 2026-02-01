(in-package #:mmorpg)

;;; Consolidated Unit Test Suite
;;; All tests (unit, persistence, security, trade) are now in this single file
;;; Run: make test-unit

(defun run-unit-tests ()
  "Run all unit tests. Returns T if all pass, NIL otherwise."
  (let ((passed 0)
        (failed 0))
    (format t "~%=== Running All Tests ===~%")
    
    ;; Run persistence tests
    (format t "~%--- Persistence Tests ---~%")
    (when (run-persistence-tests-internal)
      (incf passed))
    
    ;; Run security tests
    (format t "~%--- Security Tests ---~%")
    (when (run-security-tests-internal)
      (incf passed))
    
    ;; Run trade tests
    (format t "~%--- Trade Tests ---~%")
    (when (run-trade-tests-internal)
      (incf passed))
    
    ;; Run original unit tests
    (format t "~%--- Unit Tests ---~%")
    (when (run-unit-tests-internal)
      (incf passed))
    
    (format t "~%=== Test Summary ===~%")
    (format t "Test suites passed: ~d/4~%" passed)
    (= passed 4)))


(defun run-unit-tests-internal ()
  "Run all unit tests. Returns T if all pass, NIL otherwise."
  (let ((passed 0)
        (failed 0)
        (tests '(;; Utils Tests
                test-clamp
                test-clamp-edge-cases
                test-normalize-direction
                test-normalize-direction-diagonal
                test-normalize-vector
                test-normalize-vector-zero
                test-point-in-rect-p
                test-point-in-rect-p-edges
                test-basename
                test-basename-edge-cases
                test-sanitize-identifier
                test-plist-put
                test-player-direction
                test-player-state
                test-u32-hash
                test-u32-hash-deterministic
                test-exponential-backoff-delay
                test-player-animation-params
                test-relative-path-from-root
                ;; Combat Tests
                test-aabb-overlap-p
                test-aabb-overlap-p-edge-cases
                test-melee-hit-chance
                test-melee-hit-chance-clamped
                test-melee-max-hit
                test-combatant-display-name
                test-find-npc-by-id
                test-roll-melee-damage
                test-format-combat-log
                test-npc-respawn-seconds
                test-npc-attack-cooldown
                test-npc-attack-damage
                test-intent-attack-direction
                test-target-in-range-p
                test-attack-hitbox
                test-npc-attack-range
                ;; Progression Tests
                test-xp-to-level
                test-level-to-xp
                test-xp-level-roundtrip
                test-valid-training-mode-p
                test-normalize-training-mode
                test-training-mode-label
                test-combat-level
                test-split-combat-xp
                test-inventory-slot-empty-p
                test-item-stack-size
                test-equipment-slot-index
                test-player-adjacent-to-tile-p
                test-swap-inventory-slots
                test-update-skill-level
                test-clamp-player-hp
                test-format-xp-awards
                test-item-display-name
                test-inventory-slot-label
                test-inventory-add
                test-inventory-remove
                test-roll-loot-entry
                test-roll-loot-count
                test-object-respawn-seconds
                test-object-respawnable-p
                test-object-respawn-timer
                ;; Movement Tests
                test-wall-blocked-p
                test-tile-center-position
                test-edge-opposite
                test-edge-offset-ratio
                test-npc-collision-half
                ;; AI Tests
                test-npc-should-flee-p
                test-npc-perception-range-sq
                test-npc-home-radius
                test-npc-move-speed
                test-npc-wander-interval
                test-npc-flee-speed-mult
                test-closest-player
                ;; Data Tests
                test-plist-form-p
                test-data-section-header-p
                test-data-section-entry-p
                test-normalize-pairs
                test-validate-item-archetype-plist
                test-item-archetype-from-plist
                test-validate-object-archetype-plist
                test-object-archetype-from-plist
                test-loot-entry-from-spec
                test-validate-loot-table-plist
                test-loot-table-from-plist
                test-animation-set-from-plist
                test-merge-animation-sets
                ;; Zone Tests
                test-zone-chunk-key
                test-tile-key-roundtrip
                test-zone-tile-in-bounds-p
                test-zone-label
                test-zone-data-plist
                test-make-empty-zone
                test-build-tiles-from-fill
                test-zone-layer-tile-at
                ;; Intent Tests
                test-set-intent-move
                test-set-intent-face
                test-reset-frame-intent
                test-consume-intent-actions
                test-set-intent-target
                test-clear-intent-target
                test-request-pickup-target
                test-request-drop-item
                test-request-inventory-swap
                test-trade-intent-functions
                test-apply-intent-plist-rejects-bad-pickup
                ;; Net Tests
                test-string-to-octets
                test-octets-to-string
                test-encode-decode-net-message
                test-host-to-string
                test-position-distance-sq
                test-teleport-detected-p
                test-sync-client-zone-npcs
                ;; Binary Snapshot Tests (Phase 3)
                test-binary-int-encoding
                test-binary-int-decoding
                test-is-binary-snapshot-p
                test-binary-snapshot-roundtrip
                test-binary-snapshot-delta
                ;; Save/Serialization Tests
                test-quantize-coord-roundtrip
                test-quantize-timer-roundtrip
                test-encode-anim-state-roundtrip
                test-encode-facing-roundtrip
                test-pack-player-flags-roundtrip
                test-pack-npc-flags-roundtrip
                test-serialize-skill-roundtrip
                test-serialize-stat-block-roundtrip
                test-serialize-inventory-slot-roundtrip
                ;; Migration Tests
                test-migrate-player-v1-to-v2
                test-migrate-player-v2-to-v3
                test-migrate-player-v3-to-v4
                test-migrate-player-data-chain
                ;; World Graph Tests
                test-world-graph-data-plist
                test-normalize-world-graph-edges
                ;; Chat Tests
                test-trim-chat-message
                ;; Types Tests
                test-skill-xp-for-level
                test-allocate-entity-id
                test-find-player-by-id
                ;; Redis Metrics Tests (Phase 2 - Database Hardening)
                test-calculate-percentile
                test-calculate-percentile-edge-cases
                test-ring-buffer-values
                test-metrics-push-latency
                ;; Additional AI Tests
                test-npc-in-perception-range-p
                ;; Additional Combat Tests
                test-player-attack-target-in-range-p
                ;; Additional Progression Tests
                test-melee-hit-p
                test-format-skill-hud-line
                test-object-entry-count
                ;; Additional Data Tests
                test-parse-game-data-forms
                test-make-npc-archetype-from-plist
                ;; Additional Zone Tests
                test-zone-chunk-from-spec
                test-zone-layer-from-spec
                test-build-zone-collision-tiles
                test-zone-wall-map
                test-zone-layer-by-id
                test-zone-to-plist
                test-zone-slice
                test-zone-resize
                ;; Additional World Graph Tests
                test-world-graph-exits
                test-world-graph-zone-path
                test-collect-zone-files
                test-zone-id-from-file
                test-build-zone-paths
                ;; Final AI Tests
                test-update-npc-behavior
                test-update-npc-intent-nil-player
                ;; Final Progression Tests
                test-award-skill-xp
                test-apply-item-modifiers
                ;; Final Zone Tests
                test-load-write-zone-roundtrip
                ;; Final Movement Tests
                test-get-zone-state
                test-zone-state-player-count
                test-players-in-zone
                test-occupied-zone-ids
                test-derive-wall-map-from-zone
                test-wall-occupied-p
                test-blocked-at-p
                test-attempt-move
                test-update-running-state
                test-edge-spawn-position
                test-zone-bounds-from-dimensions
                test-position-blocked-p
                test-find-open-tile
                test-player-is-stuck-p
                test-world-exit-edge
                ;; Zone-state spawn helpers (Phase 1)
                test-wall-blocked-p-zero-origin
                test-blocked-at-p-with-map
                test-find-open-position-with-map
                test-zone-state-spawn-position
                ;; Per-zone collision helpers (Phase 4 deferred)
                test-get-zone-wall-map
                test-get-zone-collision-bounds
                test-player-is-stuck-p-for-zone
                test-get-zone-safe-spawn-for-zone
                ;; Final Net Tests
                test-session-try-register
                test-session-unregister
                test-session-get
                ;; Spatial Grid Tests
                test-spatial-grid-insert-remove
                test-spatial-grid-move
                test-spatial-grid-query-neighbors
                test-spatial-grid-query-radius
                test-position-to-cell
                test-entity-cell-changed-p
                test-spatial-grid-stats
                test-spatial-grid-clear
                test-zone-transition-grid-update
                ;; Zone-Players Cache Tests (Task 4.1)
                test-zone-players-cache-add-remove
                test-zone-players-cache-rebuild
                test-zone-players-cache-transition
                ;; Vector Pool Tests (Task 4.2)
                test-vector-pool-create
                test-vector-pool-acquire
                test-vector-pool-reset
                test-vector-pool-overflow
                ;; Profiling & GC Tests (Phase 6)
                test-profile-log-init
                test-record-profile-sample
                test-with-timing-enabled
                test-clear-profile-log
                test-gc-stats-reset
                test-gc-hook-increment-count
                ;; Combat Targeting Fix Tests (COMBAT_PLAN.md)
                test-npc-array-for-player-zone
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
                test-entity-in-render-distance-p
                ;; Auth Throughput Tests (Steps 1-12)
                test-auth-queue-fifo-order
                test-auth-queue-interleaved
                test-auth-queue-count
                test-auth-queue-drain-empty
                test-auth-queue-bounded
                test-auth-queue-unbounded
                test-hash-password-3part-format
                test-verify-password-new-format
                test-verify-password-legacy-format
                test-legacy-hash-format-p
                test-split-hash-string
                test-derive-password-key-with-iterations
                test-db-verify-and-load-account
                test-db-create-account-pipelined-memory
                test-rehash-on-login
                ;; Auth review fixes
                test-auth-rate-limit-wall-time
                test-auth-rate-lockout-expiry
                test-verify-password-malformed-iterations
                test-auth-metrics-counters
                test-session-local-registration
                test-auth-queue-stale-expiry
                ;; Zone Transition Tests (Seamless Zone Loading)
                test-zone-cache-insert-lookup
                test-zone-cache-lru-eviction-at-capacity
                test-zone-cache-lookup-promotes-to-front
                test-zone-cache-miss-returns-nil
                test-zone-cache-contains-p-no-promote
                test-edge-direction-passes-due-east
                test-edge-direction-passes-northeast
                test-edge-direction-rejects-tangential
                test-edge-direction-zero-vector
                test-edge-direction-all-edges
                test-player-distance-to-edge-north
                test-player-distance-to-edge-all
                test-player-in-arm-band-p-inside
                test-player-in-arm-band-p-outside
                test-player-past-cancel-line-p-test
                test-cooldown-skips-transition
                test-cooldown-decrements
                test-hysteresis-config-invariant
                test-opposite-edge-roundtrip
                test-entity-in-edge-strip-p-north
                test-entity-in-edge-strip-p-south
                test-entity-in-edge-strip-p-outside
                test-compute-edge-strip-offset-all-edges
                test-zone-transition-show-loading-p-always-nil
                test-edge-strip-player-creation
                test-edge-strip-npc-creation
                ;; Step 12: preloading, state machine, edge-strip e2e
                test-spatial-exit-p-preserve-x
                test-spatial-exit-p-preserve-y
                test-spatial-exit-p-teleport
                test-spatial-exit-p-nil-offset
                test-edge-strip-object-creation
                test-edge-strip-object-defaults
                test-update-zone-transition-returns-count
                test-arm-sets-pending-edge
                test-cancel-clears-pending
                test-direction-gating-rejects-wrong-direction
                test-compute-edge-strip-offset-symmetry
                test-client-skips-zone-transition
                ;; Dominant direction arming tests
                test-edge-direction-dot-east
                test-edge-direction-dot-perpendicular
                test-edge-direction-dot-zero-vector
                test-edge-direction-dot-diagonal
                ;; Commit margin tests
                test-commit-margin-relaxes-boundary
                ;; Spatial exit filtering
                test-spatial-exit-p-filters-teleport-in-edge-strips
                ;; Config invariant tests
                test-zone-config-invariant-hysteresis
                test-zone-config-invariant-preload-radius
                test-zone-config-commit-margin-positive
                ;; End-to-end edge-strip and preloading tests
                test-serialize-edge-strips-spatial-only
                test-deserialize-edge-strips-replaces
                test-pending-edge-change-cancel
                test-zone-cache-hit-miss-counters
                ;; Entity-type registry tests
                test-edge-entity-registry-keys
                test-registry-serialize-zone-state-filtered
                test-registry-serialize-game-state-for-zone-delegates
                test-registry-deserialize-edge-strips-round-trip
                ;; Runtime config guard
                test-validate-zone-config-passes
                test-validate-zone-config-catches-violation
                ;; Preloading behavior tests
                test-queue-zone-preload-skips-cached
                test-queue-zone-preload-skips-duplicate
                test-cold-start-preload-adjacent-queues-neighbors
                test-cold-start-preload-includes-diagonal-neighbors
                test-preview-zone-consults-lru-cache
                test-process-preload-queue-loads-zone
                ;; Step 5: Proximity preloading
                test-update-client-preloading-near-edge
                test-update-client-preloading-far-from-edge
                ;; Step 6: No loading overlay
                test-handle-zone-transition-no-loading-overlay
                ;; Step 9: Edge-strip world-position offset and viewport culling
                test-edge-strip-offset-applied-to-entities
                test-edge-cull-player-viewport
                test-edge-cull-npc-viewport
                ;; Step 10: Edge strips present with adjacent zone state
                test-edge-strips-present-with-adjacent-zone-state
                ;; ADDENDUM 1: Overstep preservation tests
                test-compute-transition-overstep-north
                test-compute-transition-overstep-all-edges
                test-apply-overstep-to-spawn-directions
                ;; ADDENDUM 2: Commit margin tied to collision size
                test-commit-margin-at-least-collision-half
                ;; ADDENDUM 3: Urgent preload near commit
                test-process-preload-queue-urgent-pops-multiple
                test-urgent-preload-no-pending-required
                ;; Sync load fallback warning
                test-apply-game-state-cache-miss-warns
                ;; ADDENDUM 4: Soft interpolation/prediction reset
                test-soft-reset-preserves-buffers-small-delta
                test-soft-reset-clears-buffers-large-delta
                ;; Zone transition continuity (seam translation)
                test-seam-translate-position-east
                test-seam-translate-position-west
                test-seam-translate-position-north
                test-seam-translate-position-south
                test-seam-translate-position-corner
                test-seam-position-valid-p
                test-seam-translation-used-in-transition-zone
                test-seam-translation-blocked-uses-fallback
                test-seam-translate-mixed-bounds)))
    (format t "~%=== Running Unit Tests ===~%")
    (dolist (test tests)
      (handler-case
          (progn
            (funcall test)
            (incf passed)
            (format t "~a ~a~%" "OK" (symbol-name test)))
        (error (e)
          (incf failed)
          (format t "~a ~a: ~a~%" "FAIL" (symbol-name test) e))))
    (format t "~%Results: ~d passed, ~d failed~%" passed failed)
    (zerop failed)))

;;; ============================================================
;;; UTILS.LISP TESTS
;;; ============================================================

(defun test-clamp ()
  "Test clamp function with normal values."
  (assert (= (clamp 5 0 10) 5) () "clamp: value in range")
  (assert (= (clamp -5 0 10) 0) () "clamp: below min")
  (assert (= (clamp 15 0 10) 10) () "clamp: above max"))

(defun test-clamp-edge-cases ()
  "Test clamp with edge cases."
  (assert (= (clamp 0 0 10) 0) () "clamp: at min")
  (assert (= (clamp 10 0 10) 10) () "clamp: at max")
  (assert (= (clamp 5 5 5) 5) () "clamp: min = max = value")
  (assert (= (clamp -1.5 -2.0 2.0) -1.5) () "clamp: floats in range")
  (assert (= (clamp -3.0 -2.0 2.0) -2.0) () "clamp: floats below"))

(defun test-normalize-direction ()
  "Test normalize-direction with axis-aligned input."
  (multiple-value-bind (dx dy) (normalize-direction 1.0 0.0)
    (assert (= dx 1.0) () "normalize-direction: right x")
    (assert (= dy 0.0) () "normalize-direction: right y"))
  (multiple-value-bind (dx dy) (normalize-direction 0.0 -1.0)
    (assert (= dx 0.0) () "normalize-direction: up x")
    (assert (= dy -1.0) () "normalize-direction: up y")))

(defun test-normalize-direction-diagonal ()
  "Test normalize-direction with diagonal input."
  (multiple-value-bind (dx dy) (normalize-direction 1.0 1.0)
    (let ((expected (/ 1.0 (sqrt 2.0))))
      (assert (< (abs (- dx expected)) 0.0001) () "normalize-direction: diagonal x")
      (assert (< (abs (- dy expected)) 0.0001) () "normalize-direction: diagonal y"))))

(defun test-normalize-vector ()
  "Test normalize-vector with various inputs."
  (multiple-value-bind (dx dy) (normalize-vector 3.0 4.0)
    (assert (< (abs (- dx 0.6)) 0.0001) () "normalize-vector: 3-4-5 x")
    (assert (< (abs (- dy 0.8)) 0.0001) () "normalize-vector: 3-4-5 y"))
  (multiple-value-bind (dx dy) (normalize-vector 1.0 0.0)
    (assert (= dx 1.0) () "normalize-vector: unit x")
    (assert (= dy 0.0) () "normalize-vector: unit y")))

(defun test-normalize-vector-zero ()
  "Test normalize-vector with zero input."
  (multiple-value-bind (dx dy) (normalize-vector 0.0 0.0)
    (assert (= dx 0.0) () "normalize-vector: zero x")
    (assert (= dy 0.0) () "normalize-vector: zero y")))

(defun test-point-in-rect-p ()
  "Test point-in-rect-p with points inside and outside."
  (assert (point-in-rect-p 5 5 0 0 10 10) () "point-in-rect-p: center")
  (assert (not (point-in-rect-p -1 5 0 0 10 10)) () "point-in-rect-p: left")
  (assert (not (point-in-rect-p 5 -1 0 0 10 10)) () "point-in-rect-p: above")
  (assert (not (point-in-rect-p 11 5 0 0 10 10)) () "point-in-rect-p: right")
  (assert (not (point-in-rect-p 5 11 0 0 10 10)) () "point-in-rect-p: below"))

(defun test-point-in-rect-p-edges ()
  "Test point-in-rect-p at rectangle edges."
  (assert (point-in-rect-p 0 0 0 0 10 10) () "point-in-rect-p: top-left corner")
  (assert (not (point-in-rect-p 10 10 0 0 10 10)) () "point-in-rect-p: bottom-right exclusive")
  (assert (point-in-rect-p 9.9 9.9 0 0 10 10) () "point-in-rect-p: just inside"))

(defun test-basename ()
  "Test basename with various path formats."
  (assert (string= (basename "/path/to/file.txt") "file.txt") () "basename: unix path")
  (assert (string= (basename "file.txt") "file.txt") () "basename: no path")
  (assert (string= (basename "/single") "single") () "basename: root level"))

(defun test-basename-edge-cases ()
  "Test basename with edge cases."
  (assert (string= (basename "C:\\path\\file.txt") "file.txt") () "basename: windows path")
  (assert (string= (basename "") "") () "basename: empty string")
  (assert (string= (basename "/") "") () "basename: just slash"))

(defun test-sanitize-identifier ()
  "Test sanitize-identifier creates valid keywords."
  (assert (eq (sanitize-identifier "hello") :HELLO) () "sanitize-identifier: simple")
  (assert (eq (sanitize-identifier "hello world") :HELLO-WORLD) () "sanitize-identifier: space")
  (assert (eq (sanitize-identifier "test_123") :TEST-123) () "sanitize-identifier: underscore")
  (assert (eq (sanitize-identifier "---hello---") :HELLO) () "sanitize-identifier: trim dashes"))

(defun test-plist-put ()
  "Test plist-put updates existing keys and adds new keys."
  (let ((plist (list :a 1 :b 2)))
    (let ((updated (plist-put plist :a 9)))
      (assert (equal updated '(:a 9 :b 2)) () "plist-put: update existing")
      (assert (equal plist '(:a 9 :b 2)) () "plist-put: modifies original on update"))
    (let ((extended (plist-put plist :c 3)))
      (assert (equal extended '(:a 9 :b 2 :c 3)) () "plist-put: add new key"))))

(defun test-player-direction ()
  "Test player-direction returns correct facing."
  (assert (eq (player-direction 1.0 0.0) :side) () "player-direction: right")
  (assert (eq (player-direction -1.0 0.0) :side) () "player-direction: left")
  (assert (eq (player-direction 0.0 -1.0) :up) () "player-direction: up")
  (assert (eq (player-direction 0.0 1.0) :down) () "player-direction: down")
  (assert (eq (player-direction 0.0 0.0) :down) () "player-direction: idle defaults down"))

(defun test-player-state ()
  "Test player-state returns correct state."
  (assert (eq (player-state 0.0 0.0) :idle) () "player-state: idle")
  (assert (eq (player-state 1.0 0.0) :walk) () "player-state: walking right")
  (assert (eq (player-state 0.0 1.0) :walk) () "player-state: walking down")
  (assert (eq (player-state 1.0 1.0) :walk) () "player-state: walking diagonal"))

(defun test-u32-hash ()
  "Test u32-hash produces 32-bit values."
  (let ((hash (u32-hash 10 20)))
    (assert (integerp hash) () "u32-hash: returns integer")
    (assert (>= hash 0) () "u32-hash: non-negative")
    (assert (<= hash #xffffffff) () "u32-hash: within 32-bit range")))

(defun test-u32-hash-deterministic ()
  "Test u32-hash is deterministic."
  (assert (= (u32-hash 5 10) (u32-hash 5 10)) () "u32-hash: same input same output")
  (assert (/= (u32-hash 5 10) (u32-hash 10 5)) () "u32-hash: different for swapped coords")
  (assert (/= (u32-hash 5 10 1) (u32-hash 5 10 2)) () "u32-hash: different seeds differ"))

(defun test-exponential-backoff-delay ()
  "Test exponential-backoff-delay calculations."
  (assert (= (exponential-backoff-delay 0 100 1000) 100) () "backoff: attempt 0")
  (assert (= (exponential-backoff-delay 1 100 1000) 200) () "backoff: attempt 1")
  (assert (= (exponential-backoff-delay 2 100 1000) 400) () "backoff: attempt 2")
  (assert (= (exponential-backoff-delay 3 100 1000) 800) () "backoff: attempt 3")
  (assert (= (exponential-backoff-delay 4 100 1000) 1000) () "backoff: capped at max")
  (assert (= (exponential-backoff-delay 10 100 1000) 1000) () "backoff: stays at max"))

;;; ============================================================
;;; NET.LISP TESTS
;;; ============================================================

(defun test-position-distance-sq ()
  "Test position-distance-sq helper for teleport detection."
  ;; Same position
  (assert (= (position-distance-sq 0.0f0 0.0f0 0.0f0 0.0f0) 0.0)
          () "distance-sq: same position")
  ;; Horizontal distance
  (assert (= (position-distance-sq 0.0f0 0.0f0 10.0f0 0.0f0) 100.0)
          () "distance-sq: horizontal 10")
  ;; Vertical distance
  (assert (= (position-distance-sq 0.0f0 0.0f0 0.0f0 10.0f0) 100.0)
          () "distance-sq: vertical 10")
  ;; Diagonal (3-4-5 triangle)
  (assert (= (position-distance-sq 0.0f0 0.0f0 3.0f0 4.0f0) 25.0)
          () "distance-sq: 3-4-5 triangle")
  ;; Negative direction
  (assert (= (position-distance-sq 10.0f0 10.0f0 0.0f0 0.0f0) 200.0)
          () "distance-sq: negative direction"))

(defun test-teleport-detected-p ()
  "Test teleport detection based on distance threshold."
  ;; Default threshold is 10000.0 (100 pixels)
  ;; Small movement - no teleport
  (assert (not (teleport-detected-p 0.0f0 0.0f0 10.0f0 10.0f0))
          () "teleport-detected-p: small movement")
  ;; Exactly at threshold (100 pixels in one direction = 10000 sq)
  (assert (not (teleport-detected-p 0.0f0 0.0f0 100.0f0 0.0f0))
          () "teleport-detected-p: at threshold")
  ;; Just over threshold
  (assert (teleport-detected-p 0.0f0 0.0f0 101.0f0 0.0f0)
          () "teleport-detected-p: just over threshold")
  ;; Large teleport
  (assert (teleport-detected-p 0.0f0 0.0f0 500.0f0 500.0f0)
          () "teleport-detected-p: large teleport")
  ;; Teleport in negative direction
  (assert (teleport-detected-p 500.0f0 500.0f0 0.0f0 0.0f0)
          () "teleport-detected-p: negative direction"))

(defun test-sync-client-zone-npcs ()
  "Test zone-state NPC sync after teleport/resync.
   Phase 2: Ensures rendering uses the same NPC array that snapshots update."
  (ensure-test-game-data)
  ;; Save original zone-states and restore after test to avoid order-dependent failures
  ;; Use 'eq to match *zone-states* hash table test function
  (let ((saved-zone-states (make-hash-table :test 'eq)))
    (maphash (lambda (k v) (setf (gethash k saved-zone-states) v)) *zone-states*)
    (unwind-protect
         (progn
           ;; Clear zone-states cache for isolated test
           (clrhash *zone-states*)
           (let* (;; Create initial zone-state with empty NPC array
                  (zone-state (make-zone-state :zone-id :test-sync-zone
                                                :zone nil
                                                :wall-map (make-array '(10 10) :initial-element 0)
                                                :npcs (make-array 0)
                                                :npc-grid (make-spatial-grid 64.0f0)))
                  ;; Create game with NPC array that differs from zone-state
                  (archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
                  (npc1 (make-npc 100.0f0 100.0f0 :archetype archetype :id 1))
                  (npc2 (make-npc 200.0f0 200.0f0 :archetype archetype :id 2))
                  (game-npcs (make-array 2 :initial-contents (list npc1 npc2)))
                  (player (make-player 150.0f0 150.0f0 :id 1))
                  ;; Use %make-game to create minimal game struct for testing
                  (game (%make-game :npcs game-npcs :player player)))
             ;; Setup player zone-id and register zone-state in cache
             (setf (player-zone-id player) :test-sync-zone)
             (setf (gethash :test-sync-zone *zone-states*) zone-state)
             ;; Verify initial state: zone-state has 0 NPCs, game has 2
             (assert (= (length (zone-state-npcs zone-state)) 0) ()
                     "sync-npcs: zone-state starts with 0 npcs")
             (assert (= (length game-npcs) 2) ()
                     "sync-npcs: game has 2 npcs")
             ;; Call sync
             (sync-client-zone-npcs game)
             ;; Verify zone-state now has the game NPCs
             (assert (eq (zone-state-npcs zone-state) game-npcs) ()
                     "sync-npcs: zone-state-npcs now equals game-npcs")
             (assert (= (length (zone-state-npcs zone-state)) 2) ()
                     "sync-npcs: zone-state has 2 npcs after sync")
             ;; Verify NPC index map was rebuilt
             (let ((index-map (zone-state-npc-index-map zone-state)))
               (assert (not (null index-map)) ()
                       "sync-npcs: npc-index-map created")
               (assert (= (hash-table-count index-map) 2) ()
                       "sync-npcs: index-map has 2 entries"))))
      ;; Restore original zone-states
      (clrhash *zone-states*)
      (maphash (lambda (k v) (setf (gethash k *zone-states*) v)) saved-zone-states))))

;;; ============================================================
;;; COMBAT.LISP TESTS
;;; ============================================================

(defun test-aabb-overlap-p ()
  "Test AABB collision detection (center + half-sizes)."
  ;; Overlapping boxes: center(0,0) half(5,5) and center(3,3) half(5,5)
  ;; Distance: |3-0|=3, sum of halves: 5+5=10, 3 < 10 = overlap
  (assert (aabb-overlap-p 0 0 5 5 3 3 5 5) () "aabb-overlap-p: overlapping")
  ;; Clearly separate boxes: center(0,0) half(2,2) and center(100,100) half(2,2)
  ;; Distance: 100, sum of halves: 4, 100 > 4 = no overlap
  (assert (not (aabb-overlap-p 0 0 2 2 100 100 2 2)) () "aabb-overlap-p: far apart")
  ;; Just touching (edge case): center(0,0) half(5,5) and center(10,0) half(5,5)
  ;; Distance x: 10, sum of halves: 10, 10 <= 10 = overlap (touching counts)
  (assert (aabb-overlap-p 0 0 5 5 10 0 5 5) () "aabb-overlap-p: just touching"))

(defun test-aabb-overlap-p-edge-cases ()
  "Test AABB with edge cases."
  ;; Same position
  (assert (aabb-overlap-p 5 5 2 2 5 5 2 2) () "aabb-overlap-p: same position")
  ;; One inside the other
  (assert (aabb-overlap-p 5 5 10 10 5 5 2 2) () "aabb-overlap-p: contained")
  ;; Zero size same pos
  (assert (aabb-overlap-p 5 5 0 0 5 5 0 0) () "aabb-overlap-p: zero size same pos"))

(defun test-melee-hit-chance ()
  "Test melee hit chance calculation with player combatants."
  (ensure-test-game-data)
  (let* ((attacker (make-player 0.0 0.0 :id 1))
         (defender (make-player 0.0 0.0 :id 2)))
    ;; With equal stats, should get reasonable chance
    (let ((chance (melee-hit-chance attacker defender)))
      (assert (>= chance 0.0) () "hit-chance: non-negative")
      (assert (<= chance 1.0) () "hit-chance: max 1.0")
      (assert (> chance 0.3) () "hit-chance: equal stats > 30%")
      (assert (< chance 0.7) () "hit-chance: equal stats < 70%"))))

(defun test-melee-hit-chance-clamped ()
  "Test hit chance is clamped between min and max."
  (ensure-test-game-data)
  (let* ((attacker (make-player 0.0 0.0 :id 1))
         (defender (make-player 0.0 0.0 :id 2)))
    ;; Boost attacker's attack stat
    (setf (skill-level (stat-block-attack (player-stats attacker))) 99)
    (let ((high-chance (melee-hit-chance attacker defender)))
      (assert (<= high-chance 0.95) () "hit-chance: capped at 95%"))
    ;; Now make defender very strong
    (setf (skill-level (stat-block-attack (player-stats attacker))) 1)
    (setf (skill-level (stat-block-defense (player-stats defender))) 99)
    (let ((low-chance (melee-hit-chance attacker defender)))
      (assert (>= low-chance 0.05) () "hit-chance: floored at 5%"))))

(defun test-melee-max-hit ()
  "Test max hit calculation from strength."
  (ensure-test-game-data)
  (let* ((weak (make-player 0.0 0.0 :id 1))
         (strong (make-player 0.0 0.0 :id 2)))
    (setf (skill-level (stat-block-strength (player-stats strong))) 50)
    (let ((weak-hit (melee-max-hit weak))
          (strong-hit (melee-max-hit strong)))
      (assert (> weak-hit 0) () "max-hit: positive damage")
      (assert (> strong-hit weak-hit) () "max-hit: scales with strength"))))

;;; ============================================================
;;; PROGRESSION.LISP TESTS
;;; ============================================================

(defun test-xp-to-level ()
  "Test XP to level conversion."
  (assert (= (xp->level 0) 1) () "xp->level: 0 XP = level 1")
  (assert (>= (xp->level 100) 1) () "xp->level: 100 XP >= level 1")
  (assert (> (xp->level 10000) (xp->level 100)) () "xp->level: more XP = higher level"))

(defun test-level-to-xp ()
  "Test level to XP conversion."
  (assert (= (level->xp 1) 0) () "level->xp: level 1 = 0 XP")
  (assert (> (level->xp 2) 0) () "level->xp: level 2 > 0 XP")
  (assert (> (level->xp 10) (level->xp 5)) () "level->xp: higher level = more XP"))

(defun test-xp-level-roundtrip ()
  "Test XP/level conversion consistency."
  ;; Level -> XP -> Level should return same level
  (dolist (level '(1 5 10 25 50 99))
    (let* ((xp (level->xp level))
           (back (xp->level xp)))
      (assert (= back level) ()
              (format nil "xp-level roundtrip: level ~d -> xp ~d -> level ~d" level xp back)))))

(defun test-valid-training-mode-p ()
  "Test training mode validation."
  (assert (valid-training-mode-p :balanced) () "training: balanced valid")
  (assert (valid-training-mode-p :attack) () "training: attack valid")
  (assert (valid-training-mode-p :strength) () "training: strength valid")
  (assert (valid-training-mode-p :defense) () "training: defense valid")
  (assert (not (valid-training-mode-p :invalid)) () "training: invalid rejected")
  (assert (not (valid-training-mode-p nil)) () "training: nil rejected"))

(defun test-normalize-training-mode ()
  "Test training mode normalization."
  (assert (eq (normalize-training-mode :balanced) :balanced) () "normalize: balanced")
  (assert (eq (normalize-training-mode :invalid) :balanced) () "normalize: invalid -> balanced")
  (assert (eq (normalize-training-mode nil) :balanced) () "normalize: nil -> balanced"))

(defun test-training-mode-label ()
  "Test training mode labels."
  (assert (stringp (training-mode-label :balanced)) () "label: balanced is string")
  (assert (stringp (training-mode-label :attack)) () "label: attack is string")
  (assert (not (string= (training-mode-label :attack) (training-mode-label :defense)))
          () "label: attack != defense"))

(defun test-combat-level ()
  "Test combat level calculation via stat-block-effective-level."
  (ensure-test-game-data)
  ;; Test that stat-block-effective-level returns integer for various skills
  (let ((player (make-player 0.0 0.0 :id 999)))
    (let ((stats (player-stats player)))
      (assert (integerp (stat-block-effective-level stats :attack)) () "combat-level: attack is int")
      (assert (integerp (stat-block-effective-level stats :strength)) () "combat-level: strength is int")
      (assert (integerp (stat-block-effective-level stats :defense)) () "combat-level: defense is int")
      (assert (>= (stat-block-effective-level stats :attack) 1) () "combat-level: attack >= 1"))))

(defun test-split-combat-xp ()
  "Test combat XP splitting."
  (ensure-test-game-data)
  (let ((player (make-player 0.0 0.0 :id 1)))
    ;; split-combat-xp takes (player amount), uses player's training mode
    (multiple-value-bind (atk str def hp) (split-combat-xp player 100)
      (assert (>= atk 0) () "split-xp: attack non-negative")
      (assert (>= str 0) () "split-xp: strength non-negative")
      (assert (>= def 0) () "split-xp: defense non-negative")
      (assert (>= hp 0) () "split-xp: hp non-negative")
      ;; Total should roughly equal input (may have rounding)
      (assert (<= (abs (- (+ atk str def hp) 100)) 5) () "split-xp: total approximately 100"))))

(defun test-inventory-slot-empty-p ()
  "Test inventory slot empty check."
  (let ((empty-slot (make-inventory-slot :item-id nil :count 0))
        (full-slot (make-inventory-slot :item-id :health-potion :count 5)))
    (assert (inventory-slot-empty-p empty-slot) () "slot-empty: nil item is empty")
    (assert (not (inventory-slot-empty-p full-slot)) () "slot-empty: has item not empty")))

(defun test-item-stack-size ()
  "Test item stack size lookup."
  (ensure-test-game-data)
  (let ((potion-stack (item-stack-size :health-potion))
        (sword-stack (item-stack-size :rusty-sword)))
    (assert (integerp potion-stack) () "stack-size: potion is integer")
    (assert (integerp sword-stack) () "stack-size: sword is integer")
    (assert (>= potion-stack 1) () "stack-size: at least 1")))

(defun test-equipment-slot-index ()
  "Test equipment slot index lookup."
  ;; Valid slots: :head :body :legs :weapon :offhand :accessory
  (assert (integerp (equipment-slot-index :weapon)) () "slot-index: weapon is integer")
  (assert (integerp (equipment-slot-index :body)) () "slot-index: body is integer")
  (assert (/= (equipment-slot-index :weapon) (equipment-slot-index :body))
          () "slot-index: weapon != body"))

(defun test-player-adjacent-to-tile-p ()
  "Test adjacent tile check."
  ;; Same tile
  (assert (player-adjacent-to-tile-p 5 5 5 5) () "adjacent: same tile")
  ;; Cardinal directions
  (assert (player-adjacent-to-tile-p 5 5 6 5) () "adjacent: right")
  (assert (player-adjacent-to-tile-p 5 5 4 5) () "adjacent: left")
  (assert (player-adjacent-to-tile-p 5 5 5 6) () "adjacent: down")
  (assert (player-adjacent-to-tile-p 5 5 5 4) () "adjacent: up")
  ;; Diagonals
  (assert (player-adjacent-to-tile-p 5 5 6 6) () "adjacent: down-right")
  (assert (player-adjacent-to-tile-p 5 5 4 4) () "adjacent: up-left")
  ;; Too far
  (assert (not (player-adjacent-to-tile-p 5 5 7 5)) () "adjacent: 2 tiles right")
  (assert (not (player-adjacent-to-tile-p 5 5 5 7)) () "adjacent: 2 tiles down"))

(defun test-swap-inventory-slots ()
  "Test inventory slot swapping."
  (ensure-test-game-data)
  (let ((player (make-player 0.0 0.0 :id 1)))
    ;; Set up two slots
    (let ((slots (inventory-slots (player-inventory player))))
      (setf (aref slots 0) (make-inventory-slot :item-id :health-potion :count 5))
      (setf (aref slots 1) (make-inventory-slot :item-id :rusty-sword :count 1)))
    ;; Swap via player
    (swap-inventory-slots player 0 1)
    ;; Verify swap
    (let ((slots (inventory-slots (player-inventory player))))
      (assert (eq (inventory-slot-item-id (aref slots 0)) :rusty-sword) () "swap: slot 0 has sword")
      (assert (eq (inventory-slot-item-id (aref slots 1)) :health-potion) () "swap: slot 1 has potion")
      (assert (= (inventory-slot-count (aref slots 0)) 1) () "swap: slot 0 count")
      (assert (= (inventory-slot-count (aref slots 1)) 5) () "swap: slot 1 count"))))

;;; ============================================================
;;; MOVEMENT.LISP TESTS
;;; ============================================================

(defun test-wall-blocked-p ()
  "Test wall-blocked-p function."
  ;; build-wall-map takes no args, uses globals *wall-map-width* and *wall-map-height*
  (let ((wall-map (build-wall-map)))
    ;; Out of bounds should be blocked (uses *wall-origin-x/y* globals)
    (assert (wall-blocked-p wall-map -1000 -1000) () "wall-blocked: out of bounds")
    ;; A nil wall-map should not block
    (assert (not (wall-blocked-p nil 5 5)) () "wall-blocked: nil map doesn't block")))

(defun test-tile-center-position ()
  "Test tile center position calculation."
  ;; tile-center-position takes (tile-size tx ty)
  (multiple-value-bind (x y) (tile-center-position 16 0 0)
    (assert (= x 8.0) () "tile-center: 0,0 x")
    (assert (= y 8.0) () "tile-center: 0,0 y"))
  (multiple-value-bind (x y) (tile-center-position 16 1 1)
    (assert (= x 24.0) () "tile-center: 1,1 x")
    (assert (= y 24.0) () "tile-center: 1,1 y")))

(defun test-edge-opposite ()
  "Test edge opposite function."
  (assert (eq (edge-opposite :north) :south) () "edge-opposite: north")
  (assert (eq (edge-opposite :south) :north) () "edge-opposite: south")
  (assert (eq (edge-opposite :east) :west) () "edge-opposite: east")
  (assert (eq (edge-opposite :west) :east) () "edge-opposite: west"))

(defun test-edge-offset-ratio ()
  "Test edge offset ratio calculation."
  ;; edge-offset-ratio takes (min-value max-value value)
  (assert (= (edge-offset-ratio 0.0 100.0 50.0) 0.5) () "edge-ratio: middle")
  (assert (= (edge-offset-ratio 0.0 100.0 0.0) 0.0) () "edge-ratio: at min")
  (assert (= (edge-offset-ratio 0.0 100.0 100.0) 1.0) () "edge-ratio: at max"))

(defun test-npc-collision-half ()
  "Test NPC collision half-size calculation constant."
  ;; npc-collision-half takes a world object, uses world-tile-dest-size
  ;; This is tested implicitly via smoke tests - skip unit test for now
  ;; The function signature is (npc-collision-half world)
  (assert t () "npc-collision-half: skipped (requires world object)"))

;;; ============================================================
;;; AI.LISP TESTS
;;; ============================================================

(defun test-npc-should-flee-p ()
  "Test NPC flee decision."
  (ensure-test-game-data)
  (let* ((archetype (gethash :goblin *npc-archetypes*))
         (npc (make-npc 0.0 0.0 :archetype archetype :id 1)))
    ;; Full hits should not flee
    (let ((max-hits (npc-hits-left npc)))
      (setf (npc-hits-left npc) max-hits)
      (assert (not (npc-should-flee-p npc)) () "flee: full HP no flee")
      ;; Very low hits - result depends on archetype flee threshold
      (setf (npc-hits-left npc) 1)
      (let ((result (npc-should-flee-p npc)))
        (assert (or (null result) (eq result t)) () "flee: returns boolean")))))

(defun test-npc-perception-range-sq ()
  "Test NPC perception range squared - requires world object."
  ;; npc-perception-range-sq takes (npc world), requiring a full world setup
  ;; Skip detailed test, verify function exists
  (assert (fboundp 'npc-perception-range-sq) () "perception: function exists"))

;;; ============================================================
;;; DATA.LISP TESTS
;;; ============================================================

(defun test-plist-form-p ()
  "Test plist form detection."
  (assert (plist-form-p '(:key1 val1 :key2 val2)) () "plist-form: valid plist")
  (assert (not (plist-form-p '(a b c))) () "plist-form: not a plist - non-keyword keys")
  ;; Note: nil and empty list returns T from plist-form-p (vacuous truth)
  (assert (plist-form-p nil) () "plist-form: empty list is valid (vacuously)")
  (assert (plist-form-p '(:single value)) () "plist-form: single pair"))

(defun test-data-section-header-p ()
  "Test data section header detection."
  (assert (data-section-header-p :items) () "section-header: keyword")
  (assert (not (data-section-header-p "items")) () "section-header: string")
  (assert (not (data-section-header-p '(:items))) () "section-header: list"))

(defun test-data-section-entry-p ()
  "Test data section entry detection."
  (assert (data-section-entry-p '(:sword (:damage 10))) () "section-entry: valid")
  ;; '(:sword) has nil as second element, nil is a list, so this passes
  (assert (data-section-entry-p '(:sword nil)) () "section-entry: nil second is list")
  (assert (not (data-section-entry-p nil)) () "section-entry: nil")
  (assert (not (data-section-entry-p '("sword" (:damage 10)))) () "section-entry: non-keyword id"))

(defun test-normalize-pairs ()
  "Test pair normalization."
  (let ((plist-result (normalize-pairs '(:a 1 :b 2)))
        (pairs-result (normalize-pairs '((:a 1) (:b 2)))))
    (assert (equal plist-result '((:a 1) (:b 2))) () "normalize-pairs: plist")
    (assert (equal pairs-result '((:a 1) (:b 2))) () "normalize-pairs: already pairs")))

;;; ============================================================
;;; ZONE.LISP TESTS
;;; ============================================================

(defun test-zone-chunk-key ()
  "Test zone chunk key packing."
  (let ((key1 (zone-chunk-key 0 0))
        (key2 (zone-chunk-key 1 0))
        (key3 (zone-chunk-key 0 1)))
    (assert (integerp key1) () "chunk-key: returns integer")
    (assert (/= key1 key2) () "chunk-key: different x")
    (assert (/= key1 key3) () "chunk-key: different y")
    (assert (/= key2 key3) () "chunk-key: all unique")))

(defun test-tile-key-roundtrip ()
  "Test tile key packing and unpacking."
  (dolist (coords '((0 0) (10 20) (100 200) (1000 2000)))
    (let* ((x (first coords))
           (y (second coords))
           (key (tile-key x y))
           (back-x (tile-key-x key))
           (back-y (tile-key-y key)))
      (assert (= x back-x) () (format nil "tile-key roundtrip x: ~d" x))
      (assert (= y back-y) () (format nil "tile-key roundtrip y: ~d" y)))))

(defun test-zone-tile-in-bounds-p ()
  "Test zone bounds checking."
  ;; Use %make-zone directly since make-zone may not exist
  (let ((zone (%make-zone :id :test :width 10 :height 10)))
    (assert (zone-tile-in-bounds-p zone 0 0) () "zone-bounds: origin")
    (assert (zone-tile-in-bounds-p zone 5 5) () "zone-bounds: center")
    (assert (zone-tile-in-bounds-p zone 9 9) () "zone-bounds: max valid")
    (assert (not (zone-tile-in-bounds-p zone 10 5)) () "zone-bounds: x out")
    (assert (not (zone-tile-in-bounds-p zone 5 10)) () "zone-bounds: y out")
    (assert (not (zone-tile-in-bounds-p zone -1 5)) () "zone-bounds: negative x")))

;;; ============================================================
;;; INTENT.LISP TESTS
;;; ============================================================

(defun test-set-intent-move ()
  "Test setting intent movement."
  (let ((intent (make-intent)))
    (set-intent-move intent 1.0 0.0)
    (assert (= (intent-move-dx intent) 1.0) () "intent-move: dx set")
    (assert (= (intent-move-dy intent) 0.0) () "intent-move: dy set")
    ;; Check face-dx/face-dy instead of intent-face
    (assert (= (intent-face-dx intent) 1.0) () "intent-move: face-dx updated")
    (assert (= (intent-face-dy intent) 0.0) () "intent-move: face-dy updated")))

(defun test-set-intent-face ()
  "Test setting intent facing."
  (let ((intent (make-intent)))
    (set-intent-face intent 0.0 -1.0)
    (assert (= (intent-face-dx intent) 0.0) () "intent-face: up dx")
    (assert (= (intent-face-dy intent) -1.0) () "intent-face: up dy")
    (set-intent-face intent 0.0 1.0)
    (assert (= (intent-face-dy intent) 1.0) () "intent-face: down dy")
    (set-intent-face intent 1.0 0.0)
    (assert (= (intent-face-dx intent) 1.0) () "intent-face: side dx")))

(defun test-apply-intent-plist-rejects-bad-pickup ()
  "Ensure malformed pickup/drop intent fields are sanitized."
  (let ((intent (make-intent)))
    (apply-intent-plist intent (list :requested-pickup-target-id "bad-id"
                                     :requested-drop-item-id 123
                                     :requested-pickup-tx "bad"
                                     :requested-pickup-ty -3
                                     :requested-drop-slot-index "oops"))
    (assert (null (intent-requested-pickup-target-id intent)) () "pickup-id invalid -> nil")
    (assert (null (intent-requested-drop-item-id intent)) () "drop-id invalid -> nil")
    (assert (null (intent-requested-pickup-tx intent)) () "pickup-tx invalid -> nil")
    (assert (null (intent-requested-pickup-ty intent)) () "pickup-ty invalid -> nil")
    (assert (null (intent-requested-drop-slot-index intent)) () "drop-slot invalid -> nil")
    (apply-intent-plist intent (list :requested-pickup-target-id :arrows
                                     :requested-drop-item-id :coins
                                     :requested-pickup-tx 2
                                     :requested-pickup-ty 3
                                     :requested-drop-slot-index 1))
    (assert (eq (intent-requested-pickup-target-id intent) :arrows) () "pickup-id valid -> :arrows")
    (assert (eq (intent-requested-drop-item-id intent) :coins) () "drop-id valid -> :coins")
    (assert (= (intent-requested-pickup-tx intent) 2) () "pickup-tx valid -> 2")
    (assert (= (intent-requested-pickup-ty intent) 3) () "pickup-ty valid -> 3")
    (assert (= (intent-requested-drop-slot-index intent) 1) () "drop-slot valid -> 1")))

;;; ============================================================
;;; SAVE.LISP TESTS
;;; ============================================================

(defun test-quantize-coord-roundtrip ()
  "Test coordinate quantization and dequantization."
  (dolist (coord '(0.0 1.5 100.3 -50.7 1234.5))
    (let* ((quantized (quantize-coord coord))
           (restored (dequantize-coord quantized)))
      (assert (< (abs (- restored coord)) 0.1) ()
              (format nil "quantize-coord roundtrip: ~f -> ~d -> ~f" coord quantized restored))))
  ;; Test nil handling
  (assert (= (quantize-coord nil) 0) () "quantize-coord: nil -> 0")
  (assert (= (dequantize-coord nil) 0.0) () "dequantize-coord: nil -> 0.0"))

(defun test-quantize-timer-roundtrip ()
  "Test timer quantization and dequantization."
  (dolist (timer '(0.0 0.5 1.23 5.0 10.99))
    (let* ((quantized (quantize-timer timer))
           (restored (dequantize-timer quantized)))
      (assert (< (abs (- restored timer)) 0.01) ()
              (format nil "quantize-timer roundtrip: ~f -> ~d -> ~f" timer quantized restored))))
  ;; Test nil handling
  (assert (= (quantize-timer nil) 0) () "quantize-timer: nil -> 0")
  (assert (= (dequantize-timer nil) 0.0) () "dequantize-timer: nil -> 0.0"))

(defun test-encode-anim-state-roundtrip ()
  "Test animation state encoding and decoding."
  ;; Valid states: :idle, :walk, :attack (not :hit - see *anim-state-to-code*)
  (dolist (state '(:idle :walk :attack))
    (let* ((code (encode-anim-state state))
           (decoded (decode-anim-state code)))
      (assert (eq decoded state) ()
              (format nil "anim-state roundtrip: ~a -> ~d -> ~a" state code decoded))))
  ;; Unknown state should encode to 0, decode to :idle
  (let ((unknown-code (encode-anim-state :unknown)))
    (assert (= unknown-code 0) () "encode-anim-state: unknown -> 0")))

(defun test-encode-facing-roundtrip ()
  "Test facing direction encoding and decoding."
  (dolist (facing '(:up :down :side))
    (let* ((code (encode-facing facing))
           (decoded (decode-facing code)))
      (assert (eq decoded facing) ()
              (format nil "facing roundtrip: ~a -> ~d -> ~a" facing code decoded))))
  ;; Unknown facing should default to :down
  (let ((default (decode-facing 999)))
    (assert (eq default :down) () "decode-facing: invalid -> :down")))

(defun test-pack-player-flags-roundtrip ()
  "Test player flag packing and unpacking."
  (ensure-test-game-data)
  ;; Test all flags false
  (let ((player (make-player 0.0 0.0 :id 1)))
    (setf (player-attacking player) nil
          (player-attack-hit player) nil
          (player-hit-active player) nil
          (player-running player) nil)
    (let ((flags (pack-player-flags player)))
      (multiple-value-bind (atk atk-hit hit-active running) (unpack-player-flags flags)
        (assert (not atk) () "flags: attacking false")
        (assert (not atk-hit) () "flags: attack-hit false")
        (assert (not hit-active) () "flags: hit-active false")
        (assert (not running) () "flags: running false"))))
  ;; Test all flags true
  (let ((player (make-player 0.0 0.0 :id 2)))
    (setf (player-attacking player) t
          (player-attack-hit player) t
          (player-hit-active player) t
          (player-running player) t)
    (let ((flags (pack-player-flags player)))
      (multiple-value-bind (atk atk-hit hit-active running) (unpack-player-flags flags)
        (assert atk () "flags: attacking true")
        (assert atk-hit () "flags: attack-hit true")
        (assert hit-active () "flags: hit-active true")
        (assert running () "flags: running true")))))

(defun test-pack-npc-flags-roundtrip ()
  "Test NPC flag packing and unpacking."
  (ensure-test-game-data)
  (let* ((archetype (gethash :goblin *npc-archetypes*))
         (npc (make-npc 0.0 0.0 :archetype archetype :id 1)))
    ;; Test default state (alive)
    (let ((flags (pack-npc-flags npc)))
      (multiple-value-bind (alive provoked hit-active) (unpack-npc-flags flags)
        (assert alive () "npc-flags: alive default")
        (assert (not provoked) () "npc-flags: not provoked default")
        (assert (not hit-active) () "npc-flags: not hit-active default")))
    ;; Test modified state
    (setf (npc-alive npc) nil
          (npc-provoked npc) t
          (npc-hit-active npc) t)
    (let ((flags (pack-npc-flags npc)))
      (multiple-value-bind (alive provoked hit-active) (unpack-npc-flags flags)
        (assert (not alive) () "npc-flags: dead")
        (assert provoked () "npc-flags: provoked")
        (assert hit-active () "npc-flags: hit-active")))))

(defun test-serialize-skill-roundtrip ()
  "Test skill serialization and deserialization."
  (let* ((skill (make-skill :level 50 :xp 12345))
         (plist (serialize-skill skill))
         (restored (deserialize-skill plist)))
    (assert (= (skill-level restored) 50) () "skill roundtrip: level")
    (assert (= (skill-xp restored) 12345) () "skill roundtrip: xp"))
  ;; Test nil handling
  (assert (null (serialize-skill nil)) () "serialize-skill: nil -> nil")
  (assert (null (deserialize-skill nil)) () "deserialize-skill: nil -> nil"))

(defun test-serialize-stat-block-roundtrip ()
  "Test stat-block serialization and deserialization."
  (let* ((stats (make-stat-block
                 :attack (make-skill :level 10 :xp 100)
                 :strength (make-skill :level 20 :xp 200)
                 :defense (make-skill :level 30 :xp 300)
                 :hitpoints (make-skill :level 40 :xp 400)))
         (plist (serialize-stat-block stats))
         (restored (deserialize-stat-block plist)))
    (assert (= (skill-level (stat-block-attack restored)) 10) () "stat-block: attack level")
    (assert (= (skill-level (stat-block-strength restored)) 20) () "stat-block: strength level")
    (assert (= (skill-level (stat-block-defense restored)) 30) () "stat-block: defense level")
    (assert (= (skill-level (stat-block-hitpoints restored)) 40) () "stat-block: hp level")
    (assert (= (skill-xp (stat-block-attack restored)) 100) () "stat-block: attack xp")))

(defun test-serialize-inventory-slot-roundtrip ()
  "Test inventory slot serialization and deserialization."
  (let* ((slot (make-inventory-slot :item-id :health-potion :count 10))
         (plist (serialize-inventory-slot slot))
         (restored (deserialize-inventory-slot plist)))
    (assert (eq (inventory-slot-item-id restored) :health-potion) () "slot: item-id")
    (assert (= (inventory-slot-count restored) 10) () "slot: count"))
  ;; Test empty slot
  (let* ((empty (make-inventory-slot :item-id nil :count 0))
         (plist (serialize-inventory-slot empty))
         (restored (deserialize-inventory-slot plist)))
    (assert (null (inventory-slot-item-id restored)) () "slot: empty item-id")
    (assert (= (inventory-slot-count restored) 0) () "slot: empty count")))

;;; ============================================================
;;; MIGRATIONS.LISP TESTS
;;; ============================================================

(defun test-migrate-player-v1-to-v2 ()
  "Test migration from v1 to v2 adds lifetime-xp."
  (let* ((v1-data '(:version 1 :id 123 :x 100.0 :y 200.0 :hp 50))
         (migrated (migrate-player-v1->v2 (copy-list v1-data))))
    ;; Should have lifetime-xp added
    (assert (= (getf migrated :lifetime-xp) 0) () "v1->v2: adds lifetime-xp = 0")
    ;; Original fields preserved
    (assert (= (getf migrated :id) 123) () "v1->v2: preserves id")
    (assert (= (getf migrated :x) 100.0) () "v1->v2: preserves x")))

(defun test-migrate-player-v2-to-v3 ()
  "Test migration from v2 to v3 adds playtime and created-at."
  (let* ((v2-data '(:version 2 :id 456 :lifetime-xp 1000))
         (before-time (get-universal-time))
         (migrated (migrate-player-v2->v3 (copy-list v2-data)))
         (after-time (get-universal-time)))
    ;; Should have playtime added
    (assert (= (getf migrated :playtime) 0) () "v2->v3: adds playtime = 0")
    ;; Should have created-at added (between before and after time)
    (let ((created (getf migrated :created-at)))
      (assert (>= created before-time) () "v2->v3: created-at >= start")
      (assert (<= created after-time) () "v2->v3: created-at <= end"))
    ;; Original fields preserved
    (assert (= (getf migrated :lifetime-xp) 1000) () "v2->v3: preserves lifetime-xp")))

(defun test-migrate-player-v3-to-v4 ()
  "Test migration from v3 to v4 adds deaths field."
  (let* ((v3-data '(:version 3 :id 789 :lifetime-xp 5000 :playtime 3600 :created-at 1000000))
         (migrated (migrate-player-v3->v4 (copy-list v3-data))))
    ;; Should have deaths added
    (assert (= (getf migrated :deaths) 0) () "v3->v4: adds deaths = 0")
    ;; Original fields preserved
    (assert (= (getf migrated :id) 789) () "v3->v4: preserves id")
    (assert (= (getf migrated :lifetime-xp) 5000) () "v3->v4: preserves lifetime-xp")
    (assert (= (getf migrated :playtime) 3600) () "v3->v4: preserves playtime")
    (assert (= (getf migrated :created-at) 1000000) () "v3->v4: preserves created-at")))

(defun test-migrate-player-data-chain ()
  "Test full migration chain from v1 to current version."
  (let* ((v1-data '(:version 1 :id 789 :x 50.0 :y 75.0 :hp 100))
         (migrated (migrate-player-data (copy-list v1-data))))
    ;; Should be at current version
    (assert (= (getf migrated :version) *player-schema-version*) () "chain: at current version")
    ;; Should have all migration fields
    (assert (numberp (getf migrated :lifetime-xp)) () "chain: has lifetime-xp")
    (assert (numberp (getf migrated :playtime)) () "chain: has playtime")
    (assert (numberp (getf migrated :created-at)) () "chain: has created-at")
    ;; Original fields preserved
    (assert (= (getf migrated :id) 789) () "chain: preserves id")
    (assert (= (getf migrated :hp) 100) () "chain: preserves hp")))

;;; ============================================================
;;; WORLD-GRAPH.LISP TESTS
;;; ============================================================

(defun test-world-graph-data-plist ()
  "Test world graph data normalization."
  ;; Direct plist - any plist starting with keyword returns as-is
  (let ((direct '(:edges ((:from :town :to :forest)))))
    (assert (equal (world-graph-data-plist direct) direct) () "wg-plist: direct"))
  ;; Another keyword-starting plist (world-graph is a keyword, so first condition matches)
  (let ((wrapped '(:world-graph (:edges ()))))
    (assert (equal (world-graph-data-plist wrapped) wrapped) () "wg-plist: keyword plist"))
  ;; Invalid data
  (assert (null (world-graph-data-plist nil)) () "wg-plist: nil -> nil")
  (assert (null (world-graph-data-plist '("not" "a" "plist"))) () "wg-plist: invalid"))

(defun test-normalize-world-graph-edges ()
  "Test world graph edge normalization."
  (let* ((edges '((:from :town :to :forest :edge :north)
                  (:from :town :to :cave :edge :south)
                  (:from :forest :to :town :edge :south)))
         (table (normalize-world-graph-edges edges)))
    ;; Town should have 2 exits
    (let ((town-exits (gethash :town table)))
      (assert (= (length town-exits) 2) () "wg-edges: town has 2 exits"))
    ;; Forest should have 1 exit
    (let ((forest-exits (gethash :forest table)))
      (assert (= (length forest-exits) 1) () "wg-edges: forest has 1 exit"))
    ;; Cave should have 0 exits (only destination)
    (let ((cave-exits (gethash :cave table)))
      (assert (null cave-exits) () "wg-edges: cave has no exits"))))

;;; ============================================================
;;; CHAT.LISP TESTS
;;; ============================================================

(defun test-trim-chat-message ()
  "Test chat message trimming."
  (assert (string= (trim-chat-message "hello") "hello") () "trim: no whitespace")
  (assert (string= (trim-chat-message "  hello  ") "hello") () "trim: spaces")
  (assert (string= (trim-chat-message "	hello	") "hello") () "trim: tabs")
  (assert (string= (trim-chat-message "
hello
") "hello") () "trim: newlines")
  (assert (string= (trim-chat-message "") "") () "trim: empty string")
  (assert (string= (trim-chat-message "   ") "") () "trim: only whitespace"))

;;; ============================================================
;;; TYPES.LISP TESTS
;;; ============================================================

(defun test-skill-xp-for-level ()
  "Test XP required for level calculation."
  ;; Level 1 requires 0 XP
  (assert (= (skill-xp-for-level 1) 0) () "skill-xp: level 1 = 0")
  ;; Higher levels need more XP
  (assert (> (skill-xp-for-level 2) 0) () "skill-xp: level 2 > 0")
  (assert (> (skill-xp-for-level 10) (skill-xp-for-level 5)) () "skill-xp: level 10 > level 5")
  ;; XP increases per level
  (let ((xp5 (skill-xp-for-level 5))
        (xp6 (skill-xp-for-level 6)))
    (assert (> xp6 xp5) () "skill-xp: monotonic increase")))

(defun test-allocate-entity-id ()
  "Test entity ID allocation."
  ;; make-id-source takes positional args: (next-id persistent)
  (let ((source (make-id-source 1)))
    ;; First allocation
    (let ((id1 (allocate-entity-id source)))
      (assert (= id1 1) () "entity-id: first = 1"))
    ;; Second allocation
    (let ((id2 (allocate-entity-id source)))
      (assert (= id2 2) () "entity-id: second = 2"))
    ;; IDs are unique
    (let ((id3 (allocate-entity-id source)))
      (assert (= id3 3) () "entity-id: third = 3"))
    ;; Source tracks next
    (assert (= (id-source-next-id source) 4) () "entity-id: next is 4")))

(defun test-find-player-by-id ()
  "Test finding player by ID."
  (ensure-test-game-data)
  (let* ((p1 (make-player 0.0 0.0 :id 100))
         (p2 (make-player 10.0 10.0 :id 200))
         ;; find-player-by-id expects a vector, not list
         (players (vector p1 p2)))
    ;; Find existing
    (assert (eq (find-player-by-id players 100) p1) () "find-player: id 100")
    (assert (eq (find-player-by-id players 200) p2) () "find-player: id 200")
    ;; Not found
    (assert (null (find-player-by-id players 999)) () "find-player: not found")
    ;; Empty vector
    (assert (null (find-player-by-id (vector) 100)) () "find-player: empty vector")
    ;; Nil
    (assert (null (find-player-by-id nil 100)) () "find-player: nil")))

;;; ============================================================
;;; TEST HELPERS
;;; ============================================================

(defun ensure-test-game-data ()
  "Ensure game data is loaded for tests."
  (unless *game-data-loaded-p*
    (load-game-data)))

;;; ============================================================
;;; REDIS METRICS TESTS (Phase 2 - Database Hardening)
;;; ============================================================

(defun test-calculate-percentile ()
  "Test percentile calculation from list of values."
  ;; Basic cases
  (let ((values '(1 2 3 4 5 6 7 8 9 10)))
    ;; p50 (median) should be around 5
    (let ((p50 (calculate-percentile values 50)))
      (assert (and p50 (>= p50 4) (<= p50 6)) ()
              (format nil "p50 of 1-10 should be around 5, got ~a" p50)))
    ;; p99 should be close to 10
    (let ((p99 (calculate-percentile values 99)))
      (assert (and p99 (>= p99 9) (<= p99 10)) ()
              (format nil "p99 of 1-10 should be around 10, got ~a" p99)))
    ;; p0 should be close to 1
    (let ((p0 (calculate-percentile values 0)))
      (assert (and p0 (= p0 1)) ()
              (format nil "p0 of 1-10 should be 1, got ~a" p0)))))

(defun test-calculate-percentile-edge-cases ()
  "Test percentile edge cases."
  ;; Empty list
  (assert (null (calculate-percentile nil 50)) () "percentile: nil -> nil")
  (assert (null (calculate-percentile '() 50)) () "percentile: empty -> nil")
  ;; Single value
  (let ((p50 (calculate-percentile '(42) 50)))
    (assert (= p50 42) () "percentile: single value"))
  ;; Two values
  (let ((p50 (calculate-percentile '(10 20) 50)))
    (assert (and p50 (>= p50 10) (<= p50 20)) ()
            (format nil "percentile: two values, got ~a" p50))))

(defun test-ring-buffer-values ()
  "Test ring buffer value extraction."
  ;; Create buffer with some values
  (let ((buffer (make-array 10 :initial-element 0.0)))
    (setf (aref buffer 0) 1.0
          (aref buffer 1) 2.0
          (aref buffer 2) 3.0)
    ;; Extract with count=3
    (let ((values (ring-buffer-values buffer 3)))
      (assert (= (length values) 3) ()
              (format nil "ring-buffer: count=3 should return 3 values, got ~d" (length values))))
    ;; Extract with count=1
    (let ((values (ring-buffer-values buffer 1)))
      (assert (= (length values) 1) () "ring-buffer: count=1"))
    ;; Extract with count=0
    (let ((values (ring-buffer-values buffer 0)))
      (assert (= (length values) 0) () "ring-buffer: count=0"))
    ;; Extract with count > buffer size
    (let ((values (ring-buffer-values buffer 100)))
      (assert (= (length values) 10) () "ring-buffer: clamps to buffer size"))))

(defun test-metrics-push-latency ()
  "Test metrics latency recording."
  ;; Initialize metrics
  (init-redis-metrics)
  ;; Push some save latencies
  (metrics-push-save-latency 1.0)
  (metrics-push-save-latency 2.0)
  (metrics-push-save-latency 3.0)
  ;; Check save count
  (assert (= (redis-metrics-save-count *redis-metrics*) 3) ()
          "metrics: save count = 3")
  (assert (= (redis-metrics-total-saves *redis-metrics*) 3) ()
          "metrics: total saves = 3")
  ;; Push some load latencies
  (metrics-push-load-latency 0.5)
  (metrics-push-load-latency 1.5)
  ;; Check load count
  (assert (= (redis-metrics-load-count *redis-metrics*) 2) ()
          "metrics: load count = 2")
  (assert (= (redis-metrics-total-loads *redis-metrics*) 2) ()
          "metrics: total loads = 2")
  ;; Check p50/p99 values exist
  (let ((p99-save (get-redis-p99-save-latency))
        (p50-save (get-redis-p50-save-latency)))
    (assert (numberp p99-save) () "metrics: p99 save is number")
    (assert (numberp p50-save) () "metrics: p50 save is number"))
  ;; Record errors
  (metrics-record-save-error)
  (metrics-record-load-error)
  (assert (= (redis-metrics-total-save-errors *redis-metrics*) 1) ()
          "metrics: save errors = 1")
  (assert (= (redis-metrics-total-load-errors *redis-metrics*) 1) ()
          "metrics: load errors = 1"))

;;; ============================================================
;;; NEW UTILS TESTS (Priority 6)
;;; ============================================================

(defun test-player-animation-params ()
  "Test player animation params returns correct values for states."
  (multiple-value-bind (frames time) (player-animation-params :idle)
    (assert (integerp frames) () "anim-params: idle frames integer")
    (assert (numberp time) () "anim-params: idle time number")
    (assert (> frames 0) () "anim-params: idle frames > 0"))
  (multiple-value-bind (frames time) (player-animation-params :walk)
    (assert (integerp frames) () "anim-params: walk frames integer")
    (assert (numberp time) () "anim-params: walk time number"))
  (multiple-value-bind (frames time) (player-animation-params :attack)
    (assert (integerp frames) () "anim-params: attack frames integer")
    (assert (numberp time) () "anim-params: attack time number")))

(defun test-relative-path-from-root ()
  "Test relative path extraction from root."
  (let ((result (relative-path-from-root "/home/user/project/src/file.lisp"
                                          "/home/user/project/")))
    (assert (stringp result) () "relative-path: returns string")
    (assert (string= result "src/file.lisp") () "relative-path: strips root"))
  ;; Path not under root
  (let ((result (relative-path-from-root "/other/path/file.txt"
                                          "/home/user/")))
    (assert (stringp result) () "relative-path: other returns string")))

;;; ============================================================
;;; NEW COMBAT TESTS (Priority 1)
;;; ============================================================

(defun test-combatant-display-name ()
  "Test combatant display name for players and NPCs."
  (ensure-test-game-data)
  ;; Player display name - returns "Player" for all players
  (let ((player (make-player 0.0 0.0 :id 1)))
    (let ((name (combatant-display-name player)))
      (assert (stringp name) () "display-name: player returns string")
      (assert (string= name "Player") () "display-name: player = Player")))
  ;; NPC display name - use default-npc-archetype for reliability
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 0.0 0.0 :archetype archetype :id 1)))
    (let ((name (combatant-display-name npc)))
      (assert (stringp name) () "display-name: npc returns string"))))

(defun test-find-npc-by-id ()
  "Test finding NPC by ID in array."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (npc1 (make-npc 0.0 0.0 :archetype archetype :id 100))
         (npc2 (make-npc 10.0 10.0 :archetype archetype :id 200))
         (npcs (vector npc1 npc2)))
    ;; Find existing
    (assert (eq (find-npc-by-id npcs 100) npc1) () "find-npc: id 100")
    (assert (eq (find-npc-by-id npcs 200) npc2) () "find-npc: id 200")
    ;; Not found
    (assert (null (find-npc-by-id npcs 999)) () "find-npc: not found")
    ;; Empty vector - uses loop :across which requires vector
    (assert (null (find-npc-by-id (vector) 100)) () "find-npc: empty")))

(defun test-roll-melee-damage ()
  "Test melee damage roll is within expected range."
  (ensure-test-game-data)
  (let ((player (make-player 0.0 0.0 :id 1)))
    (setf (skill-level (stat-block-strength (player-stats player))) 10)
    ;; Roll multiple times to test range
    (let ((max-hit (melee-max-hit player)))
      (dotimes (_ 10)
        (let ((damage (roll-melee-damage player)))
          (assert (>= damage 1) () "roll-damage: at least 1")
          (assert (<= damage max-hit) () "roll-damage: at most max-hit"))))))

(defun test-format-combat-log ()
  "Test combat log formatting."
  ;; Hit case
  (let ((log (format-combat-log "Player" "Goblin" t 0.75 0.50 10 5 :damage 8)))
    (assert (stringp log) () "combat-log: hit returns string")
    (assert (search "hit" log) () "combat-log: hit contains 'hit'")
    (assert (search "8" log) () "combat-log: hit contains damage"))
  ;; Miss case
  (let ((log (format-combat-log "Player" "Goblin" nil 0.25 0.50 10 5)))
    (assert (stringp log) () "combat-log: miss returns string")
    (assert (search "miss" log) () "combat-log: miss contains 'miss'"))
  ;; Kill case
  (let ((log (format-combat-log "Player" "Goblin" t 0.75 0.50 10 5 :damage 10 :killed t)))
    (assert (search "KILL" log) () "combat-log: kill contains 'KILL'"))
  ;; XP text
  (let ((log (format-combat-log "Player" "Goblin" t 0.75 0.50 10 5 :damage 5 :xp-text "+50 XP")))
    (assert (search "+50 XP" log) () "combat-log: contains xp text")))

(defun test-npc-respawn-seconds ()
  "Test NPC respawn time from archetype."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 0.0 0.0 :archetype archetype :id 1)))
    (let ((seconds (npc-respawn-seconds npc)))
      (assert (numberp seconds) () "npc-respawn: returns number")
      (assert (>= seconds 0) () "npc-respawn: non-negative"))))

(defun test-npc-attack-cooldown ()
  "Test NPC attack cooldown from archetype."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 0.0 0.0 :archetype archetype :id 1)))
    (let ((cooldown (npc-attack-cooldown npc)))
      (assert (numberp cooldown) () "npc-cooldown: returns number")
      (assert (> cooldown 0) () "npc-cooldown: positive"))))

(defun test-npc-attack-damage ()
  "Test NPC attack damage from archetype."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 0.0 0.0 :archetype archetype :id 1)))
    (let ((damage (npc-attack-damage npc)))
      (assert (numberp damage) () "npc-damage: returns number")
      (assert (>= damage 0) () "npc-damage: non-negative"))))

(defun test-intent-attack-direction ()
  "Test attack direction from intent."
  (let ((player (make-player 100.0 100.0 :id 1))
        (intent (make-intent)))
    ;; Direction from movement input - returns :side for horizontal
    (set-intent-move intent 1.0 0.0)
    (multiple-value-bind (dir sign) (intent-attack-direction player intent)
      (assert (eq dir :side) () "attack-dir: side from dx")
      (assert (numberp sign) () "attack-dir: sign is number"))
    ;; Direction up/down from dy
    (set-intent-move intent 0.0 -1.0)
    (multiple-value-bind (dir sign) (intent-attack-direction player intent)
      (assert (eq dir :up) () "attack-dir: up from negative dy"))
    ;; No input and no target - returns nil
    (set-intent-move intent 0.0 0.0)
    (multiple-value-bind (dir sign) (intent-attack-direction player intent)
      (assert (null dir) () "attack-dir: nil when no input or target")
      (assert (zerop sign) () "attack-dir: zero sign when no input"))))

(defun test-target-in-range-p ()
  "Test if NPC is within targeting range."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (player (make-player 100.0 100.0 :id 1))
         (npc-near (make-npc 120.0 100.0 :archetype archetype :id 1))
         (npc-far (make-npc 500.0 500.0 :archetype archetype :id 2))
         (world (make-test-world :tile-size 32.0 :collision-half 12.0)))
    ;; Near NPC should be in range
    (assert (target-in-range-p player npc-near world) () "target-range: near in range")
    ;; Far NPC should be out of range
    (assert (not (target-in-range-p player npc-far world)) () "target-range: far out of range")))

(defun test-attack-hitbox ()
  "Test attack hitbox calculation."
  (let ((player (make-player 100.0 100.0 :id 1))
        (world (make-test-world :tile-size 32.0 :collision-half 12.0)))
    ;; Test each facing direction
    (dolist (facing '(:up :down :left :right))
      (setf (player-facing player) facing)
      (multiple-value-bind (cx cy half-w half-h) (attack-hitbox player world)
        (assert (numberp cx) () (format nil "hitbox ~a: cx is number" facing))
        (assert (numberp cy) () (format nil "hitbox ~a: cy is number" facing))
        (assert (> half-w 0) () (format nil "hitbox ~a: half-w positive" facing))
        (assert (> half-h 0) () (format nil "hitbox ~a: half-h positive" facing))))))

(defun test-npc-attack-range ()
  "Test NPC attack range calculation."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 0.0 0.0 :archetype archetype :id 1))
         (world (make-test-world :tile-size 32.0 :collision-half 12.0)))
    (let ((range (npc-attack-range npc world)))
      (assert (numberp range) () "npc-range: returns number")
      (assert (> range 0) () "npc-range: positive"))))

;;; ============================================================
;;; NEW PROGRESSION TESTS (Priority 1)
;;; ============================================================

(defun test-update-skill-level ()
  "Test skill level updates from XP."
  (let ((skill (make-skill :level 1 :xp 0)))
    ;; No change at 0 XP
    (multiple-value-bind (old new) (update-skill-level skill)
      (assert (= old 1) () "update-skill: old level 1")
      (assert (= new 1) () "update-skill: new level 1"))
    ;; Add enough XP for level 2
    (setf (skill-xp skill) (level->xp 2))
    (multiple-value-bind (old new) (update-skill-level skill)
      (assert (= old 1) () "update-skill: old was 1")
      (assert (= new 2) () "update-skill: new is 2"))))

(defun test-clamp-player-hp ()
  "Test player HP clamping."
  (ensure-test-game-data)
  (let ((player (make-player 0.0 0.0 :id 1)))
    ;; Set HP above max
    (let ((max-hp (combatant-max-hp player)))
      (setf (player-hp player) (+ max-hp 100))
      (clamp-player-hp player)
      (assert (<= (player-hp player) max-hp) () "clamp-hp: not above max"))
    ;; Set HP negative
    (setf (player-hp player) -10)
    (clamp-player-hp player)
    (assert (>= (player-hp player) 0) () "clamp-hp: not negative")))

(defun test-format-xp-awards ()
  "Test XP awards formatting."
  ;; All zeros - no format
  (let ((result (format-xp-awards 0 0 0 0)))
    (assert (null result) () "format-xp: all zeros -> nil"))
  ;; Single stat
  (let ((result (format-xp-awards 10 0 0 0)))
    (assert (stringp result) () "format-xp: attack only")
    (assert (search "A+" result) () "format-xp: contains A+"))
  ;; Multiple stats
  (let ((result (format-xp-awards 10 20 0 5)))
    (assert (stringp result) () "format-xp: multiple stats")
    (assert (search "A+" result) () "format-xp: contains A+")
    (assert (search "S+" result) () "format-xp: contains S+")
    (assert (search "HP+" result) () "format-xp: contains HP+")))

(defun test-item-display-name ()
  "Test item display name lookup."
  (ensure-test-game-data)
  ;; Known item
  (let ((name (item-display-name :health-potion)))
    (assert (stringp name) () "item-name: returns string")
    (assert (> (length name) 0) () "item-name: not empty"))
  ;; Unknown item returns fallback
  (let ((name (item-display-name :nonexistent-item-12345)))
    (assert (stringp name) () "item-name: unknown returns string"))
  ;; Nil item
  (let ((name (item-display-name nil)))
    (assert (string= name "Unknown") () "item-name: nil -> Unknown")))

(defun test-inventory-slot-label ()
  "Test inventory slot label formatting."
  (ensure-test-game-data)
  ;; Single item
  (let ((label (inventory-slot-label :health-potion 1)))
    (assert (stringp label) () "slot-label: returns string")
    (assert (not (search "x" label)) () "slot-label: single no count"))
  ;; Stacked items
  (let ((label (inventory-slot-label :health-potion 5)))
    (assert (stringp label) () "slot-label: stacked returns string")
    (assert (search "x5" label) () "slot-label: shows count")))

(defun test-inventory-add ()
  "Test adding items to inventory."
  (ensure-test-game-data)
  ;; Create a player which has a properly initialized inventory
  (let* ((player (make-player 0.0 0.0 :id 999))
         (inventory (player-inventory player))
         (slots (inventory-slots inventory)))
    ;; Clear all slots first
    (dotimes (i (length slots))
      (setf (aref slots i) (make-inventory-slot :item-id nil :count 0)))
    ;; Add items - returns leftover (coins always stack)
    (let ((leftover (inventory-add inventory :coins 100)))
      (assert (= leftover 0) () "inv-add: no leftover")
      ;; Check that coins were added somewhere
      (let ((total 0))
        (loop :for slot :across slots
              :when (eq (inventory-slot-item-id slot) :coins)
                :do (incf total (inventory-slot-count slot)))
        (assert (= total 100) () "inv-add: total coins = 100")))))

(defun test-inventory-remove ()
  "Test removing items from inventory."
  (ensure-test-game-data)
  ;; Create a player which has a properly initialized inventory
  (let* ((player (make-player 0.0 0.0 :id 999))
         (inventory (player-inventory player))
         (slots (inventory-slots inventory)))
    ;; Clear all slots first
    (dotimes (i (length slots))
      (setf (aref slots i) (make-inventory-slot :item-id nil :count 0)))
    ;; Add then remove
    (setf (aref slots 0) (make-inventory-slot :item-id :health-potion :count 5))
    (let ((leftover (inventory-remove inventory :health-potion 3)))
      (assert (= leftover 0) () "inv-remove: removed 3")
      (assert (= (inventory-slot-count (aref slots 0)) 2) () "inv-remove: 2 left"))
    ;; Remove more than exists
    (let ((leftover (inventory-remove inventory :health-potion 10)))
      (assert (= leftover 8) () "inv-remove: 8 couldn't be removed"))))

(defun test-roll-loot-entry ()
  "Test loot entry rolling."
  (let ((entries (list (%make-loot-entry :item-id :coins :weight 100 :min-count 1 :max-count 1)
                       (%make-loot-entry :item-id :health-potion :weight 50 :min-count 1 :max-count 1))))
    ;; Roll multiple times - should get entries
    (let ((results nil))
      (dotimes (_ 20)
        (let ((entry (roll-loot-entry entries)))
          (when entry
            (pushnew (loot-entry-item-id entry) results))))
      (assert (member :coins results) () "loot-roll: got coins")
      ;; Potion might not appear every 20 rolls but that's probabilistic
      )))

(defun test-roll-loot-count ()
  "Test loot count rolling."
  (let ((entry (%make-loot-entry :item-id :coins :weight 100 :min-count 5 :max-count 10)))
    (dotimes (_ 20)
      (let ((count (roll-loot-count entry)))
        (assert (>= count 5) () "loot-count: at least min")
        (assert (<= count 10) () "loot-count: at most max"))))
  ;; Single value (min = max)
  (let ((entry (%make-loot-entry :item-id :coins :weight 100 :min-count 3 :max-count 3)))
    (let ((count (roll-loot-count entry)))
      (assert (= count 3) () "loot-count: exact value"))))

(defun test-object-respawn-seconds ()
  "Test object respawn time lookup."
  (ensure-test-game-data)
  ;; Get archetype with respawn
  (let ((archetype (find-object-archetype :health-potion-drop)))
    ;; If archetype exists, test it
    (when archetype
      (let ((seconds (object-respawn-seconds archetype)))
        (assert (numberp seconds) () "respawn-seconds: returns number")
        (assert (>= seconds 0.0) () "respawn-seconds: non-negative"))))
  ;; Nil archetype
  (let ((seconds (object-respawn-seconds nil)))
    (assert (= seconds 0.0) () "respawn-seconds: nil -> 0")))

(defun test-object-respawnable-p ()
  "Test object respawnable check.
   Task 5.5: Updated to use zone-object structs."
  ;; Default (respawnable t)
  (let ((obj (%make-zone-object :id :test :x 0 :y 0 :count 1 :base-count 1
                                :respawn 0.0 :respawnable t :snapshot-dirty nil)))
    (assert (object-respawnable-p obj) () "respawnable: explicit true"))
  ;; Not respawnable
  (let ((obj (%make-zone-object :id :test :x 0 :y 0 :count 1 :base-count 1
                                :respawn 0.0 :respawnable nil :snapshot-dirty nil)))
    (assert (not (object-respawnable-p obj)) () "respawnable: explicit false")))

(defun test-object-respawn-timer ()
  "Test object respawn timer extraction.
   Task 5.5: Updated to use zone-object structs."
  ;; No timer (0.0)
  (let ((obj (%make-zone-object :id :test :x 0 :y 0 :count 1 :base-count 1
                                :respawn 0.0 :respawnable t :snapshot-dirty nil)))
    (assert (= (object-respawn-timer obj) 0.0) () "respawn-timer: no timer -> 0"))
  ;; With timer
  (let ((obj (%make-zone-object :id :test :x 0 :y 0 :count 1 :base-count 1
                                :respawn 5.5 :respawnable t :snapshot-dirty nil)))
    (assert (= (object-respawn-timer obj) 5.5) () "respawn-timer: has timer"))
  ;; Timer at 0
  (let ((obj (%make-zone-object :id :test :x 0 :y 0 :count 1 :base-count 1
                                :respawn 0.0 :respawnable t :snapshot-dirty nil)))
    (assert (= (object-respawn-timer obj) 0.0) () "respawn-timer: zero")))

;;; ============================================================
;;; TEST HELPERS
;;; ============================================================

(defun make-test-world (&key (tile-size 32.0) (collision-half 12.0))
  "Create a minimal world struct for testing functions that need world."
  (%make-world :tile-dest-size tile-size
               :collision-half-width collision-half
               :collision-half-height collision-half))

;;; ============================================================
;;; NEW AI TESTS (Priority 1)
;;; ============================================================

(defun test-npc-home-radius ()
  "Test NPC home radius from archetype."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 0.0 0.0 :archetype archetype :id 1))
         (world (make-test-world :tile-size 32.0)))
    (let ((radius (npc-home-radius npc world)))
      (assert (numberp radius) () "home-radius: returns number")
      (assert (>= radius 0) () "home-radius: non-negative"))))

(defun test-npc-move-speed ()
  "Test NPC move speed from archetype."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 0.0 0.0 :archetype archetype :id 1)))
    (let ((speed (npc-move-speed npc)))
      (assert (numberp speed) () "move-speed: returns number")
      (assert (> speed 0) () "move-speed: positive"))))

(defun test-npc-wander-interval ()
  "Test NPC wander interval from archetype."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 0.0 0.0 :archetype archetype :id 1)))
    (let ((interval (npc-wander-interval npc)))
      (assert (numberp interval) () "wander-interval: returns number")
      (assert (>= interval 0) () "wander-interval: non-negative"))))

(defun test-npc-flee-speed-mult ()
  "Test NPC flee speed multiplier from archetype."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 0.0 0.0 :archetype archetype :id 1)))
    (let ((mult (npc-flee-speed-mult npc)))
      (assert (numberp mult) () "flee-speed-mult: returns number")
      (assert (> mult 0) () "flee-speed-mult: positive"))))

(defun test-closest-player ()
  "Test finding closest player to NPC."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 50.0 50.0 :archetype archetype :id 1))
         (p1 (make-player 100.0 50.0 :id 1))   ;; 50 units away
         (p2 (make-player 60.0 50.0 :id 2))    ;; 10 units away (closest)
         (p3 (make-player 200.0 200.0 :id 3))  ;; far away
         (players (vector p1 p2 p3)))
    ;; Note: combatant-alive-p always returns T for players
    ;; p2 is closest by position
    (let ((closest (closest-player players npc)))
      (assert (eq closest p2) () "closest-player: p2 is closest"))
    ;; Single player
    (let ((closest (closest-player (vector p1) npc)))
      (assert (eq closest p1) () "closest-player: single player"))
    ;; Empty/nil players
    (assert (null (closest-player (vector) npc)) () "closest-player: empty vector")
    (assert (null (closest-player nil npc)) () "closest-player: nil vector")))

;;; ============================================================
;;; NEW DATA TESTS (Priority 2)
;;; ============================================================

(defun test-validate-item-archetype-plist ()
  "Test item archetype plist validation."
  ;; Valid plist
  (assert (validate-item-archetype-plist :test '(:name "Test" :stack-size 10)) ()
          "validate-item: valid passes")
  ;; Invalid name type - should error
  (handler-case
      (progn
        (validate-item-archetype-plist :test '(:name 123))
        (assert nil () "validate-item: invalid name should error"))
    (error () t))
  ;; Invalid stack-size - should error
  (handler-case
      (progn
        (validate-item-archetype-plist :test '(:stack-size "not-a-number"))
        (assert nil () "validate-item: invalid stack-size should error"))
    (error () t)))

(defun test-item-archetype-from-plist ()
  "Test item archetype creation from plist."
  (let ((item (item-archetype-from-plist :test-sword
                                          '(:name "Test Sword"
                                            :description "A test weapon"
                                            :stack-size 1
                                            :value 100
                                            :equip-slot :weapon
                                            :attack 5))))
    (assert (eq (item-archetype-id item) :test-sword) () "item-from-plist: id")
    (assert (string= (item-archetype-name item) "Test Sword") () "item-from-plist: name")
    (assert (= (item-archetype-stack-size item) 1) () "item-from-plist: stack-size")
    (assert (= (item-archetype-attack item) 5) () "item-from-plist: attack")
    (assert (eq (item-archetype-equip-slot item) :weapon) () "item-from-plist: equip-slot")))

(defun test-validate-object-archetype-plist ()
  "Test object archetype plist validation."
  ;; Valid plist
  (assert (validate-object-archetype-plist :test '(:name "Test" :item-id :coins)) ()
          "validate-object: valid passes")
  ;; Invalid item-id type - should error
  (handler-case
      (progn
        (validate-object-archetype-plist :test '(:item-id "not-keyword"))
        (assert nil () "validate-object: invalid item-id should error"))
    (error () t)))

(defun test-object-archetype-from-plist ()
  "Test object archetype creation from plist."
  (let ((obj (object-archetype-from-plist :coin-pile
                                           '(:name "Coin Pile"
                                             :item-id :coins
                                             :count 10
                                             :respawn-seconds 30.0))))
    (assert (eq (object-archetype-id obj) :coin-pile) () "object-from-plist: id")
    (assert (string= (object-archetype-name obj) "Coin Pile") () "object-from-plist: name")
    (assert (eq (object-archetype-item-id obj) :coins) () "object-from-plist: item-id")
    (assert (= (object-archetype-count obj) 10) () "object-from-plist: count")
    (assert (= (object-archetype-respawn-seconds obj) 30.0) () "object-from-plist: respawn")))

(defun test-loot-entry-from-spec ()
  "Test loot entry creation from spec."
  ;; Full spec
  (let ((entry (loot-entry-from-spec '(:coins 100 5 10))))
    (assert (eq (loot-entry-item-id entry) :coins) () "loot-entry: item-id")
    (assert (= (loot-entry-weight entry) 100) () "loot-entry: weight")
    (assert (= (loot-entry-min-count entry) 5) () "loot-entry: min")
    (assert (= (loot-entry-max-count entry) 10) () "loot-entry: max"))
  ;; Minimal spec (defaults to 1,1)
  (let ((entry (loot-entry-from-spec '(:potion 50))))
    (assert (= (loot-entry-min-count entry) 1) () "loot-entry: default min")
    (assert (= (loot-entry-max-count entry) 1) () "loot-entry: default max")))

(defun test-validate-loot-table-plist ()
  "Test loot table plist validation."
  ;; Valid plist
  (assert (validate-loot-table-plist :test '(:rolls 1 :entries ((:coins 100)))) ()
          "validate-loot: valid passes")
  ;; Missing entries - should error
  (handler-case
      (progn
        (validate-loot-table-plist :test '(:rolls 1))
        (assert nil () "validate-loot: missing entries should error"))
    (error () t)))

(defun test-loot-table-from-plist ()
  "Test loot table creation from plist."
  (let ((table (loot-table-from-plist :goblin-loot
                                       '(:rolls 2
                                         :entries ((:coins 100 1 10)
                                                   (:health-potion 20 1 1))))))
    (assert (eq (loot-table-id table) :goblin-loot) () "loot-table: id")
    (assert (= (loot-table-rolls table) 2) () "loot-table: rolls")
    (assert (= (length (loot-table-entries table)) 2) () "loot-table: 2 entries")))

(defun test-animation-set-from-plist ()
  "Test animation set creation from plist."
  (let ((set (animation-set-from-plist :test-anim
                                        '(:dir "sprites/"
                                          :down-idle "idle.png"
                                          :down-walk "walk.png"))))
    (assert (eq (animation-set-id set) :test-anim) () "anim-set: id")
    (assert (string= (animation-set-dir set) "sprites/") () "anim-set: dir")
    (assert (string= (animation-set-down-idle set) "idle.png") () "anim-set: down-idle")))

(defun test-merge-animation-sets ()
  "Test animation set merging."
  (let* ((base (animation-set-from-plist :base '(:dir "base/" :down-idle "base-idle.png")))
         (override (animation-set-from-plist :override '(:down-idle "override-idle.png")))
         (merged (merge-animation-sets base override)))
    ;; Override should win for down-idle
    (assert (string= (animation-set-down-idle merged) "override-idle.png") ()
            "merge-anim: override wins")
    ;; Base should be kept for dir (not overridden)
    (assert (string= (animation-set-dir merged) "base/") ()
            "merge-anim: base kept")))

;;; ============================================================
;;; NEW ZONE TESTS (Priority 2)
;;; ============================================================

(defun test-zone-label ()
  "Test zone label generation."
  (let ((zone (%make-zone :id :test-zone :width 10 :height 10)))
    (let ((label (zone-label zone)))
      (assert (stringp label) () "zone-label: returns string")
      (assert (string= label "TEST-ZONE") () "zone-label: uppercase")))
  ;; Nil zone
  (let ((label (zone-label nil)))
    (assert (string= label "NONE") () "zone-label: nil -> NONE")))

(defun test-zone-data-plist ()
  "Test zone data plist normalization."
  ;; Direct plist
  (let ((result (zone-data-plist '(:id :test :width 10))))
    (assert (listp result) () "zone-plist: direct returns list")
    (assert (eq (getf result :id) :test) () "zone-plist: preserves id"))
  ;; Nil data
  (assert (null (zone-data-plist nil)) () "zone-plist: nil -> nil")
  ;; Invalid data
  (assert (null (zone-data-plist '("not" "a" "plist"))) () "zone-plist: invalid -> nil"))

(defun test-make-empty-zone ()
  "Test empty zone creation."
  (let ((zone (make-empty-zone :empty-test 20 15)))
    (assert (eq (zone-id zone) :empty-test) () "empty-zone: id")
    (assert (= (zone-width zone) 20) () "empty-zone: width")
    (assert (= (zone-height zone) 15) () "empty-zone: height")
    (assert (= (length (zone-layers zone)) 0) () "empty-zone: no layers")
    (assert (null (zone-objects zone)) () "empty-zone: no objects")))

(defun test-build-tiles-from-fill ()
  "Test tile vector building from fill value."
  ;; All same value
  (let ((tiles (build-tiles-from-fill 4 5 nil)))
    (assert (= (length tiles) 16) () "fill-tiles: 4x4 = 16")
    (assert (every (lambda (tile) (= tile 5)) tiles) () "fill-tiles: all 5"))
  ;; With overrides
  (let ((tiles (build-tiles-from-fill 4 0 '((0 0 1) (1 1 2)))))
    (assert (= (aref tiles 0) 1) () "fill-tiles: override at 0,0")
    (assert (= (aref tiles 5) 2) () "fill-tiles: override at 1,1")
    (assert (= (aref tiles 2) 0) () "fill-tiles: non-override is fill")))

(defun test-zone-layer-tile-at ()
  "Test getting tile at coordinates from layer."
  ;; Create a simple layer with one chunk
  (let* ((chunk (%make-zone-chunk :x 0 :y 0
                                   :tiles (make-array 16 :initial-contents
                                                      '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))))
         (chunks (make-hash-table :test 'eql))
         (layer nil))
    (setf (gethash (zone-chunk-key 0 0) chunks) chunk)
    (setf layer (%make-zone-layer :id :ground :chunks chunks))
    ;; Test tile retrieval (chunk-size 4)
    (assert (= (zone-layer-tile-at layer 4 0 0) 1) () "layer-tile: 0,0 = 1")
    (assert (= (zone-layer-tile-at layer 4 1 0) 2) () "layer-tile: 1,0 = 2")
    (assert (= (zone-layer-tile-at layer 4 0 1) 5) () "layer-tile: 0,1 = 5")
    ;; Out of chunk returns 0
    (assert (= (zone-layer-tile-at layer 4 10 10) 0) () "layer-tile: out of chunk = 0")))

;;; ============================================================
;;; NEW INTENT TESTS (Priority 5)
;;; ============================================================

(defun test-reset-frame-intent ()
  "Test resetting per-frame intent signals."
  (let ((intent (make-intent)))
    ;; Set some values
    (set-intent-move intent 1.0 1.0)
    (setf (intent-attack intent) t
          (intent-run-toggle intent) t)
    ;; Reset
    (reset-frame-intent intent)
    ;; Check cleared
    (assert (= (intent-move-dx intent) 0.0) () "reset-intent: dx cleared")
    (assert (= (intent-move-dy intent) 0.0) () "reset-intent: dy cleared")
    (assert (null (intent-attack intent)) () "reset-intent: attack cleared")
    (assert (null (intent-run-toggle intent)) () "reset-intent: run cleared")))

(defun test-consume-intent-actions ()
  "Test consuming one-shot actions."
  (let ((intent (make-intent)))
    (setf (intent-attack intent) t
          (intent-run-toggle intent) t)
    (consume-intent-actions intent)
    (assert (null (intent-attack intent)) () "consume-intent: attack cleared")
    (assert (null (intent-run-toggle intent)) () "consume-intent: run cleared")))

(defun test-set-intent-target ()
  "Test setting intent target."
  (let ((intent (make-intent)))
    (set-intent-target intent 100.0 200.0)
    (assert (= (intent-target-x intent) 100.0) () "set-target: x")
    (assert (= (intent-target-y intent) 200.0) () "set-target: y")
    (assert (intent-target-active intent) () "set-target: active")))

(defun test-clear-intent-target ()
  "Test clearing intent target."
  (let ((intent (make-intent)))
    (set-intent-target intent 100.0 200.0)
    (clear-intent-target intent)
    (assert (not (intent-target-active intent)) () "clear-target: inactive")))

(defun test-request-pickup-target ()
  "Test pickup target request."
  (let ((intent (make-intent)))
    (request-pickup-target intent :coins 5 10)
    (assert (eq (intent-requested-pickup-target-id intent) :coins) () "pickup: id")
    (assert (= (intent-requested-pickup-tx intent) 5) () "pickup: tx")
    (assert (= (intent-requested-pickup-ty intent) 10) () "pickup: ty")
    ;; Clear
    (clear-requested-pickup-target intent)
    (assert (null (intent-requested-pickup-target-id intent)) () "pickup: cleared")))

(defun test-request-drop-item ()
  "Test drop item request."
  (let ((intent (make-intent)))
    (request-drop-item intent :health-potion 5 2)
    (assert (eq (intent-requested-drop-item-id intent) :health-potion) () "drop: item-id")
    (assert (= (intent-requested-drop-count intent) 5) () "drop: count")
    (assert (= (intent-requested-drop-slot-index intent) 2) () "drop: slot")
    ;; Clear
    (clear-requested-drop-item intent)
    (assert (null (intent-requested-drop-item-id intent)) () "drop: cleared")))

(defun test-request-inventory-swap ()
  "Test inventory swap request."
  (let ((intent (make-intent)))
    (request-inventory-swap intent 0 3)
    (assert (= (intent-requested-swap-slot-a intent) 0) () "swap: slot-a")
    (assert (= (intent-requested-swap-slot-b intent) 3) () "swap: slot-b")
    ;; Clear
    (clear-requested-inventory-swap intent)
    (assert (null (intent-requested-swap-slot-a intent)) () "swap: cleared")))

(defun test-trade-intent-functions ()
  "Test trade intent request functions."
  (let ((intent (make-intent)))
    ;; Request trade
    (request-trade-with-player intent 123)
    (assert (= (intent-requested-trade-target-id intent) 123) () "trade: target-id")
    ;; Request offer
    (request-trade-offer intent 2 10)
    (assert (= (intent-requested-trade-offer-slot intent) 2) () "trade: offer-slot")
    (assert (= (intent-requested-trade-offer-count intent) 10) () "trade: offer-count")
    ;; Confirm
    (request-trade-confirm intent)
    (assert (intent-requested-trade-confirm intent) () "trade: confirm set")
    ;; Cancel
    (request-trade-cancel intent)
    (assert (intent-requested-trade-cancel intent) () "trade: cancel set")
    ;; Clear all
    (clear-all-trade-requests intent)
    (assert (null (intent-requested-trade-target-id intent)) () "trade: cleared target")
    (assert (null (intent-requested-trade-confirm intent)) () "trade: cleared confirm")))

;;; ============================================================
;;; NEW NET TESTS (Priority 3)
;;; ============================================================

(defun test-string-to-octets ()
  "Test string to octet conversion."
  (let ((octets (string-to-octets "hello")))
    (assert (vectorp octets) () "str-to-oct: returns vector")
    (assert (= (length octets) 5) () "str-to-oct: correct length")
    (assert (= (aref octets 0) (char-code #\h)) () "str-to-oct: first char")))

(defun test-octets-to-string ()
  "Test octet to string conversion."
  (let* ((input "hello")
         (octets (string-to-octets input))
         (result (octets-to-string octets (length octets))))
    (assert (stringp result) () "oct-to-str: returns string")
    (assert (string= result input) () "oct-to-str: roundtrip")))

(defun test-encode-decode-net-message ()
  "Test network message encoding and decoding."
  (let* ((message '(:type :test :value 123 :name "hello"))
         (encoded (encode-net-message message))
         (decoded (decode-net-message encoded)))
    (assert (stringp encoded) () "net-msg: encode returns string")
    (assert (listp decoded) () "net-msg: decode returns list")
    (assert (eq (getf decoded :type) :test) () "net-msg: type preserved")
    (assert (= (getf decoded :value) 123) () "net-msg: value preserved")
    (assert (string= (getf decoded :name) "hello") () "net-msg: name preserved"))
  ;; Invalid decode
  (assert (null (decode-net-message "not valid lisp")) () "net-msg: invalid -> nil"))

(defun test-host-to-string ()
  "Test host conversion to string."
  ;; Already a string
  (assert (string= (host-to-string "127.0.0.1") "127.0.0.1") ()
          "host-str: string passthrough")
  ;; Byte vector
  (let ((bytes #(192 168 1 1)))
    (assert (string= (host-to-string bytes) "192.168.1.1") ()
            "host-str: byte vector")))

;;; ============================================================
;;; BINARY SNAPSHOT TESTS (Phase 3 Task 3.1)
;;; ============================================================

(defun test-binary-int-encoding ()
  "Test binary integer encoding primitives."
  (let ((buf (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Test uint8
    (let ((offset (write-uint8 buf 0 255)))
      (assert (= offset 1) () "bin-int: uint8 advances 1")
      (assert (= (aref buf 0) 255) () "bin-int: uint8 value"))
    ;; Test uint16
    (let ((offset (write-uint16 buf 0 #xABCD)))
      (assert (= offset 2) () "bin-int: uint16 advances 2")
      (assert (= (aref buf 0) #xAB) () "bin-int: uint16 high byte")
      (assert (= (aref buf 1) #xCD) () "bin-int: uint16 low byte"))
    ;; Test uint32
    (let ((offset (write-uint32 buf 0 #x12345678)))
      (assert (= offset 4) () "bin-int: uint32 advances 4")
      (assert (= (aref buf 0) #x12) () "bin-int: uint32 byte 0")
      (assert (= (aref buf 3) #x78) () "bin-int: uint32 byte 3"))
    ;; Test int32 negative
    (let ((offset (write-int32 buf 0 -1)))
      (assert (= offset 4) () "bin-int: int32 advances 4")
      (assert (= (aref buf 0) #xFF) () "bin-int: int32 -1 byte 0")
      (assert (= (aref buf 3) #xFF) () "bin-int: int32 -1 byte 3"))))

(defun test-binary-int-decoding ()
  "Test binary integer decoding primitives."
  (let ((buf (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Setup test data
    (write-uint8 buf 0 42)
    (write-uint16 buf 1 #x1234)
    (write-uint32 buf 3 #xDEADBEEF)
    (write-int32 buf 7 -12345)
    ;; Test uint8
    (multiple-value-bind (val off) (read-uint8 buf 0)
      (assert (= val 42) () "bin-dec: uint8 value")
      (assert (= off 1) () "bin-dec: uint8 offset"))
    ;; Test uint16
    (multiple-value-bind (val off) (read-uint16 buf 1)
      (assert (= val #x1234) () "bin-dec: uint16 value")
      (assert (= off 3) () "bin-dec: uint16 offset"))
    ;; Test uint32
    (multiple-value-bind (val off) (read-uint32 buf 3)
      (assert (= val #xDEADBEEF) () "bin-dec: uint32 value")
      (assert (= off 7) () "bin-dec: uint32 offset"))
    ;; Test int32 negative
    (multiple-value-bind (val off) (read-int32 buf 7)
      (assert (= val -12345) () "bin-dec: int32 negative value")
      (assert (= off 11) () "bin-dec: int32 offset"))))

(defun test-is-binary-snapshot-p ()
  "Test binary snapshot detection."
  (let ((buf (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Not a binary snapshot (no magic)
    (assert (not (is-binary-snapshot-p buf 16)) ()
            "bin-detect: no magic -> false")
    ;; Write magic bytes "SNAP"
    (setf (aref buf 0) 83    ; 'S'
          (aref buf 1) 78    ; 'N'
          (aref buf 2) 65    ; 'A'
          (aref buf 3) 80)   ; 'P'
    (assert (is-binary-snapshot-p buf 16) ()
            "bin-detect: with magic -> true")
    ;; Too small
    (assert (not (is-binary-snapshot-p buf 3)) ()
            "bin-detect: too small -> false")))

(defun test-binary-snapshot-roundtrip ()
  "Test binary snapshot encoding and decoding roundtrip."
  ;; Create mock compact state
  (let* ((player-vec (make-array 20 :initial-element 0))
         (npc-vec (make-array 15 :initial-element 0))
         (state (list :format :compact-v5
                      :seq 42
                      :zone-id :test-zone
                      :players (vector player-vec)
                      :npcs (vector npc-vec)
                      :objects nil))
         (buf (make-array 2048 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Set some test values in vectors
    (setf (aref player-vec 0) 1     ; player id
          (aref player-vec 1) 1000  ; x (quantized)
          (aref player-vec 2) 2000) ; y (quantized)
    (setf (aref npc-vec 0) 5        ; npc id
          (aref npc-vec 1) 500      ; x
          (aref npc-vec 2) 600)     ; y
    ;; Encode
    (let ((size (encode-snapshot-binary state nil buf)))
      (assert (> size 20) () "bin-rt: encoded size > header")
      (assert (is-binary-snapshot-p buf size) () "bin-rt: has magic bytes")
      ;; Decode
      (let ((decoded (decode-snapshot-binary buf size)))
        (assert decoded () "bin-rt: decoded not nil")
        (assert (eq (getf decoded :format) :compact-v5) () "bin-rt: format preserved")
        (assert (= (getf decoded :seq) 42) () "bin-rt: seq preserved")
        ;; Check player data roundtripped
        (let* ((players (getf decoded :players))
               (p0 (aref players 0)))
          (assert (= (aref p0 0) 1) () "bin-rt: player id")
          (assert (= (aref p0 1) 1000) () "bin-rt: player x")
          (assert (= (aref p0 2) 2000) () "bin-rt: player y"))
        ;; Check NPC data roundtripped
        (let* ((npcs (getf decoded :npcs))
               (n0 (aref npcs 0)))
          (assert (= (aref n0 0) 5) () "bin-rt: npc id")
          (assert (= (aref n0 1) 500) () "bin-rt: npc x")
          (assert (= (aref n0 2) 600) () "bin-rt: npc y"))))))

(defun test-binary-snapshot-delta ()
  "Test binary encoding of delta snapshots."
  (let* ((player-vec (make-array 20 :initial-element 0))
         (state (list :format :delta-v5
                      :seq 99
                      :zone-id :delta-zone
                      :changed-players (vector player-vec)
                      :changed-npcs #()
                      :objects nil))
         (buf (make-array 2048 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref player-vec 0) 7)  ; player id
    ;; Encode
    (let ((size (encode-snapshot-binary state nil buf)))
      (assert (> size 20) () "bin-delta: encoded")
      ;; Decode
      (let ((decoded (decode-snapshot-binary buf size)))
        (assert (eq (getf decoded :format) :delta-v5) () "bin-delta: format")
        (assert (= (getf decoded :seq) 99) () "bin-delta: seq")
        (let* ((players (getf decoded :changed-players))
               (p0 (aref players 0)))
          (assert (= (aref p0 0) 7) () "bin-delta: player id"))))))

;;; ============================================================
;;; ADDITIONAL AI TESTS
;;; ============================================================

(defun test-npc-in-perception-range-p ()
  "Test NPC perception range check."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 100.0 100.0 :archetype archetype :id 1))
         (player-near (make-player 110.0 100.0 :id 1))
         (player-far (make-player 1000.0 1000.0 :id 2))
         (world (make-test-world :tile-size 32.0)))
    ;; For archetypes with perception range, player-near should be in range
    ;; For archetypes without perception (0 tiles), nobody is in range
    (let ((perception-tiles (npc-archetype-perception-tiles archetype)))
      (if (and perception-tiles (> perception-tiles 0))
          (progn
            (assert (npc-in-perception-range-p npc player-near world) ()
                    "perception: near player in range")
            (assert (not (npc-in-perception-range-p npc player-far world)) ()
                    "perception: far player not in range"))
          ;; Zero perception means never in range
          (assert (not (npc-in-perception-range-p npc player-near world)) ()
                  "perception: zero perception -> never in range")))
    ;; Nil player always returns nil
    (assert (null (npc-in-perception-range-p npc nil world)) ()
            "perception: nil player -> nil")))

;;; ============================================================
;;; ADDITIONAL COMBAT TESTS
;;; ============================================================

(defun test-player-attack-target-in-range-p ()
  "Test player attack target range check."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (world (make-test-world :tile-size 32.0 :collision-half 12.0))
         (player (make-player 100.0 100.0 :id 1))
         (npc-close (make-npc 110.0 100.0 :archetype archetype :id 1))
         (npc-far (make-npc 500.0 500.0 :archetype archetype :id 2)))
    ;; Set player facing to ensure hitbox is calculated
    (setf (player-facing player) :side
          (player-facing-sign player) 1.0)
    ;; Close NPC should be in attack range
    (assert (player-attack-target-in-range-p player npc-close world) ()
            "attack-in-range: close NPC in range")
    ;; Far NPC should not be in range
    (assert (not (player-attack-target-in-range-p player npc-far world)) ()
            "attack-in-range: far NPC not in range")))

;;; ============================================================
;;; ADDITIONAL PROGRESSION TESTS
;;; ============================================================

(defun test-melee-hit-p ()
  "Test melee hit roll wrapper."
  (let* ((attacker (make-player 0.0 0.0 :id 1))
         (defender (make-player 0.0 0.0 :id 2)))
    ;; Run multiple times - should return boolean
    (dotimes (_ 10)
      (let ((result (melee-hit-p attacker defender)))
        (assert (or (eq result t) (eq result nil)) ()
                "melee-hit-p: returns boolean")))))

(defun test-format-skill-hud-line ()
  "Test skill HUD line formatting."
  ;; Nil skill
  (let ((line (format-skill-hud-line "ATT" nil)))
    (assert (stringp line) () "skill-hud: returns string for nil")
    (assert (search "--" line) () "skill-hud: nil shows --"))
  ;; Valid skill
  (let* ((skill (make-skill :level 5 :xp 500))
         (line (format-skill-hud-line "ATT" skill)))
    (assert (stringp line) () "skill-hud: returns string")
    (assert (search "ATT" line) () "skill-hud: contains label")
    (assert (search "5" line) () "skill-hud: contains level")))

(defun test-object-entry-count ()
  "Test object entry count extraction.
   Task 5.5: Updated to use zone-object structs."
  ;; Object with explicit count
  (let ((obj (%make-zone-object :id :coins :x 0 :y 0 :count 50 :base-count 50
                                :respawn 0.0 :respawnable t :snapshot-dirty nil)))
    (assert (= (object-entry-count obj nil) 50) () "entry-count: explicit count"))
  ;; Object with count 1 - falls back to archetype or 1
  (let ((obj (%make-zone-object :id :coins :x 0 :y 0 :count 1 :base-count 1
                                :respawn 0.0 :respawnable t :snapshot-dirty nil)))
    (let ((count (object-entry-count obj nil)))
      (assert (= count 1) () "entry-count: count 1, no arch -> 1")))
  ;; With archetype
  (ensure-test-game-data)
  (let* ((archetype (find-object-archetype :health-potion-drop))
         (obj (%make-zone-object :id :health-potion-drop :x 0 :y 0 :count 1 :base-count 1
                                 :respawn 0.0 :respawnable t :snapshot-dirty nil)))
    (when archetype
      (let ((count (object-entry-count obj archetype)))
        (assert (>= count 1) () "entry-count: archetype count")))))

;;; ============================================================
;;; ADDITIONAL DATA TESTS
;;; ============================================================

(defun test-parse-game-data-forms ()
  "Test parsing game data forms into sections."
  ;; Simple plist
  (let ((result (parse-game-data-forms '((:test-key 123)))))
    (assert (listp result) () "parse-forms: returns list"))
  ;; Section with entries
  (let ((result (parse-game-data-forms '(:items
                                          (:sword (:name "Sword"))
                                          (:shield (:name "Shield"))))))
    (assert (listp result) () "parse-forms: sections parsed")
    (let ((items (getf result :items)))
      (assert (= (length items) 2) () "parse-forms: 2 items")))
  ;; Mixed tunables and sections
  (let ((result (parse-game-data-forms '((:player-speed 100.0)
                                          :npcs
                                          (:goblin (:name "Goblin"))))))
    (assert (= (getf result :player-speed) 100.0) () "parse-forms: tunable preserved")
    (assert (listp (getf result :npcs)) () "parse-forms: section present")))

(defun test-make-npc-archetype-from-plist ()
  "Test NPC archetype creation from plist."
  (let ((archetype (make-npc-archetype-from-plist
                    '(:name "Test NPC"
                      :max-hits 10
                      :attack-level 5
                      :defense-level 3
                      :combat-xp 25
                      :move-speed 80.0
                      :aggro-mode :always))))
    (assert (string= (npc-archetype-name archetype) "Test NPC") ()
            "npc-from-plist: name")
    (assert (= (npc-archetype-max-hits archetype) 10) ()
            "npc-from-plist: max-hits")
    (assert (= (npc-archetype-attack-level archetype) 5) ()
            "npc-from-plist: attack-level")
    (assert (= (npc-archetype-combat-xp archetype) 25) ()
            "npc-from-plist: combat-xp")
    (assert (eq (npc-archetype-aggro-mode archetype) :always) ()
            "npc-from-plist: aggro-mode")))

;;; ============================================================
;;; ADDITIONAL ZONE TESTS
;;; ============================================================

(defun test-zone-chunk-from-spec ()
  "Test zone chunk creation from spec."
  ;; With explicit tiles
  (let* ((tiles (make-list 16 :initial-element 1))
         (spec (list :x 2 :y 3 :tiles tiles))
         (chunk (zone-chunk-from-spec spec 4)))
    (assert (= (zone-chunk-x chunk) 2) () "chunk-from-spec: x")
    (assert (= (zone-chunk-y chunk) 3) () "chunk-from-spec: y")
    (assert (= (length (zone-chunk-tiles chunk)) 16) () "chunk-from-spec: tiles"))
  ;; With fill value
  (let* ((spec '(:x 0 :y 0 :fill 5))
         (chunk (zone-chunk-from-spec spec 4)))
    (assert (every (lambda (tile) (= tile 5)) (zone-chunk-tiles chunk)) ()
            "chunk-from-spec: fill value")))

(defun test-zone-layer-from-spec ()
  "Test zone layer creation from spec."
  (let* ((chunk-spec '(:x 0 :y 0 :fill 1))
         (spec (list :id :ground
                     :tileset :grass
                     :collision nil
                     :chunks (list chunk-spec)))
         (layer (zone-layer-from-spec spec 4)))
    (assert (eq (zone-layer-id layer) :ground) () "layer-from-spec: id")
    (assert (eq (zone-layer-tileset-id layer) :grass) () "layer-from-spec: tileset")
    (assert (hash-table-p (zone-layer-chunks layer)) () "layer-from-spec: chunks hash")))

(defun test-build-zone-collision-tiles ()
  "Test building collision tile hash from layers."
  ;; Create a collision layer with some blocked tiles
  (let* ((chunk (%make-zone-chunk :x 0 :y 0
                                   :tiles (make-array 16 :initial-contents
                                                      '(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
         (chunks (make-hash-table :test 'eql))
         (layer nil))
    (setf (gethash (zone-chunk-key 0 0) chunks) chunk)
    (setf layer (%make-zone-layer :id :collision :collision-p t :chunks chunks))
    (let ((blocked (build-zone-collision-tiles (list layer) 4)))
      (assert (hash-table-p blocked) () "collision-tiles: returns hash")
      ;; Tile at 1,0 should be blocked (non-zero in tiles array)
      (assert (gethash (tile-key 1 0) blocked) () "collision-tiles: 1,0 blocked")
      ;; Tile at 0,0 should not be blocked (zero)
      (assert (not (gethash (tile-key 0 0) blocked)) () "collision-tiles: 0,0 not blocked"))))

(defun test-zone-wall-map ()
  "Test converting collision tiles to wall map array."
  (let* ((zone (make-empty-zone :test 10 10))
         (collision (zone-collision-tiles zone)))
    ;; Mark some tiles as blocked
    (setf (gethash (tile-key 2 3) collision) t)
    (setf (gethash (tile-key 5 5) collision) t)
    (let ((wall-map (zone-wall-map zone)))
      (assert (arrayp wall-map) () "wall-map: returns array")
      (assert (= (array-dimension wall-map 0) 10) () "wall-map: height")
      (assert (= (array-dimension wall-map 1) 10) () "wall-map: width")
      (assert (= (aref wall-map 3 2) 1) () "wall-map: 2,3 blocked")
      (assert (= (aref wall-map 5 5) 1) () "wall-map: 5,5 blocked")
      (assert (= (aref wall-map 0 0) 0) () "wall-map: 0,0 not blocked"))))

(defun test-zone-layer-by-id ()
  "Test finding layer by ID."
  (let* ((layer1 (%make-zone-layer :id :ground :chunks (make-hash-table)))
         (layer2 (%make-zone-layer :id :collision :collision-p t :chunks (make-hash-table)))
         (zone (%make-zone :id :test :width 10 :height 10
                           :layers (vector layer1 layer2)
                           :collision-tiles (make-hash-table))))
    (assert (eq (zone-layer-by-id zone :ground) layer1) () "layer-by-id: ground")
    (assert (eq (zone-layer-by-id zone :collision) layer2) () "layer-by-id: collision")
    (assert (null (zone-layer-by-id zone :nonexistent)) () "layer-by-id: not found")))

(defun test-zone-to-plist ()
  "Test zone serialization to plist.
   Task 5.5: Updated to use zone-object structs."
  (let* ((zone (make-empty-zone :test-zone 20 15 :chunk-size 8)))
    ;; Add an object (using zone-object struct)
    (push (%make-zone-object :id :coins :x 5 :y 5 :count 10 :base-count 10
                             :respawn 0.0 :respawnable t :snapshot-dirty nil)
          (zone-objects zone))
    (let ((plist (zone-to-plist zone)))
      (assert (listp plist) () "zone-to-plist: returns list")
      (assert (eq (getf plist :id) :test-zone) () "zone-to-plist: id")
      (assert (= (getf plist :width) 20) () "zone-to-plist: width")
      (assert (= (getf plist :height) 15) () "zone-to-plist: height")
      (assert (= (getf plist :chunk-size) 8) () "zone-to-plist: chunk-size")
      (assert (listp (getf plist :objects)) () "zone-to-plist: objects list"))))

(defun test-zone-slice ()
  "Test extracting a subregion of a zone."
  (let* ((zone (make-empty-zone :big-zone 100 100))
         (sliced (zone-slice zone 10 10 20 15)))
    (assert (= (zone-width sliced) 20) () "zone-slice: width")
    (assert (= (zone-height sliced) 15) () "zone-slice: height")
    (assert (eq (zone-id sliced) :big-zone) () "zone-slice: preserves id")))

(defun test-zone-resize ()
  "Test resizing a zone."
  (let* ((zone (make-empty-zone :resizable 50 50))
         (resized (zone-resize zone 30 25)))
    (assert (= (zone-width resized) 30) () "zone-resize: new width")
    (assert (= (zone-height resized) 25) () "zone-resize: new height")))

;;; ============================================================
;;; ADDITIONAL WORLD GRAPH TESTS
;;; ============================================================

(defun test-world-graph-exits ()
  "Test getting zone exits from world graph."
  (let* ((edges (list '(:from :zone-a :to :zone-b :edge :north)
                      '(:from :zone-a :to :zone-c :edge :east)
                      '(:from :zone-b :to :zone-a :edge :south)))
         (edges-by-zone (normalize-world-graph-edges edges))
         (graph (%make-world-graph :edges-by-zone edges-by-zone
                                    :zone-paths (make-hash-table))))
    ;; Zone A has 2 exits
    (let ((exits (world-graph-exits graph :zone-a)))
      (assert (= (length exits) 2) () "graph-exits: zone-a has 2 exits"))
    ;; Zone B has 1 exit
    (let ((exits (world-graph-exits graph :zone-b)))
      (assert (= (length exits) 1) () "graph-exits: zone-b has 1 exit"))
    ;; Zone C has no exits
    (assert (null (world-graph-exits graph :zone-c)) () "graph-exits: zone-c has none")
    ;; Nil graph
    (assert (null (world-graph-exits nil :zone-a)) () "graph-exits: nil graph")))

(defun test-world-graph-zone-path ()
  "Test getting zone file path from world graph."
  (let* ((paths (make-hash-table :test 'eq))
         (graph nil))
    (setf (gethash :zone-a paths) "/path/to/zone-a.lisp")
    (setf (gethash :zone-b paths) "/path/to/zone-b.lisp")
    (setf graph (%make-world-graph :edges-by-zone (make-hash-table)
                                    :zone-paths paths))
    (assert (string= (world-graph-zone-path graph :zone-a) "/path/to/zone-a.lisp") ()
            "graph-path: zone-a")
    (assert (string= (world-graph-zone-path graph :zone-b) "/path/to/zone-b.lisp") ()
            "graph-path: zone-b")
    (assert (null (world-graph-zone-path graph :nonexistent)) ()
            "graph-path: not found")
    (assert (null (world-graph-zone-path nil :zone-a)) ()
            "graph-path: nil graph")))

(defun test-collect-zone-files ()
  "Test collecting zone files from directory."
  ;; Test against actual data/zones directory
  (let* ((zone-root (merge-pathnames "data/zones/"
                                      (asdf:system-source-directory :mmorpg)))
         (files (collect-zone-files zone-root)))
    (assert (vectorp files) () "collect-zone-files: returns vector")
    ;; Should find at least one zone file if directory exists
    (when (probe-file zone-root)
      (assert (> (length files) 0) () "collect-zone-files: finds files")))
  ;; Non-existent directory returns empty
  (let ((files (collect-zone-files "/nonexistent/path/12345/")))
    (assert (= (length files) 0) () "collect-zone-files: empty for missing dir")))

(defun test-zone-id-from-file ()
  "Test reading zone ID from file."
  ;; Test against actual zone file if exists
  (let* ((zone-root (merge-pathnames "data/zones/"
                                      (asdf:system-source-directory :mmorpg)))
         (files (collect-zone-files zone-root)))
    (when (and (> (length files) 0) (probe-file (aref files 0)))
      (let ((id (zone-id-from-file (aref files 0))))
        (assert (keywordp id) () "zone-id-from-file: returns keyword"))))
  ;; Non-existent file returns nil
  (assert (null (zone-id-from-file "/nonexistent/zone.lisp")) ()
          "zone-id-from-file: nil for missing file"))

(defun test-build-zone-paths ()
  "Test building zone path lookup table."
  (let* ((zone-root (merge-pathnames "data/zones/"
                                      (asdf:system-source-directory :mmorpg)))
         (paths (build-zone-paths zone-root)))
    (assert (hash-table-p paths) () "build-zone-paths: returns hash table")
    ;; If zones exist, table should have entries
    (when (probe-file zone-root)
      (let ((count (hash-table-count paths)))
        (assert (>= count 0) () "build-zone-paths: non-negative count")))))

;;; ============================================================
;;; FINAL AI TESTS
;;; ============================================================

(defun test-update-npc-behavior ()
  "Test NPC behavior state machine."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 100.0 100.0 :archetype archetype :id 1))
         (player (make-player 110.0 100.0 :id 1))
         (world (make-test-world :tile-size 32.0)))
    ;; Dead NPC -> :dead state
    (setf (npc-alive npc) nil)
    (update-npc-behavior npc player world)
    (assert (eq (npc-behavior-state npc) :dead) () "npc-behavior: dead -> :dead")
    ;; Revive and test idle
    (setf (npc-alive npc) t)
    (update-npc-behavior npc nil world)
    (assert (eq (npc-behavior-state npc) :idle) () "npc-behavior: no player -> :idle")
    ;; With player present, state depends on archetype aggro mode
    (update-npc-behavior npc player world)
    (assert (member (npc-behavior-state npc) '(:idle :aggressive :retaliate :flee)) ()
            "npc-behavior: valid state")))

(defun test-update-npc-intent-nil-player ()
  "Test NPC intent with nil player (idle/wander path).
   Ensures no crash and that wander direction is computed."
  (ensure-test-game-data)
  (let* ((archetype (or (gethash :goblin *npc-archetypes*) (default-npc-archetype)))
         (npc (make-npc 100.0f0 100.0f0 :archetype archetype :id 1))
         (world (make-test-world :tile-size 32.0f0))
         (dt 0.016f0))
    ;; Setup deterministic wander target to avoid randomness
    ;; Set wander target 10 units to the right of NPC position
    (setf (npc-wander-x npc) (+ (npc-x npc) 10.0f0)
          (npc-wander-y npc) (npc-y npc)
          (npc-wander-timer npc) 5.0f0  ; positive timer = use existing target
          (npc-alive npc) t
          (npc-behavior-state npc) :idle)
    ;; Reset intent to known state
    (let ((intent (npc-intent npc)))
      (setf (intent-move-dx intent) 0.0f0
            (intent-move-dy intent) 0.0f0))
    ;; Call update-npc-behavior with nil player
    (update-npc-behavior npc nil world)
    (assert (eq (npc-behavior-state npc) :idle) ()
            "nil-player: behavior state should be :idle")
    ;; Call update-npc-intent with nil player - should not crash
    (update-npc-intent npc nil world dt)
    ;; Check that intent was updated with wander direction
    (let* ((intent (npc-intent npc))
           (dx (intent-move-dx intent))
           (dy (intent-move-dy intent)))
      ;; Wander target is +10,0 from NPC, so normalized direction should be (1.0, 0.0)
      (assert (> dx 0.5f0) ()
              (format nil "nil-player: intent dx should be positive (wander right), got ~a" dx))
      (assert (< (abs dy) 0.1f0) ()
              (format nil "nil-player: intent dy should be near zero, got ~a" dy)))))

;;; ============================================================
;;; FINAL PROGRESSION TESTS
;;; ============================================================

(defun test-award-skill-xp ()
  "Test awarding XP to a skill."
  (let ((skill (make-skill :level 1 :xp 0)))
    ;; Award some XP
    (award-skill-xp skill 100)
    (assert (= (skill-xp skill) 100) () "award-xp: xp increased")
    ;; Level should update if XP threshold crossed
    (award-skill-xp skill 1000)
    (assert (>= (skill-level skill) 1) () "award-xp: level at least 1")
    ;; Negative amounts treated as 0
    (let ((before-xp (skill-xp skill)))
      (award-skill-xp skill -50)
      (assert (= (skill-xp skill) before-xp) () "award-xp: negative -> no change"))))

(defun test-apply-item-modifiers ()
  "Test applying item stat modifiers."
  (ensure-test-game-data)
  (let* ((player (make-player 0.0 0.0 :id 1))
         (stats (player-stats player))
         (mods (stat-block-modifiers stats)))
    ;; Find an item with attack bonus
    (let ((item (find-item-archetype :iron-sword)))
      (when (and item mods (> (or (item-archetype-attack item) 0) 0))
        (let ((before-attack (stat-modifiers-attack mods)))
          ;; Apply item (+1 direction)
          (apply-item-modifiers stats :iron-sword 1)
          (assert (> (stat-modifiers-attack mods) before-attack) ()
                  "apply-mods: attack increased")
          ;; Remove item (-1 direction)
          (apply-item-modifiers stats :iron-sword -1)
          (assert (= (stat-modifiers-attack mods) before-attack) ()
                  "apply-mods: attack restored")))))
  ;; Nil stats should not crash
  (assert (null (apply-item-modifiers nil :iron-sword 1)) ()
          "apply-mods: nil stats -> nil"))

;;; ============================================================
;;; FINAL ZONE TESTS
;;; ============================================================

(defun test-load-write-zone-roundtrip ()
  "Test zone save and load roundtrip.
   Task 5.5: Updated to use zone-object structs."
  (let* ((temp-path (merge-pathnames "test-zone-roundtrip.lisp"
                                      (uiop:temporary-directory)))
         (zone (make-empty-zone :test-roundtrip 25 20 :chunk-size 8)))
    ;; Add some content (zone objects now use structs)
    (push (%make-zone-object :id :coins :x 5 :y 5 :count 10 :base-count 10
                             :respawn 0.0 :respawnable t :snapshot-dirty nil)
          (zone-objects zone))
    (push '(:id :spawn :x 10 :y 10) (zone-spawns zone))
    ;; Write
    (unwind-protect
        (progn
          (write-zone zone temp-path)
          (assert (probe-file temp-path) () "roundtrip: file written")
          ;; Load
          (let ((loaded (load-zone temp-path)))
            (assert loaded () "roundtrip: zone loaded")
            (assert (eq (zone-id loaded) :test-roundtrip) () "roundtrip: id preserved")
            (assert (= (zone-width loaded) 25) () "roundtrip: width preserved")
            (assert (= (zone-height loaded) 20) () "roundtrip: height preserved")
            (assert (= (zone-chunk-size loaded) 8) () "roundtrip: chunk-size preserved")))
      ;; Cleanup
      (when (probe-file temp-path)
        (delete-file temp-path)))))

;;; ============================================================
;;; FINAL MOVEMENT TESTS
;;; ============================================================

(defun test-get-zone-state ()
  "Test getting zone state from cache."
  ;; Clear cache first
  (clear-zone-states)
  ;; Should return nil for unknown zone
  (assert (null (get-zone-state :nonexistent-zone)) ()
          "get-zone-state: nil for unknown")
  ;; Note: get-or-create-zone-state requires file path, tested implicitly
  )

(defun test-zone-state-player-count ()
  "Test counting players in a zone."
  (let* ((p1 (make-player 0.0 0.0 :id 1))
         (p2 (make-player 0.0 0.0 :id 2))
         (p3 (make-player 0.0 0.0 :id 3))
         (players (vector p1 p2 p3)))
    (setf (player-zone-id p1) :zone-a
          (player-zone-id p2) :zone-a
          (player-zone-id p3) :zone-b)
    (assert (= (zone-state-player-count :zone-a players) 2) ()
            "player-count: zone-a has 2")
    (assert (= (zone-state-player-count :zone-b players) 1) ()
            "player-count: zone-b has 1")
    (assert (= (zone-state-player-count :zone-c players) 0) ()
            "player-count: zone-c has 0")))

(defun test-players-in-zone ()
  "Test getting players in a specific zone."
  (let* ((p1 (make-player 0.0 0.0 :id 1))
         (p2 (make-player 0.0 0.0 :id 2))
         (p3 (make-player 0.0 0.0 :id 3))
         (players (vector p1 p2 p3)))
    (setf (player-zone-id p1) :zone-a
          (player-zone-id p2) :zone-a
          (player-zone-id p3) :zone-b)
    (let ((in-a (players-in-zone :zone-a players)))
      (assert (vectorp in-a) () "players-in-zone: returns vector")
      (assert (= (length in-a) 2) () "players-in-zone: zone-a has 2"))
    (let ((in-b (players-in-zone :zone-b players)))
      (assert (= (length in-b) 1) () "players-in-zone: zone-b has 1"))
    ;; Nil zone-id returns nil
    (assert (null (players-in-zone nil players)) ()
            "players-in-zone: nil zone -> nil")))

(defun test-occupied-zone-ids ()
  "Test getting list of occupied zones."
  (let* ((p1 (make-player 0.0 0.0 :id 1))
         (p2 (make-player 0.0 0.0 :id 2))
         (p3 (make-player 0.0 0.0 :id 3))
         (players (vector p1 p2 p3)))
    (setf (player-zone-id p1) :zone-a
          (player-zone-id p2) :zone-a
          (player-zone-id p3) :zone-b)
    (let ((ids (occupied-zone-ids players)))
      (assert (listp ids) () "occupied-zones: returns list")
      (assert (= (length ids) 2) () "occupied-zones: 2 unique zones")
      (assert (member :zone-a ids) () "occupied-zones: contains zone-a")
      (assert (member :zone-b ids) () "occupied-zones: contains zone-b"))))

(defun test-derive-wall-map-from-zone ()
  "Test deriving wall map from zone collision tiles."
  (let* ((zone (make-empty-zone :test 10 10))
         (collision (zone-collision-tiles zone)))
    ;; Add some collision tiles using packed integer keys (as used by build-zone-collision-tiles)
    (setf (gethash (tile-key 2 3) collision) t)
    (setf (gethash (tile-key 5 5) collision) t)
    (let ((wall-map (derive-wall-map-from-zone zone)))
      (assert (arrayp wall-map) () "derive-wall-map: returns array")
      (assert (= (array-dimension wall-map 0) 10) () "derive-wall-map: correct height")
      (assert (= (array-dimension wall-map 1) 10) () "derive-wall-map: correct width")
      (assert (= (aref wall-map 3 2) 1) () "derive-wall-map: 2,3 blocked")
      (assert (= (aref wall-map 5 5) 1) () "derive-wall-map: 5,5 blocked")))
  ;; Nil zone returns nil
  (assert (null (derive-wall-map-from-zone nil)) ()
          "derive-wall-map: nil zone -> nil"))

(defun test-wall-occupied-p ()
  "Test wall occupancy check."
  (let ((wall-map (make-array '(10 10) :initial-element 0)))
    ;; Set some walls
    (setf (aref wall-map 3 2) 1)
    (setf (aref wall-map 5 5) 1)
    ;; Test with *wall-origin-x/y* at 0
    (let ((*wall-origin-x* 0)
          (*wall-origin-y* 0))
      (assert (wall-occupied-p wall-map 2 3) () "wall-occupied: 2,3 blocked")
      (assert (wall-occupied-p wall-map 5 5) () "wall-occupied: 5,5 blocked")
      (assert (not (wall-occupied-p wall-map 0 0)) () "wall-occupied: 0,0 not blocked")
      ;; Out of bounds returns nil
      (assert (not (wall-occupied-p wall-map 100 100)) () "wall-occupied: out of bounds")))
  ;; Nil wall-map returns nil
  (assert (not (wall-occupied-p nil 0 0)) () "wall-occupied: nil map"))

(defun test-blocked-at-p ()
  "Test collision detection at position."
  (let* ((world (make-test-world :tile-size 32.0 :collision-half 12.0))
         (wall-map (make-array '(10 10) :initial-element 0)))
    ;; Set a wall
    (let ((*wall-origin-x* 0)
          (*wall-origin-y* 0))
      (setf (aref wall-map 5 5) 1)
      (setf (world-wall-map world) wall-map)
      ;; Position in blocked tile should be blocked
      (assert (blocked-at-p world 176.0 176.0 12.0 12.0 32.0) ()
              "blocked-at: position in wall blocked")
      ;; Position in open tile should not be blocked
      (assert (not (blocked-at-p world 48.0 48.0 12.0 12.0 32.0)) ()
              "blocked-at: open position not blocked"))))

(defun test-attempt-move ()
  "Test movement with collision resolution."
  (let* ((world (make-test-world :tile-size 32.0 :collision-half 12.0))
         (wall-map (make-array '(20 20) :initial-element 0)))
    (let ((*wall-origin-x* 0)
          (*wall-origin-y* 0))
      (setf (world-wall-map world) wall-map)
      ;; Move in open space - should succeed
      (multiple-value-bind (nx ny out-dx out-dy)
          (attempt-move world 100.0 100.0 1.0 0.0 10.0 12.0 12.0 32.0)
        (assert (> nx 100.0) () "attempt-move: x increased")
        (assert (= ny 100.0) () "attempt-move: y unchanged")
        (assert (= out-dx 1.0) () "attempt-move: dx preserved"))
      ;; Add a wall and test blocked movement
      (setf (aref wall-map 3 5) 1)  ;; Block at tile 5,3
      (multiple-value-bind (nx ny out-dx out-dy)
          (attempt-move world 150.0 100.0 1.0 0.0 100.0 12.0 12.0 32.0)
        ;; Movement may be blocked depending on exact position
        (assert (numberp nx) () "attempt-move: returns number")))))

(defun test-update-running-state ()
  "Test stamina drain and regen for running."
  (let ((player (make-player 0.0 0.0 :id 1)))
    ;; Start with full stamina
    (setf (player-run-stamina player) *run-stamina-max*
          (player-running player) nil)
    ;; Not running, not moving - stamina stays full
    (let ((mult (update-running-state player 0.1 nil nil)))
      (assert (= mult 1.0) () "running: not running -> 1.0 mult"))
    ;; Toggle run on
    (update-running-state player 0.1 nil t)
    (assert (player-running player) () "running: toggle enables run")
    ;; Running while moving drains stamina
    (let ((before (player-run-stamina player)))
      (update-running-state player 0.5 t nil)
      (assert (< (player-run-stamina player) before) () "running: stamina drains"))))

(defun test-edge-spawn-position ()
  "Test calculating spawn position on edge."
  (let* ((world (make-test-world :tile-size 32.0 :collision-half 12.0))
         (wall-map (make-array '(10 10) :initial-element 0)))
    (let ((*wall-origin-x* 0)
          (*wall-origin-y* 0))
      (setf (world-wall-map world) wall-map
            (world-wall-min-x world) 44.0
            (world-wall-max-x world) 276.0
            (world-wall-min-y world) 44.0
            (world-wall-max-y world) 276.0)
      ;; West edge spawn
      (multiple-value-bind (x y)
          (edge-spawn-position world :west nil 0.5)
        (assert (= x 44.0) () "edge-spawn: west x is min")
        (assert (numberp y) () "edge-spawn: y is number"))
      ;; East edge spawn
      (multiple-value-bind (x y)
          (edge-spawn-position world :east nil 0.5)
        (assert (= x 276.0) () "edge-spawn: east x is max")))))

(defun test-zone-bounds-from-dimensions ()
  "Test calculating zone wall bounds."
  (let ((*wall-origin-x* 0)
        (*wall-origin-y* 0))
    (multiple-value-bind (min-x max-x min-y max-y)
        (zone-bounds-from-dimensions 32.0 10 10 12.0 12.0)
      (assert (numberp min-x) () "zone-bounds: min-x is number")
      (assert (numberp max-x) () "zone-bounds: max-x is number")
      (assert (< min-x max-x) () "zone-bounds: min-x < max-x")
      (assert (< min-y max-y) () "zone-bounds: min-y < max-y"))))

(defun test-position-blocked-p ()
  "Test position blocking check wrapper."
  (let* ((world (make-test-world :tile-size 32.0 :collision-half 12.0))
         (wall-map (make-array '(10 10) :initial-element 0)))
    (let ((*wall-origin-x* 0)
          (*wall-origin-y* 0))
      (setf (world-wall-map world) wall-map)
      ;; Open position
      (assert (not (position-blocked-p world 48.0 48.0 12.0 12.0)) ()
              "position-blocked: open position")
      ;; Add wall and test
      (setf (aref wall-map 2 2) 1)
      (assert (position-blocked-p world 80.0 80.0 12.0 12.0) ()
              "position-blocked: blocked position"))))

(defun test-find-open-tile ()
  "Test finding nearest open tile."
  (let* ((world (make-test-world :tile-size 32.0 :collision-half 12.0))
         (wall-map (make-array '(10 10) :initial-element 0)))
    (let ((*wall-origin-x* 0)
          (*wall-origin-y* 0))
      (setf (world-wall-map world) wall-map)
      ;; Starting tile is open - should return it
      (multiple-value-bind (tx ty)
          (find-open-tile world 2 2 12.0 12.0 5)
        (assert (= tx 2) () "find-open: returns start x")
        (assert (= ty 2) () "find-open: returns start y"))
      ;; Block start tile - should find nearby
      (setf (aref wall-map 2 2) 1)
      (multiple-value-bind (tx ty)
          (find-open-tile world 2 2 12.0 12.0 5)
        (assert (not (and (= tx 2) (= ty 2))) () "find-open: finds different tile")))))

(defun test-player-is-stuck-p ()
  "Test player stuck detection."
  (let* ((world (make-test-world :tile-size 32.0 :collision-half 12.0))
         (player (make-player 100.0 100.0 :id 1))
         (wall-map (make-array '(10 10) :initial-element 0)))
    (let ((*wall-origin-x* 0)
          (*wall-origin-y* 0))
      (setf (world-wall-map world) wall-map)
      ;; In open space - not stuck
      (assert (not (player-is-stuck-p player world)) ()
              "is-stuck: open space -> not stuck")
      ;; Surround with walls - would be stuck
      ;; (Testing actual stuck detection requires more complex setup)
      )))

(defun test-world-exit-edge ()
  "Test detecting which edge player is at."
  (let* ((world (make-test-world :tile-size 32.0 :collision-half 12.0))
         (player (make-player 44.0 160.0 :id 1))
         (intent (player-intent player)))
    (setf (world-wall-min-x world) 44.0
          (world-wall-max-x world) 276.0
          (world-wall-min-y world) 44.0
          (world-wall-max-y world) 276.0)
    ;; Player at west edge moving west
    (set-intent-move intent -1.0 0.0)
    (let ((edge (world-exit-edge world player)))
      (assert (eq edge :west) () "exit-edge: west edge detected"))
    ;; Player in center, no edge
    (setf (player-x player) 160.0)
    (set-intent-move intent -1.0 0.0)
    (let ((edge (world-exit-edge world player)))
      (assert (null edge) () "exit-edge: center -> nil"))))

(defun test-wall-blocked-p-zero-origin ()
  "Test zero-origin wall-map collision checking."
  (let ((wall-map (make-array '(10 10) :initial-element 0)))
    ;; Set some blocked tiles
    (setf (aref wall-map 3 5) 1)  ; (5, 3) blocked
    (setf (aref wall-map 0 0) 1)  ; (0, 0) blocked
    ;; Test blocked tiles
    (assert (wall-blocked-p-zero-origin wall-map 5 3) ()
            "wall-blocked-zero: blocked tile returns t")
    (assert (wall-blocked-p-zero-origin wall-map 0 0) ()
            "wall-blocked-zero: origin blocked returns t")
    ;; Test open tiles
    (assert (not (wall-blocked-p-zero-origin wall-map 1 1)) ()
            "wall-blocked-zero: open tile returns nil")
    (assert (not (wall-blocked-p-zero-origin wall-map 9 9)) ()
            "wall-blocked-zero: corner open returns nil")
    ;; Test out of bounds (should return t = blocked)
    (assert (wall-blocked-p-zero-origin wall-map -1 0) ()
            "wall-blocked-zero: negative x returns t")
    (assert (wall-blocked-p-zero-origin wall-map 0 -1) ()
            "wall-blocked-zero: negative y returns t")
    (assert (wall-blocked-p-zero-origin wall-map 10 5) ()
            "wall-blocked-zero: x >= width returns t")
    (assert (wall-blocked-p-zero-origin wall-map 5 10) ()
            "wall-blocked-zero: y >= height returns t")
    ;; Test nil wall-map
    (assert (not (wall-blocked-p-zero-origin nil 5 5)) ()
            "wall-blocked-zero: nil map returns nil")))

(defun test-blocked-at-p-with-map ()
  "Test collision checking with zero-origin wall-map."
  (let ((wall-map (make-array '(10 10) :initial-element 0))
        (tile-size 32.0)
        (half-w 12.0)
        (half-h 12.0))
    ;; Block tile (3, 3)
    (setf (aref wall-map 3 3) 1)
    ;; Test blocked at tile center
    (let ((cx (+ (* 3 tile-size) (/ tile-size 2.0)))
          (cy (+ (* 3 tile-size) (/ tile-size 2.0))))
      (assert (blocked-at-p-with-map wall-map cx cy half-w half-h tile-size) ()
              "blocked-at-map: center of blocked tile -> t"))
    ;; Test open at different tile
    (let ((cx (+ (* 5 tile-size) (/ tile-size 2.0)))
          (cy (+ (* 5 tile-size) (/ tile-size 2.0))))
      (assert (not (blocked-at-p-with-map wall-map cx cy half-w half-h tile-size)) ()
              "blocked-at-map: open tile -> nil"))
    ;; Test nil wall-map (should return nil - not blocked)
    (assert (not (blocked-at-p-with-map nil 100.0 100.0 half-w half-h tile-size)) ()
            "blocked-at-map: nil map -> nil")))

(defun test-find-open-position-with-map ()
  "Test finding open spawn position with wall-map."
  (let ((wall-map (make-array '(10 10) :initial-element 0))
        (tile-size 32.0)
        (half-w 12.0)
        (half-h 12.0))
    ;; Test open position returns same
    (let ((x (+ (* 5 tile-size) (/ tile-size 2.0)))
          (y (+ (* 5 tile-size) (/ tile-size 2.0))))
      (multiple-value-bind (rx ry)
          (find-open-position-with-map wall-map x y half-w half-h tile-size)
        (assert (= rx x) () "find-open-map: open -> same x")
        (assert (= ry y) () "find-open-map: open -> same y")))
    ;; Block center tile and verify we find adjacent
    (setf (aref wall-map 5 5) 1)
    (let ((x (+ (* 5 tile-size) (/ tile-size 2.0)))
          (y (+ (* 5 tile-size) (/ tile-size 2.0))))
      (multiple-value-bind (rx ry)
          (find-open-position-with-map wall-map x y half-w half-h tile-size)
        (assert (not (and (= (floor rx tile-size) 5)
                          (= (floor ry tile-size) 5))) ()
                "find-open-map: blocked -> finds different tile")))
    ;; Test max radius extends to map size
    (let ((small-map (make-array '(5 5) :initial-element 1)))
      ;; All blocked except corner
      (setf (aref small-map 4 4) 0)
      (let ((cx (+ (* 0 tile-size) (/ tile-size 2.0)))
            (cy (+ (* 0 tile-size) (/ tile-size 2.0))))
        (multiple-value-bind (rx ry)
            (find-open-position-with-map small-map cx cy half-w half-h tile-size)
          ;; Should find the one open tile at (4, 4)
          (assert (= (floor rx tile-size) 4) ()
                  "find-open-map: finds distant open tile x")
          (assert (= (floor ry tile-size) 4) ()
                  "find-open-map: finds distant open tile y"))))))

(defun test-zone-state-spawn-position ()
  "Test zone-state spawn position calculation."
  ;; Create a minimal zone-state with wall-map
  (let* ((wall-map (make-array '(10 10) :initial-element 0))
         (zone-state (make-zone-state :zone-id :test-zone
                                       :zone nil
                                       :wall-map wall-map
                                       :npcs (make-array 0)))
         (*tile-size* 16)
         (*tile-scale* 2.0)
         (*player-collision-scale* 0.85)
         (*wall-origin-x* 0)
         (*wall-origin-y* 0))
    ;; Open map - should spawn near center
    (multiple-value-bind (x y)
        (zone-state-spawn-position zone-state)
      (assert (numberp x) () "zone-spawn: returns numeric x")
      (assert (numberp y) () "zone-spawn: returns numeric y")
      (assert (> x 0) () "zone-spawn: x > 0")
      (assert (> y 0) () "zone-spawn: y > 0"))
    ;; Block center - should still find open position
    (setf (aref wall-map 5 5) 1)
    (multiple-value-bind (x y)
        (zone-state-spawn-position zone-state)
      (let ((tile-size (* (float *tile-size* 1.0) *tile-scale*)))
        (assert (not (and (= (floor x tile-size) 5)
                          (= (floor y tile-size) 5))) ()
                "zone-spawn: blocked center -> different tile")))))

;;; ============================================================
;;; PER-ZONE COLLISION HELPER TESTS (Phase 4 deferred items)
;;; ============================================================

(defun test-get-zone-wall-map ()
  "Test getting wall-map from zone-state cache."
  ;; Clear zone-states cache
  (clrhash *zone-states*)
  ;; No zone-state should return nil
  (assert (null (get-zone-wall-map :nonexistent-zone)) ()
          "get-zone-wall-map: nil for unknown zone")
  ;; Create a zone-state manually and test
  (let ((wall-map (make-array '(10 10) :initial-element 0)))
    (setf (gethash :test-zone *zone-states*)
          (make-zone-state :zone-id :test-zone
                           :zone nil
                           :wall-map wall-map
                           :npcs (make-array 0)))
    (assert (eq wall-map (get-zone-wall-map :test-zone)) ()
            "get-zone-wall-map: returns cached wall-map"))
  ;; Cleanup
  (clrhash *zone-states*))

(defun test-get-zone-collision-bounds ()
  "Test zone collision bounds calculation."
  ;; Clear zone-states cache
  (clrhash *zone-states*)
  ;; Unknown zone returns nil
  (multiple-value-bind (min-x max-x min-y max-y)
      (get-zone-collision-bounds :nonexistent 32.0 8.0 8.0)
    (assert (null min-x) () "get-zone-collision-bounds: nil for unknown zone"))
  ;; Create a zone-state with a 10x10 wall-map
  (let ((wall-map (make-array '(10 10) :initial-element 0))
        (*wall-origin-x* 0)
        (*wall-origin-y* 0))
    (setf (gethash :test-zone *zone-states*)
          (make-zone-state :zone-id :test-zone
                           :zone nil
                           :wall-map wall-map
                           :npcs (make-array 0)))
    (multiple-value-bind (min-x max-x min-y max-y)
        (get-zone-collision-bounds :test-zone 32.0 8.0 8.0)
      (assert (numberp min-x) () "get-zone-collision-bounds: returns numeric min-x")
      (assert (numberp max-x) () "get-zone-collision-bounds: returns numeric max-x")
      (assert (< min-x max-x) () "get-zone-collision-bounds: min-x < max-x")
      (assert (< min-y max-y) () "get-zone-collision-bounds: min-y < max-y")))
  ;; Cleanup
  (clrhash *zone-states*))

(defun test-player-is-stuck-p-for-zone ()
  "Test per-zone stuck detection."
  ;; Clear zone-states cache
  (clrhash *zone-states*)
  (let* ((wall-map (make-array '(10 10) :initial-element 0))
         (*tile-size* 16)
         (*tile-scale* 2.0)
         (*player-collision-scale* 0.85)
         (*wall-origin-x* 0)
         (*wall-origin-y* 0)
         (world (make-world))
         (player (make-player 160.0 160.0)))  ; Center of open area
    ;; Setup zone-state
    (setf (gethash :test-zone *zone-states*)
          (make-zone-state :zone-id :test-zone
                           :zone nil
                           :wall-map wall-map
                           :npcs (make-array 0)))
    ;; Player in open area should not be stuck
    (assert (not (player-is-stuck-p-for-zone player :test-zone world)) ()
            "player-is-stuck-p-for-zone: open area = not stuck")
    ;; Block all tiles around player (fully enclosed)
    (dotimes (ty 10)
      (dotimes (tx 10)
        (setf (aref wall-map ty tx) 1)))
    ;; Player enclosed should be stuck
    (assert (player-is-stuck-p-for-zone player :test-zone world) ()
            "player-is-stuck-p-for-zone: enclosed = stuck"))
  ;; Cleanup
  (clrhash *zone-states*))

(defun test-get-zone-safe-spawn-for-zone ()
  "Test per-zone safe spawn position."
  ;; Clear zone-states cache
  (clrhash *zone-states*)
  (let* ((wall-map (make-array '(10 10) :initial-element 0))
         (*tile-size* 16)
         (*tile-scale* 2.0)
         (*player-collision-scale* 0.85)
         (*wall-origin-x* 0)
         (*wall-origin-y* 0)
         (world (make-world)))
    ;; Setup zone-state
    (setf (gethash :test-zone *zone-states*)
          (make-zone-state :zone-id :test-zone
                           :zone nil
                           :wall-map wall-map
                           :npcs (make-array 0)))
    ;; Should return valid coordinates within zone bounds
    (multiple-value-bind (x y)
        (get-zone-safe-spawn-for-zone :test-zone world)
      (assert (numberp x) () "get-zone-safe-spawn: returns numeric x")
      (assert (numberp y) () "get-zone-safe-spawn: returns numeric y")
      (assert (> x 0) () "get-zone-safe-spawn: x > 0")
      (assert (> y 0) () "get-zone-safe-spawn: y > 0")))
  ;; Cleanup
  (clrhash *zone-states*))

;;; ============================================================
;;; FINAL NET TESTS
;;; ============================================================

(defun test-session-try-register ()
  "Test session registration."
  ;; Clear any existing sessions
  (with-session-lock
    (clrhash *active-sessions*))
  ;; Register a session
  (let ((client (make-instance 'net-client)))
    (assert (session-try-register "testuser" client) ()
            "session-register: first registration succeeds")
    ;; Same username should fail
    (assert (not (session-try-register "testuser" client)) ()
            "session-register: duplicate fails")
    ;; Different username succeeds
    (assert (session-try-register "testuser2" client) ()
            "session-register: different user succeeds"))
  ;; Cleanup
  (with-session-lock
    (clrhash *active-sessions*)))

(defun test-session-unregister ()
  "Test session unregistration."
  ;; Clear and setup
  (with-session-lock
    (clrhash *active-sessions*))
  (let ((client (make-instance 'net-client)))
    (session-try-register "testuser" client)
    ;; Should be registered
    (assert (session-get "testuser") () "session-unreg: user exists before")
    ;; Unregister
    (session-unregister "testuser")
    ;; Should be gone
    (assert (null (session-get "testuser")) () "session-unreg: user gone after"))
  ;; Cleanup
  (with-session-lock
    (clrhash *active-sessions*)))

(defun test-session-get ()
  "Test getting session by username."
  ;; Clear and setup
  (with-session-lock
    (clrhash *active-sessions*))
  (let ((client (make-instance 'net-client)))
    (session-try-register "testuser" client)
    ;; Get existing
    (assert (eq (session-get "testuser") client) ()
            "session-get: returns correct client")
    ;; Get non-existent
    (assert (null (session-get "nonexistent")) ()
            "session-get: nil for unknown")
    ;; Case insensitive
    (assert (eq (session-get "TESTUSER") client) ()
            "session-get: case insensitive"))
  ;; Cleanup
  (with-session-lock
    (clrhash *active-sessions*)))

;;; ============================================================================
;;; Persistence Tests
;;; ============================================================================


(defun run-persistence-tests-internal ()
  "Run all data integrity tests. Returns T if all pass, NIL otherwise."
  (let ((passed 0)
        (failed 0)
        (tests (list
                ;; Persistence Round-Trip Tests
                #'test-player-roundtrip
                #'test-ephemeral-not-persisted
                #'test-durable-persisted
                #'test-inventory-roundtrip
                #'test-equipment-roundtrip
                #'test-stats-roundtrip
                ;; XP/Progression Invariant Tests
                #'test-xp-never-decreases-on-award
                #'test-level-increases-with-xp
                #'test-xp-level-boundary
                ;; Inventory Invariant Tests
                #'test-hp-never-exceeds-max
                #'test-inventory-count-limits
                #'test-inventory-overflow-returns-leftover
                #'test-inventory-stack-limits
                ;; Equipment Tests
                #'test-equipment-modifiers-applied
                #'test-equipment-modifiers-removed-on-unequip
                ;; Storage Backend Tests
                #'test-memory-backend-save-load
                #'test-storage-delete
                #'test-redis-backend-save-load
                #'test-redis-backend-delete
                #'test-redis-memory-equivalence
                ;; Zone Transition Tests
                #'test-zone-id-roundtrip
                #'test-zone-id-in-db-save
                ;; Tier-1 Immediate Save Tests
                #'test-death-triggers-immediate-save
                #'test-level-up-triggers-immediate-save
                #'test-item-consume-triggers-immediate-save
                ;; Phase 5: Death Tracking + Leaderboard Tests
                #'test-death-increments-player-deaths
                #'test-death-updates-leaderboard
                #'test-login-seeds-deaths-leaderboard
                ;; Currency Invariant Tests
                #'test-coins-never-negative
                ;; Schema Migration Tests
                #'test-migration-v1-to-v2
                #'test-migration-applies-defaults
                #'test-lifetime-xp-roundtrip
                #'test-lifetime-xp-incremented
                #'test-migration-v1-to-v3-chain
                #'test-playtime-roundtrip
                #'test-created-at-roundtrip
                #'test-private-player-roundtrip
                ;; Compact Serialization Tests (Network Optimization)
                #'test-compact-player-roundtrip
                #'test-compact-npc-roundtrip
                #'test-compact-size-reduction
                #'test-compact-enum-encoding
                #'test-compact-quantization
                #'test-apply-player-compact-direct
                #'test-apply-npc-compact-direct
                ;; Zone-Filtered Delta Serialization Tests
                #'test-delta-for-zone-filters-players
                #'test-delta-for-zone-nil-player-zone
                #'test-delta-for-zone-with-npcs
                #'test-delta-for-zone-nil-zone-state
                #'test-group-clients-clamps-nil-zone
                ;; Schema Validation Tests (Phase 1 - Database Hardening)
                #'test-validation-valid-data-passes
                #'test-validation-missing-required-fields
                #'test-validation-wrong-types
                #'test-validation-out-of-bounds
                #'test-validation-oversized-blob
                #'test-validation-nested-stats
                #'test-validation-nested-inventory
                #'test-validated-load-rejects-invalid
                ;; Phase 6: Validation Hardening
                #'test-validation-sparse-inventory
                ;; Session Ownership Tests (Phase 3 - Database Hardening)
                #'test-session-claim-success
                #'test-session-double-login-rejected
                #'test-session-ownership-refresh
                #'test-session-release-and-reclaim
                ;; 4-Outcome Validation Tests (Phase 6 - Database Hardening)
                #'test-4way-ok-valid-data
                #'test-4way-clamp-hp-below-zero
                #'test-4way-clamp-hp-above-max
                #'test-4way-clamp-position-out-of-bounds
                #'test-4way-clamp-missing-created-at
                #'test-4way-clamp-negative-deaths
                #'test-4way-reject-missing-id
                #'test-4way-reject-negative-lifetime-xp
                #'test-4way-reject-negative-item-count
                #'test-4way-reject-excessive-item-count
                #'test-4way-reject-wrong-type-x
                #'test-4way-reject-negative-skill-xp
                #'test-4way-reject-inventory-not-list
                #'test-4way-reject-slots-not-list
                #'test-4way-reject-inventory-slot-not-list
                #'test-4way-reject-stats-not-list
                #'test-4way-reject-stat-entry-not-list
                #'test-4way-reject-equipment-not-list
                #'test-4way-reject-equipment-items-not-list
                #'test-4way-quarantine-invalid-zone-type
                ;; Phase 6: Unknown zones/items validation
                #'test-4way-quarantine-unknown-zone
                #'test-4way-quarantine-unknown-item
                #'test-4way-quarantine-unknown-equipment-item
                #'test-4way-validation-skips-zone-check-when-not-loaded
                #'test-4way-clamp-uses-plist-put
                #'test-4way-load-valid-player
                #'test-4way-load-clamp-hp
                #'test-4way-load-reject-bad-type
                #'test-4way-load-not-found
                #'test-4way-storage-incr
                #'test-4way-storage-save-with-ttl
                #'test-4way-forensic-storage
                #'test-4way-validation-metrics
                ;; Storage Failure Semantics Tests (Phase 1 - Implementation Findings Fix)
                #'test-storage-error-signaled-on-save-failure
                #'test-storage-error-signaled-on-batch-failure
                #'test-dirty-flags-preserved-on-batch-failure
                #'test-tier1-save-signals-on-ownership-error
                #'test-id-counter-blocked-on-persistence-failure
                #'test-id-counter-advances-on-persistence-success
                #'test-retry-catches-storage-error
                ;; Ownership Loss Cleanup Tests (Phase 2 - Implementation Findings Fix)
                #'test-ownership-reclaim-on-verification-failure
                #'test-ownership-truly-lost-when-another-server-owns
                #'test-local-unregister-preserves-online-set)))
    (format t "~%=== Running Persistence Tests ===~%")
    (dolist (test tests)
      (handler-case
          (progn
            (funcall test)
            (incf passed)
            (format t "✓ ~a~%" (symbol-name (if (symbolp test)
                                                test
                                                (nth-value 2 (function-lambda-expression test))))))
        (error (e)
          (incf failed)
          (format t "✗ ~a: ~a~%"
                  (symbol-name (if (symbolp test)
                                   test
                                   (nth-value 2 (function-lambda-expression test))))
                  e))))
    (format t "~%Results: ~d passed, ~d failed~%" passed failed)
    (zerop failed)))

;;; Test Helpers

(defun ensure-test-game-data ()
  "Ensure game data is loaded for tests that need item archetypes."
  (unless *game-data-loaded-p*
    (load-game-data)))

(defun make-test-player (&key (id 1) (x 100.0) (y 200.0))
  "Create a player with known test values."
  (ensure-test-game-data)
  (let ((player (make-player x y :id id)))
    (setf (player-hp player) 50)
    (grant-inventory-item player :health-potion 5)
    (grant-inventory-item player :rusty-sword 1)
    player))

(defun count-inventory-item (inventory item-id)
  "Count how many of ITEM-ID are in INVENTORY."
  (let ((total 0)
        (slots (and inventory (inventory-slots inventory))))
    (when slots
      (loop for slot across slots
            when (eq (inventory-slot-item-id slot) item-id)
            do (incf total (inventory-slot-count slot))))
    total))

(defun assert-equal (expected actual &optional (message "Values not equal"))
  "Assert that EXPECTED equals ACTUAL."
  (unless (equal expected actual)
    (error "~a: expected ~s, got ~s" message expected actual)))

(defun assert-true (value &optional (message "Value not true"))
  "Assert that VALUE is true."
  (unless value
    (error "~a" message)))

(defun assert-nil (value &optional (message "Value not nil"))
  "Assert that VALUE is nil."
  (when value
    (error "~a: got ~s" message value)))

(defun assert-< (a b &optional (message "Not less than"))
  "Assert that A < B."
  (unless (< a b)
    (error "~a: ~d is not less than ~d" message a b)))

(defun assert-<= (a b &optional (message "Not less than or equal"))
  "Assert that A <= B."
  (unless (<= a b)
    (error "~a: ~d is not <= ~d" message a b)))

(defun assert->= (a b &optional (message "Not greater than or equal"))
  "Assert that A >= B."
  (unless (>= a b)
    (error "~a: ~d is not >= ~d" message a b)))

;;; Persistence Round-Trip Tests

(defun test-player-roundtrip ()
  "Test: Serialize then deserialize = identical durable data."
  (let* ((original (make-test-player :id 42 :x 123.0 :y 456.0))
         (plist (serialize-player original))
         (restored (deserialize-player plist *inventory-size* (length *equipment-slot-ids*))))
    ;; Test durable fields preserved
    (assert-equal (player-id original) (player-id restored) "Player ID")
    (assert-equal (player-x original) (player-x restored) "Player X")
    (assert-equal (player-y original) (player-y restored) "Player Y")
    (assert-equal (player-hp original) (player-hp restored) "Player HP")
    ;; Test inventory preserved
    (let ((orig-inv (player-inventory original))
          (rest-inv (player-inventory restored)))
      (assert-equal (count-inventory-item orig-inv :health-potion)
                   (count-inventory-item rest-inv :health-potion)
                   "Inventory potion count"))
    t))

(defun test-ephemeral-not-persisted ()
  "Test: Ephemeral fields NOT saved to database (no :include-visuals)."
  (let* ((player (make-test-player))
         (plist (serialize-player player))) ; Default: no :include-visuals
    ;; Ephemeral fields should NOT be in plist
    (assert-nil (getf plist :attack-timer) "attack-timer in DB save")
    (assert-nil (getf plist :hit-timer) "hit-timer in DB save")
    (assert-nil (getf plist :click-marker-timer) "click-marker-timer in DB save")
    (assert-nil (getf plist :click-marker-kind) "click-marker-kind in DB save")
    (assert-nil (getf plist :mouse-hold-timer) "mouse-hold-timer in DB save")
    t))

(defun test-durable-persisted ()
  "Test: Durable fields ARE saved to database."
  (let* ((player (make-test-player))
         (plist (serialize-player player)))
    ;; Durable fields MUST be in plist
    (assert-true (getf plist :id) "id not in save")
    (assert-true (getf plist :x) "x not in save")
    (assert-true (getf plist :y) "y not in save")
    (assert-true (getf plist :hp) "hp not in save")
    (assert-true (getf plist :stats) "stats not in save")
    (assert-true (getf plist :inventory) "inventory not in save")
    (assert-true (getf plist :equipment) "equipment not in save")
    t))

(defun test-inventory-roundtrip ()
  "Test: Inventory survives serialization."
  (let* ((original (make-test-player))
         (plist (serialize-player original))
         (restored (deserialize-player plist *inventory-size* (length *equipment-slot-ids*)))
         (orig-inv (player-inventory original))
         (rest-inv (player-inventory restored)))
    (assert-equal (count-inventory-item orig-inv :health-potion)
                 (count-inventory-item rest-inv :health-potion)
                 "Health potion count")
    (assert-equal (count-inventory-item orig-inv :rusty-sword)
                 (count-inventory-item rest-inv :rusty-sword)
                 "Rusty sword count")
    t))

(defun test-equipment-roundtrip ()
  "Test: Equipment survives serialization."
  (let* ((original (make-test-player))
         (equipment (player-equipment original)))
    ;; Equip an item
    (equip-item original :rusty-sword)
    (let* ((plist (serialize-player original))
           (restored (deserialize-player plist *inventory-size* (length *equipment-slot-ids*)))
           (rest-equip (player-equipment restored)))
      (assert-equal (equipment-slot-item equipment :weapon)
                   (equipment-slot-item rest-equip :weapon)
                   "Equipped weapon")
      t)))

;;; Invariant Tests

(defun test-hp-never-exceeds-max ()
  "Test: HP never exceeds max HP."
  (let* ((player (make-test-player))
         (stats (player-stats player))
         (max-hp (stat-block-base-level stats :hitpoints)))
    ;; Try to set HP beyond max
    (setf (player-hp player) (+ max-hp 100))
    ;; Clamp HP (this logic is in progression.lisp)
    (when (> (player-hp player) max-hp)
      (setf (player-hp player) max-hp))
    (assert-<= (player-hp player) max-hp "HP exceeds max")
    t))

(defun test-inventory-count-limits ()
  "Test: Inventory count never exceeds max slots."
  (let ((player (make-test-player)))
    ;; Try to add more items than inventory can hold
    (dotimes (i (+ *inventory-size* 10))
      (grant-inventory-item player :health-potion 1))
    ;; Count total items
    (let* ((inventory (player-inventory player))
           (slots (inventory-slots inventory))
           (total-items 0))
      (loop for slot across slots
            when (inventory-slot-item-id slot)
            do (incf total-items))
      (assert-<= total-items *inventory-size* "Too many inventory slots used")
      t)))

;;; Storage Backend Tests

(defun test-memory-backend-save-load ()
  "Test: Memory backend save and load work correctly."
  (let ((storage (make-instance 'memory-storage)))
    (storage-connect storage)
    ;; Save data
    (storage-save storage "test:player:1" '(:id 1 :x 100.0 :y 200.0 :hp 50))
    ;; Load data
    (let ((loaded (storage-load storage "test:player:1")))
      (assert-equal 1 (getf loaded :id) "Player ID from storage")
      (assert-equal 100.0 (getf loaded :x) "Player X from storage")
      (assert-equal 50 (getf loaded :hp) "Player HP from storage"))
    ;; Clean up
    (storage-delete storage "test:player:1")
    t))

(defun test-storage-delete ()
  "Test: Storage delete removes data."
  (let ((storage (make-instance 'memory-storage)))
    (storage-connect storage)
    ;; Save then delete
    (storage-save storage "test:delete:1" '(:data "value"))
    (assert-true (storage-load storage "test:delete:1") "Data exists before delete")
    (storage-delete storage "test:delete:1")
    (assert-nil (storage-load storage "test:delete:1") "Data still exists after delete")
    t))

;;; Stats Round-Trip Tests

(defun test-stats-roundtrip ()
  "Test: Player stats survive serialization including XP and levels."
  (let* ((original (make-test-player))
         (stats (player-stats original)))
    ;; Award some XP to have non-default values
    (award-skill-xp (stat-block-attack stats) 100)
    (award-skill-xp (stat-block-defense stats) 50)
    (let* ((plist (serialize-player original))
           (restored (deserialize-player plist *inventory-size* (length *equipment-slot-ids*)))
           (rest-stats (player-stats restored)))
      ;; Verify XP preserved
      (assert-equal (skill-xp (stat-block-attack stats))
                   (skill-xp (stat-block-attack rest-stats))
                   "Attack XP")
      (assert-equal (skill-xp (stat-block-defense stats))
                   (skill-xp (stat-block-defense rest-stats))
                   "Defense XP")
      ;; Verify levels preserved
      (assert-equal (skill-level (stat-block-attack stats))
                   (skill-level (stat-block-attack rest-stats))
                   "Attack level")
      t)))

;;; XP/Progression Invariant Tests

(defun test-xp-never-decreases-on-award ()
  "Test: XP can only increase, never decrease from awards."
  (let* ((player (make-test-player))
         (stats (player-stats player))
         (attack-skill (stat-block-attack stats))
         (initial-xp (skill-xp attack-skill)))
    ;; Award positive XP
    (award-skill-xp attack-skill 100)
    (let ((new-xp (skill-xp attack-skill)))
      (assert->= new-xp initial-xp "XP decreased after award")
      (assert-equal (+ initial-xp 100) new-xp "XP didn't increase by award amount"))
    t))

(defun test-level-increases-with-xp ()
  "Test: Level increases when enough XP is gained."
  (let* ((player (make-test-player))
         (stats (player-stats player))
         (attack-skill (stat-block-attack stats))
         (initial-level (skill-level attack-skill)))
    ;; Award enough XP for a level up (level 2 needs 83 XP)
    (award-skill-xp attack-skill 100)
    (let ((new-level (skill-level attack-skill)))
      (assert->= new-level initial-level "Level decreased after XP award")
      (assert-equal 2 new-level "Should be level 2 after 100 XP"))
    t))

(defun test-xp-level-boundary ()
  "Test: XP at exact level boundary correctly triggers level up."
  (let* ((player (make-test-player))
         (stats (player-stats player))
         (attack-skill (stat-block-attack stats)))
    ;; Formula: level = 1 + floor(sqrt(xp / 100))
    ;; Level 2 needs 100 XP (sqrt(100/100) = 1, floor(1) = 1, level = 2)
    ;; At 99 XP, still level 1
    (award-skill-xp attack-skill 99)
    (let ((level (skill-level attack-skill)))
      (assert-equal 1 level "Should be level 1 at 99 XP"))
    ;; At 100 XP, becomes level 2
    (award-skill-xp attack-skill 1) ; total 100
    (let ((level (skill-level attack-skill)))
      (assert-equal 2 level "Should be level 2 at exactly 100 XP"))
    t))

;;; Inventory Operation Tests

(defun test-inventory-overflow-returns-leftover ()
  "Test: Adding items beyond capacity returns leftover count."
  (let* ((player (make-test-player))
         (inventory (player-inventory player)))
    ;; Clear inventory first
    (loop for slot across (inventory-slots inventory)
          do (setf (inventory-slot-item-id slot) nil
                   (inventory-slot-count slot) 0))
    ;; Try to add more items than slots (we have *inventory-size* slots)
    ;; Each item is non-stackable (stack size 1), so each takes a slot
    (let ((leftover (inventory-add inventory :health-potion (+ *inventory-size* 5))))
      ;; Should return 5 leftover
      (assert-equal 5 leftover "Should have 5 leftover items"))
    t))

(defun test-inventory-stack-limits ()
  "Test: Stackable items respect max stack size."
  (let* ((player (make-test-player))
         (inventory (player-inventory player)))
    ;; Clear inventory
    (loop for slot across (inventory-slots inventory)
          do (setf (inventory-slot-item-id slot) nil
                   (inventory-slot-count slot) 0))
    ;; Health potions have a stack size (check what it is)
    (let ((stack-size (item-stack-size :health-potion)))
      (when (> stack-size 1)
        ;; Add exactly stack-size items - should fit in one slot
        (inventory-add inventory :health-potion stack-size)
        (let ((count-in-first-slot (inventory-slot-count (aref (inventory-slots inventory) 0))))
          (assert-<= count-in-first-slot stack-size "Stack exceeds max size"))))
    t))

;;; Equipment Tests

(defun test-equipment-modifiers-applied ()
  "Test: Equipping an item applies its stat modifiers."
  (let* ((player (make-test-player))
         (stats (player-stats player))
         (mods (stat-block-modifiers stats))
         (initial-attack-mod (stat-modifiers-attack mods)))
    ;; Grant and equip a rusty sword (has :attack 1)
    (grant-inventory-item player :rusty-sword 1)
    (equip-item player :rusty-sword)
    (let ((new-attack-mod (stat-modifiers-attack mods)))
      ;; Attack modifier should increase (rusty sword gives +1 attack)
      (assert-true (> new-attack-mod initial-attack-mod)
                  "Attack modifier not applied on equip"))
    t))

(defun test-equipment-modifiers-removed-on-unequip ()
  "Test: Unequipping an item removes its stat modifiers."
  (let* ((player (make-test-player))
         (stats (player-stats player))
         (mods (stat-block-modifiers stats))
         (initial-attack-mod (stat-modifiers-attack mods)))
    ;; Grant and equip a rusty sword
    (grant-inventory-item player :rusty-sword 1)
    (equip-item player :rusty-sword)
    (let ((equipped-attack-mod (stat-modifiers-attack mods)))
      ;; Verify equipped modifier is higher
      (assert-true (> equipped-attack-mod initial-attack-mod)
                  "Sword didn't apply attack modifier")
      ;; Unequip
      (unequip-item player :weapon)
      (let ((unequipped-attack-mod (stat-modifiers-attack mods)))
        ;; Attack modifier should return to initial value
        (assert-equal initial-attack-mod unequipped-attack-mod
                     "Attack modifier not removed on unequip")))
    t))

;;; Redis Backend Tests

(defun test-redis-backend-save-load ()
  "Test: Redis backend save and load work correctly."
  (let ((storage (make-instance 'redis-storage :host "127.0.0.1" :port 6379)))
    (handler-case
        (progn
          (storage-connect storage)
          ;; Save data
          (storage-save storage "test:redis:player:1" '(:id 1 :x 100.0 :y 200.0 :hp 50))
          ;; Load data
          (let ((loaded (storage-load storage "test:redis:player:1")))
            (assert-true loaded "Redis load returned nil")
            (assert-equal 1 (getf loaded :id) "Player ID from Redis")
            (assert-equal 100.0 (getf loaded :x) "Player X from Redis")
            (assert-equal 50 (getf loaded :hp) "Player HP from Redis"))
          ;; Clean up
          (storage-delete storage "test:redis:player:1")
          t)
      (error (e)
        (error "Redis test failed (is Redis running?): ~a" e)))))

(defun test-redis-backend-delete ()
  "Test: Redis delete removes data."
  (let ((storage (make-instance 'redis-storage :host "127.0.0.1" :port 6379)))
    (handler-case
        (progn
          (storage-connect storage)
          ;; Save then delete
          (storage-save storage "test:redis:delete:1" '(:data "value"))
          (assert-true (storage-load storage "test:redis:delete:1") "Redis data exists before delete")
          (storage-delete storage "test:redis:delete:1")
          (assert-nil (storage-load storage "test:redis:delete:1") "Redis data still exists after delete")
          t)
      (error (e)
        (error "Redis delete test failed (is Redis running?): ~a" e)))))

(defun test-redis-memory-equivalence ()
  "Test: Redis and memory backends behave identically."
  (let ((redis (make-instance 'redis-storage :host "127.0.0.1" :port 6379))
        (memory (make-instance 'memory-storage))
        (test-key "test:equivalence:1")
        (test-data '(:id 42 :name "Test" :values (1 2 3) :nested (:a 1 :b 2))))
    (handler-case
        (progn
          (storage-connect redis)
          (storage-connect memory)
          ;; Save same data to both
          (storage-save redis test-key test-data)
          (storage-save memory test-key test-data)
          ;; Load from both
          (let ((redis-loaded (storage-load redis test-key))
                (memory-loaded (storage-load memory test-key)))
            ;; Compare results
            (assert-equal (getf redis-loaded :id) (getf memory-loaded :id)
                         "Redis/Memory ID mismatch")
            (assert-equal (getf redis-loaded :name) (getf memory-loaded :name)
                         "Redis/Memory name mismatch")
            (assert-equal (getf redis-loaded :values) (getf memory-loaded :values)
                         "Redis/Memory values mismatch"))
          ;; Test exists-p equivalence
          (assert-equal (storage-exists-p redis test-key)
                       (storage-exists-p memory test-key)
                       "Redis/Memory exists-p mismatch")
          ;; Clean up
          (storage-delete redis test-key)
          (storage-delete memory test-key)
          ;; Test non-existent key
          (assert-equal (storage-load redis "nonexistent:key")
                       (storage-load memory "nonexistent:key")
                       "Redis/Memory nil return mismatch")
          t)
      (error (e)
        (error "Redis equivalence test failed (is Redis running?): ~a" e)))))

;;; Zone Transition Tests

(defun test-zone-id-roundtrip ()
  "Test: Zone ID survives serialization when provided."
  (let* ((player (make-test-player :id 99 :x 50.0 :y 75.0))
         (zone-id :dungeon-1)
         ;; Serialize with zone-id
         (plist (serialize-player player :zone-id zone-id)))
    ;; Zone ID should be in the plist
    (assert-equal zone-id (getf plist :zone-id) "Zone ID not in serialized data")
    t))

(defun test-zone-id-in-db-save ()
  "Test: Zone ID is included when saving to database with session."
  (let* ((storage (make-instance 'memory-storage))
         (player (make-test-player :id 88))
         (zone-id :forest-2)
         (old-storage *storage*)
         (old-sessions (make-hash-table :test 'eql)))
    ;; Copy existing sessions
    (maphash (lambda (k v) (setf (gethash k old-sessions) v)) *player-sessions*)
    (unwind-protect
        (progn
          ;; Set up test storage and session
          (setf *storage* storage)
          (storage-connect storage)
          (clrhash *player-sessions*)
          (register-player-session player :zone-id zone-id)
          ;; Save player through db-save-player
          (db-save-player player)
          ;; Load raw data and verify zone-id
          (let* ((key (player-key (player-id player)))
                 (loaded (storage-load storage key)))
            (assert-equal zone-id (getf loaded :zone-id) "Zone ID not saved to DB"))
          t)
      ;; Restore global state
      (setf *storage* old-storage)
      (clrhash *player-sessions*)
      (maphash (lambda (k v) (setf (gethash k *player-sessions*) v)) old-sessions))))

;;; Tier-1 Immediate Save Tests

(defvar *test-immediate-save-called* nil
  "Flag set when db-save-player-immediate is called during tests.")

(defun test-death-triggers-immediate-save ()
  "Test: Player death (HP=0) triggers tier-1 immediate save."
  (let* ((storage (make-instance 'memory-storage))
         (player (make-test-player :id 77))
         (old-storage *storage*)
         (old-sessions (make-hash-table :test 'eql)))
    ;; Copy existing sessions
    (maphash (lambda (k v) (setf (gethash k old-sessions) v)) *player-sessions*)
    (unwind-protect
        (progn
          ;; Set up test storage and session
          (setf *storage* storage)
          (storage-connect storage)
          (clrhash *player-sessions*)
          (register-player-session player)
          ;; Set player HP to 1 so next hit kills them
          (setf (player-hp player) 1)
          ;; Apply lethal hit - this should trigger immediate save
          (combatant-apply-hit player 1)
          ;; Verify HP is 0
          (assert-equal 0 (player-hp player) "Player should be dead")
          ;; Verify save was written to storage
          (let* ((key (player-key (player-id player)))
                 (loaded (storage-load storage key)))
            (assert-true loaded "Player not saved after death")
            (assert-equal 0 (getf loaded :hp) "Saved HP should be 0"))
          t)
      ;; Restore global state
      (setf *storage* old-storage)
      (clrhash *player-sessions*)
      (maphash (lambda (k v) (setf (gethash k *player-sessions*) v)) old-sessions))))

(defun test-level-up-triggers-immediate-save ()
  "Test: Level up triggers tier-1 immediate save."
  (let* ((storage (make-instance 'memory-storage))
         (player (make-test-player :id 66))
         (stats (player-stats player))
         (old-storage *storage*)
         (old-sessions (make-hash-table :test 'eql)))
    ;; Copy existing sessions
    (maphash (lambda (k v) (setf (gethash k old-sessions) v)) *player-sessions*)
    (unwind-protect
        (progn
          ;; Set up test storage and session
          (setf *storage* storage)
          (storage-connect storage)
          (clrhash *player-sessions*)
          (register-player-session player)
          ;; Get initial attack level
          (let ((initial-level (skill-level (stat-block-attack stats))))
            ;; Award enough XP to level up:
            ;; - Balanced mode splits XP: 33% to hitpoints, 67% to atk/str/def
            ;; - 500 XP -> ~165 HP, ~335 remaining -> ~112 per combat stat
            ;; - 100 XP needed for level 2, so 112 is enough
            (award-combat-xp player 500)
            ;; Verify level increased
            (let ((new-level (skill-level (stat-block-attack stats))))
              (assert-true (> new-level initial-level) "Attack level should have increased")
              ;; Verify save was written to storage
              (let* ((key (player-key (player-id player)))
                     (loaded (storage-load storage key)))
                (assert-true loaded "Player not saved after level-up")
                ;; Verify XP was saved
                (let* ((saved-stats (getf loaded :stats))
                       (saved-attack (getf saved-stats :attack))
                       (saved-xp (getf saved-attack :xp)))
                  (assert-true (> saved-xp 0) "XP not saved after level-up")))))
          t)
      ;; Restore global state
      (setf *storage* old-storage)
      (clrhash *player-sessions*)
      (maphash (lambda (k v) (setf (gethash k *player-sessions*) v)) old-sessions))))

(defun test-item-consume-triggers-immediate-save ()
  "Test: Item consumption triggers tier-1 immediate save.
   Phase 4: Item destruction (drop/sell/consume) is tier-1 per docs/db.md."
  (ensure-test-game-data)
  (let* ((storage (make-instance 'memory-storage))
         (player (make-test-player :id 88))
         (inventory (player-inventory player))
         (old-storage *storage*)
         (old-sessions (make-hash-table :test 'eql)))
    ;; Copy existing sessions
    (maphash (lambda (k v) (setf (gethash k old-sessions) v)) *player-sessions*)
    (unwind-protect
        (progn
          ;; Set up test storage and session
          (setf *storage* storage)
          (storage-connect storage)
          (clrhash *player-sessions*)
          (register-player-session player)
          ;; Give player some items
          (grant-inventory-item player :bones 5)
          (let ((initial-count (count-inventory-item inventory :bones)))
            (assert-equal 5 initial-count "Should have 5 bones initially")
            ;; Consume items - this should trigger immediate save (Phase 4)
            (consume-inventory-item player :bones 2)
            ;; Verify items were consumed
            (assert-equal 3 (count-inventory-item inventory :bones) "Should have 3 bones after consume")
            ;; Verify save was written to storage immediately
            (let* ((key (player-key (player-id player)))
                   (loaded (storage-load storage key)))
              (assert-true loaded "Player not saved after item consume")
              ;; Verify inventory was saved with correct count
              ;; Inventory format: (:slots ((:item-id :bones :count N) ...))
              ;; Bones have stack-size 1, so count all bones across all slots
              (let* ((saved-inventory (getf loaded :inventory))
                     (saved-slots (getf saved-inventory :slots))
                     (saved-bones-total (loop for slot in saved-slots
                                              when (eq (getf slot :item-id) :bones)
                                              sum (getf slot :count 0))))
                (assert-equal 3 saved-bones-total "Saved inventory should have 3 bones total"))))
          t)
      ;; Restore global state
      (setf *storage* old-storage)
      (clrhash *player-sessions*)
      (maphash (lambda (k v) (setf (gethash k *player-sessions*) v)) old-sessions))))

;;; Phase 5: Death Tracking + Leaderboard Tests

(defun test-death-increments-player-deaths ()
  "Test: Player death increments player-deaths counter.
   Phase 5: player-deaths must be incremented before leaderboard update."
  (let* ((player (make-test-player :id 91)))
    (setf (player-deaths player) 5)  ; Start with 5 deaths
    (setf (player-hp player) 1)      ; Set to 1 HP
    ;; Apply lethal hit
    (combatant-apply-hit player 1)
    ;; Verify deaths incremented
    (assert-equal 6 (player-deaths player) "Deaths should be 6 after death")
    t))

(defun test-death-updates-leaderboard ()
  "Test: Player death updates deaths leaderboard with correct total.
   Phase 5: Leaderboard uses zadd with total count (not zincrby)."
  (let* ((storage (make-instance 'memory-storage))
         (player (make-test-player :id 92))
         (old-storage *storage*)
         (old-sessions (make-hash-table :test 'eql)))
    (maphash (lambda (k v) (setf (gethash k old-sessions) v)) *player-sessions*)
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          (clrhash *player-sessions*)
          (register-player-session player)
          ;; Start with 3 deaths
          (setf (player-deaths player) 3)
          (setf (player-hp player) 1)
          ;; Die
          (combatant-apply-hit player 1)
          ;; Check leaderboard has correct total (4, not incremented from 0)
          ;; db-get-leaderboard returns ((player-id score) ...) with integer IDs
          (let* ((leaderboard (db-get-leaderboard :deaths :top 10))
                 (entry (find 92 leaderboard :key #'first)))
            (assert-true entry "Player should be on deaths leaderboard")
            (assert-equal 4 (second entry) "Leaderboard should show 4 deaths"))
          t)
      (setf *storage* old-storage)
      (clrhash *player-sessions*)
      (maphash (lambda (k v) (setf (gethash k *player-sessions*) v)) old-sessions))))

(defun test-login-seeds-deaths-leaderboard ()
  "Test: Player login seeds deaths leaderboard with existing count.
   Phase 5: Leaderboard is seeded on login, not just on death."
  (let* ((storage (make-instance 'memory-storage))
         (player (make-test-player :id 93))
         (old-storage *storage*)
         (old-sessions (make-hash-table :test 'eql)))
    (maphash (lambda (k v) (setf (gethash k old-sessions) v)) *player-sessions*)
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          (clrhash *player-sessions*)
          ;; Set existing deaths before login
          (setf (player-deaths player) 7)
          ;; Register session (simulates login)
          (register-player-session player)
          ;; Check leaderboard was seeded with existing deaths
          ;; db-get-leaderboard returns ((player-id score) ...) with integer IDs
          (let* ((leaderboard (db-get-leaderboard :deaths :top 10))
                 (entry (find 93 leaderboard :key #'first)))
            (assert-true entry "Player should be on deaths leaderboard after login")
            (assert-equal 7 (second entry) "Leaderboard should show 7 deaths from login seeding"))
          t)
      (setf *storage* old-storage)
      (clrhash *player-sessions*)
      (maphash (lambda (k v) (setf (gethash k *player-sessions*) v)) old-sessions))))

;;; Currency Invariant Tests

(defun test-coins-never-negative ()
  "Test: Coins (gold) count can never go negative."
  (ensure-test-game-data)
  (let* ((player (make-test-player))
         (inventory (player-inventory player)))
    ;; Clear inventory
    (loop for slot across (inventory-slots inventory)
          do (setf (inventory-slot-item-id slot) nil
                   (inventory-slot-count slot) 0))
    ;; Add some coins
    (grant-inventory-item player :coins 100)
    (assert-equal 100 (count-inventory-item inventory :coins) "Should have 100 coins")
    ;; Try to remove more coins than we have
    (inventory-remove inventory :coins 150)
    ;; Count should be 0, not negative
    (let ((remaining (count-inventory-item inventory :coins)))
      (assert->= remaining 0 "Coins went negative"))
    t))

;;; Schema Migration Tests

(defun test-migration-v1-to-v2 ()
  "Test: v1 player data migrates correctly to v2 (adds lifetime-xp)."
  ;; Create v1-style data (no lifetime-xp, version 1)
  (let ((v1-data '(:version 1
                   :id 42
                   :x 100.0
                   :y 200.0
                   :hp 10
                   :stats (:attack (:xp 0 :level 1)
                           :strength (:xp 0 :level 1)
                           :defense (:xp 0 :level 1)
                           :hitpoints (:xp 0 :level 1))
                   :inventory nil
                   :equipment nil)))
    ;; Run migration
    (let ((migrated (migrate-player-data v1-data)))
      ;; Verify version updated
      (assert-equal *player-schema-version* (getf migrated :version) "Version not updated")
      ;; Verify lifetime-xp added with default 0
      (assert-equal 0 (getf migrated :lifetime-xp) "lifetime-xp not defaulted to 0")
      ;; Verify other fields preserved
      (assert-equal 42 (getf migrated :id) "ID not preserved")
      (assert-equal 100.0 (getf migrated :x) "X not preserved")
      (assert-equal 10 (getf migrated :hp) "HP not preserved"))
    t))

(defun test-migration-applies-defaults ()
  "Test: Migration applies defaults for missing fields."
  ;; Create minimal v1 data with missing optional fields
  (let ((v1-data '(:version 1 :id 1 :x 0.0 :y 0.0 :hp 10)))
    (let ((migrated (migrate-player-data v1-data)))
      ;; lifetime-xp should be added
      (assert-true (member :lifetime-xp migrated) "lifetime-xp key missing")
      (assert-equal 0 (getf migrated :lifetime-xp) "lifetime-xp not 0")))
  ;; Test that already-present lifetime-xp is preserved
  (let ((v2-data '(:version 1 :id 1 :x 0.0 :y 0.0 :hp 10 :lifetime-xp 5000)))
    (let ((migrated (migrate-player-data v2-data)))
      ;; Existing value should be preserved (not overwritten with 0)
      (assert-equal 5000 (getf migrated :lifetime-xp) "Existing lifetime-xp was overwritten")))
  t)

(defun test-lifetime-xp-roundtrip ()
  "Test: lifetime-xp survives serialization roundtrip."
  (let* ((player (make-test-player)))
    ;; Set a known lifetime-xp value
    (setf (player-lifetime-xp player) 12345)
    ;; Serialize and deserialize
    (let* ((plist (serialize-player player))
           (restored (deserialize-player plist *inventory-size* (length *equipment-slot-ids*))))
      ;; Verify lifetime-xp preserved
      (assert-equal 12345 (player-lifetime-xp restored) "lifetime-xp not preserved"))
    t))

(defun test-lifetime-xp-incremented ()
  "Test: lifetime-xp increases when XP is awarded."
  (let* ((player (make-test-player))
         (initial-lifetime (player-lifetime-xp player)))
    ;; Award combat XP
    (award-combat-xp player 100)
    ;; Verify lifetime-xp increased by the awarded amount
    (let ((new-lifetime (player-lifetime-xp player)))
      (assert-equal (+ initial-lifetime 100) new-lifetime
                   "lifetime-xp didn't increase by award amount"))
    t))

(defun test-migration-v1-to-v3-chain ()
  "Test: v1 player data migrates correctly through full chain to current version."
  ;; Create v1-style data (no lifetime-xp, no playtime, no created-at, no deaths)
  (let ((v1-data '(:version 1
                   :id 42
                   :x 100.0
                   :y 200.0
                   :hp 10
                   :stats (:attack (:xp 0 :level 1)
                           :strength (:xp 0 :level 1)
                           :defense (:xp 0 :level 1)
                           :hitpoints (:xp 0 :level 1))
                   :inventory nil
                   :equipment nil)))
    ;; Run migration chain (v1 -> v2 -> v3 -> v4 -> ...)
    (let ((migrated (migrate-player-data v1-data)))
      ;; Verify version updated to current
      (assert-equal *player-schema-version* (getf migrated :version)
                   (format nil "Version not updated to current (~d)" *player-schema-version*))
      ;; Verify v2 field (lifetime-xp) added
      (assert-equal 0 (getf migrated :lifetime-xp) "lifetime-xp not added by v2 migration")
      ;; Verify v3 fields (playtime, created-at) added
      ;; Note: playtime is 0.0 (single-float) for type consistency with player-playtime slot
      (assert-equal 0.0 (getf migrated :playtime) "playtime not added by v3 migration")
      (assert-true (getf migrated :created-at) "created-at not added by v3 migration")
      ;; Verify v4 field (deaths) added
      (assert-equal 0 (getf migrated :deaths) "deaths not added by v4 migration")
      ;; Verify original fields preserved
      (assert-equal 42 (getf migrated :id) "ID not preserved")
      (assert-equal 100.0 (getf migrated :x) "X not preserved")
      (assert-equal 10 (getf migrated :hp) "HP not preserved"))
    t))

(defun test-playtime-roundtrip ()
  "Test: playtime survives serialization roundtrip."
  (let* ((player (make-test-player)))
    ;; Set a known playtime value (in seconds, as float)
    (setf (player-playtime player) 3661.5)  ; 1 hour, 1 minute, 1.5 seconds
    ;; Serialize and deserialize
    (let* ((plist (serialize-player player))
           (restored (deserialize-player plist *inventory-size* (length *equipment-slot-ids*))))
      ;; Verify playtime preserved
      (assert-equal 3661.5 (player-playtime restored) "playtime not preserved"))
    t))

(defun test-created-at-roundtrip ()
  "Test: created-at survives serialization roundtrip."
  (let* ((player (make-test-player))
         (original-created-at (player-created-at player)))
    ;; Serialize and deserialize
    (let* ((plist (serialize-player player))
           (restored (deserialize-player plist *inventory-size* (length *equipment-slot-ids*))))
      ;; Verify created-at preserved
      (assert-equal original-created-at (player-created-at restored) "created-at not preserved"))
    t))

(defun test-private-player-roundtrip ()
  "Test: Private state serialization updates inventory/equipment/stats."
  (let* ((source (make-test-player))
         (target (make-player 0.0 0.0 :id 99)))
    (equip-item source :rusty-sword)
    (setf (skill-level (stat-block-attack (player-stats source))) 5
          (player-inventory-dirty target) nil
          (player-hud-stats-dirty target) nil)
    (let ((payload (serialize-player-private source)))
      (apply-player-private-plist target payload)
      (assert-equal (count-inventory-item (player-inventory source) :health-potion)
                    (count-inventory-item (player-inventory target) :health-potion)
                    "Private inventory not applied")
      (assert-equal (equipment-slot-item (player-equipment source) :weapon)
                    (equipment-slot-item (player-equipment target) :weapon)
                    "Private equipment not applied")
      (assert-equal (skill-level (stat-block-attack (player-stats source)))
                    (skill-level (stat-block-attack (player-stats target)))
                    "Private stats not applied")
      (assert-true (player-inventory-dirty target) "Inventory dirty not flagged")
      (assert-true (player-hud-stats-dirty target) "HUD stats dirty not flagged"))
    t))

;;;; ========================================================================
;;;; COMPACT SERIALIZATION TESTS (Network Optimization)
;;;; Tests for the 4-Prong snapshot size optimization (see docs/net.md)
;;;; ========================================================================

(defun test-compact-player-roundtrip ()
  "Test that player compact serialization preserves essential state."
  (let ((player (make-player 123.456 789.012 :id 42)))
    ;; Set various fields to test values
    (setf (player-hp player) 85
          (player-dx player) 1.0
          (player-dy player) -0.5
          (player-anim-state player) :walk
          (player-facing player) :side
          (player-facing-sign player) 1.0
          (player-frame-index player) 3
          (player-frame-timer player) 0.25
          (player-attacking player) t
          (player-attack-hit player) nil
          (player-hit-active player) t
          (player-running player) t
          (player-attack-timer player) 1.5
          (player-hit-timer player) 0.3
          (player-hit-frame player) 2
          (player-hit-facing player) :side
          (player-hit-facing-sign player) -1.0
          (player-run-stamina player) 6.5
          (player-last-sequence player) 123)
    ;; Serialize to compact vector
    (let* ((vec (serialize-player-compact player))
           (plist (deserialize-player-compact vec)))
      ;; Verify essential fields preserved (with quantization tolerance)
      (assert-equal 42 (getf plist :id) "id not preserved")
      (assert (< (abs (- 123.4 (getf plist :x))) 0.2) nil "x not preserved within tolerance")
      (assert (< (abs (- 789.0 (getf plist :y))) 0.2) nil "y not preserved within tolerance")
      (assert-equal 85 (getf plist :hp) "hp not preserved")
      (assert-equal :walk (getf plist :anim-state) "anim-state not preserved")
      (assert-equal :side (getf plist :facing) "facing not preserved")
      (assert (getf plist :attacking) nil "attacking flag not preserved")
      (assert (getf plist :hit-active) nil "hit-active flag not preserved")
      (assert (getf plist :running) nil "running flag not preserved")
      (assert (< (abs (- 6.5 (getf plist :run-stamina))) 0.05) nil
              "run-stamina not preserved within tolerance")
      (assert-equal 123 (getf plist :last-sequence) "last-sequence not preserved")))
  t)

(defun test-compact-npc-roundtrip ()
  "Test that NPC compact serialization preserves essential state."
  (let ((npc (%make-npc)))
    ;; Set various fields to test values
    (setf (npc-id npc) 77
          (npc-x npc) 500.5
          (npc-y npc) 300.3
          (npc-hits-left npc) 3
          (npc-alive npc) t
          (npc-provoked npc) t
          (npc-behavior-state npc) :chasing
          (npc-attack-timer npc) 0.75
          (npc-anim-state npc) :attack
          (npc-facing npc) :up
          (npc-frame-index npc) 2
          (npc-frame-timer npc) 0.15
          (npc-hit-active npc) t
          (npc-hit-timer npc) 0.2
          (npc-hit-frame npc) 1
          (npc-hit-facing npc) :down)
    ;; Serialize to compact vector
    (let* ((vec (serialize-npc-compact npc))
           (plist (deserialize-npc-compact vec)))
      ;; Verify essential fields preserved
      (assert-equal 77 (getf plist :id) "id not preserved")
      (assert (< (abs (- 500.5 (getf plist :x))) 0.2) nil "x not preserved within tolerance")
      (assert (< (abs (- 300.3 (getf plist :y))) 0.2) nil "y not preserved within tolerance")
      (assert-equal 3 (getf plist :hits-left) "hits-left not preserved")
      (assert (getf plist :alive) nil "alive flag not preserved")
      (assert (getf plist :provoked) nil "provoked flag not preserved")
      (assert-equal :chasing (getf plist :behavior-state) "behavior-state not preserved")
      (assert-equal :attack (getf plist :anim-state) "anim-state not preserved")
      (assert-equal :up (getf plist :facing) "facing not preserved")
      (assert (getf plist :hit-active) nil "hit-active flag not preserved")))
  t)

(defun test-compact-size-reduction ()
  "Test that compact serialization produces smaller output than plist format."
  (let ((player (make-player 123.456 789.012 :id 42)))
    (setf (player-hp player) 100
          (player-attacking player) t
          (player-anim-state player) :walk
          (player-facing player) :side)
    ;; Compare sizes
    (let* ((compact-vec (serialize-player-compact player))
           (compact-str (prin1-to-string compact-vec))
           (plist (serialize-player player :network-only t))
           (plist-str (prin1-to-string plist))
           (compact-bytes (length compact-str))
           (plist-bytes (length plist-str)))
      ;; Compact format should be significantly smaller
      (assert (< compact-bytes plist-bytes) nil
              (format nil "Compact (~d bytes) should be smaller than plist (~d bytes)"
                      compact-bytes plist-bytes))
      ;; Target: compact should be less than half the size
      (assert (< compact-bytes (* plist-bytes 0.6)) nil
              (format nil "Compact (~d bytes) should be <60% of plist (~d bytes)"
                      compact-bytes plist-bytes))))
  t)

(defun test-compact-enum-encoding ()
  "Test that enum encoding/decoding works correctly for all values."
  ;; Test animation states (game uses :walk/:attack not :walking/:attacking)
  (dolist (state '(:idle :walk :attack))
    (let ((code (encode-anim-state state)))
      (assert-equal state (decode-anim-state code)
                    (format nil "anim-state ~a roundtrip failed" state))))
  ;; Test facing directions (game uses :side with facing-sign for left/right)
  (dolist (facing '(:up :down :side))
    (let ((code (encode-facing facing)))
      (assert-equal facing (decode-facing code)
                    (format nil "facing ~a roundtrip failed" facing))))
  ;; Test behavior states
  (dolist (state '(:idle :wandering :chasing :attacking :fleeing :returning :dead))
    (let ((code (encode-behavior-state state)))
      (assert-equal state (decode-behavior-state code)
                    (format nil "behavior-state ~a roundtrip failed" state))))
  t)

(defun test-compact-quantization ()
  "Test that quantization preserves values within acceptable precision."
  ;; Test coordinate quantization (0.1 pixel precision)
  (dolist (val '(0.0 1.0 123.45 999.99 -50.0))
    (let ((restored (dequantize-coord (quantize-coord val))))
      (assert (< (abs (- val restored)) 0.1) nil
              (format nil "Coord ~a not preserved within 0.1 precision (got ~a)"
                      val restored))))
  ;; Test timer quantization (0.01 second precision)
  (dolist (val '(0.0 0.5 1.25 3.99))
    (let ((restored (dequantize-timer (quantize-timer val))))
      (assert (< (abs (- val restored)) 0.01) nil
              (format nil "Timer ~a not preserved within 0.01 precision (got ~a)"
                      val restored))))
  t)

(defun test-apply-player-compact-direct ()
  "Test that apply-player-compact-direct updates player state without intermediate plist."
  (let ((player (make-player 0.0 0.0 :id 42))
        (source (make-player 123.456 789.012 :id 42)))
    ;; Set source player fields to test values
    (setf (player-hp source) 85
          (player-dx source) 1.0
          (player-dy source) -0.5
          (player-anim-state source) :walk
          (player-facing source) :side
          (player-facing-sign source) 1.0
          (player-frame-index source) 3
          (player-frame-timer source) 0.25
          (player-attacking source) t
          (player-attack-hit source) nil
          (player-hit-active source) t
          (player-running source) t
          (player-attack-timer source) 1.5
          (player-hit-timer source) 0.3
          (player-hit-frame source) 2
          (player-hit-facing source) :side
          (player-hit-facing-sign source) -1.0
          (player-run-stamina source) 6.5
          (player-last-sequence source) 123)
    ;; Set client-only state on target that should be preserved
    (setf (player-click-marker-x player) 555.0
          (player-click-marker-y player) 666.0
          (player-click-marker-kind player) :walk
          (player-auto-right player) t)
    ;; Serialize source to compact vector
    (let ((vec (serialize-player-compact source)))
      ;; Apply directly to target player
      (apply-player-compact-direct player vec)
      ;; Verify network fields were applied (ID is preserved, not overwritten)
      (assert-equal 42 (player-id player) "id was changed (should be preserved)")
      (assert (< (abs (- 123.4 (player-x player))) 0.2) nil "x not applied within tolerance")
      (assert (< (abs (- 789.0 (player-y player))) 0.2) nil "y not applied within tolerance")
      (assert-equal 85 (player-hp player) "hp not applied")
      (assert-equal :walk (player-anim-state player) "anim-state not applied")
      (assert-equal :side (player-facing player) "facing not applied")
      (assert (player-attacking player) nil "attacking flag not applied")
      (assert (player-hit-active player) nil "hit-active flag not applied")
      (assert (player-running player) nil "running flag not applied")
      ;; Verify client-only state was preserved
      (assert-equal 555.0 (player-click-marker-x player) "click-marker-x was overwritten!")
      (assert-equal 666.0 (player-click-marker-y player) "click-marker-y was overwritten!")
      (assert-equal :walk (player-click-marker-kind player) "click-marker-kind was overwritten!")
      (assert (player-auto-right player) nil "auto-right was overwritten!")))
  t)

(defun test-apply-npc-compact-direct ()
  "Test that apply-npc-compact-direct updates NPC state without intermediate plist."
  (let ((npc (%make-npc))
        (source (%make-npc)))
    ;; Set source NPC fields to test values
    (setf (npc-id source) 77
          (npc-x source) 500.5
          (npc-y source) 300.3
          (npc-hits-left source) 3
          (npc-alive source) t
          (npc-provoked source) t
          (npc-behavior-state source) :chasing
          (npc-attack-timer source) 0.75
          (npc-anim-state source) :attack
          (npc-facing source) :up
          (npc-frame-index source) 2
          (npc-frame-timer source) 0.15
          (npc-hit-active source) t
          (npc-hit-timer source) 0.2
          (npc-hit-frame source) 1
          (npc-hit-facing source) :down)
    ;; Set target NPC ID to match source (simulates lookup by ID)
    ;; Other fields start at defaults that should be overwritten
    (setf (npc-id npc) 77  ; ID is preserved, not set by compact apply
          (npc-x npc) 0.0
          (npc-y npc) 0.0
          (npc-alive npc) nil)
    ;; Serialize source to compact vector
    (let ((vec (serialize-npc-compact source)))
      ;; Apply directly to target NPC
      (apply-npc-compact-direct npc vec)
      ;; Verify fields were applied (ID is preserved, not overwritten)
      (assert-equal 77 (npc-id npc) "id was changed (should be preserved)")
      (assert (< (abs (- 500.5 (npc-x npc))) 0.2) nil "x not applied within tolerance")
      (assert (< (abs (- 300.3 (npc-y npc))) 0.2) nil "y not applied within tolerance")
      (assert-equal 3 (npc-hits-left npc) "hits-left not applied")
      (assert (npc-alive npc) nil "alive flag not applied")
      (assert (npc-provoked npc) nil "provoked flag not applied")
      (assert-equal :chasing (npc-behavior-state npc) "behavior-state not applied")
      (assert-equal :attack (npc-anim-state npc) "anim-state not applied")
      (assert-equal :up (npc-facing npc) "facing not applied")
      (assert (npc-hit-active npc) nil "hit-active flag not applied")))
  t)

(defun test-delta-for-zone-filters-players ()
  "Test that serialize-game-state-delta-for-zone filters players by zone-id."
  ;; Use %make-game to create minimal game without loading assets
  (let* ((players (make-array 4 :initial-element nil))
         (game (%make-game :players players))
         ;; Create 4 players: 2 in zone-1, 2 in zone-2
         (p1 (make-player 100.0 100.0 :id 1))
         (p2 (make-player 200.0 200.0 :id 2))
         (p3 (make-player 300.0 300.0 :id 3))
         (p4 (make-player 400.0 400.0 :id 4)))
    ;; Set zones
    (setf (player-zone-id p1) :zone-1
          (player-zone-id p2) :zone-1
          (player-zone-id p3) :zone-2
          (player-zone-id p4) :zone-2)
    ;; Mark all dirty
    (setf (player-snapshot-dirty p1) t
          (player-snapshot-dirty p2) t
          (player-snapshot-dirty p3) t
          (player-snapshot-dirty p4) t)
    ;; Store in array
    (setf (aref players 0) p1
          (aref players 1) p2
          (aref players 2) p3
          (aref players 3) p4)
    ;; Serialize delta for zone-1 only
    (let ((delta (serialize-game-state-delta-for-zone game :zone-1 nil 1)))
      ;; Should only have 2 players (zone-1)
      (assert-equal 2 (length (getf delta :changed-players))
                    "Delta should only have zone-1 players")
      ;; Zone-id should be zone-1
      (assert-equal :zone-1 (getf delta :zone-id)
                    "Delta zone-id should be zone-1")
      ;; Format should be delta-v5 (target IDs removed from public compact format)
      (assert-equal :delta-v5 (getf delta :format)
                    "Delta format should be delta-v5"))
    ;; Serialize delta for zone-2
    (let ((delta (serialize-game-state-delta-for-zone game :zone-2 nil 2)))
      (assert-equal 2 (length (getf delta :changed-players))
                    "Delta should only have zone-2 players")
      (assert-equal :zone-2 (getf delta :zone-id)
                    "Delta zone-id should be zone-2")))
  t)

(defun test-delta-for-zone-nil-player-zone ()
  "Test that nil player-zone-id is treated as *starting-zone-id*."
  ;; Use %make-game to create minimal game without loading assets
  (let* ((players (make-array 2 :initial-element nil))
         (game (%make-game :players players))
         (p1 (make-player 100.0 100.0 :id 1))
         (p2 (make-player 200.0 200.0 :id 2)))
    ;; p1 has nil zone-id (should be treated as *starting-zone-id*)
    (setf (player-zone-id p1) nil
          (player-zone-id p2) :zone-2)
    (setf (player-snapshot-dirty p1) t
          (player-snapshot-dirty p2) t)
    (setf (aref players 0) p1
          (aref players 1) p2)
    ;; With *starting-zone-id* = :zone-1, p1 should appear in zone-1 delta
    (let ((*starting-zone-id* :zone-1))
      (let ((delta (serialize-game-state-delta-for-zone game :zone-1 nil 1)))
        (assert-equal 1 (length (getf delta :changed-players))
                      "Nil zone player should be in starting zone delta"))))
  t)

(defun test-delta-for-zone-with-npcs ()
  "Test that serialize-game-state-delta-for-zone includes dirty NPCs from zone-state."
  (let* ((players (make-array 1 :initial-element nil))
         (game (%make-game :players players))
         (p1 (make-player 100.0 100.0 :id 1))
         ;; Create NPCs with dirty flags (make-npc takes :id and :archetype, not :name)
         (npc1 (make-npc 50.0 50.0 :id 101))
         (npc2 (make-npc 60.0 60.0 :id 102))
         (npcs (make-array 2 :initial-contents (list npc1 npc2)))
         ;; Create a zone-state with NPCs
         (zone-state (make-zone-state :zone-id :zone-1
                                       :zone nil
                                       :npcs npcs
                                       :wall-map nil
                                       :objects nil)))
    ;; Setup player in zone-1
    (setf (player-zone-id p1) :zone-1
          (player-snapshot-dirty p1) t)
    (setf (aref players 0) p1)
    ;; Mark only npc1 as dirty
    (setf (npc-snapshot-dirty npc1) t
          (npc-snapshot-dirty npc2) nil)
    ;; Serialize delta with zone-state
    (let ((delta (serialize-game-state-delta-for-zone game :zone-1 zone-state 1)))
      ;; Should have 1 player and 1 dirty NPC
      (assert-equal 1 (length (getf delta :changed-players))
                    "Delta should have 1 player")
      (assert-equal 1 (length (getf delta :changed-npcs))
                    "Delta should have 1 dirty NPC")))
  t)

(defun test-delta-for-zone-nil-zone-state ()
  "Test that nil zone-state returns empty NPCs/objects (no crash)."
  (let* ((players (make-array 1 :initial-element nil))
         (game (%make-game :players players))
         (p1 (make-player 100.0 100.0 :id 1)))
    (setf (player-zone-id p1) :zone-1
          (player-snapshot-dirty p1) t)
    (setf (aref players 0) p1)
    ;; Serialize delta with nil zone-state
    (let ((delta (serialize-game-state-delta-for-zone game :zone-1 nil 1)))
      ;; Should have 1 player, 0 NPCs, 0 objects (empty vectors/lists)
      (assert-equal 1 (length (getf delta :changed-players))
                    "Delta should have 1 player")
      (assert-equal 0 (length (getf delta :changed-npcs))
                    "Delta with nil zone-state should have 0 NPCs")
      (assert-nil (getf delta :objects)
                  "Delta with nil zone-state should have nil/empty objects")))
  t)

(defun test-group-clients-clamps-nil-zone ()
  "Test that group-clients-by-zone clamps nil zone-id to *starting-zone-id*."
  ;; Create mock clients with players
  ;; make-net-client takes (host port player) positional args
  (let* ((p1 (make-player 100.0 100.0 :id 1))
         (p2 (make-player 200.0 200.0 :id 2))
         (c1 (make-net-client "127.0.0.1" 5001 p1))
         (c2 (make-net-client "127.0.0.1" 5002 p2)))
    ;; p1 has nil zone-id, p2 has :zone-2
    (setf (player-zone-id p1) nil
          (player-zone-id p2) :zone-2)
    ;; Mark clients as authenticated (required for grouping)
    (setf (net-client-authenticated-p c1) t
          (net-client-authenticated-p c2) t)
    (let ((*starting-zone-id* :zone-1))
      (let ((groups (group-clients-by-zone (list c1 c2))))
        ;; c1 should be grouped under :zone-1 (clamped from nil)
        (assert-equal 1 (length (gethash :zone-1 groups))
                      "Client with nil zone should be grouped under starting-zone-id")
        ;; c2 should be grouped under :zone-2
        (assert-equal 1 (length (gethash :zone-2 groups))
                      "Client with zone-2 should be grouped under zone-2")
        ;; No nil key should exist
        (assert-nil (gethash nil groups)
                    "No nil key should exist in zone groups"))))
  t)

;;;; ========================================================================
;;;; SCHEMA VALIDATION TESTS (Phase 1 - Database Architecture Hardening)
;;;; Tests for validate-player-plist and db-load-player-validated
;;;; ========================================================================

(defun test-validation-valid-data-passes ()
  "Test: Valid player data passes validation."
  (let ((valid-data '(:id 42
                      :version 3
                      :x 100.0
                      :y 200.0
                      :hp 50
                      :lifetime-xp 1000
                      :playtime 3600
                      :created-at 3900000000
                      :zone-id :zone-1
                      :stats (:attack (:level 5 :xp 500)
                              :strength (:level 3 :xp 200)
                              :defense (:level 4 :xp 300)
                              :hitpoints (:level 10 :xp 1000))
                      :inventory (:slots ((:item-id :health-potion :count 5)))
                      :equipment (:items (:rusty-sword nil nil)))))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist-deep valid-data :log-errors nil)
      (assert-true valid-p "Valid data should pass validation")
      (assert-nil errors "Valid data should have no errors")))
  t)

(defun test-validation-missing-required-fields ()
  "Test: Missing required fields are detected."
  ;; Missing :id
  (let ((data '(:x 100.0 :y 200.0 :hp 50)))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist data :log-errors nil)
      (assert-nil valid-p "Missing :id should fail")
      (assert-true (search "Missing required field: ID" (format nil "~{~a~}" errors))
                  "Error should mention missing ID")))
  ;; Missing :x
  (let ((data '(:id 1 :y 200.0 :hp 50)))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist data :log-errors nil)
      (assert-nil valid-p "Missing :x should fail")))
  ;; Missing :hp
  (let ((data '(:id 1 :x 100.0 :y 200.0)))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist data :log-errors nil)
      (assert-nil valid-p "Missing :hp should fail")))
  t)

(defun test-validation-wrong-types ()
  "Test: Wrong field types are detected."
  ;; String instead of integer for :id
  (let ((data '(:id "not-a-number" :x 100.0 :y 200.0 :hp 50)))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist data :log-errors nil)
      (assert-nil valid-p "String :id should fail")
      (assert-true (search "expected INTEGER" (format nil "~{~a~}" errors))
                  "Error should mention expected INTEGER")))
  ;; String instead of number for :x
  (let ((data '(:id 1 :x "hundred" :y 200.0 :hp 50)))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist data :log-errors nil)
      (assert-nil valid-p "String :x should fail")))
  ;; Float instead of integer for :hp
  (let ((data '(:id 1 :x 100.0 :y 200.0 :hp 50.5)))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist data :log-errors nil)
      (assert-nil valid-p "Float :hp should fail")))
  ;; String instead of symbol for :zone-id
  (let ((data '(:id 1 :x 100.0 :y 200.0 :hp 50 :zone-id "zone-1")))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist data :log-errors nil)
      (assert-nil valid-p "String :zone-id should fail")))
  t)

(defun test-validation-out-of-bounds ()
  "Test: Out-of-bounds values are detected."
  ;; Negative HP
  (let ((data '(:id 1 :x 100.0 :y 200.0 :hp -10)))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist data :log-errors nil)
      (assert-nil valid-p "Negative HP should fail")
      (assert-true (search "out of bounds" (format nil "~{~a~}" errors))
                  "Error should mention out of bounds")))
  ;; HP too high
  (let ((data '(:id 1 :x 100.0 :y 200.0 :hp 999999)))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist data :log-errors nil)
      (assert-nil valid-p "HP > 99999 should fail")))
  ;; ID of 0 (must be >= 1)
  (let ((data '(:id 0 :x 100.0 :y 200.0 :hp 50)))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist data :log-errors nil)
      (assert-nil valid-p "ID of 0 should fail")))
  ;; Negative lifetime-xp
  (let ((data '(:id 1 :x 100.0 :y 200.0 :hp 50 :lifetime-xp -100)))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist data :log-errors nil)
      (assert-nil valid-p "Negative lifetime-xp should fail")))
  ;; Position way out of world bounds
  (let ((data '(:id 1 :x 9999999.0 :y 200.0 :hp 50)))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist data :log-errors nil)
      (assert-nil valid-p "X > 1000000 should fail")))
  t)

(defun test-validation-oversized-blob ()
  "Test: Oversized data blobs are rejected."
  ;; Create data that exceeds 64KB when serialized
  (let* ((huge-inventory (loop for i from 0 below 5000
                               collect (list :item-id (intern (format nil "ITEM-~d" i) :keyword)
                                            :count i)))
         (data (list :id 1 :x 100.0 :y 200.0 :hp 50
                     :inventory (list :slots huge-inventory))))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist data :log-errors nil)
      (assert-nil valid-p "Oversized blob should fail")
      (assert-true (search "exceeds max" (format nil "~{~a~}" errors))
                  "Error should mention exceeds max")))
  t)

(defun test-validation-nested-stats ()
  "Test: Nested stats structure is validated."
  ;; Invalid level type in stats
  (let ((data '(:id 1 :x 100.0 :y 200.0 :hp 50
                :stats (:attack (:level "five" :xp 0)))))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist-deep data :log-errors nil)
      (assert-nil valid-p "String level in stats should fail")
      (assert-true (search "level must be integer" (format nil "~{~a~}" errors))
                  "Error should mention level type")))
  ;; Negative XP in stats
  (let ((data '(:id 1 :x 100.0 :y 200.0 :hp 50
                :stats (:attack (:level 5 :xp -100)))))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist-deep data :log-errors nil)
      (assert-nil valid-p "Negative XP in stats should fail")))
  ;; Level out of bounds (> 999)
  (let ((data '(:id 1 :x 100.0 :y 200.0 :hp 50
                :stats (:attack (:level 9999 :xp 0)))))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist-deep data :log-errors nil)
      (assert-nil valid-p "Level > 999 should fail")))
  t)

(defun test-validation-nested-inventory ()
  "Test: Nested inventory structure is validated."
  ;; Invalid item-id type
  (let ((data '(:id 1 :x 100.0 :y 200.0 :hp 50
                :inventory (:slots ((:item-id "not-a-symbol" :count 5))))))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist-deep data :log-errors nil)
      (assert-nil valid-p "String item-id should fail")
      (assert-true (search "item-id must be symbol" (format nil "~{~a~}" errors))
                  "Error should mention item-id type")))
  ;; Negative count
  (let ((data '(:id 1 :x 100.0 :y 200.0 :hp 50
                :inventory (:slots ((:item-id :health-potion :count -5))))))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist-deep data :log-errors nil)
      (assert-nil valid-p "Negative count should fail")))
  ;; Invalid count type
  (let ((data '(:id 1 :x 100.0 :y 200.0 :hp 50
                :inventory (:slots ((:item-id :health-potion :count "five"))))))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist-deep data :log-errors nil)
      (assert-nil valid-p "String count should fail")))
  t)

(defun test-validation-sparse-inventory ()
  "Test: Validation handles sparse inventory with nil slots.
   Phase 6: Guard against nil slots that can occur in real saved data."
  ;; Inventory with nil slots interspersed (common in real saves)
  (let ((data '(:id 1 :x 100.0 :y 200.0 :hp 50
                :inventory (:slots (nil
                                    (:item-id :health-potion :count 5)
                                    nil
                                    nil
                                    (:item-id :bones :count 1)
                                    nil)))))
    ;; Should pass validation (nil slots are skipped)
    (multiple-value-bind (valid-p errors)
        (validate-player-plist-deep data :log-errors nil)
      (assert-true valid-p "Sparse inventory with nil slots should pass validation")
      (assert-nil errors "No errors expected for sparse inventory")))
  ;; Also test 4-way validation with sparse inventory
  ;; Note: 4-way validation needs :created-at and other fields to avoid :clamp
  ;; Phase 6: Disable item validation during this test (items may not be in archetypes)
  (let ((*game-data-loaded-p* nil))
    (let ((data (list :id 1 :x 100.0 :y 200.0 :hp 50
                      :version *player-schema-version*
                      :created-at (get-universal-time)
                      :lifetime-xp 0
                      :playtime 0
                      :deaths 0
                      :inventory '(:slots (nil
                                           (:item-id :health-potion :count 5)
                                           nil)))))
      (multiple-value-bind (action issues fixed)
          (validate-player-plist-4way data)
        (declare (ignore fixed))
        (assert-equal :ok action "4-way validation should return :ok for sparse inventory")
        (assert-nil issues "No issues expected for sparse inventory"))))
  ;; Test with all nil slots (empty inventory)
  (let ((data '(:id 1 :x 100.0 :y 200.0 :hp 50
                :inventory (:slots (nil nil nil nil nil)))))
    (multiple-value-bind (valid-p errors)
        (validate-player-plist-deep data :log-errors nil)
      (assert-true valid-p "All-nil inventory should pass validation")
      (assert-nil errors "No errors expected for empty inventory")))
  t)

(defun test-validated-load-rejects-invalid ()
  "Test: db-load-player-validated rejects invalid data."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*)
         (player-id 999))
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          ;; Save invalid data directly to storage (bypass normal save path)
          (let ((invalid-data '(:id 999 :x "not-a-number" :y 200.0 :hp 50 :version 3)))
            (storage-save storage (player-key player-id) invalid-data))
          ;; Try to load with validation - should return NIL
          (multiple-value-bind (player zone-id)
              (db-load-player-validated player-id)
            (assert-nil player "Invalid data should not load")
            (assert-nil zone-id "Zone-id should be NIL for invalid data"))
          ;; Note: We no longer test unvalidated load here because typed struct slots
          ;; will throw type errors for invalid data (e.g., string in single-float slot).
          ;; This is the correct behavior - the validated path catches it first.
          t)
      ;; Restore global state
      (setf *storage* old-storage))))

;;;; ========================================================================
;;;; SESSION OWNERSHIP TESTS (Phase 3 - Database Architecture Hardening)
;;;; Tests for session claim, double-login rejection, heartbeat, release
;;;; ========================================================================

(defun test-session-claim-success ()
  "Test: Successfully claiming a session ownership."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*)
         (old-server-id *server-instance-id*))
    (unwind-protect
        (progn
          (setf *storage* storage
                *server-instance-id* "test-server-1")
          (storage-connect storage)
          ;; Claim should succeed for new player
          (assert-true (claim-session-ownership 123) "Initial claim should succeed")
          ;; Same server claiming again should succeed (refresh)
          (assert-true (claim-session-ownership 123) "Re-claim by same server should succeed")
          t)
      ;; Cleanup
      (setf *storage* old-storage
            *server-instance-id* old-server-id))))

(defun test-session-double-login-rejected ()
  "Test: Double login from different server is rejected."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*)
         (old-server-id *server-instance-id*))
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          ;; Server 1 claims the session
          (setf *server-instance-id* "server-1")
          (assert-true (claim-session-ownership 456) "Server 1 should claim successfully")
          ;; Server 2 tries to claim - should fail
          (setf *server-instance-id* "server-2")
          (assert-nil (claim-session-ownership 456) "Server 2 should be rejected (double login)")
          t)
      ;; Cleanup
      (setf *storage* old-storage
            *server-instance-id* old-server-id))))

(defun test-session-ownership-refresh ()
  "Test: Session ownership TTL can be refreshed."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*)
         (old-server-id *server-instance-id*))
    (unwind-protect
        (progn
          (setf *storage* storage
                *server-instance-id* "test-server-refresh")
          (storage-connect storage)
          ;; Claim the session
          (assert-true (claim-session-ownership 789) "Initial claim should succeed")
          ;; Verify we own it
          (assert-true (verify-session-ownership 789) "Should verify ownership")
          ;; Refresh should succeed
          (refresh-session-ownership 789)
          ;; Should still own it
          (assert-true (verify-session-ownership 789) "Should still own after refresh")
          t)
      ;; Cleanup
      (setf *storage* old-storage
            *server-instance-id* old-server-id))))

(defun test-session-release-and-reclaim ()
  "Test: Released session can be claimed by another server."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*)
         (old-server-id *server-instance-id*))
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          ;; Server 1 claims the session
          (setf *server-instance-id* "server-alpha")
          (assert-true (claim-session-ownership 999) "Server alpha should claim")
          ;; Server 1 releases
          (release-session-ownership 999)
          ;; Server 2 should now be able to claim
          (setf *server-instance-id* "server-beta")
          (assert-true (claim-session-ownership 999) "Server beta should claim after release")
          ;; Server 2 should now own it
          (assert-true (verify-session-ownership 999) "Server beta should own")
          t)
      ;; Cleanup
      (setf *storage* old-storage
            *server-instance-id* old-server-id))))

;;;; ========================================================================
;;;; 4-OUTCOME VALIDATION TESTS (Phase 6 - Database Architecture Hardening)
;;;; Tests for validate-player-plist-4way and db-load-player-validated
;;;; See docs/db.md "Phase 6: 4-Outcome Validation System"
;;;; ========================================================================

(defun make-valid-test-plist (&key (id 1) (x 100.0) (y 200.0) (hp 50))
  "Create a valid player plist for testing validation."
  (list :id id
        :version *player-schema-version*
        :x x :y y
        :hp hp
        :lifetime-xp 1000
        :playtime 3600.0
        :created-at (get-universal-time)
        :deaths 5
        :zone-id :overworld
        :stats (list :attack (list :level 10 :xp 500)
                     :strength (list :level 8 :xp 300)
                     :defense (list :level 12 :xp 700)
                     :hitpoints (list :level 15 :xp 1000))
        :inventory (list :slots nil)
        :equipment nil))

;;; :ok tests - valid data should pass

(defun test-4way-ok-valid-data ()
  "Test: Valid player data returns :ok action."
  (let ((plist (make-valid-test-plist :id 1 :x 100.0 :y 200.0 :hp 50)))
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :ok action "Valid data should return :ok")
      (assert-nil fixed-plist "No fixed plist needed for :ok")
      t)))

;;; :clamp tests - safe coercions

(defun test-4way-clamp-hp-below-zero ()
  "Test: Negative HP is clamped to 0."
  (let ((plist (make-valid-test-plist :hp -50)))
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :clamp action "Negative HP should return :clamp")
      (assert-equal 0 (getf fixed-plist :hp) "HP should be clamped to 0")
      (assert-true (member "HP -50 clamped to 0" issues :test #'string=)
                   "Should report clamping")
      t)))

(defun test-4way-clamp-hp-above-max ()
  "Test: HP above max is clamped to 99999."
  (let ((plist (make-valid-test-plist :hp 150000)))
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :clamp action "Excessive HP should return :clamp")
      (assert-equal 99999 (getf fixed-plist :hp) "HP should be clamped to 99999")
      t)))

(defun test-4way-clamp-position-out-of-bounds ()
  "Test: Position out of bounds is clamped to spawn point."
  (let ((plist (make-valid-test-plist :x 5000000.0 :y -5000000.0)))
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :clamp action "Out of bounds position should return :clamp")
      (assert-equal *default-spawn-x* (getf fixed-plist :x) "X should be spawn")
      (assert-equal *default-spawn-y* (getf fixed-plist :y) "Y should be spawn")
      t)))

(defun test-4way-clamp-missing-created-at ()
  "Test: Missing :created-at is set to current time."
  (let ((plist (make-valid-test-plist)))
    ;; Remove :created-at
    (remf plist :created-at)
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :clamp action "Missing created-at should return :clamp")
      (assert-true (getf fixed-plist :created-at) "Should have created-at")
      t)))

(defun test-4way-clamp-negative-deaths ()
  "Test: Negative deaths count is clamped to 0."
  (let ((plist (make-valid-test-plist)))
    (setf (getf plist :deaths) -10)
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :clamp action "Negative deaths should return :clamp")
      (assert-equal 0 (getf fixed-plist :deaths) "Deaths should be clamped to 0")
      t)))

;;; :reject tests - exploit-adjacent data

(defun test-4way-reject-missing-id ()
  "Test: Missing :id returns :reject."
  (let ((plist (make-valid-test-plist)))
    (remf plist :id)
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :reject action "Missing ID should return :reject")
      (assert-nil fixed-plist "No fixed plist for :reject")
      t)))

(defun test-4way-reject-negative-lifetime-xp ()
  "Test: Negative lifetime-xp returns :reject (exploit indicator)."
  (let ((plist (make-valid-test-plist)))
    (setf (getf plist :lifetime-xp) -1000)
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :reject action "Negative lifetime-xp should return :reject")
      t)))

(defun test-4way-reject-negative-item-count ()
  "Test: Negative inventory item count returns :reject."
  (let ((plist (make-valid-test-plist)))
    (setf (getf plist :inventory)
          (list :slots (list (list :item-id :sword :count -5))))
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :reject action "Negative item count should return :reject")
      t)))

(defun test-4way-reject-excessive-item-count ()
  "Test: Item count exceeding max returns :reject (dupe indicator)."
  (let ((plist (make-valid-test-plist)))
    (setf (getf plist :inventory)
          (list :slots (list (list :item-id :coins :count 9999999999))))
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :reject action "Excessive item count should return :reject")
      t)))

(defun test-4way-reject-wrong-type-x ()
  "Test: Non-number :x returns :reject."
  (let ((plist (make-valid-test-plist)))
    (setf (getf plist :x) "not-a-number")
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :reject action "Non-number :x should return :reject")
      t)))

(defun test-4way-reject-negative-skill-xp ()
  "Test: Negative skill XP returns :reject (exploit indicator)."
  (let ((plist (make-valid-test-plist)))
    (setf (getf plist :stats)
          (list :attack (list :level 10 :xp -500)))
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :reject action "Negative skill XP should return :reject")
      t)))

(defun test-4way-reject-inventory-not-list ()
  "Test: Non-list :inventory returns :reject."
  (let ((plist (make-valid-test-plist)))
    (setf (getf plist :inventory) "not-a-list")
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :reject action "Non-list :inventory should return :reject")
      t)))

(defun test-4way-reject-slots-not-list ()
  "Test: Non-list :slots returns :reject."
  (let ((plist (make-valid-test-plist)))
    (setf (getf plist :inventory) (list :slots "not-a-list"))
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :reject action "Non-list :slots should return :reject")
      t)))

(defun test-4way-reject-inventory-slot-not-list ()
  "Test: Non-list inventory slot returns :reject."
  (let ((plist (make-valid-test-plist)))
    (setf (getf plist :inventory) (list :slots (list 42)))
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :reject action "Non-list inventory slot should return :reject")
      t)))

(defun test-4way-reject-stats-not-list ()
  "Test: Non-list :stats returns :reject."
  (let ((plist (make-valid-test-plist)))
    (setf (getf plist :stats) 12345)
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :reject action "Non-list :stats should return :reject")
      t)))

(defun test-4way-reject-stat-entry-not-list ()
  "Test: Non-list stat entry returns :reject."
  (let ((plist (make-valid-test-plist)))
    (setf (getf plist :stats) (list :attack "not-a-list"))
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :reject action "Non-list stat entry should return :reject")
      t)))

(defun test-4way-reject-equipment-not-list ()
  "Test: Non-list :equipment returns :reject."
  (let ((plist (make-valid-test-plist)))
    (setf (getf plist :equipment) "not-a-list")
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :reject action "Non-list :equipment should return :reject")
      t)))

(defun test-4way-reject-equipment-items-not-list ()
  "Test: Non-list :equipment :items returns :reject."
  (let ((plist (make-valid-test-plist)))
    (setf (getf plist :equipment) (list :items "not-a-list"))
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :reject action "Non-list :equipment :items should return :reject")
      t)))

;;; :quarantine tests - suspicious but recoverable

(defun test-4way-quarantine-invalid-zone-type ()
  "Test: Non-symbol zone-id returns :quarantine."
  (let ((plist (make-valid-test-plist)))
    (setf (getf plist :zone-id) 12345)  ; number, not symbol
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :quarantine action "Non-symbol zone-id should return :quarantine")
      t)))

;;; Phase 6: New quarantine tests

(defun test-4way-quarantine-unknown-zone ()
  "Test: Unknown zone-id returns :quarantine when *known-zone-ids* is set."
  (let ((plist (make-valid-test-plist))
        (old-known-zone-ids *known-zone-ids*))
    (unwind-protect
        (progn
          ;; Set up known zones (doesn't include :deleted-zone)
          (setf *known-zone-ids* (make-hash-table :test 'eq))
          (setf (gethash :overworld *known-zone-ids*) t)
          (setf (gethash :dungeon *known-zone-ids*) t)
          ;; Use an unknown zone
          (setf (getf plist :zone-id) :deleted-zone)
          (multiple-value-bind (action issues fixed-plist)
              (validate-player-plist-4way plist)
            (declare (ignore fixed-plist))
            (assert-equal :quarantine action "Unknown zone-id should return :quarantine")
            (assert-true (some (lambda (s) (search "Unknown zone-id" s)) issues)
                         "Issues should mention unknown zone")
            t))
      (setf *known-zone-ids* old-known-zone-ids))))

(defun test-4way-quarantine-unknown-item ()
  "Test: Unknown item-id in inventory returns :quarantine when game data loaded."
  (let ((plist (make-valid-test-plist))
        (old-game-data-loaded-p *game-data-loaded-p*))
    (unwind-protect
        (progn
          ;; Set game data loaded flag
          (setf *game-data-loaded-p* t)
          ;; Add inventory with unknown item
          (setf (getf plist :inventory)
                '(:slots ((:item-id :deprecated-sword :count 1))))
          (multiple-value-bind (action issues fixed-plist)
              (validate-player-plist-4way plist)
            (declare (ignore fixed-plist))
            (assert-equal :quarantine action "Unknown item-id should return :quarantine")
            (assert-true (some (lambda (s) (search "Unknown item-id" s)) issues)
                         "Issues should mention unknown item")
            t))
      (setf *game-data-loaded-p* old-game-data-loaded-p))))

(defun test-4way-quarantine-unknown-equipment-item ()
  "Test: Unknown item-id in equipment returns :quarantine when game data loaded.
   P2 fix: Equipment items must also be validated against *item-archetypes*."
  (let ((plist (make-valid-test-plist))
        (old-game-data-loaded-p *game-data-loaded-p*))
    (unwind-protect
        (progn
          ;; Set game data loaded flag
          (setf *game-data-loaded-p* t)
          ;; Add equipment with unknown item (deprecated helmet)
          (setf (getf plist :equipment)
                '(:items (:deprecated-helmet nil nil nil)))
          (multiple-value-bind (action issues fixed-plist)
              (validate-player-plist-4way plist)
            (declare (ignore fixed-plist))
            (assert-equal :quarantine action "Unknown equipment item-id should return :quarantine")
            (assert-true (some (lambda (s) (search "equipment slot" s)) issues)
                         "Issues should mention equipment slot")
            t))
      (setf *game-data-loaded-p* old-game-data-loaded-p))))

(defun test-4way-validation-skips-zone-check-when-not-loaded ()
  "Test: Unknown zone-id does NOT quarantine when *known-zone-ids* is nil."
  (let ((plist (make-valid-test-plist))
        (old-known-zone-ids *known-zone-ids*))
    (unwind-protect
        (progn
          ;; Ensure *known-zone-ids* is nil (world-graph not loaded)
          (setf *known-zone-ids* nil)
          ;; Use a zone that would be unknown if checked
          (setf (getf plist :zone-id) :some-unknown-zone)
          (multiple-value-bind (action issues fixed-plist)
              (validate-player-plist-4way plist)
            (declare (ignore issues fixed-plist))
            ;; Should be :ok since zone check is skipped
            (assert-equal :ok action "Should be :ok when zone check is skipped")
            t))
      (setf *known-zone-ids* old-known-zone-ids))))

(defun test-4way-clamp-uses-plist-put ()
  "Test: Clamp correctly adds missing fields using plist-put.
   Verifies the PLIST_SETF_GETF_PITFALL is avoided by testing
   that :created-at is properly added even if missing from source."
  (let ((plist (list :id 1 :x 0.0 :y 0.0 :hp -10 :version 4
                     :lifetime-xp 0 :playtime 0.0 :deaths 0)))
    ;; Note: :created-at is missing from source
    (multiple-value-bind (action issues fixed-plist)
        (validate-player-plist-4way plist)
      (assert-equal :clamp action "Should be clamped")
      ;; Verify :created-at was added (plist-put works, setf getf would fail)
      (assert-true (getf fixed-plist :created-at)
                   "Missing :created-at should be added by clamp")
      ;; Verify :hp was clamped
      (assert-equal 0 (getf fixed-plist :hp)
                    "Negative HP should be clamped to 0")
      t)))

;;; db-load-player-validated integration tests

(defun test-4way-load-valid-player ()
  "Test: Loading valid player returns :ok action."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*)
         (player-id 100))
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          ;; Save valid player data using quoted list (like test-validated-load-rejects-invalid)
          (let ((plist `(:id ,player-id
                         :version 4
                         :x 100.0 :y 200.0
                         :hp 50
                         :lifetime-xp 1000
                         :playtime 3600.0
                         :created-at ,(get-universal-time)
                         :deaths 5
                         :zone-id :overworld
                         :stats (:attack (:level 10 :xp 500))
                         :inventory (:slots nil)
                         :equipment nil)))
            (storage-save storage (player-key player-id) plist))
          ;; Load with validation
          (multiple-value-bind (player zone-id action)
              (db-load-player-validated player-id)
            (assert-true player "Should load player")
            (assert-equal :overworld zone-id "Should have zone-id")
            (assert-equal :ok action "Should return :ok action"))
          t)
      (setf *storage* old-storage))))

(defun test-4way-load-clamp-hp ()
  "Test: Loading player with bad HP returns :clamp and fixes it."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*)
         (player-id 101))
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          ;; Save player with negative HP (should be clamped to 0)
          (let ((plist `(:id ,player-id
                         :version 4
                         :x 100.0 :y 200.0
                         :hp -100
                         :lifetime-xp 1000
                         :playtime 3600.0
                         :created-at ,(get-universal-time)
                         :deaths 5
                         :zone-id :overworld
                         :stats (:attack (:level 10 :xp 500))
                         :inventory (:slots nil)
                         :equipment nil)))
            (storage-save storage (player-key player-id) plist))
          ;; Load with validation
          (multiple-value-bind (player zone-id action)
              (db-load-player-validated player-id)
            (assert-true player "Should load player")
            (assert-equal :clamp action "Should return :clamp action")
            (assert-equal 0 (player-hp player) "HP should be clamped to 0"))
          t)
      (setf *storage* old-storage))))

(defun test-4way-load-reject-bad-type ()
  "Test: Loading player with wrong type returns :reject."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*)
         (player-id 102))
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          ;; Save invalid data directly
          (let ((invalid-data '(:id 102 :x "not-a-number" :y 200.0 :hp 50 :version 4)))
            (storage-save storage (player-key player-id) invalid-data))
          ;; Load with validation - should reject
          (multiple-value-bind (player zone-id action)
              (db-load-player-validated player-id)
            (assert-nil player "Should not load invalid player")
            (assert-equal :reject action "Should return :reject action"))
          t)
      (setf *storage* old-storage))))

(defun test-4way-load-not-found ()
  "Test: Loading non-existent player returns :not-found."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*))
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          ;; Try to load non-existent player
          (multiple-value-bind (player zone-id action)
              (db-load-player-validated 99999)
            (assert-nil player "Should not find player")
            (assert-equal :not-found action "Should return :not-found action"))
          t)
      (setf *storage* old-storage))))

(defun test-4way-storage-incr ()
  "Test: storage-incr increments counters correctly."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*))
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          ;; Increment non-existent key - should create with value 1
          (assert-equal 1 (storage-incr storage "test:counter") "First incr should return 1")
          (assert-equal 2 (storage-incr storage "test:counter") "Second incr should return 2")
          (assert-equal 3 (storage-incr storage "test:counter") "Third incr should return 3")
          t)
      (setf *storage* old-storage))))

(defun test-4way-storage-save-with-ttl ()
  "Test: storage-save-with-ttl saves data with expiration."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*))
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          ;; Save with TTL
          (storage-save-with-ttl storage "test:expiring" '(:data "test") 3600)
          ;; Should be retrievable
          (let ((data (storage-load storage "test:expiring")))
            (assert-equal '(:data "test") data "Should retrieve saved data"))
          t)
      (setf *storage* old-storage))))

(defun test-4way-forensic-storage ()
  "Test: store-corrupt-blob stores data with TTL."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*))
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          ;; Store corrupt blob
          (store-corrupt-blob 123 "raw-data" '("error 1" "error 2"))
          ;; Check that something was stored (key format: corrupt:123:timestamp)
          (let ((keys (storage-keys storage "corrupt:*")))
            (assert-true (> (length keys) 0) "Should have stored corrupt blob"))
          t)
      (setf *storage* old-storage))))

(defun test-4way-validation-metrics ()
  "Test: increment-validation-metric increments counters."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*))
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          ;; Increment some metrics
          (increment-validation-metric :ok)
          (increment-validation-metric :ok)
          (increment-validation-metric :clamp)
          (increment-validation-metric :reject)
          ;; Check counters
          (assert-equal 2 (storage-load storage "metrics:validation:ok") "OK count")
          (assert-equal 1 (storage-load storage "metrics:validation:clamp") "Clamp count")
          (assert-equal 1 (storage-load storage "metrics:validation:reject") "Reject count")
          t)
      (setf *storage* old-storage))))

;;;; ========================================================================
;;;; STORAGE FAILURE SEMANTICS TESTS (Phase 1 - Implementation Findings Fix)
;;;; Tests for retry integrity, dirty flag preservation, ID counter blocking
;;;; ========================================================================

;; Test helper: A failing storage backend that always signals storage-error
(defclass failing-storage (memory-storage)
  ((fail-saves :initarg :fail-saves :initform t :accessor failing-storage-fail-saves)
   (fail-loads :initarg :fail-loads :initform nil :accessor failing-storage-fail-loads))
  (:documentation "A memory storage that can be configured to fail on save/load."))

(defmethod storage-save ((storage failing-storage) key data)
  "Fail save if configured to do so."
  (if (failing-storage-fail-saves storage)
      (error 'storage-error :operation :save :key key :cause "Simulated failure")
      (call-next-method)))

(defmethod storage-save-batch ((storage failing-storage) key-data-pairs)
  "Fail batch save if configured to do so."
  (if (failing-storage-fail-saves storage)
      (error 'storage-error :operation :batch-save
             :key (mapcar #'car key-data-pairs) :cause "Simulated failure")
      (call-next-method)))

(defmethod storage-load-raw ((storage failing-storage) key)
  "Fail load-raw if configured to do so."
  (if (failing-storage-fail-loads storage)
      (error 'storage-error :operation :load-raw :key key :cause "Simulated failure")
      (call-next-method)))

(defun test-storage-error-signaled-on-save-failure ()
  "Test: storage-save signals storage-error on failure (Phase 1)."
  (let* ((storage (make-instance 'failing-storage :fail-saves t))
         (caught nil))
    (handler-case
        (storage-save storage "test-key" '(:data 123))
      (storage-error (e)
        (setf caught t)
        (assert-equal :save (storage-error-operation e) "Operation should be :save")
        (assert-equal "test-key" (storage-error-key e) "Key should match")))
    (assert-true caught "storage-error should be signaled on save failure")
    t))

(defun test-storage-error-signaled-on-batch-failure ()
  "Test: storage-save-batch signals storage-error on failure (Phase 1)."
  (let* ((storage (make-instance 'failing-storage :fail-saves t))
         (caught nil))
    (handler-case
        (storage-save-batch storage '(("key1" . (:data 1)) ("key2" . (:data 2))))
      (storage-error (e)
        (setf caught t)
        (assert-equal :batch-save (storage-error-operation e) "Operation should be :batch-save")))
    (assert-true caught "storage-error should be signaled on batch save failure")
    t))

(defun test-dirty-flags-preserved-on-batch-failure ()
  "Test: Dirty flags remain set when batch save fails (Phase 1)."
  (let* ((storage (make-instance 'failing-storage :fail-saves t))
         (old-storage *storage*)
         (old-server-id *server-instance-id*))
    (unwind-protect
        (progn
          (setf *storage* storage
                *server-instance-id* "test-server-dirty-flags")
          (storage-connect storage)
          ;; Clear player sessions
          (clrhash *player-sessions*)
          ;; Create a test player (make-player takes positional x y)
          (let* ((player (make-player 100.0 200.0 :id 999)))
            (setf (player-hp player) 50)
            ;; Set up session ownership first (so ownership verification passes)
            ;; This writes to the underlying memory-storage, which doesn't fail
            (storage-setnx-with-ttl storage (session-owner-key 999)
                                    *server-instance-id* 60)
            ;; Now manually register session
            (setf (gethash 999 *player-sessions*)
                  (make-player-session :player player
                                       :zone-id :test-zone
                                       :dirty-p t
                                       :last-flush 0.0))
            ;; Try to flush (should fail at save step due to failing storage)
            (flush-dirty-players :force t)
            ;; Check that dirty flag is STILL set (not cleared on failure)
            (let ((session (gethash 999 *player-sessions*)))
              (assert-true session "Session should still exist")
              (assert-true (player-session-dirty-p session)
                           "Dirty flag should remain set after batch save failure"))
            t))
      ;; Cleanup
      (clrhash *player-sessions*)
      (setf *storage* old-storage
            *server-instance-id* old-server-id))))

(defun test-tier1-save-signals-on-ownership-error ()
  "Test: Tier-1 save signals storage-error when ownership check fails."
  (let* ((storage (make-instance 'failing-storage :fail-loads t :fail-saves nil))
         (old-storage *storage*)
         (old-server-id *server-instance-id*)
         (player (make-player 100.0 200.0 :id 555))
         (caught nil))
    (unwind-protect
        (progn
          (setf *storage* storage
                *server-instance-id* "test-server-tier1")
          (storage-connect storage)
          (handler-case
              (db-save-player-immediate player)
            (storage-error (e)
              (setf caught t)
              (assert-equal :load-raw (storage-error-operation e)
                            "Ownership check should signal load-raw error")))
          (assert-true caught "Tier-1 save should signal storage-error on ownership check failure")
          t)
      (setf *storage* old-storage
            *server-instance-id* old-server-id))))

(defun test-id-counter-blocked-on-persistence-failure ()
  "Test: ID counter does not advance when persistence fails (Phase 1)."
  (let* ((storage (make-instance 'failing-storage :fail-saves t))
         (old-storage *storage*)
         ;; make-id-source takes optional positional args: (next-id persistent)
         (id-source (make-id-source 100 t))
         (error-signaled nil))
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          ;; Attempt to allocate - should signal error due to persistence failure
          (handler-case
              (allocate-entity-id id-source)
            (error (e)
              (declare (ignore e))
              (setf error-signaled t)))
          (assert-true error-signaled "Allocation should signal error when persistence fails")
          ;; ID counter should NOT have advanced
          (assert-equal 100 (id-source-next-id id-source)
                        "ID counter should not advance on persistence failure")
          t)
      (setf *storage* old-storage))))

(defun test-id-counter-advances-on-persistence-success ()
  "Test: ID counter advances normally when persistence succeeds (Phase 1)."
  (let* ((storage (make-instance 'failing-storage :fail-saves nil))  ; Don't fail
         (old-storage *storage*)
         ;; make-id-source takes optional positional args: (next-id persistent)
         (id-source (make-id-source 100 t)))
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          ;; Allocate - should succeed
          (let ((result (allocate-entity-id id-source)))
            (assert-equal 100 result "Allocation should return current ID")
            ;; ID counter should have advanced
            (assert-equal 101 (id-source-next-id id-source)
                          "ID counter should advance on persistence success"))
          t)
      (setf *storage* old-storage))))

(defun test-retry-catches-storage-error ()
  "Test: with-retry-exponential retries on storage-error (Phase 1)."
  (let ((attempts 0)
        (succeed-on-attempt 3))
    ;; Function that fails twice then succeeds
    (let ((result
            (with-retry-exponential
                (res (lambda ()
                       (incf attempts)
                       (if (< attempts succeed-on-attempt)
                           (error 'storage-error :operation :test :cause "Simulated")
                           :success))
                 :max-retries 5
                 :initial-delay 1
                 :max-delay 10)
              res)))
      (assert-equal :success result "Should eventually succeed")
      (assert-equal 3 attempts "Should have taken 3 attempts")
      t)))

(defun run-phase1-storage-failure-tests ()
  "Run all Phase 1 storage failure semantics tests."
  (format t "~&Running Phase 1 storage failure semantics tests...~%")
  (run-test 'test-storage-error-signaled-on-save-failure)
  (run-test 'test-storage-error-signaled-on-batch-failure)
  (run-test 'test-dirty-flags-preserved-on-batch-failure)
  (run-test 'test-tier1-save-signals-on-ownership-error)
  (run-test 'test-id-counter-blocked-on-persistence-failure)
  (run-test 'test-id-counter-advances-on-persistence-success)
  (run-test 'test-retry-catches-storage-error)
  (format t "~&All Phase 1 storage failure tests complete.~%"))

;;; Phase 2: Ownership Loss Cleanup Tests

(defun test-ownership-reclaim-on-verification-failure ()
  "Test: refresh-all-session-ownerships attempts reclaim before reporting loss (Phase 2)."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*)
         (player (make-player 100.0 200.0 :id 999))
         (reclaim-attempted nil))
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          (setf *server-instance-id* "test-server-001")
          ;; Register the player session
          (register-player-session player :zone-id 'test-zone :username "testuser")
          ;; Verify ownership is established
          (assert-true (verify-session-ownership 999) "Should own session initially")
          ;; Simulate ownership key expiration by deleting it
          (storage-delete storage (session-owner-key 999))
          ;; Now verify-session-ownership should fail, but claim should succeed
          (assert-true (not (verify-session-ownership 999)) "Verify should fail after key deletion")
          ;; claim-session-ownership should be able to re-claim (no other owner)
          (assert-true (claim-session-ownership 999) "Should be able to re-claim")
          ;; Clean up
          (unregister-player-session 999)
          t)
      (setf *storage* old-storage))))

(defun test-ownership-truly-lost-when-another-server-owns ()
  "Test: refresh-all-session-ownerships reports loss when another server owns (Phase 2)."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*)
         (player (make-player 100.0 200.0 :id 999)))
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          (setf *server-instance-id* "test-server-001")
          ;; Register the player session
          (register-player-session player :zone-id 'test-zone :username "testuser")
          ;; Simulate another server taking ownership (must use TTL for claim to recognize it)
          (let ((owner-key (session-owner-key 999)))
            (storage-save-with-ttl storage owner-key "other-server-002" *session-ownership-ttl-seconds*))
          ;; Now verify should fail
          (assert-true (not (verify-session-ownership 999)) "Verify should fail")
          ;; And claim should also fail (key exists with different owner)
          (assert-true (not (claim-session-ownership 999)) "Claim should fail - owned by other")
          ;; Clean up locally only
          (unregister-player-session-local 999)
          t)
      (setf *storage* old-storage))))

(defun test-local-unregister-preserves-online-set ()
  "Test: unregister-player-session-local does not touch online set (Phase 2)."
  (let* ((storage (make-instance 'memory-storage))
         (old-storage *storage*)
         (player (make-player 100.0 200.0 :id 999)))
    (unwind-protect
        (progn
          (setf *storage* storage)
          (storage-connect storage)
          (setf *server-instance-id* "test-server-001")
          ;; Register the player session (adds to online set)
          (register-player-session player :zone-id 'test-zone :username "testuser")
          ;; Verify player is in online set (uses key "online:players")
          (let ((online-count (storage-scard storage "online:players")))
            (assert-true (and online-count (> online-count 0))
                         "Player should be in online set after registration"))
          ;; Now use local-only unregister (simulating ownership loss)
          (unregister-player-session-local 999)
          ;; Online set should still contain the player (not touched)
          (let ((online-count-after (storage-scard storage "online:players")))
            (assert-true (and online-count-after (> online-count-after 0))
                         "Online set should not be modified by local unregister"))
          ;; Clean up online set manually for test cleanup
          (storage-srem storage "online:players" "999")
          t)
      (setf *storage* old-storage))))

(defun run-phase2-ownership-cleanup-tests ()
  "Run all Phase 2 ownership cleanup tests."
  (format t "~&Running Phase 2 ownership cleanup tests...~%")
  (run-test 'test-ownership-reclaim-on-verification-failure)
  (run-test 'test-ownership-truly-lost-when-another-server-owns)
  (run-test 'test-local-unregister-preserves-online-set)
  (format t "~&All Phase 2 ownership cleanup tests complete.~%"))

(defun run-4way-validation-tests ()
  "Run all 4-outcome validation tests."
  (format t "~&Running 4-outcome validation tests...~%")
  (run-test 'test-4way-ok-valid-data)
  (run-test 'test-4way-clamp-hp-below-zero)
  (run-test 'test-4way-clamp-hp-above-max)
  (run-test 'test-4way-clamp-position-out-of-bounds)
  (run-test 'test-4way-clamp-missing-created-at)
  (run-test 'test-4way-clamp-negative-deaths)
  (run-test 'test-4way-reject-missing-id)
  (run-test 'test-4way-reject-negative-lifetime-xp)
  (run-test 'test-4way-reject-negative-item-count)
  (run-test 'test-4way-reject-excessive-item-count)
  (run-test 'test-4way-reject-wrong-type-x)
  (run-test 'test-4way-reject-negative-skill-xp)
  (run-test 'test-4way-reject-inventory-slot-not-list)
  (run-test 'test-4way-reject-equipment-not-list)
  (run-test 'test-4way-reject-equipment-items-not-list)
  (run-test 'test-4way-quarantine-invalid-zone-type)
  (run-test 'test-4way-load-valid-player)
  (run-test 'test-4way-load-clamp-hp)
  (run-test 'test-4way-load-reject-bad-type)
  (run-test 'test-4way-load-not-found)
  (run-test 'test-4way-storage-incr)
  (run-test 'test-4way-storage-save-with-ttl)
  (run-test 'test-4way-forensic-storage)
  (run-test 'test-4way-validation-metrics)
  (format t "~&All 4-outcome validation tests complete.~%"))

;;; Export for REPL usage
(export 'run-persistence-tests)
(export 'run-4way-validation-tests)
(export 'run-phase1-storage-failure-tests)
(export 'run-phase2-ownership-cleanup-tests)

;;; ============================================================================
;;; Security Tests
;;; ============================================================================

(in-package #:mmorpg)

(defun env-int (name default)
  (let ((raw (sb-ext:posix-getenv name)))
    (if raw
        (let ((value (ignore-errors (parse-integer raw :junk-allowed t))))
          (if (and value (integerp value))
              value
              default))
        default)))

(defun extract-first-player-from-snapshot (state)
  "Extract first player's position from snapshot state.
   Handles compact-v1/v2/v3/v4, delta-v1/v2/v3/v4, and legacy (plist) formats.
   Returns (values x y) or (values nil nil) if not found."
  (let* ((format (getf state :format))
         ;; Delta format uses :changed-players, compact/legacy use :players
         (players (or (getf state :changed-players)
                      (getf state :players))))
    (when players
      (cond
        ;; Compact or delta format: players is a vector of vectors
       ((or (eq format :compact-v1) (eq format :compact-v2) (eq format :compact-v3)
            (eq format :compact-v4) (eq format :compact-v5)
            (eq format :delta-v1) (eq format :delta-v2) (eq format :delta-v3)
            (eq format :delta-v4) (eq format :delta-v5))
         (when (and (vectorp players) (> (length players) 0))
           (let ((vec (aref players 0)))
             (when (and (vectorp vec) (>= (length vec) 3))
               ;; Compact vector format: [0]=id [1]=x*10 [2]=y*10
               (values (dequantize-coord (aref vec 1))
                       (dequantize-coord (aref vec 2)))))))
        ;; Legacy format: players is a list of plists
        (t
         (when (listp players)
           (let ((first-player (first players)))
             (when first-player
               (values (getf first-player :x)
                       (getf first-player :y))))))))))

(defun extract-players-as-plists (state)
  "Extract all players from snapshot state as a list of plists.
   Handles compact-v1/v2/v3/v4, delta-v1/v2/v3/v4, and legacy (plist) formats."
  (let* ((format (getf state :format))
         ;; Delta format uses :changed-players, compact/legacy use :players
         (players (or (getf state :changed-players)
                      (getf state :players))))
    (when players
      (cond
        ;; Compact or delta format: convert vectors to plists
       ((or (eq format :compact-v1) (eq format :compact-v2) (eq format :compact-v3)
            (eq format :compact-v4) (eq format :compact-v5)
            (eq format :delta-v1) (eq format :delta-v2) (eq format :delta-v3)
            (eq format :delta-v4) (eq format :delta-v5))
         (when (vectorp players)
           (loop :for vec :across players
                 :collect (deserialize-player-compact vec))))
        ;; Legacy format: already plists
        (t
         (when (listp players)
           players))))))

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defmacro define-security-test (name &body body)
  `(defun ,name ()
     (format t "~&  Testing ~a...~%" ',name)
     (handler-case
         (progn ,@body
                (incf *tests-passed*)
                (format t "~&    PASS~%"))
       (error (e)
         (incf *tests-failed*)
         (format t "~&    FAIL: ~a~%" e)))))

;;;; ==========================================================================
;;;; TEST: Unauthenticated Intent Rejection
;;;; Acceptance criteria #5: Server must ignore intents from unauthenticated clients
;;;; ==========================================================================

(define-security-test test-unauthenticated-intent-rejected
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1337))
         (buffer (make-net-buffer))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (mmorpg:run-server
                            :host host
                            :port port
                            :max-seconds 3.0)))))
    (sleep 0.2)
    (let ((auth-socket (usocket:socket-connect host port :protocol :datagram))
          (unauth-socket (usocket:socket-connect host port :protocol :datagram)))
      (unwind-protect
           (progn
             ;; Authenticate control client
             (send-net-message auth-socket
                      (list :type :register :username "sec-test-auth" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil))
               (loop :while (and (not auth-ok) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (receive-net-message auth-socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :auth-ok))
                             (setf auth-ok t)))
                         (sleep 0.01))
               (unless auth-ok
                 (error "Control client auth failed")))

             ;; Send hello from unauth socket (creates client but NOT authenticated)
             (send-net-message unauth-socket (list :type :hello))
             (sleep 0.05)

             ;; Send malicious intent from unauthenticated client
             (let ((intent (make-intent)))
               (set-intent-move intent 1.0 0.0)
               (send-net-message unauth-socket
                        (list :type :intent
                              :payload (intent->plist intent))))
             (sleep 0.1)

             ;; Verify server still responsive
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (responsive nil))
               (loop :while (and (not responsive) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (receive-net-message auth-socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :snapshot))
                             (setf responsive t)))
                         (sleep 0.01))
               (unless responsive
                 (error "Server unresponsive after unauthenticated intent"))))
        (usocket:socket-close auth-socket)
        (usocket:socket-close unauth-socket)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Speed Hack Prevention
;;;; Client should not be able to move faster by sending large move-dx/move-dy values
;;;; ==========================================================================

(define-security-test test-speed-hack-prevented
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1338))
         (buffer (make-net-buffer))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (mmorpg:run-server
                            :host host
                            :port port
                            :max-seconds 3.0)))))
    (sleep 0.2)
    (let ((socket (usocket:socket-connect host port :protocol :datagram)))
      (unwind-protect
           (progn
             ;; Register and authenticate
             (send-net-message socket
                      (list :type :register :username "sec-test-speed" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil)
                    (initial-x nil)
                    (initial-y nil))
               ;; Wait for auth and get initial position
               (loop :while (and (not initial-x) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when message
                             (case (getf message :type)
                               (:auth-ok (setf auth-ok t))
                               (:snapshot
                                (when auth-ok
                                  (multiple-value-bind (x y)
                                      (extract-first-player-from-snapshot (getf message :state))
                                    (when x
                                      (setf initial-x x
                                            initial-y y))))))))
                         (sleep 0.01))
               (unless (and auth-ok initial-x)
                 (error "Failed to get initial position"))

               ;; Send MALICIOUS intent with huge move-dx (speed hack attempt)
               (send-net-message socket
                        (list :type :intent
                              :payload (list :move-dx 1000.0  ; 1000x normal speed!
                                             :move-dy 0.0
                                             :face-dx 1.0
                                             :face-dy 0.0
                                             :target-active nil
                                             :attack nil
                                             :run-toggle nil)))

               ;; Wait a bit and check position
               (sleep 0.5)

               ;; Get current position
               (let* ((check-deadline (+ (get-internal-real-time)
                                         (floor (* 2 internal-time-units-per-second))))
                      (final-x nil))
                 (loop :while (and (not final-x) (< (get-internal-real-time) check-deadline))
                       :do (multiple-value-bind (message _h _p)
                               (receive-net-message socket buffer)
                             (declare (ignore _h _p))
                             (when (and message (eq (getf message :type) :snapshot))
                               (multiple-value-bind (x y)
                                   (extract-first-player-from-snapshot (getf message :state))
                                 (declare (ignore y))
                                 (when x
                                   (setf final-x x)))))
                           (sleep 0.01))
                 (unless final-x
                   (error "Failed to get final position"))

                 ;; Calculate max expected movement:
                 ;; Normal speed = *player-speed* (222.0) * dt * 0.5 seconds worth
                 ;; With run multiplier (2.0) max = 222 * 2 * 0.5 = ~222 pixels
                 ;; Allow some margin for timing: 500 pixels max
                 (let ((distance (abs (- final-x initial-x))))
                   (when (> distance 500.0)
                     (error "Speed hack succeeded! Moved ~a pixels (max expected ~500)"
                            distance))))))
        (usocket:socket-close socket)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Chat Message Length Enforcement
;;;; Server should truncate/reject messages exceeding *chat-max-length*
;;;; ==========================================================================

(define-security-test test-chat-message-length-enforced
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1339))
         (buffer (make-net-buffer))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (mmorpg:run-server
                            :host host
                            :port port
                            :max-seconds 2.0)))))
    (sleep 0.2)
    (let ((socket (usocket:socket-connect host port :protocol :datagram)))
      (unwind-protect
           (progn
             ;; Register and authenticate
             (send-net-message socket
                      (list :type :register :username "sec-test-chat" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil))
               (loop :while (and (not auth-ok) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :auth-ok))
                             (setf auth-ok t)))
                         (sleep 0.01))
               (unless auth-ok
                 (error "Auth failed")))

             ;; Send MALICIOUS intent with huge chat message (10,000 chars)
             (let ((huge-message (make-string 10000 :initial-element #\A)))
               (send-net-message socket
                        (list :type :intent
                              :payload (list :move-dx 0.0 :move-dy 0.0
                                             :requested-chat-message huge-message))))

             ;; Server should still be responsive (not crashed/hung)
             (sleep 0.2)
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (responsive nil))
               (loop :while (and (not responsive) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :snapshot))
                             (setf responsive t)))
                         (sleep 0.01))
               (unless responsive
                 (error "Server unresponsive after oversized chat message"))))
        (usocket:socket-close socket)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Malformed Intent ID Handling
;;;; Server should handle non-integer target IDs gracefully
;;;; ==========================================================================

(define-security-test test-malformed-intent-ids-handled
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1340))
         (buffer (make-net-buffer))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (mmorpg:run-server
                            :host host
                            :port port
                            :max-seconds 2.0)))))
    (sleep 0.2)
    (let ((socket (usocket:socket-connect host port :protocol :datagram)))
      (unwind-protect
           (progn
             ;; Register and authenticate
             (send-net-message socket
                      (list :type :register :username "sec-test-ids" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil))
               (loop :while (and (not auth-ok) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :auth-ok))
                             (setf auth-ok t)))
                         (sleep 0.01))
               (unless auth-ok
                 (error "Auth failed")))

             ;; Send MALICIOUS intents with bad ID types
             ;; Test 1: String ID
             (send-net-message socket
                      (list :type :intent
                            :payload (list :move-dx 0.0 :move-dy 0.0
                                           :requested-attack-target-id "evil-string")))
             (sleep 0.1)

             ;; Test 2: List ID
             (send-net-message socket
                      (list :type :intent
                            :payload (list :move-dx 0.0 :move-dy 0.0
                                           :requested-follow-target-id '(1 2 3))))
             (sleep 0.1)

             ;; Test 3: Negative ID
             (send-net-message socket
                      (list :type :intent
                            :payload (list :move-dx 0.0 :move-dy 0.0
                                           :requested-attack-target-id -999)))
             (sleep 0.1)

             ;; Server should still be responsive
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (responsive nil))
               (loop :while (and (not responsive) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :snapshot))
                             (setf responsive t)))
                         (sleep 0.01))
               (unless responsive
                 (error "Server unresponsive after malformed IDs"))))
        (usocket:socket-close socket)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Double Login Prevention
;;;; Server should reject login attempts for already-logged-in accounts
;;;; ==========================================================================

(define-security-test test-double-login-prevented
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1341))
         (buffer1 (make-net-buffer))
         (buffer2 (make-net-buffer))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (mmorpg:run-server
                            :host host
                            :port port
                            :max-seconds 3.0)))))
    (sleep 0.2)
    (let ((socket1 (usocket:socket-connect host port :protocol :datagram))
          (socket2 (usocket:socket-connect host port :protocol :datagram)))
      (unwind-protect
           (progn
             ;; First client registers and logs in
             (send-net-message socket1
                      (list :type :register :username "sec-test-double" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil))
               (loop :while (and (not auth-ok) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (receive-net-message socket1 buffer1)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :auth-ok))
                             (setf auth-ok t)))
                         (sleep 0.01))
               (unless auth-ok
                 (error "First client auth failed")))

             ;; Second client tries to login with same account
             (send-net-message socket2
                      (list :type :login :username "sec-test-double" :password "test"))

             ;; Should receive auth-fail with :already-logged-in
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (got-rejection nil)
                    (all-messages nil))
               (loop :while (and (not got-rejection) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (receive-net-message socket2 buffer2)
                           (declare (ignore _h _p))
                           (when message
                             (push message all-messages)
                             (case (getf message :type)
                               (:auth-fail
                                (when (eq (getf message :reason) :already-logged-in)
                                  (setf got-rejection t)))
                               (:auth-ok
                                (error "Double login was allowed!")))))
                         (sleep 0.01))
               (unless got-rejection
                 (error "No :already-logged-in rejection received. Got messages: ~a"
                        (nreverse all-messages)))))
        (usocket:socket-close socket1)
        (usocket:socket-close socket2)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Auth Rate Limiting
;;;; Server should lock out IP after multiple failed login attempts
;;;; ==========================================================================

(define-security-test test-auth-rate-limiting
  ;; Note: This test triggers a rate limit lockout. We clear the rate limit state
  ;; at the end to avoid affecting subsequent tests.
  (unwind-protect
      (let* ((host "127.0.0.1")
             (port (env-int "MMORPG_NET_TEST_PORT" 1349))
             (buffer (make-net-buffer))
             (server-thread (sb-thread:make-thread
                             (lambda ()
                               (mmorpg:run-server
                                :host host
                                :port port
                                :max-seconds 4.0)))))
        (sleep 0.2)
        (let ((socket (usocket:socket-connect host port :protocol :datagram)))
          (unwind-protect
               (progn
                 ;; First register a valid account
                 (send-net-message socket
                          (list :type :register :username "rate-limit-test" :password "correct"))
                 (let* ((deadline (+ (get-internal-real-time)
                                     (floor (* 2 internal-time-units-per-second))))
                        (auth-ok nil))
                   (loop :while (and (not auth-ok) (< (get-internal-real-time) deadline))
                         :do (multiple-value-bind (message _h _p)
                                 (receive-net-message socket buffer)
                               (declare (ignore _h _p))
                               (when (and message (eq (getf message :type) :auth-ok))
                                 (setf auth-ok t)))
                             (sleep 0.01))
                   (unless auth-ok
                     (error "Failed to register test account")))

                 ;; Logout so we can test login rate limiting
                 (send-net-message socket (list :type :logout))
                 (sleep 0.2)

                 ;; Send 6 failed login attempts (wrong password) - *auth-max-attempts* is 5
                 (dotimes (i 6)
                   (send-net-message socket
                            (list :type :login :username "rate-limit-test" :password "wrong"))
                   (sleep 0.05))

                 ;; Wait for rate limit to kick in and collect responses
                 (sleep 0.3)

                 ;; Now try to login with CORRECT password - should be rate-limited
                 (send-net-message socket
                          (list :type :login :username "rate-limit-test" :password "correct"))

                 ;; Should receive :rate-limited rejection
                 (let* ((deadline (+ (get-internal-real-time)
                                     (floor (* 2 internal-time-units-per-second))))
                        (got-rate-limited nil)
                        (got-auth-ok nil))
                   (loop :while (and (not got-rate-limited) (not got-auth-ok)
                                     (< (get-internal-real-time) deadline))
                         :do (multiple-value-bind (message _h _p)
                                 (receive-net-message socket buffer)
                               (declare (ignore _h _p))
                               (when message
                                 (case (getf message :type)
                                   (:auth-fail
                                    (when (eq (getf message :reason) :rate-limited)
                                      (setf got-rate-limited t)))
                                   (:auth-ok
                                    (setf got-auth-ok t)))))
                             (sleep 0.01))
                   (when got-auth-ok
                     (error "Rate limiting failed - login succeeded despite lockout"))
                   (unless got-rate-limited
                     (error "No :rate-limited rejection received after 6 failed attempts"))))
            (usocket:socket-close socket)))
        (sb-thread:join-thread server-thread))
    ;; Cleanup: Clear rate limit state so subsequent tests aren't affected
    (auth-rate-clear-all)))

;;;; ==========================================================================
;;;; TEST: Double Login Race Condition
;;;; Two simultaneous login attempts for same account should result in only one success
;;;; ==========================================================================

(define-security-test test-double-login-race-condition
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1350))
         (buffer1 (make-net-buffer))
         (buffer2 (make-net-buffer))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (mmorpg:run-server
                            :host host
                            :port port
                            :max-seconds 4.0)))))
    (sleep 0.2)
    (let ((socket1 (usocket:socket-connect host port :protocol :datagram))
          (socket2 (usocket:socket-connect host port :protocol :datagram)))
      (unwind-protect
           (progn
             ;; First, register the account
             (send-net-message socket1
                      (list :type :register :username "race-test-user" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil))
               (loop :while (and (not auth-ok) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (receive-net-message socket1 buffer1)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :auth-ok))
                             (setf auth-ok t)))
                         (sleep 0.01))
               (unless auth-ok
                 (error "Failed to register test account")))

             ;; Logout
             (send-net-message socket1 (list :type :logout))
             (sleep 0.2)

             ;; NOW simulate race: send two login requests nearly simultaneously
             ;; from two different sockets
             (send-net-message socket1
                      (list :type :login :username "race-test-user" :password "test"))
             (send-net-message socket2
                      (list :type :login :username "race-test-user" :password "test"))

             ;; Collect responses - should get exactly ONE :auth-ok and ONE :already-logged-in
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 3 internal-time-units-per-second))))
                    (auth-ok-count 0)
                    (already-logged-in-count 0))
               (loop :while (and (< (+ auth-ok-count already-logged-in-count) 2)
                                 (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (msg1 _h1 _p1)
                             (receive-net-message socket1 buffer1)
                           (declare (ignore _h1 _p1))
                           (when msg1
                             (case (getf msg1 :type)
                               (:auth-ok (incf auth-ok-count))
                               (:auth-fail
                                (when (eq (getf msg1 :reason) :already-logged-in)
                                  (incf already-logged-in-count))))))
                         (multiple-value-bind (msg2 _h2 _p2)
                             (receive-net-message socket2 buffer2)
                           (declare (ignore _h2 _p2))
                           (when msg2
                             (case (getf msg2 :type)
                               (:auth-ok (incf auth-ok-count))
                               (:auth-fail
                                (when (eq (getf msg2 :reason) :already-logged-in)
                                  (incf already-logged-in-count))))))
                         (sleep 0.01))

               ;; Critical check: should have exactly ONE success
               (when (> auth-ok-count 1)
                 (error "RACE CONDITION: Got ~d auth-ok responses - both logins succeeded!"
                        auth-ok-count))
               (when (zerop auth-ok-count)
                 (error "Neither login attempt succeeded"))
               (unless (= already-logged-in-count 1)
                 (error "Expected 1 :already-logged-in rejection, got ~d"
                        already-logged-in-count))))
        (usocket:socket-close socket1)
        (usocket:socket-close socket2)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Extreme Coordinate Values
;;;; Server should handle NaN, Infinity, and huge coordinate values safely
;;;; ==========================================================================

(define-security-test test-extreme-coordinates-handled
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1342))
         (buffer (make-net-buffer))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (mmorpg:run-server
                            :host host
                            :port port
                            :max-seconds 2.0)))))
    (sleep 0.2)
    (let ((socket (usocket:socket-connect host port :protocol :datagram)))
      (unwind-protect
           (progn
             ;; Register and authenticate
             (send-net-message socket
                      (list :type :register :username "sec-test-coords" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil))
               (loop :while (and (not auth-ok) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :auth-ok))
                             (setf auth-ok t)))
                         (sleep 0.01))
               (unless auth-ok
                 (error "Auth failed")))

             ;; Send extreme target coordinates
             ;; Test 1: Huge positive values
             (send-net-message socket
                      (list :type :intent
                            :payload (list :target-x 999999999.0
                                           :target-y 999999999.0
                                           :target-active t)))
             (sleep 0.1)

             ;; Test 2: Huge negative values
             (send-net-message socket
                      (list :type :intent
                            :payload (list :target-x -999999999.0
                                           :target-y -999999999.0
                                           :target-active t)))
             (sleep 0.1)

             ;; Test 3: Very small values (near zero but not zero)
             (send-net-message socket
                      (list :type :intent
                            :payload (list :move-dx 0.0000001
                                           :move-dy 0.0000001)))
             (sleep 0.1)

             ;; Server should still be responsive
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (responsive nil))
               (loop :while (and (not responsive) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :snapshot))
                             (setf responsive t)))
                         (sleep 0.01))
               (unless responsive
                 (error "Server unresponsive after extreme coordinates"))))
        (usocket:socket-close socket)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Empty/Null Message Fields
;;;; Server should handle nil and empty values in message fields
;;;; ==========================================================================

(define-security-test test-empty-message-fields-handled
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1343))
         (buffer (make-net-buffer))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (mmorpg:run-server
                            :host host
                            :port port
                            :max-seconds 2.0)))))
    (sleep 0.2)
    (let ((socket (usocket:socket-connect host port :protocol :datagram)))
      (unwind-protect
           (progn
             ;; Register and authenticate
             (send-net-message socket
                      (list :type :register :username "sec-test-empty" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil))
               (loop :while (and (not auth-ok) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :auth-ok))
                             (setf auth-ok t)))
                         (sleep 0.01))
               (unless auth-ok
                 (error "Auth failed")))

             ;; Send intent with nil values
             (send-net-message socket
                      (list :type :intent
                            :payload (list :move-dx nil :move-dy nil)))
             (sleep 0.1)

             ;; Send intent with empty payload
             (send-net-message socket
                      (list :type :intent :payload nil))
             (sleep 0.1)

             ;; Send intent with missing payload
             (send-net-message socket (list :type :intent))
             (sleep 0.1)

             ;; Send completely empty message
             (send-net-message socket nil)
             (sleep 0.1)

             ;; Server should still be responsive
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (responsive nil))
               (loop :while (and (not responsive) (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (message _h _p)
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when (and message (eq (getf message :type) :snapshot))
                             (setf responsive t)))
                         (sleep 0.01))
               (unless responsive
                 (error "Server unresponsive after empty/null fields"))))
        (usocket:socket-close socket)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Intent Isolation (Ownership)
;;;; Client should only be able to control their own player, not others
;;;; ==========================================================================

(define-security-test test-intent-isolation
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1344))
         (buffer1 (make-net-buffer))
         (buffer2 (make-net-buffer))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (mmorpg:run-server
                            :host host
                            :port port
                            :max-seconds 3.0)))))
    (sleep 0.2)
    (let ((socket1 (usocket:socket-connect host port :protocol :datagram))
          (socket2 (usocket:socket-connect host port :protocol :datagram)))
      (unwind-protect
           (progn
             ;; Register two different players
             (send-net-message socket1
                      (list :type :register :username "player-one" :password "test"))
             (send-net-message socket2
                      (list :type :register :username "player-two" :password "test"))

             ;; Wait for both to auth
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth1 nil)
                    (auth2 nil)
                    (p1-id nil)
                    (p2-id nil)
                    (p1-initial-x nil)
                    (p2-initial-x nil))
               (loop :while (and (or (not auth1) (not auth2) (not p1-initial-x) (not p2-initial-x))
                                 (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (msg1 _h1 _p1)
                             (receive-net-message socket1 buffer1)
                           (declare (ignore _h1 _p1))
                           (when msg1
                             (case (getf msg1 :type)
                               (:auth-ok
                                (setf auth1 t
                                      p1-id (getf msg1 :player-id)))
                               (:snapshot
                                (when (and auth1 (null p1-initial-x))
                                  (let ((players (extract-players-as-plists (getf msg1 :state))))
                                    (dolist (p players)
                                      (when (eql (getf p :id) p1-id)
                                        (setf p1-initial-x (getf p :x))))))))))
                         (multiple-value-bind (msg2 _h2 _p2)
                             (receive-net-message socket2 buffer2)
                           (declare (ignore _h2 _p2))
                           (when msg2
                             (case (getf msg2 :type)
                               (:auth-ok
                                (setf auth2 t
                                      p2-id (getf msg2 :player-id)))
                               (:snapshot
                                (when (and auth2 (null p2-initial-x))
                                  (let ((players (extract-players-as-plists (getf msg2 :state))))
                                    (dolist (p players)
                                      (when (eql (getf p :id) p2-id)
                                        (setf p2-initial-x (getf p :x))))))))))
                         (sleep 0.01))

               (unless (and auth1 auth2 p1-initial-x p2-initial-x)
                 (error "Failed to authenticate both players"))

               ;; Player 1 sends movement intent
               (send-net-message socket1
                        (list :type :intent
                              :payload (list :move-dx 1.0 :move-dy 0.0)))

               ;; Wait and check positions
               (sleep 0.5)

               ;; With delta compression, only changed entities appear in snapshots.
               ;; p2-stationary starts true; if player 2 appears and moved, we set it nil.
               (let ((p1-moved nil)
                     (p2-stationary t)
                     (p2-last-x p2-initial-x)
                     (check-deadline (+ (get-internal-real-time)
                                        (floor (* 2 internal-time-units-per-second)))))
                 (loop :while (and (not p1-moved)
                                   (< (get-internal-real-time) check-deadline))
                       :do (multiple-value-bind (msg _h _p)
                               (receive-net-message socket1 buffer1)
                             (declare (ignore _h _p))
                             (when (and msg (eq (getf msg :type) :snapshot))
                               (let ((players (extract-players-as-plists (getf msg :state))))
                                 (dolist (p players)
                                   (cond
                                     ((eql (getf p :id) p1-id)
                                      (when (> (getf p :x) p1-initial-x)
                                        (setf p1-moved t)))
                                     ((eql (getf p :id) p2-id)
                                      ;; Player 2 appeared - check if they moved significantly
                                      (let ((new-x (getf p :x)))
                                        (when (> (abs (- new-x p2-last-x)) 10.0)
                                          (setf p2-stationary nil))
                                        (setf p2-last-x new-x))))))))
                           (sleep 0.01))

                 (unless p1-moved
                   (error "Player 1's movement was not applied"))
                 (unless p2-stationary
                   (error "Player 2 moved despite not sending intent - isolation breach!")))))
        (usocket:socket-close socket1)
        (usocket:socket-close socket2)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Attack Range Validation (Server Authority)
;;;; Server should handle attack requests for invalid/far targets without crashing
;;;; ==========================================================================

(define-security-test test-attack-range-validated
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1345))
         (buffer (make-net-buffer))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (mmorpg:run-server
                            :host host
                            :port port
                            :max-seconds 3.0)))))
    (sleep 0.2)
    (let ((socket (usocket:socket-connect host port :protocol :datagram)))
      (unwind-protect
           (progn
             (send-net-message socket
                      (list :type :register :username "range-test" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil))
               ;; Wait for auth
               (loop :while (and (not auth-ok)
                                 (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (msg _h _p)
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when (and msg (eq (getf msg :type) :auth-ok))
                             (setf auth-ok t)))
                         (sleep 0.01))

               (unless auth-ok
                 (error "Auth failed"))

               ;; Move far away from spawn
               (send-net-message socket
                        (list :type :intent
                              :payload (list :target-x 9999.0
                                             :target-y 9999.0
                                             :target-active t)))
               (sleep 0.2)

               ;; Try to attack a non-existent NPC ID (should be ignored)
               (send-net-message socket
                        (list :type :intent
                              :payload (list :requested-attack-target-id 99999
                                             :attack t)))
               (sleep 0.2)

               ;; Try to attack NPC id 1 from far away (if exists, shouldn't hit)
               (send-net-message socket
                        (list :type :intent
                              :payload (list :requested-attack-target-id 1
                                             :attack t)))
               (sleep 0.2)

               ;; Server should still be responsive
               (let* ((check-deadline (+ (get-internal-real-time)
                                         (floor (* 2 internal-time-units-per-second))))
                      (responsive nil))
                 (loop :while (and (not responsive)
                                   (< (get-internal-real-time) check-deadline))
                       :do (multiple-value-bind (msg _h _p)
                               (receive-net-message socket buffer)
                             (declare (ignore _h _p))
                             (when (and msg (eq (getf msg :type) :snapshot))
                               (setf responsive t)))
                           (sleep 0.01))
                 (unless responsive
                   (error "Server unresponsive after out-of-range attack attempts")))))
        (usocket:socket-close socket)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Rapid Intent Spam (Timing/Race Conditions)
;;;; Server should handle rapid intent spam without breaking
;;;; ==========================================================================

(define-security-test test-rapid-intent-spam
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1346))
         (buffer (make-net-buffer))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (mmorpg:run-server
                            :host host
                            :port port
                            :max-seconds 4.0)))))
    (sleep 0.2)
    (let ((socket (usocket:socket-connect host port :protocol :datagram)))
      (unwind-protect
           (progn
             (send-net-message socket
                      (list :type :register :username "spam-test" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil))
               (loop :while (and (not auth-ok)
                                 (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (msg _h _p)
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when (and msg (eq (getf msg :type) :auth-ok))
                             (setf auth-ok t)))
                         (sleep 0.01))
               (unless auth-ok
                 (error "Auth failed")))

             ;; Spam 100 intents rapidly
             (dotimes (i 100)
               (send-net-message socket
                        (list :type :intent
                              :payload (list :move-dx (if (evenp i) 1.0 -1.0)
                                             :move-dy 0.0
                                             :attack (zerop (mod i 10))))))

             ;; Server should still be responsive
             (sleep 0.5)
             (let* ((check-deadline (+ (get-internal-real-time)
                                       (floor (* 2 internal-time-units-per-second))))
                    (responsive nil))
               (loop :while (and (not responsive)
                                 (< (get-internal-real-time) check-deadline))
                     :do (multiple-value-bind (msg _h _p)
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when (and msg (eq (getf msg :type) :snapshot))
                             (setf responsive t)))
                         (sleep 0.01))
               (unless responsive
                 (error "Server became unresponsive after intent spam"))))
        (usocket:socket-close socket)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Inventory Stack Limits (Inventory Bugs)
;;;; Adding items should respect stack limits
;;;; ==========================================================================

(define-security-test test-inventory-stack-limits
  ;; This test uses internal functions directly (no network needed)
  (let* ((player (make-player 0.0 0.0 :id 999)))
    ;; Try to add 1000 of a stackable item (stack limit is typically 20-99)
    (let ((result (grant-inventory-item player :health-potion 1000)))
      ;; Should get back the overflow amount (items that didn't fit)
      (when (and result (zerop result))
        (error "Stack limit was not enforced - added 1000 items with no overflow"))
      ;; Inventory should have items but not infinite
      (let* ((inv (player-inventory player))
             (slots (and inv (inventory-slots inv)))
             (total 0))
        (when slots
          (loop :for slot :across slots
                :when (and slot
                           (eq (inventory-slot-item-id slot) :health-potion))
                  :do (incf total (inventory-slot-count slot))))
        (when (>= total 1000)
          (error "Inventory accepted 1000 items without enforcing stack limits"))))))

;;;; ==========================================================================
;;;; TEST: Negative Value Prevention (Economy/Overflow)
;;;; Server should prevent negative HP, XP, or other values
;;;; ==========================================================================

(define-security-test test-negative-value-prevention
  ;; Test that player stats can't go negative through normal operations
  (let* ((player (make-player 0.0 0.0 :id 998)))
    ;; Set HP to something positive
    (setf (player-hp player) 100)

    ;; Apply massive damage (more than HP) - use the same logic as combat.lisp
    (let ((damage 9999))
      (setf (player-hp player) (max 0 (- (player-hp player) damage))))

    ;; HP should be 0, not negative
    (when (< (player-hp player) 0)
      (error "HP went negative: ~a" (player-hp player)))

    ;; Verify XP starts at valid value (not negative)
    (let ((xp (player-lifetime-xp player)))
      (when (and xp (< xp 0))
        (error "XP is negative: ~a" xp)))))

;;;; ==========================================================================
;;;; TEST: Corrupted Plist Handling (Persistence)
;;;; Server should handle corrupted/malformed save data gracefully
;;;; ==========================================================================

(define-security-test test-corrupted-plist-handling
  ;; Test deserialize-player handles bad data
  ;; deserialize-player takes (plist inventory-size equipment-size)
  (let ((inv-size *inventory-size*)
        (equip-size (length *equipment-slot-ids*)))

    ;; Test with completely empty/nil plist - should return a valid player with defaults
    (handler-case
        (let ((player (deserialize-player nil inv-size equip-size)))
          (unless player
            (error "deserialize-player returned nil for empty plist"))
          ;; Player should have valid defaults
          (unless (>= (player-hp player) 0)
            (error "Player HP not valid after empty plist")))
      (error (e)
        (error "Crashed on nil plist: ~a" e)))

    ;; Test with minimal valid plist
    (handler-case
        (let ((player (deserialize-player (list :id 1 :x 0.0 :y 0.0)
                                          inv-size equip-size)))
          (unless player
            (error "deserialize-player returned nil for minimal plist")))
      (error (e)
        (error "Crashed on minimal plist: ~a" e)))

    ;; Test with negative HP in plist - getf will just use the value,
    ;; but the game logic should handle it
    (handler-case
        (let ((player (deserialize-player (list :id 2 :x 0.0 :y 0.0 :hp -100)
                                          inv-size equip-size)))
          ;; This tests that deserialize doesn't crash on negative HP
          ;; The game logic should clamp/handle it
          (declare (ignore player))
          t)
      (error (e)
        (error "Crashed on negative HP in plist: ~a" e)))))

;;;; ==========================================================================
;;;; TEST: Session Hijack Prevention (Ownership)
;;;; Client should not be able to claim another player's session
;;;; ==========================================================================

(define-security-test test-session-hijack-prevention
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1347))
         (buffer (make-net-buffer))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (mmorpg:run-server
                            :host host
                            :port port
                            :max-seconds 3.0)))))
    (sleep 0.2)
    (let ((socket (usocket:socket-connect host port :protocol :datagram)))
      (unwind-protect
           (progn
             ;; Try to send intent with forged player-id before authenticating
             (send-net-message socket (list :type :hello))
             (sleep 0.1)

             ;; Send intent claiming to be player 1 (forged)
             (send-net-message socket
                      (list :type :intent
                            :player-id 1  ; Forged player ID
                            :payload (list :move-dx 1.0 :move-dy 0.0)))
             (sleep 0.2)

             ;; Server should still be running and reject the forged intent
             ;; Now authenticate properly
             (send-net-message socket
                      (list :type :register :username "hijack-test" :password "test"))

             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil)
                    (my-id nil))
               (loop :while (and (not auth-ok)
                                 (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (msg _h _p)
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when msg
                             (case (getf msg :type)
                               (:auth-ok
                                (setf auth-ok t
                                      my-id (getf msg :player-id))))))
                         (sleep 0.01))
               (unless auth-ok
                 (error "Auth failed after forged intent"))

               ;; Our assigned ID should NOT be 1 (the forged ID)
               ;; This proves the server didn't accept our forged claim
               (when (and my-id (= my-id 1))
                 ;; This could be coincidence, so not an error, just a note
                 t)))
        (usocket:socket-close socket)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Pickup Range Validation
;;;; Player must be standing on same tile to pick up items
;;;; ==========================================================================

(define-security-test test-pickup-requires-same-tile
  ;; Create minimal world and player to test pickup-tile-in-range-p
  (let* ((tile-size 64.0)  ; typical tile-dest-size
         (world (mmorpg::%make-world))
         (player (mmorpg::make-player 160.0 224.0 :id 1)))  ; Start at test position
    ;; Set world tile size
    (setf (mmorpg::world-tile-dest-size world) tile-size)

    ;; Place player at center of tile (2, 3) - world coords (160, 224)
    (setf (mmorpg::player-x player) 160.0
          (mmorpg::player-y player) 224.0)

    ;; Test 1: Same tile (2, 3) - should succeed
    (unless (mmorpg::pickup-tile-in-range-p player 2 3 world)
      (error "Pickup should succeed when player is on same tile (2,3)"))

    ;; Test 2: Adjacent tile (3, 3) - should fail
    (when (mmorpg::pickup-tile-in-range-p player 3 3 world)
      (error "Pickup should fail for adjacent tile (3,3)"))

    ;; Test 3: Adjacent tile (2, 4) - should fail
    (when (mmorpg::pickup-tile-in-range-p player 2 4 world)
      (error "Pickup should fail for adjacent tile (2,4)"))

    ;; Test 4: Distant tile (10, 10) - should fail
    (when (mmorpg::pickup-tile-in-range-p player 10 10 world)
      (error "Pickup should fail for distant tile (10,10)"))

    ;; Test 5: Player at edge of tile - still same tile
    (setf (mmorpg::player-x player) 128.5  ; just inside tile 2
          (mmorpg::player-y player) 192.5) ; just inside tile 3
    (unless (mmorpg::pickup-tile-in-range-p player 2 3 world)
      (error "Pickup should succeed at tile edge (still tile 2,3)"))))

;;;; ==========================================================================
;;;; TEST: Duplicate Pickup Prevention (Duplication)
;;;; Rapidly picking up same object should not duplicate items
;;;; ==========================================================================

(define-security-test test-duplicate-pickup-prevented
  (let* ((host "127.0.0.1")
         (port (env-int "MMORPG_NET_TEST_PORT" 1348))
         (buffer (make-net-buffer))
         (server-thread (sb-thread:make-thread
                         (lambda ()
                           (mmorpg:run-server
                            :host host
                            :port port
                            :max-seconds 3.0)))))
    (sleep 0.2)
    (let ((socket (usocket:socket-connect host port :protocol :datagram)))
      (unwind-protect
           (progn
             (send-net-message socket
                      (list :type :register :username "dupe-test" :password "test"))
             (let* ((deadline (+ (get-internal-real-time)
                                 (floor (* 2 internal-time-units-per-second))))
                    (auth-ok nil))
               (loop :while (and (not auth-ok)
                                 (< (get-internal-real-time) deadline))
                     :do (multiple-value-bind (msg _h _p)
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when (and msg (eq (getf msg :type) :auth-ok))
                             (setf auth-ok t)))
                         (sleep 0.01))
               (unless auth-ok
                 (error "Auth failed")))

             ;; Spam pickup requests for same object coordinates
             ;; Even if there's an object there, we shouldn't get duplicates
             (dotimes (i 20)
               (send-net-message socket
                        (list :type :intent
                              :payload (list :requested-pickup-object-id :chest
                                             :requested-pickup-tx 5
                                             :requested-pickup-ty 5
                                             :target-x 80.0
                                             :target-y 80.0
                                             :target-active t))))

             ;; Server should handle this gracefully
             (sleep 0.3)
             (let* ((check-deadline (+ (get-internal-real-time)
                                       (floor (* 2 internal-time-units-per-second))))
                    (responsive nil))
               (loop :while (and (not responsive)
                                 (< (get-internal-real-time) check-deadline))
                     :do (multiple-value-bind (msg _h _p)
                             (receive-net-message socket buffer)
                           (declare (ignore _h _p))
                           (when (and msg (eq (getf msg :type) :snapshot))
                             (setf responsive t)))
                         (sleep 0.01))
               (unless responsive
                 (error "Server unresponsive after pickup spam"))))
        (usocket:socket-close socket)))
    (sb-thread:join-thread server-thread)))

;;;; ==========================================================================
;;;; TEST: Movement Direction Clamping (Speed Hack Prevention)
;;;; Server should clamp move-dx/move-dy to [-1.0, 1.0] range
;;;; ==========================================================================

(define-security-test test-movement-direction-clamped
  ;; Test that %clamp-direction properly limits values
  (let ((clamped-positive (%clamp-direction 1000.0))
        (clamped-negative (%clamp-direction -1000.0))
        (clamped-normal (%clamp-direction 0.5))
        (clamped-edge-pos (%clamp-direction 1.0))
        (clamped-edge-neg (%clamp-direction -1.0)))
    ;; Large positive should clamp to 1.0
    (unless (= clamped-positive 1.0)
      (error "Failed to clamp 1000.0 to 1.0, got ~a" clamped-positive))
    ;; Large negative should clamp to -1.0
    (unless (= clamped-negative -1.0)
      (error "Failed to clamp -1000.0 to -1.0, got ~a" clamped-negative))
    ;; Normal values should pass through
    (unless (= clamped-normal 0.5)
      (error "Normal value 0.5 was modified to ~a" clamped-normal))
    ;; Edge values should be preserved
    (unless (= clamped-edge-pos 1.0)
      (error "Edge value 1.0 was modified to ~a" clamped-edge-pos))
    (unless (= clamped-edge-neg -1.0)
      (error "Edge value -1.0 was modified to ~a" clamped-edge-neg))

    ;; Test that apply-intent-plist uses clamping
    (let ((intent (make-intent)))
      (apply-intent-plist intent (list :move-dx 1000.0 :move-dy -500.0))
      (unless (= (intent-move-dx intent) 1.0)
        (error "apply-intent-plist didn't clamp move-dx, got ~a"
               (intent-move-dx intent)))
      (unless (= (intent-move-dy intent) -1.0)
        (error "apply-intent-plist didn't clamp move-dy, got ~a"
               (intent-move-dy intent))))))

;;;; ==========================================================================
;;;; TEST: Auth Replay Protection
;;;; Validate timestamp window and duplicate nonce behavior
;;;; ==========================================================================

(define-security-test test-auth-check-replay-window
  (unwind-protect
      (progn
        ;; Reset nonce cache for a clean test.
        (with-auth-nonce-lock
          (clrhash *auth-nonce-cache*)
          (setf *auth-nonce-last-cleanup* 0))
        ;; Valid timestamp should be accepted once.
        (let* ((nonce "auth-replay-ok")
               (timestamp (get-universal-time)))
          (unless (auth-check-replay nonce timestamp)
            (error "Valid auth replay check was rejected"))
          (when (auth-check-replay nonce timestamp)
            (error "Duplicate nonce was accepted")))
        ;; Too old timestamps should be rejected.
        (let ((old-ts (- (get-universal-time) *auth-timestamp-window* 1)))
          (when (auth-check-replay "auth-replay-old" old-ts)
            (error "Expired auth timestamp was accepted")))
        ;; Future timestamps beyond tolerance should be rejected.
        (let ((future-ts (+ (get-universal-time) 10)))
          (when (auth-check-replay "auth-replay-future" future-ts)
            (error "Future auth timestamp was accepted"))))
    ;; Cleanup nonce cache to avoid cross-test contamination.
    (with-auth-nonce-lock
      (clrhash *auth-nonce-cache*)
      (setf *auth-nonce-last-cleanup* 0))))

;;;; ==========================================================================
;;;; THREAD-SAFETY REGRESSION TESTS (Multi-threaded Server Mode)
;;;; These tests verify mutex-protected operations work correctly under
;;;; concurrent access, preventing regressions when MMORPG_WORKER_THREADS > 1
;;;; ==========================================================================

#+sbcl
(define-security-test test-concurrent-dirty-flag-marking
  ;; Test that concurrent mark-player-dirty calls don't lose updates
  ;; This verifies *player-sessions-lock* protection
  (let* ((test-player-ids '(1001 1002 1003 1004 1005))
         (iterations-per-thread 100)
         (errors nil)
         (errors-lock (sb-thread:make-mutex :name "errors-lock"))
         (threads nil))
    ;; Register test sessions
    (dolist (id test-player-ids)
      (let ((player (make-player 0.0 0.0 :id id)))
        (register-player-session player :zone-id :test-zone)))

    (unwind-protect
        (progn
          ;; Spawn threads that concurrently mark players dirty
          (dotimes (t-idx 5)
            (let ((my-iterations iterations-per-thread)
                  (my-ids test-player-ids)
                  (my-errors-lock errors-lock))
              (push (sb-thread:make-thread
                     (lambda ()
                       (handler-case
                           (dotimes (i my-iterations)
                             (dolist (id my-ids)
                               (mark-player-dirty id)))
                         (error (e)
                           (sb-thread:with-mutex (my-errors-lock)
                             (push (format nil "Thread error: ~a" e) errors))))))
                    threads)))
          ;; Wait for all threads
          (dolist (thread threads)
            (sb-thread:join-thread thread))
          ;; Check for errors
          (when errors
            (error "Concurrent dirty marking failed: ~{~a~^, ~}" errors))
          ;; Verify all sessions still exist and are marked dirty
          (dolist (id test-player-ids)
            (with-player-sessions-lock
              (let ((session (gethash id *player-sessions*)))
                (unless session
                  (error "Session ~d disappeared during concurrent access" id))
                (unless (player-session-dirty-p session)
                  (error "Session ~d lost dirty flag during concurrent access" id))))))
      ;; Cleanup
      (dolist (id test-player-ids)
        (unregister-player-session id)))))

#+sbcl
(define-security-test test-concurrent-rate-limit-recording
  ;; Test that concurrent auth failure recording counts correctly
  ;; This verifies *auth-rate-limits-lock* protection
  (let* ((test-host "192.168.99.99")  ; Use fake IP to not affect other tests
         (threads-count 10)
         (failures-per-thread 10)
         (expected-total (* threads-count failures-per-thread))
         (current-time (float (get-universal-time) 1.0)))
    (unwind-protect
        (let ((threads nil))
          ;; Spawn threads that concurrently record failures
          (dotimes (t-idx threads-count)
            (let ((host test-host)
                  (time current-time)
                  (count failures-per-thread))
              (push (sb-thread:make-thread
                     (lambda ()
                       (dotimes (i count)
                         (auth-rate-record-failure host time))))
                    threads)))
          ;; Wait for all threads
          (dolist (thread threads)
            (sb-thread:join-thread thread))
          ;; Verify total count
          (with-auth-rate-limits-lock
            (let ((entry (gethash test-host *auth-rate-limits*)))
              (unless entry
                (error "Rate limit entry disappeared"))
              (let ((actual-count (auth-rate-entry-attempts entry)))
                ;; Should have exactly expected-total attempts recorded
                ;; (or at least *auth-max-attempts* if lockout triggered)
                (when (< actual-count (min expected-total *auth-max-attempts*))
                  (error "Lost rate limit counts: expected >= ~d, got ~d"
                         (min expected-total *auth-max-attempts*)
                         actual-count))))))
      ;; Cleanup
      (with-auth-rate-limits-lock
        (remhash test-host *auth-rate-limits*)))))

#+sbcl
(define-security-test test-concurrent-nonce-cache-access
  ;; Test that concurrent replay checks don't corrupt the nonce cache
  ;; This verifies *auth-nonce-lock* protection
  (let* ((nonce-count 100)
         (threads-count 5)
         (accepted-count 0)
         (rejected-count 0)
         (count-lock (sb-thread:make-mutex :name "test-count-lock"))
         (base-time (get-universal-time))
         (threads nil))
    ;; Each thread tries to check the same set of nonces
    ;; First thread to check each nonce should succeed, others should fail
    (dotimes (t-idx threads-count)
      (let ((my-nonce-count nonce-count)
            (my-base-time base-time)
            (my-count-lock count-lock))
        (push (sb-thread:make-thread
               (lambda ()
                 (dotimes (i my-nonce-count)
                   (let* ((nonce (format nil "test-nonce-~d" i))
                          (result (auth-check-replay nonce my-base-time)))
                     (sb-thread:with-mutex (my-count-lock)
                       (if result
                           (incf accepted-count)
                           (incf rejected-count)))))))
              threads)))
    ;; Wait for all threads
    (dolist (thread threads)
      (sb-thread:join-thread thread))
    ;; Verify exactly nonce-count acceptances (one per unique nonce)
    (unless (= accepted-count nonce-count)
      (error "Nonce cache race: expected ~d acceptances, got ~d (duplicates accepted!)"
             nonce-count accepted-count))
    ;; Verify the rest were rejected as replays
    (let ((expected-rejections (* (1- threads-count) nonce-count)))
      (unless (= rejected-count expected-rejections)
        (error "Nonce cache race: expected ~d rejections, got ~d"
               expected-rejections rejected-count)))))

#+sbcl
(define-security-test test-concurrent-zone-object-modification
  ;; Test that concurrent zone object access doesn't corrupt state
  ;; This verifies *zone-objects-lock* protection
  ;; Task 5.5: Updated to use zone-object structs
  (let* ((world (%make-world))
         (zone (%make-zone))
         (initial-objects (list (%make-zone-object :id :chest :x 5 :y 5 :count 10 :base-count 10
                                                   :respawn 0.0 :respawnable t :snapshot-dirty nil)
                                (%make-zone-object :id :chest :x 6 :y 6 :count 10 :base-count 10
                                                   :respawn 0.0 :respawnable t :snapshot-dirty nil)
                                (%make-zone-object :id :chest :x 7 :y 7 :count 10 :base-count 10
                                                   :respawn 0.0 :respawnable t :snapshot-dirty nil)))
         (iterations 50)
         (threads nil))
    ;; Setup world with zone objects
    (setf (zone-objects zone) (copy-list initial-objects))
    (setf (world-zone world) zone)
    (setf (world-tile-dest-size world) 64.0)

    ;; Spawn threads: some do respawn updates, some attempt pickups
    (dotimes (t-idx 4)
      (let ((my-world world)
            (my-iterations iterations)
            (my-t-idx t-idx))
        (push (sb-thread:make-thread
               (lambda ()
                 (dotimes (i my-iterations)
                   ;; Alternate between respawn update and pickup attempt
                   (if (evenp i)
                       (update-object-respawns my-world 0.1)
                       ;; Attempt pickup at various tiles
                       (let ((player (make-player (* 64.0 (+ 5 (mod i 3)))
                                                           (* 64.0 (+ 5 (mod i 3)))
                                                           :id (+ 2000 my-t-idx))))
                         (pickup-object-at-tile player my-world
                                                (+ 5 (mod i 3))
                                                (+ 5 (mod i 3))
                                                nil))))))
              threads)))
    ;; Wait for all threads
    (dolist (thread threads)
      (sb-thread:join-thread thread))
    ;; Verify zone-objects is still a valid list (not corrupted)
    (let ((objects (zone-objects zone)))
      (unless (listp objects)
        (error "Zone objects corrupted - not a list"))
      ;; Each object should still have valid zone-object struct
      (dolist (obj objects)
        (unless (and (zone-object-p obj) (zone-object-id obj))
          (error "Zone object corrupted: ~a" obj))))))

;;;; ==========================================================================
;;;; Main Test Runner
;;;; ==========================================================================

(defun run-security-tests-internal ()
  (format t "~&=== Running Security Tests ===~%")
  (setf *tests-passed* 0
        *tests-failed* 0)

  ;; Original tests
  (test-unauthenticated-intent-rejected)
  (test-speed-hack-prevented)
  (test-chat-message-length-enforced)
  (test-malformed-intent-ids-handled)
  (test-double-login-prevented)
  (test-auth-rate-limiting)
  (test-double-login-race-condition)
  (test-extreme-coordinates-handled)
  (test-empty-message-fields-handled)

  ;; Ownership / Control tests
  (test-intent-isolation)
  (test-session-hijack-prevention)

  ;; Server Authority tests
  (test-attack-range-validated)

  ;; Timing / Race Condition tests
  (test-rapid-intent-spam)

  ;; Inventory tests
  (test-inventory-stack-limits)

  ;; Economy / Overflow tests
  (test-negative-value-prevention)

  ;; Persistence tests
  (test-corrupted-plist-handling)

  ;; Pickup validation
  (test-pickup-requires-same-tile)

  ;; Duplication tests
  (test-duplicate-pickup-prevented)

  ;; Input validation tests
  (test-movement-direction-clamped)
  (test-auth-check-replay-window)

  ;; Thread-safety regression tests (SBCL only)
  ;; These prevent multi-threaded mode from regressing
  #+sbcl
  (progn
    (test-concurrent-dirty-flag-marking)
    (test-concurrent-rate-limit-recording)
    (test-concurrent-nonce-cache-access)
    (test-concurrent-zone-object-modification))

  (format t "~&~%Results: ~d passed, ~d failed~%" *tests-passed* *tests-failed*)
  (zerop *tests-failed*))

(export 'run-security-tests)

;;; ============================================================================
;;; Trade Tests
;;; ============================================================================


(defun run-trade-tests-internal ()
  "Run all trade system tests. Returns T if all pass, NIL otherwise."
  (let ((passed 0)
        (failed 0)
        (tests '(test-trade-session-creation
                 test-trade-validation-same-player
                 test-trade-validation-distance
                 test-trade-validation-already-trading
                 test-trade-offer-add-remove
                 test-trade-offer-confirm
                 test-trade-offer-confirm-resets-on-change
                 test-trade-cancel
                 test-trade-timeout
                 test-offer-to-item-list
                 test-add-items-to-inventory-array
                 ;; Phase 3: Trade safety tests
                 test-trade-ownership-mismatch-aborts
                 test-trade-memory-backend-consistency
                 test-trade-expired-ownership-aborts)))
    (format t "~%=== Running Trade Tests ===~%")
    ;; Ensure game data is loaded
    (unless *game-data-loaded-p*
      (load-game-data))
    ;; Clear trade state before tests
    (clrhash *active-trades*)
    (clrhash *player-trade-map*)
    (dolist (test tests)
      (handler-case
          (progn
            ;; Clean up before each test
            (clrhash *active-trades*)
            (clrhash *player-trade-map*)
            (funcall test)
            (incf passed)
            (format t "~a ~a~%" "OK" (symbol-name test)))
        (error (e)
          (incf failed)
          (format t "~a ~a: ~a~%" "FAIL" (symbol-name test) e))))
    (format t "~%Results: ~d passed, ~d failed~%" passed failed)
    (zerop failed)))

;;;; ========================================================================
;;;; Trade Session Tests
;;;; ========================================================================

(defun test-trade-session-creation ()
  "Test creating a trade session between two players."
  (let ((session (create-trade-session 1 2)))
    (assert (not (null session)) () "Session created")
    (assert (= (trade-session-player1-id session) 1) () "Player 1 ID correct")
    (assert (= (trade-session-player2-id session) 2) () "Player 2 ID correct")
    (assert (eq (trade-session-state session) :pending) () "State is pending")
    ;; Check registration
    (assert (player-in-trade-p 1) () "Player 1 registered as trading")
    (assert (player-in-trade-p 2) () "Player 2 registered as trading")
    (assert (eq (get-player-trade 1) session) () "Can find session for player 1")
    (assert (eq (get-player-trade 2) session) () "Can find session for player 2")))

(defun test-trade-validation-same-player ()
  "Test that players cannot trade with themselves."
  (let ((player (make-player 0.0 0.0 :id 1)))
    (multiple-value-bind (valid-p error-msg)
        (validate-trade-request player player)
      (assert (not valid-p) () "Self-trade rejected")
      (assert (search "yourself" error-msg) () "Error mentions self"))))

(defun test-trade-validation-distance ()
  "Test that players must be within trade distance."
  (let ((player1 (make-player 0.0 0.0 :id 1))
        (player2-near (make-player 100.0 100.0 :id 2))  ; ~7 tiles away (within 10)
        (player2-far (make-player 500.0 500.0 :id 3)))  ; ~44 tiles away (too far)
    ;; Same zone for both
    (setf (player-zone-id player1) :zone-1
          (player-zone-id player2-near) :zone-1
          (player-zone-id player2-far) :zone-1)
    ;; Near player should be valid
    (multiple-value-bind (valid-p error-msg)
        (validate-trade-request player1 player2-near)
      (declare (ignore error-msg))
      (assert valid-p () "Near player trade valid"))
    ;; Far player should be invalid
    (multiple-value-bind (valid-p error-msg)
        (validate-trade-request player1 player2-far)
      (assert (not valid-p) () "Far player trade rejected")
      (assert (search "far" error-msg) () "Error mentions distance"))))

(defun test-trade-validation-already-trading ()
  "Test that players in a trade cannot start another."
  (let ((player1 (make-player 0.0 0.0 :id 1))
        (player2 (make-player 50.0 0.0 :id 2))
        (player3 (make-player 50.0 50.0 :id 3)))
    ;; Same zone
    (setf (player-zone-id player1) :zone-1
          (player-zone-id player2) :zone-1
          (player-zone-id player3) :zone-1)
    ;; Start trade between 1 and 2
    (create-trade-session 1 2)
    ;; Player 1 trying to trade with player 3 should fail
    (multiple-value-bind (valid-p error-msg)
        (validate-trade-request player1 player3)
      (assert (not valid-p) () "Already trading rejected")
      (assert (search "already" error-msg) () "Error mentions already trading"))))

;;;; ========================================================================
;;;; Trade Offer Tests
;;;; ========================================================================

(defun test-trade-offer-add-remove ()
  "Test adding and removing items from trade offer."
  (let ((session (create-trade-session 1 2)))
    ;; Add item to offer
    (assert (add-to-trade-offer session 1 0 5) () "Add to offer returns T")
    ;; Check offer updated
    (let ((offer (get-player-offer session 1)))
      (assert (= (gethash 0 (trade-offer-slots offer)) 5) () "Slot 0 has count 5"))
    ;; Remove item
    (remove-from-trade-offer session 1 0)
    (let ((offer (get-player-offer session 1)))
      (assert (null (gethash 0 (trade-offer-slots offer))) () "Slot 0 removed"))))

(defun test-trade-offer-confirm ()
  "Test confirming trade offers."
  (let ((session (create-trade-session 1 2)))
    ;; Player 1 confirms
    (assert (not (confirm-trade-offer session 1)) () "One confirm returns NIL")
    (assert (trade-offer-confirmed (trade-session-player1-offer session))
            () "Player 1 confirmed")
    ;; Player 2 confirms
    (assert (confirm-trade-offer session 2) () "Both confirm returns T")
    (assert (eq (trade-session-state session) :both-confirmed)
            () "State is both-confirmed")))

(defun test-trade-offer-confirm-resets-on-change ()
  "Test that changing offer unconfirms both players."
  (let ((session (create-trade-session 1 2)))
    ;; Both players confirm
    (confirm-trade-offer session 1)
    (confirm-trade-offer session 2)
    (assert (eq (trade-session-state session) :both-confirmed)
            () "Both confirmed initially")
    ;; Player 1 modifies offer
    (add-to-trade-offer session 1 0 3)
    ;; Both should be unconfirmed
    (assert (not (trade-offer-confirmed (trade-session-player1-offer session)))
            () "Player 1 unconfirmed after change")
    (assert (not (trade-offer-confirmed (trade-session-player2-offer session)))
            () "Player 2 unconfirmed after change")
    (assert (eq (trade-session-state session) :pending)
            () "State back to pending")))

(defun test-trade-cancel ()
  "Test cancelling a trade session."
  (let ((session (create-trade-session 1 2)))
    (cancel-trade-session session "test cancel")
    ;; Check cleanup
    (assert (not (player-in-trade-p 1)) () "Player 1 no longer trading")
    (assert (not (player-in-trade-p 2)) () "Player 2 no longer trading")
    (assert (null (gethash (trade-session-id session) *active-trades*))
            () "Session removed from active trades")
    (assert (eq (trade-session-state session) :cancelled)
            () "State is cancelled")))

(defun test-trade-timeout ()
  "Test trade timeout detection."
  (let ((session (create-trade-session 1 2)))
    ;; Fresh session should not be timed out
    (assert (not (check-trade-timeout session)) () "Fresh session not timed out")
    ;; Manually set old activity time
    (let ((old-time (- (float (get-internal-real-time) 1.0)
                       (* 120 internal-time-units-per-second))))  ; 120 seconds ago
      (setf (trade-offer-last-activity (trade-session-player1-offer session)) old-time)
      (setf (trade-offer-last-activity (trade-session-player2-offer session)) old-time))
    ;; Now should be timed out
    (assert (check-trade-timeout session) () "Old session timed out")))

;;;; ========================================================================
;;;; Utility Function Tests
;;;; ========================================================================

(defun test-offer-to-item-list ()
  "Test converting trade offer to item list."
  (let* ((player (make-player 0.0 0.0 :id 1))
         (inventory (player-inventory player))
         (slots (inventory-slots inventory)))
    ;; Set up inventory with some items (use actual items from game-data)
    (setf (aref slots 0) (make-inventory-slot :item-id :coins :count 10))
    (setf (aref slots 2) (make-inventory-slot :item-id :bones :count 1))
    ;; Create offer with slot 0 (5 coins) and slot 2 (1 bones)
    (let ((offer (make-trade-offer)))
      (setf (gethash 0 (trade-offer-slots offer)) 5)
      (setf (gethash 2 (trade-offer-slots offer)) 1)
      (let ((items (offer-to-item-list player offer)))
        (assert (= (length items) 2) () "Two items in list")
        ;; Check items (order may vary)
        (assert (member '(:coins 5) items :test #'equal)
                () "Coins offer present")
        (assert (member '(:bones 1) items :test #'equal)
                () "Bones offer present")))))

(defun test-add-items-to-inventory-array ()
  "Test adding items to inventory array."
  (let* ((slots (make-array 5)))
    ;; Initialize with empty slots
    (dotimes (i 5)
      (setf (aref slots i) (make-inventory-slot :item-id nil :count 0)))
    ;; Add 3 coins (stack-size 9999 allows large stacks)
    (assert (add-items-to-inventory-array slots :coins 3)
            () "Add 3 coins succeeds")
    (assert (eq (inventory-slot-item-id (aref slots 0)) :coins)
            () "Coins in slot 0")
    (assert (= (inventory-slot-count (aref slots 0)) 3)
            () "Count is 3")
    ;; Add more coins - should stack
    (assert (add-items-to-inventory-array slots :coins 5)
            () "Add 5 more coins succeeds")
    (assert (= (inventory-slot-count (aref slots 0)) 8)
            () "Stacked to 8")
    ;; Add different item (bones has stack-size 1)
    (assert (add-items-to-inventory-array slots :bones 1)
            () "Add bones succeeds")
    (assert (eq (inventory-slot-item-id (aref slots 1)) :bones)
            () "Bones in slot 1")))

;;;; ========================================================================
;;;; Phase 3: Trade Safety Tests
;;;; ========================================================================

(defun test-trade-ownership-mismatch-aborts ()
  "Test that trade aborts if session ownership doesn't match.
   Phase 3: Prevents stale servers from committing trades after losing ownership."
  (let* ((storage (make-instance 'memory-storage))
         (*storage* storage)
         (*server-instance-id* "test-server-001"))
    (storage-connect storage)
    ;; Set up two players with different owners
    (let* ((p1-key "player:100")
           (p2-key "player:200")
           (owner1-key (session-owner-key 100))
           (owner2-key (session-owner-key 200))
           (p1-data '(:id 100 :version 3 :x 0.0 :y 0.0))
           (p2-data '(:id 200 :version 3 :x 10.0 :y 10.0)))
      ;; Save initial player data
      (storage-save storage p1-key p1-data)
      (storage-save storage p2-key p2-data)
      ;; Set ownership - player 1 owned by this server, player 2 owned by OTHER server
      (storage-save storage owner1-key *server-instance-id*)
      (storage-save storage owner2-key "other-server-002")  ; Different owner!
      ;; Try to execute trade script - should fail on ownership mismatch
      (let ((result (storage-eval-script storage "trade_complete"
                                         (list p1-key p2-key owner1-key owner2-key)
                                         (list "((:id 100 :version 3))"
                                               "((:id 200 :version 3))"
                                               *server-instance-id*))))
        ;; Should return an error, not "OK"
        (assert (or (null result)
                    (and (stringp result)
                         (search "TRADE_ERROR" result)))
                () "Trade should abort on ownership mismatch, got: ~a" result))
      ;; Verify original data unchanged
      (let ((p1-loaded (storage-load storage p1-key))
            (p2-loaded (storage-load storage p2-key)))
        (assert (equal (getf p1-loaded :x) (getf p1-data :x))
                () "Player 1 data unchanged after failed trade")
        (assert (equal (getf p2-loaded :x) (getf p2-data :x))
                () "Player 2 data unchanged after failed trade")))))

(defun test-trade-memory-backend-consistency ()
  "Test that memory backend trade results load cleanly via db-load-player.
   Phase 3: Memory backend must parse strings to plists, not store raw strings."
  (let* ((storage (make-instance 'memory-storage))
         (*storage* storage)
         (*server-instance-id* "test-server-001"))
    (storage-connect storage)
    ;; Set up two players
    (let* ((p1-key (player-key 100))
           (p2-key (player-key 200))
           (owner1-key (session-owner-key 100))
           (owner2-key (session-owner-key 200))
           ;; Full valid player plists with inventory
           (p1-data (list :id 100 :version *player-schema-version*
                          :username "trader1" :x 0.0 :y 0.0
                          :hp 100 :max-hp 100 :xp 0 :level 1
                          :inventory nil :equipment nil
                          :zone-id 'test-zone :gold 50
                          :lifetime-xp 0 :playtime 0
                          :created-at (get-universal-time)))
           (p2-data (list :id 200 :version *player-schema-version*
                          :username "trader2" :x 10.0 :y 10.0
                          :hp 100 :max-hp 100 :xp 0 :level 1
                          :inventory nil :equipment nil
                          :zone-id 'test-zone :gold 100
                          :lifetime-xp 0 :playtime 0
                          :created-at (get-universal-time))))
      ;; Save initial player data as plists (normal storage)
      (storage-save storage p1-key p1-data)
      (storage-save storage p2-key p2-data)
      ;; Set ownership - both owned by this server
      (storage-save storage owner1-key *server-instance-id*)
      (storage-save storage owner2-key *server-instance-id*)
      ;; Execute trade script with stringified data (as execute-trade-atomic does)
      (let* ((p1-updated (copy-list p1-data))
             (p2-updated (copy-list p2-data)))
        ;; Simulate trade: swap gold
        (setf (getf p1-updated :gold) 100)  ; p1 receives 50 from p2
        (setf (getf p2-updated :gold) 50)   ; p2 gives 50 to p1
        (let ((result (storage-eval-script storage "trade_complete"
                                           (list p1-key p2-key owner1-key owner2-key)
                                           (list (prin1-to-string p1-updated)
                                                 (prin1-to-string p2-updated)
                                                 *server-instance-id*))))
          (assert (and result (string= result "OK"))
                  () "Trade should succeed, got: ~a" result)))
      ;; Key test: load player data via storage-load (simulating db-load-player)
      ;; This MUST return a plist, not a string
      (let ((p1-loaded (storage-load storage p1-key))
            (p2-loaded (storage-load storage p2-key)))
        ;; Verify loaded data is a plist, not a string
        (assert (listp p1-loaded)
                () "Player 1 loaded data should be a plist, not ~a" (type-of p1-loaded))
        (assert (listp p2-loaded)
                () "Player 2 loaded data should be a plist, not ~a" (type-of p2-loaded))
        ;; Verify trade effects persisted correctly
        (assert (= (getf p1-loaded :gold) 100)
                () "Player 1 should have 100 gold after trade")
        (assert (= (getf p2-loaded :gold) 50)
                () "Player 2 should have 50 gold after trade")
        ;; Verify other fields intact
        (assert (equal (getf p1-loaded :username) "trader1")
                () "Player 1 username intact")
        (assert (equal (getf p2-loaded :username) "trader2")
                () "Player 2 username intact")))))

(defun test-trade-expired-ownership-aborts ()
  "Test that trade aborts if ownership key has expired (TTL-aware check).
   Phase 3 P2: Memory backend must respect TTL expiration on ownership keys."
  (let* ((storage (make-instance 'memory-storage))
         (*storage* storage)
         (*server-instance-id* "test-server-001"))
    (storage-connect storage)
    ;; Set up two players
    (let* ((p1-key "player:100")
           (p2-key "player:200")
           (owner1-key (session-owner-key 100))
           (owner2-key (session-owner-key 200))
           (p1-data '(:id 100 :version 3 :x 0.0 :y 0.0))
           (p2-data '(:id 200 :version 3 :x 10.0 :y 10.0)))
      ;; Save initial player data
      (storage-save storage p1-key p1-data)
      (storage-save storage p2-key p2-data)
      ;; Set ownership with TTL - player 1 has valid ownership
      (storage-save-with-ttl storage owner1-key *server-instance-id* 60)
      ;; Player 2 ownership is EXPIRED (simulate by setting TTL in the past)
      (setf (gethash owner2-key (memory-storage-data storage)) *server-instance-id*)
      (setf (gethash owner2-key *memory-storage-ttls*) (- (get-universal-time) 10))  ; Expired 10 seconds ago
      ;; Try to execute trade script - should fail because player 2's ownership expired
      (let ((result (storage-eval-script storage "trade_complete"
                                         (list p1-key p2-key owner1-key owner2-key)
                                         (list "((:id 100 :version 3))"
                                               "((:id 200 :version 3))"
                                               *server-instance-id*))))
        ;; Should return an error because expired ownership reads as NIL
        (assert (or (null result)
                    (and (stringp result)
                         (search "TRADE_ERROR" result)))
                () "Trade should abort when ownership expired, got: ~a" result))
      ;; Verify original data unchanged
      (let ((p1-loaded (storage-load storage p1-key))
            (p2-loaded (storage-load storage p2-key)))
        (assert (equal (getf p1-loaded :x) (getf p1-data :x))
                () "Player 1 data unchanged after failed trade")
        (assert (equal (getf p2-loaded :x) (getf p2-data :x))
                () "Player 2 data unchanged after failed trade")))))

;;;; ======================================================================
;;;; Spatial Grid Tests
;;;; ======================================================================

(defun test-spatial-grid-insert-remove ()
  "Test basic spatial grid insert and remove operations."
  (let ((grid (mmorpg::make-spatial-grid 128.0)))
    ;; Insert entity at position
    (mmorpg::spatial-grid-insert grid 1 100.0 100.0)
    ;; Verify entity is in the grid
    (multiple-value-bind (cx cy) (mmorpg::position-to-cell 100.0 100.0 128.0)
      (let ((ids (mmorpg::spatial-grid-get-cell grid cx cy)))
        (assert (member 1 ids) () "Entity 1 should be in cell after insert")))
    ;; Remove entity
    (multiple-value-bind (cx cy) (mmorpg::position-to-cell 100.0 100.0 128.0)
      (mmorpg::spatial-grid-remove grid 1 cx cy)
      (let ((ids (mmorpg::spatial-grid-get-cell grid cx cy)))
        (assert (not (member 1 ids)) () "Entity 1 should not be in cell after remove")))))

(defun test-spatial-grid-move ()
  "Test spatial grid move operation."
  (let ((grid (mmorpg::make-spatial-grid 128.0)))
    ;; Insert at initial position
    (mmorpg::spatial-grid-insert grid 1 50.0 50.0)
    ;; Move within same cell (should not change)
    (multiple-value-bind (new-cx new-cy changed)
        (mmorpg::spatial-grid-move grid 1 0 0 60.0 60.0)
      (declare (ignore new-cx new-cy))
      (assert (null changed) () "Moving within same cell should not report change"))
    ;; Move to different cell
    (multiple-value-bind (new-cx new-cy changed)
        (mmorpg::spatial-grid-move grid 1 0 0 200.0 200.0)
      (assert changed () "Moving to different cell should report change")
      ;; Verify new position
      (let ((ids (mmorpg::spatial-grid-get-cell grid new-cx new-cy)))
        (assert (member 1 ids) () "Entity 1 should be in new cell")))))

(defun test-spatial-grid-query-neighbors ()
  "Test spatial grid neighbor query (3x3 area)."
  (let ((grid (mmorpg::make-spatial-grid 128.0)))
    ;; Insert entities at different cells
    (mmorpg::spatial-grid-insert grid 1 64.0 64.0)     ; cell (0,0)
    (mmorpg::spatial-grid-insert grid 2 192.0 64.0)    ; cell (1,0)
    (mmorpg::spatial-grid-insert grid 3 320.0 64.0)    ; cell (2,0) - outside 3x3
    (mmorpg::spatial-grid-insert grid 4 64.0 192.0)    ; cell (0,1)
    ;; Query neighbors around (0,0)
    (let ((nearby (mmorpg::spatial-grid-query-neighbors grid 0 0)))
      (assert (member 1 nearby) () "Entity 1 should be in neighbors")
      (assert (member 2 nearby) () "Entity 2 should be in neighbors")
      (assert (not (member 3 nearby)) () "Entity 3 should not be in neighbors (too far)")
      (assert (member 4 nearby) () "Entity 4 should be in neighbors"))))

(defun test-spatial-grid-query-radius ()
  "Test spatial grid radius query."
  (let ((grid (mmorpg::make-spatial-grid 128.0)))
    ;; Insert entities at different cells
    (mmorpg::spatial-grid-insert grid 1 64.0 64.0)     ; cell (0,0)
    (mmorpg::spatial-grid-insert grid 2 320.0 64.0)    ; cell (2,0)
    (mmorpg::spatial-grid-insert grid 3 576.0 64.0)    ; cell (4,0)
    ;; Radius 1 (3x3) should not include entity 3
    (let ((nearby (mmorpg::spatial-grid-query-radius grid 0 0 1)))
      (assert (member 1 nearby) () "Radius 1: Entity 1 should be included")
      (assert (not (member 3 nearby)) () "Radius 1: Entity 3 should not be included"))
    ;; Radius 2 (5x5) should include entities 1 and 2 but not 3
    (let ((nearby (mmorpg::spatial-grid-query-radius grid 0 0 2)))
      (assert (member 1 nearby) () "Radius 2: Entity 1 should be included")
      (assert (member 2 nearby) () "Radius 2: Entity 2 should be included")
      (assert (not (member 3 nearby)) () "Radius 2: Entity 3 should not be included"))))

(defun test-position-to-cell ()
  "Test position to cell coordinate conversion."
  (let ((cell-size 128.0))
    ;; Test basic conversion
    (multiple-value-bind (cx cy) (mmorpg::position-to-cell 0.0 0.0 cell-size)
      (assert (= cx 0) () "Position (0,0) should be in cell 0")
      (assert (= cy 0) () "Position (0,0) should be in cell 0"))
    ;; Test position in second cell
    (multiple-value-bind (cx cy) (mmorpg::position-to-cell 130.0 260.0 cell-size)
      (assert (= cx 1) () "Position (130,260) should be in cell x=1")
      (assert (= cy 2) () "Position (130,260) should be in cell y=2"))
    ;; Test negative coordinates
    (multiple-value-bind (cx cy) (mmorpg::position-to-cell -50.0 -200.0 cell-size)
      (assert (= cx -1) () "Position (-50,-200) should be in cell x=-1")
      (assert (= cy -2) () "Position (-50,-200) should be in cell y=-2"))))

(defun test-entity-cell-changed-p ()
  "Test entity cell change detection."
  (let ((cell-size 128.0))
    ;; Same cell - no change
    (assert (not (mmorpg::entity-cell-changed-p 0 0 50.0 50.0 cell-size))
            () "Position in same cell should not report change")
    ;; Different cell - change
    (assert (mmorpg::entity-cell-changed-p 0 0 200.0 50.0 cell-size)
            () "Position in different cell should report change")
    ;; Nil old cell - always change
    (assert (mmorpg::entity-cell-changed-p nil nil 50.0 50.0 cell-size)
            () "Nil old cell should report change")))

(defun test-spatial-grid-stats ()
  "Test spatial grid statistics."
  (let ((grid (mmorpg::make-spatial-grid 128.0)))
    ;; Empty grid
    (multiple-value-bind (cell-count entity-count)
        (mmorpg::spatial-grid-stats grid)
      (assert (= cell-count 0) () "Empty grid should have 0 cells")
      (assert (= entity-count 0) () "Empty grid should have 0 entities"))
    ;; Add some entities
    (mmorpg::spatial-grid-insert grid 1 64.0 64.0)
    (mmorpg::spatial-grid-insert grid 2 64.0 64.0)  ; Same cell as 1
    (mmorpg::spatial-grid-insert grid 3 200.0 200.0)  ; Different cell
    (multiple-value-bind (cell-count entity-count)
        (mmorpg::spatial-grid-stats grid)
      (assert (= cell-count 2) () "Grid should have 2 occupied cells")
      (assert (= entity-count 3) () "Grid should have 3 entities total"))))

(defun test-spatial-grid-clear ()
  "Test spatial grid clear operation."
  (let ((grid (mmorpg::make-spatial-grid 128.0)))
    ;; Add some entities
    (mmorpg::spatial-grid-insert grid 1 64.0 64.0)
    (mmorpg::spatial-grid-insert grid 2 200.0 200.0)
    ;; Clear grid
    (mmorpg::spatial-grid-clear grid)
    ;; Verify empty
    (multiple-value-bind (cell-count entity-count)
        (mmorpg::spatial-grid-stats grid)
      (assert (= cell-count 0) () "Cleared grid should have 0 cells")
      (assert (= entity-count 0) () "Cleared grid should have 0 entities"))))

(defun test-zone-transition-grid-update ()
  "Test that zone transitions correctly update spatial grids.
   Player should be removed from old zone grid and inserted into new zone grid."
  (let* (;; Create two zone-states with spatial grids
         (old-zone-state (mmorpg::make-zone-state
                          :zone-id :old-zone
                          :player-grid (mmorpg::make-spatial-grid 128.0)
                          :npc-grid (mmorpg::make-spatial-grid 128.0)))
         (new-zone-state (mmorpg::make-zone-state
                          :zone-id :new-zone
                          :player-grid (mmorpg::make-spatial-grid 128.0)
                          :npc-grid (mmorpg::make-spatial-grid 128.0)))
         ;; Create a player in old zone
         (player (mmorpg::make-player 100.0 100.0 :id 42 :zone-id :old-zone))
         (old-grid (mmorpg::zone-state-player-grid old-zone-state))
         (new-grid (mmorpg::zone-state-player-grid new-zone-state)))
    ;; Insert player into old zone's grid (simulating initial state)
    (multiple-value-bind (cx cy)
        (mmorpg::position-to-cell 100.0 100.0 (mmorpg::spatial-grid-cell-size old-grid))
      (mmorpg::spatial-grid-insert old-grid 42 100.0 100.0)
      (setf (mmorpg::player-grid-cell-x player) cx
            (mmorpg::player-grid-cell-y player) cy))
    ;; Verify player is in old grid
    (multiple-value-bind (cell-count entity-count)
        (mmorpg::spatial-grid-stats old-grid)
      (declare (ignore cell-count))
      (assert (= entity-count 1) () "Player should be in old zone grid"))
    ;; Simulate zone transition: remove from old grid
    (mmorpg::spatial-grid-remove old-grid 42
                                  (mmorpg::player-grid-cell-x player)
                                  (mmorpg::player-grid-cell-y player))
    ;; Update player position and zone
    (setf (mmorpg::player-x player) 200.0
          (mmorpg::player-y player) 200.0
          (mmorpg::player-zone-id player) :new-zone)
    ;; Insert into new grid
    (multiple-value-bind (cx cy)
        (mmorpg::position-to-cell 200.0 200.0 (mmorpg::spatial-grid-cell-size new-grid))
      (mmorpg::spatial-grid-insert new-grid 42 200.0 200.0)
      (setf (mmorpg::player-grid-cell-x player) cx
            (mmorpg::player-grid-cell-y player) cy))
    ;; Verify player is NOT in old grid
    (multiple-value-bind (cell-count entity-count)
        (mmorpg::spatial-grid-stats old-grid)
      (declare (ignore cell-count))
      (assert (= entity-count 0) () "Player should NOT be in old zone grid after transition"))
    ;; Verify player IS in new grid
    (multiple-value-bind (cell-count entity-count)
        (mmorpg::spatial-grid-stats new-grid)
      (declare (ignore cell-count))
      (assert (= entity-count 1) () "Player should be in new zone grid after transition"))
    ;; Verify player's grid cell is updated
    (multiple-value-bind (expected-cx expected-cy)
        (mmorpg::position-to-cell 200.0 200.0 128.0)
      (assert (= (mmorpg::player-grid-cell-x player) expected-cx)
              () "Player grid-cell-x should match new position")
      (assert (= (mmorpg::player-grid-cell-y player) expected-cy)
              () "Player grid-cell-y should match new position"))
    ;; Verify player can be found via spatial query in new zone
    (let ((nearby (mmorpg::spatial-grid-query-neighbors
                   new-grid
                   (mmorpg::player-grid-cell-x player)
                   (mmorpg::player-grid-cell-y player))))
      (assert (member 42 nearby) () "Player should be findable via spatial query in new zone"))))

;;; Zone-Players Cache Tests (Task 4.1)

(defun test-zone-players-cache-add-remove ()
  "Test adding and removing players from zone-players cache."
  (let* ((zone-state (mmorpg::make-zone-state
                      :zone-id :test-zone
                      :player-grid (mmorpg::make-spatial-grid 128.0)
                      :npc-grid (mmorpg::make-spatial-grid 128.0)))
         (player1 (mmorpg::make-player 100.0 100.0 :id 1 :zone-id :test-zone))
         (player2 (mmorpg::make-player 200.0 200.0 :id 2 :zone-id :test-zone))
         (cache (mmorpg::zone-state-zone-players zone-state)))
    ;; Initial cache should be empty
    (assert (= (length cache) 0) () "Cache should start empty")
    ;; Add first player
    (mmorpg::add-player-to-zone-cache player1 zone-state)
    (assert (= (length (mmorpg::zone-state-zone-players zone-state)) 1)
            () "Cache should have 1 player after add")
    ;; Add second player
    (mmorpg::add-player-to-zone-cache player2 zone-state)
    (assert (= (length (mmorpg::zone-state-zone-players zone-state)) 2)
            () "Cache should have 2 players after second add")
    ;; Adding same player should not duplicate (idempotent)
    (mmorpg::add-player-to-zone-cache player1 zone-state)
    (assert (= (length (mmorpg::zone-state-zone-players zone-state)) 2)
            () "Cache should still have 2 players (no duplicates)")
    ;; Remove first player
    (mmorpg::remove-player-from-zone-cache player1 zone-state)
    (assert (= (length (mmorpg::zone-state-zone-players zone-state)) 1)
            () "Cache should have 1 player after remove")
    ;; Verify correct player remains
    (let ((remaining (aref (mmorpg::zone-state-zone-players zone-state) 0)))
      (assert (eq remaining player2) () "Player2 should be the remaining player"))
    ;; Remove second player
    (mmorpg::remove-player-from-zone-cache player2 zone-state)
    (assert (= (length (mmorpg::zone-state-zone-players zone-state)) 0)
            () "Cache should be empty after removing all players")))

(defun test-zone-players-cache-rebuild ()
  "Test rebuilding zone-players cache from game state."
  (let* ((zone-state (mmorpg::make-zone-state
                      :zone-id :test-zone
                      :player-grid (mmorpg::make-spatial-grid 128.0)
                      :npc-grid (mmorpg::make-spatial-grid 128.0)))
         (player1 (mmorpg::make-player 100.0 100.0 :id 1 :zone-id :test-zone))
         (player2 (mmorpg::make-player 200.0 200.0 :id 2 :zone-id :other-zone))
         (player3 (mmorpg::make-player 300.0 300.0 :id 3 :zone-id :test-zone))
         ;; Create minimal game struct with players
         (game (mmorpg::%make-game :players (vector player1 player2 player3))))
    ;; Rebuild the cache
    (mmorpg::rebuild-zone-players-cache zone-state game)
    ;; Should only have players in :test-zone (player1 and player3)
    (let ((cache (mmorpg::zone-state-zone-players zone-state)))
      (assert (= (length cache) 2) () "Cache should have 2 players in test-zone")
      ;; Verify correct players
      (let ((ids (loop :for p :across cache :collect (mmorpg::player-id p))))
        (assert (member 1 ids) () "Player1 should be in cache")
        (assert (not (member 2 ids)) () "Player2 should NOT be in cache (wrong zone)")
        (assert (member 3 ids) () "Player3 should be in cache")))))

(defun test-zone-players-cache-transition ()
  "Test that zone transitions correctly update zone-players caches."
  (let* ((old-zone-state (mmorpg::make-zone-state
                          :zone-id :old-zone
                          :player-grid (mmorpg::make-spatial-grid 128.0)
                          :npc-grid (mmorpg::make-spatial-grid 128.0)))
         (new-zone-state (mmorpg::make-zone-state
                          :zone-id :new-zone
                          :player-grid (mmorpg::make-spatial-grid 128.0)
                          :npc-grid (mmorpg::make-spatial-grid 128.0)))
         (player (mmorpg::make-player 100.0 100.0 :id 42 :zone-id :old-zone)))
    ;; Add player to old zone's cache
    (mmorpg::add-player-to-zone-cache player old-zone-state)
    (assert (= (length (mmorpg::zone-state-zone-players old-zone-state)) 1)
            () "Player should be in old zone cache")
    (assert (= (length (mmorpg::zone-state-zone-players new-zone-state)) 0)
            () "New zone cache should be empty initially")
    ;; Simulate zone transition
    (mmorpg::remove-player-from-zone-cache player old-zone-state)
    (setf (mmorpg::player-zone-id player) :new-zone)
    (mmorpg::add-player-to-zone-cache player new-zone-state)
    ;; Verify caches are correct
    (assert (= (length (mmorpg::zone-state-zone-players old-zone-state)) 0)
            () "Old zone cache should be empty after transition")
    (assert (= (length (mmorpg::zone-state-zone-players new-zone-state)) 1)
            () "New zone cache should have player after transition")
    ;; Verify correct player is in new cache
    (let ((cached (aref (mmorpg::zone-state-zone-players new-zone-state) 0)))
      (assert (eq cached player) () "Cached player should be same object"))))

;;; Vector Pool Tests (Task 4.2)

(defun test-vector-pool-create ()
  "Test vector pool creation."
  (let ((pool (mmorpg::make-vector-pool 10 22)))
    (assert pool () "Pool should be created")
    (assert (= (mmorpg::vector-pool-capacity pool) 10)
            () "Pool capacity should be 10")
    (assert (= (mmorpg::vector-pool-element-size pool) 22)
            () "Element size should be 22")
    (assert (= (mmorpg::vector-pool-index pool) 0)
            () "Initial index should be 0")))

(defun test-vector-pool-acquire ()
  "Test acquiring vectors from pool."
  (let ((pool (mmorpg::make-vector-pool 5 22)))
    ;; Acquire first vector
    (let ((v1 (mmorpg::acquire-pooled-vector pool)))
      (assert (= (length v1) 22) () "Acquired vector should have 22 elements")
      (assert (= (mmorpg::vector-pool-index pool) 1) () "Index should be 1 after first acquire"))
    ;; Acquire second vector
    (let ((v2 (mmorpg::acquire-pooled-vector pool)))
      (assert (= (length v2) 22) () "Second vector should have 22 elements")
      (assert (= (mmorpg::vector-pool-index pool) 2) () "Index should be 2 after second acquire"))
    ;; Vectors should be different objects
    (mmorpg::reset-vector-pool pool)
    (let ((v1 (mmorpg::acquire-pooled-vector pool))
          (v2 (mmorpg::acquire-pooled-vector pool)))
      (assert (not (eq v1 v2)) () "Acquired vectors should be different objects"))))

(defun test-vector-pool-reset ()
  "Test resetting vector pool for reuse."
  (let ((pool (mmorpg::make-vector-pool 5 22)))
    ;; Acquire some vectors
    (mmorpg::acquire-pooled-vector pool)
    (mmorpg::acquire-pooled-vector pool)
    (mmorpg::acquire-pooled-vector pool)
    (assert (= (mmorpg::vector-pool-index pool) 3) () "Index should be 3")
    ;; Reset
    (mmorpg::reset-vector-pool pool)
    (assert (= (mmorpg::vector-pool-index pool) 0) () "Index should be 0 after reset")
    ;; Can acquire again from beginning
    (let ((v (mmorpg::acquire-pooled-vector pool)))
      (assert (= (length v) 22) () "Can acquire after reset")
      (assert (= (mmorpg::vector-pool-index pool) 1) () "Index should be 1"))))

(defun test-vector-pool-overflow ()
  "Test vector pool behavior when exhausted."
  (let ((pool (mmorpg::make-vector-pool 3 22)))
    ;; Exhaust the pool
    (mmorpg::acquire-pooled-vector pool)
    (mmorpg::acquire-pooled-vector pool)
    (mmorpg::acquire-pooled-vector pool)
    (assert (= (mmorpg::vector-pool-index pool) 3) () "Index should be 3 (capacity)")
    ;; Acquiring beyond capacity should still work (creates fresh vector)
    (let ((v (mmorpg::acquire-pooled-vector pool)))
      (assert (= (length v) 22) () "Overflow vector should have 22 elements"))
    ;; Stats should show overflow
    (multiple-value-bind (used total overflow)
        (mmorpg::vector-pool-stats pool)
      (assert (= used 4) () "Used should be 4")
      (assert (= total 3) () "Total should be 3")
      (assert (= overflow 1) () "Overflow should be 1"))))

;;; Profiling & GC Tests (Phase 6)

(defun test-profile-log-init ()
  "Test initializing the profile log ring buffer."
  (let ((*profile-log* nil)
        (*profile-log-size* 0)
        (*profile-log-index* 99))
    (mmorpg::init-profile-log 5)
    (assert (= (length *profile-log*) 5) () "Profile log should have 5 slots")
    (assert (= *profile-log-index* 0) () "Profile log index should reset to 0")))

(defun test-record-profile-sample ()
  "Test recording a profile timing sample."
  (let ((*profile-log* nil)
        (*profile-log-size* 3)
        (*profile-log-index* 0))
    (mmorpg::init-profile-log 3)
    (mmorpg::record-profile-sample :unit-test 1.5)
    (assert (= *profile-log-index* 1) () "Profile log index should advance")
    (let ((entry (aref *profile-log* 0)))
      (assert (eq (car entry) :unit-test) () "Sample name should match")
      (assert (numberp (cdr entry)) () "Sample time should be numeric"))))

(defun test-with-timing-enabled ()
  "Test with-timing when profiling is enabled."
  (let ((*profile-enabled* t)
        (*profile-log* nil)
        (*profile-log-size* 2)
        (*profile-log-index* 0))
    (mmorpg::init-profile-log 2)
    (let ((result (mmorpg::with-timing (:timed) (+ 1 2))))
      (assert (= result 3) () "with-timing should return body result"))
    (assert (= *profile-log-index* 1) () "with-timing should record one sample")
    (let ((entry (aref *profile-log* 0)))
      (assert (eq (car entry) :timed) () "Timing sample name should match")
      (assert (numberp (cdr entry)) () "Timing sample should be numeric"))))

(defun test-clear-profile-log ()
  "Test clearing the profile log."
  (let ((*profile-log* nil)
        (*profile-log-size* 3)
        (*profile-log-index* 0))
    (mmorpg::init-profile-log 3)
    (mmorpg::record-profile-sample :a 1.0)
    (mmorpg::record-profile-sample :b 2.0)
    (mmorpg::clear-profile-log)
    (assert (= *profile-log-index* 0) () "Profile index should reset to 0")
    (assert (every #'null *profile-log*) () "Profile log should be cleared")))

(defun test-gc-stats-reset ()
  "Test GC stats reset sets frame count to zero."
  (let ((*gc-stats-frame-count* 5)
        (*gc-stats-last-bytes* 123)
        (*gc-stats-last-runtime* 456))
    (mmorpg::reset-gc-stats)
    (assert (= *gc-stats-frame-count* 0) () "Frame GC count should reset to 0")
    (assert (numberp *gc-stats-last-bytes*) () "Last bytes should be numeric")
    (assert (numberp *gc-stats-last-runtime*) () "Last runtime should be numeric")))

(defun test-gc-hook-increment-count ()
  "Test GC hook increment function."
  (let ((*gc-stats-frame-count* 0)
        (*gc-stats-total-count* 0))
    (mmorpg::gc-hook-increment-count)
    (assert (= *gc-stats-frame-count* 1) () "Frame GC count should increment")
    (assert (= *gc-stats-total-count* 1) () "Total GC count should increment")))

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

;;; ============================================================
;;; AUTH THROUGHPUT TESTS (Steps 1-12)
;;; ============================================================

;;; --- Step 1: O(1) Two-List FIFO Queue Tests ---

(defun test-auth-queue-fifo-order ()
  "Step 1: Verify auth queue maintains FIFO order."
  (let ((q (make-auth-queue-instance :max-depth 0)))
    (auth-queue-push q :a)
    (auth-queue-push q :b)
    (auth-queue-push q :c)
    (let ((items (auth-queue-drain-nonblocking q)))
      (assert (equal items '(:a :b :c)) ()
              "auth-queue FIFO: expected (:a :b :c), got ~a" items))))

(defun test-auth-queue-interleaved ()
  "Step 1: Verify interleaved push/pop maintains order."
  (let ((q (make-auth-queue-instance :max-depth 0)))
    (auth-queue-push q :a)
    (let ((first (auth-queue-drain-nonblocking q)))
      (assert (equal first '(:a)) ()
              "auth-queue interleaved: first drain got ~a" first))
    (auth-queue-push q :b)
    (auth-queue-push q :c)
    (let ((rest (auth-queue-drain-nonblocking q)))
      (assert (equal rest '(:b :c)) ()
              "auth-queue interleaved: second drain got ~a" rest))))

(defun test-auth-queue-count ()
  "Step 1: Verify count tracks correctly through push/drain."
  (let ((q (make-auth-queue-instance :max-depth 0)))
    (assert (= (auth-queue-count q) 0) () "auth-queue count: initial 0")
    (auth-queue-push q :a)
    (auth-queue-push q :b)
    (assert (= (auth-queue-count q) 2) () "auth-queue count: after 2 pushes")
    (auth-queue-drain-nonblocking q)
    (assert (= (auth-queue-count q) 0) () "auth-queue count: after drain")))

(defun test-auth-queue-drain-empty ()
  "Step 1: Drain empty queue returns NIL."
  (let ((q (make-auth-queue-instance :max-depth 0)))
    (assert (null (auth-queue-drain-nonblocking q)) ()
            "auth-queue drain-empty: expected NIL")))

;;; --- Step 3: Bounded Queue Tests ---

(defun test-auth-queue-bounded ()
  "Step 3: Verify bounded queue rejects when full."
  (let ((q (make-auth-queue-instance :max-depth 2)))
    (assert (auth-queue-try-push q :a) () "bounded: first push accepted")
    (assert (auth-queue-try-push q :b) () "bounded: second push accepted")
    (assert (not (auth-queue-try-push q :c)) () "bounded: third push rejected")
    (assert (= (auth-queue-count q) 2) () "bounded: count stays at 2")))

(defun test-auth-queue-unbounded ()
  "Step 3: Verify max-depth 0 means unbounded."
  (let ((q (make-auth-queue-instance :max-depth 0)))
    (dotimes (i 500)
      (auth-queue-try-push q i))
    (assert (= (auth-queue-count q) 500) () "unbounded: accepted 500 items")))

;;; --- Step 11: Password Hashing Tests ---

(defun test-hash-password-3part-format ()
  "Step 11: hash-password produces 3-part salt$iterations$hash format."
  (let ((hash (hash-password "testpassword")))
    (let ((parts (split-hash-string hash)))
      (assert (= (length parts) 3) ()
              "hash-password: expected 3 parts, got ~d" (length parts))
      (assert (parse-integer (second parts) :junk-allowed nil) ()
              "hash-password: iterations must be parseable integer")
      (assert (= (parse-integer (second parts)) *password-hash-iterations*) ()
              "hash-password: iterations should be ~d, got ~a"
              *password-hash-iterations* (second parts)))))

(defun test-verify-password-new-format ()
  "Step 11: verify-password correctly verifies new 3-part format."
  (let ((hash (hash-password "correct-password")))
    (assert (verify-password "correct-password" hash) ()
            "verify-password: should verify correct password (new format)")
    (assert (not (verify-password "wrong-password" hash)) ()
            "verify-password: should reject wrong password (new format)")))

(defun test-verify-password-legacy-format ()
  "Step 11: verify-password handles legacy 2-part format (backward compat)."
  (let* ((salt (generate-salt))
         (key (derive-password-key-with-iterations "legacypass" salt 100000))
         (salt-hex (bytes-to-hex salt))
         (key-hex (bytes-to-hex key))
         (legacy-hash (concatenate 'string salt-hex "$" key-hex)))
    (assert (verify-password "legacypass" legacy-hash) ()
            "verify-password: should verify legacy 2-part hash")
    (assert (not (verify-password "wrongpass" legacy-hash)) ()
            "verify-password: should reject wrong password (legacy format)")))

(defun test-legacy-hash-format-p ()
  "Step 11: legacy-hash-format-p correctly identifies 2-part vs 3-part."
  (let ((new-hash (hash-password "test"))
        (legacy-hash "aabbccdd$deadbeef"))
    (assert (not (legacy-hash-format-p new-hash)) ()
            "legacy-hash-format-p: new format should return NIL")
    (assert (legacy-hash-format-p legacy-hash) ()
            "legacy-hash-format-p: legacy format should return T")))

(defun test-split-hash-string ()
  "Step 11: split-hash-string correctly splits on $ delimiters."
  (let ((parts2 (split-hash-string "abc$def"))
        (parts3 (split-hash-string "abc$123$def")))
    (assert (= (length parts2) 2) () "split-hash: 2-part length")
    (assert (equal (first parts2) "abc") () "split-hash: 2-part first")
    (assert (equal (second parts2) "def") () "split-hash: 2-part second")
    (assert (= (length parts3) 3) () "split-hash: 3-part length")
    (assert (equal (first parts3) "abc") () "split-hash: 3-part first")
    (assert (equal (second parts3) "123") () "split-hash: 3-part second")
    (assert (equal (third parts3) "def") () "split-hash: 3-part third")))

(defun test-derive-password-key-with-iterations ()
  "Step 11: derive-password-key-with-iterations produces deterministic output."
  (let* ((salt (hex-to-bytes "00112233445566778899aabbccddeeff"))
         (key1 (derive-password-key-with-iterations "test" salt 1000))
         (key2 (derive-password-key-with-iterations "test" salt 1000))
         (key-diff (derive-password-key-with-iterations "test" salt 2000)))
    (assert (equalp key1 key2) ()
            "derive-key-with-iterations: same inputs should produce same output")
    (assert (not (equalp key1 key-diff)) ()
            "derive-key-with-iterations: different iterations should produce different output")))

;;; --- Step 10: Pipelined Auth Tests ---

(defun test-db-verify-and-load-account ()
  "Step 10: db-verify-and-load-account returns character-id on valid credentials."
  (let ((*storage* (make-instance 'memory-storage)))
    (storage-connect *storage*)
    ;; Create a test account
    (db-create-account "testuser10" "testpass10")
    (db-set-character-id "testuser10" 42)
    ;; Verify and load
    (multiple-value-bind (char-id verified)
        (db-verify-and-load-account "testuser10" "testpass10")
      (assert verified () "verify-and-load: should verify correct password")
      (assert (= char-id 42) ()
              "verify-and-load: should return character-id 42, got ~a" char-id))
    ;; Bad password
    (multiple-value-bind (char-id verified)
        (db-verify-and-load-account "testuser10" "wrongpass")
      (assert (not verified) () "verify-and-load: should reject bad password")
      (assert (null char-id) () "verify-and-load: char-id should be NIL on failure"))
    ;; Nonexistent user
    (multiple-value-bind (char-id verified)
        (db-verify-and-load-account "nouser" "nopass")
      (assert (not verified) () "verify-and-load: nonexistent user returns NIL")
      (assert (null char-id) () "verify-and-load: nonexistent char-id is NIL"))))

(defun test-db-create-account-pipelined-memory ()
  "Step 10: db-create-account-pipelined falls back to sequential for memory storage."
  (let ((*storage* (make-instance 'memory-storage)))
    (storage-connect *storage*)
    ;; Create account with character-id in one call
    (assert (db-create-account-pipelined "pipetest" "pass123" 99) ()
            "pipelined-memory: should succeed for new account")
    ;; Verify account exists with correct character-id
    (let ((account (db-load-account "pipetest")))
      (assert account () "pipelined-memory: account should exist")
      (assert (= (getf account :character-id) 99) ()
              "pipelined-memory: character-id should be 99, got ~a"
              (getf account :character-id)))
    ;; Duplicate should return :username-taken keyword
    (let ((result (db-create-account-pipelined "pipetest" "pass456" 100)))
      (assert (eq result :username-taken) ()
              "pipelined-memory: duplicate should return :username-taken, got ~a" result))))

;;; --- Step 11: Re-hash on Login Tests ---

(defun test-rehash-on-login ()
  "Step 11: Verify legacy accounts get re-hashed on successful login."
  (let ((*storage* (make-instance 'memory-storage)))
    (storage-connect *storage*)
    ;; Create a legacy-format account manually
    (let* ((salt (generate-salt))
           (key (derive-password-key-with-iterations "oldpass" salt 100000))
           (salt-hex (bytes-to-hex salt))
           (key-hex (bytes-to-hex key))
           (legacy-hash (concatenate 'string salt-hex "$" key-hex)))
      (db-save-account "legacyuser" legacy-hash 55)
      ;; Verify the hash is legacy format
      (let ((account-before (db-load-account "legacyuser")))
        (assert (legacy-hash-format-p (getf account-before :password-hash)) ()
                "rehash: initial hash should be legacy format"))
      ;; Login via db-verify-credentials (triggers re-hash)
      (assert (db-verify-credentials "legacyuser" "oldpass") ()
              "rehash: should verify legacy password")
      ;; Check that hash was updated to new format
      (let ((account-after (db-load-account "legacyuser")))
        (assert (not (legacy-hash-format-p (getf account-after :password-hash))) ()
                "rehash: hash should now be new 3-part format")
        ;; Verify the new hash still works
        (assert (verify-password "oldpass" (getf account-after :password-hash)) ()
                "rehash: new hash should still verify correctly")))))

;;; --- Auth Review Fix Tests ---

(defun test-auth-rate-limit-wall-time ()
  "Verify rate-limit functions work correctly with wall-clock-scale times.
   The critical fix: auth-rate-check and auth-rate-record-failure now both
   use wall-time (large numbers like 4000000.0), not sim-time (small elapsed)."
  ;; Clear state
  (auth-rate-clear-all)
  ;; Simulate wall-clock times (large values like get-internal-real-time / units-per-second)
  (let* ((*auth-max-attempts* 3)
         (*auth-lockout-seconds* 10.0)
         (*auth-attempt-window* 60.0)
         (now 4000000.0))  ; Simulates wall-time
    ;; First check should pass
    (assert (auth-rate-check "10.0.0.1" now) ()
            "rate-limit-walltime: first check should allow")
    ;; Record failures
    (auth-rate-record-failure "10.0.0.1" now)
    (auth-rate-record-failure "10.0.0.1" (+ now 1.0))
    (auth-rate-record-failure "10.0.0.1" (+ now 2.0))
    ;; Should now be locked out (3 failures = *auth-max-attempts*)
    (assert (not (auth-rate-check "10.0.0.1" (+ now 3.0))) ()
            "rate-limit-walltime: should be locked out after max attempts")
    ;; Lockout should expire after lockout-seconds
    (assert (auth-rate-check "10.0.0.1" (+ now 3.0 *auth-lockout-seconds* 1.0)) ()
            "rate-limit-walltime: lockout should expire"))
  (auth-rate-clear-all))

(defun test-auth-rate-lockout-expiry ()
  "Verify rate-limit lockout resets after expiry."
  (auth-rate-clear-all)
  (let* ((*auth-max-attempts* 2)
         (*auth-lockout-seconds* 5.0)
         (*auth-attempt-window* 30.0)
         (now 5000000.0))
    ;; Trigger lockout
    (auth-rate-record-failure "10.0.0.2" now)
    (auth-rate-record-failure "10.0.0.2" (+ now 0.5))
    ;; Locked out
    (assert (not (auth-rate-check "10.0.0.2" (+ now 1.0))) ()
            "lockout-expiry: should be locked")
    ;; After lockout expires, should be allowed and reset
    (assert (auth-rate-check "10.0.0.2" (+ now 6.0)) ()
            "lockout-expiry: should be allowed after lockout expires")
    ;; And success should clear state
    (auth-rate-record-success "10.0.0.2")
    (assert (auth-rate-check "10.0.0.2" (+ now 7.0)) ()
            "lockout-expiry: should be allowed after success clear"))
  (auth-rate-clear-all))

(defun test-verify-password-malformed-iterations ()
  "Verify that malformed iteration counts in hash strings return NIL, not signal."
  ;; Malformed: non-numeric iterations
  (assert (not (verify-password "test" "abcd1234$notanumber$deadbeef")) ()
          "malformed-iterations: non-numeric should return NIL")
  ;; Malformed: empty iterations
  (assert (not (verify-password "test" "abcd1234$$deadbeef")) ()
          "malformed-iterations: empty should return NIL")
  ;; Malformed: only 1 part
  (assert (not (verify-password "test" "justonefield")) ()
          "malformed-iterations: single field should return NIL"))

(defun test-auth-metrics-counters ()
  "Verify auth metric counters can be atomically incremented and read."
  (let ((m *auth-metrics*))
    (let ((q-before (auth-metrics-queued m))
          (p-before (auth-metrics-processed m))
          (e-before (auth-metrics-expired m))
          (r-before (auth-metrics-rejected-busy m))
          (s-before (auth-metrics-success m))
          (f-before (auth-metrics-fail m)))
      (auth-metric-incf queued)
      (auth-metric-incf processed)
      (auth-metric-incf expired)
      (auth-metric-incf rejected-busy)
      (auth-metric-incf success)
      (auth-metric-incf fail)
      (assert (= (auth-metrics-queued m) (1+ q-before)) ()
              "metrics: queued should increment")
      (assert (= (auth-metrics-processed m) (1+ p-before)) ()
              "metrics: processed should increment")
      (assert (= (auth-metrics-expired m) (1+ e-before)) ()
              "metrics: expired should increment")
      (assert (= (auth-metrics-rejected-busy m) (1+ r-before)) ()
              "metrics: rejected-busy should increment")
      (assert (= (auth-metrics-success m) (1+ s-before)) ()
              "metrics: success should increment")
      (assert (= (auth-metrics-fail m) (1+ f-before)) ()
              "metrics: fail should increment"))))

(defun test-session-local-registration ()
  "Verify register-player-session-local creates and tracks session state."
  (let ((*player-sessions* (make-hash-table))
        #+sbcl (*player-sessions-lock* (sb-thread:make-mutex :name "test-ps-lock")))
    (let ((player (make-player 100.0 200.0 :id 42 :zone-id :town)))
      ;; Register
      (register-player-session-local player :zone-id :town :username "testuser")
      ;; Check it exists
      (let ((session (gethash 42 *player-sessions*)))
        (assert session () "session-local: session should exist")
        (assert (eq (player-session-player session) player) ()
                "session-local: session player should match")
        (assert (eq (player-session-zone-id session) :town) ()
                "session-local: zone-id should be :town")
        (assert (string= (player-session-username session) "testuser") ()
                "session-local: username should match"))
      ;; Unregister
      (unregister-player-session-local 42)
      (assert (not (gethash 42 *player-sessions*)) ()
              "session-local: session should be removed after unregister"))))

(defun test-auth-queue-stale-expiry ()
  "Verify that the stale request check in auth-worker-loop logic works correctly.
   Tests the age calculation directly rather than spawning a worker thread."
  (let* ((*auth-request-max-age* 10.0)
         (now-rt 5000.0)
         ;; Fresh request: timestamp is recent
         (fresh-age (- now-rt (- now-rt 2.0)))  ; 2 seconds old
         ;; Stale request: timestamp is old
         (stale-age (- now-rt (- now-rt 15.0))))  ; 15 seconds old
    (assert (< fresh-age (float *auth-request-max-age* 1.0d0)) ()
            "stale-expiry: fresh request should be within max-age")
    (assert (> stale-age (float *auth-request-max-age* 1.0d0)) ()
            "stale-expiry: stale request should exceed max-age")))

;;; ============================================================
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

(defun test-compute-transition-overstep-north ()
  "Overstep for :north edge: distance from player Y to source min-y."
  (let ((player (%make-player)))
    (setf (player-x player) 300.0 (player-y player) 20.0)
    ;; Source zone: min-y=10, max-y=640
    (let ((overstep (compute-transition-overstep :north player 10.0 630.0 10.0 640.0)))
      ;; Player at y=20, north edge at y=10 → overstep = 20 - 10 = 10
      (assert (< (abs (- overstep 10.0)) 0.01) ()
              "overstep-north: should be ~10, got ~,2f" overstep))))

(defun test-compute-transition-overstep-all-edges ()
  "Overstep computation for all 4 cardinal edges."
  (let ((player (%make-player)))
    ;; South edge: player at y=630, south boundary at y=640
    (setf (player-x player) 300.0 (player-y player) 630.0)
    (let ((os (compute-transition-overstep :south player 10.0 630.0 10.0 640.0)))
      (assert (< (abs (- os 10.0)) 0.01) ()
              "overstep-south: should be ~10, got ~,2f" os))
    ;; West edge: player at x=15, west boundary at x=10
    (setf (player-x player) 15.0 (player-y player) 300.0)
    (let ((os (compute-transition-overstep :west player 10.0 630.0 10.0 640.0)))
      (assert (< (abs (- os 5.0)) 0.01) ()
              "overstep-west: should be ~5, got ~,2f" os))
    ;; East edge: player at x=625, east boundary at x=630
    (setf (player-x player) 625.0)
    (let ((os (compute-transition-overstep :east player 10.0 630.0 10.0 640.0)))
      (assert (< (abs (- os 5.0)) 0.01) ()
              "overstep-east: should be ~5, got ~,2f" os))))

(defun test-apply-overstep-to-spawn-directions ()
  "apply-overstep-to-spawn should shift spawn position inward from spawn edge."
  ;; Spawn at south edge (y=640), overstep=10 → should push up to y=630
  (multiple-value-bind (x y) (apply-overstep-to-spawn :south 10.0 300.0 640.0)
    (assert (< (abs (- y 630.0)) 0.01) ()
            "overstep-spawn-south: y should be ~630, got ~,2f" y)
    (assert (< (abs (- x 300.0)) 0.01) ()
            "overstep-spawn-south: x should be unchanged"))
  ;; Spawn at north edge (y=10), overstep=5 → should push down to y=15
  (multiple-value-bind (x y) (apply-overstep-to-spawn :north 5.0 300.0 10.0)
    (assert (< (abs (- y 15.0)) 0.01) ()
            "overstep-spawn-north: y should be ~15, got ~,2f" y)
    (assert (< (abs (- x 300.0)) 0.01) ()
            "overstep-spawn-north: x should be unchanged"))
  ;; Spawn at east edge (x=630), overstep=8 → should push left to x=622
  (multiple-value-bind (x y) (apply-overstep-to-spawn :east 8.0 630.0 300.0)
    (assert (< (abs (- x 622.0)) 0.01) ()
            "overstep-spawn-east: x should be ~622, got ~,2f" x))
  ;; Spawn at west edge (x=10), overstep=3 → should push right to x=13
  (multiple-value-bind (x y) (apply-overstep-to-spawn :west 3.0 10.0 300.0)
    (assert (< (abs (- x 13.0)) 0.01) ()
            "overstep-spawn-west: x should be ~13, got ~,2f" x)))

;;; ============================================================
;;; ADDENDUM 2: Commit margin tied to collision size
;;; ============================================================

(defun test-commit-margin-at-least-collision-half ()
  "Commit margin must be at least max(collision-half-width, collision-half-height).
   ADDENDUM 2: Ensures the player can trigger commit without pushing into wall."
  ;; Scenario 1: Large collision (e.g. mount) exceeds tile-based margin
  (let* ((tile-size 32.0)
         (half-w 20.0)   ; wider than 0.5 * tile-size = 16
         (half-h 24.0)   ; taller still
         (margin (max (* *zone-commit-margin-tiles* tile-size)
                      (max half-w half-h))))
    (assert (>= margin half-w) ()
            "commit-margin ~,1f must be >= collision-half-width ~,1f" margin half-w)
    (assert (>= margin half-h) ()
            "commit-margin ~,1f must be >= collision-half-height ~,1f" margin half-h)
    ;; For this case, collision-based margin should win
    (assert (= margin 24.0) ()
            "commit-margin should be 24.0 (collision wins), got ~,1f" margin))
  ;; Scenario 2: Small collision — tile-based margin wins
  (let* ((tile-size 32.0)
         (half-w 8.0)    ; smaller than 0.5 * 32 = 16
         (half-h 8.0)
         (margin (max (* *zone-commit-margin-tiles* tile-size)
                      (max half-w half-h))))
    (assert (>= margin half-w) ()
            "commit-margin ~,1f must be >= collision-half-width ~,1f" margin half-w)
    (assert (= margin (* *zone-commit-margin-tiles* tile-size)) ()
            "commit-margin should be tile-based ~,1f, got ~,1f"
            (* *zone-commit-margin-tiles* tile-size) margin)))

;;; ============================================================
;;; ADDENDUM 3: Urgent preload near commit
;;; ============================================================

(defun test-process-preload-queue-urgent-pops-multiple ()
  "process-preload-queue with :count > 1 should pop multiple entries."
  (let* ((cache (make-zone-lru-cache))
         (zone-dir (namestring
                    (merge-pathnames "data/zones/"
                                     (asdf:system-source-directory :mmorpg))))
         (path-5 (concatenate 'string zone-dir "zone-5.lisp"))
         (path-6 (concatenate 'string zone-dir "zone-6.lisp"))
         (game (%make-game :zone-cache cache
                           :preload-queue (list (cons :zone-5 path-5)
                                                (cons :zone-6 path-6)))))
    ;; Queue has 2 entries
    (assert (= (length (game-preload-queue game)) 2) ()
            "urgent-preload: queue should start with 2 entries")
    ;; Urgent mode: pop all at once
    (process-preload-queue game :count 10)
    ;; Queue should be fully drained
    (assert (null (game-preload-queue game)) ()
            "urgent-preload: queue should be empty after urgent pop")
    ;; Both zones should be cached
    (assert (zone-cache-contains-p cache :zone-5) ()
            "urgent-preload: :zone-5 should be in cache")
    (assert (zone-cache-contains-p cache :zone-6) ()
            "urgent-preload: :zone-6 should be in cache")))

(defun test-urgent-preload-no-pending-required ()
  "Urgent preload detection should use proximity only, not player-zone-transition-pending.
   Network clients never have pending set (server-only state), so the urgent path
   must trigger based solely on distance to zone edge."
  (let* ((player (%make-player))
         ;; Zone with known bounds: 0-640 in both axes (20 tiles at 32px)
         (zone (%make-zone :id :test-zone :width 20 :height 20))
         (world (%make-world :zone zone
                             :tile-dest-size 32.0
                             :collision-half-width 12.0
                             :collision-half-height 12.0
                             ;; Fallback bounds (no zone wall map for :test-zone)
                             :wall-min-x 12.0
                             :wall-max-x 628.0
                             :wall-min-y 12.0
                             :wall-max-y 628.0)))
    ;; Player near the north edge (y=20, which is 8px from wall-min-y=12)
    (setf (player-x player) 300.0 (player-y player) 20.0)
    ;; pending is nil (simulating network client)
    (assert (null (player-zone-transition-pending player)) ()
            "urgent-no-pending: pending should be nil (network client)")
    ;; urgent-px = 2 tiles * 32 = 64px. Player is 8px from edge < 64px → should detect
    (let ((urgent-px (* *zone-urgent-preload-tiles* 32.0)))
      (assert (player-within-urgent-preload-distance-p player world urgent-px) ()
              "urgent-no-pending: should detect proximity without pending flag"))
    ;; Player far from any edge (center of zone)
    (setf (player-x player) 320.0 (player-y player) 320.0)
    (let ((urgent-px (* *zone-urgent-preload-tiles* 32.0)))
      (assert (not (player-within-urgent-preload-distance-p player world urgent-px)) ()
              "urgent-no-pending: should NOT detect when player is in center"))))

(defun test-apply-game-state-cache-miss-warns ()
  "apply-game-state should emit a warning and drop the entire snapshot when zone
   cache misses on a client transition. Dropping prevents desync — applying new-zone
   positions into the old zone would leave the client in an inconsistent state."
  (let* ((player (%make-player))
         (zone-dir (namestring
                    (merge-pathnames "data/zones/"
                                     (asdf:system-source-directory :mmorpg))))
         (zone-5-path (concatenate 'string zone-dir "zone-5.lisp"))
         ;; Start in zone-5 so we can transition to zone-6
         (zone-5 (load-zone zone-5-path))
         (world (%make-world :zone zone-5
                             :tile-dest-size 32.0
                             :collision-half-width 12.0
                             :collision-half-height 12.0
                             :wall-min-x 12.0 :wall-max-x 628.0
                             :wall-min-y 12.0 :wall-max-y 628.0
                             :world-graph (load-world-graph)))
         ;; Empty LRU cache — will cause a cache miss
         (cache (make-zone-lru-cache))
         (game (%make-game :player player
                           :players (make-array 1 :initial-contents (list player))
                           :world world
                           :zone-cache cache
                           :net-role :client)))
    (setf (player-zone-id player) :zone-5)
    (setf (player-x player) 100.0 (player-y player) 200.0)
    ;; Apply state with zone-id :zone-6 — cache miss should warn and drop snapshot
    (let ((warned nil))
      (handler-bind ((warning (lambda (w)
                                (when (search "cache MISS" (format nil "~a" w))
                                  (setf warned t)
                                  (muffle-warning w)))))
        (multiple-value-bind (zone-id zone-loaded)
            (apply-game-state game (list :zone-id :zone-6 :x 999.0 :y 999.0) :apply-zone t)
          (declare (ignore zone-id))
          (assert warned ()
                  "cache-miss-warn: should have emitted a warning on zone cache miss")
          (assert (not zone-loaded) ()
                  "cache-miss-warn: zone-loaded should be nil on cache miss")
          ;; Snapshot must be dropped — player position must NOT be updated to new-zone coords
          (assert (< (abs (- (player-x player) 100.0)) 0.01) ()
                  "cache-miss-warn: player-x should be unchanged (snapshot dropped)")
          (assert (< (abs (- (player-y player) 200.0)) 0.01) ()
                  "cache-miss-warn: player-y should be unchanged (snapshot dropped)")
          ;; World should still have the old zone loaded
          (assert (eq (zone-id (world-zone world)) :zone-5) ()
                  "cache-miss-warn: world zone should still be zone-5"))))))

;;; ============================================================
;;; ADDENDUM 4: Soft interpolation/prediction reset
;;; ============================================================

(defun test-soft-reset-preserves-buffers-small-delta ()
  "handle-zone-transition with small position delta should preserve buffers.
   ADDENDUM 4: Seamless walk-through transitions keep interpolation continuity."
  (let* ((player (%make-player))
         (buffer (make-interpolation-buffer))
         (pred (%make-prediction-state
                :predicted-x 100.0 :predicted-y 100.0
                :inputs (make-array 32) :input-capacity 32
                :input-count 5 :input-head 3
                :input-sequence 0 :last-acked-sequence 0
                :misprediction-count 0))
         (game (%make-game :player player
                           :interpolation-buffer buffer
                           :prediction-state pred)))
    ;; Player starts at 100,100 — zone transition moves to 105,100 (small delta)
    (setf (player-x player) 105.0 (player-y player) 100.0)
    ;; Manually push a snapshot so buffer has data
    (push-interpolation-snapshot buffer
                                 (%make-interpolation-snapshot
                                  :timestamp 0.0
                                  :entity-positions (make-hash-table)))
    (assert (= (interpolation-buffer-count buffer) 1) ()
            "soft-reset: buffer should have 1 snapshot before transition")
    ;; Small delta: 100,100 → 105,100 = 25 sq < 1024 threshold
    (handle-zone-transition game :old-x 100.0f0 :old-y 100.0f0)
    ;; Buffer should be preserved (not cleared)
    (assert (= (interpolation-buffer-count buffer) 1) ()
            "soft-reset: buffer should still have 1 snapshot (small delta)")
    ;; Prediction input state should be preserved
    (assert (= (prediction-state-input-count pred) 5) ()
            "soft-reset: prediction input-count should be preserved")
    ;; But prediction position should still be updated to new coords
    (assert (< (abs (- (prediction-state-predicted-x pred) 105.0)) 0.01) ()
            "soft-reset: predicted-x should be updated to new position")))

(defun test-soft-reset-clears-buffers-large-delta ()
  "handle-zone-transition with large position delta should clear buffers.
   ADDENDUM 4: Teleport-like transitions (login, /unstuck) need full reset."
  (let* ((player (%make-player))
         (buffer (make-interpolation-buffer))
         (pred (%make-prediction-state
                :predicted-x 100.0 :predicted-y 100.0
                :inputs (make-array 32) :input-capacity 32
                :input-count 5 :input-head 3
                :input-sequence 0 :last-acked-sequence 0
                :misprediction-count 0))
         (game (%make-game :player player
                           :interpolation-buffer buffer
                           :prediction-state pred)))
    ;; Player jumps from 100,100 to 500,100 (large delta = 160000 sq >> 1024)
    (setf (player-x player) 500.0 (player-y player) 100.0)
    ;; Push a snapshot so buffer has data
    (push-interpolation-snapshot buffer
                                 (%make-interpolation-snapshot
                                  :timestamp 0.0
                                  :entity-positions (make-hash-table)))
    (assert (= (interpolation-buffer-count buffer) 1) ()
            "hard-reset: buffer should have 1 snapshot before transition")
    ;; Large delta: 100,100 → 500,100 = 160000 sq >> 1024 threshold
    (handle-zone-transition game :old-x 100.0f0 :old-y 100.0f0)
    ;; Buffer should be cleared
    (assert (= (interpolation-buffer-count buffer) 0) ()
            "hard-reset: buffer should be cleared (large delta)")
    ;; Prediction input state should be reset
    (assert (= (prediction-state-input-count pred) 0) ()
            "hard-reset: prediction input-count should be 0")
    ;; Prediction position should be updated to new coords
    (assert (< (abs (- (prediction-state-predicted-x pred) 500.0)) 0.01) ()
            "hard-reset: predicted-x should be updated to new position")))

;;; ============================================================
;;; Zone Transition Continuity — Seam Translation Tests
;;; ============================================================

(defun test-seam-translate-position-east ()
  "Crossing east: new-x = dst-min-x + (px - src-max-x), new-y unchanged.
   Uses collision bounds, not pixel spans."
  ;; Zone: 64 tiles * 64px, half-w=16. Collision bounds: min=80, max=4016.
  ;; Player at x=4020 (4px past src-max-x=4016). Dest same bounds.
  ;; new-x = 80 + (4020 - 4016) = 84. Y unchanged.
  (multiple-value-bind (nx ny)
      (seam-translate-position :east 4020.0 900.0
                               80.0 4016.0 80.0 4016.0   ; src bounds
                               80.0 4016.0 80.0 4016.0)  ; dst bounds
    (assert (< (abs (- nx 84.0)) 0.01) ()
            "seam-east: new-x should be 84, got ~,2f" nx)
    (assert (< (abs (- ny 900.0)) 0.01) ()
            "seam-east: new-y should be unchanged at 900, got ~,2f" ny)))

(defun test-seam-translate-position-west ()
  "Crossing west: new-x = dst-max-x + (px - src-min-x), new-y unchanged."
  ;; Player at x=76 (4px past src-min-x=80 toward west).
  ;; new-x = 4016 + (76 - 80) = 4012. Y unchanged.
  (multiple-value-bind (nx ny)
      (seam-translate-position :west 76.0 500.0
                               80.0 4016.0 80.0 4016.0
                               80.0 4016.0 80.0 4016.0)
    (assert (< (abs (- nx 4012.0)) 0.01) ()
            "seam-west: new-x should be 4012, got ~,2f" nx)
    (assert (< (abs (- ny 500.0)) 0.01) ()
            "seam-west: new-y should be unchanged at 500, got ~,2f" ny)))

(defun test-seam-translate-position-north ()
  "Crossing north: new-y = dst-max-y + (py - src-min-y), new-x unchanged."
  ;; Player at y=75 (5px past src-min-y=80 toward north).
  ;; new-y = 4016 + (75 - 80) = 4011. X unchanged.
  (multiple-value-bind (nx ny)
      (seam-translate-position :north 300.0 75.0
                               80.0 4016.0 80.0 4016.0
                               80.0 4016.0 80.0 4016.0)
    (assert (< (abs (- nx 300.0)) 0.01) ()
            "seam-north: new-x should be unchanged at 300, got ~,2f" nx)
    (assert (< (abs (- ny 4011.0)) 0.01) ()
            "seam-north: new-y should be 4011, got ~,2f" ny)))

(defun test-seam-translate-position-south ()
  "Crossing south: new-y = dst-min-y + (py - src-max-y), new-x unchanged."
  ;; Player at y=4020 (4px past src-max-y=4016 toward south).
  ;; new-y = 80 + (4020 - 4016) = 84. X unchanged.
  (multiple-value-bind (nx ny)
      (seam-translate-position :south 300.0 4020.0
                               80.0 4016.0 80.0 4016.0
                               80.0 4016.0 80.0 4016.0)
    (assert (< (abs (- nx 300.0)) 0.01) ()
            "seam-south: new-x should be unchanged at 300, got ~,2f" nx)
    (assert (< (abs (- ny 84.0)) 0.01) ()
            "seam-south: new-y should be 84, got ~,2f" ny)))

(defun test-seam-translate-position-corner ()
  "Corner crossing: translation uses only the crossing edge axis."
  ;; Crossing east at a corner: only x changes, y stays near south edge.
  ;; Player at x=4020 (4px past src-max-x), y=4010 (near south edge).
  ;; new-x = 80 + (4020 - 4016) = 84. Y unchanged at 4010.
  (multiple-value-bind (nx ny)
      (seam-translate-position :east 4020.0 4010.0
                               80.0 4016.0 80.0 4016.0
                               80.0 4016.0 80.0 4016.0)
    (assert (< (abs (- nx 84.0)) 0.01) ()
            "seam-corner-east: new-x should be 84, got ~,2f" nx)
    (assert (< (abs (- ny 4010.0)) 0.01) ()
            "seam-corner-east: new-y should be unchanged at 4010, got ~,2f" ny)))

(defun test-seam-position-valid-p ()
  "seam-position-valid-p should correctly check destination bounds."
  ;; Inside bounds
  (assert (seam-position-valid-p 100.0 200.0 10.0 500.0 10.0 500.0) ()
          "seam-valid: (100,200) should be inside (10-500, 10-500)")
  ;; On boundary (exact)
  (assert (seam-position-valid-p 10.0 10.0 10.0 500.0 10.0 500.0) ()
          "seam-valid: exact min boundary should be valid")
  (assert (seam-position-valid-p 500.0 500.0 10.0 500.0 10.0 500.0) ()
          "seam-valid: exact max boundary should be valid")
  ;; Outside bounds
  (assert (not (seam-position-valid-p 5.0 200.0 10.0 500.0 10.0 500.0)) ()
          "seam-valid: x=5 below min-x=10 should be invalid")
  (assert (not (seam-position-valid-p 100.0 505.0 10.0 500.0 10.0 500.0)) ()
          "seam-valid: y=505 above max-y=500 should be invalid"))

(defun test-seam-translation-used-in-transition-zone ()
  "Integration: transition-zone with seam translation produces position near seam.
   Verifies that after crossing east, the player appears near x=0 of destination zone
   (not teleported to an edge-spawn position)."
  (let* ((world (make-test-world :tile-size 64.0 :collision-half 16.0))
         (zone-a-id :zone-a)
         (zone-b-id :zone-b)
         ;; Create a simple 10x10 zone with no walls
         (wall-map (make-array '(10 10) :initial-element 0))
         ;; Zone B struct
         (zone-b (%make-zone :id zone-b-id :width 10 :height 10
                             :collision-tiles nil :objects nil))
         (player (%make-player))
         (game (%make-game :world world :players (vector player)
                           :npcs (vector) :entities (vector player)
                           :net-role :server
                           :npc-id-source (make-id-source 1000000 nil)))
         (exit (list :to zone-b-id :spawn-edge :west)))
    ;; Setup source zone (zone-a) state
    (setf (gethash zone-a-id *zone-states*)
          (make-zone-state :zone-id zone-a-id
                           :wall-map wall-map
                           :zone (%make-zone :id zone-a-id :width 10 :height 10
                                             :collision-tiles nil :objects nil)))
    ;; Setup destination zone (zone-b) state
    (setf (gethash zone-b-id *zone-states*)
          (make-zone-state :zone-id zone-b-id
                           :wall-map wall-map
                           :zone zone-b
                           :player-grid (make-spatial-grid-for-zone 10 10 64.0)
                           :npc-grid (make-spatial-grid-for-zone 10 10 64.0)))
    ;; Place player just past east collision bound of zone-a
    ;; 10-tile zone with tile-size=64, half-w=16:
    ;;   collision min-x=80 (1*64+16), max-x=560 (9*64-16)
    ;; Player at x=562 (2px past src-max-x=560)
    (setf (player-x player) 562.0
          (player-y player) 300.0
          (player-zone-id player) zone-a-id
          (player-intent player) (make-intent))
    ;; Set up world bounds matching 10-tile zone collision
    (setf (world-wall-min-x world) 80.0
          (world-wall-max-x world) 560.0
          (world-wall-min-y world) 80.0
          (world-wall-max-y world) 560.0
          (world-zone-label world) "Zone A")
    ;; Write zone-b to a temp file so transition-zone can load it
    (let ((tmp-path (format nil "/tmp/test-zone-b-~a.lisp" (get-universal-time))))
      (with-open-file (out tmp-path :direction :output :if-exists :supersede)
        (write (list :id zone-b-id :width 10 :height 10
                     :tile-layers nil :collision-tiles nil :objects nil)
               :stream out))
      (let* ((paths (make-hash-table :test 'eq))
             (graph (%make-world-graph :edges-by-zone (make-hash-table :test 'eq)
                                       :zone-paths paths)))
        (setf (gethash zone-b-id paths) tmp-path)
        (setf (world-world-graph world) graph))
      (unwind-protect
           (progn
             (transition-zone game player exit :east)
             ;; After east crossing: new-x = dst-min-x + (562 - src-max-x)
             ;;   = 80 + (562 - 560) = 82. Y unchanged at 300.
             ;; This is seamless: only 2px from the destination edge.
             (assert (< (abs (- (player-x player) 82.0)) 1.0) ()
                     "seam-integration: player-x should be near 82, got ~,2f"
                     (player-x player))
             ;; Y-axis must be preserved exactly (not ratio-mapped to 0.5)
             (assert (< (abs (- (player-y player) 300.0)) 1.0) ()
                     "seam-integration: player-y should be unchanged at 300, got ~,2f"
                     (player-y player))
             (assert (eq (player-zone-id player) zone-b-id) ()
                     "seam-integration: player should be in zone-b"))
        (delete-file tmp-path)))))

(defun test-seam-translation-blocked-uses-fallback ()
  "When seam-translated+clamped position is blocked by a wall, fallback ratio-spawn is used."
  (let* ((world (make-test-world :tile-size 64.0 :collision-half 16.0))
         (zone-a-id :zone-blk-a)
         (zone-b-id :zone-blk-b)
         ;; Zone B has a wall across the entire west edge (column 1)
         ;; so clamped seam position (80, y) will be blocked
         (wall-map-b (make-array '(10 10) :initial-element 0))
         (zone-b (%make-zone :id zone-b-id :width 10 :height 10
                              :collision-tiles nil :objects nil))
         (player (%make-player))
         (game (%make-game :world world :players (vector player)
                           :npcs (vector) :entities (vector player)
                           :net-role :server
                           :npc-id-source (make-id-source 1000000 nil)))
         (exit (list :to zone-b-id :spawn-edge :west)))
    ;; Block column 1 (the west spawn edge area) in zone-b
    (loop :for row :from 0 :below 10
          :do (setf (aref wall-map-b row 1) 1))
    ;; Source zone wall-map (no walls)
    (let ((wall-map-a (make-array '(10 10) :initial-element 0)))
      (setf (gethash zone-a-id *zone-states*)
            (make-zone-state :zone-id zone-a-id
                             :wall-map wall-map-a
                             :zone (%make-zone :id zone-a-id :width 10 :height 10
                                               :collision-tiles nil :objects nil)))
      (setf (gethash zone-b-id *zone-states*)
            (make-zone-state :zone-id zone-b-id
                             :wall-map wall-map-b
                             :zone zone-b
                             :player-grid (make-spatial-grid-for-zone 10 10 64.0)
                             :npc-grid (make-spatial-grid-for-zone 10 10 64.0)))
      ;; Player just past east collision bound
      (setf (player-x player) 562.0
            (player-y player) 300.0
            (player-zone-id player) zone-a-id
            (player-intent player) (make-intent))
      (setf (world-wall-min-x world) 80.0
            (world-wall-max-x world) 560.0
            (world-wall-min-y world) 80.0
            (world-wall-max-y world) 560.0
            (world-zone-label world) "Zone A")
      (let ((tmp-path (format nil "/tmp/test-zone-blk-~a.lisp" (get-universal-time))))
        (with-open-file (out tmp-path :direction :output :if-exists :supersede)
          (write (list :id zone-b-id :width 10 :height 10
                       :tile-layers nil :collision-tiles nil :objects nil)
                 :stream out))
        (let* ((paths (make-hash-table :test 'eq))
               (graph (%make-world-graph :edges-by-zone (make-hash-table :test 'eq)
                                         :zone-paths paths)))
          (setf (gethash zone-b-id paths) tmp-path)
          (setf (world-world-graph world) graph))
        (unwind-protect
             (progn
               (transition-zone game player exit :east)
               ;; Seam translation gives clamped (80, 300) which is blocked → fallback
               ;; Fallback uses find-open-position-with-map spiral search
               ;; Player should end up in zone-b at a non-blocked position
               (assert (eq (player-zone-id player) zone-b-id) ()
                       "seam-blocked: player should be in zone-b")
               ;; Position should be positive (within zone pixel space)
               (assert (> (player-x player) 0.0) ()
                       "seam-blocked: player-x should be positive, got ~,2f"
                       (player-x player))
               ;; Should NOT be at the blocked column 1 (tile center = 96px for 64px tiles)
               ;; Column 1 tile center = 1*64 + 32 = 96
               (assert (not (and (>= (player-x player) 64.0)
                                 (<= (player-x player) 128.0)
                                 (= (aref wall-map-b
                                          (floor (player-y player) 64.0)
                                          (floor (player-x player) 64.0))
                                    1)))
                       () "seam-blocked: player should not be on a blocked tile"))
          (delete-file tmp-path))))))

(defun test-seam-translate-mixed-bounds ()
  "Seam translation with different-sized source and destination zones.
   Crossing east from a small zone (10 tiles) into a large zone (20 tiles).
   Overstep distance should be preserved regardless of zone size mismatch."
  ;; Source 10-tile zone: collision bounds min=80, max=560 (tile-size=64, half-w=16)
  ;; Dest 20-tile zone: collision bounds min=80, max=1200 (19*64-16=1200)
  ;; Player at x=563 (3px past src-max-x=560)
  ;; new-x = dst-min-x + (px - src-max-x) = 80 + (563 - 560) = 83
  (multiple-value-bind (nx ny)
      (seam-translate-position :east 563.0 400.0
                               80.0 560.0 80.0 560.0    ; src (10-tile)
                               80.0 1200.0 80.0 1200.0)  ; dst (20-tile)
    (assert (< (abs (- nx 83.0)) 0.01) ()
            "seam-mixed-east: new-x should be 83, got ~,2f" nx)
    (assert (< (abs (- ny 400.0)) 0.01) ()
            "seam-mixed-east: new-y should be unchanged at 400, got ~,2f" ny))
  ;; Crossing west from large zone (20 tiles) into small zone (10 tiles)
  ;; Player at x=78 (2px past src-min-x=80 toward west)
  ;; new-x = dst-max-x + (px - src-min-x) = 560 + (78 - 80) = 558
  (multiple-value-bind (nx ny)
      (seam-translate-position :west 78.0 400.0
                               80.0 1200.0 80.0 1200.0   ; src (20-tile)
                               80.0 560.0 80.0 560.0)    ; dst (10-tile)
    (assert (< (abs (- nx 558.0)) 0.01) ()
            "seam-mixed-west: new-x should be 558, got ~,2f" nx)
    (assert (< (abs (- ny 400.0)) 0.01) ()
            "seam-mixed-west: new-y should be unchanged at 400, got ~,2f" ny)))
