(in-package #:mmorpg)

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
