(in-package #:mmorpg)

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
  (let* ((archetype (gethash :street-punk *npc-archetypes*))
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


(defvar *tests-save*
  '(test-quantize-coord-roundtrip
    test-quantize-timer-roundtrip
    test-encode-anim-state-roundtrip
    test-encode-facing-roundtrip
    test-pack-player-flags-roundtrip
    test-pack-npc-flags-roundtrip
    test-serialize-skill-roundtrip
    test-serialize-stat-block-roundtrip
    test-serialize-inventory-slot-roundtrip)
  "Save/serialization domain test functions.")
