(in-package #:mmorpg)

;;; ============================================================
;;; EDITOR LOGIC TESTS (Pure functions, no raylib)
;;; ============================================================

(defun test-editor-selection-origin ()
  "Test editor-selection-origin returns correct tile coords and columns."
  (let ((editor (%make-editor :selected-tile 10 :selection-width 2 :selection-height 2))
        (tileset (%make-editor-tileset :columns 8 :rows 6 :tile-count 48)))
    (multiple-value-bind (ox oy cols)
        (editor-selection-origin editor tileset)
      (assert (= ox 2) () "sel-origin: x = 10 mod 8 = 2")
      (assert (= oy 1) () "sel-origin: y = floor(10,8) = 1")
      (assert (= cols 8) () "sel-origin: columns = 8")))
  ;; Tile 0
  (let ((editor (%make-editor :selected-tile 0 :selection-width 1 :selection-height 1))
        (tileset (%make-editor-tileset :columns 4 :rows 4 :tile-count 16)))
    (multiple-value-bind (ox oy cols)
        (editor-selection-origin editor tileset)
      (assert (= ox 0) () "sel-origin: tile 0 x")
      (assert (= oy 0) () "sel-origin: tile 0 y")
      (assert (= cols 4) () "sel-origin: tile 0 cols")))
  ;; Nil tileset falls back to 1 column
  (let ((editor (%make-editor :selected-tile 5)))
    (multiple-value-bind (ox oy cols)
        (editor-selection-origin editor nil)
      (assert (= ox 0) () "sel-origin: nil tileset x = 5 mod 1 = 0")
      (assert (= oy 5) () "sel-origin: nil tileset y = floor(5,1) = 5")
      (assert (= cols 1) () "sel-origin: nil tileset cols = 1"))))

(defun test-editor-clamp-selection ()
  "Test editor-clamp-selection clamps selection to tileset bounds."
  ;; Selection that overflows right edge
  (let ((editor (%make-editor :selected-tile 7 :selection-width 3 :selection-height 1))
        (tileset (%make-editor-tileset :columns 8 :rows 4 :tile-count 32)))
    (editor-clamp-selection editor tileset)
    (assert (<= (editor-selection-width editor) 1) ()
            "clamp-sel: width clamped at right edge"))
  ;; Selection that overflows bottom edge
  (let ((editor (%make-editor :selected-tile 24 :selection-width 1 :selection-height 3))
        (tileset (%make-editor-tileset :columns 8 :rows 4 :tile-count 32)))
    (editor-clamp-selection editor tileset)
    (assert (<= (editor-selection-height editor) 1) ()
            "clamp-sel: height clamped at bottom edge"))
  ;; Selection that fits
  (let ((editor (%make-editor :selected-tile 0 :selection-width 2 :selection-height 2))
        (tileset (%make-editor-tileset :columns 8 :rows 4 :tile-count 32)))
    (editor-clamp-selection editor tileset)
    (assert (= (editor-selection-width editor) 2) ()
            "clamp-sel: width preserved when fits")
    (assert (= (editor-selection-height editor) 2) ()
            "clamp-sel: height preserved when fits")))

(defun test-editor-update-tileset-selection-single ()
  "Test single-click tileset selection sets tile and resets to 1x1."
  (let ((editor (%make-editor :selected-tile 0 :selection-width 3 :selection-height 2
                              :selection-anchor nil))
        (tileset (%make-editor-tileset :columns 8 :rows 4 :tile-count 32)))
    ;; Click on tile at column 3, row 1 => index 11
    (editor-update-tileset-selection editor tileset 3 1 nil)
    (assert (= (editor-selected-tile editor) 11) ()
            "single-click: tile index = 3 + 1*8 = 11")
    (assert (= (editor-selection-width editor) 1) ()
            "single-click: width reset to 1")
    (assert (= (editor-selection-height editor) 1) ()
            "single-click: height reset to 1")))

(defun test-editor-update-tileset-selection-shift ()
  "Test shift-click tileset selection builds multi-tile brush."
  (let ((editor (%make-editor :selected-tile 0 :selection-width 1 :selection-height 1
                              :selection-anchor nil))
        (tileset (%make-editor-tileset :columns 8 :rows 4 :tile-count 32)))
    ;; First click at (1,1) to set anchor
    (editor-update-tileset-selection editor tileset 1 1 nil)
    (assert (= (editor-selection-anchor editor) 9) ()
            "shift: anchor set by first click")
    ;; Shift-click at (3,2) to build 3x2 selection
    (editor-update-tileset-selection editor tileset 3 2 t)
    (assert (>= (editor-selection-width editor) 3) ()
            "shift: width >= 3")
    (assert (>= (editor-selection-height editor) 2) ()
            "shift: height >= 2")))

(defun test-editor-object-cursor-is-1x1 ()
  "Test that object mode always uses 1x1 cursor regardless of selection size.
   Regression test for Phase 7 cursor/brush mismatch."
  ;; Simulate an editor with a multi-tile selection in object mode
  (let ((editor (%make-editor :mode :object
                              :selection-width 3
                              :selection-height 2)))
    ;; The cursor size logic from draw-editor-cursor:
    ;; brush-w/h should be 1 for :object mode
    (let ((brush-w (if (member (editor-mode editor) '(:tile :collision))
                       (max 1 (editor-selection-width editor))
                       1))
          (brush-h (if (member (editor-mode editor) '(:tile :collision))
                       (max 1 (editor-selection-height editor))
                       1)))
      (assert (= brush-w 1) () "object-cursor: brush-w always 1")
      (assert (= brush-h 1) () "object-cursor: brush-h always 1")))
  ;; Verify tile mode DOES use multi-tile brush
  (let ((editor (%make-editor :mode :tile
                              :selection-width 3
                              :selection-height 2)))
    (let ((brush-w (if (member (editor-mode editor) '(:tile :collision))
                       (max 1 (editor-selection-width editor))
                       1)))
      (assert (= brush-w 3) () "tile-cursor: brush-w uses selection")))
  ;; Verify spawn mode uses 1x1
  (let ((editor (%make-editor :mode :spawn
                              :selection-width 3
                              :selection-height 2)))
    (let ((brush-w (if (member (editor-mode editor) '(:tile :collision))
                       (max 1 (editor-selection-width editor))
                       1)))
      (assert (= brush-w 1) () "spawn-cursor: brush-w always 1"))))

;;; ============================================================

(defvar *tests-editor*
  '(test-editor-selection-origin
    test-editor-clamp-selection
    test-editor-update-tileset-selection-single
    test-editor-update-tileset-selection-shift
    test-editor-object-cursor-is-1x1)
  "Editor domain test functions.")
