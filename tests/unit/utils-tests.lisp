(in-package #:mmorpg)

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
;;; VIRTUAL DISPLAY TESTS
;;; ============================================================

(defun test-compute-present-scale ()
  "Test integer scale computation for common display resolutions."
  ;; 1280x720 display → scale 2 (640*2=1280, 360*2=720, perfect fit)
  (multiple-value-bind (scale ox oy)
      (compute-present-scale 1280 720 640 360)
    (assert (= scale 2) () "present-scale: 1280x720 = 2x")
    (assert (= ox 0) () "present-scale: 1280x720 no X offset")
    (assert (= oy 0) () "present-scale: 1280x720 no Y offset"))
  ;; 1920x1080 → scale 3
  (multiple-value-bind (scale ox oy)
      (compute-present-scale 1920 1080 640 360)
    (assert (= scale 3) () "present-scale: 1920x1080 = 3x")
    (assert (= ox 0) () "present-scale: 1920x1080 no X offset")
    (assert (= oy 0) () "present-scale: 1920x1080 no Y offset"))
  ;; 2560x1440 → scale 4
  (multiple-value-bind (scale ox oy)
      (compute-present-scale 2560 1440 640 360)
    (assert (= scale 4) () "present-scale: 2560x1440 = 4x")
    (assert (= ox 0) () "present-scale: 2560x1440 no X offset")
    (assert (= oy 0) () "present-scale: 2560x1440 no Y offset"))
  ;; 3840x2160 → scale 6
  (multiple-value-bind (scale ox oy)
      (compute-present-scale 3840 2160 640 360)
    (assert (= scale 6) () "present-scale: 3840x2160 = 6x")
    (assert (= ox 0) () "present-scale: 3840x2160 no X offset")
    (assert (= oy 0) () "present-scale: 3840x2160 no Y offset")))

(defun test-compute-present-scale-letterbox ()
  "Test letterbox offsets for non-standard aspect ratios."
  ;; 1920x1200 (16:10) → scale 3, letterbox Y
  (multiple-value-bind (scale ox oy)
      (compute-present-scale 1920 1200 640 360)
    (assert (= scale 3) () "present-scale: 1920x1200 = 3x")
    (assert (= ox 0) () "present-scale: 1920x1200 X offset = 0")
    (assert (= oy 60) () "present-scale: 1920x1200 Y offset = 60"))
  ;; Small display that can only fit 1x
  (multiple-value-bind (scale ox oy)
      (compute-present-scale 800 600 640 360)
    (assert (= scale 1) () "present-scale: 800x600 = 1x")
    (assert (= ox 80) () "present-scale: 800x600 X offset = 80")
    (assert (= oy 120) () "present-scale: 800x600 Y offset = 120")))

(defun test-display-to-virtual-mouse ()
  "Test display-to-virtual coordinate conversion."
  ;; Scale 2, no offset: pixel-perfect mapping
  (multiple-value-bind (vx vy)
      (display-to-virtual-mouse 100 200 2 0 0 640 360)
    (assert (= vx 50) () "virtual-mouse: 100/2 = 50")
    (assert (= vy 100) () "virtual-mouse: 200/2 = 100"))
  ;; Scale 3 with letterbox offset
  (multiple-value-bind (vx vy)
      (display-to-virtual-mouse 10 70 3 0 60 640 360)
    (assert (= vx 3) () "virtual-mouse: (10-0)/3 = 3")
    (assert (= vy 3) () "virtual-mouse: (70-60)/3 = 3"))
  ;; Clamp to bounds
  (multiple-value-bind (vx vy)
      (display-to-virtual-mouse 0 0 2 100 100 640 360)
    (assert (= vx 0) () "virtual-mouse: clamped X to 0")
    (assert (= vy 0) () "virtual-mouse: clamped Y to 0"))
  ;; Clamp upper bound
  (multiple-value-bind (vx vy)
      (display-to-virtual-mouse 5000 5000 2 0 0 640 360)
    (assert (= vx 639) () "virtual-mouse: clamped X to 639")
    (assert (= vy 359) () "virtual-mouse: clamped Y to 359")))

(defun test-display-to-virtual-mouse-zero-scale ()
  "Test display-to-virtual-mouse clamps scale to 1 when given 0 or negative."
  (multiple-value-bind (vx vy)
      (display-to-virtual-mouse 100 200 0 0 0 640 360)
    (assert (= vx 100) () "virtual-mouse-zero-scale: X with scale 0 treated as 1")
    (assert (= vy 200) () "virtual-mouse-zero-scale: Y with scale 0 treated as 1"))
  (multiple-value-bind (vx vy)
      (display-to-virtual-mouse 100 200 -1 0 0 640 360)
    (assert (= vx 100) () "virtual-mouse-neg-scale: X with scale -1 treated as 1")
    (assert (= vy 200) () "virtual-mouse-neg-scale: Y with scale -1 treated as 1")))

(defun test-current-screen-always-virtual ()
  "Test that current-screen-width/height always return virtual dimensions.
   Phase 1 contract: all game code works in 640x360 virtual space."
  (assert (= (current-screen-width) *virtual-width*)
          () "current-screen-width: returns *virtual-width*")
  (assert (= (current-screen-height) *virtual-height*)
          () "current-screen-height: returns *virtual-height*")
  ;; Verify contract holds regardless of virtual resolution values
  (let ((*virtual-width* 800)
        (*virtual-height* 450))
    (assert (= (current-screen-width) 800)
            () "current-screen-width: tracks *virtual-width* changes")
    (assert (= (current-screen-height) 450)
            () "current-screen-height: tracks *virtual-height* changes")))

(defun test-camera-offset-always-virtual-center ()
  "Test that make-camera creates camera with virtual center offset.
   Phase 1 contract: camera offset is always (virtual-width/2, virtual-height/2)."
  (let* ((*virtual-width* 640)
         (*virtual-height* 360)
         (cam (make-camera))
         (offset (camera-offset cam)))
    (assert (= (raylib:vector2-x offset) 320.0)
            () "camera: X offset = virtual-width/2")
    (assert (= (raylib:vector2-y offset) 180.0)
            () "camera: Y offset = virtual-height/2")))

(defun test-login-layout-fits-virtual-bounds ()
  "Test that login panel and all buttons fit within 640x360 virtual space.
   Regression test: panel was 550px tall, Quit button landed at y=375 (unreachable)."
  (let* ((*virtual-width* 640)
         (*virtual-height* 360)
         (screen-width *virtual-width*)
         (screen-height *virtual-height*)
         (panel-width 380)
         (panel-height 320)
         (panel-x (truncate (/ (- screen-width panel-width) 2)))
         (panel-y (truncate (/ (- screen-height panel-height) 2)))
         (button-height 36)
         ;; Button Y positions must match draw-login-screen in ui.lisp
         (login-y (+ panel-y 170))
         (register-y (+ panel-y 215))
         (quit-y (+ panel-y 265)))
    ;; Panel must be fully within virtual bounds
    (assert (>= panel-x 0) () "login-layout: panel-x >= 0")
    (assert (>= panel-y 0) () "login-layout: panel-y >= 0")
    (assert (<= (+ panel-x panel-width) screen-width)
            () "login-layout: panel right edge within bounds")
    (assert (<= (+ panel-y panel-height) screen-height)
            () "login-layout: panel bottom edge within bounds")
    ;; All button bottoms must be reachable (< virtual height)
    (assert (< (+ login-y button-height) screen-height)
            () "login-layout: login button bottom reachable")
    (assert (< (+ register-y button-height) screen-height)
            () "login-layout: register button bottom reachable")
    (assert (< (+ quit-y button-height) screen-height)
            () "login-layout: quit button bottom reachable")))

(defvar *tests-utils*
  '(test-clamp
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
    test-compute-present-scale
    test-compute-present-scale-letterbox
    test-display-to-virtual-mouse
    test-display-to-virtual-mouse-zero-scale
    test-current-screen-always-virtual
    test-camera-offset-always-virtual-center
    test-login-layout-fits-virtual-bounds)
  "Utils domain test functions.")
