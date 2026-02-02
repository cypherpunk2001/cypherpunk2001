(in-package #:mmorpg)

;;; ============================================================
;;; TEST HELPERS
;;; Shared helpers used across all test domain files.
;;; ============================================================

(defun ensure-test-game-data ()
  "Ensure game data is loaded for tests."
  (unless *game-data-loaded-p*
    (load-game-data)))

(defun make-test-world (&key (tile-size 32.0) (collision-half 12.0))
  "Create a minimal world struct for testing functions that need world."
  (%make-world :tile-dest-size tile-size
               :collision-half-width collision-half
               :collision-half-height collision-half))

;;; ============================================================
;;; Security Test Helpers
;;; Used by security-input-tests.lisp and security-gamestate-tests.lisp
;;; ============================================================

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
