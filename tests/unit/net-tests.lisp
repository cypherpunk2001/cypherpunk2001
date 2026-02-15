(in-package #:mmorpg)

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
                  (archetype (or (gethash :street-punk *npc-archetypes*) (default-npc-archetype)))
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



(defvar *tests-net*
  '(test-string-to-octets
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
    ;; Final Net Tests
    test-session-try-register
    test-session-unregister
    test-session-get)
  "Net domain test functions.")
