;; NOTE: If you change behavior here, update docs/net.md :)
;; net-protocol.lisp — message formats, encode/decode, validation, constants, buffer helpers
;; Split from net.lisp. Load order: net-protocol → net-auth → net-snapshot → net-server → net-client → net
(in-package #:mmorpg)

(defun make-net-buffer (&optional (size *net-buffer-size*))
  ;; Allocate a reusable UDP buffer.
  (make-array size :element-type '(unsigned-byte 8) :initial-element 0))

(defun string-to-octets (string)
  ;; Encode STRING as ASCII octets.
  (let* ((len (length string))
         (bytes (make-array len :element-type '(unsigned-byte 8))))
    (dotimes (i len)
      (setf (aref bytes i) (char-code (aref string i))))
    bytes))

(defun octets-to-string (octets length)
  ;; Decode LENGTH octets into an ASCII string.
  (let ((chars (make-string length)))
    (dotimes (i length)
      (setf (aref chars i) (code-char (aref octets i))))
    chars))

(defun encode-net-message (message)
  ;; Serialize MESSAGE as a readable string.
  (with-output-to-string (out)
    (prin1 message out)))

(defun decode-net-message (string)
  ;; Parse a message string into a Lisp object (safe read).
  (let ((*read-eval* nil))
    (handler-case
        (multiple-value-bind (form _) (read-from-string string nil nil)
          (declare (ignore _))
          (when (listp form)
            form))
      (error () nil))))

(defun send-net-message (socket message &key host port)
  ;; Send MESSAGE over SOCKET to HOST/PORT (or connected peer).
  ;; Returns T on success, NIL on failure (non-fatal, logs warning).
  (handler-case
      (let* ((text (encode-net-message message))
             (octets (string-to-octets text))
             (size (length octets)))
        (when (> size *net-buffer-size*)
          (warn "Dropping UDP payload (~d bytes) over buffer size ~d" size *net-buffer-size*)
          (log-verbose "UDP payload too large: ~d bytes (limit ~d)" size *net-buffer-size*)
          (return-from send-net-message nil))
        (usocket:socket-send socket octets size :host host :port port)
        t)
    (error (e)
      (log-verbose "Failed to send message to ~a:~d: ~a" host port e)
      nil)))

(defun send-net-message-with-retry (socket message &key host port (max-retries 3) (delay 50))
  "Send critical MESSAGE with linear retry on failure (for auth responses, etc).
   Returns T on success, NIL after all retries exhausted."
  (with-retry-linear (sent (lambda () (send-net-message socket message :host host :port port))
                       :max-retries max-retries
                       :delay delay)
    sent))

(defun get-nproc ()
  ;; Return number of CPU cores, or 1 if unable to determine.
  #+sbcl
  (or (ignore-errors
       (parse-integer
        (with-output-to-string (s)
          (sb-ext:run-program "nproc" nil :output s :search t))
        :junk-allowed t))
      1)
  #-sbcl
  1)

;;; Phase 3 perf: Cache host strings to avoid per-packet format/coerce allocation
(defparameter *host-string-cache* (make-hash-table :test 'eql :size 256)
  "Cache of packed-ip -> host string. Avoids per-packet format allocation.")

(declaim (inline pack-ipv4-host))
(defun pack-ipv4-host (host)
  "Pack IPv4 byte vector into a single fixnum for hash key."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (and (vectorp host) (= (length host) 4))
      (the fixnum (logior (ash (aref host 0) 24)
                          (ash (aref host 1) 16)
                          (ash (aref host 2) 8)
                          (aref host 3)))
      0))

(defun host-to-string (host)
  "Convert a host (byte vector or string) to string. Uses cache for IPv4."
  (if (stringp host)
      host
      (let ((key (pack-ipv4-host host)))
        (or (gethash key *host-string-cache*)
            (let ((str (format nil "~{~d~^.~}" (coerce host 'list))))
              (setf (gethash key *host-string-cache*) str)
              str)))))

;;;; ========================================================================
;;;; BINARY SNAPSHOT ENCODING - Phase 3 Task 3.1
;;;; Compact binary format for snapshot messages to reduce bandwidth and CPU.
;;;; Uses 4-byte signed integers for all values (matching quantized compact vectors).
;;;; Format: magic(4) + version(1) + format(1) + seq(4) + zone(4) + counts(6) + data
;;;; ========================================================================

;;; Magic header for binary snapshots (ASCII "SNAP")
(defparameter *binary-snapshot-magic* #(83 78 65 80)  ; "SNAP" in ASCII
  "Magic bytes identifying binary snapshot format.")
(defparameter *binary-snapshot-version* 1
  "Binary snapshot format version for compatibility checks.")

;;; Reusable send buffer for binary encoding (avoids per-send allocation)
(defparameter *binary-send-buffer* nil
  "Pre-allocated buffer for binary snapshot encoding. Initialized on first use.")

(defun ensure-binary-send-buffer ()
  "Ensure *binary-send-buffer* is allocated. Returns the buffer."
  (or *binary-send-buffer*
      (setf *binary-send-buffer*
            (make-array *net-buffer-size* :element-type '(unsigned-byte 8)
                        :initial-element 0))))

;;; Low-level binary encoding primitives
(declaim (inline write-uint8 write-uint16 write-uint32 write-int32))

(defun write-uint8 (buffer offset value)
  "Write unsigned 8-bit integer to buffer. Returns new offset."
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum offset)
           (type (unsigned-byte 8) value)
           (optimize (speed 3) (safety 0) (debug 0)))
  (setf (aref buffer offset) value)
  (the fixnum (1+ offset)))

(defun write-uint16 (buffer offset value)
  "Write unsigned 16-bit integer (big-endian) to buffer. Returns new offset."
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum offset)
           (type (unsigned-byte 16) value)
           (optimize (speed 3) (safety 0) (debug 0)))
  (setf (aref buffer offset) (ldb (byte 8 8) value)
        (aref buffer (1+ offset)) (ldb (byte 8 0) value))
  (the fixnum (+ offset 2)))

(defun write-uint32 (buffer offset value)
  "Write unsigned 32-bit integer (big-endian) to buffer. Returns new offset."
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum offset)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((v (logand value #xFFFFFFFF)))
    (setf (aref buffer offset) (ldb (byte 8 24) v)
          (aref buffer (+ offset 1)) (ldb (byte 8 16) v)
          (aref buffer (+ offset 2)) (ldb (byte 8 8) v)
          (aref buffer (+ offset 3)) (ldb (byte 8 0) v)))
  (the fixnum (+ offset 4)))

(defun write-int32 (buffer offset value)
  "Write signed 32-bit integer (big-endian, two's complement) to buffer. Returns new offset."
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum offset)
           (optimize (speed 3) (safety 0) (debug 0)))
  ;; Handle negative values with two's complement
  (let ((v (if (< value 0)
               (logand (+ (ash 1 32) value) #xFFFFFFFF)
               (logand value #xFFFFFFFF))))
    (setf (aref buffer offset) (ldb (byte 8 24) v)
          (aref buffer (+ offset 1)) (ldb (byte 8 16) v)
          (aref buffer (+ offset 2)) (ldb (byte 8 8) v)
          (aref buffer (+ offset 3)) (ldb (byte 8 0) v)))
  (the fixnum (+ offset 4)))

;;; Low-level binary decoding primitives
(declaim (inline read-uint8 read-uint16 read-uint32 read-int32))

(defun read-uint8 (buffer offset)
  "Read unsigned 8-bit integer from buffer. Returns (values value new-offset)."
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum offset)
           (optimize (speed 3) (safety 0) (debug 0)))
  (values (aref buffer offset) (the fixnum (1+ offset))))

(defun read-uint16 (buffer offset)
  "Read unsigned 16-bit integer (big-endian) from buffer. Returns (values value new-offset)."
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum offset)
           (optimize (speed 3) (safety 0) (debug 0)))
  (values (the fixnum (logior (ash (aref buffer offset) 8)
                              (aref buffer (1+ offset))))
          (the fixnum (+ offset 2))))

(defun read-uint32 (buffer offset)
  "Read unsigned 32-bit integer (big-endian) from buffer. Returns (values value new-offset)."
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum offset)
           (optimize (speed 3) (safety 0) (debug 0)))
  (values (logior (ash (aref buffer offset) 24)
                  (ash (aref buffer (+ offset 1)) 16)
                  (ash (aref buffer (+ offset 2)) 8)
                  (aref buffer (+ offset 3)))
          (the fixnum (+ offset 4))))

(defun read-int32 (buffer offset)
  "Read signed 32-bit integer (big-endian, two's complement) from buffer."
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum offset)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((v (logior (ash (aref buffer offset) 24)
                   (ash (aref buffer (+ offset 1)) 16)
                   (ash (aref buffer (+ offset 2)) 8)
                   (aref buffer (+ offset 3)))))
    ;; Convert from two's complement if high bit set
    (values (if (logbitp 31 v)
                (- v (ash 1 32))
                v)
            (the fixnum (+ offset 4)))))

;;; High-level binary snapshot encoding
(defun encode-snapshot-binary (state events buffer)
  "Encode snapshot STATE and EVENTS into BUFFER using binary format.
   Returns the number of bytes written.
   STATE is a plist with :format, :seq, :zone-id, :players, :npcs, :objects (or delta keys).
   EVENTS is a list of event plists (currently not binary-encoded, included as nil count)."
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer))
  (let* ((format (getf state :format))
         (is-delta (member format '(:delta-v5)))
         (seq (or (getf state :seq) 0))
         (zone-id-hash (encode-zone-id (getf state :zone-id)))
         ;; Player vectors are either :players (full) or :changed-players (delta)
         (players (if is-delta
                      (getf state :changed-players)
                      (getf state :players)))
         (npcs (if is-delta
                   (getf state :changed-npcs)
                   (getf state :npcs)))
         (objects (getf state :objects))
         (player-count (if players (length players) 0))
         (npc-count (if npcs (length npcs) 0))
         (object-count (if objects (length objects) 0))
         (offset 0))
    (declare (type fixnum offset player-count npc-count object-count))
    ;; Write header
    ;; Magic bytes (4)
    (dotimes (i 4)
      (setf (aref buffer i) (aref *binary-snapshot-magic* i)))
    (setf offset 4)
    ;; Version (1)
    (setf offset (write-uint8 buffer offset *binary-snapshot-version*))
    ;; Format flag: 0=compact-v5, 1=delta-v5 (1)
    (setf offset (write-uint8 buffer offset (if is-delta 1 0)))
    ;; Sequence number (4)
    (setf offset (write-uint32 buffer offset seq))
    ;; Zone-id hash (4)
    (setf offset (write-uint32 buffer offset zone-id-hash))
    ;; Counts: player (2), npc (2), object (2) = 6 bytes
    (setf offset (write-uint16 buffer offset player-count))
    (setf offset (write-uint16 buffer offset npc-count))
    (setf offset (write-uint16 buffer offset object-count))
    ;; Header total: 4+1+1+4+4+6 = 20 bytes

    ;; Write player vectors (20 int32 each = 80 bytes per player)
    (when players
      (loop :for pvec :across players
            :do (loop :for i :from 0 :below (min 20 (length pvec))
                      :do (setf offset (write-int32 buffer offset (aref pvec i))))))

    ;; Write NPC vectors (15 int32 each = 60 bytes per NPC)
    (when npcs
      (loop :for nvec :across npcs
            :do (loop :for i :from 0 :below (min 15 (length nvec))
                      :do (setf offset (write-int32 buffer offset (aref nvec i))))))

    ;; Write objects (keyword id as sxhash, x, y, count = 4 int32 = 16 bytes per object)
    ;; Task 5.5: Use zone-object struct accessors (objects are now structs, not plists)
    (when objects
      (dolist (obj objects)
        (let* ((id (zone-object-id obj))
               (id-hash (if (keywordp id) (sxhash id) 0))
               (x (zone-object-x obj))
               (y (zone-object-y obj))
               (count (zone-object-count obj)))
          (setf offset (write-uint32 buffer offset (logand id-hash #xFFFFFFFF)))
          (setf offset (write-int32 buffer offset x))
          (setf offset (write-int32 buffer offset y))
          (setf offset (write-int32 buffer offset count)))))

    ;; Events not included in binary format (requires plist fallback for complex events)
    ;; Future: encode event count and binary event data

    offset))

(defun is-binary-snapshot-p (buffer size)
  "Check if BUFFER contains a binary snapshot (starts with magic bytes)."
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum size))
  (and (>= size 4)
       (= (aref buffer 0) 83)   ; 'S'
       (= (aref buffer 1) 78)   ; 'N'
       (= (aref buffer 2) 65)   ; 'A'
       (= (aref buffer 3) 80))) ; 'P'

(defun decode-snapshot-binary (buffer size)
  "Decode binary snapshot from BUFFER into plist format for apply-game-state.
   Returns plist compatible with deserialize-game-state-compact/delta."
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum size))
  (when (< size 20)  ; Minimum header size
    (return-from decode-snapshot-binary nil))
  ;; Skip magic bytes (already validated by is-binary-snapshot-p)
  (let ((offset 4))
    (declare (type fixnum offset))
    ;; Read version (1)
    (multiple-value-bind (version new-off) (read-uint8 buffer offset)
      (setf offset new-off)
      (when (/= version *binary-snapshot-version*)
        (warn "Binary snapshot version mismatch: got ~d, expected ~d" version *binary-snapshot-version*)
        (return-from decode-snapshot-binary nil)))
    ;; Read format flag (1)
    (multiple-value-bind (format-flag new-off) (read-uint8 buffer offset)
      (setf offset new-off)
      (let ((is-delta (= format-flag 1)))
        ;; Read sequence (4)
        (multiple-value-bind (seq new-off) (read-uint32 buffer offset)
          (setf offset new-off)
          ;; Read zone-id hash (4)
          (multiple-value-bind (zone-hash new-off) (read-uint32 buffer offset)
            (setf offset new-off)
            ;; Read counts (2+2+2)
            (multiple-value-bind (player-count new-off) (read-uint16 buffer offset)
              (setf offset new-off)
              (multiple-value-bind (npc-count new-off) (read-uint16 buffer offset)
                (setf offset new-off)
                (multiple-value-bind (object-count new-off) (read-uint16 buffer offset)
                  (setf offset new-off)
                  ;; Read player vectors
                  (let ((players (make-array player-count)))
                    (dotimes (p player-count)
                      (let ((pvec (make-array 20 :initial-element 0)))
                        (dotimes (i 20)
                          (multiple-value-bind (val new-off) (read-int32 buffer offset)
                            (setf (aref pvec i) val
                                  offset new-off)))
                        (setf (aref players p) pvec)))
                    ;; Read NPC vectors
                    (let ((npcs (make-array npc-count)))
                      (dotimes (n npc-count)
                        (let ((nvec (make-array 15 :initial-element 0)))
                          (dotimes (i 15)
                            (multiple-value-bind (val new-off) (read-int32 buffer offset)
                              (setf (aref nvec i) val
                                    offset new-off)))
                          (setf (aref npcs n) nvec)))
                      ;; Read objects (as id-hash, x, y, count - need reverse lookup)
                      ;; Note: We lose keyword identity, but objects are matched by position
                      (let ((objects nil))
                        (dotimes (o object-count)
                          (multiple-value-bind (id-hash new-off) (read-uint32 buffer offset)
                            (setf offset new-off)
                            (multiple-value-bind (x new-off) (read-int32 buffer offset)
                              (setf offset new-off)
                              (multiple-value-bind (y new-off) (read-int32 buffer offset)
                                (setf offset new-off)
                                (multiple-value-bind (count new-off) (read-int32 buffer offset)
                                  (setf offset new-off)
                                  ;; Use :unknown for id since we can't reverse sxhash
                                  ;; Client will use position for matching
                                  (push (list :id :unknown-binary
                                              :id-hash id-hash
                                              :x x :y y :count count)
                                        objects))))))
                        ;; Build result plist
                        (if is-delta
                            (list :format :delta-v5
                                  :seq seq
                                  :zone-id nil  ; Zone-id not recoverable from hash
                                  :zone-id-hash zone-hash
                                  :changed-players players
                                  :changed-npcs npcs
                                  :objects (nreverse objects))
                            (list :format :compact-v5
                                  :seq seq
                                  :zone-id nil  ; Zone-id not recoverable from hash
                                  :zone-id-hash zone-hash
                                  :players players
                                  :npcs npcs
                                  :objects (nreverse objects)))))))))))))))

(defun receive-net-message (socket buffer)
  ;; Receive a single UDP message if ready; returns message and sender.
  ;; Returns NIL gracefully if server unreachable (CONNECTION-REFUSED-ERROR).
  ;; Phase 3: Detects and decodes binary snapshots when *use-binary-snapshots* enabled.
  (handler-case
      (when (usocket:wait-for-input socket :timeout 0 :ready-only t)
        (multiple-value-bind (recv size host port)
            (usocket:socket-receive socket buffer (length buffer))
          (declare (ignore recv))
          (when (and size (> size 0))
            ;; Phase 3: Check for binary snapshot format
            (if (is-binary-snapshot-p buffer size)
                ;; Binary snapshot - decode and wrap in snapshot message format
                (let ((state (decode-snapshot-binary buffer size)))
                  (if state
                      (values (list :type :snapshot :state state :events nil)
                              (host-to-string host) port)
                      (progn
                        (log-verbose "Failed to decode binary snapshot from ~a:~d (~d bytes)"
                                     (host-to-string host) port size)
                        (values nil (host-to-string host) port))))
                ;; Text format - decode as before
                (let* ((text (octets-to-string buffer size))
                       (message (decode-net-message text)))
                  (unless message
                    (log-verbose "Dropped malformed packet from ~a:~d (~d bytes)"
                                 (host-to-string host)
                                 port
                                 size))
                  (values message (host-to-string host) port))))))
    (usocket:connection-refused-error ()
      ;; Server unreachable - return nil gracefully
      nil)))

;;; Struct definitions used across net-* files

(defstruct (net-client (:constructor %make-net-client))
  ;; Server-side view of a connected client.
  host port player intent last-heard
  authenticated-p       ; T after successful login, NIL before
  account-username      ; Username of logged-in account
  ;; Delta compression (see docs/net.md Prong 2)
  last-acked-seq        ; Sequence number client confirmed receiving
  needs-full-resync     ; T after zone change or reconnect
  ;; Zone tracking (Phase 5) - detect zone changes and trigger resync
  zone-id               ; Last known zone-id for this client (nil until auth)
  ;; Private state (inventory/equipment/stats) owner-only updates
  private-state
  private-retries)

;;; UDP Fragmentation - See docs/net.md Prong 3
(defstruct chunk-buffer
  "Client-side buffer for reassembling fragmented snapshots.
   Phase 3: Uses reusable buffers to avoid per-chunk allocation."
  (seq nil)                    ; Sequence number being assembled
  (total 0 :type fixnum)       ; Expected chunk count
  (chunk-lengths nil)          ; Vector of chunk lengths (reused)
  (chunk-offsets nil)          ; Vector of chunk start offsets (reused)
  (data-buffer nil)            ; Reusable byte buffer for reassembly
  (data-fill 0 :type fixnum)   ; Current fill pointer in data-buffer
  (timestamp 0.0))             ; For timeout detection

;;; Phase 3: Reusable reassembly buffer (avoid per-snapshot allocation)
(defparameter *chunk-reassembly-buffer-size* (* 256 1024)
  "Size of reusable chunk reassembly buffer (256KB default, handles large snapshots).")

(defun ensure-chunk-buffer-storage (buffer max-chunks)
  "Ensure BUFFER has storage for reassembly. Reuses existing if large enough."
  (unless (and (chunk-buffer-data-buffer buffer)
               (>= (length (chunk-buffer-data-buffer buffer)) *chunk-reassembly-buffer-size*))
    (setf (chunk-buffer-data-buffer buffer)
          (make-array *chunk-reassembly-buffer-size*
                      :element-type '(unsigned-byte 8)
                      :initial-element 0)))
  (unless (and (chunk-buffer-chunk-lengths buffer)
               (>= (length (chunk-buffer-chunk-lengths buffer)) max-chunks))
    (setf (chunk-buffer-chunk-lengths buffer)
          (make-array (max 32 max-chunks) :element-type 'fixnum :initial-element 0)
          (chunk-buffer-chunk-offsets buffer)
          (make-array (max 32 max-chunks) :element-type 'fixnum :initial-element 0))))

;;; Client management helpers (used by auth, snapshot, server)

(defun make-net-client (host port player)
  ;; Build a client record with an intent seeded to PLAYER position.
  ;; If player is nil (unauthenticated), creates a default intent at origin.
  (%make-net-client :host host
                    :port port
                    :player player
                    :intent (if player
                                (make-intent :target-x (player-x player)
                                             :target-y (player-y player))
                                (make-intent :target-x 0 :target-y 0))
                    :last-heard 0.0
                    :authenticated-p nil
                    :account-username nil
                    :last-acked-seq nil
                    :needs-full-resync t
                    :private-state nil
                    :private-retries 0))

(defun find-net-client (clients host port)
  ;; Return the matching CLIENT for HOST/PORT, if any.
  (loop :for client :in clients
        :when (and (string= host (net-client-host client))
                   (= port (net-client-port client)))
          :do (return client)))

(defun client-uses-player-p (clients player)
  ;; Return true when PLAYER is already assigned to a CLIENT.
  (loop :for client :in clients
        :when (eq (net-client-player client) player)
          :do (return t)))

(defun find-unassigned-player (game clients)
  ;; Return a player without a client assignment, if any.
  (let ((players (game-players game)))
    (when players
      (loop :for player :across players
            :unless (client-uses-player-p clients player)
              :do (return player)))))

(defun reset-player-for-client (player)
  ;; Clear transient state when binding a player to a client.
  (when player
    (let ((intent (player-intent player)))
      (when intent
        (reset-frame-intent intent)
        (clear-intent-target intent)))
    (clear-player-auto-walk player)
    (setf (player-attacking player) nil
          (player-attack-hit player) nil
          (player-hit-active player) nil
          (player-attack-target-id player) 0
          (player-follow-target-id player) 0)))

(defun add-player-to-game (game player)
  ;; Append PLAYER to GAME and refresh entity list.
  ;; Also inserts player into zone's spatial grid for proximity queries,
  ;; and adds to zone-players cache for O(zone-players) serialization.
  (let* ((players (game-players game))
         (count (if players (length players) 0))
         (new-players (make-array (1+ count))))
    (when players
      (replace new-players players))
    (setf (aref new-players count) player)
    (setf (game-players game) new-players
          (game-entities game)
          (make-entities new-players (game-npcs game)))
    ;; Update player index map - add new entry for the appended player
    (let ((map (or (game-player-index-map game)
                   (setf (game-player-index-map game)
                         (make-hash-table :test 'eql :size 512)))))
      (setf (gethash (player-id player) map) count))
    ;; Insert player into zone's spatial grid and zone-players cache
    (let* ((zone-id (player-zone-id player))
           (zone-state (when zone-id (get-zone-state zone-id))))
      (when zone-state
        ;; Add to zone-players cache (Task 4.1)
        (add-player-to-zone-cache player zone-state)
        ;; Add to spatial grid
        (let ((grid (zone-state-player-grid zone-state)))
          (when grid
            (multiple-value-bind (cx cy)
                (position-to-cell (player-x player) (player-y player)
                                  (spatial-grid-cell-size grid))
              (spatial-grid-insert grid (player-id player) (player-x player) (player-y player))
              (setf (player-grid-cell-x player) cx
                    (player-grid-cell-y player) cy)))))))
  player)

(defun remove-player-from-game (game player)
  "Remove PLAYER from GAME and refresh entity list.
   Also removes player from zone's spatial grid and zone-players cache."
  ;; Remove from spatial grid and zone-players cache first (before player becomes unreachable)
  (let* ((zone-id (player-zone-id player))
         (zone-state (when zone-id (get-zone-state zone-id))))
    (when zone-state
      ;; Remove from zone-players cache (Task 4.1)
      (remove-player-from-zone-cache player zone-state)
      ;; Remove from spatial grid
      (let ((grid (zone-state-player-grid zone-state)))
        (when (and grid (player-grid-cell-x player) (player-grid-cell-y player))
          (spatial-grid-remove grid (player-id player)
                               (player-grid-cell-x player)
                               (player-grid-cell-y player))
          (setf (player-grid-cell-x player) nil
                (player-grid-cell-y player) nil)))))
  (let* ((players (game-players game))
         (filtered (remove player players :test #'eq)))
    (when (< (length filtered) (length players))
      ;; Player was found and removed
      (setf (game-players game) filtered
            (game-entities game)
            (make-entities filtered (game-npcs game)))
      ;; Rebuild entire index map - indices shift after removal
      (rebuild-player-index-map game)
      t)))

(defun register-net-client (game clients host port &key (timestamp 0.0))
  ;; Ensure a client exists for HOST/PORT and return updated list.
  ;; Note: Does NOT spawn a player - players are spawned during authentication
  (declare (ignore game))
  (let ((client (find-net-client clients host port)))
    (if client
        (progn
          (setf (net-client-last-heard client) timestamp)
          (values client clients nil))
        (let ((client (make-net-client host port nil)))
          (log-verbose "New client registered: ~a:~d (unauthenticated)" host port)
          (setf (net-client-last-heard client) timestamp)
          (values client (cons client clients) t)))))

;;; Intent plist conversion and validation helpers

(defun intent->plist (intent)
  ;; Convert INTENT into a plist suitable for network transport.
  (when intent
    (list :move-dx (intent-move-dx intent)
          :move-dy (intent-move-dy intent)
          :face-dx (intent-face-dx intent)
          :face-dy (intent-face-dy intent)
          :target-x (intent-target-x intent)
          :target-y (intent-target-y intent)
          :target-raw-x (intent-target-raw-x intent)
          :target-raw-y (intent-target-raw-y intent)
          :target-clamped-p (intent-target-clamped-p intent)
          :target-active (intent-target-active intent)
          :attack (intent-attack intent)
          :run-toggle (intent-run-toggle intent)
          :requested-attack-target-id (intent-requested-attack-target-id intent)
          :requested-follow-target-id (intent-requested-follow-target-id intent)
          :requested-pickup-target-id (intent-requested-pickup-target-id intent)
          :requested-pickup-tx (intent-requested-pickup-tx intent)
          :requested-pickup-ty (intent-requested-pickup-ty intent)
          :requested-drop-item-id (intent-requested-drop-item-id intent)
          :requested-drop-count (intent-requested-drop-count intent)
          :requested-drop-slot-index (intent-requested-drop-slot-index intent)
          :requested-swap-slot-a (intent-requested-swap-slot-a intent)
          :requested-swap-slot-b (intent-requested-swap-slot-b intent)
          :requested-chat-message (intent-requested-chat-message intent)
          :requested-unstuck (intent-requested-unstuck intent))))

(defun %float-or (value default)
  ;; Return VALUE as float if it's a number, otherwise DEFAULT.
  (if (numberp value)
      (float value 1.0)
      (float default 1.0)))

(defun %int-or (value default)
  ;; Return VALUE as integer if it's an integer, otherwise DEFAULT.
  ;; Security: Rejects non-integer values (strings, lists, floats) from malicious clients.
  (if (integerp value)
      value
      default))

(defun %nonneg-int-or (value default)
  ;; Return VALUE as non-negative integer, otherwise DEFAULT.
  (let ((int (%int-or value default)))
    (if (and (integerp int) (>= int 0))
        int
        default)))

(defun %symbol-or (value default)
  ;; Return VALUE if it is a symbol, otherwise DEFAULT.
  (if (symbolp value)
      value
      default))

(defun %sanitize-chat-message (value)
  ;; Sanitize chat message: ensure string and enforce length limit.
  ;; Security: Prevents oversized messages from malicious clients.
  (when (stringp value)
    (if (> (length value) *chat-max-length*)
        (subseq value 0 *chat-max-length*)
        value)))

(defun %clamp-direction (value)
  ;; Clamp movement direction to [-1.0, 1.0] range.
  ;; Security: Prevents speed hacks via large move-dx/move-dy values.
  ;; Client sends normalized direction (-1 to 1), server multiplies by speed.
  (clamp (%float-or value 0.0) -1.0 1.0))

(defun apply-intent-plist (intent plist)
  ;; Apply PLIST values to INTENT in place.
  ;; Security: All values are validated/sanitized to prevent type confusion attacks.
  (let* ((pickup-id (%symbol-or (getf plist :requested-pickup-target-id) nil))
         (pickup-tx (%nonneg-int-or (getf plist :requested-pickup-tx) nil))
         (pickup-ty (%nonneg-int-or (getf plist :requested-pickup-ty) nil))
         (drop-id (%symbol-or (getf plist :requested-drop-item-id) nil))
         (drop-count (%int-or (getf plist :requested-drop-count) 0)))
    (when pickup-id
      (log-verbose "RECV-INTENT: pickup id=~a tx=~a ty=~a"
                   pickup-id
                   pickup-tx
                   pickup-ty))
    (when drop-id
      (log-verbose "RECV-INTENT: drop item=~a count=~a"
                   drop-id
                   drop-count))
    (when (and intent plist)
      (setf (intent-move-dx intent) (%clamp-direction (getf plist :move-dx))
            (intent-move-dy intent) (%clamp-direction (getf plist :move-dy))
            (intent-face-dx intent) (%float-or (getf plist :face-dx) 0.0)
            (intent-face-dy intent) (%float-or (getf plist :face-dy) 0.0)
            (intent-target-x intent) (%float-or (getf plist :target-x) 0.0)
            (intent-target-y intent) (%float-or (getf plist :target-y) 0.0)
            (intent-target-raw-x intent) (%float-or (getf plist :target-raw-x) (intent-target-x intent))
            (intent-target-raw-y intent) (%float-or (getf plist :target-raw-y) (intent-target-y intent))
            (intent-target-clamped-p intent) (and (getf plist :target-clamped-p) t)
            (intent-target-active intent) (getf plist :target-active nil)
            (intent-attack intent) (getf plist :attack nil)
            (intent-run-toggle intent) (getf plist :run-toggle nil)
            (intent-requested-attack-target-id intent)
            (%int-or (getf plist :requested-attack-target-id) 0)
            (intent-requested-follow-target-id intent)
            (%int-or (getf plist :requested-follow-target-id) 0)
            (intent-requested-pickup-target-id intent)
            pickup-id
            (intent-requested-pickup-tx intent)
            pickup-tx
            (intent-requested-pickup-ty intent)
            pickup-ty
            (intent-requested-drop-item-id intent)
            drop-id
            (intent-requested-drop-count intent)
            drop-count
            (intent-requested-drop-slot-index intent)
            (%nonneg-int-or (getf plist :requested-drop-slot-index) nil)
            (intent-requested-swap-slot-a intent)
            (%int-or (getf plist :requested-swap-slot-a) nil)
            (intent-requested-swap-slot-b intent)
            (%int-or (getf plist :requested-swap-slot-b) nil)
            (intent-requested-chat-message intent)
            (%sanitize-chat-message (getf plist :requested-chat-message))
            (intent-requested-unstuck intent)
            (and (getf plist :requested-unstuck) t)))
    intent))

;;; Combat event conversion

(defun combat-event->plist (event)
  ;; Convert a combat EVENT to a serializable plist.
  (when event
    (list :type (combat-event-type event)
          :text (combat-event-text event))))

(defun plist->combat-event (plist)
  ;; Convert a plist back into a combat event.
  (when plist
    (make-combat-event :type (getf plist :type)
                       :text (getf plist :text))))

;;; Shared client/server helpers

(defun apply-client-intents (clients)
  ;; Copy each client intent into its assigned player intent.
  ;; After copying, clear one-shot intent fields so they don't repeat.
  (dolist (client clients)
    (let* ((player (net-client-player client))
           (server-intent (and player (player-intent player)))
           (client-intent (net-client-intent client)))
      (when (and client-intent (intent-requested-drop-item-id client-intent))
        (log-verbose "APPLY-CLIENT-INTENTS: client-player=~a (obj ~a)"
                     (and player (player-id player)) player))
      (apply-client-intent server-intent client-intent)
      (when client-intent
        ;; Clear one-shot requests after processing
        (when (intent-requested-chat-message client-intent)
          (clear-requested-chat-message client-intent))
        (when (intent-requested-pickup-target-id client-intent)
          (clear-requested-pickup-target client-intent))
        (when (intent-requested-drop-item-id client-intent)
          (clear-requested-drop-item client-intent))
        (when (intent-requested-swap-slot-a client-intent)
          (clear-requested-inventory-swap client-intent))))))

(defun reconcile-net-clients (game clients)
  ;; Rebind clients to players that exist in the current game state.
  ;; Only reconciles authenticated clients; unauthenticated clients stay as nil.
  (let ((players (game-players game)))
    (dolist (client clients)
      (when (net-client-authenticated-p client)
        (let* ((current (net-client-player client))
               (current-id (and current (player-id current)))
               (matching (and current-id (find-player-by-id players current-id))))
          (cond
            ((and matching (not (eq matching current)))
             (setf (net-client-player client) matching))
            ((null matching)
             (let ((assigned (or (find-unassigned-player game clients)
                                 (add-player-to-game
                                  game
                                  (spawn-player-at-world (game-world game)
                                                         (game-id-source game))))))
               (reset-player-for-client assigned)
               (setf (net-client-player client) assigned))))))))
  clients)

(defun send-snapshot-bytes (socket octets size host port)
  "Send pre-encoded snapshot bytes to a client. Returns T on success."
  (handler-case
      (progn
        (usocket:socket-send socket octets size :host host :port port)
        t)
    (error (e)
      (log-verbose "Failed to send snapshot to ~a:~d: ~a" host port e)
      nil)))

;;; Private state helpers (used by auth integration and snapshot sending)

(defun queue-private-state (client player &key (retries *private-state-retries*))
  ;; Queue owner-only state updates (inventory/equipment/stats) for CLIENT.
  (when (and client player)
    (setf (net-client-private-state client) (serialize-player-private player)
          (net-client-private-retries client) (max 1 retries))))
