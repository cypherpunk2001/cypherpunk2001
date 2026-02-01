;; NOTE: If you change behavior here, update docs/net.md :)
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

(defparameter *active-sessions* (make-hash-table :test 'equal :size 512)
  "Map of username (lowercase) -> net-client for logged-in accounts.")

#+sbcl
(defvar *session-lock* (sb-thread:make-mutex :name "session-lock")
  "Mutex protecting *active-sessions* for atomic check-and-set operations.")

(defmacro with-session-lock (&body body)
  "Execute BODY with *session-lock* held for atomic session operations."
  #+sbcl
  `(sb-thread:with-mutex (*session-lock*)
     ,@body)
  #-sbcl
  `(progn ,@body))

(defun session-clear-all ()
  "Clear all active sessions. Called during server shutdown/startup for clean state."
  (with-session-lock
    (clrhash *active-sessions*)))

(defun session-try-register (username client)
  "Atomically check if USERNAME is available and register CLIENT.
   Returns T if registered, NIL if already logged in."
  (with-session-lock
    (let ((key (string-downcase username)))
      (if (gethash key *active-sessions*)
          nil
          (progn
            (setf (gethash key *active-sessions*) client)
            t)))))

(defun session-unregister (username)
  "Atomically remove USERNAME from active sessions."
  (with-session-lock
    (remhash (string-downcase username) *active-sessions*)))

(defun session-get (username)
  "Get the client for USERNAME if logged in."
  (with-session-lock
    (gethash (string-downcase username) *active-sessions*)))

(defparameter *client-timeout-seconds* 30.0
  "Seconds of inactivity before a client is considered disconnected.")

;;;; Auth Rate Limiting

(defparameter *auth-max-attempts* 5
  "Maximum failed auth attempts before lockout.")

(defparameter *auth-lockout-seconds* 300.0
  "Lockout duration in seconds after max failed attempts (5 minutes).")

(defparameter *auth-attempt-window* 60.0
  "Time window in seconds to count failed attempts.")

(defstruct auth-rate-entry
  "Tracks auth attempts for rate limiting."
  (attempts 0 :type fixnum)
  (first-attempt-time 0.0 :type single-float)
  (lockout-until 0.0 :type single-float))

(defparameter *auth-rate-limits* (make-hash-table :test 'equal :size 256)
  "Map of IP address -> auth-rate-entry for rate limiting.")

;;; Auth Metrics (Step 6) - Thread-safe atomic counters via struct slots
;;; sb-ext:atomic-incf requires CAS-able places: struct slots with :type sb-ext:word.
(defstruct (auth-metrics (:constructor make-auth-metrics))
  "Atomic counters for auth throughput monitoring."
  (queued 0 :type sb-ext:word)
  (processed 0 :type sb-ext:word)
  (expired 0 :type sb-ext:word)
  (rejected-busy 0 :type sb-ext:word)
  (success 0 :type sb-ext:word)
  (fail 0 :type sb-ext:word))

(defvar *auth-metrics* (make-auth-metrics)
  "Global auth metrics counters (atomic on SBCL).")

;;; Convenience accessors matching old *auth-metric-* names
(defmacro auth-metric-incf (slot)
  "Atomically increment an auth metric counter (SBCL), or plain incf otherwise."
  #+sbcl `(sb-ext:atomic-incf (,(intern (format nil "AUTH-METRICS-~a" slot)) *auth-metrics*))
  #-sbcl `(incf (,(intern (format nil "AUTH-METRICS-~a" slot)) *auth-metrics*)))

;;; Thread-safe auth rate limit access
#+sbcl
(defvar *auth-rate-limits-lock* (sb-thread:make-mutex :name "auth-rate-limits-lock")
  "Mutex protecting *auth-rate-limits* for thread-safe access.")

(defmacro with-auth-rate-limits-lock (&body body)
  "Execute BODY with *auth-rate-limits-lock* held for thread-safe rate limit operations."
  #+sbcl
  `(sb-thread:with-mutex (*auth-rate-limits-lock*)
     ,@body)
  #-sbcl
  `(progn ,@body))

(defun auth-rate-check (host current-time)
  "Check if HOST is rate-limited. Returns T if allowed, NIL if blocked.
   Thread-safe: protects rate limit table from concurrent access."
  (with-auth-rate-limits-lock
    (let ((entry (gethash host *auth-rate-limits*)))
      (cond
        ;; No entry - allowed
        ((null entry) t)
        ;; Currently locked out
        ((> (auth-rate-entry-lockout-until entry) current-time)
         nil)
        ;; Lockout expired - reset and allow
        ((> (auth-rate-entry-lockout-until entry) 0.0)
         (setf (auth-rate-entry-attempts entry) 0
               (auth-rate-entry-lockout-until entry) 0.0
               (auth-rate-entry-first-attempt-time entry) 0.0)
         t)
        ;; Window expired - reset attempts and allow
        ((> (- current-time (auth-rate-entry-first-attempt-time entry))
            *auth-attempt-window*)
         (setf (auth-rate-entry-attempts entry) 0
               (auth-rate-entry-first-attempt-time entry) 0.0)
         t)
        ;; Under limit - allowed
        (t t)))))

(defun auth-rate-record-failure (host current-time)
  "Record a failed auth attempt for HOST. Returns T if now locked out.
   Thread-safe: protects rate limit table from concurrent access."
  (with-auth-rate-limits-lock
    (let ((entry (gethash host *auth-rate-limits*)))
      (unless entry
        (setf entry (make-auth-rate-entry)
              (gethash host *auth-rate-limits*) entry))
      ;; Reset if window expired
      (when (and (> (auth-rate-entry-first-attempt-time entry) 0.0)
                 (> (- current-time (auth-rate-entry-first-attempt-time entry))
                    *auth-attempt-window*))
        (setf (auth-rate-entry-attempts entry) 0
              (auth-rate-entry-first-attempt-time entry) 0.0))
      ;; Record attempt
      (when (zerop (auth-rate-entry-first-attempt-time entry))
        (setf (auth-rate-entry-first-attempt-time entry) current-time))
      (incf (auth-rate-entry-attempts entry))
      ;; Check if lockout triggered
      (when (>= (auth-rate-entry-attempts entry) *auth-max-attempts*)
        (setf (auth-rate-entry-lockout-until entry)
              (+ current-time *auth-lockout-seconds*))
        (warn "Rate limit: ~a locked out for ~ds after ~d failed attempts"
              host (round *auth-lockout-seconds*) (auth-rate-entry-attempts entry))
        t))))

(defun auth-rate-record-success (host)
  "Clear rate limit tracking for HOST after successful auth.
   Thread-safe: protects rate limit table from concurrent access."
  (with-auth-rate-limits-lock
    (remhash host *auth-rate-limits*)))

(defun auth-rate-clear-all ()
  "Clear all rate limit state. For testing only.
   Thread-safe: protects rate limit table from concurrent access."
  (with-auth-rate-limits-lock
    (clrhash *auth-rate-limits*)))

;;;; Auth Replay Protection

(defparameter *auth-timestamp-window* 60
  "Maximum age in seconds for auth timestamps. Older messages rejected.")

(defparameter *auth-nonce-cache* (make-hash-table :test 'equal :size 1024)
  "Cache of recently seen nonces to prevent replay attacks.")

(defparameter *auth-nonce-cleanup-interval* 120
  "Seconds between nonce cache cleanup runs.")

(defvar *auth-nonce-last-cleanup* 0
  "Last time nonce cache was cleaned up.")

;;; Thread-safe nonce cache access
#+sbcl
(defvar *auth-nonce-lock* (sb-thread:make-mutex :name "auth-nonce-lock")
  "Mutex protecting *auth-nonce-cache* for thread-safe access.")

(defmacro with-auth-nonce-lock (&body body)
  "Execute BODY with *auth-nonce-lock* held for thread-safe nonce operations."
  #+sbcl
  `(sb-thread:with-mutex (*auth-nonce-lock*)
     ,@body)
  #-sbcl
  `(progn ,@body))

(defun auth-nonce-cleanup (current-time)
  "Remove expired nonces from cache.
   Internal: must be called with *auth-nonce-lock* held."
  (when (> (- current-time *auth-nonce-last-cleanup*) *auth-nonce-cleanup-interval*)
    (let ((cutoff (- current-time *auth-timestamp-window* 10)))
      (maphash (lambda (nonce timestamp)
                 (when (< timestamp cutoff)
                   (remhash nonce *auth-nonce-cache*)))
               *auth-nonce-cache*))
    (setf *auth-nonce-last-cleanup* current-time)))

(defun auth-check-replay (encrypted-payload timestamp)
  "Check if this auth request is a replay. Returns T if valid, NIL if replay.
   ENCRYPTED-PAYLOAD is used as the nonce (unique per request).
   TIMESTAMP is the claimed time of the request.
   Thread-safe: protects nonce cache from concurrent access."
  (with-auth-nonce-lock
    (let* ((current-time (get-universal-time))
           (age (- current-time timestamp)))
      ;; Cleanup old nonces periodically
      (auth-nonce-cleanup current-time)
      (cond
        ;; Timestamp too old
        ((> age *auth-timestamp-window*)
         (warn "Auth replay check: timestamp too old (~d seconds)" age)
         nil)
        ;; Timestamp in future (clock skew tolerance of 5 seconds)
        ((< age -5)
         (warn "Auth replay check: timestamp in future (~d seconds)" (- age))
         nil)
        ;; Already seen this nonce
        ((gethash encrypted-payload *auth-nonce-cache*)
         (warn "Auth replay check: duplicate nonce detected")
         nil)
        ;; Valid - record nonce
        (t
         (setf (gethash encrypted-payload *auth-nonce-cache*) current-time)
         t)))))

;;;; Auth Worker Thread (Non-Blocking Login)
;;;;
;;;; Auth requests (register/login) are queued and processed on a worker thread
;;;; to avoid blocking the main game loop with DB operations and retries.

(defstruct auth-queue
  "Thread-safe queue for auth work items.
   Uses two-list FIFO: push-list for producers, pop-list for consumers.
   O(1) amortized push/pop instead of O(n) last/butlast."
  (push-list nil :type list)   ; Producers push here
  (pop-list nil :type list)    ; Consumers pop from here
  (count 0 :type fixnum)       ; Current depth for backpressure
  (max-depth 200 :type fixnum) ; Max items before rejecting (0 = unlimited)
  (lock nil)
  (condvar nil))

(defstruct auth-request
  "Request to process on auth worker thread."
  (type nil :type (member :register :login nil))
  (host nil :type (or null string))
  (port 0 :type integer)
  (username nil :type (or null string))
  (password nil :type (or null string))
  (client nil)
  (timestamp 0.0 :type single-float)
  (stop-signal nil :type boolean))  ; Sentinel to stop worker

(defstruct auth-result
  "Result from auth worker to integrate on main thread."
  (type nil :type (member :register :login nil))
  (success nil :type boolean)
  (host nil :type (or null string))
  (port 0 :type integer)
  (username nil :type (or null string))
  (client nil)
  (player nil)
  (player-id nil :type (or null integer))
  (error-reason nil)  ; :bad-credentials, :username-taken, etc.
  (zone-id nil))

(defun make-auth-queue-instance (&key (max-depth *auth-queue-max-depth*))
  "Create a new thread-safe auth queue with bounded depth."
  #+sbcl
  (make-auth-queue :push-list nil
                   :pop-list nil
                   :count 0
                   :max-depth max-depth
                   :lock (sb-thread:make-mutex :name "auth-queue-lock")
                   :condvar (sb-thread:make-waitqueue :name "auth-queue-condvar"))
  #-sbcl
  (make-auth-queue :push-list nil :pop-list nil :count 0 :max-depth max-depth
                   :lock nil :condvar nil))

(defun auth-queue-push (queue item)
  "Thread-safe push ITEM to QUEUE (unbounded). Signals waiting threads."
  #+sbcl
  (sb-thread:with-mutex ((auth-queue-lock queue))
    (push item (auth-queue-push-list queue))
    (incf (auth-queue-count queue))
    (sb-thread:condition-notify (auth-queue-condvar queue)))
  #-sbcl
  (progn
    (push item (auth-queue-push-list queue))
    (incf (auth-queue-count queue))))

(defun auth-queue-try-push (queue item)
  "Thread-safe push ITEM if queue is below max depth. Returns T on success, NIL if full."
  #+sbcl
  (sb-thread:with-mutex ((auth-queue-lock queue))
    (when (or (zerop (auth-queue-max-depth queue))
              (< (auth-queue-count queue) (auth-queue-max-depth queue)))
      (push item (auth-queue-push-list queue))
      (incf (auth-queue-count queue))
      (sb-thread:condition-notify (auth-queue-condvar queue))
      t))
  #-sbcl
  (when (or (zerop (auth-queue-max-depth queue))
            (< (auth-queue-count queue) (auth-queue-max-depth queue)))
    (push item (auth-queue-push-list queue))
    (incf (auth-queue-count queue))
    t))

(defun auth-queue-pop-blocking (queue)
  "Block until an item is available, then pop and return it (FIFO).
   O(1) amortized via two-list technique. Used by worker thread."
  #+sbcl
  (sb-thread:with-mutex ((auth-queue-lock queue))
    (loop :while (and (null (auth-queue-pop-list queue))
                      (null (auth-queue-push-list queue)))
          :do (sb-thread:condition-wait (auth-queue-condvar queue)
                                         (auth-queue-lock queue)))
    ;; Transfer push-list to pop-list if pop-list is empty
    (when (null (auth-queue-pop-list queue))
      (setf (auth-queue-pop-list queue) (nreverse (auth-queue-push-list queue)))
      (setf (auth-queue-push-list queue) nil))
    (let ((item (pop (auth-queue-pop-list queue))))
      (decf (auth-queue-count queue))
      item))
  #-sbcl
  (progn
    (loop :while (and (null (auth-queue-pop-list queue))
                      (null (auth-queue-push-list queue)))
          :do (sleep 0.01))
    (when (null (auth-queue-pop-list queue))
      (setf (auth-queue-pop-list queue) (nreverse (auth-queue-push-list queue)))
      (setf (auth-queue-push-list queue) nil))
    (let ((item (pop (auth-queue-pop-list queue))))
      (decf (auth-queue-count queue))
      item)))

(defun auth-queue-drain-nonblocking (queue)
  "Return all items from QUEUE without blocking. For main thread.
   Returns items in FIFO order (oldest first)."
  #+sbcl
  (sb-thread:with-mutex ((auth-queue-lock queue))
    (let* ((popped (auth-queue-pop-list queue))
           (pushed (nreverse (auth-queue-push-list queue)))
           (items (nconc popped pushed)))
      (setf (auth-queue-push-list queue) nil)
      (setf (auth-queue-pop-list queue) nil)
      (setf (auth-queue-count queue) 0)
      items))
  #-sbcl
  (let* ((popped (auth-queue-pop-list queue))
         (pushed (nreverse (auth-queue-push-list queue)))
         (items (nconc popped pushed)))
    (setf (auth-queue-push-list queue) nil)
    (setf (auth-queue-pop-list queue) nil)
    (setf (auth-queue-count queue) 0)
    items))

(defun auth-queue-depth (queue)
  "Return current queue depth. Thread-safe."
  #+sbcl
  (sb-thread:with-mutex ((auth-queue-lock queue))
    (auth-queue-count queue))
  #-sbcl
  (auth-queue-count queue))

(defun process-register-async (request world id-source)
  "Process registration on worker thread. Returns auth-result.
   Step 10: Reordered flow - spawn player first, then pipelined account creation.
   Reduces 4-5 Redis connections to 1 per registration."
  (let* ((username (auth-request-username request))
         (password (auth-request-password request))
         (host (auth-request-host request))
         (port (auth-request-port request))
         (client (auth-request-client request))
         (timestamp (auth-request-timestamp request)))
    (declare (ignorable timestamp))
    ;; Step 9: Rate limit check removed from worker - main thread is authoritative gate.
    ;; Step 10: Spawn player FIRST to get character-id for pipelined account creation
    (handler-case
        (let* ((player (handler-case
                           (spawn-player-at-world world id-source)
                         (error (e)
                           (warn "Registration failed: ID allocation error for ~a: ~a" username e)
                           (return-from process-register-async
                             (make-auth-result :type :register
                                               :success nil
                                               :host host
                                               :port port
                                               :username username
                                               :client client
                                               :error-reason :internal-error))))))
          (unless player
            (return-from process-register-async
              (make-auth-result :type :register
                                :success nil
                                :host host
                                :port port
                                :username username
                                :client client
                                :error-reason :internal-error)))
          (let* ((player-id (player-id player))
                 (zone (world-zone world))
                 (zone-id (and zone (zone-id zone))))
            ;; Step 10: Pipelined account creation - EXISTS + hash + SET + RENAME in 1 connection
            ;; db-create-account-pipelined returns:
            ;;   T              = success
            ;;   :USERNAME-TAKEN = username already exists (not an error)
            ;;   signals        = infra error (with-retry-exponential retries, returns NIL if exhausted)
            (let ((result (with-retry-exponential
                              (r (lambda ()
                                   (db-create-account-pipelined username password player-id))
                               :max-retries 3
                               :initial-delay 50
                               :max-delay 200)
                            r)))
              (cond
                ((eq result t)
                 ;; Account created with character-id already linked
                 (session-try-register username client)
                 ;; Step 5: Register DB session (ownership, online set, leaderboards) in worker
                 (register-player-session-db player-id player)
                 (make-auth-result :type :register
                                   :success t
                                   :host host
                                   :port port
                                   :username username
                                   :client client
                                   :player player
                                   :player-id player-id
                                   :zone-id zone-id))
                ((eq result :username-taken)
                 ;; Username taken (player ID wasted but IDs are cheap)
                 (log-verbose "Registration failed from ~a:~d - username ~a already taken"
                              host port username)
                 (make-auth-result :type :register
                                   :success nil
                                   :host host
                                   :port port
                                   :username username
                                   :client client
                                   :error-reason :username-taken))
                (t
                 ;; NIL = retries exhausted (infra failure)
                 (warn "Registration failed from ~a:~d - infrastructure error for ~a"
                       host port username)
                 (make-auth-result :type :register
                                   :success nil
                                   :host host
                                   :port port
                                   :username username
                                   :client client
                                   :error-reason :internal-error))))))
      (error (e)
        (warn "Registration error for ~a: ~a" username e)
        (auth-rate-record-failure host timestamp)
        (make-auth-result :type :register
                          :success nil
                          :host host
                          :port port
                          :username username
                          :client client
                          :error-reason :internal-error)))))

(defun process-login-async (request world id-source)
  "Process login on worker thread. Returns auth-result.
   Step 10: Uses db-verify-and-load-account to load account once instead of twice."
  (let* ((username (auth-request-username request))
         (password (auth-request-password request))
         (host (auth-request-host request))
         (port (auth-request-port request))
         (client (auth-request-client request))
         (timestamp (auth-request-timestamp request))
         (zone (world-zone world))
         (zone-id (and zone (zone-id zone))))
    (declare (ignorable timestamp zone-id))
    ;; Step 9: Rate limit check removed from worker - main thread is authoritative gate.
    (handler-case
        ;; Step 10: Verify credentials AND get character-id in one account load
        (multiple-value-bind (character-id verified-p)
            (with-retry-exponential
                (result (lambda ()
                          (multiple-value-list (db-verify-and-load-account username password)))
                 :max-retries 3
                 :initial-delay 50
                 :max-delay 200)
              (values (first result) (second result)))
          (cond
            ;; Bad credentials or account not found
            ((not verified-p)
             (auth-rate-record-failure host timestamp)
             (log-verbose "Login failed from ~a:~d - bad credentials for ~a" host port username)
             (make-auth-result :type :login
                               :success nil
                               :host host
                               :port port
                               :username username
                               :client client
                               :error-reason :bad-credentials))
            ;; Check session availability (already thread-safe)
            ((not (session-try-register username client))
             (log-verbose "Login rejected: session-try-register failed for ~a" username)
             (make-auth-result :type :login
                               :success nil
                               :host host
                               :port port
                               :username username
                               :client client
                               :error-reason :already-logged-in))
            ;; Credentials valid, session available - load or create character
            (t
             (let ((player nil))
             (cond
               ;; Existing character - load from DB with Phase 6 validation
               (character-id
                ;; Claim session ownership FIRST (required by db-load-player-validated)
                (unless (claim-session-ownership character-id)
                  (log-verbose "Login rejected: claim-session-ownership failed for player ~d (Redis ownership conflict)"
                               character-id)
                  (session-unregister username)
                  (return-from process-login-async
                    (make-auth-result :type :login
                                      :success nil
                                      :host host
                                      :port port
                                      :username username
                                      :client client
                                      :error-reason :already-logged-in)))
                ;; Load with 4-outcome validation
                (multiple-value-bind (loaded-player loaded-zone-id action)
                    (with-retry-exponential
                        (result (lambda ()
                                  (multiple-value-list (db-load-player-validated character-id)))
                         :max-retries 3
                         :initial-delay 100
                         :max-delay 300)
                      (values (first result) (second result) (third result)))
                  (case action
                    ((:ok :clamp)
                     ;; Normal load or corrected data - proceed
                     (when (eq action :clamp)
                       (log-verbose "Player ~a loaded with corrections" character-id))
                     (setf player loaded-player)
                     (log-verbose "Loaded existing character ~d for account ~a" character-id username))
                    (:quarantine
                     ;; Account needs admin repair - reject login
                     (release-session-ownership character-id)
                     (session-unregister username)
                     (return-from process-login-async
                       (make-auth-result :type :login
                                         :success nil
                                         :host host
                                         :port port
                                         :username username
                                         :client client
                                         :error-reason :account-quarantined)))
                    (:reject
                     ;; Dangerous data - reject login
                     (release-session-ownership character-id)
                     (session-unregister username)
                     (return-from process-login-async
                       (make-auth-result :type :login
                                         :success nil
                                         :host host
                                         :port port
                                         :username username
                                         :client client
                                         :error-reason :data-corrupted)))
                    ((:not-found nil)
                     ;; No character data - should not happen for existing account
                     (release-session-ownership character-id)
                     (session-unregister username)
                     (return-from process-login-async
                       (make-auth-result :type :login
                                         :success nil
                                         :host host
                                         :port port
                                         :username username
                                         :client client
                                         :error-reason :load-failed))))))
               ;; No character yet - spawn new one
               (t
                (handler-case
                    (setf player (spawn-player-at-world world id-source))
                  (error (e)
                    ;; ID allocation failed - return login failure
                    (warn "Login failed: ID allocation error for ~a: ~a" username e)
                    (session-unregister username)
                    (return-from process-login-async
                      (make-auth-result :type :login
                                        :success nil
                                        :host host
                                        :port port
                                        :username username
                                        :client client
                                        :error-reason :internal-error))))
                (unless player
                  (session-unregister username)
                  (return-from process-login-async
                    (make-auth-result :type :login
                                      :success nil
                                      :host host
                                      :port port
                                      :username username
                                      :client client
                                      :error-reason :internal-error)))
                (let ((linked (with-retry-exponential
                                  (set-result (lambda () (db-set-character-id username (player-id player)))
                                   :max-retries 3
                                   :initial-delay 50
                                   :max-delay 200
                                   :on-final-fail (lambda (e)
                                                    (warn "Failed to set character-id for ~a: ~a" username e)))
                                set-result)))
                  (unless linked
                    (warn "Login rollback: unable to link character for ~a" username)
                    (session-unregister username)
                    (return-from process-login-async
                      (make-auth-result :type :login
                                        :success nil
                                        :host host
                                        :port port
                                        :username username
                                        :client client
                                        :error-reason :internal-error)))
                  (log-verbose "Created new character ~d for account ~a" (player-id player) username))))
             ;; Phase 2.5: Clamp nil/unknown zone-id to starting zone
             ;; Use player's zone-id (not global world zone) and validate it exists
             (let ((player-zone (player-zone-id player)))
               (when (null player-zone)
                 (warn "Player ~d has nil zone-id, clamping to ~a"
                       (player-id player) *starting-zone-id*)
                 (setf (player-zone-id player) *starting-zone-id*)
                 (setf player-zone *starting-zone-id*)
                 (mark-player-dirty (player-id player)))  ; Persist the fix
               ;; Step 5: Register DB session (online set, leaderboards) in worker
               ;; Ownership already claimed above for existing characters;
               ;; for new characters, claim it now
               (unless character-id
                 (claim-session-ownership (player-id player)))
               (register-player-session-db (player-id player) player)
               ;; Return success with player's zone-id (now guaranteed non-nil)
               (make-auth-result :type :login
                                 :success t
                                 :host host
                                 :port port
                                 :username username
                                 :client client
                                 :player player
                                 :player-id (player-id player)
                                 :zone-id player-zone))))))
      (error (e)
        (warn "Login error for ~a: ~a" username e)
        ;; Clean up session registration on error
        (session-unregister username)
        (auth-rate-record-failure host timestamp)
        (make-auth-result :type :login
                          :success nil
                          :host host
                          :port port
                          :username username
                          :client client
                          :error-reason :internal-error)))))

(defun auth-worker-loop (request-queue result-queue game)
  "Main loop for auth worker thread.
   Blocks on request-queue, processes DB operations, pushes to result-queue.
   Includes stale request expiry: requests older than *auth-request-max-age* are
   skipped to avoid wasting worker time on clients that have already timed out.
   Step 10c: Opens a persistent Redis connection per worker (if Redis backend)."
  (let ((world (game-world game))
        (id-source (game-id-source game)))
    (flet ((worker-main-loop ()
    (loop
      (let ((request (auth-queue-pop-blocking request-queue)))
        ;; Check for stop signal
        (when (auth-request-stop-signal request)
          (log-verbose "Auth worker received stop signal, exiting")
          (return))
        ;; Step 4: Check if request is stale (client likely timed out)
        (let* ((now-rt (/ (get-internal-real-time)
                          (float internal-time-units-per-second 1.0d0)))
               (age (- now-rt (auth-request-timestamp request))))
          (if (> age (float *auth-request-max-age* 1.0d0))
              ;; Stale - skip processing, return expired result
              (progn
                (auth-metric-incf expired)
                (auth-queue-push result-queue
                  (make-auth-result :type (auth-request-type request)
                                    :success nil
                                    :host (auth-request-host request)
                                    :port (auth-request-port request)
                                    :username (auth-request-username request)
                                    :client (auth-request-client request)
                                    :error-reason :request-expired)))
              ;; Fresh - process normally
              (let ((result (handler-case
                                (case (auth-request-type request)
                                  (:register (process-register-async request world id-source))
                                  (:login (process-login-async request world id-source))
                                  (otherwise
                                   (warn "Auth worker: unknown request type ~a" (auth-request-type request))
                                   nil))
                              (error (e)
                                (warn "Auth worker: unhandled error processing ~a: ~a"
                                      (auth-request-type request) e)
                                (make-auth-result :type (auth-request-type request)
                                                  :success nil
                                                  :host (auth-request-host request)
                                                  :port (auth-request-port request)
                                                  :username (auth-request-username request)
                                                  :client (auth-request-client request)
                                                  :error-reason :internal-error)))))
                (auth-metric-incf processed)
                ;; Push result for main thread to integrate
                (when result
                  (auth-queue-push result-queue result)))))))))
      ;; Step 10c: Wrap in persistent Redis connection if using Redis backend
      (if (and *storage* (typep *storage* 'redis-storage))
          (redis:with-persistent-connection (:host (redis-storage-host *storage*)
                                             :port (redis-storage-port *storage*))
            (worker-main-loop))
          (worker-main-loop)))))

(defun start-auth-worker (request-queue result-queue game &key (name "auth-worker"))
  "Start auth worker thread. Returns thread object."
  #+sbcl
  ;; Capture special variables in closure to ensure visibility in child thread
  ;; (works around SBCL special variable visibility edge cases in some contexts)
  (let ((sessions *active-sessions*)
        (lock *session-lock*)
        (storage *storage*))
    (sb-thread:make-thread
     (lambda ()
       ;; Re-bind for thread-local access
       (let ((*active-sessions* sessions)
             (*session-lock* lock)
             (*storage* storage))
         (auth-worker-loop request-queue result-queue game)))
     :name name))
  #-sbcl
  nil)

(defun start-auth-workers (request-queue result-queue game count)
  "Start COUNT auth worker threads. Returns list of thread objects."
  (loop :for i :from 1 :to count
        :collect (start-auth-worker request-queue result-queue game
                                    :name (format nil "auth-worker-~d" i))))

(defun stop-auth-workers (request-queue worker-threads)
  "Stop all auth worker threads gracefully."
  ;; Send one stop signal per worker
  (dolist (wt worker-threads)
    (declare (ignore wt))
    (auth-queue-push request-queue (make-auth-request :stop-signal t)))
  ;; Wait for all threads to exit
  #+sbcl
  (dolist (wt worker-threads)
    (when wt
      (sb-thread:join-thread wt :timeout 5.0)))
  #-sbcl
  nil)

(defun integrate-auth-results (result-queue socket game elapsed)
  "Process completed auth results on main thread.
   Integrates players into game, updates clients, sends responses.
   Called each frame from main loop - non-blocking."
  (let ((results (auth-queue-drain-nonblocking result-queue)))
    (dolist (result results)
      (let ((host (auth-result-host result))
            (port (auth-result-port result))
            (username (auth-result-username result))
            (client (auth-result-client result))
            (success (auth-result-success result))
            (player (auth-result-player result))
            (player-id (auth-result-player-id result))
            (error-reason (auth-result-error-reason result))
            (zone-id (auth-result-zone-id result)))
        (cond
          ;; Success - integrate player into game
          (success
           (auth-metric-incf success)
           ;; For login: remove any existing player with same ID (stale session)
           (when (and (eq (auth-result-type result) :login) player)
             (let ((existing (find player-id (game-players game) :key #'player-id)))
               (when existing
                 (remove-player-from-game game existing)
                 (log-verbose "Removed stale player ~d before re-adding" player-id))))
           ;; Add player to game
           (when player
             (add-player-to-game game player)
             ;; Update client state
             (setf (net-client-player client) player)
             (setf (net-client-authenticated-p client) t)
             (setf (net-client-account-username client) (string-downcase username))
             (setf (net-client-last-heard client) elapsed)
             ;; Phase 5: Initialize zone tracking - first snapshot is always full
             (setf (net-client-zone-id client) zone-id)
             (setf (net-client-needs-full-resync client) t)
             ;; Step 5: Register local session only (DB calls moved to worker thread)
             (register-player-session-local player :zone-id zone-id
                                            :username (string-downcase username))
             (queue-private-state client player)
             (setf (player-inventory-dirty player) nil
                   (player-hud-stats-dirty player) nil)
             ;; Clear rate limit on success
             (auth-rate-record-success host)
             ;; Send success response
             (send-net-message-with-retry socket
                                          (list :type :auth-ok :player-id player-id)
                                          :host host :port port
                                          :max-retries 3
                                          :delay 50)
             (log-verbose "~a successful: ~a (~a:~d) -> player-id=~d"
                         (if (eq (auth-result-type result) :register) "Registration" "Login")
                         username host port player-id)))
          ;; Failure - send error response
          (t
           (auth-metric-incf fail)
           ;; Clean up session registration for login failures (was registered in worker)
           (when (and (eq (auth-result-type result) :login)
                      (not (eq error-reason :already-logged-in))
                      (not (eq error-reason :bad-credentials)))
             ;; Session was registered in worker before DB operations failed
             (session-unregister username))
           ;; Send failure response
           (let ((fail-message (list :type :auth-fail :reason error-reason)))
             (when (and (eq error-reason :wrong-zone) zone-id)
               (setf fail-message (append fail-message (list :zone-id zone-id))))
             (send-net-message-with-retry socket
                                          fail-message
                                          :host host :port port
                                          :max-retries 3
                                          :delay 50))
           (log-verbose "~a failed from ~a:~d - ~a"
                       (if (eq (auth-result-type result) :register) "Registration" "Login")
                       host port error-reason)))))))

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

(defun intent->plist (intent)
  ;; Convert INTENT into a plist suitable for network transport.
  (when intent
    (list :move-dx (intent-move-dx intent)
          :move-dy (intent-move-dy intent)
          :face-dx (intent-face-dx intent)
          :face-dy (intent-face-dy intent)
          :target-x (intent-target-x intent)
          :target-y (intent-target-y intent)
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

(defun send-snapshot-bytes (socket octets size host port)
  "Send pre-encoded snapshot bytes to a client. Returns T on success."
  (handler-case
      (progn
        (usocket:socket-send socket octets size :host host :port port)
        t)
    (error (e)
      (log-verbose "Failed to send snapshot to ~a:~d: ~a" host port e)
      nil)))

;;;; ========================================================================
;;;; UDP FRAGMENTATION - See docs/net.md Prong 3
;;;; Phase 3: Uses reusable buffers to minimize allocation
;;;; ========================================================================

;;; Reusable buffer for fragmented snapshot payloads
(defparameter *fragmentation-payload-buffer* nil
  "Reusable string buffer for fragmented snapshot serialization.")
(defparameter *fragmentation-payload-size* (* 256 1024)
  "Size of fragmentation payload buffer (256KB).")
(defparameter *fragmentation-chunk-buffer* nil
  "Reusable byte buffer for sending chunk data.")

(defun ensure-fragmentation-buffers ()
  "Ensure fragmentation buffers are allocated. Returns payload buffer."
  (unless (and *fragmentation-payload-buffer*
               (>= (array-dimension *fragmentation-payload-buffer* 0)
                   *fragmentation-payload-size*))
    (setf *fragmentation-payload-buffer*
          (make-array *fragmentation-payload-size*
                      :element-type 'character
                      :fill-pointer 0
                      :adjustable nil)))
  (unless (and *fragmentation-chunk-buffer*
               (>= (length *fragmentation-chunk-buffer*) *max-chunk-payload*))
    (setf *fragmentation-chunk-buffer*
          (make-array *max-chunk-payload*
                      :element-type '(unsigned-byte 8)
                      :initial-element 0)))
  *fragmentation-payload-buffer*)

(defun send-fragmented-snapshot (socket state event-plists seq host port)
  "Split snapshot into chunks and send each via UDP.
   Used when snapshot exceeds UDP buffer size.
   Phase 3: Uses reusable buffers to minimize allocation."
  ;; Ensure buffers exist and reset fill pointer
  (let ((payload-buf (ensure-fragmentation-buffers)))
    (setf (fill-pointer payload-buf) 0)
    ;; Serialize to reusable buffer
    (with-output-to-string (out payload-buf)
      (prin1 (list :state state :events event-plists) out))
    (let* ((total-size (fill-pointer payload-buf))
           (chunk-count (ceiling total-size *max-chunk-payload*)))
      (declare (type fixnum total-size chunk-count))
      (log-verbose "Fragmenting snapshot: ~d bytes into ~d chunks for ~a:~d"
                   total-size chunk-count host port)
      ;; Send chunks using displaced arrays to avoid subseq allocation
      (loop :for chunk-idx fixnum :from 0 :below chunk-count
            :for start fixnum = (* chunk-idx *max-chunk-payload*)
            :for end fixnum = (min (+ start *max-chunk-payload*) total-size)
            :for chunk-len fixnum = (- end start)
            ;; Create displaced string for this chunk (no copy)
            :for chunk-data = (make-array chunk-len
                                          :element-type 'character
                                          :displaced-to payload-buf
                                          :displaced-index-offset start)
            :do (send-net-message socket
                                 (list :type :snapshot-chunk
                                       :seq seq
                                       :chunk chunk-idx
                                       :total chunk-count
                                       :data chunk-data)
                                 :host host :port port)))))

(defun receive-snapshot-chunk (buffer chunk-message current-time)
  "Add chunk to reassembly buffer. Returns complete state or nil.
   BUFFER is a chunk-buffer struct.
   CHUNK-MESSAGE is the incoming :snapshot-chunk message.
   CURRENT-TIME is used for timeout detection.
   Phase 3: Uses reusable buffers to avoid per-chunk allocation."
  (let ((seq (getf chunk-message :seq))
        (idx (getf chunk-message :chunk))
        (total (getf chunk-message :total))
        (data (getf chunk-message :data)))
    (declare (type fixnum idx total))
    ;; Timeout check - discard old incomplete sequences
    (when (and (chunk-buffer-seq buffer)
               (> (- current-time (chunk-buffer-timestamp buffer)) *chunk-timeout*))
      (log-verbose "Chunk timeout: discarding incomplete seq ~d" (chunk-buffer-seq buffer))
      (setf (chunk-buffer-seq buffer) nil
            (chunk-buffer-data-fill buffer) 0))
    ;; New sequence? Reset buffer and ensure storage
    (when (or (null (chunk-buffer-seq buffer))
              (/= seq (chunk-buffer-seq buffer)))
      (ensure-chunk-buffer-storage buffer total)
      (setf (chunk-buffer-seq buffer) seq
            (chunk-buffer-total buffer) total
            (chunk-buffer-data-fill buffer) 0
            (chunk-buffer-timestamp buffer) current-time)
      ;; Clear chunk tracking (lengths=0 means not received)
      (let ((lengths (chunk-buffer-chunk-lengths buffer)))
        (dotimes (i total)
          (setf (aref lengths i) 0))))
    ;; Store chunk data into reusable buffer
    (when (and data (stringp data))
      (let* ((data-len (length data))
             (lengths (chunk-buffer-chunk-lengths buffer))
             (offsets (chunk-buffer-chunk-offsets buffer))
             (data-buf (chunk-buffer-data-buffer buffer)))
        (declare (type fixnum data-len))
        ;; Only store if not already received (avoid duplicates)
        (when (and (< idx total) (zerop (aref lengths idx)))
          ;; Calculate offset for this chunk (sum of prior chunks)
          (let ((offset 0))
            (declare (type fixnum offset))
            (dotimes (i idx)
              (incf offset (aref lengths i)))
            (setf (aref offsets idx) offset)
            ;; Copy string bytes to data buffer
            (when (<= (+ offset data-len) (length data-buf))
              (dotimes (i data-len)
                (setf (aref data-buf (+ offset i))
                      (char-code (char data i))))
              (setf (aref lengths idx) data-len)
              (incf (chunk-buffer-data-fill buffer)))))))
    ;; Check if complete (all chunks received)
    (when (= (chunk-buffer-data-fill buffer) total)
      ;; Reassemble: compute total size and convert to string
      (let* ((lengths (chunk-buffer-chunk-lengths buffer))
             (data-buf (chunk-buffer-data-buffer buffer))
             (total-bytes 0))
        (declare (type fixnum total-bytes))
        (dotimes (i total)
          (incf total-bytes (aref lengths i)))
        ;; Convert bytes to string (single allocation for final parse)
        (let* ((combined (make-string total-bytes))
               (pos 0))
          (declare (type fixnum pos))
          (dotimes (i total-bytes)
            (setf (char combined i) (code-char (aref data-buf i))))
          (let* ((*read-eval* nil)
                 (parsed (handler-case
                             (read-from-string combined nil nil)
                           (error () nil))))
            (when parsed
              ;; Clear buffer for next sequence
              (setf (chunk-buffer-seq buffer) nil
                    (chunk-buffer-data-fill buffer) 0)
              ;; Return the parsed state and events
              (values (getf parsed :state)
                      (getf parsed :events)))))))))

(defun client-needs-full-resync-p (client current-seq)
  "Return T if CLIENT needs a full snapshot instead of delta.
   Checks: needs-full-resync flag, missing ack, ack gap, or ack too old."
  (let ((last-ack (net-client-last-acked-seq client)))
    (or (net-client-needs-full-resync client)
        (null last-ack)
        (> (- current-seq last-ack) *max-delta-gap*)
        (> (- current-seq last-ack) *max-delta-age*))))

(defun group-clients-by-zone (clients)
  "Group authenticated clients by their player's zone-id.
   Returns hash table: zone-id -> list of clients.
   Nil zone-ids are clamped to *starting-zone-id* to ensure clients
   always receive zone-filtered snapshots (never empty)."
  (let ((groups (make-hash-table :test 'eq :size 64)))
    (dolist (c clients)
      (when (and (net-client-authenticated-p c)
                 (net-client-player c))
        (let* ((player (net-client-player c))
               (zone-id (or (player-zone-id player) *starting-zone-id*)))
          (push c (gethash zone-id groups)))))
    groups))

(defun broadcast-snapshots-with-delta (socket clients game current-seq event-plists)
  "Send zone-filtered snapshots to clients with delta compression.

   Players in different zones receive different snapshots containing only
   players/NPCs in their zone. This enables true multi-zone gameplay.

   For efficiency, clients are grouped by zone and snapshots are encoded
   once per zone (maintaining the 'encode once, send to many' optimization).

   ANTI-STARVATION GUARD: If a client's zone-id has no valid zone-path,
   we fall back to *starting-zone-id* instead of skipping. This ensures
   clients always receive snapshots even with corrupted zone data."
  ;; PHASE 5: Detect zone changes and trigger full resync
  ;; This runs BEFORE grouping so resync flag is set before partition
  (dolist (client clients)
    (let ((player (net-client-player client)))
      (when player
        (let ((player-zone (player-zone-id player))
              (client-zone (net-client-zone-id client)))
          ;; If zone changed, trigger full resync and update cached zone
          (when (and player-zone (not (eq player-zone client-zone)))
            (log-verbose "Zone change detected: player ~d zone ~a -> ~a"
                         (player-id player) client-zone player-zone)
            (setf (net-client-needs-full-resync client) t)
            (setf (net-client-zone-id client) player-zone)))
        ;; Check for player-initiated full resync (unstuck teleport)
        (when (player-force-full-resync player)
          (log-verbose "Player ~d force-full-resync (unstuck teleport)"
                       (player-id player))
          (setf (net-client-needs-full-resync client) t)
          (setf (player-force-full-resync player) nil)))))
  ;; Now proceed with zone-grouped broadcast
  (let ((zone-groups (group-clients-by-zone clients))
        (any-sent nil)
        (world (game-world game)))
    ;; Process each zone group
    (maphash
     (lambda (zone-id zone-clients)
       ;; GUARD: Resolve zone-path with fallback to starting zone
       (let* ((zone-path (zone-path-for-id world zone-id))
              (effective-zone-id zone-id)
              (fell-back nil))
         ;; If zone-path is nil, fall back to starting zone (anti-starvation)
         (unless zone-path
           (setf zone-path (zone-path-for-id world *starting-zone-id*))
           (setf effective-zone-id *starting-zone-id*)
           (setf fell-back t)
           (warn "Zone ~a: unknown, forcing fallback to ~a for ~d clients"
                 zone-id *starting-zone-id* (length zone-clients)))

         ;; Now get-or-create zone-state (should always succeed for starting zone)
         (let ((zone-state (when zone-path
                             (get-or-create-zone-state effective-zone-id zone-path))))
           (cond
             ;; Starting zone itself is broken - log and skip (shouldn't happen)
             ((null zone-state)
              (warn "CRITICAL: Cannot create zone-state for ~a - skipping ~d clients"
                    effective-zone-id (length zone-clients)))

             ;; Valid zone-state: proceed with zone-filtered serialization
             (t
              ;; Force full resync if we fell back to a different zone
              (when fell-back
                (dolist (c zone-clients)
                  (setf (net-client-needs-full-resync c) t)))

              (let ((resync-clients nil)
                    (delta-clients nil))
                ;; Partition by resync vs delta
                (dolist (c zone-clients)
                  (if (client-needs-full-resync-p c current-seq)
                      (push c resync-clients)
                      (push c delta-clients)))

                ;; Send zone-filtered full snapshot to clients needing resync
                (when resync-clients
                  (let ((full-state (serialize-game-state-for-zone
                                     game effective-zone-id zone-state :use-pool t)))
                    (setf full-state (plist-put full-state :seq current-seq))
                    ;; Mark as explicit resync so client knows to sync zone-state NPCs
                    (setf full-state (plist-put full-state :resync t))
                    (log-verbose "Zone ~a: resync ~d clients (seq ~d, ~d players)"
                                 effective-zone-id (length resync-clients) current-seq
                                 (length (getf full-state :players)))
                    (send-snapshots-parallel socket resync-clients full-state event-plists 1)
                    (dolist (c resync-clients)
                      (setf (net-client-needs-full-resync c) nil))
                    (setf any-sent t)))

                ;; Send zone-filtered delta snapshot to synced clients
                (when delta-clients
                  (let ((delta-state (serialize-game-state-delta-for-zone
                                      game effective-zone-id zone-state current-seq :use-pool t)))
                    (log-verbose "Zone ~a: delta ~d clients" effective-zone-id (length delta-clients))
                    (send-snapshots-parallel socket delta-clients delta-state event-plists 1)
                    (setf any-sent t)))))))))
     zone-groups)

    ;; Clear dirty flags after ALL sends complete
    (when any-sent
      (clear-snapshot-dirty-flags game))))

(defun send-snapshots-parallel (socket clients state event-plists worker-threads)
  ;; Send snapshots to clients.
  ;; Only sends to authenticated clients with a player.
  ;;
  ;; PERFORMANCE OPTIMIZATION: Encode the snapshot ONCE and send identical bytes
  ;; to all clients. This reduces encoding from O(clients × state_size) to O(state_size).
  ;; The player-id field is omitted from broadcast snapshots - clients use the ID
  ;; they received from auth-ok (stored in game-net-player-id).
  ;;
  ;; UDP FRAGMENTATION (Prong 3): If snapshot exceeds buffer size, split into chunks.
  ;; BINARY ENCODING (Phase 3 Task 3.1): When *use-binary-snapshots* is T, use compact
  ;; binary format instead of plist text. Reduces bandwidth ~50% and encoding CPU.
  (declare (ignore worker-threads)) ; Parallel disabled - socket serializes sends anyway
  (let ((authenticated-clients nil))
    ;; Filter authenticated clients
    (dolist (c clients)
      (when (and (net-client-authenticated-p c)
                 (net-client-player c))
        (push c authenticated-clients)))
    (when authenticated-clients
      ;; Phase 3 Task 3.1: Use binary encoding when enabled
      (if *use-binary-snapshots*
          ;; BINARY PATH: Encode directly to reusable buffer
          (let* ((buffer (ensure-binary-send-buffer))
                 (size (encode-snapshot-binary state event-plists buffer)))
            (declare (type fixnum size))
            (cond
              ;; Normal case: binary snapshot fits in one UDP packet
              ((<= size *net-buffer-size*)
               (dolist (client authenticated-clients)
                 (send-snapshot-bytes socket buffer size
                                      (net-client-host client)
                                      (net-client-port client))))
              ;; Large binary snapshot: fall back to text + fragmentation
              ;; (Binary fragmentation not implemented - rare case for large zones)
              (t
               (log-verbose "Binary snapshot too large (~d bytes), falling back to text+frag"
                            size)
               (let* ((message (list :type :snapshot :state state :events event-plists))
                      (text (encode-net-message message))
                      (octets (string-to-octets text))
                      (seq (or (getf state :seq) 0)))
                 (declare (ignore octets))
                 (dolist (client authenticated-clients)
                   (send-fragmented-snapshot socket state event-plists seq
                                             (net-client-host client)
                                             (net-client-port client)))))))
          ;; TEXT PATH: Original plist encoding
          ;; CRITICAL OPTIMIZATION: Encode snapshot once, reuse for all clients
          ;; Previously we called encode-net-message 40 times for 40 clients!
          (let* ((message (list :type :snapshot
                                :state state
                                :events event-plists))
                 (text (encode-net-message message))
                 (octets (string-to-octets text))
                 (size (length octets)))
            (cond
              ;; Normal case: snapshot fits in one UDP packet
              ((<= size *net-buffer-size*)
               (dolist (client authenticated-clients)
                 (send-snapshot-bytes socket octets size
                                      (net-client-host client)
                                      (net-client-port client))))
              ;; Large snapshot: use UDP fragmentation (Prong 3)
              (t
               (let ((seq (or (getf state :seq) 0)))
                 (log-verbose "Snapshot too large (~d bytes), fragmenting for ~d clients"
                              size (length authenticated-clients))
                 (dolist (client authenticated-clients)
                   (send-fragmented-snapshot socket state event-plists seq
                                             (net-client-host client)
                                             (net-client-port client)))))))))))

(defun queue-private-state (client player &key (retries *private-state-retries*))
  ;; Queue owner-only state updates (inventory/equipment/stats) for CLIENT.
  (when (and client player)
    (setf (net-client-private-state client) (serialize-player-private player)
          (net-client-private-retries client) (max 1 retries))))

(defun send-private-states (socket clients)
  ;; Send queued private state updates to owning clients.
  (dolist (client clients)
    (when (and (net-client-authenticated-p client)
               (net-client-player client))
      (let ((player (net-client-player client)))
        (when (or (player-inventory-dirty player)
                  (player-hud-stats-dirty player))
          (queue-private-state client player)
          (setf (player-inventory-dirty player) nil
                (player-hud-stats-dirty player) nil)))
      (let ((payload (net-client-private-state client)))
        (when payload
          (send-net-message socket
                            (list :type :private-state
                                  :player-id (player-id (net-client-player client))
                                  :payload payload)
                            :host (net-client-host client)
                            :port (net-client-port client))
          (decf (net-client-private-retries client))
          (when (<= (net-client-private-retries client) 0)
            (setf (net-client-private-state client) nil)))))))

(defun apply-private-state (game payload &key player-id)
  ;; Apply owner-only state updates (inventory/equipment/stats) to the local player.
  (when (and game payload)
    (let* ((players (game-players game))
           (id (or player-id (game-net-player-id game)))
           (player (and id players (find-player-by-id players id))))
      (if player
          (apply-player-private-plist player payload)
          (log-verbose "Private state ignored (player ~a not found)" id)))))

(defun position-distance-sq (x1 y1 x2 y2)
  "Compute squared distance between two positions. Avoids sqrt for efficiency."
  (declare (type single-float x1 y1 x2 y2))
  (let ((dx (- x2 x1))
        (dy (- y2 y1)))
    (+ (* dx dx) (* dy dy))))

(defun teleport-detected-p (old-x old-y new-x new-y)
  "Return T if position jump exceeds teleport threshold (same-zone teleport).
   Used to detect unstuck teleports and reset client sync state."
  (declare (type single-float old-x old-y new-x new-y))
  (> (position-distance-sq old-x old-y new-x new-y)
     *teleport-distance-threshold-sq*))

(defun reset-client-sync-state (game)
  "Reset interpolation and prediction state after same-zone teleport (unstuck).
   Lighter-weight than handle-zone-transition - no UI/editor side effects.
   Lives in net.lisp to avoid cross-module dependency (called from apply-snapshot)."
  (let* ((buffer (game-interpolation-buffer game))
         (pred (game-prediction-state game))
         (player (game-player game)))
    ;; Clear interpolation buffer - stale positions cause frozen sprites
    (when buffer
      (setf (interpolation-buffer-count buffer) 0
            (interpolation-buffer-head buffer) 0))
    ;; Reset prediction state - stale predicted position causes movement issues
    (when (and pred player)
      (setf (prediction-state-predicted-x pred) (player-x player)
            (prediction-state-predicted-y pred) (player-y player)
            (prediction-state-input-count pred) 0
            (prediction-state-input-head pred) 0))))


(defun apply-snapshot (game state event-plists &key player-id)
  ;; Apply a snapshot state and queue HUD/combat events for UI.
  ;; Returns (values zone-id delta-positions) where delta-positions is
  ;; non-nil for delta snapshots (used for interpolation buffer fix).
  ;; Detects same-zone teleports (unstuck) and resets sync state to avoid frozen sprites.
  (handler-case
      (let* ((player (game-player game))
             (buffer (game-interpolation-buffer game))
             ;; Guard: only detect teleports after we have prior data
             ;; Prevents false positive on first snapshot (old-x/old-y = 0,0)
             (has-prior-data (and player
                                  buffer
                                  (> (interpolation-buffer-count buffer) 0)))
             ;; Capture pre-snapshot position for teleport detection
             (old-x (if player (player-x player) 0.0f0))
             (old-y (if player (player-y player) 0.0f0)))
        (when player-id
          (setf (game-net-player-id game) player-id))
        (multiple-value-bind (zone-id zone-changed delta-positions)
            (apply-game-state game state :apply-zone t)
          (when zone-changed
            (log-verbose "Client zone transitioned to ~a" zone-id)
            (handle-zone-transition game))
          ;; Detect same-zone teleport (unstuck) - reset sync state if position jumped
          ;; Only check if we had prior data to avoid first-snapshot false positives
          (let* ((new-player (game-player game))
                 (teleported (and new-player
                                  has-prior-data
                                  (not zone-changed)
                                  (teleport-detected-p old-x old-y
                                                       (player-x new-player)
                                                       (player-y new-player))))
                 ;; Explicit resync flag from server (unstuck, reconnect, etc.)
                 ;; Only sync NPC grid when server explicitly marks resync, not just compact snapshot
                 (server-resync-p (getf state :resync)))
            (when teleported
              (log-verbose "Teleport detected (unstuck): (~,1f,~,1f) -> (~,1f,~,1f)"
                           old-x old-y (player-x new-player) (player-y new-player))
              ;; Use lightweight reset - no UI/editor side effects
              (reset-client-sync-state game))
            ;; Phase 2: Sync zone-state NPCs after explicit server resync or teleport
            ;; Zone change already syncs via handle-zone-transition
            ;; Teleport and explicit resync need sync here (not every compact snapshot)
            (when (and (not zone-changed)
                       (or teleported server-resync-p))
              (sync-client-zone-npcs game)))
          (let ((queue (game-combat-events game)))
            (dolist (event-plist event-plists)
              (let ((event (plist->combat-event event-plist)))
                (when event
                  (push-combat-event queue event)))))
          (let ((final-player (game-player game)))
            (when final-player
              (mark-player-hud-stats-dirty final-player)
              (mark-player-inventory-dirty final-player)))
          (values zone-id delta-positions)))
    (error (e)
      (warn "Failed to apply snapshot: ~a" e)
      (log-verbose "Snapshot application error: ~a" e)
      (values nil nil))))

;;;; Client-Side Interpolation

(defun make-interpolation-buffer ()
  ;; Create a new interpolation buffer for smoothing remote entity movement.
  ;; Pre-allocates a pool of position tables (one per snapshot slot) to avoid
  ;; allocation during capture and prevent aliasing between snapshots.
  ;; INVARIANT: position-pool length MUST equal capacity (one table per snapshot slot).
  (let ((cap 4))  ; If making capacity configurable, ensure position-pool follows
    (%make-interpolation-buffer
     :snapshots (make-array cap :initial-element nil)
     :head 0
     :count 0
     :capacity cap
     :position-pool (let ((pool (make-array cap)))
                      (dotimes (i cap pool)
                        (setf (aref pool i) (make-hash-table :test 'eql :size 64)))))))

(defun push-interpolation-snapshot (buffer snapshot)
  ;; Add a snapshot to the ring buffer.
  (when buffer
    (let* ((cap (interpolation-buffer-capacity buffer))
           (head (interpolation-buffer-head buffer)))
      (setf (aref (interpolation-buffer-snapshots buffer) head) snapshot
            (interpolation-buffer-head buffer) (mod (1+ head) cap))
      (when (< (interpolation-buffer-count buffer) cap)
        (incf (interpolation-buffer-count buffer))))))

(defun get-interpolation-snapshot-at-index (buffer index)
  ;; Return snapshot at logical index (0 = oldest valid).
  (when (and buffer (>= index 0) (< index (interpolation-buffer-count buffer)))
    (let* ((cap (interpolation-buffer-capacity buffer))
           (count (interpolation-buffer-count buffer))
           (head (interpolation-buffer-head buffer))
           (physical-index (mod (+ (- head count) index) cap)))
      (aref (interpolation-buffer-snapshots buffer) physical-index))))

(defun lerp (a b alpha)
  ;; Linear interpolation: a + (b - a) * alpha
  (+ a (* (- b a) alpha)))

(defun clamp (value min-val max-val)
  ;; Clamp VALUE between MIN-VAL and MAX-VAL.
  (max min-val (min max-val value)))

(defun capture-entity-positions (game local-player-id
                                  &key delta-positions previous-positions buffer)
  ;; Capture positions of all entities except local player for interpolation.
  ;; For delta snapshots, pass DELTA-POSITIONS (hash table from deserialize)
  ;; and PREVIOUS-POSITIONS (from last interpolation snapshot) to avoid
  ;; capturing stale interpolated positions for entities not in the delta.
  ;; BUFFER: if provided, uses pooled position table for current head slot to avoid allocation.
  ;; Each snapshot slot has its own dedicated table, preventing aliasing.
  (let ((positions
         ;; Use pooled table from buffer if available, else allocate
         (if (and buffer (interpolation-buffer-position-pool buffer))
             (let* ((head (interpolation-buffer-head buffer))
                    (pool (interpolation-buffer-position-pool buffer))
                    (table (aref pool head)))
               (clrhash table)
               table)
             (make-hash-table :test 'eql :size 64)))
        (players (game-players game))
        (npcs (game-npcs game)))
    ;; Capture other players
    (when players
      (loop :for player :across players
            :for id = (player-id player)
            :when (and (> id 0) (/= id local-player-id))
            :do (cond
                  ;; Delta mode: entity was in delta, use its snapshot position
                  ((and delta-positions (gethash id delta-positions))
                   (setf (gethash id positions) (gethash id delta-positions)))
                  ;; Delta mode: entity NOT in delta, copy from previous snapshot
                  ((and delta-positions previous-positions
                        (gethash id previous-positions))
                   (setf (gethash id positions) (gethash id previous-positions)))
                  ;; Full snapshot mode: capture current position
                  ((not delta-positions)
                   (setf (gethash id positions)
                         (list (player-x player) (player-y player)))))))
    ;; Capture NPCs (use negative IDs to distinguish from players)
    (when npcs
      (loop :for npc :across npcs
            :for id = (npc-id npc)
            :for neg-id = (- id)
            :when (> id 0)
            :do (cond
                  ;; Delta mode: NPC was in delta
                  ((and delta-positions (gethash neg-id delta-positions))
                   (setf (gethash neg-id positions)
                         (gethash neg-id delta-positions)))
                  ;; Delta mode: NPC NOT in delta, copy from previous
                  ((and delta-positions previous-positions
                        (gethash neg-id previous-positions))
                   (setf (gethash neg-id positions)
                         (gethash neg-id previous-positions)))
                  ;; Full snapshot mode: capture current
                  ((not delta-positions)
                   (setf (gethash neg-id positions)
                         (list (npc-x npc) (npc-y npc)))))))
    positions))

(defun find-interpolation-bounds (buffer render-time)
  ;; Find two snapshots to interpolate between for RENDER-TIME.
  ;; Returns: (values snap-before snap-after alpha) or NIL if insufficient data.
  (when (and buffer (>= (interpolation-buffer-count buffer) 2))
    (let ((count (interpolation-buffer-count buffer)))
      ;; Search for snapshots bracketing render-time
      (loop :for i :from 0 :below (1- count)
            :for snap-a = (get-interpolation-snapshot-at-index buffer i)
            :for snap-b = (get-interpolation-snapshot-at-index buffer (1+ i))
            :when (and snap-a snap-b)
            :do (let ((time-a (interpolation-snapshot-timestamp snap-a))
                      (time-b (interpolation-snapshot-timestamp snap-b)))
                  (when (and (<= time-a render-time) (<= render-time time-b))
                    (let ((alpha (if (= time-a time-b)
                                     0.0
                                     (/ (- render-time time-a) (- time-b time-a)))))
                      (return-from find-interpolation-bounds
                        (values snap-a snap-b (clamp alpha 0.0 1.0)))))))
      ;; If render-time is outside all snapshots, use boundary snapshots
      (let* ((oldest (get-interpolation-snapshot-at-index buffer 0))
             (newest (get-interpolation-snapshot-at-index buffer (1- count))))
        (cond
          ((and oldest (< render-time (interpolation-snapshot-timestamp oldest)))
           (values oldest oldest 0.0))
          ((and newest (> render-time (interpolation-snapshot-timestamp newest)))
           (values newest newest 1.0))
          (t nil))))))

(defun interpolate-remote-entities (game)
  ;; Interpolate positions for all remote entities (not local player).
  (with-timing (:interpolate)
    (let ((buffer (game-interpolation-buffer game))
        (delay (or (game-interpolation-delay game) *interpolation-delay-seconds*))
        (current-time (or (game-client-time game) 0.0))
        (local-id (or (game-net-player-id game) 0)))
    (when (and buffer (> (interpolation-buffer-count buffer) 1))
      (let ((render-time (- current-time delay)))
        (multiple-value-bind (snap-a snap-b alpha)
            (find-interpolation-bounds buffer render-time)
          (when (and snap-a snap-b)
            (let ((pos-a (interpolation-snapshot-entity-positions snap-a))
                  (pos-b (interpolation-snapshot-entity-positions snap-b)))
              ;; Interpolate other players
              (let ((players (game-players game)))
                (when players
                  (loop :for player :across players
                        :for id = (player-id player)
                        :when (and (> id 0) (/= id local-id))
                        :do (let ((pa (gethash id pos-a))
                                  (pb (gethash id pos-b)))
                              (when (and pa pb)
                                (setf (player-x player) (lerp (first pa) (first pb) alpha)
                                      (player-y player) (lerp (second pa) (second pb) alpha)))))))
              ;; Interpolate NPCs (stored with negative IDs)
              (let ((npcs (game-npcs game)))
                (when npcs
                  (loop :for npc :across npcs
                        :for id = (npc-id npc)
                        :when (> id 0)
                        :do (let ((pa (gethash (- id) pos-a))
                                  (pb (gethash (- id) pos-b)))
                              (when (and pa pb)
                                (setf (npc-x npc) (lerp (first pa) (first pb) alpha)
                                      (npc-y npc) (lerp (second pa) (second pb) alpha)))))))))))))))

;;;; Client-Side Prediction

(defun make-prediction-state (player)
  ;; Create prediction state initialized to player's current position.
  (when player
    (%make-prediction-state
     :inputs (make-array 64 :initial-element nil)
     :input-head 0
     :input-count 0
     :input-capacity 64
     :input-sequence 0
     :last-acked-sequence 0
     :predicted-x (player-x player)
     :predicted-y (player-y player)
     :misprediction-count 0)))

(defun store-prediction-input (pred-state intent timestamp)
  ;; Store input for potential replay during reconciliation.
  ;; Returns the sequence number assigned to this input.
  (when (and pred-state intent)
    (let* ((seq (prediction-state-input-sequence pred-state))
           (cap (prediction-state-input-capacity pred-state))
           (head (prediction-state-input-head pred-state))
           (input (%make-prediction-input
                   :sequence seq
                   :timestamp timestamp
                   :move-dx (intent-move-dx intent)
                   :move-dy (intent-move-dy intent)
                   :target-x (intent-target-x intent)
                   :target-y (intent-target-y intent)
                   :target-active (intent-target-active intent))))
      (setf (aref (prediction-state-inputs pred-state) head) input
            (prediction-state-input-head pred-state) (mod (1+ head) cap))
      (when (< (prediction-state-input-count pred-state) cap)
        (incf (prediction-state-input-count pred-state)))
      (incf (prediction-state-input-sequence pred-state))
      seq)))

(defun apply-local-prediction (game intent dt)
  ;; Apply movement locally for instant feedback (prediction).
  (when *client-prediction-enabled*
    (let* ((pred (game-prediction-state game))
           (player (game-player game))
           (world (game-world game)))
      (when (and pred player world intent)
        ;; Apply movement using same logic as server
        (let* ((input-dx (intent-move-dx intent))
               (input-dy (intent-move-dy intent))
               (speed-mult (if (player-running player) *run-speed-mult* 1.0)))
          (update-player-position player intent world speed-mult dt)
          ;; Track predicted position
          (setf (prediction-state-predicted-x pred) (player-x player)
                (prediction-state-predicted-y pred) (player-y player)))))))

(defun reconcile-prediction (game server-x server-y server-sequence)
  ;; Compare server state to prediction and correct if needed.
  (when *client-prediction-enabled*
    (let* ((pred (game-prediction-state game))
           (player (game-player game)))
      (when (and pred player (> server-sequence (prediction-state-last-acked-sequence pred)))
        (setf (prediction-state-last-acked-sequence pred) server-sequence)
        ;; Check prediction error
        (let ((error-x (abs (- server-x (prediction-state-predicted-x pred))))
              (error-y (abs (- server-y (prediction-state-predicted-y pred)))))
          (when (or (> error-x *prediction-error-threshold*)
                    (> error-y *prediction-error-threshold*))
            ;; Misprediction detected - snap to server position
            (incf (prediction-state-misprediction-count pred))
            (log-verbose "Prediction misprediction #~d: error=~,1f,~,1f"
                         (prediction-state-misprediction-count pred)
                         error-x error-y)
            ;; Reset to server position
            (setf (player-x player) server-x
                  (player-y player) server-y
                  (prediction-state-predicted-x pred) server-x
                  (prediction-state-predicted-y pred) server-y)))))))

(defun send-intent-message (socket intent &key host port sequence ack)
  ;; Send the current INTENT as a UDP message.
  ;; If SEQUENCE is provided, includes it for server-side tracking.
  ;; If ACK is provided, acknowledges last received snapshot (delta compression).
  (let ((pickup-id (intent-requested-pickup-target-id intent)))
    (when pickup-id
      (log-verbose "SEND-INTENT: pickup id=~a tx=~a ty=~a"
                   pickup-id
                   (intent-requested-pickup-tx intent)
                   (intent-requested-pickup-ty intent))))
  (let ((drop-id (intent-requested-drop-item-id intent)))
    (when drop-id
      (log-verbose "SEND-INTENT: drop item=~a count=~a"
                   drop-id
                   (intent-requested-drop-count intent))))
  (let ((payload (intent->plist intent))
        (message (list :type :intent)))
    (when sequence
      (setf payload (append payload (list :sequence sequence))))
    (setf message (append message (list :payload payload)))
    ;; Add ack at message level (not in payload) for delta compression
    (when ack
      (setf message (append message (list :ack ack))))
    (send-net-message socket message :host host :port port)
    ;; Clear one-shot intent fields after sending to prevent repeat
    ;; NOTE: Do NOT clear pickup intent here - it needs to persist until player
    ;; reaches the target. Server handles clearing via sync-player-pickup-target.
    (when (intent-requested-swap-slot-a intent)
      (clear-requested-inventory-swap intent))
    (when (intent-requested-drop-item-id intent)
      (clear-requested-drop-item intent))
    (when (intent-requested-chat-message intent)
      (clear-requested-chat-message intent))))

(defun send-auth-message (socket msg-type username password &key host port)
  "Send an authentication message (login or register).
   If *auth-encryption-enabled* and *server-auth-public-key* are set,
   encrypts the credentials with timestamp for replay protection.
   Otherwise sends plaintext."
  (if (and *auth-encryption-enabled* *server-auth-public-key*)
      ;; Encrypted auth: send encrypted credentials payload with timestamp
      (let* ((timestamp (get-universal-time))
             (creds-plist (format nil "(:username ~s :password ~s :timestamp ~d)"
                                 username password timestamp))
             (encrypted (encrypt-auth-payload creds-plist *server-auth-public-key*)))
        (send-net-message socket
                          (list :type msg-type
                                :encrypted-payload encrypted)
                          :host host :port port)
        (log-verbose "Sent encrypted ~a request for ~a" msg-type username))
      ;; Plaintext auth: send username and password directly
      (send-net-message socket
                        (list :type msg-type
                              :username username
                              :password password)
                        :host host :port port)))

(defun extract-auth-credentials (message)
  "Extract username and password from an auth message.
   Supports both encrypted (:encrypted-payload) and plaintext (:username/:password).
   When *auth-require-encryption* is T, rejects plaintext auth.
   Encrypted auth includes replay protection via timestamp and nonce tracking.
   Returns (values username password) or (values NIL NIL) on failure."
  (let ((encrypted-payload (getf message :encrypted-payload)))
    (if encrypted-payload
        ;; Encrypted auth: decrypt and parse
        (let ((decrypted (decrypt-auth-payload encrypted-payload)))
          (if decrypted
              (let* ((*read-eval* nil)
                     (creds (ignore-errors (read-from-string decrypted))))
                (if (listp creds)
                    (let ((username (getf creds :username))
                          (password (getf creds :password))
                          (timestamp (getf creds :timestamp)))
                      ;; Check for replay attack if timestamp present
                      (if (and timestamp (not (auth-check-replay encrypted-payload timestamp)))
                          (progn
                            (log-verbose "Auth rejected: replay attack detected")
                            (values nil nil))
                          (values username password)))
                    (progn
                      (warn "Failed to parse decrypted auth credentials")
                      (values nil nil))))
              (progn
                (log-verbose "Failed to decrypt auth payload")
                (values nil nil))))
        ;; Plaintext auth: check if allowed
        (if *auth-require-encryption*
            (progn
              (warn "Rejecting plaintext auth - encryption required")
              (values nil nil))
            (values (getf message :username)
                    (getf message :password))))))

(defun handle-server-load (game)
  ;; Load game state on the server and reset transient flags.
  (let* ((event-queue (game-combat-events game))
         (players (game-players game))
         (player (game-player game))
         (world (game-world game))
         (zone-id (load-game game *save-filepath*)))
    (if zone-id
        (progn
          (when players
            (loop :for current-player :across players
                  :do (let ((server-intent (player-intent current-player)))
                        (when server-intent
                          (reset-frame-intent server-intent)
                          (clear-intent-target server-intent)))
                      (clear-player-auto-walk current-player)
                      (setf (player-attacking current-player) nil
                            (player-attack-hit current-player) nil
                            (player-hit-active current-player) nil)
                      (mark-player-hud-stats-dirty current-player)
                      (mark-player-inventory-dirty current-player)))
          (when world
            (setf (world-minimap-spawns world)
                  (build-adjacent-minimap-spawns world player))
            (setf (world-minimap-collisions world)
                  (build-minimap-collisions world)))
          (emit-hud-message-event event-queue
                                  (format nil "Game loaded (~a)." zone-id)))
        (emit-hud-message-event event-queue "Load failed."))
    zone-id))

;;;; Account Authentication Handlers

(defun handle-register-request (client host port message socket game elapsed)
  "Handle account registration request.
   Supports both encrypted and plaintext credentials."
  ;; Rate limiting check (use wall-clock time, not sim elapsed)
  (let ((now-rt (/ (get-internal-real-time) (float internal-time-units-per-second 1.0))))
    (declare (ignorable now-rt))
  (unless (auth-rate-check host now-rt)
    (send-net-message-with-retry socket
                                 (list :type :auth-fail :reason :rate-limited)
                                 :host host :port port
                                 :max-retries 3
                                 :delay 50)
    (log-verbose "Registration blocked from ~a:~d - rate limited" host port)
    (return-from handle-register-request nil))
  (multiple-value-bind (username password)
      (extract-auth-credentials message)
    (cond
      ((not (and username password))
       (send-net-message-with-retry socket
                                    (list :type :auth-fail :reason :missing-credentials)
                                    :host host :port port
                                    :max-retries 3
                                    :delay 50)
       (auth-rate-record-failure host now-rt)
       (log-verbose "Registration failed from ~a:~d - missing credentials" host port))
      ((with-retry-exponential (created (lambda () (db-create-account username password))
                                 :max-retries 3
                                 :initial-delay 50
                                 :max-delay 200)
         created)
       ;; Account created successfully - spawn new character
       (let* ((world (game-world game))
              (player (handler-case
                          (spawn-player-at-world world (game-id-source game))
                        (error (e)
                          ;; ID allocation failed (storage outage) - rollback account creation
                          (warn "Registration failed: ID allocation error for ~a: ~a" username e)
                          (with-retry-exponential
                              (deleted (lambda () (db-delete-account username))
                               :max-retries 3
                               :initial-delay 50
                               :max-delay 200
                               :on-final-fail (lambda (e2)
                                                (warn "CRITICAL: Failed to delete account ~a after spawn failure: ~a"
                                                      username e2)))
                            deleted)
                          (send-net-message-with-retry socket
                                                       (list :type :auth-fail :reason :internal-error)
                                                       :host host :port port
                                                       :max-retries 3
                                                       :delay 50)
                          (return-from handle-register-request nil)))))
         (unless player
           (return-from handle-register-request nil))
         (let ((player-id (player-id player)))
           ;; Link account to new character (with retry)
           (let ((linked (with-retry-exponential
                           (set-result (lambda () (db-set-character-id username player-id))
                            :max-retries 3
                            :initial-delay 50
                            :max-delay 200
                            :on-final-fail (lambda (e)
                                             (warn "Failed to set character-id for ~a: ~a" username e)))
                         set-result)))
           (if linked
               (progn
                 ;; Add player to game
                 (add-player-to-game game player)
                 ;; Mark client as authenticated
                 (setf (net-client-player client) player)
                 (setf (net-client-authenticated-p client) t)
                 (setf (net-client-account-username client) (string-downcase username))
                 ;; Register session for persistence
                 (let* ((zone (world-zone world))
                        (zone-id (and zone (zone-id zone))))
                   (register-player-session player :zone-id zone-id
                                            :username (string-downcase username)))
                 ;; Track active session (atomic, should always succeed for new account)
                 (session-try-register username client)
                 ;; Clear rate limit on success
                 (auth-rate-record-success host)
                 ;; Send success response (with retry - critical auth message)
                 (send-net-message-with-retry socket
                                              (list :type :auth-ok :player-id player-id)
                                              :host host :port port
                                              :max-retries 3
                                              :delay 50)
                 (log-verbose "Registration successful: ~a (~a:~d) -> player-id=~d"
                             username host port player-id))
               (progn
                 (warn "Registration rollback: unable to link character for ~a" username)
                 (with-retry-exponential
                     (deleted (lambda () (db-delete-account username))
                      :max-retries 3
                      :initial-delay 50
                      :max-delay 200
                      :on-final-fail (lambda (e)
                                       (warn "CRITICAL: Failed to delete account ~a after link failure: ~a"
                                             username e)))
                   deleted)
                 (send-net-message-with-retry socket
                                              (list :type :auth-fail :reason :internal-error)
                                              :host host :port port
                                              :max-retries 3
                                              :delay 50)
                 (log-verbose "Registration failed from ~a:~d - internal error (link failure)"
                              host port)))))))
      (t
       ;; Username already exists - don't count as rate limit failure (could be probing)
       (send-net-message-with-retry socket
                                    (list :type :auth-fail :reason :username-taken)
                                    :host host :port port
                                    :max-retries 3
                                    :delay 50)
       (log-verbose "Registration failed from ~a:~d - username ~a already taken"
                   host port username))))))

(defun handle-login-request (client host port message socket game clients elapsed)
  "Handle account login request.
   Supports both encrypted and plaintext credentials."
  (declare (ignore clients))
  ;; Use wall-clock time for rate limiting (elapsed is sim-time, wrong timebase)
  (let ((now-rt (/ (get-internal-real-time) (float internal-time-units-per-second 1.0))))
    (declare (ignorable now-rt))
  ;; Rate limiting check
  (unless (auth-rate-check host now-rt)
    (send-net-message-with-retry socket
                                 (list :type :auth-fail :reason :rate-limited)
                                 :host host :port port
                                 :max-retries 3
                                 :delay 50)
    (log-verbose "Login blocked from ~a:~d - rate limited" host port)
    (return-from handle-login-request nil))
  (multiple-value-bind (username password)
      (extract-auth-credentials message)
    (cond
      ((not (and username password))
       (send-net-message-with-retry socket
                                    (list :type :auth-fail :reason :missing-credentials)
                                    :host host :port port
                                    :max-retries 3
                                    :delay 50)
       (auth-rate-record-failure host now-rt)
       (log-verbose "Login failed from ~a:~d - missing credentials" host port))
      ;; Verify credentials first (with retry for transient DB failures)
      ((not (with-retry-exponential (verified (lambda () (db-verify-credentials username password))
                                      :max-retries 3
                                      :initial-delay 50
                                      :max-delay 200)
              verified))
       (send-net-message-with-retry socket
                                    (list :type :auth-fail :reason :bad-credentials)
                                    :host host :port port
                                    :max-retries 3
                                    :delay 50)
       ;; Bad credentials - count toward rate limit
       (auth-rate-record-failure host now-rt)
       (log-verbose "Login failed from ~a:~d - bad credentials for ~a"
                   host port username))
      ;; Atomically try to register session (prevents double-login race)
      ((not (session-try-register username client))
       (send-net-message-with-retry socket
                                    (list :type :auth-fail :reason :already-logged-in)
                                    :host host :port port
                                    :max-retries 3
                                    :delay 50)
       (log-verbose "Login failed from ~a:~d - account ~a already logged in"
                   host port username))
      (t
       ;; Login successful
       (let* ((character-id (with-retry-exponential (id (lambda () (db-get-character-id username))
                                                      :max-retries 3
                                                      :initial-delay 50
                                                      :max-delay 200)
                              id))
              (world (game-world game))
              (zone (world-zone world))
              (zone-id (and zone (zone-id zone)))
              (player nil))
         (cond
           ;; Existing character - load from DB with Phase 6 validation
           (character-id
            ;; Claim session ownership FIRST (required by db-load-player-validated)
            (unless (claim-session-ownership character-id)
              (session-unregister username)
              (send-net-message-with-retry socket
                                           (list :type :auth-fail :reason :ownership-conflict)
                                           :host host :port port
                                           :max-retries 3
                                           :delay 50)
              (log-verbose "Login failed from ~a:~d - ownership conflict for ~a"
                          host port username)
              (return-from handle-login-request nil))
            ;; Load with 4-outcome validation
            (multiple-value-bind (loaded-player loaded-zone-id action)
                (with-retry-exponential
                    (result (lambda ()
                              (multiple-value-list (db-load-player-validated character-id)))
                     :max-retries 3
                     :initial-delay 100
                     :max-delay 300)
                  (values (first result) (second result) (third result)))
              (case action
                ((:ok :clamp)
                 ;; Normal load or corrected data - proceed
                 (when (eq action :clamp)
                   (log-verbose "Player ~a loaded with corrections" character-id))
                 (setf player loaded-player)
                 ;; Remove any existing player with same ID (from stale session)
                 (let ((existing (find (player-id player) (game-players game)
                                       :key #'player-id)))
                   (when existing
                     (remove-player-from-game game existing)
                     (log-verbose "Removed stale player ~d before re-adding"
                                  (player-id player))))
                 ;; Add loaded player to game
                 (add-player-to-game game player)
                 (log-verbose "Loaded existing character ~d for account ~a"
                              character-id username))
                (:quarantine
                 ;; Account needs admin repair - reject login
                 (release-session-ownership character-id)
                 (session-unregister username)
                 (send-net-message-with-retry socket
                                              (list :type :auth-fail :reason :account-quarantined)
                                              :host host :port port
                                              :max-retries 3
                                              :delay 50)
                 (log-verbose "Login failed from ~a:~d - account quarantined for ~a"
                              host port username)
                 (return-from handle-login-request nil))
                (:reject
                 ;; Dangerous data - reject login
                 (release-session-ownership character-id)
                 (session-unregister username)
                 (send-net-message-with-retry socket
                                              (list :type :auth-fail :reason :data-corrupted)
                                              :host host :port port
                                              :max-retries 3
                                              :delay 50)
                 (log-verbose "Login failed from ~a:~d - data corrupted for ~a"
                              host port username)
                 (return-from handle-login-request nil))
                ((:not-found nil)
                 ;; No character data - should not happen for existing account
                 (release-session-ownership character-id)
                 (session-unregister username)
                 (send-net-message-with-retry socket
                                              (list :type :auth-fail :reason :load-failed)
                                              :host host :port port
                                              :max-retries 3
                                              :delay 50)
                 (log-verbose "Login failed from ~a:~d - load failed for ~a"
                              host port username)
                 (return-from handle-login-request nil)))))
           ;; No character yet - spawn new one (shouldn't happen after registration)
           (t
            (handler-case
                (setf player (spawn-player-at-world world (game-id-source game)))
              (error (e)
                ;; ID allocation failed (storage outage) - return login failure
                (warn "Login failed: ID allocation error for ~a: ~a" username e)
                (session-unregister username)
                (send-net-message-with-retry socket
                                             (list :type :auth-fail :reason :internal-error)
                                             :host host :port port
                                             :max-retries 3
                                             :delay 50)
                (return-from handle-login-request nil)))
            (unless player
              (return-from handle-login-request nil))
            (let ((linked (with-retry-exponential
                              (set-result (lambda () (db-set-character-id username (player-id player)))
                               :max-retries 3
                               :initial-delay 50
                               :max-delay 200
                               :on-final-fail (lambda (e)
                                                (warn "Failed to set character-id for ~a: ~a" username e)))
                            set-result)))
              (if linked
                  (progn
                    (add-player-to-game game player)
                    (log-verbose "Created new character ~d for account ~a"
                                (player-id player) username))
                  (progn
                    (warn "Login rollback: unable to link character for ~a" username)
                    (session-unregister username)
                    (send-net-message-with-retry socket
                                                 (list :type :auth-fail :reason :internal-error)
                                                 :host host :port port
                                                 :max-retries 3
                                                 :delay 50)
                    (log-verbose "Login failed from ~a:~d - internal error (link failure)"
                                host port)
                    (return-from handle-login-request nil))))))

         (when player
           ;; Mark client as authenticated
           (setf (net-client-player client) player)
           (setf (net-client-authenticated-p client) t)
           (setf (net-client-account-username client) (string-downcase username))
           (setf (net-client-last-heard client) elapsed)
           ;; Register session for persistence
           (register-player-session player :zone-id zone-id
                                    :username (string-downcase username))
           ;; Note: active session already registered atomically via session-try-register
           ;; Clear rate limit on success
           (auth-rate-record-success host)
           ;; Send success response (with retry - critical auth message)
           (send-net-message-with-retry socket
                                        (list :type :auth-ok :player-id (player-id player))
                                        :host host :port port
                                        :max-retries 3
                                        :delay 50)
           (log-verbose "Login successful: ~a (~a:~d) -> player-id=~d"
                       username host port (player-id player)))))))))

(defun handle-logout-request (client host port game)
  "Handle logout request from authenticated client."
  (when (and client (net-client-authenticated-p client))
    (let ((username (net-client-account-username client))
          (player (net-client-player client)))
      ;; Clear authentication first to prevent further state changes
      (setf (net-client-authenticated-p client) nil)
      (setf (net-client-account-username client) nil)
      ;; Phase 5: Reset zone tracking to prevent stale state on reconnect
      (setf (net-client-zone-id client) nil)
      (setf (net-client-player client) nil)
      (setf (net-client-needs-full-resync client) nil)
      (when player
        ;; Save and unregister session BEFORE removing from active sessions
        ;; This ensures all state changes are persisted
        (db-logout-player player)
        ;; Remove player from game world
        (remove-player-from-game game player))
      (when username
        ;; Remove from active sessions AFTER save completes
        (session-unregister username)
        (log-verbose "Logout: ~a (~a:~d)" username host port)))))

(defun check-client-timeouts (clients elapsed game)
  "Check for timed-out clients and free their sessions. Returns updated client list."
  (let ((updated-clients nil))
    (dolist (client clients)
      (let ((inactive-time (- elapsed (net-client-last-heard client))))
        (if (and (net-client-authenticated-p client)
                 (> inactive-time *client-timeout-seconds*))
            ;; Client timed out - free the session
            (let ((username (net-client-account-username client))
                  (player (net-client-player client)))
              ;; Clear auth first to prevent further state changes
              (setf (net-client-authenticated-p client) nil)
              (setf (net-client-account-username client) nil)
              ;; Phase 5: Reset zone tracking to prevent stale state on reconnect
              (setf (net-client-zone-id client) nil)
              (setf (net-client-player client) nil)
              (setf (net-client-needs-full-resync client) nil)
              (when player
                ;; Save BEFORE removing from active sessions
                (db-logout-player player)
                ;; Remove player from game world
                (remove-player-from-game game player))
              (when username
                ;; Remove from active sessions AFTER save
                (session-unregister username)
                (log-verbose "Client timeout: ~a (inactive for ~,1fs)"
                            username inactive-time))
              ;; Don't add to updated list - remove this client
              )
            ;; Client still active - keep it
            (push client updated-clients))))
    (nreverse updated-clients)))

(defun run-server (&key (host *net-default-host*)
                        (port *net-default-port*)
                        (max-seconds 0.0)
                        (max-frames 0)
                        (worker-threads 1))
  ;; Run a UDP server that simulates the game and streams snapshots.
  ;; Scaling: This runs ONE zone. For 10k users @ 500/zone, run 20 server processes.
  ;; See SERVER_PERFORMANCE.md for horizontal scaling strategy.
  ;;
  ;; WORKER-THREADS: Number of threads for parallel snapshot sending (default 1).
  ;;   - 1 = serial sending (simple, default)
  ;;   - N = parallel sending across N threads (for high client counts)
  ;;   - Recommended: (get-nproc) on multi-core machines
  ;;   - Safe: Only parallelizes network I/O, simulation remains serial
  (with-fatal-error-log ((format nil "Server runtime (~a:~d)" host port))
    ;; Initialize storage backend from environment variables:
    ;; MMORPG_DB_BACKEND: "memory" or "redis" (default: redis)
    ;; MMORPG_REDIS_HOST: Redis host (default: 127.0.0.1)
    ;; MMORPG_REDIS_PORT: Redis port (default: 6379)
    (let* ((backend-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_DB_BACKEND")
                            #-sbcl (uiop:getenv "MMORPG_DB_BACKEND")
                            "redis"))
           (backend (cond
                      ((string-equal backend-str "redis") :redis)
                      ((string-equal backend-str "memory") :memory)
                      (t (progn
                           (warn "Unknown MMORPG_DB_BACKEND=~a, defaulting to redis" backend-str)
                           :redis))))
           (redis-host (or #+sbcl (sb-ext:posix-getenv "MMORPG_REDIS_HOST")
                           #-sbcl (uiop:getenv "MMORPG_REDIS_HOST")
                           "127.0.0.1"))
           (redis-port-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_REDIS_PORT")
                               #-sbcl (uiop:getenv "MMORPG_REDIS_PORT")
                               "6379"))
           (redis-port (or (ignore-errors (parse-integer redis-port-str)) 6379)))
      (init-storage :backend backend :host redis-host :port redis-port)
      ;; Load Redis Lua scripts for atomic operations (Phase 5 - Trade System)
      (load-trade-scripts))
    ;; Initialize profiling from environment variables (Phase 6)
    ;; MMORPG_PROFILE: "1" to enable timing hooks
    ;; MMORPG_VERBOSE_GC: "1" to log GC stats per frame
    (let ((profile-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_PROFILE")
                           #-sbcl (uiop:getenv "MMORPG_PROFILE")))
          (gc-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_VERBOSE_GC")
                      #-sbcl (uiop:getenv "MMORPG_VERBOSE_GC")))
          (snapshot-rate-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_SNAPSHOT_RATE")
                                 #-sbcl (uiop:getenv "MMORPG_SNAPSHOT_RATE")))
          (binary-snapshots-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_BINARY_SNAPSHOTS")
                                    #-sbcl (uiop:getenv "MMORPG_BINARY_SNAPSHOTS"))))
      (when (and profile-str (string= profile-str "1"))
        (setf *profile-enabled* t)
        (init-profile-log 10000)
        (format t "~&SERVER: Profiling enabled~%"))
      (when (and gc-str (string= gc-str "1"))
        (setf *verbose-gc* t)
        (format t "~&SERVER: GC monitoring enabled~%"))
      ;; Phase 3: Configurable snapshot rate (default 20Hz)
      (when snapshot-rate-str
        (let ((rate (parse-integer snapshot-rate-str :junk-allowed t)))
          (when (and rate (> rate 0) (<= rate 60))
            (setf *snapshot-rate-hz* rate
                  *snapshot-interval* (/ 1.0 rate))
            (format t "~&SERVER: Snapshot rate set to ~dHz~%" rate))))
      ;; Phase 3 Task 3.1: Binary snapshot encoding
      (when (and binary-snapshots-str (string= binary-snapshots-str "1"))
        (setf *use-binary-snapshots* t)
        (ensure-binary-send-buffer)
        (format t "~&SERVER: Binary snapshots enabled~%")))
    ;; Phase 4 Task 4.4: NPC object pooling
    (let ((npc-pool-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_NPC_POOL")
                            #-sbcl (uiop:getenv "MMORPG_NPC_POOL"))))
      (when (and npc-pool-str (string= npc-pool-str "1"))
        (setf *use-npc-pool* t)
        (prewarm-npc-pool 256)
        (format t "~&SERVER: NPC object pooling enabled (256 pre-warmed)~%")))
    ;; Phase 5 Task 5.4: Strategic GC scheduling
    (let ((gc-sched-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_GC_SCHEDULING")
                            #-sbcl (uiop:getenv "MMORPG_GC_SCHEDULING"))))
      (when (and gc-sched-str (string= gc-sched-str "1"))
        (setf *gc-scheduling-enabled* t
              *gc-last-time* 0.0)
        (format t "~&SERVER: Strategic GC scheduling enabled (~,0fs interval)~%"
                *gc-interval-seconds*)))
    ;; Auth throughput: configurable worker count, queue depth, message cap
    (let ((auth-workers-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_AUTH_WORKERS")
                                #-sbcl (uiop:getenv "MMORPG_AUTH_WORKERS")))
          (auth-queue-max-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_AUTH_QUEUE_MAX")
                                  #-sbcl (uiop:getenv "MMORPG_AUTH_QUEUE_MAX")))
          (max-msg-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_MAX_MESSAGES_PER_TICK")
                           #-sbcl (uiop:getenv "MMORPG_MAX_MESSAGES_PER_TICK"))))
      (when auth-workers-str
        (let ((n (parse-integer auth-workers-str :junk-allowed t)))
          (when (and n (> n 0) (<= n 32))
            (setf *auth-worker-count* n))))
      (when auth-queue-max-str
        (let ((n (parse-integer auth-queue-max-str :junk-allowed t)))
          (when (and n (>= n 0))
            (setf *auth-queue-max-depth* n))))
      (when max-msg-str
        (let ((n (parse-integer max-msg-str :junk-allowed t)))
          (when (and n (> n 0))
            (setf *max-messages-per-tick* n)))))
    ;; Step 11: Configurable PBKDF2 iteration count
    (let ((hash-iter-str (or #+sbcl (sb-ext:posix-getenv "MMORPG_PASSWORD_HASH_ITERATIONS")
                              #-sbcl (uiop:getenv "MMORPG_PASSWORD_HASH_ITERATIONS"))))
      (when hash-iter-str
        (let ((n (parse-integer hash-iter-str :junk-allowed t)))
          (when (and n (>= n 1000) (<= n 1000000))
            (setf *password-hash-iterations* n)
            (format t "~&SERVER: PBKDF2 iterations set to ~d~%" n)))))
    ;; Clear any stale session state from previous REPL runs
    ;; (Important when restarting server without restarting the Lisp process)
    (log-verbose "Clearing stale sessions (active=~d, player=~d)"
                 (hash-table-count *active-sessions*)
                 (hash-table-count *player-sessions*))
    (session-clear-all)
    (clear-all-player-sessions)
    (log-verbose "Sessions cleared")
    ;; Initialize auth encryption (generates server keypair)
    ;; The server public key should be shared with clients for encrypted auth
    (when *auth-encryption-enabled*
      (let ((pub-key (init-server-encryption)))
        (format t "~&SERVER: Auth encryption enabled. Public key: ~a~%" pub-key)
        (finish-output)))
    (let* ((socket (usocket:socket-connect nil nil
                                          :protocol :datagram
                                          :local-host host
                                          :local-port port))
           (recv-buffer (make-net-buffer))
           (game (make-server-game))
           (clients nil)
           (stop-flag nil)
           (stop-reason nil)
           (last-flush-time 0.0)
           (last-ownership-refresh-time 0.0)
           (last-auth-metrics-time 0.0)
           ;; Auth worker threads for non-blocking login/registration
           (auth-request-queue (make-auth-queue-instance))
           (auth-result-queue (make-auth-queue-instance :max-depth 0)) ; result queue unbounded
           (auth-workers (start-auth-workers auth-request-queue auth-result-queue
                                             game *auth-worker-count*)))
      ;; Set global admin variables
      (setf *server-game* game)
      (setf *server-socket* socket)
      (setf *server-clients* clients)
      (setf *server-start-time* (get-internal-real-time))
      (setf *server-total-saves* 0)
      (format t "~&SERVER: listening on ~a:~d (worker-threads=~d, auth-workers=~d, queue-max=~d)~%"
              host port worker-threads *auth-worker-count* *auth-queue-max-depth*)
      (log-verbose "Server config: tick=~,3fs buffer=~d workers=~d auth-workers=~d max-msg/tick=~d"
                   *sim-tick-seconds* *net-buffer-size* worker-threads
                   *auth-worker-count* *max-messages-per-tick*)
      (finish-output)
      (unwind-protect
           (handler-case
               (loop :with elapsed = 0.0
                     :with frames = 0
                     :with accumulator = 0.0
                     :with snapshot-seq = 0  ; Delta compression sequence counter
                     :with snapshot-accumulator = 0.0  ; Phase 3: decouple snapshot rate
                     :with tick-units = (floor (* *sim-tick-seconds*
                                                  internal-time-units-per-second))
                     :until (or stop-flag
                                (and (> max-seconds 0.0)
                                     (>= elapsed max-seconds))
                                (and (> max-frames 0)
                                     (>= frames max-frames)))
                     :do ;; Track frame start time for accurate sleep
                         (let ((frame-start (get-internal-real-time)))
                         ;; 1. Receive pending intents (non-blocking UDP receive)
                         ;; Step 7: Cap messages per tick to prevent starvation
                         (let ((msg-count 0)
                               (now-rt (/ (float (get-internal-real-time) 1.0d0)
                                          (float internal-time-units-per-second 1.0d0))))
                         (loop
                           (when (>= msg-count *max-messages-per-tick*)
                             (return))
                           (multiple-value-bind (message host port)
                               (receive-net-message socket recv-buffer)
                             (unless message
                               (return))
                             (incf msg-count)
                             (multiple-value-bind (client next-clients _new)
                                 (register-net-client game clients host port
                                                      :timestamp elapsed)
                               (declare (ignore _new))
                               (setf clients next-clients)
                               (setf *server-clients* next-clients)
                               (case (getf message :type)
                                 (:hello
                                  (log-verbose "Handshake received from ~a:~d" host port)
                                  ;; Respond so client knows server is online
                                  (send-net-message socket (list :type :hello-ack)
                                                    :host host :port port))
                                 (:register
                                  ;; Queue for async processing with backpressure (Steps 3, 6)
                                  (if (auth-rate-check host now-rt)
                                      (multiple-value-bind (username password)
                                          (extract-auth-credentials message)
                                        (if (and username password)
                                            ;; Step 3: try-push with backpressure
                                            (if (auth-queue-try-push auth-request-queue
                                                             (make-auth-request :type :register
                                                                                :host host
                                                                                :port port
                                                                                :username username
                                                                                :password password
                                                                                :client client
                                                                                :timestamp (float now-rt 1.0)))
                                                (auth-metric-incf queued)
                                                ;; Queue full - immediate rejection
                                                (progn
                                                  (auth-metric-incf rejected-busy)
                                                  (send-net-message-with-retry socket
                                                    (list :type :auth-fail :reason :server-busy)
                                                    :host host :port port :max-retries 3 :delay 50)))
                                            (progn
                                              (send-net-message-with-retry socket
                                                                           (list :type :auth-fail :reason :missing-credentials)
                                                                           :host host :port port :max-retries 3 :delay 50)
                                              (auth-rate-record-failure host now-rt))))
                                      (send-net-message-with-retry socket
                                                                   (list :type :auth-fail :reason :rate-limited)
                                                                   :host host :port port :max-retries 3 :delay 50)))
                                 (:login
                                  ;; Queue for async processing with backpressure (Steps 3, 6)
                                  (if (auth-rate-check host now-rt)
                                      (multiple-value-bind (username password)
                                          (extract-auth-credentials message)
                                        (if (and username password)
                                            ;; Step 3: try-push with backpressure
                                            (if (auth-queue-try-push auth-request-queue
                                                             (make-auth-request :type :login
                                                                                :host host
                                                                                :port port
                                                                                :username username
                                                                                :password password
                                                                                :client client
                                                                                :timestamp (float now-rt 1.0)))
                                                (auth-metric-incf queued)
                                                ;; Queue full - immediate rejection
                                                (progn
                                                  (auth-metric-incf rejected-busy)
                                                  (send-net-message-with-retry socket
                                                    (list :type :auth-fail :reason :server-busy)
                                                    :host host :port port :max-retries 3 :delay 50)))
                                            (progn
                                              (send-net-message-with-retry socket
                                                                           (list :type :auth-fail :reason :missing-credentials)
                                                                           :host host :port port :max-retries 3 :delay 50)
                                              (auth-rate-record-failure host now-rt))))
                                      (send-net-message-with-retry socket
                                                                   (list :type :auth-fail :reason :rate-limited)
                                                                   :host host :port port :max-retries 3 :delay 50)))
                                 (:logout
                                  (handle-logout-request client host port game))
                                 (:intent
                                  (when (and client (net-client-authenticated-p client))
                                    ;; Parse ack for delta compression
                                    (let ((ack (getf message :ack)))
                                      (when ack
                                        (setf (net-client-last-acked-seq client) ack)
                                        (setf (net-client-needs-full-resync client) nil)))
                                    (let* ((payload (getf message :payload))
                                           (seq (and payload (getf payload :sequence)))
                                           (player (net-client-player client)))
                                      (when (and player (integerp seq))
                                        (setf (player-last-sequence player) seq))
                                      (apply-intent-plist
                                       (net-client-intent client)
                                       payload))))
                                 (t
                                  (log-verbose "Unknown message type from ~a:~d -> ~s"
                                               host port (getf message :type))))))))
                         ;; 1b. Integrate completed auth results (non-blocking)
                         ;; This adds newly authenticated players to the game
                         (integrate-auth-results auth-result-queue socket game elapsed)
                         ;; 2. Apply client intents to player state (O(clients), cheap)
                         (apply-client-intents clients)
                         ;; 3. Run fixed-tick simulation (O(players * npcs), main bottleneck)
                         (let ((dt *sim-tick-seconds*))
                           (incf elapsed dt)
                           (incf frames)
                           (incf snapshot-accumulator dt)  ; Phase 3: track time for snapshot rate
                           (multiple-value-bind (new-acc transitions)
                               (server-step game nil dt accumulator)
                             (setf accumulator new-acc)
                             (dotimes (_ transitions)
                               (declare (ignore _)))))
                         ;; 3b. Periodic batch flush (tier-2 writes every ~30s)
                         (when (>= (- elapsed last-flush-time) *batch-flush-interval*)
                           (flush-dirty-players)
                           (setf last-flush-time elapsed))
                         ;; 3b2. Periodic auth metrics logging (every 30s)
                         (when (>= (- elapsed last-auth-metrics-time) 30.0)
                           (when (> (auth-metrics-queued *auth-metrics*) 0)
                             (format t "[AUTH] queued=~d processed=~d success=~d fail=~d expired=~d rejected=~d queue-depth=~d~%"
                                     (auth-metrics-queued *auth-metrics*)
                                     (auth-metrics-processed *auth-metrics*)
                                     (auth-metrics-success *auth-metrics*)
                                     (auth-metrics-fail *auth-metrics*)
                                     (auth-metrics-expired *auth-metrics*)
                                     (auth-metrics-rejected-busy *auth-metrics*)
                                     (auth-queue-count auth-request-queue))
                             (finish-output))
                           (setf last-auth-metrics-time elapsed))
                         ;; 3c. Periodic session ownership refresh (every ~30s, half of TTL)
                         (when (>= (- elapsed last-ownership-refresh-time) *ownership-refresh-interval*)
                           (let ((lost-sessions (refresh-all-session-ownerships)))
                             (when lost-sessions
                               ;; Handle lost ownership - full cleanup including net clients
                               ;; Phase 2: refresh-all-session-ownerships now attempts re-claim before
                               ;; reporting as lost, so these are truly lost to another server
                               (dolist (player-id lost-sessions)
                                 (warn "Session ownership lost for player ~a - forcing cleanup" player-id)
                                 (let ((session (gethash player-id *player-sessions*)))
                                   (when session
                                     (let ((player (player-session-player session))
                                           (username (player-session-username session)))
                                       ;; 1. Find and de-auth the net client (Phase 2)
                                       (when username
                                         (let ((client (session-get username)))
                                           (when client
                                             ;; Clear auth fields
                                             (setf (net-client-authenticated-p client) nil)
                                             (setf (net-client-player client) nil)
                                             ;; Remove from clients list
                                             (setf clients (remove client clients :test #'eq))
                                             (log-verbose "De-authed client for ~a due to ownership loss" username)))
                                         ;; Remove from active sessions (by username)
                                         (session-unregister username))
                                       ;; 2. Remove from player sessions locally only (Phase 2)
                                       ;; Don't touch online set - new owner controls that now
                                       (unregister-player-session-local player-id)
                                       ;; 3. Remove player from game world
                                       (when player
                                         (remove-player-from-game game player))))))))
                           (setf last-ownership-refresh-time elapsed))
                         ;; 3d. Check for timed-out clients (free sessions after 30s inactivity)
                         (setf clients (check-client-timeouts clients elapsed game))
                         ;; 4. Send snapshots to all clients (Phase 3: rate-limited)
                         ;; Toggle between Prong 1 (full) and Prong 2 (delta) via *delta-compression-enabled*
                         ;; EXCEPTION HANDLING: Snapshot errors are non-fatal, skip frame and continue.
                         ;; Phase 3: Decouple snapshot rate from sim rate (default 20Hz vs 60Hz sim)
                         (when (and clients (>= snapshot-accumulator *snapshot-interval*))
                           (decf snapshot-accumulator *snapshot-interval*)
                           (handler-case
                               (let* ((events (pop-combat-events (game-combat-events game)))
                                      (event-plists (mapcar #'combat-event->plist events))
                                      (current-seq (incf snapshot-seq)))
                                 (if *delta-compression-enabled*
                                     ;; Prong 2: Delta compression - dirty entities only
                                     (broadcast-snapshots-with-delta socket clients game
                                                                     current-seq event-plists)
                                     ;; Prong 1: Full snapshots - all entities every frame
                                     (let ((state (serialize-game-state-compact game)))
                                       (setf state (plist-put state :seq current-seq))
                                       (send-snapshots-parallel socket clients state event-plists
                                                                worker-threads))))
                             (error (e)
                               (warn "Failed to serialize/send snapshot (frame ~d): ~a" frames e)
                               (log-verbose "Snapshot error, skipping frame: ~a" e))))
                         ;; 4b. Send private state updates (inventory/equipment/stats)
                         (when clients
                           (send-private-states socket clients))
                         ;; 4c. Strategic GC scheduling (Task 5.4)
                         ;; Trigger GC at safe points (after snapshots) to reduce worst-case spikes
                         (when (and *gc-scheduling-enabled*
                                    (>= (- elapsed *gc-last-time*) *gc-interval-seconds*))
                           (setf *gc-last-time* elapsed)
                           #+sbcl (sb-ext:gc :full nil)  ; Incremental, not full
                           (when *verbose-gc*
                             (format t "~&SERVER: Scheduled GC at ~,1fs~%" elapsed)))
                         ;; 5. Sleep for remaining tick time (accounts for processing duration)
                         ;; This ensures consistent tick rate regardless of frame complexity.
                         (let* ((frame-end (get-internal-real-time))
                                (frame-elapsed (- frame-end frame-start))
                                (remaining-units (- tick-units frame-elapsed)))
                           (when (> remaining-units 0)
                             (sleep (/ remaining-units internal-time-units-per-second))))))
             #+sbcl
             (sb-sys:interactive-interrupt ()
               (setf stop-flag t stop-reason "interrupt")))
        (when stop-reason
          (format t "~&SERVER: shutdown requested (~a).~%" stop-reason)
          (finish-output))
        ;; Stop auth worker threads
        (stop-auth-workers auth-request-queue auth-workers)
        ;; Graceful shutdown: flush all dirty players and close storage
        (db-shutdown-flush)
        (usocket:socket-close socket)))))

(defun run-client (&key (host *net-default-host*)
                        (port *net-default-port*)
                        (max-seconds 0.0)
                        (max-frames 0)
                        (auto-login-username nil)
                        (auto-login-password nil))
  ;; Run a client that connects to the UDP server and renders snapshots.
  ;; If auto-login credentials are provided, attempts register first, then login on failure.
  (with-fatal-error-log ((format nil "Client runtime (~a:~d)" host port))
    (log-verbose "Client starting: connecting to ~a:~d" host port)
    ;; Set window flags before init (must be called before with-window)
    (when *window-resize-enabled*
      (raylib:set-config-flags +flag-window-resizable+))
    (raylib:with-window ("Hello MMO" (*window-width* *window-height*))
      (raylib:set-target-fps *client-target-fps*)
      (raylib:set-exit-key 0)
      (raylib:init-audio-device)
      (let* ((socket (usocket:socket-connect host port :protocol :datagram))
             (recv-buffer (make-net-buffer))
             (game (make-game))
             (ui (game-ui game)))
        (setf (game-net-role game) :client)
        (unwind-protect
             (loop :with elapsed = 0.0
                   :with frames = 0
                   :with auto-login-attempted = nil
                   :with auto-login-register-failed = nil
                   :with busy-retry-count = 0       ; Step 8: server-busy retry counter
                   :with busy-retry-time = 0.0      ; Step 8: when to retry after :server-busy
                   :with busy-retry-type = nil       ; :register or :login
                   :with busy-retry-username = nil
                   :with busy-retry-password = nil
                   :with last-auth-type = nil        ; Tracks last sent auth type for busy retry
                   :with last-snapshot-seq = nil  ; Delta compression: last received seq
                   :with chunk-buf = (make-chunk-buffer)  ; UDP fragmentation: reassembly buffer
                   :until (or (raylib:window-should-close)
                              (ui-exit-requested ui)
                              (and (> max-seconds 0.0)
                                   (>= elapsed max-seconds))
                              (and (> max-frames 0)
                                   (>= frames max-frames)))
                   :do (let ((dt (raylib:get-frame-time)))
                         (incf elapsed dt)
                         (incf frames)

                         ;; Window resize handling (when enabled)
                         (handle-window-resize game)

                         ;; Login screen phase
                         (cond
                           ((and (ui-login-active ui) (not (ui-auth-complete ui)))
                            ;; Periodic ping to detect server availability and measure RTT
                            (when (>= elapsed (ui-server-next-ping ui))
                              (handler-case
                                  (progn
                                    (setf (ui-ping-send-time ui) elapsed)
                                    (send-net-message socket (list :type :hello)))
                                (error ()
                                  ;; Send failed - server unreachable
                                  (setf (ui-server-status ui) :offline)))
                              ;; Schedule next ping: random 3-7s
                              (setf (ui-server-next-ping ui)
                                    (+ elapsed (+ 3.0 (random 4.0)))))
                            ;; Auto-login: Try register first, then login if taken
                            (when (and auto-login-username auto-login-password
                                      (not auto-login-attempted))
                              (handler-case
                                  (progn
                                    (send-auth-message socket :register
                                                       auto-login-username auto-login-password)
                                    (setf auto-login-attempted t
                                          last-auth-type :register)
                                    (log-verbose "Auto-login: attempting register for ~a" auto-login-username))
                                (error ()
                                  (setf (ui-server-status ui) :offline))))
                            ;; Step 8: Retry auth after :server-busy with exponential backoff
                            (when (and busy-retry-type
                                      (> busy-retry-time 0.0)
                                      (>= elapsed busy-retry-time)
                                      (<= busy-retry-count 3))
                              (handler-case
                                  (progn
                                    (send-auth-message socket busy-retry-type
                                                       busy-retry-username busy-retry-password)
                                    (log-verbose "Retrying ~a after server-busy (attempt ~d)"
                                                busy-retry-type busy-retry-count))
                                (error ()
                                  (setf (ui-server-status ui) :offline)))
                              (setf busy-retry-time 0.0)) ; Clear so we don't resend
                            ;; Update login input
                            (update-login-input ui)
                            ;; F11 toggles fullscreen on login screen
                            (when (raylib:is-key-pressed +key-f11+)
                              (raylib:toggle-fullscreen))
                            ;; Enter key triggers login (only if server online)
                            (when (and (raylib:is-key-pressed +key-enter+)
                                      (ui-username-buffer ui)
                                      (plusp (length (ui-username-buffer ui))))
                              (if (eq (ui-server-status ui) :offline)
                                  (setf (ui-auth-error-message ui) "Server is offline")
                                  (handler-case
                                      (progn
                                        (send-auth-message socket :login
                                                           (ui-username-buffer ui)
                                                           (ui-username-buffer ui))
                                        (setf (ui-auth-error-message ui) nil
                                              last-auth-type :login)
                                        (log-verbose "Sending login request for ~a (Enter key)" (ui-username-buffer ui)))
                                    (error ()
                                      (setf (ui-server-status ui) :offline
                                            (ui-auth-error-message ui) "Server is offline")))))
                            ;; Check for login messages from server
                            (let ((got-message nil))
                              (loop
                                (multiple-value-bind (message _host _port)
                                    (receive-net-message socket recv-buffer)
                                  (declare (ignore _host _port))
                                  (unless message
                                    (return))
                                  (setf got-message t)
                                  (case (getf message :type)
                                  (:auth-ok
                                   ;; Store player-id from auth-ok (used to find local player in snapshots)
                                   ;; Critical since network-only snapshots don't include per-client player-id
                                   (let ((player-id (getf message :player-id)))
                                     (when player-id
                                       (setf (game-net-player-id game) player-id)
                                       (log-verbose "Assigned player ID: ~d" player-id)))
                                   (setf (ui-auth-complete ui) t)
                                   (setf (ui-login-active ui) nil)
                                   (log-verbose "Authentication successful"))
                                  (:auth-fail
                                   (let ((reason (getf message :reason)))
                                     ;; Auto-login: if register failed because username taken, try login
                                     (when (and auto-login-username auto-login-password
                                               auto-login-attempted
                                               (not auto-login-register-failed)
                                               (eq reason :username-taken))
                                       (send-auth-message socket :login
                                                          auto-login-username auto-login-password)
                                       (setf auto-login-register-failed t
                                             last-auth-type :login)
                                       (log-verbose "Auto-login: register failed, trying login for ~a"
                                                   auto-login-username))
                                     ;; Step 8: Handle :server-busy with exponential backoff retry
                                     (when (eq reason :server-busy)
                                       (if (< busy-retry-count 3)
                                           (let* ((delay (expt 2.0 busy-retry-count)) ; 1s, 2s, 4s
                                                  ;; Re-derive credentials for retry
                                                  ;; MVP: password = username (ui-password-buffer unused)
                                                  (uname (or auto-login-username
                                                             (ui-username-buffer ui)))
                                                  (pwd (or auto-login-password
                                                           (ui-username-buffer ui))))
                                             (setf busy-retry-time (+ elapsed delay))
                                             (incf busy-retry-count)
                                             ;; Use last-auth-type to retry the same action
                                             (setf busy-retry-type (or last-auth-type :login))
                                             (setf busy-retry-username uname
                                                   busy-retry-password pwd)
                                             (setf (ui-auth-error-message ui)
                                                   (format nil "Server busy, retrying (~d/3)..." busy-retry-count))
                                             (log-verbose "Server busy, retry ~d in ~,1fs" busy-retry-count delay))
                                           ;; Max retries exceeded
                                           (setf (ui-auth-error-message ui)
                                                 "Server is too busy. Please try again later.")))
                                     (unless (eq reason :server-busy)
                                       (setf (ui-auth-error-message ui)
                                             (case reason
                                               (:bad-credentials "Invalid username or password")
                                               (:username-taken "Username already taken")
                                               (:already-logged-in "Account already logged in")
                                               (:missing-credentials "Missing username or password")
                                               (:request-expired "Request timed out, please try again")
                                               (:wrong-zone (let ((zone-id (getf message :zone-id)))
                                                              (if zone-id
                                                                  (format nil "Wrong server for this character (zone: ~a)" zone-id)
                                                                  "Wrong server for this character")))
                                               (t "Authentication failed"))))
                                     (log-verbose "Authentication failed: ~a" reason)))
                                  (:private-state
                                   (apply-private-state game
                                                        (getf message :payload)
                                                        :player-id (getf message :player-id)))
                                  (:hello-ack
                                   ;; Server responded to ping - calculate RTT
                                   (let ((rtt-ms (round (* 1000.0 (- elapsed (ui-ping-send-time ui))))))
                                     (setf (ui-ping-rtt-ms ui) rtt-ms)
                                     (log-verbose "Server ping: ~dms" rtt-ms)))
                                  (t
                                   (log-verbose "Unexpected message during login: ~s"
                                               (getf message :type))))))
                              ;; Update server status based on whether we got a message
                              ;; Timeout (10s) must exceed max ping interval (7s) to avoid false offline
                              (if got-message
                                  (setf (ui-server-status ui) :online
                                        (ui-server-last-heard ui) elapsed)
                                  (when (> (- elapsed (ui-server-last-heard ui)) 10.0)
                                    (setf (ui-server-status ui) :offline))))
                            ;; Draw login screen and handle button clicks
                            (raylib:begin-drawing)
                            (let ((action (draw-login-screen ui)))
                              (when action
                                (let ((username (ui-username-buffer ui)))
                                  (when (and username (plusp (length username)))
                                    (if (eq (ui-server-status ui) :offline)
                                        (setf (ui-auth-error-message ui) "Server is offline")
                                        ;; For MVP: password = username
                                        (handler-case
                                            (case action
                                              (:login
                                               (send-auth-message socket :login username username)
                                               (setf (ui-auth-error-message ui) nil
                                                     last-auth-type :login)
                                               (log-verbose "Sending login request for ~a" username))
                                              (:register
                                               (send-auth-message socket :register username username)
                                               (setf (ui-auth-error-message ui) nil
                                                     last-auth-type :register)
                                               (log-verbose "Sending register request for ~a" username)))
                                          (error ()
                                            (setf (ui-server-status ui) :offline
                                                  (ui-auth-error-message ui) "Server is offline"))))))))
                            (raylib:end-drawing))

                           ;; Gameplay phase (after authentication)
                           (t
                            ;; Update client time for interpolation
                            (when (game-interpolation-buffer game)
                              (incf (game-client-time game) dt))

                            ;; Periodic ping to measure RTT (every 5-8 seconds)
                            (when (>= elapsed (ui-server-next-ping ui))
                              (handler-case
                                  (progn
                                    (setf (ui-ping-send-time ui) elapsed)
                                    (send-net-message socket (list :type :hello)))
                                (error () nil))
                              (setf (ui-server-next-ping ui)
                                    (+ elapsed (+ 5.0 (random 3.0)))))

                            (update-client-input game dt)
                            (dolist (request (drain-net-requests game))
                              (send-net-message socket request))

                            ;; Handle intent sending (with optional prediction)
                            ;; Includes ack for delta compression if we received a snapshot
                            (let ((intent (game-client-intent game))
                                  (pred (game-prediction-state game)))
                              (if (and *client-prediction-enabled* pred)
                                  ;; Prediction enabled: store input, apply locally, send with sequence
                                  (let ((seq (store-prediction-input pred intent (game-client-time game))))
                                    (apply-local-prediction game intent dt)
                                    (send-intent-message socket intent :sequence seq :ack last-snapshot-seq))
                                  ;; No prediction: just send intent normally (with ack for delta)
                                  (send-intent-message socket intent :ack last-snapshot-seq)))
                            ;; Clear one-shot intent fields after sending
                            ;; NOTE: Do NOT clear pickup target here - it needs to persist
                            ;; until player walks to target and picks up. Server handles
                            ;; clearing via sync-player-pickup-target when pickup completes.
                            (clear-requested-chat-message (game-client-intent game))
                            (clear-requested-drop-item (game-client-intent game))

                            ;; Receive and apply snapshots
                            (let ((latest-state nil)
                                  (latest-events nil)
                                  (latest-player-id nil)
                                  (latest-sequence 0)
                                  (latest-snapshot-seq nil)  ; Delta compression seq
                                  (latest-private nil)
                                  (latest-private-player-id nil))
                              (loop
                                (multiple-value-bind (message _host _port)
                                    (receive-net-message socket recv-buffer)
                                  (declare (ignore _host _port))
                                  (unless message
                                    (return))
                                  (case (getf message :type)
                                    (:snapshot
                                     (setf latest-state (getf message :state)
                                           latest-player-id (or (getf message :player-id)
                                                                latest-player-id)
                                           latest-sequence (or (getf message :last-sequence) 0))
                                     ;; Extract delta compression sequence from state
                                     (let ((state-seq (getf latest-state :seq)))
                                       (when state-seq
                                         (setf latest-snapshot-seq state-seq)))
                                     (dolist (event (getf message :events))
                                       (push event latest-events)))
                                    ;; Fragmented snapshot chunk (Prong 3)
                                    (:snapshot-chunk
                                     (multiple-value-bind (reassembled-state reassembled-events)
                                         (receive-snapshot-chunk chunk-buf message
                                                                 (game-client-time game))
                                       (when reassembled-state
                                         ;; Complete snapshot reassembled - use it
                                         (setf latest-state reassembled-state)
                                         (let ((state-seq (getf reassembled-state :seq)))
                                           (when state-seq
                                             (setf latest-snapshot-seq state-seq)))
                                         (dolist (event reassembled-events)
                                           (push event latest-events)))))
                                    (:private-state
                                     (setf latest-private (getf message :payload)
                                           latest-private-player-id (getf message :player-id)))
                                    (:hello-ack
                                     ;; Server responded to ping - calculate RTT
                                     (let ((rtt-ms (round (* 1000.0 (- elapsed (ui-ping-send-time ui))))))
                                       (setf (ui-ping-rtt-ms ui) rtt-ms)))
                                    (t
                                     (log-verbose "Unknown message type from server: ~s"
                                                  (getf message :type)))))
                              ;; Update last-snapshot-seq for next frame's ack
                              (when latest-snapshot-seq
                                (setf last-snapshot-seq latest-snapshot-seq))
                              (when latest-state
                                (multiple-value-bind (zone-id delta-positions)
                                    (apply-snapshot game latest-state (nreverse latest-events)
                                                    :player-id latest-player-id)
                                  (declare (ignore zone-id))
                                  ;; Buffer snapshot for interpolation
                                  (when (game-interpolation-buffer game)
                                    (let* ((buffer (game-interpolation-buffer game))
                                           (local-id (or (game-net-player-id game) 0))
                                           ;; For deltas, get previous positions to avoid
                                           ;; capturing stale interpolated values
                                           (prev-snap
                                            (when (> (interpolation-buffer-count buffer) 0)
                                              (get-interpolation-snapshot-at-index
                                               buffer
                                               (1- (interpolation-buffer-count buffer)))))
                                           (prev-pos
                                            (when prev-snap
                                              (interpolation-snapshot-entity-positions prev-snap)))
                                           (positions
                                            (capture-entity-positions game local-id
                                                                      :delta-positions delta-positions
                                                                      :previous-positions prev-pos
                                                                      :buffer buffer))
                                           (snapshot (%make-interpolation-snapshot
                                                      :timestamp (game-client-time game)
                                                      :entity-positions positions)))
                                      (push-interpolation-snapshot buffer snapshot)
                                      (setf (game-last-snapshot-time game)
                                            (game-client-time game))))
                                  ;; Reconcile prediction if enabled
                                  (when (and *client-prediction-enabled*
                                             (game-prediction-state game)
                                             (> latest-sequence 0))
                                    (let ((player (game-player game)))
                                      (when player
                                        (reconcile-prediction game
                                                             (player-x player)
                                                             (player-y player)
                                                             latest-sequence))))))
                              (when latest-private
                                (apply-private-state game latest-private
                                                     :player-id latest-private-player-id)))

                            ;; Interpolate remote entities before drawing
                            (interpolate-remote-entities game)
                            (process-combat-events game)
                            (draw-game game))))))
          (shutdown-game game)
          (usocket:socket-close socket)
          (raylib:close-audio-device))))))
