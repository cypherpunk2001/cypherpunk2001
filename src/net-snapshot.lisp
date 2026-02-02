;; net-snapshot.lisp — snapshot send/receive, delta logic, compression, fragmentation, interpolation, prediction
;; Split from net.lisp. Load order: net-protocol → net-auth → net-snapshot → net-client → net-server → net
(in-package #:mmorpg)

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

;;;; Delta compression and zone-filtered snapshot broadcasting

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
  ;; to all clients. This reduces encoding from O(clients * state_size) to O(state_size).
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

;;;; Private state sending

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

;;;; Teleport detection and sync state reset

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

;;;; Apply snapshot (client-side)

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
            (handle-zone-transition game :old-x old-x :old-y old-y))
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

;;;; Intent sending (client-side)

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

;;;; Server load handler

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
