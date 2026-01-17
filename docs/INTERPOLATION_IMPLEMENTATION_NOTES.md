# Interpolation Implementation Notes

## Code Locations for Implementation

### 1. Add Interpolation State to Game Struct (src/types.lisp:167)

```lisp
(defstruct (game (:constructor %make-game))
  ;; Existing fields...
  world player players npcs entities id-source audio ui render assets camera editor
  combat-events client-intent net-role net-requests net-player-id

  ;; NEW: Interpolation support (client-side only)
  (interpolation-buffer nil)  ; Ring buffer of recent snapshots
  (interpolation-delay 0.15)  ; Render 150ms behind (1.5 server ticks)
  (interpolation-time 0.0)    ; Current interpolated time
  (last-snapshot-time 0.0))   ; When last snapshot arrived
```

### 2. Create Interpolation Buffer Structure (src/types.lisp)

```lisp
(defstruct interpolation-snapshot
  ;; Snapshot of entity positions at a specific time
  (timestamp 0.0)
  (entity-positions nil)) ; Hash table: entity-id -> (x y)

(defstruct interpolation-buffer
  ;; Circular buffer of recent snapshots for interpolation
  (snapshots (make-array 4)) ; Last 4 snapshots (400ms @ 100ms tick)
  (head 0)                   ; Write position
  (count 0))                 ; Number of valid snapshots

(defun push-interpolation-snapshot (buffer snapshot)
  ;; Add snapshot to ring buffer
  (let ((head (interpolation-buffer-head buffer)))
    (setf (aref (interpolation-buffer-snapshots buffer) head) snapshot
          (interpolation-buffer-head buffer) (mod (1+ head) 4))
    (when (< (interpolation-buffer-count buffer) 4)
      (incf (interpolation-buffer-count buffer)))))

(defun get-interpolation-snapshots (buffer render-time)
  ;; Return two snapshots to interpolate between for RENDER-TIME
  ;; Returns: (snapshot-before snapshot-after alpha)
  ;; Alpha is [0.0, 1.0] for linear interpolation
  (let ((snapshots (interpolation-buffer-snapshots buffer))
        (count (interpolation-buffer-count buffer)))
    (when (< count 2)
      (return-from get-interpolation-snapshots nil))

    ;; Find snapshots where time-before <= render-time <= time-after
    (loop :for i :from 0 :below (1- count)
          :for snap-a = (aref snapshots (mod (- (interpolation-buffer-head buffer) count i) 4))
          :for snap-b = (aref snapshots (mod (- (interpolation-buffer-head buffer) count (1+ i)) 4))
          :when (and snap-a snap-b
                     (<= (interpolation-snapshot-timestamp snap-a) render-time)
                     (>= (interpolation-snapshot-timestamp snap-b) render-time))
            :do (let* ((time-a (interpolation-snapshot-timestamp snap-a))
                       (time-b (interpolation-snapshot-timestamp snap-b))
                       (alpha (/ (- render-time time-a) (- time-b time-a))))
                  (return (values snap-a snap-b alpha))))))
```

### 3. Store Snapshot When Received (src/net.lisp:436 in run-client)

Current code:
```lisp
(when latest-state
  (apply-snapshot game latest-state (nreverse latest-events)
                  :player-id latest-player-id))
```

Enhanced with interpolation:
```lisp
(when latest-state
  ;; Apply snapshot to authoritative state
  (apply-snapshot game latest-state (nreverse latest-events)
                  :player-id latest-player-id)

  ;; NEW: Store snapshot for interpolation
  (when (eq (game-net-role game) :client)
    (let* ((buffer (game-interpolation-buffer game))
           (current-time (get-internal-real-time))
           (snapshot (make-interpolation-snapshot
                       :timestamp (/ current-time internal-time-units-per-second)
                       :entity-positions (capture-entity-positions game))))
      (when buffer
        (push-interpolation-snapshot buffer snapshot))
      (setf (game-last-snapshot-time game) current-time))))
```

### 4. Interpolate Before Rendering (src/net.lisp:439 in run-client)

Current code:
```lisp
(process-combat-events game)
(draw-game game)
```

Enhanced with interpolation:
```lisp
(process-combat-events game)

;; NEW: Interpolate entity positions before drawing
(when (eq (game-net-role game) :client)
  (interpolate-entity-positions game dt))

(draw-game game)
```

### 5. Interpolation Function (new in src/net.lisp or src/client.lisp)

```lisp
(defun capture-entity-positions (game)
  ;; Capture current positions of all entities (except local player)
  (let ((positions (make-hash-table)))
    (loop :for entity :across (game-entities game)
          :when (not (eq entity (game-player game))) ; Don't interpolate local player
          :do (setf (gethash (entity-id entity) positions)
                    (list (entity-x entity) (entity-y entity))))
    positions))

(defun interpolate-entity-positions (game dt)
  ;; Interpolate entity positions between snapshots
  (let* ((buffer (game-interpolation-buffer game))
         (delay (game-interpolation-delay game))
         (current-time (/ (get-internal-real-time) internal-time-units-per-second))
         (render-time (- current-time delay)))

    ;; Update interpolation time
    (setf (game-interpolation-time game) render-time)

    ;; Get snapshots to interpolate between
    (multiple-value-bind (snap-a snap-b alpha)
        (get-interpolation-snapshots buffer render-time)

      (when (and snap-a snap-b)
        ;; Interpolate each entity (except local player)
        (loop :for entity :across (game-entities game)
              :for entity-id = (entity-id entity)
              :when (not (eq entity (game-player game)))
              :do (let* ((pos-a (gethash entity-id
                                        (interpolation-snapshot-entity-positions snap-a)))
                         (pos-b (gethash entity-id
                                        (interpolation-snapshot-entity-positions snap-b))))
                    (when (and pos-a pos-b)
                      ;; Linear interpolation
                      (setf (entity-x entity) (lerp (first pos-a) (first pos-b) alpha)
                            (entity-y entity) (lerp (second pos-a) (second pos-b) alpha)))))))))

(defun lerp (a b alpha)
  ;; Linear interpolation: a + (b - a) * alpha
  (+ a (* (- b a) alpha)))
```

### 6. Initialize Interpolation Buffer (src/main.lisp make-game)

Add to `make-game` for client mode:
```lisp
(defun make-game ()
  (multiple-value-bind (world player players npcs entities id-source combat-events)
      (make-sim-state)
    (let* ((audio (make-audio))
           (ui (make-ui))
           (render (make-render))
           (assets (load-assets world))
           (camera (make-camera))
           (editor (make-editor world assets player))
           (client-intent (make-intent :target-x (player-x player)
                                       :target-y (player-y player)))
           ;; NEW: Create interpolation buffer
           (interp-buffer (make-interpolation-buffer)))
      (%make-game :world world
                  :player player
                  :players players
                  :npcs npcs
                  :entities entities
                  :id-source id-source
                  :audio audio
                  :ui ui
                  :render render
                  :assets assets
                  :camera camera
                  :editor editor
                  :combat-events combat-events
                  :client-intent client-intent
                  :net-role :local
                  :net-requests nil
                  :net-player-id (player-id player)
                  ;; NEW: Add interpolation support
                  :interpolation-buffer interp-buffer
                  :interpolation-delay 0.15  ; 150ms delay
                  :interpolation-time 0.0
                  :last-snapshot-time 0.0))))
```

## Testing the Implementation

### Visual Test
1. Run server: `make server`
2. Run two clients: `make client` (in separate terminals)
3. Move one character and observe in the other client
4. Should see smooth movement, not teleporting

### Latency Test
```bash
# Add 100ms artificial latency
sudo tc qdisc add dev lo root netem delay 100ms

# Run server and client
make server
make client

# Observe: movement should still feel smooth
# Remove latency when done
sudo tc qdisc del dev lo root
```

### Debug Visualization
Add to rendering code to visualize interpolation:
```lisp
(defun draw-interpolation-debug (game)
  ;; Draw interpolation state for debugging
  (let* ((buffer (game-interpolation-buffer game))
         (render-time (game-interpolation-time game))
         (current-time (/ (get-internal-real-time) internal-time-units-per-second)))

    ;; Draw timeline
    (raylib:draw-text (format nil "Render time: ~,3f" render-time) 10 40 20 +color-white+)
    (raylib:draw-text (format nil "Current time: ~,3f" current-time) 10 60 20 +color-white+)
    (raylib:draw-text (format nil "Delay: ~,3f" (- current-time render-time)) 10 80 20 +color-white+)
    (raylib:draw-text (format nil "Snapshots: ~d" (interpolation-buffer-count buffer)) 10 100 20 +color-white+)

    ;; Draw interpolated vs actual positions
    (loop :for entity :across (game-entities game)
          :when (typep entity 'player)
          :do (let ((actual-x (player-x entity))
                    (actual-y (player-y entity)))
                ;; Draw actual position as red dot
                (raylib:draw-circle (floor actual-x) (floor actual-y) 3 +color-red+)))))
```

## Performance Considerations

### Memory
- Ring buffer of 4 snapshots @ 100ms = 400ms history
- Each snapshot stores position hash table
- For 500 entities: 500 * 2 floats * 4 snapshots = 4KB (negligible)

### CPU
- Interpolation runs every frame (~60 FPS)
- For 500 entities: 500 lerp operations = ~0.01ms (negligible)
- Hash table lookups: O(1) per entity

### Network
- No change: snapshots are already sent every 100ms
- Could reduce snapshot rate if interpolation is smooth enough

## Known Limitations

### 1. Interpolation Delay
- Entities rendered 150ms behind actual time
- Trade-off: smoother movement vs. slight "lag"
- Acceptable for RPG, problematic for FPS

### 2. New Entities
- Entity appearing suddenly has no previous snapshot
- Solution: Fade in or snap to position

### 3. Zone Transitions
- Entity changing zones loses interpolation history
- Solution: Reset interpolation buffer on zone change

### 4. Local Player
- Don't interpolate local player (use prediction instead)
- Interpolating local player would make controls feel laggy

## Next Steps

1. **Implement basic interpolation** (this document)
2. **Test with 2+ clients** (does movement look smooth?)
3. **Measure impact** (FPS, memory, network)
4. **Tune interpolation delay** (100ms? 150ms? 200ms?)
5. **Add prediction** (CLIENT_PREDICTION_INTERPOLATION.md Phase 2)

## Rollback Plan

If interpolation causes issues:
1. Remove interpolation buffer from game struct
2. Remove interpolate-entity-positions call
3. Entities will snap between snapshots (original behavior)
4. No other code changes needed (backward compatible)
