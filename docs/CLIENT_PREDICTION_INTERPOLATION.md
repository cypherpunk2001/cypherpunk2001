# Client Prediction & Interpolation Exploration

## Problem Statement

With UDP networking, clients experience latency between sending input and receiving server confirmation. This can make controls feel sluggish and other entities appear to teleport between positions.

**Current behavior:**
- Client sends intent → Server receives → Server simulates → Server sends snapshot → Client displays
- Round trip time (RTT): typically 20-100ms
- Without prediction: controls feel laggy by RTT/2
- Without interpolation: entities teleport between server updates

## UDP Advantages We Can Leverage

UDP is ideal for real-time games because:
1. **No head-of-line blocking**: Lost packets don't delay subsequent packets (unlike TCP)
2. **Lower latency**: No connection handshake, no retransmission delays
3. **Packet independence**: Each snapshot is self-contained
4. **Fire-and-forget**: Perfect for "latest state wins" approach

## Technique 1: Client-Side Prediction

### Concept
The client immediately applies its own input locally (optimistic), then reconciles when the server snapshot arrives.

### Benefits
- Controls feel instant (0ms perceived latency)
- Smooth local player movement
- Works even with 100ms+ latency

### Implementation Strategy

```lisp
;; Store recent inputs with sequence numbers
(defstruct prediction-state
  (inputs (make-array 32)) ; Ring buffer of recent inputs
  (input-sequence 0)       ; Monotonic counter
  (last-acked-sequence 0)  ; Last sequence server confirmed
  (predicted-x 0.0)        ; Client's predicted position
  (predicted-y 0.0))

;; When client sends input:
(defun send-predicted-intent (game intent)
  (let* ((pred (game-prediction-state game))
         (seq (prediction-state-input-sequence pred)))
    ;; Store input with sequence number
    (store-input pred seq intent)
    ;; Apply input locally (optimistic)
    (apply-local-movement game intent *sim-tick-seconds*)
    ;; Send to server with sequence
    (send-intent-message socket intent :sequence seq)
    (incf (prediction-state-input-sequence pred))))

;; When snapshot arrives:
(defun reconcile-prediction (game snapshot)
  (let* ((pred (game-prediction-state game))
         (server-seq (getf snapshot :last-input-sequence))
         (server-x (getf snapshot :player-x))
         (server-y (getf snapshot :player-y)))
    ;; Update last acknowledged input
    (setf (prediction-state-last-acked-sequence pred) server-seq)

    ;; Check prediction error
    (let ((error-x (abs (- server-x (prediction-state-predicted-x pred))))
          (error-y (abs (- server-y (prediction-state-predicted-y pred)))))
      (when (or (> error-x 5.0) (> error-y 5.0)) ; Threshold: 5 pixels
        ;; Misprediction: reset to server position
        (setf (player-x (game-player game)) server-x
              (player-y (game-player game)) server-y
              (prediction-state-predicted-x pred) server-x
              (prediction-state-predicted-y pred) server-y)

        ;; Replay inputs after server-seq
        (replay-inputs-after pred game server-seq)))))
```

### Edge Cases
- **Collision differences**: Client predicts movement into wall, server corrects
- **Ability failures**: Client predicts attack, server rejects (cooldown/range)
- **Teleports**: Server forcibly moves player (zone transition), client snaps

### When to Use
- Always for local player movement (WASD keys)
- Sometimes for instant abilities (attack button)
- Never for other players (use interpolation instead)

## Technique 2: Entity Interpolation

### Concept
Other players and NPCs are rendered slightly in the past, smoothly interpolating between received snapshots.

### Benefits
- Smooth movement for all entities
- Hides packet loss (interpolate over missing updates)
- Consistent with server authority

### Implementation Strategy

```lisp
;; Store recent snapshots for interpolation
(defstruct interpolation-buffer
  (snapshots (make-array 3)) ; Last 3 snapshots
  (snapshot-count 0)
  (interpolation-time 0.0)   ; Current render time
  (snapshot-interval 0.1))   ; Server tick rate (100ms)

;; When snapshot arrives:
(defun buffer-snapshot (interp-buffer snapshot timestamp)
  ;; Add snapshot to ring buffer
  (push-snapshot interp-buffer snapshot timestamp)
  (incf (interpolation-buffer-snapshot-count interp-buffer)))

;; Every render frame:
(defun interpolate-entities (game dt)
  (let* ((buffer (game-interpolation-buffer game))
         (render-time (interpolation-buffer-interpolation-time buffer))
         (interval (interpolation-buffer-snapshot-interval buffer)))

    ;; Advance render time (100ms behind real time)
    (incf (interpolation-buffer-interpolation-time buffer) dt)

    ;; Find two snapshots to interpolate between
    (multiple-value-bind (snapshot-a snapshot-b alpha)
        (find-interpolation-snapshots buffer render-time)

      ;; Interpolate each entity
      (loop :for entity :across (game-entities game)
            :when (not (eq entity (game-player game))) ; Don't interpolate local player
            :do (let* ((id (entity-id entity))
                       (pos-a (get-entity-pos snapshot-a id))
                       (pos-b (get-entity-pos snapshot-b id)))
                  (when (and pos-a pos-b)
                    ;; Linear interpolation
                    (setf (entity-x entity) (lerp (pos-x pos-a) (pos-x pos-b) alpha)
                          (entity-y entity) (lerp (pos-y pos-a) (pos-y pos-b) alpha))))))))

(defun lerp (a b alpha)
  ;; Linear interpolation: a + (b - a) * alpha
  (+ a (* (- b a) alpha)))
```

### Interpolation Delay
- Typical: 100-200ms behind real time (1-2 server ticks)
- Trade-off: More delay = smoother, but slightly "laggy"
- Current server tick: 100ms (*sim-tick-seconds*)
- Recommended: 150ms (1.5 ticks)

### Advanced: Extrapolation
For very high latency, extrapolate forward instead of interpolating backward.

```lisp
(defun extrapolate-entity (entity velocity dt)
  ;; Move entity forward based on last known velocity
  ;; Risk: overshoots if entity stopped/changed direction
  (incf (entity-x entity) (* (velocity-x velocity) dt))
  (incf (entity-y entity) (* (velocity-y velocity) dt)))
```

## Technique 3: Lag Compensation (Server-Side)

### Concept
When client shoots at target, server rewinds time to where target appeared on client's screen.

### Benefits
- "Hit where you aimed" even with latency
- Fair for fast-paced combat
- Standard in FPS games

### Implementation (Not Urgent)
```lisp
;; Store entity positions for last 1 second
(defstruct lag-compensation-history
  (snapshots (make-ring-buffer 10))) ; 10 snapshots @ 100ms = 1 second

;; When client attacks:
(defun server-hit-detection (attacker target-id client-timestamp)
  (let* ((client-rtt (- (get-internal-real-time) client-timestamp))
         (rewind-time (* client-rtt 0.5)) ; Half RTT
         (historical-snapshot (get-snapshot-at-time rewind-time)))

    ;; Check hit against target's position at client's perceived time
    (hit-test attacker target-id historical-snapshot)))
```

**Warning**: Adds complexity and can feel unfair to the target ("I was behind cover!").

## Technique 4: Dead Reckoning

### Concept
Predict entity movement based on last known velocity when no update arrives.

### Benefits
- Smooth movement during packet loss
- Reduces bandwidth (send updates less frequently)
- Graceful degradation

### Implementation
```lisp
(defstruct dead-reckoning-state
  (last-update-time 0.0)
  (last-position (make-vec2 0.0 0.0))
  (last-velocity (make-vec2 0.0 0.0))
  (max-extrapolation-time 0.5)) ; 500ms max

(defun update-with-dead-reckoning (entity dt time-since-update)
  (let ((state (entity-dead-reckoning-state entity)))
    (if (< time-since-update (dead-reckoning-state-max-extrapolation-time state))
        ;; Extrapolate forward
        (progn
          (incf (entity-x entity) (* (vec2-x (dead-reckoning-state-last-velocity state)) dt))
          (incf (entity-y entity) (* (vec2-y (dead-reckoning-state-last-velocity state)) dt)))
        ;; Too old: stop extrapolating
        (setf (vec2-x (dead-reckoning-state-last-velocity state)) 0.0
              (vec2-y (dead-reckoning-state-last-velocity state)) 0.0))))
```

## Current Architecture Assessment

### What We Have
1. **UDP transport**: Good foundation for prediction/interpolation
2. **Snapshot-based**: Each message is self-contained (good)
3. **Fixed timestep**: Deterministic simulation (good)
4. **Intent-based**: Client sends desires, server validates (good)

### What We Need
1. **Sequence numbers**: Track which inputs server has processed
2. **Snapshot buffering**: Store recent snapshots for interpolation
3. **Prediction state**: Track local player's optimistic position
4. **Reconciliation**: Detect mispredictions and correct

### Changes Required

#### 1. Add sequence numbers to messages
```lisp
;; Client → Server
(:type :intent :sequence 42 :payload <intent>)

;; Server → Client
(:type :snapshot :last-sequence 41 :state <state> :events <events>)
```

#### 2. Add prediction state to game
```lisp
(defstruct game
  ...
  (prediction-state nil)      ; Client-side prediction
  (interpolation-buffer nil)  ; Entity interpolation
  (client-time 0.0))          ; Client's local time
```

#### 3. Modify run-client to use prediction
```lisp
(defun run-client (...)
  ...
  (update-client-input game dt)
  (apply-client-prediction game dt) ; NEW: Predict local movement
  (send-intent-with-sequence socket ...) ; NEW: Add sequence
  (receive-and-interpolate-snapshots socket ...) ; NEW: Interpolate
  (draw-game game))
```

## Recommendations

### Phase 1: Basic Interpolation (RECOMMENDED)
**Priority**: High
**Complexity**: Low
**Impact**: Smooth entity movement

Implement entity interpolation for other players/NPCs:
1. Buffer last 2-3 snapshots
2. Render entities 100-150ms in the past
3. Linear interpolation between snapshots

Benefits:
- Smooth movement for all entities
- No gameplay changes
- Low risk

### Phase 2: Client Prediction (OPTIONAL)
**Priority**: Medium
**Complexity**: Medium
**Impact**: Responsive local player controls

Implement prediction for local player only:
1. Add sequence numbers to intents
2. Store recent inputs on client
3. Apply local movement immediately
4. Reconcile with server snapshots

Benefits:
- Instant feedback for WASD movement
- Works well with UDP
- Standard practice in networked games

Risks:
- Mispredictions can cause rubber-banding
- More complex client code
- Need to replay inputs after correction

### Phase 3: Advanced Techniques (FUTURE)
**Priority**: Low
**Complexity**: High
**Impact**: Marginal improvement

Only implement if profiling shows need:
- Lag compensation for hit detection
- Dead reckoning for packet loss
- Extrapolation for high latency

## Testing Approach

### Simulate Network Conditions
```bash
# Add artificial latency (100ms)
tc qdisc add dev lo root netem delay 100ms

# Add packet loss (5%)
tc qdisc add dev lo root netem loss 5%

# Remove restrictions
tc qdisc del dev lo root
```

### Metrics to Measure
1. **Prediction accuracy**: How often client position matches server
2. **Rubber-banding frequency**: How often corrections are visible
3. **Interpolation smoothness**: Visual quality of entity movement
4. **Input lag**: Time from keypress to visual feedback

### Success Criteria
- Local player movement feels instant (< 50ms perceived latency)
- Other entities move smoothly (no visible teleporting)
- Corrections are rare and subtle
- Works well with 100ms RTT

## UDP-Specific Considerations

### Packet Loss
**UDP loses packets** - our techniques handle this:
- Interpolation: Smooth over missing updates (extrapolate briefly)
- Prediction: Client doesn't need every snapshot to predict
- Snapshots: Each is self-contained, no dependency on previous

### Out-of-Order Delivery
**UDP can deliver packets out of order** - handle with:
- Sequence numbers: Ignore old snapshots
- Timestamps: Sort snapshots before interpolating
- Latest-wins: Only apply snapshot if newer than last

### No Congestion Control
**UDP doesn't throttle** - we must:
- Limit snapshot size: Don't send entire world
- Rate limit: Don't exceed client's ability to process
- Drop old data: Don't buffer infinitely

## Conclusion

### Immediate Action: Entity Interpolation
Implement basic interpolation for smooth NPC/player movement:
1. Buffer 2-3 snapshots per entity
2. Render 100ms in the past
3. Lerp between snapshots
4. **Low risk, high reward**

### Next Step: Client Prediction (If Needed)
After testing interpolation, add prediction if controls still feel laggy:
1. Add sequence numbers to protocol
2. Predict local player only
3. Reconcile with server
4. **Medium risk, medium reward**

### Skip for Now: Advanced Techniques
Lag compensation and extrapolation are complex and have limited benefit for our game type (RPG, not twitch shooter).

**Remember**: These are optimizations. Profile first, optimize second. UDP already gives us low latency - prediction/interpolation are polish, not requirements.
