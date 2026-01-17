# ECS Architecture Evaluation: Should We Do It?

## What is ECS?

**Entity Component System (ECS)** is an architectural pattern where:
- **Entities**: IDs representing game objects (just numbers)
- **Components**: Pure data (position, health, sprite)
- **Systems**: Pure functions operating on components (movement, combat, rendering)

### ECS Example
```lisp
;; Traditional OOP
(defclass player ()
  ((x :initform 0.0)
   (y :initform 0.0)
   (health :initform 100)
   (sprite :initform nil)))

(defmethod update ((p player) dt)
  (setf (slot-value p 'x) (+ (slot-value p 'x) (* velocity dt))))

;; ECS
;; Entities are just IDs
(defvar *entities* (make-hash-table)) ; ID -> component-mask

;; Components are pure data
(defvar *position-components* (make-hash-table)) ; ID -> (x y)
(defvar *health-components* (make-hash-table))   ; ID -> health
(defvar *sprite-components* (make-hash-table))   ; ID -> sprite-id

;; Systems operate on component arrays
(defun movement-system (dt)
  (loop :for (id velocity) :in (get-entities-with '(position velocity))
        :do (let ((pos (gethash id *position-components*)))
              (incf (pos-x pos) (* (velocity-x velocity) dt)))))
```

## Current Architecture: Struct-Based Lite-ECS

### What We Have Now

```lisp
;; src/types.lisp
(defstruct player
  id x y dx dy            ; Position + velocity (components)
  intent                  ; Input component
  stats inventory         ; Stat + inventory components
  attack-timer hit-timer  ; Combat state components
  anim-state facing       ; Rendering components
  ...)

;; src/main.lisp
(defun update-sim (game dt)
  (loop :for player :across (game-players game)
        :do (update-player-position player (player-intent player) world dt))
  (loop :for npc :across (game-npcs game)
        :do (update-npc-movement npc world dt)))
```

**This is already similar to ECS:**
- Entities: Players and NPCs have IDs
- Components: Stats, position, inventory are separate concerns
- Systems: `update-player-position`, `update-npc-movement` are system-like

**Difference**: Components are bundled in structs, not separated in component arrays.

## ECS Benefits (In Theory)

### 1. Cache-Friendly Memory Layout ✅
**Theory**: Storing components in arrays improves CPU cache hits.
```lisp
;; Bad for cache (current)
(player x=100 y=200 health=50 mana=30 sprite=... animation-timer=...)

;; Good for cache (ECS)
*x-components* = [100 101 102 103 ...]
*y-components* = [200 201 202 203 ...]
```

**Reality**: Only matters if you have thousands of entities and tight loops.

**Verdict**: 🟡 Marginal benefit for 500 players. Significant benefit for 10,000+ entities.

### 2. Data-Oriented Design ✅
**Theory**: Pure component data is easier to serialize, network, and version.

**Reality**: Our current structs are already data-oriented.
```lisp
;; Current (already data-oriented)
(serialize-player player) ; → plist

;; ECS (slightly easier)
(serialize-components entity-id) ; → plist of components
```

**Verdict**: 🟡 Slight benefit, but we're already doing well.

### 3. Flexible Entity Composition ✅
**Theory**: Mix and match components without inheritance hell.

```lisp
;; ECS: Easy to add new entity types
(make-entity '(position health ai-component)) ; NPC
(make-entity '(position health player-input)) ; Player
(make-entity '(position sprite))              ; Decoration
```

**Reality**: Our current approach is already flexible.
```lisp
(defstruct player id x y intent stats ...) ; Has intent
(defstruct npc id x y intent stats ...)    ; Has intent
(defstruct object id x y)                  ; No intent
```

**Verdict**: 🟡 Current approach is good enough. ECS is more flexible but we don't need it yet.

### 4. System Reusability ✅
**Theory**: Systems work on any entity with required components.

```lisp
;; ECS: One movement system for all entities
(defun movement-system (dt)
  (for-entities-with '(position velocity)
    (lambda (id)
      (update-position id dt))))
```

**Reality**: We already have reusable functions.
```lisp
(defun update-entity-position (entity intent world dt)
  ;; Works for players AND NPCs
  ...)
```

**Verdict**: 🟡 ECS makes this slightly easier, but we're already there.

### 5. Parallelization Potential ✅
**Theory**: Systems operating on disjoint component sets can run in parallel.

```lisp
;; ECS: These can run in parallel
(parallel
  (movement-system dt)      ; Touches: position, velocity
  (animation-system dt))    ; Touches: sprite, animation-timer
```

**Reality**: Requires careful component partitioning and thread safety.

**Verdict**: 🟢 Real benefit for multi-threading. But we chose horizontal scaling instead (see SERVER_PERFORMANCE.md).

## ECS Costs

### 1. Complexity ⚠️
**Cost**: More indirection, harder to debug.

```lisp
;; Current: Direct access
(player-x player) ; Easy to understand

;; ECS: Indirection
(gethash entity-id *position-components*) ; Need to know component exists
```

**Impact**: Debugging is harder. Need better tooling.

### 2. Performance Overhead (Small Entities) ⚠️
**Cost**: Hash table lookups vs. struct field access.

```lisp
;; Current: O(1) field access
(player-health player) ; Direct memory offset

;; ECS: O(1) hash table lookup
(gethash player-id *health-components*) ; Hash + lookup
```

**Impact**: Negligible for large batch operations. Measurable for single-entity queries.

### 3. Loss of Type Safety ⚠️
**Cost**: Structs give compile-time type checking.

```lisp
;; Current: Type-safe
(player-x player) ; Compile error if player-x doesn't exist

;; ECS: Runtime errors
(gethash entity-id *position-components*) ; NIL if component missing
```

**Impact**: More runtime errors, less compiler help.

### 4. Refactoring Cost ⚠️⚠️
**Cost**: Converting current codebase to ECS is HUGE effort.

- Rewrite all entity structs as component arrays
- Rewrite all update functions as systems
- Change all code that touches entities
- Re-serialize save files

**Impact**: Weeks of work, high risk of bugs.

## Decision Matrix

| Criterion | Current Struct-Based | ECS | Winner |
|-----------|---------------------|-----|--------|
| Cache efficiency | Fair | Excellent | ECS |
| Data-oriented | Good | Excellent | ECS (slight) |
| Flexibility | Good | Excellent | ECS (slight) |
| Type safety | Excellent | Poor | Struct |
| Debuggability | Excellent | Fair | Struct |
| Learning curve | Easy | Hard | Struct |
| Refactoring cost | N/A | Very High | Struct |
| Current codebase fit | Excellent | Poor | Struct |
| Performance (500 entities) | Excellent | Excellent | Tie |
| Performance (10k entities) | Good | Excellent | ECS |

## Are We Already Using ECS?

**Sort of!**

Our current architecture has ECS-like properties:
1. ✅ Entities have IDs (`player-id`, `npc-id`)
2. ✅ Data separated from logic (structs hold data, functions do work)
3. ✅ Systems operate on entity arrays (`update-sim` loops over entities)
4. ✅ Components are conceptually separated (intent, stats, inventory)

**What we're missing:**
1. ❌ Component arrays (we use structs instead)
2. ❌ Sparse entity IDs (we use dense arrays)
3. ❌ Dynamic component composition (structs are fixed)

**Conclusion**: We have a **"Lite-ECS"** or **"Struct-of-Arrays-like"** architecture.

## Recommendation: NO (for now)

### Reasons to NOT adopt full ECS:

1. **Current architecture works well**
   - Clean, understandable, maintainable
   - Performance is good for current scale (500 entities/zone)
   - Type-safe and easy to debug

2. **Refactoring cost is too high**
   - Weeks of risky work
   - High chance of introducing bugs
   - No immediate benefit

3. **We're already ECS-adjacent**
   - Our structs are component-like
   - Our update functions are system-like
   - Easy to migrate later if needed

4. **Horizontal scaling is better than ECS**
   - For 10k users: run 20 servers (done)
   - ECS helps with single-server scale
   - We don't need single-server scale yet

### When to CONSIDER ECS:

1. **If we hit performance walls**
   - 1000+ entities per zone with slowdowns
   - Profiling shows struct access is bottleneck
   - Cache misses are measurable problem

2. **If we need extreme flexibility**
   - Player-created content (modding)
   - Dynamic entity types at runtime
   - Procedural entity generation

3. **If we're rewriting anyway**
   - Major refactor planned
   - New language/engine
   - Fresh start on architecture

4. **If we're building from scratch**
   - Starting new project: consider ECS early
   - Retrofitting existing code: probably not worth it

## Alternative: Incremental Improvements

Instead of full ECS, consider these targeted improvements:

### 1. Separate Hot/Cold Data ✅
```lisp
;; Current: Everything in one struct
(defstruct player
  id x y dx dy intent stats inventory equipment
  anim-state facing frame-index frame-timer
  ...)

;; Better: Hot data separate from cold data
(defstruct player-hot
  x y dx dy intent) ; Updated every frame

(defstruct player-cold
  inventory equipment stats) ; Updated rarely

(defstruct player
  id
  hot-data  ; Pointer to hot data
  cold-data) ; Pointer to cold data
```

**Benefit**: Better cache usage without full ECS refactor.

### 2. Struct-of-Arrays for Specific Systems ✅
```lisp
;; Current: Array of structs
(game-npcs game) ; → #(npc npc npc ...)

;; Better: Struct of arrays (for specific hot paths)
(defstruct npc-batch
  (ids (make-array 100))
  (x-positions (make-array 100))
  (y-positions (make-array 100))
  (velocities (make-array 100)))

;; Use for batch operations only
(defun batch-update-npc-positions (batch dt)
  (loop :for i :below (length (npc-batch-ids batch))
        :do (incf (aref (npc-batch-x-positions batch) i)
                  (* (aref (npc-batch-velocities batch) i) dt))))
```

**Benefit**: Cache-friendly for hot paths, keep structs elsewhere.

### 3. Component Traits (Mixins) ✅
```lisp
;; Add optional components via flags
(defstruct entity
  id x y
  (has-inventory nil)
  (has-ai nil)
  (inventory nil)
  (ai-state nil))

;; Systems check flags
(defun inventory-system (entities)
  (loop :for entity :across entities
        :when (entity-has-inventory entity)
        :do (update-inventory (entity-inventory entity))))
```

**Benefit**: Some ECS flexibility without full rewrite.

## Comparison to Other Engines

| Engine | Architecture | Verdict |
|--------|-------------|---------|
| Unity (old) | OOP + Components | Flexible but slow |
| Unity (DOTS) | Full ECS | Fast but complex |
| Unreal | OOP + Actor model | Easy but not cache-friendly |
| Bevy (Rust) | Full ECS | Fast and type-safe (Rust) |
| **Our engine** | **Struct-based Lite-ECS** | **Simple and fast enough** |

## Conclusion

### Should we do full ECS? **NO**

**Reasons:**
1. ✅ Current architecture is good
2. ✅ Performance is acceptable (smooth with hundreds of entities)
3. ⚠️ Refactoring cost is very high
4. ⚠️ Benefits are marginal at current scale
5. ✅ Horizontal scaling solves 10k user problem

### What should we do instead? **Iterate on current design**

**Actions:**
1. ✅ Keep current struct-based architecture
2. ✅ Consider hot/cold data separation if profiling shows need
3. ✅ Consider struct-of-arrays for specific bottlenecks
4. ✅ Revisit ECS if we hit 1000+ entities/zone with performance issues

### Are we doing ECS-like things? **YES**

**We already have:**
- Entity IDs
- Data-oriented structs
- System-like update functions
- Component-like separation of concerns

**We're doing a "Lite-ECS" which is appropriate for:**
- Medium-scale games (hundreds of entities)
- Rapid development
- Maintainability over micro-optimization
- Common Lisp strengths (structs, arrays, closures)

## Final Verdict

**Grade: Current Architecture ⭐⭐⭐⭐☆ (4/5)**

**Grade: Full ECS for Our Project ⭐⭐☆☆☆ (2/5)**

**Recommendation: Keep current architecture, consider ECS-lite improvements if needed.**

The juice is not worth the squeeze. Our struct-based approach is working well and fits the project's scale and goals.
