# spatial.lisp

Purpose
- Provides spatial grid data structure for efficient O(1) proximity queries.
- Reduces NPC targeting and melee combat from O(n²) to O(n) complexity.
- Each zone maintains separate grids for players and NPCs.

Why we do it this way
- Array-backed grids for zones with known dimensions give O(1) access with no consing.
- Hash-based fallback remains for tests/unbounded grids.
- Cell size (128px default) balances query efficiency with cell population.
- Per-zone grids avoid cross-zone queries and simplify zone transitions.
- Entity cell tracking enables O(1) grid updates on movement.
- Scratch vectors keep neighbor queries allocation-free in hot loops.

Key structures
- `spatial-grid` - Either:
  - **Array-backed** `cells-array` for zones (2D array of lists).
  - **Hash fallback** `cells` for tests/unbounded grids (packed fixnum keys).
- `*spatial-cell-size*` - Configurable cell size (default 128px = 4 tiles).
- `*spatial-scratch-vector*`, `*spatial-scratch-vector-2*` - Reused vectors for queries.

Key functions
- `make-spatial-grid` - Create **hash-based** grid (fallback/tests).
- `make-spatial-grid-for-zone` - Create **array-backed** grid sized for zone dimensions.
- `spatial-grid-insert` / `spatial-grid-remove` / `spatial-grid-move` - Update membership.
- `spatial-grid-query-neighbors-into` - Fill a scratch vector (no allocation).
- `spatial-grid-query-rect-into` - Fill a scratch vector for viewport culling.
- `spatial-grid-query-neighbors`, `spatial-grid-query-rect` - Convenience wrappers (allocate lists).
- `position-to-cell`, `entity-cell-changed-p` - Coordinate helpers.

Entity cell tracking
Entities track their current cell for O(1) grid updates:
```lisp
;; Player and NPC structs have:
grid-cell-x  ; Current cell X (nil if not in grid)
grid-cell-y  ; Current cell Y (nil if not in grid)
```

Zone integration
Each `zone-state` has dedicated grids:
```lisp
(zone-state-player-grid zone-state)  ; For players in this zone
(zone-state-npc-grid zone-state)     ; For NPCs in this zone
```

Update hooks (CRITICAL)
Grid membership MUST be updated at these locations:

| Location | Function | Action |
|----------|----------|--------|
| movement.lisp | Position update | Call spatial-grid-move if cell changed |
| ai.lisp | NPC movement | Call spatial-grid-move if cell changed |
| movement.lisp | Zone transition | Remove from old zone, insert to new zone |
| net.lisp | Player join | Insert into zone's player grid |
| net.lisp | Player leave | Remove from zone's player grid |

Example: NPC targeting with allocation-free spatial query
```lisp
;; OLD: O(n) scan of all players
(loop :for player :across players
      :when (< (distance npc player) range)
      :collect player)

;; NEW: O(1) cell lookup + local scan (no per-query consing)
(let* ((cx (npc-grid-cell-x npc))
       (cy (npc-grid-cell-y npc))
       (scratch *spatial-scratch-vector*)
       (count (spatial-grid-query-neighbors-into player-grid cx cy scratch)))
  (loop :for i fixnum :from 0 :below count
        :for id fixnum = (aref scratch i)
        :for player = (find-player-by-id-fast game id)
        :when (and player (< (distance npc player) range))
        :collect player))
```

Performance characteristics
- Array-backed insert/remove/move: O(1) with direct array access (no consing).
- Hash fallback insert/remove/move: O(1) amortized (hash table ops).
- Move (same cell): O(1) - no-op.
- Query neighbors: O(9 + k) where k = entities in 3x3 region.
- Query radius N: O((2N+1)² + k) where k = entities in region.
- `*-into` queries reuse scratch vectors; list-returning wrappers allocate.

Design notes
- Cell size should be >= typical interaction range to minimize cross-cell queries.
- 128px default covers melee range (~64px) with margin for movement.
- IDs are stored, not entity references, for serialization safety.
- Use `spatial-grid-query-*-into` in hot loops to avoid allocation.
- List-returning query wrappers are for tooling/debug or non-hot code paths.
- Grid is cleared on zone unload; entities reset grid-cell-x/y to nil.
