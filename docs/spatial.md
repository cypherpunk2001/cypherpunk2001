# spatial.lisp

Purpose
- Provides spatial grid data structure for efficient O(1) proximity queries.
- Reduces NPC targeting and melee combat from O(n²) to O(n) complexity.
- Each zone maintains separate grids for players and NPCs.

Why we do it this way
- Hash-based grid allows O(1) cell lookup and constant-time neighbor queries.
- Cell size (128px default) balances query efficiency with cell population.
- Per-zone grids avoid cross-zone queries and simplify zone transitions.
- Entity cell tracking enables O(1) grid updates on movement.

Key structures
- `spatial-grid` - Hash table mapping (cell-x . cell-y) to list of entity IDs.
- `*spatial-cell-size*` - Configurable cell size (default 128px = 4 tiles).

Key functions
- `make-spatial-grid` - Create empty grid with given cell size.
- `spatial-grid-insert` - Add entity ID at world position.
- `spatial-grid-remove` - Remove entity ID from specific cell.
- `spatial-grid-move` - Update entity position (remove old, insert new).
- `spatial-grid-query-neighbors` - Get IDs in cell and 8 neighbors (3x3).
- `spatial-grid-query-radius` - Get IDs within N cells of center.
- `spatial-grid-query-position` - Query neighbors at world position.
- `position-to-cell` - Convert world coords to cell coords.
- `entity-cell-changed-p` - Check if position is in different cell.

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

Example: NPC targeting with spatial query
```lisp
;; OLD: O(n) scan of all players
(loop :for player :across players
      :when (< (distance npc player) range)
      :collect player)

;; NEW: O(1) cell lookup + local scan
(let* ((cx (npc-grid-cell-x npc))
       (cy (npc-grid-cell-y npc))
       (nearby-ids (spatial-grid-query-neighbors player-grid cx cy)))
  (loop :for id :in nearby-ids
        :for player = (find-player-by-id-fast game id)
        :when (and player (< (distance npc player) range))
        :collect player))
```

Performance characteristics
- Insert/Remove: O(1) amortized (hash table operations)
- Move (same cell): O(1) - no-op
- Move (different cell): O(1) amortized
- Query neighbors: O(9 + k) where k = entities in 3x3 region
- Query radius N: O((2N+1)² + k) where k = entities in region

Design notes
- Cell size should be >= typical interaction range to minimize cross-cell queries.
- 128px default covers melee range (~64px) with margin for movement.
- IDs are stored, not entity references, for serialization safety.
- Query functions return fresh lists to avoid aliasing issues.
- Grid is cleared on zone unload; entities reset grid-cell-x/y to nil.
