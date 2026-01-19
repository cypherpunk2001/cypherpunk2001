# Inventory Drag-and-Drop Reordering

## Overview
Allow players to customize inventory slot ordering via click-drag-and-drop. When an item is dragged to another slot, the items swap positions. This arrangement persists across sessions.

## Key Insight: No Database Migration Needed
The current inventory serialization already stores slots in array order (slot 0, 1, 2, etc.). When we swap slot contents, the next save will preserve the new arrangement. No schema changes required.

Current serialization flow:
```lisp
;; serialize-inventory: slots stored in array order
(:slots ((:item-id :coins :count 50)    ; slot 0
         (:item-id :arrows :count 20)   ; slot 1
         nil                            ; slot 2 (empty)
         ...))

;; deserialize-inventory: slots restored to same positions
```

If player swaps slot 0 and slot 1, next save will store:
```lisp
(:slots ((:item-id :arrows :count 20)   ; slot 0 (was in 1)
         (:item-id :coins :count 50)    ; slot 1 (was in 0)
         nil                            ; slot 2 (empty)
         ...))
```

## Implementation Plan

### Step 1: Add Intent Fields for Inventory Swap
**File: `src/intent.lisp`**
- Add `requested-swap-slot-a` and `requested-swap-slot-b` to intent struct
- Add helper function `request-inventory-swap (intent slot-a slot-b)`
- Add helper function `clear-requested-inventory-swap (intent)`

### Step 2: Add Intent Serialization
**File: `src/net.lisp`**
- Add `:requested-swap-slot-a` and `:requested-swap-slot-b` to `intent->plist`
- Add deserialization in `apply-intent-plist`
- Add copy in `apply-client-intents` to propagate swap from client to server intent

### Step 3: Add Server-Side Swap Handler
**File: `src/progression.lisp`**
- Add `swap-inventory-slots (player slot-a slot-b)` function
  - Validate slot indices are within bounds
  - Get both slots from inventory array
  - Swap slot contents (or just swap array elements)
  - Mark player as snapshot-dirty for persistence
  - Log verbose message for debugging
- Add `process-player-inventory-swap (player intent)` function
  - Extract slot-a and slot-b from intent
  - Call swap-inventory-slots
  - Clear the swap request from intent

### Step 4: Integrate Swap Processing in Server Update
**File: `src/server.lisp`**
- In `apply-client-intent`, copy swap slot fields from client intent
- In `update-sim` or player processing, call `process-player-inventory-swap`

### Step 5: Add Client UI Drag State
**File: `src/ui.lisp`**
- Add UI state fields for drag operation:
  - `ui-drag-slot-index` - source slot being dragged (nil when not dragging)
  - `ui-drag-item-id` - item ID being dragged (for visual feedback)
  - `ui-drag-start-x/y` - where drag started
  - `ui-drag-current-x/y` - current drag position
- Add functions:
  - `ui-start-inventory-drag (ui slot-index item-id x y)`
  - `ui-update-inventory-drag (ui x y)`
  - `ui-end-inventory-drag (ui)` - returns source and dest slots

### Step 6: Handle Drag Input in Main Loop
**File: `src/main.lisp`**
- On left-click in inventory:
  - If slot has item and not currently dragging: start drag
  - Store source slot index
- While dragging (mouse held):
  - Update drag position
  - Show visual feedback (item following cursor)
- On mouse release:
  - Get destination slot from cursor position
  - If valid destination slot and different from source:
    - Send swap intent to server via `request-inventory-swap`
  - End drag state

### Step 7: Add Visual Feedback for Dragging
**File: `src/rendering.lisp`**
- In inventory rendering:
  - If dragging, dim the source slot slightly
  - Draw the dragged item at cursor position
  - Highlight destination slot on hover

### Step 8: Test and Verify Persistence
- Test swap operation works
- Logout and login - verify inventory order persists
- Test edge cases:
  - Swap with empty slot (should just move item)
  - Swap same slot (no-op)
  - Swap out of bounds (reject)
  - Multiple rapid swaps

## Files Modified Summary
| File | Changes |
|------|---------|
| `src/intent.lisp` | Add swap slot fields and helpers |
| `src/net.lisp` | Add swap intent serialization |
| `src/progression.lisp` | Add swap logic and handler |
| `src/server.lisp` | Integrate swap processing |
| `src/ui.lisp` | Add drag state management |
| `src/main.lisp` | Handle drag input events |
| `src/rendering.lisp` | Visual feedback during drag |

## Testing Checklist
- [ ] Drag item from slot A to slot B - items swap
- [ ] Drag item to empty slot - item moves, source becomes empty
- [ ] Drag to same slot - no change (no-op)
- [ ] Logout and login - order persists
- [ ] Rapid swaps work correctly
- [ ] Keyboard Escape cancels drag
- [ ] Click outside inventory cancels drag

## No Migration Required
Confirmed: The inventory structure stores slots by index. Swapping contents of slots and marking player dirty will persist the new arrangement on next save. No database schema change needed.

## Estimated Scope
- Intent/net changes: Small (~30 lines)
- Progression swap logic: Small (~40 lines)
- Server integration: Small (~10 lines)
- UI state management: Medium (~50 lines)
- Input handling: Medium (~60 lines)
- Visual feedback: Medium (~40 lines)
- Total: ~230 lines across 7 files
