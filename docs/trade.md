# Trade System

Player-to-player trading system with atomic item swaps.

## Overview

The trade system allows two players to securely exchange items. All trades are:
- **Server-authoritative**: All validation happens server-side
- **Atomic**: Either both inventories update or neither does (via Redis Lua scripts)
- **Proximity-based**: Players must be within 10 tiles and same zone
- **Time-limited**: Sessions timeout after 60 seconds of inactivity

## Trade Flow

1. **Initiate**: Player A right-clicks Player B and selects "Trade"
2. **Offer**: Both players add items from their inventory to the trade window
3. **Confirm**: Both players click "Confirm" when satisfied
4. **Execute**: Server atomically swaps items using Lua script
5. **Complete**: Trade window closes, inventories updated

## Configuration

| Parameter | Default | Description |
|-----------|---------|-------------|
| `*trade-max-distance-tiles*` | 10 | Max tile distance for trade |
| `*trade-timeout-seconds*` | 60 | Inactivity timeout |
| `*trade-max-offer-slots*` | 12 | Max slots in a single offer |

## Data Structures

### Trade Session (Ephemeral)

```lisp
(defstruct trade-session
  id              ; unique session ID
  player1-id      ; initiator
  player2-id      ; recipient
  player1-offer   ; trade-offer struct
  player2-offer   ; trade-offer struct
  created-at      ; internal-real-time
  state)          ; :pending, :both-confirmed, :executing, :completed, :cancelled
```

### Trade Offer

```lisp
(defstruct trade-offer
  slots           ; hash-table: slot-index -> count
  confirmed       ; boolean
  last-activity)  ; internal-real-time
```

## Intent Fields

Trade actions use these intent fields:

| Field | Purpose |
|-------|---------|
| `requested-trade-target-id` | Player ID to initiate trade with |
| `requested-trade-offer-slot` | Slot index to add/remove |
| `requested-trade-offer-count` | Count to offer (0 = remove) |
| `requested-trade-confirm` | Confirm current offer |
| `requested-trade-cancel` | Cancel active trade |

## Validation Rules

Before trade initiation:
- Players cannot trade with themselves
- Neither player can be in another trade
- Players must be in the same zone
- Players must be within 10 tiles of each other

Before item offer:
- Slot must be valid inventory slot
- Slot must contain items
- Count must be 1 to slot's current count

Before execution:
- Both players must have confirmed
- Both players must still be online
- Both players must have inventory space for received items

## Atomic Execution

The trade completion uses a Redis Lua script (`trade_complete.lua`) to ensure atomicity and
verify session ownership before committing:

```lua
-- Verify session ownership before committing
local actual_owner1 = redis.call('GET', owner_key1)
local actual_owner2 = redis.call('GET', owner_key2)
if actual_owner1 ~= expected_owner or actual_owner2 ~= expected_owner then
  return redis.error_reply("TRADE_ERROR: Ownership mismatch")
end
redis.call('SET', player1_key, new_player1_data)
redis.call('SET', player2_key, new_player2_data)
return "OK"
```

If the server crashes between these operations, Redis rolls back the entire script.

## Server Integration

### Startup

```lisp
;; Load trade scripts when Redis storage is connected
(when (typep *storage* 'redis-storage)
  (load-trade-scripts))
```

### Per-Tick Processing

Trade intent processing is implemented but not currently wired into the server tick loop.
To enable trading, `process-trade-intents` and `cleanup-timed-out-trades` need to be called
from the server update loop.

```lisp
;; In server tick, after player intents
(dolist (player players)
  (process-trade-intents player players))

;; Periodically (every few seconds)
(cleanup-timed-out-trades)
```

## API Functions

### Session Management

```lisp
(get-player-trade player-id)        ; Get active trade or NIL
(player-in-trade-p player-id)       ; Check if trading
(create-trade-session p1-id p2-id)  ; Create new session
(cancel-trade-session session)      ; Cancel and cleanup
```

### Intent Processing

```lisp
(process-trade-intents player players)  ; Handle all trade intents
```

### Maintenance

```lisp
(cleanup-timed-out-trades)  ; Cancel timed out sessions
(load-trade-scripts)        ; Load Lua scripts at startup
```

## Coins as Items

The trade system treats coins like any other inventory item. There is no special "gold" field - coins stack in inventory slots just like potions or other stackable items.

## Error Handling

Trade failures result in cancellation with logged reason:
- "inventory error" - Not enough space for received items
- "atomic execution failed" - Redis script failed
- "timeout" - 60 seconds of inactivity
- "player cancelled" - One player cancelled

## Security Considerations

1. **No client trust**: Server validates all operations
2. **Atomic swaps**: Lua script prevents item duplication
3. **Proximity check**: Prevents trading across zones
4. **Timeout**: Prevents abandoned trade locks
5. **Confirmation reset**: Modifying offer unconfirms both players
