-- trade_complete.lua - Atomic trade execution between two players
--
-- This script atomically swaps items between two players' inventories.
-- If any validation fails, no changes are made (atomic rollback).
--
-- KEYS:
--   KEYS[1] = player1 Redis key (e.g., "player:123")
--   KEYS[2] = player2 Redis key (e.g., "player:456")
--
-- ARGV:
--   ARGV[1] = serialized player1 data (with updated inventory after trade)
--   ARGV[2] = serialized player2 data (with updated inventory after trade)
--
-- Returns:
--   "OK" on success
--   Error string on failure
--
-- The trade validation (item ownership, quantities, etc.) is done in Lisp
-- before calling this script. This script just performs the atomic swap.

local key1 = KEYS[1]
local key2 = KEYS[2]
local data1 = ARGV[1]
local data2 = ARGV[2]

-- Validate inputs
if not key1 or not key2 then
    return redis.error_reply("TRADE_ERROR: Missing player keys")
end

if not data1 or not data2 then
    return redis.error_reply("TRADE_ERROR: Missing player data")
end

-- Check both players exist (optional safety check)
local exists1 = redis.call('EXISTS', key1)
local exists2 = redis.call('EXISTS', key2)

if exists1 == 0 then
    return redis.error_reply("TRADE_ERROR: Player 1 not found")
end

if exists2 == 0 then
    return redis.error_reply("TRADE_ERROR: Player 2 not found")
end

-- Atomically update both players
-- If either SET fails, Redis will roll back the script
redis.call('SET', key1, data1)
redis.call('SET', key2, data2)

return "OK"
