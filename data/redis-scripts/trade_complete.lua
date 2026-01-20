-- trade_complete.lua - Atomic trade execution between two players
--
-- This script atomically swaps items between two players' inventories.
-- If any validation fails, no changes are made (atomic rollback).
--
-- Phase 3 (Trade Safety): Verifies session ownership before committing.
-- This prevents stale servers from overwriting data after losing ownership.
--
-- KEYS:
--   KEYS[1] = player1 Redis key (e.g., "player:123")
--   KEYS[2] = player2 Redis key (e.g., "player:456")
--   KEYS[3] = player1 ownership key (e.g., "session:owner:123")
--   KEYS[4] = player2 ownership key (e.g., "session:owner:456")
--
-- ARGV:
--   ARGV[1] = serialized player1 data (with updated inventory after trade)
--   ARGV[2] = serialized player2 data (with updated inventory after trade)
--   ARGV[3] = expected server instance ID (must own both sessions)
--
-- Returns:
--   "OK" on success
--   Error string on failure (ownership mismatch aborts trade)
--
-- The trade validation (item ownership, quantities, etc.) is done in Lisp
-- before calling this script. This script just performs the atomic swap
-- after verifying the server still owns both player sessions.

local key1 = KEYS[1]
local key2 = KEYS[2]
local owner_key1 = KEYS[3]
local owner_key2 = KEYS[4]
local data1 = ARGV[1]
local data2 = ARGV[2]
local expected_owner = ARGV[3]

-- Validate inputs
if not key1 or not key2 then
    return redis.error_reply("TRADE_ERROR: Missing player keys")
end

if not owner_key1 or not owner_key2 then
    return redis.error_reply("TRADE_ERROR: Missing ownership keys")
end

if not data1 or not data2 then
    return redis.error_reply("TRADE_ERROR: Missing player data")
end

if not expected_owner then
    return redis.error_reply("TRADE_ERROR: Missing expected owner")
end

-- Phase 3: Verify session ownership before committing trade
-- This prevents a stale server from overwriting data after losing ownership
local actual_owner1 = redis.call('GET', owner_key1)
local actual_owner2 = redis.call('GET', owner_key2)

if actual_owner1 ~= expected_owner then
    return redis.error_reply("TRADE_ERROR: Ownership mismatch for player 1 (expected " .. expected_owner .. ", got " .. tostring(actual_owner1) .. ")")
end

if actual_owner2 ~= expected_owner then
    return redis.error_reply("TRADE_ERROR: Ownership mismatch for player 2 (expected " .. expected_owner .. ", got " .. tostring(actual_owner2) .. ")")
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
