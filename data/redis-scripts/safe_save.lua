-- safe_save.lua
-- Ownership-checked save operation
--
-- KEYS[1] = session ownership key (e.g., "session:owner:123")
-- KEYS[2] = player data key (e.g., "player:123")
-- ARGV[1] = expected owner (server instance ID)
-- ARGV[2] = player data (serialized)
--
-- Returns:
--   1 if save succeeded (we are the owner)
--   0 if save rejected (not the owner, stale session)
--
-- This prevents a stale server process from overwriting data
-- that another server is now managing.

local owner_key = KEYS[1]
local data_key = KEYS[2]
local expected_owner = ARGV[1]
local player_data = ARGV[2]

-- Verify ownership
local current_owner = redis.call('GET', owner_key)
if current_owner ~= expected_owner then
    return 0  -- Not the owner, reject save
end

-- Owner verified, perform save
redis.call('SET', data_key, player_data)
return 1  -- Save succeeded
