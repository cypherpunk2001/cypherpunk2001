-- session_claim.lua
-- Atomic session ownership claim with TTL
--
-- KEYS[1] = session ownership key (e.g., "session:owner:123")
-- ARGV[1] = server instance ID (claimant)
-- ARGV[2] = TTL in seconds
--
-- Returns:
--   1 if claimed successfully (key didn't exist or was expired)
--   0 if already claimed by another server (double-login rejection)
--
-- Note: This uses SET NX EX for atomic claim-with-TTL.
-- If the key exists (session active elsewhere), returns 0.
-- If the key doesn't exist or expired, claims it and returns 1.

local key = KEYS[1]
local server_id = ARGV[1]
local ttl = tonumber(ARGV[2])

-- Try to SET if Not eXists, with EXpiration
local result = redis.call('SET', key, server_id, 'NX', 'EX', ttl)

if result then
    return 1  -- Successfully claimed
else
    -- Check if we already own it (reconnect case)
    local current_owner = redis.call('GET', key)
    if current_owner == server_id then
        -- We already own it, refresh TTL
        redis.call('EXPIRE', key, ttl)
        return 1
    end
    return 0  -- Claimed by another server
end
