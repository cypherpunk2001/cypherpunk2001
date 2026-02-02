Global Client Rate Limiting Plan
================================

Goal
----
Add a default, server-side rate limiter that applies to *all* client requests.
Only explicitly whitelisted message types will bypass it.
This reduces abuse risk without needing per-event reasoning upfront.

Design Principles
-----------------
- Default-deny: rate limit everything unless explicitly exempt.
- Low complexity: one limiter per client (host+port), with a small token bucket.
- No heavy anti-cheat; just guardrails against spam/abuse.

Scope (Rate Limited by Default)
-------------------------------
- All client→server messages *after* connection, including:
  - :intent
  - :trade / :logout / :hello
  - any future custom message types

Whitelist (Not Rate Limited)
----------------------------
- Auth handshake messages may use existing auth rate limiting instead:
  - :login
  - :register
  (these already go through auth rate checks and queueing)

Recommended Limits (Initial)
-----------------------------
- Token bucket per client:
  - capacity: 30 tokens
  - refill: 15 tokens/second
  - cost per message: 1 token
- Hard drop when bucket empty (no response to avoid amplification)
- Optional: log a throttling warning only when verbose flag enabled.

Implementation Steps
--------------------
1) Add limiter state to net-client struct
   - token count
   - last refill timestamp
2) Implement a small token bucket helper
   - refill on each message
   - allow/deny check
3) Apply limiter early in net-server loop
   - after message decode, before dispatch
   - if limited: drop message and increment a counter
4) Provide config/env overrides
   - MMORPG_CLIENT_RATE_LIMIT_ENABLED=1
   - MMORPG_CLIENT_RATE_LIMIT_CAPACITY
   - MMORPG_CLIENT_RATE_LIMIT_RPS

Testing
-------
- Unit: bucket refills over time
- Unit: burst allowed up to capacity, then drops
- Integration: spam :intent; verify drops after threshold
- Confirm auth flows still function (login/register unaffected)

Notes
-----
- This does not replace validation; server must still validate intents.
- If UDP reordering becomes an issue, consider separate buckets per message type.
