Trade System Completion Plan
============================

Context
-------
Trade is partially implemented. Current UX gaps:
- Right-clicking a player does not offer a Trade option.
- Accept flow is unclear (no obvious way to accept/invite back).
- No trade dialog UI like Runescape Classic (two offer boxes, accept states).

Goal
----
Deliver a complete, intuitive trade flow:
- Right-click player -> "Trade" option
- Target player accepts by right-clicking back and selecting "Trade"
- Two-party trade window opens
- Both players offer items/stack amounts
- Both must accept; on both accept, swap atomically

Design: User Flow
-----------------
1) Initiator right-clicks target player -> Context menu includes "Trade".
2) Initiator selects Trade -> sends trade request intent to server.
3) Target receives a notification (HUD) and can:
   - Right-click initiator -> click "Trade" to accept
   - (Optional) Add explicit "Accept Trade" HUD button later
4) On server accept, open trade dialog for both players.
5) Players drag items/enter stack counts into their offer boxes.
6) Each player clicks Accept.
7) If either changes offer, both Accept states reset.
8) When both Accept = true, server validates and swaps items atomically.
9) Trade dialog closes; HUD message confirms success/failure.

Server Requirements
-------------------
- Authoritative trade session object keyed by player IDs.
- Validate:
  - Players are in same zone
  - Players are within trade distance
  - Both players not already trading
  - Items still exist in inventory (no race)
- Accept state resets on any offer change.
- Atomic swap of items and counts (no partial state).
- Reject and notify on mismatch/invalid offer.

Client Requirements
-------------------
- Context menu entry for :player target "Trade".
- Trade dialog UI:
  - Two offer boxes (mine / theirs)
  - Offer slots + stack amount inputs
  - Accept button with status (Waiting/Accepted)
  - Cancel button
- Client sends intents for:
  - trade request
  - offer add/remove/update
  - accept/unaccept
  - cancel
- Client UI updates from server trade snapshots (authoritative).

Protocol / Data
---------------
- Add/confirm current trade payload in snapshots or as a separate
  private trade-state message.
- Each trade state includes:
  - partner ID
  - my offers (slot index + count)
  - their offers (slot index + count)
  - accept status for both

Implementation Steps
--------------------
1) Context menu hook
   - Add "Trade" option when right-clicking a player.
   - Wire to intent: request-trade-target-id.
2) Server: trade request
   - Validate and create trade session.
   - Notify both players (HUD event + trade-state).
3) Client: accept flow
   - If player right-clicks initiator and selects Trade, send accept intent.
   - Server transitions session to "active" and opens UI for both.
4) Trade dialog UI
   - Build panel layout + offer boxes.
   - Drag/drop inventory items into offer; prompt for count on stacks.
5) Server: offer updates
   - Validate offers against inventory each update.
   - Reset accept flags on any change.
6) Server: accept
   - When both accepted, revalidate inventories, apply atomic swap.
7) Cancellation
   - Either player cancels -> close for both.

Tests
-----
- Unit: validate-trade-request same-zone + distance
- Unit: trade offer updates reset accept flags
- Unit: trade accept swaps items atomically
- Unit: invalid offer rejected (insufficient count)
- Unit: cancel closes session cleanly

Nice-to-Have
------------
- Timeout for unanswered trade requests
- Sound/hud notification on incoming trade
- Grey out trade option if player not eligible (too far / busy)
