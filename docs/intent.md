# intent.lisp

Purpose
- Provide a shared action layer that input, AI, and future networking can
  all write to and systems can consume.

Why we do it this way
- It keeps decision-making separate from execution.
- It makes AI and player controls symmetric: both express intent, and the
  movement/combat systems apply it.
- It supports a future client/server split without rewriting logic.

What an intent contains
- Movement direction (dx, dy)
- Facing direction (face-dx, face-dy)
- A target point (for click-to-move)
- One-frame actions (attack, run toggle)
- Client-requested targets (for server validation):
  - `requested-attack-target-id`: NPC id the client wants to attack
  - `requested-follow-target-id`: NPC id the client wants to follow
  - `requested-pickup-target-id`, `requested-pickup-tx`, `requested-pickup-ty`: Object the client wants to pick up
  - `requested-drop-item-id`, `requested-drop-count`: Item the client wants to drop from inventory
- Chat payloads:
  - `requested-chat-message`: pending chat message for the server to broadcast
- Utility requests:
  - `requested-unstuck`: flag indicating player wants to use the unstuck teleport feature
- Client prediction:
  - `:sequence`: optional per-input sequence number for server reconciliation

Key functions
- `reset-frame-intent`: clears per-frame signals without erasing targets.
- `consume-intent-actions`: clears one-shot actions after a sim tick.
- `set-intent-move`, `set-intent-face`: standardize motion signals.
- `set-intent-target`, `clear-intent-target`: manage click-to-move.
- `request-intent-attack`, `request-intent-run-toggle`.
- `request-attack-target`, `request-follow-target`, `request-pickup-target`: client sends target requests for server validation.
- `request-drop-item`: client sends drop request for server validation.
- `request-chat-message`: client sends a chat request for server broadcast.
- `clear-requested-attack-target`, `clear-requested-follow-target`, `clear-requested-pickup-target`, `clear-requested-drop-item`: clear target requests.
- `clear-requested-chat-message`: clear pending chat requests.
- `request-unstuck`: client requests the unstuck teleport feature.
- `clear-requested-unstuck`: clear the unstuck request after processing.

Walkthrough: click-to-move
1) Mouse click is converted to a world-space target.
2) Input writes `intent-target-x/y` and sets `intent-target-active`.
3) Movement reads the target and moves toward it until close enough.
4) Movement clears the target when it arrives or gets stuck.

Example flow
```lisp
(reset-frame-intent player-intent)
(update-input-direction player player-intent mouse-clicked)
(update-input-actions player-intent (not mouse-clicked))
(update-player-position player player-intent world speed-mult dt)
(consume-intent-actions player-intent)
```

Design note
- Think of intent as a "what I want" packet. Systems decide "what happens."

Client/Server Separation (preparation for future networking)
- Input functions (client-side) only set intent fields, never authoritative player state
- Sync functions in combat.lisp (server-side) validate requested targets and set authoritative state
- The server loop copies client intent into the authoritative intent each frame (`apply-client-intent`)
- This enforces: client sends intent → server validates → server updates state → client renders result
- Examples:
  - `set-player-attack-target` sets `requested-attack-target-id` in intent
  - `sync-player-attack-target` validates the NPC exists/is alive, then sets `player-attack-target-id`
  - Invalid requests are rejected (cleared) without modifying authoritative state
