# chat.lisp

Purpose
- Process chat requests from client intent and broadcast them as HUD log events.

Why we do it this way
- Chat is still authoritative: the client only requests a message, and the server
  decides whether to emit it. This matches the client/server split pattern.

What it does
- Trims incoming chat text.
- Emits a HUD message tagged with the current zone label and player name.
- Clears the pending chat request after processing.

Key functions
- `process-chat-intent`, `trim-chat-message`.

Flow
1) Client opens chat with `T` and submits with Enter.
2) Input writes `requested-chat-message` into the client intent.
3) The server loop copies intent and `process-chat-intent` emits a HUD message.
4) The HUD log renders the chat line and fades it out over time.
