# AGENTS.md

Minimal Common Lisp + raylib project slowly iterating in to a single player rpg.

## Project layout

```
mmorpg/
  AGENTS.md
  mmorpg.asd
  assets/*
    ...
  src/
    package.lisp
    main.lisp
    ...
  docs/
    raylib_cheatsheet.md
    claw-raylib-readme.org
```

---

## Current Task

# AGENTS.md

This document defines how **agents** (human players, NPCs, bots) communicate in-game.
We start with a **chat system** that is fully decoupled from rendering and gameplay authority.
Once chat is working, we will add the first NPC agent (LLM-backed via Ollama) as a chat participant.

---

## Part 1 — Chat System (Foundation)

### Goal
Implement a chat system where:
- Press **T** to enter typing mode
- Typing writes to an input buffer
- **Enter** sends the message to the public arena chat
- **Esc** cancels typing
- Chat is **decoupled from rendering**
- Chat output can later be shown as:
  - a chat log UI panel
  - floating text above character heads (optional later)

### Key rule
Chat is a **data + events system**, not a render system.

Rendering will consume chat state, but chat will not depend on rendering.

---

## Chat Architecture

### Components

#### 1) ChatInput (state machine)
Tracks whether the player is typing and what’s in the current buffer.

- `active_p` — whether typing mode is enabled
- `buffer` — current input line
- `max_len` — clamp input length

Typing mode should suppress movement controls so WASD doesn’t move the player while typing.

#### 2) ChatLog (history)
Stores chat messages for:
- chat window UI
- debugging
- later replay

The log is game-state data, not UI.

#### 3) ChatBus (publish/subscribe seam)
A simple interface for emitting chat messages.

Today:
- `publish(msg)` just queues locally

Later:
- `publish(msg)` sends to server
- incoming messages are delivered from server into the client log

This is the seam that allows client/server migration without rewriting the game.

---

## Chat Message Contract

A message is a plain data object with these minimum fields:

- `from_id` — player id / agent id
- `channel` — `:public` for now (future: :local, :guild, :whisper)
- `text` — the message content
- `time_ms` — timestamp or tick when sent

Optional future fields (not required for part 1):
- `pos` / `zone` (for proximity and overhead bubbles)
- `to_id` (directed messages, whispers, npc targeting)
- `conversation_id` (for NPC memory)

---

## Input Handling Rules (Raylib / game loop)

### Enter typing mode
- When not typing:
  - If **T** is pressed: activate chat input (clear buffer)

### Capture typed characters
- When typing:
  - Characters come from `GetCharPressed()` (loop until 0)
  - Append printable characters to buffer
  - Clamp length to `max_len`

### Editing
- When typing:
  - Backspace deletes last character

### Submit
- When typing:
  - Enter sends message:
    - trim whitespace
    - ignore empty messages
    - create `chat-message`
    - push to `chat-log`
    - publish to `chat-bus`
    - exit typing mode

### Cancel
- When typing:
  - Esc cancels:
    - clear buffer
    - exit typing mode

---

## Non-Goals for Part 1
- Networking
- Message moderation/filters
- Overhead bubbles / proximity logic
- Chat commands (`/w`, `/me`, etc.)
- NPC logic

Those come after the foundation is stable.

---

## How to keep it smooth as we scale

- Avoid per‑frame consing in hot loops: reuse rectangles, vectors, strings, and animation state.
- Cull off‑screen tiles/sprites; draw only what’s visible.
- Chunk the map (e.g., 32×32 tiles) and cache static chunks in a render texture.
- Keep entity data in arrays/structs, not lists; use object pools.
- Separate update/draw; keep animation state lightweight.
