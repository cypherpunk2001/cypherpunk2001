PLAN: Security Improvement Project
=================================

This unified plan combines the two security initiatives below into a single, ordered project. All original content is preserved verbatim, just grouped into one file for coordinated implementation.

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

Session MAC + Replay Protection Plan
====================================

Goal
----
Add lightweight anti‑3rd‑party friction by requiring:
1) A per‑session secret (MAC/HMAC) on client→server messages
2) Monotonic sequencing + replay detection on the server

Scope
-----
- Applies to all client→server messages after authentication:
  :intent, :logout, :trade, etc.
- Does NOT change server→client snapshots (optional future extension).
- Intended as protocol hardening, not full anti‑cheat.

Design Overview
---------------
1) Server generates a random session key on successful auth.
2) Server returns the session key in :auth-ok (or a follow‑up :session-key).
3) Client includes:
   - sequence number (monotonic, per session)
   - MAC computed over message payload + sequence
4) Server validates:
   - sequence > last-seen (or within a small window)
   - MAC matches
   - rejects replay or tampering

Message Format (Client → Server)
--------------------------------
Add the following fields to all client→server messages after auth:
- :seq           (integer)
- :mac           (hex/base64 of HMAC)
- :payload       (existing payload, unchanged)

MAC Input
---------
MAC = HMAC_SHA256(session_key, concat(type, seq, payload))
- Ensure deterministic serialization (stable plist order or stable binary)
- For binary messages, MAC over raw bytes + seq

Replay Protection
-----------------
Server tracks per client:
- last-seq (highest accepted)
- optional sliding window (e.g., accept last‑seq+1 only for simplicity)

Rules:
- If seq <= last-seq → reject (replay)
- If seq jumps too far → reject or resync (configurable)
- Update last‑seq only on valid MAC

Implementation Steps
--------------------
1) Data model
   - Add session-key + last-seq to net-client struct (src/net-protocol.lisp)
   - Store server-side per client after auth

2) Key generation + delivery
   - On auth success, generate random 32‑byte key
   - Send to client in :auth-ok (or :session-key)
   - Store in client state

3) Client MAC
   - Include :seq + :mac for all messages after auth
   - Maintain local seq counter
   - Do NOT MAC auth requests (no key yet)

4) Server verification
   - On receipt of any post-auth message:
     - extract seq + mac
     - recompute HMAC
     - compare + check seq monotonicity
   - Reject invalid and log (verbose)

5) Failure handling
   - On MAC or replay failure: drop message, optionally warn client
   - Consider de-authing on repeated failures

6) Tests
   - Valid message passes
   - MAC mismatch rejected
   - Replay (same seq) rejected
   - Out‑of‑order seq rejected

Security Notes
--------------
- This blocks casual packet injection and replay.
- It does NOT prevent a reverse‑engineered client from implementing the MAC.
- Still valuable as friction + integrity check.

Files Likely Touched
--------------------
- src/net-protocol.lisp (net-client struct, message parsing)
- src/net-client.lisp (seq counter + mac on send)
- src/net-server.lisp (verify MAC + seq on receive)
- src/net-auth.lisp (deliver session key on auth ok)

Open Questions
--------------
- Where to store deterministic serialization for MAC input?
  Option: reuse intent->plist order or encode to string consistently.
- Should we MAC only intents, or all client->server messages?
- Should we allow a small seq window (e.g., +5) for UDP reordering?

--------------------------------------------------------------------------------

Input Validation & Injection Hardening Plan
===========================================

Goal
----
Establish a reusable, default-safe input validation layer for all untrusted
client inputs (auth, chat, intents, future features), to prevent DoS, malformed
payload crashes, and symbol interning abuse. This is a **framework-level** change
so every future message type is safe by default.

Scope
-----
- All client→server messages after decode and before dispatch.
- Auth payload parsing and chat payload parsing.
- Any `read-from-string` usage on data influenced by clients (including Redis
  values if sourced from clients).

Decisions (No Optionality)
--------------------------
1) **Centralized validation is mandatory**: All inbound messages must pass a
   single validation/normalization pipeline before any gameplay handler runs.
2) **Keyword-only + whitelist**: All message keys must be keywords and must be
   in a per-message schema allowlist. Unknown keys are rejected.
3) **Payload shape validation**: `:payload` must be a proper plist when present.
   Non-list payloads are dropped without touching game state.
4) **Size caps enforced**: Hard size caps on message byte length and
   read-from-string inputs, even if they fit within UDP buffers.

Architecture (Reusable Patterns)
--------------------------------
Create a small validation module with explicit schemas and helpers:

- `define-message-schema` macro (or simple defun registry):
  - Registers allowed keys, required keys, and type/shape validators.
  - Example schema: `:intent` allows `:move-dx` (single-float), `:move-dy` (single-float),
    `:attack` (boolean), `:payload` (plist) etc.
- `validate-message` function:
  - Accepts decoded plist, returns `(values ok? sanitized-plist)`.
  - Drops unknown keys, validates types/ranges, enforces max size.
  - Ensures `:type` is keyword and known.
- `valid-plist-p` helper:
  - Ensures list length is even and key/value pairs are keywords.

This keeps future input features safe by default: new message types must register
schemas or they are rejected.

Implementation Steps
--------------------
1) **Add validation module**
   - New file: `src/net-validate.lisp`.
   - Implement:
     - `valid-plist-p` (keyword-only, even length).
     - `message-size-ok-p` (bytes or characters cap).
     - `validate-message` (schema-driven checks).
     - `*message-schemas*` registry and `define-message-schema` helper.

2) **Harden decode path**
   - In `decode-net-message` (`src/net-protocol.lisp`):
     - Bind `*read-eval* nil` (already done).
     - Bind `*package*` to a dedicated decoding package (or :keyword).
     - Require decoded form to be a plist with keyword `:type`.
     - Enforce max message length before decode.

3) **Gate dispatch through validation**
   - In `src/net-server.lisp`, after decode and before dispatch:
     - `validate-message` → if not ok, drop and log (verbose only).
     - Pass sanitized plist to handlers.

4) **Payload and intent hardening**
   - In `apply-intent-plist` call site:
     - Ensure `:payload` is a proper plist.
     - Reject non-list payloads and continue safely.
   - Keep validation centralized — no ad-hoc checks in handlers.

5) **Size caps on read-from-string**
   - Wrap `storage-load` reads (and any other `read-from-string` on untrusted
     inputs) with a length check.
   - For Redis values influenced by clients, reject oversized payloads before
     parsing.

6) **Schema coverage for existing message types**
   - Add schemas for:
     - `:intent`
     - `:hello`, `:logout`, `:trade`, `:chat` (and any current client→server types)
     - Auth messages remain gated by auth handlers but still run through
       basic plist validation.

Testing
-------
- **Decode safety**: `#.(error ...)` input does not evaluate; decode returns NIL.
- **Payload robustness**: `:payload` as integer/string/vector is rejected safely.
- **Unknown keys**: rejected when not in schema.
- **Size caps**: oversized message string rejected before parsing.
- **Chat input**: still sanitized; confirm validation doesn’t break chat flow.

Files Likely Touched
--------------------
- `src/net-protocol.lisp` (decode hardening)
- `src/net-server.lisp` (dispatch gate)
- `src/net-validate.lisp` (new)
- `src/chat.lisp` (if schema needs chat field constraints)
- `src/db-storage.lisp` or `src/db-players.lisp` (storage-load size caps)

Notes
-----
- This plan is additive and compatible with the existing rate limiter and
  session MAC plans in this file.
- The goal is **default safety**: any new message type must define a schema or
  it will be rejected.

--------------------------------------------------------------------------------

ADDENDUM (Historical): security_injection_findings.md
====================================================

Security Injection Deep Dive
============================

Scope
-----
Assess whether untrusted inputs (auth login, player chat, modified client traffic)
can inject Common Lisp code on the server, toggle server variables, overwrite
functions, or inject malicious data via Redis. Provide current status, risks,
and concrete recommendations/tests.

Executive Summary
-----------------
- **Remote code execution via network inputs is not currently possible** through
  auth, chat, or normal client intents, because all network deserialization uses
  `read-from-string` with `*read-eval* nil`, and no server logic `eval`s or
  `funcall`s client-provided symbols.
- **Data injection into Redis can corrupt state but does not execute code**
  (deserialization uses `*read-eval* nil`, and player-load path validates and
  size-checks before parsing). If an attacker gains Redis write access, they
  can still alter player data/metrics/leaderboards.
- **Primary residual risks are denial-of-service (DoS) and data corruption**,
  not code execution: untrusted inputs are still read with the Lisp reader
  (symbol interning / oversized payloads) and `apply-intent-plist` does not
  guard against non-list payloads.

Direct Answers (Yes/No)
-----------------------
1) **Auth login → inject CL code server-side?**
   - **No (current code)**. Auth messages are parsed with `*read-eval* nil` and
     only read into data structures. No eval path exists.
   - Evidence: `decode-net-message` in `src/net-protocol.lisp` and
     `extract-auth-credentials` in `src/net-auth.lisp` both bind `*read-eval* nil`.

2) **Player chat → inject CL code server-side?**
   - **No (current code)**. Chat is treated as plain string data, sanitized for
     length, and interpolated with `format` as a value (not as format control).
   - Evidence: `%sanitize-chat-message` and `process-chat-intent`.

3) **Modified client → inject CL code server-side?**
   - **No (current code)**. Network plists are parsed with `*read-eval* nil`,
     and intent payload values are sanitized and type-checked before use.
   - Evidence: `decode-net-message` and `apply-intent-plist`.

Threat Model & Current Guards
-----------------------------

A) Reader-eval injection (`#.`) / arbitrary code execution
- **Threat:** Use reader-eval or reader macros to execute code during parsing.
- **Guard:** All network parsing binds `*read-eval* nil`, which disables `#.`.
- **Status:** **Guarded.** No eval or compile of client data observed.

B) Variable/function overwrite via symbol injection
- **Threat:** Send symbols that are later `set`/`symbol-value`/`funcall`ed.
- **Guard:** No code uses client-provided symbols as function/variable names.
- **Status:** **Guarded.** No dynamic dispatch on client symbols found.

C) Database injection (malicious Redis data)
- **Threat:** Write crafted data in Redis to trigger code execution or corrupt state.
- **Guard:** Deserialization uses `*read-eval* nil`. Player-load path validates
  and size-checks before parsing.
- **Status:** **Partially guarded.** Code execution blocked, but **data corruption
  still possible** if Redis is compromised. Some non-player keys are read with
  `storage-load` without size checks.

D) DoS via reader / symbol interning / malformed payloads
- **Threat:** Send huge/complex s-expressions or non-list payloads to crash or
  degrade server (memory intern, reader complexity, type errors).
- **Guard:** UDP size is capped by `*net-buffer-size*`, chat length is capped,
  and many intent fields are sanitized.
- **Status:** **Partially guarded.** `decode-net-message` accepts any list; payload
  can be non-list and trigger type errors in `apply-intent-plist`.

Key Evidence (Current Code)
---------------------------
- Network deserialization uses `*read-eval* nil`:
  - `src/net-protocol.lisp` (`decode-net-message`)
  - `src/net-auth.lisp` (`extract-auth-credentials`)
  - `src/net-snapshot.lisp` (snapshot reassembly)
- Chat input is sanitized and treated as data:
  - `%sanitize-chat-message` in `src/net-protocol.lisp`
  - `process-chat-intent` in `src/chat.lisp`
- Player data load is size-checked and validated before parse:
  - `db-load-player-validated` in `src/db-players.lisp`

Findings (Severity)
-------------------

High
- None related to **code execution**. No eval path from network/chat/auth.

Medium
1) **Intent payload type not validated (DoS risk)**
   - `apply-intent-plist` assumes payload is a plist. A malicious client can send
     `:payload` as a non-list, which can trigger a type error during `getf`,
     potentially crashing the server loop.

2) **Reader-based DoS risk via symbol interning**
   - `decode-net-message` uses the Lisp reader on untrusted text. Attackers can
     send many unique symbols (interned into `mmorpg` package) to cause memory
     growth. Size is limited by UDP buffer, but repeated packets can bloat memory.

Low
1) **Non-player Redis keys parsed without size check**
   - `storage-load` uses `read-from-string` without length check. If Redis is
     compromised, large values could induce parse/alloc spikes.

Recommendations (Directives)
-----------------------------
1) **Harden intent payload parsing (MEDIUM)**
   - In `net-server.lisp`, before calling `apply-intent-plist`, require that
     `payload` is a proper list/plist; otherwise drop it and log once.
   - This closes the easiest DoS vector from malformed payloads.

2) **Add a safe message format guard (MEDIUM)**
   - In `decode-net-message`, require a plist with a keyword `:type` and limit
     maximum message length explicitly (even if within UDP buffer).
   - Reject non-plists early to avoid reader edge cases.

3) **Reduce symbol interning exposure (MEDIUM)**
   - Option A (lightweight): bind `*package*` to a private package for decoding
     and reject symbols outside a whitelist (keywords only).
   - Option B (robust): replace reader-based parsing with a custom parser or
     binary encoding for client intents (already have binary snapshot infra).

4) **Add size checks before any `read-from-string` on untrusted data (LOW)**
   - For any use of `storage-load` on data influenced by clients, enforce a
     conservative size cap to avoid allocation spikes.

Recommended Tests
-----------------
1) **Decode safety test**: Ensure `decode-net-message` does not evaluate
   reader-eval payloads (e.g., `#.(error ...)` returns NIL safely).
2) **Intent payload robustness**: Send `:payload` as non-list types (integer,
   string, vector) and assert server does not crash and drops the payload.
3) **Symbol flooding guard**: (if whitelist added) attempt to send unknown
   symbols and verify the message is rejected.
4) **Auth payload parse safety**: Encrypted auth payload containing malformed
   s-expr should be rejected without side effects.

Status Summary
--------------
- **Code execution via auth/chat/modified client:** **No** (guarded by `*read-eval* nil` and no eval paths).
- **Server variable/function overwrite via network data:** **No** (no dynamic symbol usage).
- **Database injection:** **No code execution**, but **yes for data corruption** if Redis is compromised.
- **Primary remaining risk:** **DoS from malformed payloads / reader intern flooding**.
