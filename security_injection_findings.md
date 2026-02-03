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

