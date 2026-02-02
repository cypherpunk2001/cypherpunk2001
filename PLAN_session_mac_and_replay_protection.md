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
