Client Version Enforcement Plan
================================

Goal
----
Disallow login/registration when client version does not match server version.
Exact version match required (no compatibility range).

Design Summary
--------------
- Add a single authoritative version string/number on the server.
- Client sends its version with auth requests (login/register).
- Server rejects mismatched versions before auth work begins.
- Client surfaces a clear error and stops auto-retry on mismatch.

Proposed Version Source
-----------------------
- Define *client-build-version* and *server-build-version* in config or net-protocol.
- Default to a single *game-version* value used by both client and server builds.
- Allow override via env var (e.g., MMORPG_VERSION) for rapid hotfixes.

Auth Flow Changes
-----------------
1) Client
   - Include :client-version in auth messages (login/register).
   - For encrypted auth payloads, include version inside the encrypted plist.
   - On :auth-fail :reason :version-mismatch, show server version and stop retries.

2) Server
   - Extract client version in auth message parsing (encrypted + plaintext paths).
   - Compare to server version before queueing auth workers.
   - On mismatch, send :auth-fail with :reason :version-mismatch
     and include :server-version and :client-version for UI messaging.

Data Shape (Auth Messages)
--------------------------
- Request (client -> server):
  (:type :login :payload (:username "..." :password "..." :client-version "X.Y.Z"))
  (:type :register :payload (... :client-version "X.Y.Z"))
- Response (server -> client) on mismatch:
  (:type :auth-fail :reason :version-mismatch :server-version "X.Y.Z" :client-version "A.B.C")

Where to Implement
------------------
- src/net-auth.lisp
  - send-auth-message: add client version into payload
  - extract-auth-credentials: return version in parsed plist
  - handle-login-request / handle-register-request: early version check
- src/net-client.lisp
  - Login UI: display server/client version mismatch
  - Auto-login: stop retrying on version mismatch
- src/config.lisp or src/net-protocol.lisp
  - Define version constant(s) + env override

Tests
-----
- Unit test: login rejected on version mismatch (server)
- Unit test: login accepted on exact match
- Unit test: register rejected on version mismatch
- Unit test: client shows error text and stops auto-retry on mismatch

Rollout Steps
-------------
1) Add version constants + env override.
2) Plumb version through auth message serialization + parsing.
3) Enforce server-side check with clear error response.
4) Update client UI handling and auto-login logic.
5) Add tests and run full test suite.

Notes
-----
- Keep rejection reason distinct (:version-mismatch) so it never triggers
  backoff/retry logic intended for transient errors.
- Exact match only, per requirement (no compatibility range).
