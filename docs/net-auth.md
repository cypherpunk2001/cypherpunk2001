# net-auth.lisp

Purpose
- Authentication handling, session management with thread-safe locking, rate limiting, and login/register message processing.

Key responsibilities
- `*active-sessions*`: thread-safe hash table mapping usernames to net-client connections.
- `with-session-lock`: mutex-protected session operations for atomic check-and-set.
- `session-try-register` / `session-unregister` / `session-get`: atomic session lifecycle management.
- `session-clear-all`: clean session state on server startup/shutdown.
- Login and register message handling with credential verification.
- Client timeout detection (`*client-timeout-seconds*`) for stale connection cleanup.
- Rate limiting for auth attempts to prevent brute-force attacks.
- Auth queue processing for non-blocking credential verification on the main thread.

Load order
- Loaded second among net files: `net-protocol` -> `net-auth` -> `net-snapshot` -> `net-client` -> `net-server` -> `net`.
- Depends on `net-protocol` for message encoding; depends on `db-accounts` for credential verification.
