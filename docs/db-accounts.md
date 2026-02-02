# db-accounts.lisp

Purpose
- Account management, credential storage, password hashing with PBKDF2-SHA256, and auth encryption via X25519 + AES-256-GCM.

Key responsibilities
- Password hashing using ironclad PBKDF2-SHA256 with configurable iterations and salt.
- Salt generation with cryptographically secure random data.
- Account creation and verification against stored credentials.
- Hex encoding/decoding utilities for password hashes and salts.
- Key derivation with explicit iteration counts for legacy hash verification.
- Password hash migration (re-hash with updated iteration count on successful login).
- Auth encryption key exchange and message encryption/decryption.

Load order
- Loaded third among db files: `db-storage` -> `db-players` -> `db-accounts` -> `db-admin` -> `db`.
- Depends on `db-storage` for storage protocol and `db-players` for player operations.
