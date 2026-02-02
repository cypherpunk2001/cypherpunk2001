# net-protocol.lisp

Purpose
- Network message encoding/decoding, buffer management, validation, and UDP send/receive primitives for the client/server protocol.

Key responsibilities
- `make-net-buffer`: allocate reusable UDP byte buffers.
- `string-to-octets` / `octets-to-string`: ASCII encoding/decoding for network messages.
- `encode-net-message` / `decode-net-message`: serialize/deserialize message plists with safe read (`*read-eval* nil`).
- `send-net-message`: send a plist message over UDP with size validation and error handling.
- Message format constants and protocol type definitions (`:hello`, `:intent`, `:snapshot`, `:save`, `:load`).
- Buffer size enforcement to prevent oversized UDP payloads.
- Binary snapshot encoding/decoding helpers for compact wire format.
- Client management data structures and connection tracking.

Load order
- Loaded first among net files: `net-protocol` -> `net-auth` -> `net-snapshot` -> `net-client` -> `net-server` -> `net`.
- All other net files depend on message encoding and buffer utilities defined here.
