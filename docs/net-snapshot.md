# net-snapshot.lisp

Purpose
- Snapshot transmission including fragmentation for large payloads, delta compression, reusable buffer management, and client-side interpolation/prediction.

Key responsibilities
- `send-fragmented-snapshot`: split oversized snapshots into UDP-safe chunks with reusable buffers.
- `ensure-fragmentation-buffers`: lazy allocation of reusable payload and chunk buffers to minimize per-frame allocation.
- Delta snapshot logic: send only dirty entities to reduce bandwidth.
- Snapshot compression and decompression for wire efficiency.
- Client-side snapshot interpolation for smooth entity movement between updates.
- Client-side prediction and reconciliation for local player responsiveness.
- Fragmentation reassembly on the client side from ordered chunks.
- Configurable fragmentation parameters (`*max-chunk-payload*`, buffer sizes).

Load order
- Loaded third among net files: `net-protocol` -> `net-auth` -> `net-snapshot` -> `net-client` -> `net-server` -> `net`.
- Depends on `net-protocol` for encoding; depends on `save-delta` and `save-serialize` for snapshot serialization.
