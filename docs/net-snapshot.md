# net-snapshot.lisp

Purpose
- Snapshot transmission including fragmentation for large payloads, delta compression, reusable buffer management, and client-side interpolation/prediction.

Status
- **Implemented.** This doc reflects `src/net-snapshot.lisp` as of 2026-02-03.

Key responsibilities
- **Zone‑grouped snapshot broadcast:** group clients by zone and serialize once per zone (encode‑once‑send‑many).
- **Delta compression:** send only dirty entities when safe; force full resync on zone changes, large gaps, or explicit resync flags.
- **UDP fragmentation:** split oversized snapshots into chunks and reassemble on client.
- **Reusable buffers:** avoid per‑frame allocations for fragmentation payloads and chunk buffers.
- **Client apply path:** apply snapshots, detect teleports, and reset interpolation/prediction state as needed.
- **Client sync helpers:** on zone change or resync, call `sync-client-zone-npcs` to align rendering arrays and NPC grids.
- **Binary snapshot support:** encode/decode compact binary snapshot format when enabled.

Key functions
- `broadcast-snapshots-with-delta`: zone grouping, resync/delta partition, per‑zone serialization.
- `send-snapshots-parallel`: encode once, send to many clients (parallel worker threads).
- `send-fragmented-snapshot` / `receive-snapshot-chunk`: UDP fragmentation with reusable buffers.
- `apply-snapshot`: apply game state, detect zone change/teleport, reset interpolation/prediction if needed, and sync NPC zone-state on resync.
- `reset-client-sync-state`: clears interpolation/prediction buffers after teleports.

Behavior notes
- **Zone change:** forces resync and triggers `handle-zone-transition` on client.
- **Teleport detection:** uses distance threshold to avoid stale interpolation (“frozen” artifacts).
- **Resync:** if server marks `:resync`, client rebuilds NPC zone-state data.

Load order
- Loaded third among net files: `net-protocol` -> `net-auth` -> `net-snapshot` -> `net-client` -> `net-server` -> `net`.
- Depends on `net-protocol` for encoding and `save-serialize`/`save-delta` for snapshot formats.
