# save.lisp

Purpose
- Serialize and deserialize authoritative game state for save/load functionality.
- Provides the foundation for future snapshot synchronization in networked play.

Why we do it this way
- Pure state serialization with no rendering dependency keeps the server-shaped core clean.
- Versioned format enables forward/backward compatibility and smooth migrations.
- Plist format is human-readable for debugging and easily extensible.

What it serializes
- Player state: position, HP, stats (attack/strength/defense/hitpoints with XP), inventory, equipment, timers (for all players)
- NPC state: position, HP, alive status, respawn timer, provocation, behavior state, home position
- Object state: position, count, respawn timer, respawnable flag
- World context: current zone ID, entity ID source
- Version tag: format version for migration support
- Optional visual fields (facing/frames/hit state) when `:include-visuals` is true for snapshot streaming

Key functions

**Top-Level Save/Load:**
- `serialize-game-state` - Convert game to plist snapshot (optionally `:include-visuals`).
- `deserialize-game-state` - Restore game from plist snapshot.
- `apply-game-state` - Apply snapshot into existing game, loading zones if needed.
- `save-game` - Write game state to file (logs in verbose mode).
- `load-game` - Read game state from file, optionally apply saved zone (logs in verbose mode).

**Player Serialization:**
- `serialize-player` - Convert player state to plist (durable fields + optional visuals).
- `deserialize-player` - Restore player from plist.
- `apply-player-plist` - Apply plist fields onto existing player, preserving client-only state.
- `apply-player-plists` - Apply multiple player plists to game, preserving local player state.
- `players-match-order-p` - Return true when players and plists share same ID ordering.
- `serialize-player-private` - Serialize owner-only state (inventory/equipment/stats).
- `apply-player-private-plist` - Apply owner-only state onto the local player.

Performance notes (snapshot path)
- `serialize-game-state-for-zone` prefers the zone-state `zone-players` cache when present, avoiding an O(total-players) filter pass.
- `serialize-player-compact` can use a pooled vector (`:use-pool t`) to reduce per-frame allocations on the server.

**CRITICAL - Multi-Client Player ID Lookup:**
When applying snapshots with multiple connected clients, `apply-player-plists` uses `game-net-player-id` to find the local player by ID. **Never falls back to the first player** if lookup fails - this prevents teleporting to other clients' positions. Logs a warning if player not found and keeps current player to maintain correct view.

**NPC Serialization:**
- `serialize-npc` - Convert NPC state to plist (optionally `:include-visuals`).
- `deserialize-npc` - Restore NPC from plist into existing NPC array.

**Object Serialization:**
- `serialize-object` - Convert zone object to plist.
- `deserialize-object` - Restore zone object from plist.

**Component Serialization:**
- `serialize-skill` / `deserialize-skill` - Convert skill to/from plist.
- `serialize-stat-block` / `deserialize-stat-block` - Convert stat-block to/from plist.
- `serialize-inventory` / `deserialize-inventory` - Convert inventory to/from plist.
- `serialize-inventory-slot` / `deserialize-inventory-slot` - Convert slot to/from plist.
- `serialize-equipment` / `deserialize-equipment` - Convert equipment to/from plist.

**Schema Validation:**
- `*player-schema*` - Schema definition with type/bounds rules for all player fields.
- `validate-player-plist` - Strict schema validation, returns (valid-p errors).
- `validate-player-plist-deep` - Full validation including inventory/equipment sub-structures.
- `*max-player-blob-size*` - Max raw blob size checked before parsing.
- `validate-player-plist-4way` - 4-outcome validation (action, issues, fixed-plist).

**Validation Outcomes (4-way system):**

Schema validation uses a graduated response system based on data quality:

| Outcome | When | Action |
|---------|------|--------|
| `:ok` | All data valid | Load normally |
| `:clamp` | Minor out-of-bounds | Clamp to limits, warn, load |
| `:quarantine` | Suspicious but recoverable | Quarantine account, deny login |
| `:reject` | Exploit-adjacent or malformed | Reject login with error message |

**Clampable fields** (minor issues auto-corrected):
- `hp` - Clamped to [0, 99999]
- Position fields (`x`, `y`) - Out-of-bounds → spawn point
- `playtime` - < 0 → 0
- `deaths` - < 0 → 0
- `created-at` - Missing → current time
- `version` - Set to current schema version in fixed-plist

**Rejection triggers** (structural failures):
- Invalid player ID (nil or <= 0)
- `:x`/`:y` not numeric, `:hp` not integer
- `lifetime-xp` < 0
- Inventory slot count < 0 or > `*max-item-stack-size*`
- Inventory item-id present but not a symbol
- Malformed inventory/stats structures (non-list) or negative skill XP
- Blob size exceeds `*max-player-blob-size*` (checked before parsing)

**Quarantine triggers** (severe corruption):
- `:zone-id` not a symbol
- Unknown `:zone-id` when `*known-zone-ids*` is set
- Unknown inventory item-id when `*game-data-loaded-p*` is true
- Unknown equipment item-id when `*game-data-loaded-p*` is true

Quarantined accounts cannot log in until an admin reviews and repairs the data. The server returns `:account-quarantined` on login and stores the blob for inspection. See `docs/db.md` "Phase 6: 4-Outcome Validation System" for details.

Walkthrough: save game
1) Player triggers save action
2) `serialize-game-state` captures authoritative state to plist
3) `save-game` writes plist to disk
4) Version tag ensures format compatibility

Walkthrough: load game
1) Player triggers load action
2) `load-game` reads plist from disk
3) Version check warns if format is newer
4) `deserialize-game-state` restores state into existing game
5) Returns zone ID for zone switching if needed

Save format (plist structure)
```lisp
(:version 4
 :zone-id :overworld
 :id-next 42
 :players ((:id 1 :x 100.0 :y 200.0 :hp 10 :lifetime-xp 5000
            :playtime 3600 :created-at 3940000000 :deaths 5
            :stats (:attack (:level 5 :xp 123) ...)
            :inventory (:slots ((:item-id :coins :count 50) ...))
            :equipment (:items (:wooden-sword nil nil ...))
            :attack-timer 0.0 :hit-timer 0.0 :run-stamina 1.0
            :attack-target-id 0 :follow-target-id 0)
           ...)
 :npcs ((:id 10 :x 150.0 :y 180.0 :home-x 150.0 :home-y 180.0
         :hits-left 3 :alive t :respawn-timer 0.0
         :provoked nil :behavior-state :idle :attack-timer 0.0) ...)
 :objects ((:id :arrows :x 5 :y 10 :count 5
            :respawn 0.0 :respawnable t) ...))
```

Design note
- Only server-authoritative state is saved (no UI, no rendering state).
- Intent is NOT saved (it's ephemeral per-frame input).
- Save files omit visual fields; network snapshots can opt in to visuals via `:include-visuals`.
- This format becomes the future snapshot sync format for networking.
- Network-only fields (e.g., `:last-sequence` for prediction) live only in compact snapshots.

Client/Server Preparation
- Save state is the "server-shaped core" - authoritative game state only
- No rendering, UI, or client-side presentation state included
- Deterministic: same save file should produce same game state
- Ready to become snapshot format for future client/server split
- Version tag enables seamless protocol migrations
- Compact snapshots exclude private state (inventory/equipment/stats); those sync via private messages.

Relationship to db.md (Database Architecture)
-----------------------------------------------
**db.md is the authoritative long-term persistence spec.** This file (save.md) documents the serialization layer.

How they work together:
- **save.lisp** = Serialization (what gets saved, plist format, versioning)
- **db.md** = Storage backend (where it goes: Redis, file, Postgres)

```
Game State -> serialize-game-state -> plist -> storage-save -> Redis/File/etc.
                 (save.lisp)                     (db.lisp)
```

The serialization functions (`serialize-game-state`, `deserialize-game-state`) remain unchanged regardless of storage backend. The storage abstraction layer (defined in db.md) handles the actual persistence.

Key db.md concepts that affect this code:
- **Durable vs Ephemeral**: save.lisp serializes durable state only
- **Write Tiers**: serialize-game-state is used for both tier-2 batched writes and tier-3 logout snapshots
- **Versioned format**: Already implemented here, migrations defined per db.md spec
- **HP is durable**: Current HP must be serialized (prevents logout-heal exploit)
- **lifetime-xp is durable**: Total XP ever earned (v2 schema addition, shows progression)

When implementing the storage layer:
1. Keep serialization in save.lisp (serialize/deserialize functions)
2. Add storage abstraction in db.lisp (storage-load/storage-save protocol)
3. Game code calls db.lisp, which uses save.lisp for format conversion

Future extensions
-----------------
Serialization improvements:
- Add world tick counter for deterministic replay
- Add timestamp for save file sorting
- Add player-provided save name/description
- Compress large save files
- Add checksum for corruption detection

Storage improvements (see db.md for full spec):
- Redis backend with RDB+AOF persistence
- Tiered write system (immediate vs batched)
- Migration-on-login for schema changes
- Postgres cold storage for scale
