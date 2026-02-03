# config-server.lisp

Purpose
- Server-only configuration parameters.
- Separated from client config for clarity and to prevent loading unnecessary options.

Status
- **Implemented.** This doc reflects `src/config-server.lisp` as of 2026-02-03.

Why we do it this way
- Clear separation: developers know which options affect server vs client.
- Reduced cognitive load: server operators only see relevant settings.
- Potential future benefit: headless server builds could skip client config entirely.

How it connects
- Values can be overridden by `data/game-data.lisp` via `load-game-data` when included in `*tunable-keys*`.
- Environment variables override some server-only settings at startup (see tables below).

---

## Restart Required Parameters

### Network Configuration
| Parameter | Default | Description | Env Override |
|-----------|---------|-------------|--------------|
| `*net-default-host*` | `"127.0.0.1"` | Default UDP host for client/server | `MMORPG_NET_HOST` (where used) |
| `*net-default-port*` | `1337` | Default UDP port for client/server | `MMORPG_NET_PORT` (where used) |
| `*net-buffer-size*` | `65507` | Max UDP payload size for snapshot messages | — |
| `*private-state-retries*` | `3` | Frames to resend owner-only private state updates | — |

*Why restart:* UDP socket and buffers are created once per server startup.

### Snapshot Rate
| Parameter | Default | Description | Env Override |
|-----------|---------|-------------|--------------|
| `*snapshot-rate-hz*` | `20` | Snapshot send rate in Hz (decoupled from sim rate) | `MMORPG_SNAPSHOT_RATE` |
| `*snapshot-interval*` | `1/20` | Seconds between snapshots (computed from rate) | Derived |

### Delta Compression
| Parameter | Default | Description | Env Override |
|-----------|---------|-------------|--------------|
| `*delta-compression-enabled*` | `t` | Enable delta compression (dirty entities only) | `MMORPG_DELTA_COMPRESSION` |
| `*max-delta-age*` | `60` | Max snapshots behind before forcing full resync | — |
| `*max-delta-gap*` | `5` | Max snapshot gap tolerated before forcing full resync | — |

### UDP Fragmentation
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*chunk-overhead*` | `100` | Reserved bytes for chunk message header |
| `*max-chunk-payload*` | `65507-100` | Max payload bytes per UDP chunk |
| `*chunk-timeout*` | `1.0` | Seconds before discarding incomplete chunk sequences |

### Binary Snapshots
| Parameter | Default | Description | Env Override |
|-----------|---------|-------------|--------------|
| `*use-binary-snapshots*` | `nil` | Use compact binary snapshots instead of plist text | `MMORPG_BINARY_SNAPSHOTS=1` |

### Auth Worker Pool
| Parameter | Default | Description | Env Override |
|-----------|---------|-------------|--------------|
| `*auth-worker-count*` | `4` | Number of auth worker threads | `MMORPG_AUTH_WORKERS` |
| `*auth-queue-max-depth*` | `200` | Max queued auth requests (0 = unlimited) | `MMORPG_AUTH_QUEUE_MAX` |
| `*auth-request-max-age*` | `25.0` | Seconds before queued auth expires | — |
| `*max-messages-per-tick*` | `2000` | Max UDP messages processed per tick | `MMORPG_MAX_MESSAGES_PER_TICK` |

### Auth Metrics Logging
| Parameter | Default | Description | Env Override |
|-----------|---------|-------------|--------------|
| `*auth-metrics-logging*` | `nil` | Log auth metrics every ~30s when changed | `MMORPG_AUTH_METRICS=1` |

### Password Hashing
| Parameter | Default | Description | Env Override |
|-----------|---------|-------------|--------------|
| `*password-hash-iterations*` | `10000` | PBKDF2 iterations for new hashes | `MMORPG_PASSWORD_HASH_ITERATIONS` |
| `*password-legacy-iterations*` | `100000` | Legacy iteration count for old hashes | — |
| `*password-salt-bytes*` | `16` | Salt length in bytes | — |

### Auth Encryption
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*auth-encryption-enabled*` | `nil` | Enable auth payload encryption (X25519 + ChaCha20-Poly1305) |
| `*server-auth-public-key*` | `nil` | Server public key (hex string) |
| `*auth-require-encryption*` | `nil` | Reject plaintext auth when T |

*Why restart:* Encryption keys are initialized at startup and must match clients.

### Player Data Structures
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*inventory-size*` | `20` | Player inventory slots |
| `*equipment-slot-ids*` | `#(:head :body :legs :weapon :offhand :accessory)` | Equipment slot order |

### New Player Stats
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*player-base-attack*` | `1` | Base attack level |
| `*player-base-strength*` | `1` | Base strength level |
| `*player-base-defense*` | `1` | Base defense level |
| `*player-base-hitpoints*` | `10` | Base hitpoints level |
| `*player-training-mode*` | `:balanced` | Training focus for new players |

---

## Immediate Effect Parameters

### Simulation Timing
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*sim-tick-seconds*` | `1/60` | Fixed simulation tick length |
| `*sim-max-steps-per-frame*` | `5` | Max sim ticks per frame (spiral-of-death guard) |

### Combat & XP
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*stat-xp-per-level*` | `100` | XP needed for each level step |
| `*stat-max-level*` | `99` | Maximum stat level |
| `*xp-per-damage*` | `4` | XP per damage point dealt |
| `*combat-hitpoints-xp-multiplier*` | `0.33` | HP XP multiplier for focused combat XP |
| `*attack-hitbox-scale*` | `1.0` | Attack hitbox scale vs tile size |

### NPC Behavior
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*npc-max-hits*` | `3` | Hits to defeat NPC |
| `*npc-walk-speed*` | `120.0` | Base NPC movement speed |
| `*npc-flee-speed-mult*` | `1.4` | Speed multiplier while fleeing |
| `*npc-attack-range-tiles*` | `0.9` | NPC melee range in tiles |
| `*npc-attack-cooldown*` | `0.9` | Seconds between NPC attacks |
| `*npc-attack-damage*` | `1` | Damage per NPC hit |
| `*npc-home-radius-tiles*` | `2.0` | Roam radius around spawn |
| `*npc-wander-interval*` | `1.1` | Seconds between wander target changes |
| `*npc-wander-arrive-distance*` | `6.0` | Pixels to consider wander target reached |
| `*npc-respawn-seconds*` | `5.0` | Respawn cooldown |
| `*npc-default-archetype-id*` | `:rat` | Default NPC archetype |
| `*npc-default-loot-table-id*` | `nil` | Fallback loot table |
| `*npc-collision-scale*` | `2.0` | Collision box size vs tile |

### Combat Targeting
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*max-target-distance-tiles*` | `15` | Max tiles away a player can target NPCs |

### GC Scheduling
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*gc-scheduling-enabled*` | `nil` | Enable periodic safe-point GC |
| `*gc-interval-seconds*` | `60.0` | Minimum seconds between scheduled GC |
| `*gc-last-time*` | `0.0` | Internal timer for last GC trigger |

---

## Notes

- This file controls server-only behavior. Client settings live in `docs/config-client.md`.
- Some values can be data-driven by adding them to `*tunable-keys*` in `data.lisp`.
