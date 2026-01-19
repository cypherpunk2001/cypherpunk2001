# config-server.lisp

Purpose
- Server-only configuration parameters.
- Separated from client config for clarity and to prevent loading unnecessary options.

Why we do it this way
- Clear separation: developers know which options affect server vs client.
- Reduced cognitive load: server operators only see relevant settings.
- Potential future benefit: headless server builds could skip client config entirely.

How it connects
- Values can be overridden by `data/game-data.lisp` via `load-game-data`.
- New tunables should be added to `*tunable-keys*` in `data.lisp` so they can be data-driven.

---

## Restart Required Parameters

### Network Configuration
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*net-default-host*` | `"127.0.0.1"` | Default UDP host for client/server |
| `*net-default-port*` | `1337` | Default UDP port for client/server |
| `*net-buffer-size*` | `65507` | Max UDP payload size for snapshot messages |
| `*private-state-retries*` | `3` | Frames to resend private state updates to the owning client |

*Why restart:* UDP receive buffer created once per socket. Changing this doesn't resize existing buffers.

### Delta Compression
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*delta-compression-enabled*` | `t` | Enable delta compression (dirty entities only) |
| `*max-delta-age*` | `60` | Max snapshots behind before forcing full resync |
| `*max-delta-gap*` | `5` | Max snapshot gap tolerated before forcing full resync |

### Auth Encryption
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*auth-encryption-enabled*` | `nil` | Enable auth payload encryption (X25519 + ChaCha20-Poly1305) |
| `*server-auth-public-key*` | `nil` | Server's X25519 public key as hex string |
| `*auth-require-encryption*` | `nil` | Server rejects plaintext auth when T (set for production) |

*Why restart:* Client must know server's public key to encrypt credentials. Server generates this at startup.

### Player Data Structures
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*inventory-size*` | `20` | Player inventory slots |
| `*equipment-slot-ids*` | `#(:head :body :legs :weapon :offhand :accessory)` | Equipment slot order |

*Why restart:* Inventory/equipment arrays allocated with fixed size when player struct is created.

### New Player Stats
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*player-base-attack*` | `1` | Base attack level for new players |
| `*player-base-strength*` | `1` | Base strength level for new players |
| `*player-base-defense*` | `1` | Base defense level for new players |
| `*player-base-hitpoints*` | `10` | Base hitpoints level for new players |
| `*player-training-mode*` | `:balanced` | Training focus (:attack/:strength/:defense/:hitpoints/:balanced) |

*Why restart:* These set initial values for newly created players. Existing players retain their stats.

---

## Immediate Effect Parameters

### Simulation Timing
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*sim-tick-seconds*` | `1/60` | Fixed simulation tick length in seconds |
| `*sim-max-steps-per-frame*` | `5` | Max sim ticks per frame to avoid spiral of death |

### Combat & XP
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*stat-xp-per-level*` | `100` | XP needed for each quadratic level step |
| `*stat-max-level*` | `99` | Maximum attainable level per stat |
| `*xp-per-damage*` | `4` | XP awarded per point of damage dealt |
| `*combat-hitpoints-xp-multiplier*` | `0.33` | HP XP multiplier applied to focused combat XP |
| `*attack-hitbox-scale*` | `1.0` | Attack hitbox size relative to one tile |

### NPC Behavior
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*npc-max-hits*` | `3` | Hits required to defeat the NPC |
| `*npc-walk-speed*` | `120.0` | Base NPC movement speed in pixels per second |
| `*npc-flee-speed-mult*` | `1.4` | Speed multiplier while fleeing |
| `*npc-attack-range-tiles*` | `0.9` | NPC melee range in tiles |
| `*npc-attack-cooldown*` | `0.9` | Seconds between NPC attacks |
| `*npc-attack-damage*` | `1` | Damage per NPC hit |
| `*npc-home-radius-tiles*` | `2.0` | Roam radius around spawn in tiles |
| `*npc-wander-interval*` | `1.1` | Seconds between wander target changes |
| `*npc-wander-arrive-distance*` | `6.0` | Pixels to consider wander target reached |
| `*npc-respawn-seconds*` | `5.0` | Default respawn cooldown in seconds |
| `*npc-default-archetype-id*` | `:rat` | Default NPC archetype ID to spawn |
| `*npc-default-loot-table-id*` | `nil` | Fallback loot table when NPC archetypes omit one |
| `*npc-collision-scale*` | `2.0` | Collision box size relative to one tile |

### Combat Targeting
| Parameter | Default | Description |
|-----------|---------|-------------|
| `*max-target-distance-tiles*` | `15` | Maximum tiles away a player can target an NPC |
