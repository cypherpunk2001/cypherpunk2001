# Admin Commands

REPL-based admin tools for managing the game server. All commands run from SLIME connected to the server process - no CLI tool or GUI needed.

## Tier A: Implemented

Commands that can be built with current infrastructure.

### Player Data Inspection

```lisp
(admin-print-save player-id)        ; Pretty-print player's saved data
(admin-print-save "username")       ; Lookup by username
(admin-list-players)                ; List all online players (id, name, zone, hp)
(admin-find-player "partial-name")  ; Search players by partial name match
```

### Player Modification

```lisp
(admin-grant-item player-id :iron-sword 1)   ; Give item
(admin-remove-item player-id :coins 500)     ; Take item
(admin-clear-inventory player-id)            ; Wipe inventory
(admin-set-hp player-id 100)                 ; Set current HP
(admin-set-xp player-id 5000)                ; Set XP (recalculates level)
(admin-set-level player-id 10)               ; Set level (adjusts XP to match)
(admin-set-coins player-id 10000)            ; Set gold
(admin-teleport player-id :overworld 500 300) ; Move to zone/coords
(admin-teleport player-id :to-player other-id) ; Teleport to another player
```

### Character Management

```lisp
(admin-wipe-character player-id)    ; Delete character from DB (irreversible)
(admin-kick player-id "reason")     ; Disconnect player from server
(admin-reset-position player-id)    ; Move to zone spawn point (stuck fix)
```

### Server Operations

```lisp
(admin-broadcast "Server restart in 5 minutes")  ; Message to all players
(admin-save-all)                    ; Force flush all dirty players to DB
(admin-player-count)                ; Current online count
(admin-server-stats)                ; Uptime, total saves, connected clients
```

## Tier B: Need Infrastructure First

Commands that require new systems to be built.

### Ban System

Requires: ban table in Redis, check on login.

```lisp
(admin-ban player-id :reason "exploiting" :duration :permanent)
(admin-ban player-id :reason "harassment" :duration (* 24 60 60)) ; 24h in seconds
(admin-unban player-id)
(admin-list-bans)                   ; Show all active bans
(admin-check-ban player-id)         ; Is player banned?
```

### IP Tracking

Requires: store IP on connect, history table.

```lisp
(admin-player-ips player-id)        ; All IPs this player has used
(admin-ip-players "192.168.1.x")    ; All players from this IP (alt detection)
(admin-ban-ip "192.168.1.100")      ; IP ban
```

### Login History

Requires: log timestamps on login/logout.

```lisp
(admin-login-history player-id)     ; Last N logins with timestamps
(admin-playtime player-id)          ; Total playtime (we track this already)
(admin-last-seen player-id)         ; When did they last log out
```

### Chat Logs

Requires: log chat messages to storage.

```lisp
(admin-chat-history player-id)      ; Recent chat from player
(admin-chat-search "bad word")      ; Search chat logs
```

## Tier C: Nice to Have

Future convenience commands.

```lisp
(admin-spawn-npc :goblin 500 300)   ; Spawn NPC at location
(admin-kill-npcs :zone :overworld)  ; Clear all NPCs in zone
(admin-set-weather :zone :rain)     ; Weather control (if we add weather)
(admin-maintenance-mode t)          ; Block new logins
(admin-rollback player-id "2025-01-17T14:00") ; Restore from backup timestamp
```

## Implementation Notes

1. **Export all commands** from the `mmorpg` package
2. **Validate player-id exists** before modifying
3. **Log all admin actions** (who did what, when) for accountability
4. **Persistence**: Most admin modifications mark players dirty (tier-2) and rely on batch flush;
   destructive actions (wipe/kill) persist immediately. Use `db-save-player-immediate` if you
   need immediate persistence for a specific admin path.
5. **Return values**: plist for inspection commands, T/NIL for actions
6. **Error handling**: Signal clear conditions, don't silently fail

## Example Session

```lisp
MMORPG> (admin-list-players)
((1 "alice" :overworld 100 10)    ; id name zone hp level
 (2 "bob" :dungeon-1 45 8))

MMORPG> (admin-print-save 1)
(:VERSION 3
 :ID 1
 :USERNAME "alice"
 :HP 100
 :XP 12500
 ...)

MMORPG> (admin-grant-item 1 :health-potion 5)
T  ; success

MMORPG> (admin-teleport 2 :overworld 100 100)
T  ; bob is now in overworld

MMORPG> (admin-kick 2 "testing admin commands")
T  ; bob disconnected
```

## Status

- **Tier A**: Implemented
- **Tier B**: Blocked on infrastructure
- **Tier C**: Future roadmap
