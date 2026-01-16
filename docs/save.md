# save.lisp

Purpose
- Serialize and deserialize authoritative game state for save/load functionality.
- Provides the foundation for future snapshot synchronization in networked play.

Why we do it this way
- Pure state serialization with no rendering dependency keeps the server-shaped core clean.
- Versioned format enables forward/backward compatibility and smooth migrations.
- Plist format is human-readable for debugging and easily extensible.

What it serializes
- Player state: position, HP, stats (attack/strength/defense/hitpoints with XP), inventory, equipment, timers
- NPC state: position, HP, alive status, respawn timer, provocation, behavior state, home position
- Object state: position, count, respawn timer, respawnable flag
- World context: current zone ID, entity ID source
- Version tag: format version for migration support

Key functions
- `serialize-game-state`: converts game to plist snapshot
- `deserialize-game-state`: restores game from plist snapshot
- `save-game`: writes game state to file
- `load-game`: reads game state from file

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
(:version 1
 :zone-id :overworld
 :id-next 42
 :player (:id 1 :x 100.0 :y 200.0 :hp 10
          :stats (:attack (:level 5 :xp 123) ...)
          :inventory (:slots ((:item-id :coins :count 50) ...))
          :equipment (:items (:wooden-sword nil nil ...))
          :attack-timer 0.0 :hit-timer 0.0 :run-stamina 1.0
          :attack-target-id 0 :follow-target-id 0)
 :npcs ((:id 10 :x 150.0 :y 180.0 :home-x 150.0 :home-y 180.0
         :hits-left 3 :alive t :respawn-timer 0.0
         :provoked nil :behavior-state :idle :attack-timer 0.0) ...)
 :objects ((:id :arrows :x 5 :y 10 :count 5
            :respawn 0.0 :respawnable t) ...))
```

Design note
- Only server-authoritative state is saved (no UI, no rendering state).
- Intent is NOT saved (it's ephemeral per-frame input).
- Animation state is NOT saved (it's derived from current action).
- This format becomes the future snapshot sync format for networking.

Client/Server Preparation
- Save state is the "server-shaped core" - authoritative game state only
- No rendering, UI, or client-side presentation state included
- Deterministic: same save file should produce same game state
- Ready to become snapshot format for future client/server split
- Version tag enables seamless protocol migrations

Future extensions
- Add world tick counter for deterministic replay
- Add timestamp for save file sorting
- Add player-provided save name/description
- Compress large save files
- Support multiple save slots
- Add checksum for corruption detection
