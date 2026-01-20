# Test Coverage TODO

This file documents test coverage status and remaining gaps.

**Updated test counts (after this session):**
- unit-test.lisp: 173 tests (was ~98)
- persistence-test.lisp: 98 tests
- security-test.lisp: 27 tests
- trade-test.lisp: 14 tests
- **Total: 312 tests**

**All tests pass:** `make tests` completes successfully.

---

## Completed Tests (This Session)

The following tests were added and pass:

### Priority 1: Core Game Logic
- [x] `combatant-display-name` - Returns "Player" for players, archetype name for NPCs
- [x] `find-npc-by-id` - Find NPC in vector by ID
- [x] `roll-melee-damage` - Damage roll within expected range
- [x] `update-skill-level` - Sync skill level from XP
- [x] `clamp-player-hp` - Clamp HP to effective max
- [x] `format-xp-awards` - Format XP award text (nil when all zeros)
- [x] `item-display-name` - Get item name from archetype
- [x] `inventory-slot-label` - Build slot label with count
- [x] `inventory-add` - Add items with stacking logic (tested with coins)
- [x] `inventory-remove` - Remove items from slots
- [x] `roll-loot-entry` - Weighted random loot selection
- [x] `roll-loot-count` - Roll count between min/max
- [x] `object-respawn-seconds` - Get respawn time from archetype
- [x] `object-respawnable-p` - Check if object respawns
- [x] `object-respawn-timer` - Get active respawn timer
- [x] `npc-home-radius` - Function exists (requires world param - see notes)
- [x] `npc-move-speed` - Get movement speed from archetype
- [x] `npc-wander-interval` - Get wander interval from archetype

### Priority 2: Data Loading/Parsing
- [x] `validate-item-archetype-plist` - Validate item plist fields/types
- [x] `item-archetype-from-plist` - Build item archetype from plist
- [x] `validate-object-archetype-plist` - Validate object plist fields
- [x] `object-archetype-from-plist` - Build object archetype from plist
- [x] `loot-entry-from-spec` - Parse loot entry tuple
- [x] `validate-loot-table-plist` - Validate loot table plist
- [x] `loot-table-from-plist` - Build loot table from plist
- [x] `animation-set-from-plist` - Build animation set from plist
- [x] `merge-animation-sets` - Merge override into base animation
- [x] `zone-label` - Get display label for zone
- [x] `zone-data-plist` - Normalize zone data to plist
- [x] `make-empty-zone` - Create blank zone
- [x] `build-tiles-from-fill` - Build tile vector from fill value
- [x] `zone-layer-tile-at` - Get tile at coordinates

### Priority 3: Network Protocol
- [x] `string-to-octets` - ASCII string to byte vector
- [x] `octets-to-string` - Byte vector to ASCII string
- [x] `encode-net-message` / `decode-net-message` - Plist roundtrip
- [x] `host-to-string` - Convert byte vector to IP string

### Priority 5: Intent System
- [x] `reset-frame-intent` - Clear per-frame signals
- [x] `consume-intent-actions` - Clear one-shot actions
- [x] `set-intent-target` - Set and activate target
- [x] `clear-intent-target` - Deactivate target
- [x] `request-pickup-target` - Set pickup request
- [x] `request-drop-item` - Set drop request
- [x] `request-inventory-swap` - Set swap request
- [x] Trade intent functions - request/clear trade operations

### Priority 6: Utils/Helpers
- [x] `player-animation-params` - Get frame count/time for state
- [x] `relative-path-from-root` - Make path relative to root

---

## Still Needs Tests (Future Work)

### ai.lisp - Complete
- [x] `closest-player` - Find nearest alive player to NPC (uses make-test-world helper)
- [x] `npc-in-perception-range-p` - Check if player within perception range
- [x] `update-npc-behavior` - State machine transitions (tests dead/idle/valid states)
- [x] `npc-flee-speed-mult` - Get flee multiplier from archetype

### combat.lisp - Complete
- [x] `format-combat-log` - Format hit/miss combat log string
- [x] `npc-respawn-seconds` - Get respawn time from archetype
- [x] `target-in-range-p` - Check if NPC within targeting range
- [x] `npc-attack-range` - Calculate NPC melee range in pixels
- [x] `npc-attack-cooldown` - Get attack cooldown from archetype
- [x] `npc-attack-damage` - Get attack damage from archetype
- [x] `intent-attack-direction` - Choose attack direction from intent (returns nil when no input/target)
- [x] `attack-hitbox` - Calculate attack hitbox for facing direction
- [x] `player-attack-target-in-range-p` - Check if target in melee hitbox

### progression.lisp - Complete
- [x] `award-skill-xp` - Add XP and update level
- [x] `melee-hit-p` - Simple hit check wrapper
- [x] `format-skill-hud-line` - Format HUD line for skill
- [x] `apply-item-modifiers` - Apply/remove equipment stat bonuses
- [x] `object-entry-count` - Get pickup count from object

### data.lisp - Complete
- [x] `parse-game-data-forms` - Merge data forms into sections
- [x] `make-npc-archetype-from-plist` - Build NPC archetype from plist

### zone.lisp - Complete
- [x] `zone-chunk-from-spec` - Build chunk from spec
- [x] `zone-layer-from-spec` - Build layer from spec
- [x] `build-zone-collision-tiles` - Build blocked tile hash
- [x] `zone-wall-map` - Convert collision to wall array
- [x] `zone-layer-by-id` - Find layer by ID
- [x] `zone-to-plist` - Serialize zone for saving
- [x] `zone-slice` - Extract subregion of zone
- [x] `zone-resize` - Resize zone preserving content
- [x] `load-zone` / `write-zone` roundtrip test

### world-graph.lisp - Complete
- [x] `collect-zone-files` - Find .lisp files in directory
- [x] `zone-id-from-file` - Read zone ID without full load
- [x] `build-zone-paths` - Build zone-id -> path lookup
- [x] `world-graph-exits` - Get exits for zone
- [x] `world-graph-zone-path` - Get path for zone ID

### movement.lisp - Complete
- [x] `get-zone-state` - Get zone state from cache
- [x] `zone-state-player-count` - Count players in zone
- [x] `players-in-zone` - Get player vector for zone
- [x] `occupied-zone-ids` - Get list of zones with players
- [x] `derive-wall-map-from-zone` - Build wall map from zone
- [x] `wall-occupied-p` - Check if tile has wall
- [x] `blocked-at-p` - Test collider against tiles
- [x] `attempt-move` - Resolve movement with collision
- [x] `update-running-state` - Stamina drain/regen
- [x] `edge-preserve-axis` - Already tested via edge-offset-ratio
- [x] `edge-spawn-position` - Calculate spawn position on edge
- [x] `zone-bounds-from-dimensions` - Calculate wall bounds
- [x] `position-blocked-p` - Check if position is blocked
- [x] `find-open-tile` - Find nearest unblocked tile
- [x] `player-is-stuck-p` - Check if player can't move
- [x] `world-exit-edge` - Determine which edge player is at

### net.lisp - Mostly Complete
- [x] `session-try-register` - Atomic session registration
- [x] `session-unregister` - Remove from active sessions
- [x] `session-get` - Get client by username
- [ ] `auth-check-replay` - Nonce/timestamp replay detection (requires time manipulation, tested via integration)

---

## Requests for src/ Changes

The following issues were encountered during test writing that would benefit from src/ modifications:

### 1. `npc-home-radius` Requires World Object - RESOLVED
**File:** `src/ai.lisp:19`
**Issue:** Function takes `(npc world)` but `world` is hard to construct in tests.
**Resolution:** Created `make-test-world` helper in unit-test.lisp that builds a minimal world struct. Test now passes.

### 2. `combatant-display-name` Returns Generic "Player"
**File:** `src/combat.lisp:140`
**Current:** Returns "Player" for all players regardless of username.
**Suggestion:** Consider making it return the actual username if available (would need to pass username or look up from session).

### 3. `player` struct has no `username` field
**File:** `src/types.lisp:4`
**Issue:** Username is tracked in net-client, not player struct.
**Impact:** Tests can't easily verify player-specific display names.

---

## Not Recommended for Unit Tests

These are better tested via integration/smoke tests:

### admin.lisp - Admin Commands
Admin functions rely on global server state and are better tested manually or via integration tests.

### rendering.lisp, ui.lisp, editor.lisp, input.lisp, audio.lisp
Require GPU, audio device, or real-time input. Tested via smoke test.

### main.lisp, server.lisp
Entry points tested via CI and smoke tests.

---

## Test Writing Guidelines

When adding tests for remaining functions:

1. **Location by type:**
   - Pure game logic -> `tests/unit-test.lisp`
   - Serialization/persistence -> `tests/persistence-test.lisp`
   - Input validation/exploits -> `tests/security-test.lisp`
   - Trade system -> `tests/trade-test.lisp`

2. **Test naming:** `test-<function-name>` or `test-<function-name>-<scenario>`

3. **Run tests after adding:**
   ```bash
   make checkparens && make ci && make smoke && make test-unit
   ```

4. **Full test suite:**
   ```bash
   make tests
   ```
