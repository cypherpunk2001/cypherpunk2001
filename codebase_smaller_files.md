# PLAN: Split large src/*.lisp into smaller, descriptive files (~<=1000 LOC)

## Decision
Proceed. Several `src/` files exceed 1,000–3,600 LOC, which makes changes riskier and harder for LLMs and humans. Splitting these files into coherent, domain‑named modules reduces syntax errors and improves reviewability without changing behavior.

## Goals
- Keep each `src/*.lisp` file around ~1,000 LOC or less.
- Preserve load order and semantics (ASDF `:serial t`).
- File names clearly reflect their contents.
- Documentation stays aligned with the new file layout.

## Non‑goals
- No behavioral changes.
- No real code changes — this is strictly organizational/stylistic (move code, update load order/docs).
- No refactors beyond moving code and adjusting file order/load.
- No change to test framework or runtime behavior.

## Inventory (current large files)
Based on `wc -l` (Feb 2026):
- `src/net.lisp` ~3600
- `src/rendering.lisp` ~2600
- `src/db.lisp` ~2300
- `src/save.lisp` ~2100
- `src/movement.lisp` ~1800
- `src/editor.lisp` ~1050

## Proposed splits (names reflect content)

### 1) `src/net.lisp` (split into protocol + client/server + auth + snapshots)
Proposed files:
- `src/net-protocol.lisp` — message formats, encode/decode, validation
- `src/net-auth.lisp` — login/register handling, auth queues
- `src/net-snapshot.lisp` — snapshot send/receive, delta logic, compression
- `src/net-client.lisp` — client networking loop, reconnect, client handlers
- `src/net-server.lisp` — server UDP loop, dispatch, connection tracking
- `src/net.lisp` — thin glue / shared helpers

### 2) `src/rendering.lisp` (split by render pipeline stage)
Proposed files:
- `src/rendering-core.lisp` — core draw helpers, rectangles, textures
- `src/rendering-tiles.lisp` — map/tileset rendering, preview zones
- `src/rendering-entities.lisp` — players/NPCs/objects rendering + culling
- `src/rendering-ui.lisp` — HUD overlays that are rendering‑only (no UI state)
- `src/rendering.lisp` — glue + shared state

### 3) `src/db.lisp` (split by responsibilities)
Proposed files:
- `src/db-storage.lisp` — storage abstraction + backend selection
- `src/db-players.lisp` — player save/load, dirty flags, logout
- `src/db-accounts.lisp` — account creation/verification
- `src/db-admin.lisp` — admin tooling, migrate‑all
- `src/db.lisp` — glue + shared helpers

### 4) `src/save.lisp` (split by serialization vs deserialization vs edge strips)
Proposed files:
- `src/save-serialize.lisp` — serialize‑* functions, snapshot build
- `src/save-deserialize.lisp` — deserialize‑* functions, apply‑state
- `src/save-delta.lisp` — delta encoding/decoding
- `src/save-edge-strips.lisp` — edge strip serialization/deserialization
- `src/save-validate.lisp` — schema checks, bounds validation
- `src/save.lisp` — glue + shared helpers

### 5) `src/movement.lisp` (split by collision/transition/preview)
Proposed files:
- `src/movement-core.lisp` — movement integration, intent processing
- `src/movement-collision.lisp` — collision checks, wall maps, bounds
- `src/movement-transition.lisp` — zone transitions, hysteresis, cooldown
- `src/movement-preview.lisp` — preview zones, camera edge checks
- `src/movement.lisp` — glue + shared helpers

### 6) `src/editor.lisp` (split by UI/tools/serialization)
Proposed files:
- `src/editor-core.lisp` — editor state, modes, primary loop
- `src/editor-tools.lisp` — brush/tools, hotkeys
- `src/editor-io.lisp` — loading/saving map data
- `src/editor.lisp` — glue + shared helpers

## Plan Steps

1) **Define module boundaries**
   - For each large file above, confirm the function groupings and dependencies.
   - Keep hot‑path functions together to avoid scattered cross‑file references.

2) **Create new files and move code**
   - Move functions verbatim into the new files.
   - Keep `(in-package #:mmorpg)` at the top of each new file.
   - Minimize changes: no renames, no signature changes.

3) **Update ASDF load order**
   - Modify `mmorpg.asd` to include the new files in a strict, deterministic order.
   - Ensure any function referenced across files is loaded earlier in `:serial t`.

4) **Update docs**
   - Add new docs files for each new `src/*.lisp` file (`docs/<file>.md`) to satisfy `make checkdocs`.
   - Update existing docs (e.g., `docs/net.md`, `docs/movement.md`, etc.) to reference the new split structure.
   - Update `docs/README.md` with the new module map.
   - Update repo `README.md` with the new source layout summary.
   - Update `CLAUDE.md` coding style section with a terse rule‑of‑thumb: “Prefer ~1000 LOC per file when it’s a *natural* split; don’t force it.” Add a brief example (e.g., `net.lisp` → `net-protocol.lisp`, `net-server.lisp`, `net-client.lisp`).

5) **Verify**
   - Run `make checkdocs` after docs changes.
   - Run full `make tests` after code moves.

## Acceptance Criteria
- No behavior changes; tests pass.
- No `checkdocs` failures (all new files documented).
- Each large file reduced to ≤ ~1000 LOC.
- New file names match contents.
