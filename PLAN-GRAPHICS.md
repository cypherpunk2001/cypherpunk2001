# PLAN-GRAPHICS.md — Locked Engine Implementation Plan

This plan is implementation-only (Claude work), aligned with `GRAPHICS.md` and the current codebase.
No creative/art-direction decisions are included here.

Status: LOCKED

---

## Locked Decisions

1. Virtual resolution is fixed at `640x360`.
2. Windowed mode uses scale `2x` (`1280x720`).
3. Fullscreen uses scale `floor(display_height / 360)`, minimum `1`.
4. All gameplay rendering and login rendering use the same virtual RenderTexture presentation path.
5. `current-screen-width` and `current-screen-height` represent virtual dimensions only (`640`, `360`).
6. New helpers provide real display dimensions (`display-screen-width`, `display-screen-height`).
7. Mouse/input hit-testing uses virtual mouse coordinates everywhere.
8. Engine animation direction support remains current 3-direction + horizontal flip.
9. 8-direction engine support is explicitly out of scope for this plan.

---

## Codebase Constraints (Validated)

1. Gameplay draw path is `draw-game` in `src/rendering.lisp`.
2. Login draw path is separate in `src/net-client.lisp` and currently draws directly via `draw-login-screen` from `src/ui.lisp`.
3. `current-screen-width/height` are used widely in rendering/UI/editor code, so changing semantics requires consistent input remapping.
4. Raw `raylib:get-mouse-x/y` reads exist in `src/main.lisp`, `src/input.lisp`, `src/ui.lisp`, `src/rendering-ui.lisp`, and `src/editor-tools.lisp`.
5. Runtime tunables come from `data/game-data.lisp` via `apply-tunables` in `src/data.lisp`; config file defaults alone are not authoritative.
6. Render chunk cache pre-rendering (`prepare-game-render-caches`) occurs before drawing and must remain intact.

---

## Phase 1: Virtual Display Model and Helpers

### Objective
Introduce one authoritative virtual-display model used by render, input, and UI.

### Changes
1. Add virtual constants in `src/config-client.lisp`:
   - `*virtual-width* = 640`
   - `*virtual-height* = 360`
2. Add display/query helpers in `src/utils.lisp`:
   - `display-screen-width`
   - `display-screen-height`
   - pure helper to compute integer scale and letterbox offsets from display size
   - pure helper to convert display mouse coords to virtual coords
3. Change `current-screen-width` and `current-screen-height` in `src/utils.lisp` to always return virtual dimensions.
4. Keep camera offset virtual and constant:
   - update `make-camera` in `src/input.lisp` to `(320, 180)`
   - update `update-camera-for-window-resize` to keep virtual center, not display center

### Notes
1. This phase must add pure helper functions so unit tests can validate math without GPU dependency.

---

## Phase 2: RenderTexture Lifecycle and Presentation

### Objective
Render to a virtual target, then present with nearest-neighbor integer scaling.

### Changes
1. Extend render state in `src/types.lisp` (render struct) to hold:
   - virtual `RenderTexture2D`
   - current present scale
   - present destination rect (x, y, width, height)
2. Create/refresh/unload virtual render target in `src/rendering-core.lisp`.
3. Initialize virtual render target during game init and destroy on shutdown:
   - initialize from `make-game` in `src/main.lisp`
   - unload from `shutdown-game` in `src/main.lisp`
4. Update `draw-game` in `src/rendering.lisp`:
   - keep `prepare-game-render-caches` before draw
   - draw world/UI into virtual RenderTexture (`begin-texture-mode` / `end-texture-mode`)
   - present RenderTexture to display in `begin-drawing` with point filtering
   - blit source rect uses negative height to handle raylib render-texture Y flip
5. Apply point filtering on the virtual render texture (`TEXTURE_FILTER_POINT`).

### Notes
1. Render chunk cache behavior remains unchanged.
2. Game logic/simulation/networking remain unchanged.

---

## Phase 3: Login Screen Through the Same Virtual Pipeline

### Objective
Eliminate split behavior between login and gameplay rendering.

### Changes
1. In `src/net-client.lisp`, remove direct login `begin-drawing/end-drawing` path.
2. Add login frame presentation through the same virtual target + present step used by gameplay.
3. Keep `draw-login-screen` in `src/ui.lisp` operating in virtual coordinates.

### Why this is required
1. After locking `current-screen-width/height` to virtual values, direct display rendering for login would misalign layout and mouse hit tests.

---

## Phase 4: Input and Mouse Coordinate Unification

### Objective
Guarantee all mouse-driven behavior runs in virtual space.

### Changes
1. Replace raw display mouse usage with virtual-mouse helpers in:
   - `src/main.lisp`
   - `src/input.lisp`
   - `src/ui.lisp`
   - `src/rendering-ui.lisp`
   - `src/editor-tools.lisp`
2. Keep `screen-to-world` math unchanged, but ensure its screen inputs are virtual coordinates.
3. Ensure minimap and context-menu hit tests use virtual coordinates.
4. Clamp converted virtual mouse coords to virtual frame bounds.

### Required callsite coverage
1. NPC/object picking
2. click-to-move
3. minimap click/drag
4. inventory drag/drop hover
5. menu/context hover and clicks
6. login button hover and clicks
7. editor tileset preview and editor world picking

---

## Phase 5: Fullscreen/Resize Event Handling

### Objective
Keep scale and letterbox rect correct after mode changes.

### Changes
1. Introduce a single helper to refresh virtual presentation metrics.
2. Replace all direct fullscreen toggles with wrapper logic that toggles mode then refreshes metrics:
   - `src/main.lisp` F11 handling
   - `src/net-client.lisp` login F11 handling
   - `src/ui.lisp` fullscreen menu action
3. On resize events, refresh presentation metrics instead of changing virtual dimensions.
4. Keep camera offset fixed at virtual center (`320, 180`) after toggles/resizes.

---

## Phase 6: Asset Configuration and Data Wiring (32x32)

### Objective
Switch runtime data to cyberpunk assets and native 32px rendering in virtual space.

### Source of truth
Runtime tunables are loaded from `data/game-data.lisp`; update this file first.

### Changes
1. Update tunables in `data/game-data.lisp`:
   - `:tile-size` -> `32`
   - `:tile-scale` -> `1.0`
   - `:sprite-scale` -> `1.0`
   - `:tileset-path` -> cyberpunk atlas path
   - `:tileset-columns` -> atlas column count
   - `:editor-tileset-root` -> cyberpunk tileset root
   - `:player-animation-set-id` -> cyberpunk player set id
2. Add/update player animation set entry in `data/game-data.lisp` under `:animation-sets`.
3. Keep direction keys in existing 3-direction format (`down/up/side`) for this plan.
4. Align fallback defaults in `src/config.lisp` and `src/config-client.lisp` with the same values.
5. Keep `*tileset-columns*` runtime auto-derivation in `src/rendering-core.lisp`; configured columns must match atlas during transition.

---

## Phase 7: Editor Validation and Targeted Fixes

### Objective
Confirm editor usability at 32px tiles and virtual resolution.

### Checks
1. Tileset preview panel sizing and hit-testing
2. grid snap and paint accuracy
3. collision paint/edit correctness
4. object placement correctness

### Rule
Apply only minimal fixes required by failures found in these checks.

---

## Test and Verification Gates (Mandatory)

### Unit tests to add/update
1. `tests/unit/utils-tests.lisp`:
   - scale/offset computation for 1280x720, 1920x1080, 2560x1440, 3840x2160
   - display->virtual coordinate conversion (including letterbox offsets)
2. `tests/unit/data-tests.lisp`:
   - tunable application for updated graphics keys (`:tile-size`, `:tile-scale`, `:sprite-scale`, `:tileset-path`, `:player-animation-set-id`)
3. Add/update tests for any new pure helpers introduced by this plan.

### Required command sequence
1. `make checkparens`
2. `make ci`
3. `make smoke`
4. `make test-unit`
5. `make checkdocs`
6. `make tests` (final full run)

### Manual verification matrix
1. Windowed `1280x720` (`2x`) crisp output, correct mouse/UI/editor behavior
2. Fullscreen `1920x1080` (`3x`) crisp output
3. Fullscreen `2560x1440` (`4x`) crisp output
4. Fullscreen `3840x2160` (`6x`) crisp output
5. Login screen interaction correct in windowed and fullscreen

---

## Acceptance Criteria (All Required)

1. No blurry bilinear scaling in fullscreen at target resolutions.
2. Camera offset is always virtual center.
3. All UI/input/editor mouse interactions are correct after scaling and letterboxing.
4. Login and gameplay both use the same virtual presentation path.
5. 32x32 tiles and 32x32 sprites render at scale `1.0` in virtual space.
6. Existing chunk cache, zoom behavior, and gameplay logic remain functionally unchanged.
7. All required tests pass.

---

## Explicit Non-Goals (This Plan)

1. 8-direction engine animation support
2. Isometric rendering
3. New gameplay mechanics
4. New networking/persistence behavior
