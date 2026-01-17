# ui.lisp

Purpose
- Menu and HUD layout plus menu interaction.
- Login/register screen for authentication.

Why we do it this way
- UI state is precomputed to avoid per-frame layout work.
- UI should toggle systems, not implement them.

What it does
- Builds layout constants (panel sizes, button positions, labels).
- Handles menu click actions (logout, unstuck, music, volume, debug, editor mode, fullscreen).
- Provides precomputed stamina labels to avoid consing.
- Tracks a short loading overlay timer for zone transitions.
- Tracks inventory overlay visibility for the `I` hotkey.
- Tracks chat input state (active flag, buffer, prompt, max length) for `T` chat mode.
- Stores minimap layout/colors (including collision marker color) for rendering and input hit-testing.
- Caches combat log lines for the debug overlay plus a lightweight HUD feedback log with per-line fade timers.
- Tracks a right-click context menu (position, options, target type, and Walk/Attack/Follow/Pick up/Examine/Drop labels, including inventory examine).
- Tracks the hovered NPC name for top-middle HUD display.
- Provides inventory grid layout and slot hit-testing for context menus.
- Context menu selection logic supports follow-only layouts when attack is unavailable.
- Displays login/register screen with username input before game starts.

Key functions

**UI Construction:**
- `make-ui` - Build UI layout constants and colors for the menu and HUD.
- `make-stamina-labels` - Precompute stamina HUD strings to avoid per-frame consing.

**Menu Interaction:**
- `update-ui-input` - Handle UI toggle input and click interactions. Returns action keywords.
- `handle-menu-click` - Process menu clicks for logout, unstuck, music, volume, and toggles. Returns `:logout`, `:unstuck`, `:toggle-editor`, or nil.

**HUD Logging:**
- `ui-push-combat-log` - Append text to the UI combat log ring buffer.
- `ui-push-hud-log` - Append text to the HUD feedback ring buffer. Duplicate messages refresh the timer.
- `update-ui-hud-log` - Tick down HUD log message timers.

**Context Menu:**
- `open-context-menu` - Open a context menu anchored at screen coordinates with target info.
- `close-context-menu` - Close any open context menu and reset state.
- `handle-context-menu-click` - Handle a click against the context menu; returns action keyword, `:close`, or nil.
- `context-menu-actions` - Return the ordered list of context menu actions.
- `context-menu-option-count` - Return the number of context menu options.
- `context-menu-action-for-index` - Map a menu index to an action keyword.

**Inventory:**
- `inventory-grid-layout` - Return layout values for the inventory grid (positions, sizes, gaps).
- `inventory-slot-at-screen` - Return the inventory slot index at screen coordinates, or nil.

**Loading Overlay:**
- `ui-trigger-loading` - Ensure the loading overlay stays visible for at least N seconds.
- `update-ui-loading` - Advance the loading overlay timer.

**Login Screen:**
- `update-login-input` - Handle text input for username field on login screen.
- `draw-login-screen` - Draw the login/register screen. Returns `:login`, `:register`, or nil.

Key parameters

| Parameter | Default | Description |
|-----------|---------|-------------|
| `*login-max-username-length*` | `20` | Maximum characters for username input |
| `*login-max-password-length*` | `30` | Maximum characters for password input |

UI state fields (login-related)

| Field | Description |
|-------|-------------|
| `login-active` | Whether the login screen is displayed |
| `auth-complete` | Whether authentication has succeeded |
| `username-buffer` | Current username input text |
| `password-buffer` | Current password input text |
| `auth-error-message` | Error message to display (e.g., "Username taken") |
| `server-selector-index` | Selected server index (for future multi-server support) |

Walkthrough: login flow
1. Client starts with `(ui-login-active ui)` = t.
2. Player types username, clicks Login or Register.
3. `draw-login-screen` returns `:login` or `:register`.
4. Client sends auth message to server via `send-auth-message`.
5. Server responds with `:auth-ok` or `:auth-fail`.
6. On success: `(ui-login-active ui)` = nil, game begins.
7. On failure: `(ui-auth-error-message ui)` set to error text.

Walkthrough: debug toggle
1. Player opens the menu with Escape.
2. Click on the debug checkbox.
3. UI flips `*debug-collision-overlay*` and `*debug-npc-logs*`.
4. Rendering reads the flag and draws the overlay.

Walkthrough: zone loading overlay
1. Movement triggers a zone transition.
2. `ui-trigger-loading` starts a short timer.
3. `update-ui-loading` counts down each frame.
4. Rendering draws "Loading..." while the timer is active.

Walkthrough: unstuck feature
1. Player opens the menu with Escape.
2. Click on the "Unstuck" button (positioned above Logout).
3. UI returns `:unstuck`, which sets `(intent-requested-unstuck intent)` to true.
4. Server's `process-player-unstuck` validates player is actually stuck (can't move in any cardinal direction).
5. If stuck: teleport to a random position within zone bounds (if still stuck, click again).
6. If not stuck: request denied (prevents exploit as free teleport).

Design note
- UI toggles debug overlays without touching the rendering logic directly.
- Editor Mode is exposed as a checkbox, but gameplay systems own the behavior.
- Login screen is modal - blocks game rendering until auth completes.
- MVP: Password field uses username as password (simplification for testing).
