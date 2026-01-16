# input.lisp

Purpose
- Translate device input into intent.

Why we do it this way
- Input should not move entities directly. It should only express what the
  player wants to do. This keeps input, AI, and future network input aligned.

What it does
- Converts WASD/arrow keys into a movement intent.
- Handles auto-walk toggles.
- Converts mouse clicks into a target intent (click-to-move).
- Converts minimap clicks into a target intent (minimap click-to-move).
- Emits action intents for running and attacking.
- Updates training mode hotkeys for combat progression.
- Flags the HUD stats cache to refresh when training mode changes.
- Hitpoints auto-train from combat XP and are not a selectable hotkey.
- Left click on NPCs attacks; left click on objects sets a pickup target; left click on ground walks.
- Provides NPC/object/item hit testing plus examine descriptions for context menus.
- Tracks the NPC under the cursor for a top-middle HUD name display.
- Updates camera zoom from the mouse wheel.
- `I` toggles the inventory overlay.

Key functions
- `update-input-direction`, `update-target-from-mouse`, `update-target-from-minimap`,
  `update-input-actions`, `update-training-mode`, `update-ui-hovered-npc`,
  `npc-examine-description`, `object-examine-description`, `item-examine-description`.

Walkthrough: mouse click to target
1) Convert screen coordinates to world coordinates with camera offset/zoom.
2) Write `intent-target-x/y` and mark the target active.
3) Movement system consumes that target later in the frame.

Walkthrough: minimap click to target
1) Check if the click lands inside the minimap rectangle.
2) Convert minimap screen coordinates into world coordinates using a view centered on the player.
3) Write `intent-target-x/y` and mark the target active.

Training hotkeys (default)
- `1` attack, `2` strength, `3` defense, `Z` balanced.

Example flow
```lisp
(update-target-from-mouse player player-intent camera dt mouse-clicked mouse-down)
(update-input-direction player player-intent mouse-clicked)
(update-input-actions player-intent (not mouse-clicked))
```

Design note
- All logic here is input-to-intent. Movement happens elsewhere.
