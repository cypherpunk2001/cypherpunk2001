# AGENTS.md

Minimal Common Lisp + raylib project slowly iterating in to a single player rpg.

## Project layout

```
mmorpg/
  AGENTS.md
  mmorpg.asd
  assets/*
    ...
  src/
    package.lisp
    main.lisp
  docs/
    raylib_cheatsheet.md
    claw-raylib-readme.org
```

---

## Current Task

Replace the player placeholder rectangle with an animated sprite.

Use the png files found in './assets/1 Characters/1'

Only **Idle** and **Walk** animations are required at this stage.

Do not introduce combat, camera changes, physics systems, or additional game states.

---

## Player Sprite Contract

### Frame Size
- All character animation frames are **32×32 pixels**

### Layout
- Animations are stored as **single-row horizontal strips**
- Frames are ordered left → right

### Directions
- `U` = Up
- `D` = Down
- `S` = Side (right-facing only)
- Left-facing movement mirrors `S_*` animations horizontally in code

### Supported States (for now)
| State | Frames |
|------|--------|
| Idle | 4 |
| Walk | 6 |

### Naming Convention
- Sprite files follow: `<DIR>_<STATE>.png`
- Examples:
  - `D_Idle.png`
  - `U_Walk.png`
  - `S_Walk.png`

---

## Animation Behavior

- Idle animation plays when player velocity is `(0, 0)`
- Walk animation plays when player velocity is non-zero
- Direction selection:
  - If `abs(vx) > abs(vy)` → Side
  - Else if `vy < 0` → Up
  - Else → Down

---

## Rendering Rules

- Player position is world-space pixel coordinates
- Sprite is drawn **centered** on the player position
- Use raylib texture source rectangles for frame selection
- Horizontal mirroring is used for left-facing movement
- No sprite rotation is used

---

## Constraints

- Do not change movement logic
- Do not modify tilemap or camera behavior
- Do not add new assets or animation states
- Do not refactor unrelated systems

Focus only on replacing the placeholder rectangle with animated Idle/Walk sprites.
