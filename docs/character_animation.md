# Character Sprite Animation

This project uses sprite-sheet animations for the player character. The setup
is minimal and deterministic, focused on clean animation timing and simple
direction handling.

## Assets and Layout

- Sprite atlas directory: `assets/1 Characters/1`
- Each animation is a separate image:
  - `D_Idle.png`, `D_Walk.png`
  - `U_Idle.png`, `U_Walk.png`
  - `S_Idle.png`, `S_Walk.png`

Each sheet is a horizontal strip of frames.

## Frame Size and Scaling

- Frame size: `32x32` pixels
- Draw scale: `4.0` (so 128x128 on screen)

Frame size and scale are defined in `src/main.lisp`:

- `*sprite-frame-width*`
- `*sprite-frame-height*`
- `*sprite-scale*`

## Directions and States

The animation system uses two dimensions:

- **State**: `:idle` or `:walk`
- **Direction**: `:down`, `:up`, `:side`

Direction selection is derived from movement:

- If `abs(dx) > abs(dy)`, use `:side`
- Else if `dy < 0`, use `:up`
- Else use `:down`

State selection is based on movement magnitude:

- If both `dx` and `dy` are zero, use `:idle`
- Otherwise use `:walk`

## Left/Right Handling

Side movement uses a single side-facing sheet (`S_*`) and mirrors it:

- Moving right: draw side frames flipped horizontally
- Moving left: draw normally

This avoids the need for separate left/right sprites.

## Frame Counts and Timing

Frame counts and per-frame timing are configurable:

- `*idle-frame-count*` (default 4)
- `*walk-frame-count*` (default 6)
- `*idle-frame-time*` (seconds per frame)
- `*walk-frame-time*` (seconds per frame)

Animation advances based on real frame time (`dt`).

## Render Order

- Floor is drawn first.
- Player is drawn last on top of the floor.

## Scaling Path (Why This Works Later)

This animation spec is simple but extensible:

- Add more states (attack, cast, hit) by adding new sheets.
- Add diagonal directions by extending direction selection.
- Swap animation sets based on equipment or class.
- Use the same frame logic for NPCs and mobs.

The core contract is stable: **given state + direction + time, choose the
correct frame in a sprite sheet**.
