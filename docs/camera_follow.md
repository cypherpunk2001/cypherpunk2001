# Camera Follow (World-Space Rendering)

This project uses a 2D camera to render the world in world coordinates while
keeping the player centered on screen. This lets the world scroll under the
player and allows indefinite travel without hitting screen edges.

## Goals

- World scrolls under the player.
- Player remains near the screen center.
- Rendering stays in world coordinates.
- Movement logic stays unchanged.

## How It Works

In `src/main.lisp`, a `Camera2D` is created with:

- **target**: the player position `(x, y)`
- **offset**: half the screen size (screen center)
- **rotation**: 0.0
- **zoom**: 1.0

Each frame:

1. Player position updates in world space.
2. Camera target updates to follow the player.
3. World rendering happens inside `raylib:with-mode-2d camera`.
4. UI/debug text is drawn afterward in screen space.

## Infinite Travel

The player is no longer clamped to the window bounds, so movement is not
limited by screen size.

To avoid rendering the entire map every frame, the draw loop:

- Computes the visible world bounds.
- Converts those to tile row/column ranges.
- Renders only the tiles in view.

The current floor is a repeating chunk (built once); modulo indexing maps world
tiles to the prebuilt floor array.

## Scaling Path

This camera model scales into future systems:

- **Chunk streaming**: render only visible chunks.
- **Large maps**: same camera logic, more data behind it.
- **Networking**: camera remains client-side, world state can be streamed.
- **Multiple entities**: all entities render in world space; camera stays the same.

## Future Options

- Replace modulo tiling with direct `floor-tile-at` calls to get infinite
  non-repeating floor visuals.
- Add smooth camera interpolation for a softer follow feel.
