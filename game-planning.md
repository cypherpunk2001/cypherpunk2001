# Game Planning Notes: Cypherpunk 2001

## Project Context
- Common Lisp/MMORPG prototype on SBCL + Quicklisp, UDP client/server, intent-based input, compact/delta snapshot streaming, Redis/memory storage abstraction.
- Separation of simulation and rendering, data driven configs, extensive docs/tests.
- Current work: PLAN_code_standards.md, updates to docs, debug investigations (zones, NPCs, edge strips, security).

## Creative Vision
- Name: **Cypherpunk 2001**, 2D cyberpunk MMO with RuneScape-classic map sensibility but unique lore.
  - The player world is reminiscent of classic MMO zones (central city, wilderness, tunnels) without referencing existing names.
  - Wilderness is a cypherpunk-styled area (corrupted datascape, neon ruins) for PvP/”outlaw” feel.
  - Late-game optional time-machine feature: overlay converts visuals to SNES-style Zelda aesthetic, complete with retro palette and sounds.
- Lighting: Dual tile themes (light/dark) per zone or via overlay/shader; consider palette swaps, glow overlays, vignette, and optional day/night transitions.
- Vision includes an optional retro filter, glitchy shaders, neon decals, and city/wilderness faction vibes.

## Systems Ideas
- Factions & Turf: dynamic street groups in city and wilderness, contested zones, data druids/augmented samurai NPCs.
- Reputation tied to central grid vs. wilderness activity, unlocking abilities or story arcs.
- Camera-leash, minimap previews, edge-strip rendering, zone transition fixes already part of architecture.
- Future enhancements: time machine event, optional retro shader/palette, SNES overlay toggled by artifact/quest.

## Next Steps
1. Document creative vision in design notes (done here).
2. Continue plan execution: complete remaining code standards tasks if any; maintain docs alignment.
3. Build optional overlays (light/dark tiles, SNES retheme) as part of art/UX iteration.
4. Tie storytelling to existing mechanics (e.g., time-machine quest reveals retro style, wilderness zone as digital wilds).
