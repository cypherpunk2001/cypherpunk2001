No — only the ground footprint is 64×32 (if you choose that classic 2:1 iso tile). Everything else can be different sizes.

Think of 64×32 as the size of one map cell’s diamond on the ground. It’s a coordinate unit for the world, not a rule for every sprite.

What is typically 64×32

Ground tiles (street, sidewalk, dirt, etc.)

Any “flat” floor detail that lives on the ground plane (manholes, decals) can also be 64×32 (or smaller overlays)

What is NOT 64×32
Characters / players

Usually taller, like 64×96, 64×128, etc.

They still “stand” on one tile cell (their feet anchor sits on that cell’s center-ish point)

Props

Can be any size:

crates might be ~64×64

lamppost might be ~64×128

trees might be ~128×192

What matters is: the prop has a footprint (which tile(s) it occupies) and an anchor point (where it touches the ground)

Buildings

Often multi-tile footprints and tall sprites

Common trick: split into:

base (collides, blocks movement)

top (drawn above and can fade when player goes behind)

Inventory/UI icons

Completely separate from world tiles

Often 32×32, 48×48, 64×64 square icons

UI is not isometric-projected at all

The key concept: “footprint vs sprite size”

Every world object has:

Footprint in tiles (1×1, 2×1, 2×2, etc.)

Sprite pixels (any size)

Anchor point (where footprint meets sprite)

That’s how Diablo-style games do it.

Practical defaults (good starting point)
If you choose 64×32 iso ground tiles:
-Characters: ~64×96 (readable) or 64×128 (more detail)
-Props: width often 64, height varies
-Icons: 64×64 (since you’re generating 256 masters → downscale)
