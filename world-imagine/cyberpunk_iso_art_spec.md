# CYBERPUNK ISO MMO -- ART SPEC (FOR PIXELLAB.AI AGENT)

## 1. GAME OVERVIEW

We are building a 2D isometric MMO inspired by the atmosphere of Diablo
II, but set in a dark cyberpunk 2001-era city environment.

This is NOT 3D. This is NOT camera-rotatable. This is fixed-angle 2D
isometric sprite rendering.

The goal is: - Dark urban atmosphere - Strong readability for
multiplayer density - Gritty but not noisy - Clear silhouettes at
runtime resolution

All assets must downscale cleanly.

------------------------------------------------------------------------

## 2. PERSPECTIVE RULES (CRITICAL)

-   2D Isometric (approx 45° rotated grid)
-   Fixed camera
-   No perspective distortion
-   All ground tiles are diamond-shaped in runtime view
-   Objects are drawn from consistent isometric angle
-   No dynamic light direction changes in sprite shading

Shading must assume a consistent light direction (top-left or top-right,
choose one and stay consistent).

------------------------------------------------------------------------

## 3. MASTER → RUNTIME PIPELINE

All assets generated at:

-   256x256 MASTER resolution

Runtime export target:

-   Characters ≈ 64--80px tall
-   Ground tiles = 64x32 diamond footprint (2:1 ratio)
-   Props scaled proportionally to tile footprint

All designs must remain readable when downscaled 4x.

Avoid micro-detail that will vanish at runtime.

------------------------------------------------------------------------

## 4. STYLE LANGUAGE

### Mood

-   Dark
-   Gritty
-   Early 2000s cyberpunk
-   Industrial decay
-   Subtle neon accents

### Palette

-   Cool ambient tones (blue/green/grayscale)
-   Warm sodium street lamps
-   Occasional neon magenta/cyan highlights
-   Muted saturation overall
-   High contrast silhouettes

### Materials

-   Wet asphalt
-   Cracked concrete
-   Rusted metal
-   Faded paint
-   Old CRT signage glow
-   Industrial signage, pipes, cables

Avoid: - Cartoon proportions - Overly glossy sci-fi - Hyper-clean
futuristic look

------------------------------------------------------------------------

## 5. CHARACTER DESIGN RULES

-   Semi-realistic proportions
-   No chibi exaggeration
-   Strong silhouette per class
-   Clear head/torso separation
-   Clean edge definition for readability
-   Slight rim-light effect for nighttime readability

Animation Requirements (minimum viable): - 8 directions (preferred) OR 4
directions (fallback) - Idle (4 frames) - Walk (6--8 frames)

Clothing: - Tactical, hacker, mercenary, industrial worker aesthetic -
Layered jackets, boots, gloves, visors - Subtle cyber augment hints

------------------------------------------------------------------------

## 6. ENVIRONMENT TILE RULES

Ground Tiles: - 64x32 diamond footprint (runtime) - Asphalt - Sidewalk
concrete - Street markings - Sewer grates - Oil stains - Subtle texture
variation

Edge Rules: - Sidewalk edges must align cleanly - Road-to-sidewalk
transitions must tile seamlessly - No perspective mismatch

------------------------------------------------------------------------

## 7. PROPS -- ZONE 1 STARTER KIT

Generate coherent set:

Street Set: - Street lamp (warm light source) - Neon sign (vertical and
horizontal versions) - Utility pole - Trash bags - Dumpster - Traffic
barrier - Industrial door - Metal fence section - Crates (metal +
plastic)

Lighting Props: - Lamppost with flame/light core - Neon sign with glow
core - Vending machine with internal glow

These must look cohesive as one district.

------------------------------------------------------------------------

## 8. LIGHTING INTENT (IMPORTANT FOR STYLE)

Runtime lighting system will use:

-   Global dark overlay at night
-   Radial glow sprites for lamps/neon
-   No physically accurate shadows initially

Therefore:

Sprites must not rely on dramatic baked shadow direction. Keep lighting
baked softly and generally neutral.

Glow cores (for lamps/neon) should be separable from base prop if
possible.

------------------------------------------------------------------------

## 9. READABILITY RULE (MMO CRITICAL)

At runtime there may be many players visible.

Assets must:

-   Read clearly at small size
-   Avoid noisy texture clutter
-   Use strong shape contrast
-   Avoid low-contrast midtone mush

Ground must be darker and less saturated than characters.

------------------------------------------------------------------------

## 10. CONSISTENCY MANDATE

All assets generated in batches must:

-   Share identical perspective
-   Share identical light direction
-   Share identical color grading
-   Share consistent edge softness

If generating multiple assets, treat them as part of the same district.

No style drift.

------------------------------------------------------------------------

END OF ART SPEC
