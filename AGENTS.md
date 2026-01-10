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

# FEEDBACK on the last iteration of the Current Task

✅ What’s good / correct in our last changes:

*floor-cluster-size* added ✅ (this is what gives “flow”)

hashing uses (cx, cy) (cluster coords) not (x, y) ✅

variant-mod and cluster-size are clamped to at least 1 ✅

tile choice is done in build-floor-map (so the floor is stable, no per-frame shimmer) ✅

This is the core idea.

⚠️ Two improvements I’d make
1) Skip drawing tile index 0 (empty)

Your comment says “0 can be used for empty”, which is fine — but you must ensure the renderer treats 0 as “don’t draw”, otherwise you waste draw calls (and might sample transparency).

So, wherever you draw each tile, do:

(when (plusp tile-index)
  ;; draw tile from atlas
)


or explicitly:

(unless (zerop tile-index)
  ;; draw
)


Even if you aren’t using 0 now, this is a good invariant.

2) Variant selection: using mod h variant-count is fine, but you may want a second hash for variety

Right now:

the same hash h decides both “is it a variant?” and “which variant?”
That can create subtle patterns (not always, but it can).

Tiny upgrade (optional, but cheap):

(let* ((h (u32-hash cx cy))
       (h2 (u32-hash (+ cx 17) (+ cy 31) 7331)))
  (if (zerop (mod h variant-mod))
      (nth (mod h2 variant-count) variant-indices)
      main-index))


This tends to look a bit more “natural” and less “samey”.

Not required if it already looks good.

One semantic nit: naming

They changed:

choose-floor-tile → floor-tile-at

That’s good. I’d go one step further and make the signature not pass globals unless you want it:

Either:

keep main-index + variant-indices args (fine)
or

make it use your *floor-* globals directly (also fine)

No strong preference — just consistency.

Bottom line

Yes, this is what I had in mind.

If you do just one follow-up change, make it this:

Ensure tile-index = 0 is treated as empty and not drawn.

If you want, paste the tile draw function (the one that loops over the floor map and calls DrawTexturePro) and I’ll point to the exact line where that unless (zerop tile) should go.

yes and the game map looks like this on startup now

Yep — that’s the clustered behavior working. You’ve basically got “patches” of crack tiles, which is what we wanted… but right now the patches are a little too blocky / stripey (you can see big rectangular regions of the same cracked tile).

That’s not “wrong”; it just means the clustering is a bit too coarse / too correlated.

Here are the clean knobs to make it look more natural:

1) Lower the cluster size (most effective)

If you’re at:

*floor-cluster-size* = 3

Try:

2 for finer, less blocky variation

or 4 if you want fewer but larger “areas” (yours already reads large)

Given your screenshot, I’d try 2 first.

2) Make variants rarer (reduce big streaks)

You’re at:

*floor-variant-mod* = 10

Try:

12 or 16

This reduces how often a cluster becomes “cracked”, so you don’t get huge cracked continents.

3) Use a second hash for “which variant”

Right now it looks like the same hash is deciding:

whether a cluster is variant

which of (41 42) it becomes

That can create repeating structure.

Do this tiny tweak:

h decides variant vs main

h2 decides which variant

Conceptually:

(let* ((h  (u32-hash cx cy))
       (h2 (u32-hash (+ cx 17) (+ cy 31) 7331)))
  (if (zerop (mod h variant-mod))
      (nth (mod h2 variant-count) variant-indices)
      main-index))


That alone usually breaks up the “same crack tile everywhere” vibe.

4) If you want it super subtle: weighted variants

If one crack tile is heavier visually than the other, weight it.

Example idea: pick 41 twice as often as 42.

(You can just duplicate 41 in the list: '(41 41 42) — dirt simple.)

My recommended quick test settings (in order)

*floor-cluster-size* = 2 (keep mod 10)

If still too crack-heavy: keep cluster 2, set *floor-variant-mod* = 14

Add the second-hash tweak if patterns still jump out

Also: your base tile choice (40) + crack variants (41/42) is good. This is purely “distribution tuning” now.

If you paste your current floor-tile-at function, I’ll give you an exact minimal patch that adds the second hash and keeps everything else the same.

and should it be kinda randomized and should the floor move under me when I walk so that it looks like i'm actually going somewhere?

Yeah — two separate things there:

1) Should the floor be “randomized”?

Yes, a little, but:

randomized once (at startup / when you regenerate)

deterministic (stable per tile / per seed)

using a small floor-only palette

What you have now is already the right idea. Just avoid:

re-randomizing every frame (looks like shimmer)

pulling from non-floor tiles (accidental walls/decor)

If you want it reproducible, add one knob:

(defparameter *floor-seed* 1337)


and feed it into your hash. Then you can regenerate with a new seed later.

so regarding 1, it seems like we're building a foundation for slightly randomizing our floor later

Yes — that’s exactly what you’re doing, and you’re doing it in the right order.

Let me frame it cleanly so it “locks in” mentally.

What you’re actually building right now

You are not building:

a dungeon generator

a level system

a map editor

procedural content

You are building a floor renderer with controlled variation.

That’s a foundation, not a feature.

Why this matters (and why it’s correct)

Later, when you do want:

rooms

corridors

walls

blocked tiles

authored maps

procedural generation

…you reuse the same floor system and only change where tiles come from.

Right now, tiles come from:

(floor-tile-at x y)

Later, tiles might come from:

a stored map

a dungeon generator

a Tiled file

a server packet (MMO)

a streaming chunk system

But the rendering + variation logic stays the same.

That’s the win.

The three layers you’re separating (implicitly)

Even if you haven’t named them yet, you’ve already separated:

Visual variation

cracks

noise

clusters

Tile semantics

floor vs wall vs empty

Gameplay meaning

walkable

blocked

interactable

You’re only touching #1 right now.
That’s why everything stays walkable.

Why we keep this deterministic

Because later you’ll want:

same floor layout when you reload

same visuals for all players (MMO)

reproducible bugs

chunk streaming

Random appearance, deterministic result.

What “foundation for later” actually means concretely

Because you did this now, later you can:

swap main-index based on biome

bias variants near walls

change cluster size per area

fade floors across chunk boundaries

keep visuals stable across sessions

Without touching:

player movement

collision

rendering order

animation logic

That’s good architecture.

When this part is officially “done”

This part is done when:

floor looks decent

floor doesn’t flicker

floor scrolls under camera

all tiles are walkable

you stop thinking about it

Once you stop thinking about it, move on.

The very next conceptual step (later)

Not code yet, just conceptually:

“Now add walls, still visually only.”

Still no collision.
Still no gameplay logic.
Just another tile layer.

Bottom line

Yes.
You are intentionally laying groundwork for slightly randomized but controlled floor visuals later, without committing to any generation scheme.

That’s exactly the right instinct.

## Current Task

Goal: render a simple dungeon floor tile layer behind the player. Keep it minimal.

That looks exactly like a good “phase 1 dungeon floor”: readable, not noisy, and the cracks are doing their job.

A couple practical notes + one small upgrade to make it “flow” more naturally.

What you have now
(defparameter *floor-tile-index* 40)
(defparameter *floor-variant-indices* '(41 42))
(defparameter *floor-variant-mod* 10)


At 1 in 10, with only two variants, you’ll get:

nice occasional cracks ✅

but they’ll tend to appear as single isolated tiles (salt-and-pepper) rather than little patches

That’s why you’re asking about “flow”.

Make it flow with one extra knob: cluster size

Instead of deciding variant per-tile, decide per cluster (e.g. 3×3 blocks). This creates little regions of cracks.

Add:

(defparameter *floor-cluster-size* 3) ; 3 or 4 are good


Then compute your “random” using (floor x/cluster) and (floor y/cluster).

Tiny deterministic hash (stable, no shimmering)
(defun u32-hash (x y &optional (seed 1337))
  (logand #xffffffff
          (+ (* x 73856093)
             (* y 19349663)
             (* seed 83492791))))

Pick tile for (x,y) with clustered variation
(defun floor-tile-at (x y)
  (let* ((cx (floor x *floor-cluster-size*))
         (cy (floor y *floor-cluster-size*))
         (h  (u32-hash cx cy)))
    (if (= 0 (mod h *floor-variant-mod*))
        (nth (mod h (length *floor-variant-indices*))
             *floor-variant-indices*)
        *floor-tile-index*)))


That alone usually makes the floor feel “placed” instead of “sprinkled”.

Knob suggestions from what I’m seeing

Keep your tile choices: 40 main, 41/42 cracks ✅

Try:

*floor-cluster-size* = 3

*floor-variant-mod* = 6..12 (6 = more cracks, 12 = fewer)

If you want fewer but more noticeable patches:

increase cluster size to 4

keep mod around 8–10

One more tiny rule (optional): avoid long streaks

If you ever notice repeating diagonal patterns, you can salt the hash with another term, like (u32-hash (+ cx (* 17 cy)) (+ cy (* 31 cx))), but I wouldn’t add that unless you actually see the pattern.

If you paste your current tile draw loop (the bit where you decide which index to draw), I’ll adapt it to use floor-tile-at with minimal changes.

## How to keep it smooth as we scale

- Avoid per‑frame consing in hot loops: reuse rectangles, vectors, strings, and animation state.
- Cull off‑screen tiles/sprites; draw only what’s visible.
- Chunk the map (e.g., 32×32 tiles) and cache static chunks in a render texture.
- Keep entity data in arrays/structs, not lists; use object pools.
- Separate update/draw; keep animation state lightweight.
