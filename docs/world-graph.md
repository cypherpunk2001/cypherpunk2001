# world-graph.lisp

Purpose
- Define and load zone-to-zone connections for edge-based transitions.

Why we do it this way
- Zones stay focused on tiles/objects/spawns while the world graph encodes travel.
- Edge links scale to large worlds without monolithic map files.
- The format is data-driven and easy to edit by hand.

Data format (world graph)
- A world graph file is a plist stored under `data/`.
- Key: `:edges` is a list of exit specs.
- Each exit spec should include:
  - `:from` zone ID (keyword)
  - `:edge` one of `:north`, `:south`, `:east`, `:west`
  - `:to` zone ID (keyword)
- Optional keys:
  - `:spawn-edge` (defaults to the opposite edge)
  - `:offset` can be `:preserve-x`, `:preserve-y`, or `:none`

Example
```lisp
(:edges
 ((:from :zone-1 :edge :north :to :zone-2 :spawn-edge :south :offset :preserve-x)
  (:from :zone-2 :edge :west :to :zone-3 :spawn-edge :east :offset :preserve-y)))
```

Key functions
- `load-world-graph`: loads edges, builds a zone-id-to-path index, and populates `*known-zone-ids*` for validation; logs counts in verbose mode and falls back to an empty graph when missing.
- `world-graph-exits`: fetches exits for a given zone.
- `world-graph-zone-path`: resolves a target zone ID to a file path.
- `zone-path-for-id`: convenience wrapper to get zone path from world's graph for a zone-id. Returns nil if zone-id not found.
- `zone-path-for-id-exists-p`: returns T if zone-id has a valid path in world's graph. Used for zone validation.

Design note
- Edge links are directional; define both directions if you want two-way travel.
- Zone IDs are resolved to files via `:zone-root`, so file names and `:id` values
  should stay in sync.
- Zone roots are normalized to directory paths before scanning for zone files.
