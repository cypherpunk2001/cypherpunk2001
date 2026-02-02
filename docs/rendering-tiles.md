# rendering-tiles.lisp

Purpose
- Map and tileset rendering with a chunk-based render texture cache for efficient tile drawing and preview zone rendering.

Key responsibilities
- Render chunk cache system: pre-render static tile chunks into render textures to minimize draw calls.
- `*zone-render-caches*`: per-zone cache storage mapping zone IDs to chunk textures.
- Cache instrumentation: track created, re-rendered, evicted, hit, and miss counts per frame.
- `reset-render-cache-stats` / `log-render-cache-stats`: per-frame diagnostic tracking.
- `get-total-cache-size`: monitor total cached chunks across all zones.
- Dirty chunk detection and selective re-rendering when tiles change.
- Chunk eviction for memory management under pressure.
- Preview zone tile rendering for seamless zone boundary display.
- Tileset texture management and atlas rendering.

Load order
- Loaded second among rendering files: `rendering-core` -> `rendering-tiles` -> `rendering-entities` -> `rendering-ui`.
- Depends on `rendering-core` for tile/rectangle helpers; used by main rendering pipeline for map display.
