# utils.lisp

Purpose
- Provide small, reusable helpers for math and common game tasks.

Why we do it this way
- Small helpers reduce duplication and make systems easier to test.
- Side-effectful utilities are kept explicit (like verbose logging).

Key helpers
- `log-verbose`: emit standardized verbose logs when `*verbose*` is enabled.
- `log-fatal-error`, `with-fatal-error-log`: attach context and backtraces to fatal errors.
- `clamp`: world bounds and UI limits.
- `normalize-direction`, `normalize-vector`: avoid faster diagonals.
- `screen-to-world`: convert mouse to world coordinates with camera zoom.
- `minimap-view-bounds`: compute a player-centered minimap view box.
- `basename`: derive a short filename label for UI overlays.
- `sanitize-identifier`: turn filenames into keyword-safe IDs.
- `plist-put`: set or add a key in a plist without relying on `setf getf`.
- `relative-path-from-root`: normalize asset paths relative to a root folder.
- `player-direction`, `player-state`, `player-animation-params`: animation logic helpers.
- `u32-hash`: deterministic variation for wall tile selection.
- `vector-pool` helpers: reuse fixed-size vectors for hot serialization paths.
- `current-screen-width`, `current-screen-height`: dynamic window dimensions for resize support.

Zone bounds
- `zone-bounds-zero-origin`: calculate movement bounds for a zero-origin zone wall-map. Moved from movement-core.lisp to fix load-order inversion (world-graph.lisp loads before movement-core.lisp but needs this function).

Retry utilities
- `with-retry-exponential`: retry critical operations with exponential backoff
- `with-retry-linear`: retry network operations with fixed delay
- `exponential-backoff-delay`: calculate backoff timing for retries

Profiling & GC utilities
- `with-timing`: optional timing wrapper controlled by `*profile-enabled*`.
- `profile-summary`, `clear-profile-log`: inspect and reset timing samples.
- `reset-gc-stats`, `log-gc-delta`, `gc-summary`: allocation + GC tracking.
- `*verbose-gc*`: enable per-frame allocation/GC logging.

Example: diagonal movement normalization
```lisp
(multiple-value-bind (dx dy) (normalize-direction 1.0 1.0)
  ;; dx/dy are length 1.0, not sqrt(2.0)
  (values dx dy))
```

Example: retry with exponential backoff (for database operations)
```lisp
;; Retry death save up to 5 times with exponential backoff
(with-retry-exponential (saved (lambda () (db-save-player-immediate player))
                          :max-retries 5
                          :initial-delay 100
                          :max-delay 500
                          :on-final-fail (lambda (e)
                                           (warn "CRITICAL: Save failed after all retries: ~a" e)))
  ;; Body executed only if save succeeded
  (log-verbose "Player saved successfully"))
```

Example: retry with linear delay (for network operations)
```lisp
;; Retry auth message 3 times with 50ms delay between attempts
(with-retry-linear (sent (lambda () (send-net-message socket msg :host host :port port))
                     :max-retries 3
                     :delay 50)
  ;; Body executed only if send succeeded
  sent)
```
