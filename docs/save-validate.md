# save-validate.lisp

Purpose
- Schema validation and bounds checking for deserialized player data to ensure data integrity and prevent corrupt state from entering the game.

Key responsibilities
- `*player-schema*`: declarative schema definition with field types, required flags, and min/max bounds.
- `*max-player-blob-size*`: size limit to prevent memory exhaustion from malformed data.
- `validate-field-type`: type checking for integer, number, symbol, plist, and string fields.
- `validate-field-bounds`: range validation against min/max constraints.
- Schema-driven validation loop over all defined fields with detailed error reporting.
- 4-outcome validation system: ok (pass), clamp (fix bounds), quarantine (isolate), reject (discard).
- Integration with forensic storage for corrupt blob inspection.
- Validation metric tracking for monitoring data quality in production.

Load order
- Loaded last among save files: `save-serialize` -> `save-deserialize` -> `save-delta` -> `save-edge-strips` -> `save-validate`.
- Depends on `save-serialize` for `*save-format-version*`; used by `db-players` during player load validation.
