# movement-transition.lisp

Purpose
- Zone transition logic including player zone changes, NPC carry across boundaries, and seamless transition without teleport pop.

Key responsibilities
- `update-running-state`: stamina and sprint state management during transitions.
- `update-player-position`: position integration with collision and zone-aware movement.
- Zone boundary detection and transition triggering when player crosses zone edges.
- NPC carry: move NPCs near boundaries into the new zone during transitions.
- Session zone update and dirty flag marking on zone change.
- Coordinate remapping from source zone space to destination zone space.
- Continuity handling to prevent visual teleport artifacts during transitions.

Load order
- Loaded last among movement files: `movement-core` -> `movement-collision` -> `movement-preview` -> `movement-transition`.
- Depends on all other movement files; integrates zone state, collision, and world graph for complete transition logic.
