# editor-io.lisp

Purpose
- Loading/saving map data, zone lifecycle management, and file I/O for the editor.

Key responsibilities
- Reset player/NPC positions after zone changes (`editor-reset-game-for-zone`).
- Activate a zone and refresh all editor state (`editor-activate-zone`).
- Load zones from disk (`editor-load-zone`).
- Create new blank zones (`editor-create-zone`).
- Delete zones and switch to next available (`editor-delete-zone`).
- Cycle through zone files (`editor-cycle-zone`).
- Handle zone lifecycle hotkeys F6/F7/F8/F9 (`editor-handle-zone-actions`).
- Export the current zone to disk (`editor-export-zone`).

Load order
- Loaded after `editor-core` and `editor-tools`, before `editor`.
- Depends on core helpers and tool functions from earlier editor files.
