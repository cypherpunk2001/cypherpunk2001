# audio.lisp

Purpose
- Music playback and volume control.

Why we do it this way
- Audio is a system like any other: it owns its state and exposes simple
  commands from UI. This keeps the main loop small.

What it does
- Loads music streams on startup.
- Sets the initial volume from config defaults before playback.
- Keeps display labels for the menu.
- Updates the current track and auto-advances when a track ends.

Key functions
- `make-audio`, `shutdown-audio`.
- `update-audio`, `audio-advance-track`, `audio-adjust-volume`.

Walkthrough: track switching
1) UI requests previous/next track.
2) `audio-advance-track` swaps the active stream.
3) Volume is reapplied to the new stream.

Design note
- Precomputing label strings avoids per-frame allocations in the menu.
