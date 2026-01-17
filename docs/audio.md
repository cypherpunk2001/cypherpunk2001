# audio.lisp

Purpose
- Music playback and volume control.

Why we do it this way
- Audio is a system like any other: it owns its state and exposes simple
  commands from UI. This keeps the main loop small.

What it does
- Loads music streams on startup, skipping tracks that fail to load.
- Sets the initial volume from config defaults before playback.
- Keeps display labels for the menu.
- Updates the current track and auto-advances when a track ends.

Key functions
- `make-audio` - Load music streams and initialize audio state.
- `shutdown-audio` - Unload music streams stored in audio state.
- `update-audio` - Update streaming music and auto-advance near track end.
- `audio-advance-track` - Switch to next/previous track and restart playback.
- `audio-adjust-volume` - Adjust volume level and apply to current music.
- `build-volume-bars` - Create prebuilt volume bar strings for the menu UI.

Walkthrough: track switching
1) UI requests previous/next track.
2) `audio-advance-track` swaps the active stream.
3) Volume is reapplied to the new stream.

Design note
- Precomputing label strings avoids per-frame allocations in the menu.
