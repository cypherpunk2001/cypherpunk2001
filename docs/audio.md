# audio.lisp

Purpose: soundtrack loading, playback, and volume control.

Key responsibilities:
- Load music streams and display labels.
- Handle track switching and volume updates.
- Update playback and auto-advance at end of track.

Key functions:
- `make-audio`, `shutdown-audio`.
- `update-audio`, `audio-advance-track`, `audio-adjust-volume`.

Notes:
- Uses raylib music streams and simple volume steps.
