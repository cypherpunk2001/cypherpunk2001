(in-package #:mmorpg)

(defun build-volume-bars (volume-steps)
  ;; Create prebuilt volume bar strings for the menu UI.
  (let ((bars (make-array (1+ volume-steps))))
    (loop :for i :from 0 :to volume-steps
          :do (let ((s (make-string (+ volume-steps 2)
                                    :initial-element #\-)))
                (setf (aref s 0) #\[)
                (setf (aref s (1+ volume-steps)) #\])
                (dotimes (j i)
                  (setf (aref s (1+ j)) #\|))
                (setf (aref bars i) s)))
    bars))

(defun make-audio ()
  ;; Load music streams and initialize audio state.
  (let* ((soundtrack-count (length *soundtrack-tracks*))
         (soundtrack-music (make-array soundtrack-count))
         (soundtrack-names (make-array soundtrack-count))
         (soundtrack-labels (make-array soundtrack-count))
         (soundtrack-index 0)
         (menu-no-music-label "No music loaded")
         (current-track-label menu-no-music-label)
         (volume-steps 10)
         (volume-level volume-steps)
         (volume-bars (build-volume-bars volume-steps))
         (music-volume (/ volume-level (float volume-steps 1.0)))
         (current-music nil))
    (loop :for index :from 0 :below soundtrack-count
          :for path = (aref *soundtrack-tracks* index)
          :for display = (if (and (< index (length *soundtrack-display-names*))
                                  (aref *soundtrack-display-names* index))
                             (aref *soundtrack-display-names* index)
                             (basename path))
          :for label = (format nil "Now Playing: ~a" display)
          :do (setf (aref soundtrack-names index) display
                    (aref soundtrack-labels index) label
                    (aref soundtrack-music index)
                    (raylib:load-music-stream path)))
    (when (> soundtrack-count 0)
      (setf current-music (aref soundtrack-music 0)
            current-track-label (aref soundtrack-labels 0))
      (raylib:play-music-stream current-music)
      (raylib:set-music-volume current-music music-volume))
    (%make-audio :soundtrack-count soundtrack-count
                 :soundtrack-music soundtrack-music
                 :soundtrack-names soundtrack-names
                 :soundtrack-labels soundtrack-labels
                 :soundtrack-index soundtrack-index
                 :current-music current-music
                 :current-track-label current-track-label
                 :volume-steps volume-steps
                 :volume-level volume-level
                 :volume-bars volume-bars
                 :music-volume music-volume)))

(defun shutdown-audio (audio)
  ;; Unload music streams stored in audio state.
  (let ((count (audio-soundtrack-count audio))
        (music (audio-soundtrack-music audio)))
    (loop :for index :from 0 :below count
          :do (raylib:unload-music-stream (aref music index)))))

(defun audio-advance-track (audio step)
  ;; Switch to the next or previous track and restart playback.
  (let ((count (audio-soundtrack-count audio)))
    (when (> count 0)
      (let ((old-music (audio-current-music audio)))
        (setf (audio-soundtrack-index audio)
              (mod (+ (audio-soundtrack-index audio) step) count)
              (audio-current-music audio)
              (aref (audio-soundtrack-music audio)
                    (audio-soundtrack-index audio))
              (audio-current-track-label audio)
              (aref (audio-soundtrack-labels audio)
                    (audio-soundtrack-index audio)))
        (when old-music
          (raylib:stop-music-stream old-music))
        (raylib:play-music-stream (audio-current-music audio))
        (raylib:set-music-volume (audio-current-music audio)
                                 (audio-music-volume audio)))))
  audio)

(defun audio-adjust-volume (audio delta)
  ;; Adjust volume level and apply it to current music.
  (let* ((steps (audio-volume-steps audio))
         (new-level (clamp (+ (audio-volume-level audio) delta) 0 steps))
         (music-volume (/ new-level (float steps 1.0))))
    (setf (audio-volume-level audio) new-level
          (audio-music-volume audio) music-volume)
    (let ((current (audio-current-music audio)))
      (when current
        (raylib:set-music-volume current music-volume))))
  audio)

(defun update-audio (audio)
  ;; Update streaming music and auto-advance near track end.
  (let ((current (audio-current-music audio)))
    (when current
      (raylib:update-music-stream current)
      (let* ((track-length (raylib:get-music-time-length current))
             (track-played (raylib:get-music-time-played current)))
        (when (and (> track-length 0.0)
                   (>= track-played (- track-length 0.05)))
          (audio-advance-track audio 1)))))
  audio)
