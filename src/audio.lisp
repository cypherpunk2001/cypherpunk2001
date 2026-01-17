;; NOTE: If you change behavior here, update docs/audio.md :)
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
  (let* ((volume-steps (max 1 *music-volume-steps*))
         (volume-level (clamp *music-default-volume-level* 0 volume-steps))
         (volume-bars (build-volume-bars volume-steps))
         (music-volume (/ volume-level (float volume-steps 1.0)))
         (menu-no-music-label "No music loaded")
         (tracks nil)
         (names nil)
         (labels nil))
    (loop :for index :from 0 :below (length *soundtrack-tracks*)
          :for path = (aref *soundtrack-tracks* index)
          :for display = (if (and (< index (length *soundtrack-display-names*))
                                  (aref *soundtrack-display-names* index))
                             (aref *soundtrack-display-names* index)
                             (basename path))
          :for label = (format nil "Now Playing: ~a" display)
          :do (handler-case
                  (let ((stream (raylib:load-music-stream path)))
                    (push stream tracks)
                    (push display names)
                    (push label labels)
                    (log-verbose "Loaded music track: ~a" path))
                (error (e)
                  (warn "Failed to load music track ~a: ~a" path e)
                  (log-verbose "Music load error for ~a: ~a" path e))))
    (let* ((soundtrack-music (coerce (nreverse tracks) 'vector))
           (soundtrack-names (coerce (nreverse names) 'vector))
           (soundtrack-labels (coerce (nreverse labels) 'vector))
           (soundtrack-count (length soundtrack-music))
           (soundtrack-index 0)
           (current-track-label (if (> soundtrack-count 0)
                                    (aref soundtrack-labels 0)
                                    menu-no-music-label))
           (current-music (if (> soundtrack-count 0)
                              (aref soundtrack-music 0)
                              nil)))
      (when current-music
        (raylib:play-music-stream current-music)
        (raylib:set-music-volume current-music music-volume))
      (when (zerop soundtrack-count)
        (warn "No music tracks loaded; audio will be silent"))
      (log-verbose "Audio ready: tracks=~d volume=~d/~d"
                   soundtrack-count volume-level volume-steps)
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
                   :music-volume music-volume))))

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
