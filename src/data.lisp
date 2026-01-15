;; NOTE: If you change behavior here, update docs/data.md :)
(in-package #:mmorpg)

(defstruct (animation-set (:constructor %make-animation-set))
  ;; Sprite filenames for a directional animation set.
  id dir
  down-idle down-walk down-attack
  up-idle up-walk up-attack
  side-idle side-walk side-attack
  down up side)

(defstruct (item-archetype (:constructor %make-item-archetype))
  ;; Static item data for inventory and loot.
  id name stack-size value)

(defstruct (loot-entry (:constructor %make-loot-entry))
  ;; Single weighted loot entry.
  item-id weight min-count max-count)

(defstruct (loot-table (:constructor %make-loot-table))
  ;; Loot table with weighted entries and roll count.
  id rolls entries)

(defparameter *animation-sets* (make-hash-table :test 'eq))
(defparameter *npc-archetypes* (make-hash-table :test 'eq))
(defparameter *item-archetypes* (make-hash-table :test 'eq))
(defparameter *loot-tables* (make-hash-table :test 'eq))
(defparameter *game-data-loaded-p* nil)

(defparameter *game-data-path*
  (merge-pathnames "data/game-data.lisp"
                   (asdf:system-source-directory :mmorpg)))

(defparameter *tunable-keys*
  '((:player-speed . *player-speed*)
    (:sim-tick-seconds . *sim-tick-seconds*)
    (:sim-max-steps-per-frame . *sim-max-steps-per-frame*)
    (:auto-walk-enabled . *auto-walk-enabled*)
    (:camera-zoom-default . *camera-zoom-default*)
    (:camera-zoom-min . *camera-zoom-min*)
    (:camera-zoom-max . *camera-zoom-max*)
    (:camera-zoom-step . *camera-zoom-step*)
    (:run-speed-mult . *run-speed-mult*)
    (:run-stamina-max . *run-stamina-max*)
    (:player-base-attack . *player-base-attack*)
    (:player-base-strength . *player-base-strength*)
    (:player-base-defense . *player-base-defense*)
    (:player-base-hitpoints . *player-base-hitpoints*)
    (:player-training-mode . *player-training-mode*)
    (:stat-xp-per-level . *stat-xp-per-level*)
    (:stat-max-level . *stat-max-level*)
    (:xp-per-damage . *xp-per-damage*)
    (:combat-hitpoints-xp-multiplier . *combat-hitpoints-xp-multiplier*)
    (:inventory-size . *inventory-size*)
    (:mouse-hold-repeat-seconds . *mouse-hold-repeat-seconds*)
    (:editor-move-speed . *editor-move-speed*)
    (:editor-start-enabled . *editor-start-enabled*)
    (:minimap-width . *minimap-width*)
    (:minimap-height . *minimap-height*)
    (:minimap-padding . *minimap-padding*)
    (:minimap-point-size . *minimap-point-size*)
    (:minimap-preview-edge-tiles . *minimap-preview-edge-tiles*)
    (:sprite-frame-width . *sprite-frame-width*)
    (:sprite-frame-height . *sprite-frame-height*)
    (:sprite-scale . *sprite-scale*)
    (:player-animation-set-id . *player-animation-set-id*)
    (:tileset-path . *tileset-path*)
    (:tile-size . *tile-size*)
    (:tile-scale . *tile-scale*)
    (:tileset-columns . *tileset-columns*)
    (:zone-path . *zone-path*)
    (:zone-root . *zone-root*)
    (:zone-default-width . *zone-default-width*)
    (:zone-default-height . *zone-default-height*)
    (:zone-default-chunk-size . *zone-default-chunk-size*)
    (:world-graph-path . *world-graph-path*)
    (:zone-loading-seconds . *zone-loading-seconds*)
    (:editor-tileset-paths . *editor-tileset-paths*)
    (:editor-tileset-root . *editor-tileset-root*)
    (:editor-tileset-preview-padding . *editor-tileset-preview-padding*)
    (:editor-tileset-preview-max-width . *editor-tileset-preview-max-width*)
    (:editor-tileset-preview-max-height . *editor-tileset-preview-max-height*)
    (:editor-export-path . *editor-export-path*)
    (:editor-tile-layer-id . *editor-tile-layer-id*)
    (:editor-collision-layer-id . *editor-collision-layer-id*)
    (:editor-object-layer-id . *editor-object-layer-id*)
    (:music-volume-steps . *music-volume-steps*)
    (:music-default-volume-level . *music-default-volume-level*)
    (:floor-tile-index . *floor-tile-index*)
    (:wall-map-width . *wall-map-width*)
    (:wall-map-height . *wall-map-height*)
    (:wall-origin-x . *wall-origin-x*)
    (:wall-origin-y . *wall-origin-y*)
    (:wall-seed . *wall-seed*)
    (:player-collision-scale . *player-collision-scale*)
    (:target-epsilon . *target-epsilon*)
    (:collision-edge-epsilon . *collision-edge-epsilon*)
    (:npc-collision-scale . *npc-collision-scale*)
    (:npc-max-hits . *npc-max-hits*)
    (:npc-walk-speed . *npc-walk-speed*)
    (:npc-flee-speed-mult . *npc-flee-speed-mult*)
    (:npc-attack-range-tiles . *npc-attack-range-tiles*)
    (:npc-attack-cooldown . *npc-attack-cooldown*)
    (:npc-attack-damage . *npc-attack-damage*)
    (:npc-home-radius-tiles . *npc-home-radius-tiles*)
    (:npc-wander-interval . *npc-wander-interval*)
    (:npc-wander-arrive-distance . *npc-wander-arrive-distance*)
    (:npc-count . *npc-count*)
    (:npc-spawn-columns . *npc-spawn-columns*)
    (:npc-spawn-gap-tiles . *npc-spawn-gap-tiles*)
    (:npc-default-archetype-id . *npc-default-archetype-id*)
    (:npc-spawn-ids . *npc-spawn-ids*)
    (:npc-default-loot-table-id . *npc-default-loot-table-id*)
    (:attack-hitbox-scale . *attack-hitbox-scale*)
    (:blood-frame-count . *blood-frame-count*)
    (:blood-frame-time . *blood-frame-time*)
    (:health-bar-height . *health-bar-height*)
    (:health-bar-offset . *health-bar-offset*)
    (:idle-frame-count . *idle-frame-count*)
    (:walk-frame-count . *walk-frame-count*)
    (:attack-frame-count . *attack-frame-count*)
    (:idle-frame-time . *idle-frame-time*)
    (:walk-frame-time . *walk-frame-time*)
    (:attack-frame-time . *attack-frame-time*)))

(defun item-archetype-from-plist (id plist)
  ;; Build an item-archetype from plist values.
  (let ((name (or (getf plist :name)
                  (string-capitalize (string id))))
        (stack-size (getf plist :stack-size 1))
        (value (getf plist :value 0)))
    (%make-item-archetype :id id
                          :name name
                          :stack-size (max 1 stack-size)
                          :value value)))

(defun register-item-archetype (id item)
  ;; Store ITEM under ID.
  (setf (gethash id *item-archetypes*) item))

(defun find-item-archetype (id)
  ;; Lookup item archetype by ID.
  (gethash id *item-archetypes*))

(defun loot-entry-from-spec (spec)
  ;; Parse a loot entry spec of (item-id weight min max).
  (destructuring-bind (item-id weight &optional (min-count 1) (max-count 1))
      spec
    (%make-loot-entry :item-id item-id
                      :weight (max 0 weight)
                      :min-count (max 0 min-count)
                      :max-count (max min-count max-count))))

(defun loot-table-from-plist (id plist)
  ;; Build a loot-table from plist values.
  (let* ((rolls (getf plist :rolls 1))
         (entries (getf plist :entries))
         (parsed (loop :for entry :in entries
                       :collect (loot-entry-from-spec entry))))
    (%make-loot-table :id id
                      :rolls (max 0 rolls)
                      :entries parsed)))

(defun register-loot-table (id table)
  ;; Store TABLE under ID.
  (setf (gethash id *loot-tables*) table))

(defun find-loot-table (id)
  ;; Lookup loot table by ID.
  (gethash id *loot-tables*))

(defun normalize-pairs (data)
  ;; Normalize either a plist or list of pairs into a list of (key value).
  (cond
    ((null data) nil)
    ((and (listp data) (keywordp (first data)))
     (loop :for (key value) :on data :by #'cddr
           :collect (list key value)))
    (t data)))

(defun plist-form-p (form)
  ;; Return true when FORM looks like a keyword plist.
  (and (listp form)
       (loop :for (key _value) :on form :by #'cddr
             :always (keywordp key))))

(defun data-section-header-p (form)
  ;; Return true when FORM is a keyword section header.
  (and (symbolp form) (keywordp form)))

(defun data-section-entry-p (form)
  ;; Return true when FORM looks like (id plist).
  (and (listp form)
       (keywordp (first form))
       (listp (second form))))

(defun parse-game-data-forms (forms)
  ;; Merge a list of FORMS into a single plist of data sections.
  (let ((data nil)
        (section nil)
        (section-items nil))
    (labels ((flush-section ()
               (when section
                 (setf (getf data section) (nreverse section-items))
                 (setf section-items nil))))
      (dolist (form forms)
        (cond
          ((data-section-header-p form)
           (flush-section)
           (setf section form))
          ((and section (listp form))
           (if (data-section-entry-p form)
               (push form section-items)
               (error "Invalid entry under ~a: ~s" section form)))
          ((plist-form-p form)
           (dolist (pair (normalize-pairs form))
             (setf (getf data (first pair)) (second pair))))
          ((and (listp form) (keywordp (first form)))
           (setf (getf data (first form)) (rest form)))
          (t nil)))
      (flush-section)
      data)))

(defun read-game-data (path)
  ;; Read one or more forms from PATH without evaluation.
  (when (and path (probe-file path))
    (with-open-file (in path :direction :input)
      (with-standard-io-syntax
        (let ((*read-eval* nil)
              (forms nil))
          (loop :for form = (read in nil :eof)
                :until (eq form :eof)
                :do (push form forms))
          (parse-game-data-forms (nreverse forms)))))))

(defun apply-tunables (tunables)
  ;; Apply tunable overrides from data to known config variables.
  (dolist (pair (normalize-pairs tunables))
    (let* ((key (first pair))
           (value (second pair))
           (symbol (cdr (assoc key *tunable-keys*))))
      (when symbol
        (set symbol value)))))

(defun animation-set-value (set key)
  ;; Fetch a value from SET for the given KEY.
  (ecase key
    (:dir (animation-set-dir set))
    (:down-idle (animation-set-down-idle set))
    (:down-walk (animation-set-down-walk set))
    (:down-attack (animation-set-down-attack set))
    (:up-idle (animation-set-up-idle set))
    (:up-walk (animation-set-up-walk set))
    (:up-attack (animation-set-up-attack set))
    (:side-idle (animation-set-side-idle set))
    (:side-walk (animation-set-side-walk set))
    (:side-attack (animation-set-side-attack set))
    (:down (animation-set-down set))
    (:up (animation-set-up set))
    (:side (animation-set-side set))))

(defun merge-animation-sets (base override)
  ;; Merge OVERRIDE into BASE, preferring non-nil override values.
  (%make-animation-set
   :id (or (animation-set-id override) (animation-set-id base))
   :dir (or (animation-set-dir override) (animation-set-dir base))
   :down-idle (or (animation-set-down-idle override)
                  (animation-set-down-idle base))
   :down-walk (or (animation-set-down-walk override)
                  (animation-set-down-walk base))
   :down-attack (or (animation-set-down-attack override)
                    (animation-set-down-attack base))
   :up-idle (or (animation-set-up-idle override)
                (animation-set-up-idle base))
   :up-walk (or (animation-set-up-walk override)
                (animation-set-up-walk base))
   :up-attack (or (animation-set-up-attack override)
                  (animation-set-up-attack base))
   :side-idle (or (animation-set-side-idle override)
                  (animation-set-side-idle base))
   :side-walk (or (animation-set-side-walk override)
                  (animation-set-side-walk base))
   :side-attack (or (animation-set-side-attack override)
                    (animation-set-side-attack base))
   :down (or (animation-set-down override) (animation-set-down base))
   :up (or (animation-set-up override) (animation-set-up base))
   :side (or (animation-set-side override) (animation-set-side base))))

(defun register-animation-set (set)
  ;; Store SET in the animation registry, merging with existing if needed.
  (let* ((id (animation-set-id set))
         (existing (gethash id *animation-sets*)))
    (setf (gethash id *animation-sets*)
          (if existing
              (merge-animation-sets existing set)
              set))))

(defun animation-set-from-plist (id plist)
  ;; Build an animation set from a plist.
  (%make-animation-set
   :id id
   :dir (getf plist :dir)
   :down-idle (getf plist :down-idle)
   :down-walk (getf plist :down-walk)
   :down-attack (getf plist :down-attack)
   :up-idle (getf plist :up-idle)
   :up-walk (getf plist :up-walk)
   :up-attack (getf plist :up-attack)
   :side-idle (getf plist :side-idle)
   :side-walk (getf plist :side-walk)
   :side-attack (getf plist :side-attack)
   :down (getf plist :down)
   :up (getf plist :up)
   :side (getf plist :side)))

(defun register-default-animation-sets ()
  ;; Ensure baseline animation sets are available.
  (register-animation-set
   (%make-animation-set
    :id :player
    :dir *player-sprite-dir*
    :down-idle "D_Idle.png"
    :down-walk "D_Walk.png"
    :down-attack "D_Attack.png"
    :up-idle "U_Idle.png"
    :up-walk "U_Walk.png"
    :up-attack "U_Attack.png"
    :side-idle "S_Idle.png"
    :side-walk "S_Walk.png"
    :side-attack "S_Attack.png"))
  (register-animation-set
   (%make-animation-set
    :id :npc
    :dir *npc-sprite-dir*
    :down-idle "D_Idle.png"
    :up-idle "U_Idle.png"
    :side-idle "S_Idle.png"))
  (register-animation-set
   (%make-animation-set
    :id :blood
    :dir *blood-sprite-dir*
    :down "D_Blood.png"
    :up "U_Blood.png"
    :side "S_Blood.png")))

(defun animation-path (set key)
  ;; Build the path for a specific animation frame entry.
  (let ((dir (animation-set-dir set))
        (file (animation-set-value set key)))
    (when (and dir file)
      (format nil "~a/~a" dir file))))

(defun get-animation-set (id &optional fallback-id)
  ;; Retrieve animation set by ID, falling back to defaults or fallback ID.
  (or (gethash id *animation-sets*)
      (and fallback-id (gethash fallback-id *animation-sets*))
      (progn
        (register-default-animation-sets)
        (or (gethash id *animation-sets*)
            (and fallback-id (gethash fallback-id *animation-sets*)))))) 

(defun make-npc-archetype-from-plist (plist)
  ;; Build an npc-archetype instance from plist values.
  (labels ((getf-or (key fallback)
             (let ((value (getf plist key :missing)))
               (if (eq value :missing) fallback value))))
    (make-instance 'npc-archetype
                   :name (getf-or :name "NPC")
                   :max-hits (getf-or :max-hits *npc-max-hits*)
                   :attack-level (getf-or :attack-level 1)
                   :strength-level (getf-or :strength-level 1)
                   :defense-level (getf-or :defense-level 1)
                   :hitpoints-level (getf-or :hitpoints-level
                                             (getf-or :max-hits *npc-max-hits*))
                   :combat-xp (getf-or :combat-xp 0)
                   :loot-table-id (getf-or :loot-table-id *npc-default-loot-table-id*)
                   :move-speed (getf-or :move-speed *npc-walk-speed*)
                   :attack-range-tiles (getf-or :attack-range-tiles *npc-attack-range-tiles*)
                   :attack-cooldown (getf-or :attack-cooldown *npc-attack-cooldown*)
                   :attack-damage (getf-or :attack-damage *npc-attack-damage*)
                   :home-radius-tiles (getf-or :home-radius-tiles *npc-home-radius-tiles*)
                   :wander-interval (getf-or :wander-interval *npc-wander-interval*)
                   :flee-speed-mult (getf-or :flee-speed-mult *npc-flee-speed-mult*)
                   :animation-set-id (getf-or :animation-set-id :npc)
                   :aggro-mode (getf-or :aggro-mode :never)
                   :retaliate (getf-or :retaliate nil)
                   :flee-at-hits (getf-or :flee-at-hits 0)
                   :perception-tiles (getf-or :perception-tiles 0.0))))

(defun register-npc-archetype (id archetype)
  ;; Store ARCHETYPE under ID.
  (setf (gethash id *npc-archetypes*) archetype))

(defun find-npc-archetype (id)
  ;; Lookup archetype by ID.
  (gethash id *npc-archetypes*))

(defun first-npc-archetype ()
  ;; Return the first available archetype from the registry.
  (let ((result nil))
    (maphash (lambda (_key value)
               (declare (ignore _key))
               (unless result
                 (setf result value)))
             *npc-archetypes*)
    result))

(defun default-npc-archetype ()
  ;; Select the default archetype for new NPCs.
  (or (find-npc-archetype *npc-default-archetype-id*)
      (first-npc-archetype)))

(defun npc-archetype-ids ()
  ;; Return a vector of available NPC archetype IDs sorted by name.
  (let ((ids nil))
    (maphash (lambda (id _value)
               (declare (ignore _value))
               (push id ids))
             *npc-archetypes*)
    (when ids
      (setf ids (sort ids #'string< :key #'symbol-name)))
    (if ids
        (coerce ids 'vector)
        (make-array 0))))

(defun npc-animation-set-ids ()
  ;; Return the unique animation set IDs referenced by NPC archetypes.
  (let ((ids nil))
    (maphash (lambda (_key archetype)
               (declare (ignore _key))
               (let ((set-id (npc-archetype-animation-set-id archetype)))
                 (when set-id
                   (pushnew set-id ids))))
             *npc-archetypes*)
    (pushnew :npc ids)
    (nreverse ids)))

(defun ensure-default-npc-archetype ()
  ;; Install a fallback archetype when none are loaded.
  (when (zerop (hash-table-count *npc-archetypes*))
    (register-npc-archetype
     :default
     (make-instance 'npc-archetype
                    :name "Default NPC"
                    :max-hits *npc-max-hits*
                    :attack-level 1
                    :strength-level 1
                    :defense-level 1
                    :hitpoints-level *npc-max-hits*
                    :combat-xp 0
                    :loot-table-id *npc-default-loot-table-id*
                    :move-speed *npc-walk-speed*
                    :attack-range-tiles *npc-attack-range-tiles*
                    :attack-cooldown *npc-attack-cooldown*
                    :attack-damage *npc-attack-damage*
                    :home-radius-tiles *npc-home-radius-tiles*
                    :wander-interval *npc-wander-interval*
                    :flee-speed-mult *npc-flee-speed-mult*
                    :aggro-mode :never
                    :retaliate nil
                    :flee-at-hits 0
                    :perception-tiles 0.0))))

(defun load-animation-sets (specs)
  ;; Load animation sets from data specs.
  (dolist (pair (normalize-pairs specs))
    (let ((id (first pair))
          (plist (second pair)))
      (when id
        (register-animation-set (animation-set-from-plist id plist))))))

(defun load-npc-archetypes (specs)
  ;; Load NPC archetypes from data specs.
  (dolist (pair (normalize-pairs specs))
    (let ((id (first pair))
          (plist (second pair)))
      (when id
        (register-npc-archetype id (make-npc-archetype-from-plist plist))))))

(defun load-items (specs)
  ;; Load item archetypes from data specs.
  (dolist (pair (normalize-pairs specs))
    (let ((id (first pair))
          (plist (second pair)))
      (when id
        (register-item-archetype id (item-archetype-from-plist id plist))))))

(defun load-loot-tables (specs)
  ;; Load loot tables from data specs.
  (dolist (pair (normalize-pairs specs))
    (let ((id (first pair))
          (plist (second pair)))
      (when id
        (register-loot-table id (loot-table-from-plist id plist))))))

(defun ensure-default-items ()
  ;; Install a fallback item when none are loaded.
  (when (zerop (hash-table-count *item-archetypes*))
    (register-item-archetype
     :coins
     (%make-item-archetype :id :coins :name "Coins" :stack-size 9999 :value 1))))

(defun load-game-data (&optional (path *game-data-path*))
  ;; Load tunables, animation sets, and archetypes from disk once.
  (unless *game-data-loaded-p*
    (clrhash *animation-sets*)
    (clrhash *npc-archetypes*)
    (clrhash *item-archetypes*)
    (clrhash *loot-tables*)
    (register-default-animation-sets)
    (let ((data (read-game-data path)))
      (when (and data (listp data) (not (plist-form-p data)))
        (setf data (parse-game-data-forms (list data))))
      (when data
        (apply-tunables (getf data :tunables))
        (load-animation-sets (getf data :animation-sets))
        (load-npc-archetypes (getf data :npc-archetypes))
        (load-items (getf data :items))
        (load-loot-tables (getf data :loot-tables))))
    (ensure-default-npc-archetype)
    (ensure-default-items)
    (setf *game-data-loaded-p* t)))

(defun ensure-game-data ()
  ;; Ensure data is loaded at runtime.
  (unless *game-data-loaded-p*
    (load-game-data)))
