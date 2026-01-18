;; NOTE: If you change behavior here, update docs/config-server.md :)
(in-package #:mmorpg)

;;;; ========================================================================
;;;; SERVER OPTIONS - Restart Required
;;;; Server must be restarted for these changes to take effect.
;;;; These are read once at server initialization.
;;;; ========================================================================

;;; Network Configuration - Socket/buffer allocated once at startup
(defparameter *net-default-host* "127.0.0.1"
  "Default UDP host for client/server.")
(defparameter *net-default-port* 1337
  "Default UDP port for client/server.")
(defparameter *net-buffer-size* usocket:+max-datagram-packet-size+
  "Max UDP payload size for snapshot messages.")

;;; Delta Compression - See docs/net.md Prong 2
(defparameter *delta-compression-enabled* nil
  "Enable delta compression (Prong 2). When T, server sends only dirty entities
   to synced clients. When NIL, always sends full snapshots (Prong 1 only).
   Toggle via MMORPG_DELTA_COMPRESSION=1 environment variable.")
(defparameter *max-delta-age* 60
  "Max snapshots a client can be behind before forcing full resync.
   At 20 Hz, 60 = 3 seconds of missed packets.")

;;; UDP Fragmentation - See docs/net.md Prong 3
(defparameter *chunk-overhead* 100
  "Reserved bytes for chunk message header.")
(defparameter *max-chunk-payload* (- usocket:+max-datagram-packet-size+ 100)
  "Maximum payload bytes per UDP chunk (buffer size minus header overhead).")
(defparameter *chunk-timeout* 1.0
  "Seconds before discarding incomplete chunk sequences.")

;;; Auth Encryption - X25519 + ChaCha20-Poly1305
(defparameter *auth-encryption-enabled* nil
  "Enable auth payload encryption. Requires server public key.")
(defparameter *server-auth-public-key* nil
  "Server's X25519 public key as hex string. Set by server or config.")
(defparameter *auth-require-encryption* nil
  "Server rejects plaintext auth when T. Set for production.")

;;; Player Data Structures - Array sizes set at player creation
(defparameter *inventory-size* 20
  "Player inventory slots.")
(defparameter *equipment-slot-ids* #(:head :body :legs :weapon :offhand :accessory)
  "Equipment slot order used by the equipment vector.")

;;; New Player Stats - Read only when spawning new players
(defparameter *player-base-attack* 1
  "Base attack level for new players.")
(defparameter *player-base-strength* 1
  "Base strength level for new players.")
(defparameter *player-base-defense* 1
  "Base defense level for new players.")
(defparameter *player-base-hitpoints* 10
  "Base hitpoints level for new players.")
(defparameter *player-training-mode* :balanced
  "Training focus (:attack/:strength/:defense/:hitpoints/:balanced).")

;;;; ========================================================================
;;;; SERVER OPTIONS - Immediate (SLIME tunable)
;;;; Changes take effect immediately. Modify via SLIME: (setf *var* value)
;;;; These are read every tick/frame during server simulation.
;;;; ========================================================================

;;; Simulation Timing
(defparameter *sim-tick-seconds* (/ 1.0 60.0)
  "Fixed simulation tick length in seconds.")
(defparameter *sim-max-steps-per-frame* 5
  "Max sim ticks per frame to avoid spiral of death.")

;;; Combat & XP - Read during combat calculations
(defparameter *stat-xp-per-level* 100
  "XP needed for each quadratic level step.")
(defparameter *stat-max-level* 99
  "Maximum attainable level per stat.")
(defparameter *xp-per-damage* 4
  "XP awarded per point of damage dealt.")
(defparameter *combat-hitpoints-xp-multiplier* 0.33
  "HP XP multiplier applied to focused combat XP.")
(defparameter *attack-hitbox-scale* 1.0
  "Attack hitbox size relative to one tile.")

;;; NPC Behavior - Read every AI/movement tick
(defparameter *npc-max-hits* 3
  "Hits required to defeat the NPC.")
(defparameter *npc-walk-speed* 120.0
  "Base NPC movement speed in pixels per second.")
(defparameter *npc-flee-speed-mult* 1.4
  "Speed multiplier while fleeing.")
(defparameter *npc-attack-range-tiles* 0.9
  "NPC melee range in tiles.")
(defparameter *npc-attack-cooldown* 0.9
  "Seconds between NPC attacks.")
(defparameter *npc-attack-damage* 1
  "Damage per NPC hit.")
(defparameter *npc-home-radius-tiles* 2.0
  "Roam radius around spawn in tiles.")
(defparameter *npc-wander-interval* 1.1
  "Seconds between wander target changes.")
(defparameter *npc-wander-arrive-distance* 6.0
  "Pixels to consider wander target reached.")
(defparameter *npc-respawn-seconds* 5.0
  "Default respawn cooldown in seconds.")
(defparameter *npc-default-archetype-id* :rat
  "Default NPC archetype ID to spawn.")
(defparameter *npc-default-loot-table-id* nil
  "Fallback loot table when NPC archetypes omit one.")
(defparameter *npc-collision-scale* 2.0
  "Collision box size relative to one tile.")

;;; Combat Targeting - Read when validating target requests
(defparameter *max-target-distance-tiles* 15
  "Maximum tiles away a player can target an NPC.")
