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
(defparameter *private-state-retries* 3
  "Frames to resend private state updates to the owning client.")

;;; Snapshot Rate - Phase 3 perf: decouple snapshot rate from sim rate
(defparameter *snapshot-rate-hz* 20
  "Snapshot send rate in Hz. Default 20Hz with client interpolation.
   Lower than sim rate (60Hz) to reduce bandwidth and serialization load.
   Override via MMORPG_SNAPSHOT_RATE environment variable.")
(defparameter *snapshot-interval* (/ 1.0 20)
  "Seconds between snapshot broadcasts. Computed from *snapshot-rate-hz*.")

;;; Delta Compression - See docs/net.md Prong 2
(defparameter *delta-compression-enabled* t
  "Enable delta compression (Prong 2). When T, server sends only dirty entities
   to synced clients. When NIL, always sends full snapshots (Prong 1 only).
   Toggle via MMORPG_DELTA_COMPRESSION=1 environment variable.")
(defparameter *max-delta-age* 60
  "Max snapshots a client can be behind before forcing full resync.
   At 20 Hz, 60 = 3 seconds of missed packets.")
(defparameter *max-delta-gap* 5
  "Max snapshot gap tolerated before forcing full resync.
   Keeps delta updates safe without per-client delta history tracking.")

;;; UDP Fragmentation - See docs/net.md Prong 3
(defparameter *chunk-overhead* 100
  "Reserved bytes for chunk message header.")
(defparameter *max-chunk-payload* (- usocket:+max-datagram-packet-size+ 100)
  "Maximum payload bytes per UDP chunk (buffer size minus header overhead).")
(defparameter *chunk-timeout* 1.0
  "Seconds before discarding incomplete chunk sequences.")

;;; Binary Snapshots - Phase 3 perf: reduce serialization overhead
(defparameter *use-binary-snapshots* nil
  "Enable binary snapshot encoding (Phase 3 Task 3.1).
   When T, snapshots use compact binary format instead of plist text.
   Reduces bandwidth and serialization CPU. Requires client/server sync.
   Toggle via MMORPG_BINARY_SNAPSHOTS=1 environment variable.")

;;; Auth Worker Pool - Multiple threads for parallel auth processing
(defparameter *auth-worker-count* 4
  "Number of auth worker threads. Each independently processes auth requests.
   Override via MMORPG_AUTH_WORKERS environment variable.")
(defparameter *auth-queue-max-depth* 200
  "Maximum auth requests queued. Beyond this, server responds :server-busy.
   0 = unlimited. Override via MMORPG_AUTH_QUEUE_MAX environment variable.")
(defparameter *auth-request-max-age* 25.0
  "Seconds before a queued auth request is considered stale and skipped.
   Should be less than client auth timeout (30s) to avoid wasted work.")
(defparameter *max-messages-per-tick* 2000
  "Maximum UDP messages processed per tick before yielding to simulation.
   Prevents message flood from starving auth integration and snapshots.
   Override via MMORPG_MAX_MESSAGES_PER_TICK environment variable.")

;;; Auth Metrics Logging
(defparameter *auth-metrics-logging* nil
  "When T, log auth metrics every 30s (only when metrics change).
   Disabled by default to reduce log noise during normal local play.
   Override via MMORPG_AUTH_METRICS=1 environment variable.")

;;; Password Hashing (PBKDF2-SHA256)
(defparameter *password-hash-iterations* 10000
  "PBKDF2-SHA256 iteration count for password hashing.
   Step 11: Reduced from 100k to 10k for ~10x faster hashing.
   At 10k iterations with 4 workers, auth throughput ~400 req/s.
   10k PBKDF2-SHA256 still requires months of GPU time to brute-force.
   Override via MMORPG_PASSWORD_HASH_ITERATIONS environment variable.")
(defparameter *password-legacy-iterations* 100000
  "Legacy PBKDF2 iteration count for verifying old 2-part format hashes.
   Used only for backward compatibility with accounts created before Step 11.")
(defparameter *password-salt-bytes* 16
  "Salt length in bytes. 16 bytes = 128 bits, sufficient for security.")

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
;;; NOTE: This is SERVER-SIDE ONLY. Affects ALL connected players.
;;; This controls how often the server processes game logic (physics, combat, AI).
;;; Client FPS is controlled separately by *client-target-fps* in config-client.lisp.
;;; Common values: 1/60 (60Hz, responsive), 1/30 (30Hz, lower CPU, larger delta)
(defparameter *sim-tick-seconds* (/ 1.0 60.0)
  "Fixed simulation tick length in seconds. Server processes game logic at this rate.
   1/60 = 60Hz tick rate. Changing this affects ALL players connected to the server.")
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
(defparameter *npc-default-archetype-id* :sewer-drone
  "Default NPC archetype ID to spawn.")
(defparameter *npc-default-loot-table-id* nil
  "Fallback loot table when NPC archetypes omit one.")
(defparameter *npc-collision-scale* 2.0
  "Collision box size relative to one tile.")

;;; Combat Targeting - Read when validating target requests
(defparameter *max-target-distance-tiles* 15
  "Maximum tiles away a player can target an NPC.")

;;; Strategic GC Scheduling (Task 5.4)
;;; Trigger GC at safe points to reduce worst-case spikes.
(defparameter *gc-scheduling-enabled* nil
  "When T, trigger periodic GC at safe points (after snapshot broadcast).
   Enable via MMORPG_GC_SCHEDULING=1 environment variable.")
(defparameter *gc-interval-seconds* 60.0
  "Seconds between scheduled GC triggers. Default 60s.")
(defparameter *gc-last-time* 0.0
  "Internal: last GC trigger time. Do not modify.")
