(:tunables
 (:zone-path "data/zones/zone-1.lisp")
 (:zone-root "data/zones")
 (:zone-default-width 64)
 (:zone-default-height 64)
 (:zone-default-chunk-size 8)
 (:world-graph-path "data/world-graph.lisp")
 (:zone-loading-seconds 0.35)
 (:minimap-width 220)
 (:minimap-height 220)
 (:minimap-padding 12)
 (:minimap-point-size 4)
 (:editor-start-enabled nil)
 (:music-volume-steps 10)
 (:music-default-volume-level 1)
 (:tileset-path "../assets/Zelda-like/Overworld.png")
 (:tileset-columns 40)
 (:floor-tile-index 0)
 (:editor-tileset-root "../assets/Zelda-like")
 (:editor-object-layer-id :objects)
 (:player-animation-set-id :player-wizard)
 (:npc-default-archetype-id :rat)
 (:npc-spawn-ids (:rat :goblin :orc))
 (:npc-count 3)
 (:npc-spawn-columns 3)
 (:npc-spawn-gap-tiles 2.0)
 (:player-speed 222.0)
 (:player-collision-scale 0.85)
 (:npc-collision-scale 0.95)
 (:collision-edge-epsilon 0.05)
 (:run-speed-mult 2.0)
 (:run-stamina-max 10.0)
 (:player-base-attack 1)
 (:player-base-strength 1)
 (:player-base-defense 1)
 (:player-base-hitpoints 10)
 (:player-training-mode :balanced)
 (:stat-xp-per-level 100)
 (:stat-max-level 99)
 (:xp-per-damage 4)
 (:combat-hitpoints-xp-multiplier 0.33)
 (:inventory-size 20)
 (:idle-frame-count 4)
 (:walk-frame-count 6)
 (:attack-frame-count 4)
 (:idle-frame-time 0.25)
 (:walk-frame-time 0.12)
 (:attack-frame-time 0.1)
 (:blood-frame-count 4)
 (:blood-frame-time 0.08))

:animation-sets
 (:player-archer
  (:dir "../assets/1 Characters/1"
   :down-idle "D_Idle.png"
   :down-walk "D_Walk.png"
   :down-attack "D_Attack.png"
   :up-idle "U_Idle.png"
   :up-walk "U_Walk.png"
   :up-attack "U_Attack.png"
   :side-idle "S_Idle.png"
   :side-walk "S_Walk.png"
   :side-attack "S_Attack.png"))
 (:player-warrior
  (:dir "../assets/1 Characters/2"
   :down-idle "D_Idle.png"
   :down-walk "D_Walk.png"
   :down-attack "D_Attack.png"
   :up-idle "U_Idle.png"
   :up-walk "U_Walk.png"
   :up-attack "U_Attack.png"
   :side-idle "S_Idle.png"
   :side-walk "S_Walk.png"
   :side-attack "S_Attack.png"))
 (:player-wizard
  (:dir "../assets/1 Characters/3"
   :down-idle "D_Idle.png"
   :down-walk "D_Walk.png"
   :down-attack "D_Attack.png"
   :up-idle "U_Idle.png"
   :up-walk "U_Walk.png"
   :up-attack "U_Attack.png"
   :side-idle "S_Idle.png"
   :side-walk "S_Walk.png"
   :side-attack "S_Attack.png"))
 (:npc-rat
  (:dir "../assets/3 Dungeon Enemies/1"
   :down-idle "D_Idle.png"
   :up-idle "U_Idle.png"
   :side-idle "S_Idle.png"))
 (:npc-goblin
  (:dir "../assets/3 Dungeon Enemies/2"
   :down-idle "D_Idle.png"
   :up-idle "U_Idle.png"
   :side-idle "S_Idle.png"))
 (:npc-orc
  (:dir "../assets/3 Dungeon Enemies/3"
   :down-idle "D_Idle.png"
   :up-idle "U_Idle.png"
   :side-idle "S_Idle.png"))
 (:npc-witch-doctor
  (:dir "../assets/3 Dungeon Enemies/4"
   :down-idle "D_Idle.png"
   :up-idle "U_Idle.png"
   :side-idle "S_Idle.png"))
 (:blood
  (:dir "../assets/1 Characters/Other"
   :down "D_Blood.png"
   :up "U_Blood.png"
   :side "S_Blood.png"))

:items
 (:coins
  (:name "Coins"
   :stack-size 9999
   :value 1))
 (:bones
  (:name "Bones"
   :stack-size 1
   :value 5))
 (:rat-tail
  (:name "Rat Tail"
   :stack-size 1
   :value 2))
 (:goblin-ear
  (:name "Goblin Ear"
   :stack-size 1
   :value 4))

:loot-tables
 (:rat
  (:rolls 1
   :entries
   ((:coins 10 1 5)
    (:bones 2 1 1)
    (:rat-tail 1 1 1))))
 (:goblin
  (:rolls 1
   :entries
   ((:coins 10 2 8)
    (:bones 2 1 1)
    (:goblin-ear 1 1 1))))
 (:orc
  (:rolls 1
   :entries
   ((:coins 10 3 12)
    (:bones 2 1 1))))
 (:witch-doctor
  (:rolls 1
   :entries
   ((:coins 10 4 16)
    (:bones 3 1 2))))

:npc-archetypes
 (:rat
  (:name "Dungeon Rat"
   :animation-set-id :npc-rat
   :max-hits 3
   :attack-level 1
   :strength-level 1
   :defense-level 1
   :hitpoints-level 3
   :combat-xp 2
   :loot-table-id :rat
   :move-speed 120.0
   :attack-range-tiles 0.85
   :attack-cooldown 0.9
   :attack-damage 1
   :home-radius-tiles 2.0
   :wander-interval 1.1
   :flee-speed-mult 1.4
   :aggro-mode :provoked
   :retaliate t
   :flee-at-hits 1
   :perception-tiles 4.0))
 (:goblin
  (:name "Goblin"
   :animation-set-id :npc-goblin
   :max-hits 4
   :attack-level 3
   :strength-level 3
   :defense-level 2
   :hitpoints-level 4
   :combat-xp 4
   :loot-table-id :goblin
   :move-speed 140.0
   :attack-range-tiles 0.9
   :attack-cooldown 0.8
   :attack-damage 1
   :home-radius-tiles 3.0
   :wander-interval 1.0
   :flee-speed-mult 1.2
   :aggro-mode :always
   :retaliate t
   :flee-at-hits 1
   :perception-tiles 6.0))
 (:orc
  (:name "Orc"
   :animation-set-id :npc-orc
   :max-hits 6
   :attack-level 5
   :strength-level 5
   :defense-level 4
   :hitpoints-level 6
   :combat-xp 6
   :loot-table-id :orc
   :move-speed 120.0
   :attack-range-tiles 0.95
   :attack-cooldown 1.1
   :attack-damage 2
   :home-radius-tiles 3.0
   :wander-interval 1.3
   :flee-speed-mult 1.0
   :aggro-mode :always
   :retaliate t
   :flee-at-hits 0
   :perception-tiles 7.0))
 (:witch-doctor
  (:name "Witch Doctor"
   :animation-set-id :npc-witch-doctor
   :max-hits 5
   :attack-level 4
   :strength-level 4
   :defense-level 3
   :hitpoints-level 5
   :combat-xp 5
   :loot-table-id :witch-doctor
   :move-speed 110.0
   :attack-range-tiles 1.1
   :attack-cooldown 1.0
   :attack-damage 1
   :home-radius-tiles 3.5
   :wander-interval 1.4
   :flee-speed-mult 1.3
   :aggro-mode :provoked
   :retaliate t
   :flee-at-hits 2
   :perception-tiles 8.0))
