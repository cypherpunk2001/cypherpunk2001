(:tunables
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

:npc-archetypes
 (:rat
  (:name "Dungeon Rat"
   :animation-set-id :npc-rat
   :max-hits 3
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
