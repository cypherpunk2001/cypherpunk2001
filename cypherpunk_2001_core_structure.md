# CYPHERPUNK_2001 Core Structure

## Core Loop

Gather → Process → Risk → Upgrade → Repeat

PvE everywhere.\
Full-loot PvP only in Wildy (Nullzone).

### Cities

-   Skill training
-   Economy
-   Reputation

### Wildy (Nullzone)

-   Better resources
-   Higher risk
-   Inventory loss on death

------------------------------------------------------------------------

## Skill Spine (RuneScape Classic → CYPHERPUNK_2001)

-   Fishing → Scavenging\
-   Firemaking → Ignition\
-   Cooking → Cooking\
-   Woodcutting → Salvage\
-   Mining → Deep Extraction\
-   Smithing → Fabrication\
-   Ranged → Projectiles\
-   Magic → Hacking

Everything else is flavor.

If this loop feels good, the game works.

------------------------------------------------------------------------

## Map Equivalents (RuneScape Classic → CYPHERPUNK_2001)

-   Wilderness → Nullzone\
-   Lumbridge (starter) → Bootyard\
-   Varrock (main market hub) → Kernel City\
-   Edgeville (wildy border town) → Nullgate\
-   Falador (order / authority city) → Garrison\
-   Underground / black market areas → Drainnet

Safe start → Main trade city → Wildy threshold → Authority hub → Shadow
economy → Full-loot wilderness.

Same topology.\
Different skin.

CYPHERPUNK_2001 — Skill↔Item Tier Spec (v0.1)
Scope: Levels 1–70 only. Tier bands are ALWAYS 1–10, 11–20, 21–30, 31–40, 41–50, 51–60, 61–70.
Goal: Tight RuneScape Classic-style gating: level → resource → tool → processing → combat utility.

============================================================
UNIVERSAL MATERIAL LADDER (used by Extraction/Salvage/Fabrication/Tools/Ammo)
============================================================
1–10   Scrap
11–20  Iron
21–30  Carbon
31–40  Composite
41–50  Reinforced
51–60  Industrial
61–70  Military

Rule: If it’s “material/structure/metal,” it uses this ladder and the same banding.

============================================================
SCAVENGING (Fishing equivalent)
============================================================
Primary Gathered (food base; some hack fuel drops)
1–10   Spoiled Biomass
11–20  Raw Protein
21–30  Clean Protein
31–40  Synthetic Protein
41–50  Fortified Protein
51–60  Combat Nutrient Base
61–70  Military Nutrient Base

Notes:
- Scavenging feeds Cooking directly (same band in → same band out, see Cooking).
- Scavenging also drops low-tier Hacking fuel items:
  1–10 Junk Scripts
  11–20 Basic Exploits
  (higher tiers primarily crafted, see Hacking)

============================================================
IGNITION (Firemaking equivalent)
============================================================
Primary Items (burn/fuel items enabling cooking + utility)
1–10   Scrap Fuel
11–20  Fuel Gel
21–30  Treated Fuel Mix
31–40  Composite Fuel Pack
41–50  Reinforced Fuel Block
51–60  Industrial Fuel Cell
61–70  Military Fuel Cell

Uses:
- Required for cooking higher tiers (and for world utility like signals/lighting).
- Ignition is “support infrastructure,” not a main economy pillar.

============================================================
COOKING (Cooking equivalent)
============================================================
Inputs: Scavenging protein/biomass tier.
Outputs: Edible sustain items (never metal-named).

1–10   Basic Ration
11–20  Refined Ration
21–30  Fortified Ration
31–40  Combat Ration
41–50  Reinforced Combat Pack
51–60  Industrial Combat Pack
61–70  Military Combat Pack

Rule: Cooking tier names are about quality/packaging, not material ladder.

============================================================
SALVAGE (Woodcutting equivalent)
============================================================
Primary Gathered (structural materials)
1–10   Scrap Frame
11–20  Iron Frame
21–30  Carbon Frame
31–40  Composite Frame
41–50  Reinforced Frame
51–60  Industrial Casing
61–70  Military Plating

Feeds:
- Fabrication (gear/tools)
- Ignition (some tiers may provide burnable components if desired)

============================================================
DEEP EXTRACTION (Mining equivalent)
============================================================
Primary Gathered (alloy/material nodes)
1–10   Scrap Vein
11–20  Iron Deposit
21–30  Carbon Node
31–40  Composite Deposit
41–50  Reinforced Vein
51–60  Industrial Seam
61–70  Military Vein

Feeds:
- Fabrication (gear/tools)
- Projectiles (ammo)
- Hacking (higher-tier fuel components via rare substrates)

============================================================
FABRICATION (Smithing equivalent)
============================================================
Craftable Tier Outputs (match universal material ladder)
1–10   Scrap Gear
11–20  Iron Gear
21–30  Carbon Gear
31–40  Composite Gear
41–50  Reinforced Gear
51–60  Industrial Gear
61–70  Military Gear

Includes:
- Weapons/Armor chassis
- Tools (drills/cutters/rigs)
- Ammo parts (casings)
- Hacking hardware (deck modules/interfaces)

============================================================
TOOLS (non-consumable, tier-gated)
============================================================
Tool tiers match universal material ladder. Tool must be at/above band to gather that band.

Deep Extraction tools:
1–10   Scrap Drill
11–20  Iron Drill
21–30  Carbon Drill
31–40  Composite Drill
41–50  Reinforced Drill
51–60  Industrial Drill
61–70  Military Drill

Salvage tools:
1–10   Scrap Cutter
11–20  Iron Cutter
21–30  Carbon Cutter
31–40  Composite Cutter
41–50  Reinforced Cutter
51–60  Industrial Cutter
61–70  Military Cutter

Fabrication tools (optional but consistent):
1–10   Scrap Rig
11–20  Iron Rig
21–30  Carbon Rig
31–40  Composite Rig
41–50  Reinforced Rig
51–60  Industrial Rig
61–70  Military Rig

============================================================
PROJECTILES (Ranged equivalent)
============================================================
Ammo (stackable, consumable; requires Fabrication inputs)
1–10   Scrap Slugs
11–20  Iron Slugs
21–30  Carbon Rounds
31–40  Composite Rounds
41–50  Reinforced Rounds
51–60  Industrial Rounds
61–70  Military Rounds

Rule: Ammo is always player-produced and stackable. This is a core economy driver.

============================================================
HACKING (Magic equivalent; previously “Anomaly”)
============================================================
Fuel (stackable, consumable “runes”)
1–10   Junk Scripts
11–20  Basic Exploits
21–30  Stable Exploits
31–40  Targeted Payloads
41–50  Hardened Payloads
51–60  Industrial ICEbreakers
61–70  Military ICEbreakers

Typical “spell” outputs (examples; tune later):
- Damage: Overload / Spike / Burnout
- Root/Snare: Lockout
- Debuff: Signal Jam / Armor Strip
- Defense: Firewall / Shield Protocol
- Mobility/Teleport: Transit Ping / Relocation Beacon

Source/Crafting links:
- Scavenging drops low tiers (Junk Scripts, Basic Exploits).
- Higher tiers are crafted using:
  - Deep Extraction rare substrates + power components
  - Fabrication hardware modules (decks/interfaces)

============================================================
CURRENCY / STORAGE / TRADE (support primitives)
============================================================
gp → Credits
bank → Vault Terminal
trade window → Direct Link Trade

============================================================
GLOBAL RULES (keep the RSC “feel”)
============================================================
- Banding stays uniform: 1–10, 11–20, 21–30, 31–40, 41–50, 51–60, 61–70.
- Tools gate gathering. Higher-tier resource requires appropriate-tier tool.
- Processing skills create combat-critical outputs:
  - Scavenging→Cooking = sustain
  - Extraction/Salvage→Fabrication = gear/tools/ammo
  - Hacking fuel = stackable and expensive (like runes)
- High-tier materials primarily sourced in high-risk zones (Nullzone) past ~50+.
