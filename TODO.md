# TODO

We noticed while running `scripts/stress-test.lisp` that when server has around 1000 concurrent players that it becomes near impossible for users to login or register. The concern is that under high server loads that auth may become unusable.

- Stress test (simple mode) still shows auth timeouts/drops even at 1 registration per 0.5s; indicates server-side auth throughput/queueing limits, not client hammering.

Is this possible? If so, can we investigate and ensure that both login and registration auth will continue to work 100% even under high server load with many concurrent players?

- Fix server auth throughput (auth queue O(1), multiple auth workers, backpressure, metrics).

no code changes yet, investigate and report to findings.md



- Decide next step:
  - Increase auth timeout in `scripts/stress-test.lisp` to avoid dropping slow auths during long runs, or
  - Fix server auth throughput (auth queue O(1), multiple auth workers, backpressure, metrics).


consider 2x'ing 36 seconds walk or 3x'ing ~50ish seconds the size of all zones  walk before zone crossing
currently walking across a new zone with a brief loading hitch is annoying every 18 seconds.
We have modern computers and servers and likely should be able to handle larger zones.
But before 'just doing it' what do you think is a reasonable zone size, we did creat the zone system on purpose along with all the chunk loading etc etc to give a massive experience to large numbers of players. we dont want to just undercut everything either.

---

Currently the way that the world map loads, is such that it's possible to hit a hard
'loading...' message 4 times in a matter of a couple of seconds, if you walk in a circle around the intersection of 4 zones. Trying to show below in ascii:

   |
   |
 ld|lding...
---+---
 ld|lding...
   |
   |

I wonder if you can think of a way to rearchitect zones such that they load a bit more intelligently, I am not sure what possible things you can think of? Currently we have an issue that 1. our zones are bit small, so we can hit a loading zone while running about every 10 seconds or so, which is too much. We could resize our zones to be bigger, but that doesnt fix the problem of crazy loading zones while running back and forth from zone to zone or especially at the intersection point of 4 zones running in a circle, it becomes a crazy amount of loading. While i do want to utilize our zone system to increase performance of the game world as we have done and to scale to masses as a proper mmo world should, i do wonder if there is not a way to improve the system to feel more natural. What is your take, what are our options, no code changes, just lay out an analysis and make recommendations in a new file: findings_loading_zones.md

---

In  Screenshot_20260131_191314.png we see that a fleeing monster became invisible, but if you carefully, there is a green square hitbox on the left beside the water, where the monster is shown to be visible in debug mode, the problem is that at some point during combat while fleeing, the NPC became invisble. Can you find the source of this problem and create a findings_invisible_npc.md file with your findings?
