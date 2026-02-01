# TODO

In 1b4fd42 I think *if I understand correctly, not sure* we may have implemented some type of redis connection
pooling, especially related to auth logins/registers. I am wondering if it is possible or worth doing (or already done?) that maybe we should re-use the redis connection pooling around the other places of the codebase that make redis connections? Since it's a MMORPG, we definitely should always take care to make sure we are scaling the way we connect to DB so as to support massive amounts of players on the same server at the same time. This task is to research my idea, and let me know the facts, are we doing this, what are we in fact doing, should we consider doing this, and if so, how would we do it. Provide recommends in findings_redis_connections.md

---

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
