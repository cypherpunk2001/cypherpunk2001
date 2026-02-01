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


Currently the way that the world map loads, is such that it's possible to hit a hard
loading... message in guaranteed locations, it's such that if you find the intersection '+' of
4 zones, then you

   |
   |
 ld|lding...
---+---
 ld|lding...
   |
   |
