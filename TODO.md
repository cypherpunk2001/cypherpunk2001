# TODO - Server Performance Testing

## Agenda for Tomorrow

### 1. Stress Test Single Server - Find Breaking Point

**Goal:** Discover actual player capacity, not theoretical estimates.

**Method:**
- Start server with `make server`
- Run stress test with increasing client counts: 100 → 200 → 400 → 800 → 1600
- Commands:
  ```bash
  STRESS_CLIENTS=100 STRESS_DURATION=120 make stress
  STRESS_CLIENTS=200 STRESS_DURATION=120 make stress
  STRESS_CLIENTS=400 STRESS_DURATION=120 make stress
  # Keep doubling until something breaks
  ```

**Watch for:**
- Tick time exceeding 16.6ms (60 Hz budget)
- Packet loss / client disconnects
- CPU saturation
- Memory usage

**Record:** At what player count does performance degrade?

---

### 2. Profile Server Under Load

**Goal:** Identify the actual bottleneck (not guesses).

**Use:** `scripts/profile-server.lisp` or SBCL profiler

**Questions to answer:**
- Is it the O(n²) combat checks in simulation loop?
- Is it snapshot serialization?
- Is it UDP socket sending?
- Something else entirely?

**Record:** CPU time breakdown by function.

---

### 3. Measure Actual Snapshot Sizes

**Goal:** Verify or debunk the ~64 bytes per player claim.

**Method:**
- Add logging to `net.lisp` to measure encoded snapshot byte sizes
- Test at different player counts (10, 50, 100, 200)
- Test with all players moving vs all idle (delta compression)

**Record:**
- Bytes per player (actual measured, not estimated)
- Bandwidth usage at different scales

---

### 4. Remove Dead Multi-Threading Code

**Goal:** Simplify codebase by deleting unimplemented features.

**What to remove:**
- `worker-threads` parameter from `run-server` (net.lisp:2079)
- `(declare (ignore worker-threads))` stub (net.lisp:1262)
- Dead references in comments/docs
- Update CLAUDE.md line 250 (remove "Optional parallel sends")

**Test:** Run `make tests` to ensure nothing breaks.

---

### 5. Document Actual Performance Characteristics

**Goal:** Replace theoretical estimates with measured data.

**Update these locations:**
- `docs/net.md` lines 369-374 (capacity table) - replace with measured data
- `CLAUDE.md` line 306 (scaling estimate) - update with real numbers
- Add note: "Tested capacity: X players (measured), theoretical: Y (estimate)"

**Create:** Performance report with:
- Tested player counts
- Bottleneck identified
- Actual snapshot sizes
- Recommendations for optimization (if needed)

---

## Key Insights from Today

**What Codex Got Wrong:**
- 300-500 player capacity is **untested theory**
- Parallel snapshot sending **never implemented** (code ignores worker-threads)
- No benchmark data exists in repo

**What We Know:**
- Snapshot architecture is solid (delta compression, compact vectors, encode-once-send-many)
- Redis isn't the bottleneck (batched writes, rare tier-1 saves)
- Single-threaded server might handle way more than expected
- OR it might cap at 200 - **we don't know until we test**

**The Plan:**
- Measure reality before building infrastructure we might not need
- If single server handles 2000+ players, we're golden
- If it caps at 200, we'll know exactly where to optimize
- Skip load balancer and multi-threading until we have data

---

## Success Criteria

✅ Know actual player capacity (not guesses)
✅ Know actual bottleneck (profiling data)
✅ Know actual snapshot sizes (measured bytes)
✅ Codebase simplified (dead code removed)
✅ Docs updated with real numbers
