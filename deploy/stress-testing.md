Moved this doc here out from Claude.md, as devops will soon need to:

- Separate make stress, make server, and make client into independent compute environments to obtain more accurate profiling data and more realistic gameplay QA results.

### Stress Testing

Test server performance with headless clients:

```bash
make stress                           # 10 clients for 60 seconds (default)
STRESS_CLIENTS=50 make stress         # 50 clients for 60 seconds
STRESS_DURATION=300 make stress       # 10 clients for 5 minutes
STRESS_CLIENTS=100 STRESS_DURATION=120 make stress  # 100 clients for 2 minutes
```

Each headless client:
- Registers with unique username (`stress000001`, `stress000002`, ...)
- Authenticates via UDP
- Walks randomly (changes direction every 3 seconds)
- Sends movement intents every 100ms
- Receives and processes snapshots

Useful for:
- Finding server bottlenecks
- Testing concurrent connection limits
- Stress testing dirty flag batching
- Profiling CPU/memory usage under load

**Note:** Start server separately before running stress test, or it will timeout trying to connect.
