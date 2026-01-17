## Current Tasks / TODO

- [ ] Add logout button to ESC menu (currently have to wait for timeout)
----------------------------------
- [x] Test Redis persistence end-to-end (login, save, logout, reload)
- [x] Switch from memory-storage to redis-storage backend
- [x] Verify Redis (Valkey) connectivity and data persistence
- [x] Fix ID counter persistence (NPCs no longer waste persistent IDs)


## Future Tasks / Roadmap

Networking polish that unlocks “MMO feel”
After ownership, the next noticeable improvement is:
Interpolation for remote entities (if not already)
Basic rate limiting + sanity checks (intent frequency, movement bounds)
Optional later: prediction for local player, but only after everything’s stable

----------------------------------

confirm with opus the exacts of this type of thing, i might not be understanding well but i want to work on a plan similar to below but together with your feedback as you understand what we already have better than I do. So from what follows, revise and correct it please, stating, 1. what we have already and 2. where we need to get to. Remember: Keep it simple stupid.

Make persistence boring and safe (still simple)
You don’t need enterprise DB work, but you do want “can’t corrupt progress easily”:
Versioned save schema + migrations (even if it’s just :version 1)
Atomic-ish saves (write new blob then swap key / keep last-good)
Periodic snapshot backup (copy player blobs to backup:<timestamp>:<id> or export dump)
Admin/dev commands: “wipe character”, “teleport to spawn”, “grant item”, “print save”

----------------------------------

Tests (only where they actually help)
Personally I do not like TDD or unit tests at all and that is why we dont have unit tests in our codebase.
But as you are the coder (opus|claude) I am wondering if you would find it helpful to add any tests, but only where they may actually help.
My primary concerns as the owner of the project is:
database - no corruption, schemas are solid and aligning with our code, things that should be saved are saved, things that should not be saved should not be saved.
things like that. things that keep the players happy.
If there's other things though, like, maybe basic things that we know about the code:
this should always be true
or
this should always be false
if we can test for those and ensure them, we might good codebase coverage?
I mean if you want to go ham and create a 99% covered codebase you're welcome to do that too. I really dont care, they're your tests and it's your life. Do they help you? Do they hurt you? Do you want them? Chime in. If you want to do this, they make up a plan, otherwise we can get on to doing more fun stuff, like making the game more fun. Let me know.



----------------------------------
- [ ] Test unauthenticated connection intent handling (acceptance criteria #5: verify server ignores intents from unauthenticated clients)
