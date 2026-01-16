# package.lisp

Purpose
- Provide a clean namespace and a small public API.

Why we do it this way
- A small export surface keeps the engine modular. You can load the system
  in a REPL and still keep most symbols internal, which prevents accidental
  coupling between modules.

Key idea
- Only entry points are exported (`run`, `run-headless`). Everything else stays
  internal until it becomes a deliberate API.

Example
```lisp
(ql:quickload :mmorpg)
(mmorpg:run)
(mmorpg:run-headless :max-seconds 2.0)
```
