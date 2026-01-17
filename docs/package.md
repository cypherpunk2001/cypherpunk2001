# package.lisp

Purpose
- Provide a clean namespace and a small public API.

Why we do it this way
- A small export surface keeps the engine modular. You can load the system
  in a REPL and still keep most symbols internal, which prevents accidental
  coupling between modules.

Key idea
- Only entry points are exported (`run-headless`, `run-server`, `run-client`, `run-local`).
  Everything else stays internal until it becomes a deliberate API.

Example
```lisp
;; Load and run server
(ql:register-local-projects)
(ql:quickload :mmorpg)
(mmorpg:run-server :host "127.0.0.1" :port 1337)

;; Load and run client (in separate session)
(ql:register-local-projects)
(ql:quickload :mmorpg)
(mmorpg:run-client :host "127.0.0.1" :port 1337)

;; Run local single-player mode
(mmorpg:run-local)

;; Run headless server for testing
(mmorpg:run-headless :max-seconds 2.0)
```
