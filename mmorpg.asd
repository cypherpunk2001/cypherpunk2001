(asdf:defsystem #:mmorpg
  :description "Minimal Common Lisp + raylib proof"
  :depends-on (#:claw-raylib)
  :serial t
  :components ((:file "src/package")
               (:file "src/main")))
