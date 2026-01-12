(asdf:defsystem #:mmorpg
  :description "Minimal Common Lisp + raylib proof"
  :depends-on (#:claw-raylib)
  :serial t
  :components ((:file "src/package")
               (:file "src/config")
               (:file "src/data")
               (:file "src/intent")
               (:file "src/types")
               (:file "src/utils")
               (:file "src/map")
               (:file "src/movement")
               (:file "src/input")
               (:file "src/combat")
               (:file "src/ai")
               (:file "src/audio")
               (:file "src/ui")
               (:file "src/rendering")
               (:file "src/main")))
