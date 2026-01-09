on system : emacs/slime,sbcl,quicklisp,raylib,raygui

Open main.lisp

M-x slime

(ql:register-local-projects) ; once per session
(ql:quickload :mmorpg)
(mmorpg:run)
