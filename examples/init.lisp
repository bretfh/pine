(in-package :pine.user)

(defcommand "hello" () (:describes "a command this config added")
  "hello from the config")

(pine.repl.mode:bind "text" "C-c h" "hello")

(pine.repl.mode:mode "org" :parent "text"
                           :settings '(:indicator "Org")
                           :claims '((:files "*.org")))

(setf (pine.fs.node:contents
       (pine.world.world:ensure pine.world.world:*world* "active-theme"))
      :ef-dream)

(declare-frontends '("editor"))
