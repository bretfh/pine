(in-package :pine)

(pine.repl.command:defcommand "hello" () (:describes "a command a config added")
  "hello from the config")

(pine.repl.mode:bind "text" "C-c h" "hello")

(setf (pine.fs.node:contents
       (pine.world.world:ensure pine.world.world:*world* "config" "loaded"))
      t)
