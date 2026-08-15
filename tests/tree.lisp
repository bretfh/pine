(in-package :pine/test)

(def-suite* :pine/tree :in :pine)

(defmacro with-tree (&body body)
  `(unwind-protect (progn (pine:start) ,@body) (pine:stop)))

(defun root-of () (pine/world/world:root pine/world/world:*world*))

(defun at-path (&rest names)
  (apply #'pine/fs/tree:at (root-of) names))

(test every-name-at-the-root-is-a-kind-of-thing-pine-knows
  "The root grew by accretion: each install picked a name and no level meant
anything. What is here now is one entry per kind, and nothing loose beside them."
  (with-tree
    (dolist (name '("dev" "sys" "env" "file" "sh"
                    "proc" "task" "tick" "fault" "client" "log" "store"
                    "buffer" "window" "cmd" "mode" "lang" "theme" "style"))
      (is-true (at-path name) "~a is not at the root" name))
    (is (null (at-path "attached"))
        "a count of each attached kind is not a thing to address")
    (is (null (at-path "syntax")) "the languages are at /lang")
    (is (null (at-path "active-theme")) "which theme is on belongs with them")
    (is (null (at-path "history")) "the prompt's history is the prompt's")))

(test the-machine-s-devices-hang-together
  (with-tree
    (pine:audio)
    (pine:network)
    (is-true (at-path "dev" "clock" "hour"))
    (is-true (at-path "dev" "audio"))
    (is-true (at-path "dev" "net"))
    (is (null (at-path "clock")) "a device is not a root name of its own")
    (is (null (at-path "audio")))))

(test which-theme-is-on-is-kept-with-the-themes
  (with-tree
    (is-true (pine/fs/node:contents (at-path "theme" "active")))
    (setf (pine/fs/node:contents (at-path "theme" "active")) :probe-theme)
    (is (eq :probe-theme (pine/fs/node:contents (at-path "theme" "active"))))
    (is (member "active" (pine/fs/tree:listing (at-path "theme")) :test #'equal)
        "and it is listed with them, not beside them")))

(test every-mode-and-what-it-is-bound-to-can-be-read
  "A mode is what an end user implements to make pine understand something new.
It was a registry nothing could ask about."
  (with-tree
    (is (member "lisp" (pine/fs/node:contents (at-path "mode")) :test #'equal))
    (is-true (at-path "mode" "lisp"))
    (pine/repl/mode:bind "text" "C-c probe" "list-next")
    (is (equal "list-next"
               (pine/data:at (pine/fs/node:contents (at-path "mode" "text" "keys"))
                             "C-c probe"))
        "a chord a config bound is there without anything being told")))

(test the-threads-and-the-ticks-and-the-log-answer
  (with-tree
    (pine/run/log:note "a probe line")
    (is (equal "a probe line" (first (pine/fs/node:contents (at-path "log")))))
    (is (member "SUPERVISOR" (pine/fs/node:contents (at-path "tick"))
                :test #'equal)
        "what repeats on the image's clock is readable")
    (let ((tk (pine/run/task:spawn "probe-task" (lambda () (sleep 30)))))
      (unwind-protect
           (is-true (pine/fs/node:contents (at-path "task" "probe-task")))
        (pine/run/task:stop tk)))))

(test a-fault-is-a-place-and-taking-a-restart-is-a-write
  "The debugger buffer and this are the same act. Over a pipe to a child image
the thread there is still standing in it, so a restart taken at a path resumes
a thread in another process."
  (with-tree
    (pine/run/fault:forget-faults)
    (pine/run/fault:attempt (lambda () (error "a probe fault")) "probing")
    (is (eql 1 (pine/fs/node:contents (at-path "fault"))))
    (is (search "a probe fault" (pine/fs/node:contents (at-path "fault" "0"))))
    (is (member "ABORT" (pine/fs/node:contents (at-path "fault" "0" "offers"))
                :test #'equal))
    (pine/run/fault:forget-faults)))

(test where-this-pine-persists-is-a-node
  (with-tree
    (is (null (pine/fs/node:contents (at-path "store")))
        "nothing was opened, so it says so rather than pretending")))
