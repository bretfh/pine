(defpackage #:pine
            (:use #:cl)
            (:shadow #:describe #:read #:write)
            (:local-nicknames (#:ui #:pine/ui)
                              (#:d #:pine/data)
                              (#:node #:pine/fs/node) (#:commit #:pine/fs/commit)
                              (#:tree #:pine/fs/tree)
                              (#:path #:pine/fs/path) (#:mount #:pine/fs/mount)
                              (#:store #:pine/fs/store)
                              (#:libs #:pine/run/libs) (#:log #:pine/fs/log)
                              (#:meter #:pine/run/meter) (#:fault #:pine/run/fault)
                              (#:actors #:pine/run/actors) (#:job #:pine/run/job)
                              (#:watch #:pine/run/watch)
                              (#:command #:pine/run/command)
                              (#:image #:pine/run/image)
                              (#:peer #:pine/run/peer) (#:system #:pine/run/system)
                              (#:session #:pine/run/session))
            (:export
   #:start #:stop #:main #:daemon #:quit
   #:use #:drop #:reach #:spawn #:style
   #:at #:read #:write #:watch #:ls #:standsp
   #:toggle #:include #:exclude #:blend
   #:speaks #:load-config #:user-package
   #:console #:opening))
(in-package #:pine)

(defgeneric opening (what)
            (:documentation "Open WHAT, if anything loaded here knows how.

:DISPLAY is the one pine asks for. A frontend answers it in its own file, so an
image built without one has nothing to unset and nothing to check -- there is
simply no method, which is the truth about that image.")
            (:method (what) (declare (ignore what)) nil))

(defun %cursor ()
  "Where a command given no place looks: the node a session was moved to by CD, or
the root. A default for an argument nobody supplied, and nothing more -- it is not
a place a name can be measured from, so it reaches neither a config nor the wire."
  (or (and session:*session* (session:in session:*session*)) (tree:root)))

(defun at (where &rest names)
  "The node WHERE names, and NAMES on from there."
  (apply #'tree:at where names))

(defun read (where &key (else nil elsep))
  "What stands at WHERE, and whether anything does.

Answers the value and one of :HELD or :ABSENT. A place nobody has written and a
place somebody wrote NIL to are different questions, and NIL is only the answer to
the second: without the second value the two cannot be told apart, and every caller
has to guess. ELSE is what to say instead of nothing, so a reader that has an
answer of its own does not spell it as an OR."
  (let ((n (tree:at where)))
    (if (null n)
        (values (if elsep else nil) :absent)
        (let ((value (node:contents n)))
          (values (if (and (null value) elsep) else value) :held)))))

(defun standsp (where)
  "Whether anything stands at WHERE."
  (and (tree:at where) t))

(defun write (where value)
  "Put VALUE at WHERE, making the place if nothing has been put there yet: a read
finds what is there and a write makes what is not."
  (setf (node:contents (tree:ensure where)) value))

(defun ls (where)
  "The names directly under WHERE, and none where nothing stands.

The fourth verb. It was a command and not a word, so a session could say it and a
config could not."
  (let ((n (tree:at where)))
    (if n (tree:listing n) (list))))

(defun watch (where tells &rest options)
  "Say TELLS whenever what stands at WHERE moves. It is given the node and what it
now holds.

WHERE names a place the way the other three verbs do. Watching one nothing stands
at is a mistake rather than a silence: the watcher would be told about a node the
world is going to replace."
  (let ((n (tree:at where)))
    (unless n (error 'tree:absent :where where))
    (apply #'watch:watch n tells options)))

(defun toggle (where)
  "Flip what stands at WHERE.

A write, like the three below it: the four of them are what NODE:VERB has always
done, said in words rather than by writing a seq that begins with a keyword. That
spelling worked from the shell and not from lisp, which is why the mute button in
a config could mute and never unmute."
  (node:verb (tree:ensure where) :toggle nil))

(defun include (where value)
  "Put VALUE into the set at WHERE."
  (node:verb (tree:ensure where) :conj (list value)))

(defun exclude (where value)
  "Take VALUE out of the set at WHERE."
  (node:verb (tree:ensure where) :disj (list value)))

(defun blend (where map)
  "Merge MAP into the map at WHERE."
  (node:verb (tree:ensure where) :merge (list map)))

(defun describe (where)
  (let ((n (tree:at where)))
    (when n
      (list :name (node:full-name n)
            :class (class-name (class-of n))
            :describes (node:describes n)
            :under (tree:listing n)
            :saved (node:savedp n)
            :live (node:livep n)))))

(defun speaks (name)
  "Put every word the vocabulary NAME holds into the language.

A vocabulary is a package that uses nothing and imports what it offers, so what it
holds is exactly what it offers and the compiler checked every name in it. A system
says this as it loads, which is what makes the words it brings sayable in a config
that was already read.

USE-PACKAGE is the whole mechanism. Two vocabularies claiming one word is a
PACKAGE-ERROR naming both symbols, where it happens, rather than a sentence
collected in a list nobody reads."
  (let ((p (find-package name))
        (into (find-package '#:pine/user))
        (all nil))
    (unless p (error "~a is not a vocabulary this image has." name))
    (do-symbols (s p) (pushnew s all))
    (cl:export all p)
    (when into
      (use-package p into)
      (cl:export all into))
    p))

(defun use (name) (system:use name))
(defun drop (name) (system:drop name))
(defun reach (name &rest arguments) (apply #'peer:reach name arguments))
(defun serve (&rest arguments) (apply #'peer:serve arguments))

(defun style (selector properties)
  "One rule, put on the sheet. What a config says on top of the theme."
  (first (ui:put-rules (list (list selector properties)))))

(defun spawn (name &key (systems '(:pine)))
  "Another lisp of pine's own, supervised. Work can be done in it, and a fault it
stands in comes back here with the restarts it is still offering."
  (let ((j (make-instance 'image:child :name (princ-to-string name)
                          :systems systems)))
    (job:supervise j)
    (job:start j)
    j))

(command:defcommand "pwd" () (:describes "where this session is")
                    (node:full-name (%cursor)))

(command:defcommand "ls" (&optional where) (:describes "what is under a node")
                    (let ((n (if where (tree:at where) (%cursor))))
                      (if n (tree:listing n) (list))))

(command:defcommand "cd" (&optional where) (:describes "go to a node")
                    (let ((n (if where (tree:at where) (tree:root))))
                      (when (and n session:*session*) (setf (session:in session:*session*) n))
                      (and n (node:full-name n))))

(command:defcommand "cat" (where) (:describes "what a node holds")
                    (let ((n (tree:at where)))
                      (and n (node:contents n))))

(command:defcommand "put" (where value) (:describes "write a node")
                    (setf (node:contents (tree:ensure where))
                          value))

(command:defcommand "mkdir" (where) (:describes "make a branch")
                    (node:full-name (tree:ensure where)))

(command:defcommand "rm" (where) (:describes "take a node off")
                    (and (tree:erase where) t))

(command:defcommand "tree" (&optional where)
                    (:describes "every node under one that pine keeps")
  (let ((n (if where (tree:at where) (%cursor))))
    (unless n (error 'tree:absent :where where))
    (tree:paths n)))

(command:defcommand "live" ()
                    (:describes "what answers from the world, not the store")
                    (let (out)
                      (tree:walk (tree:root)
                                 (lambda (n) (when (node:livep n) (push (node:full-name n) out))))
                      (nreverse out)))

(command:defcommand "mount" (what name)
                    (:describes "put a directory, or another pine, in the tree")
                    (let ((it (or (peer:named what) (pathname (princ-to-string what)))))
                      (node:full-name (mount:mount it (tree:root) (princ-to-string name)))))

(command:defcommand "reach" (name port &optional host)
                    (:describes "get to another pine")
                    (job:name (peer:reach (princ-to-string name)
                                          :host (and host (princ-to-string host))
                                          :port (if (integerp port)
                                                    port
                                                  (parse-integer (princ-to-string port))))))

(command:defcommand "use" (name) (:describes "load a system and start it")
                    (let ((s (use name))) (and s (job:name s))))

(command:defcommand "drop" (name) (:describes "stop a system and take it off")
                    (let ((s (drop name))) (and s (job:name s))))

(command:defcommand "systems" () (:describes "what pine has loaded, and what it offers")
                    (list :running (mapcar #'job:name (system:systems))
                          :offered (system:offered)))

(command:defcommand "jobs" () (:describes "what is running")
                    (loop :for j :in (job:jobs)
                          :collect (list (job:name j) (job:state j) (job:tries j))))

(command:defcommand "spawn" (name) (:describes "another lisp of pine's own")
                    (job:name (spawn name)))

(command:defcommand "kill" (name) (:describes "stop a job and forget it")
                    (let ((j (job:named (princ-to-string name))))
                      (when j (job:stop j) (job:forget (job:name j)) t)))

(command:defcommand "help" (&optional name) (:describes "what a command is for")
                    (if name
                        (let ((c (command:named (princ-to-string name))))
                          (and c (command:describes c)))
                      (loop :for c :in (command:sorted)
                            :collect (list (command:name c) (command:describes c)))))

(command:defcommand "faults" () (:describes "what has broken here")
                    (loop :for f :in (fault:faults)
                          :collect (list (fault:label f)
                                         (if (fault:standingp f) :standing :done)
                                         (princ-to-string (fault:condition-of f)))))

(command:defcommand "take" (restart)
                    (:describes "hand a standing fault one of its restarts")
                    (let ((f (first (fault:standing))))
                      (and f (fault:take f (princ-to-string restart)))))

(command:defcommand "snapshot" () (:describes "write the tree to its store")
                    (and store:*store* (store:snapshot store:*store*)))

(command:defcommand "metrics" ()
                    (:describes "how long what pine does is taking")
                    (meter:readings))

(command:defcommand "metrics-reset" ()
                    (:describes "start a fresh window of samples")
                    (meter:reset)
                    :reset)

(command:defcommand "describe" (where) (:describes "what stands at a place")
                    (describe where))


(defun start (&key (name "pine") store remoting)
  (libs:attend)
  (tree:make-root)
  (unless (actors:runningp) (actors:boot :remoting remoting))
  (let ((root (tree:root)))
    (setf (node:contents (tree:ensure root "name")) name)
    (tree:built root)
    (tree:ensure "/surface")
    (mount:mount #p"/" root "file"))
  (job:attend)
  (when store
    (store:open-store store)
    (store:restore store:*store*)
    (store:keeping))
  (tree:root))

(defun stop ()
  (fault:or-nothing "there may be no socket to close"
    (pine/serve/socket:close-socket))
  (commit:forget-listeners)
  (dolist (s (session:sessions)) (session:close s))
  (dolist (j (system:systems)) (fault:attempt (lambda () (job:stop j)) (job:name j)))
  (watch:forget-all)
  (dolist (j (job:jobs)) (fault:attempt (lambda () (job:stop j)) (job:name j)))
  (when store:*store*
    (fault:attempt (lambda () (store:snapshot store:*store*))
                   "writing the tree down")
    (store:close-store store:*store*))
  (actors:leave)
  t)

(defun config-file ()
  (merge-pathnames "pine/init.lisp" (uiop:xdg-config-home)))

(defun store-file ()
  (merge-pathnames "pine/tree.db" (uiop:xdg-data-home)))

(defun user-package ()
  "Where somebody writes their own: PINE/USER, declared in src/user.lisp.

A system loaded later puts its own vocabulary here as it loads, through
RUN/SYSTEM:SPEAKS, so what a config can name follows what is loaded."
  (find-package '#:pine/user))

(defun load-config (&optional (file (config-file)))
  (user-package)
  (when (and file (probe-file file))
    (let ((*package* (user-package))
          (*readtable* (named-readtables:find-readtable 'pine/fs/reader:syntax))
          (before (length (fault:faults))))
      (log:note "reading ~a" file)
      (fault:attempt
       (lambda ()
         (handler-bind ((sb-kernel:redefinition-with-defmethod #'muffle-warning))
           (commit:writing (load file))))
       (format nil "reading ~a" file))
      (let ((broke (- (length (fault:faults)) before)))
        (when (plusp broke)
          (log:note "~a did not read: ~a" file
                    (fault:condition-of (first (fault:faults)))))
        (zerop broke)))))

(defun quit (&optional (grace 5))
  (job:start (make-instance 'job:thread :name "quit-watchdog" :on-fault :leave
                            :runs (lambda ()
                                     (sleep grace)
                                     (sb-ext:exit :abort t :code 0))))
  (job:start (make-instance 'job:thread :name "quit" :on-fault :leave
                            :runs (lambda ()
                                     (sleep 0.2)
                                     (fault:or-nothing
                                      "leaving anyway"
                                      (stop))
                                     (sb-ext:exit :abort t :code 0))))
  t)

(defun console ()
  "The session a pine in this terminal reads its forms in: the language, the
syntax a config is read in, and where CD has moved to.

The readtable and not only the package, so /dev/audio/volume means at the prompt
what it means in the file. Without it a config taught you a spelling the prompt
answered with an error."
  (session:open-session :name "console" :in (tree:root)
                        :package (user-package)
                        :readtable (named-readtables:find-readtable
                                    'pine/fs/reader:syntax)))

(defun main (&key (store (store-file)))
  "A pine in this terminal, with no daemon: the namespace, and a repl on it."
  (start :store store)
  (let ((s (console)))
    (unwind-protect (session:interact s)
      (stop))))

(defun daemon (&key (store (store-file)) (remoting actors:*port*)
                    (config (config-file)))
  "A pine other images talk to. The store is opened after the config: a surface a
config declares takes the name it is declared under, so a panel restored before the
config would be the value node the surface then replaced.

Peers are answered last of all. What crosses that way is a read, a write and work
to do in this image, and opening it before the config has been read is answering
for a pine the person running it has not finished describing."
  (start :remoting remoting)
  (command:defcommand "quit" () (:describes "stop this pine")
                      (quit))
  (command:defcommand "reload" () (:describes "read the config again")
                      (load-config config)
                      :reloaded)
  (load-config config)
  (when store
    (store:open-store store)
    (log:note "~d node~:p came back" (store:restore store:*store*))
    (store:keeping))
  (peer:serve)
  (fault:attempt (lambda () (pine/serve/socket:open-socket))
                 "answering on a socket")
  (fault:attempt (lambda () (opening :display)) "opening the display")
  (log:note "~a: remoting ~a, ~d command~:p, ~d running"
            (node:contents (tree:at "/name"))
            (actors:remoting)
            (length (command:commands))
            (length (job:jobs)))
  (tree:root))

