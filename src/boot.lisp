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
                              (#:session #:pine/run/session)
                              (#:word #:pine/word))
            (:export
   #:start #:stop #:main #:daemon #:quit
   #:use #:drop #:reach #:spawn #:style
   #:at #:read #:write #:load-config #:user-package
   #:opening))
(in-package #:pine)

(defgeneric opening (what)
            (:documentation "Open WHAT, if anything loaded here knows how.

:DISPLAY is the one pine asks for. A frontend answers it in its own file, so an
image built without one has nothing to unset and nothing to check -- there is
simply no method, which is the truth about that image.")
            (:method (what) (declare (ignore what)) nil))

(defun here () (tree:here))

(defun at (where &rest names)
  "The node WHERE names, and NAMES on from there."
  (apply #'tree:at where names))

(defun read (where &optional default)
  "What stands at WHERE, or DEFAULT where nothing does."
  (let* ((n (tree:at where))
         (value (and n (node:contents n))))
    (if (null value) default value)))

(defun write (where value)
  "Put VALUE at WHERE, making the place if nothing has been put there yet: a read
finds what is there and a write makes what is not."
  (setf (node:contents (tree:ensure where)) value))

(defun describe (where)
  (let ((n (tree:at where)))
    (when n
      (list :name (node:full-name n)
            :class (class-name (class-of n))
            :describes (node:describes n)
            :under (tree:listing n)
            :saved (node:savedp n)
            :live (node:livep n)))))

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
                    (node:full-name (here)))

(command:defcommand "ls" (&optional where) (:describes "what is under a node")
                    (let ((n (tree:at where)))
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

(command:defcommand "tree" (&optional where) (:describes "every node under one")
  (tree:paths (tree:at where)))

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
    (ui:root)
    (mount:mount #p"/" root "file"))
  (job:attend)
  (when store
    (store:open-store store)
    (store:restore store:*store*)
    (store:keeping))
  (tree:root))

(defun stop ()
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
  "Where somebody writes their own, built from what every package lent.

Nothing here says what is in it. A word is offered in the file that defines it,
so a system loaded later brings its own words and a word that is renamed or goes
takes its offer with it, rather than being dropped in silence by a list kept
somewhere else."
  (word:user))

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
           (load file)))
       (format nil "reading ~a" file))
      (let ((broke (- (length (fault:faults)) before)))
        (when (plusp broke)
          (log:note "~a did not read: ~a" file
                    (fault:condition-of (first (fault:faults)))))
        (zerop broke)))))

(defun quit (&optional (grace 5))
  (job:start (make-instance 'job:thread :name "quit-watchdog" :restarts nil
                            :thunk (lambda ()
                                     (sleep grace)
                                     (sb-ext:exit :abort t :code 0))))
  (job:start (make-instance 'job:thread :name "quit" :restarts nil
                            :thunk (lambda ()
                                     (sleep 0.2)
                                     (fault:or-nothing
                                      "leaving anyway"
                                      (stop))
                                     (sb-ext:exit :abort t :code 0))))
  t)

(defun main (&key (store (store-file)))
  "A pine in this terminal, with no daemon: the namespace, and a repl on it."
  (start :store store)
  (let ((s (session:open-session :name "console" :at (tree:root)
                                 :package (user-package))))
    (unwind-protect (session:interact s)
      (stop))))

(defun daemon (&key (store (store-file)) (remoting actors:*port*)
                    (config (config-file)))
  "A pine other images talk to. The store is opened after the config: a surface a
config declares takes the name it is declared under, so a panel restored before the
config would be the value node the surface then replaced."
  (start :remoting remoting)
  (peer:serve)
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
  (fault:attempt (lambda () (opening :display)) "opening the display")
  (log:note "~a: remoting ~a, ~d command~:p, ~d running"
            (node:contents (tree:at nil "name"))
            (actors:remoting)
            (length (command:commands))
            (length (job:jobs)))
  (tree:root))

(pine/word:lends "use" "drop" "reach" "spawn" "style" "at" "read" "write")
