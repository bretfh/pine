(defpackage #:pine
  (:use #:cl)
  (:shadow #:describe #:read #:write)
  (:shadowing-import-from #:pine/data #:map #:set)
  (:import-from #:pine/data #:seq)
  (:import-from #:pine/fs/log #:note)
  (:import-from #:pine/fs/mount #:mount)
  (:import-from #:pine/fs/node
   #:contents #:derived #:describes #:name #:node #:place #:value)
  (:import-from #:pine/fs/tree #:ensure #:erase #:listing #:root)
  (:import-from #:pine/run/command #:defcommand #:run)
  (:import-from #:pine/run/fault #:attempt)
  (:import-from #:pine/run/job #:start #:stop)
  (:import-from #:pine/run/peer #:reach #:serve)
  (:import-from #:pine/run/system #:drop #:puts #:system #:use)
  (:import-from #:pine/run/watch #:unwatch)
  (:local-nicknames (#:d #:pine/data)
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
   #:boot #:leave #:main #:daemon #:quit #:console #:opening #:load-config #:spawn
   #:at #:read #:write #:watch #:ls #:standsp #:toggle #:include #:exclude #:blend
   #:use #:drop #:reach #:serve
   #:seq #:map #:set #:note #:mount
   #:contents #:derived #:describes #:name #:node #:place #:value
   #:ensure #:erase #:listing #:root
   #:defcommand #:run #:attempt #:start #:stop #:puts #:system #:unwatch))
(in-package #:pine)

(defgeneric opening (what)
            (:documentation "Open WHAT, if anything loaded here knows how.

:DISPLAY is the one pine asks for. A frontend answers it in its own file, so an
image built without one has nothing to unset and nothing to check -- there is
simply no method, which is the truth about that image.")
            (:method (what) (declare (ignore what)) nil))

(defun spawn (name &key (systems '(:pine)))
  "Another lisp of pine's own, supervised. Work can be done in it, and a fault it
stands in comes back here with the restarts it is still offering."
  (let ((j (make-instance 'image:child :name (princ-to-string name)
                          :systems systems)))
    (job:supervise j)
    (job:start j)
    j))

(defun boot (&key (name "pine") store remoting)
  (libs:attend)
  (tree:make-root)
  (unless (actors:runningp) (actors:boot :remoting remoting))
  (let ((root (tree:root)))
    (setf (node:contents (tree:ensure root "name")) name)
    (setf (node:contents (tree:ensure root "port")) (actors:remoting))
    (tree:built root)
    (tree:ensure "/surface")
    (mount:mount #p"/" root "file"))
  (job:attend)
  (when store
    (store:open-store store)
    (store:restore store:*store*)
    (store:keeping))
  (tree:root))

(defun leave ()
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

(defun load-config (&optional (file (config-file)))
  (find-package '#:pine/user)
  (when (and file (probe-file file))
    (let ((*package* (find-package '#:pine/user))
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
                                      (leave))
                                     (sb-ext:exit :abort t :code 0))))
  t)

(defun console ()
  "The session a pine in this terminal reads its forms in: the language, the
syntax a config is read in, and where CD has moved to.

The readtable and not only the package, so /dev/audio/volume means at the prompt
what it means in the file. Without it a config taught you a spelling the prompt
answered with an error."
  (session:open-session :name "console" :in (tree:root)
                        :package (find-package '#:pine/user)
                        :readtable (named-readtables:find-readtable
                                    'pine/fs/reader:syntax)))

(defun main (&key (store (store-file)))
  "A pine in this terminal, with no daemon: the namespace, and a repl on it."
  (boot :store store)
  (let ((s (console)))
    (unwind-protect (session:interact s)
      (leave))))

(defun daemon (&key (store (store-file)) (remoting actors:*port*)
                    (config (config-file)))
  "A pine other images talk to. The store is opened after the config: a surface a
config declares takes the name it is declared under, so a panel restored before the
config would be the value node the surface then replaced.

Peers are answered last of all. What crosses that way is a read, a write and work
to do in this image, and opening it before the config has been read is answering
for a pine the person running it has not finished describing.

REMOTING is the port other pines reach this one on. A daemon has one because that
is what makes it a daemon: REACH, MOUNT and evaluating in another image are the
whole of what a peer is, and none of them can happen to a pine nothing can get to.
Say NIL for one that answers only on its socket."
  (boot :remoting remoting)
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

