(defpackage #:pine
  (:use :cl)
  (:export
   #:main
   #:start-daemon
   #:run-daemon
   #:run-all
   #:run-app
   #:frontend
   #:stop
   ;; what the daemon cannot do to itself, so the CLI does it
   #:port-free-p
   #:kill-port
   #:load-config
   #:+frontend-unavailable+))

(in-package #:pine)
(named-readtables:in-readtable pine.path:syntax)


;;;; Frontends. The editor and the desktop are not part of this process: each
;;;; is its own OS image -- `pine editor' / `pine desktop', the same binary
;;;; re-invoked -- so one can crash, be killed, or hang without touching the
;;;; daemon or the other. Each attaches back over remoting and renders the
;;;; surfaces the daemon builds from init.lisp.
;;;;
;;;; They are declarations under /proc like anything else that runs.

(defparameter +frontends+ '("desktop" "editor")
  "The frontend verbs pine ships, each its own process.

Which of them a machine runs is not decided here: they are declarations under
/proc, so a config that wants one and not the other writes (write /proc/desktop
nil), the way it would stop anything else.")

(defun daemon-is-binary-p ()
  "True when this daemon runs from the built `pine' binary, which can re-invoke
itself with a verb. Under `make daemon' argv0 is sbcl, which cannot."
  (let ((self (first sb-ext:*posix-argv*)))
    (and self (not (search "sbcl" (namestring self))))))

(defun frontend-command (verb)
  "The command that starts frontend VERB as its own process. From the binary
that is the binary and the verb. From source it is this same sbcl run again on
the wayland system: argv0 is sbcl and takes no verb, but the child inherits the
environment that made this image loadable, so it can load what this one did."
  (if (daemon-is-binary-p)
      (list (first sb-ext:*posix-argv*) verb)
      (list (namestring sb-ext:*runtime-pathname*)
            "--no-userinit" "--non-interactive"
            "--eval" "(require :asdf)"
            "--eval" "(asdf:load-system :pine/wayland)"
            "--eval" (format nil "(pine:run-app ~s)" verb))))

(defun frontend-environment ()
  "This daemon's environment, with PINE_PORT naming the port it listens on."
  (let ((prefix "PINE_PORT="))
    (cons (format nil "~a~d" prefix pine.core.server:*port*)
          (remove-if (lambda (entry)
                       (and (>= (length entry) (length prefix))
                            (string= prefix entry :end2 (length prefix))))
                     (sb-ext:posix-environ)))))

(defconstant +frontend-unavailable+ 70
  "Exit status a frontend uses to say it cannot run in this session.

The window manager exits with it under a compositor that offers no window
management. Nothing starts that frontend again until the display changes.")

(defun frontend (verb)
  "The /proc declaration that keeps frontend VERB running: what to run, the
environment to run it in, and the two rules that are not just liveness -- it
needs a display, and it does not run when a frontend of its kind has attached
from somewhere else. A config declaring one pine does not ship writes
(write /proc/wm (frontend \"wm\"))."
  (let ((kind (string-downcase verb)))
    (fset:map (:run (fset:convert 'fset:seq (frontend-command verb)))
              (:env (fset:convert 'fset:seq (frontend-environment)))
              (:needs (fset:seq /display))
              (:unless (fset:seq (pine.path:child /attached kind))))))

(defun declare-frontends ()
  "Declare the frontends pine ships, unless the config already said. It is
loaded before this runs, so what it declared -- another command, another
frontend, or nothing at all -- is what stands."
  (dolist (verb +frontends+)
    (let ((at (pine.path:child /proc (string-downcase verb))))
      (unless (pine.ns:held at)
        (pine.ns:write at (frontend verb))))))

(defun watch-unavailable ()
  "A frontend that says it cannot run here is not started again until the
display changes."
  (pine.ns:watch /proc
                 (lambda (value)
                   (declare (ignore value))
                   (let ((stop (fset:empty-map)))
                     (dolist (verb +frontends+ stop)
                       (let ((at (pine.path:child /proc (string-downcase verb))))
                         (when (eql +frontend-unavailable+
                                    (pine.ns:read (pine.path:child at "exit")))
                           (setf stop (fset:with stop at (fset:seq :stop))))))))
                 :as :frontend-unavailable)
  (pine.ns:watch /display
                 (lambda (value)
                   (declare (ignore value))
                   (let ((start (fset:empty-map)))
                     (dolist (verb +frontends+ start)
                       (setf start (fset:with start
                                              (pine.path:child /proc
                                                               (string-downcase verb))
                                              (fset:seq :start))))))
                 :as :frontend-display))

(defun declare-reaper ()
  "Remoting says nothing when a peer dies, so an app that was killed would hold
its kind forever and the :unless rule above would never let another start. The
check is a socket connect, so it is declared on an interval like anything else
that runs."
  (pine.ns:write /proc/reap
                 (fset:map (:every 5)
                           (:thread (lambda () (pine.core.attach:reap-clients))))))

(defun start-frontends ()
  "Keep the frontends running, one process each, attended with everything else."
  (pine.ns:write /display (session-display))
  (declare-frontends)
  (declare-reaper)
  (watch-unavailable))

(defun stop-frontends ()
  "Stop every frontend this daemon declared, so a daemon shutdown takes its
editor and desktop down with it."
  (dolist (verb +frontends+)
    (pine.ns:write (pine.path:child /proc (string-downcase verb)) nil)))

(defun kill-port (port)
  "Kill whatever process holds PORT. Version-independent, so `pine stop' works
even on an old or wedged daemon that has no clean :stop."
  (ignore-errors
    (uiop:run-program (list "fuser" "-k" (format nil "~d/tcp" port))
                      :ignore-error-status t :output nil :error-output nil)))

(defun port-free-p (port)
  "True when nothing listens on PORT (the daemon is down)."
  (multiple-value-bind (o e code)
      (ignore-errors (uiop:run-program (list "fuser" (format nil "~d/tcp" port))
                                       :ignore-error-status t :output nil :error-output nil))
    (declare (ignore o e))
    (not (eql code 0))))                     ; fuser: 0 = held, nonzero = free

(defun stop ()
  "Take the daemon down: the hooks first, so what wants a last write gets one,
then everything it serves.

What comes off is what the registry says went up, so a subsystem added since
cannot be left running by a shutdown that never heard of it. /proc goes with
it, which is what stops the frontends and everything else declared."
  (pine.core.hooks:run-shutdown-hooks)
  (pine.ns:lower-all))



(defun main (&key (workers pine.core.server:*workers*) (remoting-port 0))
  "Start the whole daemon in this image, for REPL use: (pine:main)."
  (start-daemon :workers workers :remoting-port remoting-port))

(defun start-daemon (&key (workers pine.core.server:*workers*) (remoting-port 0)
                          (config (config-directory)))
  (let ((srv (pine.core.server:start-server :workers workers :remoting-port remoting-port)))
    (setf pine.core.server:*server* srv)
    (setf (pine.core.server:ts-runtime srv) (pine.ts.runtime:make-ts-runtime))
    (handler-case (pine.ts.runtime:ensure-ts (pine.core.server:ts-runtime srv))
      (error () nil))
    (pine.core.actor:start-agent-registry srv)
    (pine.core.actor:start-local-agent srv)
    (pine.core.actor:start-agent-debug srv)
    ;; everything pine serves, in the order the registry says. There is no
    ;; list here to fall behind the one the tests raise.
    (let ((up (pine.ns:raise-all
               :system (pine.core.server:actor-system srv)
               :runtime (pine.core.server:ts-runtime srv))))
      (setf (pine.core.server:proc srv) (fset:lookup up :proc)
            (pine.core.server:store srv) (fset:lookup up :store)))
    ;; the clock an :every write asks for. The namespace says what was asked;
    ;; the thing with a scheduler is what asks it again
    (setf (pine.ns:on-interval)
          (let ((timer (sento.actor-system:scheduler
                        (pine.core.server:actor-system srv))))
            (lambda (path seconds)
              (let ((key (list :every (pine.path:text path))))
                (ignore-errors (sento.wheel-timer:cancel timer key))
                (sento.wheel-timer:schedule-recurring
                 timer seconds seconds
                 (lambda ()
                   (pine.err:attempt (lambda () (pine.ns:again path))
                                     (format nil "~a every ~ds"
                                             (pine.path:text path) seconds)))
                 key)))))
    (pine.core.attach:start-attach-listener srv)
    (start-control srv)
    (pine.core.hooks:add-shutdown-hook :store
      (lambda () (pine.buf:record-places)))
    (load-init config)
    (pine.log:note "daemon ready [remoting ~a]" (pine.core.server:remoting-port srv))
    srv))

(defun %setenv (name value)
  (cffi:foreign-funcall "setenv" :string name :string value :int 1 :int))

(defun session-display ()
  "Return the wayland display the daemon may start frontends on, or NIL.

Only what the environment says. The daemon never picks a display for itself:
a guess can land on a compositor that is not this session's, and frontends
would open somewhere nobody asked for. A session announces itself with
`pine session'."
  (let ((display (uiop:getenv "WAYLAND_DISPLAY")))
    (when (and display (plusp (length display))) display)))

(defun set-session-display (display)
  "Adopt DISPLAY as the session's, for frontends started from now on."
  (when (and display (plusp (length display)))
    (%setenv "WAYLAND_DISPLAY" display)
    (pine.ns:write /display display)
    display))

(defun handle-termination ()
  "Take the frontends down with the daemon when the supervisor stops it.

`pine stop' does this through the control actor, but a service manager sends a
signal, and a daemon that exits on one leaves its frontends attached to nothing
and outliving every restart."
  (sb-sys:enable-interrupt
   sb-unix:sigterm
   (lambda (&rest args)
     (declare (ignore args))
     (handler-case (stop-frontends)
       (error (c) (format *error-output* "pine: ~a~%" c)))
     (stop)
     (sb-ext:exit :code 0 :abort t))))

(defun daemon-listening-p (port)
  (ignore-errors
    (let ((s (usocket:socket-connect pine.core.server:*host* port :timeout 1)))
      (usocket:socket-close s) t)))

(defun run-app (verb)
  "Run the frontend VERB names, in this image.

The daemon registers one app per kind at startup; the backing defines how each
one runs. Without a backing the app says so rather than the CLI guessing."
  (let ((app (pine.core.attach:find-app (intern (string-upcase verb) :keyword))))
    (if app
        (pine.core.attach:run-frontend app)
        (format t "pine: no ~a app~%" verb))))

(defun run-all (&key (port pine.core.server:*port*))
  "`pine start': the shim. Kick the daemon in its own process if it is not already
up, then return. The daemon reads init.lisp and spawns + supervises the frontends
(editor + desktop) as their own processes -- this shim renders nothing itself, so
the daemon, the editor, and the desktop are three separate images."
  (if (daemon-listening-p port)
      (format t "pine: daemon already up on ~a:~d~%" pine.core.server:*host* port)
      (progn
        (let ((self (first sb-ext:*posix-argv*)))
          (uiop:launch-program (list self "daemon")
                               :output "/tmp/pine-daemon.log"
                               :error-output "/tmp/pine-daemon.log"
                               :if-output-exists :supersede
                               :if-error-output-exists :supersede))
        (loop repeat 120 when (daemon-listening-p port) do (return)
              do (sleep 0.5)
              finally (format t "pine: daemon did not come up (log /tmp/pine-daemon.log)~%")
                      (return-from run-all))
        (format t "pine: daemon up on ~a:~d -- editor + desktop spawning.~%"
                pine.core.server:*host* port)))
  (finish-output))

(defun run-daemon (&key (port pine.core.server:*port*))
  (setf pine.core.server:*port* port)
  (start-daemon :remoting-port port)
  (handle-termination)
  (start-frontends)
  (pine.log:note "daemon up on ~a:~d, frontends supervised"
                 pine.core.server:*host* port)
  (loop (sleep 3600)))

;;;; The user's init.lisp: the config that defines their surfaces, sources,
;;;; commands, and theme. Loaded in :pine.user. An error is surfaced, not fatal:
;;;; the daemon stays up with whatever loaded before it.

(defun config-directory ()
  "Where a configuration lives, when the caller does not say."
  (merge-pathnames "pine/" (uiop:xdg-config-home)))

(defun register-config-systems (dir)
  "Add DIR to the ASDF registry, so a configuration that is a system of its own
is found by (asdf:load-system :mine)."
  (when (probe-file dir)
    (pushnew dir asdf:*central-registry* :test #'equal)))

(defun load-config (path)
  "Load PATH as a configuration: in PINE.USER, and in pine's own readtable, so
a config does not have to ask for the language it is written in."
  (let ((*package* (find-package :pine.user))
        (*readtable* (named-readtables:find-readtable 'pine.path:syntax)))
    (load path)))

(defun load-init (&optional (dir (config-directory)))
  "Load DIR's init.lisp. An error is reported and the daemon keeps whatever
loaded before it."
  (register-config-systems dir)
  (let ((path (merge-pathnames "init.lisp" dir)))
    (when (probe-file path)
      (handler-case
          (progn (load-config path)
                 (pine.log:note "loaded ~a" path))
        (error (e)
          (pine.log:note "init.lisp error: ~a" e))))))

;;;; The control endpoint. The CLI connects, asks one message, prints, exits.
;;;;
;;;; What crosses is text: a path is text, and a value goes over as readable
;;;; lisp, so the CLI needs nothing of this image but the socket.

(defun %said (value)
  "VALUE as the CLI will print it."
  (pine.data:serialize value))

(defun %heard (text)
  "The value TEXT says."
  (pine.data:deserialize text 'pine.path:data))

(defun %watcher (uri)
  "The remote the CLI is listening on, or NIL when it has gone."
  (ignore-errors
    (sento.remoting:make-remote-ref
     (pine.core.server:actor-system pine.core.server:*server*) uri)))

(defun %watch-for (server text uri)
  "Watch what TEXT names and tell URI each change, until the telling fails.

The watch is named by the uri, so a CLI that asks twice replaces its own rather
than doubling it, and a `pine watch' that goes away takes its watch with it."
  (declare (ignore server))
  (let ((at (pine.path:parse text))
        (ref (%watcher uri)))
    (when ref
      (pine.ns:watch
       at
       (lambda (value)
         (handler-case (sento.actor:tell ref (list :moved
                                                   (pine.path:text (pine.ns:here))
                                                   (%said value)))
           (error () (pine.ns:watch at nil :as uri)))
         nil)
       :as uri)
      "watching")))

(defun start-control (server)
  (sento.actor-context:actor-of (pine.core.server:actor-system server) :name "control"
    :dispatcher :pinned
    :receive
    (lambda (msg)
      (flet ((r (x) (sento.actor:reply x)))
        (handler-case
            (case (first msg)
              ;; the three verbs, against the running daemon
              (:read (r (%said (pine.ns:read (pine.path:parse (second msg))))))
              (:write (pine.ns:write (pine.path:parse (second msg))
                                     (%heard (third msg))
                                     :force (fourth msg))
                      (r "ok"))
              ;; what a write to this path would touch, so the caller can say
              ;; where it lands before it lands
              (:matches (r (mapcar #'pine.path:text
                                   (pine.ns:matches
                                    (pine.path:parse (second msg))))))
              (:watch (r (or (%watch-for server (second msg) (third msg))
                             "nowhere to send it")))
              (:diff (r (%said (pine.ns:diff (pine.path:parse (second msg))
                                             (pine.path:parse (third msg))))))
              (:status
               (r (format nil "pine up  port ~a  agents ~d"
                          (pine.core.server:remoting-port server)
                          (length (pine.core.actor:list-agents server)))))
              ;; the same language a config is written in, so a form typed at
              ;; the shell may say /audio/volume like one in init.lisp
              (:eval
               (let ((*package* (find-package :pine.user))
                     (*readtable* (named-readtables:find-readtable 'pine.path:syntax)))
                 (r (princ-to-string (eval (read-from-string (second msg)))))))
              (:stop
               (r "stopping")
               ;; reply first, then tear down off the actor thread: kill the
               ;; frontends this daemon spawned, then exit the image.
               (bordeaux-threads:make-thread
                (lambda ()
                  (ignore-errors (stop-frontends))
                  (sleep 0.2)
                  (sb-ext:exit :code 0 :abort t))
                :name "pine-shutdown"))
              (:session
               (r (or (set-session-display (second msg))
                      "no display given")))
              ;; nothing here tells anyone what moved: a config is writes, and
              ;; what watches those paths hears about them
              (:reload (load-init)
                       (ignore-errors (pine.desktop:refresh-all))
                       (r "reloaded"))
              (:agents (r (mapcar #'pine.core.actor:agent-info-name (pine.core.actor:list-agents server))))
              (:spawn (pine.core.actor:spawn-agent server (second msg)) (r "spawned"))
              (:kill  (pine.core.actor:kill-agent server (second msg)) (r "killed"))
              (t (r (list :unknown (first msg)))))
          (error (e) (r (format nil "error: ~a" e))))))))