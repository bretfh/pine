(defpackage #:pine
  (:use :cl)
  (:local-nicknames (#:world #:pine.state.world))
  (:export
   #:main
   #:start-daemon
   #:run-daemon
   #:stop
   ;; which frontends the daemon spawns and keeps alive; a config sets it
   #:*frontends*
   #:+frontend-unavailable+))

(in-package #:pine)
(named-readtables:in-readtable pine.path:syntax)

(defun main (&key (workers 4) (remoting-port 0))
  "Start the whole daemon in this image, for REPL use: (pine:main)."
  (start-daemon :workers workers :remoting-port remoting-port))

(defun start-daemon (&key (workers 4) (remoting-port 0))
  (let ((srv (pine.core.server:start-server :workers workers :remoting-port remoting-port)))
    (setf pine.core.server:*server* srv)
    (setf (pine.core.server:ts-runtime srv) (pine.ts.runtime:make-ts-runtime))
    (handler-case (pine.ts.runtime:ensure-ts (pine.core.server:ts-runtime srv))
      (error () nil))
    (pine.core.actor:start-agent-registry srv)
    (pine.core.actor:start-local-agent srv)
    (pine.core.actor:start-agent-debug srv)
    (pine.text.buffer:start-buffer-registry srv)
    ;; /proc is what attends everything this daemon runs, so it is mounted
    ;; before anything is declared under it
    (setf (pine.core.server:proc srv)
          (pine.proc:mount :system (pine.core.server:actor-system srv)))
    ;; /buf serves the buffers and drives their parsers: a buffer is parsed
    ;; because its lines, its window's range or its mode moved
    (pine.mode:mount)
    (pine.cmd:mount)
    (pine.editor.keymap:mount)
    (pine.term:mount-mode)
    (pine.editor.overwrite:mount-mode)
    (pine.editor.repl:mount-mode)
    (pine.editor.view:install)
    (pine.buf:mount :system (pine.core.server:actor-system srv)
                    :runtime (pine.core.server:ts-runtime srv))
    (pine.core.attach:start-attach-listener srv)
    (start-control srv)
    (world:open)
    (pine.core.hooks:add-shutdown-hook :store
      (lambda ()
        (world:save)
        (pine.editor.file:record-places)
        (world:close)))
    (load-init)
    (ignore-errors (pine.source:start-sources srv))   ; sources feed refs the desktop reads
    (format t "pine daemon ready [remoting ~a]~%" (pine.core.server:remoting-port srv))
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
     (pine.core.hooks:run-shutdown-hooks)
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
  (format t "pine daemon up on ~a:~d, frontends supervised~%"
          pine.core.server:*host* port)
  (finish-output)
  (loop (sleep 3600)))

;;;; The user's init.lisp: the config that defines their surfaces, sources,
;;;; commands, and theme. Loaded in :pine.user. An error is surfaced, not fatal:
;;;; the daemon stays up with whatever loaded before it.

(defun config-directory ()
  "Return the directory holding the user's configuration."
  (merge-pathnames "pine/" (uiop:xdg-config-home)))

(defun register-config-systems ()
  "Add the config directory to the ASDF registry.

A configuration may be a system of its own, defined in an .asd beside
init.lisp, with its own package, components and dependencies. Registering the
directory is what makes (asdf:load-system :mine) find it."
  (let ((dir (config-directory)))
    (when (probe-file dir)
      (pushnew dir asdf:*central-registry* :test #'equal))))

(defun load-init ()
  "Load the configuration's entry point, init.lisp, in the PINE.USER package.

The entry point may hold the whole configuration or load a system that does.
An error is reported and the daemon keeps whatever loaded before it."
  (register-config-systems)
  (let ((path (merge-pathnames "init.lisp" (config-directory))))
    (when (probe-file path)
      (handler-case
          (let ((*package* (find-package :pine.user)))
            (load path)
            (format t "pine: loaded ~a~%" path))
        (error (e)
          (format *error-output* "~&pine: init.lisp error: ~a~%" e))))))

;;;; The control endpoint. The CLI connects, asks one message, prints, exits.

(defun start-control (server)
  (sento.actor-context:actor-of (pine.core.server:actor-system server) :name "control"
    :dispatcher :pinned
    :receive
    (lambda (msg)
      (flet ((r (x) (sento.actor:reply x)))
        (handler-case
            (case (first msg)
              (:status
               (r (format nil "pine up  port ~a  agents ~d"
                          (pine.core.server:remoting-port server)
                          (length (pine.core.actor:list-agents server)))))
              (:eval
               (let ((*package* (find-package :pine.user)))
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
              (:reload (load-init)
                       (ignore-errors (pine.desktop:refresh-all))
                       (ignore-errors (pine.editor.session:reseed-editor-sessions))
                       (r "reloaded"))
              (:agents (r (mapcar #'pine.core.actor:agent-info-name (pine.core.actor:list-agents server))))
              (:spawn (pine.core.actor:spawn-agent server (second msg)) (r "spawned"))
              (:kill  (pine.core.actor:kill-agent server (second msg)) (r "killed"))
              (:surface
               (destructuring-bind (&key op name) (rest msg)
                 (dolist (c pine.core.attach:*clients*)
                   (when (eq (pine.core.attach:attached-client-kind c) :desktop)
                     (ecase op
                       (:show   (pine.desktop:show-panel c name))
                       (:hide   (pine.desktop:hide-panel c name))
                       (:toggle (pine.desktop:show-panel c name)))))
                 (r "ok")))
              (t (r (list :unknown (first msg)))))
          (error (e) (r (format nil "error: ~a" e))))))))

;;;; The CLI: pine VERB ARGS. Control verbs ask the running daemon and print;
;;;; start boots it. Not a REPL.

(defparameter *cli-usage*
  "usage: pine {start | stop | restart | daemon | editor | desktop | wm |
             session [DISPLAY] | status | eval FORM | reload | agents |
             spawn NAME | kill NAME | show|hide|toggle NAME}")

(defun cli-request (msg &key (host pine.core.server:*host*) (port pine.core.server:*port*))
  "Connect, ask the daemon's control actor, print, return. The process
exits after, so the ephemeral actor system needs no teardown."
  (let ((sys (sento.actor-system:make-actor-system
              '(:dispatchers (:shared (:workers 1 :strategy :random))))))
    (sento.remoting:enable-remoting sys :host pine.core.server:*host* :port 0)
    (handler-case
        (let ((ref (sento.remoting:make-remote-ref
                    sys (pine.core.server:daemon-uri "control" :host host :port port))))
          (format t "~a~%" (pine.core.actor:ask ref msg :timeout 5)))
      (error () (format t "pine: no daemon at ~a:~d~%" host port)))))

;;;; Frontends. The editor and the desktop are not part of this process: each
;;;; is its own OS image -- `pine editor' / `pine desktop', the same binary
;;;; re-invoked -- so one can crash, be killed, or hang without touching the
;;;; daemon or the other. Each attaches back over remoting and renders the
;;;; surfaces the daemon builds from init.lisp.
;;;;
;;;; They are declarations under /proc like anything else that runs.

(defvar *frontends* '("desktop" "editor")
  "The frontend verbs the daemon keeps alive, each its own process.
init.lisp may rebind this to choose which frontends come up.")

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
            "--eval" (format nil "(pine::run-app ~s)" verb))))

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

(defun declare-frontends ()
  "Declare the frontends under /proc, and say what makes each one runnable.

Writing the declaration is what keeps it running. The two rules that are not
just liveness are said as paths: it needs a display to run on, and it does not
run when a frontend of its kind has attached from somewhere else."
  (dolist (verb *frontends*)
    (let ((kind (string-downcase verb)))
      (pine.ns:write (pine.path:child /proc kind)
                     (fset:map (:run (fset:convert 'fset:seq (frontend-command verb)))
                               (:env (fset:convert 'fset:seq (frontend-environment)))
                               (:needs (fset:seq /display))
                               (:unless (fset:seq (pine.path:child /attached kind))))))))

(defun watch-unavailable ()
  "A frontend that says it cannot run here is not started again until the
display changes."
  (pine.ns:watch /proc
                 (lambda (value)
                   (declare (ignore value))
                   (let ((stop (fset:empty-map)))
                     (dolist (verb *frontends* stop)
                       (let ((at (pine.path:child /proc (string-downcase verb))))
                         (when (eql +frontend-unavailable+
                                    (pine.ns:read (pine.path:child at "exit")))
                           (setf stop (fset:with stop at (fset:seq :stop))))))))
                 :as :frontend-unavailable)
  (pine.ns:watch /display
                 (lambda (value)
                   (declare (ignore value))
                   (let ((start (fset:empty-map)))
                     (dolist (verb *frontends* start)
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
  (dolist (verb *frontends*)
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

(defun cli-stop (&key (port pine.core.server:*port*))
  "Stop the daemon: ask it to shut down (taking its frontends with it), then make
sure the port is free even if it was an old or wedged daemon."
  (ignore-errors
    (let ((sys (sento.actor-system:make-actor-system
                '(:dispatchers (:shared (:workers 1 :strategy :random))))))
      (sento.remoting:enable-remoting sys :host pine.core.server:*host* :port 0)
      (ignore-errors
        (pine.core.actor:ask
         (sento.remoting:make-remote-ref sys (pine.core.server:daemon-uri "control" :port port))
         '(:stop) :timeout 3))))
  (sleep 0.4)
  (unless (port-free-p port) (kill-port port))
  (format t "pine: stopped~%"))

(defun cli-restart (&key (port pine.core.server:*port*))
  "Stop the daemon, wait for the port to free, then start it fresh."
  (cli-stop :port port)
  (loop repeat 40 until (port-free-p port) do (sleep 0.25))
  (run-all))

(defun cli (&optional (args (rest sb-ext:*posix-argv*)))
  ;; a CLI prints its answer, nothing else: quiet sento/log4cl's INFO chatter
  ;; (actor-system config, "Remoting enabled on ...") that otherwise buries it.
  (log:config :error)
  (let ((verb (first args)) (more (rest args)))
    (cond
      ((null verb) (format t "~a~%" *cli-usage*))
      ((string= verb "start")  (run-all))
      ((string= verb "stop")    (cli-stop))
      ((string= verb "restart") (cli-restart))
      ((string= verb "daemon") (run-daemon))
      ((member verb '("editor" "desktop" "wm") :test #'string=) (run-app verb))
      ((string= verb "status") (cli-request '(:status)))
      ((string= verb "eval")   (cli-request (list :eval (format nil "~{~a~^ ~}" more))))
      ((string= verb "session")
       (cli-request (list :session (or (first more)
                                       (uiop:getenv "WAYLAND_DISPLAY")))))
      ((string= verb "reload") (cli-request '(:reload)))
      ((string= verb "agents") (cli-request '(:agents)))
      ((string= verb "spawn")  (cli-request (list :spawn (first more))))
      ((string= verb "kill")   (cli-request (list :kill (first more))))
      ((member verb '("show" "hide" "toggle") :test #'string=)
       (cli-request (list :surface :op (intern (string-upcase verb) :keyword)
                          :name (string-downcase (first more)))))
      (t (format t "pine: unknown verb ~a~%~a~%" verb *cli-usage*)))))

(defun stop ()
  (pine.core.hooks:run-shutdown-hooks))

