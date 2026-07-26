(defpackage #:pine
  (:use :cl)
  (:export
   #:main
   #:start-daemon
   #:run-daemon
   #:stop
   ;; which frontends the daemon spawns and keeps alive; a config sets it
   #:*frontends*
   #:+frontend-unavailable+))

(in-package #:pine)

(defun main (&key (workers 4) (remoting-port 0))
  "Start the whole daemon in this image, for REPL use: (pine:main)."
  (start-daemon :workers workers :remoting-port remoting-port))

(defun start-daemon (&key (workers 4) (remoting-port 0))
  (let ((srv (pine.core.server:start-server :workers workers :remoting-port remoting-port)))
    (setf pine.core.server:*server* srv)
    (setf (pine.core.server:ts-runtime srv) (pine.ts.runtime:make-ts-runtime))
    (handler-case (pine.ts.runtime:ensure-ts (pine.core.server:ts-runtime srv))
      (error () nil))
    (pine.core.event:make-event-bus srv)
    (pine.core.actor:start-agent-registry srv)
    (pine.core.actor:start-local-agent srv)
    (pine.core.actor:start-agent-debug srv)
    (pine.text.buffer:start-buffer-registry srv)
    (pine.core.attach:start-attach-listener srv)
    (start-control srv)
    (pine.state.store:open-store)
    (pine.core.hooks:add-shutdown-hook :store
      (lambda ()
        (pine.state.world:save-world)
        (pine.editor.file:record-places)
        (pine.state.store:close-store)))
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

;;;; Frontends are agents. The editor and the desktop are not part of this
;;;; process: the daemon spawns each as its own OS image -- `pine editor' /
;;;; `pine desktop', the same binary re-invoked -- so one can crash, be killed,
;;;; or hang without touching the daemon or the other, and a supervisor respawns
;;;; any that dies. Each attaches back over remoting and renders the surfaces the
;;;; daemon builds from init.lisp. This is spawn-agent / start-agent-supervisor
;;;; (core/actor.lisp) applied to the things you look at.

(defvar *frontends* '("desktop" "editor")
  "The frontend verbs the daemon spawns and keeps alive, each its own process.
init.lisp may rebind this to choose which frontends come up.")

(defvar *frontend-procs* (make-hash-table :test 'equal)
  "frontend verb -> its uiop process, so the supervisor can see it has died.")

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

(defun spawn-frontend (verb)
  "Launch one frontend as its own process, logging to /tmp. The child is told
which daemon started it, so a daemon on any other port keeps its own frontends
instead of handing them to whoever holds the default."
  (let ((log (format nil "/tmp/pine-~a.log" verb)))
    (setf (gethash verb *frontend-procs*)
          (uiop:launch-program (frontend-command verb)
                               :environment (frontend-environment)
                               :output log :error-output log
                               :if-output-exists :supersede
                               :if-error-output-exists :supersede))))

(defvar *frontend-supervise* t
  "While true the supervisor respawns dead frontends; cleared on daemon stop so
it does not fight a deliberate shutdown.")

(defconstant +frontend-unavailable+ 70
  "Exit status a frontend uses to say it cannot run in this session.

The window manager exits with it under a compositor that offers no window
management. The supervisor then stops starting that frontend until the
display changes.")

(defvar *frontend-unavailable* (make-hash-table :test 'equal)
  "Frontend verb to the display on which it reported itself unavailable.")

(defun frontend-attached-p (verb)
  "Return the attached client of VERB's kind, whichever process started it."
  (let ((kind (intern (string-upcase verb) :keyword)))
    (find kind pine.core.attach:*clients* :key #'pine.core.attach:attached-client-kind)))

(defun note-frontend-exit (verb display)
  "Record VERB as unavailable on DISPLAY when its process said so."
  (let ((process (gethash verb *frontend-procs*)))
    (when (and process (not (uiop:process-alive-p process))
               (eql (uiop:wait-process process) +frontend-unavailable+))
      (setf (gethash verb *frontend-unavailable*) display))))

(defun frontend-runnable-p (verb display)
  "Return true when the daemon should start VERB on DISPLAY.

False when there is no display, when VERB reported itself unavailable there,
when the process the daemon started is alive, or when a frontend of that kind
is attached already."
  (let ((process (gethash verb *frontend-procs*)))
    (and display
         (not (equal (gethash verb *frontend-unavailable*) display))
         (not (and process (uiop:process-alive-p process)))
         (not (frontend-attached-p verb)))))

(defun start-frontends ()
  "Keep the frontends named by `*frontends*' running, one process each.

The decision is made every cycle rather than once, because the configuration
can change under a reload and the display can appear long after the daemon
starts. The cycle that first sees a display only observes, leaving a frontend
started elsewhere time to attach and claim its kind. Apps that have died are
reaped first, or their kind would count as attached forever and never come
back."
  (setf *frontend-supervise* t)
  (bordeaux-threads:make-thread
   (lambda ()
     (let ((previous nil))
       (loop :while *frontend-supervise*
             :do (dolist (client (pine.core.attach:reap-clients))
                   (format t "pine: ~(~a~) is gone, starting it again~%"
                           (pine.core.attach:attached-client-kind client))
                   (finish-output))
                 (let ((display (session-display)))
                   (cond
                     ((not (equal display previous))
                      (clrhash *frontend-unavailable*)
                      (setf previous display))
                     (t
                      (dolist (verb *frontends*)
                        (note-frontend-exit verb display)
                        (when (frontend-runnable-p verb display)
                          (handler-case (spawn-frontend verb)
                            (error (c)
                              (format *error-output*
                                      "pine: cannot start ~a: ~a~%" verb c))))))))
                 (sleep 3))))
   :name "pine-frontend-supervisor"))

(defun stop-frontends ()
  "Stop supervising and kill every frontend process this daemon spawned, so a
daemon shutdown takes its editor and desktop down with it."
  (setf *frontend-supervise* nil)
  (maphash (lambda (v p) (declare (ignore v))
             (ignore-errors (uiop:terminate-process p :urgent t)))
           *frontend-procs*)
  (clrhash *frontend-procs*))

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

