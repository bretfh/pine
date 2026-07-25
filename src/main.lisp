(in-package :pine)

(defun main (&key (workers 4) (remoting-port 0))
  "Start the whole daemon in this image, for REPL use: (pine:main)."
  (start-daemon :workers workers :remoting-port remoting-port))

(defun start-daemon (&key (workers 4) (remoting-port 0))
  (let ((srv (pine.server:start-server :workers workers :remoting-port remoting-port)))
    (setf pine.server:*server* srv)
    (setf (pine.server:ts-runtime srv) (pine.ts:make-ts-runtime))
    (handler-case (pine.ts:ensure-ts (pine.server:ts-runtime srv))
      (error () nil))
    (pine.event:make-event-bus srv)
    (pine.actor:start-agent-registry srv)
    (pine.actor:start-local-agent srv)
    (pine.actor:start-agent-debug srv)
    (pine.buffer:start-buffer-registry srv)
    (pine.attach:start-attach-listener srv)
    (start-control srv)
    (pine.buffer:install-default-faces)
    (pine.mode:install-default-modes)
    (pine.editor:install-commands)
    (pine.editor:install-bindings)
    (pine.editor:install-editor-sessions)
    (pine.desktop:install-desktop-sessions)
    (pine.wm:install-wm-sessions)
    (pine.store:open-store)
    (pine.hooks:add-shutdown-hook :store
      (lambda ()
        (pine.world:save-world)
        (pine.file:record-places)
        (pine.store:close-store)))
    (load-init)
    (pine.jobs:install-jobs)
    (ignore-errors (pine.source:start-sources srv))   ; sources feed refs the desktop reads
    (format t "pine daemon ready [remoting ~a]~%" (pine.server:remoting-port srv))
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

(defun run-daemon (&key (port pine.server:*port*))
  (start-daemon :remoting-port port)
  (start-frontends)
  (format t "pine daemon up on ~a:~d, frontends supervised~%"
          pine.server:*host* port)
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
  (sento.actor-context:actor-of (pine.server:actor-system server) :name "control"
    :dispatcher :pinned
    :receive
    (lambda (msg)
      (flet ((r (x) (sento.actor:reply x)))
        (handler-case
            (case (first msg)
              (:status
               (r (format nil "pine up  port ~a  agents ~d"
                          (pine.server:remoting-port server)
                          (length (pine.actor:list-agents server)))))
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
                       (ignore-errors (pine.editor:reseed-editor-sessions))
                       (r "reloaded"))
              (:agents (r (mapcar #'pine.actor:agent-info-name (pine.actor:list-agents server))))
              (:spawn (pine.actor:spawn-agent server (second msg)) (r "spawned"))
              (:kill  (pine.actor:kill-agent server (second msg)) (r "killed"))
              (:surface
               (destructuring-bind (&key op name) (rest msg)
                 (dolist (c pine.attach:*clients*)
                   (when (eq (pine.attach:attached-client-kind c) :desktop)
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

(defun cli-request (msg &key (host pine.server:*host*) (port pine.server:*port*))
  "Connect, ask the daemon's control actor, print, return. The process
exits after, so the ephemeral actor system needs no teardown."
  (let ((sys (sento.actor-system:make-actor-system
              '(:dispatchers (:shared (:workers 1 :strategy :random))))))
    (sento.remoting:enable-remoting sys :host pine.server:*host* :port 0)
    (handler-case
        (let ((ref (sento.remoting:make-remote-ref
                    sys (pine.server:daemon-uri "control" :host host :port port))))
          (format t "~a~%" (sento.actor:ask-s ref msg :time-out 5)))
      (error () (format t "pine: no daemon at ~a:~d~%" host port)))))

(defvar *start-hook* nil
  "Set by the frontend layer (:pine/wayland) to a function that starts the daemon
AND renders the surfaces. When bound, `pine start' brings up the whole desktop;
otherwise it starts the headless daemon only.")

(defvar *editor-hook* nil
  "Set by the frontend layer to the editor frontend runner; `pine editor' calls
it. The editor attaches to the running daemon as its own process.")

(defvar *desktop-hook* nil
  "Set by the frontend layer to the desktop frontend runner; `pine desktop' calls
it. The desktop attaches to the running daemon as its own process.")

(defvar *wm-hook* nil
  "Set by the frontend layer to the river window manager runner; `pine wm'
calls it. Only meaningful under river (design/wm.org).")

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

(defvar *frontend-forms*
  '(("editor"  . "(pine.wl-editor:run-editor)")
    ("desktop" . "(pine.wayland:run-desktop)")
    ("wm"      . "(pine.wl-wm:run-wm)"))
  "The form that runs each frontend, for a daemon started from source. The
built binary takes the verb instead.")

(defun frontend-command (verb)
  "The command that starts frontend VERB as its own process. From the binary
that is the binary and the verb. From source it is this same sbcl run again on
the wayland system: argv0 is sbcl and takes no verb, but the child inherits the
environment that made this image loadable, so it can load what this one did."
  (if (daemon-is-binary-p)
      (list (first sb-ext:*posix-argv*) verb)
      (let ((form (cdr (assoc verb *frontend-forms* :test #'string=))))
        (unless form
          (error "no way to start frontend ~s from source" verb))
        (list (namestring sb-ext:*runtime-pathname*)
              "--no-userinit" "--non-interactive"
              "--eval" "(require :asdf)"
              "--eval" "(asdf:load-system :pine/wayland)"
              "--eval" form))))

(defun spawn-frontend (verb)
  "Launch one frontend as its own process, logging to /tmp."
  (let ((log (format nil "/tmp/pine-~a.log" verb)))
    (setf (gethash verb *frontend-procs*)
          (uiop:launch-program (frontend-command verb)
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
    (find kind pine.attach:*clients* :key #'pine.attach:attached-client-kind)))

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
started elsewhere time to attach and claim its kind."
  (setf *frontend-supervise* t)
  (bordeaux-threads:make-thread
   (lambda ()
     (let ((previous nil))
       (loop :while *frontend-supervise*
             :do (let ((display (session-display)))
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

(defun cli-stop (&key (port pine.server:*port*))
  "Stop the daemon: ask it to shut down (taking its frontends with it), then make
sure the port is free even if it was an old or wedged daemon."
  (ignore-errors
    (let ((sys (sento.actor-system:make-actor-system
                '(:dispatchers (:shared (:workers 1 :strategy :random))))))
      (sento.remoting:enable-remoting sys :host pine.server:*host* :port 0)
      (ignore-errors
        (sento.actor:ask-s
         (sento.remoting:make-remote-ref sys (pine.server:daemon-uri "control" :port port))
         '(:stop) :time-out 3))))
  (sleep 0.4)
  (unless (port-free-p port) (kill-port port))
  (format t "pine: stopped~%"))

(defun cli-restart (&key (port pine.server:*port*))
  "Stop the daemon, wait for the port to free, then start it fresh."
  (cli-stop :port port)
  (loop repeat 40 until (port-free-p port) do (sleep 0.25))
  (if *start-hook* (funcall *start-hook*) (run-daemon)))

(defun cli (&optional (args (rest sb-ext:*posix-argv*)))
  ;; a CLI prints its answer, nothing else: quiet sento/log4cl's INFO chatter
  ;; (actor-system config, "Remoting enabled on ...") that otherwise buries it.
  (log:config :error)
  (let ((verb (first args)) (more (rest args)))
    (cond
      ((null verb) (format t "~a~%" *cli-usage*))
      ((string= verb "start")  (if *start-hook* (funcall *start-hook*) (run-daemon)))
      ((string= verb "stop")    (cli-stop))
      ((string= verb "restart") (cli-restart))
      ((string= verb "daemon") (run-daemon))
      ((string= verb "editor") (if *editor-hook* (funcall *editor-hook*)
                                   (format t "pine: no editor frontend in this build~%")))
      ((string= verb "desktop") (if *desktop-hook* (funcall *desktop-hook*)
                                    (format t "pine: no desktop frontend in this build~%")))
      ((string= verb "wm") (if *wm-hook* (funcall *wm-hook*)
                               (format t "pine: no wm frontend in this build~%")))
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
  (pine.hooks:run-shutdown-hooks))

