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

(defun discover-session-env ()
  (let* ((dir (or (uiop:getenv "XDG_RUNTIME_DIR") "/run/user/1000"))
         (names (ignore-errors (uiop:run-program (list "ls" "-t" dir) :output :lines))))
    (unless (uiop:getenv "WAYLAND_DISPLAY")
      (let ((wl (find-if (lambda (n) (and (uiop:string-prefix-p "wayland-" n)
                                          (not (uiop:string-suffix-p ".lock" n))))
                         names)))
        (when wl (%setenv "WAYLAND_DISPLAY" wl))))
    (unless (uiop:getenv "NIRI_SOCKET")
      (let ((sock (find-if (lambda (n) (and (uiop:string-prefix-p "niri." n)
                                            (uiop:string-suffix-p ".sock" n)))
                           names)))
        (when sock (%setenv "NIRI_SOCKET" (format nil "~a/~a" dir sock)))))))

(defun run-daemon (&key (port pine.server:*port*))
  (discover-session-env)
  (start-daemon :remoting-port port)
  (if (daemon-is-binary-p)
      (progn
        (start-frontends)
        (format t "pine daemon up on ~a:~d -- frontends spawned + supervised.~%"
                pine.server:*host* port))
      (format t "pine daemon up on ~a:~d -- attach editor/desktop apps.~%"
              pine.server:*host* port))
  (finish-output)
  (loop (sleep 3600)))

;;;; The user's init.lisp: the config that defines their surfaces, sources,
;;;; commands, and theme. Loaded in :pine.user. An error is surfaced, not fatal:
;;;; the daemon stays up with whatever loaded before it.

(defun load-init ()
  (let ((path (merge-pathnames "pine/init.lisp" (uiop:xdg-config-home))))
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
  "usage: pine {start | stop | restart | daemon | editor | desktop | status |
             eval FORM | reload | agents | spawn NAME | kill NAME |
             show|hide|toggle NAME}")

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
  "True when this daemon runs from the built `pine' binary (so it can re-invoke
itself to spawn a frontend). Under `make daemon' argv0 is sbcl and there is no
binary to spawn -- then the daemon stays headless and frontends are run by hand."
  (let ((self (first sb-ext:*posix-argv*)))
    (and self (not (search "sbcl" (namestring self))))))

(defun spawn-frontend (verb)
  "Launch one frontend (`pine VERB') as its own process, logging to /tmp."
  (let ((log (format nil "/tmp/pine-~a.log" verb)))
    (setf (gethash verb *frontend-procs*)
          (uiop:launch-program (list (first sb-ext:*posix-argv*) verb)
                               :output log :error-output log
                               :if-output-exists :supersede
                               :if-error-output-exists :supersede))))

(defvar *frontend-supervise* t
  "While true the supervisor respawns dead frontends; cleared on daemon stop so
it does not fight a deliberate shutdown.")

(defun start-frontends (&optional (verbs *frontends*))
  "Spawn each frontend as its own process and keep it alive: a dedicated thread
respawns any whose process has exited. The daemon is the supervisor, exactly as
it is for process agents."
  (setf *frontend-supervise* t)
  (dolist (v verbs) (ignore-errors (spawn-frontend v)))
  (bordeaux-threads:make-thread
   (lambda ()
     (loop while *frontend-supervise* do
       (sleep 3)
       (when *frontend-supervise*
         (dolist (v verbs)
           (let ((p (gethash v *frontend-procs*)))
             (unless (and p (uiop:process-alive-p p))
               (ignore-errors (spawn-frontend v))))))))
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

