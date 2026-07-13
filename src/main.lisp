(in-package :pine)

(defvar *version* "0.0.1")

(defvar *target*
  #+darwin :macos
  #+linux :linux
  #+windows :windows
  #-(or darwin linux windows) :linux)

(defun desktop? ()
  (member *target* '(:linux :macos :windows)))

(defun mobile? ()
  (member *target* '(:android :ios)))

(defun main (&key (workers 4) (remoting-port 0))
  (format t "pine ~a on ~a ~a [~a]~%"
          *version*
          (lisp-implementation-type)
          (lisp-implementation-version)
          *target*)
  (let ((srv (pine.server:start-server :workers workers
                                       :remoting-port remoting-port)))
    (setf pine.server:*server* srv)
    (setf (pine.server:ts-runtime srv) (pine.ts:make-ts-runtime))
    (pine.event:make-event-bus srv)
    (pine.actor:start-agent-registry srv)
    (pine.actor:start-local-agent srv)
    (let ((cli (pine.client:start-client srv)))
      (setf pine.client:*client* cli)
      (pine.buffer:install-default-faces)
      (pine.editor:start-editor)
      (pine.hooks:run-init-hooks)
      
      (format t "pine ready [port ~a]~%"
              (or (pine.server:remoting-port srv) "none"))
      (values srv cli))))

(defun start-daemon (&key (workers 4) (remoting-port 0))
  "Boot the headless substrate: server, registries, eval, and remoting. No
client, no GTK, no rendering. Apps attach over remoting. Returns the server; the
caller keeps the process alive. This is the daemon."
  (let ((srv (pine.server:start-server :workers workers :remoting-port remoting-port)))
    (setf pine.server:*server* srv)
    (setf (pine.server:ts-runtime srv) (pine.ts:make-ts-runtime))
    (handler-case (pine.ts:ensure-ts (pine.server:ts-runtime srv))   ; tree-sitter for highlights
      (error () nil))
    (pine.event:make-event-bus srv)
    (pine.actor:start-agent-registry srv)
    (pine.actor:start-local-agent srv)
    (pine.actor:start-agent-debug srv)
    (pine.buffer:start-buffer-registry srv)
    (pine.attach:start-attach-listener srv)
    (pine.buffer:install-default-faces)
    (pine.mode:install-default-modes)
    (pine.editor:install-commands)
    (pine.editor:install-bindings)
    (pine.editor:install-editor-sessions)
    (pine.desktop:install-desktop-sessions)
    (pine.desktop:install-desktop-config)
    (pine.jobs:install-jobs)
    (ignore-errors (pine.source:start-sources srv))   ; sources feed cells the desktop reads
    (format t "pine daemon ready [remoting ~a]~%" (pine.server:remoting-port srv))
    srv))

(defun %setenv (name value)
  (cffi:foreign-funcall "setenv" :string name :string value :int 1 :int))

(defun discover-session-env ()
  "A shepherd service starts outside the wayland session, so WAYLAND_DISPLAY and
NIRI_SOCKET are unset. Fill them from XDG_RUNTIME_DIR -- a wayland-N socket and
the newest niri.*.sock -- so the daemon's sources and app launches reach the
running session. A no-op when already set (an in-session `make daemon` run)."
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

(defun run-daemon (&key (port 17000))
  "Start the headless daemon and keep the process alive. Editor and desktop apps
attach over remoting on PORT. This is the entry `make daemon` runs."
  (discover-session-env)
  (start-daemon :remoting-port port)
  (format t "pine daemon up on 127.0.0.1:~d -- attach editor/desktop apps.~%" port)
  (finish-output)
  (loop (sleep 3600)))

(defun stop ()
  (pine.hooks:run-shutdown-hooks))

(defun on-minibuffer-accept (text)
  (pine.editor:on-minibuffer-accept text))
