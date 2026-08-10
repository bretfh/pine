(defpackage #:pine
  (:use #:cl)
  (:shadow #:describe)
  (:local-nicknames (#:node #:pine.fs.node) (#:tree #:pine.fs.tree)
                    (#:mount #:pine.fs.mount) (#:computed #:pine.fs.computed)
                    (#:watch #:pine.fs.watch)
                    (#:world #:pine.world.world) (#:store #:pine.world.store)
                    (#:cmd #:pine.repl.command) (#:mode #:pine.repl.mode)
                    (#:session #:pine.repl.session)
                    (#:process #:pine.proc.process)
                    (#:super #:pine.proc.supervisor)
                    (#:task #:pine.run.task) (#:ui #:pine.ui.paths)
                    (#:buffer #:pine.edit.buffer) (#:window #:pine.edit.window)
                    (#:edit #:pine.edit.defaults) (#:key #:pine.edit.key)
                    (#:render #:pine.edit.render)
                    (#:net #:pine.net.server) (#:attach #:pine.net.attach)
                    (#:control #:pine.net.control)
                    (#:agent #:pine.net.agent) (#:plisp #:pine.proc.lisp)
                    (#:sh #:pine.provider.sh) (#:env #:pine.provider.env)
                    (#:clock #:pine.provider.clock) (#:sys #:pine.provider.sys)
                    (#:term #:pine.edit.term) (#:fault #:pine.run.fault)
                    (#:load #:pine.run.log) (#:place #:pine.path.place)
                    (#:audio #:pine.provider.audio) (#:screen #:pine.provider.screen)
                    (#:power #:pine.provider.power) (#:net-p #:pine.provider.net)
                    (#:media #:pine.provider.media) (#:wm #:pine.provider.wm)
                    (#:live #:pine.provider.live) (#:esession #:pine.edit.session)
                    (#:runtime #:pine.ts.runtime) (#:syntax #:pine.ts.syntax)
                    (#:parser #:pine.ts.parser)
                    (#:lisp #:pine.edit.eval) (#:motion #:pine.edit.motion) (#:shell #:pine.repl.shell) (#:listing #:pine.edit.listing) (#:isearch #:pine.edit.isearch)
                    (#:erepl #:pine.edit.repl) (#:debugger #:pine.edit.debugger)
                    (#:desktop #:pine.app.desktop) (#:surface #:pine.app.surface)
                    (#:wmapp #:pine.app.wm) (#:css #:pine.ui.css)
                    (#:compositor #:pine.app.compositor))
  (:export #:start #:stop #:main #:*supervisor* #:*store* #:*image* #:here
           #:describe #:frame #:type!
           #:daemon #:quit #:spawn-agent #:run-app #:load-config #:config-file
           #:user-package #:write-at #:read-at
           #:audio #:screen #:power #:network #:media #:procfs #:shell #:niri #:compositor #:style
           #:frontend #:declare-frontends #:+frontends+))

(in-package #:pine)

(defvar *supervisor* nil)

(defvar *store* nil)

(defvar *image* nil)

(defparameter +frontends+ '("editor" "desktop"))

(defparameter +user-surface+
  '((:pine "declare-frontends" "frontend" "start" "stop" "here" "spawn-agent"
     "audio" "screen" "power" "network" "media" "procfs" "shell" "niri" "compositor"
     "style")
    (:pine.repl.command "defcommand" "command" "command-named" "commands" "run")
    (:pine.repl.mode "mode" "minor" "bind" "handle" "setting" "modes")
    (:pine.fs.node "contents" "nodes" "name" "attach" "describes")
    (:pine.fs.tree "at" "ensure" "place" "erase" "listing")
    (:pine.world.world "*world*" "root")
    (:pine.app.surface "surface" "defsurface" "show!" "hide!" "toggle!" "surfaces")
    (:pine.edit.render "frame-tree")
    (:pine.ui.face "color" "metric")
    (:pine.ui.css "css-glass" "css-rad" "css-mono")
    (:pine.path.path "leaf")
    (:pine.ui.build "column" "row" "label" "icon" "button" "box" "center"
     "scroll" "gap" "rule" "slider" "grid" "stack" "field" "rows" "choice"
     "calendar" "image" "centerbox" "ring" "cells" "here" "acting")
    (:pine.fs.node "stir" "child")
    (:pine.edit.prompt "ask")
    (:pine.run.log "note")
    (:pine.data "map" "seq" "set")))

(defun %seed-modes ()
  (mode:mode "text" :settings '(:tab-width 8 :indicator "Text"))
  (mode:mode "prog" :parent "text" :settings '(:indent 2 :comment ";"))
  (mode:mode "lisp" :parent "prog" :settings '(:indicator "Lisp")
                    :claims '((:files "*.lisp" "*.asd" "*.cl")))
  (mode:mode "shell" :settings '(:indicator "Shell")))

(defun %seed-syntax ()
  (let ((runtime (runtime:make-ts-runtime)))
    (fault:attempt (lambda () (runtime:ensure-ts runtime)) "loading tree-sitter")
    (when (runtime:ts-loaded-p runtime)
      (setf parser:*runtime* runtime)
      (syntax:install world:*world*))
    parser:*runtime*))

(defun start (&key (name "pine") store remoting)
  (setf world:*world* (world:make-world :name name)
        *supervisor* (super:supervisor))
  (when remoting
    (setf *image* (net:start-server :remoting-port remoting)
          net:*server* *image*)
    (attach:listen-for-attach *image*)
    (agent:listen-for-agents *image*))
  (node:attach (make-instance 'shell:commands-node :name "cmd"
                                                   :describes "every command there is")
               (world:root world:*world*))
  (world:ensure world:*world* "buf")
  (world:ensure world:*world* "win")
  (setf shell:*supervisor* *supervisor* shell:*store* *store*)
  (shell:install)
  (%seed-modes)
  (ui:install world:*world*)
  (let ((root (world:root world:*world*)))
    (sh:install root)
    (env:install root)
    (sys:install root)
    (clock:install root :supervisor *supervisor*)
    (mount:mount "/" root "file"))
  (edit:install)
  (motion:install)
  (listing:install)
  (isearch:install)
  (erepl:install)
  (debugger:install)
  (lisp:install)
  (desktop:install)
  (wmapp:install)
  (compositor:install)
  (term:install)
  (esession:install)
  (%seed-syntax)
  (let ((scratch (buffer:make-buffer "scratch" :mode "lisp")))
    (setf (buffer:current) scratch)
    (window:seed! scratch))
  (when store
    (setf *store* (store:open-store store))
    (setf shell:*store* *store*)
    (store:restore world:*world* *store*)
    (store:keeping *store*))
  (super:watch *supervisor*)
  world:*world*)

(defun stop ()
  (setf cmd:*asking* nil
        pine.ui.build:*asking* nil
        pine.ui.build:*editing* nil)
  (key:take-next nil)
  (isearch:took-all)
  (live:leave-all)
  (parser:forget-all)
  (watch:forget-all)
  (when *supervisor*
    (super:unwatch *supervisor*)
    (super:stop-all *supervisor*))
  (dolist (s (session:sessions)) (session:close s))
  (setf node:*on-write* nil)
  (when *store*
    (ignore-errors (store:snapshot world:*world* *store*))
    (ignore-errors (store:close-store *store*))
    (setf *store* nil))
  (dolist (tk (task:tasks)) (task:stop tk))
  (when *image*
    (ignore-errors (net:stop-server *image*))
    (setf *image* nil net:*server* nil))
  t)

(defun main (&key store)
  (start :store store)
  (let ((s (session:open-session :name "console" :mode "shell"
                                 :node (world:root world:*world*)
                                 :package (find-package :pine))))
    (unwind-protect (session:interact s)
      (stop))))

(defun type! (text &optional (session session:*session*))
  (declare (ignore session))
  (loop :for ch :across text
        :do (key:dispatch nil (key:make-key (string ch))))
  (buffer:point (buffer:current)))

(defun frame (&key (width 80) (height 24))
  (mapcar #'car (render:rows :width width :height height)))

(defun procfs () (live:attend (sys:install (world:root world:*world*))))
(defun shell () (sh:install (world:root world:*world*)))
(defun audio (&optional (name "audio"))
  (live:attend (audio:install (world:root world:*world*) name)))
(defun screen (&optional (name "screen"))
  (live:attend (screen:install (world:root world:*world*) name)))
(defun power (&optional (name "power"))
  (live:attend (power:install (world:root world:*world*) name)))
(defun network (&optional (name "net"))
  (live:attend (net-p:install (world:root world:*world*) name)))
(defun media (&key (name "media") player)
  (live:attend (media:install (world:root world:*world*) :name name :player player)))

(defun niri (&optional (name "wm")) (live:attend (wm:install (world:root world:*world*) name)))

(defun compositor () (compositor:pine-wm world:*world*))

(defun style (selector props)
  (prog1 (first (css:install (list (list selector props))))
    (css:broadcast)))

(defun %place (where)
  (cond ((node:nodep where) where)
        ((stringp where) (tree:at (world:root world:*world*) where))
        (t where)))

(defun write-at (where value)
  (let ((it (%place where)))
    (if (node:nodep it)
        (setf (node:contents it) value)
        (setf (place:contents it) value))))

(defun read-at (where &optional default)
  (let* ((it (%place where))
         (value (if (node:nodep it) (node:contents it) (place:contents it))))
    (if (null value) default value)))

(defun user-package ()
  (or (find-package :pine.user)
      (let ((p (make-package :pine.user :use '(:cl))))
        (dolist (entry +user-surface+)
          (dolist (name (rest entry))
            (let ((symbol (find-symbol (string-upcase name) (first entry))))
              (when symbol (shadowing-import symbol p)))))
        (shadow '("WRITE" "READ") p)
        (setf (fdefinition (intern "WRITE" p)) #'write-at
              (fdefinition (intern "READ" p)) #'read-at)
        (export (list (intern "WRITE" p) (intern "READ" p)) p)
        p)))

(defun config-file ()
  (merge-pathnames "pine/init.lisp" (uiop:xdg-config-home)))

(defun load-config (&optional (file (config-file)))
  (user-package)
  (when (and file (probe-file file))
    (let ((*package* (user-package))
          (*readtable* (named-readtables:find-readtable 'pine.path.reader:syntax))
          (before (length (fault:faults))))
      (load:note "reading ~a" file)
      (fault:attempt (lambda () (load file)) (format nil "reading ~a" file))
      (let ((broke (- (length (fault:faults)) before)))
        (when (plusp broke)
          (load:note "~a did not load: ~a" file
                     (fault:condition-of (first (fault:faults)))))
        (zerop broke)))))

(defun %frontend-command (verb)
  (let ((self (first sb-ext:*posix-argv*)))
    (if (and self (not (search "sbcl" (namestring self))))
        (list self verb)
        (list (namestring sb-ext:*runtime-pathname*)
              "--noinform" "--no-userinit" "--non-interactive"
              "--eval" "(require :asdf)"
              "--eval" "(asdf:load-system :pine/wayland)"
              "--eval" (format nil "(pine:run-app ~s)" verb)))))

(defun %frontend-environment ()
  (cons (format nil "PINE_PORT=~d" net:*port*)
        (remove-if (lambda (entry)
                     (and (>= (length entry) 10)
                          (string= "PINE_PORT=" entry :end2 10)))
                   (sb-ext:posix-environ))))

(defun frontend (verb)
  (make-instance 'process:program
                 :name verb
                 :argv (%frontend-command verb)
                 :env (%frontend-environment)))

(defun declare-frontends (&optional (which +frontends+))
  (dolist (verb which)
    (unless (super:process-named *supervisor* verb)
      (let ((p (frontend verb)))
        (super:supervise *supervisor* p)
        (setf (node:contents (world:ensure world:*world* "proc" verb)) :declared)
        (fault:attempt (lambda () (process:start p))
                       (format nil "starting the ~a frontend" verb)))))
  (mapcar #'process:name (super:processes *supervisor*)))

(defun quit (&optional (grace 5))
  (task:spawn "quit-watchdog"
              (lambda () (sleep grace) (sb-ext:exit :abort t :code 0)))
  (task:spawn "quit"
              (lambda ()
                (sleep 0.2)
                (ignore-errors (stop))
                (sb-ext:exit :abort t :code 0)))
  t)

(defun daemon (&key store (remoting net:*port*) (config (config-file)))
  (start :store store :remoting remoting)
  (cmd:defcommand "agents" () (:describes "every image attached to this one")
    (mapcar #'agent:name (agent:agents)))
  (cmd:defcommand "clients" () (:describes "every frontend attached")
    (loop :for c :in (attach:clients)
          :collect (list (attach:client-kind c) (attach:client-id c))))
  (control:serve *image* :on-quit #'quit)
  (load-config config)
  (load:note "~a: remoting ~d, ~d command~:p, ~d running"
             (world:name world:*world*)
             (net:remoting-port *image*)
             (length (cmd:commands))
             (length (super:processes *supervisor*)))
  *image*)

(defun spawn-agent (name)
  (let ((p (make-instance 'plisp:lisp-process :name name :systems '(:pine))))
    (super:supervise *supervisor* p)
    (process:start p)
    p))

(defun run-app (verb)
  (net:read-environment)
  (attach:run-frontend (intern (string-upcase verb) :keyword)))

(defun describe (where)
  (let ((n (shell:resolve nil where)))
    (when n
      (list :name (node:full-name n)
            :class (class-name (class-of n))
            :describes (node:describe n)
            :under (tree:listing n)
            :persists (node:persistp n)))))
