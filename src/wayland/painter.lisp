(defpackage #:pine/wayland/painter
  (:use #:cl #:wayflan-client)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree) (#:mount #:pine/fs/mount)
                    (#:job #:pine/run/job) (#:actors #:pine/run/actors)
                    (#:peer #:pine/run/peer) (#:image #:pine/run/image)
                    (#:fault #:pine/run/fault)
                    (#:log #:pine/run/log)
                    (#:w #:pine/ui/widget) (#:wire #:pine/ui/wire)
                    (#:hit #:pine/ui/hit) (#:face #:pine/ui/face)
                    (#:sheet #:pine/ui/sheet)
                    (#:pump #:pine/wayland/pump) (#:display #:pine/wayland/display)
                    (#:shell #:pine/wayland/shell)
                    (#:pane #:pine/wayland/pane)
                    (#:input #:pine/wayland/input)
                    (#:wm #:pine/wayland/wm))
  (:export #:painter #:run #:wm-of #:*at* #:*port* #:*host*))
(in-package #:pine/wayland/painter)

(defvar *at* "pine"
  "What the daemon is mounted as here. Everything the painter reads and writes is
a path under it, because a painter is a pine that mounted another one.")

(defvar *host* nil)
(defvar *port* nil)
(defconstant +left+ #x110)

(defclass painter ()
  ((display :initarg :display :reader display-of)
   (shell   :initarg :shell   :accessor shell-of)
   (daemon  :initarg :daemon  :reader daemon)
   (pump    :initarg :pump    :reader pump)
   (says    :initform nil     :accessor says)
   (shown   :initform (d:table) :reader shown)
   (watching :initform nil    :accessor watching)
   (wm      :initform nil     :accessor wm-of)
   (keys    :initform (input:make-keys) :reader keys)
   (pointer :initform (input:make-pointer) :reader pointer)
   (done    :initform nil     :accessor done))
  (:documentation "A pine that shows another one. It mounts the daemon, watches
the surfaces it declares, and paints each where its role says it goes."))

(defun %where (name &rest under)
  "The path here: under what the daemon is mounted as."
  (format nil "/~a/surface/~a~{/~a~}" *at* name under))

(defun %theirs (name &rest under)
  "The same place as the daemon calls it. What is mounted here is its root there,
so a watch is asked for by the name it has where it lives."
  (format nil "/surface/~a~{/~a~}" name under))

(defun %at (name &rest under)
  (tree:at nil (apply #'%where name under)))

(defun %read (name &rest under)
  (let ((n (apply #'%at name under)))
    (if n
        (node:contents n)
        (progn (log:note "~a says nothing" (apply #'%where name under)) nil))))

(defun tell (p thunk)
  "Do something that talks to the daemon, off the thread holding the compositor
connection. A remote write is a wait, and a painter that waits is a painter that
has stopped drawing.

Answers whether it went: before the daemon is reached there is nowhere to say it,
and whoever asked has to know that so it can ask again."
  (let ((to (says p)))
    (when to (job:tell to thunk) t)))

(defun %names (p)
  (declare (ignore p))
  (let ((n (tree:at nil (format nil "/~a/surface" *at*))))
    (and n (mapcar #'node:name (node:nodes n)))))

(defun %tree (p name)
  (let ((n (%at name "wire")))
    (when n
      (let ((form (%read name "wire")))
        (when form
          (wire:from-wire
           form
           :on-action
           (lambda (id)
             (lambda (&rest arguments)
               (tell p (lambda ()
                         (setf (node:contents (%at name "click"))
                               (cons id arguments))))))))))))

(defun %shownp (name) (%read name "shown"))

(defun %windowp (name)
  (and (member (princ-to-string (or (%read name "role") "")) '("window" "tile")
               :test #'equal)
       t))

(defun %sizing (s cell-w cell-h)
  (let ((wide (pane:wide s)) (tall (pane:tall s)))
    (list :wide wide :tall tall
          :cols (max 1 (floor wide (max 1 (or cell-w 9))))
          :lines (max 1 (floor tall (max 1 (or cell-h 18))))
          :font pane:*font*)))

(defun %say-size (p name s &key cell-w cell-h now)
  "Tell the daemon how big this surface came out, in pixels and in cells. What it
lays out is what lands here, so this is said in one place.

Opening one says it and waits: what comes back is where to put it, and that cannot
be read before the size it is worked out from has landed."
  (let ((said (%sizing s cell-w cell-h)))
    (if now
        (setf (node:contents (%at name "size")) said)
        (tell p (lambda () (setf (node:contents (%at name "size")) said))))))

(defun open-one (p name)
  "Put a surface up. Two halves: what the daemon has to be asked, which can be
anywhere, and what the compositor has to be told, which is only ever the thread
holding it."
  (let ((tree (%tree p name)))
    (when (and tree (null (d:at (d:all (shown p)) name)))
      (let ((s (make-instance 'pane:pane
                              :name name :shell (shell-of p) :tree tree
                              :on-resize
                              (lambda (s)
                                (multiple-value-bind (cw ch) (pane:cell s)
                                  (%say-size p name s :cell-w cw :cell-h ch))))))
        (when (eq s (d:claim (shown p) name s))
          (multiple-value-bind (wide tall) (pane:measure s)
            (setf (pane:wide s) wide (pane:tall s) tall)
            (multiple-value-bind (cw ch) (pane:cell s)
              (%say-size p name s :cell-w cw :cell-h ch :now t)))
          (let ((where (%read name "where"))
                (windowp (%windowp name)))
            (pump:hand (pump p)
                       (lambda ()
                         (pane:open-pane s where :windowp windowp)
                         (%shows p s)
                         (log:note "~a is up" name)))))))))

(defun moved (p name)
  "The surface said something changed. What it now holds is read here, wherever
here is; painting it is handed to the thread that owns the compositor."
  (let ((s (d:at (d:all (shown p)) name))
        (tree (%tree p name)))
    (cond ((and s tree)
           (multiple-value-bind (cw ch) (pane:cell s)
             (%say-size p name s :cell-w cw :cell-h ch))
           (pump:hand (pump p)
                      (lambda () (setf (pane:tree s) tree) (%shows p s))))
          ((and (null s) tree (%shownp name)) (open-one p name))
          (t nil))))

(defun toggled (p name)
  (if (%shownp name)
      (unless (d:at (d:all (shown p)) name) (open-one p name))
      (let ((s (d:at (d:all (shown p)) name)))
        (when s
          (d:drop! (shown p) name)
          (pump:hand (pump p) (lambda () (pane:close-pane s)))))))

(defun %listen (p name)
  "Hear about a pane: its tree, and whether it is up."
  (push (peer:listen-to (daemon p) (%theirs name "wire")
                        (lambda (where said)
                          (declare (ignore where said))
                          (moved p name)))
        (watching p))
  (push (peer:listen-to (daemon p) (%theirs name "shown")
                        (lambda (where said)
                          (declare (ignore where said))
                          (toggled p name)))
        (watching p)))

(defun %pointer (p sh &rest event)
  (let ((at (pointer p)))
    (event-case event
      (:enter (serial surface x y)
       (setf (input:pointer-serial at) serial
             (input:pointer-focus at) (shell:at-surface sh surface)
             (input:pointer-at-x at) x
             (input:pointer-at-y at) y))
      (:motion (time-ms x y)
       (declare (ignore time-ms))
       (setf (input:pointer-at-x at) x (input:pointer-at-y at) y)
       (when (input:pointer-drag at) (%drag p)))
      (:leave (serial surface)
       (declare (ignore serial surface))
       (setf (input:pointer-focus at) nil))
      (:button (serial time-ms button state)
       (declare (ignore serial time-ms))
       (when (= button +left+)
         (ecase state
           (:pressed (%press p))
           (:released (%release p)))))
      (:axis (time-ms axis delta) (declare (ignore time-ms axis delta)))
      (:frame ()))))

(defun %over (p)
  (let* ((at (pointer p))
         (s (input:pointer-focus at)))
    (when (and s (pane:tree s))
      (hit:at (pane:tree s) (round (input:pointer-at-y at))
              (round (input:pointer-at-x at))))))

(defun %press (p)
  (let ((found (%over p)))
    (if (typep found 'w:slider)
        (progn (setf (input:pointer-drag (pointer p)) found) (%drag p))
        (%click p))))

(defun %drag (p)
  (let ((slider (input:pointer-drag (pointer p)))
        (s (input:pointer-focus (pointer p))))
    (when (and slider s)
      (setf (w:value slider)
            (hit:value-at slider (round (input:pointer-at-x (pointer p)))))
      (pane:paint s))))

(defun %release (p)
  (let ((slider (input:pointer-drag (pointer p))))
    (when slider
      (setf (input:pointer-drag (pointer p)) nil)
      (let ((fn (w:changed slider)))
        (when fn (funcall fn (w:value slider)))))))

(defun %click (p)
  (let* ((at (pointer p))
         (s (input:pointer-focus at)))
    (when (and s (pane:tree s))
      (let ((thunk (hit:clicked-at (pane:tree s)
                                   (round (input:pointer-at-y at))
                                   (round (input:pointer-at-x at)))))
        (when thunk (funcall thunk))))))

(defun %keyboard (p sh &rest event)
  (declare (ignore sh))
  (let ((k (keys p)))
    (event-case event
      (:keymap (format fd size)
       (assert (eq format :xkb-v1))
       (input:keymap k fd size))
      (:modifiers (serial depressed latched locked group)
       (declare (ignore serial))
       (input:modifiers k depressed latched locked group))
      (:key (serial time-ms key state)
       (declare (ignore serial time-ms))
       (case state
         (:pressed (let ((said (input:pressed k key)))
                     (when said (%typed p said))))
         (:released (input:released k key))))
      (:enter (serial surface keys) (declare (ignore serial surface keys)))
      (:leave (serial surface)
       (declare (ignore serial surface))
       (input:forget-held k))
      (:repeat-info (rate delay)
       (setf (input:keys-rate k) rate (input:keys-delay k) delay)))))

(defun %typed (p said)
  (tell p (lambda ()
            (let ((n (tree:at nil (format nil "/~a/key" *at*))))
              (if n
                  (setf (node:contents n) said)
                  (log:note "nothing at /~a/key" *at*))))))

(defun %tick (p)
  (let ((again (input:repeating (keys p))))
    (when again (%typed p again))))

(defun %loop (p)
  (let ((d (display-of p)))
    (loop :until (done p)
          :do (%tick p)
              (display:dispatch d)
              (pump:drain (pump p))
              (let ((woke (display:wait d (pump p)
                                        (or (input:deadline (keys p)) 100))))
                (when woke (pump:drain-wake (pump p)))))))

(defun %managing (p said)
  "Say what the compositor handed over, and hear back where it all goes. Off the
thread holding the connection: the compositor is waiting on that thread, and a
manager that waits is one it gives up on."
  (tell p
        (lambda ()
          (flet ((at (what)
                   (tree:at nil (format nil "/~a/wm/~a" *at* what))))
            (let ((where (at "said")))
              (when where (setf (node:contents where) said)))
            (let ((wants (let ((n (at "wants"))) (and n (node:contents n))))
                  (layout (let ((n (at "placement"))) (and n (node:contents n)))))
              (pump:hand (pump p)
                         (lambda ()
                           (let ((w (wm-of p)))
                             (when w
                               (when wants (wm:take w wants))
                               (wm:laid w layout))))))))))

(defun %rendering (p)
  "The compositor has opened a render sequence: commit whatever furniture of
pine's own is waiting. This is the only place one may be committed."
  (d:do-each (s (d:vals (d:all (shown p))))
    (when (and (pane:chromep s) (pane:dirty s))
      (fault:attempt (lambda () (pane:render s))
                     (format nil "rendering ~a" (pane:name-of s))))))

(defun %shows (p s)
  "Show what a pane holds. Furniture asks the compositor for a render sequence
rather than committing where it stands."
  (pane:paint s)
  (when (and (pane:chromep s) (pane:dirty s)) (wm:wake (wm-of p))))

(defun %styles (p)
  (declare (ignore p))
  (let ((n (tree:at nil (format nil "/~a/style" *at*))))
    (when n
      (let ((said (node:contents n)))
        (when said (sheet:put said))))))

(defun %attend (p daemon name)
  "Get to the daemon and take up what it declared. Everything here waits on
another image, so none of it is done on the thread holding the compositor: that
thread has a compositor waiting on it."
  (mount:mount daemon (tree:root) *at*)
  (setf (says p)
        (job:start (make-instance 'job:actor
                                  :name name :restarts nil :dispatcher :pinned
                                  :receive (lambda (thunk)
                                             (fault:attempt thunk
                                                            "telling the daemon")))))
  (when (wm-of p)
    (fault:attempt
     (lambda ()
       (image:evaluate
        daemon
        '(progn (setf (pine/fs/node:contents
                       (pine/fs/tree:ensure nil "wm-manage"))
                      t)
                (pine:use :wm))))
     "asking the daemon to manage the windows"))
  (%styles p)
  (let ((all (%names p)))
    (dolist (each all)
      (%listen p each)
      (when (%shownp each) (open-one p each)))
    (log:note "~d surface~:p up, ~d watched" (d:size (d:all (shown p)))
              (length all)))
  p)

(defun run (&key (host *host*) (port *port*) (name "painter"))
  "Show a pine. The daemon must be up; everything after that is reads and writes
of its namespace."
  (let* ((d (display:connect))
         (p (make-instance 'painter :display d :daemon nil :pump (pump:make-pump))))
    (setf (shell-of p) (shell:bind d
                                   :on-pointer (lambda (sh &rest e)
                                                 (apply #'%pointer p sh e))
                                   :on-keyboard (lambda (sh &rest e)
                                                  (apply #'%keyboard p sh e))))
    (setf (wm-of p) (wm:bind d :on-said (lambda (said) (%managing p said))
                               :on-render (lambda () (%rendering p))))
    (when (wm-of p)
      (setf (shell:chrome (shell-of p)) (wm:manager (wm-of p))))
    (actors:boot :remoting 0)
    (tree:make-root)
    (let ((daemon (peer:reach "daemon" :host host :port port)))
      (setf (slot-value p 'daemon) daemon)
      (actors:blocking "attending" (lambda () (%attend p daemon name)))
      (unwind-protect (%loop p)
        (dolist (j (watching p)) (ignore-errors (job:stop j)))
        (when (says p) (ignore-errors (job:stop (says p))))
        (ignore-errors (job:stop daemon))
        (pump:close-pump (pump p))
        (display:disconnect d)))))
