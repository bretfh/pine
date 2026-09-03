(defpackage #:pine/wayland/screen
  (:use #:cl #:wayflan-client)
  (:local-nicknames (#:ui #:pine/ui)
                    (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree) (#:job #:pine/run/job)
                    (#:watch #:pine/run/watch) (#:system #:pine/run/system)
                    (#:fault #:pine/run/fault) (#:log #:pine/fs/log)
                    (#:pump #:pine/wayland/pump) (#:display #:pine/wayland/display)
                    (#:shell #:pine/wayland/shell)
                    (#:pane #:pine/wayland/pane)
                    (#:input #:pine/wayland/input)
                    (#:wm #:pine/wayland/wm))
  (:export
   #:screen #:wm-of #:tell #:pump #:keys
   #:pointer #:pointing #:keyboard-said #:typed #:chorded
   #:chords-wanted))
(in-package #:pine/wayland/screen)

(defconstant +left+ #x110)
(defparameter +settling+ 10
  "Turns of the loop between passes over what is up. A surface can go without
saying so, and a pane with nothing behind it is a window nothing can close.")

(defgeneric pointing (s sh &rest event)
  (:documentation "What the pointer did. The screen holds the connection and says
one arrived; what it was for is answered beside the other hands."))
(defgeneric keyboard-said (s sh &rest event))
(defgeneric typed (s said))
(defgeneric chorded (s said))
(defgeneric chords-wanted (s))

(defclass screen (job:thread)
  ((display :initform nil     :accessor display-of)
   (shell   :initform nil     :accessor shell-of)
   (pump    :initform nil     :accessor pump)
   (says    :initform nil     :accessor says)
   (up      :initform (d:table) :reader up)
   (watching :initform (d:table) :reader watching)
   (wm      :initform nil     :accessor wm-of)
   (keys    :initform (input:make-keys) :reader keys)
   (pointer :initform (input:make-pointer) :reader pointer)
   (turns   :initform 0       :accessor turns)
   (done    :initform nil     :accessor done))
  (:documentation "The display this pine paints on. A THREAD, because a compositor
connection is a thing you block on."))

(defun availablep () (and (uiop:getenv "WAYLAND_DISPLAY") t))

(defun %at (name &rest under)
  (apply #'tree:at "/surface" name under))

(defun tell (s thunk)
  "Do something that is not drawing, off the thread holding the connection."
  (let ((to (says s)))
    (when to (job:tell to thunk) t)))

(defun %named (name) (tree:at "/surface" name))

(defun %tree (name)
  (let ((it (%named name))) (when it (node:contents it))))

(defun %shownp (name)
  (let ((it (%named name))) (and it (ui:shown it) t)))

(defun %windowp (name)
  (let ((it (%named name)))
    (and it (typep (ui:role it) '(or ui:window ui:tile)) t)))

(defun %where (name)
  (let ((n (%at name "where")))
    (and n (node:contents n))))

(defun %sizing (p cell-w cell-h)
  (list :wide (pane:wide p) :tall (pane:tall p)
        :cols (max 1 (floor (pane:wide p) (max 1 (or cell-w 9))))
        :lines (max 1 (floor (pane:tall p) (max 1 (or cell-h 18))))
        :font pane:*font*))

(defun %say-size (s name p &key cell-w cell-h now)
  "How big this surface came out. Opening one says it and waits: where it goes is
worked out from the size."
  (let ((said (%sizing p cell-w cell-h))
        (n (%at name "size")))
    (flet ((put ()
             (when (and n (not (equal said (node:contents n))))
               (setf (node:contents n) said))))
      (if now (put) (tell s #'put)))))

(defun open-one (s name)
  "Put a surface up. What has to be told to the compositor is only ever the thread
holding it."
  (let ((tree (%tree name)))
    (when (and tree (null (d:lookup (d:all (up s)) name)))
      (let ((p (make-instance 'pane:pane
                              :name name :shell (shell-of s) :tree tree
                              :on-resize
                              (lambda (p)
                                (multiple-value-bind (cw ch) (pane:cell p)
                                  (%say-size s name p :cell-w cw :cell-h ch))))))
        (when (eq p (d:claim (up s) name p))
          (multiple-value-bind (wide tall) (pane:measure p)
            (setf (pane:wide p) wide (pane:tall p) tall)
            (multiple-value-bind (cw ch) (pane:cell p)
              (%say-size s name p :cell-w cw :cell-h ch :now t)))
          (let ((where (%where name))
                (windowp (%windowp name)))
            (pump:hand (pump s)
                       (lambda ()
                         (pane:open-pane p where :windowp windowp)
                         (%shows s p)
                         (log:note "~a is up" name)))))))))

(defun moved (s name)
  "The surface worked itself out again. How big it is is not said here: that is a
write to a node the surface reads, and on every repaint it never settles."
  (let ((p (d:lookup (d:all (up s)) name))
        (tree (%tree name)))
    (cond ((and p tree)
           (pump:hand (pump s)
                      (lambda () (setf (pane:tree p) tree) (%shows s p))))
          ((and (null p) tree (%shownp name)) (open-one s name))
          (t nil))))

(defun toggled (s name)
  (if (%shownp name)
      (unless (d:lookup (d:all (up s)) name) (open-one s name))
      (let ((p (d:lookup (d:all (up s)) name)))
        (when p
          (d:drop! (up s) name)
          (pump:hand (pump s) (lambda () (pane:close-pane p)))))))

(defun %unlisten (s name)
  (dolist (w (d:lookup (d:all (watching s)) name))
    (fault:or-nothing "a watch already let go of is let go of"
      (watch:unwatch w)))
  (d:drop! (watching s) name))

(defun %listen (s name)
  "Hear about this surface. A config read again puts a new node under the same
name, and the old one is something nothing writes."
  (%unlisten s name)
  (let ((it (%named name)))
    (when it
      (let ((on-tree (watch:watch it (lambda (of said)
                                       (declare (ignore of said))
                                       (moved s name))
                                  :tells-when :always :poll nil
                                  :name (format nil "screen<-~a" name)))
            (shown (%at name "shown")))
        (d:keep! (watching s) name
                 (list* on-tree
                        (when shown
                          (list (watch:watch shown
                                             (lambda (of said)
                                               (declare (ignore of said))
                                               (toggled s name))
                                             :poll nil
                                             :name (format nil "screen<-~a/shown"
                                                            name))))))))))

(defun %settle (s)
  "Take down what /surface no longer says."
  (d:do-each (name (d:keys (d:all (up s))))
    (unless (tree:at "/surface" name)
      (let ((p (d:lookup (d:all (up s)) name)))
        (%unlisten s name)
        (d:drop! (up s) name)
        (when p
          (pane:close-pane p)
          (log:note "~a is gone" name))))))

(defun %tick (s)
  (let ((again (input:repeating (keys s))))
    (when again (typed s again)))
  (when (zerop (mod (incf (turns s)) +settling+)) (%settle s)))

(defun %loop (s)
  (let ((d (display-of s)))
    (loop :until (or (done s) (job:stopping s))
          :do (%tick s)
              (display:dispatch d)
              (pump:drain (pump s))
              (let ((woke (display:wait d (pump s)
                                        (or (input:deadline (keys s)) 100))))
                (when woke (pump:drain-wake (pump s)))))))

(defun %managing (s said)
  "What the compositor handed over, and where it all goes. Off the thread holding
it: river kills a manager that waits."
  (tell s
        (lambda ()
          (let ((where (tree:at "/wm/said")))
            (when where (setf (node:contents where) said)))
          (let ((wants (let ((n (tree:at "/wm/wants")))
                         (and n (node:contents n))))
                (layout (let ((n (tree:at "/wm/placement")))
                          (and n (node:contents n)))))
            (pump:hand (pump s)
                       (lambda ()
                         (let ((it (wm-of s)))
                           (when it
                             (when wants (wm:take it wants))
                             (wm:laid it layout)))))))))

(defun %rendering (s)
  "A render sequence is open: commit pine's own furniture."
  (d:do-each (p (d:vals (d:all (up s))))
    (when (and (pane:chromep p) (pane:dirty p))
      (fault:attempt (lambda () (pane:render p))
                     (format nil "rendering ~a" (pane:name-of p))))))

(defun %shows (s p)
  (pane:paint p)
  (when (and (pane:chromep p) (pane:dirty p)) (wm:wake (wm-of s))))

(defun %names ()
  (let ((n (tree:at "/surface")))
    (and n (mapcar #'node:name (node:nodes n)))))

(defun %managing-windows (s)
  "Where the compositor asked for a manager, say so and load the one that is it.
A wm already up is the wrong one, so it goes first."
  (when (wm-of s)
    (fault:attempt
     (lambda ()
       (setf (node:contents (tree:ensure "/wm-manages")) :pine)
       (when (system:named "wm") (system:drop "wm"))
       (system:use "wm")
       (chords-wanted s))
     "taking the windows over")))

(defun %rebound (s name p)
  "A pane over a surface node that has just been replaced. The pane stays where the
compositor put it; what it draws is the new node's to say."
  (multiple-value-bind (cw ch) (pane:cell p)
    (%say-size s name p :cell-w cw :cell-h ch :now t))
  (let ((tree (%tree name)))
    (when tree
      (pump:hand (pump s) (lambda () (setf (pane:tree p) tree) (%shows s p))))))

(defun took-up (s name)
  "One surface, watched and shown, however it came to be declared."
  (%listen s name)
  (let ((p (d:lookup (d:all (up s)) name)))
    (cond ((not (%shownp name))
           (when p
             (d:drop! (up s) name)
             (pump:hand (pump s) (lambda () (pane:close-pane p)))))
          (p (%rebound s name p))
          (t (open-one s name)))))

(defun %take-up (s)
  (%managing-windows s)
  (dolist (name (%names)) (took-up s name))
  (log:note "~d surface~:p up, ~d watched" (d:size (d:all (up s)))
            (length (%names)))
  s)

(defun %attend (s)
  "Everything that touches the connection, on the thread that owns it."
  (let ((d (display:connect)))
    (setf (display-of s) d
          (pump s) (pump:make-pump))
    (setf (shell-of s) (shell:bind d
                                   :on-pointer (lambda (sh &rest e)
                                                 (apply #'pointing s sh e))
                                   :on-keyboard (lambda (sh &rest e)
                                                  (apply #'keyboard-said s sh e))))
    (setf (wm-of s) (wm:bind d :on-said (lambda (said) (%managing s said))
                               :on-render (lambda () (%rendering s))
                               :on-chord (lambda (said) (chorded s said))))
    (when (wm-of s)
      (setf (shell:chrome (shell-of s)) (wm:manager (wm-of s))))
    (setf (says s)
          (job:start (make-instance 'job:actor
                                    :name (format nil "~a-work" (node:name s))
                                    :on-fault :leave :dispatcher :pinned
                                    :receive (lambda (thunk)
                                               (fault:attempt thunk "the screen")))))
    (%take-up s)
    s))

(defun %shut (s)
  (d:do-each (name (d:keys (d:all (watching s))))
    (%unlisten s name))
  (d:do-each (p (d:vals (d:all (up s))))
    (fault:or-nothing "a pane whose compositor has gone cannot be told"
      (pane:close-pane p)))
  (d:do-each (name (d:keys (d:all (up s))))
    (d:drop! (up s) name))
  (when (says s) (fault:or-nothing "one already stopped stays stopped"
                   (job:stop (says s))))
  (when (pump s) (pump:close-pump (pump s)))
  (when (display-of s) (display:disconnect (display-of s)))
  (setf (display-of s) nil (shell-of s) nil (wm-of s) nil)
  s)

(defmethod job:start :before ((s screen))
  (setf (job:runs s)
        (lambda ()
          (unwind-protect (progn (%attend s) (%loop s))
            (%shut s)))))

(defun open-screen (&key (name "screen"))
  "Paint this pine's surfaces: what is declared and shown is on screen."
  (let ((s (make-instance 'screen :name name :on-fault :restart)))
    (job:supervise s)
    (job:start s)
    s))

(defmethod ui:declared ((it ui:surface))
  "A surface declared while a screen is up goes up on it. The screen finds itself
in the namespace: the surface layer holds no pointer to whatever is painting, and
there being nothing painting is not a case anybody has to write down."
  (let ((s (job:named "screen")))
    (when (and (typep s 'screen) (job:alivep s))
      (tell s (lambda () (took-up s (node:name it)))))))

(defun close-screen (&optional (name "screen"))
  (let ((s (job:named name)))
    (when (typep s 'screen)
      (setf (done s) t)
      (when (pump s) (pump:wake (pump s)))
      (job:stop s))
    s))

(defmethod pine:opening ((what (eql :display)))
  "Loading this is what says pine can paint. Nothing writes a closure anywhere:
the method is here, in the file that knows how, and there being no display to
open is answered here too."
  (when (availablep) (open-screen)))
