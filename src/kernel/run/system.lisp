(defpackage #:pine/run/system
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node) (#:sb-mop #:sb-mop)
                    (#:tree #:pine/fs/tree) (#:job #:pine/run/job)
                    (#:command #:pine/run/command) (#:log #:pine/fs/log)
                    (#:fault #:pine/run/fault))
  (:export
   #:system #:use #:drop #:systems
   #:named #:kinds #:puts #:owned #:undoes #:*owner* #:*undoes*))
(in-package #:pine/run/system)

(defvar *under* nil)

(defvar *put* (d:table)
  "What each system put up while it ran, by the package its code is written in: the
paths it attached and the chords it bound, newest last.

The same answer as *OWNS* is for commands, for the two things a system puts up that
a command is not. Without it a system took its commands away when it stopped and
left its nodes and its surfaces standing, so DROP half worked and every app had to
finish the job by hand -- naming its own paths in its own STOP, which is a list to
keep in step with the code that made them.")

(defvar *owner* nil
  "The package of the system that is starting or stopping, while it does.

Bound around START and STOP rather than passed, because what puts a node up is the
app's own code and threading an owner through it would be asking every app to say
twice what package it is written in.")

(defvar *undoes* (d:table)
  "How to take back each kind of thing a system can put up, by the keyword that
names that kind. Filled in by whatever knows what the kind is, because this layer
loads before any of them -- the same reason NODE:*ELSEWHERE* is a variable.

One table and not one variable per kind. A system can put up a chord, a style
property, a theme, a way of answering a prompt, a device declaration and a surface,
and every one of those lives in a layer this one cannot name; a variable each would
mean editing this file to add a sixth.")

(defclass system (job:job) ()
  (:documentation "A package pine loaded. It starts and stops like anything else
that runs, which is what replaces an INSTALL called by hand in a fixed order.

Nothing here is privileged: the editor, the desktop, the window manager and the
machine's own devices are all this class, and so is anything you write."))

(defun %classes ()
  "Every kind of system there is, found in the class graph rather than a list: a
class that subclasses SYSTEM is a system, and nothing has to say so twice."
  (labels ((under (c) (cons c (mapcan #'under (sb-mop:class-direct-subclasses c)))))
    (remove (find-class 'system) (under (find-class 'system)))))

(defun %class (name)
  "The system called NAME, by its class name. A system is named by what it is."
  (let ((name (string-downcase (princ-to-string name))))
    (find name (%classes)
          :key (lambda (c) (string-downcase (symbol-name (class-name c))))
          :test #'equal)))

(defun %package (class)
  (string-downcase (package-name (symbol-package (class-name class)))))

(defun owns (name)
  (let ((c (%class name))) (and c (%package c))))

(defun undoes (kind taking)
  "Say how to take back what OWNED was told as KIND. Said by the layer that knows
what that kind is, in the file that puts one up."
  (d:keep! *undoes* kind taking)
  kind)

(defun owned (what &optional (home *owner*))
  "Say the system that is starting put WHAT up. A string is a path to erase; a list
is (KIND . ARGUMENTS), handed back to whatever said how to take that kind back.

Called by whatever does the putting -- MAKE-SURFACE, BIND -- so an app declares a
thing once and the undoing is not its to write."
  (when home
    (d:update! *put* home (lambda (had) (append had (list what)))))
  what)

(defun puts (node &optional (into (tree:root)))
  "Attach NODE, and say the running system put it there. What a system puts up it
takes down: this is ATTACH for an app, and the reason an app needs no STOP."
  (owned (node:full-name (node:attach node into)))
  node)

(defun %take-down (home)
  "Take off what the system written in HOME put up, newest first.

Newest first because a system may put a node under one it put up itself, and taking
the branch off first would leave the erase of what was under it looking for a parent
that has gone."
  (dolist (what (reverse (or (d:lookup (d:all *put*) home) nil)))
    (etypecase what
      (string (fault:or-nothing "a path a system put up may have gone already"
                (tree:erase what)))
      (cons (let ((taking (d:lookup (d:all *undoes*) (first what))))
              (when taking
                (fault:or-nothing "what a system put up may have gone already"
                  (apply taking (rest what))))))))
  (d:drop! *put* home))

(defmethod job:start :around ((s system))
  "What the system puts up while it starts is its. Cleared first, so a system
started again does not carry what the last run put up."
  (let ((home (owns (job:name s))))
    (when home (d:drop! *put* home))
    (let ((*owner* home))
      (call-next-method))))

(defmethod job:start :after ((s system))
  "After, not before: a system may define commands as it starts, and those are
its as much as the ones its file defined."
  (let ((prefix (owns (job:name s)))) (when prefix (command:offer prefix))))

(defmethod job:stop ((s system))
  "A system that only puts things up has nothing of its own to stop: what it put up
goes in the :AFTER, by what OWNED was told as it went up. One that runs something of
its own -- a thread, a child image -- says so by defining this method.

A JOB has to say how it stops, because a job is something running and one that
cannot be stopped is a leak. A system is the kind of job where declaring is the
whole of what most of them do, and making every app write an empty STOP to say so
was asking each of them to keep a list of what to undo."
  s)

(defmethod job:stop :after ((s system))
  "What a system defined goes when it does. Nothing keeps a list of names: a command
knows the package it was written in, and the system knows which package is its.

Its nodes and its surfaces go the same way, by what OWNED was told as they went up.
An app that puts up a place and a surface writes no STOP at all.

Its commands and not its words: a command that has stood down is one nothing will
run, while a word still names the class it named, because a system that is stopped
is still a system that is loaded."
  (let ((prefix (owns (job:name s))))
    (when prefix
      (command:withdraw prefix)
      (let ((*owner* prefix)) (%take-down prefix)))))

(defun kinds ()
  "Every system there is to load, running or not."
  (mapcar (lambda (c) (string-downcase (symbol-name (class-name c)))) (%classes)))

(defun systems ()
  (remove-if-not (lambda (j) (typep j 'system)) (job:jobs)))

(defun named (name)
  (let ((j (job:named (string-downcase (princ-to-string name)))))
    (and (typep j 'system) j)))

(defun %attach (root)
  (setf *under* (node:attach (make-instance 'node:place :name "system"
                                         :nodes #'systems
                                         :describes "what pine has loaded")
                             root))
  (dolist (s (systems) *under*) (setf (node:parent s) *under*)))

(defun use (name)
  "Load a system and start it. /system/<name> is a node afterwards, so
pine write /system/desk '(:stop)' takes it away again."
  (let ((name (string-downcase (princ-to-string name))))
    (or (named name)
        (progn
          (unless (%class name)
            (asdf:load-system (if (asdf:find-system name nil)
                                  name
                                  (format nil "pine/~a" name))))
          (let ((class (%class name)))
            (unless class
              (error "~a loaded but is not a system." name))
            (command:claim (%package class))
            (let ((s (make-instance (class-name class) :name name :on-fault :leave)))
              (when *under* (setf (node:parent s) *under*))
              (job:supervise s)
              (job:start s)
              (log:note "~a is up" name)
              s))))))

(defun drop (name)
  "Stop a system and take it off the tree."
  (let ((s (named name)))
    (when s
      (job:stop s)
      (job:forget (job:name s))
      (log:note "~a is down" (job:name s)))
    s))


(pine/fs/tree:builder #'%attach)
