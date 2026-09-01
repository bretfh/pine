(defpackage #:pine/host
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree) (#:mount #:pine/fs/mount)
                    (#:job #:pine/run/job) (#:system #:pine/run/system)
                    (#:actors #:pine/run/actors) (#:watch #:pine/run/watch)
                    (#:command #:pine/run/command) (#:fault #:pine/run/fault)
                    (#:sh #:pine/host/shell) (#:dev #:pine/host/device)
                    (#:declared #:pine/host/declared))
  (:import-from #:pine/host/declared #:defdevice #:defbacking)
  (:export
   #:device #:defdevice #:defbacking))
(in-package #:pine/host)

(defvar *attending* nil)

(setf watch:*streaming* #'sh:streaming)

(defclass host (system:system) ()
  (:documentation "The machine, in the namespace: its devices, its filesystem, its
environment and what it will run.

A system like any other. /dev/audio is loaded the way the editor is, and nothing in
the substrate names either."))

(system:offers 'host)

(defun %make (name &rest arguments)
  "The device NAME names, made with ARGUMENTS.

Every device is a declaration. There is no second way of getting one, so a device a
config declared and a device pine ships are made by the same call -- which is the
whole of what makes /dev something you can add to."
  (apply #'declared:made name arguments))

(defun device (what &rest arguments)
  "Start what WHAT declared: the streams whose lines say the world behind it moved,
and its interval where it has no stream to speak for it.

A name in place of a node is made and put under /dev first:

  (device \"media\" :player \"emms\")"
  (let ((n (if (node:nodep what)
               what
               (let ((it (apply #'%make what arguments)))
                 (when it
                   (node:attach it (tree:ensure (tree:root) "dev")))))))
    (when (node:nodep n) (system:owned (node:full-name n)))
    (%attend n)))

(defun %attend (n)
  (when (node:nodep n)
    (let ((held (watch:following n)))
      (d:swap *attending* (lambda (all) (cons (list n held) all)))))
  n)

(defun attending () (mapcar #'first *attending*))

(defun leave ()
  (dolist (each *attending*)
    (destructuring-bind (n held) each
      (declare (ignore n))
      (watch:let-go held)))
  (setf *attending* nil)
  (sh:forget-all))

(command:defcommand "devices" () (:describes "what the machine has")
  (tree:listing (tree:at (tree:root) "dev")))

(command:defcommand "device" (name &rest arguments)
    (:describes "put a device in the tree")
  (let ((it (apply #'device name arguments)))
    (and it (node:full-name it))))

(command:defcommand "sh" (line) (:describes "run something")
  (sh:run-line (princ-to-string line)))

(defmethod job:start ((s host))
  (let ((root (tree:root)))
    (system:puts (sh:sh-node) root)
    (device (system:puts (declared:made "env") root))
    (device (system:puts (declared:made "sys") root))
    (system:owned (node:full-name (mount:mount #p"/" root "file")))
    (device "clock")
    (job:supervise
     (job:start (make-instance 'job:tick :name "clock" :every 1
                                           :on-fault :leave
                                           :runs #'dev:tick)))
    root)
  s)

(defmethod job:stop ((s host))
  "The paths go by what OWNED was told as they went up. What is left here is the
streams and the ticks, which are running rather than standing."
  (leave)
  s)

