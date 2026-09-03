(in-package #:pine/host)

(defvar *attending* nil)

(setf watch:*streaming* #'sh:streaming)

(defclass host (system:system) ()
  (:documentation "The machine, in the namespace: its devices, its filesystem, its
environment and what it will run.

A system like any other. /dev/audio is loaded the way the editor is, and nothing in
the substrate names either."))


(defun %make (name &rest arguments)
  "The device NAME names, made with ARGUMENTS.

Every device is a declaration. There is no second way of getting one, so a device a
config declared and a device pine ships are made by the same call -- which is the
whole of what makes /dev something you can add to."
  (apply #'made name arguments))

(defun device (what &rest arguments)
  "Start what WHAT declared: the streams whose lines say the world behind it moved,
and its interval where it has no stream to speak for it.

A name in place of a node is made and put under /dev first:

  (device \"media\" :player \"emms\")"
  (let ((n (if (fs:kind what)
               what
               (let ((it (apply #'%make what arguments)))
                 (when it
                   (fs:attach it (fs:ensure (fs:root) "dev")))))))
    (when (and (fs:kind n) fs:*owner*) (setf (fs:owner n) fs:*owner*))
    (%attend n)))

(defun %attend (n)
  (when (fs:kind n)
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
  (mapcar #'fs:name (fs:entries (fs:at (fs:root) "dev"))))

(command:defcommand "device" (name &rest arguments)
    (:describes "put a device in the tree")
  (let ((it (apply #'device name arguments)))
    (and it (fs:full-name it))))

(command:defcommand "sh" (line) (:describes "run something")
  (sh:run-line (princ-to-string line)))

(defmethod job:start ((s host))
  (let ((root (fs:root)))
    (system:puts (sh:sh-node) root)
    (device (system:puts (made "env") root))
    (device (system:puts (made "sys") root))
    (setf (fs:owner (mount:mount #p"/" root "file")) fs:*owner*)
    (device "clock")
    (job:supervise
     (job:start (make-instance 'job:tick :name "clock" :every 1
                                           :on-fault :leave
                                           :runs #'tick)))
    root)
  s)

(defmethod job:stop ((s host))
  "What it put in the tree goes with it. What is left here is the streams and the
ticks, which are running rather than standing."
  (leave)
  s)

