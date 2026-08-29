(defpackage #:pine/host
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree) (#:mount #:pine/fs/mount)
                    (#:job #:pine/run/job) (#:system #:pine/run/system)
                    (#:actors #:pine/run/actors) (#:watch #:pine/run/watch)
                    (#:command #:pine/run/command) (#:fault #:pine/run/fault)
                    (#:sh #:pine/host/shell) (#:device #:pine/host/device))
  (:export
   #:attend))
(in-package #:pine/host)

(defvar *attending* nil)

(defclass host (system:system) ()
  (:documentation "The machine, in the namespace: its devices, its filesystem, its
environment and what it will run.

A system like any other. /dev/audio is loaded the way the editor is, and nothing in
the substrate names either."))

(system:offers 'host)

(defun %make (name &rest arguments)
  "The device NAME names, made. What builds one is a function DEVICE exports under
that name and that answers a node; a name it keeps to itself is not a device, and
neither is one that answers something else, however well either reads."
  (multiple-value-bind (make status)
      (find-symbol (string-upcase (princ-to-string name)) :pine/host/device)
    (when (and (eq :external status) (fboundp make))
      (let ((it (apply make arguments)))
        (when (node:nodep it) it)))))

(defun attend (what &rest arguments)
  "Start what WHAT declared: the streams whose lines say the world behind it moved,
and its interval where it has no stream to speak for it.

A name in place of a node is made and put under /dev first:

  (attend \"media\" :player \"emms\")"
  (let ((n (if (node:nodep what)
               what
               (let ((it (apply #'%make what arguments)))
                 (when it
                   (node:attach it (tree:ensure (tree:root) "dev")))))))
    (%attend n)))

(defun %attend (n)
  (when (node:nodep n)
    (let ((watching (remove nil
                            (mapcar (lambda (line)
                                      (let ((s (sh:streaming line)))
                                        (when s
                                          (watch:watch
                                           s
                                           (lambda (of said)
                                             (declare (ignore of said))
                                             (fault:attempt (lambda () (node:stir n))
                                                            (node:name n)))
                                           :tells-when :always :poll nil
                                           :name (format nil "~a<-~a"
                                                         (node:name n) line)))))
                                    (node:announces n))))
          (ticking (let ((seconds (node:refreshes n)))
                     (when seconds
                       (actors:repeat seconds (lambda () (node:stir n))
                                      :as (list :host (node:full-name n))
                                      :what (node:name n))))))
      (d:swap *attending* (lambda (all) (cons (list n watching ticking) all)))))
  n)

(defun attending () (mapcar #'first *attending*))

(defun leave ()
  (dolist (each *attending*)
    (destructuring-bind (n watching ticking) each
      (declare (ignore n))
      (mapc #'watch:unwatch watching)
      (when ticking (actors:cancel ticking))))
  (setf *attending* nil)
  (sh:forget-all))

(command:defcommand "devices" () (:describes "what the machine has")
  (tree:listing (tree:at (tree:root) "dev")))

(command:defcommand "attend" (name &rest arguments)
    (:describes "put a device in the tree")
  (let ((it (apply #'attend name arguments)))
    (and it (node:full-name it))))

(command:defcommand "sh" (line) (:describes "run something")
  (sh:run-line (princ-to-string line)))

(defmethod job:start ((s host))
  (let ((root (tree:root)))
    (node:attach (sh:sh-node) root)
    (attend (node:attach (device:env) root))
    (attend (node:attach (device:sys) root))
    (mount:mount #p"/" root "file")
    (dolist (make (list #'device:clock))
      (attend (node:attach (funcall make) (tree:ensure root "dev"))))
    (job:supervise
     (job:start (make-instance 'job:tick :name "clock" :every 1
                                           :on-fault :leave
                                           :runs #'device:tick)))
    root)
  s)

(defmethod job:stop ((s host))
  (leave)
  s)

