(defpackage #:pine/host
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree) (#:mount #:pine/fs/mount)
                    (#:job #:pine/run/job) (#:system #:pine/run/system)
                    (#:actors #:pine/run/actors) (#:watch #:pine/run/watch)
                    (#:command #:pine/run/command) (#:fault #:pine/run/fault)
                    (#:sh #:pine/host/shell) (#:device #:pine/host/device))
  (:export #:host #:attend #:leave #:attending))
(in-package #:pine/host)

(defvar *attending* (d:box nil))

(defclass host (system:system) ()
  (:documentation "The machine, in the namespace: its devices, its filesystem, its
environment and what it will run.

A system like any other. /dev/audio is loaded the way the editor is, and nothing in
the substrate names either."))

(system:offers 'host)

(defun attend (n)
  "Start what N declared: the streams whose lines say the world behind it moved, and
its interval where it has no stream to speak for it."
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
                                           :only nil :poll nil
                                           :name (format nil "~a<-~a"
                                                         (node:name n) line)))))
                                    (node:announces n))))
          (ticking (let ((seconds (node:refreshes n)))
                     (when seconds
                       (actors:repeat seconds (lambda () (node:stir n))
                                      :as (list :host (node:full-name n))
                                      :what (node:name n))))))
      (d:swap! *attending* (lambda (all) (cons (list n watching ticking) all)))))
  n)

(defun attending () (mapcar #'first (d:held *attending*)))

(defun leave ()
  (dolist (each (d:held *attending*))
    (destructuring-bind (n watching ticking) each
      (declare (ignore n))
      (mapc #'watch:unwatch watching)
      (when ticking (actors:cancel ticking))))
  (d:put! *attending* nil)
  (sh:forget-all))

(defmethod job:start ((s host))
  (let ((root (tree:root)))
    (sh:attach root)
    (attend (node:attach (device:env) root))
    (attend (node:attach (device:sys) root))
    (mount:mount #p"/" root "file")
    (dolist (make (list #'device:clock))
      (attend (node:attach (funcall make) (tree:ensure root "dev"))))
    (job:supervise
     (job:start (make-instance 'job:thread :name "clock" :seconds 1
                                           :restarts nil
                                           :thunk #'device:tick)))
    (command:defcommand "devices" () (:describes "what the machine has")
      (tree:listing (tree:at root "dev")))
    (command:defcommand "attend" (name) (:describes "put a device in the tree")
      (let ((make (find-symbol (string-upcase (princ-to-string name))
                               :pine/host/device)))
        (when (and make (fboundp make))
          (let ((it (funcall make)))
            (node:attach it (tree:ensure root "dev"))
            (attend it)
            (node:full-name it)))))
    (command:defcommand "sh" (line) (:describes "run something")
      (sh:run-line (princ-to-string line))))
  s)

(defmethod job:stop ((s host))
  (leave)
  s)
