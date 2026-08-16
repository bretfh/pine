(defpackage #:pine/run/system
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree) (#:job #:pine/run/job)
                    (#:log #:pine/run/log))
  (:export #:system #:offers #:use #:drop #:systems #:named #:attach #:offered
           #:*on-loaded*))
(in-package #:pine/run/system)

(defvar *offered* (d:table)
  "Name to the class a loaded system offers. A system says this as its code loads,
so USE has something to make once the asdf system is there.")

(defvar *under* nil)
(defvar *on-loaded* nil
  "Told that a system's code is in the image. What somebody writes their own in
takes its vocabulary from what is loaded, so it is built again here.")

(defclass system (job:job) ()
  (:documentation "A package pine loaded. It starts and stops like anything else
that runs, which is what replaces an INSTALL called by hand in a fixed order.

Nothing here is privileged: the editor, the desktop, the window manager and the
machine's own devices are all this class, and so is anything you write."))

(defclass systems-node (node:node)
  ((livep  :allocation :class :initform t   :reader node:livep)
   (savedp :allocation :class :initform nil :reader node:savedp)))

(defun offers (class &optional (name (string-downcase (symbol-name class))))
  "Say what class this system is. Written once, where the system is defined."
  (d:keep! *offered* name class)
  name)

(defun offered () (d:keys (d:all *offered*)))

(defun systems ()
  (remove-if-not (lambda (j) (typep j 'system)) (job:jobs)))

(defun named (name)
  (let ((j (job:named (string-downcase (princ-to-string name)))))
    (and (typep j 'system) j)))

(defmethod node:nodes ((n systems-node)) (systems))

(defmethod node:resolve ((n systems-node) name)
  (find (princ-to-string name) (systems) :key #'job:name :test #'equal))

(defmethod node:contents ((n systems-node)) (mapcar #'job:name (systems)))

(defun attach (root)
  (setf *under* (node:attach (make-instance 'systems-node :name "system"
                                            :describes "what pine has loaded")
                             root))
  (dolist (s (systems) *under*) (setf (node:over s) *under*)))

(defun use (name)
  "Load a system and start it. /system/<name> is a node afterwards, so
pine write /system/desk '(:stop)' takes it away again."
  (let ((name (string-downcase (princ-to-string name))))
    (or (named name)
        (progn
          (unless (d:at (d:all *offered*) name)
            (asdf:load-system (format nil "pine/~a" name))
            (when *on-loaded* (funcall *on-loaded*)))
          (let ((class (d:at (d:all *offered*) name)))
            (unless class
              (error "~a loaded but offered no system class." name))
            (let ((s (make-instance class :name name :restarts nil)))
              (when *under* (setf (node:over s) *under*))
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
