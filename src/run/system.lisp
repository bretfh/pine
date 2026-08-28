(defpackage #:pine/run/system
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:tree #:pine/fs/tree) (#:job #:pine/run/job)
                    (#:command #:pine/run/command) (#:log #:pine/fs/log))
  (:export
   #:system #:offers #:use #:drop #:systems
   #:named #:offered))
(in-package #:pine/run/system)

(defvar *offered* (d:table)
  "Name to the class a loaded system offers. A system says this as its code loads,
so USE has something to make once the asdf system is there.")

(defvar *owns* (d:table)
  "Name to the package a system's code is written in. What it defined there stands
while it runs, which is what replaces a list of names to forget by hand.")

(defvar *under* nil)

(defclass system (job:job) ()
  (:documentation "A package pine loaded. It starts and stops like anything else
that runs, which is what replaces an INSTALL called by hand in a fixed order.

Nothing here is privileged: the editor, the desktop, the window manager and the
machine's own devices are all this class, and so is anything you write."))

(defun offers (class &optional (name (string-downcase (symbol-name class))))
  "Say what class this system is, and that what is written here is its. Written
once, where the system is defined."
  (d:keep! *offered* name class)
  (d:keep! *owns* name (string-downcase (package-name *package*)))
  (command:claim)
  name)

(defun owns (name)
  (d:lookup (d:all *owns*) (string-downcase (princ-to-string name))))

(defmethod job:start :before ((s system))
  (let ((prefix (owns (job:name s)))) (when prefix (command:offer prefix))))

(defmethod job:stop :after ((s system))
  "What a system defined goes when it does. Nothing keeps a list of names: a command
knows the package it was written in, and the system knows which package is its."
  (let ((prefix (owns (job:name s)))) (when prefix (command:withdraw prefix))))

(defun offered () (d:keys (d:all *offered*)))

(defun systems ()
  (remove-if-not (lambda (j) (typep j 'system)) (job:jobs)))

(defun named (name)
  (let ((j (job:named (string-downcase (princ-to-string name)))))
    (and (typep j 'system) j)))

(defun %attach (root)
  (setf *under* (node:attach (node:place "system" :nodes #'systems
                                         :describes "what pine has loaded")
                             root))
  (dolist (s (systems) *under*) (setf (node:over s) *under*)))

(defun use (name)
  "Load a system and start it. /system/<name> is a node afterwards, so
pine write /system/desk '(:stop)' takes it away again."
  (let ((name (string-downcase (princ-to-string name))))
    (or (named name)
        (progn
          (unless (d:lookup (d:all *offered*) name)
            (asdf:load-system (format nil "pine/~a" name))
            (pine/word:user))
          (let ((class (d:lookup (d:all *offered*) name)))
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

(pine/word:lends "system" "offers")

(pine/fs/tree:builder #'%attach)
