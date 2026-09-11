(defpackage #:pine/run/module
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:fs #:pine/fs) (#:sb-mop #:sb-mop) (#:job #:pine/run/job)
                    (#:command #:pine/run/command) (#:log #:pine/fs/log)
                    (#:fault #:pine/run/fault))
  (:export
   #:module #:use #:drop #:modules
   #:named #:kinds))
(in-package #:pine/run/module)

(defclass module (job:job) ())

(defun %classes ()
  (labels ((under (c) (cons c (mapcan #'under (sb-mop:class-direct-subclasses c)))))
    (remove (find-class 'module) (under (find-class 'module)))))

(defun %class (name)
  (let ((name (string-downcase (princ-to-string name))))
    (find name (%classes)
          :key (lambda (c) (string-downcase (symbol-name (class-name c))))
          :test #'equal)))

(defun %take-down (home)
  (labels ((sweep (d)
             (dolist (each (fs:children d))
               (cond ((equal (fs:owner each) home)
                      (fault:or-nothing "what a module put up may have gone already"
                        (if (typep each 'job:job)
                            (job:forget (fs:name each))
                            (fs:detach d (fs:name each)))))
                     (t (fs:let-go each home)
                        (when (and (typep each 'fs:mount) (not (fs:volatile-p each)))
                          (sweep each)))))))
    (sweep (fs:root))))

(defmethod job:start :around ((s module))
  (let ((fs:*owner* (job:name s))
        (fs:*declaring* t))
    (call-next-method)))

(defmethod job:stop ((s module))
  s)

(defmethod job:stop :after ((s module))
  (%take-down (job:name s)))

(defun kinds ()
  (mapcar (lambda (c) (string-downcase (symbol-name (class-name c)))) (%classes)))

(defun modules ()
  (remove-if-not (lambda (j) (typep j 'module)) (job:jobs)))

(defun named (name)
  (let ((j (job:named (string-downcase (princ-to-string name)))))
    (and (typep j 'module) j)))

(defun use (name)
  (let ((name (string-downcase (princ-to-string name))))
    (or (named name)
        (progn
          (unless (%class name)
            (asdf:load-system (if (asdf:find-system name nil)
                                  name
                                  (format nil "pine/~a" name))))
          (let ((class (%class name)))
            (unless class
              (error "~a loaded but is not a module." name))
            (let ((s (make-instance (class-name class) :name name :on-fault :leave)))
              (job:supervise s)
              (job:start s)
              (log:note "~a is up" name)
              s))))))

(defun drop (name)
  (let ((s (named name)))
    (when s
      (job:stop s)
      (job:forget (job:name s))
      (log:note "~a is down" (job:name s)))
    s))
