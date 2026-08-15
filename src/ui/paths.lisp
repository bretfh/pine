(defpackage #:pine/ui/paths
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node) (#:tree #:pine/fs/tree)
                    (#:world #:pine/world/world)
                    (#:face #:pine/ui/face) (#:css #:pine/ui/css))
  (:export #:themes-node #:faces-node #:install #:face-plist))
(in-package #:pine/ui/paths)

(defclass themes-node (node:node) ())
(defclass theme-node (node:node) ())
(defclass faces-node (node:node) ())
(defclass face-node (node:node) ())

(defun face-plist (f)
  (when f
    (list :fg (face:fg f) :bg (face:bg f) :bold (face:bold f)
          :italic (face:italic f) :underline (face:underline f))))

(defun %theme-names ()
  (sort (d:keys (d:all face:*themes*)) #'string< :key #'symbol-name))

(defun %theme (n name)
  (node:child n name
              (lambda () (make-instance 'theme-node :name name :parent n))))

(defun %active (n)
  "Which theme is on. A value like any other, kept with the themes rather than
loose at the root beside them."
  (node:child n "active" (lambda () (node:make-node "active" :parent n))))

(defmethod node:nodes ((n themes-node))
  (cons (%active n)
        (loop :for name :in (%theme-names)
              :collect (%theme n (string-downcase (symbol-name name))))))

(defmethod node:resolve ((n themes-node) name)
  (cond ((equal name "active") (%active n))
        ((member name (%theme-names)
                 :key (lambda (each) (string-downcase (symbol-name each)))
                 :test #'equal)
         (%theme n name))))

(defmethod node:contents ((n themes-node)) (%theme-names))
(defmethod node:contents ((n theme-node))
  "The theme this path names, as it stands now. A node that kept the theme it
was made with would go on answering the one it was made with."
  (let ((it (face:find-theme (node:name n))))
    (list :palette (face:theme-palette it) :metrics (face:theme-metrics it))))
(defmethod node:leafp ((n theme-node)) t)
(defmethod node:persistp ((n themes-node)) nil)
(defmethod node:persistp ((n theme-node)) nil)

(defun %face (n name)
  (node:child n name
              (lambda () (make-instance 'face-node :name name :parent n))))

(defun %key (name)
  "NAME as the keyword a face is kept under, without making one that is not
already there: a path nobody named should not grow the keyword package."
  (find-symbol (string-upcase (princ-to-string name)) :keyword))

(defun %face-of (n)
  (let ((key (%key (node:name n))))
    (and key (face:find-face key))))

(defmethod node:nodes ((n faces-node))
  (let (acc)
    (maphash (lambda (name f)
               (declare (ignore f))
               (push (%face n (string-downcase (symbol-name name))) acc))
             (face:faces))
    (sort acc #'string< :key #'node:name)))

(defmethod node:resolve ((n faces-node) name)
  (let ((key (%key name)))
    (when (and key (face:find-face key)) (%face n name))))

(defmethod node:contents ((n faces-node))
  (mapcar #'node:name (node:nodes n)))
(defmethod node:contents ((n face-node)) (face-plist (%face-of n)))
(defmethod node:leafp ((n face-node)) t)
(defmethod node:persistp ((n faces-node)) nil)
(defmethod node:persistp ((n face-node)) nil)

(defun install (&optional (w world:*world*))
  (let* ((root (world:root w))
         (themes (node:attach (make-instance 'themes-node :name "theme"
                                                          :describes "every theme there is, and which is on")
                              root)))
    (node:attach (make-instance 'faces-node :name "faces"
                                            :describes "every face in force")
                 root)
    (world:ensure w "face")
    (world:ensure w "style")
    (let ((active (%active themes)))
      (unless (node:contents active)
        (setf (node:contents active) face:+default-theme+)))
    (css:install (css:stylesheet))
    w))
