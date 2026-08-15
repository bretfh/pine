(defpackage #:pine.ui.paths
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node) (#:tree #:pine/fs/tree)
                    (#:world #:pine.world.world)
                    (#:face #:pine.ui.face) (#:css #:pine.ui.css))
  (:export #:themes-node #:faces-node #:install #:face-plist))

(in-package #:pine.ui.paths)

(defclass themes-node (node:node) ())
(defclass theme-node (node:node)
  ((theme :initarg :theme :reader theme)))
(defclass faces-node (node:node) ())
(defclass face-node (node:node)
  ((face-of :initarg :face :reader face-of)))

(defun face-plist (f)
  (when f
    (list :fg (face:fg f) :bg (face:bg f) :bold (face:bold f)
          :italic (face:italic f) :underline (face:underline f))))

(defun %theme-names ()
  (sort (d:keys (d:all face:*themes*)) #'string< :key #'symbol-name))

(defmethod node:nodes ((n themes-node))
  (loop :for name :in (%theme-names)
        :collect (make-instance 'theme-node :name (string-downcase (symbol-name name))
                                            :parent n
                                            :theme (face:find-theme name))))

(defmethod node:resolve ((n themes-node) name)
  (let ((it (ignore-errors (face:find-theme name))))
    (when it
      (make-instance 'theme-node :name name :parent n :theme it))))

(defmethod node:contents ((n themes-node)) (%theme-names))
(defmethod node:contents ((n theme-node))
  (list :palette (face:theme-palette (theme n))
        :metrics (face:theme-metrics (theme n))))
(defmethod node:leafp ((n theme-node)) t)
(defmethod node:persistp ((n themes-node)) nil)
(defmethod node:persistp ((n theme-node)) nil)

(defmethod node:nodes ((n faces-node))
  (let (acc)
    (maphash (lambda (name f)
               (push (make-instance 'face-node
                                    :name (string-downcase (symbol-name name))
                                    :parent n :face f)
                     acc))
             (face:faces))
    (sort acc #'string< :key #'node:name)))

(defmethod node:resolve ((n faces-node) name)
  (let ((f (face:find-face (intern (string-upcase name) :keyword))))
    (when f (make-instance 'face-node :name name :parent n :face f))))

(defmethod node:contents ((n faces-node))
  (mapcar #'node:name (node:nodes n)))
(defmethod node:contents ((n face-node)) (face-plist (face-of n)))
(defmethod node:leafp ((n face-node)) t)
(defmethod node:persistp ((n faces-node)) nil)
(defmethod node:persistp ((n face-node)) nil)

(defun install (&optional (w world:*world*))
  (let ((root (world:root w)))
    (node:attach (make-instance 'themes-node :name "theme"
                                             :describes "every theme there is")
                 root)
    (node:attach (make-instance 'faces-node :name "faces"
                                            :describes "every face in force")
                 root)
    (world:ensure w "face")
    (world:ensure w "style")
    (let ((active (world:ensure w "active-theme")))
      (unless (node:contents active)
        (setf (node:contents active) face:+default-theme+)))
    (css:install (css:stylesheet))
    w))
