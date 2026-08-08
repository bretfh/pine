(defpackage #:pine.provider.env
  (:use #:cl)
  (:local-nicknames (#:node #:pine.fs.node))
  (:export #:env-node #:variable-node #:install #:names))

(in-package #:pine.provider.env)

(defclass env-node (node:node) ())

(defclass variable-node (node:node) ())

(defun names ()
  (sort (mapcar (lambda (entry) (subseq entry 0 (position #\= entry)))
                (sb-ext:posix-environ))
        #'string<))

(defmethod node:nodes ((n env-node))
  (loop :for name :in (names)
        :collect (make-instance 'variable-node :name name :parent n)))

(defmethod node:resolve ((n env-node) name)
  (make-instance 'variable-node :name name :parent n))

(defmethod node:contents ((n env-node)) (names))

(defmethod node:contents ((n variable-node)) (uiop:getenv (node:name n)))

(defmethod (setf node:contents) (value (n variable-node))
  (if (null value)
      (sb-posix:unsetenv (node:name n))
      (sb-posix:setenv (node:name n) (princ-to-string value) 1))
  value)

(defmethod node:leafp ((n variable-node)) t)
(defmethod node:persistp ((n env-node)) nil)
(defmethod node:persistp ((n variable-node)) nil)

(defmethod node:livep ((n env-node)) t)
(defmethod node:livep ((n variable-node)) t)

(defun install (root)
  (node:attach (make-instance 'env-node :name "env"
                                        :describes "the environment this image was started in")
               root))
