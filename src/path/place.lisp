(defpackage #:pine/path/place
  (:use #:cl)
  (:local-nicknames (#:node #:pine/fs/node) (#:tree #:pine/fs/tree)
                    (#:path #:pine/path/path) (#:world #:pine/world/world))
  (:export #:at #:ensure #:contents #:matching #:erase))
(in-package #:pine/path/place)

(defun %in (where)
  (etypecase where
    (world:world (world:root where))
    (node:node where)
    (null (world:root world:*world*))))

(defun at (p &optional (where world:*world*))
  (apply #'tree:at (%in where) (mapcar #'path:value (path:segments p))))

(defun ensure (p &optional (where world:*world*))
  (apply #'tree:ensure (%in where) (mapcar #'path:value (path:segments p))))

(defun erase (p &optional (where world:*world*))
  (apply #'tree:erase (%in where) (mapcar #'path:value (path:segments p))))

(defun contents (p &optional (where world:*world*))
  (let ((n (at p where)))
    (and n (node:contents n))))

(defun (setf contents) (value p &optional (where world:*world*))
  (setf (node:contents (ensure p where)) value))

(defun matching (pattern &optional (where world:*world*))
  (let ((found nil))
    (tree:walk (%in where)
               (lambda (each)
                 (let ((subject (path:parse (node:full-name each))))
                   (when (path:match pattern subject)
                     (push each found)))))
    (nreverse found)))
