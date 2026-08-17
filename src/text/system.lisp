(defpackage #:pine/text
  (:use #:cl)
  (:local-nicknames (#:node #:pine/fs/node) (#:tree #:pine/fs/tree)
                    (#:mount #:pine/fs/mount) (#:job #:pine/run/job)
                    (#:system #:pine/run/system) (#:command #:pine/run/command)
                    (#:doc #:pine/text/document) (#:mode #:pine/text/mode)
                    (#:fault #:pine/run/fault)
                    (#:runtime #:pine/text/ts/runtime)
                    (#:syntax #:pine/text/ts/syntax)
                    (#:parser #:pine/text/ts/parser))
  (:export #:text #:visit #:save #:revert #:recent))
(in-package #:pine/text)

(defvar *recent* nil)
(defparameter +recent-kept+ 50)

(defclass text (system:system) ()
  (:documentation "Documents, and what their modes make of them."))

(system:offers 'text)

(defun recent () *recent*)

(defun %recently (name)
  (setf *recent* (cons name (remove name *recent* :test #'equal)))
  (when (> (length *recent*) +recent-kept+)
    (setf *recent* (subseq *recent* 0 +recent-kept+)))
  name)

(defun %place (where)
  "The node WHERE names: a node is itself, a path already in the tree is what stands
there, and anything else is a place on the host."
  (cond ((node:nodep where) where)
        ((and (stringp where) (plusp (length where)) (tree:at nil where)))
        (t (let* ((at (tree:at nil "file"))
                  (names (tree:split-name (namestring where))))
             (loop :while (and at (rest names))
                   :do (setf at (node:resolve at (pop names))))
             (and at names (mount:place at (first names)))))))

(defun visit (document where)
  "Open DOCUMENT onto WHERE: a file on the host, or any node in the tree. What it
shows is whatever stands there, so /metric/frame reads like a file does."
  (let ((n (%place where)))
    (when n
      (setf (doc:source document) n)
      (%recently (doc:origin document))
      (setf (node:contents document) (or (node:contents n) ""))
      (let ((m (mode:mode-for (doc:origin document))))
        (when m (setf (doc:mode-of document) m)))
      (let ((had (doc:visited document)))
        (if had (doc:goto document (first had) (second had))
            (doc:goto document 0 0)))
      (doc:restructure document)
      (setf (doc:modified document) nil))
    document))

(defun save (document &optional where)
  "Write what DOCUMENT holds back where it came from. Whatever stands there says
what writing means: a file is written, a device is acted on."
  (when where (setf (doc:source document) (%place where)))
  (let ((n (doc:source document)))
    (when n
      (mode:save (doc:mode-of document) document)
      (setf (node:contents n) (doc:text document))
      (setf (doc:modified document) nil)
      (doc:origin document))))

(defun revert (document)
  (let ((n (doc:source document)))
    (when (and n (node:contents n))
      (doc:leaving document)
      (visit document n))))

(defun %syntax ()
  "Load tree-sitter and put the languages in the tree. A grammar that will not
load is a fault like any other: the text still opens, uncoloured."
  (let ((it (runtime:make-ts-runtime)))
    (fault:attempt (lambda () (runtime:ensure-ts it)) "loading tree-sitter")
    (when (runtime:ts-loaded-p it)
      (setf parser:*runtime* it)
      (syntax:attach (tree:root)))))

(defmethod job:start ((s text))
  (setf doc:*visiting* #'visit
        doc:*on-kill* #'parser:forget)
  (%syntax)
  (mode:attach (tree:root))
  (doc:root)
  (command:defcommand "documents" () (:describes "every document there is")
    (mapcar #'node:name (doc:documents)))
  (command:defcommand "structure" (&optional name)
    (:describes "what this document's mode makes of it")
    (let ((d (if name (doc:named name) (doc:current))))
      (when d (mapcar #'node:name (doc:regions d)))))
  (let ((scratch (doc:make-document "scratch" :mode (make-instance 'mode:lisp))))
    (setf (doc:current) scratch))
  s)

(defmethod job:stop ((s text))
  (setf doc:*visiting* nil
        doc:*on-kill* nil)
  (parser:forget-all)
  (dolist (d (doc:documents)) (doc:kill (node:name d)))
  s)
