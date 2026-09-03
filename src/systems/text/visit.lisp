(in-package #:pine/text)

(defun recent () *recent*)

(defun %recently (name)
  (setf *recent* (cons name (remove name *recent* :test #'equal)))
  (when (> (length *recent*) +recent-kept+)
    (setf *recent* (subseq *recent* 0 +recent-kept+)))
  name)

(defun %on-the-host (where)
  "The place on this machine's filesystem that WHERE names, whether or not a file
is there yet: opening one that is not is a document on a place, not on a file."
  (let ((at (tree:at "/file"))
        (names (tree:split-name (namestring where))))
    (loop :while (and at (rest names))
          :do (setf at (node:resolve at (pop names))))
    (and at names (mount:node-for at (first names)))))

(defun %place (where)
  "The node WHERE names, or a place on the host where the tree has none. AT says
what a node, a path and a name each mean, so there is nothing to ask here."
  (or (tree:at where) (%on-the-host where)))

(defun visit (document where)
  "Open DOCUMENT onto WHERE: a file on the host, or any node in the tree. What it
shows is whatever stands there, so /metric/frame reads like a file does."
  (let ((n (%place where)))
    (when n
      (setf (source document) n)
      (%recently (origin document))
      (setf (node:contents document) (or (node:contents n) ""))
      (let ((m (mode:mode-for (origin document))))
        (when m (setf (mode-of document) m)))
      (let ((had (visited document)))
        (if had (goto document (first had) (second had))
            (goto document 0 0)))
      (restructure document)
      (setf (modified document) nil))
    document))

(defmethod visiting ((document document) where)
  "Writing a document's source opens it onto what stands there. The document
declares that somebody might know how; this is the somebody."
  (visit document where))

(defun save (document &optional where)
  "Write what DOCUMENT holds back where it came from. Whatever stands there says
what writing means: a file is written, a device is acted on."
  (when where (setf (source document) (%place where)))
  (let ((n (source document)))
    (when n
      (mode:saving (mode-of document) document)
      (setf (node:contents n) (text document))
      (setf (modified document) nil)
      (origin document))))

(defun revert (document)
  (let ((n (source document)))
    (when (and n (node:contents n))
      (leaving document)
      (visit document n))))

(defun %syntax ()
  "Load tree-sitter and put the languages in the tree. A grammar that will not
load is a fault like any other: the text still opens, uncoloured."
  (let ((it (make-ts-runtime)))
    (fault:attempt (lambda () (ensure-ts it)) "loading tree-sitter")
    (when (ts-loaded-p it)
      (setf *runtime* it)
      (lang-node (tree:root)))))

(command:defcommand "documents" () (:describes "every document there is")
  (mapcar #'node:name (documents)))

(command:defcommand "structure" (&optional name)
    (:describes "what this document's mode makes of it")
  (let ((d (if name (tree:at (root) name) (current))))
    (when d (mapcar #'node:name (regions d)))))

(defmethod job:start ((s text))
  (%syntax)
  (system:puts (mode:mode-node))
  (root)
  (let ((scratch (make-document "scratch" :mode (make-instance 'mode:lisp))))
    (setf (current) scratch))
  s)

(defmethod job:stop ((s text))
  "What it put up goes by what OWNED was told as it went up. The documents are its
own doing rather than a path it attached, so they are named here."
  (forget-all)
  (dolist (d (documents)) (kill (node:name d)))
  s)

