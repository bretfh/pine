(defpackage #:pine/edit/source
  (:use #:cl)
  (:local-nicknames (#:node #:pine/fs/node) (#:mount #:pine/fs/mount)
                    (#:tree #:pine/fs/tree) (#:world #:pine/world/world)
                    (#:mode #:pine/repl/mode) (#:buffer #:pine/edit/buffer))
  (:export #:install #:source-for #:visit! #:save! #:revert! #:recent))
(in-package #:pine/edit/source)

(defvar *recent* nil)
(defparameter *kept* 50)

(defun recent ()
  "The places opened here, the last one first."
  *recent*)

(defun %recently (name)
  (setf *recent* (cons name (remove name *recent* :test #'equal)))
  (when (> (length *recent*) *kept*)
    (setf *recent* (subseq *recent* 0 *kept*)))
  name)

(defun %root ()
  (and world:*world* (world:root world:*world*)))

(defun %under-file (path)
  "The place PATH names under the mounted host filesystem, whether or not
anything stands there yet."
  (let* ((root (%root))
         (n (and root (tree:at root "file")))
         (names (tree:split-name (namestring path))))
    (loop :while (and n (rest names))
          :do (setf n (node:resolve n (pop names))))
    (and n names (mount:place n (first names)))))

(defun source-for (place)
  "The node PLACE stands for: a node is itself, a path already in the tree is
the node there, and anything else names a place on the host."
  (cond ((node:nodep place) place)
        ((let ((root (%root)))
           (and root (stringp place) (plusp (length place))
                (tree:at root place))))
        (t (%under-file place))))

(defun visit! (b place)
  "Open B onto PLACE: a file on the host, or any node in the tree. What a
buffer shows is whatever stands there, so /metric/frame reads like a file does."
  (let ((n (source-for place)))
    (when n
      (setf (buffer:source b) n)
      (%recently (buffer:origin b))
      (setf (node:contents b) (or (node:contents n) ""))
      (let ((m (mode:mode-for (buffer:origin b))))
        (when m (setf (buffer:mode-of b) (mode:name m))))
      (let ((was (buffer:visited b)))
        (if was
            (buffer:goto! b (first was) (second was))
            (buffer:goto! b 0 0)))
      (setf (buffer:modified b) nil))
    b))

(defun save! (b &optional place)
  "Write what B holds back where it came from. Whatever stands there says what
writing means: a file is written, a provider is acted on."
  (when place (setf (buffer:source b) (source-for place)))
  (let ((n (buffer:source b)))
    (when n
      (setf (node:contents n) (buffer:text-of b))
      (setf (buffer:modified b) nil)
      (buffer:origin b))))

(defun revert! (b)
  "What the source says now, in place of what has been typed over it."
  (let ((n (buffer:source b)))
    (when (and n (node:contents n))
      (buffer:leaving! b)
      (visit! b n))))

(defun install ()
  (setf buffer:*visiting* #'visit!)
  t)
