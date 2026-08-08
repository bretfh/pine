(defpackage #:pine.edit.buffer
  (:use #:cl)
  (:local-nicknames (#:c #:pine.run.cell) (#:node #:pine.fs.node)
                    (#:tree #:pine.fs.tree) (#:world #:pine.world.world)
                    (#:mode #:pine.repl.mode) (#:text #:pine.edit.text))
  (:export #:buffer #:slot-node #:make-buffer #:buffers #:buffer-named
           #:kill-buffer #:current #:current-buffer #:lines #:point #:mark
           #:mode-of #:minors-of #:file-of #:tick #:properties
           #:line #:line-count #:text-of #:insert! #:delete-back! #:newline!
           #:goto! #:move! #:region-of #:mark! #:visit! #:save!
           #:point-line #:point-col #:changed))

(in-package #:pine.edit.buffer)

(defvar *current* nil)

(defclass slot-node (node:node)
  ((object  :initarg :object :reader object)
   (slot-of :initarg :slot   :reader slot-of)))

(defmethod node:contents ((n slot-node))
  (slot-value (object n) (slot-of n)))

(defmethod (setf node:contents) (value (n slot-node))
  (setf (slot-value (object n) (slot-of n)) value)
  (node:invalidate n)
  value)

(defmethod node:leafp ((n slot-node)) t)
(defmethod node:persistp ((n slot-node)) t)

(defclass buffer (node:node)
  ((lines      :initform (c:cell (text:lines-of "")) :reader lines)
   (point-line :initform 0   :accessor point-line)
   (point-col  :initform 0   :accessor point-col)
   (mark       :initform nil :accessor mark)
   (mode-of    :initarg :mode   :accessor mode-of   :initform "text")
   (minors-of  :initarg :minors :accessor minors-of :initform nil)
   (file-of    :initarg :file   :accessor file-of   :initform nil)
   (tick       :initform 0   :accessor tick)
   (properties :initform nil :accessor properties)))

(defmethod print-object ((b buffer) stream)
  (print-unreadable-object (b stream :type t)
    (format stream "~a ~d:~d" (node:name b) (point-line b) (point-col b))))

(defun point (b) (list (point-line b) (point-col b)))

(defmethod node:contents ((b buffer)) (text:text-of (c:held (lines b))))

(defmethod (setf node:contents) (value (b buffer))
  (c:put (lines b) (text:lines-of (princ-to-string value)))
  (changed b)
  value)

(defmethod node:persistp ((b buffer)) nil)

(defmethod mode:in-force ((b buffer))
  (append (sort (remove nil (mapcar #'mode:mode-named (minors-of b)))
                #'> :key #'mode:precedence)
          (mode:chain (mode:mode-named (mode-of b)))))

(defmethod mode:setting ((b buffer) key &optional default)
  (mode:setting (mode:mode-named (mode-of b)) key default))

(defun changed (b)
  (incf (tick b))
  (node:invalidate b)
  b)

(defun %attach-slots (b)
  (dolist (spec '(("point-line" point-line) ("point-col" point-col)
                  ("mode" mode-of) ("file" file-of) ("tick" tick)))
    (destructuring-bind (name slot) spec
      (node:attach (make-instance 'slot-node :name name :object b :slot slot) b)))
  b)

(defun %buffers-node (&optional (w world:*world*))
  (world:ensure w "buf"))

(defun make-buffer (name &rest initargs &key &allow-other-keys)
  (let ((b (apply #'make-instance 'buffer :name name initargs)))
    (node:attach b (%buffers-node))
    (%attach-slots b)
    (world:identify world:*world* b)
    b))

(defun buffers (&optional (w world:*world*))
  (remove-if-not (lambda (n) (typep n 'buffer)) (node:nodes (%buffers-node w))))

(defun buffer-named (name &optional (w world:*world*))
  (let ((n (tree:at (%buffers-node w) (princ-to-string name))))
    (and (typep n 'buffer) n)))

(defun kill-buffer (name)
  (let ((b (buffer-named name)))
    (when b
      (when (eq b *current*) (setf *current* nil))
      (node:detach (%buffers-node) (node:name b)))
    b))

(defun current () *current*)

(defun current-buffer () *current*)

(defun (setf current) (b)
  (setf *current* (if (stringp b) (buffer-named b) b)))

(defun line (b n) (text:line-at (c:held (lines b)) n))

(defun line-count (b) (text:line-count (c:held (lines b))))

(defun text-of (b) (text:text-of (c:held (lines b))))

(defun goto! (b line col)
  (multiple-value-bind (line col) (text:clamp (c:held (lines b)) line col)
    (setf (point-line b) line (point-col b) col)
    (node:invalidate b)
    (point b)))

(defun move! (b unit n)
  (multiple-value-bind (line col)
      (text:move-by unit (c:held (lines b)) (point-line b) (point-col b) n)
    (goto! b line col)))

(defun insert! (b string)
  (multiple-value-bind (fresh line col)
      (text:insert (c:held (lines b)) (point-line b) (point-col b) string)
    (c:put (lines b) fresh)
    (setf (point-line b) line (point-col b) col)
    (changed b)
    (point b)))

(defun newline! (b) (insert! b (string #\Newline)))

(defun delete-back! (b &optional (n 1))
  (multiple-value-bind (line col)
      (text:move-by :char (c:held (lines b)) (point-line b) (point-col b) (- n))
    (multiple-value-bind (fresh at-line at-col taken)
        (text:delete (c:held (lines b)) line col (point-line b) (point-col b))
      (c:put (lines b) fresh)
      (setf (point-line b) at-line (point-col b) at-col)
      (changed b)
      taken)))

(defun mark! (b &optional (line (point-line b)) (col (point-col b)))
  (setf (mark b) (list line col)))

(defun region-of (b)
  (when (mark b)
    (destructuring-bind (line col) (mark b)
      (text:region (c:held (lines b)) line col (point-line b) (point-col b)))))

(defun visit! (b path)
  (setf (file-of b) (namestring path))
  (setf (node:contents b)
        (if (probe-file path) (uiop:read-file-string path) ""))
  (let ((m (mode:mode-for path)))
    (when m (setf (mode-of b) (mode:name m))))
  (goto! b 0 0)
  b)

(defun save! (b &optional (path (file-of b)))
  (when path
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
      (write-string (text-of b) out))
    (setf (file-of b) (namestring path))
    path))
