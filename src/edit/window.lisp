(defpackage #:pine.edit.window
  (:use #:cl)
  (:local-nicknames (#:node #:pine.fs.node) (#:tree #:pine.fs.tree)
                    (#:world #:pine.world.world) (#:buffer #:pine.edit.buffer))
  (:export #:window #:make-window #:windows #:window-named #:focused #:focus!
           #:buffer-of #:scroll-of #:width-of #:height-of #:runs-of #:parts
           #:splitp #:split! #:close! #:only! #:seed! #:show!))

(in-package #:pine.edit.window)

(defvar *counter* 0)

(defclass window (node:node)
  ((buffer-of :initarg :buffer :accessor buffer-of :initform nil)
   (scroll-of :initarg :scroll :accessor scroll-of :initform 0)
   (width-of  :initarg :width  :accessor width-of  :initform 80)
   (height-of :initarg :height :accessor height-of :initform 24)
   (runs-of   :initarg :runs   :accessor runs-of   :initform nil)))

(defmethod print-object ((w window) stream)
  (print-unreadable-object (w stream :type t)
    (format stream "~a~@[ ~a~]" (node:name w)
            (and (buffer-of w) (node:name (buffer-of w))))))

(defmethod node:contents ((w window))
  (and (buffer-of w) (node:name (buffer-of w))))

(defmethod node:persistp ((w window)) nil)

(defun %root (&optional (world world:*world*))
  (world:ensure world "win"))

(defun make-window (&key buffer (into (%root)) name)
  (let ((w (make-instance 'window :name (or name (format nil "~d" (incf *counter*)))
                                  :buffer buffer)))
    (node:attach w into)
    w))

(defun parts (of)
  (remove-if-not (lambda (n) (typep n 'window)) (node:nodes of)))

(defun splitp (w) (and (runs-of w) t))

(defun windows (&optional (of (%root)))
  (if (and (typep of 'window) (not (splitp of)))
      (list of)
      (loop :for part :in (parts of) :append (windows part))))

(defun window-named (name &optional (of (%root)))
  (find name (windows of) :key #'node:name :test #'equal))

(defun focused (&optional (of (%root)))
  (let ((named (node:contents (tree:ensure of "focused"))))
    (or (and named (window-named named of))
        (first (windows of)))))

(defun focus! (w &optional (of (%root)))
  (setf (node:contents (tree:ensure of "focused")) (node:name w))
  w)

(defun show! (w b)
  (setf (buffer-of w) (if (stringp b) (buffer:buffer-named b) b))
  (setf (buffer:current) (buffer-of w))
  (node:invalidate w)
  w)

(defun split! (w side)
  (let* ((runs (if (member side '(:beside :right :left)) :row :column))
         (a (make-window :buffer (buffer-of w) :into w))
         (b (make-window :buffer (buffer-of w) :into w)))
    (setf (runs-of w) runs (buffer-of w) nil)
    (focus! (if (member side '(:above :left)) a b))
    (values a b)))

(defun close! (w &optional (of (%root)))
  (let ((up (node:parent w)))
    (when (and up (typep up 'window))
      (node:detach up (node:name w))
      (let ((left (parts up)))
        (when (= 1 (length left))
          (let ((only (first left)))
            (setf (buffer-of up) (buffer-of only)
                  (runs-of up) (runs-of only))
            (node:detach up (node:name only)))))
      (focus! (or (first (windows of)) up) of))
    w))

(defun only! (w &optional (of (%root)))
  (dolist (part (parts of)) (node:detach of (node:name part)))
  (let ((fresh (make-window :buffer (buffer-of w) :into of)))
    (focus! fresh of)
    fresh))

(defun seed! (b &optional (of (%root)))
  (or (first (windows of))
      (let ((w (make-window :buffer b :into of)))
        (focus! w of)
        w)))
