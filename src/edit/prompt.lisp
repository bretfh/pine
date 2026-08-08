(defpackage #:pine.edit.prompt
  (:use #:cl)
  (:local-nicknames (#:d #:pine.data) (#:c #:pine.run.cell)
                    (#:node #:pine.fs.node) (#:world #:pine.world.world)
                    (#:buffer #:pine.edit.buffer) (#:log #:pine.run.log)
                    (#:fault #:pine.run.fault))
  (:export #:prompt #:asking #:asking-p #:ask #:answer! #:cancel! #:said
           #:question #:answer-buffer #:then #:candidates #:chosen #:choose! #:was
           #:matching #:complete! #:source #:sources #:install #:*prompt*
           #:showing #:category #:annotation))

(in-package #:pine.edit.prompt)

(defvar *prompt* nil)
(defvar *sources* (c:cell (d:no-map)))
(defvar +buffer+ "*prompt*")

(defclass prompt ()
  ((question   :initarg :question   :reader question)
   (was        :initarg :was        :reader was        :initform nil)
   (then       :initarg :then       :reader then       :initform nil)
   (category   :initarg :category   :reader category   :initform nil)
   (candidates :initarg :candidates :accessor candidates :initform nil)
   (chosen     :initform 0          :accessor chosen)))

(defmethod print-object ((p prompt) stream)
  (print-unreadable-object (p stream :type t)
    (write-string (question p) stream)))

(defun asking () *prompt*)

(defun asking-p () (and *prompt* t))

(defun answer-buffer ()
  (or (buffer:buffer-named +buffer+) (buffer:make-buffer +buffer+)))

(defun said ()
  (let ((b (buffer:buffer-named +buffer+)))
    (if b (buffer:text-of b) "")))

(defun source (category function)
  (c:swap *sources* (lambda (all) (d:with all category function)))
  category)

(defun sources () (d:keys (c:held *sources*)))

(defun %candidates (category)
  (let ((fn (d:at (c:held *sources*) category)))
    (when fn (funcall fn))))

(defun matching (&optional (p *prompt*))
  (let ((text (said))
        (all (candidates p)))
    (if (or (null text) (zerop (length text)))
        all
        (remove-if-not (lambda (each)
                         (search text (princ-to-string each) :test #'char-equal))
                       all))))

(defun ask (question &key then category initial)
  (let ((b (answer-buffer)))
    (setf (node:contents b) (or initial ""))
    (buffer:move! b :buffer 1)
    (setf *prompt* (make-instance 'prompt :question question :then then
                                          :category category
                                          :was (buffer:current)
                                          :candidates (%candidates category))
          (buffer:current) b))
  (when world:*world*
    (setf (node:contents (world:ensure world:*world* "prompt")) question))
  *prompt*)

(defun choose! (delta)
  (let* ((p *prompt*)
         (found (and p (matching p)))
         (n (length found)))
    (when (plusp n)
      (setf (chosen p) (mod (+ (chosen p) delta) n))
      (nth (chosen p) found))))

(defun complete! ()
  (let* ((p *prompt*)
         (found (and p (matching p)))
         (pick (and found (nth (min (chosen p) (1- (length found))) found))))
    (when pick
      (let ((b (answer-buffer)))
        (setf (node:contents b) (princ-to-string pick))
        (buffer:move! b :buffer 1))
      pick)))

(defun %close ()
  (let ((was (and *prompt* (was *prompt*))))
    (when was (setf (buffer:current) was)))
  (setf *prompt* nil)
  (when world:*world*
    (setf (node:contents (world:ensure world:*world* "prompt")) nil))
  (let ((b (buffer:buffer-named +buffer+)))
    (when b (setf (node:contents b) "")))
  nil)

(defun answer! (&optional text)
  (let* ((p *prompt*)
         (answer (or text (said)))
         (fn (and p (then p))))
    (%close)
    (when fn (fault:attempt (lambda () (funcall fn answer)) "answering a prompt"))
    answer))

(defun cancel! ()
  (%close)
  (log:note "cancelled")
  nil)

(defun showing ()
  (if (asking-p)
      (format nil "~a~a" (question *prompt*) (said))
      (or (log:last-said) "")))

(defun annotation (each)
  (typecase each
    (cons (format nil "~a" (rest each)))
    (t "")))
