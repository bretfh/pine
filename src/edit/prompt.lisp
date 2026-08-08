(defpackage #:pine.edit.prompt
  (:use #:cl)
  (:local-nicknames (#:d #:pine.data) (#:c #:pine.run.cell)
                    (#:node #:pine.fs.node) (#:world #:pine.world.world)
                    (#:buffer #:pine.edit.buffer) (#:log #:pine.run.log)
                    (#:fault #:pine.run.fault))
  (:export #:prompt #:asking #:asking-p #:ask #:answer! #:cancel! #:said
           #:question #:answer-buffer #:then #:candidates #:chosen #:choose! #:was
           #:matching #:complete! #:source #:sources #:install #:*prompt*
           #:showing #:category #:annotation #:name-of #:shows #:given
           #:must-match))

(in-package #:pine.edit.prompt)

(defvar *prompt* nil)
(defvar *sources* (c:cell (d:no-map)))
(defvar +buffer+ "*prompt*")

(defclass prompt ()
  ((question   :initarg :question   :reader question)
   (was        :initarg :was        :reader was        :initform nil)
   (then       :initarg :then       :reader then       :initform nil)
   (category   :initarg :category   :reader category   :initform nil)
   (given      :initarg :candidates :reader given      :initform nil)
   (must-match :initarg :must-match :reader must-match :initform nil)
   (seen       :initform nil        :accessor seen)
   (chose      :initform 0          :accessor chose)))

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

(defun candidates (&optional (p *prompt*))
  (when p
    (or (given p)
        (let ((fn (d:at (c:held *sources*) (category p))))
          (when fn (fault:attempt (lambda () (funcall fn (said))) "the candidates"))))))

(defun name-of (each) (princ-to-string (if (consp each) (first each) each)))

(defun %sync (p)
  (let ((text (said)))
    (unless (equal text (seen p))
      (setf (seen p) text (chose p) 0)))
  p)

(defun chosen (&optional (p *prompt*))
  (when p (chose (%sync p))))

(defun (setf chosen) (value p)
  (setf (chose (%sync p)) value))

(defun matching (&optional (p *prompt*))
  (when p
    (%sync p)
    (let ((text (said))
          (all (candidates p)))
      (if (or (null text) (zerop (length text)))
          all
          (let ((opening nil) (within nil))
            (dolist (each all)
              (let ((name (name-of each)))
                (cond ((and (<= (length text) (length name))
                            (string-equal text name :end2 (length text)))
                       (push each opening))
                      ((search text name :test #'char-equal) (push each within)))))
            (nconc (nreverse opening) (nreverse within)))))))

(defun ask (question &key then category initial must-match candidates)
  (let ((b (answer-buffer)))
    (setf (node:contents b) (or initial ""))
    (buffer:move! b :buffer 1)
    (setf *prompt* (make-instance 'prompt :question question :then then
                                          :category category
                                          :must-match must-match
                                          :candidates candidates
                                          :was (buffer:current))
          (buffer:current) b)
    (setf (seen *prompt*) (said)))
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
        (setf (node:contents b) (name-of pick))
        (buffer:move! b :buffer 1)
        (setf (seen p) (said)))
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

(defun %answered (p)
  (let ((said (said)))
    (if (and p (must-match p))
        (let* ((found (matching p))
               (pick (nth (min (chosen p) (max 0 (1- (length found)))) found)))
          (if pick (name-of pick) said))
        said)))

(defun answer! (&optional text)
  (let* ((p *prompt*)
         (answer (or text (%answered p)))
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
  (if (consp each) (princ-to-string (rest each)) ""))

(defun shows (each width)
  (let* ((name (name-of each))
         (note (annotation each))
         (gap (- width (length name) (length note))))
    (if (and (plusp (length note)) (> gap 1))
        (concatenate 'string name (make-string gap :initial-element #\space) note)
        name)))
