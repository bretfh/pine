(defpackage #:pine/edit/listing
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:command #:pine/run/command)
                    (#:doc #:pine/text/document) (#:emode #:pine/edit/mode)
                    (#:log #:pine/run/log))
  (:export #:listing #:into #:listings #:rows #:acts #:activate #:row-at #:place
           #:step-row #:said #:commands))
(in-package #:pine/edit/listing)

(defvar *listings* (d:table))

(defclass listing ()
  ((rows :initarg :rows :accessor rows :initform nil)
   (acts :initarg :acts :accessor acts :initform nil))
  (:documentation "Rows that stand for things. A row is a string, or (TEXT . PLACE)
where PLACE is what that row is about, so a key acts on the thing rather than
reading the line back out of the text."))

(defmethod print-object ((l listing) stream)
  (print-unreadable-object (l stream :type t)
    (format stream "~d row~:p~:[~; you can act on~]" (length (rows l)) (acts l))))

(defun listings () (d:all *listings*))

(defun %of (document) (d:at (d:all *listings*) (node:name document)))

(defun said (row) (if (consp row) (car row) (princ-to-string row)))

(defun row-at (document &optional (line (doc:at-line document)))
  "The row point is on: what it says, and what it stands for."
  (let ((l (%of document)))
    (when l (nth line (rows l)))))

(defun place (&optional (document (doc:current)))
  (let ((row (row-at document)))
    (and (consp row) (cdr row))))

(defun %mark (document)
  (setf (doc:setting document :selection) (place document))
  document)

(defun into (name rows &optional acts)
  "Put ROWS in a document of its own and show it. With ACTS, RET on a row hands
ACTS the place that row stands for."
  (let ((document (or (doc:named name)
                      (doc:make-document name :mode (make-instance 'emode:listing))))
        (rows (if (stringp rows)
                  (uiop:split-string rows :separator '(#\Newline))
                  rows)))
    (setf (node:contents document)
          (format nil "~{~a~^~%~}" (mapcar #'said rows)))
    (doc:goto document 0 0)
    (unless (typep (doc:mode-of document) 'emode:listing)
      (setf (doc:mode-of document) (make-instance 'emode:listing)))
    (d:keep! *listings* name (make-instance 'listing :rows rows :acts acts))
    (setf (doc:current) document)
    (%mark document)
    (node:name document)))

(defun activate ()
  (let* ((document (doc:current))
         (l (%of document)))
    (when (and l (acts l))
      (funcall (acts l) (place document)))))

(defun step-row (delta &optional (document (doc:current)))
  "The row after this one, or before it, wrapping round the ends."
  (let* ((l (%of document))
         (n (length (and l (rows l)))))
    (when (plusp n)
      (doc:goto document (mod (+ (doc:at-line document) delta) n) 0)
      (%mark document)
      (log:note "~a" (said (row-at document)))
      (doc:at-line document))))

(defun commands ()
  (command:defcommand "list-activate" ()
      (:describes "open what this row stands for")
    (activate))
  (command:defcommand "list-next" () (:describes "the row after this one")
    (step-row 1))
  (command:defcommand "list-previous" () (:describes "the row before this one")
    (step-row -1))
  (command:defcommand "list-place" ()
      (:describes "what the row point is on stands for")
    (let ((it (place)))
      (log:note "~a" (cond ((null it) "this row stands for nothing")
                           ((node:nodep it) (node:full-name it))
                           (t it)))
      it)))
