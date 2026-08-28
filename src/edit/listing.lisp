(defpackage #:pine/edit/listing
  (:use #:cl)
  (:local-nicknames (#:text #:pine/text)
                    (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:command #:pine/run/command) (#:emode #:pine/edit/mode)
                    (#:log #:pine/fs/log))
  (:export
   #:listing #:into #:acts #:place))
(in-package #:pine/edit/listing)

(defvar *listings* (d:table))

(defclass listing ()
  ((shown-rows :initarg :shown-rows :accessor shown-rows :initform nil)
   (acts :initarg :acts :accessor acts :initform nil))
  (:documentation "Rows that stand for things. A row is a string, or (TEXT . PLACE)
where PLACE is what that row is about, so a key acts on the thing rather than
reading the line back out of the text."))

(defmethod print-object ((l listing) stream)
  (print-unreadable-object (l stream :type t)
    (format stream "~d row~:p~:[~; you can act on~]" (length (shown-rows l)) (acts l))))

(defun listings () (d:all *listings*))

(defun %listing (document)
  (d:lookup (d:all *listings*) (node:name document)))

(defun said (row) (if (consp row) (car row) (princ-to-string row)))

(defun row-at (document &optional (line (text:at-line document)))
  "The row point is on: what it says, and what it stands for."
  (let ((l (%listing document)))
    (when l (nth line (shown-rows l)))))

(defun place (&optional (document (text:current)))
  (let ((row (row-at document)))
    (and (consp row) (cdr row))))

(defun %mark (document)
  (setf (text:setting document :selection) (place document))
  document)

(defun into (name shown-rows &optional acts)
  "Put ROWS in a document of its own and show it. With ACTS, RET on a row hands
ACTS the place that row stands for."
  (let ((document (or (text:named name)
                      (text:make-document name :mode (make-instance 'emode:listing))))
        (shown-rows (if (stringp shown-rows)
                  (uiop:split-string shown-rows :separator '(#\Newline))
                  shown-rows)))
    (setf (node:contents document)
          (format nil "~{~a~^~%~}" (mapcar #'said shown-rows)))
    (text:goto document 0 0)
    (unless (typep (text:mode-of document) 'emode:listing)
      (setf (text:mode-of document) (make-instance 'emode:listing)))
    (d:keep! *listings* name (make-instance 'listing :shown-rows shown-rows :acts acts))
    (setf (text:current) document)
    (%mark document)
    (node:name document)))

(defun activate ()
  (let* ((document (text:current))
         (l (%listing document)))
    (when (and l (acts l))
      (funcall (acts l) (place document)))))

(defun step-row (delta &optional (document (text:current)))
  "The row after this one, or before it, wrapping round the ends."
  (let* ((l (%listing document))
         (n (length (and l (shown-rows l)))))
    (when (plusp n)
      (text:goto document (mod (+ (text:at-line document) delta) n) 0)
      (%mark document)
      (log:note "~a" (said (row-at document)))
      (text:at-line document))))

(command:defcommand "list-activate" ()
    (:describes "open what this row stands for" :on '(listing "RET"))
  (activate))

(command:defcommand "list-next" ()
    (:describes "the row after this one" :on '(listing "n" "C-n" "Down"))
  (step-row 1))

(command:defcommand "list-previous" ()
    (:describes "the row before this one" :on '(listing "p" "C-p" "Up"))
  (step-row -1))

(command:defcommand "list-place" ()
    (:describes "what the row point is on stands for" :on '(listing "."))
  (let ((it (place)))
    (log:note "~a" (cond ((null it) "this row stands for nothing")
                         ((node:nodep it) (node:full-name it))
                         (t it)))
    it))
