(in-package #:pine/edit)

(defvar *listings* (d:table))

(defclass listed ()
  ((shown-rows :initarg :shown-rows :accessor shown-rows :initform nil)
   (on-enter :initarg :on-enter :accessor on-enter :initform nil))
  (:documentation "Rows that stand for things. A row is a string, or (TEXT . PLACE)
where PLACE is what that row is about, so a key acts on the thing rather than
reading the line back out of the text."))

(defmethod print-object ((l listed) stream)
  (print-unreadable-object (l stream :type t)
    (format stream "~d row~:p~:[~; you can act on~]" (length (shown-rows l)) (on-enter l))))

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
  (setf (mode:setting document :selection) (place document))
  document)

(defun show-listing (name shown-rows &optional on-enter)
  "Put ROWS in a document of its own and show it. With ON-ENTER, RET on a row hands
it the place that row stands for."
  (let ((document (or (tree:at "/text" name)
                      (text:make-document name :mode (make-instance 'listing))))
        (shown-rows (if (stringp shown-rows)
                  (uiop:split-string shown-rows :separator '(#\Newline))
                  shown-rows)))
    (setf (node:contents document)
          (format nil "~{~a~^~%~}" (mapcar #'said shown-rows)))
    (text:goto document 0 0)
    (unless (typep (text:mode-of document) 'listing)
      (setf (text:mode-of document) (make-instance 'listing)))
    (d:keep! *listings* name (make-instance 'listed :shown-rows shown-rows :on-enter on-enter))
    (setf (text:current) document)
    (%mark document)
    (node:name document)))

(defmethod text:killing :before ((document text:document))
  "A killed document takes its rows with it. Kept, they are rows standing for things
in a document nothing can reach, held for as long as the image runs.

:BEFORE and not :AFTER because the parse already takes itself off in an :AFTER on
this same class, and a second method of the same qualifier and specializers is not
another method -- it is the same one, written again."
  (d:drop! *listings* (node:name document)))

(defun activate ()
  (let* ((document (text:current))
         (l (%listing document)))
    (when (and l (on-enter l))
      (funcall (on-enter l) (place document)))))

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

