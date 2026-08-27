(defpackage #:pine/text/document
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:commit #:pine/fs/commit)
                    (#:tree #:pine/fs/tree) (#:mount #:pine/fs/mount)
                    (#:lines #:pine/text/lines) (#:mode #:pine/mode))
  (:export #:document #:region #:make-document #:documents #:named #:kill
           #:current #:root #:scratch #:asidep #:killing #:showing #:visiting
           #:lines #:line #:line-count #:text #:point #:at-line #:at-col #:mark
           #:mode-of #:source #:file-of #:origin #:tick #:past #:edit-of #:changed
           #:modified #:setting #:settings #:visited #:leaving
           #:goto #:move #:insert #:delete-back #:newline #:delete-region
           #:mark-at #:put-mark #:drop-mark #:region-of #:indent-line #:indent-of
           #:undo #:redo #:undoable #:redoable
           #:span #:spans #:forget-spans #:overlay #:overlays #:forget-overlays
           #:covers #:regions #:restructure #:fresh-structure))
(in-package #:pine/text/document)

(defvar *current* nil)
(defvar *places* (d:no-map))
(defvar *undo-kept* 200)

(defstruct (was (:constructor was (lines at col))) lines at col)

(defclass document (node:node)
  ((lines    :initform (lines:of "") :accessor lines)
   (at-line  :initform 0   :accessor at-line)
   (at-col   :initform 0   :accessor at-col)
   (mark     :initform nil :accessor mark)
   (mode-of  :initarg :mode :accessor mode-of :initform (make-instance 'mode:text))
   (source   :initarg :source :reader source :initform nil)
   (file-of  :initarg :file   :reader file-of :initform nil)
   (tick     :initform 0   :accessor tick)
   (structured :initform nil :accessor structured)
   (done     :initform nil :accessor done)
   (undone   :initform nil :accessor undone)
   (marks    :initform (d:no-map) :accessor marks)
   (spans    :initform nil :reader spans)
   (overlays :initform nil :accessor overlays)
   (edit-of  :initform nil :accessor edit-of)
   (modified :initform nil :accessor modified)
   (settings :initform (d:no-map) :accessor settings))
  (:documentation "Text, and the structure its mode gives it.

An emacs buffer is characters with a flat property list laid over them. This is a
document: what its mode makes of the text is in the namespace under it, as regions
with identity, so a form or a heading is a place anything can read and write.

SPANS and OVERLAYS are what is laid over the text without being in it: a colour
on part of a line, and something shown after one. A search that has just landed
says a span, a parse says spans, and a terminal says one for every run of colour
its program asked for -- the same few numbers a cell is painted with, whoever
worked them out. They belong to the document, so they go when it does."))

(defclass region (node:node)
  ((livep :initform t)
   (covers :initarg :covers :accessor covers))
  (:documentation "A stretch of a document. Its contents is the text it covers,
its nodes are its sub-regions, and writing it replaces that stretch."))

(defmethod print-object ((doc document) stream)
  (print-unreadable-object (doc stream :type t)
    (format stream "~a ~d:~d" (node:name doc) (at-line doc) (at-col doc))))

(defun root () (tree:ensure nil "text"))

(defun line (doc n) (lines:line (lines doc) n))
(defun line-count (doc) (lines:line-count (lines doc)))
(defun text (doc) (lines:joined (lines doc)))
(defun point (doc) (list (at-line doc) (at-col doc)))

(defmethod node:contents ((doc document)) (text doc))

(defmethod (setf node:contents) (value (doc document))
  (%remember doc)
  (setf (edit-of doc) nil)
  (setf (lines doc) (lines:of (princ-to-string value)))
  (changed doc)
  value)

(defun %bytes (text)
  (length (sb-ext:string-to-octets (or text "") :external-format :utf-8)))

(defun changed (doc)
  (setf (modified doc) t)
  (incf (tick doc))
  (node:moved doc)
  doc)

(defun %edited (doc had at old new bytes)
  "What the last edit did, for whoever reads this document: at AT, OLD lines became
NEW and it grew by BYTES, counted from HAD."
  (setf (edit-of doc) (list (list at old new bytes) had)))

(defgeneric visiting (document where)
  (:documentation "Open DOCUMENT onto WHERE. Answered above, by whatever knows how
to turn a name into somewhere text is kept. Nothing here does, so a document on
its own is a document and not a view of anything.")
  (:method (document where) (declare (ignore document where)) nil))

(defgeneric showing (document)
  (:documentation "Say DOCUMENT is the one being shown now. Whatever is showing
documents hangs a method here; a document does not have to know that anything is.")
  (:method (document) (declare (ignore document)) nil))

(defgeneric killing (document)
  (:documentation "Say DOCUMENT is about to go, while it is still here to be
asked. Whatever keeps something per document -- a parse, a window, a watcher --
lets it go here.")
  (:method (document) (declare (ignore document)) nil))

(defun make-document (name &rest initargs &key (class 'document) &allow-other-keys)
  "A document, of whatever class. A terminal is one, and so is anything else that
is text plus something of its own."
  (let ((doc (apply #'make-instance class :name name
                    (alexandria:remove-from-plist initargs :class))))
    (node:attach doc (root))
    (node:slots doc doc "at-line" 'at-line "at-col" 'at-col "tick" 'tick)
    (node:attach (node:place "source"
                             :reads (lambda () (origin doc))
                             :writes (lambda (value)
                                       (visiting doc (princ-to-string value)))
                             :describes "where this document reads and writes")
                 doc)
    doc))

(defun documents ()
  (remove-if-not (lambda (n) (typep n 'document)) (node:nodes (root))))

(defun named (name)
  (let ((n (tree:at (root) (princ-to-string name))))
    (and (typep n 'document) n)))

(defun scratch ()
  (or (named "scratch")
      (make-document "scratch" :mode (make-instance 'mode:lisp))))

(defun kill (name)
  (let ((doc (named name)))
    (when doc
      (killing doc)
      (node:detach (root) (node:name doc))
      (when (eq doc *current*) (setf *current* (or (first (documents)) (scratch)))))
    doc))

(defun current () *current*)

(defun (setf current) (doc)
  (when *current* (leaving *current*))
  (setf *current* (if (stringp doc) (named doc) doc))
  (when *current* (showing *current*))
  *current*)

(defun asidep (doc)
  (and (typep doc 'document) (setting doc :aside) t))

(defun setting (doc key)
  "What this document reads for KEY: its own, then its mode's."
  (or (d:lookup (settings doc) key) (mode:setting (mode-of doc) key)))

(defun (setf setting) (value doc key)
  (setf (settings doc) (d:with (settings doc) key value))
  value)

(defun goto (doc at col)
  (multiple-value-bind (at col) (lines:clamp (lines doc) at col)
    (setf (at-line doc) at (at-col doc) col)
    (node:moved doc)
    (point doc)))

(defun move (doc unit n)
  (multiple-value-bind (at col)
      (lines:move-by unit (lines doc) (at-line doc) (at-col doc) n)
    (goto doc at col)))

(defun %remember (doc)
  (d:swap (slot-value doc 'done) #'d:capped
           (was (lines doc) (at-line doc) (at-col doc))
           *undo-kept*)
  (setf (undone doc) nil)
  doc)

(defun undoable (doc) (and (done doc) t))
(defun redoable (doc) (and (undone doc) t))

(defun %restore (doc it)
  (when it
    (setf (lines doc) (was-lines it))
    (setf (at-line doc) (was-at it) (at-col doc) (was-col it))
    (changed doc))
  (and it (point doc)))

(defun undo (doc)
  (let ((all (done doc)))
    (when all
      (setf (done doc) (rest all))
      (d:swap (slot-value doc 'undone)
               (lambda (u) (cons (was (lines doc) (at-line doc)
                                      (at-col doc))
                                 u)))
      (%restore doc (first all)))))

(defun redo (doc)
  (let ((all (undone doc)))
    (when all
      (setf (undone doc) (rest all))
      (d:swap (slot-value doc 'done)
               (lambda (u) (cons (was (lines doc) (at-line doc)
                                      (at-col doc))
                                 u)))
      (%restore doc (first all)))))

(defun insert (doc string)
  (%remember doc)
  (let ((had (lines doc))
        (at (at-line doc)))
    (multiple-value-bind (fresh line col)
        (lines:insert had (at-line doc) (at-col doc) string)
      (%edited doc had at 1 (1+ (count #\Newline string)) (%bytes string))
      (setf (lines doc) fresh)
      (setf (at-line doc) line (at-col doc) col)
      (changed doc)
      (point doc))))

(defun newline (doc) (insert doc (string #\Newline)))

(defun delete-back (doc &optional (n 1))
  (%remember doc)
  (multiple-value-bind (at col)
      (lines:move-by :char (lines doc) (at-line doc) (at-col doc) (- n))
    (multiple-value-bind (fresh line col taken)
        (lines:delete (lines doc) at col (at-line doc) (at-col doc))
      (setf (lines doc) fresh)
      (setf (at-line doc) line (at-col doc) col)
      (changed doc)
      taken)))

(defun delete-region (doc from-line from-col to-line to-col)
  (%remember doc)
  (let ((had (lines doc)))
    (multiple-value-bind (fresh line col taken)
        (lines:delete had from-line from-col to-line to-col)
      (%edited doc had from-line (1+ (- to-line from-line)) 1 (- (%bytes taken)))
      (setf (lines doc) fresh)
      (goto doc line col)
      (changed doc)
      taken)))

(defun region-of (doc)
  (when (mark doc)
    (destructuring-bind (at col) (mark doc)
      (lines:region (lines doc) at col (at-line doc) (at-col doc)))))

(defun span (doc line from to face)
  "Colour part of a line, for as long as somebody wants it there. FACE is a face
name or the numbers themselves: (FG BG ATTR), where a colour is (R G B) or
nothing for whatever the theme says."
  (push (list line from to face) (spans doc))
  (node:stir doc)
  doc)

(defun (setf spans) (runs doc)
  "All of them at once. A terminal works out every run of colour on its screen
whenever the program writes, and saying so a line at a time would stir the
document a hundred times for one keystroke."
  (setf (slot-value doc 'spans) runs)
  (node:stir doc)
  runs)

(defun forget-spans (doc)
  (setf (spans doc) nil)
  doc)

(defun overlay (doc line text face)
  "Text shown after a line without being in it. What an evaluation answers is put
here, so the document is what was typed and nothing else."
  (push (list line text face) (overlays doc))
  (node:stir doc)
  doc)

(defun forget-overlays (doc)
  (setf (overlays doc) nil)
  (node:stir doc)
  doc)

(defun mark-at (doc name) (d:lookup (marks doc) name))

(defun put-mark (doc name &optional (at (at-line doc)) (col (at-col doc)))
  (setf (marks doc) (d:with (marks doc) name (list at col)))
  (list at col))

(defun drop-mark (doc name)
  (setf (marks doc) (d:without (marks doc) name))
  name)

(defun indent-of (doc at) (lines:indent (line doc at)))

(defun indent-line (doc at target)
  (let* ((text (line doc at))
         (had (lines:indent text))
         (body (subseq text had))
         (fresh (concatenate 'string (make-string target :initial-element #\Space)
                             body)))
    (unless (equal text fresh)
      (%remember doc)
      (let ((was (lines doc)))
        (%edited doc was at 1 1 (- (%bytes fresh) (%bytes text)))
        (setf (lines doc) (d:with-at was at fresh)))
      (when (= at (at-line doc))
        (setf (at-col doc) (max 0 (+ (at-col doc) (- target had)))))
      (changed doc))
    target))

(defun origin (doc)
  (or (file-of doc) (let ((n (source doc))) (and n (node:full-name n)))))

(defun visited (doc) (d:lookup *places* (origin doc)))

(defun leaving (doc)
  (let ((where (origin doc)))
    (when where (setf *places* (d:with *places* where (point doc)))))
  doc)

(defun (setf source) (n doc)
  (setf (slot-value doc 'source) n
        (slot-value doc 'file-of) (and (typep n 'mount:file)
                                       (namestring (mount:truename-of n))))
  (node:stir doc)
  n)
