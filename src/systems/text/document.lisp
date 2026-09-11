(in-package #:pine/text)

(defvar *current* nil)
(defvar *places* (d:no-map))
(defvar *undo-kept* 200)

(defstruct (was (:constructor was (lines at col))) lines at col)

(defclass buffer (fs:mount)
  ((lines    :initform (of "") :accessor lines)
   (text-node :initform nil :accessor text-node)
   (listener :initform nil :accessor listener)
   (parser   :initform nil :accessor parser)
   (at-line  :initform 0   :accessor at-line)
   (at-col   :initform 0   :accessor at-col)
   (mark     :initform nil :accessor mark)
   (mode-of  :initarg :mode :accessor mode-of :initform (make-instance 'mode:text))
   (source   :initarg :source :reader source :initform nil)
   (file-of  :initarg :file   :reader file-of :initform nil)
   (tick     :initform 0   :accessor tick)
   (structured :initform nil :accessor structured)
   (declared :initform nil :accessor declared)
   (done     :initform nil :accessor done)
   (undone   :initform nil :accessor undone)
   (marks    :initform (d:no-map) :accessor marks)
   (spans    :initform nil :reader spans)
   (overlays :initform nil :accessor overlays)
   (edit-of  :initform nil :accessor edit-of)
   (modified :initform nil :accessor modified)
   (settings :initform (d:no-map) :accessor settings)))

(defclass region (fs:mount)
  ((covers :initarg :covers :accessor covers)))

(defmethod print-object ((doc buffer) stream)
  (print-unreadable-object (doc stream :type t)
    (format stream "~a ~d:~d" (fs:name doc) (at-line doc) (at-col doc))))

(fs:mount (lambda () (make-instance 'fs:mount :describes "buffers, and terminals"))
          "/text")

(defun root () (fs:at "/text"))

(defmethod line ((doc buffer) n) (line (lines doc) n))
(defmethod line-count ((doc buffer)) (line-count (lines doc)))
(defun text (doc)
  (when (text-node doc) (fs:depend-on (text-node doc)))
  (joined (lines doc)))
(defun point (doc) (list (at-line doc) (at-col doc)))

(defgeneric (setf text) (value doc)
  (:method (value (doc buffer))
    (%remember doc)
    (setf (edit-of doc) nil)
    (setf (lines doc) (of (princ-to-string value)))
    (on-change doc)
    value))

(defun %bytes (text)
  (length (sb-ext:string-to-octets (or text "") :external-format :utf-8)))

(defun %kept-declaration (doc at old new)
  (let ((had (declared doc)))
    (when had
      (setf (car had) (tick doc))
      (setf (cdr had)
            (loop :for (key value line word) :in (rest had)
                  :for kept
                    := (cond ((and (<= at line) (< line (+ at old))) nil)
                             ((>= line (+ at old))
                              (list key value (+ line (- new old)) word))
                             ((loop :for n :from at
                                      :below (min (+ at new) (line-count doc))
                                    :thereis (search word (line doc n)
                                                     :test #'char-equal))
                              nil)
                             (t (list key value line word)))
                  :when kept :collect kept)))))

(defun on-change (doc &optional at old new)
  (setf (modified doc) t)
  (incf (tick doc))
  (if (and at old new)
      (%kept-declaration doc at old new)
      (setf (declared doc) nil))
  (fs:commit doc)
  (when (text-node doc) (fs:touch (text-node doc)))
  doc)

(defun %edited (doc had at old new bytes)
  (setf (edit-of doc) (list (list at old new bytes) had)))

(defgeneric visiting (buffer where)
  (:method (buffer where) (declare (ignore buffer where)) nil))

(defgeneric showing (buffer)
  (:method (buffer) (declare (ignore buffer)) nil))

(defgeneric killing (buffer)
  (:method (buffer) (declare (ignore buffer)) nil))

(defun make-buffer (name &rest initargs &key (class 'buffer) &allow-other-keys)
  (let ((doc (apply #'make-instance class :name name
                    (alexandria:remove-from-plist initargs :class))))
    (fs:mount doc (root))
    (setf (text-node doc) (fs:child doc "text"))
    doc))

(defmethod fs:names ((doc buffer))
  '((:text    . "what it says")
    (:at-line . "the line point is on")
    (:at-col  . "the column point is at")
    (:tick    . "how many times it has been edited")
    (:source  . "where it reads and writes")
    (:mode    . "what kind of text it is; writing another kind's name makes it that")))

(defmethod fs:read ((doc buffer) (name (eql :text)))
  (text doc))

(defmethod fs:write ((doc buffer) (name (eql :text)) value)
  (setf (text doc) value))

(defmethod fs:read ((doc buffer) (name (eql :at-line)))
  (at-line doc))

(defmethod fs:write ((doc buffer) (name (eql :at-line)) value)
  (setf (at-line doc) value))

(defmethod fs:read ((doc buffer) (name (eql :at-col)))
  (at-col doc))

(defmethod fs:write ((doc buffer) (name (eql :at-col)) value)
  (setf (at-col doc) value))

(defmethod fs:read ((doc buffer) (name (eql :tick)))
  (tick doc))

(defmethod fs:read ((doc buffer) (name (eql :source)))
  (origin doc))

(defmethod fs:write ((doc buffer) (name (eql :source)) value)
  (visiting doc (princ-to-string value)))

(defmethod fs:read ((doc buffer) (name (eql :mode)))
  (string-downcase (symbol-name (class-name (class-of (mode-of doc))))))

(defmethod fs:write ((doc buffer) (name (eql :mode)) value)
  (let ((class (find (string-downcase (princ-to-string value)) (mode:modes)
                     :key (lambda (c) (string-downcase (symbol-name (class-name c))))
                     :test #'equal)))
    (unless class (error "no mode called ~a" value))
    (setf (mode-of doc) (make-instance class))
    (fs:touch doc)
    value))

(defun buffers ()
  (remove-if-not (lambda (n) (typep n 'buffer)) (fs:children (root))))

(defun scratch ()
  (or (fs:at (root) "scratch")
      (make-buffer "scratch" :mode (make-instance 'mode:lisp))))

(defun kill (name)
  (let ((doc (fs:at (root) name)))
    (when doc
      (killing doc)
      (fs:unlink (root) (fs:name doc))
      (when (eq doc *current*) (setf *current* (or (first (buffers)) (scratch)))))
    doc))

(defun current () *current*)

(defun (setf current) (doc)
  (when *current* (leaving *current*))
  (setf *current* (if (stringp doc) (fs:at (root) doc) doc))
  (when *current* (showing *current*))
  *current*)

(defun asidep (doc)
  (and (typep doc 'buffer) (mode:says doc :aside nil) t))

(defmethod mode:setting ((doc buffer) key)
  (multiple-value-bind (said saidp) (d:lookup (settings doc) key)
    (if saidp said (mode:setting (mode-of doc) key))))

(defmethod (setf mode:setting) (value (doc buffer) key)
  (setf (settings doc) (d:with (settings doc) key value))
  value)

(defun goto (doc at col)
  (multiple-value-bind (at col) (clamp (lines doc) at col)
    (setf (at-line doc) at (at-col doc) col)
    (fs:commit doc)
    (point doc)))

(defun move (doc unit n)
  (multiple-value-bind (at col)
      (move-by unit (lines doc) (at-line doc) (at-col doc) n)
    (goto doc at col)))

(defun %remember (doc)
  (sb-ext:atomic-update (slot-value doc 'done) (lambda (old) (d:capped old (was (lines doc) (at-line doc) (at-col doc)) *undo-kept*)))
  (setf (undone doc) nil)
  doc)

(defun undoable (doc) (and (done doc) t))
(defun redoable (doc) (and (undone doc) t))

(defun %restore (doc it)
  (when it
    (setf (lines doc) (was-lines it))
    (setf (at-line doc) (was-at it) (at-col doc) (was-col it))
    (on-change doc))
  (and it (point doc)))

(defun undo (doc)
  (let ((all (done doc)))
    (when all
      (setf (done doc) (rest all))
      (sb-ext:atomic-update (slot-value doc 'undone)
               (lambda (u) (cons (was (lines doc) (at-line doc)
                                      (at-col doc))
                                 u)))
      (%restore doc (first all)))))

(defun redo (doc)
  (let ((all (undone doc)))
    (when all
      (setf (undone doc) (rest all))
      (sb-ext:atomic-update (slot-value doc 'done)
               (lambda (u) (cons (was (lines doc) (at-line doc)
                                      (at-col doc))
                                 u)))
      (%restore doc (first all)))))

(defun insert (doc string)
  (%remember doc)
  (let ((had (lines doc))
        (at (at-line doc)))
    (multiple-value-bind (fresh line col)
        (inserted had (at-line doc) (at-col doc) string)
      (%edited doc had at 1 (1+ (count #\Newline string)) (%bytes string))
      (setf (lines doc) fresh)
      (setf (at-line doc) line (at-col doc) col)
      (on-change doc at 1 (1+ (count #\Newline string)))
      (point doc))))

(defun newline (doc) (insert doc (string #\Newline)))

(defun delete-back (doc &optional (n 1))
  (%remember doc)
  (let ((to (at-line doc)))
    (multiple-value-bind (at col)
        (move-by :char (lines doc) (at-line doc) (at-col doc) (- n))
      (multiple-value-bind (fresh line col taken)
          (cut (lines doc) at col (at-line doc) (at-col doc))
        (setf (lines doc) fresh)
        (setf (at-line doc) line (at-col doc) col)
        (on-change doc at (1+ (- to at)) 1)
        taken))))

(defun delete-region (doc from-line from-col to-line to-col)
  (%remember doc)
  (let ((had (lines doc)))
    (multiple-value-bind (fresh line col taken)
        (cut had from-line from-col to-line to-col)
      (%edited doc had from-line (1+ (- to-line from-line)) 1 (- (%bytes taken)))
      (setf (lines doc) fresh)
      (goto doc line col)
      (on-change doc from-line (1+ (- to-line from-line)) 1)
      taken)))

(defun region-of (doc)
  (when (mark doc)
    (destructuring-bind (at col) (mark doc)
      (region (lines doc) at col (at-line doc) (at-col doc)))))

(defun span (doc line from to face)
  (push (list line from to face) (spans doc))
  (fs:touch doc)
  doc)

(defun (setf spans) (runs doc)
  (setf (slot-value doc 'spans) runs)
  (fs:touch doc)
  runs)

(defun forget-spans (doc)
  (setf (spans doc) nil)
  doc)

(defun overlay (doc line text face)
  (push (list line text face) (overlays doc))
  (fs:touch doc)
  doc)

(defun forget-overlays (doc)
  (setf (overlays doc) nil)
  (fs:touch doc)
  doc)

(defun mark-at (doc name) (d:lookup (marks doc) name))

(defun put-mark (doc name &optional (at (at-line doc)) (col (at-col doc)))
  (setf (marks doc) (d:with (marks doc) name (list at col)))
  (list at col))

(defun drop-mark (doc name)
  (setf (marks doc) (d:without (marks doc) name))
  name)

(defun indent-of (doc at) (leading (line doc at)))

(defun indent-line (doc at target)
  (let* ((text (line doc at))
         (had (leading text))
         (body (subseq text had))
         (fresh (concatenate 'string (make-string target :initial-element #\Space)
                             body)))
    (unless (equal text fresh)
      (%remember doc)
      (let ((was (lines doc)))
        (%edited doc was at 1 1 (- (%bytes fresh) (%bytes text)))
        (setf (lines doc) (d:with was at fresh)))
      (when (= at (at-line doc))
        (setf (at-col doc) (max 0 (+ (at-col doc) (- target had)))))
      (on-change doc at 1 1))
    target))

(defun origin (doc)
  (or (file-of doc) (let ((n (source doc))) (and n (fs:full-name n)))))

(defun visited (doc) (d:lookup *places* (origin doc)))

(defun leaving (doc)
  (let ((where (origin doc)))
    (when where (setf *places* (d:with *places* where (point doc)))))
  doc)

(defun (setf source) (n doc)
  (setf (slot-value doc 'source) n
        (slot-value doc 'file-of) (and (typep n 'fs:file)
                                       (namestring (fs:truename-of n))))
  (fs:touch doc)
  n)
