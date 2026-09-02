(in-package #:pine/text)

(defun %after (line from)
  "The word written at or after FROM in LINE, or nothing where the line ends first."
  (let* ((start (position-if-not (lambda (ch) (member ch '(#\Space #\Tab)))
                                 line :start from))
         (end (and start (or (position-if
                              (lambda (ch) (member ch '(#\Space #\Tab #\( #\))))
                              line :start start)
                             (length line)))))
    (when (and start end (< start end)) (subseq line start end))))

(defun %named-after (document word)
  "The name written after the last WORD in this document. What is in force at the
end of a file is what the last one says, so this is walked from the end.

A line at a time, and not over the text as one string. Making that string is a
copy of the whole document -- TEXT joins every line into one -- and this is asked
while a frame is being drawn. Walked as lines it allocates nothing but the answer.

Still the whole document in the worst case, because the last one wins and the last
one may be the first: nothing can be skipped without changing what this means."
  (loop :for n :from (1- (line-count document)) :downto 0
        :for line := (line document n)
        :for at := (search word line :from-end t :test #'char-equal)
        :when at
          :do (return (values (or (%after line (+ at (length word)))
                                  (and (< (1+ n) (line-count document))
                                       (%after (line document (1+ n)) 0)))
                              n))))

(defun %package-of (document)
  (multiple-value-bind (said at) (%named-after document "in-package")
    (values (or (and said (find-package (string-upcase (string-left-trim "#:" said))))
                (find-package :pine/user)
                (find-package :cl-user))
            at)))

(defun %readtable-of (document)
  (multiple-value-bind (said at) (%named-after document "in-readtable")
    (values (when said
              (or (fault:or-nothing "what the file says may name no readtable"
                    (named-readtables:find-readtable
                     (let ((*package* (find-package :cl-user)) (*read-eval* nil))
                       (read-from-string said))))
                  (fault:or-nothing "nor as a keyword"
                    (named-readtables:find-readtable
                     (intern (string-upcase (string-left-trim "#:" said))
                             :keyword)))))
            at)))

(defun %says (document key word worker)
  "What this document says it is written in, for one question, worked out once and
kept until the text moves.

Kept against TICK, the way STRUCTURED is: what a file says it is written in cannot
become something else without the text becoming something else, and the text says
when it did.

The line the answer was found on and the word it was found by are kept beside it,
so an edit somewhere else can be shown not to have changed it without walking the
document again. That is %KEPT-DECLARATION's, in the file beside this one.

One question at a time, and not both together. The two are read out of the text by
two separate walks of it, and a frame asks only which readtable -- so working out
the package as well, because it was convenient to keep them side by side, is a
second walk of the document that nobody asked for. Measured, that alone took a
frame from thirty-one milliseconds to eighty-one."
  (let ((had (declared document))
        (now (tick document)))
    (unless (and had (eql (first had) now))
      (setf had (list now))
      (setf (declared document) had))
    (let ((cell (assoc key (rest had))))
      (cond (cell (second cell))
            (t (multiple-value-bind (v at) (funcall worker document)
                 (setf (cdr had)
                       (cons (list key v (or at -1) word) (cdr had)))
                 v))))))

(defun package-of (document)
  "The package this document's forms are read in: the one its own (in-package)
names. What somebody's config defines lives there, not in CL, so this is what tells
the walk and the evaluator what a name in it means."
  (%says document :package "in-package" #'%package-of))

(defgeneric readtable-of (of)
  (:documentation "The readtable OF is written in: a document says so with its own
(in-readtable), a language says so in its declaration.")
  (:method (of) (declare (ignore of)) nil))

(defmethod readtable-of ((document document))
  "The readtable this document is written in, from its own (in-readtable), or
nothing where it is written in the standard one."
  (%says document :readtable "in-readtable" #'%readtable-of))

(defun reading (document)
  "What reading a form out of DOCUMENT means: its package and its readtable."
  (values (package-of document) (or (readtable-of document) *readtable*)))
