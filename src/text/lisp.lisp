(defpackage #:pine/text/lisp
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:mode #:pine/text/mode)
                    (#:lines #:pine/text/lines) (#:doc #:pine/text/document))
  (:export #:forms #:head #:bodyp))
(in-package #:pine/text/lisp)

(defparameter +bodies+ '("def" "with-" "do-" "when" "unless" "let" "loop" "lambda"
                         "case" "cond" "dolist" "dotimes" "handler" "unwind"
                         "if" "progn" "block" "flet" "labels" "eval-when"
                         "multiple-value-bind" "destructuring-bind")
  "Heads whose rest is a body, so it indents rather than lining up under the first
argument. A name, not a table of every macro: what a config defines is not known
here, and looking it up in the image is the parser's business, not this file's.")

(defun bodyp (name)
  (and name (some (lambda (p)
                    (and (>= (length name) (length p))
                         (string-equal p name :end2 (length p))))
                  +bodies+)))

(defun %word-at (text from)
  (let* ((n (length text))
         (start (or (position-if-not (lambda (c) (member c '(#\Space #\Tab)))
                                     text :start (min from n))
                    n))
         (end (or (position-if (lambda (c) (member c '(#\Space #\Tab #\( #\) #\")))
                               text :start start)
                  n)))
    (when (< start end) (values (subseq text start end) end))))

(defun head (text)
  "The head of a form written at the start of TEXT, and what it names."
  (let ((open (position #\( text)))
    (when open
      (multiple-value-bind (word after) (%word-at text (1+ open))
        (when word
          (values word (%word-at text after)))))))

(defun %depth (text depth in-string)
  "Where the depth and the string state stand after TEXT."
  (let ((i 0) (n (length text)))
    (loop :while (< i n)
          :do (let ((ch (char text i)))
                (cond (in-string
                       (cond ((char= ch #\\) (incf i))
                             ((char= ch #\") (setf in-string nil))))
                      ((char= ch #\;) (return))
                      ((char= ch #\") (setf in-string t))
                      ((and (char= ch #\#) (< (1+ i) n)
                            (char= (char text (1+ i)) #\\))
                       (incf i 2))
                      ((find ch "([") (incf depth))
                      ((find ch ")]") (decf depth))))
              (incf i))
    (values depth in-string)))

(defun forms (document)
  "Every toplevel form in DOCUMENT: its head, what it names, and the lines it
covers."
  (let ((depth 0) (in-string nil) (start nil) (head nil) (name nil) (found nil))
    (dotimes (at (doc:line-count document) (nreverse found))
      (let ((text (doc:line document at)))
        (when (and (null start) (zerop depth) (plusp (length text))
                   (char= #\( (char text 0)))
          (setf start at)
          (multiple-value-bind (h n) (head text)
            (setf head h name n)))
        (multiple-value-setq (depth in-string) (%depth text depth in-string))
        (when (and start (<= depth 0))
          (push (list head name start at) found)
          (setf start nil depth 0))))))

(defmethod mode:structure ((m mode:lisp) document)
  "Toplevel forms, grouped by what they are and named by what they define. So
/text/init.lisp/defcommand/hello is that form, and writing it replaces it."
  (let ((by-head (d:no-map)))
    (dolist (form (forms document))
      (destructuring-bind (head name from to) form
        (let* ((head (or head "form"))
               (name (or name (princ-to-string from)))
               (had (or (d:at by-head head) nil)))
          (setf by-head
                (d:with by-head head
                        (append had
                                (list (list name (cons from 0)
                                            (cons to (length (doc:line document to)))))))))))
    (let (out)
      (d:do-map (head kids by-head (nreverse out))
        (push (list* head
                     (second (first kids))
                     (third (car (cl:last kids)))
                     kids)
              out)))))

(defun %opens (document at)
  "The column of the innermost list still open at the start of line AT, and whether
its head is a body form."
  (let ((depth 0) (in-string nil) (stack nil))
    (dotimes (line at)
      (let ((text (doc:line document line))
            (i 0))
        (let ((n (length text)))
          (loop :while (< i n)
                :do (let ((ch (char text i)))
                      (cond (in-string
                             (cond ((char= ch #\\) (incf i))
                                   ((char= ch #\") (setf in-string nil))))
                            ((char= ch #\;) (return))
                            ((char= ch #\") (setf in-string t))
                            ((and (char= ch #\#) (< (1+ i) n)
                                  (char= (char text (1+ i)) #\\))
                             (incf i 2))
                            ((find ch "([")
                             (incf depth)
                             (push (list i (nth-value 0 (%word-at text (1+ i)))) stack))
                            ((find ch ")]")
                             (decf depth)
                             (pop stack))))
                    (incf i)))))
    (first stack)))

(defmethod mode:indent ((m mode:lisp) document at)
  "Where this line starts: inside a body form, its opener plus the mode's indent;
inside a call, one past the opener. Top level is column zero."
  (let ((open (%opens document at)))
    (if (null open)
        0
        (destructuring-bind (col head) open
          (+ col (if (bodyp head) (or (mode:setting m :indent) 2) 1))))))
