(defpackage #:pine/serve/wire
  (:use #:cl)
  (:local-nicknames (#:json #:pine/serve/json) (#:said #:pine/said))
  (:export
   #:asked #:answered #:evented #:serve #:request #:answer #:eventp
   #:*evaluates*))
(in-package #:pine/serve/wire)

(defvar *evaluates* t
  "Whether this way in will evaluate a form.

On, because the way in is a socket under the user's own runtime directory with
the user's own permissions: anything that can reach it can already run a lisp as
this user, so refusing here would protect nothing and would take the one verb a
person at a terminal most wants. A pine that answers anywhere less private than
that turns this off.")

(defparameter +doing+
  '(("read"  . :contents)
    ("write" . :write)
    ("ls"    . :nodes)
    ("verb"  . :verb)
    ("watch" . :watch)
    ("eval"  . :evaluate)
    ("ping"  . :ping))
  "What a line may ask for, and the question it is. One word each, and every one
of them names a place, which is why there is no permission logic here: what may be
done is a question about the path.")

(defun %word (it) (and it (string-downcase (princ-to-string it))))

(defun asked (line)
  "One line of json, as the message the tree takes.

What FROM-JSON answers is already the shape a place takes, so it goes on as it is:
spelling it again would wrap a map in the escape that means `a list which looks
like one', and what landed would be the list and not the map. It reads back the
same either way, which is why it has to be said here.

Answers the id it was asked under as well, because a connection carries answers
and events together and whoever asked has to be able to tell which is which."
  (let* ((it (com.inuoe.jzon:parse line))
         (id (gethash "id" it))
         (doing (%word (gethash "do" it)))
         (path (gethash "path" it))
         (kind (cdr (assoc doing +doing+ :test #'equal))))
    (values
     (cond ((null doing) (list :no "a line says what to do"))
           ((and (equal doing "eval") (not *evaluates*))
            (list :no "this way in does not evaluate"))
           ((null kind) (list :no (format nil "~a is not something to do; there ~
                                              is ~{~a~^, ~}"
                                          doing (mapcar #'car +doing+))))
           ((eq kind :ping) (list :ping))
           ((eq kind :evaluate)
            (let ((form (gethash "form" it)))
              (if (stringp form)
                  (list :evaluate
                        (let ((*read-eval* nil)
                              (*readtable* (named-readtables:find-readtable
                                            'pine/fs/reader:syntax)))
                          (read-from-string form)))
                  (list :no "eval is given a form, as a string"))))
           ((null path) (list :no (format nil "~a names a place" doing)))
           ((eq kind :write)
            (list :write path (json:from-json (gethash "value" it))))
           ((eq kind :verb)
            (list* :verb path (json:as-verb (gethash "verb" it))
                   (map 'list #'json:from-json (or (gethash "with" it) #()))))
           (t (list kind path)))
     id)))

(defun %object (&rest pairs)
  (let ((out (make-hash-table :test 'equal)))
    (loop :for (k v) :on pairs :by #'cddr :do (setf (gethash k out) v))
    out))

(defun %held (said)
  "What an answer carries. One thing for the questions that name a place, and the
whole of what it said for the ones that do not: evaluating answers what it
answered, what it printed and what it broke on, and all three are wanted.

What comes out of the tree is already spelled -- everything RECEIVED answers is --
so it is not spelled again here. Spelling twice wraps a map in the escape that
means `a list which looks like one', and the far side is handed the list."
  (if (= 2 (length said)) (second said) (rest said)))

(defun answered (id said)
  "What came back, as one line. An answer carries the id it answers.

A value with no spelling is refused rather than printed: something that reads as
a string but was an object is a lie the far side cannot catch, and the whole
point of this is that the far side can trust what it reads."
  (com.inuoe.jzon:stringify
   (if (and (consp said) (eq :ok (first said)))
       (handler-case (%object "id" (or id (quote null))
                              "ok" (json:as-json (%held said)))
         (error (c) (%object "id" (or id (quote null))
                             "no" (princ-to-string c))))
       (%object "id" (or id (quote null)) "no"
                (if (consp said) (princ-to-string (second said)) "no answer")))))

(defun request (id message)
  "A message the tree takes, as the line that asks for it. The other side of
ASKED, here so that the two cannot drift apart: what a client sends and what a
daemon reads are one table read twice."
  (let ((word (car (rassoc (first message) +doing+))))
    (unless word (error "~s is not something this speaks." (first message)))
    (com.inuoe.jzon:stringify
     (case (first message)
       (:ping (%object "id" id "do" word))
       (:evaluate (%object "id" id "do" word
                           "form" (let ((*print-readably* nil))
                                    (prin1-to-string (second message)))))
       (:write (%object "id" id "do" word "path" (second message)
                        "value" (json:as-json (said:said (third message)))))
       (:verb (%object "id" id "do" word "path" (second message)
                       "verb" (%word (third message))
                       "with" (coerce (mapcar (lambda (a)
                                                (json:as-json (said:said a)))
                                              (cdddr message))
                                      'vector)))
       (t (%object "id" id "do" word "path" (second message)))))))

(defun eventp (it) (and (hash-table-p it) (nth-value 1 (gethash "event" it))))

(defun answer (line)
  "One line from a daemon, as what it said: the answer, the id it answers, and
whether it is an event nobody asked for now."
  (let ((it (com.inuoe.jzon:parse line)))
    (cond ((eventp it)
           (values (list :moved (gethash "path" it)) nil t))
          ((nth-value 1 (gethash "ok" it))
           (values (list :ok (said:took (json:from-json (gethash "ok" it))))
                   (gethash "id" it) nil))
          (t (values (list :no (princ-to-string (gethash "no" it)))
                     (gethash "id" it) nil)))))

(defun evented (said)
  "A watch firing, as one line. No id: nobody asked for this one now."
  (com.inuoe.jzon:stringify
   (%object "event" (%word (first said)) "path" (or (second said) (quote null)))))

(defun serve (in ask say &key done)
  "Read lines from IN, ask ASK what they mean, and hand the answers to SAY.

ASK is given the message and answers what the tree said, so this file knows how
to frame a question and nothing about what any of them mean. SAY writes one line,
and is given rather than written to because an event is written by whichever
thread moved the place, and two threads sharing a stream need somewhere that
knows it. DONE is called when there are no more lines, which is where whoever
opened this lets go of what the caller left.

One line each way. A shell can write one with echo and read one with read, which
is the whole reason it is lines and not a length and a body."
  (unwind-protect
       (loop :for line := (read-line in nil nil)
             :while line
             :unless (zerop (length (string-trim '(#\Space #\Tab #\Return) line)))
               :do (multiple-value-bind (message id)
                       (handler-case (asked line)
                         (error (c) (values (list :no (princ-to-string c)) nil)))
                     (let ((said (if (eq :no (first message))
                                     message
                                     (handler-case (funcall ask message)
                                       (error (c) (list :no (princ-to-string c)))))))
                       (funcall say (answered id said)))))
    (when done (funcall done))))
