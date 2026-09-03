(defpackage #:pine/run/command
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:fs #:pine/fs))
  (:import-from #:pine/fs #:name #:describes)
  (:export
   #:command #:defcommand #:named #:commands #:forget
   #:name #:describes #:asks #:on #:run
   #:word #:sorted
   #:unknown-command #:asking #:*at*))
(in-package #:pine/run/command)

(defvar *at* nil
  "Who there is to ask, when a command needs words nobody gave it: a session on a
stream, an editor with somebody looking at it, or nothing.

An object, not a function to call. What asking means is a method on it, so this
layer knows there is somebody to ask and nothing whatever about how.")

(define-condition unknown-command (error)
  ((name-of :initarg :name :reader name-of))
  (:report (lambda (c s) (format s "no command named ~s" (name-of c)))))

(defclass command (fs:value)
  ((action :initarg :action :reader action))
  (:documentation "A named thing you can run, at /cmd/<name>. Not a lisp function:
its arguments are words, so a name nobody has fbound is still something to do.
What it holds is what it is: (:describes … :asks … :on …).

ON is the mode a chord in it means this, and the chords: (text \"C-f\" \"Right\").
Whatever keeps keymaps reads it, so a chord goes when the command it names does."))

(defmethod fs:savedp ((c command)) nil)

(defmethod describes ((c command)) (getf (fs:contents c) :describes))

(defun asks (c) (getf (fs:contents c) :asks))

(defun on (c) (getf (fs:contents c) :on))

(defmethod print-object ((c command) stream)
  (print-unreadable-object (c stream :type t)
    (write-string (name c) stream)))

(defun commandp (x) (typep x 'command))

(defun %cmd () (fs:ensure (fs:root) "cmd"))

(defun command (name action &key (describes "") asks on)
  (fs:declared (lambda ()
                 (make-instance 'command :name name :action action
                                :held (list :describes describes :asks asks :on on)))
               "cmd"))

(defun forget (name)
  (let ((c (named name)))
    (when c (fs:undeclared c))
    name))

(defun named (name)
  (etypecase name
    (null nil)
    (command name)
    (string (let ((it (fs:entry (%cmd) name))) (and (commandp it) it)))
    (symbol (named (string-downcase (symbol-name name))))))

(defun commands ()
  "Every command, in no order. A keymap asks this on every keystroke, so whoever
wants them in an order sorts them there."
  (remove-if-not #'commandp (fs:entries (%cmd))))

(defun sorted ()
  (sort (commands) #'string< :key #'name))

(defmacro defcommand (name lambda-list options &body body)
  `(command ,name (lambda ,lambda-list ,@body) ,@options))

(defun word (x)
  "What one argument at the prompt means.

A bare name is the word it spells, so (use text) says text and does not look for
a variable called text. That is what makes a command line a command line.

Anything written as a form is lisp and is worked out. /a/b is a form -- the reader
makes one -- and handing the form over rather than the place it spells is what
made (cat /dev/audio/volume) break at the prompt while (read /dev/audio/volume)
answered, which is the one thing the four verbs are supposed not to do."
  (cond ((null x) x)
        ((eq x t) x)
        ((keywordp x) x)
        ((symbolp x) (string-downcase (symbol-name x)))
        ((and (consp x) (eq 'quote (first x))) (second x))
        ((consp x) (eval x))
        (t x)))

(defgeneric run (command &optional arguments)
  (:documentation "Run COMMAND, and say once what it moved.

One command is one piece of news, however many places it writes. Opening a file
writes what the document holds, where point is and every region its mode makes
of it; told one at a time, a store writes for each and a watcher is woken for
each, and what they see in between is a document half opened.

Re-entrant, so a command that runs another is still one piece of news: the
inner one joins the batch the outer one opened.")
  (:method ((name string) &optional arguments)
    (let ((c (named name)))
      (unless c (error 'unknown-command :name name))
      (run c arguments)))
  (:method ((c command) &optional arguments)
    (fs:writing
      (if (and (null arguments) (asks c))
          (let ((asked (asking *at* c)))
            (if (eq asked :asking) :asking (apply (action c) asked)))
          (apply (action c) arguments)))))

(defgeneric asking (where command)
  (:documentation "The words COMMAND needs, asked of whoever there is to ask.

Answered above: a session reads them off its stream, an editor puts the question
on screen and answers :ASKING. Nothing to ask means nothing to say, and the
command runs with what it was given.")
  (:method (where (c command))
    (declare (ignore where c))
    nil))

(defun %attach (root)
  (setf (fs:describes (fs:ensure root "cmd")) "every command there is"))

(pine/fs:builder #'%attach)
