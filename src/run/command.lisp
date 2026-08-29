(defpackage #:pine/run/command
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node)
                    (#:commit #:pine/fs/commit))
  (:export
   #:command #:defcommand #:named #:commands #:forget
   #:name #:describes #:asks #:on #:run
   #:word #:claim #:offer #:withdraw #:sorted
   #:unknown-command #:asking #:turned #:*at*))
(in-package #:pine/run/command)

(defvar *commands* (d:table))
(defvar *turned* 0
  "How many times what stands here has changed. Whoever works something out from
the commands keeps this beside it and works it out again when it moves; without
it a keymap has to be built from every command on every keystroke.")
(defvar *defined* (d:table)
  "Every command there is, by the package it was written in. A command is defined
where its code is; whether it stands is whether its system is running.")
(defvar *claimed* nil)
(defvar *at* nil
  "Who there is to ask, when a command needs words nobody gave it: a session on a
stream, an editor with somebody looking at it, or nothing.

An object, not a function to call. What asking means is a method on it, so this
layer knows there is somebody to ask and nothing whatever about how.")

(define-condition unknown-command (error)
  ((name-of :initarg :name :reader name-of))
  (:report (lambda (c s) (format s "no command named ~s" (name-of c)))))

(defclass command ()
  ((name      :initarg :name      :reader name)
   (action    :initarg :action    :reader action)
   (describes :initarg :describes :reader describes :initform "")
   (asks      :initarg :asks      :reader asks      :initform nil)
   (on        :initarg :on        :reader on        :initform nil)
   (from      :initarg :from      :reader from      :initform nil))
  (:documentation "A named thing you can run. Not a lisp function: its arguments
are words, so a name nobody has fbound is still something to do.

FROM is the package it was written in, which is how dropping a system takes its
commands with it without anybody keeping a list of their names.

ON is the mode a chord in it means this, and the chords: (text \"C-f\" \"Right\").
The command carries it and whatever keeps keymaps reads it, so this layer names
nothing above it and a chord goes when the command it names does."))

(defmethod print-object ((c command) stream)
  (print-unreadable-object (c stream :type t)
    (write-string (name c) stream)))

(defun commandp (x) (typep x 'command))

(defun %home () (string-downcase (package-name *package*)))

(defun %underp (prefix said)
  (and prefix said
       (let ((under (concatenate 'string prefix "/")))
         (or (equal said prefix)
             (and (> (length said) (length under))
                  (string= under said :end2 (length under)))))))

(defun defined (prefix)
  "Every command written in PREFIX or under it, running or not."
  (loop :for (home . all) :in (d:pairs (d:all *defined*))
        :when (%underp prefix home) :append (d:vals all)))

(defun claim (&optional (prefix (%home)))
  "Say the commands written here belong to a system: they stand while it runs and
not before. Whatever was already defined stands down until it starts."
  (d:swap *claimed* (lambda (all) (adjoin prefix all :test #'equal)))
  (withdraw prefix))

(defun %claimedp (said)
  (some (lambda (prefix) (%underp prefix said)) *claimed*))

(defun turned ()
  "Which turn what stands is on. Answered before the commands are read and
compared after, so a keymap kept from a turn that has passed is built again."
  *turned*)

(defun %turned () (d:swap *turned* #'1+))

(defun offer (prefix)
  (dolist (c (defined prefix) (progn (%turned) prefix))
    (d:keep! *commands* (name c) c)))

(defun withdraw (prefix)
  (dolist (c (defined prefix) (progn (%turned) prefix))
    (d:drop! *commands* (name c))))

(defun command (name action &key (describes "") asks on (from (%home)))
  "A binding beside the command it names is one thing to read and one thing to
move: the chord is kept on the command, and whatever keeps keymaps asks.

FROM is the package the command was written in. DEFCOMMAND says it, because only
the form knows: a command defined inside a system's START runs with whatever
package the caller of START stood in, and taking that would make the command the
caller's rather than the system's."
  (let* ((home from)
         (c (make-instance 'command :name name :action action :from home
                                    :describes describes :asks asks :on on)))
    (d:update! *defined* home
               (lambda (had) (d:with (or had (d:no-map)) name c)))
    (unless (%claimedp home) (d:keep! *commands* name c))
    (%turned)
    c))

(defun forget (name)
  (d:drop! *commands* name)
  (%turned)
  name)

(defun named (name)
  (etypecase name
    (null nil)
    (command name)
    (string (d:lookup (d:all *commands*) name))
    (symbol (d:lookup (d:all *commands*) (string-downcase (symbol-name name))))))

(defun commands ()
  "Every command standing, in no order. A keymap asks this on every keystroke, so
whoever wants them in an order sorts them there."
  (d:vals (d:all *commands*)))

(defun sorted ()
  (sort (commands) #'string< :key #'name))

(defmacro defcommand (name lambda-list options &body body)
  `(command ,name (lambda ,lambda-list ,@body) ,@options :from ,(%home)))

(defun word (x)
  (cond ((null x) x)
        ((eq x t) x)
        ((keywordp x) x)
        ((symbolp x) (string-downcase (symbol-name x)))
        ((and (consp x) (eq 'quote (first x))) (second x))
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
    (commit:writing
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

(defun %command (name)
  "What the command at this path is for, as it stands now: one redefined at the
repl is the same path saying something else."
  (when (named name)
    (node:answers name :reads (lambda ()
                              (let ((c (named name))) (and c (describes c)))))))

(defun %attach (root)
  (node:attach (node:lists "cmd"
                           :names (lambda () (mapcar #'name (sorted)))
                           :each #'%command
                           :describes "every command there is")
               root))


(pine/fs/tree:builder #'%attach)
