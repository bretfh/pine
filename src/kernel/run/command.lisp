(defpackage #:pine/commands
  (:use)
  (:documentation "Where a command's name lives: one symbol per command, its value
the command. A namespace, the way functions have one."))

(defpackage #:pine/run/command
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:fs #:pine/fs))
  (:import-from #:pine/fs #:name #:describes)
  (:export
   #:command #:defcommand #:named #:commands #:forget
   #:name #:describes #:asks #:on #:run
   #:word #:claim #:offer #:withdraw #:sorted
   #:unknown-command #:asking #:turned #:*at*))
(in-package #:pine/run/command)

(defvar *claimed* nil)
(defvar *at* nil
  "Who there is to ask, when a command needs words nobody gave it: a session on a
stream, an editor with somebody looking at it, or nothing.

An object, not a function to call. What asking means is a method on it, so this
layer knows there is somebody to ask and nothing whatever about how.")

(define-condition unknown-command (error)
  ((name-of :initarg :name :reader name-of))
  (:report (lambda (c s) (format s "no command named ~s" (name-of c)))))

(defclass command (fs:derived)
  ((action   :initarg :action :reader action)
   (asks     :initarg :asks   :reader asks :initform nil)
   (on       :initarg :on     :reader on   :initform nil)
   (from     :initarg :from   :reader from :initform nil)
   (standing :initform t      :accessor standing))
  (:documentation "A named thing you can run. Not a lisp function: its arguments
are words, so a name nobody has fbound is still something to do. It stands at
/cmd/<name>, and what it holds is what it is for.

FROM is the package it was written in, which is how dropping a system takes its
commands with it; STANDING is whether that system is running.

ON is the mode a chord in it means this, and the chords: (text \"C-f\" \"Right\").
The command carries it and whatever keeps keymaps reads it, so this layer names
nothing above it and a chord goes when the command it names does."))

(defmethod fs:livep ((c command)) t)
(defmethod fs:works ((c command)) (describes c))

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

(defun %all ()
  (let (out)
    (do-symbols (s :pine/commands out)
      (when (boundp s) (push (symbol-value s) out)))))

(defun defined (prefix)
  "Every command written in PREFIX or under it, running or not."
  (remove-if-not (lambda (c) (%underp prefix (from c))) (%all)))

(defun claim (&optional (prefix (%home)))
  "Say the commands written here belong to a system: they stand while it runs and
not before. Whatever was already defined stands down until it starts."
  (d:swap *claimed* (lambda (all) (adjoin prefix all :test #'equal)))
  (withdraw prefix))

(defun %claimedp (said)
  (some (lambda (prefix) (%underp prefix said)) *claimed*))

(defun %dir () (and (fs:root) (fs:at "/cmd")))

(defun turned ()
  "Which version of /cmd stands. Answered before the commands are read and compared
after, so a keymap kept from a turn that has passed is built again."
  (let ((d (%dir))) (if d (fs::version d) 0)))

(defun %turned (&optional name)
  (let ((d (%dir)))
    (when d
      (when name (fs:erase-entry d name))
      (fs:moved d))))

(defun offer (prefix)
  (dolist (c (defined prefix) (progn (%turned) prefix))
    (setf (standing c) t)))

(defun withdraw (prefix)
  (dolist (c (defined prefix) (progn (%turned) prefix))
    (setf (standing c) nil)))

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
    (setf (standing c) (not (%claimedp home)))
    (setf (symbol-value (intern name :pine/commands)) c)
    (%turned name)
    c))

(defun forget (name)
  (let ((s (find-symbol name :pine/commands)))
    (when s (makunbound s) (unintern s :pine/commands)))
  (%turned name)
  name)

(defun named (name)
  (etypecase name
    (null nil)
    (command name)
    (string (let ((s (find-symbol name :pine/commands)))
              (and s (boundp s) (standing (symbol-value s)) (symbol-value s))))
    (symbol (named (string-downcase (symbol-name name))))))

(defun commands ()
  "Every command standing, in no order. A keymap asks this on every keystroke, so
whoever wants them in an order sorts them there."
  (remove-if-not #'standing (%all)))

(defun sorted ()
  (sort (commands) #'string< :key #'name))

(defmacro defcommand (name lambda-list options &body body)
  `(command ,name (lambda ,lambda-list ,@body) ,@options :from ,(%home)))

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
  (fs:attach (make-instance 'fs:dir :name "cmd"
                            :names (lambda () (mapcar #'name (sorted)))
                            :each #'named
                            :describes "every command there is")
             root))


(pine/fs:builder #'%attach)
