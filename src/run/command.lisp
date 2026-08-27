(defpackage #:pine/run/command
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node))
  (:export #:command #:commandp #:defcommand #:named #:commands #:forget
           #:name #:action #:describes #:asks #:from #:on #:arguments #:run #:word
           #:claim #:offer #:withdraw #:defined #:sorted
           #:unknown-command #:attach #:*asking*))
(in-package #:pine/run/command)

(defvar *commands* (d:table))
(defvar *defined* (d:table)
  "Every command there is, by the package it was written in. A command is defined
where its code is; whether it stands is whether its system is running.")
(defvar *claimed* nil)
(defvar *asking* nil)

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

(defun offer (prefix)
  (dolist (c (defined prefix) prefix) (d:keep! *commands* (name c) c)))

(defun withdraw (prefix)
  (dolist (c (defined prefix) prefix) (d:drop! *commands* (name c))))

(defun command (name action &key (describes "") asks on)
  "A binding beside the command it names is one thing to read and one thing to
move: the chord is kept on the command, and whatever keeps keymaps asks."
  (let* ((home (%home))
         (c (make-instance 'command :name name :action action :from home
                                    :describes describes :asks asks :on on)))
    (d:keep! *defined* home
             (d:with (or (d:at (d:all *defined*) home) (d:no-map)) name c))
    (unless (%claimedp home) (d:keep! *commands* name c))
    c))

(defun forget (name)
  (d:drop! *commands* name)
  name)

(defun named (name)
  (etypecase name
    (null nil)
    (command name)
    (string (d:at (d:all *commands*) name))
    (symbol (d:at (d:all *commands*) (string-downcase (symbol-name name))))))

(defun commands ()
  "Every command standing, in no order. A keymap asks this on every keystroke, so
whoever wants them in an order sorts them there."
  (d:vals (d:all *commands*)))

(defun sorted ()
  (sort (commands) #'string< :key #'name))

(defmacro defcommand (name lambda-list options &body body)
  `(command ,name (lambda ,lambda-list ,@body) ,@options))

(defun word (x)
  (cond ((null x) x)
        ((eq x t) x)
        ((keywordp x) x)
        ((symbolp x) (string-downcase (symbol-name x)))
        ((and (consp x) (eq 'quote (first x))) (second x))
        (t x)))

(defgeneric run (command &optional arguments)
  (:method ((name string) &optional arguments)
    (let ((c (named name)))
      (unless c (error 'unknown-command :name name))
      (run c arguments)))
  (:method ((c command) &optional arguments)
    (if (and (null arguments) (asks c))
        (let ((asked (arguments c *standard-input* *standard-output*)))
          (if (eq asked :asking) :asking (apply (action c) asked)))
        (apply (action c) arguments))))

(defun %ask-one (spec input output)
  (destructuring-bind (&key prompt (as :string) default) spec
    (when prompt
      (write-string prompt output)
      (force-output output))
    (let ((line (read-line input nil nil)))
      (cond ((or (null line) (and (string= line "") default)) default)
            ((eq as :form) (read-from-string line))
            ((eq as :integer) (parse-integer line :junk-allowed t))
            (t line)))))

(defgeneric arguments (command input output)
  (:method ((name string) input output)
    (let ((c (named name)))
      (unless c (error 'unknown-command :name name))
      (arguments c input output)))
  (:method ((c command) input output)
    (cond ((null (asks c)) nil)
          (*asking* (funcall *asking* c))
          (t (loop :for spec :in (asks c) :collect (%ask-one spec input output))))))

(defun %command (name)
  "What the command at this path is for, as it stands now: one redefined at the
repl is the same path saying something else."
  (when (named name)
    (node:place name :reads (lambda ()
                              (let ((c (named name))) (and c (describes c)))))))

(defun attach (root)
  (node:attach (node:place "cmd"
                           :names (lambda () (mapcar #'name (sorted)))
                           :each #'%command
                           :describes "every command there is")
               root))
