(defpackage #:pine/run/command
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine/fs/node))
  (:export #:command #:commandp #:defcommand #:named #:commands #:forget
           #:name #:action #:describes #:asks #:arguments #:run #:word
           #:unknown-command #:attach #:*asking*))
(in-package #:pine/run/command)

(defvar *commands* (d:table))
(defvar *asking* nil)

(define-condition unknown-command (error)
  ((name-of :initarg :name :reader name-of))
  (:report (lambda (c s) (format s "no command named ~s" (name-of c)))))

(defclass command ()
  ((name      :initarg :name      :reader name)
   (action    :initarg :action    :reader action)
   (describes :initarg :describes :reader describes :initform "")
   (asks      :initarg :asks      :reader asks      :initform nil))
  (:documentation "A named thing you can run. Not a lisp function: its arguments
are words, so a name nobody has fbound is still something to do."))

(defclass commands-node (node:node)
  ((savedp :allocation :class :initform nil :reader node:savedp)))

(defclass command-node (node:node)
  ((savedp :allocation :class :initform nil :reader node:savedp)))

(defmethod print-object ((c command) stream)
  (print-unreadable-object (c stream :type t)
    (write-string (name c) stream)))

(defun commandp (x) (typep x 'command))

(defun command (name action &key (describes "") asks)
  (d:keep! *commands* name
           (make-instance 'command :name name :action action
                                   :describes describes :asks asks)))

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
  (sort (d:vals (d:all *commands*)) #'string< :key #'name))

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

(defun %command-node (n name)
  (node:child n name
              (lambda () (make-instance 'command-node :name name :over n))))

(defmethod node:nodes ((n commands-node))
  (loop :for c :in (commands) :collect (%command-node n (name c))))

(defmethod node:resolve ((n commands-node) name)
  (when (named name) (%command-node n (princ-to-string name))))

(defmethod node:contents ((n commands-node)) (mapcar #'name (commands)))

(defmethod node:contents ((n command-node))
  "What the command at this path is for, as it stands now: one redefined at the
repl is the same path saying something else."
  (let ((c (named (node:name n))))
    (and c (describes c))))

(defun attach (root)
  (node:attach (make-instance 'commands-node :name "cmd"
                                             :describes "every command there is")
               root))
