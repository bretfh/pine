(defpackage #:pine.repl.command
  (:use #:cl)
  (:shadow #:describe)
  (:export #:command #:commandp #:defcommand #:command-named #:commands
           #:forget #:name #:action #:describes #:asks #:arguments #:run
           #:word #:unknown-command #:name-of))

(in-package #:pine.repl.command)

(defvar *commands* (make-hash-table :test 'equal))

(define-condition unknown-command (error)
  ((name-of :initarg :name :reader name-of))
  (:report (lambda (c s) (format s "no command named ~s" (name-of c)))))

(defclass command ()
  ((name      :initarg :name      :reader name)
   (action    :initarg :action    :reader action)
   (describes :initarg :describes :reader describes :initform "")
   (asks      :initarg :asks      :reader asks      :initform nil)))

(defmethod print-object ((c command) stream)
  (print-unreadable-object (c stream :type t)
    (write-string (name c) stream)))

(defun commandp (x) (typep x 'command))

(defun command (name action &key (describes "") asks)
  (setf (gethash name *commands*)
        (make-instance 'command :name name :action action
                                :describes describes :asks asks)))

(defun forget (name)
  (remhash name *commands*)
  name)

(defun command-named (name)
  (etypecase name
    (null nil)
    (command name)
    (string (gethash name *commands*))
    (symbol (gethash (string-downcase (symbol-name name)) *commands*))))

(defun commands ()
  (sort (loop :for c :being :the :hash-values :of *commands* :collect c)
        #'string< :key #'name))

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
    (let ((c (command-named name)))
      (unless c (error 'unknown-command :name name))
      (run c arguments)))
  (:method ((c command) &optional arguments)
    (apply (action c) arguments)))

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
    (let ((c (command-named name)))
      (unless c (error 'unknown-command :name name))
      (arguments c input output)))
  (:method ((c command) input output)
    (loop :for spec :in (asks c) :collect (%ask-one spec input output))))
