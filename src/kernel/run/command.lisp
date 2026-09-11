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

(defvar *at* nil)

(define-condition unknown-command (error)
  ((name-of :initarg :name :reader name-of))
  (:report (lambda (c s) (format s "no command named ~s" (name-of c)))))

(defclass command (fs:value)
  ((action :initarg :action :reader action)))

(defmethod fs:persistent-p ((c command)) nil)

(defmethod describes ((c command)) (getf (fs:contents c) :describes))

(defun asks (c) (getf (fs:contents c) :asks))

(defun on (c) (getf (fs:contents c) :on))

(defmethod print-object ((c command) stream)
  (print-unreadable-object (c stream :type t)
    (write-string (name c) stream)))

(defun commandp (x) (typep x 'command))

(defun %cmd () (fs:at "/cmd"))

(defun command (name action &key (describes "") asks on)
  (let ((on (and on (cons (string-downcase (string (first on))) (rest on)))))
    (fs:mount (lambda ()
                (make-instance 'command :action action
                               :held (list :describes describes :asks asks :on on)))
              (format nil "/cmd/~a" name))))

(defun forget (name)
  (fs:erase (format nil "/cmd/~a" name))
  name)

(defun named (name)
  (etypecase name
    (null nil)
    (command name)
    (string (let ((it (fs:child (%cmd) name))) (and (commandp it) it)))
    (symbol (named (string-downcase (symbol-name name))))))

(defun commands ()
  (remove-if-not #'commandp (fs:children (%cmd))))

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
        ((consp x) (eval x))
        (t x)))

(defgeneric run (command &optional arguments)
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
  (:method (where (c command))
    (declare (ignore where c))
    nil))

(fs:mount (lambda () (make-instance 'fs:mount :describes "every command there is"))
          "/cmd")
