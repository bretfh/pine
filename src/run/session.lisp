(defpackage #:pine/run/session
  (:use #:cl)
  (:shadow #:read #:print #:close)
  (:local-nicknames (#:d #:pine/data) (#:command #:pine/run/command))
  (:export
   #:open-session #:sessions #:close #:in #:read
   #:evaluate #:interact #:answered #:fault #:*session*))
(in-package #:pine/run/session)

(defvar *session* nil)
(defvar *sessions* nil)
(defvar *kept* 200)
(defvar *prompt* "pine> ")

(defclass evaluation ()
  ((form     :initarg :form     :reader form)
   (answered :initarg :answered :accessor answered :initform nil)
   (fault    :initarg :fault    :accessor fault    :initform nil)
   (said     :initarg :said     :accessor said     :initform "")
   (at-time  :initarg :at-time  :reader at-time    :initform (get-universal-time)))
  (:documentation "What one form did: what it answered, what it printed, and what
it broke on."))

(defmethod print-object ((e evaluation) stream)
  (print-unreadable-object (e stream :type t)
    (cl:print (form e) stream)))

(defclass session ()
  ((name         :initarg :name      :reader name       :initform "session")
   (package-of   :initarg :package   :accessor package-of
                 :initform (find-package :cl-user))
   (readtable-of :initarg :readtable :accessor readtable-of :initform nil)
   (in           :initarg :in        :accessor in       :initform nil)
   (history      :initform nil       :accessor history)
   (input        :initarg :input     :reader input      :initform *standard-input*)
   (output       :initarg :output    :reader output     :initform *standard-output*)
   (openp        :initform t         :accessor openp))
  (:documentation "Somewhere forms are read and evaluated: a package, a readtable
and a place in the namespace. A terminal has one, the editor has one, and a form
sent from another image is evaluated in one."))

(defmethod print-object ((s session) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~a ~a" (name s) (package-name (package-of s)))))

(defun open-session (&rest initargs &key &allow-other-keys)
  (let ((s (apply #'make-instance 'session initargs)))
    (cl:push s *sessions*)
    s))

(defun sessions () *sessions*)

(defmethod close ((s session))
  (setf (openp s) nil
        *sessions* (cl:remove s *sessions*))
  s)

(defgeneric read (session &optional from)
  (:method ((s session) &optional from)
    (let ((*package* (package-of s))
          (*readtable* (or (readtable-of s) *readtable*)))
      (if from
          (cl:read-from-string from)
          (cl:read (input s) nil :eof)))))

(defun %ask-one (spec input output)
  (destructuring-bind (&key prompt (as :string) default) spec
    (when prompt
      (write-string prompt output)
      (force-output output))
    (let ((line (cl:read-line input nil nil)))
      (cond ((or (null line) (and (string= line "") default)) default)
            ((eq as :form) (cl:read-from-string line))
            ((eq as :integer) (parse-integer line :junk-allowed t))
            (t line)))))

(defmethod command:asking ((s session) c)
  "A session on a stream reads what the command needs off it, one line each."
  (loop :for spec :in (command:asks c)
        :collect (%ask-one spec (input s) (output s))))

(defgeneric evaluate (session form))

(defmethod evaluate :around ((s session) form)
  (let ((e (call-next-method)))
    (setf (history s) (d:capped (history s) e *kept*))
    e))

(defun %command-for (form)
  "The command FORM names, if it names one, and the words it was given. A name
nobody has fbound is still something to do."
  (typecase form
    (symbol (values (command:named form) nil))
    (cons (values (and (symbolp (cl:first form)) (command:named (cl:first form)))
                  (cl:rest form)))
    (t nil)))

(defmethod evaluate ((s session) form)
  (multiple-value-bind (c given) (%command-for form)
    (let* ((said (make-string-output-stream))
           (*standard-output* said))
      (multiple-value-bind (answered fault)
          (handler-case
              (values (multiple-value-list
                       (let ((*package* (package-of s))
                             (*readtable* (or (readtable-of s) *readtable*))
                             (*session* s)
                             (command:*at* s))
                         (if c
                             (command:run c (and given
                                                 (mapcar #'command:word given)))
                             (eval form))))
                      nil)
            (error (broke) (values nil broke)))
        (make-instance 'evaluation :form form :answered answered :fault fault
                                   :said (get-output-stream-string said))))))

(defgeneric print (session evaluation)
  (:method ((s session) (e evaluation))
    (let ((out (output s)))
      (write-string (said e) out)
      (cond ((fault e) (format out "~&; ~a~%" (fault e)))
            (t (let ((*package* (package-of s)))
                 (dolist (v (answered e))
                   (format out "~&")
                   (cl:print v out)))
               (terpri out)))
      (force-output out)
      e)))

(defgeneric interact (session)
  (:method ((s session))
    (loop :while (openp s)
          :do (format (output s) "~&~a" *prompt*)
              (force-output (output s))
              (let ((form (read s)))
                (if (eq form :eof)
                    (close s)
                    (print s (evaluate s form)))))
    s))
