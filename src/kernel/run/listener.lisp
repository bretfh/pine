(defpackage #:pine/run/listener
  (:use #:cl)
  (:shadow #:read #:print #:close)
  (:local-nicknames (#:d #:pine/data) (#:command #:pine/run/command))
  (:export
   #:open-listener #:listeners #:close #:in #:read
   #:evaluate #:interact #:answered #:fault #:*listener*
   #:package-of #:readtable-of))
(in-package #:pine/run/listener)

(defvar *listener* nil)
(defvar *listeners* nil)
(defvar *history-kept* 200)
(defvar *prompt* "pine> ")

(defclass evaluation ()
  ((form     :initarg :form     :reader form)
   (answered :initarg :answered :accessor answered :initform nil)
   (fault    :initarg :fault    :accessor fault    :initform nil)
   (said     :initarg :said     :accessor said     :initform "")
   (at-time  :initarg :at-time  :reader at-time    :initform (get-universal-time))))

(defmethod print-object ((e evaluation) stream)
  (print-unreadable-object (e stream :type t)
    (cl:print (form e) stream)))

(defclass listener ()
  ((name         :initarg :name      :reader name       :initform "listener")
   (package-of   :initarg :package   :accessor package-of
                 :initform (find-package :cl-user))
   (readtable-of :initarg :readtable :accessor readtable-of :initform nil)
   (in           :initarg :in        :accessor in       :initform nil)
   (history      :initform nil       :accessor history)
   (input        :initarg :input     :reader input      :initform *standard-input*)
   (output       :initarg :output    :reader output     :initform *standard-output*)
   (openp        :initform t         :accessor openp)))

(defmethod print-object ((s listener) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~a ~a" (name s) (package-name (package-of s)))))

(defun open-listener (&rest initargs &key &allow-other-keys)
  (let ((s (apply #'make-instance 'listener initargs)))
    (sb-ext:atomic-update *listeners* (lambda (all) (cons s all)))
    s))

(defun listeners () *listeners*)

(defmethod close ((s listener))
  (setf (openp s) nil)
  (sb-ext:atomic-update *listeners* (lambda (all) (cl:remove s all)))
  s)

(defgeneric read (listener &optional from)
  (:method ((s listener) &optional from)
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

(defmethod command:asking ((s listener) c)
  (loop :for spec :in (command:asks c)
        :collect (%ask-one spec (input s) (output s))))

(defgeneric evaluate (listener form))

(defmethod evaluate :around ((s listener) form)
  (let ((e (call-next-method)))
    (setf (history s) (d:capped (history s) e *history-kept*))
    e))

(defun %command-for (form)
  (typecase form
    (symbol (values (command:named form) nil))
    (cons (values (and (symbolp (cl:first form)) (command:named (cl:first form)))
                  (cl:rest form)))
    (t nil)))

(defmethod evaluate ((s listener) form)
  (multiple-value-bind (c given) (%command-for form)
    (let* ((said (make-string-output-stream))
           (*standard-output* said))
      (multiple-value-bind (answered fault)
          (handler-case
              (handler-bind
                  ((error (lambda (broke)
                            (pine/run/fault:report broke (name s)))))
                (values (multiple-value-list
                         (let ((*package* (package-of s))
                               (*readtable* (or (readtable-of s) *readtable*))
                               (*listener* s)
                               (command:*at* s))
                           (if c
                               (command:run c (and given
                                                   (mapcar #'command:word given)))
                               (eval form))))
                        nil))
            (error (broke) (values nil broke)))
        (make-instance 'evaluation :form form :answered answered :fault fault
                                   :said (get-output-stream-string said))))))

(defgeneric print (listener evaluation)
  (:method ((s listener) (e evaluation))
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

(defgeneric interact (listener)
  (:method ((s listener))
    (loop :while (openp s)
          :do (format (output s) "~&~a" *prompt*)
              (force-output (output s))
              (multiple-value-bind (form broke)
                  (handler-case (values (read s) nil)
                    (end-of-file () (values :eof nil))
                    (error (c) (values nil c)))
                (cond (broke (format (output s) "~&; ~a~%" broke)
                             (force-output (output s)))
                      ((eq form :eof) (close s))
                      (t (print s (evaluate s form))))))
    s))
