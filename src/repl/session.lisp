(defpackage #:pine.repl.session
  (:use #:cl)
  (:shadow #:read #:print #:close)
  (:local-nicknames (#:cmd #:pine.repl.command) (#:mode #:pine.repl.mode))
  (:export #:session #:open-session #:sessions #:*session* #:*history-kept*
           #:*prompt* #:name #:owner #:package-of #:node-of #:mode-of #:minors
           #:history #:input #:output #:openp
           #:read #:evaluate #:print #:interact #:close
           #:evaluation #:form #:answered #:fault #:said #:at-time))

(in-package #:pine.repl.session)

(defvar *session* nil)
(defvar *sessions* nil)
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

(defclass session ()
  ((name       :initarg :name    :reader name       :initform "session")
   (owner      :initarg :owner   :reader owner      :initform nil)
   (package-of :initarg :package :accessor package-of
               :initform (find-package :cl-user))
   (node-of    :initarg :node    :accessor node-of  :initform nil)
   (mode-of    :initarg :mode    :accessor mode-of  :initform nil)
   (minors     :initarg :minors  :accessor minors   :initform nil)
   (history    :initform nil     :accessor history)
   (input      :initarg :input   :reader input      :initform *standard-input*)
   (output     :initarg :output  :reader output     :initform *standard-output*)
   (openp      :initform t       :accessor openp)))

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
        *sessions* (remove s *sessions*))
  s)

(defmethod mode:in-force ((s session))
  (append (sort (remove nil (mapcar #'mode:mode-named (minors s)))
                #'> :key #'mode:precedence)
          (mode:chain (mode:mode-named (mode-of s)))))

(defmethod mode:setting ((s session) key &optional default)
  (mode:setting (mode:mode-named (mode-of s)) key default))

(defgeneric read (session &optional from)
  (:method ((s session) &optional from)
    (let ((*package* (package-of s)))
      (if from
          (cl:read-from-string from)
          (cl:read (input s) nil :eof)))))

(defun %command-for (session form)
  (typecase form
    (symbol (values (or (mode:binding session (string-downcase (symbol-name form)))
                        (cmd:command-named form))
                    nil))
    (cons (let ((c (and (symbolp (car form)) (cmd:command-named (car form)))))
            (values c (cdr form))))
    (t nil)))

(defun %capturing (thunk)
  (let* ((said (make-string-output-stream))
         (*standard-output* said))
    (multiple-value-bind (answered fault) (funcall thunk)
      (values answered fault (get-output-stream-string said)))))

(defun %attempt (thunk)
  (handler-case (values (multiple-value-list (funcall thunk)) nil)
    (error (c) (values nil c))))

(defgeneric evaluate (session form))

(defmethod evaluate :around ((s session) form)
  (let ((e (call-next-method)))
    (cl:push e (history s))
    (let ((kept (history s)))
      (when (> (length kept) *history-kept*)
        (setf (history s) (subseq kept 0 *history-kept*))))
    e))

(defmethod evaluate ((s session) form)
  (multiple-value-bind (c given) (%command-for s form)
    (multiple-value-bind (answered fault said)
        (%capturing
         (lambda ()
           (%attempt
            (lambda ()
              (let ((*package* (package-of s))
                    (*session* s))
                (if c
                    (let ((args (if given
                                    (mapcar #'cmd:word given)
                                    (cmd:arguments c (input s) (output s)))))
                      (if (eq args :asking) :asking (cmd:run c args)))
                    (eval form)))))))
      (make-instance 'evaluation :form form :answered answered
                                 :fault fault :said said))))

(defgeneric print (session evaluation)
  (:method ((s session) (e evaluation))
    (let ((out (output s)))
      (write-string (said e) out)
      (cond ((fault e)
             (format out "~&; ~a~%" (fault e)))
            (t
             (let ((*package* (package-of s)))
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
