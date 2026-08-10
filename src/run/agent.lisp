(defpackage #:pine.run.agent
  (:use #:cl)
  (:local-nicknames (#:d #:pine.data) (#:fault #:pine.run.fault))
  (:export #:agent #:attend #:tell #:ask #:stop #:agents #:agent-named #:name #:ref
           #:in-receive-p #:blocking-ask #:*asking*))

(in-package #:pine.run.agent)

(defvar *agents* (d:table))
(defvar *system* nil)
(defvar *asking* 5)

(define-condition blocking-ask (error)
  ((of :initarg :of :reader of))
  (:report (lambda (c stream)
             (format stream "Asked ~a from inside a receive.
A receive owes its mailbox an answer, so it may not wait for one: read what it
was handed, or TELL and take the reply as a message." (of c)))))

(defclass agent ()
  ((name :initarg :name :reader name)
   (ref  :initarg :ref  :reader ref)))

(defmethod print-object ((a agent) stream)
  (print-unreadable-object (a stream :type t)
    (write-string (name a) stream)))

(defun agents () (d:vals (d:all *agents*)))

(defun agent-named (name) (d:at (d:all *agents*) name))

(defun in-receive-p ()
  "True on a thread inside a receive. Read in value position: act:*self* is a
symbol macro, so BOUNDP answers about the macro name and is false anywhere."
  (and sento.actor:*self* t))

(defun attend (system)
  "The system this image's endpoints are made in."
  (setf *system* system))

(defun agent (name receive &key (dispatcher :shared) (in *system*))
  "An endpoint of its own: messages to it are handled one at a time, in the
order they were sent. :PINNED gives it a thread, which is what something that
may park in a fault needs."
  (let* ((a (make-instance
             'agent :name name
             :ref (sento.actor-context:actor-of
                   in
                   :name name
                   :dispatcher dispatcher
                   :receive (lambda (message)
                              (fault:attempt (lambda () (funcall receive message))
                                             name))))))
    (d:keep! *agents* name a)
    a))

(defgeneric tell (to message)
  (:method ((name string) message) (tell (agent-named name) message))
  (:method ((a agent) message) (sento.actor:tell (ref a) message) message)
  (:method ((it null) message) (declare (ignore message)) nil))

(defgeneric ask (of message &key timeout)
  (:method ((name string) message &key timeout)
    (ask (agent-named name) message :timeout timeout))
  (:method ((a agent) message &key (timeout *asking*))
    (when (in-receive-p) (error 'blocking-ask :of (name a)))
    (sento.actor:ask-s (ref a) message :time-out timeout)))

(defgeneric stop (it)
  (:method ((name string) ) (stop (agent-named name)))
  (:method ((a agent))
    (ignore-errors (sento.actor:tell (ref a) :stop))
    (d:drop! *agents* (name a))
    a)
  (:method ((it null)) nil))
