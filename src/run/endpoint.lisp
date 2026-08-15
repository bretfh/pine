(defpackage #:pine/run/endpoint
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:fault #:pine/run/fault))
  (:export #:endpoint #:attend #:tell #:ask #:stop #:endpoints #:endpoint-named
           #:name #:ref #:in-receive-p #:blocking-ask #:*asking*))
(in-package #:pine/run/endpoint)

(defvar *endpoints* (d:table))
(defvar *system* nil)
(defvar *asking* 5)

(define-condition blocking-ask (error)
  ((of :initarg :of :reader of))
  (:report (lambda (c stream)
             (format stream "Asked ~a from inside a receive.
A receive owes its mailbox an answer, so it may not wait for one: read what it
was handed, or TELL and take the reply as a message." (of c)))))

(defclass endpoint ()
  ((name :initarg :name :reader name)
   (ref  :initarg :ref  :reader ref)))

(defmethod print-object ((e endpoint) stream)
  (print-unreadable-object (e stream :type t)
    (write-string (name e) stream)))

(defun endpoints () (d:vals (d:all *endpoints*)))

(defun endpoint-named (name) (d:at (d:all *endpoints*) name))

(defun in-receive-p ()
  "True on a thread inside a receive. Read in value position: act:*self* is a
symbol macro, so BOUNDP answers about the macro name and is false anywhere."
  (and sento.actor:*self* t))

(defun attend (system)
  "The system this image's endpoints are made in."
  (setf *system* system))

(defun endpoint (name receive &key (dispatcher :shared) (in *system*))
  "An endpoint of its own: messages to it are handled one at a time, in the
order they were sent. :PINNED gives it a thread, which is what something that
may park in a fault needs.

A name is one endpoint: asking again for a name that is taken replaces it,
rather than faulting the caller who asked."
  (let ((had (endpoint-named name)))
    (when had (stop had))
    (let ((e (make-instance
              'endpoint :name name
              :ref (sento.actor-context:actor-of
                    in
                    :name name
                    :dispatcher dispatcher
                    :receive (lambda (message)
                               (fault:attempt (lambda () (funcall receive message))
                                              name))))))
      (d:keep! *endpoints* name e)
      e)))

(defgeneric tell (to message)
  (:method ((name string) message) (tell (endpoint-named name) message))
  (:method ((e endpoint) message) (sento.actor:tell (ref e) message) message)
  (:method ((it null) message) (declare (ignore message)) nil))

(defgeneric ask (of message &key timeout)
  (:method ((name string) message &key timeout)
    (ask (endpoint-named name) message :timeout timeout))
  (:method ((e endpoint) message &key (timeout *asking*))
    (when (in-receive-p) (error 'blocking-ask :of (name e)))
    (sento.actor:ask-s (ref e) message :time-out timeout)))

(defgeneric stop (it)
  (:method ((name string)) (stop (endpoint-named name)))
  (:method ((e endpoint))
    (ignore-errors (sento.actor-context:stop *system* (ref e) :wait t))
    (d:drop! *endpoints* (name e))
    e)
  (:method ((it null)) nil))
