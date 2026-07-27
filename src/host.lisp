(defpackage #:pine.host
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path))
  (:export #:serve #:mount #:unmount #:*timeout*))

(in-package #:pine.host)
(named-readtables:in-readtable pine.path:syntax)

;;;; Another pine, under a prefix. Everything the API says works host
;;;; qualified or bare, because /host/NAME is a provider like any other and
;;;; what it knows is one socket.
;;;;
;;;; Only text crosses: a value is written in the serialization the store uses,
;;;; so a map, a set and a path arrive as themselves and nothing on either side
;;;; has to agree about anything else.

(defparameter *timeout* 5
  "Seconds to wait on another pine before saying it did not answer.")

(defun %out (value) (pine.data:serialize value))
(defun %in (text) (and text (pine.data:deserialize text 'p:data)))

;;;; This side: answering for our own namespace

(defun serve (system)
  "Make this image's namespace reachable by another pine."
  (sento.actor-context:actor-of system
    :name "ns"
    :dispatcher :pinned
    :receive
    (lambda (message)
      (sento.actor:reply
       (handler-case
           (destructuring-bind (verb &optional path value) message
             (ecase verb
               (:read (%out (ns:read (p:parse path))))
               (:write (ns:write (p:parse path) (%in value)) (%out t))
               (:ls (%out (mapcar #'p:text
                                  (pine.data:keys
                                   (ns:read (p:child (p:parse path) (p:any)))))))))
         (error (c) (list :error (princ-to-string c))))))))

;;;; The other side: a provider that forwards

(defun %ask (ref message)
  (let ((answer (pine.core.actor:ask ref message :timeout *timeout*)))
    (when (and (consp answer) (eq :error (first answer)))
      (error "the other pine refused: ~a" (second answer)))
    answer))

(defun provider (ref)
  (ns:provider
   (/host/?name/?@rest
    {:read (pine.data:fn []
             (%in (%ask ref (list :read (p:text (apply #'p:path rest))))))
     :write (pine.data:fn [v]
              (%ask ref (list :write (p:text (apply #'p:path rest)) (%out v))))
     :ls (pine.data:fn []
           (mapcar #'p:leaf
                   (mapcar #'p:parse
                           (%in (%ask ref (list :ls (p:text (apply #'p:path rest))))))))
     :doc "a path on another pine"})))

(defun mount (name &key system host port)
  "Reach the pine at HOST:PORT as NAME, so /host/NAME/... is its namespace."
  (let ((ref (sento.remoting:make-remote-ref
              system (pine.core.server:daemon-uri "ns" :host host :port port))))
    (ns:write (p:child /host (string-downcase (string name))) (provider ref))
    ref))

(defun unmount (name)
  (ns:write (p:child /host (string-downcase (string name))) nil))
