(defpackage #:pine.provider.env
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns))
  (:export #:server))

(in-package #:pine.provider.env)
(named-readtables:in-readtable pine.path:syntax)

;;;; The environment this image was started with, and what it hands to the
;;;; ones it starts.

(defun %set (name value)
  (cffi:foreign-funcall "setenv" :string name :string (princ-to-string value)
                                 :int 1 :int)
  value)

(defun %unset (name)
  (cffi:foreign-funcall "unsetenv" :string name :int)
  nil)

(defun %names ()
  (mapcar (lambda (entry) (subseq entry 0 (position #\= entry)))
          (sb-ext:posix-environ)))

(defun provider ()
  (ns:provider
   (/env/?name
    {:read (pine.data:fn [] (uiop:getenv name))
     :write (pine.data:fn [v] (if (null v) (%unset name) (%set name v)))
     :doc "an environment variable; nil unsets it"})
   (/env {:ls (pine.data:fn [] (%names))
          :doc "every name in the environment"})))

(defclass server (ns:server) ()
  (:default-initargs :name :env :serves (list /env))
  (:documentation "The environment this image was started in."))

(defmethod ns:raise ((s server) &key &allow-other-keys)
  (ns:write /env (provider)))

(ns:register (make-instance 'server))
