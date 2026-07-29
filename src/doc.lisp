(defpackage #:pine.doc
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path))
  (:export #:mount))

(in-package #:pine.doc)
(named-readtables:in-readtable pine.path:syntax)

;;;; What any path is, and what it takes.
;;;;
;;;;   (read /doc/audio/volume)   => "0..100"
;;;;
;;;; There is no manual and no docstring registry: a provider says what its
;;;; clause is for beside the code that answers, and this reads it back. A path
;;;; nobody documented answers nil, which is the difference between a system
;;;; that is documented and one that says it is.

(defun provider ()
  (ns:provider
   (/doc/?@rest
    {:read (pine.data:fn [] (ns:doc (apply #'p:path rest)))
     :doc "what that path is, and what it takes"})
   (/doc {:read (pine.data:fn [] "what any path is, and what it takes")})))

(defun mount ()
  (ns:write /doc (provider)))
