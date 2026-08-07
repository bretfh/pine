(defpackage #:pine.doc
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path)))

(in-package #:pine.doc)
(named-readtables:in-readtable pine.path:syntax)

;;;; What any path is, and what it takes: (read /doc/audio/volume) => "0..100".
;;;; A provider says what its clause is for beside the code that answers, and
;;;; this reads it back. A path nobody documented answers nil.

(defun provider ()
  (ns:provider
   (/doc/?@rest
    {:read (pine.data:fn [] (ns:doc (apply #'p:path rest)))
     :doc "what that path is, and what it takes"})
   (/doc {:read (pine.data:fn [] "what any path is, and what it takes")})))

(ns:serve :doc
  {:at [/doc]
   :doc "what any path is, read back from the clause that serves it"
   :up (lambda () (ns:write /doc (provider)))})
