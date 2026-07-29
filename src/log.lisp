(defpackage #:pine.log
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns))
  (:export #:mount #:note #:*kept*))

(in-package #:pine.log)
(named-readtables:in-readtable pine.path:syntax)

;;;; Output. What pine has to say is a ring at /log: a read answers the last
;;;; line, /log/* the ring, and a write appends. There is no logger, no level
;;;; and no handler, because a line of output is a value at a path and anything
;;;; that wants to show it watches.

(defparameter *kept* 500
  "How many lines /log remembers.")

(defun mount ()
  "Bound the ring, and say what it is.

Output is what this image said rather than what the world holds, so it is not
kept in the file: the store's tail is state, and a line of output is not."
  (setf (ns:setting /log :max) *kept*
        (ns:setting /log :keep) nil)
  (ns:write /log
            (ns:provider
             (/log {:in (pine.data:fn [line] (princ-to-string line))
                    :doc "the last line pine said; /log/* is the ring"}))))

(defun note (control &rest arguments)
  "Say one line: to /log, where anything watching hears it, and to the error
stream, which is where a daemon's output is read from before anything is
attached to watch."
  (let ((line (apply #'format nil control arguments)))
    (ns:write /log line)
    (format *error-output* "~&pine: ~a~%" line)
    (finish-output *error-output*)
    line))
