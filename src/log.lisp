(defpackage #:pine.log
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns))
  (:export #:note #:*kept*))

(in-package #:pine.log)
(named-readtables:in-readtable pine.path:syntax)

;;;; What pine says is a ring at /log: a read answers the last line, /log/* the
;;;; ring, and a write appends.

(defparameter *kept* 500
  "How many lines /log remembers.")

(defun note (control &rest arguments)
  "Say one line: to /log, and to the error stream, which is where a daemon's
output is read from before anything is attached to watch."
  (let ((line (apply #'format nil control arguments)))
    (ns:write /log line)
    (format *error-output* "~&pine: ~a~%" line)
    (finish-output *error-output*)
    line))

(ns:serve :log
  {:at [/log]
   :doc "what pine says, as a ring at /log"
   ;; output is what this image said rather than what the world holds, so the
   ;; ring is bounded and not kept in the file
   :up (lambda ()
         (setf (ns:setting /log :max) *kept*
               (ns:setting /log :keep) nil)
         (ns:write /log
                   (ns:provider
                    (/log {:in (pine.data:fn [line] (princ-to-string line))
                           :doc "the last line pine said; /log/* is the ring"}))))})
