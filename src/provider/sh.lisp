(defpackage #:pine.provider.sh
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path))
  (:export #:mount #:*ran-kept*))

(in-package #:pine.provider.sh)
(named-readtables:in-readtable pine.path:syntax)

;;;; Commands. Reading and doing are different acts and take different forms,
;;;; and in both the value carries the information: nothing writes t to mean go.
;;;;
;;;;   (read  /sh/${"hostname"})            run it, answer what it said
;;;;   (write /sh [:run "setsid" "-f" "x"]) argv, so nothing is shell quoted
;;;;   (write /sh [:sh "cd ~ && x | y"])    when you want the shell's own syntax
;;;;
;;;; This is the escape hatch, not the habit: where a provider models the
;;;; thing, use it.

(defparameter *ran-kept* 100
  "How many commands /sh remembers having run.")

(defun %output (argv)
  (multiple-value-bind (out err code)
      (uiop:run-program argv :output '(:string :stripped t)
                             :error-output nil
                             :ignore-error-status t)
    (declare (ignore err code))
    out))

(defvar *ran* nil
  "What has been run, newest first. Kept here rather than at a path under /sh,
because every path under /sh is a command to run.")

(defun %note (command)
  (push command *ran*)
  (when (> (length *ran*) *ran-kept*)
    (setf *ran* (subseq *ran* 0 *ran-kept*)))
  command)

(defun provider ()
  (ns:provider
   (/sh/?command
    {:read (pine.data:fn [] (%output (list "sh" "-c" command)))
     :doc "what the command says on its output"})
   (/sh
    {:read (pine.data:fn [] (fset:convert 'fset:seq *ran*))
     :verbs {:run (pine.data:fn [&rest argv]
                    (%note (format nil "~{~a~^ ~}" argv))
                    (uiop:launch-program argv :output nil :error-output nil)
                    t)
             :sh (pine.data:fn [line]
                   (%note line)
                   (uiop:launch-program (list "sh" "-c" line)
                                        :output nil :error-output nil)
                   t)}
     :doc "what has run, newest first; [:run ARGV...] or [:sh LINE] to run one"})))

(defun mount ()
  (ns:write /sh (provider)))
