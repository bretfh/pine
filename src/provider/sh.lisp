(defpackage #:pine.provider.sh
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path))
  (:export #:server #:*ran-kept*))

(in-package #:pine.provider.sh)
(named-readtables:in-readtable pine.path:syntax)

;;;; Commands. Reading and doing are different acts and take different forms,
;;;; and in both the value carries the information: nothing writes t to mean go.
;;;;
;;;;   (read  /sh/${"hostname"})            run it, answer what it said
;;;;   (write /sh [:run "setsid" "-f" "x"]) argv, so nothing is shell quoted
;;;;   (write /sh [:sh "cd ~ && x | y"])    when you want the shell's own syntax
;;;;
;;;; What has run and what is streaming are the space's, kept by the server:
;;;; every path under /sh is a command, so /sh/0 would be the command "0".

(defparameter *ran-kept* 100
  "How many commands /sh remembers having run.")

(defun %output (argv)
  (multiple-value-bind (out err code)
      (uiop:run-program argv :output '(:string :stripped t)
                             :error-output nil
                             :ignore-error-status t)
    (declare (ignore err code))
    out))

(defun %cell (which)
  (let ((kept (ns:kept :sh)))
    (and (fset:map? kept) (fset:lookup kept which))))

(defun ran ()
  (let ((cell (%cell :ran)))
    (if cell (sento.atomic:atomic-get cell) (fset:empty-seq))))

(defun %note (command)
  (let ((cell (%cell :ran)))
    (when cell
      (sento.atomic:atomic-swap
       cell
       (lambda (seq)
         (let ((next (fset:with-first seq command)))
           (if (> (fset:size next) *ran-kept*)
               (fset:subseq next 0 *ran-kept*)
               next))))))
  command)

;;;; Watching a command runs it and writes each line it says. A command that
;;;; nobody is listening to is not running: the stream starts when the first
;;;; watch appears and stops when the last one goes.

(defun %streams () (%cell :streams))

(defun %held ()
  (let ((cell (%streams)))
    (if cell (sento.atomic:atomic-get cell) (fset:empty-map))))

(defun %stream (command)
  "Run COMMAND and write each line it says to its own path."
  (let ((cell (%streams))
        (space ns:*space*)
        (at (p:child /sh command))
        (process (uiop:launch-program (list "sh" "-c" command)
                                      :output :stream :error-output nil)))
    (when cell
      (sento.atomic:atomic-swap cell (lambda (m) (fset:with m command process))))
    ;; the cell and the space are taken now, not looked up in the reader: this
    ;; thread outlives the call and the space it belongs to is not whichever
    ;; one is current when a line arrives
    (bordeaux-threads:make-thread
     (lambda ()
       (let ((ns:*space* space))
         (unwind-protect
              (loop :with out = (uiop:process-info-output process)
                    :for line = (read-line out nil nil)
                    :while line
                    :do (ns:write at line))
           (when cell
             (sento.atomic:atomic-swap cell (lambda (m) (fset:less m command)))))))
     :name (format nil "pine-sh ~a" command))
    process))

(defun %streamingp (command)
  (and (fset:lookup (%held) command) t))

(defun %quiet (command)
  (let ((cell (%streams))
        (process (fset:lookup (%held) command)))
    (when process
      (when cell
        (sento.atomic:atomic-swap cell (lambda (m) (fset:less m command))))
      (ignore-errors (uiop:terminate-process process :urgent t)))
    nil))

(defun %listen (command listening)
  (cond ((and listening (not (fset:lookup (%held) command))) (%stream command))
        ((not listening) (%quiet command))))

(defun provider ()
  (ns:provider
   (/sh/?command
    {:read (pine.data:fn []
             ;; a command that is streaming is not run again to be read: what
             ;; it last said is what it says
             (if (%streamingp command)
                 (ns:held (p:child /sh command))
                 (%output (list "sh" "-c" command))))
     :in (pine.data:fn [v]
           (if (stringp v)
               v
               (progn (%note command)
                      (uiop:launch-program (list "sh" "-c" command)
                                           :output nil :error-output nil)
                      (ns:held (p:child /sh command)))))
     :watch (pine.data:fn [listening] (%listen command listening))
     :doc "what the command says on its output; write it to run it, watch it
for each line"})
   (/sh
    {:read (pine.data:fn [] (ran))
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

(defclass server (ns:server) ()
  (:default-initargs :name :sh :serves (list /sh))
  (:documentation "Running something, and what it said."))

(defmethod ns:raise ((s server) &key &allow-other-keys)
  "Serve /sh, and answer the cells this space keeps: what it ran, and the
commands it is subscribed to."
  (ns:write /sh (provider))
  (fset:map (:ran (sento.atomic:make-atomic-reference :value (fset:empty-seq)))
            (:streams (sento.atomic:make-atomic-reference :value (fset:empty-map)))))

(defmethod ns:lower ((s server))
  "Stop every subscription. A process nobody is listening to is not running."
  (let ((cell (%cell :streams)))
    (when cell
      (fset:do-map (command process (sento.atomic:atomic-get cell))
        (declare (ignore command))
        (ignore-errors (uiop:terminate-process process :urgent t)))))
  (call-next-method))

(ns:register (make-instance 'server))
