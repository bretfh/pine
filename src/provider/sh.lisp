(defpackage #:pine.provider.sh
  (:use #:cl)
  (:local-nicknames (#:d #:pine.data) (#:c #:pine.run.cell)
                    (#:node #:pine.fs.node) (#:task #:pine.run.task))
  (:export #:sh-node #:command-node #:install #:ran #:*kept* #:*environment-out*
           #:output-of #:run-line #:launch))

(in-package #:pine.provider.sh)

(defvar *kept* 100)
(defvar *ran* (c:cell (d:no-seq)))

(defparameter *environment-out*
  '("GUIX_ENVIRONMENT" "CL_SOURCE_REGISTRY" "ASDF_OUTPUT_TRANSLATIONS"
    "LD_LIBRARY_PATH"))

(defclass sh-node (node:node) ())

(defclass command-node (node:node)
  ((line :initarg :line :reader line)))

(defun ran () (d:as :list (c:held *ran*)))

(defun %note (line)
  (c:swap *ran*
          (lambda (all)
            (let ((next (d:insert-at all 0 line)))
              (if (> (d:size next) *kept*) (d:subseq next 0 *kept*) next))))
  line)

(defun output-of (line)
  (multiple-value-bind (out err code)
      (uiop:run-program (list "sh" "-c" line)
                        :output '(:string :stripped t)
                        :error-output nil
                        :ignore-error-status t)
    (declare (ignore err code))
    out))

(defun %session-environment ()
  (remove-if (lambda (entry)
               (some (lambda (name)
                       (let ((prefix (concatenate 'string name "=")))
                         (and (>= (length entry) (length prefix))
                              (string= prefix entry :end2 (length prefix)))))
                     *environment-out*))
             (sb-ext:posix-environ)))

(defun launch (argv)
  (uiop:launch-program argv
                       :environment (%session-environment)
                       :directory (user-homedir-pathname)
                       :output nil :error-output nil))

(defun run-line (line)
  (%note line)
  (launch (list "sh" "-l" "-c" (concatenate 'string "exec " line)))
  t)

(defmethod node:nodes ((n sh-node))
  (loop :for line :in (ran)
        :collect (make-instance 'command-node :name line :parent n :line line)))

(defmethod node:resolve ((n sh-node) name)
  (make-instance 'command-node :name name :parent n :line name))

(defmethod node:contents ((n sh-node)) (ran))
(defmethod node:contents ((n command-node)) (output-of (line n)))
(defmethod node:leafp ((n command-node)) t)
(defmethod node:persistp ((n sh-node)) nil)
(defmethod node:persistp ((n command-node)) nil)

(defmethod (setf node:contents) (value (n command-node))
  (declare (ignore value))
  (run-line (line n)))

(defmethod node:livep ((n sh-node)) t)
(defmethod node:livep ((n command-node)) t)

(defun install (root)
  (node:attach (make-instance 'sh-node :name "sh"
                                       :describes "running something, and what it said")
               root))
