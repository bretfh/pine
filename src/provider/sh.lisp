(defpackage #:pine.provider.sh
  (:use #:cl)
  (:local-nicknames (#:d #:pine.data) (#:c #:pine.run.cell)
                    (#:node #:pine.fs.node) (#:task #:pine.run.task))
  (:export #:sh-node #:command-node #:stream-node #:install #:ran #:*kept*
           #:*environment-out* #:output-of #:run-line #:launch
           #:streaming #:listen! #:quiet! #:listening #:said #:asked #:forget-all #:*sh*))

(in-package #:pine.provider.sh)

(defvar *kept* 100)
(defvar *ran* (c:cell (d:no-seq)))
(defvar *sh* nil)
(defvar *lines-kept* 20)

(defparameter *environment-out*
  '("GUIX_ENVIRONMENT" "CL_SOURCE_REGISTRY" "ASDF_OUTPUT_TRANSLATIONS"
    "LD_LIBRARY_PATH"))

(defclass sh-node (node:node) ())

(defclass command-node (node:node)
  ((line :initarg :line :reader line)))

(defclass stream-node (node:node)
  ((line :initarg :line :reader line)
   (took :initform (c:cell nil) :reader took)
   (said :initform (c:cell nil) :reader said)))

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
        :collect (node:child n line
                             (lambda ()
                               (make-instance 'command-node :name line
                                                            :parent n :line line)))))

(defmethod node:resolve ((n sh-node) name)
  (node:child n name
              (lambda ()
                (make-instance 'command-node :name name :parent n :line name))))

(defun %reader (n process)
  (task:spawn (format nil "sh ~a" (line n))
              (lambda ()
                (loop :with out := (uiop:process-info-output process)
                      :for said := (handler-case (read-line out nil nil)
                                     (stream-error () nil))
                      :while said
                      :do (c:swap (said n)
                                  (lambda (all)
                                    (let ((next (cons said all)))
                                      (if (> (length next) *lines-kept*)
                                          (subseq next 0 *lines-kept*)
                                          next))))
                         (node:invalidate n)))))

(defun listening (n) (and (c:held (took n)) t))

(defun listen! (n)
  (unless (listening n)
    (let ((process (uiop:launch-program (list "sh" "-c" (line n))
                                        :output :stream :error-output nil)))
      (c:put (took n) process)
      (%reader n process)))
  n)

(defun quiet! (n)
  (let ((process (c:held (took n))))
    (when process
      (ignore-errors (uiop:terminate-process process :urgent t))
      (c:put (took n) nil)))
  n)

(defun streaming (line &optional (root *sh*))
  (when root
    (let ((n (node:child root (format nil "stream:~a" line)
                         (lambda ()
                           (make-instance 'stream-node :name line :parent root
                                                       :line line)))))
      (listen! n))))

(defmethod node:contents ((n stream-node)) (first (c:held (said n))))
(defmethod node:leafp ((n stream-node)) t)
(defmethod node:persistp ((n stream-node)) nil)
(defmethod node:livep ((n stream-node)) t)

(defmethod (setf node:contents) (value (n stream-node))
  (if value (listen! n) (quiet! n))
  value)

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

(defun asked (line)
  "What a command says, remembered while a provider is reading it, so a bar
that reads five things out of one command runs it once."
  (let ((n (node:child *sh* line
                       (lambda ()
                         (make-instance 'command-node :name line :parent *sh*
                                                      :line line)))))
    (node:contents n)))

(defun install (root)
  (setf *sh* (node:attach (make-instance 'sh-node :name "sh"
                                                  :describes "running something, and what it said")
                          root))
  (setf pine.provider.out:*through* #'asked)
  *sh*)

(defun forget-all ()
  (when *sh*
    (dolist (each (node:children *sh*))
      (when (typep each 'stream-node) (quiet! each))))
  t)
