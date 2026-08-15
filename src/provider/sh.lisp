(defpackage #:pine.provider.sh
  (:use #:cl)
  (:local-nicknames (#:d #:pine/data) (#:node #:pine.fs.node) (#:task #:pine/run/task))
  (:export #:sh-node #:command-node #:stream-node #:install #:ran #:*kept*
           #:*environment-out* #:output-of #:run-line #:launch
           #:streaming #:listen! #:quiet! #:listening #:said #:asked #:tethered
           #:forget-all #:*sh* #:*breath*))

(in-package #:pine.provider.sh)

(defvar *kept* 100)
(defvar *ran* (d:box (d:no-seq)))
(defvar *sh* nil)
(defvar *asked* (d:table))
(defparameter *breath* 1/4
  "Seconds an answer stands for. What a bar reads is read again next frame,
not three times in this one.")
(defvar *lines-kept* 20)

(defparameter *environment-out*
  '("GUIX_ENVIRONMENT" "CL_SOURCE_REGISTRY" "ASDF_OUTPUT_TRANSLATIONS"
    "LD_LIBRARY_PATH"))

(defparameter +tethered+
  "~a & pine_child=$!; trap 'kill $pine_child 2>/dev/null' EXIT; ~
   cat >/dev/null; kill $pine_child 2>/dev/null"
  "A stream, tied to the image that asked for it.

The shell holding it reads the pipe pine keeps the other end of. Pine going --
stopped, crashed or killed outright -- closes that end, the read ends, and the
stream is killed rather than left running for weeks holding a bus connection
nothing will ever ask it to let go of.")

(defclass sh-node (node:node) ())

(defclass command-node (node:node)
  ((line :initarg :line :reader line)))

(defclass stream-node (node:node)
  ((line :initarg :line :reader line)
   (took :initform (d:box nil) :reader took)
   (said :initform (d:box nil) :reader said)))

(defun ran () (d:as :list (d:held *ran*)))

(defun %note (line)
  (d:swap! *ran*
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
                      :do (d:swap! (said n)
                                  (lambda (all)
                                    (let ((next (cons said all)))
                                      (if (> (length next) *lines-kept*)
                                          (subseq next 0 *lines-kept*)
                                          next))))
                         (node:invalidate n)))))

(defun listening (n) (and (d:held (took n)) t))

(defun tethered (line)
  (list "sh" "-c" (format nil +tethered+ line)))

(defun listen! (n)
  (unless (listening n)
    (let ((process (uiop:launch-program (tethered (line n))
                                        :input :stream
                                        :output :stream :error-output nil)))
      (d:put! (took n) process)
      (%reader n process)))
  n)

(defun quiet! (n)
  (let ((process (d:held (took n))))
    (when process
      (ignore-errors (close (uiop:process-info-input process)))
      (ignore-errors (uiop:terminate-process process :urgent t))
      (ignore-errors (uiop:wait-process process))
      (d:put! (took n) nil)))
  n)

(defun streaming (line &optional (root *sh*))
  (when root
    (let ((n (node:child root (format nil "stream:~a" line)
                         (lambda ()
                           (make-instance 'stream-node :name line :parent root
                                                       :line line)))))
      (listen! n))))

(defmethod node:contents ((n stream-node)) (first (d:held (said n))))
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
  "What a command says, remembered for a moment, so a panel reading three
things out of playerctl runs it once rather than three times, and a bar built
twice in the same breath does not fork twice."
  (let* ((n (node:child *sh* line
                        (lambda ()
                          (make-instance 'command-node :name line :parent *sh*
                                                       :line line))))
         (now (get-internal-real-time))
         (had (d:at (d:all *asked*) line)))
    (cond ((and had (< (- now (cdr had))
                       (* *breath* internal-time-units-per-second)))
           (car had))
          (t (let ((said (node:contents n)))
               (d:keep! *asked* line (cons said now))
               said)))))

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
