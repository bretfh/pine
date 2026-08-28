(defpackage :pine/test
  (:use :cl :fiveam)
  (:local-nicknames (:edit :pine/edit)
                    (:text :pine/text)
                    (:ui :pine/ui)
                    (:d :pine/data) (:word :pine/word)
                    (:node :pine/fs/node) (:tree :pine/fs/tree)
                    (:mount :pine/fs/mount) (:store :pine/fs/store)
                    (:path :pine/fs/path) (:commit :pine/fs/commit)
                    (:actors :pine/run/actors) (:job :pine/run/job)
                    (:fault :pine/run/fault) (:image :pine/run/image)
                    (:peer :pine/run/peer) (:watch :pine/run/watch)
                    (:command :pine/run/command) (:system :pine/run/system)
                    (:session :pine/run/session) (:mode :pine/mode)
                    (:sh :pine/host/shell) (:device :pine/host/device)
                    (:log :pine/fs/log)
                    (:compositor :pine/wm/compositor) (:tiles :pine/wm/tiles)))

(in-package :pine/test)

(def-suite :pine)

(defvar *booted* nil)

(defun booted ()
  "One actor system for the whole run. Every test that needs something to run
shares it, because a second one is a second image."
  (unless (actors:runningp) (actors:boot))
  (setf *booted* t))

(defmacro with-tree (&body body)
  "A fresh namespace for one test. The root is what a test is about; nothing is
carried over from the last one."
  `(let ((was (tree:root)))
     (unwind-protect (progn (tree:make-root) ,@body)
       (setf tree:*root* was))))

(defun somewhere (rows needle)
  (some (lambda (row) (search needle (car row))) rows))

(defun until (thunk &key (seconds 2))
  "Wait for something another thread is doing, and answer whether it happened."
  (loop :repeat (round (/ seconds 0.01))
        :when (funcall thunk) :do (return t)
        :do (sleep 0.01)))
