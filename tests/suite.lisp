(defpackage :pine/test
  (:use :cl :fiveam)
  (:local-nicknames (:d :pine/data) (:node :pine/fs/node) (:tree :pine/fs/tree)
                    (:mount :pine/fs/mount) (:store :pine/fs/store)
                    (:path :pine/fs/path)
                    (:actors :pine/run/actors) (:job :pine/run/job)
                    (:fault :pine/run/fault) (:image :pine/run/image)
                    (:peer :pine/run/peer) (:watch :pine/run/watch)
                    (:command :pine/run/command) (:system :pine/run/system)
                    (:session :pine/run/session)
                    (:widget :pine/ui/widget) (:build :pine/ui/build)
                    (:grid :pine/ui/grid) (:layout :pine/ui/layout)
                    (:style :pine/ui/style) (:face :pine/ui/face)
                    (:surface :pine/ui/surface) (:key :pine/ui/key)
                    (:lines :pine/text/lines) (:mode :pine/mode)
                    (:doc :pine/text/document) (:text :pine/text)
                    (:sh :pine/host/shell) (:device :pine/host/device)
                    (:window :pine/edit/window) (:render :pine/edit/render)
                    (:prompt :pine/edit/prompt) (:match :pine/edit/matching)
                    (:listing :pine/edit/listing)
                    (:isearch :pine/edit/isearch) (:keys :pine/edit/keys)
                    (:emode :pine/edit/mode)
                    (:log :pine/run/log) (:parser :pine/text/ts/parser)
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
