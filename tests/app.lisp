(in-package :pine/test)

(def-suite* :pine/app :in :pine)

(defvar *app* nil)

(defun app ()
  "The example app, loaded the way a daemon loads one: in PINE/USER, with pine's
own readtable. It is nothing but a file somebody wrote."
  (editing)
  (unless *app*
    (let ((*package* (pine:user-package))
          (*readtable* (named-readtables:find-readtable 'pine/fs/reader:syntax)))
      (load (merge-pathnames "examples/notes.lisp"
                             (asdf:system-source-directory :pine))))
    (setf *app* t))
  (unless (system:named "notes") (pine:use :notes))
  (system:named "notes"))

(test an-app-is-a-system-like-any-other
  (is (not (null (app))))
  (is (typep (app) 'system:system))
  (is (not (null (tree:at nil "system/notes"))))
  (is (member "notes" (mapcar #'job:name (system:systems)) :test #'equal)))

(test its-own-kind-of-node-is-a-place
  (app)
  (command:run "note" '("today" "it works"))
  (is (equal '("today") (node:contents (tree:at nil "notes"))))
  (is (equal "it works" (node:contents (tree:at nil "notes/today"))))
  (setf (node:contents (tree:at nil "notes/today")) "written from a path")
  (is (equal "written from a path" (node:contents (tree:at nil "notes/today"))))
  (is (eq (tree:at nil "notes/today") (tree:at nil "notes/today"))
      "the same node every time, so something can watch it"))

(test a-place-of-its-own-can-be-watched-like-anything-else
  (booted)
  (app)
  (command:run "note" '("watched" "before"))
  (let ((heard (d:box nil)))
    (let ((w (watch:watch (tree:at nil "notes/watched")
                          (lambda (of said)
                            (declare (ignore of))
                            (d:put! heard said)))))
      (unwind-protect
           (progn
             (setf (node:contents (tree:at nil "notes/watched")) "after")
             (is (until (lambda () (equal "after" (d:held heard))))))
        (watch:unwatch w)))))

(test its-own-mode-gives-its-text-structure
  (app)
  (let ((document (doc:make-document "diary.note")))
    (setf (node:contents document)
          (format nil "* Today~%it works~%* Tomorrow~%it still does~%"))
    (setf (doc:mode-of document) (mode:mode-for "diary.note"))
    (is (string-equal "notes"
                      (package-name
                       (symbol-package
                        (class-name (class-of (doc:mode-of document))))))
        "the mode that claims the file is the app's own")
    (doc:restructure document)
    (is (equal '("Today" "Tomorrow")
               (mapcar #'node:name (node:nodes (tree:at document "heading")))))
    (is (equal (format nil "* Today~%it works")
               (node:contents (tree:at document "heading/Today"))))
    (setf (node:contents (tree:at document "heading/Today"))
          (format nil "* Today~%it really works"))
    (is (search "it really works" (doc:text document))
        "and writing one replaces that span")
    (doc:kill "diary.note")))

(test its-own-role-says-where-its-surface-goes
  (app)
  (let* ((s (surface:named "sticky"))
         (where (surface:anchor (surface:role s) 40 20)))
    (is (not (null s)))
    (is (equal '(:top :right) (d:at where :edges)))
    (is (equal '(16 16 0 0) (d:at where :margin)))
    (is (null (surface:shown s)) "and it waits to be asked for")))

(test its-surface-follows-what-it-read-and-crosses-the-wire
  (app)
  (command:run "note" '("zzz" "the last one written"))
  (let ((form (node:contents (tree:at nil "surface/sticky/wire"))))
    (is (search "zzz" (princ-to-string form)))
    (is (typep (pine/ui/wire:from-wire form) 'widget:column)))
  (command:run "note" '("zzzz" "later still"))
  (is (search "zzzz" (princ-to-string
                      (node:contents (tree:at nil "surface/sticky/wire"))))
      "a write to its own node works its surface out again"))

(test its-own-chord-runs-its-own-command
  (app)
  (is (eq (command:named "note")
          (mode:binding (make-instance 'mode:lisp) "C-c n"))
      "bound on text, in force in lisp, because that is what inheritance is"))

(test dropping-it-takes-everything-it-put-there-with-it
  (app)
  (pine:drop :notes)
  (setf *app* t)
  (is (null (system:named "notes")))
  (is (null (tree:at nil "notes")))
  (is (null (surface:named "sticky")))
  (is (null (command:named "note")))
  (is (null (tree:at nil "system/notes"))))

(test nothing-pine-ships-names-this-app
  "The claim is that an app is the editor's equal. This is the proof: the editor
is under src/, this is not, and src/ has never heard of it."
  (let ((named (loop :for f :in (directory
                                 (merge-pathnames
                                  "src/**/*.lisp"
                                  (asdf:system-source-directory :pine)))
                     :unless (char= #\. (char (file-namestring f) 0))
                       :when (let ((said (uiop:read-file-string f)))
                               (or (search "notes:" said) (search "sticky" said)
                                   (search "(use :notes)" said)))
                         :collect (file-namestring f))))
    (is (null named) "~{~%  ~a names it~}" named)))
