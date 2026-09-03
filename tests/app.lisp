(in-package :pine/test)

(def-suite* :pine/app :in :pine)

(defvar *app* nil)

(defun app ()
  "The example app, loaded the way pine's own are: an asdf system, by its name."
  (editing)
  (unless (system:named "notes") (pine:use :notes))
  (system:named "notes"))

(test an-app-is-a-system-like-any-other
  (is (not (null (app))))
  (is (not (null (tree:at "/system/notes"))))
  (is (member "notes" (mapcar #'job:name (system:systems)) :test #'equal)))

(test its-own-kind-of-node-is-a-place
  (app)
  (command:run "note" '("today" "it works"))
  (is (equal '("today") (node:contents (tree:at "/notes"))))
  (is (equal "it works" (node:contents (tree:at "/notes/today"))))
  (setf (node:contents (tree:at "/notes/today")) "written from a path")
  (is (equal "written from a path" (node:contents (tree:at "/notes/today"))))
  (is (eq (tree:at "/notes/today") (tree:at "/notes/today"))
      "the same node every time, so something can watch it"))

(test a-place-of-its-own-can-be-watched-like-anything-else
  (booted)
  (app)
  (command:run "note" '("watched" "before"))
  (let ((heard (cons nil nil)))
    (let ((w (watch:watch (tree:at "/notes/watched")
                          (lambda (of said)
                            (declare (ignore of))
                            (setf (car heard) said)))))
      (unwind-protect
           (progn
             (setf (node:contents (tree:at "/notes/watched")) "after")
             (is (until (lambda () (equal "after" (car heard))))))
        (watch:unwatch w)))))

(test its-own-mode-gives-its-text-structure
  (app)
  (let ((document (text:make-document "diary.note")))
    (setf (node:contents document)
          (format nil "* Today~%it works~%* Tomorrow~%it still does~%"))
    (setf (text:mode-of document) (mode:mode-for "diary.note"))
    (is (string-equal "notes"
                      (package-name
                       (symbol-package
                        (class-name (class-of (text:mode-of document))))))
        "the mode that claims the file is the app's own")
    (text:restructure document)
    (is (equal '("Today" "Tomorrow")
               (mapcar #'node:name (node:nodes (tree:at document "heading")))))
    (is (equal (format nil "* Today~%it works")
               (node:contents (tree:at document "heading/Today"))))
    (setf (node:contents (tree:at document "heading/Today"))
          (format nil "* Today~%it really works"))
    (is (search "it really works" (text:text document))
        "and writing one replaces that span")
    (text:kill "diary.note")))

(test its-own-role-says-where-its-surface-goes
  (app)
  (let* ((s (tree:at "/surface" "sticky"))
         (where (ui:anchor (ui:role s) 40 20)))
    (is (not (null s)))
    (is (equal '(:top :right) (ui:edges-of where)))
    (is (equal '(16 16 0 0) (ui:margin-of where)))
    (is (null (ui:shown s)) "and it waits to be asked for")))

(test its-surface-follows-what-it-read-and-crosses-the-wire
  (app)
  (command:run "note" '("zzz" "the last one written"))
  (let ((form (node:contents (tree:at "/surface/sticky/wire"))))
    (is (search "zzz" (princ-to-string form)))
    (is (typep (pine/ui:from-wire form) 'ui:column)))
  (command:run "note" '("zzzz" "later still"))
  (is (search "zzzz" (princ-to-string
                      (node:contents (tree:at "/surface/sticky/wire"))))
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
  (is (null (tree:at "/notes")))
  (is (null (tree:at "/surface" "sticky")))
  (is (null (command:named "note")))
  (is (null (tree:at "/system/notes"))))

(defvar *vcs* nil)

(defun vcs-app ()
  "The other example: an app that brings a device of its own."
  (editing)
  (unless (system:named "vcs") (pine:use :vcs))
  (system:named "vcs"))

(test an-app-can-bring-a-device-of-its-own
  "A device used to be a function PINE/HOST/DEVICE exported, so /dev was a closed
list and nothing anybody wrote could add to it. A declaration is a thing a package
that uses PINE/USER and nothing else can make."
  (vcs-app)
  (is (not (null (declared:named "vcs"))) "the app declared one")
  (is (not (null (tree:at "/dev/vcs"))) "and it stands in the namespace")
  (is (equal '("branch" "dirty" "head") (node:contents (tree:at "/dev/vcs")))
      "every reading either of its backings declares")
  (is (not (null (tree:at "/dev/vcs/branch")))
      "and each is a place, whichever backing this machine can use")
  (is (member :vcs-branch (pine/edit::sources))
      "and it brought a kind of question of its own, and the words that answer it")
  (is (not (null (command:named "switch-branch")))
      "asked for by a command that names that category"))

(test dropping-an-app-takes-its-device-with-it
  (vcs-app)
  (pine:drop :vcs)
  (setf *vcs* t)
  (is (null (system:named "vcs")))
  (is (null (tree:at "/dev/vcs")) "the device it put under /dev")
  (is (null (tree:at "/work")) "the place it put up")
  (is (null (tree:at "/surface" "board")) "its surface")
  (is (null (command:named "branch")) "its commands")
  (is (not (member :vcs-branch (pine/edit::sources)))
      "and the way it answered its own kind of question"))

(test everything-a-system-puts-up-is-taken-back
  "Six kinds of thing a system can contribute and one mechanism that takes all six
back. A surface, a chord and a node already went; a style key, a theme, a way of
answering a prompt and a device declaration were left standing, so dropping a system
half worked and nothing said which half."
  (with-tree
    (let ((home "pine/test/probe"))
      (let ((system:*owner* home))
        (ui:property :probe-key (lambda (props) (declare (ignore props)) nil))
        (pine/ui::register (make-instance 'pine/ui::theme :name :probe-theme))
        (edit:completes :probe-category
                        (lambda (&rest ignored) (declare (ignore ignored)) nil))
        (declared:defdevice %probe-owned :describes "declared while a system started")
        (mode:bind 'text "C-c C-probe" "help")
        (ui:make-surface "probe-surface" (lambda () (ui:label "hi")) :as 'ui:panel))

      (is (member :probe-key (ui:properties)) "the style key is there")
      (is (member :probe-theme (pine/ui::themes)) "the theme is there")
      (is (member :probe-category (pine/edit::sources)) "the prompt source is there")
      (is (not (null (declared:named "%probe-owned"))) "the declaration is there")
      (is (not (null (tree:at "/surface" "probe-surface"))) "the surface is there")
      (is (not (null (mode:binding (make-instance 'mode:text) "C-c C-probe")))
          "the chord is there")

      (pine/run/system::%take-down home)

      (is (not (member :probe-key (ui:properties))) "and the style key goes")
      (is (not (member :probe-theme (pine/ui::themes))) "and the theme goes")
      (is (not (member :probe-category (pine/edit::sources))) "and the source goes")
      (is (null (declared:named "%probe-owned")) "and the declaration goes")
      (is (null (tree:at "/surface" "probe-surface")) "and the surface goes")
      (is (null (mode:binding (make-instance 'mode:text) "C-c C-probe"))
          "and the chord goes"))))

(test a-surface-that-has-gone-leaves-no-closure-behind
  "What a widget meant crosses the wire as an id and stays behind it in *ACTS*.
Erasing the node was not the whole of taking a surface off: a click on an id of a
surface that has gone still ran what it used to mean."
  (with-tree
    (let ((home "pine/test/probe-acts"))
      (let ((system:*owner* home))
        (ui:make-surface "probe-acts" (lambda () (ui:label "hi")) :as 'ui:panel))
      (node:contents (tree:at "/surface/probe-acts/wire"))
      (flet ((held () (remove-if-not
                       (lambda (id) (eql 0 (search "probe-acts/" id)))
                       (d:keys (d:all pine/ui::*acts*)))))
        (pine/run/system::%take-down home)
        (is (null (held)) "its closures go with it")))))

(test a-word-on-the-command-line-is-all-of-it-or-none-of-it
  "READ-FROM-STRING answers with the first form and how far it got. Without the
second, pine write /x '1 2' read 1 and wrote it, losing the rest without saying so."
  (is (equal "1 2" (pine/cli::%value "1 2")))
  (is (equal "(a b) junk" (pine/cli::%value "(a b) junk")))
  (is (equal ":k junk" (pine/cli::%value ":k junk")))
  (is (= 42 (pine/cli::%value "42")) "one whole form is still that form")
  (is (eq :k (pine/cli::%value ":k")))
  (is (eq t (pine/cli::%value "t")))
  (is (equal "wide" (pine/cli::%value "wide")) "and a bare word is a word"))

(test nothing-answering-is-told-apart-from-not-answering
  "A timeout against a daemon that is up and busy read as no daemon at all, which
is the one thing a command line must not get wrong."
  (let ((nowhere "/tmp/pine-nobody-is-here.sock"))
    (ignore-errors (delete-file nowhere))
    (is (null (pine/cli::listeningp nowhere))
        "nothing is answering there")
    (is (null (pine/cli::%connect nowhere))
        "and asking for a connection answers nothing rather than breaking")))

