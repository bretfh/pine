(in-package :pine/test)

(def-suite* :pine/app :in :pine)

(defvar *app* nil)

(defun app ()
  "The example app, loaded the way pine's own are: an asdf system, by its name."
  (editing)
  (unless (module:named "notes") (pine:use :notes))
  (module:named "notes"))

(test an-app-is-a-system-like-any-other
  (is (not (null (app))))
  (is (not (null (fs:at "/proc/notes"))))
  (is (member "notes" (mapcar #'job:name (module:modules)) :test #'equal)))

(test its-own-kind-of-node-is-a-place
  (app)
  (command:run "note" '("today" "it works"))
  (is (equal '("today") (fs:contents (fs:at "/notes"))))
  (is (equal "it works" (fs:contents (fs:at "/notes/today"))))
  (setf (fs:contents (fs:at "/notes/today")) "written from a path")
  (is (equal "written from a path" (fs:contents (fs:at "/notes/today"))))
  (is (eq (fs:at "/notes/today") (fs:at "/notes/today"))
      "the same node every time, so something can watch it"))

(test a-place-of-its-own-can-be-watched-like-anything-else
  (booted)
  (app)
  (command:run "note" '("watched" "before"))
  (let ((heard (cons nil nil)))
    (let ((w (watch:watch (fs:at "/notes/watched")
                          (lambda (of said)
                            (declare (ignore of))
                            (setf (car heard) said)))))
      (unwind-protect
           (progn
             (setf (fs:contents (fs:at "/notes/watched")) "after")
             (is (until (lambda () (equal "after" (car heard))))))
        (watch:unwatch w)))))

(test its-own-mode-gives-its-text-structure
  (app)
  (let ((buffer (text:make-buffer "diary.note")))
    (setf (text:text buffer)
          (format nil "* Today~%it works~%* Tomorrow~%it still does~%"))
    (setf (text:mode-of buffer) (mode:mode-for "diary.note"))
    (is (string-equal "notes"
                      (package-name
                       (symbol-package
                        (class-name (class-of (text:mode-of buffer))))))
        "the mode that claims the file is the app's own")
    (text:restructure buffer)
    (is (equal '("Today" "Tomorrow")
               (mapcar #'fs:name (text:regions (fs:at buffer "heading")))))
    (is (equal (format nil "* Today~%it works")
               (fs:contents (fs:at buffer "heading/Today/text"))))
    (setf (fs:contents (fs:at buffer "heading/Today/text"))
          (format nil "* Today~%it really works"))
    (is (search "it really works" (text:text buffer))
        "and writing one replaces that span")
    (text:kill "diary.note")))

(test its-own-role-says-where-its-surface-goes
  (app)
  (let* ((s (fs:at "/ui/surface" "sticky"))
         (where (ui:anchor (ui:role s) 40 20)))
    (is (not (null s)))
    (is (equal '(:top :right) (ui:edges-of where)))
    (is (equal '(16 16 0 0) (ui:margin-of where)))
    (is (null (ui:shown s)) "and it waits to be asked for")))

(test its-surface-follows-what-it-read-and-crosses-the-wire
  (app)
  (command:run "note" '("zzz" "the last one written"))
  (let ((form (fs:contents (fs:at "/ui/surface/sticky/wire"))))
    (is (search "zzz" (princ-to-string form)))
    (is (typep (pine/ui:from-wire form) 'ui:column)))
  (command:run "note" '("zzzz" "later still"))
  (is (search "zzzz" (princ-to-string
                      (fs:contents (fs:at "/ui/surface/sticky/wire"))))
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
  (is (null (module:named "notes")))
  (is (null (fs:at "/notes")))
  (is (null (fs:at "/ui/surface" "sticky")))
  (is (null (command:named "note")))
  (is (null (fs:at "/proc/notes"))))

(defvar *vcs* nil)

(defun vcs-app ()
  "The other example: an app that brings a device of its own."
  (editing)
  (unless (module:named "vcs") (pine:use :vcs))
  (module:named "vcs"))

(test an-app-can-bring-a-device-of-its-own
  "A device used to be a function PINE/HOST/DEVICE exported, so /dev was a closed
list and nothing anybody wrote could add to it. A declaration is a thing a package
that uses PINE/USER and nothing else can make."
  (vcs-app)
  (is (not (null (host::device-class "vcs"))) "the app declared one")
  (is (not (null (fs:at "/dev/vcs"))) "and it stands in the namespace")
  (is (equal '("branch" "dirty" "head") (fs:contents (fs:at "/dev/vcs")))
      "every reading either of its backings declares")
  (is (not (null (fs:at "/dev/vcs/branch")))
      "and each is a place, whichever backing this machine can use")
  (is (member :vcs-branch (pine/edit::sources))
      "and it brought a kind of question of its own, and the words that answer it")
  (is (not (null (command:named "switch-branch")))
      "asked for by a command that names that category"))

(test dropping-an-app-takes-its-device-with-it
  (vcs-app)
  (pine:drop :vcs)
  (setf *vcs* t)
  (is (null (module:named "vcs")))
  (is (null (fs:at "/dev/vcs")) "the device it put under /dev")
  (is (null (fs:at "/work")) "the place it put up")
  (is (null (fs:at "/ui/surface" "board")) "its surface")
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
      (let ((fs:*owner* home))
        (ui:property :probe-key (lambda (props) (declare (ignore props)) nil))
        (pine/ui::build :probe-theme nil nil nil)
        (edit:completes :probe-category
                        (lambda (&rest ignored) (declare (ignore ignored)) nil))
        (host:defdevice %probe-owned :describes "declared while a system started")
        (mode:bind 'text "C-c C-probe" "help")
        (ui:make-surface "probe-surface" (lambda () (ui:label "hi")) :as 'ui:panel))

      (is (member :probe-key (ui:properties)) "the style key is there")
      (is (member :probe-theme (pine/ui::themes)) "the theme is there")
      (is (member :probe-category (pine/edit::sources)) "the prompt source is there")
      (is (not (null (host::device-class "%probe-owned"))) "the declaration is there")
      (is (not (null (fs:at "/ui/surface" "probe-surface"))) "the surface is there")
      (is (not (null (mode:binding (make-instance 'mode:text) "C-c C-probe")))
          "the chord is there")

      (pine/run/module::%take-down home)

      (is (not (member :probe-key (ui:properties))) "and the style key goes")
      (is (not (member :probe-theme (pine/ui::themes))) "and the theme goes")
      (is (not (member :probe-category (pine/edit::sources))) "and the source goes")
      (is (not (null (host::device-class "%probe-owned")))
          "the declaration stays: a class was loaded, not put up")
      (is (null (fs:at "/ui/surface" "probe-surface")) "and the surface goes")
      (is (null (mode:binding (make-instance 'mode:text) "C-c C-probe"))
          "and the chord goes"))))

(test a-surface-that-has-gone-leaves-no-closure-behind
  "What a widget meant crosses the wire as an id and stays on the surface under
it. A on-click on an id of a surface that has gone runs nothing."
  (with-tree
    (let ((home "pine/test/probe-acts")
          (ran nil))
      (let ((fs:*owner* home))
        (ui:make-surface "probe-acts"
                         (lambda () (ui:button :on-click (lambda () (setf ran t))
                                               (ui:label "hi")))
                         :as 'ui:panel))
      (let ((id (first (d:keys (pine/ui::acts (fs:at "/ui/surface" "probe-acts"))))))
        (fs:contents (fs:at "/ui/surface/probe-acts/wire"))
        (let ((id (or id (first (d:keys (pine/ui::acts (fs:at "/ui/surface" "probe-acts")))))))
          (is (not (null id)) "the on-click crossed as an id")
          (pine/run/module::%take-down home)
          (is (null (fs:at "/ui/surface" "probe-acts")) "the surface goes")
          (pine/ui::act "probe-acts" (list id))
          (is (null ran) "and its closures went with it"))))))

(test a-word-on-the-command-line-is-all-of-it-or-none-of-it
  "READ-FROM-STRING answers with the first form and how far it got. Without the
second, pine write /x '1 2' read 1 and wrote it, losing the rest without saying so."
  (is (equal "1 2" (pine/cli::%value "1 2")))
  (is (equal "(a b) junk" (pine/cli::%value "(a b) junk")))
  (is (equal ":k junk" (pine/cli::%value ":k junk")))
  (is (= 42 (pine/cli::%value "42")) "one whole form is still that form")
  (is (eq :k (pine/cli::%value ":k")))
  (is (eq t (pine/cli::%value "t")))
  (is (equal "width" (pine/cli::%value "width")) "and a bare word is a word"))

(test nothing-answering-is-told-apart-from-not-answering
  "A timeout against a daemon that is up and busy read as no daemon at all, which
is the one thing a command line must not get wrong."
  (let ((nowhere "/tmp/pine-nobody-is-here.sock"))
    (ignore-errors (delete-file nowhere))
    (is (null (pine/cli::listeningp nowhere))
        "nothing is answering there")
    (is (null (pine/cli::%connect nowhere))
        "and asking for a connection answers nothing rather than breaking")))

