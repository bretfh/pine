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
  (let* ((s (ui:named "sticky"))
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
  (is (null (ui:named "sticky")))
  (is (null (command:named "note")))
  (is (null (tree:at "/system/notes"))))

(defvar *vcs* nil)

(defun vcs-app ()
  "The other example: an app that brings a device of its own. Loaded the way a
daemon loads one, so a file nothing compiles is still a file the suite reads."
  (editing)
  (unless *vcs*
    (let ((*package* (pine:user-package))
          (*readtable* (named-readtables:find-readtable 'pine/fs/reader:syntax)))
      (load (merge-pathnames "examples/vcs.lisp"
                             (asdf:system-source-directory :pine))))
    (setf *vcs* t))
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
      "and each is a place, whichever backing this machine can use"))

(test dropping-an-app-takes-its-device-with-it
  (vcs-app)
  (pine:drop :vcs)
  (setf *vcs* t)
  (is (null (system:named "vcs")))
  (is (null (tree:at "/dev/vcs")) "the device it put under /dev")
  (is (null (tree:at "/work")) "the place it put up")
  (is (null (ui:named "board")) "its surface")
  (is (null (command:named "branch")) "and its commands"))

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

(test the-language-is-a-package-and-not-a-registry
  "PINE/USER is a DEFPACKAGE, so its words are symbols the compiler resolved when
the file was built. A name misspelled there is a build that fails, where a name
misspelled in a list of strings was a word that quietly was not in the language.

What a system loaded later brings is its own vocabulary, a package that uses
nothing and imports what it offers, put here by SPEAKS as it loads."
  (let ((p (find-package '#:pine/user)))
    (is (not (null p)) "the language is a package that exists")
    (dolist (said '("SWAP" "CAS" "READ" "WRITE" "NODE" "DEFCOMMAND" "DEFSURFACE"
                    "NIL" "T" "LAMBDA" "DEFUN"))
      (is (eq :external (nth-value 1 (find-symbol said p)))
          "~a is something a user program can say" said))
    (dolist (said '("DOCUMENT" "WINDOWS" "DEVICE" "ARRANGE"))
      (is (eq :external (nth-value 1 (find-symbol said p)))
          "~a came with the system that offers it" said))
    (let ((words (let ((n 0)) (do-external-symbols (s p) (declare (ignore s)) (incf n)) n))
          (cl (let ((n 0)) (do-external-symbols (s :cl) (declare (ignore s)) (incf n)) n)))
      (is (< (- words cl) 220)
          "~d words of pine's own: a language, not a grab bag" (- words cl)))))

(test two-vocabularies-cannot-claim-one-word
  "USE-PACKAGE says so, naming both symbols, at the point of conflict. The list of
sentences that used to be collected instead was a second value nobody read."
  (let ((p (find-package '#:pine/user)))
    (make-package '#:pine/test/clash :use nil)
    (unwind-protect
         (progn
           (export (list (intern "LABEL" '#:pine/test/clash)) '#:pine/test/clash)
           (signals package-error (use-package '#:pine/test/clash p)))
      (delete-package '#:pine/test/clash))
    (is (eq (find-symbol "READ" p) (find-symbol "READ" '#:pine))
        "READ is shadowed on purpose, so it is pine's and never in question")))

(test a-system-pine-ships-is-written-the-way-one-you-write-is
  "PINE/DESK uses PINE/USER and nothing else, says what it reads and writes by
path, and names no package of pine's. It is the same claim NOTHING-PINE-SHIPS-
NAMES-THIS-APP makes from the other side: if the desktop cannot be written in the
language, the language is not one."
  (let* ((file (merge-pathnames "src/desk/system.lisp"
                                (asdf:system-source-directory :pine)))
         (said (uiop:read-file-string file))
         (named (loop :for line :in (uiop:split-string said :separator '(#\Newline))
                      :when (and (search "pine/" line)
                                 (not (search "#:pine/desk" line))
                                 (not (search "#:pine/user" line))
                                 (not (search "in-readtable" line)))
                        :collect line)))
    (is (null named) "~{~%  ~a~}" named)
    (is (search "/dev/audio/volume" said) "it says what it reads by path")
    (is (null (search "local-nicknames" said)))))

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

(test a-client-names-no-actor-system
  "What a client of pine needs is what any language has: a socket and a line of
text. Naming the concurrency library here is what made every client a lisp."
  (let ((source (uiop:read-file-string
                 (merge-pathnames "src/cli.lisp"
                                  (asdf:system-source-directory :pine)))))
    (is (null (search "sento" source))
        "cli.lisp names sento, so a client still has to be one")))
