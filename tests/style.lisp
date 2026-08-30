(in-package :pine/test)

(def-suite* :pine/style :in :pine)

(defparameter +modules+ '("fs" "run" "serve" "ui" "text" "text/ts" "text/ts/lang"
                          "host" "edit" "term" "wm" "desk" "paint" "wayland")
  "Every directory of pine's own source. What is not here is not pine's: the
wayland protocol bindings are generated from the compositor's own xml, and vt is a
terminal emulator that depends on cffi and nothing of pine's.

A directory left out of this list is one every rule below silently skips, which is
worse than having no rule.")

(defparameter +line-limit+ 400)

(defparameter +definers+
  '("defun" "defmacro" "defclass" "defmethod" "defgeneric" "define-condition"
    "defstruct" "deftype" "defsetf"))

(defparameter +substrate+ '("data.lisp" "commit.lisp" "node.lisp" "tree.lisp"
                            "path.lisp" "reader.lisp" "mount.lisp" "store.lisp")
  "What pine's own syntax is built out of. These may not read in the reader they
are: everything else may, because pine's language is for pine too.")

(defparameter +builds-fresh+ '("job.lisp" "system.lisp" "managed.lisp")
  "The files whose NODES or RESOLVE may answer without NODE:CHILD, because what
they answer is already the same object every time: a job is a node itself, so is a
system, and a managed compositor makes its own places once and holds them. A name
added here for any other reason is a subtree taken out of the graph.")

(defparameter +generated+ '("pine/wayland/protocol")
  "Packages a file does not get to name for itself: the wayland protocol
bindings are four files generated from xml, and one package between them.")

(defparameter +waits-on-a-clock+ '("cli.lisp" "boot.lisp")
  "The two places a clock is the right thing to wait on. cli.lisp waits for a
socket a separate process has not opened yet, so there is nothing here to be woken
by. boot.lisp's are single delays on the way out, not a loop.")

(defparameter +identity-tables+ '("tree.lisp")
  "Where a hash table is identity and not a registry.

tree.lisp keeps what a name spells against the string itself, by EQ and weakly on
the key. It is not a registry: nothing is looked up by what it says, an entry
going is no loss because the name is simply cut again, and the whole point is the
identity of the string a compiled call site hands over. A map in a box would be
keyed by what the string says, which is the wrong question, and would hold every
name ever built for the occasion for as long as the image runs.")

(defun %module-root ()
  (merge-pathnames "src/" (asdf:system-source-directory :pine)))

(defun %lang-files ()
  (directory (merge-pathnames "text/ts/lang/*.lisp" (%module-root))))

(defun %files ()
  (remove-if (lambda (f) (char= #\. (char (file-namestring f) 0)))
             (append (list (merge-pathnames "boot.lisp" (%module-root))
                           (merge-pathnames "cli.lisp" (%module-root)))
                     (loop :for module :in +modules+
                           :append (directory
                                    (merge-pathnames (format nil "~a/*.lisp" module)
                                                     (%module-root))))
                     (%lang-files))))

(defun %lines (file)
  (with-open-file (in file)
    (loop :for line = (read-line in nil nil) :while line :collect line)))

(defun %commented-lines (file)
  (let ((in-string nil) (found nil) (n 0))
    (dolist (line (%lines file) (nreverse found))
      (incf n)
      (loop :with i := 0
            :while (< i (length line))
            :for ch := (char line i)
            :do (cond ((char= ch #\\) (incf i 2))
                      ((and (char= ch #\#) (< (1+ i) (length line))
                            (char= (char line (1+ i)) #\\))
                       (incf i 3))
                      ((char= ch #\") (setf in-string (not in-string)) (incf i))
                      ((and (char= ch #\;) (not in-string))
                       (push n found)
                       (return))
                      (t (incf i)))))))

(defun %starts-with-any (line words)
  (let ((trimmed (string-left-trim " " line)))
    (and (plusp (length trimmed))
         (char= #\( (char trimmed 0))
         (some (lambda (w)
                 (let ((head (concatenate 'string "(" w " ")))
                   (and (>= (length trimmed) (length head))
                        (string-equal head trimmed :end2 (length head)))))
               words))))

(defun %naming (what &key (except nil))
  "The files that name WHAT, other than the ones allowed to."
  (loop :for file :in (%files)
        :when (and (find-if (lambda (line) (search what line)) (%lines file))
                   (not (member (file-namestring file) except :test #'equal)))
          :collect (file-namestring file)))

(test code-carries-no-commentary
  "What a file says about itself it says in a docstring. A comment is a note to
whoever was here last, and it goes out of date without anything failing."
  (let ((found nil))
    (dolist (file (%files))
      (dolist (n (%commented-lines file))
        (push (format nil "~a:~d" (file-namestring file) n) found)))
    (is (null found) "~d commented line~:p:~{~%  ~a~}"
        (length found) (reverse found))))

(test every-global-is-in-the-block-at-the-top
  (let ((loose nil))
    (dolist (file (%files))
      (let ((lines (%lines file))
            (first-definition nil)
            (last-global nil))
        (loop :for line :in lines
              :for n :from 1
              :do (when (and (null first-definition)
                             (%starts-with-any line +definers+))
                    (setf first-definition n))
                  (when (%starts-with-any line '("defvar" "defparameter"
                                                 "defconstant"))
                    (setf last-global n)))
        (when (and first-definition last-global (> last-global first-definition))
          (push (format nil "~a: a global at line ~d, below a definition at ~d"
                        (file-namestring file) last-global first-definition)
                loose))))
    (is (null loose) "~{~%  ~a~}" (reverse loose))))

(test no-file-is-longer-than-one-idea
  (let ((long nil))
    (dolist (file (%files))
      (let ((n (length (%lines file))))
        (when (> n +line-limit+)
          (push (format nil "~a: ~d lines" (file-namestring file) n) long))))
    (is (null long) "~d file~:p over ~d lines:~{~%  ~a~}"
        (length long) +line-limit+ (reverse long))))

(test the-substrate-does-not-read-in-its-own-sugar
  "/a/b, {...} and [...] are pine's own syntax, and pine writes in it. What may not
is the part that defines it: a file under fs/ or data.lisp reading in the reader it
is building is a chicken asking to be its own egg.

Everything above that may. A system pine ships and a system somebody writes are
the same kind of thing, written the same way, or the claim is not true."
  (let ((using nil))
    (dolist (file (%files))
      (when (and (find-if (lambda (line)
                            (search "named-readtables:in-readtable" line))
                          (%lines file))
                 (member (file-namestring file) +substrate+ :test #'equal))
        (push (file-namestring file) using)))
    (is (null using) "~{~%  ~a declares a readtable~}" (reverse using))))

(test only-data-knows-what-a-value-is-kept-in
  "fset is what a value is and a compare-and-swap is how one is replaced. Both are
behind pine/data, so what pine holds is one idea in one file."
  (is (null (%naming "fset:" :except '("data.lisp")))
      "~{~%  ~a names fset~}" (%naming "fset:" :except '("data.lisp")))
  (is (null (%naming "sb-ext:cas" :except '("data.lisp")))
      "~{~%  ~a swaps a value for itself~}"
      (%naming "sb-ext:cas" :except '("data.lisp"))))

(test nothing-sleeps-in-a-loop-to-repeat
  "One clock: the actor system's wheel, and pine/run/actors over it."
  (is (null (%naming "schedule-recurring" :except '("actors.lisp")))
      "~{~%  ~a schedules its own repeat~}"
      (%naming "schedule-recurring" :except '("actors.lisp"))))

(test a-thread-is-made-only-where-something-blocks
  "A pipe read and a child's output block. Everything else is an actor or a tick,
so it makes no thread."
  (is (null (%naming "make-thread" :except '("actors.lisp" "job.lisp")))
      "~{~%  ~a makes a thread of its own~}"
      (%naming "make-thread" :except '("actors.lisp" "job.lisp"))))

(test an-actor-is-sentos-and-not-one-of-our-own
  "actor-of is what makes one, and pine/run/job is where that is said."
  (is (null (%naming "actor-context:actor-of" :except '("job.lisp" "cli.lisp")))
      "~{~%  ~a makes an actor of its own~}"
      (%naming "actor-context:actor-of" :except '("job.lisp" "cli.lisp"))))

(test a-registry-is-a-table-and-not-a-hash-table
  "A hash table is for identity, or for one call's own scratch. Anything two
threads read is a map in a box."
  (let ((loose nil))
    (dolist (file (%files))
      (dolist (line (%lines file))
        (when (and (search "(defvar *" line) (search "make-hash-table" line)
                   (not (member (file-namestring file) +identity-tables+
                                :test #'equal)))
          (push (file-namestring file) loose))))
    (is (null loose) "~{~%  ~a keeps a registry in a hash table~}"
        (reverse loose))))

(test a-node-hands-out-the-same-child-every-time
  "A node built fresh per call cannot be depended on, so nothing reading it can
ever be worked out again: a surface reading /face/keyword would never hear that the
face moved. NODE:CHILD is what makes the child the same object twice."
  (let ((loose nil))
    (dolist (file (%files))
      (let ((lines (%lines file)))
        (flet ((says (what)
                 (find-if (lambda (line) (search what line)) lines)))
          (when (and (or (says "(defmethod node:nodes")
                         (says "(defmethod node:resolve"))
                     (says "make-instance")
                     (not (says "node:child"))
                     (not (member (file-namestring file) +builds-fresh+
                                  :test #'equal)))
            (push (file-namestring file) loose)))))
    (is (null loose) "~{~%  ~a builds a child without node:child~}"
        (reverse loose))))

(defun %package-of (file)
  "The package FILE declares, as it is written."
  (let ((line (find-if (lambda (l) (search "(defpackage" l)) (%lines file))))
    (when line
      (let* ((at (+ (search "#:" line) 2))
             (end (or (position-if (lambda (c) (member c '(#\Space #\) #\Tab)))
                                   line :start at)
                      (length line))))
        (subseq line at end)))))

(test a-package-is-its-path
  "src/edit/window.lisp declares pine/edit/window. The systems are already spelled
that way; a package that is not is a name you have to translate. The root is pine,
and a directory's own system file is that directory."
  (let ((wrong nil))
    (dolist (file (%files))
      (let ((said (%package-of file))
            (path (namestring file)))
        (when said
          (let* ((from (search "src/" path))
                 (under (and from (subseq path (+ from 4))))
                 (want (and under
                            (concatenate 'string "pine/"
                                         (subseq under 0 (- (length under) 5)))))
                 (directory (and want
                                 (let ((slash (position #\/ want :from-end t)))
                                   (and slash (subseq want 0 slash))))))
            (when (and want (not (equal said want))
                       (not (equal said "pine"))
                       (not (member said +generated+ :test #'equal))
                       (not (and (equal (file-namestring file) "system.lisp")
                                 (equal said directory))))
              (push (format nil "~a says ~a" (file-namestring file) said)
                    wrong))))))
    (is (null wrong) "~{~%  ~a~}" (reverse wrong))))

(defun %dotted-package (line)
  "Whether LINE spells a package with a dot rather than a slash. A sentence that
ends in the word is not one, and neither is a file called pine.something."
  (let ((at (search "pine." line)))
    (and at (< (+ at 5) (length line))
         (alpha-char-p (char line (+ at 5)))
         (or (zerop at) (not (char= #\/ (char line (1- at))))))))

(test nothing-spells-a-package-with-a-dot
  (let ((found (loop :for file :in (%files)
                     :when (and (find-if #'%dotted-package (%lines file))
                                (not (equal "style.lisp" (file-namestring file))))
                       :collect (file-namestring file))))
    (is (null found) "~{~%  ~a names a package with a dot~}" found)))

(test in-package-is-the-line-after-the-defpackage
  (let ((loose nil))
    (dolist (file (%files))
      (let ((lines (%lines file))
            (depth 0) (start nil) (end nil) (at nil))
        (loop :for line :in lines
              :for n :from 0
              :do (when (and (null start) (search "(defpackage" line))
                    (setf start n))
                  (when (and start (null end))
                    (incf depth (count #\( line))
                    (decf depth (count #\) line))
                    (when (<= depth 0) (setf end n)))
                  (when (and (null at) (search "(in-package" line)) (setf at n)))
        (when (and end at (/= at (1+ end)))
          (push (file-namestring file) loose))))
    (is (null loose) "~{~%  ~a puts something between defpackage and in-package~}"
        (reverse loose))))

(defun %nicknames ()
  "Every (NICKNAME . PACKAGE) pair any file declares."
  (let (out)
    (dolist (file (%files) (nreverse out))
      (dolist (line (%lines file))
        (let ((at 0))
          (loop
            (let ((open (search "(#:" line :start2 at)))
              (unless open (return))
              (let* ((rest (subseq line (+ open 3)))
                     (space (position #\Space rest))
                     (name (and space (subseq rest 0 space)))
                     (tail (and space (string-left-trim " "
                                                        (subseq rest (1+ space))))))
                (when (and name tail (> (length tail) 2)
                           (string= "#:" tail :end2 2))
                  (let* ((to (or (position-if (lambda (c)
                                                (member c '(#\Space #\))))
                                              tail :start 2)
                                 (length tail)))
                         (package (subseq tail 2 to)))
                    (when (and (>= (length package) 4)
                               (string= "pine" package :end2 4))
                      (push (cons name package) out)))))
              (setf at (+ open 3)))))))))

(defun %two-ways (of by)
  "What is named twice: OF of each pair against BY of it."
  (let ((seen (make-hash-table :test 'equal))
        (clashes nil))
    (dolist (pair (%nicknames) (reverse clashes))
      (let* ((one (funcall of pair))
             (other (funcall by pair))
             (had (gethash one seen)))
        (cond ((null had) (setf (gethash one seen) other))
              ((not (equal had other))
               (pushnew (format nil "~a is ~a and ~a" one had other)
                        clashes :test #'equal)))))))

(test a-nickname-names-one-package
  "A nickname is vocabulary. It has to mean the same thing in every file, or
reading one means checking its header first."
  (let ((clashes (%two-ways #'car #'cdr)))
    (is (null clashes) "~{~%  ~a~}" clashes)))

(test a-package-answers-to-one-nickname
  "And the other way round. Two names for one package is two vocabularies for
one thing: a file that says both reads as though it used two, and a rename that
stopped half way is what leaves it that way."
  (let ((clashes (%two-ways #'cdr #'car)))
    (is (null clashes) "~{~%  ~a~}" clashes)))

(test nothing-in-the-substrate-names-what-is-loaded-on-it
  "The editor, the desktop, the window manager and the machine's own devices are
systems. A substrate that names one of them has a favourite."
  (let ((loose nil))
    (dolist (module '("fs" "run" "ui"))
      (dolist (file (directory (merge-pathnames (format nil "~a/*.lisp" module)
                                                (%module-root))))
        (dolist (name '("pine/edit" "pine/desk" "pine/wm" "pine/host" "pine/text"))
          (when (find-if (lambda (line) (search name line)) (%lines file))
            (pushnew (format nil "~a names ~a" (file-namestring file) name)
                     loose :test #'equal)))))
    (is (null loose) "~{~%  ~a~}" (reverse loose))))

(defparameter +claimed-twice+ 183
  "How many exports would have to go for every name to belong to one package: for
each name two or more claim, one fewer than the number claiming it.

Not the count of such names, which said the same thing about ATTACH across
fourteen packages as about a word two share. This counts the weight.

This number goes down. It does not go up.")

(defun %exported ()
  "Every exported name under src/, and the packages that export it."
  (let ((out (make-hash-table :test 'equal)))
    (dolist (file (%files) out)
      (let ((said (uiop:read-file-string file)) (home nil) (in-export nil))
        (dolist (line (uiop:split-string said :separator '(#\Newline)))
          (let ((at (search "(defpackage #:" line)))
            (when at (setf home (subseq line (+ at 14)
                                        (position-if
                                         (lambda (c) (member c '(#\Space #\))))
                                         line :start (+ at 14))))))
          (when (search "(:export" line) (setf in-export t))
          (when (search "(in-package" line) (setf in-export nil))
          (when (and in-export home)
            (let ((from 0))
              (loop
                (let ((at (search "#:" line :start2 from)))
                  (unless at (return))
                  (let* ((start (+ at 2))
                         (end (or (position-if
                                   (lambda (c) (member c '(#\Space #\))))
                                   line :start start)
                                  (length line)))
                         (word (string-downcase (subseq line start end))))
                    (pushnew home (gethash word out) :test #'equal)
                    (setf from end)))))))))))

(test a-word-belongs-to-one-package
  "Two packages exporting one name is two things called the same thing. Only one
of them can be in the language a config is written in, and reading either means
knowing which is meant."
  (let* ((twice (loop :for word :being :the :hash-keys :of (%exported)
                        :using (:hash-value packages)
                      :when (rest packages) :collect (cons word packages)))
         (weight (reduce #'+ twice :key (lambda (e) (1- (length (cdr e)))))))
    (is (<= weight +claimed-twice+)
        "~d exports too many across ~d names, was ~d:~{~%  ~a~}"
        weight (length twice) +claimed-twice+
        (mapcar (lambda (each)
                  (format nil "~a: ~{~a~^ ~}" (car each) (cdr each)))
                (subseq (sort twice #'> :key (lambda (e) (length (cdr e))))
                        0 (min 12 (length twice)))))))

(test nothing-swallows-a-fault-without-saying-why
  "IGNORE-ERRORS says nothing about what was expected to go wrong, so a real fault
being lost looks exactly like a question with no answer. FAULT:ATTEMPT keeps what
broke; FAULT:OR-NOTHING says in words why nothing is an answer here."
  (is (null (%naming "ignore-errors"))
      "~{~%  ~a swallows without saying why~}" (%naming "ignore-errors")))

(test nothing-waits-by-looking-again
  "A thread waits on the thing it is waiting for: a descriptor, a join, a fault, a
mailbox. Looking and sleeping and looking again is a thread awake to find out that
nothing has happened, and a timeout that counts turns rather than seconds."
  (is (null (%naming "(sleep " :except +waits-on-a-clock+))
      "~{~%  ~a waits by looking again~}"
      (%naming "(sleep " :except +waits-on-a-clock+)))

(defparameter +layers+
  '(("pine/value" ("word.lisp" "data.lisp" "said.lisp")
     ("pine/data" "pine/said"))
    ("pine/place" ("fs/")                       ("pine/fs"))
    ("pine/run"   ("run/")                      ("pine/run"))
    ("pine/ui"    ("ui/")                       ("pine/ui"))
    ("pine"       ("mode.lisp" "boot.lisp" "serve/" "cli.lisp")
     ("pine/mode" "pine" "pine/serve")))
  "The layers pine is built out of, lowest first: which files are in each, and
which packages belong to it. Each is an asdf system that loads on its own, and
each names only what is under it.

That is the whole claim of a bottom-up program: not that the files sit in
directories, but that every layer is a language you could write something else in
without dragging the ones above it along.")

(defun %layer-at (name)
  "Which layer a file path or a package name belongs to, as its depth, or NIL for
what is loaded on top of them all."
  (loop :for (nil files packages) :in +layers+
        :for depth :from 0
        :when (or (some (lambda (f) (search (concatenate 'string "src/" f) name))
                        files)
                  (some (lambda (p)
                          (or (equal name p)
                              (and (> (length name) (length p))
                                   (string= p name :end2 (length p))
                                   (char= #\/ (char name (length p))))))
                        packages))
          :do (return depth)))

(test a-layer-names-only-what-is-under-it
  "A layer that reaches upward is not a layer. PINE/PLACE naming PINE/RUN means you
cannot have the namespace without the job system, and then it was never a language
of its own -- it was the bottom half of one program."
  (let ((wrong nil))
    (dolist (file (%files))
      (let ((mine (%layer-at (namestring file))))
        (when mine
          (dolist (line (%lines file))
            (let ((at 0))
              (loop
                (let ((found (search "#:pine/" line :start2 at)))
                  (unless found (return))
                  (let* ((from (+ found 2))
                         (to (or (position-if (lambda (c) (member c '(#\Space #\))))
                                              line :start from)
                                 (length line)))
                         (named (subseq line from to))
                         (theirs (%layer-at named)))
                    (when (or (null theirs) (> theirs mine))
                      (pushnew (format nil "~a is in layer ~d and names ~a"
                                       (file-namestring file) mine named)
                               wrong :test #'equal)))
                  (setf at (+ found 7)))))))))
    (is (null wrong) "~{~%  ~a~}" (reverse wrong))))

(test concurrency-is-named-only-where-it-is-hidden
  "Sento is what runs, and it is behind pine/run: a job, a tick and an actor are
what everything above says instead. A client naming it is a client that had to be
a lisp image with the same library loaded, which is the whole of why nothing but
a lisp could drive pine."
  (let ((leaked (loop :for file :in (%files)
                      :for where := (namestring file)
                      :when (and (find-if (lambda (line) (search "sento" line))
                                          (%lines file))
                                 (not (search "/run/" where)))
                        :collect (file-namestring file))))
    (is (null leaked) "~{~%  ~a names sento~}" leaked)))

(defparameter +the-graph+
  '("depend" "undepend" "reading" "mark" "standsp" "stalep" "work-out" "freshp"
    "cached" "saw" "attach" "detach")
  "What is pine's own about a node: the edges of the graph, the versions a reading
is checked against, and the tree surgery. Called, never answered.")

(test a-node-class-answers-the-protocol-and-not-the-graph
  "Three lists in PINE/FS/NODE, and which one a name is in is the whole of what
somebody writing a node class has to know. A class that answers a graph generic is
a class fighting the substrate: it would be deciding what read what, which is the
one thing nothing outside src/fs/ may decide.

The protocol is what is left, and it is small: CONTENTS, NODES, RESOLVE, VERB,
ANNOUNCES, REFRESHES, SAVEDP, LIVEP, HOLDING, MAKE-CHILD, ERASE-CHILD, STIR."
  (let ((wrong nil))
    (dolist (file (%files))
      (let ((where (namestring file)))
        (unless (search "/fs/" where)
          (dolist (line (%lines file))
            (dolist (name +the-graph+)
              (when (search (format nil "(defmethod node:~a " name) line)
                (pushnew (format nil "~a answers node:~a"
                                 (file-namestring file) name)
                         wrong :test #'equal)))))))
    (is (null wrong) "~{~%  ~a~}" wrong)))
