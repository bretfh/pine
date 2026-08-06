(in-package :pine.test)

(def-suite* :pine.packages :in :pine)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-introspect))

(defparameter +generated-packages+ '("PINE.WAYLAND.PROTOCOL")
  "Packages whose contents a generator interns and exports: wayflan's scanner
reads the compositor's XML with :export t, so their export list is not written
here and is not this suite's to judge.")

(defun pine-packages ()
  (sort (remove-if (lambda (p) (member (package-name p) +generated-packages+
                                       :test #'string=))
                   (remove-if-not (lambda (p)
                                    (pine.provider.out:starts-with (package-name p) "PINE"))
                                  (list-all-packages)))
        #'string< :key #'package-name))

(defun own-symbols (package)
  (let ((acc nil))
    (do-symbols (s package acc)
      (when (eq (symbol-package s) package) (push s acc)))))

(defun external-symbols (package)
  (let ((acc nil))
    (do-external-symbols (s package acc) (push s acc))))

(defun names-something-p (symbol)
  (or (fboundp symbol)
      (boundp symbol)
      (macro-function symbol)
      (special-operator-p symbol)
      (find-class symbol nil)
      (fboundp (list 'setf symbol))
      (sb-int:info :type :kind symbol)
      (named-readtables:find-readtable symbol)))

;;;; "A file may only name packages that load before it." Read off the image's
;;;; cross-reference tables and the systems' own component order, so a name
;;;; reached through :use counts the same as one written pkg:sym, and a method
;;;; is credited to the file that defines it rather than to the package that
;;;; owns its generic function.

(defparameter +graphed-systems+ '("pine/vt" "pine")
  "The systems whose files make up the load order, in dependency order.")

(defun load-order ()
  "A vector of every graphed source pathname, in the order the systems load."
  (let ((files nil))
    (dolist (system +graphed-systems+ (coerce (nreverse files) 'vector))
      (labels ((walk (component)
                 (cond ((typep component 'asdf:cl-source-file)
                        (let ((path (probe-file (asdf:component-pathname component))))
                          (when path (push path files))))
                       ((typep component 'asdf:parent-component)
                        (mapc #'walk (asdf:component-children component))))))
        (let ((system (asdf:find-system system nil)))
          (when system (walk system)))))))

(defun file-positions (order)
  (let ((table (make-hash-table :test 'equal)))
    (loop :for path :across order
          :for i :from 0
          :do (setf (gethash (namestring path) table) i))
    table))

(defparameter +definition-kinds+
  '(:function :macro :generic-function :variable :constant :class :type
    :structure :condition)
  "The kinds a pine symbol can be defined as, tried in this order.")

(defun definition-file (symbol)
  (loop :for kind :in +definition-kinds+
        :thereis (loop :for source :in (sb-introspect:find-definition-sources-by-name
                                        symbol kind)
                       :for path = (sb-introspect:definition-source-pathname source)
                       :when path :return (namestring path))))

(defun reference-sites (symbol)
  "The files whose code calls or reads SYMBOL."
  (let ((acc nil))
    (dolist (entry (append (sb-introspect:who-calls symbol)
                           (sb-introspect:who-references symbol))
                   acc)
      (let ((path (sb-introspect:definition-source-pathname (cdr entry))))
        (when path (pushnew (namestring path) acc :test #'string=))))))

(defun forward-references ()
  "Every place a graphed file names something a later graphed file defines."
  (let ((positions (file-positions (load-order)))
        (violations nil))
    (dolist (package (pine-packages) violations)
      (dolist (symbol (own-symbols package))
        (let* ((home (definition-file symbol))
               (defined-at (and home (gethash home positions))))
          (when defined-at
            (dolist (site (reference-sites symbol))
              (let ((used-at (gethash site positions)))
                (when (and used-at (< used-at defined-at))
                  (pushnew (list (file-namestring site) symbol
                                 (file-namestring home))
                           violations :test #'equal))))))))))

(defun kernel-files ()
  "The graphed files that are the kernel: everything outside src/editor/."
  (remove-if (lambda (path) (search "/editor/" (namestring path)))
             (coerce (load-order) 'list)))

(defun editor-symbols ()
  "Every symbol defined in a file under src/editor/."
  (let ((acc nil))
    (dolist (package (pine-packages) acc)
      (dolist (symbol (own-symbols package))
        (let ((home (definition-file symbol)))
          (when (and home (search "/editor/" home))
            (push symbol acc)))))))

(test the-kernel-names-nothing-under-editor
  "The editor is built from pine's facilities; it is not one of them. So no
file outside src/editor/ may name anything defined inside it -- if the kernel
needs the editor to do something, the design is upside down."
  (let ((kernel (mapcar #'namestring (kernel-files)))
        (violations nil))
    (dolist (symbol (editor-symbols))
      (dolist (site (reference-sites symbol))
        (when (member site kernel :test #'string=)
          (pushnew (list (file-namestring site) symbol) violations :test #'equal))))
    (is (null violations)
        "~{~%  ~a names ~s, which src/editor/ defines~}"
        (loop :for (site symbol) :in violations :append (list site symbol)))))

(defparameter +static-tables+
  '("src/ts/highlight.lisp"                ; the highlighter's keyword sets
    "vt/write.lisp")                       ; the DEC line-drawing charset
  "The files whose hash tables are filled at load and never written again.
Everything else that more than one thread touches is a PINE.DATA:TABLE.")

(defun %plain-tables ()
  "Every global initialised with a bare hash table, and the file it is in."
  (let ((found nil))
    (loop :for path :across (load-order)
          :do (with-open-file (f path :if-does-not-exist nil)
                (when f
                  (loop :for line = (read-line f nil)
                        :while line
                        :when (and (> (length line) 8)
                                   (string= "(defvar " (subseq line 0 8))
                                   (search "make-hash-table" line))
                          :do (push (cons (namestring path)
                                          (string-trim " " (subseq line 8)))
                                    found)))))
    (nreverse found)))

(test state-two-threads-share-is-a-table-not-a-hash-table
  "A plain hash table written from two threads can corrupt on SBCL, and a lock
is a thing to forget to take. So shared state is a PINE.DATA:TABLE: a persistent
map in an atomic cell, read with a pointer read and changed with a compare and
swap, which is how the namespace itself works one level up.

The exception is a table filled once at load and never written again, which is
data rather than state."
  (let ((loose (remove-if (lambda (cell)
                            (some (lambda (allowed) (search allowed (car cell)))
                                  +static-tables+))
                          (%plain-tables))))
    (is (null loose)
        "~d global~:p hold a bare hash table:~{~%  ~a~}"
        (length loose)
        (loop :for (path . rest) :in loose
              :collect (format nil "~a  ~a" (file-namestring path)
                               (subseq rest 0 (min 40 (length rest))))))))

(test a-table-is-safe-under-contention
  "The property the conversion was for, tested rather than assumed."
  (let ((table (pine.data:table))
        (threads nil)
        (per 2000)
        (n 8))
    (dotimes (i n)
      (let ((i i))
        (push (bordeaux-threads:make-thread
               (lambda () (dotimes (j per)
                            (pine.data:put table (cons i j) j))))
              threads)))
    (mapc #'bordeaux-threads:join-thread threads)
    (is (= (* n per) (fset:size (pine.data:all table)))
        "~d of ~d writes survived eight threads" 
        (fset:size (pine.data:all table)) (* n per))))

(test claim-answers-the-winner-so-an-interned-value-is-eq
  "The loser of a race gets the winner's object. Without that, KEY= being EQ is
a lie the first time two threads parse the same chord at once."
  (let ((table (pine.data:table))
        (seen nil)
        (lock (bordeaux-threads:make-lock))
        (threads nil))
    (dotimes (i 8)
      (push (bordeaux-threads:make-thread
             (lambda ()
               (let ((mine (pine.data:claim table :one (list :made-by (list i)))))
                 (bordeaux-threads:with-lock-held (lock) (push mine seen)))))
            threads))
    (mapc #'bordeaux-threads:join-thread threads)
    (is (= 8 (length seen)))
    (is (= 1 (length (remove-duplicates seen :test #'eq)))
        "eight claims of one key answered ~d different objects"
        (length (remove-duplicates seen :test #'eq)))))

(test a-file-only-names-what-loads-before-it
  (let ((violations (forward-references)))
    (is (null violations)
        "~{~%  ~a names ~s, defined later in ~a~}"
        (loop :for (site symbol home) :in violations
              :append (list site symbol home)))))

(test every-exported-symbol-names-something
  (dolist (package (pine-packages))
    (let ((empty (sort (remove-if #'names-something-p (external-symbols package))
                       #'string< :key #'symbol-name)))
      (is (null empty)
          "~a exports ~d symbol~:p that name nothing: ~{~a~^ ~}"
          (package-name package) (length empty) empty))))

(test no-package-exports-an-internal
  "A name spelled %foo is internal by convention here, so exporting one says
the interface and the convention disagree."
  (dolist (package (pine-packages))
    (let ((internal (sort (remove-if-not
                           (lambda (s) (pine.provider.out:starts-with (symbol-name s) "%"))
                           (external-symbols package))
                          #'string< :key #'symbol-name)))
      (is (null internal)
          "~a exports ~d internal name~:p: ~{~a~^ ~}"
          (package-name package) (length internal) internal))))

(test no-package-exports-a-symbol-it-does-not-own-or-inherit
  (dolist (package (pine-packages))
    (let ((foreign nil))
      (dolist (symbol (external-symbols package))
        (unless (or (eq (symbol-package symbol) package)
                    (member (symbol-package symbol) (package-use-list package))
                    (nth-value 1 (find-symbol (symbol-name symbol) package)))
          (push symbol foreign)))
      (is (null foreign)
          "~a exports symbols from elsewhere: ~{~a~^ ~}"
          (package-name package) foreign))))

(defun %doc-gone-names ()
  "Every name the doc's \"What this replaces\" table says is gone.

Read off the doc rather than transcribed, so a name struck out there is a name
the config language has to stop answering to, without anyone remembering to
come here and say so."
  (let ((path (merge-pathnames "../doc/api.org"
                               #.(or *compile-file-truename* *load-truename*)))
        (names nil)
        (inside nil))
    (with-open-file (f path :if-does-not-exist nil)
      (when f
        (loop :for line = (read-line f nil)
              :while line
              :do (cond
                    ((and (> (length line) 2) (string= "* " (subseq line 0 2)))
                     (setf inside (search "What this replaces" line)))
                    ((and inside (plusp (length line)) (char= #\| (char line 0)))
                     (let* ((bar (position #\| line :start 1))
                            (gone (subseq line 1 (or bar (length line))))
                            (start 0))
                       (loop :for open = (position #\= gone :start start)
                             :while open
                             :for close = (position #\= gone :start (1+ open))
                             :while close
                             :do (let ((text (string-trim "*" (subseq gone (1+ open)
                                                                     close))))
                                   (when (and (plusp (length text))
                                              (every (lambda (ch)
                                                       (or (alphanumericp ch)
                                                           (find ch "-*")))
                                                     text))
                                     (pushnew text names :test #'string-equal)))
                                 (setf start (1+ close)))))))))
    (nreverse names)))

(test nothing-the-doc-struck-out-is-still-in-the-config-language
  "The left column of \"What this replaces\" is a list of names a config used to
call. A name still bound in PINE.USER is one the rewrite did not finish: the
config can go on calling it, and the path that replaced it has a rival."
  (let* ((gone (%doc-gone-names))
         (package (find-package :pine.user))
         (still (remove-if-not
                 (lambda (name)
                   (multiple-value-bind (symbol kind)
                       (find-symbol (string-upcase name) package)
                     (and symbol kind
                          (or (fboundp symbol) (boundp symbol)
                              (macro-function symbol)))))
                 gone)))
    (is (plusp (length gone)) "the replaces table was not read at all")
    (is (null still)
        "~d name~:p the doc struck out are still bound in pine.user:~{~%  ~a~}"
        (length still) still)))

(defun %doc-vocabulary ()
  "Every widget name the doc's vocabulary table lists."
  (let ((path (merge-pathnames "../doc/api.org"
                               #.(or *compile-file-truename* *load-truename*)))
        (names nil)
        (inside nil))
    (with-open-file (f path :if-does-not-exist nil)
      (when f
        (loop :for line = (read-line f nil)
              :while line
              :do (cond
                    ((search "** The vocabulary" line) (setf inside t))
                    ((and inside (> (length line) 1) (string= "**" (subseq line 0 2)))
                     (setf inside nil))
                    ((and inside (plusp (length line)) (char= #\| (char line 0)))
                     (let ((start 0))
                       (loop :for open = (position #\= line :start start)
                             :while open
                             :for close = (position #\= line :start (1+ open))
                             :while close
                             :do (let ((text (subseq line (1+ open) close)))
                                   (when (and (plusp (length text))
                                              (every (lambda (ch)
                                                       (or (alphanumericp ch)
                                                           (char= ch #\-)))
                                                     text))
                                     (pushnew text names :test #'string-equal)))
                                 (setf start (1+ close)))))))))
    (nreverse names)))

(defun %doc-helper-names ()
  "The helpers the doc says are all a config has left. Read off the sentence
that makes the claim, so the claim and the check are one thing."
  (let ((path (merge-pathnames "../doc/api.org"
                               #.(or *compile-file-truename* *load-truename*)))
        (names nil)
        (collecting nil))
    (with-open-file (f path :if-does-not-exist nil)
      (when f
        (loop :for line = (read-line f nil)
              :while line
              :do (when (search "What is left of the config" line)
                    (setf collecting t))
                  (when collecting
                    (let ((start 0))
                      (loop :for open = (position #\= line :start start)
                            :while open
                            :for close = (position #\= line :start (1+ open))
                            :while close
                            :do (pushnew (subseq line (1+ open) close) names
                                         :test #'string-equal)
                                (setf start (1+ close))))
                    (when (search "every one is a widget helper" line)
                      (setf collecting nil))))))
    (nreverse names)))

(defun %init-defuns ()
  "Every function the shipped config defines."
  (let ((path (merge-pathnames "../examples/init.lisp"
                               #.(or *compile-file-truename* *load-truename*)))
        (names nil))
    (with-open-file (f path :if-does-not-exist nil)
      (when f
        (loop :for line = (read-line f nil)
              :while line
              :when (and (> (length line) 7) (string= "(defun " (subseq line 0 7)))
                :do (let* ((rest (subseq line 7))
                           (end (position-if (lambda (ch)
                                               (or (char= ch #\Space)
                                                   (char= ch #\()))
                                             rest)))
                      (push (subseq rest 0 end) names)))))
    (nreverse names)))

(test the-shipped-config-is-only-the-helpers-the-doc-names
  "The measure the doc sets itself: a real config against this design is
providers, trees and keys, and what is left over is widget helpers and
formatters. A function in the config that is not one of them is work the design
was supposed to have taken away."
  (let* ((named (%doc-helper-names))
         (defined (%init-defuns))
         (extra (set-difference defined named :test #'string-equal))
         (unused (set-difference named defined :test #'string-equal)))
    (is (plusp (length named)) "the doc's claim was not read at all")
    (is (plusp (length defined)) "the config was not read at all")
    (is (null extra) "the config defines ~d function~:p the doc does not:~{~%  ~a~}"
        (length extra) extra)
    (is (null unused) "the doc names ~d helper~:p the config does not define:~{~%  ~a~}"
        (length unused) unused)))

(test every-widget-the-doc-names-is-in-the-config-language
  "The vocabulary table is the whole of what a config says to draw with. A name
in it that PINE.USER does not export is a widget the doc promises and the config
cannot call."
  (let* ((named (%doc-vocabulary))
         (package (find-package :pine.user))
         (missing (remove-if
                   (lambda (name)
                     (multiple-value-bind (symbol kind)
                         (find-symbol (string-upcase name) package)
                       (and symbol (eq kind :external) (fboundp symbol))))
                   named)))
    (is (plusp (length named)) "the vocabulary table was not read at all")
    (is (null missing)
        "~d widget~:p the doc names are not in pine.user:~{~%  ~a~}"
        (length missing) missing)))

(test the-user-language-vocabulary-is-bound
  (dolist (symbol (external-symbols (find-package :pine.user)))
    (is (names-something-p symbol)
        "pine.user:~a is exported but names nothing" (symbol-name symbol))))

(test every-file-a-system-names-is-on-disk
  (let ((order (load-order)))
    (is (plusp (length order)))
    (loop :for path :across order
          :do (is (probe-file path) "~a is named by a system but missing" path))))

;;;; The tree against the doc
;;;;
;;;; doc/api.org says what a buffer is. A leaf the code keeps under one and
;;;; the doc does not name is drift, and drift is how an API becomes a port of
;;;; whatever was there before.

(defun %doc-text ()
  (uiop:read-file-string
   (merge-pathnames "doc/api.org" (asdf:system-source-directory :pine))))

(defun %doc-paths (under)
  "Every path the doc names under UNDER, as text. A path is written =/like/this=."
  (let ((text (%doc-text))
        (found nil)
        (i 0))
    (loop :for start = (search (concatenate 'string "=" under) text :start2 i)
          :while start
          :for end = (position #\= text :start (1+ start))
          :while end
          :do (push (subseq text (1+ start) end) found)
              (setf i (1+ end)))
    (remove-duplicates (nreverse found) :test #'string=)))

(defun %doc-buffer-leaves ()
  "The leaf of every /buf/?name/... path the doc's table names."
  (let ((prefix "/buf/?name/"))
    (remove-duplicates
     (loop :for path :in (%doc-paths "/buf/")
           :when (and (> (length path) (length prefix))
                      (string= prefix path :end2 (length prefix)))
             :collect (let ((rest (subseq path (length prefix))))
                        (subseq rest 0 (or (position #\/ rest) (length rest)))))
     :test #'string=)))

(defparameter +leaves-with-another-home-coming+ '("tick" "asked")
  "Leaves under a buffer that the doc does not name.

The tick and what the buffer and its parser say to each other are one side of a
conversation rather than something anyone addresses. They are under the buffer
because they are the buffer's, and ASKED is a directory rather than a leaf, so
reading the buffer still answers what the doc describes.")

(test the-doc-names-a-buffers-leaves
  (let ((named (%doc-buffer-leaves)))
    (dolist (leaf '("text" "line" "prop" "point" "mark" "file" "mode" "minor"
                    "modified" "tree" "view"))
      (is (member leaf named :test #'string=)
          "the doc no longer names /buf/?name/~a, but the code serves it" leaf))))

(test a-buffer-carries-only-the-leaves-the-doc-names
  (pine.ns:with-space ()
    (pine.ts.syntax:declare-all)
      (pine.ns:raise :buf)
    (pine.ns:write (pine.buf:at "probe" :text) "hello")
    (pine.ns:write (pine.buf:at "probe" :text) (fset:seq :insert "!"))
    (pine.ns:write (pine.buf:at "probe" :mode) :text)
    (let* ((named (append (%doc-buffer-leaves) +leaves-with-another-home-coming+))
           (leaves (let ((acc nil))
                     (fset:do-map (key value (pine.ns:read (pine.buf:at "probe")))
                       (declare (ignore value))
                       (push (pine.path:name key) acc))
                     acc))
           (extra (remove-if (lambda (leaf) (member leaf named :test #'string=))
                             leaves)))
      (is (null extra)
          "a buffer carries leaves the doc does not name: ~{~a~^ ~}" extra))))

;;;; Who owns the state.
;;;;
;;;; pine is many threads, many actors and many images, and a space is what one
;;;; pine is. So there are four places a piece of mutable state may live and no
;;;; fifth:
;;;;
;;;;   a value                  a path
;;;;   a thing that runs        /proc, whose table the space keeps
;;;;   a thing this image is    the daemon, a client, an app
;;;;   a dynamic extent         a binding, and never a SETF
;;;;
;;;; A special that is assigned after load is none of those. It is one variable
;;;; that every thread and every space in the image shares, which is how
;;;; (read /theme) in one space came to answer what another space wrote and how
;;;; two buffers reading /sys/cpu came to corrupt each other's delta.
;;;;
;;;; These two checks are that rule, derived rather than remembered: one finds
;;;; every special the compiler saw assigned, the other finds every special
;;;; holding a table that more than one thread can write. What is left on the
;;;; lists is the work, and it is meant to empty.

(defun %src-file (symbol)
  "Where SYMBOL is defined, when that is a file of pine's own. The terminal
emulator counts: it is a system of its own but it is in the same image, and a
global there is shared exactly as far.

Asked of the system rather than matched against a directory name: which
directory pine is checked out into is not pine's business, and pinning it there
made every one of these look unassigned from any other checkout."
  (let ((file (definition-file symbol))
        (root (namestring (asdf:system-source-directory :pine))))
    (when (and file (or (search (concatenate 'string root "src/") file)
                        (search (concatenate 'string root "vt/") file)))
      file)))

(defun %assigned-specials ()
  "Every special defined in src/ that something assigns, as (package name file)."
  (let ((acc nil))
    (dolist (package (pine-packages) (sort acc #'string< :key #'second))
      (dolist (symbol (own-symbols package))
        (when (and (boundp symbol)
                   (sb-walker:var-globally-special-p symbol)
                   (sb-introspect:who-sets symbol))
          (let ((file (%src-file symbol)))
            (when file
              (push (list (package-name package) (symbol-name symbol)
                          (file-namestring file))
                    acc))))))))

(defparameter +state-in-a-special+
  '(;; a value: it belongs at a path
    ;; a thing that runs: it belongs at /proc
    ;; a thing this image is: it belongs on the object that is it
    ("PINE.CORE.SERVER"    "*SERVER*"             "the daemon this image is")
    ("PINE.CORE.ACTOR"     "*LOCAL-AGENT*"        "the daemon's")
    ("PINE.CORE.AGENT"     "*AGENT-SYSTEM*"       "the agent image's")
    ("PINE.CORE.AGENT"     "*NAME*"               "the agent image's")
    ("PINE.CORE.AGENT"     "*MASTER-DEBUG*"       "the agent image's")
    ("PINE.CORE.ATTACH"    "*CLIENTS*"            "the daemon's")
    ("PINE.WAYLAND.INPUT"  "*ON-HOVER*"           "the app's")
    ("PINE.VT"             "*PTY-LOADED*"         "the image's: the shared library is loaded once")
    ;; registrations a file makes as it loads, read from every thread after
    ("PINE.BUF"            "*VERBS*"              "what pine.view installed; a registry")
    ("PINE.KEY"            "*TERMINAL-HANDLER*"   "a registry of one")
    ;; settings, which are paths. A setting a test has to assign globally to
    ;; reach the thread that reads it is the argument for it being one.
    ;;
    ;; These three are what the environment said, and a saved image answered
    ;; them when it was built, so the CLI asks again before it does anything.
    ;; They are the image's own, decided once at startup.
    ("PINE.CORE.SERVER"    "*PORT*"               "a setting")
    ("PINE.CORE.SERVER"    "*WORKERS*"            "the image's, from the environment")
    ("PINE.CORE.SERVER"    "*APP-ACTOR-CONFIG*"   "the image's, built from *WORKERS*")
    ;; bound per thread, and assigned in one place that should bind too
    ("PINE.EDITOR.FRAME"   "*CLIENT*"             "the client this thread is serving")
    ;; the namespace itself
    ("PINE.NS"             "*SPACE*"              "the space this image serves"))
  "Every special that still holds something assigned after load, and where it
is going.

This list is the work, checked rather than written down somewhere: a global
cannot be added without saying which of the four homes it belongs in, and each
one taken off is one fewer thing two threads share. It is meant to empty down
to the image-identity ones.")

(defun %unlisted (found listed)
  "Every entry of FOUND that LISTED does not name, by package and symbol."
  (remove-if (lambda (one)
               (find-if (lambda (other)
                          (and (string= (first one) (first other))
                               (string= (second one) (second other))))
                        listed))
             found))

(test no-special-holds-state-two-threads-share
  "A special assigned after load is one variable every thread and every space
in the image shares. Each one still here is named above with the home it is
going to; anything else is new and has to say for itself."
  (let ((loose (%unlisted (%assigned-specials) +state-in-a-special+)))
    (is (null loose)
        "~d special~:p are assigned after load and say nothing about who owns them:~{~%  ~a~}"
        (length loose)
        (loop :for (package name file) :in loose
              :collect (format nil "~a::~a  (~a)" package name file)))))

(defun %shared-tables ()
  "Every special in src/ holding a table more than one thread can write."
  (let ((acc nil))
    (dolist (package (pine-packages) (sort acc #'string< :key #'second))
      (dolist (symbol (own-symbols package))
        (when (and (boundp symbol)
                   (sb-walker:var-globally-special-p symbol)
                   (let ((kind (type-of (symbol-value symbol))))
                     (and (symbolp kind)
                          (string= "ATOMIC-REFERENCE" (symbol-name kind))))
                   (%src-file symbol))
          (push (list (package-name package) (symbol-name symbol)
                      (file-namestring (%src-file symbol)))
                acc))))))

(defparameter +tables-in-a-special+
  '(;; the namespace itself: the one table the rest of this is about
    ("PINE.NS"             "*SPACE*"     "the space this image serves")
    ;; registries: what this image's code can do, filled as the files load
    ("PINE.NS"             "*SERVERS*"   "what pine can serve")
    ("PINE.PROC"           "*KINDS*"     "what /proc knows how to run")
    ("PINE.UI.WIRE"        "*CODEC*"     "what crosses the wire, both ways")
    ("PINE.CMD"            "*BUILTIN*"   "the commands pine ships")
    ("PINE.KEY"            "*BOUND*"     "the bindings pine ships, replayed into a fresh space")
    ("PINE.UI.FACE"        "*THEMES*"    "the themes DEFTHEME registered")
    ("PINE.ECHO"           "*SOURCES*"   "completion sources")
    ("PINE.ECHO"           "*ACTIONS*"   "what a candidate category offers")
    ("PINE.CORE.ATTACH"    "*APPS*"      "the frontend kinds")
    ("PINE.KEY"            "*CACHE*"     "interned chords, so KEY= is EQ")
    ("PINE.CORE.HOOKS"     "*HOOKS*"     "what to run coming up and going down")
    ("PINE.CORE.HOOKS"     "*ADDED*"     "the order they were added in")
    ("PINE.WAYLAND.APP.KEYS" "*EKB*"     "the app's keyboard state"))
  "Every special holding a shared table, and what it is: live state with the
path it is going to, or a registry of what this image's code can do.

A registry is data and may be image-wide. State may not, and the ones marked as
state are the second half of the work.")

(test every-shared-table-says-what-it-is
  "A table in a special is read from every thread. Either it is a registry --
what the code can do, filled as the files load -- or it is state two spaces
share, and state has an owner."
  (let ((loose (%unlisted (%shared-tables) +tables-in-a-special+)))
    (is (null loose)
        "~d shared table~:p say nothing about who owns them:~{~%  ~a~}"
        (length loose)
        (loop :for (package name file) :in loose
              :collect (format nil "~a::~a  (~a)" package name file)))))

(test neither-list-carries-a-global-that-is-gone
  "An entry naming a special that no longer exists is a list that grew a
description of work already done. The lists are the work, so what is off the
list has to be off the list."
  (let ((stale (append (%unlisted +state-in-a-special+ (%assigned-specials))
                       (%unlisted +tables-in-a-special+ (%shared-tables)))))
    (is (null stale)
        "~d entr~:@p name a special nothing assigns any more:~{~%  ~a~}"
        (length stale)
        (loop :for (package name why) :in stale
              :collect (format nil "~a::~a  (~a)" package name why)))))

(defun %defpackage-name (file)
  "The package FILE declares, or NIL."
  (with-open-file (f file)
    (loop :for line = (read-line f nil)
          :while line
          :when (and (> (length line) 12) (string= "(defpackage " line :end2 12))
            :do (let* ((rest (string-left-trim "#:" (subseq line 12)))
                       (end (position-if-not
                             (lambda (c)
                               (or (alphanumericp c) (char= c #\.) (char= c #\-)))
                             rest)))
                  (return (string-downcase (subseq rest 0 (or end (length rest)))))))))

(defun %source-files (dir)
  "Every .lisp file at or under DIR."
  (let ((acc nil))
    (dolist (entry (directory (merge-pathnames "*.*" dir)) acc)
      (cond ((null (pathname-name entry))
             (setf acc (append acc (%source-files entry))))
            ((equal "lisp" (pathname-type entry)) (push entry acc))))))

(defun %where-it-belongs (name)
  "The path under src/ a package of NAME declares itself in."
  (if (string= name "pine")
      "boot.lisp"
      (concatenate 'string
                   (substitute #\/ #\. (subseq name (length "pine.")))
                   ".lisp")))

(test the-path-to-a-package-is-its-name
  "pine.editor.win lives in src/editor/win.lisp and declares itself there.
There is no manifest, so a package that moved without its file is a name that
says where it is not."
  (let ((root (merge-pathnames "../src/"
                               #.(or *compile-file-truename* *load-truename*)))
        (wrong nil))
    (dolist (file (%source-files root))
      (let ((name (%defpackage-name file)))
        (when (and name
                   (not (member (string-upcase name) +generated-packages+
                                :test #'string=)))
          (let ((has (format nil "~{~a/~}~a.lisp"
                             (rest (member "src" (pathname-directory file)
                                           :test #'equal))
                             (pathname-name file))))
            (unless (string= (%where-it-belongs name) has)
              (push (format nil "~a is in src/~a" name has) wrong))))))
    (is (null wrong) "~d package~:p are not where their name says:~{~%  ~a~}"
        (length wrong) (nreverse wrong))))

(defparameter +shape-dispatch-allowed+
  '(("cmd.lisp" "AT"))
  "Every ETYPECASE or TYPECASE left in src/, by file and the function it is in.

A branch on what a value is, is a generic function that has not been written:
the branches are the methods, and a layer with a value of its own can add one
from its own file. What is here is what has not been turned into one yet.")

(defun %shape-dispatches ()
  "Every ETYPECASE or TYPECASE in src/, as (file . the defun it sits in)."
  (let ((root (merge-pathnames "../src/"
                               #.(or *compile-file-truename* *load-truename*)))
        (acc nil))
    (dolist (file (%source-files root) (nreverse acc))
      (with-open-file (f file)
        (let ((in nil))
          (loop :for line = (read-line f nil)
                :while line
                :do (let ((trimmed (string-left-trim " " line)))
                      (when (and (> (length trimmed) 5)
                                 (string= "(def" trimmed :end2 4))
                        (let* ((rest (subseq trimmed (1+ (or (position #\Space trimmed) 0))))
                               (end (position-if
                                     (lambda (c) (member c '(#\Space #\( #\))))
                                     rest)))
                          (setf in (string-upcase
                                    (subseq rest 0 (or end (length rest)))))))
                      (when (and (or (search "(etypecase " line)
                                     (search "(typecase " line))
                                 (not (search "\"" line)))
                        (push (list (file-namestring file) in) acc)))))))))

(test no-branch-on-what-a-value-is
  "A function that branches on a value's type is a generic function whose
methods have not been written. Each one still here is named above."
  (let ((loose (%unlisted (%shape-dispatches) +shape-dispatch-allowed+)))
    (is (null loose)
        "~d branch~:p on a value's type say nothing about why:~{~%  ~a~}"
        (length loose)
        (loop :for (file in) :in loose
              :collect (format nil "~a in ~a" in file)))))
