(in-package :pine/test)

(def-suite* :pine/style :in :pine)

(defparameter +modules+ '("run" "fs" "world" "proc" "repl" "path" "ui" "ts" "edit"
                          "provider" "net" "app" "wayland" "wayland/app" "cairo")
  "The v2 modules. Each step adds its own; what is not here has not been ported.")

(defparameter +line-limit+ 400)

(defparameter +definers+
  '("defun" "defmacro" "defclass" "defmethod" "defgeneric" "define-condition"
    "defstruct" "deftype" "defsetf"))

(defun %module-root ()
  (merge-pathnames "src/" (asdf:system-source-directory :pine)))

(defun %lang-files ()
  (append (directory (merge-pathnames "ts/lang/*.lisp" (%module-root)))
          (directory (merge-pathnames "wayland/protocol/*.lisp" (%module-root)))))

(defun %files ()
  (remove-if (lambda (f) (char= #\. (char (file-namestring f) 0)))
             (append (list (merge-pathnames "boot.lisp" (%module-root))
                           (merge-pathnames "frontend.lisp" (%module-root)))
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

(test v2-code-carries-no-commentary
  (let ((found nil))
    (dolist (file (%files))
      (dolist (n (%commented-lines file))
        (push (format nil "~a:~d" (file-namestring file) n) found)))
    (is (null found) "~d commented line~:p:~{~%  ~a~}" (length found) (reverse found))))

(test every-global-is-in-the-block-at-the-top
  (let ((loose nil))
    (dolist (file (%files))
      (let ((lines (%lines file))
            (first-definition nil)
            (last-global nil))
        (loop :for line :in lines
              :for n :from 1
              :do (when (and (null first-definition) (%starts-with-any line +definers+))
                    (setf first-definition n))
                  (when (%starts-with-any line '("defvar" "defparameter" "defconstant"))
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

(defparameter +declarations+ '("syntax.lisp" "commonlisp.lisp" "scheme.lisp"
                               "pine.lisp" "reader.lisp")
  "Where the sugar belongs: the language declarations, and the module that is it.")

(test the-operating-system-does-not-read-in-its-own-sugar
  (let ((using nil))
    (dolist (file (%files))
      (when (and (find-if (lambda (line) (search "named-readtables:in-readtable" line)) (%lines file))
                 (not (member (file-namestring file) +declarations+ :test #'equal)))
        (push (file-namestring file) using)))
    (is (null using) "~{~%  ~a declares a readtable~}" (reverse using))))

(defun %naming (what &key (except nil))
  "The files that name WHAT, other than the ones allowed to."
  (loop :for file :in (%files)
        :when (and (find-if (lambda (line) (search what line)) (%lines file))
                   (not (member (file-namestring file) except :test #'equal)))
          :collect (file-namestring file)))

(test only-data-knows-what-a-value-is-kept-in
  "fset is what a value is and sento.atomic is where one is kept. Both are
behind pine/data, so what pine holds is one idea in one file."
  (is (null (%naming "fset:" :except '("data.lisp")))
      "~{~%  ~a names fset~}" (%naming "fset:" :except '("data.lisp")))
  (is (null (%naming "sento.atomic" :except '("data.lisp")))
      "~{~%  ~a names sento.atomic~}"
      (%naming "sento.atomic" :except '("data.lisp"))))

(test nothing-sleeps-in-a-loop-to-repeat
  "One clock: the actor system's wheel timer, and pine/run/timer over it."
  (is (null (%naming "schedule-recurring" :except '("timer.lisp")))
      "~{~%  ~a schedules its own repeat~}"
      (%naming "schedule-recurring" :except '("timer.lisp"))))

(test a-thread-is-made-only-where-something-blocks
  "A pty read, a child's stdout and a frontend's loop block. Everything else is
an endpoint or a tick, so it makes no thread."
  (is (null (%naming "make-thread" :except '("task.lisp" "frontend.lisp")))
      "~{~%  ~a makes a thread of its own~}"
      (%naming "make-thread" :except '("task.lisp" "frontend.lisp"))))

(test an-endpoint-is-sentos-and-not-one-of-our-own
  "actor-of is what makes one, and pine/run/endpoint is where that is said."
  (is (null (%naming "actor-context:actor-of" :except '("endpoint.lisp" "server.lisp")))
      "~{~%  ~a makes an actor of its own~}"
      (%naming "actor-context:actor-of" :except '("endpoint.lisp" "server.lisp"))))

(test a-registry-is-a-table-and-not-a-hash-table
  "A hash table is for identity, or for one call's own scratch. Anything two
threads read is a map in a box."
  (let ((loose nil))
    (dolist (file (%files))
      (dolist (line (%lines file))
        (when (search "(defvar *" line)
          (when (search "make-hash-table" line)
            (push (file-namestring file) loose)))))
    (is (null loose) "~{~%  ~a keeps a registry in a hash table~}"
        (reverse loose))))

(test a-frontend-says-its-size-in-one-place
  "Two places built the :resize message and one of them left out the font size,
so the daemon laid the frame out for a grid the frontend was not drawing. What
a frontend tells the daemon about its geometry is written once."
  (let ((sites nil))
    (dolist (file (%files))
      (loop :for line :in (%lines file)
            :for n :from 1
            :when (search "(list :resize" line)
              :do (push (format nil "~a:~d" (file-namestring file) n) sites)))
    (is (<= (length sites) 1) "~d place~:p build a resize message:~{~%  ~a~}"
        (length sites) (reverse sites))))

(defparameter +builds-fresh+ '("supervisor.lisp")
  "The files whose NODES or RESOLVE may answer without NODE:CHILD, because what
they answer is already the same object every time: the supervisor hands out the
processes it holds, and a process is a node itself. A name added here for any
other reason is a subtree taken out of the graph.")

(test a-provider-hands-out-the-same-child-every-time
  "A node built fresh per call cannot be depended on, so nothing reading it can
ever be recomputed: a surface reading /face/keyword would never hear that the
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
             (end (or (position-if (lambda (c) (member c '(#\Space #\) #\Tab))) line
                                   :start at)
                      (length line))))
        (subseq line at end)))))

(test a-package-is-its-path
  "src/edit/buffer.lisp declares pine/edit/buffer. The systems are already
spelled that way; a package that is not is a name you have to translate. The
root is pine, and the four generated wayland protocol files are one package
between them."
  (let ((wrong nil))
    (dolist (file (%files))
      (let ((said (%package-of file))
            (path (namestring file)))
        (when said
          (let* ((from (search "src/" path))
                 (under (and from (subseq path (+ from 4))))
                 (want (and under
                            (concatenate 'string "pine/"
                                         (subseq under 0 (- (length under) 5))))))
            (when (and want (not (equal said want))
                       (not (equal said "pine"))
                       (not (equal said "pine/wayland/protocol")))
              (push (format nil "~a says ~a" (file-namestring file) said) wrong))))))
    (is (null wrong) "~{~%  ~a~}" (reverse wrong))))

(test nothing-spells-a-package-with-a-dot
  (let ((found (%naming "pine." :except '("style.lisp"))))
    (is (null found) "~{~%  ~a names a package with a dot~}" found)))

(test in-package-is-the-line-after-the-defpackage
  (let ((loose nil))
    (dolist (file (%files))
      (let ((lines (%lines file))
            (depth 0) (start nil) (end nil) (at nil))
        (loop :for line :in lines
              :for n :from 0
              :do (when (and (null start) (search "(defpackage" line)) (setf start n))
                  (when (and start (null end))
                    (incf depth (count #\( line))
                    (decf depth (count #\) line))
                    (when (<= depth 0) (setf end n)))
                  (when (and (null at) (search "(in-package" line)) (setf at n)))
        (when (and end at (/= at (1+ end)))
          (push (file-namestring file) loose))))
    (is (null loose) "~{~%  ~a puts something between defpackage and in-package~}"
        (reverse loose))))

(test a-nickname-names-one-package
  "A nickname is vocabulary. It has to mean the same thing in every file, or
reading one means checking its header first."
  (let ((seen (make-hash-table :test 'equal))
        (clashes nil))
    (dolist (file (%files))
      (dolist (line (%lines file))
        (let ((at 0))
          (loop
            (let ((open (search "(#:" line :start2 at)))
              (unless open (return))
              (let* ((rest (subseq line (+ open 3)))
                     (space (position #\Space rest))
                     (name (and space (subseq rest 0 space)))
                     (tail (and space (subseq rest (1+ space))))
                     (of (and tail (search "#:pine" tail))))
                (when (and name of)
                  (let* ((from (+ of 2))
                         (to (or (position-if (lambda (c) (member c '(#\Space #\)))) tail
                                              :start from)
                                 (length tail)))
                         (package (subseq tail from to))
                         (had (gethash name seen)))
                    (cond ((null had) (setf (gethash name seen) package))
                          ((not (equal had package))
                           (pushnew (format nil "~a is ~a and ~a" name had package)
                                    clashes :test #'equal)))))
                (setf at (+ open 3))))))))
    (is (null clashes) "~{~%  ~a~}" (reverse clashes))))
