(defpackage #:pine.mode
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path))
  (:export #:chain #:setting #:for-file #:minors #:handler #:matches-p
           #:minor-p #:names #:mount))

(in-package #:pine.mode)
(named-readtables:in-readtable pine.path:syntax)

;;;; A mode is a map at /mode/?name. Everything declarative is a leaf and
;;;; everything behavioural is a handler under :on, so writing the mode is the
;;;; whole of registering it: there is no class, no defmode and no registry.
;;;;
;;;; Nothing here holds state. Each of these is a read of the tree and a
;;;; computation over what it answered, which is the division the whole design
;;;; rests on: the pattern selects, Lisp computes.

(defun chain (name)
  "NAME and the modes it falls back to, most specific first.

A mode names its :parent, so this walks up until one names none. A cycle
someone wrote by hand ends the walk rather than hanging."
  (loop :with seen = nil
        :for at = name :then (ns:read (p:path /mode at :parent))
        :while (and at (not (member at seen)))
        :do (push at seen)
        :collect at))

(defun setting (name key &optional default)
  "What KEY is for mode NAME, taking the first mode up the chain that says.

This is what makes :parent mean anything: prog-mode says the indent width and
lisp-mode inherits it by not saying one."
  (loop :for mode :in (chain name)
        :for value = (ns:read (p:path /mode mode key))
        :when value :do (return value)
        :finally (return default)))

;;;; Which mode a file gets

(defun matches-p (pattern name)
  "Whether the glob PATTERN covers the file NAME. Only * is special, and it
covers any run of characters including none."
  (labels ((walk (p n)
             (cond ((and (null p) (null n)) t)
                   ((null p) nil)
                   ((char= (first p) #\*)
                    (or (walk (rest p) n)
                        (and n (walk p (rest n)))))
                   ((null n) nil)
                   ((char-equal (first p) (first n)) (walk (rest p) (rest n)))
                   (t nil))))
    (walk (coerce pattern 'list) (coerce name 'list))))

(defun %patterns (value)
  (cond ((null value) nil)
        ((stringp value) (list value))
        ((fset:seq? value) (fset:convert 'list value))
        (t value)))

(defun %claims ()
  "Each mode that claims file types, with the patterns it claims. The mode is
named the way a path stores it, so it reads back the same as :parent does."
  (let (acc)
    (fset:do-map (path value (ns:read /mode/*/files {}))
      (push (cons (p:key (p:leaf (p:parent path))) (%patterns value)) acc))
    (nreverse acc)))

(defun for-file (path)
  "The mode whose :files cover PATH, or NIL.

Every mode that claims a file type says so in its own map, so there is no
second table of extensions and nothing to keep in step with the modes."
  (let ((name (file-namestring (pathname path))))
    (loop :for (at . patterns) :in (%claims)
          :when (some (lambda (pattern) (matches-p pattern name)) patterns)
            :do (return at))))

;;;; Which handler answers a verb

(defun minors (buf)
  "BUF's minor modes, most specific first.

A minor mode augments whatever major mode is on, so several may claim the same
verb; /minor/?m/precedence orders them and the highest is asked first."
  (let ((names (ns:read (p:path /buf buf :minor))))
    (sort (if (fset:set? names) (fset:convert 'list names) names)
          #'>
          :key (lambda (m) (or (ns:read (p:path /minor m :precedence)) 0)))))

(defun handler (buf verb)
  "What answers VERB for BUF: a minor mode's :on entry by precedence, then the
major mode's and its parents'. NIL means the built-in verb answers.

Lookup, not method combination, so the whole of the dispatch reads back:
(read /mode/lisp/on/*) is exactly what lisp-mode overrides."
  (or (loop :for m :in (minors buf)
            :for fn = (ns:read (p:path /minor m :on verb))
            :when fn :do (return fn))
      (loop :for mode :in (chain (ns:read (p:path /buf buf :mode)))
            :for fn = (ns:read (p:path /mode mode :on verb))
            :when fn :do (return fn))))

(defun minor-p (name)
  "True when NAME is a minor mode: something is written at /minor/?name."
  (and name (ns:read (p:path /minor name)) t))

(defun names ()
  "Every mode there is, majors and minors."
  (append (mapcar (lambda (path) (p:key (p:leaf path)))
                  (pine.data:keys (ns:read /mode/* {})))
          (mapcar (lambda (path) (p:key (p:leaf path)))
                  (pine.data:keys (ns:read /minor/* {})))))

;;;; The modes pine ships. A mode is a map, so this is the whole of defining
;;;; them: no class, no defmode, no registration step. What a machine or a
;;;; config adds is another write.

(defun provider ()
  (ns:provider
   (/mode/?name
    {:doc "a mode: :parent :grammar :indicator :files :comment :indent :on"})
   (/mode {:doc "every mode there is"})))

(defun mount ()
  (ns:write /mode (provider))
  (ns:write /mode/text {:indicator "Text"})
  (ns:write /mode/prog {:parent :text
                        :indent {:width 2}
                        :comment {:line ";"}})
  (ns:write /mode/lisp {:parent :prog
                        :grammar :commonlisp
                        :indicator "Lisp"
                        :files ["*.lisp" "*.asd" "*.cl" "*.lsp"]
                        :comment {:line ";;"}
                        :indent {:width 2}
                        ;; a newline in lisp lands now and takes its column
                        ;; when the parse says what it is
                        :on {:newline (pine.data:fn [buf]
                                        (fset:map ((p:path /buf buf :text)
                                                   [:newline])
                                                  ((p:path /buf buf)
                                                   [:indent-line])))}})
  (ns:write /mode/debugger {:parent :text :indicator "Debug"})
  (ns:write /minor/minibuffer {:precedence 20})
  (ns:write /minor/layout {:precedence 15})
  nil)
