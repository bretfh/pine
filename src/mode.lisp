(defpackage #:pine.mode
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path))
  (:export #:chain #:setting #:for-file #:minors #:claimants #:handler
           #:matches-p #:minor-p #:names #:server
           #:producers #:answer #:+merged+))

(in-package #:pine.mode)
(named-readtables:in-readtable pine.path:syntax)

;;;; A mode is a map at /mode/?name: everything declarative is a leaf and
;;;; everything behavioural is a handler under :on, so writing the mode is the
;;;; whole of registering it.

(defun chain (name)
  "NAME and the modes it falls back to, most specific first. A cycle someone
wrote by hand ends the walk rather than hanging."
  (loop :with seen = nil
        :for at = name :then (ns:read (p:path /mode at :parent))
        :while (and at (not (member at seen)))
        :do (push at seen)
        :collect at))

(defun setting (name key &optional default)
  "What KEY is for mode NAME, taking the first mode up the chain that says."
  (loop :for mode :in (chain name)
        :for value = (ns:read (p:path /mode mode key))
        :when value :do (return value)
        :finally (return default)))

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

(defun %claims (leaf)
  "Each mode that claims files by LEAF, with the patterns it claims."
  (let (acc)
    (fset:do-map (path value (ns:read (p:path /mode (p:any) leaf) {}))
      (push (cons (p:key (p:leaf (p:parent path))) (%patterns value)) acc))
    (nreverse acc)))

(defun %claimed (claims against)
  (loop :for (at . patterns) :in claims
        :when (some (lambda (pattern) (matches-p pattern against)) patterns)
          :do (return at)))

(defun for-file (path)
  "The mode whose :paths or :files cover PATH, or NIL.

:PATHS is matched against the whole namestring and asked first, because where a
file is can say more than what it is called: every .lisp under a config
directory is written in the config's own reader, and nothing about the name
says so."
  (let ((full (namestring (pathname path))))
    (or (%claimed (%claims :paths) full)
        (%claimed (%claims :files) (file-namestring (pathname path))))))

(defun minors (buf)
  "BUF's minor modes, highest /minor/?m/precedence first."
  (let ((names (ns:read (p:path /buf buf :minor))))
    (sort (if (fset:set? names) (fset:convert 'list names) names)
          #'>
          :key (lambda (m) (or (ns:read (p:path /minor m :precedence)) 0)))))

(defun claimants (buf verb)
  "Every handler for VERB on BUF, most specific first: the minor modes by
precedence, then the mode and everything it falls back to.

A handler that writes the verb it claimed reaches the next of these, and the
built-in only once they run out. So a mode may lay itself over another rather
than only replacing it, and the whole chain is still one read."
  (append (loop :for m :in (minors buf)
                :for fn = (ns:read (p:path /minor m :on verb))
                :when fn :collect fn)
          (loop :for mode :in (chain (ns:read (p:path /buf buf :mode)))
                :for fn = (ns:read (p:path /mode mode :on verb))
                :when fn :collect fn)))

(defun handler (buf verb)
  "The first thing that answers VERB for BUF. NIL means the built-in does."
  (first (claimants buf verb)))

;;;; What a mode answers about a buffer, as against what it does to one. A
;;;; producer is a function at /mode/?name/answers/?verb, and a mode that
;;;; registers one gives every surface asking that verb a better answer without
;;;; any of them hearing about the mode. Nothing reaches past this: a command
;;;; asks the buffer, the buffer asks the chain.

(defparameter +merged+ '(:references :complete :diagnostics)
  "The verbs whose answer is a set, so every producer contributes to it. The
rest are about a point, and the most specific producer that answers wins.")

(defun producers (buf verb)
  "Every producer for VERB on BUF, most specific first: the minor modes by
precedence, then the mode and everything it falls back to."
  (append (loop :for m :in (minors buf)
                :for fn = (ns:read (p:path /minor m :answers verb))
                :when fn :collect fn)
          (loop :for mode :in (chain (ns:read (p:path /buf buf :mode)))
                :for fn = (ns:read (p:path /mode mode :answers verb))
                :when fn :collect fn)))

(defun answer (buf verb &optional of)
  "What BUF's modes say about VERB, OF naming what is being asked about."
  (let ((all (producers buf verb)))
    (if (member verb +merged+)
        (loop :for fn :in all :append (funcall fn buf of))
        (loop :for fn :in all :thereis (funcall fn buf of)))))

(defun minor-p (name)
  "True when NAME is a minor mode: something is written at /minor/?name."
  (and name (ns:read (p:path /minor name)) t))

(defun names ()
  "Every mode there is, majors and minors."
  (append (mapcar (lambda (path) (p:key (p:leaf path)))
                  (pine.data:keys (ns:read /mode/* {})))
          (mapcar (lambda (path) (p:key (p:leaf path)))
                  (pine.data:keys (ns:read /minor/* {})))))

(defun overwrite ()
  (ns:write
   /minor/overwrite
   {:precedence 10
    :indicator "Ovwrt"
    :on {:insert
         (pine.data:fn [buf text]
           ;; take what the insert is about to cover, then write the verb again:
           ;; it lands on whatever claims :insert under this mode, and on the
           ;; built-in when nothing does. Nothing here inserts or moves point,
           ;; because the mode underneath already knows how
           (let* ((point (ns:read (p:path /buf buf :point)))
                  (line (or (fset:lookup point 0) 0))
                  (col (or (fset:lookup point 1) 0))
                  (had (or (ns:read (p:path /buf buf :line
                                            (princ-to-string line)))
                           ""))
                  (over (min (length text) (max 0 (- (length had) col)))))
             (when (plusp over)
               (ns:write (p:path /buf buf :text)
                         (fset:seq :delete (fset:seq line col)
                                   (fset:seq line (+ col over)))))
             (fset:map ((p:path /buf buf :text) (fset:seq :insert text)))))}}))

(defun provider ()
  (ns:provider
   (/mode/?name
    {:doc "a mode: :parent :grammar :grammars :indicator :files :paths :comment
:indent :on"})
   (/mode {:doc "every mode there is"})))

(defclass server (ns:server) ()
  (:default-initargs :name :mode :serves (list /mode /minor))
  (:documentation "Modes and minor modes, and the ones pine ships."))

(defmethod ns:raise ((s server) &key &allow-other-keys)
  (ns:write /mode (provider))
  (ns:write /mode/text {:indicator "Text"})
  ;; a config is written in pine's own reader, and nothing about the file name
  ;; says so: the loader supplies the readtable, so the mode says where. A
  ;; :paths pattern is matched against the whole namestring and asked before
  ;; :files, because where a file is can say more than what it is called.
  (ns:write /mode/pine {:parent :lisp
                        :indicator "Pine"
                        :readtable 'pine.path:syntax
                        ;; * covers any run including a slash, so this is both
                        ;; ~/.config/pine/init.lisp and the examples in the tree
                        :paths ["*/pine/*.lisp"]})
  (ns:write /mode/prog {:parent :text
                        :indent {:width 2}
                        :comment {:line ";"}})
  (ns:write /mode/lisp {:parent :prog
                        :grammar :commonlisp
                        ;; a reader says what its files are written in, so a
                        ;; config with its own defreadtable names the grammar
                        ;; it parses with by writing one leaf here
                        :grammars {'pine.path:syntax :pine 'pine.path:data :pine
                                   'pine.data:syntax :pine 'pine.data:data :pine}
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
  (overwrite)
  (ns:write /minor/minibuffer {:precedence 20})
  (ns:write /minor/layout {:precedence 15})
  nil)

(ns:register (make-instance 'server))
