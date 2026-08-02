(defpackage #:pine.mode
  (:use #:cl)
  (:local-nicknames (#:ns #:pine.ns) (#:p #:pine.path))
  (:export #:chain #:setting #:for-file #:minors #:handler #:matches-p
           #:minor-p #:names #:server))

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

(defun %claims ()
  "Each mode that claims file types, with the patterns it claims."
  (let (acc)
    (fset:do-map (path value (ns:read /mode/*/files {}))
      (push (cons (p:key (p:leaf (p:parent path))) (%patterns value)) acc))
    (nreverse acc)))

(defun for-file (path)
  "The mode whose :files cover PATH, or NIL."
  (let ((name (file-namestring (pathname path))))
    (loop :for (at . patterns) :in (%claims)
          :when (some (lambda (pattern) (matches-p pattern name)) patterns)
            :do (return at))))

(defun minors (buf)
  "BUF's minor modes, highest /minor/?m/precedence first."
  (let ((names (ns:read (p:path /buf buf :minor))))
    (sort (if (fset:set? names) (fset:convert 'list names) names)
          #'>
          :key (lambda (m) (or (ns:read (p:path /minor m :precedence)) 0)))))

(defun handler (buf verb)
  "What answers VERB for BUF: a minor mode's :on entry by precedence, then the
major mode's and its parents'. NIL means the built-in verb answers."
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

(defun %overwritten (line col text)
  (let ((before (subseq line 0 (min col (length line))))
        (after (subseq line (min (length line) (+ col (length text))))))
    (concatenate 'string before text after)))

(defun overwrite ()
  (ns:write
   /minor/overwrite
   {:precedence 10
    :indicator "Ovwrt"
    :on {:insert
         (pine.data:fn [buf text]
           (let* ((point (ns:read (p:path /buf buf :point)))
                  (line (or (fset:lookup point 0) 0))
                  (col (or (fset:lookup point 1) 0))
                  (at (p:path /buf buf :line (princ-to-string line)))
                  (had (or (ns:read at) "")))
             (fset:map (at (%overwritten had col text))
                       ((p:path /buf buf :point)
                        (fset:seq line (+ col (length text)))))))}}))

(defun provider ()
  (ns:provider
   (/mode/?name
    {:doc "a mode: :parent :grammar :indicator :files :comment :indent :on"})
   (/mode {:doc "every mode there is"})))

(defclass server (ns:server) ()
  (:default-initargs :name :mode :serves (list /mode /minor))
  (:documentation "Modes and minor modes, and the ones pine ships."))

(defmethod ns:raise ((s server) &key &allow-other-keys)
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
  (overwrite)
  (ns:write /minor/minibuffer {:precedence 20})
  (ns:write /minor/layout {:precedence 15})
  nil)

(ns:register (make-instance 'server))
