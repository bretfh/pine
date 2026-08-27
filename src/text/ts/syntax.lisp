(defpackage #:pine/text/ts/syntax
  (:use #:cl)
  (:local-nicknames (#:fault #:pine/run/fault)
                    (#:pl #:pine/data) (#:hl #:pine/text/ts/highlight)
                    (#:node #:pine/fs/node) (#:tree #:pine/fs/tree)
                    (#:path #:pine/fs/path))
  (:export #:language #:declare-language #:for #:grammar-of #:languages
           #:for-readtable #:readtable-of
           #:lang-node #:compute-highlights #:hl-dump #:hl-dump-file
           #:infers))
(in-package #:pine/text/ts/syntax)

(named-readtables:in-readtable pine/fs/reader:syntax)

(defvar *compiled* (pl:table))
(defvar *inferrers* (pl:table)
  "How to guess what a head means where the declaration says nothing, by language.
A registry: a language adds itself here, and nothing here knows which languages
there are.")

(defun infers (language rule)
  "Say RULE guesses for LANGUAGE what the declaration did not spell out."
  (pl:keep! *inferrers* language rule)
  language)

(defmacro language (options &rest clauses)
  `(%language ,options
              (list ,@(loop :for (p map) :in clauses
                            :collect `(cons (path:whole ,p) ,map)))))

(defun %language (options clauses)
  (let ((nodes (make-hash-table :test 'equal))
        (heads (make-hash-table :test 'equal))
        (otherwise nil))
    (dolist (clause clauses)
      (destructuring-bind (where . rule) clause
        (let ((parts (tree:split-name where)))
          (cond ((equal '("otherwise") parts) (setf otherwise rule))
                ((equal "node" (first parts))
                 (setf (gethash (string-downcase (second parts)) nodes) rule))
                ((equal "head" (first parts))
                 (setf (gethash (string-downcase (second parts)) heads) rule))))))
    (pl:map :options options :node nodes :head heads :otherwise otherwise)))

(defun %inherit (parent raw)
  (if (null parent)
      raw
      (let ((nodes (make-hash-table :test 'equal))
            (heads (make-hash-table :test 'equal)))
        (maphash (lambda (k v) (setf (gethash k nodes) v)) (pl:lookup parent :node))
        (maphash (lambda (k v) (setf (gethash k heads) v)) (pl:lookup parent :head))
        (maphash (lambda (k v) (setf (gethash k nodes) v)) (pl:lookup raw :node))
        (maphash (lambda (k v) (setf (gethash k heads) v)) (pl:lookup raw :head))
        (pl:map :options (pl:merged (pl:lookup parent :options) (pl:lookup raw :options))
                :node nodes :head heads
                :otherwise (or (pl:lookup raw :otherwise) (pl:lookup parent :otherwise))))))

(defun %symbol (name package)
  (let ((upper (string-upcase name)))
    (or (and package (find-symbol upper package))
        (find-symbol upper :cl)
        (find-symbol upper :cl-user))))

(defun %body-position (sym)
  (let ((args (fault:or-nothing "a symbol may have no lambda list kept"
                (sb-introspect:function-lambda-list sym))))
    (loop :for a :in args
          :for i :from 0
          :when (and (symbolp a) (string= "&BODY" (symbol-name a)))
            :do (return i))))

(defun %by-name (name)
  (let ((n (length name)))
    (when (or (and (>= n 3) (string= "def" name :end2 3))
              (and (>= n 5) (string= "with-" name :end2 5))
              (and (>= n 3) (string= "do-" name :end2 3))
              (string= name "do")
              (string= name "loop"))
      {:face :keyword :rest :body})))

(defun %commonlisp-rule (name package)
  (let ((sym (%symbol name package)))
    (cond
      ((null sym) (%by-name name))
      ((special-operator-p sym) {:face :keyword :rest :body})
      ((macro-function sym)
       (let ((n (%body-position sym)))
         (cond (n (pl:map :face :keyword :rest :body :indent n))
               ((%by-name name) {:face :keyword :rest :body})
               (t {:face :keyword}))))
      ((and (fboundp sym) (eq (symbol-package sym) (find-package :cl)))
       {:face :builtin})
      ((and (boundp sym) (constantp sym)) {:face :function-call :constant t})
      (t (%by-name name)))))

(infers :commonlisp #'%commonlisp-rule)

(defun %names (set)
  (let ((out (make-hash-table :test 'equal)))
    (pl:do-each (name set out)
      (setf (gethash (string-downcase (string name)) out) t))))

(defun %compile (name raw)
  (let* ((options (pl:lookup raw :options))
         (indent (pl:lookup options :indent))
         (infer (pl:lookup (pl:all *inferrers*) name)))
    (hl:make-language
     :name name
     :grammar (pl:lookup options :grammar)
     :indent-width (or (pl:lookup indent :width) 2)
     :nodes (pl:lookup raw :node)
     :heads (pl:lookup raw :head)
     :otherwise (pl:lookup raw :otherwise)
     :constants (%names (pl:lookup options :constants))
     :infer infer
     :raw raw)))

(defun declare-language (name raw &key parent)
  (let ((full (%inherit (and parent (%raw parent)) raw)))
    (pl:keep! *compiled* name (cons full (%compile name full)))
    (when (tree:root)
      (setf (node:contents (tree:ensure nil "lang" (string-downcase (string name))))
            (pl:lookup (pl:lookup full :options) :doc)))
    name))

(defun %raw (name)
  (car (pl:lookup (pl:all *compiled*) name)))

(defun for (name)
  (cdr (pl:lookup (pl:all *compiled*) name)))

(defun languages ()
  (sort (pl:keys (pl:all *compiled*)) #'string< :key #'string))

(defun readtable-of (name)
  "The readtable a language is written in, when it says: a language whose
reader is not the standard one names it here."
  (let ((said (pl:lookup (pl:lookup (%raw name) :options) :readtable)))
    (when said (fault:or-nothing "a declaration may name no readtable"
                 (named-readtables:find-readtable said)))))

(defun for-readtable (readtable)
  "The language written in READTABLE. This is what a buffer's own
(in-readtable) picks: the file says what it is written in, and the grammar
follows it rather than the path it happens to be under."
  (when readtable
    (find-if (lambda (name) (eq readtable (readtable-of name))) (languages))))

(defun grammar-of (name)
  (let* ((lang (for name))
         (g (and lang (hl:lang-grammar lang))))
    (when g (values (pl:lookup g :lib) (pl:lookup g :fn)))))

(defun lang-node (root)
  "One node per language declared, saying what it is for."
  (dolist (name (languages) (tree:ensure root "lang"))
    (setf (node:contents (tree:ensure root "lang" (string-downcase (string name))))
          (pl:lookup (pl:lookup (%raw name) :options) :doc))))

(defun %state (runtime name)
  (multiple-value-bind (lib fn) (grammar-of name)
    (when lib
      (pine/text/ts/runtime:make-parse-state runtime name lib fn :syntax (for name)))))

(defun compute-highlights (runtime language text)
  (let ((ps (%state runtime language)))
    (when ps
      (unwind-protect
           (progn
             (pine/text/ts/runtime:parse-lines!
              ps (uiop:split-string text :separator '(#\Newline)))
             (hl:parse-highlights ps))
        (pine/text/ts/runtime:free-parse-state ps)))))

(defun hl-dump (source &optional (language :commonlisp))
  (let* ((runtime (pine/text/ts/runtime:make-ts-runtime))
         (ps (progn (pine/text/ts/runtime:ensure-ts runtime)
                    (%state runtime language))))
    (if (null ps)
        (format t "~&no grammar loaded for ~a~%" language)
        (let ((lines (coerce (uiop:split-string source :separator '(#\Newline))
                             'vector)))
          (pine/text/ts/runtime:parse-lines!
           ps (uiop:split-string source :separator '(#\Newline)))
          (dolist (h (hl:parse-highlights ps))
            (destructuring-bind (line start-col end-col face) h
              (let* ((text (if (< line (length lines)) (aref lines line) ""))
                     (end (min end-col (length text))))
                (format t "~&~2d:~2d  ~16a ~s~%" line start-col face
                        (subseq text start-col (max start-col end))))))))))

(defun hl-dump-file (path &optional (language :commonlisp))
  (with-open-file (s path :if-does-not-exist nil)
    (if (null s)
        (format t "~&no such file: ~a~%" path)
        (let ((source (make-string (file-length s))))
          (hl-dump (subseq source 0 (read-sequence source s)) language)))))
