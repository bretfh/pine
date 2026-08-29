(in-package #:pine/text)

(named-readtables:in-readtable pine/fs/reader:syntax)

(defvar *compiled* (d:table))
(defvar *inferrers* (d:table)
  "How to guess what a head means where the declaration says nothing, by language.
A registry: a language adds itself here, and nothing here knows which languages
there are.")

(defun infers (language rule)
  "Say RULE guesses for LANGUAGE what the declaration did not spell out."
  (d:keep! *inferrers* language rule)
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
    (d:map :options options :node nodes :head heads :otherwise otherwise)))

(defun %inherit (parent raw)
  (if (null parent)
      raw
      (let ((nodes (make-hash-table :test 'equal))
            (heads (make-hash-table :test 'equal)))
        (maphash (lambda (k v) (setf (gethash k nodes) v)) (d:lookup parent :node))
        (maphash (lambda (k v) (setf (gethash k heads) v)) (d:lookup parent :head))
        (maphash (lambda (k v) (setf (gethash k nodes) v)) (d:lookup raw :node))
        (maphash (lambda (k v) (setf (gethash k heads) v)) (d:lookup raw :head))
        (d:map :options (d:merged (d:lookup parent :options) (d:lookup raw :options))
                :node nodes :head heads
                :otherwise (or (d:lookup raw :otherwise) (d:lookup parent :otherwise))))))

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
         (cond (n (d:map :face :keyword :rest :body :indent n))
               ((%by-name name) {:face :keyword :rest :body})
               (t {:face :keyword}))))
      ((and (fboundp sym) (eq (symbol-package sym) (find-package :cl)))
       {:face :builtin})
      ((and (boundp sym) (constantp sym)) {:face :function-call :constant t})
      (t (%by-name name)))))

(infers :commonlisp #'%commonlisp-rule)

(defun %names (set)
  (let ((out (make-hash-table :test 'equal)))
    (d:do-each (name set out)
      (setf (gethash (string-downcase (string name)) out) t))))

(defun %compile (name raw)
  (let* ((options (d:lookup raw :options))
         (indent (d:lookup options :indent))
         (infer (d:lookup (d:all *inferrers*) name)))
    (make-language
     :name name
     :grammar (d:lookup options :grammar)
     :indent-width (or (d:lookup indent :width) 2)
     :nodes (d:lookup raw :node)
     :heads (d:lookup raw :head)
     :otherwise (d:lookup raw :otherwise)
     :constants (%names (d:lookup options :constants))
     :infer infer
     :raw raw)))

(defun declare-language (name raw &key parent)
  (let ((full (%inherit (and parent (%raw parent)) raw)))
    (d:keep! *compiled* name (cons full (%compile name full)))
    (when (tree:root)
      (setf (node:contents (tree:ensure "/lang" (string-downcase (string name))))
            (d:lookup (d:lookup full :options) :doc)))
    name))

(defun %raw (name)
  (car (d:lookup (d:all *compiled*) name)))

(defun for (name)
  (cdr (d:lookup (d:all *compiled*) name)))

(defun languages ()
  (sort (d:keys (d:all *compiled*)) #'string< :key #'string))

(defmethod readtable-of ((name symbol))
  "The readtable a language is written in, when it says: a language whose
reader is not the standard one names it here."
  (let ((said (d:lookup (d:lookup (%raw name) :options) :readtable)))
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
         (g (and lang (lang-grammar lang))))
    (when g (values (d:lookup g :lib) (d:lookup g :fn)))))

(defun lang-node (root)
  "One node per language declared, saying what it is for."
  (dolist (name (languages) (tree:ensure root "lang"))
    (setf (node:contents (tree:ensure root "lang" (string-downcase (string name))))
          (d:lookup (d:lookup (%raw name) :options) :doc))))

(defun %state (runtime name)
  (multiple-value-bind (lib fn) (grammar-of name)
    (when lib
      (make-parse-state runtime name lib fn :syntax (for name)))))

(defun compute-highlights (runtime language text)
  (let ((ps (%state runtime language)))
    (when ps
      (unwind-protect
           (progn
             (parse-lines!
              ps (uiop:split-string text :separator '(#\Newline)))
             (parse-highlights ps))
        (free-parse-state ps)))))

(defun hl-dump (source &optional (language :commonlisp))
  (let* ((runtime (make-ts-runtime))
         (ps (progn (ensure-ts runtime)
                    (%state runtime language))))
    (if (null ps)
        (format t "~&no grammar loaded for ~a~%" language)
        (let ((lines (coerce (uiop:split-string source :separator '(#\Newline))
                             'vector)))
          (parse-lines!
           ps (uiop:split-string source :separator '(#\Newline)))
          (dolist (h (parse-highlights ps))
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
