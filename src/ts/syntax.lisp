(defpackage #:pine.ts.syntax
  (:use #:cl)
  (:local-nicknames (#:pl #:pine.data) (#:hl #:pine.ts.highlight)
                    (#:node #:pine.fs.node) (#:tree #:pine.fs.tree)
                    (#:world #:pine.world.world) (#:path #:pine.path.path))
  (:export #:language #:declare-language #:for #:grammar-of #:languages
           #:for-readtable #:readtable-of
           #:install #:compute-highlights #:hl-dump #:hl-dump-file
           #:*inferrers*))

(in-package #:pine.ts.syntax)
(named-readtables:in-readtable pine.path.reader:syntax)

(defvar *compiled* (make-hash-table :test 'equal))

(defparameter *inferrers* nil)

(defmacro language (options &rest clauses)
  `(%language ,options
              (list ,@(loop :for (p map) :in clauses
                            :collect `(cons (path:text ,p) ,map)))))

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
        (maphash (lambda (k v) (setf (gethash k nodes) v)) (pl:at parent :node))
        (maphash (lambda (k v) (setf (gethash k heads) v)) (pl:at parent :head))
        (maphash (lambda (k v) (setf (gethash k nodes) v)) (pl:at raw :node))
        (maphash (lambda (k v) (setf (gethash k heads) v)) (pl:at raw :head))
        (pl:map :options (pl:merged (pl:at parent :options) (pl:at raw :options))
                :node nodes :head heads
                :otherwise (or (pl:at raw :otherwise) (pl:at parent :otherwise))))))

(defun %symbol (name package)
  (let ((upper (string-upcase name)))
    (or (and package (find-symbol upper package))
        (find-symbol upper :cl)
        (find-symbol upper :cl-user))))

(defun %body-position (sym)
  (let ((args (ignore-errors (sb-introspect:function-lambda-list sym))))
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

(setf *inferrers* (list (cons :commonlisp #'%commonlisp-rule)))

(defun %names (set)
  (let ((out (make-hash-table :test 'equal)))
    (pl:do-each (name set out)
      (setf (gethash (string-downcase (string name)) out) t))))

(defun %compile (name raw)
  (let* ((options (pl:at raw :options))
         (indent (pl:at options :indent))
         (infer (cdr (assoc name *inferrers*))))
    (hl:make-language
     :name name
     :grammar (pl:at options :grammar)
     :indent-width (or (pl:at indent :width) 2)
     :nodes (pl:at raw :node)
     :heads (pl:at raw :head)
     :otherwise (pl:at raw :otherwise)
     :constants (%names (pl:at options :constants))
     :infer infer
     :raw raw)))

(defun declare-language (name raw &key parent)
  (let ((full (%inherit (and parent (%raw parent)) raw)))
    (setf (gethash name *compiled*) (cons full (%compile name full)))
    (when world:*world*
      (setf (node:contents (world:ensure world:*world* "syntax"
                                         (string-downcase (string name))))
            (pl:at (pl:at full :options) :doc)))
    name))

(defun %raw (name)
  (car (gethash name *compiled*)))

(defun for (name)
  (cdr (gethash name *compiled*)))

(defun languages ()
  (sort (loop :for k :being :the :hash-keys :of *compiled* :collect k)
        #'string< :key #'string))

(defun readtable-of (name)
  "The readtable a language is written in, when it says: a language whose
reader is not the standard one names it here."
  (let ((said (pl:at (pl:at (%raw name) :options) :readtable)))
    (when said (ignore-errors (named-readtables:find-readtable said)))))

(defun for-readtable (readtable)
  "The language written in READTABLE. This is what a buffer's own
(in-readtable) picks: the file says what it is written in, and the grammar
follows it rather than the path it happens to be under."
  (when readtable
    (find-if (lambda (name) (eq readtable (readtable-of name))) (languages))))

(defun grammar-of (name)
  (let* ((lang (for name))
         (g (and lang (hl:lang-grammar lang))))
    (when g (values (pl:at g :lib) (pl:at g :fn)))))

(defun install (&optional (w world:*world*))
  (dolist (name (languages) w)
    (setf (node:contents (world:ensure w "syntax" (string-downcase (string name))))
          (pl:at (pl:at (%raw name) :options) :doc))))

(defun %state (runtime name)
  (multiple-value-bind (lib fn) (grammar-of name)
    (when lib
      (pine.ts.runtime:make-parse-state runtime name lib fn :syntax (for name)))))

(defun compute-highlights (runtime language text)
  (let ((ps (%state runtime language)))
    (when ps
      (unwind-protect
           (progn
             (pine.ts.runtime:parse-lines!
              ps (uiop:split-string text :separator '(#\Newline)))
             (hl:parse-highlights ps))
        (pine.ts.runtime:free-parse-state ps)))))

(defun hl-dump (source &optional (language :commonlisp))
  (let* ((runtime (pine.ts.runtime:make-ts-runtime))
         (ps (progn (pine.ts.runtime:ensure-ts runtime)
                    (%state runtime language))))
    (if (null ps)
        (format t "~&no grammar loaded for ~a~%" language)
        (let ((lines (coerce (uiop:split-string source :separator '(#\Newline))
                             'vector)))
          (pine.ts.runtime:parse-lines!
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
