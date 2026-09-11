(in-package #:pine/text)

(named-readtables:in-readtable pine/fs/reader:syntax)

(defclass lang (fs:value)
  ((compiled :initarg :compiled :reader compiled)))

(defmethod fs:persistent-p ((l lang)) nil)

(defgeneric infers (language name package)
  (:method (language name package)
    (declare (ignore language name package))
    nil))

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
        (let ((parts (fs:split-name where)))
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
      (d:map :face :keyword :rest :body))))

(defun %commonlisp-rule (name package)
  (let ((sym (%symbol name package)))
    (cond
      ((null sym) (%by-name name))
      ((special-operator-p sym) (d:map :face :keyword :rest :body))
      ((macro-function sym)
       (let ((n (%body-position sym)))
         (cond (n (d:map :face :keyword :rest :body :indent n))
               ((%by-name name) (d:map :face :keyword :rest :body))
               (t (d:map :face :keyword)))))
      ((and (fboundp sym) (eq (symbol-package sym) (find-package :cl)))
       (d:map :face :builtin))
      ((and (boundp sym) (constantp sym)) (d:map :face :function-call :constant t))
      (t (%by-name name)))))

(defmethod infers ((language (eql :commonlisp)) name package)
  (%commonlisp-rule name package))

(defun %names (set)
  (let ((out (make-hash-table :test 'equal)))
    (d:do-each (name set out)
      (setf (gethash (string-downcase (string name)) out) t))))

(defun %compile (name raw)
  (let* ((options (d:lookup raw :options))
         (indent (d:lookup options :indent))
         (infer (lambda (head package) (infers name head package))))
    (make-rules
     :name name
     :grammar (d:lookup options :grammar)
     :indent-width (or (d:lookup indent :width) 2)
     :nodes (d:lookup raw :node)
     :heads (d:lookup raw :head)
     :otherwise (d:lookup raw :otherwise)
     :constants (%names (d:lookup options :constants))
     :infer infer
     :raw raw)))

(fs:mount (lambda () (make-instance 'fs:mount :describes "every language declared"))
          "/lang")

(defun %langs () (fs:at "/lang"))

(defun declare-language (name raw &key parent)
  (let ((full (%inherit (and parent (%raw parent)) raw)))
    (fs:mount (lambda () (make-instance 'lang :held full :compiled (%compile name full)))
              (format nil "/lang/~a" (string-downcase (string name))))
    name))

(defun %declared (name)
  (let ((it (fs:child (%langs) (string-downcase (string name)))))
    (and (typep it 'lang) it)))

(defun %raw (name)
  (let ((it (%declared name))) (and it (fs:contents it))))

(defun for (name)
  (let ((it (%declared name))) (and it (compiled it))))

(defun languages ()
  (sort (loop :for each :in (fs:children (%langs))
              :when (typep each 'lang)
                :collect (intern (string-upcase (fs:name each)) :keyword))
        #'string< :key #'string))

(defmethod readtable-of ((name symbol))
  (let ((said (d:lookup (d:lookup (%raw name) :options) :readtable)))
    (when said (fault:or-nothing "a declaration may name no readtable"
                 (named-readtables:find-readtable said)))))

(defun for-readtable (readtable)
  (when readtable
    (find-if (lambda (name) (eq readtable (readtable-of name))) (languages))))

(defun grammar-of (name)
  (let* ((lang (for name))
         (g (and lang (rules-grammar lang))))
    (when g (values (d:lookup g :lib) (d:lookup g :fn)))))

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
              ps (d:as :seq (uiop:split-string text :separator '(#\Newline))))
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
           ps (d:as :seq (uiop:split-string source :separator '(#\Newline))))
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
