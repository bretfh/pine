(defpackage #:pine/text/ts/runtime
  (:use :cl)
  (:local-nicknames (#:pl #:pine/data))
  (:export

   #:ts-runtime #:make-ts-runtime #:ts-loaded-p #:ensure-ts #:libs-loaded
   #:grammars #:ts-entry #:entry-parser #:entry-language-ptr #:ensure-language

   #:grammar-library-candidates #:load-grammar-library
   #:grammar-language-pointer #:load-language-entry

   #:parse-state #:make-parse-state #:free-parse-state
   #:parse-lines! #:parse-text! #:parse-motion
   #:ps-language #:ps-syntax #:ps-package #:ps-parser #:ps-tree
   #:ps-byte-index #:ps-lines #:ps-scratch #:ps-read-buffer
   #:ps-band #:ps-band-lines #:ps-offset
   #:call-with-input #:+read-chunk+
   #:ps-hl-cache #:ps-hl-lines #:ps-hl-pending #:ps-hl-stale #:ps-hl-window

   #:build-line-index #:line-of-byte #:byte-to-line-col #:pos-to-byte
   #:byte-length #:char-byte-length

   #:ts-parser-new #:ts-parser-delete #:ts-parser-set-language
   #:ts-parser-parse-string
   #:ts-tree-delete #:ts-tree-edit #:ts-tree-root-node
   #:ts-tree-get-changed-ranges
   #:ts-node-type #:ts-node-is-null #:ts-node-parent
   #:ts-node-start-byte #:ts-node-end-byte
   #:ts-node-named-nth #:ts-node-named-count
   #:ts-node-by-field-name #:ts-node-named-descendant-for-byte-range))
(in-package #:pine/text/ts/runtime)

(defconstant +input-encoding-utf8+ 0)

(defconstant +min-compatible-language-version+ 13)

(defconstant +language-version+ 15)

(defparameter +whole-file-lines+ 4096
  "Buffers this many lines or fewer are parsed entire. Above it only a band
around the window is, since the cost of a parse follows how many nodes the root holds
and the cost of a byte index follows the line count.")

(defparameter +band-lines+ 512
  "The band's granularity: it covers whole multiples of this many lines, so
scrolling within one moves it not at all.")

(defparameter +read-chunk+ 8192
  "Bytes served per read call.")

(defvar *reading* nil
  "(LINES INDEX SCRATCH SIZE) the read callback is serving, bound per parse.
The callback runs on the thread inside ts_parser_parse, synchronously, so this
is bound per thread rather than passed through the payload pointer.")

(defparameter +library-suffix+
  #+darwin ".dylib" #-darwin ".so"
  "What a shared library is called here. The loader's own search takes the
suffix off our hands; the paths below are spelled out, so they cannot.")

(cffi:define-foreign-library libtree-sitter
  (t (:default "libtree-sitter")))

(cffi:defcstruct ts-point (row :uint32) (column :uint32))

(cffi:defcstruct ts-range
  (start-point (:struct ts-point))
  (end-point   (:struct ts-point))
  (start-byte :uint32)
  (end-byte :uint32))

(cffi:defcstruct ts-node
  (c0 :uint32) (c1 :uint32) (c2 :uint32) (c3 :uint32)
  (id :pointer) (tree :pointer))

(cffi:defcstruct ts-input
  (payload :pointer)
  (read :pointer)
  (encoding :int)
  (decode :pointer))

(cffi:defcstruct ts-input-edit
  (start-byte :uint32) (old-end-byte :uint32) (new-end-byte :uint32)
  (start-point   (:struct ts-point))
  (old-end-point (:struct ts-point))
  (new-end-point (:struct ts-point)))

(cffi:defcfun ("ts_parser_new" ts-parser-new) :pointer)
(cffi:defcfun ("ts_parser_delete" ts-parser-delete) :void (parser :pointer))
(cffi:defcfun ("ts_parser_set_language" ts-parser-set-language) (:boolean :char)
  (parser :pointer) (language :pointer))
(cffi:defcfun ("ts_parser_set_included_ranges" ts-parser-set-included-ranges)
    (:boolean :char)
  (parser :pointer) (ranges :pointer) (count :uint32))
(cffi:defcfun ("ts_parser_parse_string" ts-parser-parse-string) :pointer
  (parser :pointer) (old-tree :pointer) (string :pointer) (length :uint32))
(cffi:defcfun ("ts_parser_parse" ts-parser-parse) :pointer
  (parser :pointer) (old-tree :pointer) (input (:struct ts-input)))
(cffi:defcfun ("ts_language_version" ts-language-version) :uint32
  (language :pointer))

(cffi:defcfun ("ts_tree_delete" ts-tree-delete) :void (tree :pointer))
(cffi:defcfun ("ts_tree_edit" ts-tree-edit) :void (tree :pointer) (edit :pointer))
(cffi:defcfun ("ts_tree_root_node" ts-tree-root-node) (:struct ts-node)
  (tree :pointer))
(cffi:defcfun ("ts_tree_get_changed_ranges" ts-tree-get-changed-ranges) :pointer
  (old-tree :pointer) (new-tree :pointer) (length (:pointer :uint32)))

(cffi:defcfun ("ts_node_start_byte" ts-node-start-byte) :uint32
  (node (:struct ts-node)))
(cffi:defcfun ("ts_node_end_byte" ts-node-end-byte) :uint32
  (node (:struct ts-node)))
(cffi:defcfun ("ts_node_parent" ts-node-parent) (:struct ts-node)
  (node (:struct ts-node)))
(cffi:defcfun ("ts_node_type" %ts-node-type) :pointer
  (node (:struct ts-node)))

(cffi:defcfun ("ts_node_is_null" %ts-node-is-null) :uint8
  (node (:struct ts-node)))
(cffi:defcfun ("ts_node_named_child_count" ts-node-named-count) :uint32
  (node (:struct ts-node)))
(cffi:defcfun ("ts_node_named_child" ts-node-named-nth) (:struct ts-node)
  (node (:struct ts-node)) (index :uint32))
(cffi:defcfun ("ts_node_child_by_field_name" ts-node-by-field-name)
    (:struct ts-node)
  (node (:struct ts-node)) (name :string) (name-length :uint32))
(cffi:defcfun ("ts_node_named_descendant_for_byte_range"
               ts-node-named-descendant-for-byte-range) (:struct ts-node)
  (node (:struct ts-node)) (start :uint32) (end :uint32))

(defclass ts-runtime ()
  ((libs-loaded :accessor libs-loaded :initform nil)

   (loading     :reader loading
                :initform (bordeaux-threads:make-recursive-lock "pine-grammars"))
   (grammars    :reader grammars
                :initform (pl:map :loaded (pl:no-map) :missing (pl:no-set))))
  (:documentation "The grammars this image has loaded: {:loaded {LANGUAGE ENTRY}
:missing #{LANGUAGE}}. A hit is a slot read."))

(defclass ts-entry ()
  ((parser       :initarg :parser       :accessor entry-parser)
   (language-ptr :initarg :language-ptr :accessor entry-language-ptr))
  (:documentation "One loaded grammar: a parser, and the language pointer kept
so a per-buffer parser can set-language."))

(defclass parse-state ()
  ((language :initarg :language :accessor ps-language)

   (syntax   :initarg :syntax   :accessor ps-syntax   :initform nil)
   (package  :initarg :package  :accessor ps-package  :initform nil)
   (parser   :initarg :parser   :accessor ps-parser)
   (tree     :initform nil      :accessor ps-tree)

   (hl-cache   :initform nil :accessor ps-hl-cache)
   (hl-lines   :initform nil :accessor ps-hl-lines)
   (hl-pending :initform nil :accessor ps-hl-pending)
   (hl-stale   :initform nil :accessor ps-hl-stale)

   (hl-window  :initform nil :accessor ps-hl-window)

   (byte-index :initform nil :accessor ps-byte-index)
   (lines      :initform nil :accessor ps-lines)
   (scratch    :initform nil :accessor ps-scratch)
   (band       :initform nil :accessor ps-band)
   (band-lines :initform nil :accessor ps-band-lines))
  (:documentation "One buffer's parser, its persistent tree, and the lines the
tree reflects. Per-buffer because a TSParser is not thread-safe and different
buffers parse concurrently."))

(defun make-ts-runtime () (make-instance 'ts-runtime))

(defun ts-loaded-p (runtime) (libs-loaded runtime))

(defun ts-min-compatible-version () +min-compatible-language-version+)

(defun ts-language-version-max () +language-version+)

(defun ts-node-is-null (node)
  "True when NODE is tree-sitter's null node."
  (logbitp 0 (%ts-node-is-null node)))

(defun ts-node-type (node)
  (cffi:foreign-string-to-lisp (%ts-node-type node) :encoding :ascii))

(defun ensure-ts (runtime)
  "Load libtree-sitter once. A pine built without it still edits, so this
reports rather than refuses and LIBS-LOADED stays false. Whether it is here is
asked of the loader, not of this runtime: cffi's LOAD-FOREIGN-LIBRARY closes the
library it is about to open, so a second runtime loading it again unmaps the
copy the first one is still pointing into."
  (unless (libs-loaded runtime)
    (handler-case
        (progn (unless (cffi:foreign-symbol-pointer "ts_parser_new")
                 (cffi:load-foreign-library 'libtree-sitter))
               (setf (libs-loaded runtime) t))
      (error (c)
        (pine/run/fault:report c "loading libtree-sitter"))))
  runtime)

(defun grammar-library-candidates (library-name)
  "Places a grammar shared library may live: the loader's search path (Guix
puts grammars under lib/tree-sitter/, not lib/), and pine's own tree, which is
where a build outside Guix puts them."
  (let ((file (concatenate 'string library-name +library-suffix+))
        (env (sb-ext:posix-getenv "GUIX_ENVIRONMENT")))
    (remove nil
            (list (list :default library-name)
                  (when env (format nil "~a/lib/tree-sitter/~a" env file))
                  (format nil "~alib/tree-sitter/~a"
                          (namestring (asdf:system-source-directory :pine))
                          file)))))

(defun load-grammar-library (library-name)
  (loop for candidate in (grammar-library-candidates library-name)
        thereis (ignore-errors (cffi:load-foreign-library candidate))))

(defun grammar-language-pointer (library-name fn-name)
  "Call FN-NAME for its TSLanguage*, loading grammar LIBRARY-NAME first when the
symbol is not already here, or NIL. The symbol decides, not a table: cffi's
LOAD-FOREIGN-LIBRARY dlcloses before it dlopens, so loading a grammar that is
already loaded unmaps it and every TSLanguage* handed out before points into a
mapping that is gone."
  (handler-case
      (let ((fn (or (cffi:foreign-symbol-pointer fn-name)
                    (and (load-grammar-library library-name)
                         (cffi:foreign-symbol-pointer fn-name)))))
        (when fn (cffi:foreign-funcall-pointer fn () :pointer)))
    (error (c)
      (pine/run/fault:report
       c (format nil "loading the ~a grammar" library-name)))))

(defun claim-language (parser lang language)
  "Give PARSER the grammar LANG, and say so when it will not take it. A parser
with no language answers every parse with a null tree, so the refusal names the
two pointers and the grammar's ABI against the range this tree-sitter speaks."
  (or (ts-parser-set-language parser lang)
      (progn
        (pine/run/fault:report
         (make-condition
          'simple-error
          :format-control "~(~a~) refused: parser ~a, language ~a, abi ~a, ~
                           this tree-sitter speaks ~a..~a, set-language at ~a"
          :format-arguments (list language parser lang
                                  (ignore-errors (ts-language-version lang))
                                  (ts-min-compatible-version)
                                  (ts-language-version-max)
                                  (cffi:foreign-symbol-pointer
                                   "ts_parser_set_language")))
         "setting a grammar")
        nil)))
