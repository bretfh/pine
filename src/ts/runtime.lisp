(defpackage #:pine.ts.runtime
  (:use :cl)
  (:export
   ;; the runtime and its per-language entries
   #:ts-runtime #:make-ts-runtime #:ts-loaded-p #:ensure-ts #:libs-loaded
   #:*grammar-of* #:grammars #:ts-entry #:entry-parser #:entry-language-ptr #:ensure-language
   ;; grammars
   #:grammar-library-candidates #:load-grammar-library
   #:grammar-language-pointer #:load-language-entry
   ;; the per-buffer incremental parse state
   #:parse-state #:make-parse-state #:free-parse-state
   #:parse-lines! #:parse-text! #:parse-motion
   #:ps-language #:ps-syntax #:ps-package #:ps-parser #:ps-tree
   #:ps-byte-index #:ps-lines #:ps-scratch #:ps-read-buffer
   #:ps-band #:ps-band-lines #:ps-offset
   #:call-with-input #:+read-chunk+
   #:ps-hl-cache #:ps-hl-lines #:ps-hl-pending #:ps-hl-stale #:ps-hl-window
   ;; structural motion
   #:call-with-root #:forward-sexp-pos #:backward-sexp-pos #:defun-bounds-pos
   ;; bytes, characters and lines
   #:build-line-index #:line-of-byte #:byte-to-line-col #:pos-to-byte
   #:byte-length #:char-byte-length
   ;; the tree-sitter C surface the language walks read
   #:ts-parser-new #:ts-parser-delete #:ts-parser-set-language
   #:ts-parser-parse-string
   #:ts-tree-delete #:ts-tree-edit #:ts-tree-root-node
   #:ts-tree-get-changed-ranges
   #:ts-node-type #:ts-node-is-null #:ts-node-parent
   #:ts-node-start-byte #:ts-node-end-byte
   #:ts-node-named-nth #:ts-node-named-count
   #:ts-node-by-field-name #:ts-node-named-descendant-for-byte-range))

(in-package #:pine.ts.runtime)
(named-readtables:in-readtable pine.data:syntax)

;;;; libtree-sitter, the grammars, and one parse state per buffer.
;;;;
;;;; tree-sitter passes TSNode by value; cffi-libffi handles the by-value
;;;; struct, so no C wrapper is needed. The context[4] array in TSNode is
;;;; flattened to four scalars because CFFI has no by-value translation for an
;;;; array struct member; the fields are never read from Lisp, only passed back
;;;; to ts_node_*.
;;;;
;;;; ts_parser_parse takes a TSInput by value and calls its read callback for
;;;; the byte ranges it needs, which after an edit is a small neighbourhood.
;;;; Serving those from the fset seq means no flat copy of the buffer exists: at
;;;; a million lines the copy alone was 43 ms and 88 MB of garbage per keystroke.

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

;;;; TSPoint.column is a BYTE offset within its row. ts_tree_edit shifts a
;;;; persistent tree's positions by an edit descriptor; the next parse with that
;;;; tree as old-tree reuses the unchanged subtrees instead of re-lexing.

(cffi:defcstruct ts-input-edit
  (start-byte :uint32) (old-end-byte :uint32) (new-end-byte :uint32)
  (start-point   (:struct ts-point))
  (old-end-point (:struct ts-point))
  (new-end-point (:struct ts-point)))

(defconstant +input-encoding-utf8+ 0)

;; the range a grammar's ABI is checked against is a compile-time constant in
;; tree-sitter's header, not a symbol the library exports
(defconstant +min-compatible-language-version+ 13)
(defconstant +language-version+ 15)

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
;; the C is a one-byte bool and the bytes above it in the return register are
;; undefined, so it is read as the byte it is: as an int a live node came back
;; null often enough to lose a file's colour. (:boolean :char) will not do
;; either, because the by-value struct argument goes through libffi, which will
;; not narrow a return
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

(defclass ts-runtime ()
  ((libs-loaded :accessor libs-loaded :initform nil)
   ;; loading a grammar is a dlopen and a symbol lookup through cffi's own
   ;; registry, and that registry is one table shared by every thread. Ten
   ;; buffers opening at once would otherwise each start the same load, and a
   ;; symbol looked up while another thread is registering the library it lives
   ;; in comes back pointing at nothing a parser will take. One at a time.
   (loading     :reader loading
                :initform (bordeaux-threads:make-recursive-lock "pine-grammars"))
   (grammars    :reader grammars
                :initform (sento.atomic:make-atomic-reference
                           :value {:loaded (fset:empty-map)
                                   :missing (fset:empty-set)})))
  (:documentation "The grammars this image has loaded: {:loaded {LANGUAGE ENTRY}
:missing #{LANGUAGE}} in an atomic reference. A hit is a slot read."))

(defclass ts-entry ()
  ((parser       :initarg :parser       :accessor entry-parser)
   (language-ptr :initarg :language-ptr :accessor entry-language-ptr))
  (:documentation "One loaded grammar: a parser, and the language pointer kept
so a per-buffer parser can set-language."))

(defclass parse-state ()
  ((language :initarg :language :accessor ps-language)
   ;; the compiled rules the walk follows, and the package a head symbol is
   ;; resolved in. Both are the language's, not the parser's, so they are
   ;; handed in rather than worked out here.
   (syntax   :initarg :syntax   :accessor ps-syntax   :initform nil)
   (package  :initarg :package  :accessor ps-package  :initform nil)
   (parser   :initarg :parser   :accessor ps-parser)
   (tree     :initform nil      :accessor ps-tree)
   ;; the last full tuple list, the lines it was computed for, and the one edit
   ;; recorded since (rows in the NEW lines plus the line delta old->new). More
   ;; than one edit between highlight calls, or any doubt, sets hl-stale and the
   ;; next call walks the whole tree. Cache identity is EQ on the lines, which
   ;; is sound because the seq is immutable.
   (hl-cache   :initform nil :accessor ps-hl-cache)
   (hl-lines   :initform nil :accessor ps-hl-lines)
   (hl-pending :initform nil :accessor ps-hl-pending)
   (hl-stale   :initform nil :accessor ps-hl-stale)
   ;; the line range the cache covers as (FROM . TO), or nil for the whole file
   (hl-window  :initform nil :accessor ps-hl-window)
   ;; the byte index over the seq the tree reflects, and the foreign buffer the
   ;; read callback serves out of
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
        (pine.err:report-failure c "loading libtree-sitter"))))
  runtime)

(defun grammar-library-candidates (library-name)
  "Places a grammar shared library may live: the loader's search path (Guix
puts grammars under lib/tree-sitter/, not lib/) and pine's own tree."
  (let ((so (concatenate 'string library-name ".so"))
        (env (sb-ext:posix-getenv "GUIX_ENVIRONMENT")))
    (remove nil
            (list (list :default library-name)
                  (when env (format nil "~a/lib/tree-sitter/~a" env so))
                  (format nil "~alib/tree-sitter/~a"
                          (namestring (asdf:system-source-directory :pine)) so)))))

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
      (pine.err:report-failure
       c (format nil "loading the ~a grammar" library-name)))))

(defun claim-language (parser lang language)
  "Give PARSER the grammar LANG, and say so when it will not take it. A parser
with no language answers every parse with a null tree, so the refusal names the
two pointers and the grammar's ABI against the range this tree-sitter speaks."
  (or (ts-parser-set-language parser lang)
      (progn
        (pine.err:report-failure
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

(defvar *grammar-of* nil
  "How a language name answers (values LIBRARY FN-NAME). Whatever declares
languages sets this; until something does, no grammar loads. It is here rather
than a table in this file so that what grammars exist is written rather than
compiled in.")

(defun %grammar-of (language)
  (if *grammar-of* (funcall *grammar-of* language) (values nil nil)))

(defun load-language-entry (language library fn-name)
  (progn
    (unless library (return-from load-language-entry nil))
    (let ((lang (grammar-language-pointer library fn-name)))
      (when (and lang (not (cffi:null-pointer-p lang)))
        (pine.log:note "loaded ~(~a~): language ~a abi ~a, set-language at ~a"
                       language lang
                       (ignore-errors (ts-language-version lang))
                       (cffi:foreign-symbol-pointer "ts_parser_set_language"))
        (let ((parser (ts-parser-new)))
          (when (claim-language parser lang language)
            (make-instance 'ts-entry :parser parser :language-ptr lang)))))))

(defun %grammars (runtime)
  (sento.atomic:atomic-get (grammars runtime)))

(defun %note-loaded (runtime language entry)
  (sento.atomic:atomic-swap
   (grammars runtime)
   (lambda (state)
     (fset:with state :loaded
                (fset:with (fset:lookup state :loaded) language entry)))))

(defun %note-missing (runtime language)
  (sento.atomic:atomic-swap
   (grammars runtime)
   (lambda (state)
     (fset:with state :missing
                (fset:with (fset:lookup state :missing) language)))))

(defun ensure-language (runtime language &optional lib fn)
  "LANGUAGE's ts-entry, loaded the first time it is asked for, or NIL when its
grammar is not here. A grammar already loaded is a slot read; only the loading
itself is one thread at a time, because the loader's table is one table.

The library and the C function come from the language's own declaration, so
what grammars exist is something written rather than a list compiled in here."
  (multiple-value-bind (library fn-name)
      (if lib (values lib fn) (%grammar-of language))
  (or (fset:lookup (fset:lookup (%grammars runtime) :loaded) language)
      (bordeaux-threads:with-recursive-lock-held ((loading runtime))
        (unless (libs-loaded runtime) (ensure-ts runtime))
        (when (libs-loaded runtime)
          (let ((state (%grammars runtime)))
            ;; asked again under the lock: whoever held it before may have
            ;; loaded this very grammar while this thread waited
            (or (fset:lookup (fset:lookup state :loaded) language)
                (unless (fset:contains? (fset:lookup state :missing) language)
                  (let ((entry (load-language-entry language library fn-name)))
                    (cond
                      (entry (%note-loaded runtime language entry)
                             (fset:lookup (fset:lookup (%grammars runtime) :loaded)
                                          language))
                      (t (%note-missing runtime language)
                         (pine.err:report-failure
                          (make-condition 'simple-error
                                          :format-control "no ~(~a~) grammar to load"
                                          :format-arguments (list language))
                          "loading a grammar")
                         nil)))))))))))

(defun make-parse-state (runtime language &optional lib fn &key syntax package)
  "A parse-state for LANGUAGE, or nil if the grammar is unavailable. SYNTAX is
the compiled rules the walk follows; PACKAGE is the one a head symbol resolves
in, so a macro defined in the buffer's own package is found."
  (let ((entry (ensure-language runtime language lib fn)))
    (when entry
      (let ((parser (ts-parser-new)))
        (when (claim-language parser (entry-language-ptr entry) language)
          (make-instance 'parse-state :language language :parser parser
                                      :syntax syntax :package package))))))

(defun free-parse-state (ps)
  (when ps
    (when (ps-tree ps) (ignore-errors (ts-tree-delete (ps-tree ps))) (setf (ps-tree ps) nil))
    (when (ps-parser ps) (ignore-errors (ts-parser-delete (ps-parser ps))) (setf (ps-parser ps) nil))
    (when (ps-scratch ps)
      (ignore-errors (cffi:foreign-free (ps-scratch ps)))
      (setf (ps-scratch ps) nil))))

(defun ps-read-buffer (ps)
  "PS's foreign read buffer, allocated on first use."
  (or (ps-scratch ps)
      (setf (ps-scratch ps) (cffi:foreign-alloc :unsigned-char :count +read-chunk+))))

(defun ps-offset (ps)
  "The buffer line PS's tree starts at."
  (let ((band (ps-band ps))) (if band (car band) 0)))

(defun %fill-scratch (lines index byte scratch size)
  "Copy up to SIZE bytes of LINES from byte offset BYTE into SCRATCH. Answers
how many were written, which is zero at the end of the buffer."
  (multiple-value-bind (line offset) (pine.ts.index:byte-line index byte)
    (let ((written 0)
          (n (fset:size lines)))
      (block filling
        (loop :while (< line n)
              :do (let* ((octets (sb-ext:string-to-octets (fset:@ lines line)
                                                          :external-format :utf-8))
                         (len (length octets)))
                    (loop :for i :from (min offset len) :below len
                          :do (when (>= written size) (return-from filling))
                              (setf (cffi:mem-aref scratch :unsigned-char written)
                                    (aref octets i))
                              (incf written))
                    ;; every line but the last is followed by its newline
                    (when (< line (1- n))
                      (when (>= written size) (return-from filling))
                      (setf (cffi:mem-aref scratch :unsigned-char written) 10)
                      (incf written))
                    (setf offset 0)
                    (incf line))))
      written)))

;; TSPoint is declared :uint64 rather than (:struct ts-point) because CFFI
;; cannot take a struct by value in a callback: %defcallback accepts scalars
;; only. TSPoint is two uint32, so it is an eight-byte integer-class composite
;; and both x86-64 SysV and aarch64 pass it in one general-purpose register,
;; which is what :uint64 names. The position is not read here in any case; what
;; matters is that the argument after it, bytes_read, lands in the right place.
(cffi:defcallback %ts-read :pointer
    ((payload :pointer) (byte :uint32) (point :uint64)
     (bytes-read (:pointer :uint32)))
  (declare (ignore payload point))
  ;; a condition must not unwind into C, so a failure here ends the read, and so
  ;; the parse, rather than the process
  (handler-case
      (destructuring-bind (&optional lines index scratch size) *reading*
        (if (null lines)
            (progn (setf (cffi:mem-ref bytes-read :uint32) 0) (cffi:null-pointer))
            (let ((written (%fill-scratch lines index byte scratch size)))
              (setf (cffi:mem-ref bytes-read :uint32) written)
              (if (zerop written) (cffi:null-pointer) scratch))))
    (error (c)
      (format *error-output* "pine: the parser read failed at byte ~d: ~a~%" byte c)
      (finish-output *error-output*)
      (setf (cffi:mem-ref bytes-read :uint32) 0)
      (cffi:null-pointer))))

(defun call-with-input (lines index scratch fn)
  "Call FN with a TSInput reading LINES through INDEX, as a foreign value."
  (let ((*reading* (list lines index scratch +read-chunk+)))
    (cffi:with-foreign-object (input '(:struct ts-input))
      (setf (cffi:foreign-slot-value input '(:struct ts-input) 'payload)
            (cffi:null-pointer)
            (cffi:foreign-slot-value input '(:struct ts-input) 'read)
            (cffi:callback %ts-read)
            (cffi:foreign-slot-value input '(:struct ts-input) 'encoding)
            +input-encoding-utf8+
            (cffi:foreign-slot-value input '(:struct ts-input) 'decode)
            (cffi:null-pointer))
      (funcall fn (cffi:mem-ref input '(:struct ts-input))))))

(defun %set-point (object type slot point)
  "Write POINT into OBJECT's TSPoint SLOT. TYPE is OBJECT's own foreign type:
ts-input-edit and ts-range both hold points, at different offsets."
  (let ((p (cffi:foreign-slot-pointer object type slot)))
    (setf (cffi:foreign-slot-value p '(:struct ts-point) 'row)    (car point)
          (cffi:foreign-slot-value p '(:struct ts-point) 'column) (cdr point))))

(defun %tree-edit (tree start-byte old-end-byte new-end-byte
                   start-row old-end-row new-end-row)
  "Shift TREE's positions for a change spanning whole lines. Columns are zero
because the span runs from the start of one line to the start of another."
  (cffi:with-foreign-object (edit '(:struct ts-input-edit))
    (setf (cffi:foreign-slot-value edit '(:struct ts-input-edit) 'start-byte) start-byte
          (cffi:foreign-slot-value edit '(:struct ts-input-edit) 'old-end-byte) old-end-byte
          (cffi:foreign-slot-value edit '(:struct ts-input-edit) 'new-end-byte) new-end-byte)
    (%set-point edit '(:struct ts-input-edit) 'start-point (cons start-row 0))
    (%set-point edit '(:struct ts-input-edit) 'old-end-point (cons old-end-row 0))
    (%set-point edit '(:struct ts-input-edit) 'new-end-point (cons new-end-row 0))
    (ts-tree-edit tree edit)))

(defun %changed-row-span (old-tree new-tree)
  "Union of changed line rows between OLD-TREE and NEW-TREE as (values lo hi),
or nil when tree-sitter reports no changed ranges. The edit span alone
under-approximates: an opened string quote recolours everything after it."
  (cffi:with-foreign-object (len :uint32)
    (let ((ranges (ts-tree-get-changed-ranges old-tree new-tree len)))
      (unwind-protect
           (let ((n (cffi:mem-ref len :uint32)))
             (when (and (plusp n) (not (cffi:null-pointer-p ranges)))
               (let ((lo most-positive-fixnum) (hi 0))
                 (dotimes (i n (values lo hi))
                   (let ((r (cffi:mem-aptr ranges '(:struct ts-range) i)))
                     (let ((sp (cffi:foreign-slot-pointer r '(:struct ts-range) 'start-point))
                           (ep (cffi:foreign-slot-pointer r '(:struct ts-range) 'end-point)))
                       (setf lo (min lo (cffi:foreign-slot-value sp '(:struct ts-point) 'row))
                             hi (max hi (cffi:foreign-slot-value ep '(:struct ts-point) 'row)))))))))
        (unless (cffi:null-pointer-p ranges)
          (cffi:foreign-free ranges))))))

(defun %record-hl-edit (ps old new start-row old-end-row new-end-row)
  "Note one edit for incremental highlighting: rows from tree-sitter's changed
ranges unioned with the raw edit's rows (a pure line insert shifts everything
below while changing no named ranges), plus the line delta. A second edit before
the next highlight call, or any failure here, marks the cache stale."
  (when (ps-hl-cache ps)
    (if (ps-hl-pending ps)
        (setf (ps-hl-stale ps) t)
        (handler-case
            (let ((lo start-row)
                  (hi new-end-row)
                  (delta (- new-end-row old-end-row)))
              (unless (cffi:pointer-eq new old)
                (multiple-value-bind (rlo rhi) (%changed-row-span old new)
                  (when rlo
                    (setf lo (min lo rlo) hi (max hi rhi)))))
              (setf (ps-hl-pending ps) (list lo hi delta)))
          (error () (setf (ps-hl-stale ps) t))))))

(defun %band (lines viewport)
  "The line band to parse for VIEWPORT over LINES, or NIL for all of them."
  (let ((n (fset:size lines)))
    (when (and viewport (> n +whole-file-lines+))
      (cons (max 0 (* +band-lines+ (floor (car viewport) +band-lines+)))
            (min (1- n) (1- (* +band-lines+ (ceiling (1+ (cdr viewport))
                                                     +band-lines+))))))))

(defun %band-lines (lines band)
  "The subsequence of LINES that BAND covers, or LINES itself when BAND is nil."
  (if band (fset:subseq lines (car band) (1+ (cdr band))) lines))

(defun %parse-band (ps lines band band-lines same-band edit)
  (let* ((old-tree (ps-tree ps))
         (old-index (ps-byte-index ps))
         (shifted (and edit old-index old-tree same-band
                       (<= (car (or band '(0))) (first edit))
                       (< (first edit) (+ (or (and band (car band)) 0)
                                          (fset:size band-lines)))))
         (index nil))
    (when shifted
      (destructuring-bind (line old-lines new-lines byte-delta) edit
        (let* ((at (- line (if band (car band) 0)))
               (start (pine.ts.index:line-start old-index at))
               (old-end (pine.ts.index:line-start old-index (+ at old-lines))))
          (setf index (pine.ts.index:index-edit old-index band-lines at byte-delta
                                                (- new-lines old-lines)))
          (%tree-edit old-tree start old-end
                      (pine.ts.index:line-start index (+ at new-lines))
                      at (+ at old-lines) (+ at new-lines)))))
    (unless index (setf index (pine.ts.index:build-index band-lines)))
    (let ((incremental shifted))
      (let ((new (call-with-input
                  band-lines index (ps-read-buffer ps)
                  (lambda (input)
                    (ts-parser-parse (ps-parser ps)
                                     (if incremental old-tree (cffi:null-pointer))
                                     input)))))
        (cond
          ;; tree-sitter answers a null tree when the parser has no language, or
          ;; when the parse was cancelled. Either way the buffer keeps whatever
          ;; colour it had, so a parse that produced nothing says so
          ((or (null new) (cffi:null-pointer-p new))
           (pine.err:report-failure
            (make-condition 'simple-error
                            :format-control "the ~(~a~) parser answered no tree ~
                                             for ~d line~:p"
                            :format-arguments (list (ps-language ps)
                                                    (fset:size band-lines)))
            "parsing")
           nil)
          (t (if incremental
                 (destructuring-bind (line old-lines new-lines byte-delta) edit
                   (declare (ignore byte-delta))
                   (let ((at (- line (if band (car band) 0))))
                     (%record-hl-edit ps old-tree new at (+ at old-lines)
                                      (+ at new-lines))))
                 (setf (ps-hl-cache ps) nil
                       (ps-hl-lines ps) nil
                       (ps-hl-window ps) nil
                       (ps-hl-pending ps) nil
                       (ps-hl-stale ps) nil))
             (when (and old-tree (not (cffi:pointer-eq new old-tree)))
               (ts-tree-delete old-tree))
             (setf (ps-tree ps) new
                   (ps-byte-index ps) index
                   (ps-band ps) band
                   (ps-band-lines ps) band-lines
                   (ps-lines ps) lines)
             ps))))))

(defun parse-lines! (ps lines &key edit from viewport)
  "Parse LINES into PS's tree, reading the bytes straight from the seq.

EDIT is (LINE OLD-LINES NEW-LINES BYTE-DELTA): at LINE, OLD-LINES lines became
NEW-LINES lines and the buffer grew by BYTE-DELTA bytes. Given one, the tree is
shifted and reused and the byte index is carried forward; without one, both are
built from scratch.

FROM is the lines the edit was computed against. A shift only means anything
from that state, and more than one thread writes a buffer, so an edit that
describes a step the tree did not take is dropped rather than applied.

VIEWPORT is the (FROM-LINE . TO-LINE) some window shows. Past
+WHOLE-FILE-LINES+ only a band around it is given to tree-sitter at all. The
tree, the index and every position they report are relative to the band's first
line; PS-OFFSET is that line."
  (let* ((band (%band lines viewport))
         (band-lines (%band-lines lines band))
         (same-band (equal band (ps-band ps)))
         (old-tree (ps-tree ps)))
    (when (and old-tree same-band (eq lines (ps-lines ps)) (null edit))
      (return-from parse-lines! ps))
    (%parse-band ps lines band band-lines same-band
                 (when (or (null from) (eq from (ps-lines ps))) edit))))

(defun parse-text! (ps text)
  "Parse TEXT from scratch by splitting it into lines. For callers holding a
string rather than a buffer: a tool, a test, a snippet."
  (parse-lines! ps (fset:convert 'fset:seq
                                 (uiop:split-string text :separator '(#\Newline)))))

;;;; Structural motion off the persistent tree.

(defun %forward-sexp-byte (root byte)
  (let ((cur (ts-node-named-descendant-for-byte-range root byte byte)))
    (cond
      ((ts-node-is-null cur) nil)
      ((<= byte (ts-node-start-byte cur)) (ts-node-end-byte cur))
      (t (loop for i from 0 below (ts-node-named-count cur)
               for node = (ts-node-named-nth cur i)
               when (>= (ts-node-start-byte node) byte)
                 return (ts-node-end-byte node)
               finally (return (ts-node-end-byte cur)))))))

(defun %backward-sexp-byte (root byte)
  (let ((cur (ts-node-named-descendant-for-byte-range root byte byte)))
    (cond
      ((ts-node-is-null cur) nil)
      ((>= byte (ts-node-end-byte cur)) (ts-node-start-byte cur))
      (t (loop for i from (1- (ts-node-named-count cur)) downto 0
               for node = (ts-node-named-nth cur i)
               when (<= (ts-node-end-byte node) byte)
                 return (ts-node-start-byte node)
               finally (return (ts-node-start-byte cur)))))))

(defun %defun-bytes (root byte)
  "Start and end bytes of the top-level form containing BYTE, or nil."
  (let ((cur (ts-node-named-descendant-for-byte-range root byte byte)))
    (cond
      ((ts-node-is-null cur) nil)
      ;; climb until CUR's parent is ROOT, leaving CUR as the enclosing
      ;; top-level form rather than ROOT itself
      (t (loop for p = (ts-node-parent cur)
               until (or (ts-node-is-null p) (ts-node-is-null (ts-node-parent p)))
               do (setf cur p))
         (values (ts-node-start-byte cur) (ts-node-end-byte cur))))))

(defun parse-motion (ps kind line col)
  "A structural target from PS's persistent tree at LINE/COL, no reparse. KIND
is :forward-sexp :backward-sexp :beginning-of-defun :end-of-defun. Answers
(values line col) or nil."
  (let* ((tree (ps-tree ps)) (src (ps-byte-index ps))
         (offset (ps-offset ps))
         (line (- line offset)))
    (when (and tree src (<= 0 line))
      (handler-case
          (let ((byte (pine.ts.index:source-byte src line col))
                (root (ts-tree-root-node tree)))
            (flet ((at (b) (when b
                             (multiple-value-bind (l c)
                                 (pine.ts.index:source-line-col src b)
                               (values (+ l offset) c)))))
              (ecase kind
                (:forward-sexp (at (%forward-sexp-byte root byte)))
                (:backward-sexp (at (%backward-sexp-byte root byte)))
                (:beginning-of-defun
                 (multiple-value-bind (s e) (%defun-bytes root byte)
                   (declare (ignore e))
                   (at s)))
                (:end-of-defun
                 (multiple-value-bind (s e) (%defun-bytes root byte)
                   (declare (ignore s))
                   (at e))))))
        ;; no target is a real answer, a failure is not
        (error (c)
          (pine.err:report-failure c (format nil "~a from line ~d" kind line))
          nil)))))

;;;; Motion over a string, for a caller holding text rather than a buffer. The
;;;; tree lives only for the call.

(defun char-byte-length (ch)
  "UTF-8 encoded length in bytes of the single character CH."
  (let ((code (char-code ch)))
    (cond ((< code #x80) 1) ((< code #x800) 2) ((< code #x10000) 3) (t 4))))

(defun byte-length (text)
  (length (sb-ext:string-to-octets text :external-format :utf-8)))

(defun build-line-index (text)
  "Vector of (start-byte . start-char) per line."
  (let ((index (list (cons 0 0))) (bpos 0) (ci 0))
    (loop for ch across text
          do (incf bpos (char-byte-length ch))
             (incf ci)
             (when (char= ch #\Newline) (push (cons bpos ci) index)))
    (coerce (nreverse index) 'vector)))

(defun line-of-byte (byte-pos index)
  "Greatest line whose start byte is <= BYTE-POS."
  (let ((lo 0) (hi (1- (length index))))
    (loop while (< lo hi)
          do (let ((mid (ceiling (+ lo hi) 2)))
               (if (<= (car (aref index mid)) byte-pos)
                   (setf lo mid)
                   (setf hi (1- mid)))))
    lo))

(defun byte-to-line-col (byte-pos index text)
  "Map a UTF-8 BYTE-POS to (values line char-col) using the line INDEX and TEXT."
  (let* ((line (line-of-byte byte-pos index))
         (b (car (aref index line)))
         (ci (cdr (aref index line)))
         (len (length text))
         (col 0))
    (loop while (and (< b byte-pos) (< ci len))
          do (incf b (char-byte-length (char text ci)))
             (incf ci)
             (incf col))
    (values line col)))

(defun pos-to-byte (text line col index)
  "UTF-8 byte offset of the character position LINE/COL."
  (let* ((line (min line (1- (length index))))
         (bpos (car (aref index line)))
         (ci (cdr (aref index line)))
         (len (length text)))
    (loop for k from 0 below col
          while (< ci len)
          do (incf bpos (char-byte-length (char text ci)))
             (incf ci))
    bpos))

(defun call-with-root (runtime language text fn)
  "Parse TEXT and call FN with the tree's root node. Answers FN's values, or nil."
  (let ((entry (ensure-language runtime language)))
    (when entry
      (handler-case
          (cffi:with-foreign-string (cstr text :encoding :utf-8)
            (let ((tree (ts-parser-parse-string (entry-parser entry)
                                                (cffi:null-pointer) cstr
                                                (byte-length text))))
              (when (and tree (not (cffi:null-pointer-p tree)))
                (unwind-protect (funcall fn (ts-tree-root-node tree))
                  (ts-tree-delete tree)))))
        (error (c)
          (pine.err:report-failure c (format nil "parsing ~a source" language)))))))

(defun forward-sexp-pos (runtime language text line col)
  "Position (values line col) after the next sexp, or nil."
  (let* ((index (build-line-index text))
         (byte (pos-to-byte text line col index))
         (target (call-with-root runtime language text
                                 (lambda (root) (%forward-sexp-byte root byte)))))
    (when target (byte-to-line-col target index text))))

(defun backward-sexp-pos (runtime language text line col)
  (let* ((index (build-line-index text))
         (byte (pos-to-byte text line col index))
         (target (call-with-root runtime language text
                                 (lambda (root) (%backward-sexp-byte root byte)))))
    (when target (byte-to-line-col target index text))))

(defun defun-bounds-pos (runtime language text line col)
  "Bounds of the enclosing top-level form as (values start-line start-col
end-line end-col), or nil."
  (let* ((index (build-line-index text))
         (byte (pos-to-byte text line col index)))
    (multiple-value-bind (start end)
        (call-with-root runtime language text
                        (lambda (root) (%defun-bytes root byte)))
      (when start
        (multiple-value-bind (sl sc) (byte-to-line-col start index text)
          (multiple-value-bind (el ec) (byte-to-line-col end index text)
            (values sl sc el ec)))))))
