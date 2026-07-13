(in-package :pine.ts)

;;;; CFFI bindings to libtree-sitter. tree-sitter passes TSNode by value;
;;;; cffi-libffi handles the by-value struct, so no C wrapper is needed. The
;;;; context[4] array in TSNode is flattened to four scalars because CFFI has no
;;;; by-value translation for an array struct member; the fields are never read
;;;; from Lisp, only passed back to ts_node_*.

(cffi:define-foreign-library libtree-sitter
  (t (:default "libtree-sitter")))

(cffi:defcstruct ts-node
  (c0 :uint32) (c1 :uint32) (c2 :uint32) (c3 :uint32)
  (id :pointer) (tree :pointer))

(cffi:defcfun ("ts_parser_new" ts-parser-new) :pointer)
(cffi:defcfun ("ts_parser_delete" ts-parser-delete) :void (parser :pointer))
(cffi:defcfun ("ts_parser_set_language" ts-parser-set-language) :bool
  (parser :pointer) (language :pointer))
(cffi:defcfun ("ts_parser_parse_string" ts-parser-parse-string) :pointer
  (parser :pointer) (old-tree :pointer) (string :pointer) (length :uint32))
(cffi:defcfun ("ts_tree_delete" ts-tree-delete) :void (tree :pointer))
(cffi:defcfun ("ts_tree_root_node" ts-tree-root-node) (:struct ts-node)
  (tree :pointer))
(cffi:defcfun ("ts_node_start_byte" ts-node-start-byte) :uint32
  (node (:struct ts-node)))
(cffi:defcfun ("ts_node_end_byte" ts-node-end-byte) :uint32
  (node (:struct ts-node)))
(cffi:defcfun ("ts_node_parent" ts-node-parent) (:struct ts-node)
  (node (:struct ts-node)))
(cffi:defcfun ("ts_node_type" %ts-node-type) :pointer
  (node (:struct ts-node)))
(cffi:defcfun ("ts_node_is_null" ts-node-is-null) :bool
  (node (:struct ts-node)))
(cffi:defcfun ("ts_node_named_child_count" ts-node-named-child-count) :uint32
  (node (:struct ts-node)))
(cffi:defcfun ("ts_node_named_child" ts-node-named-child) (:struct ts-node)
  (node (:struct ts-node)) (index :uint32))
(cffi:defcfun ("ts_node_child_by_field_name" ts-node-child-by-field-name)
    (:struct ts-node)
  (node (:struct ts-node)) (name :string) (name-length :uint32))
(cffi:defcfun ("ts_node_named_descendant_for_byte_range"
               ts-node-named-descendant-for-byte-range) (:struct ts-node)
  (node (:struct ts-node)) (start :uint32) (end :uint32))

;;;; Incremental editing. TSPoint.column is a BYTE offset within its row.
;;;; ts_tree_edit shifts a persistent tree's positions by an edit descriptor;
;;;; the next ts_parser_parse_string with that tree as old-tree reuses the
;;;; unchanged subtrees instead of re-lexing the whole source.

(cffi:defcstruct ts-point (row :uint32) (column :uint32))

(cffi:defcstruct ts-input-edit
  (start-byte :uint32) (old-end-byte :uint32) (new-end-byte :uint32)
  (start-point   (:struct ts-point))
  (old-end-point (:struct ts-point))
  (new-end-point (:struct ts-point)))

(cffi:defcfun ("ts_tree_edit" ts-tree-edit) :void (tree :pointer) (edit :pointer))


;;;; Grammar registry. Each language maps to a grammar shared library and the
;;;; C function that returns its TSLanguage*. Grammar libraries come from Guix,
;;;; on the load path by name.

(defparameter *grammars*
  '((:commonlisp "libtree-sitter-commonlisp" "tree_sitter_commonlisp")
    (:scheme "libtree-sitter-scheme" "tree_sitter_scheme")))


;;;; Runtime + per-language entries

(defclass ts-runtime ()
  ((libs-loaded :accessor libs-loaded :initform nil)
   (languages   :accessor languages   :initform (make-hash-table :test 'eq))))

(defclass ts-entry ()
  ((parser       :initarg :parser       :accessor entry-parser)
   ;; the language pointer, kept so a per-buffer parser can set-language.
   (language-ptr :initarg :language-ptr :accessor entry-language-ptr)))

(defun make-ts-runtime () (make-instance 'ts-runtime))
(defun ts-loaded-p (runtime) (libs-loaded runtime))

(defun ensure-ts (runtime)
  (unless (libs-loaded runtime)
    (handler-case
        (progn (cffi:load-foreign-library 'libtree-sitter)
               (setf (libs-loaded runtime) t))
      (error () nil)))
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
  "Load grammar LIBRARY-NAME and call FN-NAME to get its TSLanguage*, or nil."
  (handler-case
      (when (load-grammar-library library-name)
        (let ((fn (cffi:foreign-symbol-pointer fn-name)))
          (when fn (cffi:foreign-funcall-pointer fn () :pointer))))
    (error () nil)))

(defun load-language-entry (language)
  (destructuring-bind (&optional library fn-name) (cdr (assoc language *grammars*))
    (unless library (return-from load-language-entry nil))
    (let ((lang (grammar-language-pointer library fn-name)))
      (when (and lang (not (cffi:null-pointer-p lang)))
        (let ((parser (ts-parser-new)))
          (ts-parser-set-language parser lang)
          (make-instance 'ts-entry :parser parser :language-ptr lang))))))

(defun ensure-language (runtime language)
  "Return LANGUAGE's ts-entry, loading it once, or nil if unsupported."
  (unless (libs-loaded runtime) (ensure-ts runtime))
  (unless (libs-loaded runtime) (return-from ensure-language nil))
  (or (gethash language (languages runtime))
      (let ((entry (load-language-entry language)))
        (when entry (setf (gethash language (languages runtime)) entry)))))




;;;; Highlight computation (portable)

(defun char-byte-length (ch)
  "UTF-8 encoded length in bytes of the single character CH."
  (let ((code (char-code ch)))
    (cond ((< code #x80) 1) ((< code #x800) 2) ((< code #x10000) 3) (t 4))))

(defun build-line-index (text)
  "Vector of (start-byte . start-char) per line. tree-sitter reports positions
in UTF-8 bytes while the cell grid and point are in characters; the index
converts both ways without assuming one byte per character."
  (let ((index (list (cons 0 0))) (bpos 0) (ci 0))
    (loop for ch across text
          do (incf bpos (char-byte-length ch))
             (incf ci)
             (when (char= ch #\Newline) (push (cons bpos ci) index)))
    (coerce (nreverse index) 'vector)))

(defun %line-of-byte (byte-pos index)
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
  (let* ((line (%line-of-byte byte-pos index))
         (b (car (aref index line)))
         (ci (cdr (aref index line)))
         (len (length text))
         (col 0))
    (loop while (and (< b byte-pos) (< ci len))
          do (incf b (char-byte-length (char text ci)))
             (incf ci)
             (incf col))
    (values line col)))

(defun byte-length (text)
  (length (sb-ext:string-to-octets text :external-format :utf-8)))

(defun ts-node-type (node)
  (cffi:foreign-string-to-lisp (%ts-node-type node) :encoding :ascii))


(defun compute-highlights (runtime language text)
  (let ((entry (ensure-language runtime language)))
    (unless entry (return-from compute-highlights nil))
    (handler-case
        (cffi:with-foreign-string (cstr text :encoding :utf-8)
          (let ((tree (ts-parser-parse-string (entry-parser entry) (cffi:null-pointer)
                                              cstr (byte-length text))))
            (when (and tree (not (cffi:null-pointer-p tree)))
              (unwind-protect
                   (walk-highlights language (ts-tree-root-node tree) text
                                    (build-line-index text))
                (ts-tree-delete tree)))))
      (error () nil))))


;;;; Structural navigation. Each motion parses the buffer, walks the tree once,
;;;; and returns a target position; the tree lives only for the call.

(defun call-with-root (runtime language text fn)
  "Parse TEXT and call FN with the tree's root node. Returns FN's values, or nil."
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
        (error () nil)))))

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

(defun %forward-sexp-byte (root byte)
  (let ((cur (ts-node-named-descendant-for-byte-range root byte byte)))
    (cond
      ((ts-node-is-null cur) nil)
      ((<= byte (ts-node-start-byte cur)) (ts-node-end-byte cur))
      (t (loop for i from 0 below (ts-node-named-child-count cur)
               for child = (ts-node-named-child cur i)
               when (>= (ts-node-start-byte child) byte)
                 return (ts-node-end-byte child)
               finally (return (ts-node-end-byte cur)))))))

(defun %backward-sexp-byte (root byte)
  (let ((cur (ts-node-named-descendant-for-byte-range root byte byte)))
    (cond
      ((ts-node-is-null cur) nil)
      ((>= byte (ts-node-end-byte cur)) (ts-node-start-byte cur))
      (t (loop for i from (1- (ts-node-named-child-count cur)) downto 0
               for child = (ts-node-named-child cur i)
               when (<= (ts-node-end-byte child) byte)
                 return (ts-node-start-byte child)
               finally (return (ts-node-start-byte cur)))))))

(defun %defun-bytes (root byte)
  "Start and end bytes of the top-level form (a direct child of ROOT)
containing BYTE, or nil."
  (let ((cur (ts-node-named-descendant-for-byte-range root byte byte)))
    (cond
      ((ts-node-is-null cur) nil)
      ;; Climb until CUR's parent is ROOT (i.e. its grandparent is null),
      ;; leaving CUR as the enclosing top-level form -- not ROOT itself.
      (t (loop for p = (ts-node-parent cur)
               until (or (ts-node-is-null p) (ts-node-is-null (ts-node-parent p)))
               do (setf cur p))
         (values (ts-node-start-byte cur) (ts-node-end-byte cur))))))

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


;;;; Per-buffer incremental parse state. One of these lives with a buffer that
;;;; has a tree-sitter language: its own parser, its persistent tree, and the
;;;; text the tree currently reflects. An edit reparses incrementally from that
;;;; tree; highlighting and structural motion read the same persistent tree, so
;;;; nothing re-parses from scratch and nothing crosses a wire. The parser is
;;;; per-buffer because tree-sitter parsers are not thread-safe and different
;;;; buffers parse concurrently.

(defclass parse-state ()
  ((language :initarg :language :accessor ps-language)
   (parser   :initarg :parser   :accessor ps-parser)   ; own TSParser*
   (tree     :initform nil      :accessor ps-tree)      ; persistent TSTree* or nil
   (text     :initform ""       :accessor ps-text)))    ; text the tree reflects

(defun make-parse-state (runtime language)
  "A parse-state for LANGUAGE, or nil if the grammar is unavailable."
  (let ((entry (ensure-language runtime language)))
    (when entry
      (let ((parser (ts-parser-new)))
        (ts-parser-set-language parser (entry-language-ptr entry))
        (make-instance 'parse-state :language language :parser parser)))))

(defun free-parse-state (ps)
  (when ps
    (when (ps-tree ps) (ignore-errors (ts-tree-delete (ps-tree ps))) (setf (ps-tree ps) nil))
    (when (ps-parser ps) (ignore-errors (ts-parser-delete (ps-parser ps))) (setf (ps-parser ps) nil))))

(defun parse-full! (ps text)
  "Parse TEXT from scratch, replacing PS's tree. Used for the first parse and
whenever there is no old tree to edit from."
  (when (ps-tree ps) (ts-tree-delete (ps-tree ps)) (setf (ps-tree ps) nil))
  (handler-case
      (cffi:with-foreign-string (cstr text :encoding :utf-8)
        (let ((tree (ts-parser-parse-string (ps-parser ps) (cffi:null-pointer)
                                            cstr (byte-length text))))
          (setf (ps-tree ps) (if (cffi:null-pointer-p tree) nil tree))))
    (error () (setf (ps-tree ps) nil)))
  (setf (ps-text ps) text)
  ps)

(defun %char->byte-point (text char-index)
  "UTF-8 byte offset of CHAR-INDEX in TEXT, and its (row . byte-col) TSPoint."
  (let ((byte 0) (row 0) (line-start 0)
        (n (min char-index (length text))))
    (dotimes (i n)
      (let ((ch (char text i)))
        (if (char= ch #\Newline)
            (progn (incf byte) (incf row) (setf line-start byte))
            (incf byte (char-byte-length ch)))))
    (values byte (cons row (- byte line-start)))))

(defun %text-edit (old-text new-text)
  "A generic tree-sitter edit descriptor for the change OLD-TEXT -> NEW-TEXT,
from the common prefix and suffix. Correct for any change (insert, delete,
undo/redo, replace), so no per-edit-op bookkeeping is needed. Returns
start/old-end/new-end byte offsets and their three TSPoints."
  (let* ((la (length old-text)) (lb (length new-text))
         (prefix (loop for i from 0 below (min la lb)
                       unless (char= (char old-text i) (char new-text i)) return i
                       finally (return (min la lb))))
         (max-suffix (- (min la lb) prefix))
         (suffix (loop for k from 0 below max-suffix
                       unless (char= (char old-text (- la 1 k))
                                     (char new-text (- lb 1 k)))
                         return k
                       finally (return max-suffix))))
    (multiple-value-bind (sb sp) (%char->byte-point old-text prefix)
      (multiple-value-bind (obe oep) (%char->byte-point old-text (- la suffix))
        (multiple-value-bind (nbe nep) (%char->byte-point new-text (- lb suffix))
          (values sb obe nbe sp oep nep))))))

(defun %set-point (edit slot point)
  (let ((p (cffi:foreign-slot-pointer edit '(:struct ts-input-edit) slot)))
    (setf (cffi:foreign-slot-value p '(:struct ts-point) 'row)    (car point)
          (cffi:foreign-slot-value p '(:struct ts-point) 'column) (cdr point))))

(defun reparse! (ps new-text)
  "Incrementally reparse PS to NEW-TEXT, editing its persistent tree from the
diff against the text it currently reflects. Falls back to a full parse when
there is no tree yet or the incremental parse fails."
  (if (null (ps-tree ps))
      (parse-full! ps new-text)
      (handler-case
          (progn
            (multiple-value-bind (sb obe nbe sp oep nep) (%text-edit (ps-text ps) new-text)
              (cffi:with-foreign-object (edit '(:struct ts-input-edit))
                (setf (cffi:foreign-slot-value edit '(:struct ts-input-edit) 'start-byte) sb
                      (cffi:foreign-slot-value edit '(:struct ts-input-edit) 'old-end-byte) obe
                      (cffi:foreign-slot-value edit '(:struct ts-input-edit) 'new-end-byte) nbe)
                (%set-point edit 'start-point sp)
                (%set-point edit 'old-end-point oep)
                (%set-point edit 'new-end-point nep)
                (ts-tree-edit (ps-tree ps) edit)))
            (cffi:with-foreign-string (cstr new-text :encoding :utf-8)
              (let* ((old (ps-tree ps))
                     (new (ts-parser-parse-string (ps-parser ps) old cstr
                                                  (byte-length new-text))))
                (cond
                  ((cffi:null-pointer-p new) (parse-full! ps new-text))
                  (t (unless (cffi:pointer-eq new old) (ts-tree-delete old))
                     (setf (ps-tree ps) new (ps-text ps) new-text)))))
            ps)
        (error () (parse-full! ps new-text)))))

(defun parse-highlights (ps text)
  "Highlights (line start-col end-col face) from PS's persistent tree over TEXT."
  (let ((tree (ps-tree ps)))
    (when tree
      (handler-case
          (walk-highlights (ps-language ps) (ts-tree-root-node tree) text
                           (build-line-index text))
        (error () nil)))))

(defun parse-motion (ps kind line col)
  "A structural target from PS's persistent tree at LINE/COL, no reparse. KIND
is :forward-sexp :backward-sexp :beginning-of-defun :end-of-defun. Returns
(values line col) or nil."
  (let ((tree (ps-tree ps)) (text (ps-text ps)))
    (when tree
      (handler-case
          (let* ((index (build-line-index text))
                 (byte (pos-to-byte text line col index))
                 (root (ts-tree-root-node tree)))
            (ecase kind
              (:forward-sexp
               (let ((b (%forward-sexp-byte root byte)))
                 (when b (byte-to-line-col b index text))))
              (:backward-sexp
               (let ((b (%backward-sexp-byte root byte)))
                 (when b (byte-to-line-col b index text))))
              (:beginning-of-defun
               (multiple-value-bind (s e) (%defun-bytes root byte)
                 (declare (ignore e))
                 (when s (byte-to-line-col s index text))))
              (:end-of-defun
               (multiple-value-bind (s e) (%defun-bytes root byte)
                 (declare (ignore s))
                 (when e (byte-to-line-col e index text))))))
        (error () nil)))))
