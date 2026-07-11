(in-package :pine.ts)

(ffi:clines "
#include <dlfcn.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

static void *ts_lib = NULL;
static void *ts_wrap_lib = NULL;
static void *ts_cl_lib = NULL;

static void *ts_sym(const char *name) {
    void *s = NULL;
    if (ts_wrap_lib) s = dlsym(ts_wrap_lib, name);
    if (!s && ts_lib) s = dlsym(ts_lib, name);
    return s;
}

typedef void TSParser;
typedef void TSTree;
typedef void TSQuery;
typedef void TSQueryCursor;
typedef void TSLanguage;

typedef struct { uint32_t ctx[4]; void *id; void *tree; } TSNode;
typedef struct { uint32_t row; uint32_t col; } TSPoint;
typedef struct {
    uint32_t id; uint16_t pattern_index; uint16_t capture_count; void *captures;
} TSQueryMatch;

typedef TSParser* (*ts_parser_new_fn)(void);
typedef void (*ts_parser_delete_fn)(TSParser*);
typedef int (*ts_parser_set_language_fn)(TSParser*, const TSLanguage*);
typedef TSTree* (*ts_parser_parse_string_fn)(TSParser*, const TSTree*, const char*, uint32_t);
typedef void (*ts_tree_delete_fn)(TSTree*);
typedef void (*ts_tree_root_node_out_fn)(TSNode*, const TSTree*);
typedef uint32_t (*ts_node_start_byte_ptr_fn)(const TSNode*);
typedef uint32_t (*ts_node_end_byte_ptr_fn)(const TSNode*);
typedef TSQuery* (*ts_query_new_fn)(const TSLanguage*, const char*, uint32_t, uint32_t*, uint32_t*);
typedef void (*ts_query_delete_fn)(TSQuery*);
typedef uint32_t (*ts_query_capture_count_fn)(const TSQuery*);
typedef const char* (*ts_query_capture_name_fn)(const TSQuery*, uint32_t, uint32_t*);
typedef TSQueryCursor* (*ts_query_cursor_new_fn)(void);
typedef void (*ts_query_cursor_delete_fn)(TSQueryCursor*);
typedef void (*ts_query_cursor_exec_ptr_fn)(TSQueryCursor*, const TSQuery*, const TSNode*);
typedef int (*ts_query_cursor_next_capture_fn)(TSQueryCursor*, TSQueryMatch*, uint32_t*);
typedef const TSLanguage* (*ts_language_fn)(void);
")

(defclass ts-runtime ()
  ((libs-loaded :accessor libs-loaded :initform nil)
   (languages   :accessor languages   :initform (make-hash-table :test 'eq))))

(defclass ts-entry ()
  ((language      :initarg :language      :accessor entry-language)
   (parser        :initarg :parser        :accessor entry-parser)
   (query         :initarg :query         :accessor entry-query)
   (capture-cache :initarg :capture-cache :accessor entry-capture-cache)))

(defun make-ts-runtime ()
  (make-instance 'ts-runtime))

(defun ts-loaded-p (runtime)
  (libs-loaded runtime))

(defun load-ts-libs ()
  (ffi:c-inline () () :int "
{
    ts_lib = dlopen(\"libtree-sitter.dylib\", RTLD_LAZY);
    if (!ts_lib) ts_lib = dlopen(\"/opt/homebrew/lib/libtree-sitter.dylib\", RTLD_LAZY);
    if (!ts_lib) { @(return) = 0; goto done; }

    const char *base = getenv(\"HECL_BASE\");
    char path[1024];
    if (base) {
        snprintf(path, sizeof(path), \"%s/lib/libts-wrapper.dylib\", base);
        ts_wrap_lib = dlopen(path, RTLD_LAZY);
    }
    if (!ts_wrap_lib) ts_wrap_lib = dlopen(\"libts-wrapper.dylib\", RTLD_LAZY);
    if (!ts_wrap_lib) ts_wrap_lib = dlopen(\"lib/libts-wrapper.dylib\", RTLD_LAZY);

    ts_cl_lib = dlopen(\"libtree-sitter-commonlisp.dylib\", RTLD_LAZY);
    if (!ts_cl_lib) ts_cl_lib = dlopen(\"/opt/homebrew/lib/libtree-sitter-commonlisp.dylib\", RTLD_LAZY);

    @(return) = (ts_lib && ts_wrap_lib && ts_cl_lib) ? 1 : 0;
    done: ;
}" :one-liner nil :side-effects t))

(defun load-ts-wrapper-path (path)
  (ffi:c-inline (path) (:cstring) :int "
{ ts_wrap_lib = dlopen(#0, RTLD_LAZY); @(return) = ts_wrap_lib ? 1 : 0; }"
  :one-liner nil :side-effects t))

(defun ensure-ts (runtime)
  (unless (libs-loaded runtime)
    (let ((wrapper-path (namestring (merge-pathnames "lib/libts-wrapper.dylib"
                                                      (asdf:system-source-directory :pine)))))
      (load-ts-wrapper-path wrapper-path))
    (when (plusp (load-ts-libs))
      (setf (libs-loaded runtime) t)))
  runtime)

(defun ensure-language (runtime language)
  "Ensure LANGUAGE's parser/query is available. Returns the ts-entry or nil."
  (unless (libs-loaded runtime) (ensure-ts runtime))
  (unless (libs-loaded runtime) (return-from ensure-language nil))
  (or (gethash language (languages runtime))
      (let ((entry (load-language-entry language)))
        (when entry
          (setf (gethash language (languages runtime)) entry)))))


;;;; FFI

(defun ts-get-language ()
  (ffi:c-inline () () :pointer-void "
{ ts_language_fn fn = (ts_language_fn)dlsym(ts_cl_lib, \"tree_sitter_commonlisp\");
  @(return) = fn ? (void*)fn() : NULL; }" :one-liner nil :side-effects t))

(defun ts-parser-new ()
  (ffi:c-inline () () :pointer-void "
{ ts_parser_new_fn fn = (ts_parser_new_fn)ts_sym(\"ts_parser_new\");
  @(return) = fn ? fn() : NULL; }" :one-liner nil :side-effects t))

(defun ts-parser-delete (p)
  (ffi:c-inline (p) (:pointer-void) :void "
{ ts_parser_delete_fn fn = (ts_parser_delete_fn)ts_sym(\"ts_parser_delete\");
  if (fn && #0) fn(#0); }" :one-liner nil :side-effects t))

(defun ts-parser-set-language (parser lang)
  (ffi:c-inline (parser lang) (:pointer-void :pointer-void) :int "
{ ts_parser_set_language_fn fn = (ts_parser_set_language_fn)ts_sym(\"ts_parser_set_language\");
  @(return) = (fn && #0) ? fn(#0, #1) : 0; }" :one-liner nil :side-effects t))

(defun ts-parser-parse-string (parser text len)
  (ffi:c-inline (parser text len) (:pointer-void :cstring :int) :pointer-void "
{ ts_parser_parse_string_fn fn = (ts_parser_parse_string_fn)ts_sym(\"ts_parser_parse_string\");
  @(return) = (fn && #0) ? fn(#0, NULL, #1, #2) : NULL; }" :one-liner nil :side-effects t))

(defun ts-tree-delete (tree)
  (ffi:c-inline (tree) (:pointer-void) :void "
{ ts_tree_delete_fn fn = (ts_tree_delete_fn)ts_sym(\"ts_tree_delete\");
  if (fn && #0) fn(#0); }" :one-liner nil :side-effects t))

(defun ts-tree-root-node (tree node-buf)
  (ffi:c-inline (tree node-buf) (:pointer-void :pointer-void) :void "
{ ts_tree_root_node_out_fn fn = (ts_tree_root_node_out_fn)ts_sym(\"ts_tree_root_node_out\");
  if (fn) fn(#1, #0); }" :one-liner nil :side-effects t))

(defun ts-query-new (lang source source-len)
  (ffi:c-inline (lang source source-len) (:pointer-void :cstring :int)
                (values :pointer-void :int :int) "
{ ts_query_new_fn fn = (ts_query_new_fn)ts_sym(\"ts_query_new\");
  if (!fn) { @(return 0) = NULL; @(return 1) = 0; @(return 2) = 0; }
  else {
      uint32_t err_offset = 0, err_type = 0;
      @(return 0) = fn(#0, #1, #2, &err_offset, &err_type);
      @(return 1) = err_offset;
      @(return 2) = err_type;
  }
}" :one-liner nil :side-effects t))

(defun ts-query-delete (q)
  (ffi:c-inline (q) (:pointer-void) :void "
{ ts_query_delete_fn fn = (ts_query_delete_fn)ts_sym(\"ts_query_delete\");
  if (fn && #0) fn(#0); }" :one-liner nil :side-effects t))

(defun ts-query-capture-count (q)
  (ffi:c-inline (q) (:pointer-void) :int "
{ ts_query_capture_count_fn fn = (ts_query_capture_count_fn)ts_sym(\"ts_query_capture_count\");
  @(return) = (fn && #0) ? fn(#0) : 0; }" :one-liner nil :side-effects t))

(defun ts-query-capture-name (q id)
  (ffi:c-inline (q id) (:pointer-void :int) :object "
{ ts_query_capture_name_fn fn = (ts_query_capture_name_fn)ts_sym(\"ts_query_capture_name_for_id\");
  if (!fn || !#0) { @(return) = ECL_NIL; }
  else {
      uint32_t len = 0;
      const char *name = fn(#0, #1, &len);
      @(return) = name ? ecl_make_simple_base_string((char*)name, len) : ECL_NIL;
  }
}" :one-liner nil :side-effects t))

(defun ts-query-cursor-new ()
  (ffi:c-inline () () :pointer-void "
{ ts_query_cursor_new_fn fn = (ts_query_cursor_new_fn)ts_sym(\"ts_query_cursor_new\");
  @(return) = fn ? fn() : NULL; }" :one-liner nil :side-effects t))

(defun ts-query-cursor-delete (c)
  (ffi:c-inline (c) (:pointer-void) :void "
{ ts_query_cursor_delete_fn fn = (ts_query_cursor_delete_fn)ts_sym(\"ts_query_cursor_delete\");
  if (fn && #0) fn(#0); }" :one-liner nil :side-effects t))

(defun ts-query-cursor-exec (cursor query node-buf)
  (ffi:c-inline (cursor query node-buf) (:pointer-void :pointer-void :pointer-void) :void "
{ ts_query_cursor_exec_ptr_fn fn = (ts_query_cursor_exec_ptr_fn)ts_sym(\"ts_query_cursor_exec_ptr\");
  if (fn) fn(#0, #1, #2); }" :one-liner nil :side-effects t))

(defun ts-node-start-byte (node-buf)
  (ffi:c-inline (node-buf) (:pointer-void) :int "
{ ts_node_start_byte_ptr_fn fn = (ts_node_start_byte_ptr_fn)ts_sym(\"ts_node_start_byte_ptr\");
  @(return) = (fn && #0) ? fn(#0) : 0; }" :one-liner nil :side-effects t))

(defun ts-node-end-byte (node-buf)
  (ffi:c-inline (node-buf) (:pointer-void) :int "
{ ts_node_end_byte_ptr_fn fn = (ts_node_end_byte_ptr_fn)ts_sym(\"ts_node_end_byte_ptr\");
  @(return) = (fn && #0) ? fn(#0) : 0; }" :one-liner nil :side-effects t))

(defun ts-alloc (size)
  (ffi:c-inline (size) (:int) :pointer-void "{ @(return) = malloc(#0); }"
  :one-liner nil :side-effects t))

(defun ts-free (ptr)
  (ffi:c-inline (ptr) (:pointer-void) :void "{ free(#0); }"
  :one-liner nil :side-effects t))

(defun ts-byte-length (text)
  (ffi:c-inline (text) (:cstring) :int "{ @(return) = strlen(#0); }"
  :one-liner nil :side-effects nil))

(defun ts-next-capture (cursor query match-buf capture-idx-buf)
  (ffi:c-inline (cursor query match-buf capture-idx-buf)
                (:pointer-void :pointer-void :pointer-void :pointer-void)
                (values :int :int :int :int) "
{
    typedef int (*next_fn)(void*, void*, uint32_t*);
    next_fn fn = (next_fn)ts_sym(\"ts_query_cursor_next_capture\");
    if (!fn || !fn(#0, #2, (uint32_t*)#3)) {
        @(return 0) = 0; @(return 1) = 0; @(return 2) = 0; @(return 3) = 0;
    } else {
        unsigned char *match = (unsigned char*)#2;
        void *captures_ptr = *(void**)(match + 8);
        uint32_t cap_idx = *(uint32_t*)#3;

        /* sizeof(TSQueryCapture) = sizeof(TSNode) + sizeof(uint32_t) + padding
           TSNode = 32 bytes on 64-bit, index at offset 32, stride 40 */
        unsigned char *cap = (unsigned char*)captures_ptr + (cap_idx * 40);

        ts_node_start_byte_ptr_fn sb = (ts_node_start_byte_ptr_fn)ts_sym(\"ts_node_start_byte_ptr\");
        ts_node_end_byte_ptr_fn eb = (ts_node_end_byte_ptr_fn)ts_sym(\"ts_node_end_byte_ptr\");

        uint32_t idx = *(uint32_t*)(cap + 32);
        uint32_t start = sb ? sb((void*)cap) : 0;
        uint32_t end   = eb ? eb((void*)cap) : 0;

        @(return 0) = 1;
        @(return 1) = idx;
        @(return 2) = start;
        @(return 3) = end;
    }
}" :one-liner nil :side-effects t))


;;;; Language loaders

(defun load-language-entry (language)
  "Build a ts-entry for LANGUAGE. Returns nil if the language isn't
supported or its libraries aren't loaded."
  (case language
    (:commonlisp (load-commonlisp-entry))
    (t nil)))

(defun read-query-file (relpath)
  (let ((path (namestring (merge-pathnames relpath
                                            (asdf:system-source-directory :pine)))))
    (with-open-file (s path :if-does-not-exist nil)
      (when s
        (let ((str (make-string (file-length s))))
          (read-sequence str s)
          str)))))

(defun build-capture-name-cache (query)
  (let* ((count (ts-query-capture-count query))
         (names (make-array count)))
    (dotimes (i count)
      (setf (aref names i) (ts-query-capture-name query i)))
    names))

(defun load-commonlisp-entry ()
  (let ((lang (ts-get-language)))
    (unless lang (return-from load-commonlisp-entry nil))
    (let ((parser (ts-parser-new)))
      (ts-parser-set-language parser lang)
      (let ((query-text (read-query-file "lib/highlights.scm")))
        (unless query-text
          (format *error-output* "ts: highlights.scm not found~%")
          (force-output *error-output*)
          (return-from load-commonlisp-entry nil))
        (multiple-value-bind (q err-offset err-type)
            (ts-query-new lang query-text (length query-text))
          (unless q
            (format *error-output* "ts: commonlisp query error at offset ~a type ~a~%"
                    err-offset err-type)
            (force-output *error-output*)
            (return-from load-commonlisp-entry nil))
          (make-instance 'ts-entry
                         :language lang
                         :parser parser
                         :query q
                         :capture-cache (build-capture-name-cache q)))))))


;;;; Face mapping

(defvar *cl-special-forms* (make-hash-table :test 'equal))

(dolist (sym '("block" "catch" "eval-when" "flet" "function" "go" "if"
               "labels" "let" "let*" "load-time-value" "locally" "macrolet"
               "multiple-value-call" "multiple-value-prog1" "progn" "progv"
               "quote" "return-from" "setq" "symbol-macrolet" "tagbody"
               "the" "throw" "unwind-protect"
               "defvar" "defparameter" "defconstant" "defstruct" "defclass"
               "deftype" "defsetf" "define-setf-expander"
               "define-symbol-macro" "define-compiler-macro"
               "define-condition" "define-modify-macro"
               "define-method-combination"
               "defpackage" "in-package" "use-package"
               "declaim" "declare" "proclaim"
               "cond" "when" "unless" "case" "ecase" "ccase" "typecase"
               "etypecase" "ctypecase"
               "do" "do*" "dolist" "dotimes" "loop"
               "and" "or" "not"
               "with-open-file" "with-open-stream" "with-input-from-string"
               "with-output-to-string" "with-accessors" "with-slots"
               "with-standard-io-syntax" "with-compilation-unit"
               "with-condition-restarts" "with-hash-table-iterator"
               "with-package-iterator" "with-simple-restart"
               "handler-case" "handler-bind" "restart-case" "restart-bind"
               "ignore-errors"
               "destructuring-bind" "multiple-value-bind" "multiple-value-setq"
               "multiple-value-list"
               "prog" "prog*" "prog1" "prog2"
               "return" "return-from"
               "setf" "psetf" "psetq" "rotatef" "shiftf"
               "push" "pop" "pushnew" "remf"
               "incf" "decf"
               "assert" "check-type"
               "trace" "untrace" "step"
               "time" "inspect"
               "export" "import" "intern" "shadow" "shadowing-import"
               "provide" "require"
               "defmethod" "defgeneric"
               "lambda"))
  (setf (gethash sym *cl-special-forms*) t))

(defvar *cl-constants* (make-hash-table :test 'equal))

(dolist (sym '("t" "nil" "pi"
               "most-positive-fixnum" "most-negative-fixnum"
               "most-positive-double-float" "most-negative-double-float"
               "most-positive-single-float" "most-negative-single-float"
               "most-positive-short-float" "most-negative-short-float"
               "most-positive-long-float" "most-negative-long-float"))
  (setf (gethash sym *cl-constants*) t))

(defun capture-name-to-face (name)
  (cond
    ((or (string= name "comment") (string= name "comment.block")) :comment)
    ((or (string= name "string") (string= name "path")) :string)
    ((or (string= name "character") (string= name "string.escape")) :escape)
    ((or (string= name "number") (string= name "constant")) :constant)
    ((string= name "constant.builtin") :constant)
    ((or (string= name "keyword") (string= name "keyword.function")) :keyword)
    ((or (string= name "function") (string= name "function.call")
         (string= name "function.definition")) :function-name)
    ((or (string= name "function.builtin") (string= name "operator")
         (string= name "variable.builtin")) :builtin)
    ((string= name "variable.parameter") :variable-param)
    ((string= name "variable") :variable)
    ((string= name "type") :type)
    (t nil)))

(defun reclassify (face text start end)
  (when (or (eq face :function-name) (eq face :variable))
    (let ((sym (string-downcase (subseq text
                                        (min start (length text))
                                        (min end (length text))))))
      (cond
        ((and (eq face :function-name) (gethash sym *cl-special-forms*))
         (return-from reclassify :keyword))
        ((and (eq face :variable) (gethash sym *cl-constants*))
         (return-from reclassify :constant)))))
  face)


;;;; Highlight computation

(defun build-line-offsets (text)
  (let ((offsets (list 0)))
    (loop for i from 0 below (length text)
          when (char= (char text i) #\Newline)
            do (push (1+ i) offsets))
    (coerce (nreverse offsets) 'vector)))

(defun byte-to-line-col (byte-pos offsets)
  (let ((lo 0) (hi (1- (length offsets))))
    (loop while (<= lo hi)
          do (let ((mid (ash (+ lo hi) -1)))
               (cond
                 ((> (aref offsets mid) byte-pos) (setf hi (1- mid)))
                 ((and (< mid (1- (length offsets)))
                       (<= (aref offsets (1+ mid)) byte-pos))
                  (setf lo (1+ mid)))
                 (t (return-from byte-to-line-col
                      (values mid (- byte-pos (aref offsets mid))))))))
    (values (1- (length offsets)) 0)))

(defun compute-highlights (runtime language text)
  (let ((entry (ensure-language runtime language)))
    (unless entry (return-from compute-highlights nil))
    (let ((highlights nil)
          (parser (entry-parser entry))
          (query  (entry-query entry))
          (cache  (entry-capture-cache entry)))
    (handler-case
        (let* ((text-bytes (ts-byte-length text))
               (tree (ts-parser-parse-string parser text text-bytes))
               (node-buf (ts-alloc 32))
               (match-buf (ts-alloc 16))
               (cap-idx-buf (ts-alloc 4))
               (offsets (build-line-offsets text)))
          (unwind-protect
               (when tree
                 (ts-tree-root-node tree node-buf)
                 (let ((cursor (ts-query-cursor-new)))
                   (unwind-protect
                        (progn
                          (ts-query-cursor-exec cursor query node-buf)
                          (loop
                            (multiple-value-bind (matched idx start end)
                                (ts-next-capture cursor query match-buf cap-idx-buf)
                              (when (zerop matched) (return))
                              (let* ((name (if (< idx (length cache))
                                               (aref cache idx)
                                               ""))
                                     (raw-face (capture-name-to-face name))
                                     (face (when raw-face
                                             (reclassify raw-face text start end))))
                                (when face
                                  (multiple-value-bind (sl sc) (byte-to-line-col start offsets)
                                    (multiple-value-bind (el ec) (byte-to-line-col end offsets)
                                      (if (= sl el)
                                          (push (list sl sc ec face) highlights)
                                          (progn
                                            (push (list sl sc 999 face) highlights)
                                            (loop for l from (1+ sl) below el
                                                  do (push (list l 0 999 face) highlights))
                                            (push (list el 0 ec face) highlights))))))))))
                     (ts-query-cursor-delete cursor)))
                 (ts-tree-delete tree))
            (ts-free node-buf)
            (ts-free match-buf)
            (ts-free cap-idx-buf)))
      (error (c)
        (ignore-errors
          (with-open-file (f "/tmp/pine-ts-trace.log" :direction :output
                             :if-exists :append :if-does-not-exist :create)
            (format f "compute-highlights ERROR: ~a~%" c)))))
      (nreverse highlights))))
