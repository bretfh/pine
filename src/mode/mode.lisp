(defpackage #:pine.editor.mode
  (:use :cl)
  (:export
   #:mode #:major-mode #:minor-mode
   #:base-mode #:text-mode #:lisp-mode #:repl-mode #:terminal-mode
   #:debugger-mode #:minibuffer-mode #:layout-mode #:overwrite-mode
   #:mode-name #:mode-keymap #:mode-indicator #:parent-mode #:ts-language
   #:precedence #:transparent
   #:register-mode #:find-mode #:all-mode-names #:global-keymap
   #:mode-for-file #:auto-mode #:*auto-modes*
   #:modes-dispatch-class
   #:defmode #:defminor
   #:dispatch-message))

(in-package #:pine.editor.mode)

;;;; Mode classes. Modes are CLOS classes; the active modes of a buffer are
;;;; composed into one dispatch class (MOP) so command execution and behavior
;;;; layer minor -> major via method combination.

(defclass mode ()
  ((name      :initarg :name      :reader mode-name      :initform nil)
   (keymap    :initarg :keymap    :reader mode-keymap    :initform nil)
   (indicator :initarg :indicator :reader mode-indicator :initform "")))

(defclass major-mode (mode)
  ((parent-mode :initarg :parent-mode :reader parent-mode :initform nil)
   (ts-language :initarg :ts-language :reader ts-language  :initform nil)))

(defclass minor-mode (mode)
  ((precedence  :initarg :precedence  :reader precedence  :initform 0)
   (transparent :initarg :transparent :reader transparent :initform nil)))

(defmethod pine.editor.keymap:keymap ((m mode)) (mode-keymap m))

;;;; The global map is the one keymap no mode owns: it applies whatever mode a
;;;; buffer is in, and it is what global-set-key writes into. Every other map
;;;; is a mode's, reached through KEYMAP.

(defvar *global-keymap* (pine.editor.keymap:make-keymap :name :global))

;;;; Registry: mode singletons, keyed by keyword name.

(defvar *modes* (make-hash-table :test 'eq))

(defun register-mode (m) (setf (gethash (mode-name m) *modes*) m))
(defun find-mode (name) (gethash name *modes*))

(defun all-mode-names ()
  "Every registered mode's keyword name."
  (loop :for name :being :the :hash-keys :of *modes* :collect name))

(defun global-keymap () *global-keymap*)

(defmethod pine.editor.keymap:keymap ((name symbol))
  "Resolve a keymap designator: :global for the map no mode owns, or a mode
keyword for that mode's."
  (if (eq name :global)
      *global-keymap*
      (let ((m (find-mode name)))
        (unless m (error "no keymap or mode named ~s" name))
        (mode-keymap m))))

;;;; Defining a mode is what registers it. DEFMODE makes the class, gives it a
;;;; keymap parented to its parent mode's, and registers the singleton; a
;;;; caller that wants a keymap of its own hands one in with :keymap. There is
;;;; no second way to define a mode, and no step afterwards that installs it.

(defmacro defmode (name (&key (parent :text-mode) (indicator "") ts-language keymap)
                   &body body)
  "Define major mode NAME (a symbol) deriving from PARENT (a mode keyword) and
register it as :NAME. Its keymap is parented to PARENT's unless KEYMAP is
given. BODY runs after the class exists, for the methods that give the mode
its behaviour. Redefining replaces the class, the keymap and the registration
together."
  (let ((key (intern (string-upcase (string name)) :keyword)))
    `(let ((parent-mode (or (find-mode ,parent)
                            (error "defmode ~s: no parent mode ~s" ',name ,parent))))
       (c2mop:ensure-class ',name :direct-superclasses (list (class-of parent-mode)))
       (register-mode
        (make-instance ',name :name ,key :parent-mode parent-mode
                       :indicator ,indicator :ts-language ,ts-language
                       :keymap (or ,keymap
                                   (pine.editor.keymap:make-keymap
                                    :name ,key
                                    :parent (pine.editor.keymap:keymap parent-mode)))))
       ,@body
       ,key)))

(defmacro defminor (name (&key (precedence 5) transparent (indicator "") keymap)
                    &body body)
  "Define minor mode NAME (a symbol) and register it as :NAME. Its keymap
stands alone: a minor mode augments whatever major mode is on, so it inherits
from none. Higher PRECEDENCE is consulted first."
  (let ((key (intern (string-upcase (string name)) :keyword)))
    `(progn
       (c2mop:ensure-class ',name
                           :direct-superclasses (list (find-class 'minor-mode)))
       (register-mode
        (make-instance ',name :name ,key :precedence ,precedence
                       :transparent ,transparent :indicator ,indicator
                       :keymap (or ,keymap (pine.editor.keymap:make-keymap :name ,key))))
       ,@body
       ,key)))


(defvar *auto-modes* (make-hash-table :test 'equalp)
  "File extension (no dot) -> mode keyword, consulted by MODE-FOR-FILE before the
built-in mappings. Users add to it with AUTO-MODE from init.lisp.")

(defun auto-mode (ext mode)
  "Open files with extension EXT (a string, no dot) in MODE (a mode keyword).
Replaces any prior mapping, so reloading init.lisp is safe."
  (setf (gethash (string ext) *auto-modes*) mode))

(defun mode-for-file (path)
  (let ((ext (pathname-type (pathname path))))
    (cond ((null ext) nil)
          ((gethash ext *auto-modes*))
          ((member ext '("lisp" "cl" "asd" "asdf" "lsp") :test #'string-equal)
           :lisp-mode)
          (t nil))))

;;;; A set of active modes composed into one class, so command execution and
;;;; buffer behaviour layer minor -> major through method combination. Which
;;;; modes are active is a client's business (pine.editor.frame); building the class
;;;; from them is this layer's.

(defvar *dispatch-classes* (make-hash-table :test 'equal))

(defun modes-dispatch-class (classes)
  (let ((key (mapcar #'class-name classes)))
    (or (gethash key *dispatch-classes*)
        (setf (gethash key *dispatch-classes*)
              (c2mop:ensure-class (gensym "PINE-MODES")
                                  :direct-superclasses classes)))))

;;;; The buffer-behavior interface. The buffer actor's receive calls
;;;; (dispatch-message MODE SELF TAG PLIST); the verb methods live in
;;;; mode/edit.lisp, layered base -> text -> subclasses.

(defgeneric dispatch-message (mode self tag plist)
  (:documentation "Handle TAG (a keyword verb) with PLIST for the buffer actor
SELF, under MODE. Specialize on the mode class; call-next-method layers a
subclass over its parent."))

;;;; The modes pine ships, in derivation order. base-mode is the root, so it
;;;; is the one that cannot use DEFMODE: it has no parent to derive from.

(defclass base-mode (major-mode) ())

(register-mode (make-instance 'base-mode :name :base-mode :indicator "BASE"
                              :keymap (pine.editor.keymap:make-keymap :name :base-mode)))

(defmode text-mode (:parent :base-mode :indicator "TEXT"))
(defmode lisp-mode (:parent :text-mode :indicator "LISP" :ts-language :commonlisp))
(defmode repl-mode (:parent :text-mode :indicator "REPL"))
(defmode terminal-mode (:parent :base-mode :indicator "TERM"))
(defmode debugger-mode (:parent :base-mode :indicator "DEBUG"))

(defminor overwrite-mode (:precedence 10 :transparent t :indicator "Ovwrt"))

;;;; The minibuffer's completion and exit keys. Every other key falls through
;;;; to text-mode, so the prompt has full editing.
(defminor minibuffer-mode (:precedence 20))

;;;; Layout buffers: selection nav and activation over the node tree.
(defminor layout-mode (:precedence 15))

